;;; Self-Documentation. -*-Mode:LISP;Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Worry about whether DOCUMENTATION-FUNCTION stuff will get called
;;; with the right streams set up.  

(DEFVAR *COM-DOCUMENTATION-ALIST*
	'((#/B . COM-EDITOR-HELP)
	  (#/C . COM-SELF-DOCUMENT)
	  (#/L . COM-WHAT-LOSSAGE)
	  (#/D . COM-DESCRIBE-COMMAND)
	  (#/A . COM-APROPOS)
	  (#/U . COM-UNDO)
	  (#/V . COM-VARIABLE-APROPOS)
	  (#/W . COM-WHERE-IS)))

(DEFCOM COM-DOCUMENTATION "Run a specified documentation command.
You type a character.  To get a basic introduction to ZWEI, type B.
To find out what a certain character does, type C and that character.
To find out what a named command does, type D and the command name.
To find all commands whose names contain a certain substring,
  type A and then the substring.
To find out the last 60 characters you typed, if you are confused, type L.
More advanced options:
   U - Undo; V - run Variable Apropos; W - run Where Is;
   SPACE repeats the previous documentation request, if any." ()
  (DO ((CHAR 0)
       (*IN-COM-DOC-P* T)
       (*REPEAT-DOC-P* NIL))
      (NIL)
    (TYPEIN-LINE " Doc A,B,C,D,L,U,V,W,<space>,?: ")
    (TYPEIN-LINE-ACTIVATE
      (SETQ CHAR (DO ((CHAR (CHAR-UPCASE (FUNCALL STANDARD-INPUT ':TYI))
			    (CHAR-UPCASE (FUNCALL STANDARD-INPUT ':TYI))))
		     ((MEMQ CHAR '(#/A #/B #/C #/D #/L #/U #/V #/W #\SP #/?)) CHAR)
		   (AND (= CHAR #/G) (BARF))
		   (BEEP))))
    (COND ((= CHAR #\SP)
	   (SETQ *REPEAT-DOC-P* T)
	   (SETQ CHAR *COM-DOC-LAST-CHAR*))
	  (T (SETQ *COM-DOC-LAST-CHAR* CHAR)))
    (IF (= CHAR #/?)
	(FORMAT T "COM-DOCUMENTATION:~%~A~&"
		(GET 'COM-DOCUMENTATION 'DOCUMENTATION))
	(LET ((FUNCTION (CDR (ASSQ CHAR *COM-DOCUMENTATION-ALIST*))))
	  (AND FUNCTION (RETURN (FUNCALL FUNCTION)))))))

;;; Show user the basic documentation file.
(DEFUN COM-EDITOR-HELP ()
  (TYPEIN-LINE "Basic ZWEI Documentation. ")
  (VIEW-FILE "AI: ZWEI; BASIC ZWEI")
  DIS-NONE)

(DEFCOM COM-DOCUMENT-CONTAINING-COMMAND "Print documentation on the command that you
are in the middle of." ()
  (FORMAT T "~%You are typing in the mini-buffer.  The command in progress is~%~A:~%"
	  (COMMAND-NAME *MINI-BUFFER-COMMAND-IN-PROGRESS*))
  (PRINT-DOC ':FULL *MINI-BUFFER-COMMAND-IN-PROGRESS*)
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
  DIS-NONE)

;;; A command (a COM- symbol) either has "smart" or "normal" handling of
;;; documentation.  All commands should have a COMMAND-NAME property, whose
;;; value is the nice-looking string form of the command's name.
;;; It should also have a DOCUMENTATION property, whose value is the
;;; string which is the full documentation.  If short documentation
;;; (the one-line kind produced by List Commands) is needed, it is just
;;; the first line of the full documentation.

;;; A command with "smart" handling is detected by the presence of
;;; a DOCUMENTATION-FUNCTION property.  The value of this property
;;; should be a function, which is called with three arguments:
;;;    First, the command symbol itself.
;;;    Secondly, the character typed to get this command, or NIL.  If the second
;;;       argument is NIL, that means that the caller does not have any particular
;;;       character in mind (e.g. in List Commands).  The documentation-function
;;;       should be prepared to deal with this case.
;;;    Thirdly, an operation which tells the function what to do.  They are:
;;;       :NAME  Return your name as a string, (e.g. "Self Insert")
;;;       :FULL  Type out full documentation to standard-output.
;;;       :SHORT Type out short documentation to standard-output.

;;; The symbols on the *COMMAND-HOOK* may also want to document
;;; themselves when the user asks for self-documentation.  Any hook
;;; which does should have an HOOK-DOCUMENTATION-FUNCTION property,
;;; whose value is a function of two arguments, the command which the
;;; hook is preceeding, and the character typed.  (The second argument
;;; will never be NIL.) This function will only be called on the user's
;;; request for FULL (not SHORT) documentation. The function should print
;;; out stuff to standard-output.  It may assume the cursor is at the left
;;; edge, and it should leave the cursor there.

(DEFCOM COM-SELF-DOCUMENT "Print out documentation for the command on a given key." (KM)
  (LET (CHAR)
    (TYPEIN-LINE "Document command: ")
    (TYPEIN-LINE-ACTIVATE
      (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
	(SETQ CHAR (FUNCALL *TYPEIN-WINDOW* ':MOUSE-OR-KBD-TYI))))
    (TYPEIN-LINE-MORE "~:@C" CHAR)
    (DOCUMENT-KEY CHAR *COMTAB*))
  DIS-NONE)

;;; Document the character CHAR in the given COMTAB, in the typeout area.
(DEFUN DOCUMENT-KEY (CHAR COMTAB)
  (PROG (TEM PREFIX)
	(FORMAT T "~:@C" CHAR)
     L  (SETQ TEM (COMMAND-LOOKUP CHAR COMTAB T))
	(COND ((NULL TEM)
	       (FORMAT T " is undefined.~%"))
	      ((SYMBOLP TEM)
	       (FORMAT T " is ~A, implemented by " (COMMAND-NAME TEM))
	       (FUNCALL STANDARD-OUTPUT ':ITEM 'FUNCTION-NAME TEM)
	       (FORMAT T ":~%")
	       (DO L *COMMAND-HOOK* (CDR L) (NULL L)
		   (LET ((DOCFN (GET (CAR L) 'HOOK-DOCUMENTATION-FUNCTION)))
		     (AND DOCFN
			  (FUNCALL DOCFN TEM CHAR))))
	       (PRINT-DOC ':FULL TEM CHAR))
	      ((LISTP TEM)
	       (FORMAT T " is an alias for ~@[~:@C ~]~:@C.~%~@[~:@C ~]~:@C"
		       PREFIX (SETQ CHAR (DPB (FIRST TEM) %%KBD-CONTROL-META (SECOND TEM)))
		       PREFIX CHAR)
	       (GO L))
	      ((MACRO-COMMAND-P TEM)
	       (FORMAT T " is a user defined macro named ~A.
With no argument, run the macro with the repeat count in its definition.
With an argument, ignore the repeat count in its definition and use
the argument instead.~%"
		       (SYMEVAL-IN-CLOSURE TEM 'SYMBOL)))
	      ((PREFIX-COMMAND-P TEM)
	       (FORMAT T " is an escape-prefix for more commands.
It reads a character (subcommand) and dispatches on it.
Type a subcommand to document (or * for all):~%")
	       (SETQ PREFIX CHAR
		     CHAR (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
			      (FUNCALL STANDARD-INPUT ':TYI)))
	       (FORMAT T "~:@C" PREFIX)
	       (COND ((= CHAR #/*)
		      (FORMAT T " has these subcommands:~%")
		      (DOTIMES (I 4)
			(DOTIMES (J 220)
			  (PRINT-SHORT-DOC-FOR-TABLE
			    (DPB I %%KBD-CONTROL-META J)
			    (GET-PREFIX-COMMAND-COMTAB TEM)
			    3))))
		     (T
		      (FORMAT T " ~:@C" CHAR)
		      (SETQ COMTAB (GET-PREFIX-COMMAND-COMTAB TEM))
		      (GO L))))
	      ((MENU-COMMAND-P TEM)
	       (FORMAT T " is a menu command with the following subcommands:~%")
	       (DO ((L (GET-MENU-COMMAND-COMMANDS TEM) (CDR L))
		    (FLAG T NIL))
		   ((NULL L))
		 (FORMAT T "~:[, ~]~A" FLAG (CAAR L))))
	      (T (FORMAT T " is garbage!?~%")))))

;;; This prints the documentation on a command.  It is NOT given a command
;;; dispatch table, and so it cannot understand aliases nor command prefixes.
;;; If the caller wants to win with these, he must hack them himself.
;;; The caller may optionally pass the character used to get this command,
;;; which will be passed to any documentation-function to use if it wants.
;;; The caller must do his own "Control-A is COM-FOOBAR:" line.
(DEFUN PRINT-DOC (OP COMMAND &OPTIONAL (CHAR NIL) &AUX DOC)
  (COND ((NULL COMMAND)
	 (FORMAT T "The command is undefined.~%"))
	((SYMBOLP COMMAND)
	 (COND ((GET COMMAND 'DOCUMENTATION-FUNCTION)
		(FUNCALL (GET COMMAND 'DOCUMENTATION-FUNCTION)
			 COMMAND CHAR OP)
		(FORMAT T "~&")
		)
	       ((SETQ DOC (GET COMMAND 'DOCUMENTATION))
		(FORMAT T "~A~&"
			(SELECTQ OP
			  (:FULL DOC)
			  (:SHORT
			   (IF DOC
			       (LET ((FIRST-CR (STRING-SEARCH-CHAR #\CR DOC)))
				 (IF FIRST-CR
				     (NSUBSTRING DOC 0 FIRST-CR)
				     DOC))
			       ""))
			  (OTHERWISE (FERROR NIL "Bad op ~A" OP)))))))
	((PREFIX-COMMAND-P COMMAND)
	 (FORMAT T "The command is an escape-prefix for more commands.~%"))))

(DEFUN COMMAND-NAME (COMMAND &AUX FN)
  (CHECK-ARG COMMAND SYMBOLP "a symbol")
  (COND ((SETQ FN (GET COMMAND 'DOCUMENTATION-FUNCTION))
	 (FUNCALL FN COMMAND NIL ':NAME))
	((GET COMMAND 'COMMAND-NAME))
	(T (FERROR NIL "~S does not have a name" COMMAND))))

(DEFCOM COM-LIST-COMMANDS "List all extended commands." ()
  (FORMAT T "~%   Extended commands:~2%")
  (DO L (EXTENDED-COMMAND-ALIST *COMTAB*) (CDR L) (NULL L)
      (FORMAT T "~30,5,2A" (CAAR L))
      (PRINT-DOC ':SHORT (CDAR L))
      (FORMAT T "~&"))
  (FORMAT T "~%Done.~%")
  DIS-NONE)

(DEFCOM COM-APROPOS "List commands whose names contain a given string.
Tell on which key(s) each command can be found.
Leading and trailing spaces in the substring are NOT ignored - they
must be matched by spaces in the command name." ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-SEARCH-STRINGS "Apropos. (Substring:)")
    (DOLIST (X *COMMAND-ALIST*)
      (LET ((NAME (COMMAND-NAME (CDR X))))
	(COND ((FUNCALL FUNCTION KEY NAME)
	       (FORMAT T "~30,5,2A" NAME)
	       (PRINT-DOC ':SHORT (CDR X))
	       (FORMAT T "~&")
	       (AND (> (FIND-COMMAND-ON-KEYS (CDR X) 4 "  which can be invoked via: ") 0)
		    (TERPRI))
	       (AND (EXTENDED-COMMAND-P (CDR X))
		    (> (FIND-COMMAND-ON-KEYS 'COM-EXTENDED-COMMAND 1
					     "  which can be invoked via: ") 0)
		    (FORMAT T " ~A~%" NAME))
	       ))))
    (FORMAT T "~%Done.~%"))
  DIS-NONE)

(DEFCOM COM-WHERE-IS "List all characters that invoke a given command.
Reads the command name with completion from the mini-buffer." ()
  (LET ((CMD (COMPLETING-READ-FROM-MINI-BUFFER
	       "Where is:" *COMMAND-ALIST* NIL NIL
	       "You are typing the name of a command, and you will be told
all characters that invoke the command."
	       )))
    (COND ((EQUAL CMD "") (BARF))
	  (T (FORMAT T (CAR CMD))
	     (COND ((ZEROP (FIND-COMMAND-ON-KEYS (CDR CMD) 77777 " can be invoked via: "))
		    (FORMAT T " is not on any keys.~%"))
		   (T (TERPRI))))))
  DIS-NONE)

;;; Infernal function of COM-APROPOS and COM-WHERE-IS.  Look for COMMAND
;;; in the keyboard dispatch table, and tell the user by printing to
;;; STANDARD-OUTPUT.  Returns the number of keys on which the command
;;; was found.  Stops looking after the LIMITth
;;; occurence of the key.
(DEFUN FIND-COMMAND-ON-KEYS (COMMAND LIMIT MESSAGE
			     &OPTIONAL (COMTAB *COMTAB*) (COUNT 0) (TAG 'TOP) PREFIX
			     &AUX CHAR TEM)
  (*CATCH TAG
    (DOTIMES (I 4)
      (DOTIMES (J 220)
	(SETQ CHAR (DPB I %%KBD-CONTROL-META J))
	(SETQ TEM (COMMAND-LOOKUP CHAR COMTAB T))
	(COND ((AND TEM (SYMBOLP TEM))
	       ;; This is a real command.
	       (COND ((EQ TEM COMMAND)
		      (COND ((> (SETQ COUNT (1+ COUNT)) LIMIT)
			     (FORMAT T ", etc.")
			     (*THROW 'TOP NIL)))
		      (FORMAT T (IF (= COUNT 1)
				    MESSAGE
				    ", "))
		      (AND PREFIX
			   (FORMAT T "~:C " PREFIX))
		      (FORMAT T "~:@C" CHAR))
		     ((AND (EQ TEM 'COM-DOCUMENTATION)
			   (SETQ TEM (CAR (RASSOC COMMAND *COM-DOCUMENTATION-ALIST*))))
		      (COND ((> (SETQ COUNT (1+ COUNT)) LIMIT)
			     (FORMAT T ", etc.")
			     (*THROW 'TOP NIL)))
		      (FORMAT T (IF (= COUNT 1)
				    MESSAGE
				    ", "))
		      (FORMAT T "~:C ~:@C" CHAR TEM))))
	      ((PREFIX-COMMAND-P TEM)
	       (SETQ COUNT (FIND-COMMAND-ON-KEYS COMMAND
						 LIMIT
						 MESSAGE
						 (GET-PREFIX-COMMAND-COMTAB TEM)
						 COUNT
						 'NOT-TOP
						 CHAR)))))))
  COUNT)

;Returns a list of the commands which are in *COMMAND-ALIST* but not reachable
;from the current comtab.
(DEFVAR ALL-COMTABS '(*STANDARD-COMTAB* *STANDARD-CONTROL-X-COMTAB* *COMPLETING-READER-COMTAB*
		      *CONTROL-R-COMTAB* *RECURSIVE-EDIT-COMTAB* *STANDALONE-COMTAB*
		      *ZMACS-COMTAB* *ZMACS-CONTROL-X-COMTAB*))
			;Unfortunately the mode-specific comtab is shared, can't win on those

(DEFUN UNREACHABLE-COMMAND-LIST (&AUX (L (MAPCAR #'CDR *COMMAND-ALIST*)))
  (DOLIST (COMTAB ALL-COMTABS)
    (SETQ L (UNREACHABLE-COMMAND-LIST-INTERNAL (SYMEVAL COMTAB) L)))
  (SORT L #'STRING-LESSP))

(DEFUN UNREACHABLE-COMMAND-LIST-INTERNAL (*COMTAB* L &AUX CHAR TEM)
   (DOTIMES (I 4)
     (DOTIMES (J 220)
       (SETQ CHAR (DPB I %%KBD-CONTROL-META J))
       (SETQ TEM (COMMAND-LOOKUP CHAR *COMTAB* T))
       (COND ((AND TEM (SYMBOLP TEM))
	      (SETQ L (DELQ TEM L)))
	      ((PREFIX-COMMAND-P TEM)
	       (SETQ L (UNREACHABLE-COMMAND-LIST-INTERNAL
			 (GET-PREFIX-COMMAND-COMTAB TEM) L))))))
   (DOLIST (C L)
     (AND (EXTENDED-COMMAND-P C) (SETQ L (DELQ C L))))
   L)

(DEFUN EXTENDED-COMMAND-P (SYMBOL)
  (DO-NAMED EXTENDED-COMMAND-P
       C *COMTAB* (COMTAB-INDIRECT-TO C) (NULL C)
    (DOLIST (X (EXTENDED-COMMAND-ALIST C))
      (AND (EQ (CDR X) SYMBOL) (RETURN-FROM EXTENDED-COMMAND-P T)))))

(DEFCOM COM-DESCRIBE-COMMAND "Describe a command.
Prints the full documentation for a command.  The command
may be a function name or an extended command name, and you
need only type enough to be unique." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
	     "Describe command:"
	     *COMMAND-ALIST*
	     NIL
	     NIL
	     "You are typing the name of a command, which will be described."
	     )))
    (COND ((EQUAL X "") (BARF))
	  (T (PRINT-DOC ':FULL (CDR X)))))
  DIS-NONE)

;;; This command goes on keys which are normally self-inserting.
;;; *STANDARD-COMMAND* is usually COM-SELF-INSERT.
(DEFCOM COM-STANDARD DOCUMENT-STANDARD-COMMAND ()
  (FUNCALL *STANDARD-COMMAND*))

;;; This is the documentation function for *STANDARD-COMMAND*.
(DEFUN DOCUMENT-STANDARD-COMMAND (COMMAND CHAR OP)
  (SELECTQ OP
    (:FULL  (PRINT-DOC ':FULL  *STANDARD-COMMAND* CHAR))
    (:SHORT (PRINT-DOC ':SHORT *STANDARD-COMMAND* CHAR))
    (:NAME  (COMMAND-NAME *STANDARD-COMMAND*))
    (OTHERWISE (FERROR NIL "Unknown operation ~A; ~S ~S" OP COMMAND
		       CHAR))))

;;; This is used for commands which make a table of subcommands, like
;;; the smart doc for bit prefixes and command prefixes (e.g. Control-X).
;;; It takes a char and a table, and an indentation in spaces.  It prints
;;; one or two lines of stuff, with the given indentation.
(DEFUN PRINT-SHORT-DOC-FOR-TABLE (CHAR COMTAB INDENTATION)
  (LET ((X (COMMAND-LOOKUP CHAR COMTAB T)))
    (COND ((NULL X))			;undefined
	  ((LISTP X))			;alias
	  ((MACRO-COMMAND-P X)
	   (FORMAT T "~VX~:C is a user defined macro.~%" INDENTATION CHAR))
	  ((PREFIX-COMMAND-P X)
	   (FORMAT T "~VX~:C reads another character and dispatches.~%"
		   INDENTATION CHAR))
	  ((NOT (SYMBOLP X)))		;??
	  (T
	   (FORMAT T "~VX~:C is ~A:~%~VX" INDENTATION CHAR X (+ 5 INDENTATION))
	   (PRINT-DOC ':SHORT X CHAR)))))
