;;; Zwei commands, see ZWEI;COMA for comments -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFUN MAYBE-PUSH-POINT (BP)
  (AND *AUTO-PUSH-POINT-OPTION*
       (BPS-FAR-APART BP (POINT) *AUTO-PUSH-POINT-OPTION*)
       (POINT-PDL-PUSH BP *WINDOW*)))

(DEFCOM COM-INSTALL-COMMAND "Install a specified function on a specified key.
The name of the function is read from the mini-buffer (the top of the kill ring
contains the name of the current defun), and a character from the echo area.
If the key is currently holding a command prefix (like Control-X), it will ask
you for another character, so that you can redefine Control-X commands.  However,
with a numeric argument, it will assume you want to redefine Control-X itself,
and will not ask for another character." ()
    (DO (NAME) (NIL)
      (SETQ NAME (READ-FUNCTION-NAME "Name of function to install"
				     (RELEVANT-FUNCTION-NAME (POINT)) NIL 'ALWAYS-READ))
      (AND (OR (FBOUNDP NAME)
	       (TYPEIN-LINE-ACTIVATE
		 (TYPEIN-LINE "~A is not defined, ok to install anyway? " NAME)
		 (Y-OR-N-P NIL *TYPEIN-WINDOW*)))
	   (RETURN (INSTALL-COMMAND-INTERNAL NAME)))))

(DEFCOM COM-INSTALL-MACRO "Install a specified user macro on a specifed key.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer, and the keystroke on which
to install it is read in the echo area.
If the key is currently holding a command prefix (like Control-X), it will ask
you for another character, so that you can redefine Control-X commands.  However,
with a numeric argument, it will assume you want to redefine Control-X itself,
and will not ask for another character." ()
  (OR (MEMQ ':MACRO-PREVIOUS-ARRAY (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream does not support macros"))
  (LET ((PACKAGE SI:PKG-USER-PACKAGE)
	NAME MAC)
    (SETQ NAME (TYPEIN-LINE-READ "Name of macro to install (CR for last macro defined):"))
    (COND ((EQ NAME '*EOF*)
	   (SETQ MAC (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY)
		 NAME (GENSYM))
	   (PUTPROP NAME MAC 'MACRO-STREAM-MACRO))
	  ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
	   (BARF "~A is not a defined macro." NAME)))
    (INSTALL-COMMAND-INTERNAL (MAKE-MACRO-COMMAND NAME))))

(DEFUN INSTALL-COMMAND-INTERNAL (COMMAND)
  (PROMPT-LINE "Key to get it:")
  (PROMPT-LINE-ACTIVATE
    (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
      (DO ((COMTAB *COMTAB*)
	   (KEY (FUNCALL STANDARD-INPUT ':MOUSE-OR-KBD-TYI)
		(FUNCALL STANDARD-INPUT ':TYI)))
	  (NIL)
	(PROMPT-LINE-MORE " ~:@C" KEY)
	(LET ((OLD-COMMAND (COMMAND-LOOKUP KEY COMTAB)))
	  (COND ((AND (PREFIX-COMMAND-P OLD-COMMAND)
		      (NOT *NUMERIC-ARG-P*))
		 (SETQ COMTAB (SYMEVAL-IN-CLOSURE OLD-COMMAND 'COMTAB)))
		(T (COMMAND-STORE COMMAND KEY COMTAB)
		   (RETURN NIL)))))))
  DIS-NONE)

(DEFCOM COM-COUNT-LINES-REGION "Print the number of lines in the region in the echo area." ()
  (REGION (BP1 BP2)
    (TYPEIN-LINE "~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T))))
  DIS-NONE)

(DEFCOM COM-WHERE-AM-I "Print various things about where the point is.
Print the X and Y positions, the octal code for the following character,
the current line number and its percentage of the total file size.
If there is a region, the number of lines in it is printed.
Fast Where Am I prints a subset of this information faster." ()
  (REDISPLAY *WINDOW* ':POINT NIL NIL T)
  (LET ((POINT (POINT))
	(FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*))
	(LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
    (LET ((POINT-LINES (1- (COUNT-LINES FIRST-BP POINT)))
	  (INTERVAL-LINES (1- (COUNT-LINES FIRST-BP LAST-BP)))
	  (AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
	  (BP-IND (BP-INDENTATION POINT))
	  (SW (FONT-SPACE-WIDTH)))
      (TYPEIN-LINE "X=[~D. chars|~D. pixels|~:[~S~;~D.~] columns] ~
			Y=~D.~@[ Char=~O~] Line=~D.(~D%)"
		   (BP-INDEX POINT)
		   BP-IND
		   (ZEROP (\ BP-IND SW))
		   (IF (ZEROP (\ BP-IND SW))
		       (// BP-IND SW)
		       (// (FLOAT BP-IND) SW))
		   (FIND-BP-IN-WINDOW *WINDOW* POINT)
		   (AND (NOT AT-END-P) (BP-CHAR POINT))
		   POINT-LINES
		   (IF (ZEROP INTERVAL-LINES)
		       0
		       (// (* 100. POINT-LINES) INTERVAL-LINES)))))
  (AND (WINDOW-MARK-P *WINDOW*)
       (REGION (BP1 BP2)
	 (TYPEIN-LINE-MORE ", Region has ~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T)))))
  DIS-NONE)

(DEFCOM COM-FAST-WHERE-AM-I "Quickly print various things about where the point is.
Print the X and Y positions, and the octal code for the following character.
Where Am I prints the same things and more." ()
  (REDISPLAY *WINDOW* ':POINT NIL NIL T)
  (LET ((POINT (POINT)))
    (LET ((AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
	  (BP-IND (BP-INDENTATION POINT))
	  (SW (FONT-SPACE-WIDTH)))
      (TYPEIN-LINE "X=[~D. chars|~D. pixels|~:[~S~;~D.~] columns] Y=~D.~@[ Char=~O~]"
		   (BP-INDEX POINT)
		   BP-IND
		   (ZEROP (\ BP-IND SW))
		   (IF (ZEROP (\ BP-IND SW))
		       (// BP-IND SW)
		       (// (FLOAT BP-IND) SW))
		   (FIND-BP-IN-WINDOW *WINDOW* POINT)
		   (AND (NOT AT-END-P) (BP-CHAR POINT)))))
  DIS-NONE)

(DEFCOM COM-ARGLIST "Print the argument list of the specified function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) and prints the arglist
in the echo area." ()
  (LET ((NAME (READ-FUNCTION-NAME "Arglist" (RELEVANT-FUNCTION-NAME (POINT)) T)))
    (PRINT-ARGLIST NAME))
  DIS-NONE)

(DEFCOM COM-QUICK-ARGLIST "Print the argument list of the function to left of cursor." ()
  (IF *NUMERIC-ARG-P*
      (COM-ARGLIST)
      (LET ((SYMBOL (RELEVANT-FUNCTION-NAME (POINT))))
	(COND ((AND (MEMQ SYMBOL '(FUNCALL FUNCALL-SELF <-))
		    (SETQ SYMBOL (RELEVANT-METHOD-NAME (POINT)
						       (IF (EQ SYMBOL 'FUNCALL-SELF) 1 2))))
	       (MULTIPLE-VALUE-BIND (ARGLIST NAME RETLIST)
		   (METHOD-ARGLIST SYMBOL)
		 (TYPEIN-LINE "~S: ~:A~@[  ~:A~]"
			      (OR NAME SYMBOL) ARGLIST RETLIST)))
	      ((FDEFINEDP SYMBOL)
	       (PRINT-ARGLIST SYMBOL))
	      ((BARF))))	;Looked hard but couldn't find a defined function
      DIS-NONE))

(DEFUN PRINT-ARGLIST (SYMBOL)
  (MULTIPLE-VALUE-BIND (ARGLIST RETURNS)
      (ARGLIST SYMBOL)
    (TYPEIN-LINE "~S: ~:A~@[  ~:A~]" SYMBOL ARGLIST RETURNS)))

(DEFCOM COM-BRIEF-DOCUMENTATION "Print brief documentation for the specified function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) and prints the first
line of its documentation in the echo area." ()
    (LET ((NAME (READ-FUNCTION-NAME "Brief Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
      (LET ((DOC (FUNCTION-DOCUMENTATION NAME)))
	(COND ((NULL DOC) (TYPEIN-LINE "~S is not documented" NAME))
	      (T (TYPEIN-LINE "~S: ~A" NAME
			      (NSUBSTRING DOC 0 (STRING-SEARCH-CHAR #\CR DOC)))))))
    DIS-NONE)

(DEFCOM COM-LONG-DOCUMENTATION "Print long documentation for the specified function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) and displays the
function's arguments and documentation" ()
    (LET ((NAME (READ-FUNCTION-NAME "Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
      (LET ((DOC (FUNCTION-DOCUMENTATION NAME)))
	(COND ((NULL DOC) (TYPEIN-LINE "~S is not documented" NAME))
	      (T (PRINT-ARGLIST NAME)
		 (FORMAT T "~%~A" DOC)))))
    DIS-NONE)

(DEFCOM COM-TRACE "Trace or untrace a function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) then pops up a menu
of trace options." ()
  (TV:TRACE-VIA-MENUS (READ-FUNCTION-NAME "Trace" (RELEVANT-FUNCTION-NAME (POINT)) T))
  DIS-NONE)

(DEFCOM COM-WHERE-IS-SYMBOL "Show which packages contain the specified symbol." ()
  (MULTIPLE-VALUE-BIND (SYMBOL NAME)
      (READ-FUNCTION-NAME "Where is symbol" NIL NIL T)
    (WHERE-IS (OR NAME SYMBOL)))
  DIS-NONE)

(DEFCOM COM-COUNT-LINES-PAGE "Type number of lines on this page.
Also add, in parentheses, the number of lines on the page
before point, and the number of lines after point." ()
   (LET ((POINT (POINT)))
     (LET ((N1 (1- (COUNT-LINES (FORWARD-PAGE POINT -1 T) POINT)))
	   (N2 (1- (COUNT-LINES POINT (FORWARD-PAGE POINT 1 T)))))
       (TYPEIN-LINE "Page has ~D (~D + ~D) lines" (+ N1 N2) N1 N2)))
   DIS-NONE)

(DEFCOM COM-LIST-ALL-DIRECTORY-NAMES "List names of all disk directories." ()
   (LOCAL-DECLARE ((SPECIAL *MFD-ARRAY*))
     (OR (BOUNDP '*MFD-ARRAY*)
       (SETQ *MFD-ARRAY* (MAKE-ARRAY NIL 'ART-Q 350. NIL '(0))))
     (STORE-ARRAY-LEADER 0 *MFD-ARRAY* 0)
     (OPEN-FILE (STREAM "DSK: M.F.D. (FILE)" '(IN))
       (DO ((STRING) (ENDP))
	   (NIL)
	 (MULTIPLE-VALUE (STRING ENDP)
	    (FUNCALL STREAM ':LINE-IN NIL))
	 (IF ENDP (RETURN NIL))
	 (ARRAY-PUSH-EXTEND *MFD-ARRAY* STRING)))
     (SORT *MFD-ARRAY* #'STRING-LESSP)
     (LET ((IDX 0)
	   (N (ARRAY-LEADER *MFD-ARRAY* 0)))
       (DO ((I 0 (1+ I))
	    (TO (// N 10.)))
	   (( I TO))
	(DO J 0 (1+ J) ( J 10.)
	  (FORMAT T "~A  " (AREF *MFD-ARRAY* IDX))
	  (SETQ IDX (1+ IDX)))
	(FORMAT T "~%"))
       (DO () (NIL)
	 (AND ( IDX N) (RETURN NIL))
	 (FORMAT T "~A  " (AREF *MFD-ARRAY* IDX))
	 (SETQ IDX (1+ IDX)))))
   DIS-NONE)

(DEFCOM COM-VIEW-DIRECTORY "List an ITS file directory." ()
  (LET ((FILENAME (DEFAULT-FILE-NAME))
	DIRECTORY DEFAULT)
    (SETQ DEFAULT (FORMAT NIL "~A: ~A;"
			  (FUNCALL FILENAME ':DEVICE) (FUNCALL FILENAME ':DIRECTORY)))
    (SETQ DIRECTORY (TYPEIN-LINE-READLINE "Directory name (Default: ~A)" DEFAULT))
    (AND (EQUAL DIRECTORY "") (SETQ DIRECTORY DEFAULT))
    (VIEW-DIRECTORY DIRECTORY)))

(LOCAL-DECLARE ((SPECIAL VIEWED-DIRECTORY))
(DEFUN VIEW-DIRECTORY (VIEWED-DIRECTORY)
  (SETQ VIEWED-DIRECTORY (STRING-TRIM '(#/; #\SP #\TAB) (STRING-UPCASE VIEWED-DIRECTORY)))
  (BIND-MODE-LINE ("Viewing Directory " VIEWED-DIRECTORY)
    (VIEW-FILE (STRING-APPEND VIEWED-DIRECTORY "; .FILE. (DIR)")))
   DIS-NONE)
)

(DEFCOM COM-VIEW-LOGIN-DIRECTORY "List files in user's directory." ()
  (VIEW-DIRECTORY (FS:FILE-USER-ID-HSNAME)))

(DEFCOM COM-VIEW-XGP-QUEUE "List XGP queue." ()
  (VIEW-DIRECTORY "XGP:FOO"))

(DEFCOM COM-VIEW-TTY-USERS "TTYF" ()
  (VIEW-DIRECTORY "TTY:FOO"))

(DEFCOM COM-VIEW-MAIL "View any new mail." ()
  (LET ((FILE-NAME (STRING-APPEND (FS:FILE-USER-ID-HSNAME) USER-ID " MAIL")))
    (COND ((FILE-EXISTS-P FILE-NAME)
	   (VIEW-FILE FILE-NAME))
	  (T
	   (TYPEIN-LINE "No new mail"))))
  DIS-NONE)

;;; Evaluation and Compilation commands.

(DEFCOM COM-EVALUATE-MINI-BUFFER "Evaluate a form from the mini-buffer." (KM)
  (EVALUATE-MINI-BUFFER))

(DEFUN EVALUATE-MINI-BUFFER (&OPTIONAL INITIAL-CONTENTS INITIAL-CHAR-POS &AUX INTERVAL)
  (MULTIPLE-VALUE (NIL NIL INTERVAL)
    (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB* INITIAL-CONTENTS INITIAL-CHAR-POS
			 '("Forms to evaluate (end with End)")))
  (LET ((FORM-STRING (STRING-INTERVAL INTERVAL)))
    (DO ((LEN (STRING-LENGTH FORM-STRING))
	 (I 0)
	 (FORM)
	 (EOF '(())))
	(NIL)
      (MULTIPLE-VALUE (FORM I)
	(READ-FROM-STRING FORM-STRING EOF I))
      (COND ((EQ FORM EOF)
	     (AND (> I LEN) (RETURN NIL))
	     (BARF "Unbalanced parentheses.")))
      (DO ((VALS (LET ((STANDARD-OUTPUT *TYPEOUT-WINDOW*)
		       (STANDARD-INPUT *TYPEOUT-WINDOW*))
		   (MULTIPLE-VALUE-LIST (EVAL FORM)))
		 (CDR VALS))
	   (FLAG T NIL))
	  ((NULL VALS))
	(FUNCALL (IF FLAG #'TYPEIN-LINE #'TYPEIN-LINE-MORE) "~:[, ~]~S" FLAG (CAR VALS)))))
  DIS-TEXT)	;DIS-TEXT in case user manually alters the buffer with Lisp code

(DEFCOM COM-EVALUATE-INTO-BUFFER
	"Evaluate a form from the mini-buffer and insert the result into the buffer.
If given an argument, things printed by the evaluation go there as well." (KM)
  (LET ((FORM (TYPEIN-LINE-READ "Lisp form:"))
	(STREAM (INTERVAL-STREAM (POINT) (POINT) T)))
    (FORMAT STREAM "~&~S"
	    (LET ((STANDARD-OUTPUT (IF *NUMERIC-ARG-P* STREAM STANDARD-OUTPUT)))
	      (EVAL FORM)))
    (MOVE-BP (POINT) (FUNCALL STREAM ':READ-BP))
    (MUNG-BP-INTERVAL (POINT)))
  DIS-TEXT)

(DEFCOM COM-EVALUATE-AND-REPLACE-INTO-BUFFER
	"Evaluate the next s-expression and replace the result into the buffer" ()
  (LET ((STREAM (INTERVAL-STREAM (POINT) (INTERVAL-LAST-BP *INTERVAL*) T))
	(POINT (POINT)) (MARK (MARK))
	FORM)
    (SETQ FORM (READ STREAM '*EOF*))
    (AND (EQ FORM '*EOF*) (BARF))
    (SETQ FORM (EVAL FORM))
    (MOVE-BP MARK (FUNCALL STREAM ':READ-BP))
    (UNDO-SAVE POINT MARK T "replacement")
    (PRIN1 FORM STREAM)
    (WITH-BP (END (FUNCALL STREAM ':READ-BP) ':NORMAL)
      (DELETE-INTERVAL POINT MARK T)
      (MOVE-BP POINT END)))
  DIS-TEXT)

(DEFCOM COM-COMPILE-DEFUN "Compile the current defun." ()
   (COMPILE-DEFUN-INTERNAL T "Compiling" "compiled.")
   DIS-NONE)

(DEFCOM COM-EVALUATE-DEFUN "Evaluate the current defun.
Result is typed out in the echo area." ()
   (COMPILE-DEFUN-INTERNAL  (GET-BUFFER-EVALUATOR *INTERVAL*)
			    "Evaluating"
			    "evaluated."
			    ':PROMPT)
   DIS-NONE)

(DEFCOM COM-EVALUATE-DEFUN-VERBOSE "Evaluate the current defun.
Result is typed out in the typeout window." ()
   (COMPILE-DEFUN-INTERNAL  (GET-BUFFER-EVALUATOR *INTERVAL*)
			    "Evaluating"
			    "evaluated."
			    T)
   DIS-NONE)

(DEFCOM COM-EVALUATE-DEFUN-HACK "Evaluate the current defun.
DEFVAR's are turned into SETQ's" ()
   (COMPILE-DEFUN-INTERNAL  (GET-BUFFER-EVALUATOR *INTERVAL*)
			    "Evaluating"
			    "evaluated."
			    ':PROMPT T)
   DIS-NONE)

(DEFUN COMPILE-DEFUN-INTERNAL (COMPILE-P MODE-NAME ECHO-NAME &OPTIONAL USE-TYPEOUT DEFVAR-HACK
							     &AUX BP1 BP2 DEFUN-NAME)
  (COND ((WINDOW-MARK-P *WINDOW*)
	 (SETQ BP1 (MARK) BP2 (POINT))
	 (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
	 (SETQ DEFUN-NAME "Region"))
	((MULTIPLE-VALUE (BP1 DEFUN-NAME) (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
	 (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
	 (SETQ DEFUN-NAME (GET-DEFUN-NAME DEFUN-NAME)
	       DEFVAR-HACK T))
	(T
	 (BARF "Unbalanced parentheses")))
  (PROMPT-LINE "~A ~A" MODE-NAME DEFUN-NAME)
  (COMPILE-INTERVAL COMPILE-P (EQ USE-TYPEOUT T) DEFVAR-HACK BP1 BP2 T
	  (COND ((> *NUMERIC-ARG* 3)
		 'COMPILER:MICRO-COMPILE)
		(T 'COMPILER:MACRO-COMPILE)))
  (FUNCALL (IF (EQ USE-TYPEOUT ':PROMPT) #'PROMPT-LINE #'TYPEIN-LINE)
	   "~A ~A" DEFUN-NAME ECHO-NAME))

;;; Given a BP to the beginning of a DEFUN (as returned by BACKWARD-DEFUN,
;;; or MARK-DEFUN), return the name of the function it defines as a
;;; temporary NSUBSTRING.  Be careful using the returned string.
(DEFUN GET-DEFUN-NAME (BP &AUX BP1)
  ;; Now get the second word after BP.
  (AND (SETQ BP (FORWARD-ATOM BP))
       (SETQ BP (FORWARD-OVER *BLANKS* BP))
       (SETQ BP1 (FORWARD-SEXP BP))
       (STRING-INTERVAL BP BP1)))

(DEFUN GET-BUFFER-EVALUATOR (BUFFER)
  (AND (ZMACS-BUFFER-P BUFFER BUFFER-FILE-GROUP-SYMBOL)
       (GET (BUFFER-FILE-GROUP-SYMBOL BUFFER) ':EVALUATOR)))

(DEFCOM COM-EVALUATE-BUFFER "Evaluate the entire buffer." ()
  (COMPILE-BUFFER "Evaluating" (GET-BUFFER-EVALUATOR *INTERVAL*)))

(DEFCOM COM-COMPILE-BUFFER "Compile the entire buffer." ()
  (COMPILE-BUFFER "Compiling" T))

(DEFUN COMPILE-BUFFER (TYPE COMPILE-P)
  (PROMPT-LINE "~A ~:[rest of ~]buffer." TYPE (NOT *NUMERIC-ARG-P*))
  (LET (BP1 BP2)
    (IF *NUMERIC-ARG-P*
	(SETQ BP1 (POINT) BP2 (INTERVAL-LAST-BP *INTERVAL*))
	(SETQ BP1 *INTERVAL*))
    (COMPILE-INTERVAL COMPILE-P T NIL BP1 BP2 T))
  DIS-NONE)

(DEFCOM COM-EVALUATE-REGION "Evaluate just between point and the mark." ()
  (PROMPT-LINE "Evaluating region.")
  (REGION (BP1 BP2)
    (COMPILE-INTERVAL (GET (BUFFER-FILE-GROUP-SYMBOL *INTERVAL*)	;NIL if no special 
			   ':EVALUATOR)	;evaluator
		      NIL NIL
		      BP1
		      BP2))
  DIS-NONE)

(DEFCOM COM-COMPILE-REGION "Compile just between point and the mark." ()
  (PROMPT-LINE "Compiling region.")
  (REGION (BP1 BP2)
	  (COMPILE-INTERVAL T T NIL BP1 BP2))
  DIS-NONE)

;If COMPILE-P is not T or NIL, its a function to call to do an EVAL-PRINT type operation 
(LOCAL-DECLARE ((SPECIAL COMPILE-P USE-TYPEOUT DEFVAR-HACK COMPILE-PROCESSING-MODE))
(DEFUN COMPILE-INTERVAL (COMPILE-P USE-TYPEOUT DEFVAR-HACK BP1 &OPTIONAL BP2 IN-ORDER-P
			   (COMPILE-PROCESSING-MODE 'COMPILER:MACRO-COMPILE)
			 &AUX (STANDARD-OUTPUT *TYPEOUT-WINDOW*)
			      FILE-GROUP-SYMBOL)
    (SETQ FILE-GROUP-SYMBOL
	  (IF (ZMACS-BUFFER-P *INTERVAL* BUFFER-FILE-GROUP-SYMBOL)
	      (BUFFER-FILE-GROUP-SYMBOL *INTERVAL*)
	      (GENSYM)))
    ;; Should re-read the mode line at the front of the file in case it has changed
    ;; Unfortunately SI:FILE-READ-PROPERTY-LIST doesn't work on interval streams.
    (GET-INTERVAL BP1 BP2 IN-ORDER-P)
    (COMPILER:COMPILE-STREAM (INTERVAL-STREAM BP1 BP2 T) FILE-GROUP-SYMBOL NIL
		    #'(LAMBDA (FORM)
			(COND ((AND DEFVAR-HACK
				    (LISTP FORM)
				    (= (LENGTH FORM) 3)
				    (MEMQ (CAR FORM) '(DEFVAR DEFCONST)))
			       (OR (SYMBOLP (CADR FORM))
				   (FERROR NIL "~S not a recignized form" FORM))
			       (PUTPROP (CADR FORM) T 'SPECIAL)	;Declare it
			       (SETF (CAR FORM) 'SETQ)))	;then always SETQ
			(COND ((EQ COMPILE-P T)
			       (COMPILER:COMPILE-DRIVER FORM #'COMPILE-BUFFER-FORM NIL))
			      (COMPILE-P (FUNCALL COMPILE-P FORM))
			      (T (RECORD-DEFUN FORM *INTERVAL*)
				 (EVAL-PRINT FORM USE-TYPEOUT))))
		    T NIL NIL)))

(DEFUN EVAL-PRINT (OBJECT USE-TYPEOUT)
    (LET ((LIST (MULTIPLE-VALUE-LIST (EVAL OBJECT))))
       (DOLIST (VAL LIST)
	 (IF USE-TYPEOUT (PRINT VAL) (LET ((PRINLENGTH 5) (PRINLEVEL 2))
				       (TYPEIN-LINE "~&~S" VAL))))
       (MVRETURN (CAR LIST) OBJECT)))

;Functional to be passed to COMPILE-DRIVER.
(LOCAL-DECLARE ((SPECIAL COMPILE-PROCESSING-MODE))
(DEFUN COMPILE-BUFFER-FORM (FORM TYPE)
  (SELECTQ TYPE
    ((DECLARE) (EVAL FORM))
    ((RANDOM SPECIAL) (EVAL FORM))
    (DEFUN
     (RECORD-DEFUN FORM *INTERVAL*)
     (COMPILER:COMPILE-1 (CADR FORM) (CONS 'LAMBDA (CDDR FORM)) COMPILE-PROCESSING-MODE))
    (MACRO
     (RECORD-DEFUN FORM *INTERVAL*)
     (COMPILER:COMPILE-1 (CADR FORM) (CONS 'MACRO (CONS 'LAMBDA (CDDR FORM))))))))

;;; This does not really get the right arguments, but can at least make it so that M-.
;;; knows what buffer to sectionize to find the thing
(DEFUN RECORD-DEFUN (FORM INTERVAL)
  (AND (LISTP FORM) (EQ (CAR FORM) 'DEFUN)
       (LET ((SYM (SYMBOL-FROM-STRING (CADR FORM))))
	 (COND ((NOT (ASSQ INTERVAL (GET SYM 'ZMACS-BUFFERS)))
		;; NIL for a LINE will never be believed to be valid, forcing sectionization.
		(PUSH (CONS INTERVAL NIL) (GET SYM 'ZMACS-BUFFERS))
		;; This will make sectionizing forget the bogus entry above.
		(PUSH SYM (GET (BUFFER-FILE-GROUP-SYMBOL INTERVAL) 'ZMACS-SECTION-LIST)))))))

(DEFCOM COM-MACRO-EXPAND-SEXP "Macroexpand the next s-expression" ()
  (LET ((STREAM (INTERVAL-STREAM *INTERVAL*)))
    (FUNCALL STREAM ':SET-BP (POINT))
    (LET ((FORM (READ STREAM '*EOF*)))
      (AND (EQ FORM '*EOF) (BARF))
      (GRIND-TOP-LEVEL (MACRO-EXPAND-ALL FORM))))
  DIS-NONE)

(DEFUN MACRO-EXPAND-ALL (FORM)
  (SETQ FORM (MACROEXPAND FORM))
  (AND (LISTP FORM)
       (DO L FORM (CDR L) (NULL L)
	   (SETF (CAR L) (MACRO-EXPAND-ALL (CAR L)))))
  FORM)

;Given a function (a symbol!), return the correspondence between
;its sublists and symbols and positions in the buffer which holds the text for it.
;Should handle functions which are not symbols.
;; The caller should set up a catch for TRANSFER-CORRESPONDENCE-LOSSAGE
;; in case the function text and expr definition don't actually match.
(DEFUN FUNCTION-CORRESPONDENCE (FUNCTION)
    (PROG* ((LOCATION (CAR (DEFINITION-TEXT-LOCATION FUNCTION)))
	    (BUFFER (CAR LOCATION))
	    (LINE (CDR LOCATION))
	    (INT (DEFUN-INTERVAL (CREATE-BP LINE 0) 1 NIL NIL))
	    (DEFINITION (FDEFINITION FUNCTION))
	    NEWSEXP TEM
	    (CORRESPONDENCE (GET FUNCTION 'ZMACS-CORRESPONDENCE)))
	  (COND ((OR (NULL CORRESPONDENCE)
		     (NEQ (CAR CORRESPONDENCE) DEFINITION)
		     (> (INTERVAL-REAL-TICK INT)
			(CADDR CORRESPONDENCE)))
		 ;; Read in the text.  Get a new sexp for the function,
		 ;; together with a correspondence between it and the text.
		 (MULTIPLE-VALUE (NEWSEXP CORRESPONDENCE)
		   (ESTABLISH-CORRESPONDENCE DEFINITION BUFFER INT))
		 ;; If function is traced, find original definition.
		 (COND ((AND (EQ (CAR DEFINITION) 'NAMED-LAMBDA)
			     (NOT (ATOM (CADR DEFINITION)))
			     (SETQ TEM (ASSQ 'TRACE (CDADR DEFINITION))))
			(SETQ DEFINITION (FDEFINITION (CADR TEM)))))
		 (SETQ TEM (MEMQ NEWSEXP CORRESPONDENCE))
		 (AND TEM (RPLACA TEM DEFINITION))
		 (SETQ NEWSEXP (CDDR NEWSEXP))	;Flush DEFUN or DEFMETHOD, and fn name.
		 (SELECTQ (CAR DEFINITION)	;Flush LAMBDA, or NAMED-LAMBDA and name.
		   (LAMBDA (SETQ DEFINITION (CDR DEFINITION)))
		   (NAMED-LAMBDA
		     (SETQ DEFINITION (CDDR DEFINITION))))
		 ;; Now the new sexp should look like the definition.
		 ;; Move the correspondence to the definition.
		 (TRANSFER-CORRESPONDENCE FUNCTION CORRESPONDENCE NEWSEXP DEFINITION)
		 (PUTPROP FUNCTION CORRESPONDENCE 'ZMACS-CORRESPONDENCE)))
	  (RETURN CORRESPONDENCE)))


(DEFUN ESTABLISH-CORRESPONDENCE (DEFINITION BUFFER BP1 &OPTIONAL BP2 IN-ORDER-P)
    (GET-INTERVAL BP1 BP2 IN-ORDER-P)
    (PROG ((STREAM (INTERVAL-STREAM BP1 BP2 T))
	   (SI:XR-CORRESPONDENCE-FLAG T)
	   SI:XR-CORRESPONDENCE)
      (RETURN (READ STREAM)
	      `(,DEFINITION ,BUFFER ,(INTERVAL-TICK BUFFER)
		,BP1 ,BP2 . ,SI:XR-CORRESPONDENCE))))

;When's the latest any line between BP1 and BP2 was modified?
(DEFUN INTERVAL-REAL-TICK (BP1 &OPTIONAL BP2 IN-ORDER-P)
    (GET-INTERVAL BP1 BP2 IN-ORDER-P)
    (DO ((LINE (BP-LINE BP1) (LINE-NEXT LINE))
	 (END-LINE (BP-LINE BP2))
	 (MAX-TICK 0))
	(())
      (SETQ MAX-TICK (MAX MAX-TICK (LINE-TICK LINE)))
      (AND (EQ LINE END-LINE)
	   (RETURN MAX-TICK))))

;; Given a correspondence from the sexp TEMPDEF, matches up TEMPDEF
;; and REALDEF and clobbers the correspondence to be from REALDEF instead.
;; FUNCTION is just for error messages.  
;; We throw to TRANSFER-CORRESPONDENCE-LOSSAGE if the two sexps don't match.
(DEFUN TRANSFER-CORRESPONDENCE (FUNCTION CORRESPONDENCE TEMPDEF REALDEF)
    (LET ((TEM (MEMQ TEMPDEF CORRESPONDENCE)))
      (AND TEM (RPLACA TEM REALDEF)))
    ;; In the real definition, some displacing macros may have gone off.
    (AND (EQ (CAR REALDEF) 'SI:DISPLACED)
	 (SETQ REALDEF (CADR REALDEF)))
    (OR (= (LENGTH TEMPDEF) (LENGTH REALDEF))
	(THROW NIL TRANSFER-CORRESPONDENCE-LOSSAGE))
    (DO ((TD TEMPDEF (CDR TD))
	 (RD REALDEF (CDR RD)))
	((NULL TD))
      (AND (COND ((ATOM (CAR TD)) (NEQ (CAR TD) (CAR RD)))
		 (T (ATOM (CAR RD))))
	   (THROW NIL TRANSFER-CORRESPONDENCE-LOSSAGE))
      (OR (ATOM (CAR TD))
	  (TRANSFER-CORRESPONDENCE FUNCTION CORRESPONDENCE (CAR TD) (CAR RD)))))

;;; Sorting commands
(DEFCOM COM-SORT-LINES "Sort the region alphabetically by lines" ()
  (REGION (BP1 BP2)
    (SORT-LINES-INTERVAL #'STRING-LESSP BP1 BP2 T))
  DIS-TEXT)

(DEFCOM COM-SORT-PARAGRAPHS "Sort the region alphabetically by paragraphs" ()
  (REGION (BP1 BP2)
    (SORT-INTERVAL-FUNCTIONS #'FORWARD-OVER-BLANK-OR-TEXT-JUSTIFIER-LINES
			     #'(LAMBDA (BP) (FORWARD-PARAGRAPH BP 1 T))
			     #'(LAMBDA (BP) BP)
			     #'INTERVAL-WITH-SORT-INTERVAL-LESSP
			     BP1 BP2 T))
  DIS-TEXT)

(DEFVAR *MAKE-KBD-MACRO-MOVER-COMTAB*)

;;; This returns a function which takes a BP and returns a resultant BP after performing
;;; the given kbd-macro operation.
(DEFUN MAKE-KBD-MACRO-MOVER (PROMPT)
  (COM-START-KBD-MACRO)
  (TYPEIN-LINE "Defining a keyboard macro to ~A~@[; type ~A to finish it~]"
	       PROMPT (KEY-FOR-COMMAND 'COM-END-KBD-MACRO))
  (LET ((STANDARD-INPUT (LET-CLOSED ((OLD-STANDARD-INPUT STANDARD-INPUT))
			  #'(LAMBDA (OP &REST REST)
			      (PROG1 (LEXPR-FUNCALL OLD-STANDARD-INPUT OP REST)
				     (COND ;;When done recording, exit the recursive edit.
					   ((EQ OP ':MACRO-POP)
					    (*THROW 'EXIT-MAKE-KBD-MACRO-MOVER T))
					   ;;If there is an error, exit, it will throw up
					   ;;further.
					   ((EQ OP ':MACRO-ERROR)
					    (*THROW 'EXIT-MAKE-KBD-MACRO-MOVER
						    ':MACRO-ERROR))))))))
    (AND (EQ (*CATCH 'EXIT-MAKE-KBD-MACRO-MOVER
	       (FUNCALL-SELF ':EDIT))
	     ':MACRO-ERROR)
	 (*THROW 'ZWEI-COMMAND-LOOP T)))
  (COND ((NOT (BOUNDP '*MAKE-KBD-MACRO-MOVER-COMTAB*))
	 (SETQ *MAKE-KBD-MACRO-MOVER-COMTAB* (CREATE-SPARSE-COMTAB))
	 (SETF (COMTAB-KEYBOARD-ARRAY *MAKE-KBD-MACRO-MOVER-COMTAB*)
	       '((-1 . COM-EXIT-KBD-MACRO-MOVER)))))
  (SET-COMTAB-INDIRECTION *MAKE-KBD-MACRO-MOVER-COMTAB* *COMTAB*)
  (LET-CLOSED ((OLD-MACRO-PREVIOUS-ARRAY (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY))
	       (STANDARD-INPUT (LET-CLOSED ((OLD-STANDARD-INPUT STANDARD-INPUT))
				 #'(LAMBDA (OP &REST REST)
				     (PROG1 (LEXPR-FUNCALL OLD-STANDARD-INPUT OP REST)
					    (COND ((EQ OP ':MACRO-ERROR)
						   (*THROW 'EXIT-KBD-MACRO-MOVER
							   ':MACRO-ERROR))))))))
    (ARRAY-PUSH-EXTEND OLD-MACRO-PREVIOUS-ARRAY -1)
    (SETF (MACRO-LENGTH OLD-MACRO-PREVIOUS-ARRAY)
	  (1- (MACRO-POSITION OLD-MACRO-PREVIOUS-ARRAY)))
    #'(LAMBDA (BP &AUX (POINT (POINT)) OLD-POINT)
	(SETQ OLD-POINT (COPY-BP POINT ':NORMAL))
	(MOVE-BP (POINT) BP)
	(UNWIND-PROTECT
	  (LET ((*COMTAB* *MAKE-KBD-MACRO-MOVER-COMTAB*))
	    (FUNCALL STANDARD-INPUT ':MACRO-EXECUTE OLD-MACRO-PREVIOUS-ARRAY 1)
	    (AND (EQ (*CATCH 'EXIT-KBD-MACRO-MOVER
		       (FUNCALL-SELF ':EDIT))
		     ':MACRO-ERROR)
		 (*THROW 'ZWEI-COMMAND-LOOP T))
	    (COPY-BP POINT))
	  (MOVE-BP (POINT) OLD-POINT)
	  (FLUSH-BP OLD-POINT)))))

(DEFUN COM-EXIT-KBD-MACRO-MOVER ()
  (*THROW 'EXIT-KBD-MACRO-MOVER T))

(DEFCOM COM-SORT-VIA-KEYBOARD-MACROS "Sort the region alphabetically.
Keyboard macros are read to move to the various part of the region to be sorted." ()
  (REGION (BP1 BP2)
    (WITH-BP (FIRST-BP BP1 ':NORMAL)
      (WITH-BP (LAST-BP BP2 ':MOVES)
	(SETF (WINDOW-MARK-P *WINDOW*) NIL)
	(MOVE-BP (POINT) FIRST-BP)
	(MUST-REDISPLAY *WINDOW* DIS-BPS)
	(LET ((MOVE-TO-KEY-MACRO (MAKE-KBD-MACRO-MOVER "move to the start of the sort key"))
	      (MOVE-OVER-KEY-MACRO (MAKE-KBD-MACRO-MOVER "move over the sort key"))
	      (MOVE-TO-NEXT-MACRO (MAKE-KBD-MACRO-MOVER "move to the end of the record")))
	  (SORT-INTERVAL-FUNCTIONS MOVE-TO-KEY-MACRO MOVE-OVER-KEY-MACRO MOVE-TO-NEXT-MACRO
				   #'INTERVAL-WITH-SORT-INTERVAL-LESSP FIRST-BP LAST-BP T)))))
  DIS-TEXT)
