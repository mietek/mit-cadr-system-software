;;; -*- Mode:Lisp; Package:ZWEI -*- ** (c) Copyright 1980 Massachusetts
;;; Institute of Technology **

;;; Functions to implement major and minor modes.

;;; NOTE: self-doc in EMACS mode, and recursive control-R for wordab still need
;;; to be done.  NOTE: The worthless TECO mode command have not yet been converted.

;;; Two specials, bound at every ZWEI invocation, are used to keep track of
;;; modes.  *MODE-LIST* is a list, each of whose elements speaks of a mode that
;;; is in effect.  The most recent mode entered is at the head of the list, and
;;; the least recent at the end.  Modes are turned on and off in a stack-like
;;; fashion, to prevent a bug in which keys end up redefined even after all
;;; modes are turned off.

;;; Each mode is known by a symbol, such as AUTO-FILL-MODE.  The symbol has a
;;; MODE property, the value of which is a list of Lisp forms.  These forms are
;;; very restricted; they must be SETQ, ASET, SET-COMTAB, PUSH, SET-CHAR-SYNTAX,
;;; SET-SYNTAX-TABLE-INDIRECTION or COMMAND-HOOK forms.  They are interpreted
;;; by ZWEI rather than Lisp, because ZWEI figures out how to undo them, and
;;; generates a list of forms to undo their effects.  An element of the
;;; *MODE-LIST* consists of a 2-list of the mode symbol, and the ZWEI-generated
;;; list of undoing forms.

;;; The symbol *MAJOR-MODE* is the symbol for the major mode currently in
;;; effect, if any (e.g. LISP-MODE).  When a new major mode is entered, the old
;;; one is exited automatically.

;;; The value of a mode symbol is a string used to display that mode, and it
;;; has a MAJOR-MODE-P property which is T if it is a major-mode symbol.

;;; Modes are defined with DEFMAJOR or DEFMINOR.  The syntax is the same:
;;; (DEFMAJOR <command-name> <mode-symbol> <mode-name> <command-documentation>
;;; <command-options>     . <body>) (DEFMINOR <command-name> <mode-symbol>
;;; <mode-name> <mode-line-position> <command-documentation> <command-options>
;;; . <body>) The <mode-name> is the value of the <mode-symbol>.  The
;;; <mode-line-position> is a fixnum that determines where the name goes on the
;;; mode line, it is stored on the MODE-LINE-POSITION property of the symbol.
;;; The other three things are used to form a DEFCOM for a command to enter or
;;; exit the mode (exiting happens when the argument is zero).  The reason that
;;; the <command-name> comes first, and isn't next to the
;;; <command-documentation>, etc., is so that M-. can find the command.  The
;;; <body> should consist of only the restricted set of forms mentioned above.
;;; If you need to use other forms, change the function below to know how to
;;; produce an undo-form.

;;; Macro to define a major mode.
(DEFMACRO DEFMAJOR (COMMAND-NAME MODE-SYMBOL MODE-NAME
		    COMMAND-DOCUMENTATION COMMAND-OPTIONS . BODY)
    (DEFINE-MODE-MACRO T MODE-SYMBOL MODE-NAME 0
		       COMMAND-NAME COMMAND-DOCUMENTATION COMMAND-OPTIONS BODY))

;;; Macro to define a minor mode.
(DEFMACRO DEFMINOR (COMMAND-NAME MODE-SYMBOL MODE-NAME MODE-LINE-POSITION
		    COMMAND-DOCUMENTATION COMMAND-OPTIONS . BODY)
    (DEFINE-MODE-MACRO NIL MODE-SYMBOL MODE-NAME MODE-LINE-POSITION
		       COMMAND-NAME COMMAND-DOCUMENTATION COMMAND-OPTIONS BODY))

;;; Internal function to generate code for a major or minor mode.
(EVAL-WHEN (LOAD COMPILE EVAL)
(DEFUN DEFINE-MODE-MACRO (MAJOR-P MODE-SYMBOL MODE-NAME MODE-LINE-POSITION
				  COMMAND-NAME COMMAND-DOCUMENTATION COMMAND-OPTIONS BODY)
    `(PROGN 'COMPILE
	    (PUTPROP ',MODE-SYMBOL ',BODY 'MODE)
	    (PUTPROP ',MODE-SYMBOL ',MAJOR-P 'MAJOR-MODE-P)
	    (PUTPROP ',MODE-SYMBOL ,MODE-LINE-POSITION 'MODE-LINE-POSITION)
	    (AND ',MAJOR-P (PUTPROP ',MODE-SYMBOL
				    ',(INTERN (STRING-APPEND MODE-SYMBOL "-HOOK") "ZWEI")
				    'MODE-HOOK-SYMBOL))
	    (SETQ ,MODE-SYMBOL ,(COND ((ZEROP (STRING-LENGTH MODE-NAME)) NIL)
				      (MAJOR-P MODE-NAME)
				      (T (STRING-APPEND #\SP MODE-NAME))))
	    (DEFCOM ,COMMAND-NAME ,COMMAND-DOCUMENTATION ,COMMAND-OPTIONS
		,(IF MAJOR-P
		     `(PROGN (TURN-OFF-MODE *MAJOR-MODE*)
                             (DOLIST (MODE *UNSTICKY-MINOR-MODES*)
			       (TURN-OFF-MODE MODE))
			     (TURN-ON-MODE ',MODE-SYMBOL))
		     `(IF (IF *NUMERIC-ARG-P* (ZEROP *NUMERIC-ARG*)
					      (ASSQ ',MODE-SYMBOL *MODE-LIST*))
			  (TURN-OFF-MODE ',MODE-SYMBOL)
			  (TURN-ON-MODE ',MODE-SYMBOL)))
		DIS-NONE)))
)

;;; Turn off the mode.  If it is not on, do nothing.
(DEFUN TURN-OFF-MODE (MODE-SYMBOL)
    (LET ((MODE-ELEMENT (ASSQ MODE-SYMBOL *MODE-LIST*)))
      (COND ((NOT (NULL MODE-ELEMENT))
	     ;; We must first turn off all more-recently added modes,
	     ;; then turn off this mode, and then turn the more-recently added
	     ;; modes on in reverse order.  To accomplish the last, backward step
	     ;; more elegantly, we use a recursive subfunction.
	     (TURN-OFF-MODE-UNDERSCORE *MODE-LIST* MODE-ELEMENT))))
    (SETQ *MODE-NAME-LIST* (DELQ MODE-SYMBOL *MODE-NAME-LIST*)))

;;; Internal recursive subfunction of TURN-OFF-MODE.
(DEFUN TURN-OFF-MODE-UNDERSCORE (LIST ELEMENT)
    (COND ((EQ (CAR LIST) ELEMENT)
	   (MAPC #'EVAL (SECOND ELEMENT))
	   (SETQ *MODE-LIST* (DELQ ELEMENT *MODE-LIST*)))
	  (T
	   (MAPC #'EVAL (SECOND (CAR LIST)))
	   (TURN-OFF-MODE-UNDERSCORE (CDR LIST) ELEMENT)
	   (SETF (SECOND (CAR LIST)) (EVALUATE-FORMING-UNDO-LIST (GET (CAAR LIST) 'MODE))))))

;;; Turn on the mode.  If it is already on, do nothing.
(DEFUN TURN-ON-MODE (MODE-SYMBOL)
    (COND ((NULL (ASSQ MODE-SYMBOL *MODE-LIST*))
	   (COND ((GET MODE-SYMBOL 'MAJOR-MODE-P)
		  (SETQ *MAJOR-MODE* MODE-SYMBOL)))
	   (PUSH (LIST MODE-SYMBOL (EVALUATE-FORMING-UNDO-LIST (GET MODE-SYMBOL 'MODE)))
		 *MODE-LIST*)))
    (COND ((NULL (MEMQ MODE-SYMBOL *MODE-NAME-LIST*))
	   (PUSH MODE-SYMBOL *MODE-NAME-LIST*)))
    (LET ((HOOK (GET MODE-SYMBOL 'MODE-HOOK-SYMBOL)))
      (AND HOOK (BOUNDP HOOK) (FUNCALL (SYMEVAL HOOK))))
    (SORT *MODE-NAME-LIST* #'(LAMBDA (X Y) (< (GET X 'MODE-LINE-POSITION)
					      (GET Y 'MODE-LINE-POSITION)))))

;;; Take a list of forms, evaluate them, and produce a list of forms which,
;;; when evaluated, will undo the effects of the evaluatation of the original list.
;;; These forms are very restricted; they must be SETQ, ASET, SET-COMTAB,
;;; PUSH, SET-CHAR-SYNTAX, SET-SYNTAX-TABLE-INDIRECTION or COMMAND-HOOK forms.
(DEFUN EVALUATE-FORMING-UNDO-LIST (FORM-LIST)
    (LET ((RESULT NIL))
      (DOLIST (FORM FORM-LIST)
         (SELECTQ (CAR FORM)
            (SETQ
             (PUSH `(SETQ ,(SECOND FORM) ',(SYMEVAL (SECOND FORM))) RESULT)
             (EVAL FORM))
            (ASET
             (PUSH `(ASET ',(EVAL `(AREF . ,(CDDR FORM))) . ,(CDDR FORM)) RESULT)
             (EVAL FORM))
            (SET-COMTAB
             ;; Knowledge of how to reverse SET-COMTAB is kept in the COMTAB > file.
             (PUSH (MAKE-SET-COMTAB-UNDO-LIST FORM) RESULT)
             (EVAL FORM))
            (PUSH
             (LET ((THING (EVAL (SECOND FORM))))
               (PUSH `(SETF ,(THIRD FORM)
                             (DELQ ',THING ,(THIRD FORM)))
                      RESULT)
               (EVAL `(PUSH ',THING ,(THIRD FORM)))))
            (COMMAND-HOOK
             (LET ((THING (EVAL (SECOND FORM))))
               (PUSH `(SETF ,(THIRD FORM)
                             (DELQ ',THING ,(THIRD FORM)))
                      RESULT)
               (COMMAND-HOOK THING (THIRD FORM))))
	    (SET-CHAR-SYNTAX
	     (LET ((SYNTAX-TABLE (SYMEVAL (THIRD FORM)))
		   (CHAR (FOURTH FORM)))
	       (PUSH `(SET-CHAR-SYNTAX ,(CHAR-SYNTAX CHAR SYNTAX-TABLE) ',SYNTAX-TABLE ,CHAR)
		     RESULT))
	     (EVAL FORM))
	    (SET-SYNTAX-TABLE-INDIRECTION
	     (LET ((OF (SYMEVAL (SECOND FORM)))
		   (TO (SYMEVAL (THIRD FORM))))
	       (PUSH `(RPLACA ',OF ',(CAR OF)) RESULT)
	       (RPLACA OF TO)))
	    (PROGN
	     (EVAL FORM))
            (OTHERWISE
             (FERROR NIL "The form ~S cannot be used in a mode, because I can't invert it."
                     FORM))))
      RESULT))

;;; Turn off all modes.  For ZMACS.
(DEFUN UN-SET-MODES ()
    (DOLIST (L *MODE-LIST*)
	(MAPC #'EVAL (SECOND L)))
    (SETQ *MODE-NAME-LIST* NIL
          *MODE-LIST* NIL))

;;; Turn on a saved set of modes.  For ZMACS.
(DEFUN SET-MODES (MODE-LIST MAJOR-MODE)
    (SET-MODES-UNDERSCORE MODE-LIST)
    (SETQ *MAJOR-MODE* MAJOR-MODE)
    (TURN-ON-MODE *MAJOR-MODE*))

(DEFUN SET-MODES-UNDERSCORE (LIST)
    (COND ((NULL LIST) NIL)
	  (T
	   (SET-MODES-UNDERSCORE (CDR LIST))
    	   (PUSH (LIST (CAAR LIST) (EVALUATE-FORMING-UNDO-LIST (GET (CAAR LIST) 'MODE)))
		 *MODE-LIST*)
           (SETQ *MODE-NAME-LIST* (NCONC *MODE-NAME-LIST* (NCONS (CAAR LIST)))))))

(DEFUN NAME-OF-MAJOR-MODE ()
    (SYMEVAL *MAJOR-MODE*))

(DEFUN GET-FILE-MAJOR-MODE (FILE-SYMBOL &AUX MODE-PROP MODE)
  (AND (SETQ MODE-PROP (GET FILE-SYMBOL ':MODE))
       (SYMBOLP MODE-PROP)
       (SETQ MODE (INTERN-SOFT (STRING-APPEND (GET-PNAME MODE-PROP) "-MODE") "ZWEI"))
       (BOUNDP MODE)
       (STRING-EQUAL (SYMEVAL MODE) (GET-PNAME MODE-PROP))
       MODE))

(DEFUN STICKY-MODE-LIST ()
  (AND (BOUNDP '*MODE-LIST*)			;Somewhat of a kludge
       (DO ((MODES *MODE-LIST* (CDR MODES))
	    (NMODES NIL))
	   ((NULL MODES)
	    (NREVERSE NMODES))
	 (AND (MEMQ (CAAR MODES) *STICKY-MINOR-MODES*)
	      (PUSH (CAR MODES) NMODES)))))

;;; Major modes.

(DEFMAJOR COM-LISP-MODE LISP-MODE "LISP"
          "Sets things up for editing Lisp code.
Puts Indent-For-Lisp on Tab." ()
    (SETQ *SPACE-INDENT-FLAG* T)
    (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
    (SETQ *COMMENT-START* 'LISP-FIND-COMMENT-START-AND-END)
    (SET-COMTAB *MODE-COMTAB* '(#\TAB COM-INDENT-FOR-LISP
				#\RUBOUT COM-TAB-HACKING-RUBOUT
				#\RUBOUT COM-RUBOUT
				#/Z COM-COMPILE-AND-EXIT
				#/Z COM-EVALUATE-AND-EXIT)))

(DEFPROP LISP-MODE T ALL-UPPERCASE)

(DEFMAJOR COM-MIDAS-MODE MIDAS-MODE "MIDAS"
          "Sets things up for editing assembly language code." ()
    (SETQ *COMMENT-COLUMN* 400)
    (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
    (SET-COMTAB *MODE-COMTAB* '(#\TAB COM-INSERT-TAB
				#/A COM-GO-TO-AC-FIELD
				#/E COM-GO-TO-ADDRESS-FIELD
				#/D COM-KILL-TERMINATED-WORD
				#/N COM-GO-TO-NEXT-LABEL
				#/P COM-GO-TO-PREVIOUS-LABEL)))

(DEFPROP MIDAS-MODE T ALL-UPPERCASE)

(DEFCOM COM-KILL-TERMINATED-WORD "Kill a word and the following character.
If the word is followed by a CRLF, the CRLF is not killed." ()
  (LET ((BP (OR (FORWARD-WORD (POINT)) (BARF))))
    (OR (= (BP-CH-CHAR BP) #\CR) (SETQ BP (FORWARD-CHAR BP 1 T)))
    (KILL-INTERVAL-ARG (POINT) BP 1))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-TEXT)

(DEFCOM COM-GO-TO-PREVIOUS-LABEL "Put point after last label." ()
  (LET ((*NUMERIC-ARG* (- *NUMERIC-ARG*)))
    (COM-GO-TO-NEXT-LABEL)))

(DEFCOM COM-GO-TO-NEXT-LABEL "Put point after the last label." ()
  (LET ((ARG (ABS *NUMERIC-ARG*))
	(SIGN (IF (MINUSP *NUMERIC-ARG*) -1 1))
	(POINT (POINT)))
    (DO ((I 0 (1+ I))
	 (BP (BEG-LINE POINT)))
	(NIL)
      (DO NIL (NIL)
	(OR (MEMQ (BP-CH-CHAR BP) '(#/* #\SP #\TAB #\CR))
	    (STRING-EQUAL (BP-LINE BP) *COMMENT-START* 0 0 (STRING-LENGTH *COMMENT-START*))
	    (RETURN NIL))
	(OR (SETQ BP (BEG-LINE BP SIGN)) (BARF)))
      (COND (( I ARG)
	     (LET ((LINE (BP-LINE BP)))
	       (MOVE-BP BP LINE
			(OR (STRING-SEARCH-SET *BLANKS* LINE) (LINE-LENGTH LINE))))
	     (COND ((IF (MINUSP SIGN) (BP-< BP POINT) (BP-< POINT BP))
		    (MOVE-BP POINT BP)
		    (RETURN NIL)))))))
  DIS-BPS)

(DEFCOM COM-GO-TO-ADDRESS-FIELD "Put point before the address field." ()
  (GO-TO-ADDRESS-OR-AC-FIELD-INTERNAL T))

(DEFCOM COM-GO-TO-AC-FIELD "Put point before the accumulator field." ()
  (GO-TO-ADDRESS-OR-AC-FIELD-INTERNAL NIL))

(DEFUN GO-TO-ADDRESS-OR-AC-FIELD-INTERNAL (ADDRESS-P &AUX LINE BP)
  (SETQ LINE (BP-LINE (POINT))
	BP (OR (FORWARD-WORD (BEG-LINE (POINT))) (BARF)))
  (OR (MEMQ (BP-CH-CHAR BP) '(#/: #/= #/_))
      (SETQ BP (BEG-LINE BP)))
  (SETQ BP (OR (FORWARD-TO-WORD BP) (BARF)))
  (MOVE-BP BP LINE (OR (STRING-SEARCH-SET *BLANKS* LINE (BP-INDEX BP))
		       (LINE-LENGTH LINE)))
  (LET ((BP1 (FORWARD-OVER *BLANKS* BP)))
    (OR (= (BP-CH-CHAR BP1) #/;)
	(SETQ BP BP1)))
  (COND ((MEMQ (LDB %%CH-CHAR (BP-CHAR-BEFORE BP)) *BLANKS*)
	 (AND ADDRESS-P
	      (LET ((I (STRING-SEARCH-SET '(#\SP #/, #/; #//) LINE (BP-INDEX BP))))
		(AND I (CHAR-EQUAL (AREF LINE I) #/,)
		     (MOVE-BP BP LINE (1+ I)))))
	 (MOVE-BP (POINT) BP)
	 DIS-BPS)
	(T
	 (MOVE-BP (POINT) (INSERT BP " "))
	 DIS-TEXT)))

(DEFMAJOR COM-TEXT-MODE TEXT-MODE "Text"
          "Sets things up for editing English text.
Puts Tab-To-Tab-Stop on Tab." ()
    (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
    (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/')
    (SET-CHAR-SYNTAX WORD-DELIMITER *MODE-WORD-SYNTAX-TABLE* #/.)
    (SET-COMTAB *MODE-COMTAB* '(#\TAB COM-TAB-TO-TAB-STOP))
    (SETQ *COMMENT-START* NIL))

(DEFMAJOR COM-BOLIO-MODE BOLIO-MODE "Bolio"
          "Sets things up for editing Bolio source files.
Like Text mode, but also makes c-m-digit and c-m-: and c-m-* do font stuff,
and makes word-abbrevs for znil and zt." ()
    (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
    (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/')
    (SET-CHAR-SYNTAX WORD-DELIMITER *MODE-WORD-SYNTAX-TABLE* #/.)
    (SET-COMTAB *MODE-COMTAB* '(#\TAB COM-TAB-TO-TAB-STOP
				;;Next line gets an error, so do it manually
				;;(#/0 10.) COM-BOLIO-INTO-FONT
				#/0 COM-BOLIO-INTO-FONT
				#/1 COM-BOLIO-INTO-FONT
				#/2 COM-BOLIO-INTO-FONT
				#/3 COM-BOLIO-INTO-FONT
				#/4 COM-BOLIO-INTO-FONT
				#/5 COM-BOLIO-INTO-FONT
				#/6 COM-BOLIO-INTO-FONT
				#/7 COM-BOLIO-INTO-FONT
				#/8 COM-BOLIO-INTO-FONT
				#/9 COM-BOLIO-INTO-FONT
				#/: COM-BOLIO-OUTOF-FONT
				#/* COM-BOLIO-OUTOF-FONT
				#\SP COM-EXPAND-ONLY))
    (SETQ *COMMENT-START* ".c ")
    (SETQ *COMMENT-BEGIN* ".c ")
    (SETQ *COMMENT-COLUMN* 0)
    (PROGN (TURN-ON-MODE 'WORD-ABBREV-MODE)
	   ;; Set up BOLIO-mode-dependent word abbrevs
	   (PUTPROP (INTERN "ZNIL" *UTILITY-PACKAGE*)	;This stuff loses at top level since
		    "3nil*"			;*UTILITY-PACKAGE* not set up at readin time.
		    '|Bolio-ABBREV|)
	   (PUTPROP (INTERN "ZT" *UTILITY-PACKAGE*)
		    "3t*"
		    '|Bolio-ABBREV|)))

(DEFCOM COM-BOLIO-INTO-FONT "Insert font-change sequence" (NM)
    (LET ((CHAR (LDB %%CH-CHAR *LAST-COMMAND-CHAR*))
	  (POINT (POINT)))
       (LET ((LINE (BP-LINE POINT)) (INDEX (BP-INDEX POINT)))
	 (INSERT-MOVING POINT #/)
	 (INSERT-MOVING POINT CHAR)
	 (MVRETURN DIS-LINE LINE INDEX))))

(DEFCOM COM-BOLIO-OUTOF-FONT "Insert font-change sequence" (NM)
    (LET ((POINT (POINT)))
       (LET ((LINE (BP-LINE POINT)) (INDEX (BP-INDEX POINT)))
	 (INSERT-MOVING POINT #/)
	 (INSERT-MOVING POINT #/*)
	 (MVRETURN DIS-LINE LINE INDEX))))

(DEFMAJOR COM-FUNDAMENTAL-MODE FUNDAMENTAL-MODE "Fundamental"
          "Return to ZWEI's fundamental mode." ())

(DEFMAJOR COM-PL1-MODE PL1-MODE "PL1"
          "Set things up for editing PL1 programs.
Makes comment delimiters //* and *//, Tab is Indent-For-PL1,
Control-Meta-H is Roll-Back-PL1-Indentation, and Control- (Top-D)
is PL1dcl.  Underscore is made alphabetic for word commands." ()
    (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
    (SET-COMTAB *MODE-COMTAB*
     '(#\TAB COM-INDENT-FOR-PL1
       #/H COM-ROLL-BACK-PL1-INDENTATION
       #/ COM-PL1DCL
       ))
    (SETQ *SPACE-INDENT-FLAG* T)
    (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
    (SETQ *COMMENT-START* "//*")
    (SETQ *COMMENT-BEGIN* "//* ")
    (SETQ *COMMENT-END* "*//")
    (SETQ *COMMENT-COLUMN* (* 60. 6)))

(DEFMAJOR COM-ELECTRIC-PL1-MODE ELECTRIC-PL1-MODE "Electric PL1!!"
          "REALLY set things up for editing PL1 programs!
Does everything PL1-Mode does:
Makes comment delimiters //* and *//, Tab is Indent-For-PL1,
Control-Meta-H is Roll-Back-PL1-Indentation, and Control- (Top-D)
is PL1dcl.  Underscore is made alphabetic for word commands.
In addition, ; is PL1-Electric-Semicolon, : is PL1-Electric-Colon,
# is Rubout, @ is Clear, \ is Quoted Insert." ()
    (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
    (PROGN (OR (BOUNDP 'PL1DCL) (READ-PL1DCL)))
    (SET-COMTAB *MODE-COMTAB*
     '(#\TAB COM-INDENT-FOR-PL1
       #/H COM-ROLL-BACK-PL1-INDENTATION
       #/ COM-PL1DCL
       #/; COM-PL1-ELECTRIC-SEMICOLON
       #/: COM-PL1-ELECTRIC-COLON
       #/# COM-RUBOUT
       #/@ COM-CLEAR
       #/\ COM-VARIOUS-QUANTITIES
     ))
    (SETQ *SPACE-INDENT-FLAG* T)
    (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
    (SETQ *COMMENT-START* "//*")
    (SETQ *COMMENT-BEGIN* "//* ")
    (SETQ *COMMENT-COLUMN* (* 60. 6))
    (SETQ *COMMENT-END* "*//"))

(DEFMAJOR COM-TECO-MODE TECO-MODE "TECO"
          "Set things up for editing (ugh) TECO.
Makes comment delimiters be !* and !. Tab is Indent-Nested,
Meta-' is Forward-Teco-Conditional, and Meta-/" is Backward-Teco-Conditional." ()
    (SET-COMTAB *MODE-COMTAB*
     '(#\TAB COM-INDENT-NESTED
       #/' COM-FORWARD-TECO-CONDITIONAL
       #/" COM-BACKWARD-TECO-CONDITIONAL
       ))
    (SETQ *SPACE-INDENT-FLAG* T)
    (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
    (SETQ *COMMENT-START* "!*")
    (SETQ *COMMENT-BEGIN* "!* ")
    (SETQ *COMMENT-END* "!"))

(DEFVAR *MACSYMA-LIST-SYNTAX-TABLE*)
(DEFVAR *MACSYMA-LIST-SYNTAX-LIST*)

(DEFMAJOR COM-MACSYMA-MODE MACSYMA-MODE "MACSYMA"
          "Enter a mode for editing Macsyma code.
Modifies the delimiter dispatch tables appropriately for Macsyma syntax,
makes comment delimiters //* and *//.  Tab is Indent-Relative." ()
    (SET-COMTAB *MODE-COMTAB*
     '(#\TAB COM-INDENT-NESTED))
    ;; Tab hacking rubout.
    (SETQ *SPACE-INDENT-FLAG* T)
    (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
    (SETQ *COMMENT-COLUMN* (* 40. 6))
    (SETQ *COMMENT-START* "//*")
    (SETQ *COMMENT-BEGIN* "//* ")
    (SETQ *COMMENT-END* "*//")
    (PROGN
     (OR (BOUNDP '*MACSYMA-LIST-SYNTAX-TABLE*)
         (SETQ *MACSYMA-LIST-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *MACSYMA-LIST-SYNTAX-LIST*))))
    (SETQ *LIST-SYNTAX-TABLE* *MACSYMA-LIST-SYNTAX-TABLE*)
    (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/?)
    ;; Also does something like make right bracket point at right paren?
    )

(SETQ *MACSYMA-LIST-SYNTAX-LIST*
    '(
      (40 LIST-ALPHABETIC)
      
      LIST-DELIMITER		;040 space
      LIST-DELIMITER		;041 ! ***
      LIST-DOUBLE-QUOTE         ;042 "     "
      LIST-DELIMITER		;043 # ***
      LIST-DELIMITER		;044 $ ***
      LIST-ALPHABETIC           ;045 %
      LIST-DELIMITER		;046 & ***
      LIST-SINGLE-QUOTE         ;047 '
      LIST-OPEN                 ;050 (
      LIST-CLOSE		;051 )
      LIST-DELIMITER		;052 * ***
      LIST-DELIMITER		;053 + ***
      LIST-DELIMITER		;054 , ***
      LIST-DELIMITER		;055 - ***
      LIST-DELIMITER		;056 . ***
      LIST-DELIMITER		;057 / ***
      (10. LIST-ALPHABETIC)			;DIGITS
      LIST-DELIMITER		;072 : ***
      LIST-DELIMITER		;073 ; ***
      LIST-DELIMITER		;074 < ***
      LIST-DELIMITER		;075 = ***
      LIST-DELIMITER		;076 > ***
      LIST-ALPHABETIC           ;077 ?
      LIST-DELIMITER		;100 @ ***
      (26. LIST-ALPHABETIC)			;LETTERS
      LIST-OPEN                 ;133 [ ***
      LIST-SLASH		;134 \ ***
      LIST-CLOSE		;135 ] ***
      LIST-DELIMITER		;136 ^ ***
      LIST-DELIMITER		;137 _ ***
      LIST-DELIMITER		;140 ` ***
      (26. LIST-ALPHABETIC)			;MORE LETTERS
      LIST-OPEN                 ;173 { ***
      LIST-DELIMITER		;174 | ***        |
      LIST-CLOSE		;175 } ***
      LIST-DELIMITER		;176 ~ ***
      LIST-ALPHABETIC           ;177 integral ???
      
      LIST-ALPHABETIC           ;200 null character
      LIST-DELIMITER		;201 break
      LIST-DELIMITER		;202 clear
      LIST-DELIMITER		;203 call
      LIST-DELIMITER		;204 escape (NOT altmode!)
      LIST-DELIMITER		;205 backnext
      LIST-DELIMITER		;206 help
      LIST-DELIMITER		;207 rubout
      LIST-ALPHABETIC           ;210 bs
      LIST-DELIMITER		;211 tab
      LIST-DELIMITER		;212 line
      LIST-DELIMITER		;213 vt
      LIST-DELIMITER		;214 form = newpage
      LIST-DELIMITER		;215 return = newline
      (162 LIST-ALPHABETIC)))

;;; Minor modes.

(DEFMINOR COM-ATOM-WORD-MODE ATOM-WORD-MODE "" 1
	  "Make word commands deal with lisp atoms.
With an argument of zero, exit Atom Word mode; otherwise enter it.
In Atom Word mode, all word commands act on Lisp atoms." ()
  (SET-SYNTAX-TABLE-INDIRECTION *MODE-WORD-SYNTAX-TABLE* *ATOM-WORD-SYNTAX-TABLE*))

(DEFMINOR COM-EMACS-MODE EMACS-MODE "Emacs" 1
	  "Minor mode to provide commands for EMACS users.
This is for people who have used EMACS from non-TV keyboards for a long
time and are not yet adjusted to the more winning commands.  It puts
bit prefix commands on Altmode, Control-^ and Control-C, and Universal
Argument on Control-U." ()
    (SET-COMTAB *MODE-COMTAB* '(#/^ COM-PREFIX-CONTROL
				#/ COM-PREFIX-META
				#/C COM-PREFIX-CONTROL-META
				#/U COM-UNIVERSAL-ARGUMENT
				#/I (0 #\TAB)
				#/H (0 #\BS))))

;;; Gets a single character from the user.  If HIGHBITSP is true, does not
;;; strip the control and meta bis.
(DEFUN GET-ECHO-CHAR (PROMPT HIGHBITSP &AUX CHAR)
  (TYPEIN-LINE "~A" PROMPT)
  (TYPEIN-LINE-ACTIVATE
    (SETQ CHAR (CHAR-UPCASE (LDB %%CH-CHAR (FUNCALL STANDARD-INPUT ':TYI)))))
  (OR HIGHBITSP (SETQ CHAR (LDB %%KBD-CHAR CHAR)))
  (TYPEIN-LINE-MORE "~:C" CHAR)
  CHAR)

(DEFCOM COM-PREFIX-CONTROL DOCUMENT-PREFIX-CHAR ()
   (KEY-EXECUTE (DPB 1 %%KBD-CONTROL (GET-ECHO-CHAR "Control-" NIL))
                *NUMERIC-ARG-P*
                *NUMERIC-ARG*))

(DEFCOM COM-PREFIX-META DOCUMENT-PREFIX-CHAR ()
        ()
   (KEY-EXECUTE (DPB 1 %%KBD-META (GET-ECHO-CHAR "Meta-" NIL))
                *NUMERIC-ARG-P*
                *NUMERIC-ARG*))

(DEFCOM COM-PREFIX-CONTROL-META DOCUMENT-PREFIX-CHAR ()
        ()
   (KEY-EXECUTE (DPB 1 %%KBD-CONTROL (DPB 1 %%KBD-META (GET-ECHO-CHAR "Control-Meta-" NIL)))
                *NUMERIC-ARG-P*
                *NUMERIC-ARG*))

(DEFUN DOCUMENT-PREFIX-CHAR (COMMAND IGNORE OP &AUX COLNUM)
  (SETQ COLNUM (CDR (ASSQ COMMAND '((COM-PREFIX-CONTROL . 1)
				    (COM-PREFIX-META . 2)
				    (COM-PREFIX-CONTROL-META . 3)))))
  (SELECTQ OP
    (:NAME (GET COMMAND 'COMMAND-NAME))
    (:SHORT (FORMAT T "Set the ~[Control~;Meta~;Control-Meta~] prefix." (1- COLNUM)))
    (:FULL (FORMAT T "Set the ~[Control~;Meta~;Control-Meta~] prefix.
Make the next character act as if it were typed with ~[CTRL~;META~;CTRL and META~]
held down, just as if you were on a losing terminal that doesn't
support all of the wonderful keys that we cleverly provide
on these marvelous keyboards.
Type a subcommand to document (or /"*/" for all): " (1- COLNUM) (1- COLNUM))
	   (LET ((CHAR (FUNCALL STANDARD-INPUT ':TYI)))
	     (COND ((= CHAR #/*)
		    (FORMAT T "~2%The following ~[Control~;Meta~;Control-Meta~]- commands are availible:~%" (1- COLNUM))
		    (LET ((N (DPB COLNUM %%KBD-CONTROL-META 0)))
		      (DO ((I N (1+ I))
			   (LIM (+ N 220)))
			  (( I LIM))
			(PRINT-SHORT-DOC-FOR-TABLE I *COMTAB* 3))))
		   (T (SETQ CHAR (DPB COLNUM %%KBD-CONTROL-META CHAR))
		      (FORMAT T "~:C~2%" CHAR)
		      (DOCUMENT-KEY CHAR *COMTAB*)))))))

(DEFCOM COM-UNIVERSAL-ARGUMENT "Sets argument or multiplies it by four.
Followed by digits, uses them to specify the
argument for the command after the digits.
Not followed by digits, multiplies the argument by four." ()
  (SETQ *NUMERIC-ARG-P* T)
  (DO ((FIRSTP T NIL)
       (MINUSP NIL)
       (DIGITP NIL)
       (NUM 1)
       (CHAR)
       )
      (NIL)
    (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI))
    (COND ((AND FIRSTP (= CHAR #/-))
	   (SETQ MINUSP T))
	  ((AND ( CHAR #/0) ( CHAR #/9))
	   (COND (DIGITP (SETQ NUM (+ (- CHAR 60) (* NUM 10.))))
		 (T (SETQ NUM (- CHAR 60))))
	   (SETQ DIGITP T))
	  (T
	   (COND ((OR MINUSP DIGITP)
		  (SETQ *NUMERIC-ARG* (IF MINUSP (MINUS NUM) NUM)))
		 (T (SETQ *NUMERIC-ARG* (* 4 *NUMERIC-ARG*))))
	   (FUNCALL STANDARD-INPUT ':UNTYI CHAR)
	   (RETURN ':ARGUMENT)))))

(DEFMINOR COM-AUTO-FILL-MODE AUTO-FILL-MODE "Fill" 2
          "Turn on auto filling.
An argument of 0 turns it off." ()
    (COMMAND-HOOK 'AUTO-FILL-HOOK *POST-COMMAND-HOOK*))

(DEFPROP AUTO-FILL-HOOK 20 COMMAND-HOOK-PRIORITY)
(DEFUN AUTO-FILL-HOOK (CHAR &AUX BP)
  (AND (MEMQ (LDB %%KBD-CHAR CHAR) '(#\SP #\CR))
       (NOT *NUMERIC-ARG-P*)
       (SETQ BP (DO ((SHEET (WINDOW-SHEET *WINDOW*))
		     (LINE (BP-LINE (POINT)))
		     (LEN (LINE-LENGTH (BP-LINE (POINT))))
		     (POS 0)
		     (CHAR-POS 0 CP)
		     (CP))
		    ((= CHAR-POS LEN) NIL)
		  (SETQ CP (OR (STRING-SEARCH-CHAR #\SP LINE (1+ CHAR-POS)) LEN)
			POS (+ POS (STRING-WIDTH LINE CHAR-POS CP SHEET)))
		  (AND (> POS *FILL-COLUMN*)
		       (RETURN (AND (> CHAR-POS 0) (CREATE-BP LINE CHAR-POS))))))
       (WITH-BP (PT (POINT) ':MOVES) ;Save point
	 (MOVE-BP (POINT) BP)
	 (LET ((LINE (BP-LINE BP))
	       (LINE2 NIL))
	   (COND ((OR (END-LINE-P (FORWARD-OVER *BLANKS* PT))
		      (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
		      (FIND-COMMENT-START LINE)
		      (> (+ (STRING-WIDTH LINE (BP-INDEX BP))
			    (STRING-WIDTH
			      (SETQ LINE2 (LINE-NEXT LINE))))
			 *FILL-COLUMN*))
		  (DELETE-INTERVAL BP (FORWARD-CHAR BP))
		  (LET ((AT-POINT-P (BP-= PT BP)))
		    (MUST-REDISPLAY *WINDOW* (COM-INDENT-NEW-COMMENT-LINE))
		    (AND AT-POINT-P (MOVE-BP PT (POINT)))))
		 (T
		  (LET ((IDX (1+ (BP-INDEX BP))))
		    (INSERT
		      (INSERT (FORWARD-OVER *BLANKS* (CREATE-BP LINE2 0))
			      (NSUBSTRING LINE IDX))
		      " ")
		    (AND (EQ (BP-LINE PT) (BP-LINE BP))
			 ( (SETQ IDX (- (BP-INDEX PT) IDX)) 0)
			 (MOVE-BP PT LINE2 IDX)))
		  (DELETE-INTERVAL BP (END-OF-LINE LINE))
		  (MUST-REDISPLAY *WINDOW* DIS-TEXT))))
	   (MOVE-BP (POINT) PT))))

(DEFPROP AUTO-FILL-HOOK DOCUMENT-AUTO-FILL-HOOK
	 HOOK-DOCUMENTATION-FUNCTION)

(DEFUN DOCUMENT-AUTO-FILL-HOOK (IGNORE CHAR)
    (AND (MEMQ (LDB %%KBD-CHAR CHAR) '(#\SP #\CR))
	 (PRINC "With no numeric argument, auto fill line if needed.
")))

(DEFMINOR COM-OVERWRITE-MODE OVERWRITE-MODE "Overwrite" 4
	  "Turn on overwrite mode.
An argument of 0 turns it off.
In overwrite mode, normal characters replace the character they are over,
instead of inserting." ()
     (SETQ *STANDARD-COMMAND* 'COM-SELF-OVERWRITE))

(DEFCOM COM-SELF-OVERWRITE "Replace the character at point with the character typed.
At the end of a line, inserts instead of replacing the newline." ()
    (LET ((PT (POINT)))
     (OR (= (BP-INDEX PT) (LINE-LENGTH (BP-LINE PT)))
	 (DELETE-INTERVAL PT (FORWARD-CHAR PT)))
     (INSERT-MOVING PT *LAST-COMMAND-CHAR*)
     (MVRETURN DIS-LINE (BP-LINE PT) (1- (BP-INDEX PT)))))

(COMMENT Word abbrev mode.)

(DEFUN INITIALIZE-WORD-ABBREV-TABLE ()
   (LET ((INIT  " ~@#;$%^&*()-_=+[]\/|:'`/"{},<.>//?!
212"))
     (SETQ *WORD-ABBREV-TABLE* (MAKE-ARRAY NIL 'ART-1B 400))
     (DO ((I 0 (1+ I))
	  (LIM (STRING-LENGTH INIT)))
	 (( I LIM))
       (ASET 1 *WORD-ABBREV-TABLE* (AREF INIT I)))))

(DEFCOM COM-EXPAND-ONLY "Expand last word, but insert nothing after it.
If given an argument, beep unless expanded." ()
    (AND (NULL (EXPAND-ABBREV)) *NUMERIC-ARG-P*
	 (BARF))
    DIS-TEXT)

(DEFPROP EXPAND-ABBREV-HOOK 10 COMMAND-HOOK-PRIORITY)
(DEFUN EXPAND-ABBREV-HOOK (IGNORE)
    (AND (EXPAND-P *LAST-COMMAND-CHAR*)
	 (NOT *NUMERIC-ARG-P*)
	 (EXPAND-ABBREV)
	 (MUST-REDISPLAY *WINDOW* DIS-TEXT)))

(DEFPROP EXPAND-ABBREV-HOOK DOCUMENT-EXPAND-ABBREV-ITEM
	 HOOK-DOCUMENTATION-FUNCTION)
(DEFUN DOCUMENT-EXPAND-ABBREV-ITEM (IGNORE CHAR)
    (AND (EXPAND-P CHAR)
	 (PRINC "With no numeric argument, expand preceeding word abbrev if any.
")))

;;; Does this character try to expand preceeding abbrev?
(DEFUN EXPAND-P (CHAR)
    (AND (ZEROP (LDB %%KBD-CONTROL-META CHAR))
	 (NOT (ZEROP (AREF *WORD-ABBREV-TABLE* (LDB %%KBD-CHAR CHAR))))))

(DEFUN EXPAND-ABBREV (&AUX BP STRING SYM TEM PROP)
    (AND (NOT (DELIMCHAR-P (BP-CHAR-BEFORE (POINT))))
	 (MULTIPLE-VALUE (STRING BP)
			 (BOUND-WORD (POINT))))
    (COND ((AND STRING
                (SETQ SYM (INTERN-SOFT (STRING-UPCASE STRING) *UTILITY-PACKAGE*))
		(SETQ TEM (OR (GET SYM (SETQ PROP (GET-ABBREV-MODE-NAME)))
			      (GET SYM (SETQ PROP '*-ABBREV)))))
	   (COND (*LAST-EXPANSION-BP*
		  (MOVE-BP *LAST-EXPANSION-BP* BP))
		 (T
		  (SETQ *LAST-EXPANSION-BP* (COPY-BP BP ':NORMAL))))
	   (COND ((AND (CHAR-EQUAL (BP-CHAR-BEFORE BP) #/-)
		       (BP-= (MOVE-BP BP (FORWARD-CHAR BP -1))
                             *WORD-ABBREV-PREFIX-MARK*))
		  (SETQ STRING (STRING-APPEND "-" STRING))
		  (DELETE-INTERVAL BP (FORWARD-CHAR BP) T))
                 (T
                  (SETQ STRING (STRING-APPEND STRING))))
	   (SETQ *LAST-EXPANDED* STRING
		 *LAST-EXPANSION* TEM
		 *LAST-EXPANSION-SYMBOL* SYM
		 *LAST-EXPANSION-USAGE-PROP* (GET-ABBREV-USAGE-NAME PROP))
	   (LET ((V (GET SYM *LAST-EXPANSION-USAGE-PROP*)))
	     (PUTPROP SYM (IF V (1+ V) 1) *LAST-EXPANSION-USAGE-PROP*))
           (MOVE-BP (POINT) (CASE-REPLACE *LAST-EXPANSION-BP* (POINT) TEM)))))

(DEFCOM COM-UNEXPAND-LAST-WORD "Undo last expansion, leaving the abbrev." ()
  (LET (BP TEM)
    (OR *LAST-EXPANSION* (BARF "No last expansion"))
    (SETQ BP (FORWARD-CHAR *LAST-EXPANSION-BP* (ARRAY-ACTIVE-LENGTH *LAST-EXPANSION*)))
    (OR (STRING-EQUAL (STRING-INTERVAL *LAST-EXPANSION-BP* BP T) *LAST-EXPANSION*)
	(BARF "No last expansion"))
    (SETQ TEM (BP-= BP (POINT))
          BP (INSERT (DELETE-INTERVAL *LAST-EXPANSION-BP* BP)
                     *LAST-EXPANDED*))
    (PUTPROP *LAST-EXPANSION-SYMBOL*
	     (1- (GET *LAST-EXPANSION-SYMBOL*
		      *LAST-EXPANSION-USAGE-PROP*))
	     *LAST-EXPANSION-USAGE-PROP*)
    (AND TEM (MOVE-BP (POINT) BP)))
  DIS-TEXT)

(DEFMINOR COM-WORD-ABBREV-MODE WORD-ABBREV-MODE "Abbrev" 3
          "Mode for expanding word abbrevs.
No arg or non-zero arg sets the mode, 0 arg clears it." ()
    (SET-COMTAB *MODE-COMTAB* '(#\SP COM-EXPAND-ONLY))
    (SET-COMTAB *STANDARD-CONTROL-X-COMTAB*
                '(#/U COM-UNEXPAND-LAST-WORD
                  #/A COM-ADD-MODE-WORD-ABBREV
                  #/+ COM-ADD-GLOBAL-WORD-ABBREV))
    (COMMAND-HOOK 'EXPAND-ABBREV-HOOK *COMMAND-HOOK*)
    (SETQ *LAST-EXPANSION-BP* NIL)
    (SETQ *LAST-EXPANDED* NIL)
    (SETQ *LAST-EXPANSION* NIL)
    (SETQ *WORD-ABBREV-PREFIX-MARK* NIL))

(DEFCOM COM-MAKE-WORD-ABBREV "Prompt for and make a new word abbrev.
An argument means make global abbrev, else local for this mode." ()
    (MAKE-WORD-ABBREV
     *NUMERIC-ARG-P*
     (TYPEIN-LINE-READLINE "Define ~:[~A mode~;global~*~] abbrev for: "
                           *NUMERIC-ARG-P* (NAME-OF-MAJOR-MODE))))

(DEFCOM COM-ADD-MODE-WORD-ABBREV "Read mode abbrev for words before point.
A negative arg means delete the word abbrev.  (If there is no such mode
abbrev, but there is a global, ask if should kill the global.)
Positive arg means expansion if last ARG words.
If there is a region, it is used instead." ()
    (COND ((MINUSP *NUMERIC-ARG*)
	   (LET ((*NUMERIC-ARG* (MINUS *NUMERIC-ARG*)))
		(COM-KILL-MODE-WORD-ABBREV)))
	  (T
	   (MAKE-WORD-ABBREV NIL))))

(DEFCOM COM-ADD-GLOBAL-WORD-ABBREV "Reads mode abbrev for words before point.
A negative arg means delete the word abbrev.
Positive arg means expansion if last ARG words.
If there is a region, it is used instead." ()
    (COND ((MINUSP *NUMERIC-ARG*)
	   (LET ((*NUMERIC-ARG* (MINUS *NUMERIC-ARG*)))
		(COM-KILL-GLOBAL-WORD-ABBREV)))
	  (T
	   (MAKE-WORD-ABBREV T))))

(DEFUN MAKE-WORD-ABBREV (GLOBAL-P &OPTIONAL STRING &AUX ABBREV)
  (OR STRING
      (SETQ STRING (COND ((WINDOW-MARK-P *WINDOW*)
			  (STRING-INTERVAL (MARK) (POINT)))
			 (T
			  (OR (BOUND-WORD (POINT) (ABS *NUMERIC-ARG*))
			      (BARF))))))

  (SETQ ABBREV (TEMP-KILL-RING STRING
		 (LET ((*COMMAND-HOOK* NIL))		;Don't expand abbrevs within
		   (TYPEIN-LINE-READLINE
		    (FORMAT NIL "~:[~A mode~;Global~*~] abbrev for /"~A/": "
			    GLOBAL-P (NAME-OF-MAJOR-MODE) STRING)))))
  (PUTPROP (INTERN (STRING-UPCASE ABBREV) *UTILITY-PACKAGE*) (STRING-APPEND STRING)
	   (COND (GLOBAL-P '*-ABBREV) (T (GET-ABBREV-MODE-NAME))))
  DIS-NONE)

(DEFCOM COM-KILL-MODE-WORD-ABBREV "Cause mode abbrev typed to be expunged." ()
    (KILL-ABBREV NIL))

(DEFCOM COM-KILL-GLOBAL-WORD-ABBREV "Cause global abbrev typed to be expunged." ()
    (KILL-ABBREV T))

(DEFUN KILL-ABBREV (GLOBAL-P &AUX STRING SYM MODE-NAME PROP)
    (SETQ STRING (TYPEIN-LINE-READLINE "Kill ~:[~A mode~;global~*~] abbrev: "
				       GLOBAL-P (NAME-OF-MAJOR-MODE)))
    (OR (SETQ SYM (INTERN-SOFT (STRING-UPCASE STRING) *UTILITY-PACKAGE*))
        (BARF "No such abbrev defined"))
    (COND (GLOBAL-P
           (OR (GET SYM '*-ABBREV) (BARF "No such global abbrev defined."))
           (REMPROP SYM '*-ABBREV)
	   (REMPROP SYM '*-ABBREV-USAGE))
          ((GET SYM (SETQ MODE-NAME (SETQ PROP (GET-ABBREV-MODE-NAME))))
           (REMPROP SYM MODE-NAME)
	   (REMPROP SYM (GET-ABBREV-USAGE-NAME PROP)))
          ((NOT (GET SYM '*-ABBREV))
           (BARF "No such abbrev defined."))
          ((Y-OR-N-P (FORMAT NIL "~A is not a ~A mode abbrev, but is a global one, kill it? "
			     STRING (NAME-OF-MAJOR-MODE)))
           (REMPROP SYM '*-ABBREV)
	   (REMPROP SYM '*-ABBREV-USAGE))
          (T
           (PROMPT-LINE "~&Not killed.")))
    DIS-NONE)

(DEFCOM COM-KILL-ALL-WORD-ABBREVS "No word abbrevs are defined after this." ()
    (MAPATOMS (FUNCTION KILL-ALL-ABBREVS-1) *UTILITY-PACKAGE* NIL)
    DIS-NONE)

(DEFUN KILL-ALL-ABBREVS-1 (SYM)
    (DO ((L (PLIST SYM) (CDDR L))
	 (IND)
	 (IND-NAME)
	 (LEN))
	((NULL L))
	(SETQ IND (CAR L)
	      IND-NAME (GET-PNAME IND)
	      LEN (STRING-LENGTH IND-NAME))
	(AND (> LEN 7)
	     (STRING-EQUAL (NSUBSTRING IND-NAME (- (STRING-LENGTH IND-NAME) 7))
			   "-ABBREV")
	     (REMPROP SYM IND)
	     (REMPROP SYM (GET-ABBREV-USAGE-NAME IND-NAME)))))

(DEFUN BOUND-WORD (BP &OPTIONAL (TIMES 1) &AUX BP1 STRING)
    (AND (SETQ BP (FORWARD-TO-WORD BP -1))
	 (SETQ BP1 (FORWARD-WORD BP (- TIMES)))
	 (SETQ STRING (STRING-INTERVAL BP BP1)))
    (MVRETURN STRING BP1))

(DEFCOM COM-WORD-ABBREV-PREFIX-MARK "Mark point as end of a prefix" ()
    (EXPAND-ABBREV)
    (COND (*WORD-ABBREV-PREFIX-MARK*
	   (MOVE-BP *WORD-ABBREV-PREFIX-MARK* (POINT)))
	  (T
	   (SETQ *WORD-ABBREV-PREFIX-MARK* (COPY-BP (POINT) ':NORMAL))))
    (INSERT-MOVING (POINT) "-")
    DIS-TEXT)

(DEFCOM COM-LIST-WORD-ABBREVS "List all abbrevs and their expansions." (X)
				 ()
    (FORMAT T "~%abbrev:   (mode)             count:     /"expansion/"~3%")
    (LIST-WORD-ABBREV-1 STANDARD-OUTPUT)
    DIS-NONE)

(DEFCOM COM-INSERT-WORD-ABBREVS "Insert all abbrevs and their expansions into the buffer."
                            (X) ()
    (LIST-WORD-ABBREV-1 (INTERVAL-STREAM *INTERVAL*))
    DIS-TEXT)

(LOCAL-DECLARE ((SPECIAL STREAM))
(DEFUN LIST-WORD-ABBREV-1 (STREAM)
    (MAPATOMS
     (FUNCTION
      (LAMBDA (SYM)
	 (DO ((L (PLIST SYM) (CDDR L))
	      (IND)
	      (IND-NAME)
	      (USAGE)
	      (LEN)
	      (STRING ""))
	     ((NULL L)
	      (FORMAT STREAM STRING))
           (SETQ IND (CAR L)
                 IND-NAME (GET-PNAME IND)
                 LEN (STRING-LENGTH IND-NAME)
                 USAGE (OR (GET SYM (GET-ABBREV-USAGE-NAME IND-NAME)) 0))
           (AND (> LEN 7)
                (STRING-EQUAL (NSUBSTRING IND-NAME (- LEN 7))
                              "-ABBREV")
                (SETQ STRING (FORMAT NIL
                                     "~A~10,4,2A~:[~15,4,2A~;~15X~*~] ~6D~6X /"~A/"~%"
                                     STRING
                                     (STRING-APPEND SYM ":")
                                     (EQ IND '*-ABBREV)
                                     (STRING-APPEND "("
                                                    (NSUBSTRING IND-NAME 0 (- LEN 7))
                                                    ")")
                                     USAGE
                                     (CADR L)))))))
     *UTILITY-PACKAGE*))
)

(DEFCOM COM-DEFINE-WORD-ABBREVS "Define word abbrevs from buffer" (X)
    (DO ((BP1 (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*)))
         (BP2)
         (MODE "*" "*")
	 (USAGE)
	 (SYM)
         (TEM))
        (())
        (OR (SETQ BP2 (SEARCH BP1 #/:)) (RETURN NIL))
        (SETQ TEM (STRING-UPCASE (STRING-INTERVAL BP1 (FORWARD-CHAR BP2 -1))))
        (SETQ SYM (INTERN TEM *UTILITY-PACKAGE*))
        (SETQ BP2 (FORWARD-OVER *BLANKS* (FORWARD-CHAR BP2)))
        (COND ((CHAR-EQUAL (BP-CHAR BP2) #/()
               (OR (SETQ BP1 (SEARCH (SETQ BP2 (FORWARD-CHAR BP2)) #/)))
                   (BARF "Unmatched paren ~A" (BP-LINE BP2)))
               (SETQ MODE (STRING-INTERVAL BP2 (FORWARD-CHAR BP1 -1)))
               (SETQ BP2 (PROG1 (FORWARD-OVER *BLANKS* (FORWARD-CHAR BP1))
                                (SETQ BP1 BP2)))))
	(MULTIPLE-VALUE (USAGE TEM)
	    (PARSE-NUMBER (BP-LINE BP2) (BP-INDEX BP2) 10.))
	(AND (= TEM (BP-INDEX BP2)) (BARF "No usage count ~A" (BP-LINE BP2)))
	(SETF (BP-INDEX BP2) TEM)
	(SETQ BP2 (FORWARD-OVER *BLANKS* BP2))
        (OR (CHAR-EQUAL (BP-CHAR BP2) #/")
            (BARF "No expansion ~A" (BP-LINE BP2)))
        (OR (SETQ BP1 (SEARCH (SETQ BP2 (FORWARD-CHAR BP2)) #/"))
            (BARF "Unmatched quote ~A" (BP-LINE BP2)))
	(PUTPROP SYM (STRING-APPEND (STRING-INTERVAL BP2 (FORWARD-CHAR BP1 -1)))
		 (GET-ABBREV-MODE-NAME MODE))
        (AND (EQ (BP-LINE BP1) (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
             (RETURN NIL))
        (MOVE-BP BP1 (BEG-LINE BP1 1)))
    DIS-NONE)

(DEFUN GET-ABBREV-MODE-NAME (&OPTIONAL (MODE (STRING (NAME-OF-MAJOR-MODE))))
  (INTERN (STRING-APPEND MODE "-ABBREV") "ZWEI"))

;;; Given the name of a X-ABBREV prop, return the usage count property.
(DEFUN GET-ABBREV-USAGE-NAME (STR)
  (INTERN (STRING-APPEND STR "-USAGE") "ZWEI"))

(DEFCOM COM-EDIT-WORD-ABBREVS "Enter recursive edit on the abbrev definitions." ()
  (LET ((INTERVAL (CREATE-INTERVAL NIL NIL T)))
    (LET ((*INTERVAL* INTERVAL))
      (COM-INSERT-WORD-ABBREVS))
    (RECURSIVE-EDIT INTERVAL "Edit Word Abbrevs")
    (COM-KILL-ALL-WORD-ABBREVS)
    (LET ((*INTERVAL* INTERVAL))
      (COM-DEFINE-WORD-ABBREVS)))
  DIS-ALL)

(DEFCOM COM-RECURSIVE-EDIT-BEEP "Exit from recursive edit without updating." ()
  (BEEP)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (*THROW 'TOP-LEVEL T))

(DEFVAR APROPOS-KEY)
(DEFVAR APROPOS-SEARCH-FUNCTION)

(DEFCOM COM-LIST-SOME-WORD-ABBREVS "List abbreviations or expansions with the given string" ()
  (MULTIPLE-VALUE-BIND (APROPOS-SEARCH-FUNCTION APROPOS-KEY)
      (GET-EXTENDED-SEARCH-STRINGS "Word abbrev apropos (substring:)")
    (MAPATOMS (FUNCTION WORD-ABBREV-APROPOS-INTERNAL) *UTILITY-PACKAGE* NIL))
  (FORMAT T "Done.~%")
  DIS-NONE)

(DEFUN WORD-ABBREV-APROPOS-INTERNAL (SYM)
  (DO ((L (PLIST SYM) (CDDR L))
       (IND)
       (IND-NAME)
       (LEN))
      ((NULL L))
    (SETQ IND (CAR L)
	  IND-NAME (GET-PNAME IND)
	  LEN (STRING-LENGTH IND-NAME))
    (AND (> LEN 7)
	 (STRING-EQUAL (NSUBSTRING IND-NAME (- LEN 7))
		       "-ABBREV")
	 (OR (FUNCALL APROPOS-SEARCH-FUNCTION APROPOS-KEY (GET-PNAME SYM))
	     (FUNCALL APROPOS-SEARCH-FUNCTION APROPOS-KEY (CADR L)))
	 (FORMAT T "~A:	~:[(~A)~;~*~]	/"~A/"~%"
		 SYM (EQ IND '*-ABBREV)
		 (NSUBSTRING IND-NAME 0 (- LEN 7)) (CADR L)))))

(DEFCOM COM-READ-WORD-ABBREV-FILE "Load up new format word abbrev file." ()
  (LET ((FNAME (READ-DEFAULTED-AUX-FILE-NAME "Load QWABL file:" ':QWABL)))
    (OPEN-FILE (STREAM  FNAME '(IN))
	       (LOAD-QWABL STREAM)))
  DIS-NONE)

(DEFUN LOAD-QWABL (STREAM)
  (FUNCALL STREAM ':LINE-IN)			;Flush some TECO macros
  (FUNCALL STREAM ':LINE-IN)
  (DO ((SYM)
       (USAGE)
       (MODE)
       (STR)
       (EOFP)
       (TEM)
       (TEM1))
      (())
    (MULTIPLE-VALUE (STR EOFP)
      (FUNCALL STREAM ':LINE-IN T))
    (AND EOFP (RETURN NIL))
    (OR (SETQ TEM (STRING-SEARCH-CHAR #\SP STR))
	(BARF "No abbrev ~S" STR))
    (SETQ TEM (1+ TEM))
    (OR (SETQ TEM1 (STRING-SEARCH-CHAR #\SP STR TEM))
	(BARF "No mode ~S" STR))
    (SETQ SYM (STRING-UPCASE (NSUBSTRING STR TEM TEM1))
	  TEM1 (1+ TEM1))
    (OR (SETQ TEM (STRING-SEARCH-CHAR #\SP STR TEM1))
	(BARF "No end of mode ~S" STR))
    (SETQ MODE (NSUBSTRING STR TEM1 TEM)
	  TEM (1+ TEM))
    (SETQ MODE (GET-ABBREV-MODE-NAME MODE))
    (OR (SETQ TEM1 (STRING-SEARCH-CHAR #/ STR TEM))
	(BARF "No expansion ~S" STR))
    (SETQ STR (NSUBSTRING STR (1+ TEM1)))
    (SETQ STR (DO ((EXPANSION "" (STRING-APPEND STR EXPANSION))
		   (STR STR (FUNCALL STREAM ':LINE-IN))
		   (POS))
		  (())
		(COND ((SETQ POS (STRING-SEARCH-CHAR #/ STR))
		       (SETQ USAGE (PARSE-NUMBER STR (1+ POS) 10.))
		       (SETQ STR (NSUBSTRING STR 0 POS))
		       (RETURN (STRING-APPEND STR EXPANSION))))))
    (SETQ SYM (INTERN SYM *UTILITY-PACKAGE*))
    (PUTPROP SYM STR MODE)
    (PUTPROP SYM USAGE (GET-ABBREV-USAGE-NAME MODE))
    ))

(LOCAL-DECLARE ((SPECIAL L))
(DEFUN WRITE-QWABL (STREAM &AUX L)
  (FUNCALL STREAM ':LINE-OUT "m.m& Make Usage Abbrev Variable[V")
  (FUNCALL STREAM ':LINE-OUT "q..q[..o")
  (MAPATOMS
    (FUNCTION (LAMBDA (SYM)
		(PUSH SYM L)))
    *UTILITY-PACKAGE*)
  (SETQ L (SORT L 'STRING-LESSP))
  (DO ((SL L (CDR SL))
       (SYM))
      ((NULL SL))
    (SETQ SYM (CAR SL))
    (DO ((LIST (PLIST SYM) (CDDR LIST))
	 (PROPNAME)
	 (LPROPNAME))
	((NULL LIST))
      (SETQ PROPNAME (CAR LIST) LPROPNAME (STRING-LENGTH PROPNAME))
      (COND ((STRING-EQUAL "-ABBREV" PROPNAME 0 (- LPROPNAME 7))
	     (FORMAT STREAM "MVX ~A ~A Abbrev~A~D~%"
		     (STRING-DOWNCASE SYM)
		     (NSUBSTRING PROPNAME 0 (- LPROPNAME 7))
		     (CADR LIST)
		     (OR (GET SYM (GET-ABBREV-USAGE-NAME PROPNAME)) 0)
		     ))))))
  )

(DEFCOM COM-WRITE-WORD-ABBREV-FILE "Write out all word abbrevs in QWABL format." ()
  (LET ((FN (READ-DEFAULTED-AUX-FILE-NAME "Write word abbrevs to:" ':QWABL)))
    (OPEN-FILE (STREAM FN '(WRITE) T)
	       (WRITE-QWABL STREAM)
	       (CLOSE STREAM)
	       (TYPEIN-LINE "Written: ~A" (FUNCALL STREAM ':GET ':UNIQUE-ID))))
  DIS-NONE)

(DEFMINOR COM-ELECTRIC-SHIFT-LOCK-MODE ELECTRIC-SHIFT-LOCK-MODE "Electric Shift-lock" 5
          "Uppercase things other than comments and strings" ()
  (COMMAND-HOOK 'SHIFT-LOCK-HOOK *COMMAND-HOOK*))

(DEFVAR *SHIFT-LOCK-HOOK-LAST-LINE* NIL)
(DEFVAR *SHIFT-LOCK-HOOK-DEFUN-BEGINNING* NIL)

(DEFPROP SHIFT-LOCK-HOOK 10 COMMAND-HOOK-PRIORITY)
(DEFUN SHIFT-LOCK-HOOK (CHAR &AUX STRING SLASH COMMENT
				  (POINT (POINT)) (*LISP-PARSE-PREPARSED-FLAG* T))
  (COND ((AND (OR (AND ( CHAR #/A) ( CHAR #/Z))
		  (AND ( CHAR #/a) ( CHAR #/z)))
	      (NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
	 (OR (AND (EQ *LAST-COMMAND-TYPE* 'SELF-INSERT)
		  (EQ (BP-LINE POINT) *SHIFT-LOCK-HOOK-LAST-LINE*))
	     (SETQ *SHIFT-LOCK-HOOK-DEFUN-BEGINNING* (FORWARD-DEFUN POINT -1 T)
		   *SHIFT-LOCK-HOOK-LAST-LINE* (BP-LINE POINT)
		   *LISP-PARSE-PREPARSED-FLAG* NIL))
	 (MULTIPLE-VALUE (STRING SLASH COMMENT)
	   (LISP-BP-SYNTACTIC-CONTEXT POINT *SHIFT-LOCK-HOOK-DEFUN-BEGINNING*))
	 (AND STRING (SETQ *SHIFT-LOCK-HOOK-LAST-LINE* NIL))
	 (OR STRING SLASH COMMENT
	     (SETQ *LAST-COMMAND-CHAR* (LOGXOR CHAR 40))))))

;It is useful to setq LISP-MODE-HOOK to this
(DEFUN ELECTRIC-SHIFT-LOCK-IF-APPROPRIATE ()
  (IF (OR (NOT (ZMACS-BUFFER-P *INTERVAL* BUFFER-FILE-GROUP-SYMBOL))
	  (GET (BUFFER-FILE-GROUP-SYMBOL *INTERVAL*) ':LOWERCASE))
      (TURN-OFF-MODE 'ELECTRIC-SHIFT-LOCK-MODE)
      (TURN-ON-MODE 'ELECTRIC-SHIFT-LOCK-MODE)))
