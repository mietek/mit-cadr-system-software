;;; Zwei commands, see ZWEI;COMA for comments -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCOM COM-FROB-LISP-CONDITIONAL "Change CONDs to ANDs or ORs and vice versa.
When changing to COND, point is left in such a place that LF will add another
clause to this condition, and M-) will add another condition.  Also in this case
an argument specifies the number of clauses that are left in the
consequent, the default is 1, i.e. all clauses but the last are assumed to
be for value, and to belong in the antecedent." ()
  (ATOM-WORD-SYNTAX-BIND
    (LET ((POINT (POINT))
	  FIXBP1 FIXBP2)
     (UNWIND-PROTECT
      (LET (COND-BP COND-TYPE UPCASE-P BP)
        (MULTIPLE-VALUE (COND-BP COND-TYPE)
	  (FIND-CONTAINING-ATOM POINT '(COND AND OR IF)))	;Locate the COND or AND or OR
        (OR COND-BP (BARF))
        (SETQ UPCASE-P (CHAR-UPPERCASE-P (BP-CHAR COND-BP)))    ;Remember if have to lowercase
        (LET ((START-DEFUN-BP (FORWARD-DEFUN POINT -1 T))
              (END-DEFUN-BP (FORWARD-DEFUN POINT 1 T))
              DEPTH)
	  ;; Parse it all once, then don't even bother checking.
	  (LISP-PARSE-FROM-DEFUN (BP-LINE END-DEFUN-BP) START-DEFUN-BP)
	  ;; Count how many levels down the next defun is from the start of this one.
	  (LET ((*LISP-PARSE-PREPARSED-FLAG* T))
	    (DO ((I -1 (1+ I))
		 (BP3 END-DEFUN-BP (FORWARD-SEXP BP3 -1 NIL 1 START-DEFUN-BP)))
		((NULL BP3) (SETQ DEPTH I))))
	  ;; Insert that many ")"'s just before point, so everything is balanced.
	  ;; These ")"'s lie between FIXBP1 and FIXBP2.  We use that to delete them later.
          (COND ((> DEPTH 0)
                 (LET ((BP (LIKELY-UNBALANCED-POINT (FORWARD-LIST COND-BP -1 NIL 1)
						    END-DEFUN-BP)))
		   (SETQ FIXBP1 (COPY-BP BP ':NORMAL)
			 FIXBP2 (COPY-BP BP ':MOVES)))
		 (INSERT FIXBP2 #\CR)
                 (DOTIMES (I DEPTH) (INSERT FIXBP2 #/)))
                 (INSERT FIXBP2 #\CR))))
        (COND ((EQ COND-TYPE 'COND)             ;Changing COND to AND or OR
               (LET ((N (COUNT-LIST-ELEMENTS (FORWARD-LIST COND-BP -1 NIL 1))))
		 (AND (> N 3) (BARF "Too many clauses"))
		 (AND (= N 3)
		      (LET ((BP1 (FORWARD-SEXP COND-BP 2)) BP2 BP3)
			(SETQ BP2 (FORWARD-LIST BP1 1 NIL -1 T)
			      BP3 (FORWARD-WORD BP2))
			(OR (AND (EQ (BP-LINE BP2) (BP-LINE BP3))
				 (STRING-EQUAL (BP-LINE BP2) "T" (BP-INDEX BP2) 0
					       (BP-INDEX BP3)))
			    (BARF "Too many clauses"))
			(SETQ BP1 (BACKWARD-OVER '(#\CR #\TAB #\SP) BP1))
			(SETQ BP1 (FORWARD-CHAR BP1 -1))
			(SETQ N (COUNT-LIST-ELEMENTS (FORWARD-SEXP COND-BP)))
			(DELETE-INTERVAL BP1 BP3 T)
			(SETQ COND-TYPE (IF (= N 1) "OR" "IF")))))
	       (DELETE-INTERVAL COND-BP (FORWARD-WORD COND-BP) T)
	       (AND (EQ COND-TYPE 'COND)	;Still not determined
		    ;; Check for (COND ((NOT ...)))
		    (LET ((BP1 (FORWARD-LIST COND-BP 1 NIL -2 T)))
		      (LET ((BP2 (FORWARD-WORD COND-BP 1 T)))
			(LET ((WORD (STRING-INTERVAL BP1 BP2)))
			  (COND ((OR (STRING-EQUAL WORD "NULL") (STRING-EQUAL WORD "NOT"))
				 (SETQ BP1 (FORWARD-LIST BP1 -1 NIL 1))
				 (LET ((BP3 (FORWARD-LIST BP1)))
				   (DELETE-INTERVAL (FORWARD-CHAR BP3 -1) BP3 T))
				 (DELETE-INTERVAL BP1 (FORWARD-OVER *BLANKS* BP2) T)
				 (SETQ COND-TYPE "OR"))
				(T
				 (SETQ COND-TYPE "AND")))))))
	       (SETQ BP (FORWARD-OVER *BLANKS* (INSERT COND-BP COND-TYPE)))
	       (LET ((BP1 (FORWARD-LIST BP)))	;Remove a level of parens
		 (DELETE-INTERVAL (FORWARD-CHAR BP1 -1) BP1 T))
	       (DELETE-INTERVAL BP (FORWARD-CHAR BP) T))
	      (T
	       (LET ((BP1 (FORWARD-LIST (FORWARD-LIST (FORWARD-CHAR COND-BP -1))
					-1 NIL -1 T)))
		 (INSERT BP1 #/))
		 (DO ((N -1 (1+ N))
		      (BP2 BP1 (FORWARD-SEXP BP2 -1))
		      (ARG (COND (*NUMERIC-ARG-P* (- 1 *NUMERIC-ARG*))
				 ((EQ COND-TYPE 'IF) -1)
				 (T 0))))
		     ((BP-= BP2 COND-BP)
		      (COND ((MINUSP (+ ARG N -3))
			     (DELETE-INTERVAL COND-BP (FORWARD-WORD COND-BP) T)
			     (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS*
						    (INSERT COND-BP "COND")))
			     (INSERT-MOVING BP #/()
			     (COND ((EQ COND-TYPE 'IF)
				    (SETQ BP (FORWARD-SEXP BP 2))
				    (INSERT-MOVING BP ")
 (T"))))
			    (T
			     (SETQ BP (INSERT COND-BP "COND (("))
			     (LET ((BP1 (IF (PLUSP ARG) (FORWARD-LIST BP 1 NIL 1)
					    (FORWARD-SEXP BP (+ ARG N)))))
			       (INSERT BP1 #/)))
			     (SETQ BP (FORWARD-CHAR BP -1))))
		      (COND ((EQ COND-TYPE 'OR)
			     (INSERT (FORWARD-SEXP BP) #/))
			     (INSERT-MOVING BP "(NOT "))))))))
        (OR UPCASE-P (DOWNCASE-INTERVAL COND-BP BP T))
        (MOVE-BP POINT (FORWARD-LIST BP -1 NIL (IF (MEMQ COND-TYPE '(IF OR)) 2 1)))
        (COM-INDENT-SEXP)                       ;Regrind changed stuff
        (MOVE-BP POINT (FORWARD-LIST (FORWARD-SEXP POINT) -1 NIL -1 T)))
      (COND (FIXBP1
	     (DELETE-INTERVAL FIXBP1 FIXBP2 T)
	     (FLUSH-BP FIXBP1)
	     (FLUSH-BP FIXBP2))))))
    DIS-TEXT)

;;; Find the containing member of set
(DEFUN FIND-CONTAINING-ATOM (BP SET)
  (DO ((BP BP)
       (BP1) (BP2) (TEM))
      (NIL)
    (OR (SETQ BP (FORWARD-LIST BP -1 NIL 1))
	(RETURN NIL))
    (SETQ BP1 (FORWARD-LIST BP 1 NIL -1 T)
	  BP2 (FORWARD-ATOM BP1))
    (AND (SETQ TEM (MEM #'STRING-EQUAL (STRING-INTERVAL BP1 BP2 T) SET))
	 (RETURN BP1 (CAR TEM)))))

(DEFUN COUNT-LIST-ELEMENTS (BP &AUX END-BP)
  (SETQ END-BP (FORWARD-SEXP BP))
  (DO ((BP (FORWARD-LIST BP 1 NIL -1 T) (FORWARD-SEXP BP))
       (I -1 (1+ I)))
      ((BP-= BP END-BP) I)))

;;; This tries to find someplace that looks like it probably doesn't have enough parens
;;; It takes the first place that has a lesser indentation level than the given BP.
(DEFUN LIKELY-UNBALANCED-POINT (BP LIMIT-BP)
  (DO ((IND (BP-INDENTATION BP))
       (LINE (LINE-NEXT (BP-LINE BP)) (LINE-NEXT LINE))
       (LIMIT-LINE (BP-LINE LIMIT-BP))
       (OLINE (BP-LINE BP)))
      ((EQ LINE LIMIT-LINE) LIMIT-BP)
    (COND ((NOT (MEMQ (LINE-TYPE LINE) '(:COMMENT :BLANK)))
	   (AND ( (LINE-INDENTATION LINE) IND)
		(RETURN (END-OF-LINE OLINE)))
	   (SETQ OLINE LINE)))))

(DEFCOM COM-FROB-DO "Interchange old and new style DO's" ()
  (ATOM-WORD-SYNTAX-BIND
   (LET (DO-BP DO-TYPE
	BP BP1 BP2 BP3)
    (MULTIPLE-VALUE (DO-BP DO-TYPE)
      (FIND-CONTAINING-ATOM (POINT) '(DO DOTIMES DOLIST)))
    (OR DO-BP (BARF))
    (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS* (FORWARD-WORD DO-BP)))
    (COND ((AND (EQ DO-TYPE 'DO)
		(= (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-OPEN))	;New style
	   (OR (= (COUNT-LIST-ELEMENTS BP) 1)
	       (BARF "Too many DO variables"))
	   (OR (SETQ BP1 (FORWARD-SEXP BP)) (BARF))
	   (OR (= (COUNT-LIST-ELEMENTS BP1) 1)
	       (BARF "Cannot have ending form"))
	   (OR (SETQ BP2 (FORWARD-SEXP BP1)) (BARF))
	   (SETQ BP3 (FORWARD-SEXP BP2 -1))
	   (DELETE-INTERVAL (FORWARD-LIST BP2 -1 NIL -1 T) BP2 T)
	   (MOVE-BP (POINT) (DELETE-INTERVAL (FORWARD-LIST BP1 -1 NIL -2 T)
					     (FORWARD-LIST BP3 1 NIL -1 T) T))
	   (INSERT-MOVING (POINT) #\SP)
	   (DELETE-INTERVAL BP (FORWARD-LIST BP 1 NIL -2 T) T))
	  (T					;Old style or special
	   (COND ((NEQ DO-TYPE 'DO)
		  (OR (SETQ BP1 (FORWARD-LIST BP 1 NIL -1 T)) (BARF))
		  (SETQ BP2 (FORWARD-SEXP BP1))
		  (LET ((VARNAME (STRING-INTERVAL BP1 BP2 T)))
		    (DELETE-INTERVAL BP BP1 T)
		    (COND ((EQ DO-TYPE 'DOTIMES)
			   (SETQ BP2 (FORWARD-SEXP BP))
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT " 0 (1+ "))
			   (INSERT-MOVING BP2 VARNAME)
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT ") ( ")))
			  ((EQ DO-TYPE 'DOLIST)
			   (SETQ BP2 (FORWARD-SEXP BP 2))
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT " (CDR "))
			   (INSERT-MOVING BP2 VARNAME)
			   (INSERT-MOVING BP2 (IN-CURRENT-FONT ") (NULL "))))
		    (INSERT-MOVING BP2 VARNAME))
		  (DELETE-INTERVAL DO-BP (FORWARD-WORD DO-BP) T)
		  (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS*
					 (INSERT DO-BP (IN-CURRENT-FONT "DO"))))))
	   (OR (SETQ BP1 (FORWARD-SEXP BP 3)) (BARF))
	   (DELETE-AROUND *WHITESPACE-CHARS* BP1)
	   (MOVE-BP (POINT) (INSERT-MOVING BP1 (IN-CURRENT-FONT #/))))
	   (INSERT-MOVING BP1 (IN-CURRENT-FONT ")
 ("))
	   (INSERT BP (IN-CURRENT-FONT "(("))
	   (INSERT (FORWARD-SEXP BP1) (IN-CURRENT-FONT #/)))
	   (INDENT-INTERVAL-FOR-LISP BP BP1 T)))))
  DIS-TEXT)

(DEFCOM COM-QUERY-REPLACE-LET-BINDING "Replace variable of LET with its value.
Point must be after or within the binding to be modified." ()
  (ATOM-WORD-SYNTAX-BIND
   (LET ((POINT (POINT))
	LET-BP BINDING-BP BP1 BP2 FROM TO)
    (OR (SETQ LET-BP (FIND-CONTAINING-ATOM POINT '(LET))) (BARF))
    (DO ((BP (FORWARD-LIST LET-BP 1 NIL -1 T) NBP)
	 (NBP))
	(NIL)
      (OR (SETQ NBP (FORWARD-SEXP BP 1 NIL 0 NIL NIL T)) (BARF))
      (OR (BP-< NBP POINT) (RETURN (SETQ BINDING-BP BP))))
    (SETQ BP1 (FORWARD-LIST BINDING-BP 1 NIL -1 T)
	  BP2 (FORWARD-SEXP BP1)
	  FROM (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-OVER *WHITESPACE-CHARS* BP2)
	  BP2 (FORWARD-SEXP BP1)
	  TO (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-SEXP LET-BP 2)
	  BP2 (FORWARD-SEXP BP1 1 NIL 1))
    (OR *NUMERIC-ARG-P* (PSETQ FROM TO TO FROM))
    (MOVE-BP POINT BP1)
    (LET ((*INTERVAL* (CREATE-INTERVAL BP1 BP2 T)))
      (QUERY-REPLACE POINT FROM TO T))))
  DIS-TEXT)

(DEFCOM COM-QUERY-REPLACE-LAST-KILL "Replace top of kill ring with region." ()
  (LET ((POINT (POINT)) (MARK (MARK)))
    (QUERY-REPLACE POINT (STRING-INTERVAL (CAR *KILL-RING*)) (STRING-INTERVAL MARK POINT)))
  DIS-TEXT)

(DEFCOM COM-JUST-ONE-SPACE "Replace all whitespace around point with arg spaces" ()
  (DELETE-AROUND *BLANKS* (POINT))
  (DOTIMES (I *NUMERIC-ARG*)
    (INSERT-MOVING (POINT) #\SP))
  DIS-TEXT)

(DEFCOM COM-CANONICALIZE-WHITESPACE "Try to fixup wrong spacing heuristically.
If given an argument, or called just after a yank type command, operates
at the mark, else at point." ()
  (LET ((BP (IF (OR *NUMERIC-ARG-P* (EQ *LAST-COMMAND-TYPE* 'YANK)) (MARK) (POINT)))
	BP1 CH1 CH2 SYN1 SYN2)
    (SETQ BP (BACKWARD-OVER *BLANKS* BP)
	  BP1 (FORWARD-OVER *BLANKS* BP)
	  CH1 (BP-CH-CHAR (FORWARD-CHAR BP -1))
	  CH2 (BP-CH-CHAR BP1)
	  SYN1 (LIST-SYNTAX CH1)
	  SYN2 (LIST-SYNTAX CH2))
    (COND ((OR (= CH2 #\CR)			;If at the end of the line,
	       (MULTIPLE-VALUE-BIND (STRING SLASH COMMENT)
		   (LISP-BP-SYNTACTIC-CONTEXT BP)
		 (OR STRING SLASH COMMENT))))	;or any funny syntax, leave it alone
	  ((NOT (= CH1 #\CR))			;If not at beginning of line,
	   (DELETE-INTERVAL BP BP1 T)		;flush whitespace, and
	   (AND ( SYN1 LIST-OPEN) ( SYN1 LIST-SINGLE-QUOTE)
		( SYN2 LIST-CLOSE)
		(INSERT BP (IN-CURRENT-FONT #\SP))))	;leave zero or one space in its place
	  (( CH2 #/()				;If not start of defun
	   (INDENT-INTERVAL-FOR-LISP BP (BEG-LINE BP 1 T) T NIL T))	;run tab
	  ((DO ((LINE (LINE-PREVIOUS (BP-LINE BP)) (LINE-PREVIOUS LINE))
		(OLINE (BP-LINE BP) LINE)	;Flush blank lines, and
		(TYPE))				;unless previous non-blank is a comment
	       (NIL)
	     (SETQ TYPE (AND LINE (LINE-TYPE LINE)))
	     (COND ((NEQ TYPE ':BLANK)
		    (DELETE-INTERVAL (CREATE-BP OLINE 0) BP T)
		    (RETURN (NEQ TYPE ':COMMENT)))))
	   (INSERT BP #\CR))))			;leave just one in their place
  DIS-TEXT)

(DEFCOM COM-FIND-UNBALANCED-PARENTHESES "Find parenthesis error in buffer" ()
  (LET ((BEG-BP (INTERVAL-FIRST-BP *INTERVAL*))
	(END-BP (INTERVAL-LAST-BP *INTERVAL*))
	(POINT (POINT))
	(OLD-TICK (INTERVAL-TICK *INTERVAL*))
	BEG-BP-1 END-BP-1 BP)
    (UNWIND-PROTECT
      (PROGN
	(SETQ BEG-BP-1 (COPY-BP BEG-BP ':MOVES)
	      END-BP-1 (COPY-BP END-BP ':NORMAL))
	(INSERT BEG-BP-1 "(
")
	(INSERT END-BP-1 "
)")
	(IF (SETQ BP (FORWARD-SEXP BEG-BP))
	    (IF (BP-= BP END-BP)		;All ok
		(TYPEIN-LINE "All parens appear balanced.")
		(MOVE-BP POINT BP)
		(TYPEIN-LINE "Probably extra right-paren here."))
	    (OR (SETQ BP (FORWARD-SEXP END-BP -1))
		(BARF "Cannot find unbalanced parenthesis"))
	    (MOVE-BP POINT BP)
	    (TYPEIN-LINE "Probably no right-paren for this left-paren.")))
      (COND (BEG-BP-1
	     (DELETE-INTERVAL BEG-BP BEG-BP-1 T)
	     (FLUSH-BP BEG-BP-1)))
      (COND (END-BP-1
	     (DELETE-INTERVAL END-BP-1 END-BP T)
	     (FLUSH-BP END-BP-1)))
      (SETF (INTERVAL-TICK *INTERVAL*) OLD-TICK)))
  DIS-BPS)

(DEFCOM COM-DESCRIBE-CLASS "Describe the specified class." ()
  (LET ((CLASS (COMPLETING-READ-FROM-MINI-BUFFER
		 "Describe class:"
		 (MAPCAR #'(LAMBDA (X)
			     (SETQ X (<- X ':CLASS-SYMBOL))
			     (CONS (FORMAT NIL "~S" X) X))
			 (CONS OBJECT-CLASS (SI:ALL-SUBCLASSES-OF-CLASS OBJECT-CLASS)))
		 NIL NIL "You are typing the name of a class, to be described.")))
    (AND (ATOM CLASS) (BARF))
    (DESCRIBE-CLASS-INTERNAL (CDR CLASS)))
  DIS-NONE)

(DEFUN DESCRIBE-CLASS-INTERNAL (CLASS)
  (OR (AND (SYMBOLP CLASS) (BOUNDP CLASS)
	   (ENTITYP (SETQ CLASS (SYMEVAL CLASS))))
      (BARF "~S is not a class" CLASS))
  (FORMAT *TYPEOUT-WINDOW* "~&Instance variables of ~A:~%"
	  (SYMEVAL-IN-CLOSURE CLASS ':NAME))
  (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST NIL (SYMEVAL-IN-CLOSURE CLASS 'SI:INSTANCE-PATTERN))
  (DO ((SYM (SYMEVAL-IN-CLOSURE CLASS 'SI:CLASS-METHOD-SYMBOL))
       (METHS NIL)
       (CL)
       (ML))
      ((EQ SYM 'SI:UNCLAIMED-MESSAGE))
    (SETQ CL (SYMEVAL SYM)
	  ML (%MAKE-POINTER DTP-LIST (FSYMEVAL SYM)))
    (FORMAT *TYPEOUT-WINDOW* "~2%Methods~:[ as a subclass~] of ~A:~%" (EQ CL CLASS)
	    (SYMEVAL-IN-CLOSURE CL ':NAME))
    (DO ((L ML (CDR L))
	 (M)
	 (LL NIL))
	((NLISTP L)
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FUNCTION-NAME (NREVERSE LL))
	 (SETQ SYM L
	       METHS (APPEND ML METHS))
	 (RPLACD (LAST METHS) NIL))
      (OR (ASSQ (CAR (SETQ M (CAR L))) METHS)
	  (PUSH M LL))))
  NIL)

(DEFVAR *ALL-FLAVOR-NAMES-ALIST* NIL)
(DEFVAR *LAST-ALL-FLAVOR-NAMES* NIL)

(DEFCOM COM-DESCRIBE-FLAVOR "Describe the specified flavor." ()
  (AND (NEQ *LAST-ALL-FLAVOR-NAMES* SI:*ALL-FLAVOR-NAMES*)
       (SETQ *LAST-ALL-FLAVOR-NAMES* SI:*ALL-FLAVOR-NAMES*
	     *ALL-FLAVOR-NAMES-ALIST* (MAPCAR #'(LAMBDA (X) (CONS (FORMAT NIL "~S" X) X))
					      *LAST-ALL-FLAVOR-NAMES*)))
  (LET ((FLAVOR (COMPLETING-READ-FROM-MINI-BUFFER
		 "Describe flavor:"
		 *ALL-FLAVOR-NAMES-ALIST*
		 NIL NIL "You are typing the name of a flavor, to be described.")))
    (AND (ATOM FLAVOR) (BARF))
    (DESCRIBE-FLAVOR-INTERNAL (CDR FLAVOR)))
  DIS-NONE)

(DEFUN DESCRIBE-FLAVOR-INTERNAL (FLAVOR &AUX FL TEM)
  (OR (SETQ FL (GET FLAVOR 'SI:FLAVOR))
      (BARF "~S is not the name of a flavor" FLAVOR))
  (COND ((SETQ TEM (SI:FLAVOR-DEPENDS-ON FL))
	 (FORMAT *TYPEOUT-WINDOW* "~&Flavor ~S directly depends on flavor~P:~%"
		 FLAVOR (LENGTH TEM))
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FLAVOR-NAME TEM))
	(T
	 (FORMAT *TYPEOUT-WINDOW*
		 "~&Flavor ~S does not directly depend on any other flavors~%"
		 FLAVOR)))
  (COND ((SETQ TEM (SI:FLAVOR-INCLUDES FL))
	 (FORMAT *TYPEOUT-WINDOW* "~& and directly includes flavor~P:~%"
		 (LENGTH TEM))
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FLAVOR-NAME TEM)))
  (COND ((SETQ TEM (SI:FLAVOR-DEPENDED-ON-BY FL))
	 (FORMAT *TYPEOUT-WINDOW* "~& and is directly depended on by flavor~P:~%"
		 (LENGTH TEM))
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'FLAVOR-NAME TEM)))
  (LOCAL-DECLARE ((SPECIAL LIV))		;For the REM-IF below
    (LET ((LIV (SI:FLAVOR-LOCAL-INSTANCE-VARIABLES FL)))
      (IF (NULL LIV)
	  (FORMAT *TYPEOUT-WINDOW* "~&~S has no local instance variables~%" FLAVOR)
	  (FORMAT *TYPEOUT-WINDOW* "~&Instance variable~P of ~S: ~{~S~^, ~}~%"
		  (LENGTH LIV) FLAVOR LIV))
      (AND (SETQ TEM (SI:FLAVOR-INSTANCE-SIZE FL))
	   (FORMAT *TYPEOUT-WINDOW* "Flavor ~S has instance size ~D,
 with inherited instance variables: ~{~S~^, ~}~%"
		   FLAVOR TEM
		   (REM-IF #'(LAMBDA (X) (MEMQ X LIV))
			   (SI:FLAVOR-ALL-INSTANCE-VARIABLES FL))))))
  (SI:MAP-OVER-COMPONENT-FLAVORS 0 T NIL #'DESCRIBE-FLAVOR-1 FLAVOR (LIST NIL NIL) FLAVOR)
  (DO ((PLIST (SI:FLAVOR-PLIST FL) (CDDR PLIST))
       (FLAG NIL))
      ((NULL PLIST))
    (COND ((NOT (MEMQ (CAR PLIST) '(:DEFAULT-INIT-PLIST
				     :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)))
	   (COND ((NOT FLAG)
		  (FORMAT *TYPEOUT-WINDOW* "Random properties:~%")
		  (SETQ FLAG T)))
	   (FORMAT *TYPEOUT-WINDOW* "~5X~S:	~S~%" (CAR PLIST) (CADR PLIST)))))
  NIL)

(DEFUN DESCRIBE-FLAVOR-1 (FL STATE TOP-FLAVOR-NAME &AUX (FLAVOR-FLAG NIL))
  (COND ((NOT (MEMQ FL (SECOND STATE)))
	 (DO ((METHS (SI:FLAVOR-METHOD-TABLE FL) (CDR METHS))
	      (METH)
	      (ELEM)
	      (MSG)
	      (MSG-FLAG NIL NIL)
	      (TEM))
	     ((NULL METHS))
	   (SETQ METH (CAR METHS) MSG (FIRST METH) METH (CDDDR METH))
	   (OR (SETQ ELEM (ASSQ MSG (FIRST STATE)))
	       (PUSH (SETQ ELEM (LIST MSG NIL NIL NIL NIL)) (FIRST STATE)))
	   (COND ((AND (SETQ TEM (ASSQ ':BEFORE METH)) (NOT (MEMQ TEM (SECOND ELEM))))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (CADR TEM) "before"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (PUSH TEM (SECOND ELEM))))
	   (COND ((AND (SETQ TEM (ASSQ 'NIL METH)) (NULL (THIRD ELEM)))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (CADR TEM) "primary"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (SETF (THIRD ELEM) TEM)))
	   (COND ((AND (SETQ TEM (ASSQ ':AFTER METH)) (NOT (MEMQ TEM (FOURTH ELEM))))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (CADR TEM) "after"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (PUSH TEM (FOURTH ELEM))))
	   (COND ((AND (SETQ TEM (ASSQ ':WRAPPER METH)) (NOT (MEMQ TEM (FIFTH ELEM))))
		  (MULTIPLE-VALUE (FLAVOR-FLAG MSG-FLAG)
		    (DESCRIBE-FLAVOR-PRINT-MSG FL MSG (CADR TEM) "wrapper"
					       MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME))
		  (PUSH TEM (FIFTH ELEM))))
	   (AND MSG-FLAG (TERPRI *TYPEOUT-WINDOW*)))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (SI:FLAVOR-GETTABLE-INSTANCE-VARIABLES FL)
			     "automatically-generated methods to get instance variable" ""
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (SI:FLAVOR-SETTABLE-INSTANCE-VARIABLES FL)
			     "automatically-generated methods to set instance variable" ""
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (SI:FLAVOR-INITABLE-INSTANCE-VARIABLES FL)
			     "instance variable" " that may be set by initialization"
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (SI:FLAVOR-INIT-KEYWORDS FL)
			     "keyword" " in the :INIT message"
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (SETQ FLAVOR-FLAG (DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST
			     FL (GET (LOCF (SI:FLAVOR-PLIST FL))
				     ':OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES)
			     "macros to access variable" ""
			     FLAVOR-FLAG TOP-FLAVOR-NAME))
	 (LET ((DEFAULT-PLIST (GET (LOCF (SI:FLAVOR-PLIST FL)) ':DEFAULT-INIT-PLIST)))
	   (COND (DEFAULT-PLIST
		  (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAVOR-FLAG TOP-FLAVOR-NAME)
		  (FORMAT *TYPEOUT-WINDOW* " Plus default init plist: ")
		  (DO ((L DEFAULT-PLIST (CDDR L))
		       (FLAG T NIL))
		      ((NULL L))
		    (FORMAT *TYPEOUT-WINDOW* "~:[, ~]~S ~S" FLAG (CAR L) (CADR L)))
		  (TERPRI *TYPEOUT-WINDOW*))))
	 (PUSH FL (SECOND STATE))))
  STATE)

(DEFUN DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME (FL FLAG TOP-FLAVOR-NAME &AUX FLAVOR-NAME)
  (COND ((NOT FLAG)				;If not already printed
	 (SETQ FLAVOR-NAME (SI:FLAVOR-NAME FL))
	 (FORMAT *TYPEOUT-WINDOW* "Method(s) ~:[inherited from~;of~] ~S:~%"
		 (EQ FLAVOR-NAME TOP-FLAVOR-NAME) FLAVOR-NAME)))
  T)						;New value of flag

(DEFUN DESCRIBE-FLAVOR-PRINT-MSG (FL MSG FUNCTION TYPE MSG-FLAG FLAVOR-FLAG TOP-FLAVOR-NAME)
  (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAVOR-FLAG TOP-FLAVOR-NAME)
  (OR MSG-FLAG (FORMAT *TYPEOUT-WINDOW* "   :~A " MSG))
  (FUNCALL *TYPEOUT-WINDOW* ':ITEM 'FUNCTION-NAME FUNCTION TYPE)
  (FUNCALL *TYPEOUT-WINDOW* ':TYO #\SP)
  (MVRETURN T T))				;New values for the flags

(DEFUN DESCRIBE-FLAVOR-PRINT-MISCELLANEOUS-LIST (FL LIST STR1 STR2 FLAG TOP-FLAVOR-NAME)
  (COND (LIST					;If there is something there
	 (DESCRIBE-FLAVOR-PRINT-FLAVOR-NAME FL FLAG TOP-FLAVOR-NAME)
	 (FORMAT *TYPEOUT-WINDOW* " Plus ~A~P~A: ~{~:S~^, ~}~%" STR1 (LENGTH LIST) STR2 LIST)
	 T)))					;New value of the flag

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FLAVOR-NAME "Edit" EDIT-DEFINITION T)


(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FLAVOR-NAME "Describe"
			  DESCRIBE-FLAVOR-INTERNAL)

;;;Multics EMACS compatible macro commands
(DEFCOM COM-START-KBD-MACRO "Begin defining a keyboard macro" ()
  (OR (MEMQ ':MACRO-PUSH (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (FUNCALL STANDARD-INPUT ':MACRO-PUSH (+ 2 *NUMERIC-ARG-N-DIGITS*))
  DIS-NONE)

(DEFCOM COM-END-KBD-MACRO "Terminate the definition of a keyboard macro" ()
  (OR (MEMQ ':MACRO-POP (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (*CATCH 'MACRO-LOOP				;In case no macro running
     (FUNCALL STANDARD-INPUT ':MACRO-POP (+ 2 *NUMERIC-ARG-N-DIGITS*)
	                                 (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*)))
  DIS-NONE)

(DEFCOM COM-CALL-LAST-KBD-MACRO "Repeat the last keyboard macro" ()
  (OR (MEMQ ':MACRO-EXECUTE (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (FUNCALL STANDARD-INPUT ':MACRO-EXECUTE NIL (AND (NOT (ZEROP *NUMERIC-ARG*)) *NUMERIC-ARG*))
  DIS-NONE)

(DEFCOM COM-KBD-MACRO-QUERY "Interactive keyboard macro" ()
  (OR (MEMQ ':MACRO-QUERY (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream doesnt support macros"))
  (FUNCALL STANDARD-INPUT ':MACRO-QUERY)
  DIS-NONE)

(DEFCOM COM-VIEW-KBD-MACRO "Typeout the specified keyboard macro.
The macro should be a /"permanent/" macro, that has a name.
The name of the macro is read from the mini-buffer, just cr means the last
one defined, which can also be temporary." ()
  (OR (MEMQ ':MACRO-PREVIOUS-ARRAY (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
      (BARF "This stream does not support macros"))
  (LET ((PACKAGE SI:PKG-USER-PACKAGE)
	NAME MAC)
    (SETQ NAME (TYPEIN-LINE-READ "Name of macro to view (CR for last macro defined):"))
    (COND ((EQ NAME '*EOF*)
	   (SETQ MAC (FUNCALL STANDARD-INPUT ':MACRO-PREVIOUS-ARRAY)))
	  ((NOT (SETQ MAC (GET NAME 'MACRO-STREAM-MACRO)))
	   (BARF "~A is not a defined macro." NAME)))
    (DO ((I 0 (1+ I))
	 (LEN (MACRO-LENGTH MAC))
	 (CH))
	((> I LEN))
      (FORMAT T (SELECTQ (SETQ CH (AREF MAC I))
		  (*MOUSE* "Mouse command ~*")
		  (*SPACE* "Macro query ~*")
		  (*RUN* "Repeat ~*")
		  (NIL "Input ~*")
		  (OTHERWISE "~:C "))
	      CH)))
  DIS-NONE)

(DEFCOM COM-DECLARE-SPECIAL "Add the nth previous word to the last special declaration" ()
  (ATOM-WORD-SYNTAX-BIND
    (LET (WORD)
      (LET ((BP1 (FORWARD-WORD (POINT) (- *NUMERIC-ARG*)))
	    BP2)
	(OR BP1 (BARF))
	(SETQ BP2 (FORWARD-WORD BP1 1))
	(OR BP2 (BARF))
	(SETQ WORD (STRING-INTERVAL BP1 BP2 T)))
      (LET ((BP (DO-NAMED DECLARES
			  ((LINE (BP-LINE (POINT)) (LINE-PREVIOUS LINE))
			   (LIMIT-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
			  (NIL)
		  (AND (STRING-EQUAL "(DECLARE " LINE 0 0 9 9)
		       ;;Found a (DECLARE ...), look for SPECIAL in the CARs of the elements
		       (DO ((BP1 (CREATE-BP LINE 9) (FORWARD-SEXP BP1))
			    (BP2)
			    (BP3))
			   ((NULL BP1))
			 (OR (SETQ BP2 (FORWARD-LIST BP1 1 NIL 1 T))
			     (RETURN NIL))
			 (OR (SETQ BP3 (FORWARD-WORD BP2)) (RETURN NIL))
			 (AND (EQ (BP-LINE BP2) (BP-LINE BP3))
			      (STRING-EQUAL "SPECIAL" (BP-LINE BP2) 0 (BP-INDEX BP2) 7
					    (BP-INDEX BP3))
			      (SETQ BP2 (FORWARD-LIST BP1))	;Found one
			      (RETURN-FROM DECLARES (FORWARD-CHAR BP2 -1)))))
		  ;;If there isnt a special declaration, make one at the start of the file
		  (AND (EQ LINE LIMIT-LINE)
		       (RETURN (FORWARD-CHAR (INSERT
					       (SKIP-OVER-BLANK-LINES-AND-COMMENTS
						 (INTERVAL-FIRST-BP *INTERVAL*) T)
					       "(DECLARE (SPECIAL))

")
					     -3))))))
	;;Now put it in and try not to overflow the line
	(WITH-BP (PT (POINT) ':MOVES)		;Preserve point
	  (MOVE-BP (POINT) BP)
	  (INSERT-MOVING (POINT) (STRING-APPEND #\SP WORD))
	  (AUTO-FILL-HOOK #\SP)
	  (COND ((END-LINE-P (POINT))
		 (MOVE-BP (POINT) (END-LINE (POINT) 1))
		 (INSERT (DELETE-BACKWARD-OVER *BLANKS* (POINT)) #\CR)
		 (COM-INDENT-FOR-LISP)))
	  (MOVE-BP (POINT) PT)))))
  DIS-TEXT)

;;; Pattern finding command
(DEFVAR *LAST-PATTERN* NIL)
(DEFVAR *LAST-PATTERN-BP* NIL)
(DEFVAR *LAST-PATTERN-RESTART-LIST*)

(DEFCOM COM-FIND-PATTERN "Move to next occurence of the given pattern.
The pattern must be a list, ** matches any one thing, ... any number of things.
A numeric argument repeats the last search." ()
  (LET (FORM RESTART BP)
    (COND (*NUMERIC-ARG-P*
	   (SETQ FORM (OR *LAST-PATTERN* (BARF "No previous pattern")))
	   (TYPEIN-LINE "Finding ~S" FORM)
	   (AND (BP-= (POINT) *LAST-PATTERN-BP*) (SETQ RESTART *LAST-PATTERN-RESTART-LIST*)))
	  (T
	   (LET ((FORM-STRING (TYPEIN-LINE-READLINE "Pattern to search for:"))
		 (EOF '(())))
	     (SETQ FORM (READ-FROM-STRING FORM-STRING EOF))
	     (AND (EQ FORM EOF) (BARF "Unbalanced parens"))
	     (OR (LISTP FORM) (BARF "I only know how to search for lists"))
	     ;; This is sort of a kludge
	     (OR (EQ PACKAGE SI:PKG-USER-PACKAGE)
		 (SETQ FORM (SUBLIS (LIST (CONS (INTERN "...") ':...)
					  (CONS (INTERN "**") ':**))
				    FORM))))))
    (MULTIPLE-VALUE (BP RESTART) (FIND-PATTERN (POINT) FORM RESTART))
    (OR BP (BARF))
    (MAYBE-PUSH-POINT BP)
    (MOVE-BP (POINT) BP)
    (SETQ *LAST-PATTERN* FORM *LAST-PATTERN-BP* BP *LAST-PATTERN-RESTART-LIST* RESTART))
  DIS-BPS)

;;; Attempt to find an instance of THING after BP, return a new BP if successful
(DEFUN FIND-PATTERN (BP PATTERN &OPTIONAL RESTART-LIST)
  (DO-NAMED FIND-PATTERN
      ((BP1 (FORWARD-DEFUN BP -1 T) (FORWARD-DEFUN BP2))
       (BP2)
       (STREAM (INTERVAL-STREAM *INTERVAL*))
       (FORM)
       (SI:XR-CORRESPONDENCE-FLAG T)
       (SI:XR-CORRESPONDENCE NIL NIL)
       (RESTART-LIST RESTART-LIST NIL)
       (PLIST)
       (TEM))
      ((NULL BP1) NIL)
    (SETQ PLIST (LOCF (LINE-CONTENTS-PLIST (BP-LINE BP1))))
    (SETQ BP2 (FORWARD-SEXP BP1))		;Find the end of this defun
    ;; Now get the form and correspondence for this defun, using previous if there
    (COND (BP2
	   (COND ((AND (SETQ TEM (GET PLIST 'CORRESPONDENCE))
		       (COND ((> (CADR TEM) (INTERVAL-REAL-TICK BP1 BP2 T))
			      (SETQ FORM (CAR TEM) SI:XR-CORRESPONDENCE (CADDR TEM))
			      T)
			     (T
			      (REMPROP PLIST 'CORRESPONDENCE)
			      NIL))))
		 (T
		  (FUNCALL STREAM ':SET-BP BP1)
		  (SETQ FORM (READ STREAM))
		  (PUTPROP PLIST (LIST FORM (TICK) SI:XR-CORRESPONDENCE) 'CORRESPONDENCE)))
	   (AND RESTART-LIST (SETQ FORM (CAR RESTART-LIST) RESTART-LIST (CDR RESTART-LIST)))
	   (DO ((FORM FORM (CAR RESTART-LIST))
		(RESTART-LIST RESTART-LIST (CDR RESTART-LIST))
		(FOUND)
		(BP3))
	       (NIL)
	     (MULTIPLE-VALUE (FOUND RESTART-LIST)
	       (FIND PATTERN FORM RESTART-LIST))
	     (OR FOUND (RETURN NIL))
	     (AND (SETQ BP3 (CADR (MEMQ FOUND SI:XR-CORRESPONDENCE)))
		  (BP-< BP BP3)
		  (RETURN-FROM FIND-PATTERN BP3 RESTART-LIST))))
	  (T					;Look forward for next defun
	   (SETQ BP2 BP1)))))

;;; Attempt to find an instance of THING in LIST
(DEFUN FIND (THING LIST &OPTIONAL RESTART-LIST &AUX VAL)
  (COND	((AND RESTART-LIST (MULTIPLE-VALUE (VAL RESTART-LIST)
			     (FIND THING (CAR RESTART-LIST) (CDR RESTART-LIST))))
	 (PUSH LIST RESTART-LIST))
	((MATCH THING LIST) (SETQ VAL LIST RESTART-LIST NIL))
	((ATOM LIST) (SETQ VAL NIL RESTART-LIST NIL))
	(T
	 (DO ((LIST LIST (CDR LIST)))
	     ((NULL LIST) (SETQ VAL NIL RESTART-LIST NIL))
	   (MULTIPLE-VALUE (VAL RESTART-LIST) (FIND THING (CAR LIST)))
	   (COND (VAL
		  (PUSH (CDR LIST) RESTART-LIST)
		  (RETURN NIL))))))
  (MVRETURN VAL RESTART-LIST))

;;; Simple minded pattern matcher
;;; ** matches an arbitrary frob, ... an arbitrary number (possibly 0) of frobs
(DEFUN MATCH (A B)
  (DO ((A A (CDR A))
       (B B (CDR B))
       (VAL))
      (NIL)
    (COND ((EQ A B) (RETURN T))
	  ((EQ A ':...) (RETURN 'CDR))
	  ((EQ A ':**) (RETURN T))
	  ((NOT (= (%DATA-TYPE A) (%DATA-TYPE B))) (RETURN NIL))
	  ((NUMBERP A) (RETURN (= A B)))
	  ((ARRAYP A) (RETURN (AND (STRINGP A) (STRINGP B) (STRING-EQUAL A B))))
	  ((NLISTP A) (RETURN NIL))
	  ((NOT (SETQ VAL (MATCH (CAR A) (CAR B)))) (RETURN NIL))
	  ((NEQ VAL T) (RETURN (OR (NULL (SETQ A (CDR A)))
				   (DO B B (CDR B) (NULL B)
				       (AND (MATCH A B) (RETURN T)))))))))

(DEFCOM COM-UNDO "Undo the last undoable command" ()
  (OR (BOUNDP '*UNDO-START-BP*) (BARF "Nothing to undo"))
  (OR (EQ (BP-INTERVAL *UNDO-START-BP*) *INTERVAL*) (BARF "No longer in the same buffer"))
  (LET ((POINT (POINT)) (MARK (MARK))
	(OLD *UNDO-OLD-INTERVAL*)
	(NAME *UNDO-TYPE*))
    (TYPEIN-LINE "Undo ~A? " NAME)
    (COND ((TYPEIN-LINE-ACTIVATE (Y-OR-N-P NIL *TYPEIN-WINDOW*))
	   (MOVE-BP MARK *UNDO-START-BP*)
	   (MOVE-BP POINT *UNDO-END-BP*)
	   (UNDO-SAVE MARK POINT T "Undo")
	   (DELETE-INTERVAL MARK POINT T)
	   (INSERT-INTERVAL-MOVING POINT OLD)
	   (TYPEIN-LINE "~A undone." NAME))))
  DIS-TEXT)
