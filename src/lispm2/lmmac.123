;; These are the macros in the Lisp Machine system.   -*-LISP-*-
;; They used to be in LISPM;MACROS > but have been moved
;; for purposes of the cold load.

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

; Macros which do the equivalent of a displace MUST use DISPLACE
; to make sure that temporary area problems are worried about.

;The IF-IN-MACLISP/IF-IN-LISPM conditionals have to do with not breaking
;the Maclisp environment when compiling.  The optimizers in COMPAT take
;over these functions when compiling in Maclisp.

(DECLARE (SETQ INHIBIT-STYLE-WARNINGS-SWITCH T)
	 (SPECIAL COMPILING-FOR-LISPM))

;THESE ARE CONDITIONAL ON WHICH SYSTEM IS EXECUTING THEM.
(DEFMACRO IF-IN-MACLISP (&REST FORMS)
    (COND ((NOT (STATUS FEATURE LISPM))
	   `(PROGN 'COMPILE . ,FORMS))))

(DEFMACRO IF-IN-LISPM (&REST FORMS)
    (COND ((STATUS FEATURE LISPM)
	   `(PROGN 'COMPILE . ,FORMS))))

;THESE ARE CONDITIONAL ON WHICH SYSTEM RESULT IS INTENDED "FOR ".
; THIS IS THE SAME AS WHICH SYSTEM IS "IN" EXCEPT IN THE CASE
; COMPILING IN MACLISP FOR LISPM (IE QCMP, AFTER COMPILER ITSELF HAS
; BEEN LOADED).  THE COMPILING-FOR-LISPM SWITCH IS SET BY .LISP. (INIT)
; AFTER QCMP HAS BEEN LOADED.

(DEFMACRO IF-FOR-MACLISP (&REST FORMS)
    (COND ((AND (NOT (STATUS FEATURE LISPM))		;IN MACLISP
		(OR (NOT (BOUNDP 'COMPILING-FOR-LISPM))
		    (NULL COMPILING-FOR-LISPM)))
	   `(PROGN 'COMPILE . ,FORMS))))

(DEFMACRO IF-FOR-LISPM (&REST FORMS)
    (COND ((OR (STATUS FEATURE LISPM)
	       (AND (BOUNDP 'COMPILING-FOR-LISPM)
		    COMPILING-FOR-LISPM))
	   `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL))
			  (PROGN 'COMPILE . ,FORMS)))))

(DEFMACRO IF-FOR-MACLISP-ELSE-LISPM (MACLISP-FORM LISPM-FORM)
    (COND ((NOT (STATUS FEATURE LISPM))
	   (COND ((OR (NOT (BOUNDP 'COMPILING-FOR-LISPM))	;QCMP DEFINES THIS TO T
		      (NULL COMPILING-FOR-LISPM))
		  MACLISP-FORM)
		 (T `(COMPILER-LET ((RUN-IN-MACLISP-SWITCH NIL)) ,LISPM-FORM))))
    ;COMPLR DOESNT KNOW (OR CARE) ABOUT COMPILER-LET.
	  (T LISPM-FORM)))

;; Needed when conditionalizing something at top level with #Q or #M because
;; splicing readmacros flushed then.  #Q and #M now work at top level, so this
;; is for compatibility only.
(DEFMACRO NULL-MACRO (FORM) FORM)

;These must appear before anything in this file that uses LET in order to win
; at cold-load readin time.
#Q (PROGN 'COMPILE  ;Do not change this to IF-FOR-LISPM!!  that would lose because it
		    ; eventually expands into a LET.
;PUSH, POP, LET, LET* now exist in COMPLR and in ITS MacLisp.  -cwh

(DEFMACRO-DISPLACE PUSH (ITEM LIST)
   `(SETF ,LIST (CONS ,ITEM ,LIST)))

(DEFMACRO-DISPLACE POP (LIST &OPTIONAL DEST)
  `(PROG1 ,(COND ((NULL DEST)          
                  `(CAR ,LIST))
                 (T `(SETF ,DEST (CAR ,LIST))))
           (SETF ,LIST (CDR ,LIST))))

; (LET ((VAR1 VAL1) (VAR2 VAL2) VAR3 ..) <BODY>)
; binds VAR1 to VAL1 and VAR2 to VAL2 and VAR3 to NIL.

(DEFMACRO-DISPLACE LET (VARLIST . BODY)
   `((LAMBDA ,(MAPCAR '(LAMBDA (V) (COND ((ATOM V) V)
					 ((CDDR V)
					  (FERROR NIL "~S extraneous in LET" V))
					 (T (CAR V))))
		      VARLIST)
	     . ,BODY)
     . ,(MAPCAR '(LAMBDA (V) (COND ((ATOM V) NIL) (T (CADR V)))) VARLIST)))

; LET* is like LET except it binds sequentially instead of in parallel.

(DEFMACRO-DISPLACE LET* (VARLIST . BODY)
	  (DO ((L (REVERSE VARLIST) (CDR L))
	       (B BODY `(((LAMBDA (,(COND ((ATOM (CAR L)) (CAR L))
					  (T (CAAR L))))
				  . ,B)
			  ,(COND ((ATOM (CAR L)) NIL)
				 (T (CADAR L)))))))
	      ((NULL L)
	       (COND ((NULL (CDR B)) (CAR B))
		     (T `(PROGN . ,B))))))

)

(DEFMACRO-DISPLACE @DEFINE (&REST IGNORE) NIL)

(DEFMACRO-DISPLACE FIRST (LIST) `(CAR ,LIST))

(DEFMACRO-DISPLACE SECOND (LIST) `(CADR ,LIST))

(DEFMACRO-DISPLACE THIRD (LIST) `(CADDR ,LIST))

(DEFMACRO-DISPLACE FOURTH (LIST) `(CADDDR ,LIST))

(DEFMACRO-DISPLACE FIFTH (LIST) `(CAR (CDDDDR ,LIST)))

(DEFMACRO-DISPLACE SIXTH (LIST) `(CADR (CDDDDR ,LIST)))

(DEFMACRO-DISPLACE SEVENTH (LIST) `(CADDR (CDDDDR ,LIST)))

(DEFMACRO-DISPLACE REST1 (LIST) `(CDR ,LIST))

(DEFMACRO-DISPLACE REST2 (LIST) `(CDDR ,LIST))

(DEFMACRO-DISPLACE REST3 (LIST) `(CDDDR ,LIST))

(DEFMACRO-DISPLACE REST4 (LIST) `(CDDDDR ,LIST))

;; (<= A B) --> (NOT (> A B))
;; (<= A B C) --> (NOT (OR (> A B) (> B C)))
;; Funny arglist to check for correct number of arguments.

(DEFMACRO-DISPLACE <= (ARG1 ARG2 &REST REST &AUX RESULT)
  (SETQ REST (LIST* ARG1 ARG2 REST))
  (DO L REST (CDR L) (NULL (CDR L))
      (PUSH `(> ,(CAR L) ,(CADR L)) RESULT))
  (COND ((NULL (CDR RESULT)) `(NOT ,(CAR RESULT)))
	(T `(NOT (OR . ,(NREVERSE RESULT))))))

;; (>= A B) --> (NOT (< A B))
;; (>= A B C) --> (NOT (OR (< A B) (< B C)))
;; Funny arglist to check for correct number of arguments.

(DEFMACRO-DISPLACE >= (ARG1 ARG2 &REST REST &AUX RESULT)
  (SETQ REST (LIST* ARG1 ARG2 REST))
  (DO L REST (CDR L) (NULL (CDR L))
      (PUSH `(< ,(CAR L) ,(CADR L)) RESULT))
  (COND ((NULL (CDR RESULT)) `(NOT ,(CAR RESULT)))
	(T `(NOT (OR . ,(NREVERSE RESULT))))))

(DEFMACRO-DISPLACE / (ARG1 ARG2 &REST REST) `(<= ,ARG1 ,ARG2 . ,REST))

(DEFMACRO-DISPLACE / (ARG1 ARG2 &REST REST) `(>= ,ARG1 ,ARG2 . ,REST))

(DEFMACRO-DISPLACE NEQ (X Y) `(NOT (EQ ,X ,Y)))

(DEFMACRO-DISPLACE / (X Y) `(NOT (= ,X ,Y)))

(DEFMACRO-DISPLACE BIT-TEST (A B)
   `(NOT (ZEROP (LOGAND ,A ,B))))

(DEFMACRO-DISPLACE LDB-TEST (A B)
   `(NOT (ZEROP (LDB ,A ,B))))

(IF-IN-LISPM
(DEFMACRO-DISPLACE CATCH (BODY TAG)
    `(*CATCH ',TAG ,BODY)))

(IF-IN-LISPM
(DEFMACRO-DISPLACE THROW (BODY TAG)
    `(*THROW ',TAG ,BODY))
   )

(IF-IN-LISPM
(DEFMACRO-DISPLACE ERRSET (BODY &OPTIONAL (PRINTFLAG T))
    `(LET ((EH:ERRSET-STATUS T)
	   (EH:ERRSET-PRINT-MSG ,PRINTFLAG))
	  (*CATCH 'EH:ERRSET-CATCH (LIST ,BODY))))
)
(IF-IN-LISPM 
(DEFMACRO-DISPLACE ERR (&OPTIONAL VALUE-FORM FLAG)
    (COND (FLAG (ERROR "ERR with two arguments is not implemented"))
	  ((NULL VALUE-FORM) '(ERROR ""))
	  (T `(COND (EH:ERRSET-STATUS (*THROW 'EH:ERRSET-CATCH ,VALUE-FORM))
		    (T (ERROR ""))))))
)

(IF-IN-LISPM
(DEFMACRO-DISPLACE ARRAYCALL (IGNORE ARRAY &REST DIMS)
  `(FUNCALL ,ARRAY . ,DIMS))
)

(DEFMACRO-DISPLACE SELECTQ (TEST-OBJECT . CLAUSES)
    (LET (TEST-EXP COND-EXP)
	 (SETQ TEST-EXP
	       (COND ((OR (ATOM TEST-OBJECT)
			  (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			       (ATOM (CADR TEST-OBJECT))))
		      TEST-OBJECT)
		     (T '*SELECTQ-ITEM*)))
	 (SETQ COND-EXP
	   (CONS 'COND
	    (MAPCAR (FUNCTION (LAMBDA (CLAUSE)
		       (COND ((OR (EQ (CAR CLAUSE) 'OTHERWISE)
				  (EQ (CAR CLAUSE) 'T)) ;Maclisp compatibility
			      (CONS T (CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      `((EQ ,TEST-EXP ',(CAR CLAUSE)) . ,(CDR CLAUSE)))
			     (T
			      `((MEMQ ,TEST-EXP ',(CAR CLAUSE)) . ,(CDR CLAUSE))))))
		    CLAUSES)))
	 (COND ((EQ TEST-EXP TEST-OBJECT) COND-EXP)
	       (T
		`(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
		      ,COND-EXP)))))

(DEFMACRO-DISPLACE SELECT (TEST-OBJECT . CLAUSES)
    (LET (TEST-EXP COND-EXP)
	 (SETQ TEST-EXP
	       (COND ((OR (ATOM TEST-OBJECT)
			  (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			       (ATOM (CADR TEST-OBJECT))))
		      TEST-OBJECT)
		     (T '*SELECTQ-ITEM*)))
	 (SETQ COND-EXP
	   (CONS 'COND
	    (MAPCAR (FUNCTION (LAMBDA (CLAUSE)
		       (COND ((OR (EQ (CAR CLAUSE) 'OTHERWISE)
				  (EQ (CAR CLAUSE) 'T)) ;Maclisp compatibility
			      (CONS T (CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      `((EQ ,TEST-EXP ,(CAR CLAUSE)) . ,(CDR CLAUSE)))
			     (T
			      `((OR . ,(MAPCAR (FUNCTION (LAMBDA (FORM)
					          `(EQ ,TEST-EXP ,FORM)))
					       (CAR CLAUSE)))
				. ,(CDR CLAUSE))))))
		    CLAUSES)))
	 (COND ((EQ TEST-EXP TEST-OBJECT) COND-EXP)
	       (T
		`(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
		      ,COND-EXP)))))

(DEFMACRO-DISPLACE SELECTOR (TEST-OBJECT TEST-FUNCTION . CLAUSES)
    (LET (TEST-EXP COND-EXP)
	 (SETQ TEST-EXP
	       (COND ((OR (ATOM TEST-OBJECT)
			  (AND (MEMQ (CAR TEST-OBJECT) '(CAR CDR CAAR CADR CDAR CDDR))
			       (ATOM (CADR TEST-OBJECT))))
		      TEST-OBJECT)
		     (T '*SELECTQ-ITEM*)))
	 (SETQ COND-EXP
	   (CONS 'COND
	    (MAPCAR (FUNCTION (LAMBDA (CLAUSE)
		       (COND ((OR (EQ (CAR CLAUSE) 'OTHERWISE)
				  (EQ (CAR CLAUSE) 'T)) ;Maclisp compatibility
			      (CONS T (CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      `((,TEST-FUNCTION ,TEST-EXP ,(CAR CLAUSE)) . ,(CDR CLAUSE)))
			     (T
			      `((OR . ,(MAPCAR (FUNCTION (LAMBDA (FORM)
					          `(,TEST-FUNCTION ,TEST-EXP ,FORM)))
					       (CAR CLAUSE)))
				. ,(CDR CLAUSE))))))
		    CLAUSES)))
	 (COND ((EQ TEST-EXP TEST-OBJECT) COND-EXP)
	       (T
		`(LET ((*SELECTQ-ITEM* ,TEST-OBJECT))
		      ,COND-EXP)))))

;EVENTUALLY THE MICRO COMPILER SHOULD BE AWARE OF THIS
(DEFMACRO-DISPLACE DISPATCH (PPSS WORD . BODY)
  (LIST
    (LIST 
      'LAMBDA
      '(*BYTE*)
      (CONS 'COND
	    (MAPCAR (FUNCTION (LAMBDA (CLAUSE)
		       (COND ((EQ (CAR CLAUSE) 'OTHERWISE)
			      (CONS T (CDR CLAUSE)))
			     ((ATOM (CAR CLAUSE))
			      (CONS (LIST '= '*BYTE* (CAR CLAUSE))
				    (CDR CLAUSE)))
			     (T
			      (CONS (CONS 'OR
					  (MAPCAR (FUNCTION (LAMBDA (ITEM)
					             (LIST '= '*BYTE* ITEM)))
						  (CAR CLAUSE)))
				    (CDR CLAUSE))))))
		    BODY)))
    (LIST 'LDB PPSS WORD)))

(DEFMACRO-DISPLACE EVERY (LIST PRED &OPTIONAL (STEP ''CDR))
   `(DO ((*L* ,LIST (FUNCALL ,STEP *L*)))
	((NULL *L*) T)
      (OR (FUNCALL ,PRED (CAR *L*)) (RETURN NIL))))

(DEFMACRO-DISPLACE SOME (LIST PRED &OPTIONAL (STEP ''CDR))
   `(DO ((*L* ,LIST (FUNCALL ,STEP *L*)))
	((NULL *L*) NIL)
      (AND (FUNCALL ,PRED (CAR *L*)) (RETURN *L*))))

;(BEGF FOO)  and  (ENDF FOO) delimit the definition of FOO, for EDFN.
(DEFMACRO-DISPLACE BEGF IGNORE '(DECLARE))

(DEFMACRO-DISPLACE ENDF IGNORE '(DECLARE))

; LET-GLOBALLY IS SIMILAR TO LET, EXCEPT THAT THE BINDING APPLIES
; TO THE WHOLE WORLD, NOT JUST THE CURRENTLY-EXECUTING STACK GROUP.
; FOR THE MOMENT, ANYWAY, IT IS IMPLEMENTED USING UNWIND-PROTECT.
(DEFMACRO-DISPLACE LET-GLOBALLY (VARLIST . BODY)
  (LET ((VARS (MAPCAR '(LAMBDA (V) (COND ((ATOM V) V) (T (CAR V)))) VARLIST))
	(VALS (MAPCAR '(LAMBDA (V) (COND ((ATOM V) NIL) (T (CADR V)))) VARLIST))
	(GENVARS (MAPCAR '(LAMBDA (IGNORE) (GENSYM)) VARLIST)))
     `(LET ,(MAPCAR 'LIST GENVARS VARS)
        (UNWIND-PROTECT (PROGN (SETQ . ,(MAPCAN 'LIST VARS VALS))
			       . ,BODY)
			(SETQ . ,(MAPCAN 'LIST VARS GENVARS))))))

;DEFUNP is like DEFUN but provides an implicit PROG.
;However, the value on falling off the end is the last thing in the body.

(DEFMACRO DEFUNP (FUNCTION ARGS &REST BODY
				&AUX (DEFAULT-CONS-AREA WORKING-STORAGE-AREA)
				     (LAST NIL))
  (SETQ BODY (APPEND BODY NIL))
  (SETQ LAST (LAST BODY))
  (COND ((OR (ATOM (CAR LAST)) (NOT (EQ 'RETURN (CAAR LAST))))
	 (RPLACA LAST (LIST 'RETURN (CAR LAST)))))
  `(DEFUN ,FUNCTION ,ARGS
     (PROG () . ,BODY)))

;This is a dummy DISPLACE for use in lisp-machine macros
;called in code being compiled on the PDP-10.
;If anyone starts wanting a real DISPLACE in QCMP
;(eg, for running interpretively part of QCMP
;which uses displacing macros)
;just flush this one and let the real one be used for this as well.
(IF-IN-MACLISP
(DEFUN DISPLACE (OLD NEW) NEW))

;(UNWIND-PROTECT risky-stuff forms-to-do-when-unwinding-this-frame)
;If risky-stuff returns, we return what it returns, doing forms-to-do
;(just as PROG1 would do).  If risky-stuff does a throw, we let the throw
;function as specified, but make sure that forms-to-do get done as well.
;forms-to-do can refer to UNWIND-PROTECT-TAG, which is the tag if
;a throw is happening, or NIL if risky-stuff is just returning.
;UNWIND-PROTECT is an fsubr in Maclisp.
(PROGN 'COMPILE
#Q
(DEFMACRO-DISPLACE UNWIND-PROTECT (BODY . UNDO-FN)
    `(MULTIPLE-VALUE-BIND (UNWIND-PROTECT-VALUE UNWIND-PROTECT-TAG
			   UNWIND-PROTECT-COUNT UNWIND-PROTECT-ACTION)
	 (*CATCH T ,BODY)
	 ,@UNDO-FN
	 (AND UNWIND-PROTECT-TAG  ;Continue whatever type of throw or unwind was happening
	      (*UNWIND-STACK UNWIND-PROTECT-TAG UNWIND-PROTECT-VALUE
			     UNWIND-PROTECT-COUNT UNWIND-PROTECT-ACTION))
	 UNWIND-PROTECT-VALUE))
)

(DEFMACRO-DISPLACE CATCH-ALL BODY
   `(*CATCH NIL (PROGN . ,BODY)))

;(IF test then-action else-action)
(DEFMACRO-DISPLACE IF (TEST THEN &REST ELSES)
   (COND ((NULL TEST) (AND ELSES `(PROGN . ,ELSES)))	;macros can generate this case...
	 ((EQ TEST T) THEN)			;and this one (avoids compiler error msg)
	 (T `(COND (,TEST ,THEN) (T . ,(OR ELSES '(NIL)))))))

;;; (CHECK-ARG STRING STRINGP "a string") signals an error if STRING is not a string.
;;; The error signals condition :WRONG-TYPE-ARGUMENT with arguments
;;; which are STRINGP (the predicate), the value of STRING (the losing value),
;;; the name of the argument (STRING), and the string "a string".
;;; If you try to proceed and do not supply a valid string to replace it,
;;; the error happens again.
;;; The second form may be the name of a predicate function, or it may be a full
;;; predicate form, as in:
;;; (CHECK-ARG A (AND (NUMBERP A) (< A 10.) (> A 0.)) "a number from one to ten" ONE-TO-TEN)
;;; ONE-TO-TEN is a symbol for the "type" which the argument failed to be.
;;; It is used instead of the second argument (the predicate) when signalling the error,
;;; since the second argument is not a suitable symbol.
;;; The value returned by CHECK-ARG is the argument's (original or respecified) value.
;;; In general, the condition :WRONG-TYPE-ARGUMENT is signalled with arguments
;;;    (1) A symbol for the desired type (NIL if not supplied)
;;;    (2) The bad value
;;;    (3) The name of the argument
;;;    (4) A string for the desired type.
(DEFMACRO-DISPLACE CHECK-ARG (ARG-NAME PREDICATE TYPE-STRING &OPTIONAL ERROR-TYPE-NAME)
    (AND (NULL ERROR-TYPE-NAME)
	 (SYMBOLP PREDICATE)
	 (SETQ ERROR-TYPE-NAME PREDICATE))
    `(DO () (,(COND ((SYMBOLP PREDICATE)
                     `(,PREDICATE ,ARG-NAME))
                    (T PREDICATE))
             ,ARG-NAME)
	 (SETQ ,ARG-NAME
	       (CERROR T NIL ':WRONG-TYPE-ARGUMENT
		       "The argument ~2G~A was ~1G~S, which is not ~3G~A"
		       ',ERROR-TYPE-NAME ,ARG-NAME ',ARG-NAME ',TYPE-STRING))))

;(KEYWORD-EXTRACT <keylist> KEY '(FOO (UGH BLETCH) BAR) '(FLAG FALG) <otherwise> ...)
;parses a TV-DEFINE-PC-PPR style list of alternating keywords and values, <keylist>.
;The symbol KEY is bound internally to the name of the next keyword to be tested.
;The keywords recognized are :FOO, :BAR and UGH;  whatever follows
;the keyword UGH is put in the variable BLETCH, whatever follows the
;keyword :FOO is put in the variable FOO, and similar for BAR.
;The flags are :FLAG and :FALG;  if :FLAG is seen, FLAG is set to T.
;<otherwise> is one or more SELECTQ clauses which can be used
;to recognize whatever else you like, in nonstandard format.
;To gobble the next thing from the <keylist>, say (FETCHR KEY).
;Note that by default the actual keywords are in the user package and
;the variables are in the current package.  Because of this, you
;cannot compile except on the real machine unless you restrict yourself
;to specifying the keywords and variables, both, as in (UGH BLETCH).
;That is ok, since code written any other way which put the keywords in
;the user package as it should would require colons and have teh same problem.
(DEFMACRO-DISPLACE KEYWORD-EXTRACT (KEYLIST KEYVAR KEYWORDS FLAGS &REST OTHERWISE)
    `(ITER-FETCHR ((MAPC ,KEYVAR ,KEYLIST)) NIL
	   (SELECTQ KEY
		    ,@(MAPCAR (FUNCTION (LAMBDA (KEYWORD)
				  (COND ((ATOM KEYWORD)
					 `(,(INTERN (STRING KEYWORD) "USER")
					   (SETQ ,KEYWORD (FETCHR ,KEYVAR))))
					(T `(,(CAR KEYWORD)
					     (SETQ ,(CADR KEYWORD) (FETCHR ,KEYVAR)))))))
			      KEYWORDS)
		    ,@(MAPCAR (FUNCTION (LAMBDA (KEYWORD)
				  `(,(INTERN (STRING KEYWORD) "USER")
				    (SETQ ,KEYWORD T))))
			      FLAGS)
		    . ,OTHERWISE)))

;PSETQ looks like SETQ but does its work in parallel.
(DEFMACRO-DISPLACE PSETQ (&REST REST)
     (COND ((CDDR REST)
            ;; Not the last pair.
            ;; Improve the efficiency of DO-stepping by detecting
            ;; that a variable is being set to its CDR or its 1+,
            ;; and doing all such variables last.
            ;; That makes it possible to do all of them with SETE-CDR.
            (COND ((AND (LISTP (CADR REST))
                        (MEMQ (CAADR REST) '(1+ CDR))
                        (EQ (CADADR REST) (CAR REST)))
                   `(PROGN (PSETQ . ,(CDDR REST))
                     (SETQ ,(CAR REST) ,(CADR REST))))
                  ;; Not set to its own 1+ or CDR;  do it the general way.
                  (T
                   `(SETQ ,(CAR REST) (PROG1 ,(CADR REST) (PSETQ . ,(CDDR REST)))))))
           ;; The last pair.  Keep it simple;  no superfluous (PROG1 (SETQ...) (PSETQ)).
           ((CDR REST)
            `(SETQ . ,REST))))

;For things which want to do a tail-recursive call, passing back multiple
;values.  This does not work in the interpreter.  This is a temporary measure
;and will go away when the calling protocol is changed to always pass back
;multiple values on "tail recursive" calls.
(DEFMACRO-DISPLACE MULTIPLE-VALUE-CALL ((FUNCTION . ARGS))
  `(PROGN (%OPEN-CALL-BLOCK (FUNCTION ,FUNCTION) 0 4) ;No ADI, destination-return
	  (%ASSURE-PDL-ROOM ,(LENGTH ARGS))
	  ,@(MAPCAR '(LAMBDA (A) `(%PUSH ,A)) ARGS)
	  (%ACTIVATE-OPEN-CALL-BLOCK)))

;(LOCAL-DECLARE ((SPECIAL FOO) (UNSPECIAL BAR)) code)
;declares FOO and BAR locally within <code>.
;LOCAL-DECLARE can also be used by macros to pass information down
;to other macros that expand inside the code they produce.
;The list of declarations (in this case, ((MUMBLE FOO BAR))) is appended
;onto the front of LOCAL-DECLARATIONS, which can be searched by
;macros expending inside of <code>.
(DEFMACRO-DISPLACE LOCAL-DECLARE (DECLARATIONS &REST BODY)
    `(COMPILER-LET ((LOCAL-DECLARATIONS (APPEND ',DECLARATIONS LOCAL-DECLARATIONS)))
		   . ,BODY))

;INHIBIT-STYLE-WARNINGS inhibits compiler style checking of what is inside it.
;In the interpreter, it is a no-op.
(DEFMACRO-DISPLACE INHIBIT-STYLE-WARNINGS (BODY)
    BODY)

;(ERROR-RESTART .... (CERROR ...) ...) causes a request by the user
;or error handler to "restart" after the error to re-execute all the
;code inside the ERROR-RESTART.
(DEFMACRO-DISPLACE ERROR-RESTART (&REST BODY)
   `(PROG ()
	  LOOP
	  (*CATCH 'ERROR-RESTART (RETURN (PROGN . ,BODY)))
	  (GO LOOP)))

;(LET-CLOSED (variables as in LET) initializations ... (FUNCTION ..))
;binds the variables and executes the initialization,
;then returns the last thing in the body, closed over those variables.
(DEFMACRO-DISPLACE LET-CLOSED (VARS &REST BODY)
    (LET ((VARNAMES (MAPCAR (FUNCTION (LAMBDA (V) (COND ((ATOM V) V) (T (CAR V))))) VARS)))
	 `(LOCAL-DECLARE ((SPECIAL . ,VARNAMES))
		 (LET ,VARS
		      (CLOSURE ',VARNAMES (PROGN . ,BODY))))))

;(DEF-OPEN-CODED FOO-COMPONENT (CURRY-AFTER AR-1 5))
;defines FOO-COMPONENT as an open-coded function with that definition.
(DEFMACRO DEF-OPEN-CODED (FUNCTION DEFINITION)
    `(PROGN 'COMPILE
	    (EVAL-WHEN (COMPILE)
		       (PUSH '(OPEN-CODE ,FUNCTION ,DEFINITION) LOCAL-DECLARATIONS))
	    (FSET-CAREFULLY ',FUNCTION ',DEFINITION)
	    (DEFPROP ,FUNCTION T 'OPEN-CODE)))

;Say that FUNCTION should be open-coded by the compiler as DEFINITION
;without changing FUNCTION's real definition.
;A call to this OPEN-CODE can be used as a local declaration, too.
;Giving NIL as the definition turns off open-coding.
(DEFMACRO OPEN-CODE (FUNCTION DEFINITION)
    `(PROGN 'COMPILE
	    (EVAL-WHEN (COMPILE)
		       (PUSH '(OPEN-CODE ,FUNCTION ,DEFINITION) LOCAL-DECLARATIONS))
	    (DEFPROP ,FUNCTION ,DEFINITION 'OPEN-CODE)))

;(DEFSUBST FOO (X) (AR-1 X 5)) is like a similar DEFUN
;except that the definition of FOO will be substituted in at compile time
;and FOO's argument variables eliminated by substitution.
;It is your responsibility to make sure that FOO's args
;are evaluated exactly once, in the right ordr, in FOO's body,
;and that the symbols used for the args do not appear except
;to represent the args.
(DEFMACRO DEFSUBST (FUNCTION LAMBDA-LIST . BODY)
  (LET ((DEF1 `(SUBST ,LAMBDA-LIST . ,BODY)))
    `(PROGN 'COMPILE
	    (EVAL-WHEN (COMPILE) (PUSH '(DEF ,FUNCTION . ,DEF1) LOCAL-DECLARATIONS))
	    (FSET-CAREFULLY ',FUNCTION ',DEF1))))

;Make a variable special and, optionally, initialize it.
;This is recorded as a definition by TAGS and ZWEI.
(DEFMACRO DEFVAR (VARIABLE &OPTIONAL (INITIAL-VALUE NIL INITIALIZE-P))
  `(PROGN 'COMPILE
	(SPECIAL ,VARIABLE)
	(RECORD-SOURCE-FILE-NAME ',VARIABLE)
	,(AND INITIALIZE-P    
	      ;Initialize in a way that works in the cold-load
	      ;Don't evaluate INITIAL-VALUE unless used
	      `(SETQ-IF-UNBOUND ,VARIABLE ,INITIAL-VALUE))))

;Similar to DEFVAR, but if initialization given, always use it (not just if
;variable was previously unbound).
(DEFMACRO DEFCONST (VARIABLE &OPTIONAL (INITIAL-VALUE NIL INITIALIZE-P))
  `(PROGN 'COMPILE
	(SPECIAL ,VARIABLE)
	(RECORD-SOURCE-FILE-NAME ',VARIABLE)
	,(AND INITIALIZE-P    
	      ;Initialize in a way that works in the cold-load
	      ;Don't evaluate INITIAL-VALUE unless used
	      `(SETQ ,VARIABLE ,INITIAL-VALUE))))

;Performs a sequence of operations while inhibiting scheduling
(DEFMACRO-DISPLACE WITHOUT-INTERRUPTS (&REST FORMS)
  `(LET ((INHIBIT-SCHEDULING-FLAG T))
     . ,FORMS))

;MAPC with a prog body instead of a function.  <form> evaluates to a list,
;and <body> is executed with <var> bound to successive elements of the list.
(DEFMACRO-DISPLACE DOLIST ((VAR FORM) &REST BODY)
   (LET ((DUMMY (GENSYM))) 
     `(DO ((,DUMMY ,FORM (CDR ,DUMMY))
           (,VAR))
          ((NULL ,DUMMY))
         (SETQ ,VAR (CAR ,DUMMY))
         . ,BODY)))

;Repeat a number of times.  <form> evaluates to the number of times,
;and <body> is executed with <var> bound to 0, 1, ...
;Don't generate dummy variable if <form> is an integer.  We could also do this
;if <form> were a symbol, but the symbol may get clobbered inside the body,
;so the behavior of the macro would change.
(DEFMACRO-DISPLACE DOTIMES ((VAR FORM) &REST BODY &AUX DUMMY)
  (COND ((FIXP FORM)
	 `(DO ((,VAR 0 (1+ ,VAR)))
	      (( ,VAR ,FORM))
	    . ,BODY))
	(T (SETQ DUMMY (GENSYM))
	   `(DO ((,VAR 0 (1+ ,VAR))
		 (,DUMMY ,FORM))
		(( ,VAR ,DUMMY))
	      . ,BODY))))
