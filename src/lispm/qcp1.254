;This file contains pass 1 and the top level of the Lisp machine Lisp compiler  -*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

(DECLARE (COND ((STATUS FEATURE LISPM))
	       ((NULL (MEMQ 'NEWIO (STATUS FEATURES)))
		(BREAK 'YOU-HAVE-TO-COMPILE-THIS-WITH-QCOMPL T))
	       ((NULL (GET 'IF-FOR-MACLISP 'MACRO))
		(LOAD '(MACROS > DSK LISPM))
		(LOAD '(DEFMAC FASL DSK LISPM2))
		(LOAD '(LMMAC > DSK LISPM2))
		(INCLUDE |LISPM;QCDEFS >|))))

(DECLARE (SETQ OPEN-CODE-MAP-SWITCH T
	       RUN-IN-MACLISP-SWITCH T))

(DECLARE (SPECIAL MC-HOLDPROG ULAP-DEBUG LAP-DEBUG QC-PREVIOUS-CONS-AREA
		  MS-HOLDPROG MSLAP-DEBUG FUNCTIONS-DEFINED
		  TRACE-TABLE QC-TF-PROCESSING-MODE QC-TF-OUTPUT-MODE))

;This function is used to dump out a compiler.  When restarted,
; if JCL is given, that file will be read and executed.
(IF-FOR-MACLISP
(DEFUN PDUMP ()
 (PROG (JCL)
   (SUSPEND '|:PDUMP LISPM1;TS QCMP|) ;user can hit carriage return or control-D
   (COND ((SETQ JCL (STATUS JCL))
	  (LOAD (MERGEF (NAMELIST (MAKNAM JCL)) `((DSK ,(STATUS UDIR)) * >))))
	 (T (RETURN 'READY)))))
)

(IF-FOR-MACLISP
(DEFUN CIRCULAR-LIST N
   ((LAMBDA (TEM)
	    (RPLACD (LAST TEM) TEM)
	    TEM)
    (APPEND (LISTIFY N) NIL)))
)
(DECLARE (*EXPR MEMQL BUTLAST ADRREFP LDIFF GETARGDESC P2 P2SBIND
		OUTF BARF ULAP MICRO-COMPILE QLAPP))

;Initialize all global variables and compiler switches, and make sure
;that some built in variables are known to be special
;(logically, the cold load would contain SPECIAL properties for them,
;but this function is how they actually get put on).
(DEFUN QC-PROCESS-INITIALIZE NIL 
	(SETQ HOLDPROG T)
	(SETQ MC-HOLDPROG T)
	(SETQ ULAP-DEBUG NIL)
	(SETQ LAP-DEBUG NIL)
	(SETQ MS-HOLDPROG T)
	(SETQ MSLAP-DEBUG NIL)
	(SETQ FUNCTION-BEING-PROCESSED NIL)  ;FOR ERROR PRINTOUTS.  AVOID ANY UNBOUND PROBLEMS
	(SETQ QCMP-OUTPUT NIL)
	(SETQ MACROLIST NIL)
	(SETQ RETAIN-VARIABLE-NAMES-SWITCH 'ARGS)
	(SETQ OPEN-CODE-MAP-SWITCH T)
	(SETQ ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH NIL)
	(SETQ ALL-SPECIAL-SWITCH NIL)
	(SETQ OBSOLETE-FUNCTION-WARNING-SWITCH T)
	(SETQ RUN-IN-MACLISP-SWITCH NIL)
	(SETQ INHIBIT-STYLE-WARNINGS-SWITCH NIL)
	(OR (BOUNDP 'QCOMPILE-TEMPORARY-AREA)
	    (SETQ QCOMPILE-TEMPORARY-AREA FASD-TEMPORARY-AREA))

	(IF-FOR-LISPM
	  (COND ((NULL (GET 'CDR-NIL 'SYSTEM-CONSTANT))
		 (MAPC (FUNCTION (LAMBDA (Y) 
			   (MAPC (FUNCTION (LAMBDA (X) 
				     (PUTPROP X T 'SYSTEM-CONSTANT)))
				 (SYMEVAL Y)) )) 
		       SYSTEM-CONSTANT-LISTS)
		 (MAPC (FUNCTION (LAMBDA (Y) 
			   (MAPC (FUNCTION (LAMBDA (X) 
				     (PUTPROP X T 'SPECIAL)))
				 (SYMEVAL Y)) )) 
		       SYSTEM-VARIABLE-LISTS))))
)

;; Compile a function which already has an interpreted definition,
;; or define it to a newly supplied definition's compilation.
;; If the definition is one which is legal but cannot meaningfully
;; be compiled, we just leave it unchanged.
(IF-FOR-LISPM
(DEFUN COMPILE (NAME &OPTIONAL LAMBDA-EXP)
     (AND QC-FILE-IN-PROGRESS	;Check for condition likely to cause temporary area lossage
	  (FORMAT ERROR-OUTPUT "~&COMPILE: Compiler recursively entered, you may lose.~%"))
     (PROG (TEM LAST-ERROR-FUNCTION BARF-SPECIAL-LIST FUNCTIONS-REFERENCED FUNCTIONS-DEFINED)
	(QC-PROCESS-INITIALIZE)
	(RESET-TEMPORARY-AREA FASD-TEMPORARY-AREA)
	(COND (LAMBDA-EXP)
	      ((AND (FDEFINEDP NAME)
		    (LISTP (FDEFINITION NAME)))
		(SETQ LAMBDA-EXP (FDEFINITION NAME)))
	      ((AND (SYMBOLP NAME)
                    (SETQ TEM (GET NAME ':PREVIOUS-EXPR-DEFINITION)))
	       (SETQ LAMBDA-EXP TEM))
	      (T (FERROR NIL "Can't find LAMBDA expression for ~S" NAME)))
	(COMPILE-1 NAME LAMBDA-EXP)
	(RETURN NAME)))
 )

(IF-FOR-LISPM		;Compile while already inside compiler environment
(DEFUN COMPILE-1 (NAME LAMBDA-EXP &OPTIONAL (PROCESSING-MODE 'MACRO-COMPILE))
  (COND ((NLISTP LAMBDA-EXP)
	 (FDEFINE NAME LAMBDA-EXP T))
	((OR (MEMQ (CAR LAMBDA-EXP) '(LAMBDA NAMED-LAMBDA))
	     (AND (EQ (CAR LAMBDA-EXP) 'MACRO)
		  (LISTP (CDR LAMBDA-EXP))
		  (MEMQ (CADR LAMBDA-EXP) '(LAMBDA NAMED-LAMBDA))))
	 (QC-TRANSLATE-FUNCTION NAME LAMBDA-EXP PROCESSING-MODE 'COMPILE-TO-CORE))
	(T (FDEFINE NAME LAMBDA-EXP T))))
 )

;; Restore the saved old interpreted definition of a function on which
;; COMPILE was used.

(IF-FOR-LISPM 
(DEFUN UNCOMPILE (NAME)			;PUT INTERPRETED BACK ON FUNCTION CELL
   (COND ((AND (BOUNDP 'TRACE-TABLE) (ASSQ NAME TRACE-TABLE))
	  "Untrace it first")
	 ((AND (GET NAME ':PREVIOUS-EXPR-DEFINITION)
	       (NOT (AND (FBOUNDP NAME)
			 (LISTP (FSYMEVAL NAME))
			 (NOT (AND (EQ (CAR (FSYMEVAL NAME)) 'MACRO)
				   (= (%DATA-TYPE (CDR (FSYMEVAL NAME))) DTP-FEF-POINTER))))))
	  (FSET NAME (GET NAME ':PREVIOUS-EXPR-DEFINITION))
	  T)
	 (T "Not compiled"))) )

;; Compile one function.  All styles of the compiler come through here.
;; QC-TF-PROCESSING-MODE should be MACRO-COMPILE or MICRO-COMPILE.
;; QC-TF-OUTPUT-MODE is used by LAP to determine where to put the compiled code.
;; It is COMPILE-TO-CORE for making an actual FEF, QFASL, or REL.
;; EXP is the lambda-expression.

(DEFUN QC-TRANSLATE-FUNCTION (FUNCTION-BEING-PROCESSED EXP
			      QC-TF-PROCESSING-MODE QC-TF-OUTPUT-MODE)
   (LET ((QCMP-OUTPUT)
   	 (ERROR-MESSAGE-HOOK
	   (IF-FOR-LISPM
	    (CLOSURE '(FUNCTION-BEING-PROCESSED) ;Since called in other SG
	             (FUNCTION (LAMBDA ()
		         (AND FUNCTION-BEING-PROCESSED
			      (FORMAT T "Error occurred while compiling ~S"
				      FUNCTION-BEING-PROCESSED)))))))
	 (QCOMPILE-POST-PROC (FUNCTION QC-TRANSLATE-FUNCTION-POST-PROC)))
	(IF-FOR-LISPM
	   (COND ((NOT (FBOUNDP 'QCMP-OUTPUT))
		  (FSET 'QCMP-OUTPUT
			(MAKE-ARRAY WORKING-STORAGE-AREA 'ART-Q-LIST 1000 NIL '(0))))
		 (T (STORE-ARRAY-LEADER 0 (FUNCTION QCMP-OUTPUT) 0))))
        (LET ((QC-PREVIOUS-CONS-AREA DEFAULT-CONS-AREA)
	      #Q (DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA))
	     (QCOMPILE0 EXP FUNCTION-BEING-PROCESSED
			(EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE))
	     (IF-FOR-LISPM
	        (SETQ QCMP-OUTPUT (G-L-P (FUNCTION QCMP-OUTPUT))))
             (AND PEEP-ENABLE (PEEP QCMP-OUTPUT))
	     (QC-TRANSLATE-FUNCTION-POST-PROC))
	T))

(DEFUN QC-TRANSLATE-FUNCTION-POST-PROC ()
  (LET ((DEFAULT-CONS-AREA QC-PREVIOUS-CONS-AREA))
    (COND ((EQ QC-TF-PROCESSING-MODE 'MACRO-COMPILE)
	   (QLAPP QCMP-OUTPUT QC-TF-OUTPUT-MODE))
	  ((EQ QC-TF-PROCESSING-MODE 'MICRO-COMPILE)
	   (MICRO-COMPILE QCMP-OUTPUT QC-TF-OUTPUT-MODE)
	   ))))

;Compile an internal lambda which must be passed as an argument
;into a separate function, which has its own name, a gensym
;not known to the user.  That name is returned.
(IF-FOR-MACLISP
 (DEFUN BREAKOFF (X)
       (PROG (FNAME)
	 (SETQ FNAME (IF (EQ (CAR X) 'NAMED-LAMBDA)
			 (IF (ATOM (CADR X)) (CADR X) (CAADR X))
			 (IMPLODE (NCONC (EXPLODEN FUNCTION-BEING-PROCESSED)
					 (EXPLODEN '-INTERNAL-)
					 (EXPLODEN (GENSYM))))))
    	 (COND (QCMP-OUTPUT (BARF QCMP-OUTPUT
				  'QCMP-OUTPUT-NON-NIL-AT-INTERNAL-RECURSION
				  'WARN)))
	 (QCOMPILE0 X FNAME GENERATING-MICRO-COMPILER-INPUT-P)
 	 (FUNCALL QCOMPILE-POST-PROC)
	 (SETQ QCMP-OUTPUT NIL)		;RESET OUTPUT
	 (RETURN FNAME))))

(IF-FOR-LISPM
(DEFUN BREAKOFF (X)
       (PROG (FNAME)
	 (SETQ FNAME (IF (EQ (CAR X) 'NAMED-LAMBDA)
			 (IF (ATOM (CADR X)) (CADR X) (CAADR X))
			 (INTERN (FORMAT NIL "~S-INTERNAL-~S" ;Shouldn't really use GENSYM!
					 FUNCTION-BEING-PROCESSED (GENSYM)))))
    	 (COND ((NOT (ZEROP (ARRAY-LEADER (FUNCTION QCMP-OUTPUT) 0)))
		(BARF NIL 'QCMP-OUTPUT-NON-EMPTY-AT-INTERNAL-RECURSION 'WARN)))
	 (QCOMPILE0 X FNAME GENERATING-MICRO-COMPILER-INPUT-P)
    	 (SETQ QCMP-OUTPUT (G-L-P (FUNCTION QCMP-OUTPUT)))
 	 (FUNCALL QCOMPILE-POST-PROC)
	 (STORE-ARRAY-LEADER 0 (FUNCTION QCMP-OUTPUT) 0)
	 (RETURN FNAME))))

;; Given a function, break it or parts of it off if appropriate
;; and return the result.  If no part needs breaking off, return NIL.
;; (OR (MAYBE-BREAKOFF X) X) returns X, broken off as appropriate.
(DEFUN MAYBE-BREAKOFF (FUNCTION)
    (COND ((ATOM FUNCTION) NIL)
	  ((MEMQ (CAR FUNCTION) '(LAMBDA NAMED-LAMBDA))
	   (BREAKOFF FUNCTION))
	  ((MEMQ (CAR FUNCTION) '(CURRY-BEFORE CURRY-AFTER))
	   (LET ((TEM (MAYBE-BREAKOFF (CADR FUNCTION))))
	       (AND TEM `(,(CAR FUNCTION) ,TEM . ,(CDDR FUNCTION)))))))

;QCOMPILE0 compiles one function, producing a list of lap code in QCMP-OUTPUT.
;The first argument is the lambda-expression which defines the function.
;  It must actually be a LAMBDA or NAMED-LAMBDA.  Other things are not allowed.
;The second argument is the name of the function.
;The third won't be useful till there's a microcompiler.

;QCOMPILE-POST-PROC is a free variable which is used to process the results
; of recursive calls on the compiler (to compile lambdas which must be made
; into separate functions).

;We expect that DEFAULT-CONS-AREA has been bound to QCOMPILE-TEMPORARY-AREA.
;The compiler does ALL consing in that temporary area unless it specifies otherwise.

(DEFUN QCOMPILE0 (EXP NAME GENERATING-MICRO-COMPILER-INPUT-P)  ;COME DIRECTLY
 (LET ((LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
  (PROG (VARS EXP1 ARGN LVCNT		  ;HERE ON INTERNAL RECURSION
	 PDLLVL MAXPDLLVL CALL-BLOCK-PDL-LEVELS
	 ALLGOTAGS GOTAGS TLEVEL P1VALUE BINDP
	 DROPTHRU ALLVARS FREEVARS RETPROGDESC
	 PROGDESCS LL TAGOUT TLFUNINIT SPECIALFLAG MACROFLAG
	 LOCAL-MAP ARG-MAP DOCUMENTATION EXPR-DEBUG-INFO
	 FAST-ARGS-POSSIBLE)
       (SETQ PDLLVL 0)	;RUNTINE LOCAL PDLLVL
       (SETQ DROPTHRU T)		;CAN DROP IN IF FALSE, FLUSH STUFF TILL TAG OR
       (SETQ MAXPDLLVL 0)			;DEEPEST LVL REACHED BY LOCAL PDL
       (SETQ TLEVEL T)
       (SETQ P1VALUE T)
       (SETQ FAST-ARGS-POSSIBLE T)
       ;; If compiling a macro, compile its expansion function
       ;; and direct lap to construct a macro later.
       (COND ((EQ (CAR EXP) 'MACRO)
	      (SETQ MACROFLAG T)
	      (SETQ EXP (CDR EXP))))
       (OR (EQ (CAR EXP) 'LAMBDA)
	   (EQ (CAR EXP) 'NAMED-LAMBDA)
	   (BARF EXP '|not a function| 'DATA))
       (SETQ FUNCTIONS-DEFINED
             (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
                (CONS NAME FUNCTIONS-DEFINED)))
       (SETQ EXP1 EXP)
       ;; If a NAMED-LAMBDA, discard the name and save debug-info in special place.
       (AND (EQ (CAR EXP1) 'NAMED-LAMBDA)
	    (SETQ EXPR-DEBUG-INFO
		  (AND (NOT (ATOM (CADR EXP1)))
		       (SUBSET #'(LAMBDA (X) (MEMQ (CAR X) '(ARGLIST RETURN-LIST)))
			       (CDADR EXP1)))
		  EXP1 (CDR EXP1)))
       (SETQ LL (CADR EXP1))	;lambda list.
       (SETQ EXP1 (CDDR EXP1))	;body
       ;; If there is a documentation string at the front
       ;; before the declaration, flush it.
       (AND (CDR EXP1)
	    (STRINGP (CAR EXP1))
	    (SETQ DOCUMENTATION (CAR EXP1) EXP1 (CDR EXP1)))
       ;; If the first thing in the body is (DECLARE (FOO X) (BAR Y)),
       ;; take the (FOO X) and (BAR Y) as local declarations for the whole function.
       (AND (NOT (ATOM (CAR EXP1)))
	    (EQ (CAAR EXP1) 'DECLARE)
	    (SETQ LOCAL-DECLARATIONS (APPEND (CDAR EXP1) LOCAL-DECLARATIONS)
		  EXP1 (CDR EXP1)))
       ;; Allow a documentation string after a declaration also.
       (AND (NULL DOCUMENTATION)
	    (CDR EXP1)
	    (STRINGP (CAR EXP1))
	    (SETQ DOCUMENTATION (CAR EXP1) EXP1 (CDR EXP1)))
       ;; Now put the remaining body together with the lambda list again, and
       ;; turn any &AUX variables in the LAMBDA into a PROG in the body.
       (SETQ EXP1 (P1AUX `(LAMBDA ,LL . ,EXP1)))
       ;; Separate lambda list and body again.
       (SETQ LL (CADR EXP1) EXP1 (CDDR EXP1))
       ;; Now process the variables in the lambda list, after the local declarations.
       (SETQ LL (P1SBIND LL 'FEF-ARG-REQ NIL NIL))
       (COND ((NOT (NULL (CDR EXP1)))
	      (SETQ EXP1 (CONS 'PROGN EXP1)))
	     ((SETQ EXP1 (CAR EXP1))))
       (SETQ EXP1 (P1 EXP1))		;DO PASS 1 TO SINGLE-EXPRESSION BODY
       (ASSIGN-LAP-ADDRESSES)
       (DO VARS1 ALLVARS (CDR VARS1) (NULL VARS1)
	   (COND ((OR (SAMEPNAMEP (VAR-NAME (CAR VARS1)) 'IGNORE)
		      (SAMEPNAMEP (VAR-NAME (CAR VARS1)) 'IGNORED))
		  (OR (ZEROP (VAR-USE-COUNT (CAR VARS1)))
		      (BARF (VAR-NAME (CAR VARS1)) '|bound but not ignored| 'WARN)))
		 ((NOT (SAMEPNAMEP (VAR-NAME (CAR VARS1)) 'OPERATION))
		  (AND (ZEROP (VAR-USE-COUNT (CAR VARS1)))
		       (EQ (VAR-TYPE (CAR VARS1)) 'FEF-LOCAL)
		       (BARF (VAR-NAME (CAR VARS1))
			     '|bound but never used| 'WARN)))))
       (OUTF (LIST 'MFEF NAME SPECIALFLAG (REVERSE ALLVARS) FREEVARS))
       (AND MACROFLAG (OUTF '(CONSTRUCT-MACRO)))
       (OUTF '(QTAG S-V-BASE))
       (OUTF '(S-V-BLOCK))
       (OUTF '(QTAG DESC-LIST-ORG))
       (OUTF '(A-D-L))
       (OUTF (LIST 'PARAM 'LLOCBLOCK LVCNT))
       (OUTF '(QTAG QUOTE-BASE))
       (OUTF '(ENDLIST))			;LAP WILL INSERT QUOTE VECTOR HERE
       ;; If there is a (LOCAL-DECLARE ((ARGLIST FOO &OPTIONAL BAR)) ...)
       ;; around this defun, put (ARGLIST (FOO &OPTIONAL BAR)) in the DEBUG-INFO.
       (LET ((ARGL (CDR (ASSQ 'ARGLIST LOCAL-DECLARATIONS)))
	     (VALUE-NAME-LIST (CDR (ASSQ 'RETURN-LIST LOCAL-DECLARATIONS)))
             (DEBUG-INFO))
         (COND (ARGL (PUSH `(ARGLIST ,ARGL) DEBUG-INFO))
	       ((SETQ ARGL (ASSQ 'ARGLIST EXPR-DEBUG-INFO))
		(PUSH ARGL DEBUG-INFO)))
	 (COND (VALUE-NAME-LIST (PUSH `(RETURN-LIST ,VALUE-NAME-LIST) DEBUG-INFO))
	       ((SETQ VALUE-NAME-LIST (ASSQ 'RETURN-LIST EXPR-DEBUG-INFO))
		(PUSH VALUE-NAME-LIST DEBUG-INFO)))
	 (AND DOCUMENTATION (PUSH `(:DOCUMENTATION ,DOCUMENTATION) DEBUG-INFO))
         ;; Include the local and arg maps if we have them.
         ;; They were built by ASSIGN-LAP-ADDRESSES.
         (AND LOCAL-MAP (PUSH `(LOCAL-MAP ,LOCAL-MAP) DEBUG-INFO))
         (AND ARG-MAP (PUSH `(ARG-MAP ,ARG-MAP) DEBUG-INFO))
         (AND DEBUG-INFO
              (OUTF `(DEBUG-INFO . ,DEBUG-INFO))))
       (OUTF 'PROGSA)
       (P2SBIND LL VARS NIL)			;CAN COMPILE INITIALIZING CODE
       (P2 EXP1 'D-RETURN)			;DO PASS 2
       (OUTF (LIST 'PARAM 'MXPDL (1+ MAXPDLLVL)))
       (RETURN T))))

;Pass 1.
;We expand all macros and perform source-optimizations
;according to the OPTIMIZERS properties.  Internal lambdas turn into progs.
;Free variables are made special and put on FREEVARS.
;PROGs are converted into an internal form which contains pointers
;to the VARS and GOTAGS lists of bound variables and prog tags.
;All self-evaluating constants (including T and NIL) are replaced by
;quote of themselves.
;P1VALUE is NIL when compiling a form whose value is to be discarded.
;Some macros and optimizers look at it.

(DEFUN P1V (FORM)
    (LET ((P1VALUE T))
       (P1 FORM)))

(DEFUN P1 (FORM)
       (PROG (TM)
	     (SETQ FORM (OPTIMIZE FORM T))
	     (COND
	      ((CONSTANTP FORM) (RETURN (LIST 'QUOTE FORM)))
	      ((ATOM FORM)
	       (RETURN (COND ((SETQ TM (ASSQ FORM VARS))
			      (VAR-INCREMENT-USE-COUNT TM)
			      FORM)
			     (T (MAKESPECIAL FORM) FORM))))
	      ((EQ (CAR FORM) 'QUOTE) (RETURN  FORM))
              ;; Certain constructs must be checked for here
              ;; so we can call P1 recursively without setting TLEVEL to NIL.
              ((NOT (ATOM (CAR FORM)))
	       (SELECTQ (CAAR FORM)
		   ((LAMBDA NAMED-LAMBDA)
		    (RETURN (P1LAMBDA (CAR FORM) (CDR FORM))))
		   (OTHERWISE
		    ;; Old Maclisp evaluated functions.
		    (BARF FORM '|function is a form to be evaluated; use FUNCALL| 'WARN)
		    (RETURN (P1 `(FUNCALL . ,FORM))))))
#Q	      ((NOT (EQ (TYPEP (CAR FORM)) ':SYMBOL))
	       (BARF (CAR FORM) '|function is not a symbol or list| 'WARN)
	       (RETURN (P1 (CONS 'PROGN (CDR FORM)))))
	      ((MEMQ (CAR FORM) '(PROG PROG* PPROG SPROG)) (RETURN (P1PROG FORM)))
              ((EQ (CAR FORM) 'RETURN)
	       (AND (CDDR FORM) (SETQ TLEVEL NIL))
	       (RETURN (P1EVARGS FORM)))
	      ((EQ (CAR FORM) '%POP)	;P2 specially checks for this
	       (RETURN FORM)))
	     (SETQ TLEVEL NIL)
	     ;; Check for functions with special P1 handlers.
	     (COND ((SETQ TM (GET (CAR FORM) 'P1))
		    (RETURN (FUNCALL TM FORM))))
	     (COND ((AND ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH
			 (ASSQ (CAR FORM) VARS)
			 (NULL (FUNCTION-P (CAR FORM))))
		    (BARF FORM
			  '|variable in function position; use FUNCALL|
			  'WARN)
		    (RETURN (P1 (CONS 'FUNCALL FORM)))))
	     (RETURN (P1ARGC FORM (GETARGDESC (CAR FORM))))))

(IF-FOR-MACLISP
 (DEFUN FUNCTION-P (X)
    (GETL X '(EXPR SUBR FEXPR FSUBR LSUBR Q-LAMBDA-LIST ARGDESC Q-ARGS-PROP
	     *FEXPR *EXPR *LEXPR AUTOLOAD))))

(IF-FOR-LISPM
(DEFUN FUNCTION-P (X)
    (OR (FBOUNDP X)
        (GETL X '(*EXPR AUTOLOAD)))))

(DEFUN MSPL2 (X)
       (COND ((NOT (SPECIALP X))
	      (BARF X '|declared special| 'WARN)
	      (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
		   (PUSH X BARF-SPECIAL-LIST))
	      (COND ((ASSQ X ALLVARS)
		     (BARF X '|previously assumed local; you will lose| 'WARN))))))

(DEFUN MAKESPECIAL (X)
       (MSPL2 X)
       (OR (MEMQ X FREEVARS)
	   (PUSH X FREEVARS))
       T)

;Given a form, apply optimizations and expand macros until no more is possible
;(at the top level).  Also apply style-checkers to the supplied input
;but not to generated output.
(DEFUN OPTIMIZE (FORM CHECK-STYLE)
       (PROG (TM)
	     (AND CHECK-STYLE (NULL INHIBIT-STYLE-WARNINGS-SWITCH)
		  (NOT (ATOM FORM))
		  (COND ((ATOM (CAR FORM))
			 (AND (SYMBOLP (CAR FORM))
			      (SETQ TM (GET (CAR FORM) 'STYLE-CHECKER))
			      (FUNCALL TM FORM)))
			((NOT RUN-IN-MACLISP-SWITCH))
			((OR (EQ (CAAR FORM) 'LAMBDA) (EQ (CAAR FORM) 'NAMED-LAMBDA))
			 (LAMBDA-STYLE (CAR FORM)))
			((MEMQ (CAAR FORM) '(CURRY-BEFORE CURRY-AFTER))
			 (BARF (CAAR FORM) '|does not work in Maclisp| 'WARN))))
	     ;; APPLY OPTIMIZATIONS, AND EXPAND MACROS, UNTIL THEY STOP CHANGING ANYTHING.
	     OPT
	     (AND (ATOM FORM) (RETURN FORM))
	     (AND (ATOM (CAR FORM)) (NOT (SYMBOLP (CAR FORM)))
		  (RETURN FORM))	;P1 will barf at it soon enough; don't get error
	     (OR (NOT (ATOM (CAR FORM)))
		 (AND (DO ((OPTS (GET (CAR FORM) 'OPTIMIZERS) (CDR OPTS)))
			  ((NULL OPTS) NIL)
			 (COND ((NEQ FORM (SETQ TM (FUNCALL (CAR OPTS) FORM)))
				;; Expression changed, so replace it and try again
				;; but first make sure the number of args was right
				;; so that optimizers don't have to check this manually
			     #Q (LET* ((AI (%ARGS-INFO (CAR FORM))))
				  (AND (< (LENGTH (CDR FORM)) (LDB %%ARG-DESC-MIN-ARGS AI))
				       (BARF FORM "too few arguments" 'WARN))
				  (AND (ZEROP (LDB %%ARG-DESC-QUOTED-REST AI))
				       (ZEROP (LDB %%ARG-DESC-EVALED-REST AI))
				       (> (LENGTH (CDR FORM)) (LDB %%ARG-DESC-MAX-ARGS AI))
				       (BARF FORM "too many arguments" 'WARN)))
				(SETQ FORM TM)		;EXPRESSION CHANGED, START OVER
				(RETURN T))))
		      (GO OPT)))
	     (OR (EQ FORM (SETQ FORM (MACROEXPAND-1 FORM T)))	;EXPAND ALL TOP LEVEL MACROS
		 (GO OPT))
	     (RETURN FORM)))

;(DEFOPTIMIZER FOO-BAR FOO (UGH BLETCH) ...)
;defines a function FOO-BAR which is an optimizer on FOO
;and expands the same way (DEFMACRO FOO (UGH BLETCH) ...) would expand.
(DEFMACRO DEFOPTIMIZER (OPTIMIZER-NAME TARGET-FUNCTION LAMBDA-LIST &REST BODY)
    `(PROGN 'COMPILE
	    ,(SI:DEFMACRO1 `(DEFMACRO ,OPTIMIZER-NAME ,LAMBDA-LIST . ,BODY) 'DEFUN)
	    (ADD-OPTIMIZER ,TARGET-FUNCTION ,OPTIMIZER-NAME)))

;(ADD-OPTIMIZER FOO BAR) puts FOO on BAR's optimizers list if it isn't there already.
(DEFUN ADD-OPTIMIZER (&QUOTE TARGET-FUNCTION OPTIMIZER-NAME)
    (LET ((OPTS (GET TARGET-FUNCTION 'OPTIMIZERS)))
	 (OR (MEMQ OPTIMIZER-NAME OPTS)
	     (PUTPROP TARGET-FUNCTION (CONS OPTIMIZER-NAME OPTS) 'OPTIMIZERS))))

;EXPAND ANY MACROS IN TOP LEVEL OF A FORM.  USE THIS BEFORE MAKING SPECIAL
;CASE CHECKS IN PASS 1.
;MACROEXPAND X ITERATIVELY UNTIL IT CAN'T EXPAND ANY MORE.
(IF-FOR-MACLISP
(DEFUN MACROEXPAND (X IGNORE)
    (DO ((TM X (MACROEXPAND-1 TM IGNORE))
	 (OTM NIL TM))
	((OR (EQ TM OTM) (ATOM TM)) TM))))

;Macroexpand X once, if possible.  If it isn't a macro invocation, return it unchanged.
;The definition of MACROEXPAND-1 for the real machine is in QFCTNS.
(IF-FOR-MACLISP
(DEFUN MACROEXPAND-1 (X IGNORE)
    (PROG (TM)
	  (AND (ATOM X) (RETURN X))
	  (COND ((NOT (ATOM (CAR X)))
		 (COND ((EQ (CAAR X) 'CURRY-AFTER)
			(RETURN `(,(CADAR X) ,@(CDR X) . ,(CDDAR X))))
		       ((EQ (CAAR X) 'CURRY-BEFORE)
			(RETURN `(,(CADAR X) ,@(CDDAR X) . ,(CDR X))))
                       ((EQ (CAAR X) 'SUBST)
                        (RETURN (SUBST-EXPAND (CAR X) X))))
		 (RETURN X))
		((SETQ TM (DECLARED-DEFINITION (CAR X)))
		 (COND ((EQ (CAR TM) 'SUBST) (RETURN (SUBST-EXPAND TM X)))
		       ((EQ (CAR TM) 'MACRO) 
			(RETURN
			  (COND ((SETQ TM (ERRSET (FUNCALL (CDR TM) X)))
				 (CAR TM))
				(T (BARF X
					 (COND ((EQ TM 'DOEXPANDER)
						'INCORRECT-DO-FORMAT)
					       (T 'LISP-ERROR-DURING-MACRO-EXPANSION))
					 'DATA)))))))
		((SETQ TM (OPEN-CODE-P (CAR X)))
		 (RETURN (CONS TM (CDR X)))))
	  (RETURN X))))

;; Given a function-spec, find any definition it has been declared to have
;; for compilation purposes, and return it.
(IF-FOR-MACLISP
(DEFUN DECLARED-DEFINITION (FUNCTION)
  (OR (DOLIST (L LOCAL-DECLARATIONS)
	(AND (EQ (CADR L) FUNCTION)
	     (COND ((EQ (CAR L) 'DEF)
		    (RETURN (CDDR L))))))
      (DOLIST (L FILE-LOCAL-DECLARATIONS)
	(AND (EQ (CADR L) FUNCTION)
	     (COND ((EQ (CAR L) 'DEF)
		    (RETURN (CDDR L))))))
      (GET FUNCTION 'LMDEF)
      (LET ((TEM (GET FUNCTION 'MACRO)))
	(AND TEM (CONS 'MACRO TEM))))))

;; Expand a call to a SUBST function.
;; SUBST is the function definition to use; FORM is the whole form.
;; Match the SUBST args with the expressions in the form
;; and then substitute the expressions for the args in the body of the function with SUBLIS.

(IF-FOR-MACLISP
(DEFUN SUBST-EXPAND (SUBST FORM)
    (LET (ALIST OPTIONAL-FLAG (LAMBDA-LIST (CADR SUBST)) (BODY (CDDR SUBST)))
        ;; Provide an implicit PROGN fob the body.
        (COND ((CDR BODY) (SETQ BODY `(PROGN . ,BODY)))
              (T (SETQ BODY (CAR BODY))))
        ;; Process the lambda list and args to make the alist.
        (DO ((VALS (CDR FORM) (CDR VALS)))
            (NIL)
           ;; We allow only &OPTIONAL.
           (DO () ((NEQ (CAR LAMBDA-LIST) '&OPTIONAL))
	     (SETQ OPTIONAL-FLAG T)
	     (POP LAMBDA-LIST))
           ;; Detect runout of lambda list or of args.
           (COND ((NULL VALS)
                  (COND ((NULL LAMBDA-LIST)
                         (RETURN NIL))
                        ((NOT OPTIONAL-FLAG)
			 (RETURN (BARF (CAR FORM)
				       '|Too few arguments to SUBST function|
				       'WARN)))))
                 ((NULL LAMBDA-LIST)
                  (RETURN (BARF (CAR FORM) '|Too many arguments to SUBST function| 'WARN))))
           ;; All lambda-list keywords aside from &OPTIONAL are erroneous.
           (AND (MEMQ (CAR LAMBDA-LIST) LAMBDA-LIST-KEYWORDS)
                (RETURN
		 (BARF (CAR FORM) '|SUBST function contains inappropriate LAMBDA-keyword| 'DATA)))
           ;; Here we have one more arg.  Add it to the alist.
           (PUSH (CONS (COND ((ATOM (CAR LAMBDA-LIST)) (CAR LAMBDA-LIST))
                             (T (CAAR LAMBDA-LIST)))
                       (COND (VALS (CAR VALS))
                             ((ATOM (CAR LAMBDA-LIST)) NIL)
                             (T (CADAR LAMBDA-LIST))))
                 ALIST)
           (POP LAMBDA-LIST))
	 (SUBLIS ALIST BODY))))

;; If symbol as a function should be open-coded
;; then return the definition to substitute in.
;; Otherwise, return NIL.
;; A local declaration (OPEN-CODE symbol definition) takes priority.
;; Next comes the value of an OPEN-CODE property.
;; If that is T, the actual function definition is used.
(IF-FOR-MACLISP
(DEFUN OPEN-CODE-P (SYMBOL)
    (OR (DO ((LDECLS LOCAL-DECLARATIONS (CDR LDECLS)))
	    ((NULL LDECLS) NIL)
	   (AND (EQ (CAAR LDECLS) 'OPEN-CODE)
		(EQ (CADAR LDECLS) SYMBOL)
		(RETURN (CADDAR LDECLS))))
	(LET ((TM (GET SYMBOL 'OPEN-CODE)))
	    (COND ((EQ TM T)
		   (GET SYMBOL 'EXPR))
		  (T TM))))))

;Pass 1 processing for a call to an ordinary function (ordinary, at least, for pass 1).
;FORM is the call to the function, and DESC is the GETARGDESC of the function.
;Processing consists of P1'ing all evaluated arguments, but not the quoted ones.
;DESC is used to determine which is which.
;In addition, &FUNCTIONAL arguments are broken off and separately compiled.
;We process the args by copying the arglist, and rplaca'ing each arg by P1 of itself if needed.
(DEFUN P1ARGC (FORM DESC)
       (PROG (COUNT TOKEN-LIST ARGS-LEFT DESCS-LEFT ARG-P1-RESULTS FCTN TM P1VALUE)
	     (SETQ P1VALUE T)
	     (SETQ DESCS-LEFT DESC)
	     (SETQ FCTN (CAR FORM))
	     (COND ((AND DESCS-LEFT
			 (MEMQ 'FEF-ARG-REST (SETQ TM (CADAR DESCS-LEFT)))
			 (MEMQ 'FEF-QT-QT TM))
		    (RETURN FORM))) 		;JUST FOR "EFFICIENCY"
	     (SETQ ARG-P1-RESULTS (SETQ ARGS-LEFT (APPEND (CDR FORM) NIL)))
             (SETQ COUNT 0)
	L3
	     ;; Figure out what descriptor to use for the next argument.
	     ;; TOKEN-LIST is the actual descriptor, and COUNT
	     ;; is the number of argumens left for it to apply to.
	     (COND ((ZEROP COUNT)
		    (COND ((NULL DESCS-LEFT)
			   ;; Out of descs for arga, and out of args, => return.
			   (OR ARGS-LEFT (RETURN (CONS FCTN ARG-P1-RESULTS)))
			   ;; Out of descriptors => complain, and treat excess args as evalled.
			   (BARF FORM '|Too many arguments| 'WARN)
			   (SETQ DESCS-LEFT '((1005 (FEF-ARG-OPT FEF-QT-EVAL))))))
		    (SETQ COUNT (CAAR DESCS-LEFT))
		    (SETQ TOKEN-LIST (CADAR DESCS-LEFT))
		    (SETQ DESCS-LEFT (CDR DESCS-LEFT))
		    (COND ((MEMQ 'FEF-ARG-REST TOKEN-LIST)
			   (SETQ COUNT 1005)))
		    ;; Check ZEROP again!
		    (GO L3)))

	     ;; If all arguments processed, return, complaining if more args required.
	     (COND ((NULL ARGS-LEFT)
		    (AND (MEMQ 'FEF-ARG-REQ TOKEN-LIST)
			 (BARF FORM '|Too few arguments| 'WARN))
		    (RETURN (CONS FCTN ARG-P1-RESULTS))))

	     ;; Process the next argument according to its descriptor.
	     (COND ((MEMQ 'FEF-QT-QT TOKEN-LIST))
		   ((OR (MEMQ 'FEF-QT-EVAL TOKEN-LIST)
			(MEMQ 'FEF-QT-DONTCARE TOKEN-LIST))
		    (RPLACA ARGS-LEFT
			  (COND ((AND (MEMQ 'FEF-FUNCTIONAL-ARG TOKEN-LIST)
				      (NOT (ATOM (SETQ TM (OPTIMIZE (CAR ARGS-LEFT) T))))
				      (EQ (CAR TM) 'QUOTE))	 ;LOOK FOR '(LAMBDA...)
				 (P1FUNCTION TM))
				(T (P1 (CAR ARGS-LEFT))))))
		   (T (BARF (CAR DESCS-LEFT) 'BAD-EVAL-CODE 'BARF)))
	     (SETQ ARGS-LEFT (CDR ARGS-LEFT))
	     (SETQ COUNT (1- COUNT))
	     (GO L3)))

;Return T if OBJECT is self-evaluating.
(DEFUN CONSTANTP (OBJECT)
    (OR (NUMBERP OBJECT)
	#Q (STRINGP OBJECT)
	(NULL OBJECT)
	(EQ OBJECT T)))

;Return T if OBJECT is something quoted.
(DEFUN QUOTEP (OBJECT)
    (AND (NOT (ATOM OBJECT))
         (EQ (CAR OBJECT) 'QUOTE)))

;; Allow the compiler to recognize automatically
;; the funny representation used for strings.
(IF-FOR-MACLISP
 (DEFUN STRINGP (OBJECT)
   (AND (NOT (ATOM OBJECT))
	(EQ (CAR OBJECT) 'QUOTE)
	(NOT (ATOM (CADR OBJECT)))
	(EQ (CAADR OBJECT) '**STRING**))))

;;When a var is handled by P1BINDVAR which is an optional arg with a specified-flag,
;;we push the flag name onto SPECIFIED-FLAGS so that a home will be made for the flag.
(DEFVAR SPECIFIED-FLAGS)

;Process a Lambda-list (X), making the variables by default of kind KIND
;(FEF-ARG-REQ for the top-level lambda,
; FEF-ARG-AUX or FEF-ARG-INTERNAL-AUX for progs).
;Return a prog variable list for the same variables with their initializations if any,
;with P1 done on each initialization.
;This function gobbles down the variables and processes keywords.
;Each variable, with its appropeiate keyword info, is passed to P1LMB.
;We can do either sequential or parallel binding.
;Processing of variables is done in two steps:
;First, create the homes and put them on VARS and ALLVARS.
;Second, process all the variables' initializations.
;This order is so that variables bound inside those initializations
;all come after all the variables of the original (higher) level.
;This is needed to make sure that (DEFUN FOO (&OPTIONAL (A (LET ((C ...)) ...)) B) ...)
;does not put C into VARS before B.
;The IGNORE-NIL-P argument is used by MULTIPLE-VALUE-BIND to say
; that if NIL appears as a variable, its initial value should be evaluated
; and discarded.
(DEFUN P1SBIND (X KIND PARALLEL IGNORE-NIL-P)
       (PROG (TM EVALCODE VARN OVARS MISC-TYPES OALLVARS SPECIFIED-FLAGS)
	     (SETQ EVALCODE 'FEF-QT-DONTCARE)
	     (SETQ OVARS VARS)
	     (SETQ OALLVARS ALLVARS)
	A    (COND ((NULL X)
		    (DOLIST (V SPECIFIED-FLAGS)
		      (CREATE-SPECIFIED-FLAG-VARIABLE V))
                    (RETURN (MAPCAR (FUNCTION VAR-COMPUTE-INIT)
                                    (REVERSE (LDIFF ALLVARS OALLVARS))
                                    (CIRCULAR-LIST PARALLEL)
                                    (CIRCULAR-LIST OVARS))))
		   ((SETQ TM (ASSQ (CAR X)
				   '((&OPTIONAL . FEF-ARG-OPT)
				     (&REST . FEF-ARG-REST) (&AUX . FEF-ARG-AUX))))
		    (COND ((OR (EQ KIND 'FEF-ARG-AUX)
			       (EQ KIND 'FEF-ARG-INTERNAL-AUX))
			   (BARF (CAR X) '|argument keywords in PROG variable list| 'DATA))
			  (T (SETQ KIND (CDR TM))))
		    (GO B))
		   ((SETQ TM (ASSQ (CAR X)
				   '((&EVAL . FEF-QT-EVAL)
				     (&QUOTE . FEF-QT-QT)
				     (&QUOTE-DONTCARE . FEF-QT-DONTCARE))))
		    (SETQ EVALCODE (CDR TM))
		    (GO B))
		   ((SETQ TM (ASSQ (CAR X)
				   '((&FUNCTIONAL . FEF-FUNCTIONAL-ARG))))
		    (PUSH (CDR TM) MISC-TYPES)
		    (GO B))
		   ((MEMQ (CAR X) LAMBDA-LIST-KEYWORDS)
		    (GO B)))
             ;; LAMBDA-list keywords have jumped to B.
             ;; Get here when (CAR X) is a variable or (var init).
	     (SETQ VARN (COND ((ATOM (CAR X)) (CAR X)) (T (CAAR X))))
	     (AND (NOT (OR (SAMEPNAMEP VARN '|IGNORE|)
			   (SAMEPNAMEP VARN '|IGNORED|)
			   (NULL VARN)))
		  (DOLIST (X1 (CDR X))
		     (COND ((OR (EQ X1 VARN)
				(AND (NOT (ATOM X1)) (EQ (CAR X1) VARN)))
			    (RETURN T))))
		  (BARF VARN '|duplicated in lambda-list| 'WARN))
	     (AND (= (GETCHARN VARN 1) #/&)
		  (BARF VARN '|probably mispelled keyword| 'WARN))
	     (COND ((AND IGNORE-NIL-P (NULL VARN))
		    (P1 (CADAR X)))		;Out of order, but works in these simple cases
		   ((OR (NULL VARN) (EQ VARN T))
		    (BARF VARN '|bound| 'WARN))
		   (T
		     ;; Make the variable's home.
		     (P1BINDVAR (CAR X) KIND EVALCODE MISC-TYPES)))
	     (SETQ MISC-TYPES NIL)
	     B
	     (SETQ X (CDR X))
	     (GO A)))

;Create a home for a variable and push the home on VARS and ALLVARS.
;We fill the variable's INIT slot with a list whose car is the init form
;and whose cadr may be the supplied-flag-name, or with nil if there is no init at all,
;rather than what is ultimately to go there (which gets there in VAR-COMPUTE-INIT).
(DEFUN P1BINDVAR (VARSPEC KIND EVAL-TYPE MISC-TYPES)
       (PROG (TYPE HOME INIT-SPECS)
	     (COND ((NOT (ATOM VARSPEC))
		    (SETQ INIT-SPECS (CDR VARSPEC))
		    (SETQ VARSPEC (CAR VARSPEC))))
	     (AND (OR (EQ VARSPEC NIL) (EQ VARSPEC T))
		  (RETURN (BARF VARSPEC '|was used as a bound variable| 'WARN)))
	     ;; If this variable is an optional arg with a specified-flag,
	     ;; remember to make a home for the flag as well.
	     (AND (CADR INIT-SPECS)
		  (COND ((NEQ KIND 'FEF-ARG-OPT)
			 (BARF VARSPEC '|has a specified-flag but isn't an optional arg|
			       'WARN))
			((NOT (EQ (TYPEP (CADR INIT-SPECS)) 'SYMBOL))
			 (BARF VARSPEC '|has a specified-flag name which isn't a symbol|
			       'WARN))
			(T
			 (PUSH (CADR INIT-SPECS) SPECIFIED-FLAGS))))
	     (COND ((NOT (EQ (TYPEP VARSPEC) 'SYMBOL))
		    (BARF VARSPEC '|non-atomic variable name| 'DATA)))
             (SETQ TYPE (FIND-TYPE VARSPEC))
	     (COND ((MEMQ TYPE '(FEF-SPECIAL FEF-REMOTE)) (SETQ SPECIALFLAG T)))
             (SETQ HOME (VAR-MAKE-HOME VARSPEC TYPE KIND INIT-SPECS
				       EVAL-TYPE MISC-TYPES))
	     (PUSH HOME VARS)
	     (PUSH HOME ALLVARS)))

;Make a home for the "specified-flag" of an optional variable
;(such as, FOOP in &OPTIONAL (FOO 69 FOOP)).
;It is marked with FEF-ARG-SPECIFIED-FLAG in the misc flags.
;This home is pushed on VARS right after the last argument, before
;the first actual aux variable, and also before any locals bound
;in initializations of optionals, and its scope is the entire function.
;It is of kind "aux" and initialized to the constant T
;regardless of the fact that TLFUNINIT is already set and so
;(usually) only FEF-INI-COMP-C is allowed at this point.
(DEFUN CREATE-SPECIFIED-FLAG-VARIABLE (NAME)
  (PROG (HOME)
    (SETQ HOME (VAR-MAKE-HOME NAME (FIND-TYPE NAME)
			      'FEF-ARG-AUX '('T)
			      'FEF-QT-DONTCARE '(FEF-ARG-SPECIFIED-FLAG)))
    (PUSH HOME ALLVARS)
    (PUSH HOME VARS)))

(DEFUN SPECIALP (SYMBOL)
  (OR (MEMQ SYMBOL BARF-SPECIAL-LIST)
      (DO ((LDS LOCAL-DECLARATIONS (CDR LDS))
	   (TEM)
	   (FIRST-P T))
	  (NIL)
	(AND (NULL LDS)
	     (IF FIRST-P (SETQ FIRST-P NIL LDS FILE-LOCAL-DECLARATIONS)
		(RETURN (OR ALL-SPECIAL-SWITCH
			    (GET SYMBOL 'SPECIAL)
			    (IF-FOR-LISPM (GET SYMBOL 'SYSTEM-CONSTANT))
			    (IF-FOR-LISPM (AND (SETQ TEM (CDR (PACKAGE-CELL-LOCATION SYMBOL)))
					       (MEMQ TEM SPECIAL-PKG-LIST)))))))
	(AND (MEMQ (CAAR LDS) '(SPECIAL UNSPECIAL))
	     (MEMQ SYMBOL (CDAR LDS))
	     (RETURN (EQ (CAAR LDS) 'SPECIAL))))))

(DEFUN FIND-TYPE (X) (COND ((SPECIALP X) 'FEF-SPECIAL) ;REMOTE???
			   (T 'FEF-LOCAL)))
                                               
;Construct and return a variable home to go on VARS and ALLVARS.
;This home has, in the VAR-INIT slot, not what is supposed to be there
;but the actual initialization-form for the variable.
;Later, VAR-COMPUTE-INIT is called to fix that up.
(DEFUN VAR-MAKE-HOME (NAME TYPE KIND INIT-SPECS EVAL-TYPE MISC-TYPES)
    (COND ((NULL (MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST
                                          FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
           (BARF KIND 'BAD-KIND 'BARF)))
    ;; Rest args interfere with fast arg option except when there are no specials.
    ;; We need to look at this to
    ;;  decide how to process all the AUX variables and can't tell when processing
    ;;  the first one whether the next will be special.
    ;;  In any case, being wrong about this should not be able to produce
    ;;  incorrect code.
    (COND ((EQ KIND 'FEF-ARG-REST)
           (SETQ FAST-ARGS-POSSIBLE NIL)))
    (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
           (AND INIT-SPECS (SETQ FAST-ARGS-POSSIBLE NIL))))
    ;; Detect vars bound to themselves which fail to be special.
    (COND ((AND (EQ NAME (CAR INIT-SPECS))
                (NOT (ASSQ NAME VARS)))
           (MSPL2 NAME)
           (SETQ TYPE 'FEF-SPECIAL)))
    ;; Cons up the variable descriptor.
    (LIST NAME KIND TYPE 0
          NIL            ;Lap address will be assigned later.
          INIT-SPECS	;Init info not determined yet; store init form.
          EVAL-TYPE MISC-TYPES))

;; After the end of pass 1, assign lap addresses to the variables.
(DEFUN ASSIGN-LAP-ADDRESSES ()
    (SETQ ARGN 0)	;Next argument number.
    (SETQ LVCNT 0)	;Next slot in local block.  Count rest arg, auxes,
                        ;and internal-auxes if they are not special.
    (SETQ ARG-MAP NIL)  ;We also build the arg map and local map,
    (SETQ LOCAL-MAP NIL) ;pushing things on in reverse order.
    (DOLIST (V (REVERSE ALLVARS))
      ;; Cons up the expression for Lap to use to refer to this variable.
      (LET ((TYPE (VAR-TYPE V))
            (KIND (VAR-KIND V))
	    (NAME (VAR-NAME V)))
        (SETF-VAR-LAP-ADDRESS V
              (COND ((EQ TYPE 'FEF-SPECIAL)
                     `(SPECIAL ,NAME))
                    ((EQ TYPE 'FEF-REMOTE)
                     `(REMOTE ,NAME))
                    ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
                     `(ARG ,ARGN))
                    (T `(LOCBLOCK ,LVCNT))))
        ;; Now increment one or more of the counters of variables
        ;; and maybe make an entry on LOCAL-MAP or ARG-MAP
        (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
               (PUSH (LIST NAME) ARG-MAP)
               (AND (= (SETQ ARGN (1+ ARGN)) 101)
                    (BARF NAME '|More than 100 arguments accepted by one function| 'DATA)))
              ((OR (EQ TYPE 'FEF-LOCAL)
                   (NOT (MEMQ KIND '(FEF-ARG-INTERNAL FEF-ARG-INTERNAL-AUX))))
               (PUSH (LIST NAME) LOCAL-MAP)
               (AND (= (SETQ LVCNT (1+ LVCNT)) 101)
                    (BARF NAME '|More than 100 local variable slots in one function|
                          'DATA))))))
    ;; Now find any variables that want to be initted to other variables
    ;; and set up their inits with the addresses of the vars to init from.
    (DOLIST (V ALLVARS)
      (LET ((INIT (VAR-INIT V)))
        (COND ((EQ (CAR INIT) 'FEF-INI-EFF-ADR)
               (RPLACA (CDR INIT) `(FIXE ,(MADR (CADR INIT))))))))
    (SETQ LOCAL-MAP (NREVERSE LOCAL-MAP)
          ARG-MAP (NREVERSE ARG-MAP)))

;Given a variable home, compute its VAR-INIT and install it.
;When we are called, the VAR-INIT contains the data for us to work on
;which looks like (init-form arg-supplied-flag-name).
;Note that for a FEF-ARG-INTERNAL-AUX variable, the init-type will
;always be FEF-INI-COMP-C.
(DEFUN VAR-COMPUTE-INIT (HOME PARALLEL OVARS)
  (PROG (INIT-SPECS INIT-FORM INIT-TYPE INIT-DATA SPECIFIED-FLAG-NAME NAME KIND TYPE)
	(SETQ NAME (VAR-NAME HOME)
	      KIND (VAR-KIND HOME)
	      TYPE (VAR-TYPE HOME)
	      INIT-SPECS (VAR-INIT HOME)
	      INIT-FORM (CAR INIT-SPECS)
	      SPECIFIED-FLAG-NAME (CADR INIT-SPECS))
	;; Determine the environment (value of VARS)
	;; to use in computing this var's init form.
	;; For a serial prog, it's all vars preceding this one.
	;; Bind VARS to it so we don't fuck up when calling things that
	;; use it free.  (e.g. MADR)
	(LET ((VARS (COND (PARALLEL OVARS)
			  (T (CDR (MEMQ HOME VARS))))))
          (COND ((NULL INIT-FORM))
                ((CONSTANTP INIT-FORM)
                 (SETQ INIT-FORM `',INIT-FORM))
                ((AND (NOT (ATOM INIT-FORM))
                      (EQ (CAR INIT-FORM) 'QUOTE)))
                (T
                 ;; Init is not NIL, constant or self => must P1 it, and maybe set TLFUNINIT.
                 (LET ((TLEVEL NIL))
                    (SETQ INIT-FORM (P1 INIT-FORM)))
                 (COND ((NOT (ADRREFP INIT-FORM))
                        (SETQ TLFUNINIT T)))))
          ;; Now that we have processed the init form, determine the ADL initialization field.
	  ;; First, must we, or would we rather, use code to initialize the variable?
	  ;; Note: specified-flags MUST be initted at entry time regardless of anything else.
	  (COND ((AND (NOT (MEMQ 'FEF-ARG-SPECIFIED-FLAG (VAR-MISC HOME)))
		      (OR (EQ KIND 'FEF-ARG-INTERNAL-AUX) TLFUNINIT
			  ;; Don't spoil the fast arg option with nontrivial inits for aux's. 
			  (AND (EQ KIND 'FEF-ARG-AUX)
			       FAST-ARGS-POSSIBLE
			       (NOT (MEMBER INIT-FORM '(NIL 'NIL))))
			  (COND (PARALLEL (NEQ TYPE 'FEF-LOCAL)))))
		 (SETQ INIT-TYPE 'FEF-INI-COMP-C)
		 ;; Note: if we are initting by code, there is no advantage
		 ;; in binding at function entry, and doing so would
		 ;; make lap stupidly turn off the fast arg option!
		 (AND (EQ KIND 'FEF-ARG-AUX)
		      (SETF-VAR-KIND HOME (SETQ KIND 'FEF-ARG-INTERNAL-AUX)))
                 (SETQ TLFUNINIT T)))
	  ;; If we aren't forced already not to use an init, figure out
	  ;; what type of init to use if there's no init-form: either "none" or "nil".
	  (OR INIT-TYPE
	      (SETQ INIT-TYPE
		    (COND ((OR (EQ KIND 'FEF-ARG-OPT)
			       (AND (EQ KIND 'FEF-ARG-AUX)
				    (MEMQ TYPE '(FEF-SPECIAL FEF-REMOTE))))
			   'FEF-INI-NIL)
			  (T 'FEF-INI-NONE))))
	  ;; Then, if there is an init form, gobble it.
	  (COND ((AND INIT-FORM (NEQ INIT-TYPE 'FEF-INI-COMP-C))
		 (COND ((NOT (MEMQ KIND
				   '(FEF-ARG-OPT FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
			(BARF NAME
			      '|initialization specified for mandatory argument|
			      'WARN))
                       ;; There's a hack for binding a special var to itself.
		       ((AND (EQ NAME INIT-FORM)
                             (NEQ TYPE 'FEF-LOCAL))
                        (SETQ INIT-TYPE 'FEF-INI-SELF))
		       ((ATOM INIT-FORM)
                        (COND ((SPECIALP INIT-FORM)
                               (SETQ INIT-TYPE 'FEF-INI-C-PNTR)
                               (SETQ INIT-DATA (LIST 'LOCATIVE-TO-S-V-CELL INIT-FORM)))
                              (T (SETQ INIT-TYPE 'FEF-INI-EFF-ADR)
                                 ;; Initted to value of local var:
                                 ;; Make init data be the var, for now.
                                 ;; ASSIGN-LAP-ADDRESSES will change it.
                                 (SETQ INIT-DATA INIT-FORM))))
		       ((MEMQ (CAR INIT-FORM) '(QUOTE FUNCTION))
			(SETQ INIT-TYPE 'FEF-INI-PNTR)
			(SETQ INIT-DATA INIT-FORM)))))
	  (COND ((AND (EQ KIND 'FEF-ARG-OPT)
		      (OR TLFUNINIT SPECIFIED-FLAG-NAME))
		 ;; Once an opt arg gets an alternate starting address,
		 ;; all following args must be similar or else FEF-INI-COMP-C.
		 (SETQ TLFUNINIT T)
		 (SETQ INIT-TYPE 'FEF-INI-OPT-SA)
		 (SETQ INIT-DATA (GENSYM)))
		;; If something not an optional arg was given a specified-flag,
		;; discard that flag now.  There has already been an error message.
		(T (SETQ SPECIFIED-FLAG-NAME NIL)))
	  (SETF-VAR-INIT HOME (LIST* INIT-TYPE INIT-DATA
				     (ASSQ SPECIFIED-FLAG-NAME ALLVARS)))
          (RETURN (COND ((NULL INIT-FORM) NAME) (T (LIST NAME INIT-FORM)))))))

;; (MULTIPLE-VALUE-BIND variable-list m-v-returning-form . body)
;; turns into (MULTIPLE-VALUE-BIND variable-list vars-segment m-v-returning-form . body)
;; where vars-segment is a sublist of VARS that should be pushed onto VARS
;; while this form is being processed on pass 2.

(DEFUN (MULTIPLE-VALUE-BIND P1) (FORM)
  (LET ((VARIABLES (CADR FORM))
	(VARS VARS) BODY
	(M-V-FORM (CADDR FORM)))
    (SETQ TLEVEL NIL)
    ;; P1 the m-v-returning-form outside the bindings we make.
    (SETQ M-V-FORM (P1 M-V-FORM))
    ;; The code should initialize each variable by popping off the stack.
    ;; The values will be in forward order so we must pop in reverse order.
    (SETQ VARIABLES (MAPCAR #'(LAMBDA (V) `(,V (%POP))) VARIABLES))
    (P1SBIND VARIABLES 'FEF-ARG-INTERNAL-AUX T T)
    (SETQ BODY (P1PROGN-1 (CDDDR FORM)))
    `(,(CAR FORM) ,VARIABLES ,VARS ,M-V-FORM . ,BODY)))

;;; If this wasn't here the compiler would be confused into not compiling the arguments
(DEFUN (PROGN P1) (FORM)
  (CONS 'PROGN (MAPCAR 'P1 (CDR FORM))))

;Analyze a prog's variable bindings and tags,
;and convert it to an internal form which looks like
;(SPROG <variable list, with keywords processed and removed>
;      <value of VARS for body of this prog>
;      <segment of GOTAGS for tags in body of this prog (not incl. outer progs)>
;      <name of this prog, or NIL>
;      <T if BIND used within this prog>
;      . <body, P1'ified>)
;Note that ALLGOTAGS contains a list of all tags seen in this function so far.
;We use it to determine whether we should
;rename a tag because it is shadowing an outer one.

;Since there is a confusion about whether PROG should compute all
;its initializations and then bind all the variables ("parallel binding"),
;or process one variable completely at a time ("sequential binding"),
;the compiler understands two forms of PROG:  SPROG and PPROG.
;SPROG does sequential binding, and PPROG does parallel binding.
;P1LAMBDA, P1AUX and DOEXPANDER all generate PPROG or SPROG as appropriate.
;PROG and PROG* are converted to PPROG and SPROG so that the decision
;of which of them is which is localized entirely within this function.

(DEFUN P1PROG (FORM)
    (LET ((PROGNAME) (VARS VARS) (GOTAGS)
	  (FN (CAR FORM)) (P1VALUE NIL) (BINDP) (BODY) (VLIST))
	 (SETQ FORM (CDR FORM))
	 ;; Extract the prog name if there is one.
	 (COND ((AND (CAR FORM)
                     (EQ (TYPEP (CAR FORM)) 'SYMBOL))
                (SETQ PROGNAME (CAR FORM))
                (SETQ FORM (CDR FORM))))
	 (COND ((EQ FN 'PROG)
		(SETQ FN 'PPROG))
	       ((EQ FN 'PROG*)
		(SETQ FN 'SPROG)))
	 (SETQ VLIST (CAR FORM))
         ;; Treat parallel binding as serial if it doesn't matter.
         (OR (CDR VLIST) (SETQ FN 'SPROG))
         (AND (EQ FN 'PPROG)
              (DO ((XX VLIST (CDR XX)))
                  ((NULL XX) (SETQ FN 'SPROG))
                 (OR (ATOM (CAR XX))
                     (CONSTANTP (CADAR XX))
                     (QUOTEP (CADAR XX))
                     (EQ (CAR XX) (CADAR XX))
                     (RETURN NIL))))
	 ;; Flush rebinding a var to itself if it isn't special
	 ;; and range of rebinding is rest of function.
	 (AND TLEVEL
	      (SETQ VLIST
		    (SUBSET-NOT #'(LAMBDA (VAR)
				    (AND (NOT (ATOM VAR))
					 (EQ (CAR VAR) (CADR VAR))
					 (EQ (VAR-TYPE (ASSQ VAR VARS)) 'FEF-LOCAL)))
				VLIST)))
	 (SETQ VLIST (P1SBIND VLIST
			      (COND (TLEVEL 'FEF-ARG-AUX)
				    (T 'FEF-ARG-INTERNAL-AUX))
			      (EQ FN 'PPROG)
			      NIL))
         (SETQ BODY (CDR FORM))
	 ;; Now convert initial SETQs to variable initializations.
	 ;; We win only for SETQs of variables bound but with no initialization spec'd,
	 ;; which set them to constant values, and only if later vars' inits didn't use them.
	 ;; When we come to anything other than a SETQ we can win for, we stop.
	 ;; For SPROG, we can't win for a special variable if anyone has called a function
	 ;; to do initting, since that function might have referred to the special.
	 ;; Even if we don't use tha ADL to init them,
	 ;; we avoid redundant settings to NIL.
	 (DO ((TEM) (HOME)) (NIL)
	    (COND ((EQUAL (CAR BODY) '(SETQ))
		   (SETQ BODY (CDR BODY)))
		  ((OR (ATOM (CAR BODY))
		       (NEQ (CAR (SETQ TEM (OPTIMIZE (CAR BODY) NIL))) 'SETQ)
		       (NOT (P1PROG-VAR-FIND (CADR TEM) VLIST))
		       (NOT (OR (CONSTANTP (CADDR TEM))
				(AND (NOT (ATOM (CADDR TEM)))
				     (EQ (CAADDR TEM) 'QUOTE))))
		       (AND (SPECIALP (CADR TEM))
			    (OR TLFUNINIT (NOT TLEVEL))
			    (EQ FN 'SPROG))
		       (NOT (ZEROP (VAR-USE-COUNT (SETQ HOME (ASSQ (CADR TEM) VARS))))))
		   (RETURN NIL))
		  (T (SETQ BODY (CONS (CONS 'SETQ (CDDDR TEM)) (CDR BODY)))
		     (RPLACA (MEMQ (CADR TEM) VLIST)
			     `(,(CADR TEM) ,(P1 (CADDR TEM))))
		     ;; For a variable bound at function entry, really set up its init.
		     ;; Other vars (FEF-ARG-INTERNAL-AUX) will be initted by code,
		     ;; despite our optimization, but it will be better code.
		     (AND TLEVEL (EQ (VAR-KIND HOME) 'FEF-ARG-AUX)
			  (SETF-VAR-INIT HOME `(FEF-INI-PNTR ,(P1 (CADDR TEM))))))))

	 ;; Now P1 process what is left of the body.
	 (AND (CDR BODY) (SETQ TLEVEL NIL))
	 (SETQ BODY (MAPCAR (FUNCTION (LAMBDA (STMT)
			         (COND ((EQ (TYPEP STMT) 'SYMBOL) (P1TAGAD STMT PROGNAME))
				       (T (P1 STMT)))))
			    BODY))
	 (LET ((RETTAG (GENSYM)))
	      ;; Push on GOTAGS a description of this prog's "return tag",
	      ;; a tag we generate and stick at the end of the prog.
	      (PUSH (MAKE-GOTAG RETTAG RETTAG NIL
			       PROGNAME PROGNAME) GOTAGS)
	      `(,FN ,VLIST ,VARS ,GOTAGS ,PROGNAME ,BINDP . ,BODY))))

;MEMQ and ASSQ together.  Find the tail of VLIST
;whose CAR or CAAR is VARNAME.
(DEFUN P1PROG-VAR-FIND (VARNAME VLIST)
    (DO ((VL VLIST (CDR VL))) ((NULL VL) NIL)
       (AND (OR (EQ VARNAME (CAR VL))
		(AND (NOT (ATOM (CAR VL)))
		     (EQ VARNAME (CAAR VL))))
	    (RETURN VL))))

(DEFUN P1TAGAD (X PROGNAME)
    (COND ((ASSQ X GOTAGS)
	   (AND X (BARF X '|duplicated PROG tag| 'WARN))
	   ;; Replace duplicate progtags with something that
	   ;; will be ignored by pass 2, to avoid making LAP get unhappy.
	   '(QUOTE NIL))
	  (T (PUSH X ALLGOTAGS)
	     (PUSH (MAKE-GOTAG X (COND ((MEMQ X ALLGOTAGS) (GENSYM)) (T X)) NIL PROGNAME)
		   GOTAGS)
	     X)))

(DEFPROP GO P1GO P1)
(DEFUN P1GO (FORM) FORM)

(DEFPROP RETURN-FROM P1RETURN-FROM P1)
(DEFUN P1RETURN-FROM (FORM)
    (CONS 'RETURN-FROM (CONS (CADR FORM) (MAPCAR 'P1V (CDDR FORM)))))

;Turn an internal lambda containing &AUX variables
;into one containing an SPROG and having no &AUX variables.
(DEFUN P1AUX (LAMBDA)
    (PROG (STANDARDIZED AUXVARS)
	  (SETQ STANDARDIZED
		(COND ((EQ (CAR LAMBDA) 'NAMED-LAMBDA) (CDR LAMBDA))
		      (T LAMBDA)))
	  (OR (SETQ AUXVARS (MEMQ '&AUX (CADR STANDARDIZED)))
	      (RETURN LAMBDA))
	  (RETURN `(LAMBDA ,(LDIFF (CADR STANDARDIZED) AUXVARS)
			   (SPROG T ,(CDR AUXVARS)
				  (INHIBIT-STYLE-WARNINGS
                                     (RETURN-FROM-T (PROGN . ,(CDDR STANDARDIZED)))))))))

;Turn a call to an internal lambda into a prog, and return P1 of that prog.
;All &AUX variables in the lambda list are extracted by P1AUX.
;We generate a PPROG, since the lambda variables should all be computed and then bound.
;This means that &OPTIONALs don't work quite right;
;but they never used to work at all in internal lambdas anyway.
;The PPROG is named T so that RETURNs to the user's PROGs aren't screwed up.
(DEFUN P1LAMBDA (LAMBDA ARGS)
    (PROG (ARGLIST BODY ARGS1 OPTIONAL PROGVARS VAR)
	  (SETQ LAMBDA (P1AUX LAMBDA))
	  (AND (EQ (CAR LAMBDA) 'NAMED-LAMBDA)
	       (SETQ LAMBDA (CDR LAMBDA)))
	  (SETQ ARGLIST (CADR LAMBDA) BODY (CDDR LAMBDA))
	  (SETQ ARGS1 ARGS)
          (DO ((ARGLIST1 ARGLIST (CDR ARGLIST1)))
              (NIL)
              (SETQ VAR (CAR ARGLIST1))
              (COND ((NULL ARGLIST1)
                     (AND ARGS1
                          (BARF (LIST ARGLIST ARGS)
                                '|Too many arguments to internal lambda| 'DATA))
                     (RETURN T))
                    ((EQ VAR '&REST)
                     (POP ARGLIST1)
                     (PUSH (LIST (CAR ARGLIST1) `(LIST . ,ARGS1)) PROGVARS)
                     (RETURN (SETQ ARGS1 NIL)))
                    ((EQ VAR '&OPTIONAL)
                     (SETQ OPTIONAL T))
                    ((MEMQ VAR LAMBDA-LIST-KEYWORDS)
                     (PUSH VAR PROGVARS))
                    ((NULL ARGS1)
                     (OR OPTIONAL
                         (BARF (LIST ARGLIST ARGS)
                               '|Too few arguments to internal lambda| 'BARF))
                     (RETURN T))
                    (T (OR (ATOM VAR)
                           (COND (OPTIONAL (SETQ VAR (CAR VAR)))
                                 (T (BARF VAR '|Initialization specified for mandatory argument|
                                          'DATA))))
                       (PUSH (LIST VAR (CAR ARGS1)) PROGVARS)
                       (POP ARGS1))))
	  (RETURN (P1 `(PPROG T ,(NREVERSE PROGVARS)
			      (INHIBIT-STYLE-WARNINGS (RETURN-FROM-T (PROGN . ,BODY))))))))

(DEFPROP COND P1COND P1)
(DEFUN P1COND (X)
       (CONS
	'COND
	(COND
	 ((ATOM (CDR X))
	  (BARF X '|atomic COND body| 'DATA))
	 (T (MAPCAR (FUNCTION P1PROGN-1) (CDR X))))))

(ADD-OPTIMIZER PROGN 1-ARG-NO-OP)
(DEFUN P1PROGN (X)
    (SETQ TLEVEL NIL)
    (CONS (CAR X) (P1PROGN-1 (CDR X))))

(DEFUN P1PROGN-1 (FORMS)
    (DO ((FORMS-LEFT (SETQ FORMS (APPEND FORMS NIL)) (CDR FORMS-LEFT)))
	((NULL FORMS-LEFT) FORMS)
       (LET ((P1VALUE P1VALUE))
	  (AND (CDR FORMS-LEFT) (SETQ P1VALUE NIL))
	  (RPLACA FORMS-LEFT (P1 (CAR FORMS-LEFT))))))

(DEFPROP MULTIPLE-VALUE P1-MULTIPLE-VALUE P1)
(DEFUN P1-MULTIPLE-VALUE (FORM)
    (AND (CDDDR FORM)
	 (BARF FORM '|too many arguments| 'WARN))
    (MAPC 'P1SETVAR (CADR FORM))
    (LIST 'MULTIPLE-VALUE
	  (CADR FORM)
	  (P1V (CADDR FORM))))

(DEFPROP MULTIPLE-VALUE-LIST ((1 (FEF-ARG-REQ FEF-QT-EVAL))) ARGDESC)
		;In pass 1, pretend this isn't a special form

(DEFPROP MULTIPLE-VALUE-RETURN ((1 (FEF-ARG-REQ FEF-QT-EVAL))) ARGDESC)

(DEFPROP SETQ P1SETQ P1)
(DEFUN P1SETQ (FORM)
    (CONS 'SETQ (P1SETQ-1 (CDR FORM))))

(DEFUN P1SETQ-1 (PAIRS)
    (COND ((NULL PAIRS) NIL)
	  ((NULL (CDR PAIRS))
	   (BARF PAIRS '|odd number of args to SETQ| 'DATA))
	  ((OR (NULL (CAR PAIRS)) (EQ (CAR PAIRS) T))
	   (BARF (CAR PAIRS) '|being SETQ'd| 'DATA))
	  (T
	   (P1SETVAR (CAR PAIRS))
	   (CONS (CAR PAIRS) (CONS (P1V (CADR PAIRS)) (P1SETQ-1 (CDDR PAIRS)))))))

(DEFUN P1SETVAR (VAR)
  (LET ((TM))
    (COND ((NULL VAR) NIL)	;FOR MULTIPLE-VALUE
	  ((OR (NUMBERP VAR)
	       (NOT (ATOM VAR)))
	   (BARF VAR '|attempt to SETQ something not a symbol| 'DATA))
	  ((SETQ TM (ASSQ VAR VARS))
	   (VAR-INCREMENT-USE-COUNT TM))
	  (T (MAKESPECIAL VAR)))))

;Given an entry on VARS, increment the usage count.
(DEFUN VAR-INCREMENT-USE-COUNT (VAR)
    (RPLACA (CDDDR VAR) (1+ (CADDDR VAR))))

;COMPILER-LET must be renamed to COMPILER-LET-INTERNAL
;by an "optimizer" so that its normal definition as a macro is bypassed.
(ADD-OPTIMIZER COMPILER-LET COMPILER-LET-INTERNALIZE)
(DEFUN COMPILER-LET-INTERNALIZE (FORM)
    `(COMPILER-LET-INTERNAL . ,(CDR FORM)))

;(compiler-let ((var form) (var form) ...) body...)
(DEFPROP COMPILER-LET-INTERNAL P1COMPILER-LET-INTERNAL P1)
(DEFUN P1COMPILER-LET-INTERNAL (FORM)
  (PROGV (MAPCAR #'(LAMBDA (X) (IF (ATOM X) X (CAR X))) (CADR FORM))
	 (MAPCAR #'(LAMBDA (X) (IF (ATOM X) NIL (EVAL (CADR X)))) (CADR FORM))
    (P1 (IF (CDDDR FORM) (CONS 'PROGN (CDDR FORM)) (CADDR FORM)))))

(DEFPROP FUNCTION P1FUNCTION P1)
(DEFUN P1FUNCTION (FORM)
    (COND ((ATOM (CADR FORM)) FORM)
	  (T
	   (LET ((TEM (MAYBE-BREAKOFF (CADR FORM))))
		(COND (TEM (LIST (COND ((ATOM TEM) 'FUNCTION) (T 'QUOTE)) TEM))
		      (T (LIST 'QUOTE (CADR FORM))))))))

;FUNCTIONAL-ALIST IS LIKE QUOTE, BUT BREAKS OFF ANY FUNCTIONS IT FINDS
; IN THE CDR OR CADR POSITION OF AN ALIST ELEMENT.
(DEFPROP FUNCTIONAL-ALIST P1FUNCTIONAL-ALIST P1)
(DEFUN P1FUNCTIONAL-ALIST (FORM)
   `(QUOTE ,(MAPCAR (FUNCTION (LAMBDA (X)
			(COND ((ATOM X) X)
			      (T (CONS (CAR X)
				       (COND ((ATOM (CDR X)) (CDR X))
					     ((ATOM (CADR X))
					      (OR (MAYBE-BREAKOFF (CDR X)) (CDR X)))
					     (T (CONS (OR (MAYBE-BREAKOFF (CADR X)) (CADR X))
						      (CDDR X)))))))))
		    (CADR FORM))))

(DEFPROP VALUE-CELL-LOCATION P1VALUE-CELL-LOCATION P1)

;Make sure that (VALUE-CELL-LOCATION 'FOO) marks FOO as "used".
(DEFUN P1VALUE-CELL-LOCATION (FORM)
    (COND ((AND (NOT (ATOM (CADR FORM)))
		(EQ (CAR (CADR FORM)) 'QUOTE))
	   (LET ((TEM (ASSQ (CADADR FORM) VARS)))
		(AND TEM (VAR-INCREMENT-USE-COUNT TEM))
                FORM))
	  (T (P1EVARGS FORM))))

(DEFPROP OR P1EVARGS P1)
(DEFPROP AND P1EVARGS P1)
(DEFUN P1EVARGS (FORM)
    (LET ((P1VALUE T))
	 (CONS (CAR FORM) (MAPCAR 'P1 (CDR FORM)))))

;Any use of BIND must set SPECIALFLAG.
(DEFPROP BIND P1BIND P1)
(DEFUN P1BIND (FORM)
    (SETQ SPECIALFLAG T)
    (SETQ BINDP T)
    (P1EVARGS FORM))

;For (CLOSURE '(X Y Z) ...), make sure that X, Y, Z are special.
(DEFPROP CLOSURE P1CLOSURE P1)
(DEFUN P1CLOSURE (FORM)
    (AND (NOT (ATOM (CADR FORM)))
	 (EQ (CAADR FORM) 'QUOTE)
	 (MAPC 'MSPL2 (CADADR FORM)))
    (P1EVARGS FORM))

;;; Convert DOs into PROGs.

(ADD-OPTIMIZER DO DOEXPANDER)
(ADD-OPTIMIZER DO-NAMED DOEXPANDER)

(DEFUN DOEXPANDER (X)
  (LET ((PROGNAME) (PROGREST))
    (SETQ PROGREST
      (PROG (DOSPECS ENDTEST ENDVALS TAG1 TAG3 PVARS STEPDVARS ONCE)
            (COND ((EQ (CAR X) 'DO-NAMED)
                   (SETQ PROGNAME (CADR X))
                   (SETQ X (CDDR X)))
                  (T (SETQ X (CDR X))))			;Get rid of "DO".
            (COND ((AND (CAR X) (ATOM (CAR X)))
                   (SETQ  DOSPECS `((,(CAR X) ,(CADR X) ,(CADDR X)))
                          ENDTEST (CAR (SETQ X (CDDDR X)))
                          ENDVALS NIL))
                  (T (SETQ DOSPECS (CAR X))
                     (SETQ X (CDR X))
                     (COND ((CAR X)
                            (SETQ ENDTEST (CAAR X)
                                  ENDVALS (AND (OR (CDDAR X)
                                                   (CADAR X))
                                               (CDAR X))))
                           (T (SETQ ONCE T)))))
            (SETQ X (CDR X))
            (SETQ DOSPECS (REVERSE DOSPECS)); Do NOT use NREVERSE, or you will destroy
					    ; every macro definition in sight!! -DLW
            ;; DOVARS has new-style list of DO variable specs,
            ;; ENDTEST has the end test form,
            ;; ENDVALS has the list of forms to be evaluated when the end test succeeds,
            ;; ONCE is T if this is a DO-once as in (DO ((VAR)) () ...),
            ;; X has the body.
            ;; Now process the variable specs.
            (DO X DOSPECS (CDR X) (NULL X)
                (COND ((ATOM (CAR X))
		       (PUSH (CAR X) PVARS))
                      ((OR (> (LENGTH (CAR X)) 3) (NOT (ATOM (CAAR X))))
		       (BARF (CAR X) '|malformatted DO-variable specification|
			     'DATA))
		      (T (PUSH `(,(CAAR X) ,(CADAR X)) PVARS)
                         (AND (CDDAR X)
                              (PUSH `(,(CAAR X) ,(CADDAR X)) STEPDVARS)))))
            (COND (ONCE
                   (AND STEPDVARS (BARF STEPDVARS '|Stepped variables in once-through DO| 'WARN))
                   (RETURN `(,PVARS . ,X))))
	    ;; Turn STEPDVARS into a PSETQ form to step the vars,
	    ;; or into NIL if there are no vars to be stepped.
            (SETQ STEPDVARS (APPLY 'NCONC STEPDVARS))
	    (AND STEPDVARS (SETQ STEPDVARS (CONS 'PSETQ STEPDVARS)))
            (SETQ TAG3 (GENSYM))
            (SETQ TAG1 (GENSYM))
            (SETQ ENDTEST (OPTIMIZE ENDTEST T))
            (COND ((NULL ENDTEST)
                   (AND ENDVALS
                        (BARF ENDVALS
                              '|DO end-test is NIL, but forms are to be evalled on exit|
                              'WARN))
                   (RETURN `(,PVARS ,TAG1
                             ,@X
			     ,STEPDVARS
                             (GO ,TAG1)))))
            (COND ((EQ PROGNAME T)
                   (SETQ ENDVALS `(INHIBIT-STYLE-WARNINGS
                                     (RETURN-FROM-T (PROGN NIL . ,ENDVALS)))))
                  (T (SETQ ENDVALS `(RETURN (PROGN NIL . ,ENDVALS)))))
	    (RETURN `(,PVARS
		      (GO ,TAG3)
		      ,TAG1
		      ,@X	;body
		      ,STEPDVARS
		      ,TAG3
		      (OR ,ENDTEST (GO ,TAG1))
		      ,ENDVALS))))
    (AND PROGNAME (SETQ PROGREST (CONS PROGNAME PROGREST)))
    (CONS 'PPROG PROGREST)))


(ADD-OPTIMIZER MAP MAPEXPAND)
(ADD-OPTIMIZER MAPC MAPEXPAND)
(ADD-OPTIMIZER MAPCAR MAPEXPAND)
(ADD-OPTIMIZER MAPLIST MAPEXPAND)
(ADD-OPTIMIZER MAPCAN MAPEXPAND)
(ADD-OPTIMIZER MAPCON MAPEXPAND)

(DEFUN MAPEXPAND (FORM)
  (COND ((NULL (CDDR FORM)) FORM)  ;Don't bomb out if no args for the function to map.
	(T
    (LET ((FN (OPTIMIZE (CADR FORM) NIL))
	  (TAKE-CARS (MEMQ (CAR FORM) '(MAPC MAPCAR MAPCAN)))
	  TEM)
         (COND ((NOT OPEN-CODE-MAP-SWITCH) FORM)
               ;; Expand maps only if specified function is a quoted LAMBDA or a SUBST,
	       ;; or some arg is a call to CIRCULAR-LIST and we are mapping on cars.
	       ((NOT (OR (AND (NOT (ATOM FN))
			      (MEMQ (CAR FN) '(QUOTE FUNCTION))
			      (NOT (ATOM (CADR FN))))
			 (AND (NOT (ATOM FN))
			      (EQ (CAR FN) 'FUNCTION)
			      (NOT (ATOM (SETQ TEM (DECLARED-DEFINITION (CADR FN)))))
			      (MEMQ (CAR TEM) '(SUBST MACRO)))
			 (AND TAKE-CARS
			      (SOME (CDDR FORM)
				    (FUNCTION (LAMBDA (X)
						(AND (NOT (ATOM X))
						     (NULL (CDDR X))
						     (EQ (CAR X) 'CIRCULAR-LIST))))))))
		FORM)
	       (T (SETQ FN (CADR FN))
		  ;; VARNMS gets a list of gensymmed variables to use to hold
		  ;; the tails of the lists we are mapping down.
		  (LET ((VARNMS)(DOCLAUSES) (ENDTEST) (CARS-OR-TAILS) (TEM))
		       ;; DOCLAUSES looks like ((G0001 expression (CDR G0001)) ...)
		       ;;  repeated for each variable.
		       ;; ENDTEST is (OR (NULL G0001) (NULL G0002) ...)
		       ;; CARS-OR-TAILS is what to pass to the specified function:
		       ;;  either (G0001 G0002 ...) or ((CAR G0001) (CAR G0002) ...)
		       (SETQ VARNMS (DO ((L (CDDR FORM) (CDR L)) (OUTPUT) )
					((NULL L) OUTPUT)
				      (PUSH (GENSYM) OUTPUT)))
		       (SETQ DOCLAUSES
			     (MAPCAR '(LAMBDA (V L)
				       (COND ((AND TAKE-CARS (NOT (ATOM L))
						   (EQ (CAR L) 'CIRCULAR-LIST)
						   (NULL (CDDR L)))
					      `(,V ,(CADR L)))
					     (T `(,V ,L (CDR ,V)))))
				     VARNMS (CDDR FORM)))
		       (SETQ ENDTEST
			     (CONS 'OR (MAPCAN '(LAMBDA (VL)
						 (AND (CDDR VL) `((NULL ,(CAR VL)))))
					       DOCLAUSES)))
		       (SETQ CARS-OR-TAILS
			     (COND (TAKE-CARS
				    (MAPCAR '(LAMBDA (DC)
					      (COND ((CDDR DC) `(CAR ,(CAR DC)))
						    (T (CAR DC))))
					    DOCLAUSES))
				   (T VARNMS)))
		       (COND ((MEMQ (CAR FORM) '(MAP MAPC))	;NO RESULT
			      (SETQ TEM `(INHIBIT-STYLE-WARNINGS
                                            (DO-NAMED T ,DOCLAUSES
						   (,ENDTEST)
						   (,FN . ,CARS-OR-TAILS))))
			      ;; Special hack for MAP or MAPC for value:
			      ;; Bind an extra local to 1st list and return that.
			      (COND (P1VALUE
                                     `(LET ((MAP-RESULT ,(PROG1 (CADAR DOCLAUSES)
								(RPLACA (CDAR DOCLAUSES)
									'MAP-RESULT))))
                                         ,TEM
                                         MAP-RESULT))
				    (T TEM)))
			     ((MEMQ (CAR FORM) '(MAPCAR MAPLIST))	;CONS UP RESULT
			      `(LET ((MAP-RESULT))
                                  (INHIBIT-STYLE-WARNINGS
				    (DO-NAMED T ((MAP-TEMP (INHIBIT-STYLE-WARNINGS
                                                            (VALUE-CELL-LOCATION 'MAP-RESULT)))
                                                 . ,DOCLAUSES)
					      (,ENDTEST)
					   (RPLACD MAP-TEMP
						   (SETQ MAP-TEMP
							 (NCONS (,FN . ,CARS-OR-TAILS))))))
				    MAP-RESULT))
			     (T
			      ;; MAPCAN and MAPCON:  NCONC the result.
			      `(INHIBIT-STYLE-WARNINGS
                                 (DO-NAMED T (,@DOCLAUSES (MAP-TEM) (MAP-RESULT))
				    (,ENDTEST MAP-RESULT)
				    (SETQ MAP-TEM (NCONC MAP-TEM (,FN . ,CARS-OR-TAILS)))
				    (OR MAP-RESULT (SETQ MAP-RESULT MAP-TEM))
				    (SETQ MAP-TEM (LAST MAP-TEM))))))
			    )))))))


(ADD-OPTIMIZER SUBSET SUBSET-EXPAND)
(ADD-OPTIMIZER SUBSET-NOT SUBSET-EXPAND)

(DEFUN SUBSET-EXPAND (FORM)
  (LET ((FN (OPTIMIZE (CADR FORM) NIL))
	PREDARGS DOCLAUSES TEM)
    (COND ((NOT OPEN-CODE-MAP-SWITCH) FORM)
	  ;; Expand only if specified function is a quoted LAMBDA or a SUBST,
	  ((NOT (OR (AND (NOT (ATOM FN))
			 (MEMQ (CAR FN) '(QUOTE FUNCTION))
			 (NOT (ATOM (CADR FN))))
		    (AND (NOT (ATOM FN))
			 (EQ (CAR FN) 'FUNCTION)
			 (NOT (ATOM (SETQ TEM (DECLARED-DEFINITION (CADR FN)))))
			 (MEMQ (CAR TEM) '(SUBST MACRO)))))
	   FORM)
	  (T (SETQ FN (CADR FN)) ;Strip off the QUOTE or FUNCTION.
	     ;; Generate N local variable names.
	     (DO ((L (CDDR FORM) (CDR L)) (I 0 (1+ I)))
		 ((NULL L))
	       (LET ((V (INTERN (FORMAT NIL "MAP-LOCAL-~D" I))))
		 (PUSH `(,V ,(CAR L) (CDR ,V)) DOCLAUSES)
		 (PUSH `(CAR ,V) PREDARGS)))	       
	     (SETQ DOCLAUSES (NREVERSE DOCLAUSES)
		   PREDARGS (NREVERSE PREDARGS))
	     `(LET (MAP-RESULT)
		(INHIBIT-STYLE-WARNINGS
		  (DO-NAMED T
		     ((MAP-TEMP (INHIBIT-STYLE-WARNINGS (VALUE-CELL-LOCATION 'MAP-RESULT)))
		      . ,DOCLAUSES)
		     ((NULL ,(CAAR DOCLAUSES)))	;Stop when first local variable runs out
		    (,(COND ((EQ (CAR FORM) 'SUBSET) 'AND) (T 'OR))
		      (,FN . ,PREDARGS)
		      (RPLACD MAP-TEMP (SETQ MAP-TEMP (NCONS ,(CAR PREDARGS)))))))
		MAP-RESULT)))))

;Express multi-argument arithmetic functions in terms of two-argument versions.
(MAPC (FUNCTION (LAMBDA (FN) (PUTPROP FN '(ARITHEXP) 'OPTIMIZERS)))
      '(+ * - // LOGAND LOGIOR LOGXOR MIN MAX PLUS TIMES DIFFERENCE QUOTIENT
        +$ *$ -$ //$))


(DEFPROP + *PLUS TWO-ARGUMENT-FUNCTION)
(DEFPROP +$ *PLUS TWO-ARGUMENT-FUNCTION)
(DEFPROP * *TIMES TWO-ARGUMENT-FUNCTION)
(DEFPROP *$ *TIMES TWO-ARGUMENT-FUNCTION)
(DEFPROP - *DIF TWO-ARGUMENT-FUNCTION)
(DEFPROP -$ *DIF TWO-ARGUMENT-FUNCTION)
(DEFPROP // *QUO TWO-ARGUMENT-FUNCTION)
(DEFPROP //$ *QUO TWO-ARGUMENT-FUNCTION)
(DEFPROP LOGIOR *LOGIOR TWO-ARGUMENT-FUNCTION)
(DEFPROP LOGAND *LOGAND TWO-ARGUMENT-FUNCTION)
(DEFPROP LOGXOR *LOGXOR TWO-ARGUMENT-FUNCTION)
(DEFPROP MIN *MIN TWO-ARGUMENT-FUNCTION)
(DEFPROP MAX *MAX TWO-ARGUMENT-FUNCTION)
(DEFPROP PLUS *PLUS TWO-ARGUMENT-FUNCTION)
(DEFPROP TIMES *TIMES TWO-ARGUMENT-FUNCTION)
(DEFPROP DIFFERENCE *DIF TWO-ARGUMENT-FUNCTION)
(DEFPROP QUOTIENT *QUO TWO-ARGUMENT-FUNCTION)

(DEFUN ARITHEXP (X)
       (PROG (L OP)
	     (SETQ L (LENGTH (CDR X)))
	     (COND ((NULL (SETQ OP (GET (CAR X) 'TWO-ARGUMENT-FUNCTION)))
		    (BARF X 'BAD-OP-ARITHEXP 'BARF))
		   ((= 0 L)
		    (OR (SETQ L (ASSQ OP '((*PLUS . 0) (*DIF . 0) (*TIMES . 1) (*QUO . 1))))
			(BARF X '|Illegal with no arguments| 'WARN))
		    (RETURN (CDR L)))
		   ((= L 1)
		    (RETURN (COND ((MEMQ (CAR X) '(- -$))
				   (LIST 'MINUS (CADR X)))
                                  ((MEMQ (CAR X) '(// //$))
				   (LIST '*QUO '1 (CADR X)))
				  (T (CADR X)))))
		   ((= L 2) (RETURN (CONS OP (CDR X))))
		   (T (RETURN (CONS OP
				    (CONS (CONS (CAR X)
						(BUTLAST (CDR X)))
					  (LAST X))))))))

(ADD-OPTIMIZER BOOLE BOOLE-EXPAND)
(DEFUN BOOLE-EXPAND (X)
  (PROG (L OP INST)
	(SETQ L (LENGTH (CDR X)))
	(SETQ OP (CADR X))
	(COND ((= L 2) (RETURN (CADDR X)))
	      ((AND (NUMBERP OP)
		    (SETQ INST (ASSQ OP '((1 . LOGAND)
				 (6 . LOGXOR) (7 . LOGIOR)))))
		(RETURN (CONS (CDR INST) (CDDR X))))
	      ((= L 3) (RETURN (CONS '*BOOLE (CDR X))))
	      (T (RETURN (LIST '*BOOLE (CADR X)
			       (CONS 'BOOLE (BUTLAST (CDR X)))
			       (CAR (LAST X))))))))

;(ADD-OPTIMIZER < COMPARISON-EXPAND)	;These 2 still take only 2 arguments
;(ADD-OPTIMIZER > COMPARISON-EXPAND)
(ADD-OPTIMIZER LESSP COMPARISON-EXPAND)
(ADD-OPTIMIZER GREATERP COMPARISON-EXPAND)
(DEFUN COMPARISON-EXPAND (FORM)
  (LET ((OP (CDR (ASSQ (CAR FORM) '((LESSP . <) (GREATERP . >)))))
	(LEN (LENGTH FORM)))
    (COND ((< LEN 3)
	   (BARF FORM '|too few arguments| 'DATA))
	  ((= LEN 3)
	   (CONS OP (CDR FORM)))
;	  ((AND (= LEN 4) (ATOM (CADDR FORM)))
;	   `(AND ,(LIST OP (CADR FORM) (CADDR FORM))
;		 ,(LIST OP (CADDR FORM) (CADDDR FORM))))
	  (T FORM))))	;With more than 2 arguments, don't open-code

(ADD-OPTIMIZER ADD1 ADD1-FIX)
(ADD-OPTIMIZER SUB1 ADD1-FIX)
(ADD-OPTIMIZER 1+$ ADD1-FIX)
(ADD-OPTIMIZER 1-$ ADD1-FIX)
(DEFUN ADD1-FIX (FORM)
  (CONS (CDR (ASSQ (CAR FORM) '((ADD1 . 1+)  (1+$ . 1+) (SUB1 . 1-) (1-$ . 1-))))
	(CDR FORM)))

(ADD-OPTIMIZER NULL NULL-NOT)
(DEFUN NULL-NOT (FORM)
    `(NOT . ,(CDR FORM)))

;Optimize (FUNCALL (FUNCTION (LAMBDA ...)) ...) into ((LAMBDA ...) ...).
;Do not optimize (FUNCALL (FUNCTION FOO) ...) because in that case
;it would not improve anything and would lose if FOO quotes some args.
(ADD-OPTIMIZER FUNCALL FUNCALL-FUNCTION)
(DEFUN FUNCALL-FUNCTION (FORM)
    (LET ((FNFORM (OPTIMIZE (CADR FORM) NIL)))
	 (COND ((AND (NOT (ATOM FNFORM))
		     (MEMQ (CAR FNFORM) '(FUNCTION QUOTE))
		     (NOT (ATOM (CADR FNFORM))))
		(CONS (CADR FNFORM) (CDDR FORM)))
	       (T FORM))))

(ADD-OPTIMIZER APPLY APPLY-TO-LIST)
(DEFUN APPLY-TO-LIST (FORM)
    (LET ((ARGFORM (OPTIMIZE (CADDR FORM) NIL)))
	 (COND ((ATOM ARGFORM)
		FORM)
	       ((EQ (CAR ARGFORM) 'LIST)
		`(FUNCALL ,(CADR FORM) . ,(CDR ARGFORM)))
	       ((EQ (CAR ARGFORM) 'LIST*)
		`(LEXPR-FUNCALL ,(CADR FORM) . ,(CDR ARGFORM)))
	       ((EQ (CAR ARGFORM) 'QUOTE)
		`(FUNCALL ,(CADR FORM)
			  . ,(MAPCAR (FUNCTION (LAMBDA (X) (LIST 'QUOTE X))) (CADR ARGFORM))))
	       (T FORM))))

(ADD-OPTIMIZER AND AND-OR-NO-OP)
(ADD-OPTIMIZER OR AND-OR-NO-OP)
(DEFUN AND-OR-NO-OP (FORM)
    (COND ((NULL (CDR FORM))
	   (COND ((EQ (CAR FORM) 'AND)
		  ''T)
		 (T ''NIL)))
	  ((NULL (CDDR FORM))
	   (CADR FORM))
	  (T FORM)))

(ADD-OPTIMIZER PROGN 1-ARG-NO-OP)
(DEFUN 1-ARG-NO-OP (FORM)
       (COND ((CDDR FORM) FORM)
             (T (CADR FORM))))

(ADD-OPTIMIZER PROG2 PROG2-NO-OP)
(DEFUN PROG2-NO-OP (FORM)
    (COND ((OR (CADR FORM) (CDDDR FORM)) FORM)
          (T (CADDR FORM))))

;Here are a bunch of random compile-time expansions for built-in functions.

(ADD-OPTIMIZER STORE STORE-CONVERT)
(DEFUN STORE-CONVERT (X)
  (COND ((AND (NOT (ATOM (CADR X)))	;(STORE (ARRAYCALL ...) ...)
	      (EQ (CAADR X) 'ARRAYCALL))
	 `(ASET ,(CADDR X) ,(CADDR (CADR X)) . ,(CDDDR (CADR X))))
	(T `(XSTORE ,(CADDR X) ,(CADR X)))))

;Turn EQUAL into EQ when that is safe.
;EQUAL can never be turned into = alone because = signals an error if either
;arg is not a number, whereas EQUAL does not.  However, (EQUAL <fixnum> xxx)
;can be turned into EQ since EQ "works" for fixnums.
;Also EQUALwith one of the arguments a number turns into
;(AND (NUMBERP <form>) (= <number> <form>))
(ADD-OPTIMIZER EQUAL EQUAL-EQ-=)
(DEFUN EQUAL-EQ-= (FORM)
  (COND ((OR (POINTER-IDENTITY-P (CADR FORM))
	     (POINTER-IDENTITY-P (CADDR FORM)))
	 (CONS 'EQ (CDR FORM)))
	((AND (NUMBERP (CADR FORM)) (ATOM (CADDR FORM)))
	 (EQUAL-= (CADR FORM) (CADDR FORM)))
	((AND (NUMBERP (CADDR FORM)) (ATOM (CADR FORM)))
	 (EQUAL-= (CADDR FORM) (CADR FORM)))
	(T FORM)))

(DEFUN EQUAL-= (NUMBER ATOM)
       `(AND (NUMBERP ,ATOM) (= ,NUMBER ,ATOM)))

(IF-FOR-LISPM
(DEFUN POINTER-IDENTITY-P (QUAN)
   (OR (EQ (TYPEP QUAN) 'FIXNUM)
       (AND (NOT (ATOM QUAN))
	    (EQ (CAR QUAN) 'QUOTE)
	    (MEMQ (TYPEP (CADR QUAN))
		  '(FIXNUM SYMBOL))))) )

(IF-FOR-MACLISP 	;ON MACLISP, MUST VERIFY THAT IT WILL REALLY BE A FIXNUM
			; ON REAL MACHINE, IE FITS IN 24 BITS.
(DEFUN POINTER-IDENTITY-P (QUAN)
   (OR (AND (EQ (TYPEP QUAN) 'FIXNUM)
	    (< (ABS QUAN) 37777777))
       (AND (NOT (ATOM QUAN))
	    (EQ (CAR QUAN) 'QUOTE)
	    (OR (EQ (TYPEP (CADR QUAN)) 'SYMBOL)
		(AND (EQ (TYPEP (CADR QUAN)) 'FIXNUM)
		     (< (ABS QUAN) 37777777)))))) )

;Turn (EQ FOO NIL) into (NOT FOO).
(DEFPROP EQ (EQ-NIL EQ-TYPEP) OPTIMIZERS)
(DEFUN EQ-NIL (FORM)
    (COND ((NULL (CDDR FORM)) FORM)  ;0 or 1 arg => let it get the error.
	  ((MEMBER (CADR FORM) '(NIL 'NIL))
           `(NOT ,(CADDR FORM)))
          ((MEMBER (CADDR FORM) '(NIL 'NIL))
           `(NOT ,(CADR FORM)))
          (T FORM)))

;Optimize (EQ (TYPEP ...) 'SYMBOL), etc.
(DEFUN EQ-TYPEP (FORM)
  (PROG NIL     
      (AND (NOT (ATOM (CADR FORM)))
	   (NOT (ATOM (CADDR FORM)))
	   (COND ((AND (MEMQ (CAADR FORM) '(TYPEP DATA-TYPE))
		       (NULL (CDDADR FORM))  ;Check that TYPEP has only one arg!
		       (EQ (CAADDR FORM) 'QUOTE))
		  (RETURN (EQ-TYPEP-1 (CADADR FORM) (CADR (CADDR FORM)) FORM)))
		 ((AND (EQ (CAADR FORM) 'QUOTE)
		       (MEMQ (CAADDR FORM) '(TYPEP DATA-TYPE))
		       (NULL (CDDR (CADDR FORM))))
		  (RETURN (EQ-TYPEP-1 (CADR (CADDR FORM)) (CADADR FORM) FORM)))))
      (RETURN FORM)))

(DEFUN EQ-TYPEP-1 (FORM TYPE TOPFORM)
    (PROG (PRED)
	  (SETQ PRED (OR (AND (MEMQ TYPE '(STRING :SYMBOL LIST))
			      (GET TYPE 'TYPEP))
			 (CAR (RASSOC TYPE TYPEP-ALIST))))
	  (COND ((NUMBERP PRED)
		 (RETURN `(= (%DATA-TYPE ,FORM) ,PRED)))
		((SYMBOLP PRED) `(,PRED ,FORM)))
	  (RETURN TOPFORM)))

(ADD-OPTIMIZER TYPEP TYPEP-TWO-ARGS)

(DEFUN TYPEP-TWO-ARGS (FORM)
  (COND ((AND (CADDR FORM)
	      (QUOTEP (CADDR FORM)))
	 (LET ((TYPE (CADR (CADDR FORM))) PRED)
	   (SETQ PRED (OR (GET TYPE 'TYPEP) (CAR (RASSOC TYPE TYPEP-ALIST))))
	   (COND ((NUMBERP PRED)
		  `(= (%DATA-TYPE ,(CADR FORM)) ,PRED))
		 (PRED `(,PRED ,(CADR FORM)))
		 ((CLASS-SYMBOLP TYPE)
		  `(SUBINSTANCE-OF-CLASS-SYMBOL-P ,(CADR FORM) ',TYPE))
		 (T FORM))))
	(T FORM)))

;;; modify signp to be (AND (NUMBERP <form>) (<op> <form>)) if form is an atom
;;; and therefore can't have side effects
(ADD-OPTIMIZER SIGNP SIGNP-EXPAND)
(DEFUN SIGNP-EXPAND (X)
  (LET ((OP (CADR X))
	(OPND (CADDR X)))
     (COND ((ATOM OPND)(SIGNP-OPTIMIZE OP OPND))		;IF ATOM, OPTIMIZE IT
	   (T X))))

(DEFUN SIGNP-OPTIMIZE (OPERATION OPERAND)
  (PROG (NEW-FORM NOTP)
    (SETQ NEW-FORM
	  (LIST (COND ((SAMEPNAMEP OPERATION 'E) 'ZEROP)
		      ((SAMEPNAMEP OPERATION 'N) (SETQ NOTP T) 'ZEROP)
		      ((SAMEPNAMEP OPERATION 'L) 'MINUSP)
		      ((SAMEPNAMEP OPERATION 'GE) (SETQ NOTP T) 'MINUSP)
		      ((SAMEPNAMEP OPERATION 'G) 'PLUSP)
		      ((SAMEPNAMEP OPERATION 'LE) (SETQ NOTP T) 'PLUSP)
		      ((BARF OPERATION '|illegal SIGNP condition| 'DATA)))
		OPERAND))
    (AND NOTP (SETQ NEW-FORM (LIST 'NOT NEW-FORM)))
   (RETURN `(AND (NUMBERP ,OPERAND) ,NEW-FORM))))

(ADD-OPTIMIZER AREF AREF-EXPANDER)
(DEFUN AREF-EXPANDER (FORM)
    (SELECTQ (LENGTH FORM)
       (3 (CONS 'AR-1 (CDR FORM)))
       (4 (CONS 'AR-2 (CDR FORM)))
       (5 (CONS 'AR-3 (CDR FORM)))
       (OTHERWISE (AND (< (LENGTH FORM) 3) (BARF FORM "Not enough args - AREF" 'DATA))
		  (CONS 'FUNCALL (CDR FORM)))))

(ADD-OPTIMIZER ASET ASET-EXPANDER)
(DEFUN ASET-EXPANDER (FORM)
    (SELECTQ (LENGTH FORM)
        (4 (CONS 'AS-1 (CDR FORM)))
        (5 (CONS 'AS-2 (CDR FORM)))
        (6 (CONS 'AS-3 (CDR FORM)))
	(OTHERWISE (AND (< (LENGTH FORM) 4) (BARF FORM "Not enough args - ASET" 'DATA))
		   (LIST 'STORE
                         (CONS 'FUNCALL (CDDR FORM))
                         (CADR FORM)))))

(ADD-OPTIMIZER ALOC ALOC-EXPANDER)
(DEFUN ALOC-EXPANDER (FORM)
    (SELECTQ (LENGTH FORM)
       (3 (CONS 'AP-1 (CDR FORM)))
       (4 (CONS 'AP-2 (CDR FORM)))
       (5 (CONS 'AP-3 (CDR FORM)))
       (OTHERWISE  (AND (< (LENGTH FORM) 3) (BARF FORM "Not enough args - ALOC" 'DATA))
		  `(GET-LOCATIVE-POINTER-INTO-ARRAY (FUNCALL . ,(CDR FORM))))))

;Next two are here mainly to avoid getting an error message from GETARGDESC about random FSUBR.
(ADD-OPTIMIZER COMMENT COMMENT-EXPAND)
(ADD-OPTIMIZER DECLARE COMMENT-EXPAND)
(DEFUN COMMENT-EXPAND (IGNORE) ''COMMENT)

(ADD-OPTIMIZER DEFPROP DEFPROP-EXPAND)
(DEFUN DEFPROP-EXPAND (X)
       `(PUTPROP ',(CADR X) ',(CADDR X) ',(CADDDR X)))

; Convert catches and throws
(ADD-OPTIMIZER CATCH CATCH-*CATCH)
(DEFUN CATCH-*CATCH (X)
       `(*CATCH ',(CADDR X) ,(CADR X)))

(ADD-OPTIMIZER THROW THROW-*THROW)
(DEFUN THROW-*THROW (X)
       `(*THROW ',(CADDR X) ,(CADR X)))

;;; ERRSET and ERR are normally macros but in Maclisp LMMAC doesn't define
;;; them to avoid messing up the Maclisp interpreter.

(IF-FOR-MACLISP (PROGN 'COMPILE	;necessary kludge
(SPECIAL SI:EH-ERRSET-STATUS SI:EH-ERRSET-PRINT-MSG) ;These colons aren't really colons

(ADD-OPTIMIZER ERRSET ERRSET-*CATCH)
(DEFUN ERRSET-*CATCH (X)
    `(LET ((SI:EH-ERRSET-STATUS T)
	   (SI:EH-ERRSET-PRINT-MSG ,(COND ((CDDR X)(CADDR X)) (T 'T))))
	  (*CATCH 'SI:ERRSET-CATCH (LIST ,(CADR X)))))

(ADD-OPTIMIZER ERR ERR-*THROW)
(DEFUN ERR-*THROW (X)
    (LET ((VALUE-FORM (CADR X))
	  (FLAG (CADDR X)))
      (COND (FLAG (ERROR '|ERR with two arguments is not implemented|))
	    ((NULL VALUE-FORM) '(ERROR ""))
	    (T `(COND (SI:EH-ERRSET-STATUS (*THROW 'SI:ERRSET-CATCH ,VALUE-FORM))
		      (T (ERROR "")))))))

(DEFPROP SI:EH-ERRSET-STATUS (SI EH-ERRSET-STATUS) MAGIC-PACKAGE-FLAG)
(DEFPROP SI:EH-ERRSET-PRINT-MSG (SI EH-ERRSET-PRINT-MSG) MAGIC-PACKAGE-FLAG)
(DEFPROP SI:ERRSET-CATCH (SI ERRSET-CATCH) MAGIC-PACKAGE-FLAG)
)) ;END IF-FOR-MACLISP

;Make PROGV work compiled.
;The resulting code will only work in CAR/CDR of NIL = NIL mode.
(ADD-OPTIMIZER PROGV PROGV-EXPAND)
(DEFUN PROGV-EXPAND (FORM)
   (LET ((VARNAMES (CADR FORM)) (VALS (CADDR FORM)) (BODY (CDDDR FORM)))
	`(PROG ((PROGV-VARS ,VARNAMES) (PROGV-VALS ,VALS))
	  LOOP (COND (PROGV-VARS
		      (INHIBIT-STYLE-WARNINGS
		         (BIND (INHIBIT-STYLE-WARNINGS (VALUE-CELL-LOCATION (CAR PROGV-VARS)))
			       (CAR PROGV-VALS)))
		      (SETQ PROGV-VARS (CDR PROGV-VARS))
		      (SETQ PROGV-VALS (CDR PROGV-VALS))
		      (GO LOOP)))
	       (RETURN (PROGN . ,BODY)))))

;Turn PROG1 into PROG2 since that is open-coded.
(ADD-OPTIMIZER PROG1 PROG1-PROG2)
(DEFUN PROG1-PROG2 (FORM)
    `(PROG2 NIL . ,(CDR FORM)))

(DEFPROP CONS (CONS-NCONS CONS-LIST) OPTIMIZERS)

;Turn (CONS foo NIL) into (NCONS foo), saving one instruction.
(DEFUN CONS-NCONS (FORM)
    (COND ((MEMBER (CADDR FORM) '(NIL 'NIL))
	   `(NCONS ,(CADR FORM)))
	  (T FORM)))

;Turn (CONS X (CONS Y NIL)) into (LIST X Y).  Doesn't change (CONS X NIL), though.
;Perhaps we want a hairier criterion, for the sake of
;those times when you create a list you are going to RPLACA
;and don't want LIST to be used.
(DEFUN CONS-LIST (FORM)
    (COND ((ATOM (CADDR FORM)) FORM)
	  ((EQ (CAADDR FORM) 'CONS)
	   (LET ((TEM (CONS-LIST (CADDR FORM))))
		(COND ((EQ (CAR TEM) 'LIST)
		       `(LIST ,(CADR FORM) . ,(CDR TEM)))
		      ((MEMBER (CADDR TEM) '(NIL 'NIL))
		       `(LIST ,(CADR FORM) ,(CADR TEM)))
		      (T FORM))))
	  (T FORM)))

;The following are here to make list-type structures work more efficiently.
;It's easier to put the optimization in the compiler than in DEFSTRUCT.

(ADD-OPTIMIZER NTH NTH-OPTIMIZE)
(ADD-OPTIMIZER NTHCDR NTHCDR-OPTIMIZE)

(DEFUN NTH-OPTIMIZE (X)
  (LET ((TEM (ASSOC (CADR X) '((0 . CAR) (1 . CADR) (2 . CADDR) (3 . CADDDR)))))
       (COND (TEM `(,(CDR TEM) ,(CADDR X)))
	     (T X))))

(DEFUN NTHCDR-OPTIMIZE (X)
  (LET ((TEM (ASSOC (CADR X) '((1 . CDR) (2 . CDDR) (3 . CDDDR) (4 . CDDDDR)))))
    (COND ((EQUAL (CADR X) 0) (CADDR X))
	  (TEM `(,(CDR TEM) ,(CADDR X)))
	  (T X))))


(DEFUN *LEXPR FEXPR (L)
  (DOLIST (X L)
     (SETQ FUNCTIONS-DEFINED
	   (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
                (CONS X FUNCTIONS-DEFINED)))
     (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-EVAL)))
	      'ARGDESC)))

(DEFUN *EXPR FEXPR (L)
  (DOLIST (X L)
     (SETQ FUNCTIONS-DEFINED
	   (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
                (CONS X FUNCTIONS-DEFINED)))
     (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-EVAL)))
	      'ARGDESC)))

(DEFUN *FEXPR FEXPR (L)
  (DOLIST (X L)
    (SETQ FUNCTIONS-DEFINED
	  (LET #Q ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA)) #M NIL 
	       (CONS X FUNCTIONS-DEFINED)))
    (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-QT)))
	     'ARGDESC)))

;Style checkers are, unlike optimizers or macro definitions,
;run only on user-supplied input, not the results of expansions.
;Also, they are not expected to return any values.
;They do not alter the input, merely print warnings if there
;is anything ugly in it.

;Style checkers are used to implement RUN-IN-MACLISP-SWITCH
;and OBSOLETE-FUNCTION-WARNING-SWITCH.  They can also warn
;about anything else that is ugly or frowned upon, though legal.

(DEFPROP GETCHARN OBSOLETE STYLE-CHECKER)
(DEFPROP GETCHAR OBSOLETE STYLE-CHECKER)
(DEFPROP IMPLODE OBSOLETE STYLE-CHECKER)
(DEFPROP MAKNAM OBSOLETE STYLE-CHECKER)
(DEFPROP EXPLODE OBSOLETE STYLE-CHECKER)
(DEFPROP EXPLODEC OBSOLETE STYLE-CHECKER)
(DEFPROP EXPLODEN OBSOLETE STYLE-CHECKER)
(DEFPROP STATUS OBSOLETE STYLE-CHECKER)
(DEFPROP SSTATUS OBSOLETE STYLE-CHECKER)
(DEFPROP SAMEPNAMEP OBSOLETE STYLE-CHECKER)

(DEFUN OBSOLETE (FORM)
    (AND OBSOLETE-FUNCTION-WARNING-SWITCH
	 (NOT RUN-IN-MACLISP-SWITCH)
	 (BARF (CAR FORM) '|obsolete function used| 'WARN)))

;; I guess these have to be on SYSTEM for this to work.
(DEFPROP MAKNUM UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP MUNKAM UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP *REARRAY UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP *FUNCTION UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP SUBRCALL UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP LSUBRCALL UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP PNGET UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP PNPUT UNIMPLEMENTED STYLE-CHECKER)
(DEFPROP FSC UNIMPLEMENTED STYLE-CHECKER)

(DEFUN UNIMPLEMENTED (FORM)
    (BARF (CAR FORM) '|Maclisp function, not implemented in Lisp machine| 'WARN))

(COMMENT
;;; These are commented out because the style checker doesn't really manage
;;; to operate only on the user's typed-in code, and
;;; calls to these with one or zero args are generated by optimizers and macros, etc.
  (DEFPROP OR NEED-TWO-ARGS STYLE-CHECKER)
  (DEFPROP AND NEED-TWO-ARGS STYLE-CHECKER)
  (DEFPROP PROGN NEED-TWO-ARGS STYLE-CHECKER))

(DEFPROP PROG1 NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP PROG2 NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP + NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP * NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP PLUS NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP TIMES NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP QUOTIENT NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP DIFFERENCE NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP NCONC NEED-TWO-ARGS STYLE-CHECKER)
(DEFPROP APPEND NEED-TWO-ARGS STYLE-CHECKER)
(DEFUN NEED-TWO-ARGS (FORM)
    (COND ((NULL (CDDR FORM))
	   (BARF FORM '|Less than two arguments| 'WARN))))

(DEFPROP SETQ NEED-AN-ARG STYLE-CHECKER)
(DEFPROP PSETQ NEED-AN-ARG STYLE-CHECKER)
;Note:  one arg will cause an error, so who needs a warning too?

(DEFPROP COND NEED-AN-ARG STYLE-CHECKER)
(DEFPROP - NEED-AN-ARG STYLE-CHECKER)
(DEFPROP // NEED-AN-ARG STYLE-CHECKER)
(DEFUN NEED-AN-ARG (FORM)
    (OR (CDR FORM)
	(BARF (CAR FORM) '|with no arguments| 'WARN)))

;ARGDESC properties for functions with hairy eval/quote argument patterns
(DEFPROP ARRAY ((2 (FEF-ARG-REQ FEF-QT-QT)) (20 (FEF-ARG-OPT FEF-QT-EVAL))) ARGDESC)

;;; Style-checkers for things that don't work in Maclisp.

;These symbols don't exist in Maclisp, though they could, but they are likely losers.
(DEFPROP LISTP NOT-MACLISP STYLE-CHECKER)
(DEFPROP NLISTP NOT-MACLISP STYLE-CHECKER)
(DEFPROP NSYMBOLP NOT-MACLISP STYLE-CHECKER)

;These functions can't be added to Maclisp by a user.
(DEFPROP *CATCH NOT-MACLISP STYLE-CHECKER)
(DEFPROP *THROW NOT-MACLISP STYLE-CHECKER)
(DEFPROP UNWIND-PROTECT NOT-MACLISP STYLE-CHECKER)
(DEFPROP INTERN-LOCAL NOT-MACLISP STYLE-CHECKER)
(DEFPROP INTERN-SOFT NOT-MACLISP STYLE-CHECKER)
(DEFPROP INTERN-LOCAL-SOFT NOT-MACLISP STYLE-CHECKER)
(DEFPROP MAKE-ARRAY NOT-MACLISP STYLE-CHECKER)
(DEFPROP G-L-P NOT-MACLISP STYLE-CHECKER)
(DEFPROP ARRAY-LEADER NOT-MACLISP STYLE-CHECKER)
(DEFPROP STORE-ARRAY-LEADER NOT-MACLISP STYLE-CHECKER)
(DEFPROP MULTIPLE-VALUE NOT-MACLISP STYLE-CHECKER)
(DEFPROP MULTIPLE-VALUE-LIST NOT-MACLISP STYLE-CHECKER)
(DEFPROP MULTIPLE-VALUE-RETURN NOT-MACLISP STYLE-CHECKER)
(DEFPROP DO-NAMED NOT-MACLISP STYLE-CHECKER)
(DEFPROP RETURN-FROM NOT-MACLISP STYLE-CHECKER)
(DEFPROP RETURN-LIST NOT-MACLISP STYLE-CHECKER)
(DEFPROP BIND NOT-MACLISP STYLE-CHECKER)
(DEFPROP COMPILER-LET NOT-MACLISP STYLE-CHECKER)
(DEFPROP LOCAL-DECLARE NOT-MACLISP STYLE-CHECKER)
(DEFPROP CONS-IN-AREA NOT-MACLISP STYLE-CHECKER)
(DEFPROP LIST-IN-AREA NOT-MACLISP STYLE-CHECKER)
(DEFPROP NCONS-IN-AREA NOT-MACLISP STYLE-CHECKER)
(DEFPROP VALUE-CELL-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP CAR-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP PROPERTY-CELL-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP FUNCTION-CELL-LOCATION NOT-MACLISP STYLE-CHECKER)
(DEFPROP FSET NOT-MACLISP STYLE-CHECKER)
(DEFPROP FBOUNDP NOT-MACLISP STYLE-CHECKER)
(DEFPROP FSYMEVAL NOT-MACLISP STYLE-CHECKER)
(DEFPROP CLOSURE NOT-MACLISP STYLE-CHECKER)

(DEFUN NOT-MACLISP (FORM)
    (AND RUN-IN-MACLISP-SWITCH
	 (BARF (CAR FORM) '|does not exist in Maclisp| 'WARN)))

;Return with more than one argument won't work in Maclisp.
(DEFPROP RETURN RETURN-STYLE STYLE-CHECKER)
(DEFUN RETURN-STYLE (FORM)
    (AND RUN-IN-MACLISP-SWITCH
	 (CDDR FORM)
	 (BARF FORM '|returning multiple values doesn't work in Maclisp| 'WARN)))

;Named PROGs don't work in Maclisp.  PROG variables can't be initialized.
;Also, lots of tags and things like a GO to a RETURN are ugly.
(DEFPROP PROG PROG-STYLE STYLE-CHECKER)
(DEFUN PROG-STYLE (FORM)
    (PROG (PROGNAME)
	  (AND (ATOM (CADR FORM))
	       (CADR FORM)
	       (PROGN (SETQ PROGNAME (CADR FORM))
		      (SETQ FORM (CDR FORM))))
	  (COND (RUN-IN-MACLISP-SWITCH
		 (AND PROGNAME
                      (BARF PROGNAME '|PROG names won't work in Maclisp| 'WARN))
		 (OR (EVERY (CADR FORM) (FUNCTION ATOM))
		     (BARF (CADR FORM) '|PROG variables can't be initialized in Maclisp|
			   'WARN))))))

;; Check a LAMBDA for things that aren't allowed in Maclisp.
;; Called only if RUN-IN-MACLISP-SWITCH is set.
(DEFUN LAMBDA-STYLE (LAMBDA-EXP)
    (COND ((EQ (CAR LAMBDA-EXP) 'NAMED-LAMBDA)
	   (BARF 'NAMED-LAMBDA '|does not work in Maclisp| 'WARN)
	   (POP LAMBDA-EXP)))
    (DO ((VARLIST (CADR LAMBDA-EXP) (CDR VARLIST)) (KWDBARF)) ((NULL VARLIST))
       (COND ((ATOM (CAR VARLIST))
	      (AND (NOT KWDBARF)
		   (MEMQ (CAR VARLIST) LAMBDA-LIST-KEYWORDS)
		   (SETQ KWDBARF T)
		   (BARF (CAR VARLIST)
			 '|LAMBDA-list keywords aren't allowed in Maclisp| 'WARN)))
	     (T (BARF (CAR VARLIST)
		      '|LAMBDA variables can't have initializations in Maclisp|
		      'WARN)))))
