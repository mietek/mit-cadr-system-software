;;; DEFMACRO					-*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; THIS FILE MUST 
;;;   1) COMPILE in both QCOMPL and QCMP, and
;;;   2) RUN in both MACLISP and LISPM.  
;;; USE CAUTION!!

;;; This defun'inition needed to compile this file
(EVAL-WHEN (EVAL COMPILE)
   (COND ((AND (NOT (STATUS FEATURE LISPM))
	       (NOT (GET 'MACRO 'MACRO)))

;  (DEFUN (FOO MACRO) (X) ...) currently makes FOO appear in the output file
;  regardless of the setting of the MACROS switch.  Since DEFMACRO produces
;  (MACRO FOO ...) forms, the MACRO macro must be defined as below or else
;  all macros defined with DEFMACRO will appear in the output file.
;  Hopefully, this will be fixed soon.

	  (DEFUN MACRO MACRO (X)  
		 `(DEFUN ,(CADR X) MACRO ,(CADDR X) . ,(CDDDR X)))

;Is this needed?
;	  (MACRO IF-IN-MACLISP (X)
;		 (COND ((NOT (STATUS FEATURE LISPM))
;			(CADR X))))
;	  (MACRO IF-IN-LISPM (X)
;		 (COND ((STATUS FEATURE LISPM)
;			(CADR X))))

	  (MACRO IF-FOR-MACLISP (X)
		 (COND ((AND (NOT (STATUS FEATURE LISPM))	;IN MACLISP
			     (OR (NOT (BOUNDP 'COMPILING-FOR-LISPM))
				 (NULL COMPILING-FOR-LISPM)))
			(CADR X))))

		)))

(DECLARE (SPECIAL DEFMACRO-CHECK-ARGS 		;runtime flag
		  *VARLIST* *VALLIST*))		;communication vars in CHEVEUX

(SETQ DEFMACRO-CHECK-ARGS T)

(DEFUN (DEFMACRO MACRO) (X) (DEFMACRO1 (CDR X) 'MACRO))

(DEFUN (MACRO-DISPLACE MACRO) (X)
       `(MACRO ,(CADR X) ,(CADDR X)
	       (DISPLACE ,(CAADDR X) (PROGN . ,(CDDDR X)))))

(DEFUN (DEFMACRO-DISPLACE MACRO) (X) (DEFMACRO1 (CDR X) 'MACRO-DISPLACE))

;; Onto this are pushed all the specified-flags of optional args
;; (such as, FOOP in &OPTIONAL (FOO 69 FOOP)).
(DECLARE (SPECIAL OPTIONAL-SPECIFIED-FLAGS))

;; X is the cdr of the DEFMACRO form.  TYPE is MACRO or MACRO-DISPLACE.
(DEFUN DEFMACRO1 (X TYPE)
  (LET (*VARLIST* *VALLIST* OPTIONAL-SPECIFIED-FLAGS)
    (LET ((PAIR (DEFMACRO-&MUMBLE-CHEVEUX (CADR X) '(CDR *MACROARG*) 0 (CADR X))))
     `(LOCAL-DECLARE ((ARGLIST . ,(CADR X)))
       (,TYPE ,(CAR X) (*MACROARG*)
	,@(COND ((AND DEFMACRO-CHECK-ARGS
		      (NOT (AND (ZEROP (CAR PAIR))
				(NULL (CDR PAIR)))))
		 `((AND ,(COND ((ZEROP (CAR PAIR))
				`(> (LENGTH *MACROARG*)
				    ,(1+ (CDR PAIR))))
			       ((NULL (CDR PAIR))
				`(< (LENGTH *MACROARG*)
				    ,(1+ (CAR PAIR))))
			       (T `(OR (< (LENGTH *MACROARG*)
				          ,(1+ (CAR PAIR)))
				       (> (LENGTH *MACROARG*)
					  ,(1+ (CDR PAIR))))))
			(ERROR '|-- wrong number of args to a macro.|
			       `(,',(CADR X) ,*MACROARG*)))))
		(T NIL))
	;; Don't use LET here if no optional-specified-flags, since that would
	;; cause infinite recursion since LET is defined with DEFMACRO
	,(COND (OPTIONAL-SPECIFIED-FLAGS
		`(LET ,OPTIONAL-SPECIFIED-FLAGS
		   ((LAMBDA ,*VARLIST* . ,(CDDR X))
		    . ,*VALLIST*)))
	       (`((LAMBDA ,*VARLIST* . ,(CDDR X))
		    . ,*VALLIST*))))))))

(DEFMACRO DESTRUCTURING-BIND (VARIABLES DATA . BODY)
  (LET (*VARLIST* *VALLIST* OPTIONAL-SPECIFIED-FLAGS)
    (DEFMACRO-&MUMBLE-CHEVEUX VARIABLES DATA 0 VARIABLES)
    `(LET ,OPTIONAL-SPECIFIED-FLAGS
       ((LAMBDA ,*VARLIST* . ,BODY)
	. ,*VALLIST*))))

;; STATE is 0 for mandatory args, 1 for optional args, 2 for rest args, 3 for aux vars.
;; If it is 4 or more, the 4 bit signifies &LIST-OF and the low two bits
;; are as usual.
;; PATH is the form which, using CAR and CDR, would extract the part of the macro arg
;; which corresponds to this arg and the following args at the same level.
;; Thus, a simple arg would be set to `(CAR ,PATH).
;; PATTERN is the rest of the arglist at this level.
;; We push arg names on *VARLIST* and their appropriate values on *VALLIST*.
;; We return a pair describing what we know, so far, about how many args the macro wants:
;; the car is the number of required args, and the cdr is the
;; maximum allowed number of args, or nil if any number are allowed.
(DEFUN DEFMACRO-&MUMBLE-CHEVEUX (PATTERN PATH STATE EPAT)
       (COND ((NULL PATTERN) (CONS 0 0))
	     ((ATOM PATTERN)
	      (COND ((> STATE 1) (ERROR '|-- bad pattern to DEFMACRO.| EPAT))
		    (T (DEFMACRO-CHEVEUX PATTERN PATH)
		       (NCONS 0))))
	     ((EQ (CAR PATTERN) '&OPTIONAL)
	      (COND ((> STATE 0) (ERROR '|-- bad pattern to DEFMACRO.| EPAT))
		    (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 1 EPAT))))
	     ((EQ (CAR PATTERN) '&REST)
	      (COND ((> STATE 1) (ERROR '|-- bad pattern to DEFMACRO.| EPAT))
		    (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 2 EPAT))))
	     ((EQ (CAR PATTERN) '&AUX)
	      (COND ((> STATE 2) (ERROR '|-- bad pattern to DEFMACRO.| EPAT))
		    (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 3 EPAT))))
	     ((EQ (CAR PATTERN) '&LIST-OF)
	      (COND ((< STATE 3)
		     (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH (+ 4 STATE) EPAT))
		    (T (ERROR '|-- bad pattern to DEFMACRO.| EPAT))))
	     ((= STATE 0)
	      (DEFMACRO-CHEVEUX (CAR PATTERN) (LIST 'CAR PATH))
	      (DEFMACRO-REQUIRED
	       (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 0 EPAT)))
	     ((= STATE 1)
	      (COND ((ATOM (CAR PATTERN))
		     (DEFMACRO-CHEVEUX (CAR PATTERN)
				       `(COND (,PATH (CAR ,PATH))
					      (T NIL))))
		    (T
		     (AND (CADDAR PATTERN)
			  (PUSH (CADDAR PATTERN) OPTIONAL-SPECIFIED-FLAGS))
		     (DEFMACRO-CHEVEUX (CAAR PATTERN)
				       `(COND (,PATH
					       ,(AND (CADDAR PATTERN)
						     `(SETQ ,(CADDAR PATTERN) T))
					       (CAR ,PATH))
					      (T ,(CADAR PATTERN))))))
	      (DEFMACRO-OPTIONAL
	       (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 1 EPAT)))
	     ((= STATE 2)
	      (DEFMACRO-CHEVEUX (CAR PATTERN) PATH)
	      (COND ((CDR PATTERN)
		     (AND (OR (ATOM (CDR PATTERN))
			      (NOT (EQ (CADR PATTERN) '&AUX)))
			  (ERROR '|-- bad pattern to DEFMACRO.| EPAT))
		     (DEFMACRO-&MUMBLE-CHEVEUX (CDDR PATTERN) PATH 3 EPAT)))
	      (NCONS 0))
	     ((= STATE 3)
	      (COND ((ATOM (CAR PATTERN))
		     (DEFMACRO-CHEVEUX (CAR PATTERN) NIL))
		    (T (DEFMACRO-CHEVEUX (CAAR PATTERN) (CADAR PATTERN))))
	      (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 3 EPAT))
	     ((= STATE 4)				;&LIST-OF not optional
	      (DEFMACRO-&LIST-OF-CHEVEUX (CAR PATTERN) `(CAR ,PATH))
	      (DEFMACRO-REQUIRED
	       (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) `(CDR ,PATH) 0 EPAT)))
	     ((= STATE 5)				;&LIST-OF optional
	      (AND (ATOM (CAR PATTERN)) (ERROR '|-- bad pattern to DEFMACRO.| EPAT))
	      (AND (CADDAR PATTERN)
		   (PUSH (CADDAR PATTERN) OPTIONAL-SPECIFIED-FLAGS))
	      (DEFMACRO-&LIST-OF-CHEVEUX (CAAR PATTERN)
					 `(COND (,PATH
						 ,(AND (CADDAR PATTERN)
						       `(SETQ ,(CADDAR PATTERN) T))
						 (CAR ,PATH))
					        (T ,(CADAR PATTERN))))
	      (DEFMACRO-OPTIONAL
	       (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) `(CDR ,PATH) 1 EPAT)))
	     ((= STATE 6)
	      (DEFMACRO-&LIST-OF-CHEVEUX (CAR PATTERN) PATH)
	      (COND ((CDR PATTERN)
		     (AND (OR (ATOM (CDR PATTERN))
			      (NOT (EQ (CADR PATTERN) '&AUX)))
			  (ERROR '|-- bad pattern to DEFMACRO.| EPAT))
		     (DEFMACRO-&MUMBLE-CHEVEUX (CDDR PATTERN) PATH 3 EPAT)))
	      (NCONS 0))
	     ))

(DEFUN DEFMACRO-&LIST-OF-CHEVEUX (PATTERN PATH)
  (SETQ *VALLIST*
	(LET (*VALLIST* (VALS *VALLIST*))
	  (DEFMACRO-CHEVEUX PATTERN 'X)
	  (DO ((NVALS (NREVERSE *VALLIST*) (CDR NVALS))
	       (VALS VALS
		     (CONS `(MAPCAR (FUNCTION
				      (LAMBDA (X) ,(CAR NVALS)))
			            ,PATH)
			   VALS)))
	      ((NULL NVALS) VALS)))))

(DEFUN DEFMACRO-CHEVEUX (PATTERN PATH)
       (COND ((NULL PATTERN))
	     ((ATOM PATTERN)
	      (SETQ *VARLIST* (CONS PATTERN *VARLIST*))
	      (SETQ *VALLIST* (CONS PATH *VALLIST*)))
	     (T 
	      (DEFMACRO-CHEVEUX (CAR PATTERN) (LIST 'CAR PATH))
	      (DEFMACRO-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH)))))

(DEFUN DEFMACRO-OPTIONAL (PAIR)
       (COND ((NULL (CDR PAIR)) PAIR)
	     (T (RPLACD PAIR (1+ (CDR PAIR))))))

(DEFUN DEFMACRO-REQUIRED (PAIR)
       (COND ((NULL (CDR PAIR)) (RPLACA PAIR (1+ (CAR PAIR))))
	     (T (RPLACA (RPLACD PAIR (1+ (CDR PAIR))) (1+ (CAR PAIR))))))

