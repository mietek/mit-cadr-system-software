; -*- Package: System-Internals; Mode: Lisp -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

; Trace package

;	"There is always a place for debugging.  No matter how
;	 hard you try to think of everything in advance, you
;	 will always find that there is something else that you
;	 hadn't thought of."
;			- My Life as a Mathematician
;			  by Hfpsh Dboups

;MISSING:
;	 - HAIRY DISPLAY FEATURES?
;	 - "TRACE-EDSUB"

(DECLARE 
    (DEFPROP TRACE ((1 (FEF-QT-QT FEF-ARG-REST))) ARGDESC)		;FEXPR
    (DEFPROP UNTRACE ((1 (FEF-QT-QT FEF-ARG-REST))) ARGDESC)		;FEXPR
    (DEFPROP TRACE-PRINT ((1005 (FEF-ARG-OPT FEF-QT-EVAL))) ARGDESC)	;LEXPR
    (SPECIAL TRACE-TABLE TRACE-LEVEL TRACE-OUTPUT INSIDE-TRACE TRACE-COMPILE-FLAG)
	;TRACE-OUTPUT IS WHERE TO SEND THE OUTPUT, NORMALLY BOUND TO 
        ;SYN TO STANDARD-OUTPUT.

	;TRACE-TABLE LIST OF FCNS TRACED
	;CAR	NAME OF FCN TRACED
	;CADR	NAME OF FCN WHEREIN, NIL IF NORMAL
	;CADDR	TRACING FUNCTION CELL CONTENTS
	;CADDDR	ORIGINAL FUNCTION CELL CONTENTS

	;TRACE-LEVEL is the total depth within all traced functions.
	;Used for indenting trace output.
	;INSIDE-TRACE is T when tracing of all functions should be disabled
	;because we are in the middle of doing the trace processing for a function.
)

(OR (BOUNDP 'TRACE-COMPILE-FLAG)
    (SETQ TRACE-COMPILE-FLAG NIL))



(DEFUN TRACE (&QUOTE &REST SPECS)
  (COND ((NOT (BOUNDP 'TRACE-TABLE)) ;INITIALIZATION KLUDGE
	 (SETQ TRACE-LEVEL 0)
	 (SETQ INSIDE-TRACE NIL)
	 (SETQ TRACE-TABLE NIL)
	 (FSET 'TRACE-APPLY (FSYMEVAL 'APPLY))
	 (AND (FBOUNDP 'STEP-APPLY)
	      (FSET 'TRACE-STEP-APPLY (FSYMEVAL 'STEP-APPLY)))))
  (COND ((NULL SPECS)
	 (MAPCAR (FUNCTION TRACE-2) TRACE-TABLE))
	((MAPCAR (FUNCTION TRACE-1) SPECS))))

(DEFUN UNTRACE (&QUOTE &REST FNS)
   (MAPCAR (FUNCTION UNTRACE-1) (OR FNS (TRACE))))

;;; A list in the args to UNTRACE is taken as a non-atomic function-name
;;; rather than a wherein-spec, as Maclisp would do, since UNTRACE WHEREIN
;;; is not implemented anyway, and since WHEREIN doesn't work that way in
;;; this TRACE anyway (that is, it still modifies the function cell.)
(DEFUN UNTRACE-1 (SPEC &AUX TEM (FDEFINE-FILE-SYMBOL NIL))
  (COND ((AND (SETQ TEM (ASSOC SPEC TRACE-TABLE))
	      (FDEFINEDP SPEC)
	      (EQ (CADDR TEM) (FDEFINITION SPEC))) ;IF CLOBBERED, JUST IGNORE
	 (FDEFINE SPEC (CADDDR TEM) NIL T)
	 (SETQ TRACE-TABLE (DELQ TEM TRACE-TABLE))))
  SPEC)

(DEFUN TRACE-1 (SPEC)
  (PROG (BREAK EXITBREAK ENTRYCOND EXITCOND WHEREIN ARGPDL ENTRY EXIT (ARG T) (VALUE T) 
	     STEP ENTRYPRINT EXITPRINT LAMBDA-NAME (BARFP T) (FDEFINE-FILE-SYMBOL NIL)
	     ENTRYVALS EXITVALS MUMBLE FCN NEW OLD OLD1 LAMBDA-LIST ARG-LIST-EXP TRFCN ERROR)
    (COND ((ATOM SPEC)
	   (SETQ FCN SPEC))
	  (T
	   (COND ((EQ (CAR SPEC) ':FUNCTION)
		  (SETQ FCN (CADR SPEC) SPEC (CDR SPEC)))
		 ((ATOM (CAR SPEC))
		  (SETQ FCN (CAR SPEC)))
		 ((DOLIST (FCN (CAR SPEC))
		    (TRACE-1 `(:FUNCTION ,FCN . ,(CDR SPEC))))))
	   (DO SPECS (CDR SPEC) (CDR SPECS) (NULL SPECS)
	     (SELECTQ (CAR SPECS)
		(:BREAK (SETQ BARFP SPECS SPECS (CDR SPECS) BREAK (CAR SPECS)))
		(:EXITBREAK (SETQ BARFP SPECS SPECS (CDR SPECS) EXITBREAK (CAR SPECS)))
		(:STEP (SETQ STEP T))
		(:ERROR (SETQ ERROR T))
		(:COND (SETQ BARFP SPECS SPECS (CDR SPECS))
		       (SETQ EXITCOND (SETQ ENTRYCOND (CAR SPECS))))
		(:ENTRYCOND (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRYCOND (CAR SPECS)))
		(:EXITCOND (SETQ BARFP SPECS SPECS (CDR SPECS) EXITCOND (CAR SPECS)))
		(:WHEREIN (SETQ BARFP SPECS SPECS (CDR SPECS) WHEREIN (CAR SPECS)))
		(:ARGPDL (SETQ BARFP SPECS SPECS (CDR SPECS) ARGPDL (CAR SPECS)))
		(:ENTRY (SETQ BARFP SPECS SPECS (CDR SPECS) ENTRY (CAR SPECS)))
		(:EXIT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CAR SPECS)))
		(:PRINT (SETQ BARFP SPECS
			      SPECS (CDR SPECS)
			      ENTRY (CONS (CAR SPECS) ENTRY)
			      EXIT (CONS (CAR SPECS) EXIT)))
		(:ENTRYPRINT (SETQ BARFP SPECS SPECS (CDR SPECS)
				   ENTRY (CONS (CAR SPECS) ENTRY)))
		(:EXITPRINT (SETQ BARFP SPECS SPECS (CDR SPECS) EXIT (CONS (CAR SPECS) EXIT)))
		((:ARG :VALUE :BOTH NIL)
		 (AND (EQ (CAR SPECS) ':ARG) (SETQ VALUE NIL))
		 (AND (EQ (CAR SPECS) ':VALUE) (SETQ ARG NIL))
		 (AND (EQ (CAR SPECS) NIL) (SETQ ARG NIL VALUE NIL))
		 (AND ARG (SETQ ENTRYVALS (CDR SPECS)))
		 (AND VALUE (SETQ EXITVALS (CDR SPECS)))
		 (RETURN NIL))
		(OTHERWISE
		 (SETQ MUMBLE (CAR SPECS))
		 (RETURN NIL)))
	     (AND (NULL BARFP) (FERROR NIL "Parameter missing")) )))
    (UNTRACE-1 FCN)
    (AND MUMBLE (RETURN (FERROR NIL "Meaningless TRACE keyword: ~S" MUMBLE)))
    (OR (SYMBOLP ARGPDL)
	(RETURN (FERROR NIL "ARGPDL specification ~S isn't a symbol" ARGPDL)))
    (SETQ LAMBDA-LIST (TRACE-LAMBDA-LIST (FDEFINITION FCN)))
    (OR (EQUAL (CDR LAMBDA-LIST) '(&REST ARGLIST))
        (SETQ ARG-LIST-EXP
	  (DO ((VL (DO ((LL LAMBDA-LIST (CDR LL))
		        (VL NIL))
		       ((NULL LL) VL)
		     (COND ((EQ (CAR LL) '&REST)
			    (RETURN VL))
			   ((MEMQ (CAR LL) LAMBDA-LIST-KEYWORDS))
			   ((SETQ VL (CONS (CAR LL) VL)))))
		   (CDR VL))
	       (AL 'ARGLIST (LIST 'CONS (CAR VL) AL)))
	      ((NULL VL) AL))))
    (SETQ TRFCN		;A symbol whose value is the level number, function is the old defn
	  (COND ((SYMBOLP FCN) (COPYSYMBOL FCN NIL))
		(T (GENSYM))))
    (SETQ ENTRYPRINT `(TRACE-PRINT ,TRFCN 'ENTER ',FCN ',ARG ',ENTRY ',ENTRYVALS))
    (SETQ EXITPRINT `(TRACE-PRINT ,TRFCN 'EXIT ',FCN ',VALUE ',EXIT ',EXITVALS))
    (SET TRFCN 0)
    (SETQ OLD (FDEFINITION FCN))
    (SETQ LAMBDA-NAME `(,FCN (TRACE ,TRFCN)))
    (SETQ NEW
          `(NAMED-LAMBDA ,LAMBDA-NAME ,LAMBDA-LIST
            (PROG* (,@(AND ARG-LIST-EXP `((ARGLIST ,ARG-LIST-EXP)))
		    ,@(AND ARGPDL `((,ARGPDL (CONS (LIST (1+ ,TRFCN) ',FCN ARGLIST)
						   ,ARGPDL))))
		    FNVALUES
		    (,TRFCN (1+ ,TRFCN))
		    (TRACE-LEVEL (1+ TRACE-LEVEL)))
                  ;; End of PROG var list.
		  ,(IF ERROR `(PROGN (CERROR T NIL ':TRACE-ERROR-BREAK "~S entered" ',FCN)
				     (RETURN (APPLY ',TRFCN ARGLIST)))
		   `(COND ((OR INSIDE-TRACE
			       . ,(AND WHEREIN `((NOT (FUNCTION-ACTIVE-P ',WHEREIN)))))
			   ;; (RETURN-LIST (MULTIPLE-VALUE-LIST ...)) doesn't work!
			   (SETQ FNVALUES (MULTIPLE-VALUE-LIST (APPLY ',TRFCN ARGLIST)))
			   (RETURN-LIST FNVALUES))
			  (T (LET ((INSIDE-TRACE T))
			       ,(COND ((NULL ENTRYCOND) ENTRYPRINT)
				      (T (LIST 'AND ENTRYCOND ENTRYPRINT)))
			       ,@(AND BREAK `((AND ,BREAK (LET (INSIDE-TRACE)
							       (BREAK ,FCN T)))))
			       (SETQ FNVALUES
				     (LET ((INSIDE-TRACE NIL))
					  (MULTIPLE-VALUE-LIST
					   (,(COND ((NOT STEP) 'TRACE-APPLY)
						   (T 'TRACE-STEP-APPLY))
					    ',TRFCN
					    ARGLIST))))
			       ,(COND ((NULL EXITCOND) EXITPRINT)
				      (T (LIST 'AND EXITCOND EXITPRINT)))
			       ,@(AND EXITBREAK `((AND ,EXITBREAK (LET (INSIDE-TRACE)
								       (BREAK ,FCN T)))))
			       (RETURN-LIST FNVALUES))))))))
    (SETQ OLD1 OLD)
    (AND (LISTP OLD) (EQ (CAR OLD) 'MACRO)
	 (SETQ OLD (CDR OLD) NEW (CONS 'MACRO NEW)))
    (SETQ TRACE-TABLE (CONS (LIST FCN NIL NEW OLD1) TRACE-TABLE))
    (FSET TRFCN OLD)
    (FDEFINE FCN NEW NIL T)
    (COND (TRACE-COMPILE-FLAG
	   (LET ((LOCAL-DECLARATIONS
		   (CONS `(SPECIAL ,TRFCN FNVALUES ARGLIST)
			 LOCAL-DECLARATIONS)))
	     (COMPILE FCN))
	   (SETF (CADDR (ASSOC FCN TRACE-TABLE))	;Change NEW
		 (FDEFINITION FCN))))
    (RETURN FCN)))

(DEFUN TRACE-2 (X)
   (COND ((NULL (CADR X))
	  (CAR X))
	 ((LIST (CAR X) 'WHEREIN (CADR X)))))


(DEFUN TRACE-PRINT (DEPTH DIRECTION FUNCTION PRINT-ARGS-FLAG EXTRAS-1 EXTRAS-2)
 (LOCAL-DECLARE ((SPECIAL ARGLIST FNVALUES))
  (TERPRI TRACE-OUTPUT)
  (DO N (* 2 TRACE-LEVEL) (1- N) (NOT (> N 2))
    (TYO #\SP TRACE-OUTPUT))
  (FORMAT TRACE-OUTPUT "(~D ~A ~S" DEPTH DIRECTION FUNCTION)
  (COND (PRINT-ARGS-FLAG
	 (PRINC "  " TRACE-OUTPUT)
	 (COND ((EQ DIRECTION 'ENTER)
		(PRIN1 ARGLIST TRACE-OUTPUT))
	       ((CDR FNVALUES)
		(FORMAT TRACE-OUTPUT "VALUES: ~S" FNVALUES))
	       (T (PRIN1 (CAR FNVALUES) TRACE-OUTPUT)))))
  (COND (EXTRAS-1
	 (PRINC "  \\" TRACE-OUTPUT)
	 (DOLIST (E EXTRAS-1)
	     (PRINC " " TRACE-OUTPUT)
	     (PRIN1 (EVAL E) TRACE-OUTPUT))))
  (COND (EXTRAS-2
	 (PRINC "  ////" TRACE-OUTPUT)
	 (DOLIST (E EXTRAS-2)
	     (PRINC " " TRACE-OUTPUT)
	     (PRIN1 (EVAL E) TRACE-OUTPUT))))
  (PRINC ")" TRACE-OUTPUT)))

;FUNCTION IN, LAMBDA LIST SUITABLE FOR TRACING
;THAT FUNCTION OUT, OR AN ERROR IS GENERATED
;TRIES TO GENERATE JUST (&EVAL &REST ARGLIST).

(DEFUN TRACE-LAMBDA-LIST (FCN)
  (COND ((LISTP FCN)
	 (SELECTQ (CAR FCN)
	   (LAMBDA (TRACE-CONVERT-LAMBDA (CADR FCN)))
	   (NAMED-LAMBDA (TRACE-CONVERT-LAMBDA (CADDR FCN)))
	   (OTHERWISE '(&REST ARGLIST))))
	(T ;A compiled or microcode function
	  (LET ((ARGS-INFO (%ARGS-INFO FCN)))
	    (COND ((BIT-TEST %ARG-DESC-INTERPRETED ARGS-INFO)
		   ;; For a random type of function, assume no quoting
		   '(&EVAL &REST ARGLIST))
		  ((BIT-TEST %ARG-DESC-FEF-QUOTE-HAIR ARGS-INFO)
		   (TRACE-CONVERT-FEF (GET-MACRO-ARG-DESC-POINTER FCN)))
		  ((BIT-TEST %ARG-DESC-QUOTED-REST ARGS-INFO)
		   (LET ((NARGS (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)))
		     (OR (= (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO) NARGS)
			 (FERROR NIL "The function ~S has overly hairy argument quoting"
				     FCN))
		     (DO ((N NARGS (1- N))
			  (LL '(&QUOTE &REST ARGLIST) (CONS (GENSYM) LL)))
			 ((= N 0) LL))))
		  (T ;an ordinary compiled function, no quoting to worry about
		   '(&EVAL &REST ARGLIST)))))))

(DEFUN TRACE-CONVERT-LAMBDA (LL
	&AUX EVARG QUARG EVOPT QUOPT EVREST QUREST)
  ;FIRST DETERMINE WHAT TYPES OF EVALAGE AND QUOTAGE ARE PRESENT (SET ABOVE AUX VARS)
  (DO ((L LL (CDR L))
       (ITEM)
       (OPTIONALP NIL)
       (QUOTEP NIL)
       (RESTP NIL))
      ((NULL L))
    (SETQ ITEM (CAR L))
    (COND ((EQ ITEM '&AUX)
	   (RETURN NIL))
	  ((OR (EQ ITEM '&EVAL) (EQ ITEM '&QUOTE-DONTCARE))
	   (SETQ QUOTEP NIL))
	  ((EQ ITEM '&QUOTE)
	   (SETQ QUOTEP T))
	  ((EQ ITEM '&OPTIONAL)
	   (SETQ OPTIONALP T))
	  ((EQ ITEM '&REST)
	   (SETQ RESTP T))
	  ((MEMQ ITEM LAMBDA-LIST-KEYWORDS))
	  (RESTP
	   (COND (QUOTEP (SETQ QUREST T))
		 (T (SETQ EVREST T)))
	   (RETURN NIL))
	  (OPTIONALP
	   (COND (QUOTEP (SETQ QUOPT T))
		 (T (SETQ EVOPT T))))
	  (T (COND (QUOTEP (SETQ QUARG T))
		   (T (SETQ EVARG T))))))
  ;DECIDE HOW HAIRY A LAMBDA LIST IS NEEDED
  (COND ((OR (AND EVOPT (OR QUOPT QUREST)) (AND QUOPT (OR EVOPT EVREST)))
	 'OVERLY-HAIRY-ARG-QUOTAGE)
	((AND (NOT QUARG) (NOT QUOPT) (NOT QUREST))
	 '(&EVAL &REST ARGLIST))
	((AND QUARG (NOT EVOPT) (NOT EVREST))
	 '(&QUOTE &REST ARGLIST))
	(T	;NEED A HAIRY ONE.
	  (NRECONC
	    (DO ((L LL (CDR L))
	         (LAMBDA-LIST NIL)
	         (ITEM))
	        ((NULL L) LAMBDA-LIST)
	      (SETQ ITEM (CAR L))
	      (COND ((MEMQ ITEM '(&AUX &OPTIONAL &REST))
		     (RETURN LAMBDA-LIST))
		    ((MEMQ ITEM '(&EVAL &QUOTE &QUOTE-DONTCARE))
		     (SETQ LAMBDA-LIST (CONS ITEM LAMBDA-LIST)))
		    ((MEMQ ITEM LAMBDA-LIST-KEYWORDS))
		    (T
		     (SETQ LAMBDA-LIST (CONS (GENSYM) LAMBDA-LIST)))))
	    (LIST (COND (QUREST '&QUOTE) (T '&EVAL)) '&REST 'ARGLIST)))))

(DEFUN TRACE-CONVERT-FEF (LL
	&AUX EVARG QUARG EVOPT QUOPT EVREST QUREST)
  ;FIRST DETERMINE WHAT TYPES OF EVALAGE AND QUOTAGE ARE PRESENT (SET ABOVE AUX VARS)
  (DO ((L LL (CDR L))
       (ITEM)
       (SYNTAX)
       (QUOTE))
      ((NULL L))
    (SETQ ITEM (CAR L))
    (OR (ZEROP (LOGAND %FEF-NAME-PRESENT ITEM)) (SETQ L (CDR L)))
    (SETQ SYNTAX (MASK-FIELD %%FEF-INIT-OPTION ITEM))	;SKIP EXTRA INIT Q
    (OR (= SYNTAX FEF-INI-NONE) (= SYNTAX FEF-INI-NIL) (= SYNTAX FEF-INI-SELF)
	(SETQ L (CDR L)))
    (SETQ SYNTAX (MASK-FIELD %%FEF-ARG-SYNTAX ITEM))
    (SETQ QUOTE (> (MASK-FIELD %%FEF-QUOTE-STATUS ITEM) FEF-QT-EVAL))
    (COND ((> SYNTAX FEF-ARG-REST)
	   (RETURN NIL))
	  ((= SYNTAX FEF-ARG-REST)
	   (COND ((NOT QUOTE)
		  (SETQ EVREST T))
		 ((SETQ QUREST T)))
	   (RETURN NIL))
	  ((= SYNTAX FEF-ARG-OPT)
	   (COND ((NOT QUOTE)
		  (SETQ EVOPT T))
		 ((SETQ QUOPT T))))
	  ((COND ((NOT QUOTE)
		  (SETQ EVARG T))
		 ((SETQ QUARG T))))))
  ;DECIDE HOW HAIRY A LAMBDA LIST IS NEEDED
  (COND ((OR (AND EVOPT (OR QUOPT QUREST)) (AND QUOPT (OR EVOPT EVREST)))
	 'OVERLY-HAIRY-ARG-QUOTAGE)
	((AND (NOT QUARG) (NOT QUOPT) (NOT QUREST))
	 '(&EVAL &REST ARGLIST))
	((AND QUARG (NOT EVOPT) (NOT EVREST))
	 '(&QUOTE &REST ARGLIST))
	(T	;NEED A HAIRY ONE.
	  (NRECONC
	    (DO ((L LL (CDR L))
	         (LAMBDA-LIST NIL)
		 (OQUOTE NIL QUOTE)
		 (QUOTE)
	         (ITEM))
	        ((NULL L) LAMBDA-LIST)
	      (SETQ ITEM (CAR L))
	      (OR (ZEROP (LOGAND %FEF-NAME-PRESENT ITEM)) (SETQ L (CDR L)))
	      (SETQ QUOTE (> (MASK-FIELD %%FEF-QUOTE-STATUS ITEM) FEF-QT-EVAL))
	      (OR (= FEF-ARG-REQ (MASK-FIELD %%FEF-ARG-SYNTAX ITEM))
		  (RETURN LAMBDA-LIST))
	      (OR (EQ QUOTE OQUOTE)
		  (SETQ LAMBDA-LIST (CONS (COND ((NOT QUOTE) '&EVAL)
						(T '&QUOTE))
					  LAMBDA-LIST)))
	      (SETQ LAMBDA-LIST (CONS (GENSYM) LAMBDA-LIST)))
	    (LIST (COND (QUREST '&QUOTE) (T '&EVAL)) '&REST 'ARGLIST)))))

; SEE IF A FUNCTION IS CURRENTLY ACTIVE
(DEFUN FUNCTION-ACTIVE-P (FN &AUX SG RP)
       (SETQ SG %CURRENT-STACK-GROUP)
       (SETQ RP (SG-REGULAR-PDL SG))
       (DO ((FNVAL (FDEFINITION FN))
	    (INIFN (SG-INITIAL-FUNCTION-INDEX SG))
	    (AP (%POINTER-DIFFERENCE (%STACK-FRAME-POINTER) RP)
                (- AP (RP-DELTA-TO-ACTIVE-BLOCK RP AP))))
	   (NIL)
	 (COND ((EQ FNVAL (RP-FUNCTION-WORD RP AP))
		(RETURN AP))
	       (( AP INIFN) (RETURN NIL)))))

;;; Display Features

(DECLARE (SPECIAL TRACE-POP-UP-WINDOW TRACE-POP-UP-MENU))
;; Items in this menu are lists of the form:
;;  ("name" :VALUE (S-expr-arg-p . what-to-append-into-trace-options))
;;			^-- if this is UNTRACE, QUIT, or DO-IT, that special function
;;			if NIL, nothing special
;;			otherwise is prompt for reading what goes into trace options
;; Try to keep this so it comes out in 3 columns
(SETQ TRACE-POP-UP-MENU
      (<- MENU-CLASS ':NEW
	  ':NAME "Trace Options"
	  ':ITEM-LIST `(("Break before" :VALUE (NIL :BREAK T))
			("Break after" :VALUE (NIL :EXITBREAK T))
			("Step" :VALUE (NIL :STEP))
			("Print" :VALUE
				("Form to evaluate and print in trace messages" :PRINT))
			("Print before" :VALUE
				("Form to evaluate and print before calling" :ENTRYPRINT))
			("Print after" :VALUE
				("Form to evaluate and print after returning" :EXITPRINT))
			("Conditional" :VALUE ("Predicate for tracing" :COND))
			("Cond before" :VALUE ("Predicate for tracing calls" :ENTRYCOND))
			("Cond after" :VALUE ("Predicate for tracing returns" :EXITCOND))
			("Cond break before" :VALUE
				("Predicate for breaking before" :BREAK))
			("Cond break after" :VALUE
				("Predicate for breaking after" :EXITBREAK))
			("ARGPDL" :VALUE ("Arg pdl variable" :ARGPDL))
			("Wherein" :VALUE ("Function within which to trace" :WHEREIN))
			("Untrace" :VALUE (UNTRACE))
			("Quit" :VALUE (QUIT))
			("Do it" :VALUE (DO-IT)))))

;;; This function is invoked in the momentary menu process when the user clicks "trace"
;;; and in the editor process by the editor's Trace command.
;;; If the function isn't supplied as an argument the user is asked for it.
(DEFUN TRACE-VIA-MENUS (&OPTIONAL FCN)
  (OR (BOUNDP 'TRACE-POP-UP-WINDOW)
      (SETQ TRACE-POP-UP-WINDOW (<- POP-UP-TEXT-WINDOW-CLASS ':NEW
				    ':NAME 'TRACE-POP-UP-WINDOW)))
  (*CATCH 'TOP-LEVEL	;In case of c-Z out of the READ
    (UNWIND-PROTECT (LET ((STREAM (<- TRACE-POP-UP-WINDOW ':STREAM)))
		      (<- TRACE-POP-UP-WINDOW ':SIZE<- 1000 300)
		      (<- TRACE-POP-UP-WINDOW  ':MOVE-NEAR MOUSE-X MOUSE-Y)
		      (<- TRACE-POP-UP-WINDOW  ':POP-UP)
		      (OR FCN
			  (PROG ()
			    ;Make sure blinker is blinking
			    (TV-SET-BLINKER-VISIBILITY
			      (CAR (PC-PPR-BLINKER-LIST (<- TRACE-POP-UP-WINDOW ':PC-PPR)))
			      ':BLINK)
			    (FORMAT STREAM "Type in name of function to be traced or untraced.
 Control-Z quits.~%")
			   RETRY
			    (LET ((STANDARD-OUTPUT STREAM) ;Just for the "QUIT" c-Z types.
				  (STANDARD-INPUT STREAM)) ;Function doesn't take an arg!
			      (SETQ FCN (READ-FOR-TOP-LEVEL)))
			    (COND ((NOT (FDEFINEDP FCN))
				   (FORMAT STREAM " ;not a defined function, try again~%")
				   (GO RETRY)))))
		      (<- TRACE-POP-UP-MENU ':MOVE-NEAR-WINDOW TRACE-POP-UP-WINDOW)
		      (DO ((FORM (IF (ATOM FCN) `(TRACE (,FCN)) `(TRACE (:FUNCTION ,FCN))))
			   (UNTRACE-MODE NIL)
			   (CHOICE) (OPTION) (ARG))
			  (NIL)
			;Put the current status on the text window
			(FUNCALL STREAM ':CLEAR-SCREEN)
			(GRIND-TOP-LEVEL FORM 76 STREAM) ;76 is width in characters
			;Not listening to the keyboard any more, shut off blinker
			(TV-SET-BLINKER-VISIBILITY
			  (CAR (PC-PPR-BLINKER-LIST (<- TRACE-POP-UP-WINDOW ':PC-PPR)))
			  NIL)
			;Get input from the menu
			(SETQ CHOICE (<- TRACE-POP-UP-MENU ':CHOOSE)
			      OPTION (FIRST CHOICE))			      
			(COND ((NULL CHOICE))	;Try again if outside menu
			      ((EQ OPTION 'UNTRACE)
			       (SETQ UNTRACE-MODE T
				     FORM `(UNTRACE ,FCN)))
			      ((EQ OPTION 'QUIT)
			       (RETURN NIL))
			      ((EQ OPTION 'DO-IT)
			       (EVAL FORM)
			       (RETURN NIL))
			      (UNTRACE-MODE
			       (TV-BEEP))
			      (T (SETF (SECOND FORM)
				       (APPEND (SECOND FORM) (CDR CHOICE)))
				 (COND (OPTION		;Needs an arg, get it
					(FORMAT STREAM "~2%~A:~%" OPTION)
					;Turn on blinker
					(TV-SET-BLINKER-VISIBILITY
					  (CAR (PC-PPR-BLINKER-LIST
						 (<- TRACE-POP-UP-WINDOW ':PC-PPR)))
					  ':BLINK)
					(LET ((STANDARD-OUTPUT STREAM) ;Just for the "QUIT"
					      (STANDARD-INPUT STREAM)) ;Doesn't take an arg!
					  (SETQ ARG (READ-FOR-TOP-LEVEL)))
					(SETF (SECOND FORM)
					      (APPEND (SECOND FORM) (LIST ARG)))))))))
        ;Cleanup forms
	;Undo things in the reverse order that they were done
        (<- TRACE-POP-UP-MENU ':DEACTIVATE)
	(<- TRACE-POP-UP-WINDOW ':POP-DOWN))))
