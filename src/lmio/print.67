;;; Lisp Machine PRINT.                    -*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(EVAL-WHEN (COMPILE)
(SPECIAL PRINLEVEL				;MAX DEPTH BEFORE **
	 PRINLENGTH				;MAX LENGTH BEFORE ...
	 STANDARD-OUTPUT			;DEFAULT I/O STREAM
	 BASE					;OUTPUT RADIX
	 *NOPOINT				;T TO NOT PUT POINT AFTER DECIMAL
	 READTABLE				;GETS SLASHIFICATION DATA FROM HERE
))

(SETQ PRINLENGTH NIL PRINLEVEL NIL)

;;; These are the things stored in the print-table (actually a section of the
;;; ARRAY-LEADER of the READTABLE)
;;;	   PTTBL-SPACE			     ;40
;;;	   PTTBL-NEWLINE		     ;215
;;;	   PTTBL-CONS-DOT		     ;" . "
;;;	   PTTBL-MINUS-SIGN		     ;#/-
;;;	   PTTBL-DECIMAL-POINT		     ;#/.
;;;	   PTTBL-SLASH			     ;#//
;;;	   PTTBL-PRINLEVEL		     ;"**"
;;;	   PTTBL-PRINLENGTH		     ;" ...)"
;;;	   PTTBL-OPEN-RANDOM		     ;"#<"
;;;	   PTTBL-CLOSE-RANDOM		     ;">"
;;;	   PTTBL-OPEN-PAREN		     ;#/(
;;;	   PTTBL-CLOSE-PAREN		     ;#/)
;;;	   PTTBL-OPEN-QUOTE-STRING	     ;#/"
;;;	   PTTBL-CLOSE-QUOTE-STRING	     ;#/"
;;;	   PTTBL-OPEN-QUOTE-SYMBOL	     ;#/|
;;;	   PTTBL-CLOSE-QUOTE-SYMBOL	     ;#/|
;;;        PTTBL-PACKAGE-CHAR                ;#/:

;MAIN ENTRIES
;These are the external entrypoints which are in the usual documentation.  They
;are compatible with MACLISP.

(DEFUN PRINT (EXP &OPTIONAL STREAM)
    (SETQ STREAM (DECODE-PRINT-ARG STREAM))
    (FUNCALL STREAM ':TYO (PTTBL-NEWLINE READTABLE))
    (PRINT-OBJECT EXP 0 T STREAM)
    (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
    EXP)

(DEFUN PRIN1 (EXP &OPTIONAL STREAM)
    (PRINT-OBJECT EXP 0 T (DECODE-PRINT-ARG STREAM))
    EXP)

(DEFUN PRIN1-THEN-SPACE (EXP &OPTIONAL STREAM)
    (SETQ STREAM (DECODE-PRINT-ARG STREAM))
    (PRINT-OBJECT EXP 0 T STREAM)
    (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
    EXP)

(DEFUN PRINC (EXP &OPTIONAL STREAM)
    (PRINT-OBJECT EXP 0 NIL (DECODE-PRINT-ARG STREAM))
    EXP)

(DEFUN TERPRI (&OPTIONAL STREAM)
    (FUNCALL (DECODE-PRINT-ARG STREAM) ':TYO (PTTBL-NEWLINE READTABLE))
    T)

(DEFUN TYO (CHAR &OPTIONAL STREAM)
    (FUNCALL (DECODE-PRINT-ARG STREAM) ':TYO CHAR)
    CHAR)

;SUBROUTINES

;Main routine, to print any Lisp object.
(DEFUN PRINT-OBJECT (EXP I-PRINDEPTH SLASHIFY-P STREAM
			    &OPTIONAL (FASTP
				       (MEMQ ':STRING-OUT
					     (FUNCALL STREAM ':WHICH-OPERATIONS)))
			    &AUX DATA-TYPE NSS)
   (SETQ DATA-TYPE (%DATA-TYPE EXP))
   (COND ((= DATA-TYPE DTP-FIX)
	  (PRINT-FIXNUM EXP STREAM))
	 ((= DATA-TYPE DTP-SYMBOL)
	  (PRINT-PNAME-STRING EXP SLASHIFY-P STREAM FASTP))
	 ((= DATA-TYPE DTP-LIST)
	  (COND ((AND PRINLEVEL 
		      (>= I-PRINDEPTH PRINLEVEL))
		 (PRINT-RAW-STRING (PTTBL-PRINLEVEL READTABLE) STREAM FASTP))
		(T
		 (PRINT-LIST EXP I-PRINDEPTH SLASHIFY-P STREAM FASTP))))
	 ((STRINGP EXP)
	  (PRINT-QUOTED-STRING EXP SLASHIFY-P STREAM FASTP))
	 ((AND (NAMED-STRUCTURE-P EXP)
	       (SYMBOLP (SETQ NSS (NAMED-STRUCTURE-SYMBOL EXP))))
	  (COND ((AND (FBOUNDP NSS)
		      (MEMQ ':PRINT (NAMED-STRUCTURE-INVOKE EXP ':WHICH-OPERATIONS)))
		 (NAMED-STRUCTURE-INVOKE EXP ':PRINT STREAM I-PRINDEPTH SLASHIFY-P))
		(T	;Named structure that doesn't print itself
		  (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
		  (PRINT-RAW-STRING (GET-PNAME NSS) STREAM FASTP)
		  (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
		  (PRINT-FIXNUM (%POINTER EXP) STREAM)
		  (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP))))
         ((AND (OR (= DATA-TYPE DTP-ENTITY) (= DATA-TYPE DTP-INSTANCE))
	       (ERRSET (<- EXP ':PRINT STREAM I-PRINDEPTH SLASHIFY-P) NIL)))	      
	 ((ARRAYP EXP)
	  (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
	  (PRINT-RAW-STRING (GET-PNAME (NTH (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD EXP 0)
					       ARRAY-TYPES))
			       STREAM
			       FASTP)
	  (DO ((I 1 (1+ I))
	       (DIM))
	      ((NULL (SETQ DIM (ARRAY-DIMENSION-N I EXP))))
	    (FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))
	    (PRINT-FIXNUM DIM STREAM))
	  (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
	  (PRINT-FIXNUM (%POINTER EXP) STREAM)
	  (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP))
	 ((= DATA-TYPE DTP-SMALL-FLONUM)
	  (PRINT-FLONUM EXP STREAM FASTP T))
	 ((= DATA-TYPE DTP-EXTENDED-NUMBER)
	  (LET ((HEADER-TYPE (%P-LDB-OFFSET %%HEADER-TYPE-FIELD EXP 0)))
	    (COND ((= HEADER-TYPE %HEADER-TYPE-FLONUM)
		   (PRINT-FLONUM EXP STREAM FASTP NIL))
		  ((= HEADER-TYPE %HEADER-TYPE-BIGNUM)
		   (PRINT-BIGNUM EXP STREAM FASTP))
		  (T (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
		     (PRINT-RAW-STRING (GET-PNAME (AR-1 (FUNCTION Q-DATA-TYPES) DATA-TYPE))
				       STREAM
				       FASTP)
		     (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
		     (PRINT-FIXNUM (%POINTER EXP) STREAM)
		     (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP)))))
	 (T  ;Some random type we don't know about
	  (PRINT-RAW-STRING (PTTBL-OPEN-RANDOM READTABLE) STREAM FASTP)
	  (PRINT-RAW-STRING (GET-PNAME (AR-1 (FUNCTION Q-DATA-TYPES) DATA-TYPE))
			       STREAM
			       FASTP)
	  (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
	  (PRINT-FIXNUM (%POINTER EXP) STREAM)
	  (COND ((= DATA-TYPE DTP-FEF-POINTER)
		 (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
		 (PRINT-OBJECT (%P-CONTENTS-OFFSET EXP %FEFHI-FCTN-NAME)
				  I-PRINDEPTH T STREAM FASTP))
		((= DATA-TYPE DTP-U-ENTRY)
		 (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
		 (PRINT-OBJECT (MICRO-CODE-ENTRY-NAME-AREA (%POINTER EXP))
				  I-PRINDEPTH T STREAM FASTP))
		((= DATA-TYPE DTP-STACK-GROUP)
		 (FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
		 (PRINT-OBJECT (ARRAY-LEADER EXP SG-NAME)
				  I-PRINDEPTH T STREAM FASTP)))
	  (PRINT-RAW-STRING (PTTBL-CLOSE-RANDOM READTABLE) STREAM FASTP)))
   EXP)

;Print a list, hacking prinlength and prinlevel.
(DEFUN PRINT-LIST (EXP I-PRINDEPTH SLASHIFY-P STREAM FASTP &AUX I-PRINLENGTH)
   (PROG NIL
	 (SETQ I-PRINLENGTH 1)  ;One frob gets printed before test.
	 (FUNCALL STREAM ':TYO (PTTBL-OPEN-PAREN READTABLE))
 RECURSE (PRINT-OBJECT (CAR EXP) (1+ I-PRINDEPTH) SLASHIFY-P STREAM FASTP)
	 (SETQ EXP (CDR EXP))
	 (COND ((NULL EXP)
		(FUNCALL STREAM ':TYO (PTTBL-CLOSE-PAREN READTABLE)))
	       ((NLISTP EXP)
		(PRINT-RAW-STRING (PTTBL-CONS-DOT READTABLE) STREAM FASTP)
		(PRINT-OBJECT EXP (1+ I-PRINDEPTH) SLASHIFY-P STREAM FASTP)
		(FUNCALL STREAM ':TYO (PTTBL-CLOSE-PAREN READTABLE)))
	       ((AND PRINLENGTH
		     (>= I-PRINLENGTH PRINLENGTH))
		(PRINT-RAW-STRING (PTTBL-PRINLENGTH READTABLE) STREAM FASTP))
	       (T
		(SETQ I-PRINLENGTH (1+ I-PRINLENGTH))
		(FUNCALL STREAM ':TYO (PTTBL-SPACE READTABLE))
		(GO RECURSE)))))

;Print a fixnum, possibly with negation, decimal point, etc.
(DEFUN PRINT-FIXNUM (X STREAM &AUX TEM)
    (COND ((MINUSP X) (FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE)))
	  (T (SETQ X (MINUS X))))
    (COND ((AND (NUMBERP BASE)
		(> BASE 1))
	   (PRINT-FIXNUM-1 X BASE STREAM))
	  ((AND (SYMBOLP BASE)
		(SETQ TEM (GET BASE 'PRINC-FUNCTION)))
	   (FUNCALL TEM X STREAM))
	  (T
	   (FERROR NIL "A BASE of ~S is meaningless" BASE)))
    (COND ((AND (NOT *NOPOINT)
		(EQ BASE 10.))
	   (FUNCALL STREAM ':TYO (PTTBL-DECIMAL-POINT READTABLE))))
    X)

;Print the digits of the fixnum.
(DEFUN PRINT-FIXNUM-1 (NUM RADIX STREAM &AUX TEM)
       (SETQ TEM (// NUM RADIX))
       (OR (ZEROP TEM)
	   (PRINT-FIXNUM-1 TEM RADIX STREAM))
       (FUNCALL STREAM ':TYO (- #/0 (\ NUM RADIX))))


;; Printing flonums. 
;; Note how the same code works for small flonums without consing and
;; for big flonums with a certain amount of flonum and bignum consing.
;; This code probably loses accuracy for large exponents.  Needs investigation.

(DEFUN PRINT-FLONUM (X STREAM FASTP SMALL
		     &OPTIONAL MAX-DIGITS (FORCE-E-FORMAT NIL)
		     &AUX EXPT)
    FASTP  ;ignored, don't care if the stream can string-out fast
    (COND ((ZEROP X)
           (FUNCALL STREAM ':STRING-OUT (IF SMALL "0.0s0" "0.0")))
          (T (COND ((MINUSP X)
                    (FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))
                    (SETQ X (- X))))
             (COND ((OR ( X 1.0s-5) (> X 1.0s5) FORCE-E-FORMAT)
		    ;; Must go to E format.
		    (MULTIPLE-VALUE (X EXPT) (SCALE-FLONUM X))
                    (PRINT-FLONUM-INTERNAL X SMALL STREAM MAX-DIGITS)
                    (FUNCALL STREAM ':TYO (IF SMALL #/s #/e))
		    (IF (MINUSP EXPT)
			(FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))
			(SETQ EXPT (- EXPT)))
		    (PRINT-FIXNUM-1 EXPT 10. STREAM))
                   (T
                    ;; It is in range, don't use E-format.
                    (PRINT-FLONUM-INTERNAL X SMALL STREAM MAX-DIGITS)
                    (IF SMALL (FUNCALL STREAM ':STRING-OUT "s0"))
                    )))))

;Scale a positive flonum so that it is  1.0 and < 10.0
;Returns two values, the new flonum and the exponent scaled by,
;which is positive if the number was large and negative if it was small.
;Tries to minimize loss of precision.  Can lose up to 3 bits of precision
;for humongous numbers, but usually loses at most 1 bit.
;This still needs more work; perhaps it should do the scaling
;in fixed-point double-precision rather than in floating point;
;the result is consistently too low when expt is large and nagtive.
(DEFUN SCALE-FLONUM (X &AUX (EXPT 0) (SMALLP (SMALL-FLOATP X)))
  (COND ((OR (< X 1s-15) (> X 1s15))	;Far off, first guess the exponent via logarithms
	 (SETQ EXPT (FIX (// (LOG X) 2.30258s0)))	;2.30258s0=(LOG 10.0s0)
	 (SETQ X (IF (MINUSP EXPT) (* X (^ 10. (- EXPT))) (// X (^ 10. EXPT))))))
  (DO ((DIV 10. (* 10. DIV))	;Divide by powers of 10. to make it less than 10.
       (Y X (// X DIV)))
      ((< Y 10.)
       (AND SMALLP (SETQ Y (SMALL-FLOAT Y)))
       (SETQ X Y))
    (SETQ EXPT (1+ EXPT)))
  (DO ((MPY 10. (* 10. MPY))	;Multiply by powers of 10. to make it not less than 1.
       (Y X (* X MPY)))
      (( Y 1)
       (AND SMALLP (SETQ Y (SMALL-FLOAT Y)))
       (RETURN Y EXPT))
    (SETQ EXPT (1- EXPT))))

;Print the mantissa.
;X is a positive non-zero flonum, SMALL is T if it's a small-flonum.
;Although X's magnitude is constrained to be between 0.1 and 100000.
;when called from the normal printer, this code should work for any X
;for the benefit of FORMAT.
;Note that ASH is the same as LSH except that it works for bignums and
;arbitrary amounts of shifting.  It is implemented with multiply and divide.
;Documentation in AI:QUUX;RADIX >
;Except that the bletcherous E-FORMAT hair in there has been flushed.
;It served only to avoid scaling the flonum to between 1 and 10 when
;printing in E-format, which this code wasn't using anyway.
;The MAX-DIGITS argument allows rounding off to a smaller precision
;than the true value.  However, it will only do this after the decimal point.
(DEFUN PRINT-FLONUM-INTERNAL (X SMALL STREAM MAX-DIGITS)
  (LET ((EXPONENT (IF SMALL (- (LDB 2107 (%POINTER X)) 121)
		      (- (%P-LDB-OFFSET 1013 X 0) 2037)))
	(MANTISSA (IF SMALL (LDB 0021 (%POINTER X))
		      (DPB (%P-LDB-OFFSET 0010 X 0) 3010
			   (DPB (%P-LDB-OFFSET 2010 X 1) 2010  ;Extra DPB fixes negative
				(%P-LDB-OFFSET 0020 X 1)))))   ;fixnum lossages
	(BAS 10.)
	K M R Q U S
	;; BUFER is needed when MAX-DIGITS is supplied because the rounding
	;; can generate a carry that has to propagate back through the digits.
	(BUFER (MAKE-ARRAY NIL 'ART-STRING 100 NIL '(0))))
    (OR MAX-DIGITS (SETQ MAX-DIGITS 1000.))	;Cause no effect.
    ;; Get integer part
    (SETQ R (ASH MANTISSA EXPONENT))
    (SETQ Q R)
    (SETQ M (ASH 1 (1- EXPONENT)))	;Actually 0 in most normal cases.
    ;; Instead of using a pdl, precompute S and K.
    ;; S gets the highest power of BAS <= R, and K is its logarithm.
    (SETQ S 1 K 0 U BAS)
    (DO () ((> U R))
      (SETQ S U U (* U BAS) K (1+ K)))
    (DO () (NIL)
      (SETQ U (// R S) R (\ R S))
      (COND ((OR (< R M) (> R (- S M)))
	     (ARRAY-PUSH BUFER (+ #/0 (IF (<= (* 2 R) S)
					  U
					  (1+ U))))
	     (SETQ MAX-DIGITS (1- MAX-DIGITS))
	     ;; This is the LEFTFILL routine in the paper.
	     (DO I 1 (1+ I) (>= I K)
	       (ARRAY-PUSH BUFER #/0)
	       (SETQ MAX-DIGITS (1- MAX-DIGITS)))
	     (RETURN NIL)))
      (ARRAY-PUSH BUFER (+ #/0 U))
      (SETQ MAX-DIGITS (1- MAX-DIGITS))
      (SETQ K (1- K))
      (IF (MINUSP K) (RETURN NIL))
      (SETQ S (// S 10.)))
    (ARRAY-PUSH BUFER (PTTBL-DECIMAL-POINT READTABLE))
    (IF (MINUSP EXPONENT)
	;; There is a fraction part.
	(LET ((Z (- EXPONENT)))
	  ;; R/S is the fraction, M/S is the error tolerance
          ;; The multiplication by 2 causes initial M to be 1/2 LSB
	  (SETQ R (* (IF (<= Z 23.) (LDB Z MANTISSA) ;If fraction bits fit in a fixnum
                         (LOGAND MANTISSA (1- (ASH 1 Z))))
                     2)
		S (ASH 2 Z)
		M 1)
	  (DO () (NIL)
	    (SETQ R (* R BAS))
	    (SETQ U (// R S) R (\ R S) M (* M BAS))
	    (AND (OR (< R M) (> R (- S M)) (< MAX-DIGITS 2))
		 (RETURN NIL))
	    (ARRAY-PUSH BUFER (+ U #/0))
	    (SETQ MAX-DIGITS (1- MAX-DIGITS)))
	  (ARRAY-PUSH BUFER (SETQ Z (+ (IF (<= (* 2 R) S) U (1+ U)) #/0)))
	  (COND ((> Z #/9)		;Oops, propagate carry backward (MAX-DIGITS case)
		 ;; Should this truncate trailing zeros, except for one zero
		 ;; immediately after the decimal point?
		 (DO I (- (ARRAY-LEADER BUFER 0) 2) (1- I) (MINUSP I)
		   (ASET #/0 BUFER (1+ I))
		  SKIP-DECIMAL
		   (SETQ Z (AREF BUFER I))
		   (COND ((= Z (PTTBL-DECIMAL-POINT READTABLE))
			  (SETQ I (1- I))
			  (GO SKIP-DECIMAL))
			 (( Z #/9)
			  (ASET (1+ Z) BUFER I)
			  (RETURN NIL)))))))
	;; There is no fraction part at all.
	(ARRAY-PUSH BUFER #/0))
    (FUNCALL STREAM ':STRING-OUT BUFER)
    (RETURN-ARRAY (PROG1 BUFER (SETQ BUFER NIL)))))

(DEFUN PRINT-BIGNUM (X STREAM FASTP &AUX TEM)
    FASTP  ;ignored, don't care if the stream can string-out fast
    (COND ((MINUSP X) (FUNCALL STREAM ':TYO (PTTBL-MINUS-SIGN READTABLE))))
    (COND ((AND (NUMBERP BASE)
		(> BASE 1))
	   (PRINT-BIGNUM-1 X BASE STREAM))
	  ((AND (SYMBOLP BASE)
		(SETQ TEM (GET BASE 'PRINC-FUNCTION)))
	   (FUNCALL TEM X STREAM))
	  (T
	   (FERROR NIL "A BASE of ~S is meaningless" BASE)))
    (COND ((AND (NOT *NOPOINT)
		(EQ BASE 10.))
	   (FUNCALL STREAM ':TYO (PTTBL-DECIMAL-POINT READTABLE))))
    X)

;;; Print the digits of a bignum
(DEFUN PRINT-BIGNUM-1 (NUM RADIX STREAM &AUX LENGTH MAX-RADIX DIGITS-PER-Q)
       (SETQ DIGITS-PER-Q (// 24. (HAULONG RADIX))
             MAX-RADIX (^ RADIX DIGITS-PER-Q)
             NUM (SYS:BIGNUM-TO-ARRAY NUM MAX-RADIX)
	     LENGTH (ARRAY-LENGTH NUM))
       (DO ((INDEX (1- LENGTH) (1- INDEX))
            (NDIGITS -1 DIGITS-PER-Q))
	   ((MINUSP INDEX))
         (PRINT-BIGNUM-PIECE (AR-1 NUM INDEX) RADIX STREAM NDIGITS))
       (RETURN-ARRAY NUM))

(DEFUN PRINT-BIGNUM-PIECE (PIECE RADIX STREAM NDIGITS)
       (COND ((OR (> NDIGITS 1) (>= PIECE RADIX))
              (PRINT-BIGNUM-PIECE (// PIECE RADIX) RADIX STREAM (1- NDIGITS))))
       (FUNCALL STREAM ':TYO (+ #/0 (\ PIECE RADIX))))

;The following functions print out strings, in three different ways.

(EVAL-WHEN (COMPILE)
	   (SPECIAL XP-STREAM XP-FASTP))

;Print out a symbol's print-name.  If slashification is on,, try slashify it
;so that if read in the right thing will happen.
(DEFUN PRINT-PNAME-STRING (SYMBOL SLASHIFY-P STREAM FASTP
				     &AUX STRING LEN FSMWINS MUST// TEM
				          (PKG (CAR (PACKAGE-CELL-LOCATION SYMBOL))))
    (AND SLASHIFY-P
	 PKG
	 (LET ((XP-FASTP FASTP)
	       (XP-STREAM STREAM))
	   (PKG-PREFIX SYMBOL
		       #'(LAMBDA (REFNAME IGNORE)
			   (PRINT-RAW-STRING REFNAME XP-STREAM XP-FASTP)
			   (FUNCALL XP-STREAM ':TYO (PTTBL-PACKAGE-CHAR READTABLE))))))

    (SETQ STRING (GET-PNAME SYMBOL))
    (COND ((NOT SLASHIFY-P)
	   (PRINT-RAW-STRING STRING STREAM FASTP))
	  (T
	   (SETQ FSMWINS
	    (AND (PLUSP (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING)))
		 (DO ((I 0 (1+ I))
		      (STATE (RDTBL-STARTING-STATE READTABLE))
		      (FSM (RDTBL-FSM READTABLE))
		      (CHAR))
		     ((= I LEN)
		      (COND ((NOT (NUMBERP STATE))
			     (DO L (RDTBL-MAKE-SYMBOL READTABLE) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    ((NOT (NUMBERP (SETQ STATE
						 (AR-2 FSM
						       STATE
						       (RDTBL-BREAK-CODE READTABLE)))))
			     (DO L (RDTBL-MAKE-SYMBOL-BUT-LAST READTABLE) (CDR L) (NULL L)
				 (AND (EQ (CAR STATE) (CAAR L))
				      (EQ (CDR STATE) (CDAR L))
				      (RETURN T))))
			    (T NIL)))
		     (SETQ CHAR (AR-1 STRING I))
		     (COND ((OR (NOT (NUMBERP STATE))	;FSM ran out OR
				(NOT			;Translated char? then fsm loses
				 (= CHAR (RDTBL-TRANS READTABLE CHAR))))
			    (OR MUST//				   ;Must we slash?
				(DO I (1+ I) (1+ I) (= I LEN)
				    (COND ((NOT
					    (ZEROP
					     (LOGAND 26 (RDTBL-BITS READTABLE
								    (AR-1 STRING I)))))
					   (SETQ MUST// T)
					   (RETURN NIL)))))
			    (RETURN NIL)))
		     (SETQ STATE
			   (AR-2 FSM
				 STATE
				 (COND ((NOT		;Must we slash?
					 (ZEROP
					  (LOGAND 26 (RDTBL-BITS READTABLE CHAR))))
					(SETQ MUST// T)	;YES: set flag.
					(RDTBL-SLASH-CODE READTABLE))
				       (T
					(RDTBL-CODE READTABLE CHAR))))))))
	   (OR FSMWINS
	       (FUNCALL STREAM ':TYO (PTTBL-OPEN-QUOTE-SYMBOL READTABLE)))
	   (COND (MUST//
		  (DO I 0 (1+ I) (= I LEN)
		      (COND ((NOT (ZEROP (LOGAND 26 (RDTBL-BITS READTABLE
                                                                (SETQ TEM (AR-1 STRING I))))))
			     (FUNCALL STREAM ':TYO (PTTBL-SLASH READTABLE))))
		      (FUNCALL STREAM ':TYO TEM)))
		 (T (PRINT-RAW-STRING STRING STREAM FASTP)))
	   (OR FSMWINS
	       (FUNCALL STREAM ':TYO (PTTBL-CLOSE-QUOTE-SYMBOL READTABLE)))
	   )))

;Print a string, and if slashification is on, slashify it appropriately.
(DEFUN PRINT-QUOTED-STRING (STRING SLASHIFY-P STREAM FASTP &AUX TEM CHAR)
       (COND ((NOT SLASHIFY-P)
	      (PRINT-RAW-STRING STRING STREAM FASTP))
	     (T
	      (FUNCALL STREAM ':TYO (PTTBL-OPEN-QUOTE-STRING READTABLE))
	      (SETQ TEM (ARRAY-ACTIVE-LENGTH STRING))
	      (COND ((DO ((I 0 (1+ I))
			  (CH))
			 ((>= I TEM) T)
		       (AND (< (SETQ CH (AR-1 STRING I)) 220)
			    (NOT (ZEROP (LOGAND 16 (RDTBL-BITS READTABLE CH))))
			    (RETURN NIL)))
		     ;; There are no double quotes, and so no slashifying.
		     (FUNCALL STREAM ':STRING-OUT STRING))
		    (T
		     (DO I 0 (1+ I) (>= I TEM)
		       (SETQ CHAR (AR-1 STRING I))
		       (COND ((AND (< CHAR 220)
				   (NOT (ZEROP (LOGAND 16 (RDTBL-BITS READTABLE CHAR)))))
			      (FUNCALL STREAM ':TYO (PTTBL-SLASH READTABLE))))
		       (FUNCALL STREAM ':TYO CHAR))))
	      (FUNCALL STREAM ':TYO (PTTBL-CLOSE-QUOTE-STRING READTABLE))
	      )))

;Print the string, with no slashification at all.
(DEFUN PRINT-RAW-STRING (STRING STREAM FASTP &AUX TEM)
    (COND (FASTP
	   (FUNCALL STREAM ':STRING-OUT STRING))
	  (T
	   (SETQ TEM (ARRAY-ACTIVE-LENGTH STRING))
	   (DO I 0 (1+ I) (>= I TEM)
	       (FUNCALL STREAM ':TYO (AR-1 STRING I))))))
    
