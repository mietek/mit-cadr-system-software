;;; -*- Mode:Lisp; Package:Format; Base:8 -*-

(DEFUN DISSECT-FLONUM (NUM &AUX (BAS 10.)	;base
				(DEXPT 0)	;decimal exponent
				(DMANT)		;decimal mantissa
				(BEXPT)		;binary exponent
				(BMANT)		;binary mantissa
				(ERROR-TOLERANCE 1)	;1/2 least-significant
							; bit initially
				(SMALLP (COND ((SMALL-FLOATP NUM) T)
					      ((FLOATP NUM) NIL)
					      ((FERROR NIL "~S not a floating-point number" NUM)))))
 "Returns decimal mantissa, exponent, and significant digits.
  The argument must be a positive flonum or small-flonum.
  The first value is an integer, which is the mantissa.  It is
  never divisible by 10 unless it is zero.  The second value is the exponent of 10
  that goes with that mantissa.  The third value is the number of
  digits in the mantissa, which is all the significant digits,
  and only those."
  (DO ()			;Loop until within range then return explicitly
      (NIL)			;with DMANT and DEXPT set up.
    ;; Parse out the binary mantissa (as an integer) and corresponding binary exponent
    (IF SMALLP (SETQ BMANT (LDB 0021 (%POINTER NUM))
		     BEXPT (- (LDB 2107 (%POINTER NUM)) 121))
	       (SETQ BMANT (DPB (%P-LDB-OFFSET 0010 NUM 0) 3010
				(DPB (%P-LDB-OFFSET 2010 NUM 1) 2010
				     (%P-LDB-OFFSET 0020 NUM 1)))
		     BEXPT (- (%P-LDB-OFFSET 1013 NUM 0) 2037)))
    ;; Now convert to decimal
    (COND ((ZEROP NUM)		;Zero is a damned special case
	   (RETURN (SETQ DMANT 0 DEXPT 0)))
	  ((> BEXPT 31.)	;If mantissa > 62. bits, divide by powers of 10
	   (SETQ BEXPT (FIX (// (LOG NUM) 2.30258s0)))	;2.30258s0=(LOG 10.0s0)
	   (SETQ NUM (// NUM (^ BAS BEXPT))
		 DEXPT (+ DEXPT BEXPT))
	   (IF SMALLP (SETQ NUM (SMALL-FLOAT NUM))
	       ;; Increase error-tolerance, since we unfortunately lost some accuracy
	       ;; The amount to increase by is empirically-determined, some numerical
	       ;; analysis is called for here!
	       (SETQ ERROR-TOLERANCE (LSH ERROR-TOLERANCE (MAX (* (1- (DHAULONG BEXPT)) 2) 0)))))
	  ((>= BEXPT 0)		;Number is really an integer, round then remove trailing zeros
	   (SETQ BMANT (ASH BMANT BEXPT)	;True integer value of num
		 ERROR-TOLERANCE (ASH ERROR-TOLERANCE (1- BEXPT))) ;1/2 lsb
	   ;; This loop tries rounding off different numbers of digits until
	   ;; it gets the error below the ERROR-TOLERANCE
	   (DO ((ROUND-EXPT (DHAULONG ERROR-TOLERANCE) (1- ROUND-EXPT))
		(ROUND))
	       (NIL)
	     (SETQ ROUND (^ BAS ROUND-EXPT)
		   DMANT (// (+ BMANT (// ROUND 2)) ROUND))
	     (AND ( (ABS (- (* DMANT ROUND) BMANT)) ERROR-TOLERANCE)
		  (RETURN (SETQ DEXPT (+ DEXPT ROUND-EXPT)))))
	   (RETURN))
	  ((< BEXPT -60.)	;Too small, multiply in powers of 10.
	   (SETQ BEXPT (- -9 (FIX (// (LOG NUM) 2.30258s0))))	;2.30258s0=(LOG 10.0s0)
	   (SETQ NUM (* NUM (^ BAS BEXPT))
		 DEXPT (- DEXPT BEXPT))
	   (IF SMALLP (SETQ NUM (SMALL-FLOAT NUM))
	       ;; Increase error-tolerance, since we unfortunately lost some accuracy
	       ;; The amount to increase by is empirically-determined, some numerical
	       ;; analysis is called for here!
	       (SETQ ERROR-TOLERANCE (LSH ERROR-TOLERANCE (MAX (* (DHAULONG BEXPT) 2) 0)))))
	  (T ;Mantissa has a fractional part.  Use rounding number-conversion after GLS.
	   ;; First get integer part, if any.  Then we will operate on the fraction
	   ;; part, represented as an integer, its true value times 2^NFRACB.
	   (SETQ DMANT (ASH BMANT BEXPT))
	   (LET ((NFRACB (1+ (- BEXPT)))	;number of bits in fraction
		 (ONE) (IPPSS) (FMASK) (DIG))
	     (SETQ ONE (ASH 1 NFRACB)		;1.0 scaled same way as fraction
		   FMASK (1- ONE)		;mask for fraction bits
		   IPPSS (+ (LSH NFRACB 6) 4))	;byte pointer to integer bits
	     (SETQ BMANT (LOGAND (ASH BMANT 1) FMASK))	;Extract fraction part
	     (DO ()		;Loop, producing digits
		 ((OR (< BMANT ERROR-TOLERANCE) (> BMANT (- ONE ERROR-TOLERANCE)))
		  ;; The last digit produced may need to be rounded
		  (AND (> BMANT (// ONE 2)) (SETQ DMANT (1+ DMANT))))
	       ;; Bring next digit up into integer part, pull it out
	       (SETQ BMANT (* BMANT BAS)
		     DIG (LDB IPPSS BMANT)
		     BMANT (LOGAND BMANT FMASK)
		     ERROR-TOLERANCE (* ERROR-TOLERANCE BAS))
	       ;; Stick that digit into the decimal mantissa being built
	       (SETQ DMANT (+ (* DMANT BAS) DIG)
		     DEXPT (1- DEXPT))))
	   (RETURN))))
  ;; Having computed DMANT and DEXPT, remove trailing zeros,
  ;; compute number of significant digits in DMANT, and return.
  (COND ((ZEROP DMANT) (PROG () (RETURN 0 0 0)))	;Zero is special case
	(T (DO () ((NOT (ZEROP (\ DMANT BAS)))	;Remove factors of 10.
		   (RETURN DMANT DEXPT (DHAULONG DMANT)))
	    (SETQ DMANT (// DMANT BAS) DEXPT (1+ DEXPT))))))

(DEFUN DHAULONG (NUM)
  "Number of digits in decimal representation of an integer.
  Plus 1 for the sign if negative.  0 is 1 digit, not 0 digits."
  (DO ((LNG (COND ((MINUSP NUM) (SETQ NUM (- NUM)) 2)
		  (T 1))
	    (+ LNG 6))
       (NUM NUM (// NUM 1000000.)))
      ((< NUM 1000000.)
       (DO ((LNG LNG (1+ LNG))
	    (NUM NUM (// NUM 10.)))
	   ((< NUM 10.) LNG)))))

(DEFUN MAKE-N-SIGNIFICANT-DIGITS (MANTISSA EXPONENT N-DIGITS-NOW N-DIGITS-WANTED)
  (COND (( N-DIGITS-NOW N-DIGITS-WANTED)
	 (DO () ((= N-DIGITS-NOW N-DIGITS-WANTED) (RETURN MANTISSA EXPONENT N-DIGITS-WANTED))
	   (SETQ MANTISSA (* MANTISSA 10.)
		 EXPONENT (1- EXPONENT)
		 N-DIGITS-NOW (1+ N-DIGITS-NOW))))
	(T (LET ((FACTOR (^ 10. (- N-DIGITS-NOW N-DIGITS-WANTED))))
	     (LET ((MANT (// MANTISSA FACTOR))
		   (REM (\ MANTISSA FACTOR)))
	       (AND (> REM (// FACTOR 2)) (SETQ MANT (1+ MANT)))
	       (MAKE-N-SIGNIFICANT-DIGITS MANT
					  (+ EXPONENT (- N-DIGITS-NOW N-DIGITS-WANTED))
					  (DHAULONG MANT)
					  (MAX 0 N-DIGITS-WANTED)))))))

;Do all good things
;Some of the arguments are numbers or NIL, others are T or NIL.
(DEFUN FORMAT-FLONUM (STREAM			;Where to send output
		      NUM			;Number to print
		      FIELD-WIDTH		;NIL or number of columns (right-just)
		      N-DIGITS-TO-RIGHT-OF-DECIMAL	;NIL or...F-format only
		      N-DIGITS-TO-LEFT-OF-DECIMAL	;NIL or...E-format only
		      N-SIGNIFICANT-DIGITS	;NIL or n digits to round to
		      ALWAYS-PRINT-SIGN-P	;T => print "+" if positive
		      SMALL-NUMBERS-BECOME-ZERO-P	;Don't go to E-format if small
		      ALWAYS-PRINT-EXPONENT-P	;T => always use E-format
		      FIXED-EXPONENT		;NIL or constant exponent
		      EXPONENT-MULTIPLE-OF-THREE-P	;In E-format, E 1000's
		      INDICATE-SMALL-FLONUM-P	;Small-float => E (S) format
		      &AUX MANT EXP NDIGS E-FORMAT-P (NEGATIVE-P NIL) WID FEXP FROB)
  ;; Allow fixed-point number as argument
  (OR (FLOATP NUM) (SETQ NUM (FLOAT NUM)))
  ;; Take care of negative numbers
  (AND (MINUSP NUM)
       (SETQ NUM (- NUM) NEGATIVE-P T))
  ;; Convert to floating-point decimal
  (MULTIPLE-VALUE (MANT EXP NDIGS) (DISSECT-FLONUM NUM))
  ;; Decide whether to use E-format
  (SETQ E-FORMAT-P (OR ALWAYS-PRINT-EXPONENT-P
		       FIXED-EXPONENT
		       (AND INDICATE-SMALL-FLONUM-P (SMALL-FLOATP NUM)) ;Right?
		       ( NUM (IF (NULL N-SIGNIFICANT-DIGITS) 1s5
				  (^ 10. (MAX 1 (- N-SIGNIFICANT-DIGITS
						   (OR N-DIGITS-TO-RIGHT-OF-DECIMAL 0))))))
		       (AND (NOT SMALL-NUMBERS-BECOME-ZERO-P)
			    (NOT (ZEROP NUM))
			    (< NUM (IF N-DIGITS-TO-RIGHT-OF-DECIMAL
				       (^ 10.0s0 (- N-DIGITS-TO-RIGHT-OF-DECIMAL))
				       10.0s-5)))))
  ;; If number of digits is specified, rescale to that.
  ;;*** Later might want to hack case of too many digits wanted differently ***
  (AND N-SIGNIFICANT-DIGITS
       (MULTIPLE-VALUE (MANT EXP NDIGS)
	 (MAKE-N-SIGNIFICANT-DIGITS MANT EXP NDIGS N-SIGNIFICANT-DIGITS)))
  ;; Figure out EXP, the exponent that goes after the E, and FEXP, the exponent
  ;; that indicates where the decimal point goes in the mantissa, 0 means just
  ;; to the right of it, (- NDIGS) means just to the left of it.
  (COND ((NOT E-FORMAT-P)
	 (COND ((NULL N-DIGITS-TO-RIGHT-OF-DECIMAL)
		(SETQ FEXP EXP EXP 0))
	       ((< EXP (- N-DIGITS-TO-RIGHT-OF-DECIMAL)) ;must round again
		(MULTIPLE-VALUE (MANT EXP NDIGS)
		  (MAKE-N-SIGNIFICANT-DIGITS MANT EXP NDIGS
					     (+ NDIGS N-DIGITS-TO-RIGHT-OF-DECIMAL EXP)))
		(SETQ FEXP EXP EXP 0))
	       ((SETQ MANT (* MANT (^ 10. (+ N-DIGITS-TO-RIGHT-OF-DECIMAL EXP)))
		      FEXP (- N-DIGITS-TO-RIGHT-OF-DECIMAL)
		      EXP 0))))
	(FIXED-EXPONENT (SETQ FEXP (- EXP FIXED-EXPONENT) EXP FIXED-EXPONENT))
	(EXPONENT-MULTIPLE-OF-THREE-P
	   (SETQ FEXP (- 1 NDIGS) EXP (- EXP FEXP))
	   (DO () ((ZEROP (\ EXP 3)))
	     (SETQ FEXP (1- FEXP) EXP (1+ EXP))))
	((NULL N-DIGITS-TO-LEFT-OF-DECIMAL)
	 (SETQ FEXP (- 1 NDIGS) EXP (- EXP FEXP)))
	(T (SETQ FEXP (- N-DIGITS-TO-LEFT-OF-DECIMAL NDIGS)
		 EXP (- EXP FEXP))))
  ;; Figure out how wide it's going to be
  (SETQ WID (+ (COND ((NOT (MINUSP FEXP))
		      (+ NDIGS FEXP 2))	;digits, zeros, decimal point, zero
		     ((< (- FEXP) NDIGS)
		      (1+ NDIGS))	;digits, decimal point, more digits
		     (T (- 2 FEXP)))	;zero, decimal point, zeros, digits
	       (COND (NEGATIVE-P 1)	;sign
		     (ALWAYS-PRINT-SIGN-P 1)
		     (T 0))
	       (COND (E-FORMAT-P	;exponent
		       (1+ (DHAULONG EXP)))
		     (T 0))))
  ;; Leading spaces to right-justify within field
  (AND FIELD-WIDTH (DOTIMES (I (- FIELD-WIDTH WID)) (FUNCALL STREAM ':TYO #\SP)))
  ;; Sign
  (COND (NEGATIVE-P (FUNCALL STREAM ':TYO (SI:PTTBL-MINUS-SIGN READTABLE)))
	(ALWAYS-PRINT-SIGN-P (FUNCALL STREAM ':TYO #/+)))  ;not in PTTBL
  ;; Digits before decimal
  (COND ((MINUSP FEXP)
	 (PRINT-DECIMAL-FIXNUM (// MANT (SETQ FROB (^ 10. (- FEXP)))) STREAM))
	(T (PRINT-DECIMAL-FIXNUM MANT STREAM)
	   (DOTIMES (I FEXP) (FUNCALL STREAM ':TYO #/0))))
  ;; Decimal point    *** do we want to be able to omit the decimal in some cases? ***
  (FUNCALL STREAM ':TYO (SI:PTTBL-DECIMAL-POINT READTABLE))
  ;; Digits after decimal
  (COND ((MINUSP FEXP)
	 (SETQ FROB (\ MANT FROB))	;fraction part
	 (DOTIMES (I (- (- FEXP) (DHAULONG FROB))) (FUNCALL STREAM ':TYO #/0))
	 (OR (ZEROP FROB)
	     (PRINT-DECIMAL-FIXNUM FROB STREAM)))
	((FUNCALL STREAM ':TYO #/0)))
  ;; Exponent
  (COND (E-FORMAT-P
	  (FUNCALL STREAM ':TYO (IF (AND INDICATE-SMALL-FLONUM-P (SMALL-FLOATP NUM)) #/s #/e))
	  (PRINT-DECIMAL-FIXNUM EXP STREAM))))

(DEFUN PRINT-DECIMAL-FIXNUM (NUM STREAM &AUX TEM)
  (COND ((MINUSP NUM)
	 (SETQ NUM (MINUS NUM))
	 (FUNCALL STREAM ':TYO (SI:PTTBL-MINUS-SIGN READTABLE))))
  (SETQ TEM (// NUM 10.))
  (OR (ZEROP TEM) (PRINT-DECIMAL-FIXNUM TEM STREAM))
  (FUNCALL STREAM ':TYO (+ (\ NUM 10.) #/0)))

;This is ordinary printing (test function)
(DEFUN P (NUM &OPTIONAL FW DIGS-TO-RIGHT NSIG)
  (FORMAT-FLONUM STANDARD-OUTPUT NUM FW DIGS-TO-RIGHT NIL NSIG NIL NIL NIL NIL NIL T))

;Magic test functions.  Big-flonums only
(DEFUN F1+ (NUM)
  (LET ((BEXPT (- (%P-LDB-OFFSET 1013 NUM 0) 2037)))
    (+ NUM (^ 2.0 BEXPT))))

(DEFUN F1- (NUM)
  (LET ((BEXPT (- (%P-LDB-OFFSET 1013 NUM 0) 2037)))
    (- NUM (^ 2.0 BEXPT))))
