;;; 		-*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;THIS FILE CONTAINS RANDOM FUNCTIONS WHICH MUST BE IN THE
;;; COLD LOAD, TYPICALLY BECAUSE READ USES THEM.
;;; IT IS ANALOGOUS TO QFCTNS BUT IN THE COLD LOAD.
;;--NOTE--  This file cannot be read into the machine after it is running
;; because it defines INTERN as the bootstrap INTERN, which will cause lossage 
;; after the bootstrapping has been done and the package system installed.
;; This is a loss..  unfortunately, a conditional can not be used since this
;; file must be processible by FROID.

;SPECIAL, UNSPECIAL, PUTPROP, AND REMPROP ARE HERE BECAUSE
;VARIOUS QFASL FILES IN THE COLD LOAD WILL CAUSE THESE TO
;BE CALLED AT INITIAL STARTUP.

(DECLARE (SPECIAL OBARRAY))  ;This is going away, but for now the old style
			     ;INTERN in here is still being used, so we need it declared.

(DEFUN SPECIAL (&REST &QUOTE L)
	(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X T 'SPECIAL)))
	      L)
	T)

(DEFUN UNSPECIAL (&REST &QUOTE L)
       (MAPC (FUNCTION (LAMBDA (X) (REMPROP X 'SPECIAL)))
	     L)
       T)

;This is like LIST except that "the last pair is dotted"
(DEFUN LIST* (&REST ARGS)
  (COND ((NULL ARGS)
	 (CERROR T NIL ':WRONG-NUMBER-OF-ARGUMENTS "~S called with ~D arguments (too few)"
		 'LIST* 0 NIL))
	((NULL (CDR ARGS)) (CAR ARGS))
	(T (LET ((LST (MAKE-LIST DEFAULT-CONS-AREA (LENGTH ARGS))))
	     (DO ((ARGS ARGS (CDR ARGS))
		  (CNS LST (CDR CNS)))
		 ((NULL (CDDR CNS))
		  (LET ((FOO (CDR CNS)))	;Final dotted pair
		    (%P-STORE-CDR-CODE CNS CDR-NORMAL)
		    (%P-STORE-CDR-CODE FOO CDR-ERROR))
		  (RPLACA CNS (CAR ARGS))
		  (RPLACD CNS (CADR ARGS))
		  LST)
	       (RPLACA CNS (CAR ARGS)))))))

(DEFUN PUTPROP (SYM PROP INDICATOR)
  (LET ((PLLOC (COND ((SYMBOLP SYM) (PROPERTY-CELL-LOCATION SYM))
		     ((OR (LISTP SYM) (LOCATIVEP SYM)) SYM)
		     (T (FERROR NIL "~S is not a symbol, a list, or a locative" SYM))))
	(INHIBIT-SCHEDULING-FLAG T)  ;Atomic
	(HUNK3 NIL))
    ;; If the property is already present, move it to the front without consing
    ;; Otherwise cons a new "hunk3" to hold it, and put it on the front of the plist
    (DO ((PL (CDR PLLOC) (CDDR PL))
	 (PPL PLLOC (CDR PL)))
	((NULL PL) (LET ((DEFAULT-CONS-AREA	;LIST* doesn't take an area argument
			   (OR (GET (AREA-NAME (%AREA-NUMBER SYM)) 'PUTPROP-AREA)
			       PROPERTY-LIST-AREA)))
		     (SETQ HUNK3 (LIST* INDICATOR PROP NIL))))
      (COND ((EQ (CAR PL) INDICATOR)
	     (RPLACD PPL (CDDR PL))
	     (SETQ HUNK3 PL)
	     (SETF (CADR HUNK3) PROP)
	     (RETURN))))
    (SETF (CDDR HUNK3) (CDR PLLOC))
    (RPLACD PLLOC HUNK3)
    PROP))

(DEFUN DEFPROP (&QUOTE SYM PROP INDICATOR)
	(PUTPROP SYM PROP INDICATOR)
	SYM)

(DEFUN REMPROP (SYM INDICATOR)
  "Remove a property.  Returns NIL if not present, or a list whose CAR is the property."
  (LET ((PLLOC (COND ((SYMBOLP SYM) (PROPERTY-CELL-LOCATION SYM))
		     ((OR (LISTP SYM) (LOCATIVEP SYM)) SYM)
		     (T (FERROR NIL "~S is not a symbol or a list" SYM))))
	(INHIBIT-SCHEDULING-FLAG T)) ;atomic
    (DO ((PL (CDR PLLOC) (CDDR PL))
	 (PPL PLLOC (CDR PL)))
	((NULL PL) NIL)
      (COND ((EQ (CAR PL) INDICATOR)
	     (RPLACD PPL (CDDR PL))
	     (RETURN (CDR PL)))))))

(DEFUN FIXP (X)
    (LET ((DTP (%DATA-TYPE X)))
      (OR (= DTP DTP-FIX)
	  (AND (= DTP DTP-EXTENDED-NUMBER)
	       (= (%P-LDB-OFFSET %%HEADER-TYPE-FIELD X 0) %HEADER-TYPE-BIGNUM)))))

(DEFUN LIST-SUM (X)
  (PROG (ANS)
	(SETQ ANS 0)
L	(COND ((NULL X) (RETURN ANS)))
	(SETQ ANS (+ ANS (ATOMEVAL (CAR X))))
	(SETQ X (CDR X))
	(GO L)))

(DEFUN LIST-PRODUCT (X)
  (PROG (ANS)
	(SETQ ANS 1)
L	(COND ((NULL X) (RETURN ANS)))
	(SETQ ANS (* ANS (ATOMEVAL (CAR X))))
	(SETQ X (CDR X))
	(GO L)))
       
(DEFUN ATOMEVAL (X)
	(COND ((NUMBERP X) X)
	      (T (SYMEVAL X))))

;--Q
(COMMENT ;The following are no longer used, they were for testing
(DEFUN PRINT-ALL-SYMBOLS NIL
  (PROG ((COUNT 0) (BUCKET 0) TAIL)
L	(COND ((NOT (< BUCKET SIZE-OF-OB-TBL)) (PRINT COUNT)
						(TERPRI)
						(RETURN T)))
	(SETQ TAIL (AR-1 OBARRAY BUCKET))
L1	(COND ((NULL TAIL) (TERPRI)
			   (SETQ BUCKET (1+ BUCKET))
			   (GO L)))
	(PRINT (CAR TAIL))
	(SETQ COUNT (1+ COUNT))
	(SETQ TAIL (CDR TAIL))
	(GO L1)))

(DEFUN CHECK-OBTBL NIL
  (PROG (BUCKET TEM TAIL HSH)
	(SETQ BUCKET 0)
L	(COND ((NOT (< BUCKET SIZE-OF-OB-TBL)) (RETURN T)))
	(SETQ TAIL (AR-1 OBARRAY BUCKET))
L1	(COND ((NULL TAIL) (SETQ BUCKET (1+ BUCKET))
			   (GO L)))
	(SETQ HSH (SXHASH (CAR TAIL)))
	(COND ((NOT (= BUCKET (\ HSH SIZE-OF-OB-TBL)))
		(PRIN1 (CAR TAIL))
		(PRINC " ")
		(PRIN1 HSH)
		(PRINC " ")
		(PRIN1 BUCKET)
		(TERPRI)))
	(SETQ TAIL (CDR TAIL))
	(GO L1)))
);end COMMENT

(DEFUN SXHASH (S-EXP)
  (PROG (HSH)
	(SETQ HSH 0)
L	(AND (SMALL-FLOATP S-EXP) (SETQ S-EXP (%POINTER S-EXP)))
	(COND ((SYMBOLP S-EXP) (RETURN (LOGXOR (SXHASH-SYMBOL S-EXP) HSH)))
	      ((STRINGP S-EXP) (RETURN (LOGXOR (SXHASH-STRING S-EXP) HSH)))
	      ((OR (FIXP S-EXP))
	       (RETURN (LOGXOR (LDB 2701 S-EXP)
			       (LDB 0027 S-EXP)
			       HSH)))
	      ((FLOATP S-EXP) (RETURN (LOGXOR (%P-LDB-OFFSET 0027 S-EXP 1)
					      (%P-LDB-OFFSET 2701 S-EXP 1)
					      (%P-LDB 0022 S-EXP)
					      HSH)))
	      ((LISTP S-EXP)
	       (SETQ HSH (LOGXOR HSH (ROT (SXHASH (CAR S-EXP)) -1)))
	       (COND ((MINUSP HSH) (SETQ HSH (LOGXOR -37777777		;-37777777 = 40000001
						     HSH))))
	       (SETQ S-EXP (CDR S-EXP))
	       (GO L))
;	      ((ARRAYP S-EXP)
;		(RETURN (LOGXOR HSH (SXHASH-ARRAY S-EXP))))
	      (T (RETURN HSH)))))

(DEFUN SXHASH-SYMBOL (SYM)
	(SXHASH-STRING (GET-PNAME SYM)))

(DEFUN SXHASH-STRING (STRING &OPTIONAL (HASH 0)
			     &AUX TEM CHAR)
    (SETQ TEM (ARRAY-ACTIVE-LENGTH STRING))
    (DO ((I 0 (1+ I)))
	((>= I TEM)
	 (COND ((MINUSP HASH)
		(LOGXOR HASH -37777777))		;-37777777 = 40000001
	       (T HASH)))
	(AND (< (SETQ CHAR (AR-1 STRING I)) 240)
	     (SETQ HASH (ROT (LOGXOR HASH CHAR) 7)))))

(DEFUN GET-MACRO-ARG-DESC-POINTER (FEF-POINTER &AUX ORIGIN)
    (CHECK-ARG FEF-POINTER (= (%DATA-TYPE FEF-POINTER) DTP-FEF-POINTER) "a FEF pointer")
    (COND ((= 1 (%P-LDB-OFFSET %%FEFH-NO-ADL FEF-POINTER %FEFHI-IPC))
	   NIL)
	  ((= 0 (SETQ ORIGIN
		      (%P-LDB-OFFSET %%FEFHI-MS-ARG-DESC-ORG FEF-POINTER %FEFHI-MISC)))
	   NIL)
	  (T (%MAKE-POINTER-OFFSET DTP-LIST FEF-POINTER ORIGIN))))

;LEADER = NIL -> NO LEADER
;LEADER = FIXNUM -> LEADER N LONG,  INITIALIZED TO NIL S
;LEADER = LIST -> LEADER LENGTH OF LIST LONG, INITIALIZED TO ELEMENTS OF LIST.
;		NOTE LIST IS IN STORAGE ORDER, WHICH IS REVERSED FROM INDEX ORDER.

(DEFUN MAKE-ARRAY (AREA TYPE DIMLIST
		   &OPTIONAL DISPLACED-P LEADER INDEX-OFFSET NAMED-STRUCTURE-P)
  (PROG (ARRAY NDIMS INDEX-LENGTH DATA-LENGTH ENTRIES-PER-Q LEADER-LENGTH 
	 HEADER-Q LONG-ARRAY-FLAG (LEADER-QS 0) IDX TEM)
	(COND ((NLISTP DIMLIST)
	       (SETQ NDIMS 1 INDEX-LENGTH (ATOMEVAL DIMLIST)))
	      (T (SETQ NDIMS (LENGTH DIMLIST)
		       INDEX-LENGTH (LIST-PRODUCT DIMLIST))))
	(COND ((MINUSP INDEX-LENGTH)
	       (FERROR NIL "Negative array size of ~D illegal" INDEX-LENGTH))
	      ((AND (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
		    (NULL DISPLACED-P))
	       (SETQ LONG-ARRAY-FLAG T)))
	(SETQ LEADER-LENGTH (COND ((NULL LEADER) 0)
				  ((NUMBERP LEADER) LEADER)
				  (T (LENGTH LEADER))))
	(COND (NAMED-STRUCTURE-P			; Do some error checks.
	       (COND (LEADER
		       (CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 1) "greater than 1"))
		     (T (OR (= NDIMS 1)
			    (FERROR NIL "A named-structure array may not be ~D-dimensional"
				    NDIMS)))))
	      (LEADER
		(CHECK-ARG LEADER-LENGTH (> LEADER-LENGTH 0) "greater than zero")))
	(AND LEADER (SETQ LEADER-QS (+ 2 LEADER-LENGTH)))
	(CHECK-ARG TYPE		;TEM gets the numeric array type
		   (SETQ TEM (COND ((NUMBERP TYPE) (LDB %%ARRAY-TYPE-FIELD TYPE))
				   ((FIND-POSITION-IN-LIST TYPE ARRAY-TYPES))))
		   "an array type")
	(SETQ TYPE TEM)
	(SETQ ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q TYPE))
	(SETQ DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			      (// (+ INDEX-LENGTH (1- ENTRIES-PER-Q))
				  ENTRIES-PER-Q)
			      (* INDEX-LENGTH (MINUS ENTRIES-PER-Q))))
	(SETQ HEADER-Q (%LOGDPB NDIMS %%ARRAY-NUMBER-DIMENSIONS
				(%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0)))
	(AND LEADER (SETQ HEADER-Q (%LOGDPB 1 %%ARRAY-LEADER-BIT HEADER-Q)))
	(SETQ HEADER-Q (COND (DISPLACED-P (+ (%LOGDPB 1 %%ARRAY-DISPLACED-BIT HEADER-Q)
					     (COND (INDEX-OFFSET 3) (T 2))))
			     (LONG-ARRAY-FLAG (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER-Q))
			     (T (+ INDEX-LENGTH HEADER-Q))))
	(SETQ ARRAY (%ALLOCATE-AND-INITIALIZE-ARRAY HEADER-Q
						    INDEX-LENGTH
						    LEADER-LENGTH
						    AREA
						    (+ NDIMS 
						       LEADER-QS
						       (COND (DISPLACED-P 
							      (COND (INDEX-OFFSET 3)
								    (T 2)))
							     (LONG-ARRAY-FLAG
							      (1+ DATA-LENGTH))
							     (T DATA-LENGTH)))))
	(AND (LISTP DIMLIST)
	     (DO ((I (COND (LONG-ARRAY-FLAG 2) (T 1)) (1+ I))
		  (N NDIMS (1- N)))
		 ((< N 2))
	       (%P-STORE-CONTENTS-OFFSET (ATOMEVAL (CAR DIMLIST)) ARRAY I)
	       (SETQ DIMLIST (CDR DIMLIST))))
	(COND (DISPLACED-P
	       (SETQ IDX NDIMS)
	       (AND LONG-ARRAY-FLAG (SETQ IDX (1+ IDX)))
	       (%P-STORE-CONTENTS-OFFSET DISPLACED-P ARRAY IDX)
	       (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ IDX))
	       (COND (INDEX-OFFSET
		      (%P-DPB-OFFSET 1 %%Q-FLAG-BIT ARRAY IDX) ;FLAG SIGNALS INDEX OFFSET
		      (%P-STORE-CONTENTS-OFFSET INDEX-OFFSET ARRAY (+ IDX 2))))))
	(COND (NAMED-STRUCTURE-P
	       (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0)
	       (COND ((ZEROP LEADER-LENGTH)
		      (AS-1 NAMED-STRUCTURE-P ARRAY 0))
		     (T (STORE-ARRAY-LEADER NAMED-STRUCTURE-P ARRAY 1)))))
	(AND (LISTP LEADER)
	     (DO I (1- LEADER-LENGTH) (1- I) (< I 0)
	       (STORE-ARRAY-LEADER (CAR LEADER) ARRAY I)
	       (SETQ LEADER (CDR LEADER))))
	(RETURN ARRAY DATA-LENGTH) ))

(DEFUN MAKE-SYMBOL (PNAME &OPTIONAL PERMANENT-P)
  (AND PERMANENT-P (NOT (= (%AREA-NUMBER PNAME) P-N-STRING))
       (LET ((DEFAULT-CONS-AREA P-N-STRING))
	 (SETQ PNAME (STRING-APPEND PNAME))))
  (LET ((SYMB (%ALLOCATE-AND-INITIALIZE DTP-SYMBOL	;Type to return.
			     DTP-SYMBOL-HEADER	;Type of header.
			     PNAME		;Pointer field of header.
			     NIL		;Value for second word.
			     (AND PERMANENT-P NR-SYM)	;Area.
			     LENGTH-OF-ATOM-HEAD)))	;Length.
    (MAKUNBOUND SYMB)			;Was initialized to NIL
    (FMAKUNBOUND SYMB)
    SYMB))

;SYM MAY BE EITHER A FULL SYMBOL OR A STRING. IN EITHER CASE,
; A FULL SYMBOL IS RETURNED.
;RETURNS A SECOND VALUE WHICH IS
; T IF SYMBOL WAS ALREADY ON THE OBARRAY, NIL IF WE ADDED IT
(DEFUN INTERN-OLD (SYM &OPTIONAL (OBARRAY OBARRAY))
  (PROG (HSH BUCKET ISTRING BUCKETN (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
	(COND ((SYMBOLP SYM) (SETQ ISTRING (GET-PNAME SYM)))
	      ((STRINGP SYM) (SETQ ISTRING SYM))
	      (T (FERROR NIL "~S is not a symbol or string" SYM)))
	(SETQ HSH (SXHASH-STRING ISTRING))
	(SETQ BUCKET (AR-1 OBARRAY (SETQ BUCKETN (\ HSH SIZE-OF-OB-TBL))))
  L	(COND ((NULL BUCKET) (GO NEW))
	      ((STRING-EQUAL ISTRING (GET-PNAME (CAR BUCKET)))
	       (RETURN (CAR BUCKET) T)))
	(SETQ BUCKET (CDR BUCKET))
	(GO L)
  NEW	(COND ((STRINGP SYM)
	       (SETQ SYM (MAKE-SYMBOL ISTRING T))))
	(AS-1 (CONS-IN-AREA SYM (AR-1 OBARRAY BUCKETN) OBT-TAILS) OBARRAY BUCKETN) 
	(RETURN SYM NIL)
))

(DEFUN REMOB-OLD (SYM)
  (PROG (HSH BUCKET BUCKETN)
	(SETQ HSH (SXHASH-STRING (GET-PNAME SYM)))
	(SETQ BUCKET (AREF OBARRAY (SETQ BUCKETN (\ HSH SIZE-OF-OB-TBL))))
	(ASET (DELQ SYM BUCKET) OBARRAY BUCKETN)))

;Revised version which uses microcode assist as much as possible
(DEFUN STRING-EQUAL (STRING1 STRING2 &OPTIONAL (IDX1 0) (IDX2 0) LIM1 LIM2)
  (OR (STRINGP STRING1) (SETQ STRING1 (STRING STRING1)))
  (OR (STRINGP STRING2) (SETQ STRING2 (STRING STRING2)))
  (COND ((OR LIM1 LIM2) 
	 (OR LIM1 (SETQ LIM1 (ARRAY-ACTIVE-LENGTH STRING1)))
	 (OR LIM2 (SETQ LIM2 (ARRAY-ACTIVE-LENGTH STRING2)))
	 (AND (= (SETQ LIM1 (- LIM1 IDX1)) (- LIM2 IDX2))
	      (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 LIM1)))
	(T (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 NIL))))

;;; T means case matters in string comparisons, NIL means it is ignored.
;;; This is bound to T by certain routines, such as INTERN, but I do not
;;; recommend changing its global value to T rather than NIL; many system
;;; functions, or at least their user interfaces, assume that string
;;; comparison is case-insensitive.
(DEFVAR ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
;This is initialized by the microcode now
;(SETQ ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL)	;Not in DEFVAR for cold-load.

;T if two characters are equal, ignoring font and alphabetic case
;This is in microcode now
;(DEFUN CHAR-EQUAL (CH1 CH2)
;  (OR (= (SETQ CH1 (LDB %%CH-CHAR CH1)) (SETQ CH2 (LDB %%CH-CHAR CH2)))
;      (AND (NOT ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
;	   (ZEROP (LOGAND 337 (LOGXOR CH1 CH2)))
;	   (OR (AND (>= CH1 101) (<= CH1 132))
;	       (AND (>= CH1 141) (<= CH1 172)))
;	   (OR (AND (>= CH2 101) (<= CH2 132))
;	       (AND (>= CH2 141) (<= CH2 172))))))

(DEFUN CHAR-LESSP (CH1 CH2)
       (SETQ CH1 (LDB %%CH-CHAR CH1))
       (SETQ CH2 (LDB %%CH-CHAR CH2))
       (COND ((NOT ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
	      (AND (>= CH1 141) (<= CH1 172) (SETQ CH1 (LOGXOR CH1 40)))
	      (AND (>= CH2 141) (<= CH2 172) (SETQ CH2 (LOGXOR CH2 40)))))
       (< CH1 CH2))

;This is in microcode now
;(DEFUN NTH (N OBJECT)
;   (CHECK-ARG N (AND (FIXP N) (NOT (MINUSP N))) "a non-negative integer") 
;   (DO ((N N (1- N))
;	(OBJECT OBJECT (CDR OBJECT)))
;       ((ZEROP N) (CAR OBJECT))))

(DEFUN ARRAY-LEADER-LENGTH (ARRAY)
    (COND ((ARRAY-HAS-LEADER-P ARRAY)
	   (%P-CONTENTS-OFFSET ARRAY -1))))

(DEFUN ARRAY-DIMENSION-N (N ARRAY &AUX NDIMS INDEX-LENGTH LONG-ARRAY-P)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (COND ((> N (SETQ NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)))
	 NIL)
	(T
	 (SETQ LONG-ARRAY-P (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
	 (COND ((<= N 0)
		(ARRAY-LEADER-LENGTH ARRAY))
	       ((< N NDIMS)
		(%P-LDB-OFFSET %%Q-POINTER ARRAY (+ N LONG-ARRAY-P)))
	       ((= N NDIMS)
		(SETQ INDEX-LENGTH
		      (COND ((NOT (ZEROP (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0)))
			     (%P-LDB-OFFSET %%Q-POINTER ARRAY (1+ NDIMS)))
			    ((= LONG-ARRAY-P 1) (%P-LDB-OFFSET %%Q-POINTER ARRAY 1))
			    (T (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))))
		(DO I 1 (1+ I) (>= I NDIMS)
		  (SETQ INDEX-LENGTH
			(// INDEX-LENGTH 
			    (%P-LDB-OFFSET %%Q-POINTER ARRAY (+ LONG-ARRAY-P I)))))
		INDEX-LENGTH)))))

(DEFUN EQUAL (A B)
  (PROG NIL 
    L	(COND ((EQ A B) (RETURN T))
	      ((NOT (= (%DATA-TYPE A) (%DATA-TYPE B))) (RETURN NIL))
	      ((NUMBERP A) (RETURN (= A B)))
	      ((ARRAYP A)
	       (RETURN (AND (STRINGP A)
			    (STRINGP B)
			    (%STRING-EQUAL A 0 B 0 NIL))))
	      ((ATOM A) (RETURN NIL))
	      ((NOT (EQUAL (CAR A) (CAR B))) (RETURN NIL)))
	(SETQ A (CDR A))
	(SETQ B (CDR B))
	(GO L)))

;This is here because the TV package calls it during initialization.
(DEFUN DELQ (ITEM LIST &OPTIONAL (/#TIMES -1))
  (PROG (LL PL)
A   (COND ((OR (= 0 /#TIMES) (ATOM LIST))
	   (RETURN LIST))
	  ((EQ ITEM (CAR LIST))
	   (SETQ LIST (CDR LIST))
	   (SETQ /#TIMES (1- /#TIMES))
	   (GO A)))
    (SETQ LL LIST)
B   (COND ((OR (= 0 /#TIMES) (ATOM LL))
	   (RETURN LIST))
	  ((EQ ITEM (CAR LL))
	   (RPLACD PL (CDR LL))
	   (SETQ /#TIMES (1- /#TIMES)))
	  ((SETQ PL LL)))
    (SETQ LL (CDR LL))
    (GO B)))

;The following are here because MAKE-SYMBOL uses them.  Maybe some of them
;should be in microcode?

(DEFUN MAKUNBOUND (X)
    (%P-STORE-TAG-AND-POINTER (VALUE-CELL-LOCATION X)
			      (+ DTP-NULL (LSH CDR-NEXT 6))
			      X)
    X)

(DEFUN FMAKUNBOUND (X)
    (%P-STORE-TAG-AND-POINTER (FUNCTION-CELL-LOCATION X)
			      (+ DTP-NULL (LSH CDR-NEXT 6))
			      X)
    X)

(DEFUN SETPLIST (X L)
    (RPLACA (PROPERTY-CELL-LOCATION X)
	    L)
    L)

(DEFUN FSET (SYMBOL DEFINITION)
  (COND ((SYMBOLP SYMBOL)
	 (RPLACA (FUNCTION-CELL-LOCATION SYMBOL) DEFINITION)
	 DEFINITION)
	((FERROR NIL "~S is not a symbol" SYMBOL))))

(DEFUN NAMED-STRUCTURE-SYMBOL (NAMED-STRUCTURE)
  (LET ((SYM (IF (ARRAY-HAS-LEADER-P NAMED-STRUCTURE) (ARRAY-LEADER NAMED-STRUCTURE 1)
		 (AREF NAMED-STRUCTURE 0))))
    (COND ((SYMBOLP SYM) SYM)
	  (T (AND (CLOSUREP SYM)
		  (SETQ SYM (CAR (%MAKE-POINTER DTP-LIST SYM))))
	     (OR (SYMBOLP SYM)
		 (FERROR NIL "~S not a symbol in named-structure-symbol slot of ~S"
			     SYM NAMED-STRUCTURE))
	     SYM))))

(DEFUN NAMED-STRUCTURE-P (STRUCTURE)
   (AND (ARRAYP STRUCTURE)
	(= 1 (%P-LDB-OFFSET %%ARRAY-NAMED-STRUCTURE-FLAG STRUCTURE 0))
	(NOT (NULL (NAMED-STRUCTURE-SYMBOL STRUCTURE)))))

;This function exists mostly for easing the phaseover to the new OBJECT scheme
; (which flushes the SELF argument to the named-structure handler, and uses instead
; a free reference to the variable SELF).
(DEFUN NAMED-STRUCTURE-INVOKE (SELF MSG-KEY &REST REST
				    &AUX (C (IF (ARRAY-HAS-LEADER-P SELF)
						(ARRAY-LEADER SELF 1)
						(AREF SELF 0))))
  (IF (EQ (TYPEP C) 'CLOSURE)			;If a closure, assume knows about SELF
      (LEXPR-FUNCALL C MSG-KEY REST)
      (LEXPR-FUNCALL C MSG-KEY SELF REST)))	;flush the SELF arg
						;when the phaseover is made.

;;; A number which increments approximately 60 times a second, and wraps
;;; around now and then (about once a day); it's only 23 bits long.
;;; 60-cycle clock not hooked up yet, simulate with microsecond clock.

(DECLARE (SPECIAL TIME-LAST-VALUE)) ;Remembers high-order bits and detects carries
(SETQ TIME-LAST-VALUE 0)            ;Maintenance of this depends on TIME being called
				    ; periodically by the scheduler

(DEFUN TIME ()
  (WITHOUT-INTERRUPTS
    (LET ((LOW (%UNIBUS-READ 764120))  ;Hardware synchronizes if you read this one first
	  (HIGH (%UNIBUS-READ 764122))
	  (SOFT (LDB 2205 TIME-LAST-VALUE)))
      (LET ((LOWTIME (DPB HIGH 0220 (LDB 1602 LOW))))  ;low 18 bits
	(SETQ TIME-LAST-VALUE
	      (DPB (IF (< LOWTIME (LDB 0022 TIME-LAST-VALUE))  ;If a carry into the high bits
		       (1+ SOFT)
		       SOFT)
		   2205 LOWTIME))))))

;;; These two functions deal with the wrap-around lossage
(DEFUN TIME-LESSP (TIME1 TIME2)
  (BIT-TEST 20000000 (%24-BIT-DIFFERENCE TIME1 TIME2)))

(DEFUN TIME-DIFFERENCE (TIME1 TIME2)
  (COND ((< TIME1 TIME2)
	 (+ (%24-BIT-DIFFERENCE TIME1 TIME2) 20000000 20000000))
	(T (%24-BIT-DIFFERENCE TIME1 TIME2))))

;This gets called during initialization to build the obarray.
(DEFUN BUILD-INITIAL-OBARRAY ()
  (SETQ OBARRAY (MAKE-ARRAY CONTROL-TABLES ART-Q-LIST SIZE-OF-OB-TBL))
  (MAPATOMS-NR-SYM
    #'(LAMBDA (SYM &AUX OTHERSYM PKG OTHERPKG FLAG)
	(MULTIPLE-VALUE (OTHERSYM FLAG) (INTERN SYM))
	(COND (FLAG		;Conflict, try to pick the more useful symbol
	       (SETQ PKG (CAR (PACKAGE-CELL-LOCATION SYM))
		     OTHERPKG (CAR (PACKAGE-CELL-LOCATION OTHERSYM)))
	       (COND ((OR (AND (NULL PKG) (NOT (NULL OTHERPKG)))
			  (EQ PKG 'GLOBAL)
			  (AND (MEMQ PKG '(SYS SYSTEM)) (NEQ OTHERPKG 'GLOBAL)))
		      (REMOB-OLD OTHERSYM)
		      (INTERN SYM))))))))

;The elements of <ARGS> are arguments to be supplied to <FUNCTION>,
; except for the last one, which is a list whose elements are arguments
; to be supplied to <FUNCTION>.
(DEFUN LEXPR-FUNCALL (FUNCTION &REST ARGS)
    (CHECK-ARG ARGS (NOT (NULL ARGS)) "a list of at least one element, the /"rest/" arg")
    (%OPEN-CALL-BLOCK FUNCTION 0 4)  ;No ADI, D-RETURN
    (%ASSURE-PDL-ROOM (+ (1- (LENGTH ARGS)) (LENGTH (LAST ARGS))))
    (DO ((ARGL ARGS (CDR ARGL)))
	((NULL (CDR ARGL))
	 (DO ((RESTARGL (CAR ARGL) (CDR RESTARGL)))
	     ((NULL RESTARGL)
	      (%ACTIVATE-OPEN-CALL-BLOCK))
	   (%PUSH (CAR RESTARGL))))
      (%PUSH (CAR ARGL))))

;USED BY LDB SETF PROPERTY.  THIS IS PRETTY CROCKISH.
(DEFUN DPB-VIA-LOCATIVE (VAL PPSS LOCATIVE)
    (RPLACD LOCATIVE (DPB VAL PPSS (CDR LOCATIVE))))

;;; Set all free pointers of an area to 0, deleting its entire contents
(DEFUN RESET-TEMPORARY-AREA (AREA)
  (WITHOUT-INTERRUPTS  ;don't let the area's region list change
    (DO REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
      (GC-RESET-FREE-POINTER REGION 0))))

;This function is used to adjust the free pointer of a region up or down,
;for functions other than the normal microcoded CONS and UN-CONS.
;It must do the following:
; Store into REGION-FREE-POINTER
; If REGION-GC-POINTER >= the old free pointer, set it to the new free pointer
;   (This does not work for compact-consing list regions, but it won't be called for them)
;   (This is actually only necessary when decreasing the free pointer, but it
;    doesn't hurt to do it all the time.)
; Reset the scavenger if it is in that region.  Could check for an actual
;  conflict, but that would be more difficult and wouldn't buy a great deal.
; Adjust A-CONS-WORK-DONE
(DEFUN GC-RESET-FREE-POINTER (REGION NEWFP)
  (OR INHIBIT-SCHEDULING-FLAG
      (FERROR NIL "This function must be called with scheduling inhibited"))
  (LET ((OLDFP (REGION-FREE-POINTER REGION)))
    (STORE (REGION-FREE-POINTER REGION) NEWFP)
    (AND (>= (REGION-GC-POINTER REGION) OLDFP)
	 (STORE (REGION-GC-POINTER REGION) NEWFP))
    (%GC-SCAV-RESET REGION)
    (%GC-CONS-WORK (- NEWFP OLDFP))))

;;; Make an array large or smaller.  For multi-dimensional arrays,
;;; changes the last dimension (the one which varies slowest).
;;; If array displaced, adjust request refers to the displaced header, not
;;; pointed-to data.
;;; If the array is copied, the value returned is the new copy but the old
;;; copy is forwarded to point to the new.
;;;*** Fails to adjust the cdr-codes of ART-Q-LIST arrays
(DEFUN ADJUST-ARRAY-SIZE (ARRAY NEW-INDEX-LENGTH
			  &AUX REGION CURRENT-DATA-LENGTH ARRAY-TYPE-NUMBER 
			       NDIMS ENTRIES-PER-Q NEW-DATA-LENGTH NEW-ARRAY 
			       FREED-ARRAY-LOCN FREED-ARRAY-LENGTH
			       ARRAY-DATA-BASE LONG-ARRAY-BIT CURRENT-INDEX-LENGTH)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (WITHOUT-INTERRUPTS		;Disallow garbage collection (flipping), references
				; to the array, and allocation in the region.
    (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY))
    ;;By this point, ARRAY cannot be in oldspace
    (SETQ NDIMS (%P-LDB %%ARRAY-NUMBER-DIMENSIONS ARRAY)
	  LONG-ARRAY-BIT (%P-LDB %%ARRAY-LONG-LENGTH-FLAG ARRAY)
	  ARRAY-DATA-BASE (+ (%MAKE-POINTER DTP-FIX ARRAY) ;Safe since can't move now
			     LONG-ARRAY-BIT
			     NDIMS)
	  CURRENT-INDEX-LENGTH (IF (ZEROP LONG-ARRAY-BIT)
				   (%P-LDB %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
				   (%P-CONTENTS-OFFSET ARRAY 1))
	  REGION (%REGION-NUMBER ARRAY)
	  ARRAY-TYPE-NUMBER (%P-LDB %%ARRAY-TYPE-FIELD ARRAY)
	  ENTRIES-PER-Q (AR-1 (FUNCTION ARRAY-ELEMENTS-PER-Q) ARRAY-TYPE-NUMBER)
	  NEW-DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
			      (// (+ NEW-INDEX-LENGTH (1- ENTRIES-PER-Q))
				  ENTRIES-PER-Q)
			      (* NEW-INDEX-LENGTH (MINUS ENTRIES-PER-Q)))
	  CURRENT-DATA-LENGTH (IF (PLUSP ENTRIES-PER-Q)
				  (// (+ CURRENT-INDEX-LENGTH (1- ENTRIES-PER-Q))
				      ENTRIES-PER-Q)
				  (* CURRENT-INDEX-LENGTH (MINUS ENTRIES-PER-Q))))
    (COND ((NOT (ZEROP (%P-LDB %%ARRAY-DISPLACED-BIT ARRAY)))	;Displaced array
	   (SETQ CURRENT-INDEX-LENGTH (%P-CONTENTS-OFFSET ARRAY-DATA-BASE 1))
	   (COND ((> NEW-INDEX-LENGTH CURRENT-INDEX-LENGTH)
		  (FERROR NIL "Can't make displaced array ~S bigger" ARRAY)))
	   (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY-DATA-BASE 1)
	   ARRAY)
	  ((<= NEW-DATA-LENGTH CURRENT-DATA-LENGTH)	;No new storage required
	   (COND ((= NEW-DATA-LENGTH CURRENT-DATA-LENGTH))	;No storage change
		 ((= (+ ARRAY-DATA-BASE CURRENT-DATA-LENGTH)	;Give back from end of region
		     (+ (REGION-ORIGIN REGION) (REGION-FREE-POINTER REGION)))
		  (GC-RESET-FREE-POINTER REGION
					 (- (+ ARRAY-DATA-BASE NEW-DATA-LENGTH)
					    (REGION-ORIGIN REGION))))
		 (T	;Fill hole in region with an ART-32B array
		  (%GC-SCAV-RESET REGION) ;Make scavenger forget about this region
		  (SETQ FREED-ARRAY-LOCN (+ ARRAY-DATA-BASE NEW-DATA-LENGTH))
		  (COND ((<= (SETQ FREED-ARRAY-LENGTH
				   (1- (- CURRENT-DATA-LENGTH NEW-DATA-LENGTH)))
			     %ARRAY-MAX-SHORT-INDEX-LENGTH)
			 (%P-STORE-TAG-AND-POINTER FREED-ARRAY-LOCN DTP-ARRAY-HEADER 
						   (+ ARRAY-DIM-MULT ART-32B
						      FREED-ARRAY-LENGTH)))
			(T (%P-STORE-TAG-AND-POINTER FREED-ARRAY-LOCN DTP-ARRAY-HEADER 
						     (+ ARRAY-DIM-MULT ART-32B
							ARRAY-LONG-LENGTH-FLAG))
			   (%P-STORE-CONTENTS-OFFSET (1- FREED-ARRAY-LENGTH)
						     FREED-ARRAY-LOCN
						     1)))))
	   (IF (ZEROP LONG-ARRAY-BIT)
	       (%P-DPB NEW-INDEX-LENGTH %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
	       (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY 1))
	   ARRAY)
	  ;; Need increased storage.  Either make fresh copy or extend existing copy.
	  ((OR (AND (ZEROP LONG-ARRAY-BIT)		;See if need to make fresh copy because
		    (> NEW-INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)) ;need format change
	       (< (+ ARRAY-DATA-BASE CURRENT-DATA-LENGTH) ;or not at end of region
		  (+ (REGION-ORIGIN REGION) (REGION-FREE-POINTER REGION)))
	       (> (+ ARRAY-DATA-BASE NEW-DATA-LENGTH)	;or region isn't big enough
		  (+ (REGION-ORIGIN REGION) (REGION-LENGTH REGION))))
	   (SETQ NEW-ARRAY (MAKE-ARRAY (%AREA-NUMBER ARRAY)
				       (AR-1 (FUNCTION ARRAY-TYPES) ARRAY-TYPE-NUMBER)
				       (IF (= NDIMS 1) NEW-INDEX-LENGTH
					   (LET ((DIMS (ARRAY-DIMENSIONS ARRAY)))
					     (RPLACA (LAST DIMS) NEW-INDEX-LENGTH)
					     DIMS))
				       NIL
				       (ARRAY-DIMENSION-N 0 ARRAY))) ;Leader length
	   (COPY-ARRAY-CONTENTS-AND-LEADER ARRAY NEW-ARRAY)
	   (%P-DPB (%P-LDB %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY)
		   %%ARRAY-NAMED-STRUCTURE-FLAG NEW-ARRAY)
	   (%P-DPB (%P-LDB %%ARRAY-FLAG-BIT ARRAY)
		   %%ARRAY-FLAG-BIT NEW-ARRAY)
	   (STRUCTURE-FORWARD ARRAY NEW-ARRAY)
	   NEW-ARRAY)
	  (T					;Array is at end of region, just make bigger
	   (GC-RESET-FREE-POINTER REGION (- (+ ARRAY-DATA-BASE NEW-DATA-LENGTH)
					    (REGION-ORIGIN REGION)))
	   (IF (ZEROP LONG-ARRAY-BIT)
	       (%P-DPB NEW-INDEX-LENGTH %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
	       (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY 1))
	   ;; Fill with NIL or 0
	   (DO ((ADR (+ ARRAY-DATA-BASE CURRENT-DATA-LENGTH) (1+ ADR))
		(N (- NEW-DATA-LENGTH CURRENT-DATA-LENGTH) (1- N))
		(NUMERIC-P (ARRAY-BITS-PER-ELEMENT ARRAY-TYPE-NUMBER)))
	       ((ZEROP N))
	     (IF NUMERIC-P
		 (%P-STORE-TAG-AND-POINTER ADR 0 0)
		 (%P-STORE-TAG-AND-POINTER ADR (+ (LSH CDR-NIL 5) DTP-SYMBOL) NIL)))
	   ARRAY))))

;;; Dispose of an array, returning its storage to free if possible.
;;; If the array is displaced, the displaced-array header is disposed of,
;;; not the pointed-to data.
;;; You had better get rid of all pointers to this array before calling this,
;;; e.g. (RETURN-ARRAY (PROG1 FOO (SETQ FOO NIL)))
;;; Returns T if storage really reclaimed, NIL if not.
(DEFUN RETURN-ARRAY (ARRAY &AUX REGION ARRAY-ORIGIN ARRAY-SIZE)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (WITHOUT-INTERRUPTS	;Turn off garbage collection, allocation in this region
    (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY))
    ;;By this point, ARRAY cannot be in oldspace
    (SETQ REGION (%REGION-NUMBER ARRAY)
	  ARRAY-ORIGIN (%POINTER (%FIND-STRUCTURE-LEADER ARRAY))
	  ARRAY-SIZE (%STRUCTURE-TOTAL-SIZE ARRAY))
    (COND ((= (+ ARRAY-ORIGIN ARRAY-SIZE)
	      (+ (REGION-ORIGIN REGION) (REGION-FREE-POINTER REGION)))
	   (GC-RESET-FREE-POINTER REGION (- ARRAY-ORIGIN (REGION-ORIGIN REGION)))
	   T)
	  (T NIL))))

;;; Formerly used by the DEFVAR macro in LMMAC.  Keep this around because it
;;; is compiled into old QFASL files.
(DEFUN DEFVAR-1 (SYMBOL VALUE)
  (OR (BOUNDP SYMBOL) (SET SYMBOL VALUE)))

;;; Next 2 functions used by DEFVAR
(DEFUN SETQ-IF-UNBOUND (&QUOTE SYMBOL FORM)
  (OR (BOUNDP SYMBOL) (SET SYMBOL (EVAL FORM))))

(DEFUN RECORD-SOURCE-FILE-NAME (SYMBOL)
  (AND (BOUNDP 'FDEFINE-FILE-SYMBOL)
       FDEFINE-FILE-SYMBOL
       (PUTPROP SYMBOL FDEFINE-FILE-SYMBOL ':SOURCE-FILE-NAME)))

;;; Call on a function on every symbol in the world, regardless of obarrays
(DEFUN MAPATOMS-NR-SYM (FN)
  (FUNCALL FN NIL)
  (FUNCALL FN T)
  (DO REGION (AREA-REGION-LIST NR-SYM) (REGION-LIST-THREAD REGION) (MINUSP REGION)
    (DO ((SY (REGION-ORIGIN REGION) (+ SY LENGTH-OF-ATOM-HEAD))
         (CT (// (REGION-FREE-POINTER REGION) LENGTH-OF-ATOM-HEAD) (1- CT))
         (SYM))
        ((ZEROP CT))
      (SETQ SYM (%MAKE-POINTER DTP-SYMBOL SY))
      (FUNCALL FN SYM))))

(DEFUN FOLLOW-STRUCTURE-FORWARDING (X)
  "Get the final structure this one may be forwarded to.
   Given a pointer to a structure, if it has been forwarded by STRUCTURE-FORWARD,
   ADJUST-ARRAY-SIZE, or the like, this will return the target structure, following
   any number of levels of forwarding."
  (WITHOUT-INTERRUPTS
    (COND ((= (%P-DATA-TYPE X) DTP-HEADER-FORWARD)
	   (FOLLOW-STRUCTURE-FORWARDING
	     (%MAKE-POINTER (%DATA-TYPE X) (%P-CONTENTS-AS-LOCATIVE X))))
	  ((= (%P-DATA-TYPE X) DTP-BODY-FORWARD)
	   (LET ((HDRP (%P-CONTENTS-AS-LOCATIVE X)))
	     (FOLLOW-STRUCTURE-FORWARDING
	       (%MAKE-POINTER-OFFSET (%DATA-TYPE X)
				     (%P-CONTENTS-AS-LOCATIVE HDRP)
				     (%POINTER-DIFFERENCE X HDRP)))))
	  (T X))))
