; -*- Mode:Lisp; Package:System-Internals; Base:8 -*-

;To do:
; For %P-CONTENTS-EQ really to work, must change implementation of AP-1

;A hash-table is an array whose leader is described by the following defstruct.
;It is used to associate keys with values.  Neither a key nor a value may be NIL;
;NIL as a key is an empty table entry and NIL as a value is the same as having
;no entry in the table.
;The array portion of the array is divided into 16.-element blocks.  The first
;8 locations are 8 keys that hash to the same place and the second 8 locations
;are their corresponding values.
;DESCRIBE of a hash-table will tell you all about it.
;Some of the function names are the same as in Interlisp.

;Note that all hash-table operations, even GETHASH, can modify the hash-table
;because they may discover a need to rehash.  Therefore it is the user's
;responsibility to ensure that the same hash-table is not referenced
;simultaneously by more than one process.

;This is temporary until this really exists
(DEFSUBST %P-CONTENTS-EQ (P X) (EQ (CAR P) X))

(DEFSTRUCT (HASH-TABLE :NAMED :ARRAY-LEADER (:CONSTRUCTOR MAKE-HASH-TABLE-INTERNAL))
    (HASH-TABLE-REHASH-FUNCTION 'HASH-TABLE-REHASH)
	;A function when rehash is required.  First argument is the hash-table,
	;second is NIL to just rehash or the rehash-size to grow it first.
	;The function must return the hash-table (which may have been moved
	;by adjust-array-size)
    (HASH-TABLE-REHASH-SIZE 1.3)
	;How much to grow by when the time comes.  A flonum is the ratio to
	;increase by, a fixnum is the number of entries to add.
	;These will get rounded up to the next appropriate size.
		;flonum = ratio to rehash up by
    (HASH-TABLE-GC-GENERATION-NUMBER %GC-GENERATION-NUMBER)
	;Used to decide when rehash required because the GC may have moved
	;some objects, changing their %POINTER and hence their hash code.
    HASH-TABLE-MODULUS
	;The number of 16.-element blocks.  Used for remainder to get hash code.
    (HASH-TABLE-FULLNESS 0))
	;The number of valid entries currently in the array.

(DEFUN MAKE-HASH-TABLE (&REST OPTIONS &AUX (SIZE 100) AREA
					   (RHF 'HASH-TABLE-REHASH) (RHS 1.3))
  (DO L OPTIONS (CDDR L) (NULL L)
      (SELECTQ (CAR L)
	(:SIZE (SETQ SIZE (CADR L)))		;Number of associations room for
	(:AREA (SETQ AREA (CADR L)))
	(:REHASH-FUNCTION (SETQ RHF (CADR L)))
	(:REHASH-SIZE (SETQ RHS (CADR L)))
	(OTHERWISE (FERROR NIL "~S not a recognized option" (CAR L)))))
  (SETQ SIZE (HASH-TABLE-GOOD-SIZE (* SIZE 2)))
  (MAKE-HASH-TABLE-INTERNAL :MAKE-ARRAY (AREA 'ART-Q SIZE)
			    HASH-TABLE-MODULUS (// SIZE 16.)
			    HASH-TABLE-REHASH-FUNCTION RHF
			    HASH-TABLE-REHASH-SIZE RHS))

;Convert SIZE (a number of array elements) to a more-or-less prime multiple of 16.
(DEFUN HASH-TABLE-GOOD-SIZE (SIZE)
  (SETQ SIZE (// (+ SIZE 15.) 16.))		;Next higher multiple of 16.
  (OR (ODDP SIZE) (SETQ SIZE (1+ SIZE)))	;Find next higher more-or-less prime
  (DO () ((AND (NOT (ZEROP (\ SIZE 3)))
	       (NOT (ZEROP (\ SIZE 5)))
	       (NOT (ZEROP (\ SIZE 7)))))
    (SETQ SIZE (+ SIZE 2)))
  (* SIZE 16.))

(DEFUN CLRHASH (HASH-TABLE)
  (CHECK-ARG HASH-TABLE (EQ (TYPEP HASH-TABLE) 'HASH-TABLE) "a hash-table")
  (COPY-ARRAY-PORTION HASH-TABLE 0 0 HASH-TABLE 0 (ARRAY-LENGTH HASH-TABLE))  ;Fill with NIL
  (SETF (HASH-TABLE-FULLNESS HASH-TABLE) 0)
  (SETF (HASH-TABLE-GC-GENERATION-NUMBER HASH-TABLE) %GC-GENERATION-NUMBER)
  HASH-TABLE)

;Given a hash-table and a key, return the index in the array of the start
;of the block which may contain an association from that key.
(DEFUN HASH-TABLE-INDEX (HASH-TABLE KEY)
  (AND (MINUSP (SETQ KEY (%POINTER KEY)))
       (SETQ KEY (+ 1_24. KEY)))		;Make sure it's positive
  (* (\ KEY (HASH-TABLE-MODULUS HASH-TABLE)) 16.))

(DEFUN GETHASH (KEY HASH-TABLE)
  (CHECK-ARG HASH-TABLE (EQ (TYPEP HASH-TABLE) 'HASH-TABLE) "a hash-table")
  (DO ((P (ALOC HASH-TABLE (HASH-TABLE-INDEX HASH-TABLE KEY))
	  (%MAKE-POINTER-OFFSET DTP-LOCATIVE P 1))
       (N 8 (1- N)))
      ((ZEROP N)				;Not found
       (COND (( (HASH-TABLE-GC-GENERATION-NUMBER HASH-TABLE) %GC-GENERATION-NUMBER)
	      ;; Some %POINTER's may have changed, try rehashing
	      (FUNCALL (HASH-TABLE-REHASH-FUNCTION HASH-TABLE) HASH-TABLE NIL)
	      (GETHASH KEY HASH-TABLE))
	     (T NIL)))				;Result is NIL if not found
    (AND (%P-CONTENTS-EQ P KEY)
	 (RETURN (%P-CONTENTS-OFFSET P 8)))))

;Putting a value of NIL means to remove
(DEFUN PUTHASH (KEY VALUE HASH-TABLE)
  (AND (NULL KEY) (FERROR NIL "NIL is not an allowed key in hash tables"))
  (CHECK-ARG HASH-TABLE (EQ (TYPEP HASH-TABLE) 'HASH-TABLE) "a hash-table")
  (DO ((P (ALOC HASH-TABLE (HASH-TABLE-INDEX HASH-TABLE KEY))
	  (%MAKE-POINTER-OFFSET DTP-LOCATIVE P 1))
       (N 8 (1- N))
       (EMPTYP NIL))
      ((ZEROP N)				;Not found
       (COND (( (HASH-TABLE-GC-GENERATION-NUMBER HASH-TABLE) %GC-GENERATION-NUMBER)
	      ;; Some %POINTER's may have changed, try rehashing
	      (FUNCALL (HASH-TABLE-REHASH-FUNCTION HASH-TABLE) HASH-TABLE NIL)
	      (PUTHASH KEY VALUE HASH-TABLE))
	     ((NULL VALUE) NIL)			;Delete when not found
	     (EMPTYP				;Add to table using empty slot found
	      (%P-STORE-CONTENTS EMPTYP KEY)
	      (%P-STORE-CONTENTS-OFFSET VALUE EMPTYP 8)
	      (SETF (HASH-TABLE-FULLNESS HASH-TABLE) (1+ (HASH-TABLE-FULLNESS HASH-TABLE)))
	      VALUE)
	     (T					;Need to make more room, then try again
	      (FUNCALL (HASH-TABLE-REHASH-FUNCTION HASH-TABLE)
		       HASH-TABLE (HASH-TABLE-REHASH-SIZE HASH-TABLE))
	      (PUTHASH KEY VALUE HASH-TABLE))))
    (COND ((%P-CONTENTS-EQ P KEY)	;Found existing entry
	   (COND ((NULL VALUE)			;Remove it
		  (%P-STORE-CONTENTS P NIL)
		  (SETF (HASH-TABLE-FULLNESS HASH-TABLE)
			(1- (HASH-TABLE-FULLNESS HASH-TABLE)))))
	   (%P-STORE-CONTENTS-OFFSET VALUE P 8)	;Update or flush associated value
	   (RETURN VALUE))
	  ((%P-CONTENTS-EQ P NIL)		;Remember empty slot
	   (SETQ EMPTYP P)))))

;These are not the same order of arguments as in Interlisp
;FUNCTION gets KEY VALUE for each pair in HASH-TABLE
;Note that if another process does either a GETHASH or a PUTHASH
; while you are MAPHASHing the whole thing can be bollixed, it is
; the responsibility of the user to synchronize this and all other
; uses of the same hash-table in multiple processes.
(DEFUN MAPHASH (FUNCTION HASH-TABLE)
  (CHECK-ARG HASH-TABLE (EQ (TYPEP HASH-TABLE) 'HASH-TABLE) "a hash-table")
  (DO ((BLOCK 0 (+ BLOCK 16.))
       (N-BLOCKS (HASH-TABLE-MODULUS HASH-TABLE) (1- N-BLOCKS)))
      ((ZEROP N-BLOCKS))
    (DO ((I (+ BLOCK 7) (1- I))
	 (TEM))
	((< I BLOCK))
      (AND (SETQ TEM (AREF HASH-TABLE I))
	   (FUNCALL FUNCTION TEM (AREF HASH-TABLE (+ I 8))))))
  HASH-TABLE)

;Standard rehash function
(DEFUN HASH-TABLE-REHASH (HASH-TABLE GROW)
  (AND GROW
       (LET ((NEW-SIZE (HASH-TABLE-GOOD-SIZE
			 (IF (FLOATP GROW) (FIX (* (ARRAY-LENGTH HASH-TABLE) GROW))
			     (+ (ARRAY-LENGTH HASH-TABLE) GROW GROW)))))
	 (SETF (HASH-TABLE-MODULUS HASH-TABLE) (// NEW-SIZE 16.))
	 (SETQ HASH-TABLE (ADJUST-ARRAY-SIZE HASH-TABLE NEW-SIZE))))
  (LET ((TEMP-ARRAY (MAKE-ARRAY NIL 'ART-Q (ARRAY-LENGTH HASH-TABLE))))
    (COPY-ARRAY-CONTENTS HASH-TABLE TEMP-ARRAY)
    (CLRHASH HASH-TABLE)
    (DO ((BLOCK 0 (+ BLOCK 16.))
	 (N-BLOCKS (HASH-TABLE-MODULUS HASH-TABLE) (1- N-BLOCKS)))
	((ZEROP N-BLOCKS))
      (DO ((I (+ BLOCK 7) (1- I))
	   (TEM))
	  ((< I BLOCK))
	(AND (SETQ TEM (AREF TEMP-ARRAY I))
	     (PUTHASH TEM (AREF TEMP-ARRAY (+ I 8)) HASH-TABLE))))
    (RETURN-ARRAY TEMP-ARRAY))
  HASH-TABLE)

;Named-structure handler
(DEFUN HASH-TABLE (MESSAGE SELF &REST IGNORE)
  (SELECTQ MESSAGE
    (:WHICH-OPERATIONS '(:DESCRIBE))
    (:DESCRIBE (FORMAT T "~&~S is a hash-table with ~D entries out of a possible ~D (~D%).~%"
		         SELF (HASH-TABLE-FULLNESS SELF) (* (HASH-TABLE-MODULUS SELF) 8)
			 (// (* (HASH-TABLE-FULLNESS SELF) 100.)
			     (* (HASH-TABLE-MODULUS SELF) 8)))
	       (OR (= (HASH-TABLE-GC-GENERATION-NUMBER SELF) %GC-GENERATION-NUMBER)
		   (FORMAT T " rehash is required due to GC.~%"))
	       (FORMAT T " The rehash function is ~S with increase parameter ~D.~%"
		         (HASH-TABLE-REHASH-FUNCTION SELF) (HASH-TABLE-REHASH-SIZE SELF))
	       (COND ((Y-OR-N-P "Do you want to see the block fullness distribution?")
		      (DO ((RES (MAKE-ARRAY NIL 'ART-32B 9))
			   (I (HASH-TABLE-MODULUS SELF))
			   (TOT (HASH-TABLE-MODULUS SELF))
			   (N 0 0))
			  ((ZEROP I)
			   (DOTIMES (N 9)
			     (FORMAT T "~&Blocks with ~D elements: ~4D ~3D%~%"
				       N (AREF RES N) (// (* (AREF RES N) 100.) TOT))))
			(SETQ I (1- I))
			(DO ((J (* I 16.) (1+ J))
			     (K 8 (1- K)))
			    ((ZEROP K))
			  (AND (AREF SELF J) (SETQ N (1+ N))))
			(ASET (1+ (AREF RES N)) RES N))))
	       (AND (NOT (ZEROP (HASH-TABLE-FULLNESS SELF)))
		    (Y-OR-N-P "Do you want to see the contents of the hash table?")
		    (IF (NOT (Y-OR-N-P "Do you want it sorted?"))
			(MAPHASH #'(LAMBDA (KEY VALUE) (FORMAT T "~& ~S -> ~S~%" KEY VALUE))
				 SELF)
			(LOCAL-DECLARE ((SPECIAL *L*))
			  (LET ((*L* NIL))
			    (MAPHASH #'(LAMBDA (KEY VALUE) (PUSH (LIST KEY VALUE) *L*))
				     SELF)
			    (SETQ *L* (SORTCAR *L* #'ALPHALESSP))
			    (FORMAT T "~&~:{ ~S -> ~S~%~}" *L*))))))))
