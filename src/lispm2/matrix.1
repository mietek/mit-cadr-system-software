;;; Matrix arithmetic.   MMcM  7/23/80  -*- Mode: Lisp; Package: MATH -*-

;;; Convert a 2d array into a list of lists of the elements
(DEFUN LIST-2D-ARRAY (ARRAY)
  (CHECK-ARG ARRAY (AND (ARRAYP ARRAY)
			(= (ARRAY-/#-DIMS ARRAY) 2))
	     "A Two-dimensional array")
  (DO ((I 0 (1+ I))
       (DIM-1 (ARRAY-DIMENSION-N 1 ARRAY))
       (DIM-2 (ARRAY-DIMENSION-N 2 ARRAY))
       (LIST NIL))
      (( I DIM-1)
       (NREVERSE LIST))
    (PUSH (DO ((J 0 (1+ J))
	       (LIST NIL))
	      (( J DIM-2)
	       (NREVERSE LIST))
	    (PUSH (AREF ARRAY I J) LIST))
	  LIST)))

;;; Fill up a 2d array from a list, like fillarray, the lists can wrap around as needed
(DEFUN FILL-2D-ARRAY (ARRAY LIST)
  (CHECK-ARG ARRAY (AND (ARRAYP ARRAY)
			(= (ARRAY-/#-DIMS ARRAY) 2))
	     "A Two-dimensional array")
  (DO ((I 0 (1+ I))
       (DIM-1 (ARRAY-DIMENSION-N 1 ARRAY))
       (DIM-2 (ARRAY-DIMENSION-N 2 ARRAY))
       (L LIST (CDR L))
       (SUBLIST))
      (( I DIM-1))
    (AND (NULL L)
	 (SETQ L LIST))
    (SETQ SUBLIST (CAR L))
    (DO ((J 0 (1+ J))
	 (L SUBLIST (CDR L)))
	(( J DIM-2))
      (AND (NULL L)
	   (SETQ L SUBLIST))
      (ASET (CAR L) ARRAY I J))))

;;; Multiply two matrices into a third.  
;;; A 1d array of dimension N is treated as a Nx1 array.
(DEFUN MULTIPLY-MATRICES (MATRIX-1 MATRIX-2 &OPTIONAL MATRIX-3
					    &AUX MAT-1 MAT-2 MAT-3
						 DIM-1 DIM-2 COM-DIM)
  (SETQ DIM-1 (ARRAY-DIMENSION-N 1 MATRIX-1)
	DIM-2 (OR (ARRAY-DIMENSION-N 2 MATRIX-2) 1)
	COM-DIM (ARRAY-DIMENSION-N 1 MATRIX-2))
  (OR (= COM-DIM (OR (ARRAY-DIMENSION-N 2 MATRIX-1) 1))
      (FERROR NIL "The matrices ~S and ~S are not compatible for multiplication"
	      MATRIX-1 MATRIX-2))
  (IF MATRIX-3
      (OR (AND (= DIM-1 (ARRAY-DIMENSION-N 1 MATRIX-3))
	       (= DIM-2 (OR (ARRAY-DIMENSION-N 2 MATRIX-3) 1)))
	  (FERROR NIL "The matrix ~S is not the right size for multiplying ~S and ~S"
		  MATRIX-3 MATRIX-1 MATRIX-2))
      (SETQ MATRIX-3 (MAKE-ARRAY NIL (ARRAY-TYPE MATRIX-1)
				 (IF (= DIM-2 1) DIM-1 (LIST DIM-1 DIM-2)))))
  ;; Make indirect arrays to any vectors, so can use ar-2 everywhere below
  (SETQ MAT-1 (IF (ARRAY-DIMENSION-N 2 MATRIX-1) MATRIX-1
		  (MAKE-ARRAY NIL (ARRAY-TYPE MATRIX-1)
			      (LIST DIM-1 COM-DIM) MATRIX-1)))
  (SETQ MAT-2 (IF (ARRAY-DIMENSION-N 2 MATRIX-2) MATRIX-2
		  (MAKE-ARRAY NIL (ARRAY-TYPE MATRIX-2)
			      (LIST COM-DIM DIM-2) MATRIX-2)))
  (SETQ MAT-3 (IF (ARRAY-DIMENSION-N 3 MATRIX-3) MATRIX-3
		  (MAKE-ARRAY NIL (ARRAY-TYPE MATRIX-3)
			      (LIST DIM-1 DIM-2) MATRIX-3)))
  ;; Do the actual multiplication
  (DOTIMES (I DIM-1)
    (DOTIMES (J DIM-2)
      (ASET (DO ((K 0 (1+ K))
		 (SUM 0 (+ SUM (* (AREF MAT-1 I K) (AREF MAT-2 K J)))))
		(( K COM-DIM) SUM))
	    MAT-3 I J)))
  ;; Try to get rid of any temporary arrays we made
  (AND (NEQ MATRIX-3 MAT-3)
       (RETURN-ARRAY MAT-3))
  (AND (NEQ MATRIX-2 MAT-2)
       (RETURN-ARRAY MAT-2))
  (AND (NEQ MATRIX-1 MAT-1)
       (RETURN-ARRAY MAT-1))
  MATRIX-3)

;;; Gauss-Jordan inversion
(DEFUN INVERT-MATRIX (MATRIX &OPTIONAL INTO-MATRIX &AUX DIM COLS TEM)
  (CHECK-ARG MATRIX (AND (ARRAYP MATRIX)
			 (EQ (SETQ DIM (ARRAY-DIMENSION-N 1 MATRIX))
			     (ARRAY-DIMENSION-N 2 MATRIX)))
	     "A square matrix")
  (IF INTO-MATRIX
      (OR (AND (EQ DIM (ARRAY-DIMENSION-N 1 INTO-MATRIX))
	       (EQ DIM (ARRAY-DIMENSION-N 2 INTO-MATRIX)))
	  (FERROR NIL "~S is not correct for the inverse of ~S" INTO-MATRIX MATRIX))
      (SETQ INTO-MATRIX (MAKE-ARRAY NIL (ARRAY-TYPE MATRIX) (LIST DIM DIM))))
  (COPY-ARRAY-CONTENTS MATRIX INTO-MATRIX)
  (SETQ COLS (MAKE-ARRAY NIL 'ART-Q DIM)
	TEM (MAKE-ARRAY NIL 'ART-Q (LIST DIM DIM)))
  (DO ((I 0 (1+ I))
       (COLS-USED (MAKE-ARRAY NIL 'ART-1B DIM))
       (J))
      (( I DIM)
       (RETURN-ARRAY COLS-USED))
    ;; Find the greatest element in this row in an unused column
    (SETQ J (DO ((J 0 (1+ J))
		 (MAX 0) (POS)
		 (TEM))
		(( J DIM)
		 (AND (ZEROP MAX)
		      (FERROR NIL "Singular matrix, you lose"))
		 POS)
	      (AND (ZEROP (AREF COLS-USED J))
		   (> (SETQ TEM (ABS (AREF INTO-MATRIX I J))) MAX)
		   (SETQ MAX TEM POS J))))
    (ASET J COLS I)
    (ASET 1 COLS-USED J)
    ;; Pivot about I,J
    (DO ((K 0 (1+ K))
	 (ELEM-I-J (AREF INTO-MATRIX I J)))
	(( K DIM))
      (DO ((L 0 (1+ L))
	   (ELEM-K-J (AREF INTO-MATRIX K J))
	   (ELEM))
	  (( L DIM))
	(SETQ ELEM (AREF INTO-MATRIX K L))
	(ASET (IF (= K I)			;Same row?
		  (IF (= L J)			;Corner itself?
		      (// ELEM)
		      (// ELEM ELEM-I-J))
		  (IF (= L J)			;Same column?
		      (- (// ELEM ELEM-I-J))
		      (- ELEM (// (* ELEM-K-J (AREF INTO-MATRIX I L)) ELEM-I-J))))
	      TEM K L)))
    (COPY-ARRAY-CONTENTS TEM INTO-MATRIX))
  ;; And finally permute
  (DOTIMES (I DIM)
    (DO ((J 0 (1+ J))
	 (K (AREF COLS I)))
	(( J DIM))
      (ASET (AREF INTO-MATRIX I J) TEM K J)))
  (DOTIMES (I DIM)
    (DO ((K 0 (1+ K))
	 (J (AREF COLS I)))
	(( K DIM))
      (ASET (AREF TEM K J) INTO-MATRIX K I)))
  (RETURN-ARRAY TEM)
  (RETURN-ARRAY COLS)
  INTO-MATRIX)

(DEFUN TRANSPOSE-MATRIX (MATRIX &OPTIONAL INTO-MATRIX &AUX DIM-1 DIM-2)
  (CHECK-ARG MATRIX (AND (ARRAYP MATRIX)
			 (= (ARRAY-/#-DIMS MATRIX) 2))
	     "A 2 dimensional array")
  (SETQ DIM-1 (ARRAY-DIMENSION-N 1 MATRIX)
	DIM-2 (ARRAY-DIMENSION-N 2 MATRIX))
  (IF INTO-MATRIX
      (OR (AND (EQ DIM-1 (ARRAY-DIMENSION-N 2 INTO-MATRIX))
	       (EQ DIM-2 (ARRAY-DIMENSION-N 1 INTO-MATRIX)))
	  (FERROR NIL "~S wrong dimensions for transpose of ~S" INTO-MATRIX MATRIX))
      (SETQ INTO-MATRIX (MAKE-ARRAY NIL (ARRAY-TYPE MATRIX) (LIST DIM-2 DIM-1))))
  (IF (EQ MATRIX INTO-MATRIX)			;Special case
      (DOTIMES (I DIM-1)
	(DO J I (1+ J) ( J DIM-1)
	  (ASET (PROG1 (AREF MATRIX I J) (ASET (AREF MATRIX J I) MATRIX I J)) MATRIX J I)))
      (DOTIMES (I DIM-1)
	(DOTIMES (J DIM-2)
	  (ASET (AREF MATRIX I J) INTO-MATRIX J I))))
  INTO-MATRIX)

(DEFUN DETERMINANT (MATRIX &AUX DIM TEM1 TEM2)
  (CHECK-ARG MATRIX (AND (ARRAYP MATRIX)
			 (EQ (SETQ DIM (ARRAY-DIMENSION-N 1 MATRIX))
			     (ARRAY-DIMENSION-N 2 MATRIX)))
	     "A square matrix")
  (SETQ TEM1 (MAKE-ARRAY NIL 'ART-1B DIM)
	TEM2 (MAKE-ARRAY NIL 'ART-1B DIM))
  (PROG1 (DET-1 MATRIX TEM1 TEM2 DIM DIM)
	 (RETURN-ARRAY TEM1)
	 (RETURN-ARRAY TEM2)))

(DEFUN DET-1 (MATRIX TEM1 TEM2 DIM N)
  (IF (= N 2)					;The case we know about really
      (LET* ((X1 (FIND-FREE-DIM TEM1 0 DIM))
	     (Y1 (FIND-FREE-DIM TEM2 0 DIM))
	     (X2 (FIND-FREE-DIM TEM1 (1+ X1) DIM))
	     (Y2 (FIND-FREE-DIM TEM2 (1+ Y1) DIM)))
	(- (* (AREF MATRIX X1 Y1) (AREF MATRIX X2 Y2))
	   (* (AREF MATRIX X1 Y2) (AREF MATRIX X2 Y1))))
      (LET ((X (FIND-FREE-DIM TEM1 0 DIM))
	    (SUM 0))
	(ASET 1 TEM1 X)
	(DO ((I 0 (1+ I))
	     (Y 0 (1+ Y)))
	    (( I N))
	  (SETQ Y (FIND-FREE-DIM TEM2 Y DIM))
	  (ASET 1 TEM2 Y)
	  (SETQ SUM (+ SUM (* (IF (ODDP (+ X Y)) -1 1) (AREF MATRIX X Y)
			      (DET-1 MATRIX TEM1 TEM2 DIM (1- N)))))
	  (ASET 0 TEM2 Y))
	(ASET 0 TEM1 X)
	SUM)))

(DEFUN FIND-FREE-DIM (ARRAY START END)
  (DO ((I START (1+ I)))
      (( I END)
       (FERROR NIL "Array ~S screwed up" ARRAY))
    (AND (ZEROP (AREF ARRAY I))
	 (RETURN I))))

;;; Linear equation solving.   DLW 8/4/80

;;; The functions below are useful for solving systems of simultaneous
;;; linear equations.  They are taken from the text "Computer Solution of
;;; Linear Algebraic Systems", by Forsythe and Moler, Prentice-Hall 1967.
;;; 
;;; The function DECOMPOSE takes a square matrix A (N by N elements) and
;;; returns a square matrix holding the LU decomposition of A.  The
;;; function finds the unique solution of L * U = A, where L is a lower
;;; triangular matrix with 1's along the diagonal, and U is an upper
;;; triangular matrix.  The function returns a square matrix holding L-I+U;
;;; that is, the lower triangle not including the diagonal holds L, and the
;;; rest holds U, with the 1's along the diagonal of L not actually stored.
;;; (Note: the LU decomposition exists uniquely only if all of the
;;; principle minor matricies made from the first K rows and columns are
;;; non-singular; see Forsythe and Moler, Theorem 9.2.)
;;; 
;;; The function SOLVE takes the LU decomposition of A, and a vector of
;;; solutions of the equations B, and returns X where A * X = B.
;;; 
;;; DECOMPOSE uses partial pivoting.  Rather than actually moving the
;;; elements of the array from one row to another, it returns a permutation
;;; array PS telling how the rows of LU are permuted.  The PS array must
;;; then be passed into SOLVE so that it can interpret LU properly.
;;;
;;; Iterative improvement is not yet implemented.


;;; Utility functions and macros.

(DEFUN 1D-ARRAYP (ARRAY)
   (AND (ARRAYP ARRAY)
	(= (ARRAY-/#-DIMS ARRAY) 1)))

(DEFUN 2D-ARRAYP (ARRAY)
   (AND (ARRAYP ARRAY)
	(= (ARRAY-/#-DIMS ARRAY) 2)))

(DEFMACRO EXCHANGE (F1 F2)
   (LET ((V1 (GENSYM))
	 (V2 (GENSYM)))
     `(LET ((,V1 ,F1)
	    (,V2 ,F2))
	(SETF ,F1 ,V2)
	(SETF ,F2 ,V1))))

(DEFMACRO MVRETURN BODY
   `(PROG () (RETURN . ,BODY)))



;;; DECOMPOSE
;;; A is an N by N array.
;;; Two values are returned: LU and PS.
;;; The caller may provide arrays to be used for LU and PS by passing
;;; the optional arguments; otherwise, new arrays will be allocated.
;;; If the same array is passed as A and LU, A is overwriten with
;;; the decomposition correctly.
;;; The condition SINGULAR is raised if the matrix is singular.

(DEFUN DECOMPOSE (A &OPTIONAL LU PS &AUX N)
   ;; Prepare arguments.
   (CHECK-ARG A 2D-ARRAYP "a two-dimensional array")
   (SETQ N (ARRAY-DIMENSION-N 1 A))
   (OR (= N (ARRAY-DIMENSION-N 2 A))
       (FERROR NIL "The first argument must be a square array."))
   (IF LU
       (CHECK-ARG LU 2D-ARRAYP "a two-dimensional array")
       (SETQ LU (MAKE-ARRAY NIL (ARRAY-TYPE A) (LIST N N))))
   (IF PS
       (CHECK-ARG PS 1D-ARRAYP "a one-dimensional array")
       (SETQ PS (MAKE-ARRAY NIL 'ART-Q N)))

   (LET ((SCALES (MAKE-ARRAY NIL 'ART-Q N)))
     ;; Init PS to the identity, LU to A, and SCALES to the reciprocal
     ;; of the largest-magnitude element on a given row.
     (DOTIMES (I N)
	(ASET I PS I)
	(LET ((NORMROW 0.0s0))
	  (DOTIMES (J N)
	     (LET ((AIJ (AREF A I J)))
	       (ASET AIJ LU I J)
	       (SETQ NORMROW (MAX (ABS AIJ) NORMROW))))
	  (IF (ZEROP NORMROW)
	      (FERROR 'SINGULAR "The matrix is singular: it has a zero row."))
	  (ASET (// 1.0s0 NORMROW) SCALES I)))

     ;; Gaussian elimination with partial pivorting.
     (DOTIMES (K (- N 1))
	;; Find the pivot index.
	(LET ((PIVOTINDEX NIL)
	      (BIGGEST 0.0s0))
	  (DO I K (1+ I) (>= I N)
	    (LET ((SIZE (* (ABS (AREF LU (AREF PS I) K))
			   (AREF SCALES (AREF PS I)))))
	      (COND ((> SIZE BIGGEST)
		     (SETQ BIGGEST SIZE)
		     (SETQ PIVOTINDEX I)))))
	  (IF (ZEROP BIGGEST)
	      (FERROR 'SINGULAR "The matrix is singular: SOLVE will divide by zero."))
	  (EXCHANGE (AREF PS PIVOTINDEX) (AREF PS K)))
 
	;; Do the elimination with that pivoting.
	(LET* ((PSK (AREF PS K))
	       (PIVOT (AREF LU PSK K)))
	  (DO I (1+ K) (1+ I) (>= I N)
	    (LET ((PSI (AREF PS I)))
	      (LET ((MULT (// (AREF LU PSI K) PIVOT)))
		(ASET MULT LU PSI K)
		(IF (NOT (ZEROP MULT))
		    (DO J (1+ K) (1+ J) (>= J N)
			(ASET (- (AREF LU PSI J) (* MULT (AREF LU PSK J)))
			      LU PSI J))))))))
     (IF (= (AREF LU (AREF PS (1- N)) (1- N)) 0.0s0)
	 (FERROR 'SINGULAR "The matrix is singular: SOLVE will divide by zero."))
     (RETURN-ARRAY SCALES))
   (MVRETURN LU PS))

;;; SOLVE
;;; LU is the N by N LU-decomposition of A.
;;; PS is the N-long permutation vector for LU.  B is an N-long array
;;; of solutions to the equations.
;;; The returned value is X: the solution of A * X = B.
;;; The caller may provide the array to be used as X by passing the optional
;;; argument.

(DEFUN SOLVE (LU PS B &OPTIONAL X &AUX N)
   ;; Prepare arguments.
   (CHECK-ARG LU 2D-ARRAYP "a two-dimensional array")
   (SETQ N (ARRAY-DIMENSION-N 1 LU))
   (OR (= N (ARRAY-DIMENSION-N 2 LU))
       (FERROR NIL "The first argument must be a square array."))
   (CHECK-ARG PS 1D-ARRAYP "a one-dimensional array")
   (CHECK-ARG B 1D-ARRAYP "a one-dimensional array")
   (IF X
       (CHECK-ARG X 1D-ARRAYP "a one-dimensional array")
       (SETQ X (MAKE-ARRAY NIL (ARRAY-TYPE B) N)))

   (DOTIMES (I N)
      (LET ((PSI (AREF PS I))
	    (DOT 0.0s0))
	(DOTIMES (J I)
	   (SETQ DOT (+ DOT (* (AREF LU PSI J) (AREF X J)))))
	(ASET (- (AREF B PSI) DOT) X I)))

   (DO I (1- N) (1- I) (< I 0)
      (LET ((PSI (AREF PS I))
	    (DOT 0.0s0))
	(DO J (1+ I) (1+ J) (>= J N)
	   (SETQ DOT (+ DOT (* (AREF LU PSI J) (AREF X J)))))
	(ASET (// (- (AREF X I) DOT)
		  (AREF LU PSI I))
	      X I)))

   X)
