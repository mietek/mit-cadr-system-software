;SORT PACKAGE   -*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;ENTRIES
;  SORT <list or array> <lessp predicate>
;  SORTCAR  ..  ..
;  SORT-SHORT-LIST <list> <lessp predicate>
;     A simple exchange sort, good for short lists.  Need not be contiguous.
;  SORTCAR-SHORT-LIST .. ..
;  SORT-GROUPED-ARRAY <array-only> <group-size> <lessp predicate>
;     Assumes logical records come in groups of <group-size> entries.
;             the key is the first entry of the group.
;     Uses ARRAY-ACTIVE-LENGTH to determine portion of array to be sorted.
;  SORT-GROUPED-ARRAY-GROUP-KEY <array-only> <group-size> <lessp predicate>
;     Similar to SORT-GROUPED-ARRAY, but <lessp predicate> should be a function
;     of four arguments, <array1> <idx1> <array2> <idx2>.  Thus, it can
;     reference the entire group, if desired, not just the first element.

;INTERNAL FUNCTIONS
;  SORT-ARRAY-QUICK <array> <left index> <right index>
;       Uses insertion sort if small, otherwise Quicksort
;	Indices are inclusive.
;  SORT-GROUPED-ARRAY-QUICK <array> <left index> <right index> <group-size>
;       Analogous to above for use by SORT-GROUPED-ARRAY.
;  SORT-GROUPED-ARRAY-GROUP-KEY-QUICK <array> <left index> <right index> <group-size>
;       Analogous for SORT-GROUPED-ARRAY-GROUP-KEY.
;  SORT-CONTIG-LIST-QUICK <list> <length> 
;  SORT-LIST <list>
;	Hacks contiguous lists, does combined merge and quick sort
;  SORT-LIST-PREFIX <height>
;  SORT-LIST-MERGE <list1> <list2>
;  SORT-LIST-MERGE-CONTIG <list1> <length1> <list2> <length2>

;SUBROUTINE (SHOULD BE PUT INTO THE NUCLEUS IN SOME FORM)
;  CONTIGUOUS-LIST-INFO <list>
;     Returns 2 values:
;	Number of contiguous CDR-NEXTs in <list> (1- the number of contiguous CAR cells)
;	"last" of the contiguous part.  CAR of this is last CAR cell, CDR is link
;	the non-contiguous part of the list.
;     If you call this with an argument of NIL, it will either loop or err out.
;
;NOTE the macros A2R and A2S herein should not be put into the environment.
; They are only an aid to coding the thing, and should be removed after compilation.

;SPECIAL VARIABLES
(DECLARE (SPECIAL SORT-LESSP-PREDICATE
		  SORT-LESSP-PREDICATE-ON-CAR
		  SORT-INPUT-LIST
		  SORT-DUMMY-ARRAY-HEADER
		  SORT-QS-BREAKEVEN 
		  SORT-ARRAY-TEMP-V)
	 (SETQ RETAIN-VARIABLE-NAMES-SWITCH 'ARGS)
	 )

(SETQ SORT-QS-BREAKEVEN 10) ;BREAKEVEN POINT BETWEEN INSERTION POINT AND QUICKSORT

;Special Considerations
;
; GC must never de-linearize lists.
;  The hairy version of NRECONC (NREVERSE) depends on this too.
; Note that a list can get de-linearized by the GC finding a pointer
;  to the middle and copying from there.  One way around this is to
;  set up an arrangement to be interrupted, signalled, thrown-through,
;  or whatever when a flip happens, then at the time the size of a
;  contiguous segment of list is counted, ensure that everything is
;  in newspace (already copied).  Great care is required.

(DEFUN SORTCAR (X SORT-LESSP-PREDICATE-ON-CAR)
  (SORT X (FUNCTION (LAMBDA (X Y) (FUNCALL SORT-LESSP-PREDICATE-ON-CAR (CAR X) (CAR Y))))))

(DEFUN SORT (X SORT-LESSP-PREDICATE &AUX TEM)
  (COND ((LISTP X)
	 (COND ((< (LENGTH X) 12.) (SORT-SHORT-LIST X SORT-LESSP-PREDICATE))
	       (T (SORT-LIST X))))
	((NULL X)					;NIL IS A LIST, SORT OF
	 X)
	((ARRAYP X)
	 (SORT-ARRAY-QUICK X 0 (1- (ARRAY-ACTIVE-LENGTH X)))
	 X)
	((AND (SYMBOLP X)
	      (ARRAYP (SETQ TEM (CAR (FUNCTION-CELL-LOCATION X)))))
	 (SORT-ARRAY-QUICK TEM 0 (1- (ARRAY-LENGTH TEM)))
	 X)
	((ERROR "ARG MUST BE A LIST OR AN ARRAY - SORT" X))))

(DEFUN SORT-SHORT-LIST (L LPRED)
  (COND ((CDR L)
	 (DO ((I (1- (LENGTH L)) (1- I))
	      (SWITCH NIL))
	     ((OR (ZEROP I) SWITCH))
	    (SETQ SWITCH T)
	    (DO LP L (CDR LP) (NULL (CDR LP))
	      (COND ((FUNCALL LPRED (CADR LP) (CAR LP))
		     (RPLACA LP (PROG1 (CADR LP) (RPLACA (CDR LP) (CAR LP))))
		     (SETQ SWITCH NIL)))))))
  L)

(DEFUN SORTCAR-SHORT-LIST (L LPRED)
 (PROG (LP SWITCH)
       (COND ((NULL (CDR L))
	      (RETURN L)))
   L0  (SETQ LP L)
   L1  (COND ((FUNCALL LPRED (CAADR LP) (CAAR LP))
	      (RPLACA LP (PROG1 (CADR LP) (RPLACA (CDR LP) (CAR LP))))
	      (SETQ SWITCH T)))
   	(SETQ LP (CDR LP))
	(COND ((CDR LP) (GO L1))
	      (SWITCH (SETQ SWITCH NIL)
		      (GO L0)))
	(RETURN L)))

;  CONTIGUOUS-LIST-INFO <list>
;     Returns 2 values:
;	Number of contiguous CDR-NEXTs in <list> (1- the number of contiguous CAR cells)
;	"last" of the contiguous part.  CAR of this is last CAR cell, CDR is link
;	the non-contiguous part of the list.
;     If you call this with an argument of NIL, it will either loop or err out.

(DEFUN CONTIGUOUS-LIST-INFO (LIST)
  (PROG ((N 0))
LOOP (OR (AND (= (%P-LDB %%Q-CDR-CODE LIST) CDR-NEXT)
	      (NEQ (%P-LDB %%Q-DATA-TYPE LIST) DTP-HEADER-FORWARD))
	 (RETURN N LIST))
     (SETQ N (1+ N) LIST (CDR LIST))
     (GO LOOP)))

(DEFUN SORT-CONTIG-LIST-QUICK (LIST LENGTH &AUX LOC)	;LENGTH IS 1- REAL LENGTH
  (OR SORT-DUMMY-ARRAY-HEADER				;HACK UP AN ARRAY
      (SETQ SORT-DUMMY-ARRAY-HEADER
	    (MAKE-ARRAY NIL 'ART-Q-LIST '(100) LIST)))
  (%P-STORE-CONTENTS-OFFSET LIST SORT-DUMMY-ARRAY-HEADER 1)	;MAKE ARRAY POINT TO THIS LIST
  (%P-STORE-CONTENTS-OFFSET (1+ LENGTH) SORT-DUMMY-ARRAY-HEADER 2)
  (COND ((= DTP-HEADER-FORWARD (%P-LDB-OFFSET %%Q-DATA-TYPE LIST LENGTH))
	 (SETQ LOC (%P-CONTENTS-AS-LOCATIVE-OFFSET LIST LENGTH))
	 (WITHOUT-INTERRUPTS			;No %P-STORE-TAG-AND-POINTER-OFFSET
	   (%P-DPB-OFFSET CDR-NIL %%Q-CDR-CODE LIST LENGTH)
	   (%P-STORE-CONTENTS-OFFSET (CAR LOC) LIST LENGTH))))
  (SORT-ARRAY-QUICK SORT-DUMMY-ARRAY-HEADER 0 LENGTH)	;CALL ARRAY QUICKSORT ON IT
  (COND (LOC
	 (RPLACA LOC (%P-CONTENTS-OFFSET LIST LENGTH))
	 (WITHOUT-INTERRUPTS
	   (%P-STORE-CONTENTS-OFFSET LOC LIST LENGTH)
	   (%P-DPB-OFFSET (+ (LSH CDR-NORMAL 6) DTP-HEADER-FORWARD)
			  %%Q-ALL-BUT-POINTER LIST LENGTH)))))

; List sorting algorithm
;
; Due to MJF and GLS.
;
; The basic idea is to do a merge sort, which gets the list into
; order by doing RPLACDs.  (This is the same algorithm as is
; used for sorting lists in Maclisp.)  It operates by considering
; the given list to be the frontier of a binary tree (which may be
; incomplete if the length of the list is not a power of two).
; At each node, the two nodes below it are merged.  The frontier
; nodes are one-element lists, these are then merged into bigger lists.
; Instead of the usual method of merging all pairs, then all pairs
; of pairs, etc., this implementation effectively does a suffix walk
; over the binary tree (thus it can grab items sequentially off the given list.)
; Warning: like DELQ and others, the safe way to use this
; function is (SETQ FOO (ALPHASORT FOO)) or whatever.
;
; On the lisp machine, the above algorithm does not work well, because
; cdr-coded (contiguous) lists cannot be RPLACD'ed without implicit CONSing.
; Instead, contiguous chunks of the list are sorted in place.
; The idea is to use a merge sort on the list of contiguous chunks
; and to be a little hairy when comparing two chunks
; in the merge.  First, on encountering each chunk it is sorted
; (using quicksort).  Then, when two chunks meet during a merge,
; they are merged together in place, one getting all the low elements
; and one all thee high elements.  Deciding which one to use for the
; high chunk is a little tricky; note the code carefully.
; The two chunks are combined by a straight insertion technique; there may be
; better ways to combine two already sorted chunks.  Another approach
; not used here would be not to sort each chunk using quicksort except
; the first, and then to be hairier about the insertion technique.

(DEFUN SORT-LIST (SORT-INPUT-LIST &AUX SORT-DUMMY-ARRAY-HEADER)
  (DO ((HEIGHT -1 (1+ HEIGHT))
       (SOFAR NIL))
      ((NULL SORT-INPUT-LIST)
       (AND SORT-DUMMY-ARRAY-HEADER
	    (RETURN-ARRAY (PROG1 SORT-DUMMY-ARRAY-HEADER (SETQ SORT-DUMMY-ARRAY-HEADER NIL))))
       SOFAR)
    (SETQ SOFAR (SORT-LIST-MERGE SOFAR (SORT-LIST-PREFIX HEIGHT)))))

(DEFUN SORT-LIST-PREFIX (HEIGHT &AUX LENGTH LAST)	;GET MERGED BINARY TREE, SPECD HEIGHT
  (COND ((NULL SORT-INPUT-LIST) NIL)			;INPUT EXHAUSTED, INCOMPLETE TREE
	((< HEIGHT 1)
	 (MULTIPLE-VALUE (LENGTH LAST)			;PULL OFF A CONTIGUOUS SEGMENT OF LIST
	       (CONTIGUOUS-LIST-INFO SORT-INPUT-LIST))
	 (AND (> LENGTH 0)				;IF MORE THAN A SINGLE CELL, SORT IT.
	      (SORT-CONTIG-LIST-QUICK SORT-INPUT-LIST LENGTH))
	 (PROG1 SORT-INPUT-LIST				;RETURN THAT SEGMENT
		(AND (SETQ SORT-INPUT-LIST (CDR LAST))	;ADVANCE TO NEXT
		     (RPLACD LAST NIL))))		;MAKE SURE RETURNED SEGMENT ENDS
	((SORT-LIST-MERGE (SORT-LIST-PREFIX (1- HEIGHT))
			  (SORT-LIST-PREFIX (1- HEIGHT))))))

(DEFUN SORT-LIST-MERGE (L1 L2 &AUX R)			;MERGE TWO SORTED LISTS, HACKING CONTIG
  (DO ((P (VALUE-CELL-LOCATION 'R))			;R ACCUMULATES RESULT, P POINTS TO TAIL
       (LAST1) (LENGTH1) (LAST2) (LENGTH2) (HIGH1) (HIGH2))
      ((COND ((NULL L1)					;IF AN INPUT IS EXHAUSTED, DONE
	      (RPLACD P L2)
	      (RETURN R))
	     ((NULL L2)
	      (RPLACD P L1)
	      (RETURN R))))
    (MULTIPLE-VALUE (LENGTH1 LAST1) (CONTIGUOUS-LIST-INFO L1))	;PULL OFF A CONTIGUOUS CHUNK
    (MULTIPLE-VALUE (LENGTH2 LAST2) (CONTIGUOUS-LIST-INFO L2))	;OF EACH LIST
    (SETQ HIGH1 (CAR LAST1) HIGH2 (CAR LAST2))
    (COND ((FUNCALL SORT-LESSP-PREDICATE HIGH2 (CAR L1))	;SEE IF CHUNK2 ALL < CHUNK1
	   (RPLACD P L2)
	   (SETQ P LAST2 L2 (CDR LAST2)))
	  ((OR (AND (= LENGTH1 0) (= LENGTH2 0))		;SMALL CHUNKS, BYPASS HAIR
	       (FUNCALL SORT-LESSP-PREDICATE HIGH1 (CAR L2)))	;SEE IF CHUNK1 ALL < CHUNK2
	   (RPLACD P L1)
	   (SETQ P LAST1 L1 (CDR LAST1)))
	  ;; GOT TO MERGE CHUNKS, CHOOSE HIGHER.  BUT CORRECT THE LENGTHS FIRST.
	  ((PROGN (AND (ZEROP LENGTH1) (SETQ LENGTH1 1))
		  (AND (ZEROP LENGTH2) (SETQ LENGTH2 1))
		  (FUNCALL SORT-LESSP-PREDICATE HIGH1 HIGH2))
	   (SORT-LIST-MERGE-CONTIG L1 LENGTH1 L2 LENGTH2)
	   (RPLACD P L1)
	   (SETQ P LAST1 L1 (CDR LAST1)))
	  (T
	   (SORT-LIST-MERGE-CONTIG L2 LENGTH2 L1 LENGTH1)
	   (RPLACD P L2)
	   (SETQ P LAST2 L2 (CDR LAST2))))))

;MACROS FOR NEXT FUNCTION, ALLOW HACKING OF THE TWO LISTS AS ONE ARRAY.
;SHOULD -NOT- BE SENT-OVER.
;ALSO NOTE THE EVALUATION OF THE SUBSCRIPT SHOULD NOT HAVE SIDE-EFFECTS.

(DECLARE (PROG (QC-FILE-IN-PROGRESS)  ;DON'T SEND OVER!!  PROBABLY THERE'S A BETTER WAY
	    (DEFMACRO A2R (I)
		      `(COND ((< ,I N1) (%P-CONTENTS-OFFSET L1 ,I))
			(T (%P-CONTENTS-OFFSET L2 (- ,I N1)))))

	    (DEFMACRO A2S (X I)
		      `(COND ((< ,I N1) (%P-STORE-CONTENTS-OFFSET ,X L1 ,I))
			(T (%P-STORE-CONTENTS-OFFSET ,X L2 (- ,I N1))))) ))

;SIMPLE-MINDED INSERTION-SORT TAIL-END TO MERGE TWO SORTED ARRAYS
(DEFUN SORT-LIST-MERGE-CONTIG (L1 N1 L2 N2 &AUX (N1+N2 (+ N1 N2)))
  (DO ((I N1 (1+ I)))
      ((= I N1+N2))
    (DO ((J (1- I) (1- J))
	 (X (A2R I)))
	((OR (< J 0)
	     (NOT (FUNCALL SORT-LESSP-PREDICATE X (A2R J))))
	 (A2S X (1+ J)))
      (A2S (A2R J) (1+ J)))))

;Quicksort for arrays.  If the array is small, does an insertion sort instead.

(DEFUN SORT-ARRAY-QUICK (A L R)
       (COND ((> L (- R SORT-QS-BREAKEVEN))		;SEE IF SHOULD DO AN INSERTION SORT
	      (DO ((I (1+ L) (1+ I)))			;THIS CLAUSE ALSO APPLIES WHEN L>R
		  ((> I R))
		(DO ((J (1- I) (1- J))
		     (X (AR-1 A I)))
		    ((OR (< J L) (NOT (FUNCALL SORT-LESSP-PREDICATE X (AR-1 A J))))
		     (AS-1 X A (1+ J)))
		  (AS-1 (AR-1 A J) A (1+ J)))))
	     (T ((LAMBDA (N)				;RANDOMLY CHOSEN POINT BETWEEN L AND R
			 ((LAMBDA (M)			;BREAK-POINT BETWEEN LOW AND HIGH
				  (SORT-ARRAY-QUICK A L (1- M))		;SORT THE LOW ELEMENTS
				  (SORT-ARRAY-QUICK A (1+ M) R))	;SORT THE HIGH ELEMENTS
			  (DO ((K (PROG1 (AR-1 A N)	;K WILL BE M'TH ELEMENT
					 (AS-1 (AR-1 A L) A N)))
			       (I L)			;A[...I-1] < K
			       (J R))			;K < A[J+1...]
			      (NIL)
			   DECRJ			;DECREASE J UNTIL K NOT LT A[J]
			    (COND ((= J I)
				   (AS-1 K A I)
				   (RETURN I))
				  ((FUNCALL SORT-LESSP-PREDICATE K (AR-1 A J))
				   (SETQ J (1- J))
				   (GO DECRJ)))
			    (AS-1 (AR-1 A J) A I)
			    (SETQ I (1+ I))
			   INCRI			;INCREASE I UNTIL K NOT GT A[I]
			    (COND ((= I J)
				   (AS-1 K A J)
				   (RETURN J))
				  ((FUNCALL SORT-LESSP-PREDICATE (AR-1 A I) K)
				   (SETQ I (1+ I))
				   (GO INCRI)))
			    (AS-1 (AR-1 A I) A J)
			    (SETQ J (1- J)))))
		 ;(+ L (RANDOM (+ (- R L) 1)))
		 (+ L (// (- R L) 2))			;USE THIS UNTIL HAVE RANDOM FUNCTION
))))


(DEFUN SORT-GROUPED-ARRAY (A GS SORT-LESSP-PREDICATE)
  (PROG (SORT-ARRAY-TEMP-V)
	(SETQ SORT-ARRAY-TEMP-V (MAKE-ARRAY NIL 'ART-Q GS))
	(SORT-GROUPED-ARRAY-QUICK A 0 (- (ARRAY-ACTIVE-LENGTH A) GS) GS)
	(RETURN-ARRAY SORT-ARRAY-TEMP-V)
	(RETURN A)))

(DEFUN SORT-GROUPED-ARRAY-QUICK (A L R GS)
       (COND ((> L (- R (* GS SORT-QS-BREAKEVEN)))	;SEE IF SHOULD DO AN INSERTION SORT
	      (DO ((I (+ L GS) (+ I GS)))		;THIS CLAUSE ALSO APPLIES WHEN L>R
		  ((> I R))
		(DO C 0 (1+ C) (= C GS)	;COPY GUY OUT
		    (AS-1 (AR-1 A (+ I C))
			  SORT-ARRAY-TEMP-V 
			  C))
		(DO ((J (- I GS) (- J GS))
		     (X (AR-1 A I)))
		    ((OR (< J L) (NOT (FUNCALL SORT-LESSP-PREDICATE X (AR-1 A J))))
		     (DO C 0 (1+ C) (= C GS)		;ON EXIT, STICK THAT ENTRY
			 (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)  ;BACK IN
			       A 
			       (+ J GS C))))
		    (DO C 0 (1+ C) (= C GS)
			(AS-1 (AR-1 A (+ C J)) A (+ J GS C))))))
	     (T ((LAMBDA (N)				;RANDOMLY CHOSEN POINT BETWEEN L AND R
			 ((LAMBDA (M)			;BREAK-POINT BETWEEN LOW AND HIGH
				  (SORT-GROUPED-ARRAY-QUICK A L (- M GS) GS)
							;SORT THE LOW ELEMENTS
				  (SORT-GROUPED-ARRAY-QUICK A (+ M GS) R GS))
							;SORT THE HIGH ELEMENTS
			  (DO ((K (PROG1 (AR-1 A N)	;K WILL BE M'TH ELEMENT
					 (DO C 0 (1+ C) (= C GS)
					     (AS-1 (AR-1 A (+ N C))   ;SAVE N IN TEMP
						   SORT-ARRAY-TEMP-V 
						   C)
					     (AS-1 (AR-1 A (+ L C)) A (+ N C))))) ;PUT
								;L WHERE N WAS
			       (I L)			;A[...I-1] < K
			       (J R))			;K < A[J+1...]
			      (NIL)
			   DECRJ			;DECREASE J UNTIL K NOT LT A[J]
			    (COND ((= J I)
				   (DO C 0 (1+ C) (= C GS)
				       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
					     A 
					     (+ I C)))
				   (RETURN I))
				  ((FUNCALL SORT-LESSP-PREDICATE K (AR-1 A J))
				   (SETQ J (- J GS))
				   (GO DECRJ)))
			    (DO C 0 (1+ C) (= C GS)
				(AS-1 (AR-1 A (+ J C)) A (+ I C)))
			    (SETQ I (+ I GS))
			   INCRI			;INCREASE I UNTIL K NOT GT A[I]
			    (COND ((= I J)
				   (DO C 0 (1+ C) (= C GS)
				       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
					     A 
					     (+ J C)))
				   (RETURN J))
				  ((FUNCALL SORT-LESSP-PREDICATE (AR-1 A I) K)
				   (SETQ I (+ I GS))
				   (GO INCRI)))
			    (DO C 0 (1+ C) (= C GS)
				(AS-1 (AR-1 A (+ I C)) A (+ J C)))
			    (SETQ J (- J GS)))))
		 ;(+ L (RANDOM (+ (- R L) 1)))
		 (+ L (* GS (// (// (- R L) 2) GS)))	;USE THIS UNTIL HAVE RANDOM FUNCTION
		 				;MAKE SURE RESULT IS A MULTIPLE OF GS
))))

;SORT-LESSP-PREDICATE HERE MUST BE A FUNCTION OF FOUR ARGS,
; <ARRAY1> <IDX1> <ARRAY2> <IDX2>.
(DEFUN SORT-GROUPED-ARRAY-GROUP-KEY (A GS SORT-LESSP-PREDICATE)
  (PROG (SORT-ARRAY-TEMP-V)
	(SETQ SORT-ARRAY-TEMP-V (MAKE-ARRAY NIL 'ART-Q GS))
	(SORT-GROUPED-ARRAY-GROUP-KEY-QUICK A 0 (- (ARRAY-ACTIVE-LENGTH A) GS) GS)
	(RETURN-ARRAY SORT-ARRAY-TEMP-V)
	(RETURN A)))

(DEFUN SORT-GROUPED-ARRAY-GROUP-KEY-QUICK (A L R GS)
       (COND ((> L (- R (* GS SORT-QS-BREAKEVEN)))	;SEE IF SHOULD DO AN INSERTION SORT
	      (DO ((I (+ L GS) (+ I GS)))		;THIS CLAUSE ALSO APPLIES WHEN L>R
		  ((> I R))
		(DO C 0 (1+ C) (= C GS)	;COPY GUY OUT
		    (AS-1 (AR-1 A (+ I C))
			  SORT-ARRAY-TEMP-V 
			  C))
		(DO ((J (- I GS) (- J GS)))
		    ((OR (< J L) (NOT (FUNCALL SORT-LESSP-PREDICATE SORT-ARRAY-TEMP-V 0 A J)))
		     (DO C 0 (1+ C) (= C GS)		;ON EXIT, STICK THAT ENTRY
			 (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)  ;BACK IN
			       A 
			       (+ J GS C))))
		    (DO C 0 (1+ C) (= C GS)
			(AS-1 (AR-1 A (+ C J)) A (+ J GS C))))))
	     (T ((LAMBDA (N)				;RANDOMLY CHOSEN POINT BETWEEN L AND R
			 ((LAMBDA (M)			;BREAK-POINT BETWEEN LOW AND HIGH
				  (SORT-GROUPED-ARRAY-GROUP-KEY-QUICK A L (- M GS) GS)
							;SORT THE LOW ELEMENTS
				  (SORT-GROUPED-ARRAY-GROUP-KEY-QUICK A (+ M GS) R GS))
							;SORT THE HIGH ELEMENTS
			  (DO ((K ;K WILL BE M'TH ELEMENT - K NOT USED IN THIS VERSION OF CODE
				  ; INSTEAD USE ARRAY SORT-ARRAY-TEMP-V, STARTING AT ELEMENT 0
				 (DO C 0 (1+ C) (= C GS)
                                     (AS-1 (AR-1 A (+ N C))	;SAVE N IN TEMP
                                           SORT-ARRAY-TEMP-V 
                                           C)
                                     (AS-1 (AR-1 A (+ L C)) A (+ N C)))) ;PUT L WHERE N WAS
			       (I L)			;A[...I-1] < K
			       (J R))			;K < A[J+1...]
			      (NIL)
			   DECRJ			;DECREASE J UNTIL K NOT LT A[J]
			    (COND ((= J I)
				   (DO C 0 (1+ C) (= C GS)
				       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
					     A 
					     (+ I C)))
				   (RETURN I))
				  ((FUNCALL SORT-LESSP-PREDICATE SORT-ARRAY-TEMP-V 0 A J)
				   (SETQ J (- J GS))
				   (GO DECRJ)))
			    (DO C 0 (1+ C) (= C GS)
				(AS-1 (AR-1 A (+ J C)) A (+ I C)))
			    (SETQ I (+ I GS))
			   INCRI			;INCREASE I UNTIL K NOT GT A[I]
			    (COND ((= I J)
				   (DO C 0 (1+ C) (= C GS)
				       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
					     A 
					     (+ J C)))
				   (RETURN J))
				  ((FUNCALL SORT-LESSP-PREDICATE A I SORT-ARRAY-TEMP-V 0)
				   (SETQ I (+ I GS))
				   (GO INCRI)))
			    (DO C 0 (1+ C) (= C GS)
				(AS-1 (AR-1 A (+ I C)) A (+ J C)))
			    (SETQ J (- J GS)))))
		 ;(+ L (RANDOM (+ (- R L) 1)))
		 (+ L (* GS (// (// (- R L) 2) GS)))	;USE THIS UNTIL HAVE RANDOM FUNCTION
		 				;MAKE SURE RESULT IS A MULTIPLE OF GS
))))
