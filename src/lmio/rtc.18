;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS -*-

;;; TO USE THIS STUFF:
;;; 1) START UP A QCMP
;;; 2) LOAD THIS
;;;    Random flags you should know about:
;;;     VERBOSE-FLAG this is initially set to T, it will print a blow by blow account
;;;      of what the readtable compiler is doing.  Set it to NIL if you don't care
;;;      to see all this cruft.
;;;     COLD-FLAG this is initially set to T, it will make a QFASL file suitable for the
;;;      cold load.  Set this to NIL if you are making a home-brew readtable (you will
;;;      sill have to load the result into SI however (sigh....))
;;; 3) INCANT "(RTC-FILE <name of file to compile>)"

(DECLARE (COND ((STATUS FEATURE MACLISP)
		(LOAD '|AI:ALAN;LSPENV FASL|)
		(LOAD '|AI:ALAN;STRUCT FASL|)
		(LOAD '|AI:LMIO;RTCMAC >|))
	       ((STATUS FEATURE LISPM))
	       (T (ERROR '|You are compiling this file with the wrong compiler! (RTC)|))))

(DECLARE
 (SPECIAL ++		  ;; These are analogous to + and *, and are used for
	  **		  ;; debugging by the functions RUN and DEBUG.
	  ACCEPT-TYPE	  ;; A symbol. While we are compileing (DEF FOO ...)
			  ;;   this will be bound to FOO.
	  DEBUG-FLAG	  ;; T => Print great gobs of cruft.  Usually NIL.
	  VERBOSE-FLAG	  ;; T => Print small gobs of cruft.  Usually T.
	  COLD-FLAG	  ;; T => Generate readtable for cold load. Usually T.
	  ))

(IF-FOR-MACLISP

;;; Ignore :'s for the moment.
(SETSYNTAX '/: (STATUS SYNTAX | |) NIL)

(DECLARE (SPECIAL FASL-OP-INITIALIZE-ARRAY FASL-OP-INITIALIZE-NUMERIC-ARRAY FASL-OP-ARRAY
		  FASL-OP-STOREIN-ARRAY-LEADER
		  FASL-OP-TEMP-LIST FASL-OP-STOREIN-SYMBOL-VALUE))

(DECLARE (*LEXPR REPORT WARNING))
)

(SETQ ACCEPT-TYPE 'RTC		;This symbol is stuffed into STATE-ACCEPTP.
      DEBUG-FLAG NIL
      VERBOSE-FLAG T
      COLD-FLAG T
      )

;;; Structure definitions and other macros:

;;; A SET means a list with no duplicates.  If something is described as
;;; being a set, then it may be crucial that there are no duplicates.

;;; The fundamental data structure is a finite state machine (FSM).
;;; It has a finite number of states.  A BUCKET is a set of characters
;;; (e.g. break characters, alphabetic characters, etc).  In traversing
;;; the FSM, you are in some state, and you get a character which is a member
;;; of some bucket.  You look up the bucket in the STATE-ALIST of the current
;;; state to find the next state.  (Things are more complicated when non-deterministic
;;; FSMs are involved, but this is the general idea.)


;;; FSM-START is a set of starting states.  (The reason there can be more
;;; than one starting state is that the FSM is non-determinsitic at some
;;; points during the compilation.)  This must be a SET, i.e. a list with
;;; no duplicates.
;;; FSM-BUCKETS is the set of all buckets associated with the FSM.
;;; Buckets are described below.
;;; FSM-STATES is the set of all states in the FSM.
;;; FSM-OPTIMIZED is a flag; if T, the FSM has been optimized.
(DEFSTRUCT (FSM (DEFAULT-POINTER FSM))
	   FSM-START			;This must be a set
	   FSM-BUCKETS			;Ditto
	   FSM-STATES			;Ditto ditto
	   FSM-OPTIMIZED		;We can save some effort this way.
	   )

;;; STATE-ACCEPTP If NIL, this is not an accept state.  Otherwise,
;;; it is an ACCEPT-TYPE symbol (such as FIXNUM, STRING, etc.)
;;; The symbol FLUSH is special to some algorithms, particularly
;;; the determinizer (q.v.).
;;; STATE-ALIST associates buckets with states; it is of the form
;;;  ((<bucket> . <SET of states>) ...)  Note that the CDR of an element
;;; is a set.  The conses of this list and the conses which are the elements
;;; of the alist are not shared with any other list, so it is OK to RPLAC any
;;; of them.  No bucket may appear more than once in the alist.
;;; STATE-FROMS is a list (which may contain duplicates) of all states
;;; whose STATE-ALIST points at this state.
;;; The next two components are used for various different things by
;;; different parts of the RTC.  STATE-MARK is usually T or NIL, to
;;; indicate that we have or have not processed this state in some way.
;;; STATE-LINK is usually some other state.

(DEFSTRUCT (STATE (DEFAULT-POINTER STATE))
	   STATE-ACCEPTP		;Is this an accept state?
	   STATE-ALIST			;ALIST is PART of the state. It CAN be bashed.
	   STATE-FROMS			;This is not a set, might contain duplicates.
	   STATE-MARK			;Random usefull info stored here. (GC etc.)
	   STATE-LINK)			;Related states etc.
;; An alist is of the form ((<bucket> . <SET of states>) ...). No bucket may appear more
;; than once and each set of states MUST be a set.

;;; BUCKET-CHARS is a list (may contain duplicates) of the characters of this
;;; bucket.
;;; BUCKET-LINK, like STATE-LINK, is used for various things; it is usually
;;; a bucket.
(DEFSTRUCT (BUCKET (DEFAULT-POINTER BUCKET))
	   BUCKET-CHARS			;Not a set
	   BUCKET-LINK)			;Related buckets etc.

(IF-FOR-MACLISP
;;; This next stuff is necessary cause QCMP runs in an out-of-date MacLisp.
;;; Also you probably wouldn't believe what a DEFMACRO expands into these days!
(DEFUN LIST* N
    (DO ((L (ARG N) (CONS (ARG I) L))
	 (I (1- N) (1- I)))
	((ZEROP I) L)))
(DEFPROP DEFMACRO ((DSK LISP) DEFMACRO FASL) AUTOLOAD)
(DEFMACRO FSM-P (X) `(HUNKP ,X))
)
(IF-FOR-LISPM
(DEFMACRO FSM-P (X) `(ARRAYP ,X))
)

;;; Handy mapping macros:

(DEFMACRO MAPCV (VAR STUFF &REST BODY)
	    `(DO ,VAR ,STUFF (CDR ,VAR) (NULL ,VAR)
		 ((LAMBDA (,VAR) . ,BODY)
		  (CAR ,VAR))))

(DEFMACRO MAPCARV (VAR STUFF &REST BODY)
	    `(DO ((,VAR ,STUFF (CDR ,VAR))
		  (**ANSWER** NIL (CONS ((LAMBDA (,VAR) . ,BODY)
					 (CAR ,VAR))
					**ANSWER**)))
		 ((NULL ,VAR) (NREVERSE **ANSWER**))))

;;; In general, any function that manipulates or computes upon a FSM
;;; must be assumed to bash it!  Take care to make copies whenever
;;; neccesary if you need to save anything.  Usually you don't.

;;; Next two functions are ways to create primative FSMs.

;;; Takes a list of chars.  Makes a bucket of them, and makes
;;; and returns a 2-state FSM.  One of the states will go
;;; to the other given a char from BUCKET; the former will
;;; be the only start state.  Thus, this is the FSM that accepts
;;; any one character string whose character is in CHARS.
(DEFUN FSM-FROM-LIST (CHARS)
       (LET ((BUCKET (MAKE-BUCKET BUCKET-CHARS CHARS))
	     (STATE1 (MAKE-STATE))
	     (STATE2 (MAKE-STATE STATE-ACCEPTP ACCEPT-TYPE)))
	    (SETF (STATE-ALIST STATE1)
		  (LIST (CONS BUCKET (LIST STATE2))))
	    (SETF (STATE-FROMS STATE2)
		  (LIST STATE1))
	    (MAKE-FSM FSM-STATES (LIST STATE1 STATE2)
		      FSM-BUCKETS (LIST BUCKET)
		      FSM-START (LIST STATE1))))

;;; Create and return a FSM with one state and no buckets.
;;; This is the FSM that accepts the null string.
(DEFUN MAKE-NULL-FSM ()
       (LET ((STATE (MAKE-STATE STATE-ACCEPTP ACCEPT-TYPE)))
	    (MAKE-FSM FSM-STATES (LIST STATE)
		      FSM-START (LIST STATE))))

;;; Given an FSM, return a copy.  Copies the FSM itself, and all buckets,
;;; states, and state-alists.
(DEFUN COPY-FSM (FSM)
       (LET ((NSTATES
	      (MAPCARV STATE (FSM-STATES)
		       (LET ((NS (MAKE-STATE STATE-ACCEPTP (STATE-ACCEPTP))))
			    (SETF (STATE-LINK) NS)
			    NS)))
	     (NBUCKETS
	      (MAPCARV BUCKET (FSM-BUCKETS)
		       (LET ((NB (MAKE-BUCKET BUCKET-CHARS (BUCKET-CHARS))))
			    (SETF (BUCKET-LINK) NB)
			    NB))))
	    (MAPCV STATE (FSM-STATES)
		   (DO ((L (STATE-ALIST) (CDR L))
			(STATE1 (STATE-LINK))
			(A NIL (CONS (CONS (BUCKET-LINK (CAAR L))
					   (DO ((L (CDAR L) (CDR L))
						(ANS)
						(NS))
					       ((NULL L) ANS)
					       (SETQ NS (STATE-LINK (CAR L)))
					       (PUSH NS ANS)
					       (PUSH STATE1 (STATE-FROMS NS))))
				     A)))
		       ((NULL L)
			(SETF (STATE-ALIST STATE1) A))))
	    (MAKE-FSM FSM-OPTIMIZED (FSM-OPTIMIZED)
		      FSM-BUCKETS NBUCKETS
		      FSM-STATES NSTATES
		      FSM-START (MAPCARV STATE (FSM-START) (STATE-LINK)))))

;How to perform primitive operations on FSMs:

;; Note that these operations tend to bash their arguments; this is the reason
;; for COPY-FSM. (The interpreter uses COPY-FSM when someone does a SETQ or causes
;; the evaluation of a symbol whose value is a saved FSM.)

;;; Given a FSM that accepts the set of strings S, returns an FSM
;;; that accepts any concatenation of elements of S.
;;; This works by calling CONC-FSM-1, thus getting something
;;; that will accept the concatenation of ONE or more strings of S.
;;; Then it creates a new state to accept the null string.
(DEFUN STAR-FSM (FSM)
       (LET ((FSM (CONC-FSM-1 FSM FSM))
	     (NSTATE (MAKE-STATE STATE-ACCEPTP ACCEPT-TYPE)))
	    (PUSH NSTATE (FSM-START))
	    (PUSH NSTATE (FSM-STATES))
	    (SETF (FSM-OPTIMIZED) NIL)
	    FSM))

;;; CONC-FSM takes two FSMs (which accept sets of strings S1 and S2),
;;; and returns a new FSM which accepts any concatenation of a string
;;; of S1 with a string of S2.
;;; This works by:
;;;   (1) Find all states of FSM that go to accepts states.
;;;   (2) Make each of these go to each of the start states of FSM1.
;;;   (3) Find all accept states of FSM and make them no longer
;;;       be accept states.
;;; Note that if any of the start states of FSM is also an accept state,
;;; you must include all of FSM1's start states as start states of the resulting
;;; FSM.

;; You must call CONC-FSM if the FSMs are not EQ.
;; You must call CONC-FSM-1 if the FSMs are EQ. (To do a + only)
;; The ONLY exception is where CONC-FSM calls CONC-FSM-1.
(DEFUN CONC-FSM (FSM FSM1)
       (CONC-FSM-1 FSM FSM1)
       (AND (DO L (FSM-START) (CDR L) (NULL L)
		(AND (STATE-ACCEPTP (CAR L))
		     (RETURN T)))
	    (SETF (FSM-START) (UNION-Q (FSM-START FSM1) (FSM-START))))
       (MAPCV STATE (FSM-STATES) (SETF (STATE-ACCEPTP) NIL))
       (SETF (FSM-BUCKETS) (UNION-Q (FSM-BUCKETS FSM1) (FSM-BUCKETS)))
       (SETF (FSM-STATES) (UNION-Q (FSM-STATES FSM1) (FSM-STATES)))
       (SETF (FSM-OPTIMIZED) NIL)
       FSM)

;;; This is the internals of CONC-FSM.  It has the interesting property
;;; that if FSM is eq to FSM1, it will do the "+" operation; that is,
;;; it will return an FSM which accepts the concatenation of one or more 
;;; strings of the set accepted by FSM.  Thus, this little function does
;;; both addition and transitive closures!
(DEFUN CONC-FSM-1 (FSM FSM1)
       (LET ((STARTERS (FSM-START FSM1)))
	    (MAPCV STATE (FSM-STATES) (SETF (STATE-MARK) NIL))
	    (MAPCV STATE (FSM-STATES)
		   (COND ((STATE-ACCEPTP)
			  (MAPCV S (STATE-FROMS)
				 (COND ((NOT (STATE-MARK S))
					(SETF (STATE-MARK S) T)
					(DO L (STATE-ALIST S) (CDR L) (NULL L)
					    (DO LL (CDAR L) (CDR LL) (NULL LL)
						(COND ((STATE-ACCEPTP (CAR LL))
						       (RPLACD (CAR L)
							       (UNION-Q STARTERS (CDAR L)))
						       (MAPCV TS STARTERS
							      (PUSH S (STATE-FROMS TS)))
						       (RETURN NIL)))))))))))
	    (SETF (FSM-OPTIMIZED) NIL)
	    FSM))

;;; Bashes FSM to be the union of FSM and FSM1.  (I.e. "unions" FSM1 into FSM.)
(DEFUN UNION-FSM (FSM FSM1)
       (SETF (FSM-START) (UNION-Q (FSM-START FSM1) (FSM-START)))
       (SETF (FSM-BUCKETS) (UNION-Q (FSM-BUCKETS FSM1) (FSM-BUCKETS)))
       (SETF (FSM-STATES) (UNION-Q (FSM-STATES FSM1) (FSM-STATES)))
       (SETF (FSM-OPTIMIZED) NIL)
       FSM)

;;; This is rather tricky.
;;; Clobbers FSM to accept all of the strings that it used to, EXCEPT those
;;; accepted by FSM1.
(DEFUN DIFFERENCE-FSM (FSM FSM1)
       (LET ((BUCKETS (FSM-BUCKETS))
	     (BUCKETS1 (FSM-BUCKETS FSM1)))
	    ;; If we are performing the subtraction {a,b} - {b,c} (which = {a}),
	    ;; the presence of the character "c" is superfluous and gets in the
	    ;; way.  So this DO form eliminates all such characters.
	    (DO L BUCKETS1 (CDR L) (NULL L)
		(DO ((C (BUCKET-CHARS (CAR L)) (CDR C))
		     (NC NIL (LET ((C (CAR C)))
				  (IF (DO BS BUCKETS (CDR BS) (NULL BS)
					  (AND (MEMBER C (BUCKET-CHARS (CAR BS)))
					       (RETURN T)))
				      (CONS C NC)
				      NC))))
		    ((NULL C)
		     (SETF (BUCKET-CHARS (CAR L)) NC))))
	    ;; The accept states of the subtrahend should not get turned into
	    ;; accept states by the determinizer, so mark them as FLUSH.
	    (MAPCV STATE (FSM-STATES FSM1)
		   (AND (STATE-ACCEPTP)
			(SETF (STATE-ACCEPTP) 'FLUSH)))
	    ;; Finally, call the optimizer.
	    (OPTIMIZE-FSM-1 (UNION-FSM FSM FSM1))))

;Optimization:
;;; This is the normal version of the optimizer, used when there
;;; are neither FLUSH states nor empty buckets in the FSM.
(DEFUN OPTIMIZE-FSM (FSM)
       (COND ((FSM-OPTIMIZED)
	      (REPORT "Already optimized"))
	     (T
	      (REPORT "Being optimized")
	      (AND DEBUG-FLAG (PRINT-FSM FSM))
	      (REPORT "GCing")
	      (SETQ FSM (GC-FSM FSM))
	      (AND DEBUG-FLAG (PRINT-FSM FSM))
	      (REPORT "Partitioning buckets")
	      (SETQ FSM (PARTITION-BUCKETS-FSM FSM))
	      (AND DEBUG-FLAG (PRINT-FSM FSM))
	      (REPORT "Making it deterministic")
	      (SETQ FSM (DETERMINE-IZE-FSM FSM))
	      (AND DEBUG-FLAG (PRINT-FSM FSM))
	      (REPORT "Removing equivalent states")
	      (SETQ FSM (REDUCE-FSM FSM))
	      (SETF (FSM-OPTIMIZED) T)
	      (REPORT "Done.")))
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       FSM)

;;; This version should be called if FSM may contain FLUSH states,
;;; or when there may be empty buckets. (both of which may happen when you do
;;; a subtraction).  This will GC a second time, and will GC buckets.
(DEFUN OPTIMIZE-FSM-1 (FSM)
       (REPORT "Being optimized")
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       (REPORT "GCing first time")
       (SETQ FSM (GC-FSM FSM))
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       (REPORT "Partitioning buckets")
       (SETQ FSM (PARTITION-BUCKETS-FSM FSM))
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       (REPORT "Making it deterministic")
       (SETQ FSM (DETERMINE-IZE-FSM FSM))
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       (REPORT "GCing second time")
       (SETQ FSM (GC-FSM FSM))
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       (REPORT "GCing buckets")
       (SETQ FSM (GC-FSM-BUCKETS FSM))
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       (REPORT "Removing equivalent states")
       (SETQ FSM (REDUCE-FSM FSM))
       (SETF (FSM-OPTIMIZED) T)
       (REPORT "Done.")
       (AND DEBUG-FLAG (PRINT-FSM FSM))
       FSM)

;; Note that each optimization (just like the primitive operations) is allowed
;; to destroy all of its arguments.

;; Search out and remove useless states:
;;; The useful states are those which (a) can get to an accept state,
;;; AND (b) can be gotten to from a start state.  The algorithm is
;;; to first find all of the (a) states, and mark them with an S; then,
;;; find all of the (b) states, and each time you find one if it is
;;; marked with an S, mark it with a T.  Finally, get rid of anything
;;; that is not marked with a T.
(DEFUN GC-FSM (FSM)
       (MAPCV STATE (FSM-STATES)  ;Clear the MARK bits.
	      (SETF (STATE-MARK) NIL))
       (MAPCV STATE (FSM-STATES)
	      (AND (STATE-ACCEPTP) (GC-FSM-MARK-BACKWARDS STATE)))
       (MAPC (FUNCTION GC-FSM-MARK-FOWARDS) (FSM-START))
       ;; At this point, all good states are marked with a T.
       ;;; Remove useless states from the FSM-START, FSM-STATES, the
       ;;; STATE-FROMSs, and all of
       ;;; the ALISTS.  Furthermore, if any alist element is left with no states,
       ;;; flush the element entirely.
       (SETF (FSM-START) (GC-FSM-FLUSH (FSM-START)))
       (SETF (FSM-STATES) (GC-FSM-FLUSH (FSM-STATES)))
       (MAPCV STATE (FSM-STATES)
	      (DO L (STATE-ALIST) (CDR L) (NULL L)
		  (LET ((N (GC-FSM-FLUSH (CDAR L))))
		       (IF (NULL N)
			   (SETF (STATE-ALIST) (DELQ (CAR L) (STATE-ALIST)))
			   (RPLACD (CAR L) N))))
	      (SETF (STATE-FROMS) (GC-FSM-FLUSH (STATE-FROMS))))
       FSM)

;; Mark phase:
;;; Find all states that can get to STATE, and mark with an S.
;;; Also mark STATE with an S.
(DEFUN GC-FSM-MARK-BACKWARDS (STATE)
       (COND ((NOT (STATE-MARK))
	      (SETF (STATE-MARK) 'S)
	      (MAPC (FUNCTION GC-FSM-MARK-BACKWARDS) (STATE-FROMS)))))

;;; Find all states reachable from STATE (and STATE itself), and if
;;; the state is marked with an S, mark it with a T.
(DEFUN GC-FSM-MARK-FOWARDS (STATE)
       (COND ((EQ 'S (STATE-MARK))
	      (SETF (STATE-MARK) 'T)
	      (DO L (STATE-ALIST) (CDR L) (NULL L)
		  (MAPC (FUNCTION GC-FSM-MARK-FOWARDS) (CDAR L))))))

;;; Flush states not marked with a T from a list (possibly a set).
(DEFUN GC-FSM-FLUSH (OL)
       (DO ((OL OL (CDR OL))
	    (NL NIL (IF (EQ (STATE-MARK (CAR OL)) 'T)
			(CONS (CAR OL) NL)
			NL)))
	   ((NULL OL) NL)))

;;; Look for buckets that are not pointed to by any ALIST.
;;; BUCKET-LINK in here is used as a mark bit.
(DEFUN GC-FSM-BUCKETS (FSM)
       (MAPCV BUCKET (FSM-BUCKETS) (SETF (BUCKET-LINK) NIL))
       (MAPCV STATE (FSM-STATES)
	      (DO A (STATE-ALIST) (CDR A) (NULL A)
		  (SETF (BUCKET-LINK (CAAR A)) T)))
       (DO ((OL (FSM-BUCKETS) (CDR OL))
	    (NL NIL (IF (BUCKET-LINK (CAR OL))
			(CONS (CAR OL) NL)
			NL)))
	   ((NULL OL)
	    (SETF (FSM-BUCKETS) NL)))
       FSM)

;;; Takes an FSM and bashes it so that all of the buckets are disjoint sets.
;;; This is the hairiest and slowest part of the whole program.
(DEFUN PARTITION-BUCKETS-FSM (FSM)
       ;; This is the hard part; see comment in front of its definition.
       (PARTITION-BUCKET-LIST (FSM-BUCKETS))
       ;; Make the BUCKET-LINK of each bucket be the list of "leaf" buckets
       ;; that comprise it.
       (MAPCV BUCKET (FSM-BUCKETS)
	      (SETF (BUCKET-LINK)
		    (IF (NULL (BUCKET-CHARS))
			NIL
			(CHASE-BUCKET-LINKS BUCKET))))
       ;; Union together all "leaves", creating the new FSM-BUCKETS list.
       (SETF (FSM-BUCKETS)
	     (DO ((L (FSM-BUCKETS) (CDR L))
		  (NL NIL (UNION-Q (BUCKET-LINK (CAR L)) NL)))
		 ((NULL L) NL)))
       ;; Go over all the alists, and fix them up to use the new buckets.
       (MAPCV STATE (FSM-STATES)
	      (DO ((L (STATE-ALIST) (CDR L))
		   (TEM)
		   (NL))
		  ((NULL L)
		   (SETF (STATE-ALIST) NL))
		  (AND (NOT (NULL (SETQ TEM (BUCKET-LINK (CAAR L)))))
		       (DO ((BS TEM (CDR BS))
			    (STS (CDAR L)))
			   ((NULL BS))
			   (COND ((SETQ TEM (ASSQ (CAR BS) NL))
				  (RPLACD TEM (UNION-Q STS (CDR TEM))))
				 (T (SETQ NL (CONS (CONS (CAR BS) STS) NL))))))))
       ;; Rebuild the STATE-FROMS lists.
       ;; It is not clear that this is neccesary, but it can't hurt  -- ALAN, DLW 7/21/78
       (MAPCV STATE (FSM-STATES) (SETF (STATE-FROMS) NIL))
       (MAPCV STATE (FSM-STATES)
	      (DO A (STATE-ALIST) (CDR A) (NULL A)
		  (DO S (CDAR A) (CDR S) (NULL S)
		      (PUSH STATE (STATE-FROMS (CAR S))))))
       FSM)

;; Chase those bucket links!
;;; Takes a bucket, and returns the fringe of its BUCKET-LINK tree (see 
;;; the comments for PARTITION-BUCKET-LIST, below).
(DEFUN CHASE-BUCKET-LINKS (BUCKET)
       (LET ((L (BUCKET-LINK)))
	    (IF (NULL L)
		(LIST BUCKET)
		(DO ((L L (CDR L))
		     (NBS NIL (NCONC (CHASE-BUCKET-LINKS (CAR L)) NBS)))   ;NCONC works here
		    ((NULL L) NBS)))))

;;; This recursive function is the guts of PARTITION-BUCKET-LIST.
;;; This takes a list of buckets.  Sets up the BUCKET-LINKs of each bucket
;;; to be a set of buckets.  For any bucket, the union of the set of
;;; buckets in its BUCKET-LINK is the bucket itself.
;;; These buckets that are in the BUCKET-LINKs may themselves have BUCKET-LINKs
;;; which are a further partitioning; thus, there is a tree of buckets, the
;;; fringe of which is the final partitioning of each bucket.
;;; A terminal bucket is one whose BUCKET-LINK is NIL.
;;; Note: This makes sure that there are no empty buckets.
(DEFUN PARTITION-BUCKET-LIST (BUCKETS)
       (COND ((NULL BUCKETS) NIL)
	     (T
	      (SETF (BUCKET-LINK (CAR BUCKETS)) NIL)
	      (DO ((BUCKET (CAR BUCKETS))
		   (SET (APPEND (BUCKET-CHARS (CAR BUCKETS)) NIL))
		   (BUCKET-LIST (PARTITION-BUCKET-LIST (CDR BUCKETS))
				(CDR BUCKET-LIST))
		   (NEW-BUCKET-LIST NIL))
		  ((NULL BUCKET-LIST)
		   (AND SET
			(LET ((NB (MAKE-BUCKET BUCKET-CHARS SET)))
			     (PUSH NB (BUCKET-LINK BUCKET))
			     (PUSH NB NEW-BUCKET-LIST)))
		   NEW-BUCKET-LIST)
		  (DO ((P-SET (BUCKET-CHARS (CAR BUCKET-LIST))
			      (CDR P-SET))
		       (OLD-BUCKET (CAR BUCKET-LIST))
		       (IN)
		       (OUT))
		      ((NULL P-SET)
		       (AND IN (LET ((NB (MAKE-BUCKET BUCKET-CHARS IN)))
				    (SETF (BUCKET-LINK OLD-BUCKET) (LIST NB))
				    (PUSH NB (BUCKET-LINK BUCKET))
				    (PUSH NB NEW-BUCKET-LIST)))
		       (AND OUT (LET ((NB (MAKE-BUCKET BUCKET-CHARS OUT)))
				     (PUSH NB (BUCKET-LINK OLD-BUCKET))
				     (PUSH NB NEW-BUCKET-LIST))))
		      (IF (MEMBER (CAR P-SET) SET)
			  (PROGN (PUSH (CAR P-SET) IN)
				 (SETQ SET (DELETE (CAR P-SET) SET)))
			  (PUSH (CAR P-SET) OUT)))))))

;;; Construct a new FSM that is deterministic.
;;; The STATE-LINK of each state of the new FSM is the set of old states
;;; that got merged to form this one.

;;; Start by merging all of the start states into one new state (this is ST-STATE).

;;; Next we must "process" every state of the new FSM.  One state
;;; is processed every time around the main DO loop.
;;; OSTATES is a list of already-processed states (initially NIL, we haven't
;;; processed anything.
;;; NSTATES is a list of states to be processed on later iterations (initially NIL).
;;; STATE is the state being processed.
;;; When we are done processing, put STATE onto OSTATES.  While processing,
;;; more states are created and put onto NSTATES.  When NSTATES is finally empty,
;;; we are all done.

;;; How to "process" STATE:
;;; F-STATES is the set of states of the OLD FSM that make up STATE.
;;; Iterate over all buckets in the FSM:
;;; (We want to figure out what STATE does when given a character from
;;;  BUCKET).
;;;    The inner DO-loop constructs the list A, which is the set of
;;;    all states such that one of the F-STATES goes to that state when
;;;    given a character from BUCKET.
;;;    If A is NIL, we don't go anywhere with this bucket; do nothing.
;;;    If A = F-STATES, then we go to ourself; add a transition.
;;;    If one of the OSTATES or NSTATES has A as its STATE-LINK,
;;;      then we go to that state; add a transition.
;;;    Else, make a new state, add a transition to it, and push it on NSTATES.

(DEFUN DETERMINE-IZE-FSM (FSM)
       (LET ((ST-STATE (MAKE-STATE STATE-ACCEPTP (DETERMINE-ACCEPT-NESS (FSM-START))
				   STATE-LINK (FSM-START)))
	     (BUCKETS (FSM-BUCKETS))
	     (F-STATES))
       (DO ((OSTATES NIL (CONS STATE OSTATES))
	    (STATE ST-STATE (CAR NSTATES))
	    (NSTATES NIL (CDR NSTATES)))
	   (NIL)					;wrong place for the test
	   (SETQ F-STATES (STATE-LINK))			;the set of states this one represents
	   (MAPCV BUCKET BUCKETS
		  (DO ((L F-STATES (CDR L))
		       (TEM)
		       (A NIL (IF (SETQ TEM (ASSQ BUCKET (STATE-ALIST (CAR L))))
				  (UNION-Q (CDR TEM) A)
				  A)))
		      ((NULL L)
		       (COND ((NULL A))
			     ((EQUAL-SET-Q A F-STATES)
			      (ADD-TRANSITION STATE STATE BUCKET))
			     ((SETQ TEM (FIND-STATE-WITH-SAME-SET A OSTATES))
			      (ADD-TRANSITION STATE TEM BUCKET))
			     ((SETQ TEM (FIND-STATE-WITH-SAME-SET A NSTATES))
			      (ADD-TRANSITION STATE TEM BUCKET))
			     (T
			      (SETQ TEM
				    (MAKE-STATE STATE-ACCEPTP (DETERMINE-ACCEPT-NESS A)
						STATE-LINK A))
			      (ADD-TRANSITION STATE TEM BUCKET)
			      (PUSH TEM NSTATES))))))
	   (COND ((NULL NSTATES)			;end-stuff here
		  (RETURN (MAKE-FSM FSM-STATES (CONS STATE OSTATES)
				    FSM-BUCKETS BUCKETS
				    FSM-START (LIST ST-STATE))))))))

;;; Add the knowledge that if you are in FROM state and get a character
;;; from BUCKET, then you should go to state TO.  Be careful to maintain
;;; the back-pointers and to not put duplicate elements into the alists.
(DEFUN ADD-TRANSITION (FROM TO BUCKET)
       (LET ((V (ASSQ BUCKET (STATE-ALIST FROM))))
	    (COND ((NULL V)
		   (PUSH (CONS BUCKET (LIST TO)) (STATE-ALIST FROM)))
		  ((NOT (MEMQ TO (CDR V)))
		   (RPLACD V (CONS TO (CDR V)))))
	    (PUSH FROM (STATE-FROMS TO))))

;;; Search through STATES, looking for one whose STATE-LINK is SET.
(DEFUN FIND-STATE-WITH-SAME-SET (SET STATES)
       (DO STATES STATES (CDR STATES) (NULL STATES)
	   (AND (EQUAL-SET-Q SET (STATE-LINK (CAR STATES)))
		(RETURN (CAR STATES)))))

;;; Given a set of states, determine if they represent an accept state. If so,
;;; then determine what flavor of accept state. Also detect errors of ambiguity.
;;; If any state's STATE-ACCEPTP is FLUSH, then return NIL.
;;; If there is more ACCEPT-TYPE among the states, it is an "ambiguity",
;;; resolved by the RTC-ORDER properties.
(DEFUN DETERMINE-ACCEPT-NESS (STATES)
       (DO A STATES (CDR A) (NULL A)
	   (LET ((AP (STATE-ACCEPTP (CAR A))))
		(AND (EQ AP 'FLUSH) (RETURN NIL))
		(AND (NOT (NULL AP))
		     (RETURN
		      (DO ((L (CDR A) (CDR L))
			   (LP))
			  ((NULL L) AP)
			  (SETQ LP (STATE-ACCEPTP (CAR L)))
			  (AND (EQ LP 'FLUSH) (RETURN NIL))
			  (AND (NOT (NULL LP))
			       (NOT (EQ AP LP))
			       (PROGN (REPORT "Ambiguity: " AP " and " LP )
				      (COND ((MEMQ AP (GET LP 'RTC-ORDER)))
					    ((MEMQ LP (GET AP 'RTC-ORDER))
					     (SETQ AP LP))
					    (T (ERROR "-- Ambiguity." (LIST AP LP))))
				      (REPORT "Resolved in favor of: " AP)))))))))

;;; Takes a deterministic FSM, and removes equivalent states.
;;; Get the list of equivalence sets (LIST-EQUAL-STATES-FSM, q.v.).
;;; First, pick the first state in each set, and make the STATE-LINKs
;;; of all members of the set point to it (it is the "model").
;;; Now collapse the FSM down to the model states, keeping
;;; the alists and back-pointers and so on straight.
(DEFUN REDUCE-FSM (FSM)
       (MAPCV STATE (FSM-STATES) (SETF (STATE-LINK) NIL))
       (DO E (LIST-EQUAL-STATES-FSM FSM) (CDR E) (NULL E)
	   (DO ((MODEL (CAAR E))
		(REST (CAR E) (CDR REST)))
	       ((NULL REST))
	       (SETF (STATE-LINK (CAR REST)) MODEL)))
       (DO ((STATES (FSM-STATES) (CDR STATES))
	    (STATE)
	    (NSTATES))
	   ((NULL STATES)
	    (MAPCV STATE (FSM-STATES)
		   (COND ((STATE-MARK)
			  (SETF (STATE-FROMS (STATE-LINK))
				(DO ((L (STATE-FROMS) (CDR L))
				     (NL (STATE-FROMS (STATE-LINK))
					 (LET ((X (STATE-LINK (CAR L))))
					      (IF (MEMQ X NL)
						  NL
						  (CONS X NL)))))
				    ((NULL L) NL))))))
	    (SETF (FSM-STATES) NSTATES))
	   (SETQ STATE (CAR STATES))
	   (COND ((EQ (STATE-LINK) STATE)
		  (DO A (STATE-ALIST) (CDR A) (NULL A)
		      (RPLACA (CDAR A) (STATE-LINK (CADAR A))))
		  (PUSH STATE NSTATES)
		  (SETF (STATE-FROMS)
			(DO ((L (STATE-FROMS) (CDR L))
			     (NL NIL (LET ((X (STATE-LINK (CAR L))))
					  (IF (MEMQ X NL)
					      NL
					      (CONS X NL)))))
			    ((NULL L) NL)))
		  (SETF (STATE-MARK) NIL))
		 (T (SETF (STATE-MARK) T))))
       (SETF (FSM-START) (LIST (STATE-LINK (CAR (FSM-START)))))
       FSM)

;;; Get the suspected-equal states, and return the really-equal states.
;;; SUSPECTED-EQUAL-STATES-FSM partitions the states into
;;; sets of states such that if two states are in different
;;; sets, they must be different states.

;;; Each time around the main DO-loop, we make this partitioning finer
;;; and finer, until it stops getting any finer (the length of OSUSP
;;; = the length of NSUSP); then we know that all the states in each
;;; set are the same.

;;; We make it finer by noticing that IF, for some bucket, two states of a set
;;; go to two states which are in DIFFERENT sets, then the two states must
;;; be different.
(DEFUN LIST-EQUAL-STATES-FSM (FSM)
       (DO ((OSUSP (SUSPECTED-EQUAL-STATES-FSM FSM) NSUSP)
	    (NSUSP NIL NIL))
	   (NIL)
	   (DO S OSUSP (CDR S) (NULL S)
	       (DO ((MODEL-ALIST (STATE-ALIST (CAAR S)))
		    (REST (CDAR S) (CDR REST))
		    (IN)
		    (OUT))
		   ((NULL REST)
		    (PUSH (CONS (CAAR S) IN) NSUSP)
		    (AND OUT (PUSH OUT NSUSP)))
		   (DO ((ALIST MODEL-ALIST (CDR ALIST))
			(ALIST1 (STATE-ALIST (CAR REST))))
		       ((NULL ALIST)
			(PUSH (CAR REST) IN))
		       (COND ((NOT (LET ((S1 (CADAR ALIST))
					 (S2 (CADR (ASSQ (CAAR ALIST) ALIST1))))
					(DO ((C OSUSP (CDR C)))
					    ((NULL C) (ERROR "Can't find states."))
					    (COND ((MEMQ S1 (CAR C))
						   (RETURN (MEMQ S2 (CAR C))))
						  ((MEMQ S2 (CAR C))
						   (RETURN NIL))))))
			      (PUSH (CAR REST) OUT)
			      (RETURN NIL))))))	   
	   (AND (= (LENGTH OSUSP) (LENGTH NSUSP))
		(RETURN NSUSP))))

;;; Cons up a list of sets of states where each set is suspected
;;; to consist of equivalent states.  Do this by checking to see if states
;;; have the same buckets in the alist and by checking their acceptness.
(DEFUN SUSPECTED-EQUAL-STATES-FSM (FSM)
       (DO ((LST)
	    (STATES (FSM-STATES) (CDR STATES)))
	   ((NULL STATES) LST)
	   (DO ((ALIST (STATE-ALIST (CAR STATES)))
		(ACCP (STATE-ACCEPTP (CAR STATES)))
		(STATE (CAR STATES))
		(LS LST (CDR LS)))
	       ((NULL LS) (PUSH (LIST STATE) LST))
	       (AND (LET ((ACCP1 (STATE-ACCEPTP (CAAR LS)))
			  (ALIST1 (STATE-ALIST (CAAR LS))))
			 (AND (EQ ACCP ACCP1)
			      (= (LENGTH ALIST)
				 (LENGTH ALIST1))
			      (DO ((A ALIST1 (CDR A)))
				  ((NULL A) T)
				  (OR (ASSQ (CAAR A) ALIST)
				      (RETURN NIL)))))
		    (RETURN (PUSH STATE (CAR LS)))))))

;Interpreter, printer, random debuging aids, etc.:

;;; Format of a regular expression:
;;; A number, meaning just that character.
;;; NIL, meaning the null FSM.
;;; A symbol bound to a FSM, meaning a copy of that FSM.
;;; Any other symbol, meaning the meaning of its value.
;;; A list whose CAR is:
;;;   :/   The FSM that accepts the characters of the CDR of the list.
;;;        (I.e. (:// 101 102 103) is the FSM accepting "A" "B" or "C".)
;;;        As a special crock, (://) means the same as NIL (rather than the
;;;        FSM that doesn't accept anything.)
;;;   :SETQ  Bind the symbol to a copy of the FSM gotten from interpreting
;;;        the next thing (e.g. (:SETQ FOO (:// 101 102 103)) ).
;;;   [For anything else, INTERPRET each element of the CDR, then proceed.]
;;;   :*   Concatenation of zero or more of its one argument.
;;;   :+   Concatenation of one or more of its one argument.
;;;   :!   Concatenation of one from each of its arguments in order (it's a lexpr!).
;;;   :U   Union of its arguments (also a lexpr).
;;;   :-   The difference of its first and second argument
;;;        E.g. (:- (:// 101 102) (:// 102 103))  ==  (:// 101)

;;; Take a regular expression and return an un-optimized FSM for it.
(DEFUN INTERPRET (DEF)
       (COND ((NUMBERP DEF)
	      (FSM-FROM-LIST (NCONS DEF)))
             ((NULL DEF)(MAKE-NULL-FSM))
	     ((NOT (ATOM DEF))
	      (SELECTQ (CAR DEF)
		       ((:// //)
			(IF (NULL (CDR DEF))
			    (MAKE-NULL-FSM)		;Compatibility.(sigh)
			    (FSM-FROM-LIST (CDR DEF))))
		       ((:SETQ SETQ)
			(LET ((FSM (OPTIMIZE-FSM (INTERPRET (CADDR DEF)))))
			     (SET (CADR DEF) (COPY-FSM FSM))
			     FSM))
		       (OTHERWISE
			((LAMBDA (ARGS)
				 (SELECTQ (CAR DEF)
					  ((:* *) (STAR-FSM (CAR ARGS)))
					  ((:+ +) (CONC-FSM-1 (CAR ARGS) (CAR ARGS)))
					  ((:! !) (CONC-FSM-LIST ARGS))
					  ((:U U) (UNION-FSM-LIST ARGS))
					  ((:- -) (DIFFERENCE-FSM (CAR ARGS)(CADR ARGS)))
					  (OTHERWISE
					   (ERROR "Unknown operation."
						  (CAR DEF)))))
			 (MAPCAR (FUNCTION INTERPRET) (CDR DEF))))))
	     ((NOT (AND (SYMBOLP DEF)
			(BOUNDP DEF)))
	      (ERROR "What is this?" DEF))
	     ((FSM-P (SYMEVAL DEF))
	      (COPY-FSM (SYMEVAL DEF)))
	     (T 
	      (INTERPRET (SYMEVAL DEF)))))

(DEFUN CONC-FSM-LIST (LS)
       (COND ((NULL (CDR LS))
	      (CAR LS))
	     (T (CONC-FSM (CAR LS) (CONC-FSM-LIST (CDR LS))))))

(DEFUN UNION-FSM-LIST (LS)
       (COND ((NULL (CDR LS))
	      (CAR LS))
	     (T (UNION-FSM (CAR LS) (UNION-FSM-LIST (CDR LS))))))

(IF-FOR-MACLISP
(DEFUN REPORT N
       (COND ((OR VERBOSE-FLAG DEBUG-FLAG)
	      (TERPRI)(PRIN1 ACCEPT-TYPE)(PRINC ": ")
	      (DO I 1 (1+ I) (> I N)
		  (PRINC (ARG I))))))

(DEFUN WARNING N
       (TERPRI)(PRIN1 ACCEPT-TYPE)(PRINC ": WARNING! ")
       (DO I 1 (1+ I) (> I N)
	   (PRINC (ARG I))))
)

(IF-FOR-LISPM
(DEFUN REPORT (&REST L)
       (COND ((OR VERBOSE-FLAG DEBUG-FLAG)
	      (TERPRI)(PRIN1 ACCEPT-TYPE)(PRINC ": ")
	      (DO L L (CDR L) (NULL L)
		  (PRINC (CAR L))))))

(DEFUN WARNING (&REST L)
       (TERPRI)(PRIN1 ACCEPT-TYPE)(PRINC ": WARNING! ")
       (DO L L (CDR L) (NULL L)
	   (PRINC (CAR L))))
)

(DEFUN DEBUG ()
       (LET ((DEBUG-FLAG T))
	    (RUN)))

(DEFUN RUN ()
       (DO ((TEM)) (())
	   (SETQ ++ TEM)
	   (TERPRI)(PRINC "-> ")
	   (SETQ TEM (READ))
	   (AND (ATOM TEM) (TYI))			;--MORE-- lossage.
	   (AND (EQ TEM T) (RETURN 'DONE))
	   (SETQ ** (OPTIMIZE-FSM (INTERPRET TEM)))
	   (OR DEBUG-FLAG (PRINT-FSM **))))

(IF-FOR-MACLISP
(DEFMACRO %POINTER (X) `(MAKNUM ,X))
)

(DEFUN PRINT-FSM (FSM)
       (TERPRI)(PRINC "#<FSM ")(PRIN1 (%POINTER FSM))(PRINC ">")
       (TERPRI)(PRINC "Character translation:")
       (DO L (FSM-BUCKETS) (CDR L) (NULL L)
	   (PRINT (LENGTH L))
	   (PRINC "= {")
	   (DO ((L (BUCKET-CHARS (CAR L)) (CDR L))
		(TEM))
	       ((NULL L))
	       (COND ((OR (MEMBER (SETQ TEM (CAR L)) '(0 7 10 11 12 14 15 40))
			  (< TEM 0))
		      (PRINC "<")(PRIN1 TEM)(PRINC ">"))
		     (T (TYO (CAR L)))))
	   (PRINC "}"))
       (TERPRI)(PRINC "Start:")
       (PRIN1 (DO ((A (FSM-START) (CDR A))
		   (STATES (FSM-STATES))
		   (B NIL (CONS (LENGTH (MEMQ (CAR A) STATES)) B)))
		  ((NULL A) B)))
       (TERPRI)(PRINC "** The Machine **")
       (DO ((ST (FSM-STATES) (CDR ST))
	    (STATES (FSM-STATES))
	    (BUCKETS (FSM-BUCKETS))
	    (I (LENGTH (FSM-STATES)) (1- I)))
	   ((NULL ST))
	   (TERPRI)(PRIN1 (LIST I (STATE-ACCEPTP (CAR ST))))(PRINC ": ")
	   (DO L (STATE-ALIST (CAR ST)) (CDR L) (NULL L)
	       (PRIN1 (CONS (LENGTH (MEMQ (CAAR L) BUCKETS))
			    (DO ((A (CDAR L) (CDR A))
				 (B NIL (CONS (LENGTH (MEMQ (CAR A) STATES)) B)))
				((NULL A) B)))))
	   (PRINC " /| ")
	   (PRIN1 (DO ((A (STATE-FROMS (CAR ST)) (CDR A))
		       (B NIL (CONS (LENGTH (MEMQ (CAR A) STATES)) B)))
		      ((NULL A) B))))
       (TERPRI)(PRINC "*****************")
       FSM)

;Set hackery:

;; Union together two sets. (Actually only the second argument need
;; be a set, the first can be a list, the result will be a set.  Thus
;; you can use (UNION-Q <list> NIL) to turn a list into a set.
(DEFUN UNION-Q (SET1 SET2)
       (DO ((SET1 SET1 (CDR SET1))
	    (TEM)
	    (RES SET2 (COND ((MEMQ (SETQ TEM (CAR SET1)) RES)
			     RES)
			    (T (CONS TEM RES)))))
	   ((NULL SET1) RES)))

;; Test to see if two sets are the same
(DEFUN EQUAL-SET-Q (SET1 SET2)
       (AND (= (LENGTH SET1) (LENGTH SET2))
	    (DO ((SET1 SET1 (CDR SET1)))
		((NULL SET1) T)
		(OR (MEMQ (CAR SET1) SET2)
		    (RETURN NIL)))))


;Make a readtable:

(IF-FOR-MACLISP
(DEFMACRO FILE-EXPAND-PATHNAME (X)
    `(MERGEF ,X '|RTC_IN >|))
)
;;; This is the real top level for the compiler.  It takes a source file
;;; and writes out the QFASL for it.
(DEFUN RTC-FILE (FILENAME)
       (LET ((FROM (OPEN (FILE-EXPAND-PATHNAME FILENAME) '(IN))))
	    (DO ((X (READ '*EOF* FROM) (READ '*EOF* FROM))
		 (LIST)
		 (RDTBL)
		 (SYMB 'READTABLE)
		 (OPTIONS))
		((EQ X '*EOF*)
		 (AND RDTBL (RTC-DUMP-QFASL SYMB RDTBL FILENAME)))
		(COND ((ATOM X)
		       (WARNING "strange object in file: " X))
		      (T (SELECTQ (CAR X)
				  ((:MAC MAC) (DO L (CDR X) (CDDR L) (ATOM L)
						  (AND (ATOM (CDR L))
						       (ERROR "-- odd number of arguments." X))
						  (OR (SYMBOLP (CAR L))
						      (ERROR "-- not a symbol (MAC)" (CAR L)))
						  (SET (CAR L) (EVAL (CADR L)))))
				  ((:DEF DEF) (PUSH (CDR X) LIST))
				  ((:OPT OPT) (SETQ OPTIONS
						    `(,(CADR X) ,(EVAL (CADDR X)) . ,OPTIONS)))
				  ((:END END)
				   (SETQ RDTBL (RTC1 (NREVERSE LIST) OPTIONS))
				   (SETQ LIST NIL
					 OPTIONS NIL)
				   (COND ((AND (NOT (NULL (CDR X)))
					       (SYMBOLP (CADR X)))
					  (SETQ SYMB (CADR X)))
					 (T
					  (WARNING "no destination for readtable."))))
				  ((:DECLARE DECLARE) NIL)
				  (OTHERWISE
				   (WARNING "strange object in file: " X))))))
	    (CLOSE FROM)))

(IF-FOR-LISPM
(DEFMACRO CREATE-ARRAY (&REST DIMS)
    `(MAKE-ARRAY NIL 'ART-Q (LIST . ,DIMS)))
)

(DEFUN RTC1 (LIST OPTIONS)
	(LET ((SLASH 57)
	      (CIRCLECROSS 26)
	      (WHITESPACE '(40 54 211 212 213 214 215))
	      (MACRO-ALIST NIL)
	      (/#-MACRO-ALIST NIL)
	      (READ-FUNCTION-PROPERTY 'STANDARD-READ-FUNCTION)
	      (BREAK-CHAR 40)
	      (BREAK-BUCKET NIL)
	      (SLASHIFIED-CHAR -1)
	      (SLASHIFIED-BUCKET NIL)
	      (EOF-CHAR -2)
	      (EOF-BUCKET NIL)
	      (MAKE-SYMBOL NIL)
	      (MAKE-SYMBOL-BUT-LAST NIL)
	      (SAVE-SYNTAX NIL)
	      (TRANSLATIONS NIL)
	      (BITS NIL)
	      (FSM NIL)
	      (RDTBL NIL)
	      (TABL NIL)
	      (N-STATES NIL)
	      (N-BUCKETS NIL)
	      (NEGATIVE-CHAR-LIST NIL)
	      )
	     (DO OP OPTIONS (CDDR OP) (OR (NULL OP) (NULL (CDR OP)))
		 (SELECTQ (CAR OP)
			  ((:MACRO-ALIST MACRO-ALIST)
			   (SETQ MACRO-ALIST (CADR OP)))
			  ((:/#-MACRO-ALIST /#-MACRO-ALIST)
			   (SETQ /#-MACRO-ALIST (CADR OP)))
			  ((:READ-FUNCTION-PROPERTY READ-FUNCTION-PROPERTY)
			   (SETQ READ-FUNCTION-PROPERTY (CADR OP)))
			  ((:SLASH SLASH)
			   (SETQ SLASH (CADR OP)))
			  ((:CIRCLECROSS CIRCLECROSS)
			   (SETQ CIRCLECROSS (CADR OP)))
			  ((:WHITE-SPACE-CHAR WHITE-SPACE-CHAR)
			   (SETQ WHITESPACE (CADR OP)))
			  ((:SLASHIFIED-CHAR SLASHIFIED-CHAR)
			   (SETQ SLASHIFIED-CHAR (CADR OP)))
			  ((:A-BREAK-CHAR A-BREAK-CHAR)
			   (SETQ BREAK-CHAR (CADR OP)))
			  ((:MAKE-SYMBOL MAKE-SYMBOL)
			   (SETQ MAKE-SYMBOL (CADR OP)))
			  ((:MAKE-SYMBOL-BUT-LAST MAKE-SYMBOL-BUT-LAST)
			   (SETQ MAKE-SYMBOL-BUT-LAST (CADR OP)))
			  ((:EOF-CHAR EOF-CHAR)
			   (SETQ EOF-CHAR (CADR OP)))
			  ((:BITS BITS)
			   (SETQ BITS (CADR OP)))
			  ((:SAVE-SYNTAX SAVE-SYNTAX)
			   (SETQ SAVE-SYNTAX (CADR OP)))
			  ((:TRANSLATIONS TRANSLATIONS)
			   (SETQ TRANSLATIONS (CADR OP)))
			  (OTHERWISE (WARNING "unrecognized option to RTC: " (CAR OP)))))
	     (SETQ FSM (RTC-MAKE-FSM LIST))
	     (SETQ RDTBL (MAKE-RDTBL))
	     (DO X 0 (1+ X) (= X RDTBL-ARRAY-SIZE)
		 (SETF (RDTBL-BITS RDTBL X) 0)
		 (SETF (RDTBL-CODE RDTBL X) 0)
		 (SETF (RDTBL-TRANS RDTBL X) X))
	     (DO TRS TRANSLATIONS (CDR TRS) (NULL TRS)
		 (LET ((FROM (CAAR TRS))
		       (TO (CADAR TRS)))
		      (IF (ATOM FROM)
			  (SETF (RDTBL-TRANS RDTBL FROM) TO)
			  (DO ((STOP (CADR FROM))
			       (I (CAR FROM) (1+ I))
			       (J (CAR TO) (1+ J)))
			      ((> I STOP))
			      (SETF (RDTBL-TRANS RDTBL I) J)))))
	     (DO ((L (FSM-BUCKETS) (CDR L))
		  (N 0 (1+ N)))
		 ((NULL L) (SETQ N-BUCKETS N))
		 (AND (MEMBER BREAK-CHAR (BUCKET-CHARS (CAR L)))
		      (SETQ BREAK-BUCKET (CAR L)))
		 (AND (MEMBER SLASHIFIED-CHAR (BUCKET-CHARS (CAR L)))
		      (SETQ SLASHIFIED-BUCKET (CAR L)))
		 (AND (MEMBER EOF-CHAR (BUCKET-CHARS (CAR L)))
		      (SETQ EOF-BUCKET (CAR L)))
		 (SETF (BUCKET-LINK (CAR L)) N))
	     (DO ((L (FSM-STATES) (CDR L))
		  (N 0)
		  (STATE)
		  (TEM))
		 ((NULL L) (SETQ N-STATES N))
		 (COND ((SETQ TEM (STATE-ACCEPTP (SETQ STATE (CAR L))))
			(AND (STATE-ALIST)
			     (WARNING "the definition of " TEM
				      " is a special case of some other token."))
			(SETF (STATE-MARK) NIL)
			(SETF (STATE-LINK) (GET TEM 'RTC-SET-UP)))
		       (T
			(SETF (STATE-MARK) T)
			(SETF (STATE-LINK) N)
			(SETQ N (1+ N)))))
	     (SETF TABL (CREATE-ARRAY N-STATES N-BUCKETS))
	     (MAPCV STATE (FSM-STATES)
	       (COND ((STATE-MARK)
		      (DO ((S (STATE-LINK))
			   (A (STATE-ALIST) (CDR A))
			   (S1))
			  ((NULL A))
			  (SETQ S1 (STATE-LINK (CADAR A)))
			  (AND (EQ (CAAR A) SLASHIFIED-BUCKET)
			       (NOT (ATOM S1))
			       (MEMQ (CAR S1) '(UNTYI-QUOTE UNTYI-FUNCTION))
			       (WARNING "reading a " (STATE-ACCEPTP (CADAR A))
					" may cause a slashified character to be untyied."))
			  (AND (EQ (CAAR A) EOF-BUCKET)
			       (ATOM S1)
			       (WARNING "the definition of some token includes an EOF."))
			  (AS-2 S1 TABL S (BUCKET-LINK (CAAR A)))))))
	     (DO ((BS (FSM-BUCKETS) (CDR BS)))
		 ((NULL BS))
		 (DO ((L (BUCKET-CHARS (CAR BS)) (CDR L))
		      (N (BUCKET-LINK (CAR BS)))
		      (C))
		     ((NULL L))
		     (COND ((> 0 (SETQ C (CAR L)))
			    (PUSH (LIST* C 0 N) NEGATIVE-CHAR-LIST))
			   (T
			    (SETF (RDTBL-CODE RDTBL C) N)))))
	     (DO ((L WHITESPACE (CDR L))
		  (TEM))
		 ((NULL L))
		 (COND ((> 0 (SETQ TEM (CAR L)))
			(LET ((X (ASSOC TEM NEGATIVE-CHAR-LIST)))
			     (IF (NULL X)
				 (PUSH (LIST* TEM 1 0) NEGATIVE-CHAR-LIST)
				 (SETF (CADR X) 1))))
		       (T
			(SETF (RDTBL-BITS RDTBL TEM) 1))))
	     (COND ((> 0 SLASH)
		    (WARNING "There will be no slashifying character in the readtable.")
		    (LET ((X (ASSOC SLASH NEGATIVE-CHAR-LIST)))
			 (IF (NULL X)
			     (PUSH (LIST* SLASH 2 0) NEGATIVE-CHAR-LIST)
			     (SETF (CADR X) (LOGIOR 2 (CADR X))))))
		   (T
		    (SETF (RDTBL-BITS RDTBL SLASH)
			  (LOGIOR 2 (RDTBL-BITS RDTBL SLASH)))))
	     (COND ((> 0 CIRCLECROSS)
		    (WARNING "There will be no circlecross-like character in the readtable.")
		    (LET ((X (ASSOC CIRCLECROSS NEGATIVE-CHAR-LIST)))
			 (IF (NULL X)
			     (PUSH (LIST* CIRCLECROSS 4 0) NEGATIVE-CHAR-LIST)
			     (SETF (CADR X) (LOGIOR 4 (CADR X))))))
		   (T
		    (SETF (RDTBL-BITS RDTBL CIRCLECROSS)
			  (LOGIOR 4 (RDTBL-BITS RDTBL CIRCLECROSS)))))
	     (DO ((BITS BITS (CDR BITS))
		  (C))
		 ((NULL BITS))
		 (IF (> 0 (SETQ C (CAAR BITS)))
		     (LET ((X (ASSOC C NEGATIVE-CHAR-LIST)))
			  (IF (NULL X)
			      (PUSH (LIST* C (CADAR BITS) 0) NEGATIVE-CHAR-LIST)
			      (SETF (CADR X) (LOGIOR (CADAR BITS) (CADR X)))))
		     (SETF (RDTBL-BITS RDTBL (CAAR BITS))
			   (LOGIOR (CADAR BITS) (RDTBL-BITS RDTBL (CAAR BITS))))))
	     (SETF (RDTBL-BREAK-CODE) (BUCKET-LINK BREAK-BUCKET))
	     (SETF (RDTBL-SLASH-CODE) (BUCKET-LINK SLASHIFIED-BUCKET))
	     (SETF (RDTBL-EOF-CODE) (BUCKET-LINK EOF-BUCKET))
	     (SETF (RDTBL-FSM) TABL)
	     (SETF (RDTBL-N-STATES) N-STATES)
	     (SETF (RDTBL-N-BUCKETS) N-BUCKETS)
	     (SETF (RDTBL-STARTING-STATE) (STATE-LINK (CAR (FSM-START))))
	     (SETF (RDTBL-MACRO-ALIST) MACRO-ALIST)
	     (SETF (RDTBL-/#-MACRO-ALIST) /#-MACRO-ALIST)
	     (SETF (RDTBL-READ-FUNCTION-PROPERTY) READ-FUNCTION-PROPERTY)
	     (SETF (RDTBL-PLIST)
		   (NCONC (DO ((L SAVE-SYNTAX (CDDR L))
			       (A NIL `(,(CAR L)
					,(LET ((CH (CADR L)))
					      (IF (< CH 0)
						  (LET ((X (ASSOC CH NEGATIVE-CHAR-LIST)))
						       (IF (NULL X)
							   (WARNING
							    "Character " CH
							    " not found to save syntax from.")
							   (CDR X)))
						  (CONS (RDTBL-BITS RDTBL CH)
							(RDTBL-CODE RDTBL CH))))
					. ,A)))
			      ((NULL L) A))
			  (RDTBL-PLIST)))
	     (SETF (RDTBL-DEFINITION) LIST)
	     (SETF (RDTBL-MAKE-SYMBOL)
		   (DO ((L MAKE-SYMBOL (CDR L))
			(R NIL (CONS (GET (CAR L) 'RTC-SET-UP) R)))
		       ((NULL L) R)))
	     (SETF (RDTBL-MAKE-SYMBOL-BUT-LAST)
		   (DO ((L MAKE-SYMBOL-BUT-LAST (CDR L))
			(R NIL (CONS (GET (CAR L) 'RTC-SET-UP) R)))
		       ((NULL L) R)))
	     (SETF (RDTBL-SLASH) SLASH)
	     (SETF (RDTBL-WHITESPACE) WHITESPACE)
	     (SETF (RDTBL-CIRCLECROSS) CIRCLECROSS)
	     (SETF (RDTBL-NAMED-STRUCTURE-SYMBOL) 'READTABLE)
	     (MAKE-ARRAY-INTO-NAMED-STRUCTURE RDTBL)
	     RDTBL))

(IF-FOR-MACLISP
(DEFUN MAKE-ARRAY-INTO-NAMED-STRUCTURE (X) T)
)

(DEFUN RTC-MAKE-FSM (LIST)
       (AND (ATOM LIST) (ERROR "-- bad token definition." LIST))
       (DO ((L LIST (CDR L))
	    (ORDER NIL (CONS ACCEPT-TYPE ORDER))
	    (FSM)
	    (FSM1)
	    (ACCEPT-TYPE)
	    (LL))
	   ((NULL L)
	    (SETQ ACCEPT-TYPE 'FINI)
	    (OPTIMIZE-FSM FSM))
	   (AND (ATOM L) (ERROR "-- bad token definition." L))
	   (SETQ LL (CAR L))
	   (AND (OR (ATOM LL)
		    (NOT (= 3 (LENGTH LL))))
		(ERROR "-- bad token definition." LL))
	   (SETQ ACCEPT-TYPE (CAR LL))
	   (PUTPROP ACCEPT-TYPE (CADR LL) 'RTC-DEF)
	   (PUTPROP ACCEPT-TYPE (CONS (CADDR LL) (CAR LL)) 'RTC-SET-UP)
	   (PUTPROP ACCEPT-TYPE ORDER 'RTC-ORDER)
	   (SETQ FSM1 (OPTIMIZE-FSM (INTERPRET (CADR LL))))
	   (SETQ FSM (IF FSM (UNION-FSM FSM1 FSM) FSM1))))

(IF-FOR-MACLISP
(DEFUN RTC-DUMP-QFASL (SYMBOL RDTBL INFILENAME)
       (LET ((ARRAY (RDTBL-ARRAY RDTBL))
	     (FSM (RDTBL-FSM RDTBL))
	     (STATES (RDTBL-N-STATES RDTBL))
	     (BUCKETS (RDTBL-N-BUCKETS RDTBL))
	     (ID NIL)
	     (SUBID NIL)
	     (ZERID NIL)
	     (LEADER-SIZE (RDTBL-SIZE))
	     (LEN RDTBL-ARRAY-SIZE)
	     (FILENAME (MERGEF '((* *) * QFASL) INFILENAME)))
	    (FASD-OPEN FILENAME)
	    (FASD-INITIALIZE)
	    (FASD-START-GROUP NIL 0 FASL-OP-INITIALIZE-NUMERIC-ARRAY)
	     (FASD-START-GROUP T 0 FASL-OP-ARRAY)
	      (FASD-CONSTANT (IF COLD-FLAG
				 'CONTROL-TABLES
				 'WORKING-STORAGE-AREA))
	      (FASD-CONSTANT 'ART-16B)
	      (FASD-CONSTANT (LIST 3 LEN))
	      (FASD-CONSTANT NIL)
	      (FASD-START-GROUP NIL 1 FASL-OP-TEMP-LIST) ;LEADER
	       (FASD-NIBBLE LEADER-SIZE)
	       (DO I (1- LEADER-SIZE) (1- I) (= I 0)
		   (FASD-CONSTANT (CXR I RDTBL)))
	       (FASD-CONSTANT NIL)   ;FSM, FILLED IN LATER
	       (FASD-TABLE-ENTER 'LIST (NCONS 0))
	      (FASD-CONSTANT NIL) ;INDEX OFFSET
	      (FASD-CONSTANT 'READTABLE) ;NAMED-STRUCTURE
	      (SETQ ID (FASD-TABLE-ENTER 'ARRAY-POINTER (GENSYM)))
	     (FASD-CONSTANT (* 3 LEN)) ;INITIALIZATION
	     (DO J 0 (1+ J) (= J LEN)
		 (DO I 0 (1+ I) (= I 3)
		     (FASD-NIBBLE (AR-2 ARRAY I J))))
	    (SETQ ZERID (FASD-CONSTANT 0))
	    (FASD-START-GROUP NIL 0 FASL-OP-INITIALIZE-ARRAY)
	    (SETQ SUBID
		  (FASD-MAKE-ARRAY (IF COLD-FLAG
				       'CONTROL-TABLES
				       'WORKING-STORAGE-AREA)
				   'ART-Q
				   (LIST STATES BUCKETS)
				   NIL
				   NIL))
	    (FASD-CONSTANT (* STATES BUCKETS))
	    (DO J 0 (1+ J) (= J BUCKETS)
	      (DO I 0 (1+ I) (= I STATES)
		(FASD-CONSTANT (AR-2 FSM I J))))

	    (FASD-START-GROUP NIL 3 FASL-OP-STOREIN-ARRAY-LEADER)
	    (FASD-NIBBLE ID)
	    (FASD-NIBBLE ZERID)
	    (FASD-NIBBLE SUBID)
	    (FASD-START-GROUP NIL 1 FASL-OP-STOREIN-SYMBOL-VALUE)
	    (FASD-NIBBLE ID)
	    (FASD-CONSTANT SYMBOL)
	    (FASD-END-WHACK)
	    (FASD-END-FILE)
	    (FASD-CLOSE FILENAME)))
)

(IF-FOR-LISPM
(DEFUN RTC-DUMP-QFASL (SYMBOL RDTBL INFILENAME &AUX RDTBL-INDEX INDEX-INDEX SYMBOL-INDEX
						    FSM-INDEX INDEX2-INDEX)
       (LET ((FILENAME (FILE-SET-FN2 INFILENAME "QFASL"))
	     (FSM (RDTBL-FSM RDTBL)))
	 (COMPILER:FASD-OPEN FILENAME)
	 (COMPILER:FASD-INITIALIZE)
	 (SETQ INDEX-INDEX (COMPILER:FASD-CONSTANT 1))
	 (SETQ SYMBOL-INDEX
	       (COMPILER:FASD-CONSTANT (SI:RDTBL-NAMED-STRUCTURE-SYMBOL RDTBL)))
	 (SETQ INDEX2-INDEX (COMPILER:FASD-CONSTANT 0))
	 (SETQ FSM-INDEX (COMPILER:FASD-CONSTANT FSM))
	 (SETF (RDTBL-FSM RDTBL) NIL)	;This makes the QFASL file readable by the cold-load
					;The area won't be CONTROL-TABLES for the cold-load,
					;but fortunately it ignores the specified area anyway.
	 (SETQ RDTBL-INDEX (COMPILER:FASD-CONSTANT RDTBL))
	 (SETF (RDTBL-FSM RDTBL) FSM)
	 (COMPILER:FASD-STORE-ARRAY-LEADER FSM-INDEX
					   RDTBL-INDEX
					   INDEX2-INDEX)
	 (COMPILER:FASD-STORE-ARRAY-LEADER SYMBOL-INDEX	;Fix named-structure symbol
					   RDTBL-INDEX	;I guess this is alleged to get loaded
					   INDEX-INDEX)	;wrong
	 (COMPILER:FASD-STORE-VALUE-CELL SYMBOL RDTBL-INDEX)
	 (COMPILER:FASD-END-WHACK)
	 (COMPILER:FASD-END-FILE)
	 (COMPILER:FASD-CLOSE FILENAME)))
)