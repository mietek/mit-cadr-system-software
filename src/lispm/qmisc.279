; -*-LISP-*- MACHINE MISCELLANEOUS FUNCTIONS NOT WORTHY OF BEING IN QFCTNS
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DECLARE (SETQ RETAIN-VARIABLE-NAMES-SWITCH 'ALL))
(DECLARE (SPECIAL ROOM))

(SETQ ROOM '(WORKING-STORAGE-AREA MACRO-COMPILED-PROGRAM))

(DEFUN ROOM-GET-AREA-LENGTH-USED (AREA)
  (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION))
       (N-REGIONS 0 (1+ N-REGIONS))
       (LENGTH 0 (+ LENGTH (24-BIT-UNSIGNED (REGION-LENGTH REGION))))
       (USED 0 (+ USED
		  (24-BIT-UNSIGNED
		    (COND ((AND (= 1 (LDB %%REGION-COMPACT-CONS-FLAG (REGION-BITS REGION)))
				(= (LDB %%REGION-REPRESENTATION-TYPE (REGION-BITS REGION))
				   %REGION-REPRESENTATION-TYPE-LIST))
			   (- (REGION-LENGTH REGION) (REGION-FREE-POINTER REGION)))
			  (T (REGION-FREE-POINTER REGION)))))))
      ((BIT-TEST (LSH 1 23.) REGION)
       (RETURN LENGTH USED N-REGIONS))))

(DEFUN 24-BIT-UNSIGNED (N)
  "Given an unsigned 24-bit fixnum, returns a positive number.
If the argument is negative, it is expanded into a bignum."
  (IF (MINUSP N) (+ N (ASH 1 24.)) N))

(DEFUN MAKE-24-BIT-UNSIGNED (N)
  "If arg a bignum, its low 24 bits are returned as a fixnum, possibly
   negative.  A fixnum arg is just returned."
  (COND ((= (%DATA-TYPE N) DTP-FIX) N)
	(T (LOGIOR (LSH (LDB 2701 N) 27) (LDB 27 N)))))

(DEFUN ROOM-PRINT-AREA (AREA &AUX LENGTH USED N-REGIONS (PACKAGE (PKG-FIND-PACKAGE "SYSTEM")))
  (COND ((NOT (NULL (AREA-NAME AREA)))
	 (MULTIPLE-VALUE (LENGTH USED N-REGIONS) (ROOM-GET-AREA-LENGTH-USED AREA))
	 (IF (= (LDB %%REGION-SPACE-TYPE (REGION-BITS (AREA-REGION-LIST AREA)))
		%REGION-SPACE-FIXED)
	     (FORMAT T "~51,1,1,'.<~S~;(~D region~:P)~> ~O//~O used.  ~D% free.~%"
		     (AREA-NAME AREA) N-REGIONS USED LENGTH
		     (COND ((ZEROP LENGTH)
			    0)
			   ((< LENGTH 40000)
			    (// (* 100. (- LENGTH USED)) LENGTH))
			   (T
			    (// (- LENGTH USED) (// LENGTH 100.))) ))
	     (FORMAT T "~51,1,1,'.<~S~;(~D region~:P)~> ~DK allocated, ~DK used.~%"
		     (AREA-NAME AREA) N-REGIONS
		     (// (+ LENGTH 1777) 2000) (// (+ USED 1777) 2000)))))
  T)

(DEFUN ROOM (&OPTIONAL ARG)
  (TERPRI)
  (COND ((NUMBERP ARG)
	 (ROOM-PRINT-AREA ARG))
	(T
	  (LET ((FREE-SIZE (ROOM-GET-AREA-LENGTH-USED FREE-AREA))
		(PHYS-SIZE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)))
	    (FORMAT T "Physical memory: ~O (~DK), Free space: ~O (~DK)"
		      PHYS-SIZE (// PHYS-SIZE 2000) FREE-SIZE (// FREE-SIZE 2000)))
	  (MULTIPLE-VALUE-BIND (N-WIRED-PAGES N-FIXED-WIRED-PAGES)
	      (COUNT-WIRED-PAGES)
	    (FORMAT T ", Wired pages ~D+~D (~D~[~;.25~;.5~;.75~]K)~%"
		      N-FIXED-WIRED-PAGES (- N-WIRED-PAGES N-FIXED-WIRED-PAGES)
		      (// N-WIRED-PAGES (// 2000 PAGE-SIZE))
		      (\ N-WIRED-PAGES (// 2000 PAGE-SIZE))))
	  (COND ((NULL ARG)
		 (DOLIST (AREA ROOM)
		   (ROOM-PRINT-AREA (IF (SYMBOLP AREA) (SYMEVAL AREA) AREA))))
		((EQ ARG 'T)
		 (PRINC "Unless otherwise noted, area names are in the SYSTEM package")
		 (TERPRI)
		 (DO AREA 0 (1+ AREA) (> AREA SIZE-OF-AREA-ARRAYS)
		   (ROOM-PRINT-AREA AREA)))
		(T (FERROR NIL "~S is the wrong type arg" ARG))))))

;First value is total number of wired pages.  Second is number of fixed-wired pages.
(DEFUN COUNT-WIRED-PAGES ()
  (DO ((ADR (REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (// (REGION-LENGTH PAGE-TABLE-AREA) 2) (1- N))
       (N-WIRED 0))
      ((ZEROP N)
       (RETURN N-WIRED (// (REGION-ORIGIN REGION-FREE-POINTER) PAGE-SIZE)))
    (AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
	 (= (%P-LDB %%PHT1-SWAP-STATUS-CODE ADR) %PHT-SWAP-STATUS-WIRED)
	 (SETQ N-WIRED (1+ N-WIRED)))))

(DEFUN STREAM-COPY-UNTIL-EOF (FROM-STREAM TO-STREAM &OPTIONAL (LEADER-SIZE T))
    (COND ((AND (MEMQ ':LINE-IN (FUNCALL FROM-STREAM ':WHICH-OPERATIONS))
                (MEMQ ':LINE-OUT (FUNCALL TO-STREAM ':WHICH-OPERATIONS)))
           (DO ((LINE)(EOF))
	       (())
	     (MULTIPLE-VALUE (LINE EOF)
			     (FUNCALL FROM-STREAM ':LINE-IN LEADER-SIZE))
	     (COND ((NOT EOF)
		    (FUNCALL TO-STREAM ':LINE-OUT LINE))
		   (T (FUNCALL TO-STREAM ':STRING-OUT LINE)
		      (RETURN NIL)))))
	  (T (DO ((CHAR))
		 ((NULL (SETQ CHAR (FUNCALL FROM-STREAM ':TYI))))
	       (FUNCALL TO-STREAM ':TYO CHAR)))))

(DEFUN DESCRIBE-ADL (ADL)
  (PROG (OPT-Q INIT-OPTION)
    L	(COND ((NULL ADL) (RETURN NIL)))
    	(SETQ OPT-Q (CAR ADL) ADL (CDR ADL))
	(TERPRI)
	(COND ((NOT (ZEROP (LOGAND OPT-Q %FEF-NAME-PRESENT)))
	       (PRINC "NAME ")
	       (PRIN1-THEN-SPACE (CAR ADL))
	       (SETQ ADL (CDR ADL))))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-SPECIALNESS OPT-Q)
			       FEF-SPECIALNESS))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-DES-DT OPT-Q)
			       FEF-DES-DT))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-QUOTE-STATUS OPT-Q)
			       FEF-QUOTE-STATUS))
	(PRIN1-THEN-SPACE (NTH (LDB %%FEF-ARG-SYNTAX OPT-Q)
			       FEF-ARG-SYNTAX))
	(PRIN1-THEN-SPACE (SETQ INIT-OPTION (NTH (LDB %%FEF-INIT-OPTION OPT-Q)
						 FEF-INIT-OPTION)))
	(COND ((MEMQ INIT-OPTION '(FEF-INI-PNTR FEF-INI-C-PNTR 
				   FEF-INI-OPT-SA FEF-INI-EFF-ADR))
	       (PRINC "ARG ")
	       (PRIN1 (CAR ADL))
	       (SETQ ADL (CDR ADL))))
	(GO L)
))

(DEFUN DESCRIBE-STACK-GROUP (SG &AUX TEM)
  (FORMAT T "~%Stack Group; name is ~S, current state ~S"
	  (SG-NAME SG)
	  (NTH (SG-CURRENT-STATE SG) SG-STATES))
  (COND ((NOT (ZEROP (SG-IN-SWAPPED-STATE SG)))
	 (FORMAT T "~%  Variables currently swapped out")))
  (COND ((NOT (ZEROP (SG-FOOTHOLD-EXECUTING-FLAG SG)))
	 (FORMAT T "~%  Foothold currently executing")))
  (COND ((NOT (ZEROP (SG-PROCESSING-ERROR-FLAG SG)))
	 (FORMAT T "~% Currently processing an error")))
  (COND ((NOT (ZEROP (SG-PROCESSING-INTERRUPT-FLAG SG)))
	 (FORMAT T "~% Currently processing an interrupt")))
  (FORMAT T "~%ERROR-MODE:")
     (PRINT-ERROR-MODE (SG-SAVED-M-FLAGS SG))
  (FORMAT T "~%SG-SAFE ~D, SG-SWAP-SV-ON-CALL-OUT ~D, SG-SWAP-SV-OF-SG-THAT-CALLS-ME ~D"
	  (SG-SAFE SG)
	  (SG-SWAP-SV-ON-CALL-OUT SG)
	  (SG-SWAP-SV-OF-SG-THAT-CALLS-ME SG))
  (FORMAT T "~%SG-INST-DISP: ~D (~:*~[Normal~;Debug~;Single-step~;Single-step done~])"
	    (SG-INST-DISP SG))
  (FORMAT T 
      "~%SG-PREVIOUS-STACK-GROUP ~S, SG-CALLING-ARGS-NUMBER ~S, SG-CALLING-ARGS-POINTER ~S"
          (SG-PREVIOUS-STACK-GROUP SG)
	  (SG-CALLING-ARGS-NUMBER SG)
	  (SG-CALLING-ARGS-POINTER SG))
  (FORMAT T "~%Regular PDL pointer ~D, ~D available, ~D limit"
          (SG-REGULAR-PDL-POINTER SG)
	  (ARRAY-LENGTH (SG-REGULAR-PDL SG))
	  (SG-REGULAR-PDL-LIMIT SG))
  (FORMAT T "~%Special PDL pointer ~D, ~D available, ~D limit"
	  (SG-SPECIAL-PDL-POINTER SG)
	  (ARRAY-LENGTH (SG-SPECIAL-PDL SG))
	  (SG-SPECIAL-PDL-LIMIT SG))
  (COND ((SETQ TEM (SG-RECOVERY-HISTORY SG))
	 (FORMAT T "~%Recovery history ~S" TEM)))
  (COND ((SETQ TEM (SG-UCODE SG))
	 (FORMAT T "~%SG-UCODE ~S" TEM)))
)

(DEFUN DESCRIBE-FEF (FEF &AUX HEADER NAME FAST-ARG SV MISC LENGTH DBI)
   (COND ((SYMBOLP FEF)
	  (DESCRIBE-FEF (CAR (FUNCTION-CELL-LOCATION FEF))))
	 ((NEQ (%DATA-TYPE FEF) DTP-FEF-POINTER)
	  (FERROR NIL "~S is not a FEF" FEF))
	 (T
	  (SETQ HEADER (%P-LDB-OFFSET %%HEADER-REST-FIELD FEF %FEFHI-IPC))
	  (SETQ LENGTH (%P-CONTENTS-OFFSET FEF %FEFHI-STORAGE-LENGTH))
	  (SETQ NAME (%P-CONTENTS-OFFSET FEF %FEFHI-FCTN-NAME))
	  (SETQ FAST-ARG (%P-CONTENTS-OFFSET FEF %FEFHI-FAST-ARG-OPT))
	  (SETQ SV (%P-CONTENTS-OFFSET FEF %FEFHI-SV-BITMAP))
	  (SETQ MISC (%P-CONTENTS-OFFSET FEF %FEFHI-MISC))
	  
	  (FORMAT T "~%FEF for function ~S~%" NAME)
	  (FORMAT T "Initial relative PC: ~S halfwords.~%" (LDB %%FEFH-PC HEADER))
; -- Print out the fast arg option
	  (FORMAT T "The Fast Argument Option is ~A"
		    (IF (ZEROP (LDB %%FEFH-FAST-ARG HEADER))
			"not active, but here it is anyway:"
			"active:"))
	  (DESCRIBE-NUMERIC-DESCRIPTOR-WORD FAST-ARG)
; -- Randomness.
	  (FORMAT T "~%The length of the local block is ~S~%"
		    (LDB %%FEFHI-MS-LOCAL-BLOCK-LENGTH MISC))
	  (FORMAT T "The total storage length of the FEF is ~S~%"
		    LENGTH)
; -- Special variables
	  (COND ((ZEROP (LDB %%FEFH-SV-BIND HEADER))
		 (PRINC "There are no special variables present."))
		(T (PRINC "There are special variables, ")
		   (TERPRI)
		   (COND ((ZEROP (LDB %%FEFHI-SVM-ACTIVE SV))
			  (PRINC "but the S-V bit map is not active. "))
			 (T (FORMAT T "and the S-V bit map is active and contains: ~O"
				      (LDB %%FEFHI-SVM-BITS SV))))))
          (TERPRI)
; -- ADL.
	  (COND ((ZEROP (LDB %%FEFH-NO-ADL HEADER))
		 (FORMAT T "There is an ADL:  It is ~S long, and starts at ~S"
			   (LDB %%FEFHI-MS-BIND-DESC-LENGTH MISC)
			   (LDB %%FEFHI-MS-ARG-DESC-ORG MISC))
		 (DESCRIBE-ADL (GET-MACRO-ARG-DESC-POINTER FEF))
		 )
		(T (PRINC "There is no ADL.")))
	  (TERPRI)
	  (COND ((SETQ DBI (FUNCTION-DEBUGGING-INFO FEF))
		 (FORMAT T "Debugging info:~%")
		 (DOLIST (ITEM DBI)
		   (FORMAT T "  ~S~%" ITEM))))
	  )))

(DEFUN DESCRIBE-NUMERIC-DESCRIPTOR-WORD (N &AUX MIN MAX)
    (TERPRI)
    (PRINC "   ")
    (AND (BIT-TEST %ARG-DESC-QUOTED-REST N)
	 (PRINC "Quoted rest arg, "))
    (AND (BIT-TEST %ARG-DESC-EVALED-REST N)
	 (PRINC "Evaluated rest arg, "))
    (AND (BIT-TEST %ARG-DESC-FEF-QUOTE-HAIR N)
	 (PRINC "Some args quoted, "))
    (AND (BIT-TEST %ARG-DESC-INTERPRETED N)
	 (PRINC "Interpreted function, "))
    (AND (BIT-TEST %ARG-DESC-FEF-BIND-HAIR N)
	 (PRINC "Linear enter must check ADL, "))
    (SETQ MAX (LDB %%ARG-DESC-MAX-ARGS N))
    (SETQ MIN (LDB %%ARG-DESC-MIN-ARGS N))
    (COND ((= MAX MIN)
	   (PRINC MAX) (PRINC " args."))
	  (T
	   (PRINC "Takes between ") (PRINC MIN) (PRINC " and ") (PRINC MAX) (PRINC " args."))))


(DEFUN DESCRIBE-ARRAY (ARRAY &AUX ARRAYDIMS NDIMS LONG-LENGTH-FLAG)
    (COND ((SYMBOLP ARRAY)
	   (COND ((AND (BOUNDP ARRAY)
		       (ARRAYP (SYMEVAL ARRAY)))
		  (DESCRIBE-ARRAY (SYMEVAL ARRAY)))
		 ((AND (FBOUNDP ARRAY)
		       (ARRAYP (FSYMEVAL ARRAY)))
		  (DESCRIBE-ARRAY (FSYMEVAL ARRAY)))
		 (T NIL)))
	  ((ARRAYP ARRAY)
	   (FORMAT STANDARD-OUTPUT "~%This is a ~S type array." (ARRAY-TYPE ARRAY))
	   (SETQ ARRAYDIMS (ARRAY-DIMENSIONS ARRAY))
	   (SETQ NDIMS (LENGTH ARRAYDIMS))
	   (SETQ LONG-LENGTH-FLAG (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
	   (COND ((> NDIMS 1)
		  (FORMAT STANDARD-OUTPUT "~%It is ~D-dimensional, with dimensions "
			  NDIMS)
		  (DO L ARRAYDIMS (CDR L) (NULL L)
		    (FORMAT STANDARD-OUTPUT "~S " (CAR L))))
		 (T (FORMAT STANDARD-OUTPUT "~%It is ~S long." (CAR ARRAYDIMS))
		    (AND (< (ARRAY-ACTIVE-LENGTH ARRAY) (CAR ARRAYDIMS))
			 (FORMAT STANDARD-OUTPUT "  Active length is ~S"
				 (ARRAY-ACTIVE-LENGTH ARRAY)))))
	   (AND (ARRAY-HAS-LEADER-P ARRAY)
		(FORMAT STANDARD-OUTPUT "~%It has a leader, of length ~S"
			(ARRAY-LEADER-LENGTH ARRAY)))
	   (COND ((ARRAY-DISPLACED-P ARRAY)
		  (COND ((ARRAY-INDIRECT-P ARRAY)
			 (FORMAT STANDARD-OUTPUT "~%The array is indirected to ~S"
				 (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG)))
			 (AND (ARRAY-INDEXED-P ARRAY)
			      (FORMAT STANDARD-OUTPUT ", with index-offset ~S"
				    (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG 2))))
			 (FORMAT STANDARD-OUTPUT "~%Description:")
			 (DESCRIBE-ARRAY (%P-CONTENTS-OFFSET ARRAY
							     (+ NDIMS LONG-LENGTH-FLAG))))
			(T (FORMAT STANDARD-OUTPUT "~%The array is displaced to ~S"
				   (%P-CONTENTS-OFFSET ARRAY (+ NDIMS LONG-LENGTH-FLAG))))))))
	  (T (FERROR NIL "~S is not an array" ARRAY))))

;DESCRIBE ANYTHING
(DEFUN DESCRIBE (ANYTHING &OPTIONAL NO-COMPLAINTS &AUX TYPE)
  (COND	((AND (NAMED-STRUCTURE-P ANYTHING)
	      (COND ((AND (FBOUNDP (NAMED-STRUCTURE-SYMBOL ANYTHING))
			  (MEMQ ':DESCRIBE
				(NAMED-STRUCTURE-INVOKE ANYTHING ':WHICH-OPERATIONS)))
		     (NAMED-STRUCTURE-INVOKE ANYTHING ':DESCRIBE))
		    ((GET (SETQ TYPE (NAMED-STRUCTURE-SYMBOL ANYTHING)) 'DEFSTRUCT-ITEMS)
		     (DESCRIBE-DEFSTRUCT TYPE ANYTHING)))))
	((OR (ENTITYP ANYTHING) (= (%DATA-TYPE ANYTHING) DTP-INSTANCE))
	 (FUNCALL ANYTHING ':DESCRIBE))
	((ARRAYP ANYTHING)
	 (DESCRIBE-ARRAY ANYTHING))
	((CLOSUREP ANYTHING)
	 (DESCRIBE-CLOSURE ANYTHING))
	((= (%DATA-TYPE ANYTHING) DTP-FEF-POINTER)
	 (DESCRIBE-FEF ANYTHING))
	((SYMBOLP ANYTHING)
	 (DESCRIBE-SYMBOL ANYTHING))
	((LISTP ANYTHING)
	 (DESCRIBE-LIST ANYTHING))
	((= (%DATA-TYPE ANYTHING) DTP-STACK-GROUP)
	 (DESCRIBE-STACK-GROUP ANYTHING))
	((SMALL-FLOATP ANYTHING)
	 (DESCRIBE-SMALL-FLONUM ANYTHING))
	((FLOATP ANYTHING)
	 (DESCRIBE-FLONUM ANYTHING))
        ((= (%DATA-TYPE ANYTHING) DTP-SELECT-METHOD)
         (DESCRIBE-SELECT-METHOD ANYTHING))
	((FIXP ANYTHING)
	 (FORMAT T "~%~R is ~[even~;odd~]" ANYTHING (LDB 0001 ANYTHING)))
	((NOT NO-COMPLAINTS)
	 (FORMAT STANDARD-OUTPUT "~%I don't know how to describe ~S" ANYTHING)))
  (FUNCALL STANDARD-OUTPUT ':FRESH-LINE))

(DEFUN DESCRIBE-1 (THING)	;AN INTERNAL SUBROUTINE
  (COND ((OR (NULL THING) ;Don't recursively describe relatively boring things
	     (NUMBERP THING) (SYMBOLP THING) (STRINGP THING))
	 NIL)
	(T (LET ((STANDARD-OUTPUT	;Arrange for indentation by 4 spaces
		   (CLOSURE '(STANDARD-OUTPUT)
		     #'(LAMBDA (&REST ARGS)
			  ;; Have to do it this way rather than with PROG1
			  ;; due to multiple-values not getting passed back
			  ;; This vile kludgery seems to be the only way to get it to work
			  ;; due to various things shafting me left and right.
			  (PROG (X1 X2 X3 X4 X5)
			    (MULTIPLE-VALUE (X1 X2 X3 X4 X5)
			       (APPLY STANDARD-OUTPUT ARGS))
			    (AND (EQ (CAR ARGS) ':TYO) (= (CADR ARGS) #\CR)
				 (FUNCALL STANDARD-OUTPUT ':STRING-OUT "    "))
			    (RETURN X1 X2 X3 X4 X5))))))
	     (DESCRIBE THING T))
	   (FUNCALL STANDARD-OUTPUT ':FRESH-LINE))))

(DEFUN DESCRIBE-SYMBOL (SYM)
  (COND ((BOUNDP SYM)
	 (LET ((PRINLEVEL 2) (PRINLENGTH 3))
	   (FORMAT STANDARD-OUTPUT "~%The value of ~S is ~S" SYM (SYMEVAL SYM)))
	 (DESCRIBE-1 (SYMEVAL SYM))))
  (COND ((FBOUNDP SYM)
	 (LET ((PRINLEVEL 2) (PRINLENGTH 3))
	   (FORMAT STANDARD-OUTPUT "~%~S is the function ~S: ~S"
		   SYM (FSYMEVAL SYM) (ARGLIST SYM)))
	 (DESCRIBE-1 (FSYMEVAL SYM))))
  (DO ((PL (PLIST SYM) (CDDR PL))
       (PRINLEVEL 2)
       (PRINLENGTH 3))
      ((NULL PL))
    (FORMAT STANDARD-OUTPUT "~%~S has property ~S: ~S"
	    SYM (CAR PL) (CADR PL))
    (DESCRIBE-1 (CADR PL)))
  NIL)

(DEFUN DESCRIBE-LIST (L)
  (FORMAT STANDARD-OUTPUT "~%~S is a list" L))

(DEFUN DESCRIBE-DEFSTRUCT (SYMBOL X)
    (FORMAT T "~%~S is a ~S~%" X SYMBOL)
    (DO L (GET SYMBOL 'DEFSTRUCT-ITEMS) (CDR L) (NULL L)
      (FORMAT T "   ~30A~S~%"
	      (STRING-APPEND (CAR L) ":")
	      (EVAL `(,(CAR L) ',X)))))

(DEFUN DESCRIBE-CLOSURE (CL)
    (LET ((C (%MAKE-POINTER DTP-LIST CL))
          (SYM NIL) (OFFSET NIL))
      (FORMAT T "~%~S is a closure of ~S:~%" CL (CAR C))
      (DO L (CDR C) (CDDR L) (NULL L)
       (SETQ SYM (%FIND-STRUCTURE-HEADER (CAR L))
	     OFFSET (%POINTER-DIFFERENCE (CAR L) SYM))
       (FORMAT T
               "   ~A cell of ~S:        ~32,7A~%"
               (SELECTQ OFFSET
                        (0 "Print name") (1 "Value") (2 "Function")
                        (3 "Property list") (4 "Package"))
               SYM
               (COND ((= (%P-DATA-TYPE (CADR L)) DTP-NULL)
                      "unbound.")
                     (T (CAADR L)))))
      (DESCRIBE-1 (CAR C))
      ))

(DEFUN DESCRIBE-SELECT-METHOD (M)
  (FORMAT T "~%~S handles:" M)
  (DO ((ML (%MAKE-POINTER DTP-LIST M) (CDR ML)))
      ((ATOM ML)
       (COND (ML
	      (FORMAT T "~%   anything else to ~S" ML)
	      (COND ((SYMBOLP ML)
		     (AND (BOUNDP ML) (FORMAT T "  -> ~S" (SYMEVAL ML))) ;probably a class
		     )))))
    (COND ((ATOM (CAR ML)) (FORMAT T "~%   subroutine ~S" (CAR ML)))
          (T (FORMAT T "~%   ~S: ~S" (CAAR ML) (CDAR ML))))))

(DEFUN DESCRIBE-SMALL-FLONUM (X)
  (FORMAT T "~%~S is a small flonum.~%  " X)
  (FORMAT T "Excess-100 exponent ~O, 17-bit mantissa ~O (with sign bit deleted)"
	    (LDB 2107 (%POINTER X)) (LDB 0021 (%POINTER X))))

(DEFUN DESCRIBE-FLONUM (X)
  (FORMAT T "~%~S is a flonum.~%  " X)
  (FORMAT T "Excess-2000 exponent ~O, 32-bit mantissa ~O~4,48O~4,48O (including sign)"
	       (%P-LDB-OFFSET 1013 X 0)
	       (%P-LDB-OFFSET 0010 X 0)
	       (%P-LDB-OFFSET 1414 X 1)
	       (%P-LDB-OFFSET 0014 X 1)))

(DEFUN DESCRIBE-AREA (AREA &AUX LENGTH USED N-REGIONS)
  (AND (NUMBERP AREA) (SETQ AREA (AREA-NAME AREA)))
  (DO AREA-NUMBER 0 (1+ AREA-NUMBER) (> AREA-NUMBER SIZE-OF-AREA-ARRAYS)
    (COND ((EQ AREA (AREA-NAME AREA-NUMBER))
	   (MULTIPLE-VALUE (LENGTH USED N-REGIONS) (ROOM-GET-AREA-LENGTH-USED AREA-NUMBER))
	   (FORMAT T "~%Area #~O: ~S has ~D region~P, max size ~O, region size ~O (octal):~%"
		     AREA-NUMBER AREA N-REGIONS N-REGIONS
		     (AREA-MAXIMUM-SIZE AREA-NUMBER) (AREA-REGION-SIZE AREA-NUMBER))
	   (DO ((REGION (AREA-REGION-LIST AREA-NUMBER) (REGION-LIST-THREAD REGION))
		(BITS))
	       ((MINUSP REGION))
	     (SETQ BITS (REGION-BITS REGION))
	     (FORMAT T "  Region #~O: Origin ~O, Length ~O, Free ~O, GC ~O, Type ~A ~A, Map ~O,~[NoScav~;Scav~]~%"
		     REGION (REGION-ORIGIN REGION) (REGION-LENGTH REGION)
		     (REGION-FREE-POINTER REGION) (REGION-GC-POINTER REGION)
		     (COND ((AND (= (LDB %%REGION-REPRESENTATION-TYPE BITS)
				    %REGION-REPRESENTATION-TYPE-LIST)
				 (NOT (ZEROP (LDB %%REGION-COMPACT-CONS-FLAG BITS))))
			    "LIST*")
			   ((NTH (LDB %%REGION-REPRESENTATION-TYPE BITS)
				 '(LIST STRUC "REP=2" "REP=3"))))
		     (NTH (LDB %%REGION-SPACE-TYPE BITS)
			  '(FREE OLD NEW STATIC FIXED EXITED EXIT EXTRA-PDL
			    WIRED MAPPED COPY "TYPE=13" "TYPE=14" "TYPE=15"
			    "TYPE=16" "TYPE=17"))
		     (LDB %%REGION-MAP-BITS BITS)
                     (LDB %%REGION-SCAVENGE-ENABLE BITS)))
	   (RETURN T)))))


(SPECIAL RANDOM-ARRAY)

(DEFSTRUCT (RANDOM-NUMBER-TABLE ARRAY-LEADER)
    RANDOM-FILL-POINTER
    RANDOM-SEED
    RANDOM-POINTER-1
    RANDOM-POINTER-2
    )

(DEFUN RANDOM-CREATE-ARRAY (SIZE OFFSET SEED &OPTIONAL (AREA NIL) &AUX ARRAY)
    (SETQ ARRAY (MAKE-RANDOM-NUMBER-TABLE
		 	MAKE-ARRAY (AREA ART-Q-LIST SIZE)))
    (SETF (RANDOM-FILL-POINTER ARRAY) SIZE)
    (SETF (RANDOM-SEED ARRAY) SEED)
    (SETF (RANDOM-POINTER-1 ARRAY) 0)
    (SETF (RANDOM-POINTER-2 ARRAY) OFFSET)
    (RANDOM-INITIALIZE ARRAY)
    ARRAY)

(DEFUN RANDOM-INITIALIZE (ARRAY &AUX SIZE X BYTE-SPEC POINTER)
   (SETQ SIZE (RANDOM-FILL-POINTER ARRAY)
	 POINTER (AP-1 ARRAY 0))
   (DO I 0 (1+ I) (= I SIZE)
     (AS-1 0 ARRAY I))
   (SETQ X (RANDOM-SEED ARRAY))
   (DO L '(1414 0014) (CDR L) (NULL L)
     (SETQ BYTE-SPEC (CAR L))
     (DO I 0 (1+ I) (= I SIZE)
       (SETQ X (%24-BIT-TIMES X 4093.))			;4093. is a prime number.
       (%P-DPB-OFFSET (LDB 1314 X) BYTE-SPEC POINTER I)))
   (SETF (RANDOM-SEED ARRAY) X))

(DEFUN RANDOM (&OPTIONAL ARG ARRAY &AUX PTR1 PTR2 SIZE ANS)
    (COND ((NULL ARRAY)
	   (OR (BOUNDP 'RANDOM-ARRAY)
	       (SETQ RANDOM-ARRAY (RANDOM-CREATE-ARRAY 71. 35. 105)))
	   (SETQ ARRAY RANDOM-ARRAY)))	   ;INITIALIZATION AS OPT ARG LOSES ON BOUNDP.
    (WITHOUT-INTERRUPTS
      (SETQ PTR1 (RANDOM-POINTER-1 ARRAY)
	    PTR2 (RANDOM-POINTER-2 ARRAY)
	    SIZE (RANDOM-FILL-POINTER ARRAY))
      (OR (< (SETQ PTR1 (1+ PTR1)) SIZE)
	  (SETQ PTR1 0))
      (OR (< (SETQ PTR2 (1+ PTR2)) SIZE)
	  (SETQ PTR2 0))
      (SETF (RANDOM-POINTER-1 ARRAY) PTR1)
      (SETF (RANDOM-POINTER-2 ARRAY) PTR2)
      (SETQ ANS (%24-BIT-PLUS (AR-1 ARRAY PTR1) (AR-1 ARRAY PTR2)))
      (AS-1 ANS ARRAY PTR2))
    (COND (ARG (\ (LOGAND ANS 37777777) ARG))   ;ASSURE POSITIVE ANSWER
	  (T ANS)))

;; Return a randomly chosen number at least LOW and less than HIGH.
(DEFUN RANDOM-IN-RANGE (LOW HIGH)
  (PROG* ((R (RANDOM))
	  (RNORM (// (LOGAND R 777777) (FLOAT 1000000))))
     (RETURN (+ LOW (* RNORM (- HIGH LOW))))))

(DEFUN SET-MEMORY-SIZE (NEW-SIZE)
  (PROG (OLD-SIZE NEWP OLDP)
	(COND ((< NEW-SIZE (+ (SYSTEM-COMMUNICATION-AREA %SYS-COM-WIRED-SIZE) 20000)) ;8K MIN
	       (FERROR NIL "~O is smaller than wired + 8K"  NEW-SIZE)))
    L   (SETQ OLD-SIZE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE))
        (SETQ OLDP (// (+ OLD-SIZE (1- PAGE-SIZE)) PAGE-SIZE))
        (SETQ NEWP (// (+ NEW-SIZE (1- PAGE-SIZE)) PAGE-SIZE))
	(COND ((OR (> NEWP (REGION-LENGTH PHYSICAL-PAGE-DATA))
		   (> NEWP (// (* 4 (REGION-LENGTH PAGE-TABLE-AREA)) 9)))
	       (FERROR NIL "~O is bigger than page tables allow"  NEW-SIZE))
	      ((= NEWP OLDP) (RETURN T))
              ((< NEWP OLDP) (GO FLUSH)))
  MORE  (COND ((%DELETE-PHYSICAL-PAGE OLD-SIZE)
               (PRINT (LIST OLD-SIZE "EXISTED"))))
        (%CREATE-PHYSICAL-PAGE OLD-SIZE)
	(STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
	       (+ OLD-SIZE PAGE-SIZE))
        (GO L)

  FLUSH (COND ((NULL (%DELETE-PHYSICAL-PAGE (- OLD-SIZE PAGE-SIZE)))
               (PRINT (LIST (- OLD-SIZE PAGE-SIZE) "DID-NOT-EXIST"))))
	(STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
	       (- OLD-SIZE PAGE-SIZE))
        (GO L)))

(DEFUN SET-ERROR-MODE (&OPTIONAL (CAR-SYM-MODE 1) (CDR-SYM-MODE 1)
			         (CAR-NUM-MODE 0) (CDR-NUM-MODE 0))
       (SETQ %MODE-FLAGS (%LOGDPB CAR-SYM-MODE %%M-FLAGS-CAR-SYM-MODE %MODE-FLAGS))
       (SETQ %MODE-FLAGS (%LOGDPB CDR-SYM-MODE %%M-FLAGS-CDR-SYM-MODE %MODE-FLAGS))
       (SETQ %MODE-FLAGS (%LOGDPB CAR-NUM-MODE %%M-FLAGS-CAR-NUM-MODE %MODE-FLAGS))
       (SETQ %MODE-FLAGS (%LOGDPB CDR-NUM-MODE %%M-FLAGS-CDR-NUM-MODE %MODE-FLAGS)))

(DEFUN PRINT-ERROR-MODE (&OPTIONAL (EM %MODE-FLAGS) (STREAM STANDARD-OUTPUT))
  (FORMAT STREAM
	  "CAR of a number is ~A.~%CDR of a number is ~A.~%CAR of a symbol is ~A.~%CDR of a symbol is a ~A.~%Trapping is ~A.~%"
	  (SELECTQ (LDB %%M-FLAGS-CAR-NUM-MODE EM)
	      (0 "an error")
	      (1 "NIL")
	      (OTHERWISE "in an unknown state"))
	  (SELECTQ (LDB %%M-FLAGS-CDR-NUM-MODE EM)
	      (0 "an error")
	      (1 "NIL")
	      (OTHERWISE "in an unknown state"))
	  (SELECTQ (LDB %%M-FLAGS-CAR-SYM-MODE EM)
	      (0 "an error")
	      (1 "NIL if the symbol is NIL, otherwise an error")
	      (2 "NIL")
	      (3 "its print-name"))
	  (SELECTQ (LDB %%M-FLAGS-CDR-SYM-MODE EM)
	      (0 "an error")
	      (1 "NIL if the symbol is NIL, otherwise an error")
	      (2 "NIL")
	      (3 "its property list"))
	  (SELECTQ (LDB %%M-FLAGS-TRAP-ENABLE EM)
	      (0 "disabled")
	      (1 "enabled"))
	  ))

(DECLARE (SPECIAL APROPOS-SUBSTRING))

(DEFUN APROPOS (APROPOS-SUBSTRING &OPTIONAL (PKG PKG-GLOBAL-PACKAGE))
    (MAPATOMS-ALL
     (FUNCTION (LAMBDA (SYMBOL)
	 (COND ((STRING-SEARCH APROPOS-SUBSTRING (GET-PNAME SYMBOL))
		(LET ((PACKAGE (CAR (PACKAGE-CELL-LOCATION SYMBOL))))
		     ;;Binding PACKAGE is to prevent printing of a package prefix.
		  (FORMAT T "~%~A:~S" PACKAGE SYMBOL)) ;ALWAYS get a prefix
		(AND (FBOUNDP SYMBOL)
		     (FORMAT T " - Function ~:S" (ARGLIST SYMBOL)))
		(AND (BOUNDP SYMBOL)
		     (COND ((FBOUNDP SYMBOL) (PRINC ", Bound"))
			   (T (PRINC " - Bound"))))))))
     PKG))

(DEFUN SYMEVAL-IN-CLOSURE (CLOSURE PTR)
  (CHECK-ARG CLOSURE (OR (CLOSUREP CLOSURE) (ENTITYP CLOSURE)) "a closure or an entity")
  (CHECK-ARG PTR (COND ((SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
		       ((= (%DATA-TYPE PTR) DTP-LOCATIVE)))
	         "a symbol or a locative")
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (CAR PTR))
    (AND (EQ (CAR L) PTR)
	 (RETURN (CAADR L)))))

(DEFUN LOCATE-IN-CLOSURE (CLOSURE PTR)
  (CHECK-ARG CLOSURE (OR (CLOSUREP CLOSURE) (ENTITYP CLOSURE)) "a closure or an entity")
  (CHECK-ARG PTR (COND ((SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
		       ((= (%DATA-TYPE PTR) DTP-LOCATIVE)))
	         "a symbol or a locative")
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       PTR)
    (AND (EQ (CAR L) PTR)
	 (RETURN (CADR L)))))

(DEFUN SET-IN-CLOSURE (CLOSURE PTR VAL)
  (CHECK-ARG CLOSURE (OR (CLOSUREP CLOSURE) (ENTITYP CLOSURE)) "a closure or an entity")
  (CHECK-ARG PTR (COND ((SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
		       ((= (%DATA-TYPE PTR) DTP-LOCATIVE)))
	         "a symbol or a locative")
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (RPLACA PTR VAL))
    (COND ((EQ (CAR L) PTR)
	   (RETURN (RPLACA (CADR L) VAL)))))
  VAL)

(DEFUN CLOSUREP (X)
    (= (%DATA-TYPE X) DTP-CLOSURE))

(DEFUN ENTITYP (X)
    (= (%DATA-TYPE X) DTP-ENTITY))

(DEFUN LOCATIVEP (X)
    (= (%DATA-TYPE X) DTP-LOCATIVE))

;ARRAY-POP, eventually to be micro-coded
;UNDOES (ARRAY-PUSH ARRAY <DATA>) AND RETURNS <DATA>
(DEFUN ARRAY-POP (ARRAY)
  (PROG (IDX VAL ARRAY-TYPE (INHIBIT-SCHEDULING-FLAG T))
	(COND ((ZEROP (SETQ IDX (ARRAY-LEADER ARRAY 0)))
	       (FERROR NIL "~S Overpopped" ARRAY)))
	(SETQ ARRAY-TYPE (AR-1 (FUNCTION ARRAY-TYPES)
			       (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0)))
	(SETQ VAL (AR-1 ARRAY (SETQ IDX (1- IDX))))	;1- BECAUSE IDX IS # ACTIVE ELEMENTS
	(COND ((MEMQ ARRAY-TYPE '(ART-Q ART-Q-LIST))
	       (AS-1 NIL ARRAY IDX)))   ;FLUSH SO NOT THERE FOR GC (HA HA)
	(STORE-ARRAY-LEADER IDX ARRAY 0)
	(COND ((AND (EQ ARRAY-TYPE 'ART-Q-LIST)
		    (NOT (ZEROP IDX)))
	       (%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- IDX)))))
	(RETURN VAL)))

;FILLARRAY as in Maclisp, eventually to be micro-coded.
;SOURCE may be an array or a list.
;Order of subscripts is currently incompatible with Maclisp for multi-dimensional
;arrays.  In any case, will not bother supporting multi-dimensional until it's micro-coded.
(DEFUN FILLARRAY (ARRAY SOURCE)
  (AND (SYMBOLP ARRAY) (SETQ ARRAY (FSYMEVAL ARRAY)))
  (COND ((ARRAYP SOURCE)
	 (COPY-ARRAY-CONTENTS SOURCE ARRAY))
	(T (DO ((I 0 (1+ I))
		(N (ARRAY-LENGTH ARRAY))
		(L SOURCE (OR (CDR L) L)))
	       ((>= I N))
	     (AS-1 (CAR L) ARRAY I))))
  ARRAY)

(DEFUN LISTARRAY (ARRAY &OPTIONAL LIMIT &AUX LST)
  (AND (SYMBOLP ARRAY) (SETQ ARRAY (FSYMEVAL ARRAY)))
  (OR LIMIT
      (SETQ LIMIT (ARRAY-ACTIVE-LENGTH ARRAY)))
  (SETQ LST (MAKE-LIST DEFAULT-CONS-AREA LIMIT))
  (DO ((I 0 (1+ I))
       (L LST (CDR L)))
      ((>= I LIMIT)
       LST)
    (RPLACA L (AR-1 ARRAY I))))

(DEFUN LIST-ARRAY-LEADER (ARRAY &OPTIONAL LIMIT &AUX LST)
       (AND (SYMBOLP ARRAY) (SETQ ARRAY (FSYMEVAL ARRAY)))
       (OR LIMIT
	   (SETQ LIMIT (OR (ARRAY-LEADER-LENGTH ARRAY) 0)))
       (SETQ LST (MAKE-LIST DEFAULT-CONS-AREA LIMIT))
       (DO ((I 0 (1+ I))
	    (L LST (CDR L)))
	   ((>= I LIMIT)
	    LST)
	   (RPLACA L (ARRAY-LEADER ARRAY I))))

(DEFUN *RSET (&OPTIONAL (NEW-MODE T))
    (SETQ *RSET NEW-MODE))

(DEFUN ARRAY-/#-DIMS (ARRAY)
    (CHECK-ARG ARRAY ARRAYP "an array")
    (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0))

(DEFUN DATA-TYPE (X)
    (AR-1 (FSYMEVAL 'Q-DATA-TYPES) (%DATA-TYPE X)))

(LOCAL-DECLARE ((SPECIAL FUNCTION))
(DEFUN WHO-CALLS (FUNCTION &OPTIONAL (PKG PKG-GLOBAL-PACKAGE))
    (SETQ PKG (PKG-FIND-PACKAGE PKG))
    (AND (STRINGP FUNCTION) (SETQ FUNCTION (INTERN FUNCTION)))
    (CHECK-ARG FUNCTION SYMBOLP "a symbol")
    (MAPATOMS-ALL (FUNCTION WHO-CALLS-AUX) PKG)
    NIL))


;; This attempts to reduce page faults
;; but it only saves as much time as the sort uses up.
;; Maybe with explicit swap-out it will be faster.
(COMMENT
(LOCAL-DECLARE ((SPECIAL FUNCTION ARRAY))
(DEFUN WHO-CALLS (FUNCTION &OPTIONAL (PKG PKG-GLOBAL-PACKAGE))
    (SETQ PKG (PKG-FIND-PACKAGE PKG))
    (AND (STRINGP FUNCTION) (SETQ FUNCTION (INTERN FUNCTION)))
    (CHECK-ARG FUNCTION SYMBOLP "a symbol")
    (LET ((ARRAY (MAKE-ARRAY NIL ART-Q 10000. NIL 1)))
        (SETF (ARRAY-LEADER ARRAY 0) 0)
        (MAPATOMS-ALL (FUNCTION (LAMBDA (SYMBOL)
                          (AND (FBOUNDP SYMBOL)
                               (ARRAY-PUSH-EXTEND ARRAY SYMBOL (ARRAY-ACTIVE-LENGTH ARRAY)))))
                      PKG)
        (SORT ARRAY (FUNCTION (LAMBDA (X Y)
                        (< (%POINTER (FSYMEVAL X)) (%POINTER (FSYMEVAL Y))))))
        (DO I (1- (ARRAY-ACTIVE-LENGTH ARRAY)) (1- I) (< I 0)
           (WHO-CALLS-AUX (AR-1 ARRAY I)))
        (RETURN-ARRAY ARRAY))
    NIL)))
(DEFUN WHO-USES (FUNCTION &OPTIONAL (PKG PKG-GLOBAL-PACKAGE)) (WHO-CALLS FUNCTION PKG))

;Print out CALLER if it refers to the symbol in the special variable FUNCTION,
;either as a function call, as a variable reference, or as a constant reference.
;The symbol UNBOUND-FUNCTION is treated specially.
(LOCAL-DECLARE ((SPECIAL FUNCTION))
(DEFUN WHO-CALLS-AUX (CALLER &AUX DEFN)
   ;; Ignore all symbols which are forwarded to others, to avoid duplication.
   (COND ((NOT (= (%P-LDB-OFFSET %%Q-DATA-TYPE CALLER 1) DTP-ONE-Q-FORWARD))
          (COND ((FBOUNDP CALLER)
                 (SETQ DEFN (FSYMEVAL CALLER))
                 ;; Don't be fooled by macros, interpreted or compiled.
                 (AND (LISTP DEFN) (EQ (CAR DEFN) 'MACRO) (SETQ DEFN (CDR DEFN)))
                 (COND ((LISTP DEFN)
                        (WHO-CALLS-AUX-LIST CALLER FUNCTION DEFN))
                       ((= (%DATA-TYPE DEFN) DTP-FEF-POINTER)
                        (WHO-CALLS-AUX-FEF CALLER FUNCTION DEFN)))))
          (DO ((L (PLIST CALLER) (CDDR L)))
              ((NULL L))
            (COND ((= (%DATA-TYPE (CADR L)) DTP-FEF-POINTER)
                   (WHO-CALLS-AUX-FEF (LIST CALLER (CAR L)) FUNCTION (CADR L)))))))))
		 
(DEFUN WHO-CALLS-AUX-FEF (CALLER FUNCTION DEFN &AUX TEM OFFSET SYM)
    (DO ((I %FEF-HEADER-LENGTH (1+ I))
         (LIM (// (FEF-INITIAL-PC DEFN) 2)))
        ((>= I LIM) NIL)
      (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I)
                DTP-EXTERNAL-VALUE-CELL-POINTER)
             (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET DEFN I)
                   SYM (%FIND-STRUCTURE-HEADER TEM)
                   OFFSET (%POINTER-DIFFERENCE TEM SYM))
             (COND ((EQ FUNCTION 'UNBOUND-FUNCTION)
                    (COND ((AND (= OFFSET 2)
                                (NOT (FBOUNDP SYM)))
                           (FORMAT STANDARD-OUTPUT "
~S calls ~S, which is currently an unbound function." CALLER SYM))))
                   ((EQ SYM FUNCTION)
                    (FORMAT T "~%~S ~A ~S."
                            CALLER
                            (NTH OFFSET '("gets the print name of"
					  "uses"
					  "calls"
					  "gets the property list of"))
                            FUNCTION))))
            ((EQ (%P-CONTENTS-OFFSET DEFN I) FUNCTION)
             (FORMAT T "~%~S uses ~S as a constant." CALLER FUNCTION))))
    ;; See if we have a function reference compiled into a misc instruction
    ;; This won't work for LIST and LIST-IN-AREA
    (AND (FEF-CALLS-MISC-FUNCTION DEFN FUNCTION)
	 (FORMAT STANDARD-OUTPUT "
~S calls ~S via a misc-instruction." CALLER FUNCTION)))

;;; See if this FEF uses a certain MISC instruction
(DEFUN FEF-CALLS-MISC-FUNCTION (FEF SYM &AUX TEM INST)
  (AND (GET SYM 'COMPILER:QINTCMP)
       (SETQ TEM (GET SYM 'COMPILER:QLVAL))
       (DO ((MISCINST (+ 15_11 TEM))	;Misc instruction sought
	    (MISCMASK 17777)		;Masks out destination
	    (LONGJUMP 14777)		;First word of 2-word jump instruction
	    (PC (FEF-INITIAL-PC FEF) (1+ PC))
	    (MAXPC (* (FEF-LENGTH FEF) 2)))
	   ((>= PC MAXPC) NIL)
	 (SETQ INST (LOGAND (%P-LDB-OFFSET (IF (ODDP PC) %%Q-HIGH-HALF %%Q-LOW-HALF)
					   FEF (// PC 2))
			    MISCMASK))
	 (COND ((= INST MISCINST) (RETURN T))
	       ((= INST LONGJUMP) (SETQ PC (1+ PC)))))))

;;; Tree-walk CALLER looking for FUNCTION.  If it appears, print a message
;;; to that effect and stop looking.  CALLER should be the function name,
;;; and LIST should be its definition.

(DEFUN WHO-CALLS-AUX-LIST (CALLER FUNCTION DEFN)
    (*CATCH 'WHO-CALLS (WHO-CALLS-AUX-LIST1 DEFN FUNCTION CALLER)))

(DEFUN WHO-CALLS-AUX-LIST1 (SUBLIST FUNCTION CALLER)
    (COND ((SYMBOLP SUBLIST)
	   (COND ((EQ SUBLIST FUNCTION)
		  (FORMAT T "~%~S, an interpreted function uses ~S somehow." CALLER FUNCTION)
		  (*THROW 'WHO-CALLS NIL))))
	  ((LISTP SUBLIST)
	   (WHO-CALLS-AUX-LIST1 (CAR SUBLIST) FUNCTION CALLER)
	   (WHO-CALLS-AUX-LIST1 (CDR SUBLIST) FUNCTION CALLER))))

(DEFUN %MAKE-PAGE-READ-ONLY (P)
  (%CHANGE-PAGE-STATUS P NIL (DPB 2 0603 (LDB %%REGION-MAP-BITS  ;CHANGE MAP-STATUS
                                              (REGION-BITS (%REGION-NUMBER P))))))
;MAR-HACKING FUNCTIONS

(DEFUN CLEAR-MAR ()
  (DO ((P %MAR-LOW (+ P 200)))
      ((> P %MAR-HIGH)) ;TROUBLE WITH NEGATIVE NUMBERS HERE!
    (%CHANGE-PAGE-STATUS P NIL (LDB %%REGION-MAP-BITS
				    (REGION-BITS (%REGION-NUMBER P)))))
  (SETQ %MAR-LOW -1
	%MAR-HIGH -2
	%MODE-FLAGS (%LOGDPB 0 %%M-FLAGS-MAR-MODE %MODE-FLAGS))
  NIL)

;NOT GC-SAFE, ADDITIONAL HAIR REQUIRED, ALSO NEGATIVE NUMBER TROUBLE
(DEFUN SET-MAR (LOCATION CYCLE-TYPE &OPTIONAL (N-WORDS 1))
					;N-WORDS SHOULD DEFAULT TO (SIZE LOCATION)
  (SETQ CYCLE-TYPE
	(SELECTQ CYCLE-TYPE
	   (:READ 1)
	   (:WRITE 2)
	   ((T) 3)  ;Parens around the T so it doesn't look like an otherwise
	   (OTHERWISE (FERROR NIL "~S is not a valid CYCLE-TYPE" CYCLE-TYPE))))
  (CLEAR-MAR) ;CLEAR OLD MAR
  (SETQ %MAR-HIGH (+ (1- N-WORDS) (SETQ %MAR-LOW (%POINTER LOCATION))))
  ;IF MAR'ED PAGES ARE IN CORE, SET UP THEIR TRAPS
  (DO P %MAR-LOW (+ P 200) (> P %MAR-HIGH)
    (%CHANGE-PAGE-STATUS P NIL (DPB 6 0604 (LDB %%REGION-MAP-BITS  ;CHANGE MAP-STATUS
						(REGION-BITS (%REGION-NUMBER P))))))
  (SETQ %MODE-FLAGS (%LOGDPB CYCLE-TYPE %%M-FLAGS-MAR-MODE %MODE-FLAGS))	;ENERGIZE
  T)

(DEFUN MAR-MODE ()
   (LET ((MODE (LDB %%M-FLAGS-MAR-MODE %MODE-FLAGS)))
     (SELECTQ MODE
	(0 'NIL)
	(1 ':READ)
	(2 ':WRITE)
	(3 'T)
	(OTHERWISE (FERROR NIL "The MAR mode, ~O, is invalid." MODE)))))

; This function sets up a 4 by 220 table, useful for keyboard dispatches.
; The table is set up from a list of four elements, called rows: one for each
; setting of the buckey bits, as follows: 0 = none, 1 = control, 2 = meta, 3 = control-meta.
; Each row is walked down, and as each element is reached, it is stored in the array, unless
; its car is recognized as a special function.
; Currently implemented special functions are:
;     (*REPEAT <times> <thing) --  <thing> is stored <times> times.
;     (*REPEAT-EVAL <times> <thing>) -- like *REPEAT except that <thing> gets EVALed
;                              before being stored.  The special variable SI:RPCNT will
;                              be set to 0 on the first iteration and incremented by 1
;                              throughout, like a MIDAS repeat loop.

(DECLARE (SPECIAL RPCNT))

(DEFUN SETUP-KEYBOARD-DISPATCH-TABLE (TABLE LISTS &AUX ENTRY TEM)
   (DO ((LISTS1 LISTS (CDR LISTS1))
	(IDX1 0 (1+ IDX1)))
       ((NULL LISTS1))
     (DO ((LIST (CAR LISTS1) (CDR LIST))
	  (IDX2 0))				  ;THE AMOUNT THIS IS INCREMENTED DEPENDS!!
	 ((NULL LIST)
	  (COND ((NOT (= IDX2 220))
		 (FERROR NIL "Row ~S was ~S long, instead of 220."
			 IDX1 IDX2))))
       (SETQ ENTRY (CAR LIST))
       (COND ((ATOM ENTRY)
	      (AS-2 ENTRY TABLE IDX1 IDX2)
	      (SETQ IDX2 (1+ IDX2)))
	     ((EQ (FIRST ENTRY) ':REPEAT)
	      (SETQ TEM (SECOND ENTRY))
	      (SETQ ENTRY (THIRD ENTRY))
	      (DO I 0 (1+ I) (= I TEM)
		(AS-2 ENTRY TABLE IDX1 IDX2)
		(SETQ IDX2 (1+ IDX2))))
	     ((EQ (FIRST ENTRY) ':REPEAT-EVAL)
	      (SETQ TEM (SECOND ENTRY))
	      (SETQ ENTRY (THIRD ENTRY))
	      (DO RPCNT 0 (1+ RPCNT) (= RPCNT TEM)
		(AS-2 (EVAL ENTRY) TABLE IDX1 IDX2)
		(SETQ IDX2 (1+ IDX2))))
	     ((EQ (FIRST ENTRY) ':EVAL)
	      (AS-2 (EVAL (SECOND ENTRY)) TABLE IDX1 IDX2)
	      (SETQ IDX2 (1+ IDX2)))
	     (T
	      (AS-2 ENTRY TABLE IDX1 IDX2)
	      (SETQ IDX2 (1+ IDX2)))))))

(DEFUN YES-OR-NO-P (&OPTIONAL MESSAGE (STREAM QUERY-IO))
    (COND ((AND MESSAGE (NOT (STRINGP MESSAGE)))
           ;; Temporary compatibility feature.
           (COND ((STRINGP STREAM)
                  (PSETQ MESSAGE STREAM STREAM MESSAGE))
                 (T (SETQ STREAM MESSAGE MESSAGE NIL)))))
    (PROG (STRING)
       (AND MESSAGE (FORMAT STREAM "~&~A" MESSAGE))
     RETRY
       (SETQ STRING (STRING-UPCASE (STRING-TRIM '(40 211 42 56) ;sp, tab, dot, double-quote
						(READLINE STREAM))))
       (COND ((EQUAL STRING "YES")
	      (RETURN T))
	     ((EQUAL STRING "NO")
	      (RETURN NIL))
	     (T (AND MESSAGE (FORMAT STREAM "~&~A" MESSAGE))
		(FORMAT STREAM "Please type /"Yes/" or /"No/". ")
		(GO RETRY)))))

(DEFUN PAIRLIS (VARS VALS &AUX ALST)
       (SETQ ALST (MAKE-LIST DEFAULT-CONS-AREA (LENGTH VARS)))
       (DO ((VARS VARS (CDR VARS))
            (VALS VALS (CDR VALS))
            (TEM ALST (CDR TEM)))
           ((NULL VARS) ALST)
           (RPLACA TEM (CONS (CAR VARS) (CAR VALS)))))

(DEFUN DEL-IF-NOT (PRED LIST)
       (PROG (LST OLST)
        A    (COND ((ATOM LIST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LIST)))
                   (T
                    (SETQ LIST (CDR LIST))
                    (GO A)))
             (SETQ OLST (SETQ LST LIST))
        B    (SETQ LST (CDR LST))
             (COND ((ATOM LST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LST))
                    (SETQ OLST LST))
                   (T
                    (RPLACD OLST (CDR LST))))
             (GO B)))

(DEFUN DEL-IF (PRED LIST)
       (PROG (LST OLST)
        A    (COND ((ATOM LIST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LIST))
                    (SETQ LIST (CDR LIST))
                    (GO A)))
             (SETQ OLST (SETQ LST LIST))
        B    (SETQ LST (CDR LST))
             (COND ((ATOM LST) (RETURN LIST))
                   ((FUNCALL PRED (CAR LST))
                    (RPLACD OLST (CDR LST)))
                   (T
                    (SETQ OLST LST)))
             (GO B)))

(DEFUN HAIPART (X N &AUX TEM)
  ;; Get number of significant bits
  (SETQ TEM (HAULONG (SETQ X (ABS X))))
  (COND ;; Positive N means get high N bits, or as many as there are
	((> N 0) (SETQ TEM (- N TEM))	;minus number of low bits to discard
		 (COND ((< TEM 0) (ASH X TEM))
		       (T X)))
	;; Zero N means return no bits
	((= N 0) 0)
	;; Negative N means get low -N bits, or as many as there are
	((< (SETQ N (MINUS N)) TEM)
	 (\ X (ASH 1 N)))
	(T X)))

(DEFUN PROGV (VARS VALS &QUOTE &REST STUFF)
  (DO-NAMED PROGV
	((VARS VARS (CDR VARS))
	 (VALS VALS (CDR VALS)))
	((NULL VARS)
	 (DO ((STUFF STUFF (CDR STUFF)))
	     (NIL)
	   (COND ((NULL (CDR STUFF))			;Pass multiple values (compiled anyway)
		  (RETURN-FROM PROGV (EVAL (CAR STUFF))))
		 ((EVAL (CAR STUFF))))))
    (BIND (VALUE-CELL-LOCATION (CAR VARS)) (CAR VALS))))


;;; This should really be fixed to expand more than just top level functions.
(DEFUN MEXP NIL
    (DO ((TEM))
	(())
      (FORMAT T "~2%Macro form ")
      (SETQ TEM (READ-FOR-TOP-LEVEL))
      (AND (SYMBOLP TEM) (RETURN NIL))
      (DO EXP (MACROEXPAND-1 TEM) (MACROEXPAND-1 EXP) (EQ EXP TEM)
	;(FORMAT T "  ~S" (SETQ TEM EXP))
	(PRINC "  ")
	(GRIND-TOP-LEVEL (SETQ TEM EXP))
	)))


;; STATUS AND SSTATUS 

(DECLARE (SPECIAL STATUS-FEATURE-LIST STATUS-STATUS-LIST STATUS-SSTATUS-LIST))

;;; These symbols are all on KWDPKG.
(SETQ STATUS-FEATURE-LIST
      '(SORT FASLOAD STRING NEWIO ROMAN TRACE GRINDEF GRIND LISPM))

(SETQ STATUS-STATUS-LIST '(FEATURE FEATURES NOFEATURE STATUS SSTATUS TABSIZE USERID))

(SETQ STATUS-SSTATUS-LIST '(FEATURE NOFEATURE))

(DEFUN RETURN-STATUS (STATUS-LIST ITEM)
       (COND ((NULL ITEM) STATUS-LIST)
	     (T (NOT (NULL (MEMQ ITEM STATUS-LIST))))))

(DEFUN STATUS (&QUOTE STATUS-FUNCTION &OPTIONAL ITEM)
       (SELECTQ STATUS-FUNCTION
		((FEATURE FEATURES) (RETURN-STATUS STATUS-FEATURE-LIST ITEM))
		(NOFEATURE (COND ((NULL ITEM)
                                  (FERROR NIL "Too few args to STATUS NOFEATURE"))
                                 (T (NOT (RETURN-STATUS STATUS-FEATURE-LIST ITEM)))))
		(STATUS (RETURN-STATUS STATUS-STATUS-LIST ITEM))
		(SSTATUS (RETURN-STATUS STATUS-SSTATUS-LIST ITEM))
		(TABSIZE 8)
		(USERID USER-ID)
		(OTHERWISE (FERROR NIL "~S is not a legal STATUS request" STATUS-FUNCTION))))

(DEFUN SSTATUS (&QUOTE STATUS-FUNCTION ITEM)
       (SELECTQ STATUS-FUNCTION
		(FEATURE (COND ((NOT (MEMQ ITEM STATUS-FEATURE-LIST))
                                (SETQ STATUS-FEATURE-LIST
                                      (CONS ITEM STATUS-FEATURE-LIST))))
				 ITEM)
		(NOFEATURE (COND ((MEMQ ITEM STATUS-FEATURE-LIST)
                                  (SETQ STATUS-FEATURE-LIST
                                        (DELQ ITEM STATUS-FEATURE-LIST))))
				   ITEM)
		(OTHERWISE (FERROR NIL "~S is not a legal SSTATUS request" STATUS-FUNCTION))))

;;; Describe all files "related" to this file name
(DEFUN DESCRIBE-FILE (FILE-NAME &AUX USER-FILE-SYMBOL QFASL-FILE-SYMBOL FILE-GROUP-SYMBOL)
  (SETQ FILE-NAME (FS:FILE-PARSE-NAME FILE-NAME))
  (SETQ USER-FILE-SYMBOL (INTERN-LOCAL-SOFT (FUNCALL FILE-NAME ':STRING-FOR-PRINTING)
					    PKG-FILE-PACKAGE))
  (MULTIPLE-VALUE (QFASL-FILE-SYMBOL FILE-GROUP-SYMBOL)
    (FS:GET-FILE-SYMBOLS (FUNCALL FILE-NAME ':COPY-WITH-TYPE ':QFASL)))
  (AND USER-FILE-SYMBOL (DESCRIBE-FILE-1 USER-FILE-SYMBOL))
  (AND (NEQ QFASL-FILE-SYMBOL USER-FILE-SYMBOL) (DESCRIBE-FILE-1 QFASL-FILE-SYMBOL))
  (AND (NEQ FILE-GROUP-SYMBOL USER-FILE-SYMBOL) (DESCRIBE-FILE-1 FILE-GROUP-SYMBOL))
  NIL)

;;; Describe a particular file-symbol
(DEFUN DESCRIBE-FILE-1 (FILE-SYMBOL &AUX TEM IDX VERSION CREATION-DATE)
  (AND (SETQ TEM (GET FILE-SYMBOL ':PACKAGE))
       (FORMAT STANDARD-OUTPUT "~%File ~A is in package ~A." FILE-SYMBOL TEM))
  (DOLIST (PKG-ID (GET FILE-SYMBOL ':FILE-ID-PACKAGE-ALIST))
    (SETQ TEM (CADR PKG-ID))	;The FILE-ID for this package
    (SETQ IDX (STRING-SEARCH-CHAR #\SP TEM))
    (SETQ VERSION (SUBSTRING TEM 0 IDX)
	  CREATION-DATE (NSUBSTRING TEM (1+ IDX) (STRING-LENGTH TEM)))
    (COND ((EQUAL VERSION "-1")
	   (FORMAT STANDARD-OUTPUT "~%Version of file ~A in package ~A was created ~A."
                                        FILE-SYMBOL (CAR PKG-ID) CREATION-DATE))
	  ((FORMAT STANDARD-OUTPUT "~%Version of file ~A in package ~A is ~A, created ~A."
                                        FILE-SYMBOL (CAR PKG-ID) VERSION CREATION-DATE))))
  NIL)

;These are here because they must be loaded after the package system is operational

;; Give the function definition corresponding to a function specifier.
;; A function-specifier is just a way of talking about a function
;; for purposes other than applying it.  It can be a symbol, in which case
;; the function cell of the symbol is used.  Or it can be a list of one of
;; these formats:
;; (:METHOD class-name operation) refers to the method in that class for
;;   that operation; this works for both Class methods and Flavor methods.
;;   In the case of Flavor methods, the specification may also be of the form
;;   (:METHOD flavor-name time operation).
;; (:INSTANCE-METHOD exp operation).  exp should evaluate to an DTP-INSTANCE.
;;   Reference is then to the operation directly on that instance.
;; (:PROPERTY symbol property) refers to (GET symbol property).
;; One place you can use a function specifier is in DEFUN.
;; Because of this, for Maclisp compatibility, a list whose car is
;; not recognized is taken to be a list of a symbol and a property.

(DEFUN FDEFINITION (FUNCTION-SPEC)
    (PROG ()
          (CHECK-ARG FUNCTION-SPEC (OR (LISTP FUNCTION-SPEC) (SYMBOLP FUNCTION-SPEC))
                     "a list or a symbol")
          (AND (SYMBOLP FUNCTION-SPEC)
               (RETURN (FSYMEVAL FUNCTION-SPEC)))
          (RETURN
            (SELECTQ (CAR FUNCTION-SPEC)
              (:METHOD
		 (COND ((GET (CADR FUNCTION-SPEC) 'FLAVOR)
			(FSYMEVAL (FLAVOR-METHOD-SYMBOL FUNCTION-SPEC)))
		       ((FSYMEVAL
			 (<- (SYMEVAL (CADR FUNCTION-SPEC))
			     ':METHOD-FOR
			     (CADDR FUNCTION-SPEC))))))
              (:INSTANCE-METHOD (<- (CLASS (EVAL (CADR FUNCTION-SPEC)))
                                    ':METHOD-FOR
                                    (CADDR FUNCTION-SPEC)))
              (:PROPERTY (APPLY 'GET (CDR FUNCTION-SPEC)))
              (OTHERWISE (APPLY 'GET FUNCTION-SPEC))))))

;; Is a function specifier defined?  A generalization of FBOUNDP.
(DEFUN FDEFINEDP (FUNCTION-SPEC)
    (COND ((SYMBOLP FUNCTION-SPEC)
           (FBOUNDP FUNCTION-SPEC))
	  ((EQ (CAR FUNCTION-SPEC) ':METHOD)
	   (COND ((GET (CADR FUNCTION-SPEC) 'FLAVOR)
		  (FBOUNDP (FLAVOR-METHOD-SYMBOL FUNCTION-SPEC)))
		 ((AND (BOUNDP (CADR FUNCTION-SPEC))
		       (<- (SYMEVAL (CADR FUNCTION-SPEC))
			   ':METHOD-FOR
			   (CADDR FUNCTION-SPEC))))))
          ((EQ (CAR FUNCTION-SPEC) ':INSTANCE-METHOD)
           (<- (CLASS (EVAL (CADR FUNCTION-SPEC)))
               ':METHOD-FOR
               (CADDR FUNCTION-SPEC)))
          (T (FDEFINITION FUNCTION-SPEC))))  ;Assumed to be property list

(DECLARE (SPECIAL FDEFINE-FILE-SYMBOL INHIBIT-FDEFINE-WARNINGS))
;(OR (BOUNDP 'FDEFINE-FILE-SYMBOL)  ;This loses since FASLOAD binds this variable.
;    (SETQ FDEFINE-FILE-SYMBOL NIL))
(OR (BOUNDP 'INHIBIT-FDEFINE-WARNINGS)
    (SETQ INHIBIT-FDEFINE-WARNINGS NIL))

(DEFUN FDEFINE (FUNCTION-SPEC DEFINITION &OPTIONAL CAREFULLY-FLAG FORCE-FLAG
                &AUX TEM TEM1 (PACKAGE-PROBLEM NIL) (MULTI-FILE-PROBLEM NIL))
"Alter the function definition of a function specifier.
CAREFULLY-FLAG means save the old definition, when possible,
and query about crossing package lines (but FORCE-FLAG inhibits this).
If FDEFINE-FILE-SYMBOL is non-NIL, then it is the file which this definition
was read from, and we make a note of that fact when possible."
  (PROG FDEFINE ()
    (CHECK-ARG FUNCTION-SPEC (OR (LISTP FUNCTION-SPEC) (SYMBOLP FUNCTION-SPEC))
               "a list or a symbol")
    (COND ((SYMBOLP FUNCTION-SPEC)
           (OR FORCE-FLAG (NOT CAREFULLY-FLAG)
	       INHIBIT-FDEFINE-WARNINGS
               (NULL (SETQ TEM (CDR (PACKAGE-CELL-LOCATION FUNCTION-SPEC))))
               (EQ TEM PACKAGE)
               (EQ (SETQ TEM1 (PKG-EXTERNAL-LIST PACKAGE)) T)
               (MEM #'STRING-EQUAL FUNCTION-SPEC TEM1)
	       (SETQ PACKAGE-PROBLEM TEM))
	   ;; Save previous definition if desired and there was one.
           (COND ((AND CAREFULLY-FLAG (FBOUNDP FUNCTION-SPEC))
		  (SETQ TEM (FSYMEVAL FUNCTION-SPEC))
		  ;; If it's traced, get the pre-traced definition to save.
		  (ERRSET
		    (AND (LISTP TEM) (EQ (CAR TEM) 'NAMED-LAMBDA)
			 (LISTP (CADR TEM))
			 (ASSQ 'TRACE (CDADR TEM))
			 (SETQ TEM (FDEFINITION (CADR (ASSQ 'TRACE (CDADR TEM))))))
		    NIL)
                  (AND (LISTP TEM)
		       (NOT (AND (EQ (CAR TEM) 'MACRO)
				 (= (%DATA-TYPE (CDR TEM)) DTP-FEF-POINTER)))
                       (PUTPROP FUNCTION-SPEC TEM ':PREVIOUS-EXPR-DEFINITION))
                  (PUTPROP FUNCTION-SPEC TEM ':PREVIOUS-DEFINITION)))
           (AND (BOUNDP 'FDEFINE-FILE-SYMBOL)  ;Just initializing it doesnt win since it is
		FDEFINE-FILE-SYMBOL	       ; bound by FASLOAD.
		(FBOUNDP 'FORMAT)	       ;dont bomb during cold load
					       ; (redefining accessor methods)
		(SETQ TEM (GET FUNCTION-SPEC ':SOURCE-FILE-NAME))
		(NEQ TEM FDEFINE-FILE-SYMBOL)
		(NOT (MEMQ TEM (GET FDEFINE-FILE-SYMBOL ':REDEFINES-FILES)))
		(NOT INHIBIT-FDEFINE-WARNINGS)
		(SETQ MULTI-FILE-PROBLEM TEM))
	   ;; If there are any problems, consult the user before proceeding
	   (COND ((OR PACKAGE-PROBLEM MULTI-FILE-PROBLEM)
		  (FORMAT QUERY-IO
"~&WARNING: Function ~S being illegally ~:[~;re~]defined~:[~; by file ~:*~A~].
~:[~;The function belongs to the ~:*~A package.~]~
~:[~;~&It was previously defined by file ~:*~A.~]  OK? (type Y, N, E, or P) "
			  FUNCTION-SPEC (FBOUNDP FUNCTION-SPEC) FDEFINE-FILE-SYMBOL
			  PACKAGE-PROBLEM MULTI-FILE-PROBLEM)
		  (FUNCALL QUERY-IO ':CLEAR-INPUT)
		  (DO () (NIL)
		    (SELECTQ (CHAR-UPCASE (FUNCALL QUERY-IO ':TYI))
		      ((#/Y #/T #\SP) (PRINC "Yes." QUERY-IO) (RETURN))
		      ((#/E) (PRINC "Error." QUERY-IO)
		             (RETURN (FDEFINE (CERROR T NIL ':ILLEGAL-FUNCTION-DEFINITION
 "Function ~S being illegally ~:[~;re~]defined~:[~; by file ~:*~A~].
~:[~;The function belongs to the ~:*~A package.~]~
~:[~;~&It was previously defined by file ~:*~A.~]"
						      FUNCTION-SPEC (FBOUNDP FUNCTION-SPEC)
						      FDEFINE-FILE-SYMBOL
						      PACKAGE-PROBLEM MULTI-FILE-PROBLEM)
					      DEFINITION CAREFULLY-FLAG FORCE-FLAG)))
		      ((#/N #\RUBOUT) (PRINC "No." QUERY-IO) (RETURN-FROM FDEFINE NIL))
		      (#/P (PRINC "Proceed." QUERY-IO)
		           (AND MULTI-FILE-PROBLEM
				(PUSH MULTI-FILE-PROBLEM
				      (GET FDEFINE-FILE-SYMBOL ':REDEFINES-FILES)))
			   (RETURN))
		      ((#/? #\HELP) (PRINC "
Type Y to proceed to redefine the function, N to not redefine it, E to go into the
 error handler, or P to proceed and not ask in the future (for this pair of files): "
					   QUERY-IO))
		      (OTHERWISE (FORMAT QUERY-IO "~& Type Y, N, E, P or [HELP]: "))))))
	   (RECORD-SOURCE-FILE-NAME FUNCTION-SPEC)
           (FSET FUNCTION-SPEC DEFINITION)
	   (RETURN-FROM FDEFINE T))
          (T
           (RETURN-FROM FDEFINE
	     (SELECTQ (CAR FUNCTION-SPEC)
	       (:METHOD
		(LET ((CS (CADR FUNCTION-SPEC))
		      (OP (CADDR FUNCTION-SPEC)))
		  (COND ((GET CS 'FLAVOR)
			 (FDEFINE-FLAVOR FUNCTION-SPEC DEFINITION CAREFULLY-FLAG FORCE-FLAG))
			((NOT (CLASS-SYMBOLP CS))
			 (FERROR NIL "Attempt to define method on ~S, which is not a CLASS"
				 CS))
			(T
			 (LET ((MN (MAKE-METHOD-NAME CS OP)))
			   (COND ((FDEFINE MN DEFINITION CAREFULLY-FLAG FORCE-FLAG)
				  ;; Can't send message because this has to work during
				  ;; loadup before messages work.
				  (ADD-METHOD CS
					      (SYMEVAL-IN-CLOSURE (SYMEVAL CS)
								  'CLASS-METHOD-SYMBOL)
					      OP
					      MN)
				  T)))))))
	       (:INSTANCE-METHOD
		(LET ((INST (EVAL (CADR FUNCTION-SPEC)))
		      (OP (CADDR FUNCTION-SPEC)))
		  (LET ((MN (MAKE-INSTANCE-METHOD-NAME INST OP)))
		    (COND ((FDEFINE MN DEFINITION CAREFULLY-FLAG FORCE-FLAG)
			   (ADD-INSTANCE-METHOD INST OP MN)
			   T)))))
	       (:PROPERTY
		(PUTPROP (CADR FUNCTION-SPEC) DEFINITION (CADDR FUNCTION-SPEC))
		T)
	       (OTHERWISE
		(PUTPROP (CAR FUNCTION-SPEC) DEFINITION (CADR FUNCTION-SPEC))
		T)))))))

;; This is an old name which everyone uses.
;; It must be defined after FDEFINE since we are clobbering a
;; temporary patch which is made so that FASLOAD can load before QMISC is loaded.

(DEFUN FSET-CAREFULLY (FUNCTION-SPEC DEFINITION &OPTIONAL FORCE-FLAG)
    (FDEFINE FUNCTION-SPEC DEFINITION T FORCE-FLAG))

;Restore the saved previous function definition of a symbol.
(DEFUN UNDEFUN (SYMBOL &AUX TEM)
    (SETQ TEM (GET SYMBOL ':PREVIOUS-DEFINITION))
    (OR TEM (FERROR NIL "~S has no previous function definition" SYMBOL))
    (FSET-CAREFULLY SYMBOL TEM T))

;Get the documentation string for a function
;NIL if not defined or no documentation
(DEFUN FUNCTION-DOCUMENTATION (FCN)
  (COND ((SYMBOLP FCN)
	 (OR (AND (FBOUNDP FCN) (FUNCTION-DOCUMENTATION (FSYMEVAL FCN)))
	     (GET FCN ':DOCUMENTATION)))
	((LISTP FCN)
	 (COND ((MEMQ (CAR FCN) '(LAMBDA NAMED-LAMBDA))
		(AND (EQ (CAR FCN) 'NAMED-LAMBDA)
		     (SETQ FCN (CDR FCN)))
		(SETQ FCN (CDDR FCN))
		(AND (LISTP (CAR FCN))
		     (EQ (CAAR FCN) 'DECLARE)
		     (SETQ FCN (CDR FCN)))
		(AND (CDR FCN)
		     (STRINGP (CAR FCN))
		     (CAR FCN)))
	       ((EQ (CAR FCN) 'MACRO)
		(FUNCTION-DOCUMENTATION (CDR FCN)))
	       (T
		(AND (FDEFINEDP FCN) (FUNCTION-DOCUMENTATION (FDEFINITION FCN))))))
	((= (%DATA-TYPE FCN) DTP-FEF-POINTER)
	 (CADR (ASSQ ':DOCUMENTATION (FUNCTION-DEBUGGING-INFO FCN))))))

;These are for reading in QCOM, and the like
(DEFUN ASSIGN-ALTERNATE (X)
   (PROG NIL 
    L	(COND ((NULL X)(RETURN NIL)))
	(SET (CAR X) (CADR X))
	(SETQ X (CDDR X))
	(GO L)))

(DEFUN GET-ALTERNATE (X)
   (PROG (Y)
    L	(COND ((NULL X)(RETURN (REVERSE Y))))
	(SETQ Y (CONS (CAR X) Y))
	(SETQ X (CDDR X))
	(GO L)))

(DEFUN ASSIGN-VALUES (INPUT-LIST &OPTIONAL (SHIFT 0) (INIT 0) (DELTA 1))
   (PROG ()
LP	(COND ((NULL INPUT-LIST)(RETURN INIT)))
	(SET (CAR INPUT-LIST) (LSH INIT SHIFT))
	(SETQ INPUT-LIST (CDR INPUT-LIST))
	(SETQ INIT (+ INIT DELTA))
	(GO LP)))

(DEFUN ASSIGN-VALUES-INIT-DELTA (INPUT-LIST SHIFT INIT DELTA)
    (PROG NIL 
LP	(COND ((NULL INPUT-LIST) (RETURN INIT)))
	(SET (CAR INPUT-LIST) (LSH INIT SHIFT))
	(SETQ INPUT-LIST (CDR INPUT-LIST))
	(SETQ INIT (+ INIT DELTA))
	(GO LP)))

;(CALL function arg-desc-1 arg-data-1 arg-desc-2 arg-data-2 ...)
;The first argument is a function to call.
;The remaining arguments are in pairs, consisting of
;a descriptor arg and a data arg.
;The descriptor arg says what to do with the data arg.
;The descriptor arg value should be either a keyword or
;a list of keywords, the allowed keywords being :SPREAD and :OPTIONAL.
;:SPREAD means that the data argument is a list of arguments
;rather than a single argument.
;:OPTIONAL means that the data argument can be ignored if
;the function being called doesn't ask for it.
;After the first :OPTIONAL, all args supplied are considered optional.

(DEFUN CALL (FN &REST ALTERNATES
		&AUX (MAX-ARGS 100) (ARGS-INF (ARGS-INFO FN)))
    (AND (ZEROP (LDB %%ARG-DESC-QUOTED-REST ARGS-INF))
         (ZEROP (LDB %%ARG-DESC-EVALED-REST ARGS-INF))
         (SETQ MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS ARGS-INF)))
    (%OPEN-CALL-BLOCK FN 0 4)
    (DO ((Y ALTERNATES (CDDR Y)) (OPTIONAL-FLAG) (SPREAD-FLAG NIL NIL)) ((NULL Y))
	(COND ((AND (SYMBOLP (CAR Y)) (NOT (NULL (CAR Y))))
	       (SELECTQ (CAR Y)
		   (:SPREAD (SETQ SPREAD-FLAG T))
		   (:OPTIONAL (SETQ OPTIONAL-FLAG T))
		   (OTHERWISE (FERROR NIL "Invalid CALL keyword ~S" (CAR Y)))))
	      (T (DO X (CAR Y) (CDR X) (NULL X)
		     (SELECTQ (CAR X)
			 (:SPREAD (SETQ SPREAD-FLAG T))
			 (:OPTIONAL (SETQ OPTIONAL-FLAG T))
			 (OTHERWISE (FERROR NIL "Invalid CALL keyword ~S" (CAR X)))))))
	(AND OPTIONAL-FLAG (<= MAX-ARGS 0)
	     (RETURN NIL))
	(COND (SPREAD-FLAG
	       (DO X (CADR Y) (CDR X) (OR (NULL X) (AND OPTIONAL-FLAG (<= MAX-ARGS 0)))
		   (%ASSURE-PDL-ROOM 1)
		   (%PUSH (CAR X))
		   (SETQ MAX-ARGS (1- MAX-ARGS))))
	      (T (%ASSURE-PDL-ROOM 1)
		 (%PUSH (CADR Y))
		 (SETQ MAX-ARGS (1- MAX-ARGS)))))
    (%ACTIVATE-OPEN-CALL-BLOCK))

;COMPILER-LET is just like LET when interpreted.
;But when compiled, it binds at compile time (on both passes).
;It is not a macro, for the sake of the compiler's definition of it,
;and for the sake of COMPILE-DRIVER.
(DEFUN COMPILER-LET (&QUOTE BINDLIST &REST BODY)
    (EVAL `(LET ,BINDLIST . ,BODY)))

(DEFUN DISK-RESTORE (&OPTIONAL PARTITION)
    (LET ((L (DISK-RESTORE-DECODE PARTITION)))
	 (AND (YES-OR-NO-P "Do you really want to reload? (Yes or No)")
	      (%DISK-RESTORE (CAR L) (CADR L)))))

(DEFVAR WHO-LINE-JUST-COLD-BOOTED-P NIL) ;Set to T upon cold boot for who-line's benefit

(DEFUN DISK-SAVE (PARTITION)
  (LET* ((L (DISK-RESTORE-DECODE PARTITION))
	 (PART-NAME (STRING-APPEND (LDB 0010 (CADR L)) (LDB 1010 (CADR L))
				   (LDB 0010 (CAR L)) (LDB 1010 (CAR L))))
	 PART-SIZE)
    (COND ((YES-OR-NO-P (FORMAT NIL "Do you really want to clobber partition ~A? (Yes or No)"
				    PART-NAME))
	   (GET-NEW-SYSTEM-VERSION)	;Update system version ID
	   (UPDATE-PARTITION-COMMENT PART-NAME SYSTEM-VERSION-STRING 0)
	   (MULTIPLE-VALUE (NIL PART-SIZE) (FIND-DISK-PARTITION PART-NAME))
	   (LOGOUT)
	   (CHAOS:RESET)
	   
	   ;Cause cold boot initializations to happen when rebooted
	   (RESET-INITIALIZATIONS 'COLD-INITIALIZATION-LIST)
	   (SETQ WHO-LINE-JUST-COLD-BOOTED-P T)
	   
	   ;Determine size of storage used.  Hope no region-consing happens
	   ;after this (presumably no one is doing anything).  Just to be
	   ;sure, we inhibit scheduling.
	   (DO ((INHIBIT-SCHEDULING-FLAG T)
		(REGION 0 (1+ REGION))
		(MAX-ADDR 0))
	       ((= REGION (REGION-LENGTH REGION-LENGTH))
		(SETQ CURRENT-PROCESS NIL)	;Prevent error message upon coming up
		(SETQ MAX-ADDR (// MAX-ADDR PAGE-SIZE))	;Number of pages
		(AND (> MAX-ADDR PART-SIZE) (FERROR NIL "Cannot save, partition too small"))
		;; Store the size in words rather than pages.  But don't get a bignum!
		(STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-VALID-SIZE) (LSH MAX-ADDR 8))
		(DO I 600 (1+ I) (= I 640)	;Clear the disk error log
		  (%P-STORE-TAG-AND-POINTER I 0 0))
		(%DISK-SAVE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)
			    (CAR L) (CADR L)))
	     (COND ((NOT (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION))
			    %REGION-SPACE-FREE))
		    (SETQ MAX-ADDR (MAX MAX-ADDR (+ (REGION-ORIGIN REGION)
						    (REGION-LENGTH REGION)))))))))))

(DEFUN DISK-RESTORE-DECODE (PARTITION &AUX LOW-16-BITS HI-16-BITS)
    (COND ((NULL PARTITION)
	   (SETQ LOW-16-BITS 0 HI-16-BITS 0))
	  ((NUMBERP PARTITION)
	   (SETQ LOW-16-BITS (+ #/L (LSH #/O 8)))
	   (SETQ HI-16-BITS (+ #/D (LSH (+ #/0 PARTITION) 8))))
	  ((STRINGP PARTITION)
	   (SETQ LOW-16-BITS (+ (AR-1 PARTITION 0) (LSH (AR-1 PARTITION 1) 8)))
	   (SETQ HI-16-BITS (+ (AR-1 PARTITION 2) (LSH (AR-1 PARTITION 3) 8))))
	  (T (FERROR NIL "~S is not a valid partition name" PARTITION)))
    (LIST HI-16-BITS LOW-16-BITS))

;This is a temporary function, which turns on the "extra-pdl" feature
(DEFUN NUMBER-GC-ON (&OPTIONAL (ON-P T))
  (SETQ NUMBER-CONS-AREA
        (COND (ON-P EXTRA-PDL-AREA)
              (T WORKING-STORAGE-AREA))))

(DEFUN PRINT-MODIFICATION-RECORD ()
    (FORMAT T "~&System:~18TModification:")
    (DO ((L (REVERSE SYSTEM-MODIFICATION-RECORD) (CDR L)))
        ((NULL L))
      (FORMAT T "~%~A ~18T~A" (CAAR L) (CADAR L)))
    (TERPRI))

(DEFUN GET-FROM-ALTERNATING-LIST (L KEY) 
"Retreive associated item from an alternating list
Like GET, but no initial CAR"
  (PROG NIL
     L	(COND ((NULL L)(RETURN NIL))
              ((EQ KEY (CAR L))
               (RETURN (CADR L))))
     	(SETQ L (CDDR L))
        (GO L)))

(DEFUN PUT-ON-ALTERNATING-LIST (ITEM L KEY)
"Put an item on an alternating association list
Modifies the current association, if any.
Otherwise adds one to the head of the list.  
Returns the augmented list as value.
The user should alway use this value unless he is
certain there is a current association"
  (PROG (PNTR)
	(SETQ PNTR L)
     L  (COND ((NULL L) (RETURN (CONS KEY (CONS ITEM L))))
	      ((EQ KEY (CAR L))
	       (RPLACA (CDR L) ITEM)
	       (RETURN L)))
	(SETQ L (CDDR L))
	(GO L)))

(DEFUN READ-METER (NAME)
"Read the value of the A Memory metering location
specified by the argument"
   (LET ((A-OFF (+ %COUNTER-BLOCK-A-MEM-ADDRESS
		   (OR (FIND-POSITION-IN-LIST NAME A-MEMORY-COUNTER-BLOCK-NAMES)
		       (FERROR NIL "~S is not a valid counter name" NAME)))))
      (WITHOUT-INTERRUPTS	;Try not to get inconsistent numbers
	  (DPB (%P-LDB 2020 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF))
	       2020
	       (%P-LDB 0020 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF))))))

(DEFUN WRITE-METER (NAME VAL)
"Set  the value of the A Memory metering location
specified by the first argument to the second argument"
    (LET ((A-OFF (+ %COUNTER-BLOCK-A-MEM-ADDRESS
		   (OR (FIND-POSITION-IN-LIST NAME A-MEMORY-COUNTER-BLOCK-NAMES)
		       (FERROR NIL "~S is not a valid counter name" NAME)))))
     (WITHOUT-INTERRUPTS
	 (%P-DPB (LDB 2020 VAL)
		 2020
		 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF))
	 (%P-DPB VAL
		 0020
		 (+ A-MEMORY-VIRTUAL-ADDRESS A-OFF)))))

;;; Change what an indirect array points at, or what its offset is.
(DEFUN CHANGE-INDIRECT-ARRAY (ARRAY TYPE DIMLIST DISPLACED-P INDEX-OFFSET
			      &AUX INDEX-LENGTH NDIMS INDIRECT-LENGTH TEM
				   OLD-NDIMS OLD-INDIRECT-LENGTH)
  (CHECK-ARG ARRAY ARRAYP "an array")
  (OR (= (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0) 1)
      (FERROR NIL "~S is not a displaced array" ARRAY))
  (CHECK-ARG DISPLACED-P ARRAYP "an array to indirect to")
  (CHECK-ARG TYPE		;TEM gets the numeric array type
	     (SETQ TEM (COND ((NUMBERP TYPE) (LDB %%ARRAY-TYPE-FIELD TYPE))
			     ((FIND-POSITION-IN-LIST TYPE ARRAY-TYPES))))
	     "an array type")
  (SETQ TYPE TEM)
  (COND ((NLISTP DIMLIST)
	 (SETQ NDIMS 1 INDEX-LENGTH (ATOMEVAL DIMLIST)))
	(T (SETQ NDIMS (LENGTH DIMLIST)
		 INDEX-LENGTH (LIST-PRODUCT DIMLIST))))
  (SETQ INDIRECT-LENGTH (IF INDEX-OFFSET 3 2)
	OLD-NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)
	OLD-INDIRECT-LENGTH (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))
  (OR (= NDIMS OLD-NDIMS)
      (FERROR NIL "Illegal attempt to change the number of dimensions from ~D to ~D"
	          OLD-NDIMS NDIMS))
  (OR (= INDIRECT-LENGTH OLD-INDIRECT-LENGTH)
      (FERROR NIL "Illegal attempt to add or remove index-offset"))
  (%P-DPB-OFFSET TYPE %%ARRAY-TYPE-FIELD ARRAY 0)
  (AND (LISTP DIMLIST)
       (DO ((I 1 (1+ I))
	    (N NDIMS (1- N)))
	   ((< N 2))
	 (%P-STORE-CONTENTS-OFFSET (ATOMEVAL (CAR DIMLIST)) ARRAY I)
	 (SETQ DIMLIST (CDR DIMLIST))))
  (%P-STORE-CONTENTS-OFFSET DISPLACED-P ARRAY NDIMS)
  (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ NDIMS))
  (COND (INDEX-OFFSET
	  (%P-DPB-OFFSET 1 %%Q-FLAG-BIT ARRAY NDIMS) ;FLAG SIGNALS INDEX OFFSET
	  (%P-STORE-CONTENTS-OFFSET INDEX-OFFSET ARRAY (+ NDIMS 2))))
  ARRAY)

(LOCAL-DECLARE
  ((SPECIAL SYM VARIABLES-BEING-MONITORED))
(SETQ VARIABLES-BEING-MONITORED NIL)
(DEFUN MONITOR-VARIABLE (SYM &OPTIONAL CURRENT-VALUE-CELL-ONLY-P MONITOR-FUNCTION)
  "Calls a given function just after a given symbol is SETQed (by
compiled code or otherwise).  Does not trigger on BINDing of the symbol.
Can apply either to all SETQs, or only those which would alter the
symbol's currently active value cell.  The function is given both
the old and new values as arguments. The default monitoring function
just prints the symbol and the old and new values.  Dont try to use this
with variables that are forwarded to A memory (ie INHIBIT-SCHEDULING-FLAG).
With CURRENT-VALUE-CELL-ONLY-P, it will work OK for DTP-EXTERNAL-VALUE-CELL
type variables."
 (PROG (ADR OLD-VALUE NEW-ARRAY)
       (COND ((NULL MONITOR-FUNCTION)
	      (SETQ MONITOR-FUNCTION
		    (CLOSURE '(SYM)
			     'DEFAULT-VARIABLE-MONITOR-FUNCTION))))
       (SETQ ADR (VALUE-CELL-LOCATION SYM)
	     OLD-VALUE (COND ((BOUNDP SYM)
			      (CAR ADR)))
	     NEW-ARRAY (MAKE-ARRAY NIL ART-Q-LIST 2))
       (AS-1 OLD-VALUE NEW-ARRAY 0)   ;MOVE CURRENT VALUE TO NEW PLACE
       (AS-1 MONITOR-FUNCTION NEW-ARRAY 1)
       (%P-DPB-OFFSET 1 %%Q-FLAG-BIT NEW-ARRAY 1) ;The FLAG-BIT in the value
						  ; cell triggers the hack.
       (%P-STORE-CONTENTS ADR
			  (%MAKE-POINTER (COND (CURRENT-VALUE-CELL-ONLY-P
						 DTP-EXTERNAL-VALUE-CELL-POINTER)
					       (T DTP-ONE-Q-FORWARD))
					 (1+ (%POINTER NEW-ARRAY))))
       (SETQ VARIABLES-BEING-MONITORED (CONS SYM VARIABLES-BEING-MONITORED))
       (RETURN T)))

(DEFUN UNMONITOR-VARIABLE (&OPTIONAL SYM)
  (COND ((NULL SYM)
	 (MAPC #'UNMONITOR-VARIABLE VARIABLES-BEING-MONITORED))
	((MEMQ SYM VARIABLES-BEING-MONITORED)
	 (SETQ VARIABLES-BEING-MONITORED (DELQ SYM VARIABLES-BEING-MONITORED))
	 (%P-DPB-OFFSET DTP-FIX 3005 (PRINT-NAME-CELL-LOCATION SYM) 1)  ;SMASH FORWARDING PNTR
	 (%P-STORE-CONTENTS (VALUE-CELL-LOCATION SYM)
			    (COND ((BOUNDP SYM)
				   (SYMEVAL SYM)))))))

(DEFUN DEFAULT-VARIABLE-MONITOR-FUNCTION (OLD NEW)
  (FORMAT T "~%Changing ~S from ~S to ~S" SYM OLD NEW))
)
