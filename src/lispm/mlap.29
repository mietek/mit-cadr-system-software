;-*- MODE: LISP; PACKAGE: COMPILER; BASE: 8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;MCLAP:
;  A self-containted format that can be hung off a property-list, written out in a
;   QFASL file, etc.  The position in control memory and the version of UCADR, etc,
;   are not assumed.

;Two lists of microcompiled functions resident in C-MEM are kept:
;*MCLAP-ACTIVE-FUNCTIONS* and *MCLAP-LOADED-FUNCTIONS*.  Active functions
;are completely installed, ready for execution.  Loaded but not active functions
;are resident in C-MEM, but not currently ready for execution nor installed.
;The main reason a function might to be suitable for execution is that it contains
;unresolved MICRO-MICRO calls.  After the function(s) being called are loaded,
;the function can be activated.
;  The *MCLAP-LOADED-FUNCTIONS* list is ordered and serves as sort of a push down
;list.  Newly loaded functions are always CONSed on the front, and the functions
;at the head of the list is always the first unloaded.


(DEFVAR *INITIALLY-MICROCOMPILED-FUNCTIONS* '(EQUAL))

;Format: 
;a list, ea element
;  a symbol -> tag
;  a list -> a storage word
;       car numeric ->  complete value
;       otherwise
;	   caar -> constant numeric value
;          cadr -> list of (field  value) pairs.
;		   field must be a numeric byte specifier
;         	   value is evaluated by (apply (car value) (cdr value)).

;  legal ops for (car value)
;    (mclap-evaluate-tag <tag>)  these kind of tags defined by symbols above
;    (mclap-evaluate-mc-linkage <linkage to UCADR>)
;       arg can be (misc-entry  <misc-instruction-symbol>)
;    (mclap-micro-micro-linkage <microcoded fctn> <nargs>)
;    (mclap-linkage-eval <mc-linkage-symbol>)
;    (mclap-get-quote-index <s-exp>)
;    (mclap-get-a-constant <inum number>)
;   The MA- prefix form of each of the above is called from MA- and is the only
;     place the MCLAP- form is generated

(DEFVAR *MC-LINKAGE-ALIST*)
(DEFVAR *UCADR-STATE-LIST*)

(DEFVAR *MCLAP-LOADED-FUNCTIONS* NIL)	;list of microcompiled fctns that actually
		;reside in control mem, "most recent" first.
		;note that these are not necesarily "activated", however.
(DEFVAR *MCLAP-ACTIVE-FUNCTIONS* NIL)	;list of microcompiled fctns actually in use.

(DEFVAR *MCLAP-CODE*)		;args to MCLAP
(DEFVAR *MCLAP-BASE-LOC*)
(DEFVAR *MCLAP-A-CONSTANT-TABLE*)
(DEFVAR *MCLAP-A-CONSTANT-TABLE-OFFSET*)
(DEFVAR *MCLAP-EXIT-VECTOR-TABLE*)
(DEFVAR *MCLAP-EXIT-VECTOR-OFFSET*)
(DEFVAR *MCLAP-MM-LINKAGE-LIST*)
(DEFVAR *MCLAP-MM-LINKAGE-FLAG*)

(DEFVAR *MCLAP-LOC*)		;current location within MCLAP

(DEFVAR NUMBER-MICRO-ENTRIES NIL)  ;Should have same value as SYSTEM:%NUMBER-OF-MICRO-ENTRIES
				   ;Point is, that one is stored in A-MEM and is reloaded
				   ;if machine gets warm-booted.

;these hold global "state-of-the-world" info.
(DEFVAR *C-MEM-LOC* NIL)		   ;Free control mem locn
(DEFVAR *A-CONSTANT-TABLE-OFFSET* NIL)	   ;Free A-MEM loc
(DEFVAR *MC-EXIT-VECTOR-ARRAY* NIL)        ;Array holds actual exit vector ref'ed by UCODE.
					   ;array-leader 0 is fill pointer.
(DEFVAR *MA-MICRO-CODE-SYMBOL-INDEX* 600)  ;allocates positions in MICRO-CODE-SYMBOL-AREA.
(DEFVAR *MA-MICRO-CODE-SYMBOL-INDEX-ASSIGNMENTS* NIL)  ;ASSQ list (<function name> . <idx>)


(DEFUN MA-LOAD (&REST FUNCTIONS)
  (COND ((NULL *UCADR-STATE-LIST*)
	 (GET-UCADR-STATE-LIST)
	 (MA-INITIALIZE-VARIABLES)))
  (DOLIST (FUNCTION-NAME FUNCTIONS)
    (MCLAP-UNLOAD FUNCTION-NAME))	;remove previous from control mem, if there
 ;do this in two phases so MICRO-MICRO calls between these fctns can work.
  (DOLIST (FUNCTION-NAME FUNCTIONS)
    (MCLAP-LOAD T (GET FUNCTION-NAME 'MCLAP)))
  (DOLIST (FUNCTION-NAME FUNCTIONS)
    (MCLAP-ACTIVATE FUNCTION-NAME)))

(DEFUN MA-PRINT (FUNCTION-NAME)
  (MCLAP-LOAD 'PRINT (GET FUNCTION-NAME 'MCLAP)))

(DEFUN MA-RESET NIL		;UNLOADS ALL
  (MCLAP-UNLOAD (CAR (LAST *MCLAP-LOADED-FUNCTIONS*))))

(DEFUN MCLAP-LOAD (LOAD-P MCLAP)
  (LET ((PARAM-LIST (FIRST MCLAP))
	(MCLAP-CODE (SECOND MCLAP)))
    (LET ((FUNCTION-NAME (CADR (ASSQ 'FUNCTION-NAME PARAM-LIST))))
      (COND ((NULL *MC-EXIT-VECTOR-ARRAY*)
	     (MA-INITIALIZE-EXIT-VECTOR)))
      (MULTIPLE-VALUE-BIND (NEW-C-LOC RTN-ACT RTN-EVT RTN-MM-LINKAGE-LIST)
	  (MCLAP LOAD-P				;load-p
		 *C-MEM-LOC*			;base C-MEM loc
		 NIL				;A-CONSTANT-TABLE
		 *A-CONSTANT-TABLE-OFFSET*
		 NIL				;EXIT-VECTOR-TABLE
		 (ARRAY-LEADER *MC-EXIT-VECTOR-ARRAY* 0)
		 MCLAP-CODE)
	(COND ((EQ LOAD-P T)
	       (PUTPROP FUNCTION-NAME
			(LIST *C-MEM-LOC* *A-CONSTANT-TABLE-OFFSET*
			      (ARRAY-LEADER *MC-EXIT-VECTOR-ARRAY* 0) RTN-MM-LINKAGE-LIST
			      (DPB (CADR (ASSQ '%MINARGS PARAM-LIST))
				   606
				   (CADR (ASSQ '%MAXARGS PARAM-LIST)))
			      (MA-ARGLIST-FROM-DEBUG-INFO
				(CDR (ASSQ 'DEBUG-INFO PARAM-LIST))))
			'MCLAP-LOADED-INFO)
	       (DOLIST (C RTN-ACT)
		 (MA-LOAD-A-MEM *A-CONSTANT-TABLE-OFFSET*
				C)
		 (SETQ *A-CONSTANT-TABLE-OFFSET* (1+ *A-CONSTANT-TABLE-OFFSET*)))
	       (DOLIST (Q RTN-EVT)
		 (MA-LOAD-EXIT-VECTOR-Q Q))
	       (SETQ *C-MEM-LOC* NEW-C-LOC)
	       (PUSH FUNCTION-NAME *MCLAP-LOADED-FUNCTIONS*)))
	))))

(DEFUN MCLAP-ACTIVATE (FUNCTION-NAME)
  (LET ((INFO (GET FUNCTION-NAME 'MCLAP-LOADED-INFO)))
    (IF (NULL INFO) (FERROR NIL "")
	(MCLAP-PLUGIN-MM-CALLS (FOURTH INFO))
	(MA-INSTALL FUNCTION-NAME
		    (FIFTH INFO)	;args q
		    (SIXTH INFO)	;arglist
		    (FIRST INFO))	;C-MEM starting loc
	(PUSH FUNCTION-NAME *MCLAP-ACTIVE-FUNCTIONS*))))

(DEFUN MCLAP-DEACTIVATE (FUNCTION-NAME)
  (COND ((MEMQ FUNCTION-NAME *MCLAP-ACTIVE-FUNCTIONS*)
	 (SETQ *MCLAP-ACTIVE-FUNCTIONS* (DELQ FUNCTION-NAME *MCLAP-ACTIVE-FUNCTIONS*))
	 (MA-UNINSTALL FUNCTION-NAME)
	 (DOLIST (F *MCLAP-LOADED-FUNCTIONS*)	;if anybody MM calls this guy, deactivate him
	   (COND ((ASSQ FUNCTION-NAME (FOURTH (GET F 'MCLAP-LOADED-INFO))) ;too.
		  (MCLAP-DEACTIVATE F)))))))

(DEFUN MCLAP-UNLOAD (FUNCTION-NAME)
  (PROG (F INFO)
	(COND ((NULL (MEMQ FUNCTION-NAME *MCLAP-LOADED-FUNCTIONS*))
	       (RETURN NIL)))
   L	(COND ((NULL *MCLAP-LOADED-FUNCTIONS*) (FERROR NIL "huh?")))
	(MCLAP-DEACTIVATE (SETQ F (CAR *MCLAP-LOADED-FUNCTIONS*)))
	(COND ((SETQ INFO (GET F 'MCLAP-LOADED-INFO))
	       (SETQ *C-MEM-LOC* (FIRST INFO)
		     *A-CONSTANT-TABLE-OFFSET* (SECOND INFO))
	       (STORE-ARRAY-LEADER (THIRD INFO) *MC-EXIT-VECTOR-ARRAY* 0)
	       (REMPROP F 'MCLAP-LOADED-INFO)))
	(SETQ *MCLAP-LOADED-FUNCTIONS* (CDR *MCLAP-LOADED-FUNCTIONS*))
	(IF (EQ F FUNCTION-NAME)
	    (RETURN T)
	    (GO L))))

(DEFUN MA-ARGLIST-FROM-DEBUG-INFO (DEBUG-INFO)
  (MAPCAR (FUNCTION CAR) (CADR (ASSQ 'ARG-MAP DEBUG-INFO))))

;This has no side effects if LOAD-P nil.  Writes C-MEM if LOAD-P T.
; In either case, it can NCONC onto A-CONSTANT-TABLE and EXIT-VECTOR-TABLE.
(DEFUN MCLAP (LOAD-P *MCLAP-BASE-LOC*
	      *MCLAP-A-CONSTANT-TABLE* *MCLAP-A-CONSTANT-TABLE-OFFSET*
	      *MCLAP-EXIT-VECTOR-TABLE* *MCLAP-EXIT-VECTOR-OFFSET*
	      *MCLAP-CODE*)
 (PROG (*MCLAP-MM-LINKAGE-LIST* *MCLAP-MM-LINKAGE-FLAG*)
       (SETQ *MCLAP-CODE*				;determine if MM links need to be
	     (MCLAP-EXAMINE-MM-LINKAGES *MCLAP-CODE*))  ;one uinst or two.
       (SETQ *MCLAP-LOC* *MCLAP-BASE-LOC*)
       (DOLIST (I *MCLAP-CODE*)
	 (IF (SYMBOLP I)
	     (IF (EQ LOAD-P 'PRINT)
		 (PRINT I))
	     (SETQ *MCLAP-MM-LINKAGE-FLAG* NIL)
	     (LET ((W (MCLAP-WORD I)))
	       (IF *MCLAP-MM-LINKAGE-FLAG*
		   (PUSH (LIST (CAR *MCLAP-MM-LINKAGE-FLAG*)        ;fctn name
   			       (CADR *MCLAP-MM-LINKAGE-FLAG*)       ;# args
			       *MCLAP-LOC*
			       W)
			 *MCLAP-MM-LINKAGE-LIST*))
	       (COND ((EQ LOAD-P T)
		      (MA-LOAD-C-MEM *MCLAP-LOC* W))
		     ((EQ LOAD-P 'PRINT)
		      (TERPRI)
		      (CADR:CC-TYPE-OUT W CADR:CC-UINST-DESC T T)))
	       (SETQ *MCLAP-LOC* (1+ *MCLAP-LOC*)))))
       (RETURN *MCLAP-LOC* *MCLAP-A-CONSTANT-TABLE*
	       *MCLAP-EXIT-VECTOR-TABLE* *MCLAP-MM-LINKAGE-LIST*)
  ))

(DEFUN MCLAP-PLUGIN-MM-CALLS (MM-LINKAGE-LIST)
  (DOLIST (MM MM-LINKAGE-LIST)
    (MA-LOAD-C-MEM (THIRD MM)
		   (DPB (MCLAP-C-MEM-ENTRY-LOC (FIRST MM))
			CADR:CONS-IR-JUMP-ADDR 
			(FOURTH MM)))))

(DEFUN MCLAP-C-MEM-ENTRY-LOC (FUNCTION-NAME)
  (LET ((INFO (GET FUNCTION-NAME 'MCLAP-LOADED-INFO)))
    (IF (OR (NULL INFO)
	    (NOT (MEMQ FUNCTION-NAME *MCLAP-LOADED-FUNCTIONS*)))
	(FERROR NIL "MICRO-MICRO Link to ~S which is not loaded" FUNCTION-NAME)
	(FIRST INFO))))	;return C-MEM starting address.
	
(DEFUN MCLAP-EXAMINE-MM-LINKAGES (CODE)
  (PROG (P TRAILP FIELDLIST TEM)
	(SETQ P CODE TRAILP (VALUE-CELL-LOCATION 'CODE))
    L	(COND ((NULL P) (RETURN CODE))
	      ((NLISTP (CAR P))
	       (GO E)))
	(SETQ FIELDLIST (CADAR P))
	(COND ((MCLAP-FIELD 'MM-MOVEI FIELDLIST)
	       (RPLACD TRAILP (SETQ P (CDR P)))  ;delete movei r, s from before, if any
	       (GO L))
	      ((SETQ TEM (MCLAP-FIELD 'MCLAP-MICRO-MICRO-LINKAGE FIELDLIST))
	       (SETQ TEM (CADR TEM))
	       (LET ((ARGS-INFO (MCLAP-ARGS-INFO (CADR TEM)))	   ;this a MM call. 2 wds?
		     (NARGS (CADDR TEM)))
		 (COND ((OR (< NARGS (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO))
			    (> NARGS (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
			(FERROR NIL 
 "~%Incorrect number of args (~s) in a micro-micro call to ~S" NARGS (CADR TEM))))
		 (COND ((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
				(LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))) 
			(LET ((WD (MA-RUNTIME-EVAL 0 0
					   00 `(MM-MOVEI ,NARGS)  ;crockish way to label this 
					   CADR:CONS-IR-OP CADR:CONS-OP-ALU  ;inst
					   CADR:CONS-IR-ALUF CADR:CONS-ALU-SETA
					   CADR:CONS-IR-OB CADR:CONS-OB-ALU
					   CADR:CONS-IR-M-MEM-DEST (MC-LINKAGE-EVAL 'R)
					   CADR:CONS-IR-A-SRC (MA-GET-A-CONSTANT NARGS))))
			  (RPLACD TRAILP (CONS WD P)))))
		 (SETQ P (CDR (SETQ TRAILP P))))))
   E	(SETQ P (CDR (SETQ TRAILP (CDR TRAILP))))
	(GO L)))

(DEFUN MM-MOVEI (NARGS)
  NARGS)	;dummy routine. this value really gets ignored.

(DEFUN MCLAP-FIELD (FIELD FIELD-LIST)
  (PROG NIL
    L	(COND ((NULL FIELD-LIST) (RETURN NIL))
	      ((AND (LISTP (CAR FIELD-LIST))
		    (LISTP (CADAR FIELD-LIST))
		    (EQ FIELD (CAADAR FIELD-LIST)))
	       (RETURN (CAR FIELD-LIST))))
       (SETQ FIELD-LIST (CDR FIELD-LIST))
       (GO L)))

(DEFUN MCLAP-ARGS-INFO (SYM &AUX TEM)
  (COND ((SETQ TEM (GET SYM 'MCLAP))
	 (LET ((PARAM-LIST (CAR TEM)))
	   (DPB (CADR (ASSQ '%MINARGS PARAM-LIST))
		606
		(CADR (ASSQ '%MAXARGS PARAM-LIST)))))
	(T (%ARGS-INFO SYM))))

;process things made by MA-RUNTIME-EVAL
(DEFUN MCLAP-WORD (W)
  (PROG (V L TEM)
	(COND ((NUMBERP W) (RETURN W)))
	(SETQ V (CAR W) L (CADR W))
    L	(COND ((NULL L) (RETURN V)))
	(COND ((NOT (NUMBERP (SETQ TEM (APPLY (CAR (CADAR L)) (CDR (CADAR L))))))
	       (FERROR NIL "Field failed to evaluate to number ~s" TEM)))
	(SETQ V (DPB TEM (CAAR L) V))
	(SETQ L (CDR L))
	(GO L)))

;simplemindedly evaluate tag by searching and counting.
(DEFUN MCLAP-EVALUATE-TAG (TAG)
  (DO ((LOC 0)
       (P *MCLAP-CODE* (CDR P)))
      ((NULL P)
       (FERROR NIL "~%tag not found ~s" TAG))
    (COND ((EQ TAG (CAR P))
	   (RETURN (+ LOC *MCLAP-BASE-LOC*)))
	  ((NOT (SYMBOLP (CAR P)))
	   (SETQ LOC (1+ LOC))))))

(DEFUN MCLAP-MICRO-MICRO-LINKAGE (FCTN NARGS)
  (SETQ *MCLAP-MM-LINKAGE-FLAG* (LIST FCTN NARGS))
  0)

(DEFUN MCLAP-EVALUATE-MC-LINKAGE (ADR)
  (COND	((EQ (CAR ADR) 'MISC-ENTRY)
	 (AR-1 (FUNCTION MICRO-CODE-SYMBOL-AREA)
	       (- (GET (CADR ADR) 'QLVAL) 200)))
	(T (FERROR NIL ""))))

(COMMENT 	 
     (COND ((NOT (= (%DATA-TYPE (SETQ TEM (CAR (FUNCTION-CELL-LOCATION (CADR ADR)))))
		    DTP-U-ENTRY))
	    (FERROR NIL "mc-entry-adr not DTP-U-ENTRY")))
     (AR-1 (FUNCTION MICRO-CODE-SYMBOL-AREA)
	   (AR-1 (FUNCTION MICRO-CODE-ENTRY-AREA)
		 (%POINTER TEM)))
)

(DEFUN MCLAP-LINKAGE-EVAL (REG)
  (LET ((ANS (CDR (ASSQ REG *MC-LINKAGE-ALIST*))))
    (COND ((NULL ANS)
	   (FORMAT T "~%MCLAP-LINKAGE ~s undefined" REG)
	   0)
	  (T (CADR ANS)))))

(DEFUN MCLAP-GET-QUOTE-INDEX (QUAN &OPTIONAL ALWAYS-ADD)
  (PROG (TEM)
	(COND (ALWAYS-ADD
	       (SETQ *MCLAP-EXIT-VECTOR-TABLE* (NCONC *MCLAP-EXIT-VECTOR-TABLE*
						(LIST QUAN)))
	       (RETURN (+ (1- (LENGTH *MCLAP-EXIT-VECTOR-TABLE*))
			  *MCLAP-EXIT-VECTOR-OFFSET*))))
     L  (COND ((SETQ TEM (FIND-POSITION-IN-LIST-EQUAL QUAN *MCLAP-EXIT-VECTOR-TABLE*))
	       (RETURN (+ TEM *MCLAP-EXIT-VECTOR-OFFSET*))))
	(SETQ *MCLAP-EXIT-VECTOR-TABLE* (NCONC *MCLAP-EXIT-VECTOR-TABLE*
					 (LIST QUAN)))
	(GO L)))

(DEFUN MCLAP-GET-A-CONSTANT (CON)
  (PROG (TEM)
    L  (COND ((SETQ TEM (FIND-POSITION-IN-LIST-EQUAL CON *MCLAP-A-CONSTANT-TABLE*))
	      (RETURN (+ TEM *MCLAP-A-CONSTANT-TABLE-OFFSET*))))
       (SETQ *MCLAP-A-CONSTANT-TABLE* (NCONC *MCLAP-A-CONSTANT-TABLE* (LIST CON)))
       (GO L)))

(DEFUN GET-UCADR-STATE-LIST ()
 (PKG-BIND "COMPILER"
  (PROG (STREAM ITEM FILENAME ASSEMBLER-STATE (IBASE 8) (BASE 8))
	(COND ((AND *UCADR-STATE-LIST*
		    (EQ %MICROCODE-VERSION-NUMBER
			(GET-FROM-ALTERNATING-LIST *UCADR-STATE-LIST* 'VERSION-NUMBER)))
	       (RETURN NIL)))
	(SETQ FILENAME (FORMAT NIL "AI:LISPM1;UCADR ~DSYM" %MICROCODE-VERSION-NUMBER))
	(SETQ STREAM (OPEN FILENAME '(READ)))
  COM0	(COND ((NOT (< (SETQ ITEM (READ STREAM)) 0))
	       (GO COM0)))
  COM	(COND ((= ITEM -1) (GO FIN))
	      ((= ITEM -2) (GO FIN))	;ignore
              ((= ITEM -4)
               (SETQ ASSEMBLER-STATE (READ STREAM))
               (GO FIN))
	      (T (FERROR NIL "~O is not a valid block header" ITEM)))
  FIN	(CLOSE STREAM)
	(SETQ *UCADR-STATE-LIST* ASSEMBLER-STATE)
	(SETQ *MC-LINKAGE-ALIST* (GET-FROM-ALTERNATING-LIST *UCADR-STATE-LIST*
							    'MC-LINKAGE-ALIST))
  	(RETURN T))))

;--- low level stuff ---

(DEFUN MA-INSTALL (FUNCTION-NAME ARG-INFO ARGLIST C-MEM-ADR
				 &AUX MICRO-CODE-ENTRY-INDEX MICRO-CODE-SYMBOL-INDEX)
  (SETQ MICRO-CODE-ENTRY-INDEX 
	(COND ((AND (FBOUNDP FUNCTION-NAME)
		    (= (%DATA-TYPE (FSYMEVAL FUNCTION-NAME)) DTP-U-ENTRY))
	       (%POINTER (FSYMEVAL FUNCTION-NAME)))
	      (T	
		(ALLOCATE-MICRO-CODE-ENTRY-SLOT FUNCTION-NAME))))
  (STORE (SYSTEM:MICRO-CODE-ENTRY-ARGLIST-AREA MICRO-CODE-ENTRY-INDEX) ARGLIST)
  (STORE (SYSTEM:MICRO-CODE-ENTRY-ARGS-INFO-AREA MICRO-CODE-ENTRY-INDEX) ARG-INFO)
  (LET ((PREV (AR-1 (FUNCTION SYSTEM:MICRO-CODE-ENTRY-AREA) MICRO-CODE-ENTRY-INDEX)))
    (COND ((AND PREV (NOT (FIXP PREV)))
	   (PUTPROP FUNCTION-NAME PREV 'DEFINITION-BEFORE-MICROCODED))))
  (SETQ MICRO-CODE-SYMBOL-INDEX (GET-MICRO-CODE-SYMBOL-INDEX FUNCTION-NAME))
  (AS-1 C-MEM-ADR (FUNCTION SYSTEM:MICRO-CODE-SYMBOL-AREA) MICRO-CODE-SYMBOL-INDEX)
  (AS-1 MICRO-CODE-SYMBOL-INDEX
	(FUNCTION SYSTEM:MICRO-CODE-ENTRY-AREA)
	MICRO-CODE-ENTRY-INDEX))

(DEFUN MA-UNINSTALL (FUNCTION-NAME)
  (LET ((PREV (GET FUNCTION-NAME 'DEFINITION-BEFORE-MICROCODED))
	(FB (FSYMEVAL FUNCTION-NAME)))
    (COND (PREV
	   (FSET FUNCTION-NAME PREV)
    (COMMENT
	   (COND ((EQ (DATA-TYPE FB) 'DTP-U-ENTRY)
		   (PUTPROP FUNCTION-NAME
			    (AR-1 (FUNCTION MICRO-CODE-ENTRY-AREA) (%POINTER FB))
			    'MICRO-CODE-SYMBOL-INDEX)
		   (AS-1 PREV (FUNCTION MICRO-CODE-ENTRY-AREA) (%POINTER FB)))
		  (T (FSET FUNCTION-NAME PREV))) )
	    (REMPROP FUNCTION-NAME 'DEFINITION-BEFORE-MICROCODED)))))

(DEFUN GET-MICRO-CODE-SYMBOL-INDEX (FUNCTION-NAME)
  (COND ((CDR (ASSQ FUNCTION-NAME *MA-MICRO-CODE-SYMBOL-INDEX-ASSIGNMENTS*)))
	(T (PROG1 *MA-MICRO-CODE-SYMBOL-INDEX*
		  (PUSH (CONS FUNCTION-NAME
			      *MA-MICRO-CODE-SYMBOL-INDEX*)
			*MA-MICRO-CODE-SYMBOL-INDEX-ASSIGNMENTS*)
		  (SETQ *MA-MICRO-CODE-SYMBOL-INDEX*
			(1+ *MA-MICRO-CODE-SYMBOL-INDEX*))))))

;Allocate a MICRO-CODE-ENTRY-SLOT.  If not already a DTP-U-UENTRY, allocates one
; and moves current function cell contents there.  Note function can still be
; macro-compiled after this.  It just has an extra level of indirecting that allows
; a microcompiled definition to be snapped in by storing a fixnum index to
; MICRO-CODE-SYMBOL-AREA in the MICRO-CODE-ENTRY-AREA slot.
(DEFUN ALLOCATE-MICRO-CODE-ENTRY-SLOT (FUNCTION-NAME)
  (LET ((FC (COND ((FBOUNDP FUNCTION-NAME) (FSYMEVAL FUNCTION-NAME)))))
    (COND ((= (%DATA-TYPE FC) DTP-U-ENTRY)
	   (%POINTER FC))
	  (T
	   (LET ((ARGS-INFO (COND (FC (ARGS-INFO FC))))  ;DO THIS FIRST SO AS NOT TO GET
		 (ARGLIST (COND (FC (ARGLIST FC)))))     ; THINGS OUT OF PHASE IF ERROR.
	     (LET ((IDX (ARRAY-PUSH (FUNCTION SYSTEM:MICRO-CODE-ENTRY-NAME-AREA)
				    FUNCTION-NAME)))
	       (COND ((NULL IDX)
		      (FERROR NIL "MICRO-CODE-ENTRY-ARRAYS FULL"))
		     (T (ARRAY-PUSH (FUNCTION SYSTEM:MICRO-CODE-ENTRY-AREA) FC)
			(ARRAY-PUSH (FUNCTION SYSTEM:MICRO-CODE-ENTRY-ARGS-INFO-AREA)
				    ARGS-INFO)
			(ARRAY-PUSH (FUNCTION SYSTEM:MICRO-CODE-ENTRY-ARGLIST-AREA)
				    ARGLIST)
			(SETQ NUMBER-MICRO-ENTRIES
			      (SETQ SYSTEM:%NUMBER-OF-MICRO-ENTRIES (1+ IDX)))
			(AS-1 NUMBER-MICRO-ENTRIES
			      (FUNCTION SYS:SCRATCH-PAD-INIT-AREA)
			      31)	;A-AMCENT reloads from here on boot.
		        (FSET FUNCTION-NAME (%MAKE-POINTER DTP-U-ENTRY IDX))
			IDX))))))))

(DEFUN MA-RESET-MICRO-CODE-ENTRY-ARRAYS (N)
       (MA-RESET)
       (STORE-ARRAY-LEADER N (FUNCTION SYSTEM:MICRO-CODE-ENTRY-NAME-AREA) 0)
       (STORE-ARRAY-LEADER N (FUNCTION SYSTEM:MICRO-CODE-ENTRY-AREA) 0)
       (STORE-ARRAY-LEADER N (FUNCTION SYSTEM:MICRO-CODE-ENTRY-ARGS-INFO-AREA) 0)
       (STORE-ARRAY-LEADER N (FUNCTION SYSTEM:MICRO-CODE-ENTRY-ARGLIST-AREA) 0)
       (SETQ NUMBER-MICRO-ENTRIES
	     (SETQ SYSTEM:%NUMBER-OF-MICRO-ENTRIES (1+ N)))
       (AS-1 NUMBER-MICRO-ENTRIES
	     (FUNCTION SYS:SCRATCH-PAD-INIT-AREA)
	     31)
       (SETQ SI:%MC-CODE-EXIT-VECTOR (+ (%POINTER *MC-EXIT-VECTOR-ARRAY*)
					1
					(%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG
						       *MC-EXIT-VECTOR-ARRAY*
						       0)))
       N)

(DEFUN MA-REBOOT NIL	;should not be neccessary now that SCRATCH-PAD-INIT-AREA hacked
  (SETQ SYSTEM:%NUMBER-OF-MICRO-ENTRIES NUMBER-MICRO-ENTRIES)
  (SETQ SI:%MC-CODE-EXIT-VECTOR (+ (%POINTER *MC-EXIT-VECTOR-ARRAY*)
				   1
				   (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG
						  *MC-EXIT-VECTOR-ARRAY*
						  0))))

(DEFUN MA-LOAD-C-MEM (ADR I)
  (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES 1 ADR
    (%LOGDPB (LDB 4020 I) 1020 (LDB 3010 I))  ;ASSURE NO BIGNUMS
    (%LOGDPB (LDB 1020 I) 1020 (LDB 0010 I))))

(DEFUN MA-LOAD-A-MEM (ADR A)
  (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES 4 ADR    ;A/M
    (%LOGDPB (LDB 4020 A) 1020 (LDB 3010 A))
    (%LOGDPB (LDB 1020 A) 1020 (LDB 0010 A))))

(DEFUN MA-INITIALIZE-EXIT-VECTOR ()
  (COND ((NULL *MC-EXIT-VECTOR-ARRAY*)
	 (SETQ *MC-EXIT-VECTOR-ARRAY*
	       (MAKE-ARRAY SYSTEM:PERMANENT-STORAGE-AREA ART-Q-LIST 1000 NIL '(0))))
	(T (STORE-ARRAY-LEADER 0 *MC-EXIT-VECTOR-ARRAY* 0)))
  (SETQ SI:%MC-CODE-EXIT-VECTOR (+ (%POINTER *MC-EXIT-VECTOR-ARRAY*)
				   1
				   (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG
						  *MC-EXIT-VECTOR-ARRAY*
						  0))))

(DEFUN MA-LOAD-EXIT-VECTOR-Q (EV &AUX DTP PTR) 
  (COND ((EQ (CAR EV) 'QUOTE)
	 (SETQ DTP (%DATA-TYPE (CADR EV))
	       PTR (%POINTER (CADR EV))))
	((EQ (CAR EV) 'SPECIAL)
	 (SETQ DTP DTP-EXTERNAL-VALUE-CELL-POINTER
	       PTR (1+ (%POINTER (CADR EV)))))
	((EQ (CAR EV) 'FUNCTION)
	 (SETQ DTP DTP-EXTERNAL-VALUE-CELL-POINTER
	       PTR (+ 2 (%POINTER (CADR EV))))))
  (ARRAY-PUSH *MC-EXIT-VECTOR-ARRAY* (%MAKE-POINTER DTP PTR)))


(DEFUN MA-INITIALIZE-VARIABLES NIL
  (LET ((A-RANGE (GET-FROM-ALTERNATING-LIST *UCADR-STATE-LIST*
					    'A-MEMORY-RANGE-LIST))
	(I-RANGE (GET-FROM-ALTERNATING-LIST *UCADR-STATE-LIST*
					    'I-MEMORY-RANGE-LIST)))
    (SETQ *A-CONSTANT-TABLE-OFFSET* (+ (CAR (CAR A-RANGE))
				       (CADR (CAR A-RANGE))))
    (SETQ *C-MEM-LOC* (+ (CAR (CAR I-RANGE))
			 (CADR (CAR I-RANGE))))))


(DEFUN WRITE-INITIALLY-MICROCOMPILED-FILE NIL
  (MA-WRITE-MCLAP-PROPS "AI:LISPM;UCINIT QFASL"
			*INITIALLY-MICROCOMPILED-FUNCTIONS*
			`(SETQ *INITIALLY-MICROCOMPILED-FUNCTIONS*
			       ',*INITIALLY-MICROCOMPILED-FUNCTIONS*)))

(DEFUN MA-WRITE-MCLAP-PROPS (FILENAME LIST-OF-FCTNS &OPTIONAL EXP &AUX LOSEP)
  (DOLIST (FCTN LIST-OF-FCTNS)
    (IF (NULL (GET FCTN 'MCLAP))
	(PROGN (FORMAT T "~%~s has no MCLAP property" FCTN)
	       (SETQ LOSEP T))))
  (IF (OR (NULL LOSEP)
	  (Y-OR-N-P "Do you want to proceed anyway?"))
      (PROGN (SETQ FILENAME (SI:FILE-PARSE-NAME FILENAME NIL T ':QFASL))
	     (FASD-INITIALIZE)
	     (FASD-OPEN FILENAME)
	     (DOLIST (FCTN LIST-OF-FCTNS)
	       (FASD-FORM `(DEFPROP ,FCTN ,(GET FCTN 'MCLAP) MCLAP)))
	     (IF EXP (FASD-FORM EXP))
	     (FASD-END-WHACK)
	     (FASD-END-FILE)
	     (CLOSE FASD-STREAM))))


;Managing microcode entries and stuff:
;  All actual microcode entry address are stored in MICRO-CODE-SYMBOL-AREA.
;This area is 1000 locations long.  The first 600 are accessible via
;misc macroinstruction (values 200-777).  MICRO-CODE-SYMBOL-NAME-AREA corresponds
;with MICRO-CODE-SYMBOL-AREA Q for Q and gives the NAME for debugging.
;  How DTP-U-ENTRY works:  DTP-U-ENTRY is sort of an indirect pointer relative
;to the origin of MICRO-CODE-ENTRY-AREA.  The Q referenced is to be interpreted
;in functional context in the normal fashion, with one exception: If the
;data type is DTP-FIX,  this is a "real" ucode entry.
;In that case, MICRO-CODE-ENTRY-NAME-AREA, MICRO-CODE-ENTRY-ARGS-INFO-AREA,
;MICRO-CODE-ENTRY-MAX-PDL-USAGE, and MICRO-CODE-ENTRY-ARGLIST-AREA (at the same index)
;give data about this entry.  The DTP-FIX in MICRO-CODE-ENTRY-AREA is an index
;to MICRO-CODE-SYMBOL-AREA which in turn contains the actual control memory
;starting address.  The reason for the indirecting step from MICRO-CODE-ENTRY-AREA
;to MICRO-CODE-SYMBOL-AREA is to separate the world into two independant pieces.
;(The microcode and MICRO-CODE-SYMBOL-AREA separate from the rest of the load).

;  Making new microcoded functions.  Two "degrees of commitment" are available,
;ie, the newly added function can be made available as a misc instruction or not.
;If a misc instruction, the system becomes completely committed
;to this function remaining microcoded forever.  If not, it is possible in the future to
;decommit this function from microcode, reinstating the macrocoded definition.

;  Decommiting can be done either by restoring the DTP-FEF-POINTER to the function cell,
;or by putting it in the MICRO-CODE-ENTRY-AREA position.  This latter option allows
;the microcoded definition to be quickly reinstalled.  
;  One problem with decomitting concerns activation-records for the microcoded
;which may be lying around on various stack-groups.  If later, an attempt is made
;to return through these, randomness will occur.  To avoid this, on a 
;macro-to-micro return, the microcode can check that the function being returnned
;to is still in fact microcoded.

;MICRO-CODE-SYMBOL-AREA is divided in two sections.  0-577 are ref'ed by
;MISC instructions 200-777, and may also be ref'ed by DTP-U-ENTRY's as described above.
;600-777 are entries to microcompiled functions.   (The old MICRO-CODE-SYMBOL-VECTOR
;which used these has been flushed.)
