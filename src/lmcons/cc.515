; -*- Mode:Lisp; Package:CADR; Base:8 -*-
;READ IN LISPM;MACROS > BEFORE RUNNING THIS INTERPRETED.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(INCLUDE |LMDOC;.COMPL PRELUD|)
#M (DECLARE (MACROS NIL))	;I suppose this won't do any good....
(INCLUDE |LMCONS;QFMAC >|)

;Cons Machine Console Program

;Documentation:

;It's like DDT

;Further Documentation:
;
;^R	reset
;^N	step
;n^N	step n times, n < 40000
;adr^N	step until about to execute micro instr at adr
;^P	run until char typed (lisp machine uses its own console)
;^L	clear screen
;^T	enter remote console mode, type ^S to leave
;105 FOOBAR  start machine
;
; :AREAS  give information about areas
; :AREA   prints area that last value typed points to.
; :MAPS   prints maps (1st and 2nd level) addressed by last value typed.
; :STKP   give backtrace of info on stack. Preceeding numeric arg is number of
;	  frames worth.  All are printed if arg is absent.  If arg negative,
;	  print (abs arg) frames direct from PDL-BUFFER (bypassing memory mapping, etc).
;	  Any character typed during printout will abort rest of printout.
; :TRACE  like :STKP except that if the last value examined is a stack group
;	  that stack group will be traced instead of the current one.
; :ATOM foo  tell about atomic symbol foo
; :BACTRACE
; :BACKTRACE
; :MEMSTAT tell all about all pages that are swapped in
; :RELPC  types out what M-AP points to, and if thats a FEF, prints out what
;	  LC is relative to that.
; :CODE   disassembles the macrocoded function being executed.
; :DISASSEMBLE-FEF  disassembles last quantity typed.  Asks for center-PC or NIL.
; :CHECK-MAP  checks contents of hardware map against PAGE-HASH-TABLE.
; :PF    Interprets open register as LP-FEF Q of a PDL-FRAME, and prints
;	  the entire frame.
; :FLAGS  Decode M-FLAGS
; :INTOFF disable machine interrupts
; arg :PHYS-MEM-WORD-SEARCH   Searches real core for arg  **CROCK** FOR NOW IT ONLY
;				SEARCHES 128K.  FIX WHEN CC KNOWS ABOUT REAL MEM.
; :DESCRIBE  if last quantity typed is a closure or entity, prints CLOSURE-ALIST sort
;             of thing.  if a stack-group, prints info from stack group header.
; :PCHECK    Use this to track down problems with hardware parity checkers.
;	   Types what the parity generator chips for the last quantity examined
;	   should put out.  Works for C-MEM ..add others.. .

; in CADRD:
;
; :START -  adr :START,   start machine, let it run
; :LOWLEVEL -  :LOWLEVEL T turns on low-level mode, in which reading most registers
;    gets what is currently in the machine rather than what is saved,
;    writing is unaffacted.  Makes the display at the bottom of the screen useful with :EX
;    :LOWLEVEL VERY enters a mode where CC tries not to "spontaneously" affect the
;    hardware in any way.  This means only the "passive" machine state is available,
;    ie no saving - frobbing - restoring is permitted.  If random things not part of
;    the passive state are examined, etc, ideally the saving, etc should be done
;    at that time.  BE VERY CAREFUL 
; :MODE - Decodes the mode register symbolically
; :CHMODE - Edits the mode register
; :RESTORE -  does a full-restore, getting software state into hardware,

; :EX - Execute .IR once.
; :SCOPE causes the machine to execute whatever is in DEBUG-IR
;	 repeatedly at full speed.  Deposit in .IR just before doing this.

;
; Breakpoints:
; :B	set breakpoint at prefix arg or open location
; :UB	unset breakpoint at ..
; :LISTB  list breakpoints
; :UAB	unset all breakpoints
; :P	proceed
; :G    do 1@G and :P
; :TB	set temporary breakpoint at .. (temp bkpt goes away when reached)
; :TBP	set temporary breakpoint and proceed
; 
; :HERE   :P connects lisp machine to ITS console
; :THERE  :P connects lisp machine to its own console
;
;Initial Symbols
; RESET VMA MWD RAIDR PSV FSV RUNNING TRYING-TO-RUN MODE
; LLMOD NOOPF FDEST FSRC .IR IR PC USP Q DC PP PI CIB OPC
;
;   Since there are many different memories in the machine, each having
;addresses running from 0 to some power of 2, a large space of register addresses
;is defined, and the various memories are assigned parts of it.
;A register address can be referred to either by specifying which memory
;and the address within the memory, as in 200@C for location 200 in control memory,
;or by specifying the register address, which would be 200+RACMO for that location.
;
;100000+n   physical main memory location n
;1000000+n  virtual memory location n
;
;n@C	control memory
;n@D	dispatch memory
;n@P	PDL buffer
;n@1	map 1
;n@2	map 2
;n@A	A memory
;n@U	micro return stack
;n@M	M memory
;FS n	functional sources
;FD n	functional destinations
;CC n	"special" registers, e.g. PC, USP
;CSW n	CCONS control switches
;RAIDR n  Raid registers
;CIB n	Console inst buffers
;OPC n  Old PCs

;n@G	set PC
;@Q	last frob typed (like Q in DDT)
;
;_nn	rotate left nn places (32 bits).  follow by space or equals to type out.
;
;_H	type out as halfwords LH,,RH
;_B	type out as bytes (right to left)
;_Q	type out as lisp Q
;_A	type out as array header
;_I	type out as macro instruction
;_U	type out as micro instruction
;_V	type out as micro instruction, old style.
;__	type out as register address
;_S	type out as lisp machine S expression (ie do PRINT, sort of)
;	  CC-SEXP-PRINLEVEL and CC-SEXP-PRINLENGTH control how deep and
;	  how long things will go, respectively.
;_#     type out as bit numbers of set bits.
;` (left slant) instead of _ causes type-in mode
;In type-in mode,  completes what has been typed so far
; as much as possible, ? lists possible completions, space
; terminates the syllable.  You can type just a space and
; if there is one possibility that types nothing in
; type-out mode, (i.e. a default) it will get used.
;
;' (right slant) is similar except typing just a space leaves
; the field set to its previous value.
; In the MODE register, bit 1.1=run slow, bit 1.2=disable error halts

(IF-FOR-MACLISP (DECLARE (CLOSED T)))	;WHEN I USE PLUS AND DIFFERENCE, IT'S FOR A REASON
(DECLARE (SETQ INHIBIT-STYLE-WARNINGS-SWITCH T)) ;DON'T HASSLE ME ABOUT CALLING GETCHARN

(IF-FOR-MACLISP 
(DEFUN QF-POINTER MACRO (X)
  (LIST 'LOGAND 77777777 (CADR X))))

(DECLARE (SPECIAL PAGE-SIZE CC-REG-ADR-PHYS-MEM-OFFSET CC-REG-ADR-VIRT-MEM-OFFSET 
		  SG-NAME SG-REGULAR-PDL SG-AP RACMWD RACVMW RACPMW DESC DESC-STACK ITEM
		  CC-FULL-SAVE-VALID CC-PASSIVE-SAVE-VALID CC-LOW-LEVEL-FLAG
		  *DONT-TOUCH-MACHINE*))
(DECLARE (SPECIAL QF-SWAP-IN-LOOP-CHECK))

(DEFVAR SPY-ACCESS-PATH 'TEN11)    ;How we get to main mem
(DEFVAR CC-ACCESS-PATH NIL)	   ;If this a closure, send it messages from
				   ;CC-REGISTER-EXAMINE, CC-REGISTER-DEPOSIT.  used by
				   ;stuff that looks at UCODE-IMAGES

(DECLARE (NOTYPE (CC-R-E FIXNUM) (CC-R-D FIXNUM NOTYPE)))
(DECLARE (SPECIAL CC-SYMBOLS-NAME CC-SYMBOLS-VALUE CC-SYMBOLS-SIZE CC-SYMBOLS-ARRAY-SIZE))
(SETQ CC-SYMBOLS-ARRAY-SIZE 3000) ;CURRENT SIZE OF ARRAYS

;alist (<file-name> cc-symbols-size cc-symbols-name cc-symbols-value)
#Q (DEFVAR CC-SYMBOL-TABLES-LOADED NIL)

(ARRAY CC-RAID-REG FIXNUM 8)
(IF-FOR-LISPM (FILLARRAY (FUNCTION CC-RAID-REG) '(0)))
;(DECLARE (ARRAY* (FIXNUM CC-RAID-REG 8)))

;REGISTER ADDRESSES IN CC:
;	0 < RACMO  			"NUMBERS"
;   RACMO < CC-REG-ADR-PHYS-MEM-OFFSET  "REGISTER ADDRESSES" (PDP-11 STYLE)
;  THENCE < CC-REG-ADR-VIRT-MEM-OFFSET   PHYSICAL MAIN MEM LOCNS ON CONS
;  ALL ABOVE				 VIRTUAL MAIN MEM LOCNS ON CONS

(IF-FOR-MACLISP (PROGN 
(DEFPROP Q-FASLOAD (FLOAD FASL DSK LMCONS) AUTOLOAD) ))

(DECLARE (FIXNUM (READ-FIXNUM))
	 (NOTYPE (CNSWDB FIXNUM FIXNUM FIXNUM FIXNUM)
		 (CC-MAIN-MEMORY-BLOCK-WRITE FIXNUM FIXNUM)))

;LISP MACHINE HACKING ROUTINES IN QF
(DECLARE (NOTYPE (QF-CLEAR-CACHE NOTYPE))	;FORGET STATE OF MACHINE, ARG=T => ALL
	 (FIXNUM ;(QF-AREA-ORIGIN NOTYPE)	;ARG = AREA NAME, VALUE = ADDRESS
		 (QF-INITIAL-AREA-ORIGIN NOTYPE) ; SAME, BUT MUST BE INITIAL AREA. MUCH FASTER.
		 (QF-SYMBOL NOTYPE)		;ARG = SYMBOL, VALUE = LISP MACHINE SYMBOL
		 (QF-VIRTUAL-MEM-READ FIXNUM)	;READ VIRTUAL ADDRESS
		 (QF-VIRTUAL-MEM-WRITE FIXNUM FIXNUM)	;WRITE VIR ADR, DATA
		 (QF-MEM-READ FIXNUM)		;READ VIRTUAL, BARF IF INACCESSIBLE
		 (QF-MEM-WRITE FIXNUM FIXNUM)	;WRITE VIRTUAL, BARF IF INACCESSIBLE
 ))

(DECLARE (SPECIAL AREA-LIST %SYS-COM-PAGE-TABLE-PNTR %SYS-COM-PAGE-TABLE-SIZE
		  %SYS-COM-/#-AREAS))

(DECLARE (SPECIAL CC-INPUT-STREAM CC-OUTPUT-STREAM))

;CONS/CADR PHYSICAL MEMORY HACKING

(DECLARE (FIXNUM (CNSPMR FIXNUM))		;READ PHYSICAL ADDRESS
	 (FIXNUM (CNSPMW FIXNUM FIXNUM)))	;WRITE PHYSICAL ADDRESS, DATA

(DECLARE (FIXNUM (PHYS-MEM-READ FIXNUM) (DBG-READ-XBUS FIXNUM))
	 (NOTYPE (PHYS-MEM-WRITE FIXNUM FIXNUM) (DBG-WRITE-XBUS FIXNUM FIXNUM)))

(DEFUN PHYS-MEM-READ (ADR)
  (COND ((EQ SPY-ACCESS-PATH 'BUSINT)
	 (DBG-READ-XBUS ADR))
	((EQ SPY-ACCESS-PATH 'TEN11)
	 (CNSPMR ADR))
	(T (ERROR '|UNKNOWN SPY-ACCESS-PATH| SPY-ACCESS-PATH))))

(DEFUN PHYS-MEM-WRITE (ADR VAL)
  (COND ((EQ SPY-ACCESS-PATH 'BUSINT)
	 (DBG-WRITE-XBUS ADR VAL))
	((EQ SPY-ACCESS-PATH 'TEN11)
	 (CNSPMW ADR VAL))
	(T (ERROR '|UNKNOWN SPY-ACCESS-PATH| SPY-ACCESS-PATH)))
  T)  ;DONT CONS VALUE OF CNSPMW

;SIMPLEMINDED EXAMINE OF REG-ADR (THAT SINGLE ADR, NO OVERLAP, ETC)
; RETURNS AN INTEGER (POSSIBLY A BIGNUM)
(DEFUN CC-REGISTER-EXAMINE (REG-ADR)
  (PROG ()
	(COND #Q ((CLOSUREP CC-ACCESS-PATH)
	       (RETURN (FUNCALL CC-ACCESS-PATH ':EXAMINE REG-ADR)))
	      ((NOT (< REG-ADR CC-REG-ADR-VIRT-MEM-OFFSET))
	       (RETURN (QF-MEM-READ (- REG-ADR CC-REG-ADR-VIRT-MEM-OFFSET))))
	      ((NOT (< REG-ADR CC-REG-ADR-PHYS-MEM-OFFSET))
	       (RETURN (PHYS-MEM-READ (- REG-ADR CC-REG-ADR-PHYS-MEM-OFFSET)))))
	(RETURN (CC-R-E REG-ADR))))

;SIMPLEMINDED DEPOSIT OF REG-ADR (THAT SINGLE ADR, NO OVERLAP, ETC)

;SPECIAL REGISTER ADDRESSES - SPECIAL STATUS REGISTERS AND FUNCTIONAL REGISTERS
; TAKES AS ARG AN INTEGER (POSSIBLY A BIGNUM)
(DEFUN CC-REGISTER-DEPOSIT (REG-ADR DATA)
  (PROG ()
	(COND #Q ((CLOSUREP CC-ACCESS-PATH)
		  (RETURN (FUNCALL CC-ACCESS-PATH ':DEPOSIT REG-ADR DATA)))
	      ((NOT (< REG-ADR CC-REG-ADR-VIRT-MEM-OFFSET))
	       (RETURN (QF-MEM-WRITE (- REG-ADR CC-REG-ADR-VIRT-MEM-OFFSET) DATA)))
	      ((NOT (< REG-ADR CC-REG-ADR-PHYS-MEM-OFFSET))
	       (RETURN (PHYS-MEM-WRITE (- REG-ADR CC-REG-ADR-PHYS-MEM-OFFSET) DATA))))
	(RETURN (CC-R-D REG-ADR DATA))))

; SPECIAL STATUS REGISTERS
;	RUNNING STATUS
;	ERROR STATUS
;	OUTPUT BUS READBACK

; FUNCTIONAL REGISTERS ("NORMAL" WAY OF ACCESSING THESE FCTNS IN PARENS)
;	RESET (CNTRL-R)
;	STEP  (CNTRL-N)
;	STOP  (CNTRL-S)
;	SET STARTING ADR (@G)
;	GO (CNTRL-P, BUT KEEP LISTENING)

(DECLARE (SPECIAL RAPC RASIR RAIR RAOBS RANOOPF RASTS 
	   RACMO RACME RADMO RADME RAPBE RAM1E RAM2E RAAME RAUSE RAMME RAFSE RAFDE 
	   RARGE RACSWE RARDRE RACIBE RAGO RASTOP RARDRO RAFDO RAOPCE
	   RARS RASTEP RASA RAAMO RAMMO RARCON RAPBO RAUSO
	   RAM1O RAM1E RAM2O RAM2E RAVMA
	   %%ARRAY-LEADER-BIT %%ARRAY-INDEX-LENGTH-IF-SHORT
	   %%ARRAY-LONG-LENGTH-FLAG 
	%%Q-CDR-CODE %%Q-FLAG-BIT %%Q-DATA-TYPE %%Q-POINTER %%Q-TYPED-POINTER
	%%M-FLAGS-QBBFL))

(DECLARE (SPECIAL CC-FIRST-STATUS-LINE CC-UPDATE-DISPLAY-FLAG
		  CC-LAST-OPEN-REGISTER CC-INITIAL-SYMS CC-LAST-VALUE-TYPED
		  CC-REMOTE-CONSOLE-MODE CC-BREAKPOINT-LIST
		  CC-TEMPORARY-BREAKPOINT-LIST
		  CC-OPEN-REGISTER CC-GETSYL-UNRCH CC-GETSYL-UNRCH-TOKEN))

(IF-FOR-MACLISP (DECLARE (SPECIAL CC-TTY-STATUS LISP-TTY-STATUS)))

;;; Symbol table management.

(IF-FOR-LISPM   ;stuff to keep symbol tables around.  Not enuf room in MACLISP for this.
(DEFUN CC-RECORD-SYMBOL-TABLE (FILENAME)
  (SETQ CC-SYMBOL-TABLES-LOADED
	(CONS (LIST FILENAME CC-SYMBOLS-SIZE CC-SYMBOLS-NAME CC-SYMBOLS-VALUE)
	      CC-SYMBOL-TABLES-LOADED))) )

;Select previously loaded symbols.  Return nil if none for file.
(IF-FOR-LISPM
(DEFUN CC-SELECT-SYMBOL-TABLE (FILENAME)
  (LET ((TEM (ASSOC FILENAME CC-SYMBOL-TABLES-LOADED)))
    (COND (TEM (SETQ CC-FILE-SYMBOLS-LOADED-FROM (CAR TEM)
		     CC-SYMBOLS-SIZE (CADR TEM)
		     CC-SYMBOLS-NAME (CADDR TEM)
		     CC-SYMBOLS-VALUE (CADDDR TEM))
	       T)))
  ) )

(DEFUN CC-INITIALIZE-SYMBOL-TABLE (DONT-END)
    (COND (#M (NOT (BOUNDP 'CC-SYMBOLS-NAME)) #Q T
	   (SETQ CC-SYMBOLS-VALUE (*ARRAY NIL T CC-SYMBOLS-ARRAY-SIZE))
	   (SETQ CC-SYMBOLS-NAME (*ARRAY NIL T CC-SYMBOLS-ARRAY-SIZE))))
    (SETQ CC-SYMBOLS-SIZE 0)

    (DO ((L CC-INITIAL-SYMS (CDR L)))
	((NULL L))
      (CC-ADD-SYMBOL (CAAR L) (EVAL (CDAR L))))
    (OR DONT-END (CC-END-ADDING-SYMBOLS))
    )

(DEFUN CC-ADD-SYMBOL (NAME VALUE)
  (LET ((I CC-SYMBOLS-SIZE))
    (DECLARE (FIXNUM I))
    (COND ((= I CC-SYMBOLS-ARRAY-SIZE)			;ABOUT TO STORE OUT OF ARRAY BOUNDS
	   (SETQ CC-SYMBOLS-ARRAY-SIZE (+ 400 CC-SYMBOLS-ARRAY-SIZE))
	   #M (*REARRAY CC-SYMBOLS-NAME T CC-SYMBOLS-ARRAY-SIZE)
	   #M (*REARRAY CC-SYMBOLS-VALUE T CC-SYMBOLS-ARRAY-SIZE)
           #Q (ADJUST-ARRAY-SIZE CC-SYMBOLS-NAME CC-SYMBOLS-ARRAY-SIZE)
           #Q (ADJUST-ARRAY-SIZE CC-SYMBOLS-VALUE CC-SYMBOLS-ARRAY-SIZE)))
    (LET ((C (CONS NAME VALUE)))
      #M (STORE (ARRAYCALL T CC-SYMBOLS-NAME I) C)
      #M (STORE (ARRAYCALL T CC-SYMBOLS-VALUE I) C)
      #Q (ASET C CC-SYMBOLS-NAME I)
      #Q (ASET C CC-SYMBOLS-VALUE I)
      (SETQ CC-SYMBOLS-SIZE (1+ I)))))

(DEFUN CC-END-ADDING-SYMBOLS ()
   (SETQ CC-SYMBOLS-ARRAY-SIZE CC-SYMBOLS-SIZE)
   #M (*REARRAY CC-SYMBOLS-NAME T CC-SYMBOLS-SIZE)
   #Q (ADJUST-ARRAY-SIZE CC-SYMBOLS-NAME CC-SYMBOLS-SIZE)
   (SORTCAR CC-SYMBOLS-NAME (FUNCTION ALPHALESSP))
   #M (*REARRAY CC-SYMBOLS-VALUE T CC-SYMBOLS-SIZE)
   #Q (ADJUST-ARRAY-SIZE CC-SYMBOLS-VALUE CC-SYMBOLS-SIZE)
   (SORT CC-SYMBOLS-VALUE (FUNCTION CC-VALUE-SORTER)))

(DEFUN CC-VALUE-SORTER (X Y)
   (< (CDR X) (CDR Y)))

(DEFUN CC-LOOKUP-NAME (NAME)
    (DO ((FIRST 0)
	 (LAST (1- CC-SYMBOLS-SIZE)))
	((> FIRST LAST) NIL)
      (DECLARE (FIXNUM FIRST LAST))
      (LET ((J (// (+ FIRST LAST) 2)))
	 (DECLARE (FIXNUM J))
	 (LET ((E (ARRAYCALL T CC-SYMBOLS-NAME J)))
	    (LET ((S (CAR E)))
	       (COND ((EQ S NAME)
		      (RETURN (CDR E)))
		     ((ALPHALESSP NAME S)
		      (SETQ LAST (1- J)))
		     (T (SETQ FIRST (1+ J)))))))))

(DEFUN CC-LOOKUP-VALUE (VALUE)
    (DO ((FIRST 0)
	 (LAST (1- CC-SYMBOLS-SIZE)))
	((> FIRST LAST) NIL)
      (DECLARE (FIXNUM FIRST LAST))
      (LET ((J (// (+ FIRST LAST) 2)))
	 (DECLARE (FIXNUM J))
	 (LET ((E (ARRAYCALL T CC-SYMBOLS-VALUE J)))
	    (LET ((N (CDR E)))
	      (DECLARE (FIXNUM N))
	       (COND ((= N VALUE)
		      (RETURN (CAR E)))
		     ((< VALUE N)
		      (SETQ LAST (1- J)))
		     (T (SETQ FIRST (1+ J)))))))))

;; Returns the index to the smallest string greater than or equal to NAME.
(DEFUN CC-FIND-NAME (NAME)
    (DO ((FIRST 0)
	 (LAST (1- CC-SYMBOLS-SIZE)))
	((> FIRST LAST) (1+ LAST))
      (DECLARE (FIXNUM FIRST LAST))
      (LET ((J (// (+ FIRST LAST) 2)))
	 (DECLARE (FIXNUM J))
	 (LET ((E (ARRAYCALL T CC-SYMBOLS-NAME J)))
	    (LET ((S (CAR E)))
	       (COND ((EQ S NAME)
		      (RETURN J))
		     ((ALPHALESSP NAME S)
		      (SETQ LAST (1- J)))
		     (T (SETQ FIRST (1+ J)))))))))

;; Index to the greatest value <= VALUE.
;; Returns -1 if no symbol < or =.
(DEFUN CC-FIND-VALUE (VALUE)
    (DO ((FIRST 0)
	 (LAST (1- CC-SYMBOLS-SIZE)))
	((> FIRST LAST) LAST)
      (DECLARE (FIXNUM FIRST LAST))
      (LET ((J (// (+ FIRST LAST) 2)))
	 (DECLARE (FIXNUM J))
	 (LET ((E (ARRAYCALL T CC-SYMBOLS-VALUE J)))
	    (LET ((N (CDR E)))
	      (DECLARE (FIXNUM N))
	       (COND ((= N VALUE)
		      (RETURN J))
		     ((< VALUE N)
		      (SETQ LAST (1- J)))
		     (T (SETQ FIRST (1+ J)))))))))

(DEFUN CC-FIND-CLOSEST-SYM (REG-ADR)
   (DECLARE (FIXNUM REG-ADR))
   (LET ((I (CC-FIND-VALUE REG-ADR)))
     (DECLARE (FIXNUM I))
     (COND ((NOT (< I 0))
	    (LET ((E (ARRAYCALL T CC-SYMBOLS-VALUE I)))
		 (LET ((NAME (CAR E)) (DELTA (- REG-ADR (CDR E))))
		      (DECLARE (FIXNUM DELTA))
		      (COND ((ZEROP DELTA) NAME)
			    ((AND (> DELTA 0)
				  (< DELTA 20))
			     (LIST NAME DELTA))
			    (T NIL))))))))

(SETQ CC-LAST-OPEN-REGISTER 40000)

(IF-FOR-MACLISP 
(SETQ CC-TTY-STATUS '(232320232323 230323030323)
      LISP-TTY-STATUS NIL) )

(IF-FOR-MACLISP 
(DEFUN CC-SET-TTY-STATUS NIL 
  (PROG (TEM)
	(SETQ TEM (STATUS TTY))
	(COND ((NOT (= (CAR TEM) (CAR CC-TTY-STATUS)))
		(SETQ LISP-TTY-STATUS (LIST (CAR TEM)(CADR TEM)))
		(SSTATUS TTYINT 23 NIL)
		(EVAL (CONS 'SSTATUS (CONS 'TTY CC-TTY-STATUS))))))) )

(IF-FOR-MACLISP
(DEFUN CC-RESTORE-TTY-STATUS NIL 
   (SSTATUS TTYINT 23 27)
   (COND (LISP-TTY-STATUS 
		(EVAL (CONS 'SSTATUS (CONS 'TTY LISP-TTY-STATUS)))))) )

(DEFUN CC-CONSOLE-INIT NIL 
  (PROG NIL 
	(SETQ CC-FIRST-STATUS-LINE (- #M (CAR (STATUS TTYSIZE))
                                      #Q (MULTIPLE-VALUE-BIND (IGNORE HT)
					     (FUNCALL TERMINAL-IO ':SIZE-IN-CHARACTERS)
					   HT)
                                      9))
	(COND ((NOT (BOUNDP 'CC-REMOTE-CONSOLE-MODE))
	       (SETQ CC-REMOTE-CONSOLE-MODE T)
	       (SETQ CC-BREAKPOINT-LIST NIL CC-TEMPORARY-BREAKPOINT-LIST NIL)))
	(COND ((NULL (ARRAYDIMS 'CC-RAID-REG))	;MADE DEAD BY FASLOAD
	       (*ARRAY 'CC-RAID-REG 'FIXNUM 8)
	       (FILLARRAY 'CC-RAID-REG '(0))))
	(COND ((NULL (BOUNDP 'RAPC))
	       (READFILE '(CONREG > DSK LISPM))))
	(COND ((NOT (BOUNDP 'CC-SYMBOLS-NAME))
	       (CC-INITIALIZE-SYMBOL-TABLE NIL)
	       #Q (CC-RECORD-SYMBOL-TABLE NIL) ))
	(COND ((NOT (AND (BOUNDP 'CC-INPUT-STREAM) (BOUNDP 'CC-OUTPUT-STREAM)))
	       (MAKE-CC-STREAM)))
	))


;DEFINITIONS OF VARIOUS WORD FORMATS, FOR BOTH TYPE-OUT AND TYPE-IN

(DECLARE (SPECIAL CC-Q-DESC CC-A-DESC CC-HWD-DESC CC-BYTE-DESC CC-INST-DESC
		  CC-I-DEST-DESC CC-I-ADDR-DESC CC-I-BR-DESC
		  CC-I-11-DESC CC-I-12-DESC CC-I-13-DESC CC-I-15-DESC
		  CC-I-ADDR-F-DESC CC-I-ADDR-R-DESC CC-REG-ADDR-DESC
		  CC-UINST-DESC CC-ALU-DESC CC-DSP-DESC CC-JMP-DESC
		  CC-JMP-BIT-DESC CC-JMP-ALU-DESC CC-BYT-DESC CC-DEST-DESC
		  CC-M-DEST-DESC CC-A-DEST-DESC CC-MODE-DESC-TABLE 
		  CC-SEXP-DESC))

(SETQ CC-MODE-DESC-TABLE '(
	(H . CC-HWD-DESC) (B . CC-BYTE-DESC)
	(Q . CC-Q-DESC) (A . CC-A-DESC) (/_ . CC-REG-ADDR-DESC)
	(I . CC-INST-DESC) (U . CC-UINST-DESC) (V . CC-O-UINST-DESC) (S . CC-SEXP-DESC)
	(/# . CC-BITS-DESC)
))


;DESC "LANGUAGE"
; (TYPE LITERAL)
;	type out specified atom.  All frobs typed are followed by space.
; (CTYPE LITERAL)
;	same, but no separating spaces before or after, and uses PRINC.
; (SELECT-FIELD <FIELD-NAME> <FIELD-POSITION> <SYMBOLS FOR CONSECUTIVE VALUES>)
;	value of field selects element of list, which is symbolic name or
;		NIL -> null typeout, and this value is the default on input.
;		T  -> numeric typeout of value.  For values that aren't expected.
;		A list can appear instead of a symbol, containing
;		alternate names.  NIL can be one of them, making that value the
;		default on input.  For type out, if NIL is present in the list
;		then nothing is typed.  The first element of the list
;		is used to tell you what you got if you got it as the default.
; (TYPE-FIELD <FIELD-NAME> <FIELD-POSITION> <REGISTER-ADR OFFSET>)
;       This is two things in one:
;	 If <REGISTER-ADR OFFSET> is NIL, then the field's contents are a number.
;	 Otherwise, <REGISTER-ADR OFFSET> should be RAAMO, RAMMO, RACMO, RAFDO, etc.
;	 and the contents are a register, which should be handled symbolically.
; (NUM <FIELD-POSITION>)
;	pure numeric field, prompting with "#: ".
; (SUB-FIELD <DESCRIPTION-NAME>)
;	call sub-description.
; (COND <FIELD-NAME> <FIELD-POSITION> <LIST-OF-DESCRIPTIONS>)
;	value of field selects element of list, do SUB-FIELD call to it.
; (CONSTANT <FIELD-POSITION> <VALUE>)
;	on type-in this constant is added in.
; (CALL <FUNCTION> <FIELD-POSITION> . <ITEMREST>)
;	for type-out, the function is called with 3 args.
;	1st arg is field value.
;	2nd arg is whole word
;	3nd arg is <ITEMREST>.
;	For input, <FUNCTION> should have an INPUT property which is the
;	function to use for input.
;	1st arg is the value accumulated so far.
;	2nd arg is WD-BITS-SET, a mask with 1's in the bits whose values are known as yet.
;	3rd arg is T if this is changing fields in the previous quantity.
;	4th arg is the CDDR of the item, or (<FIELD-POSITION> . <ITEMREST>).
; (IF-EQUAL <FIELD-NAME> <POSITION> <COMPARED-WITH> <DESC-IF-EQUAL> <DESC-IF-NOT>)
;	This is like COND on typeout, except that it is a two way dispatch
;	which compares a field's contents against a single distinguished value.
;	The two DESC arguments should be desc lists or names of such.
;	On input, if the field is already known, the appropriate branch is taken;
;	otherwise, it is required that one of the branches be nil, and the
;	other one is taken (always).
; (INPUT . <DESCS>)
;	the descriptors <DESCS> are processed only on input.
; (OUTPUT . <DESCS>)
;	the descriptors <DESCS> are processed only on output.
; (BITS)  typeout only, type bit numbers of set bits.

(SETQ CC-REG-ADDR-DESC '( (CALL CC-PRINT-ADDRESS-1 0030) ))

(SETQ CC-BITS-DESC '( (BITS)))

(SETQ CC-HWD-DESC '( (NUM 2020) (CTYPE /,/,) (NUM 0020)
))

(SETQ CC-BYTE-DESC '( (NUM 0010) (CTYPE /,) (NUM 1010) (CTYPE /,)
		      (NUM 2010) (CTYPE /,) (NUM 3010)
))

(SETQ CC-SEXP-DESC '( (CALL CC-Q-PRINT-TOPLEV-1 0035) ))


(SETQ CC-Q-DESC '( (SELECT-FIELD CDR 3602 (NIL CDR-TRAP CDR-NIL CDR-NEXT))
		   (SELECT-FIELD FLAG-BIT 3501 (NIL FLAG-BIT))
		   (SELECT-FIELD DATA-TYPE 3005
		       (NIL NULL FREE SYMBOL SYMBOL-HEADER FIX EXTENDED-NUMBER HEADER
			GC-FORWARD EXTERNAL-VALUE-CELL-POINTER ONE-Q-FORWARD
			HEADER-FORWARD BODY-FORWARD
			   LOCATIVE LIST U-ENTRY FEF-POINTER
			ARRAY-POINTER ARRAY-HEADER STACK-GROUP CLOSURE SMALL-FLONUM
		            SELECT-METHOD INSTANCE INSTANCE-HEADER
			ENTITY T T T T T T T))
		   (NUM 0030)
))

(SETQ CC-A-DESC '( (CONSTANT 3005 2)	;ARRAY-HEADER DATA-TYPE
		   (SELECT-FIELD ARRAY-TYPE 2305
		       (T ART-1B ART-2B ART-4B ART-8B ART-16B ART-32B ART-Q 
			ART-Q-LIST ART-STRING ART-STACK-GROUP-HEAD ART-SPECIAL-PDL 
			ART-TVB ART-REG-PDL T T T T T T T T T T T T T T T T T T))
		   (SELECT-FIELD HIGH-SPARE-BIT 2201 (NIL HIGH-SPARE-BIT))
		   (SELECT-FIELD LEADER 2101 (NIL LEADER))
		   (SELECT-FIELD DISPLACED 2001 (NIL DISPLACED))
		   (SELECT-FIELD FLAG 1701 (NIL FLAG))
		   (TYPE-FIELD /#DIMS 1403 NIL)
		   (SELECT-FIELD LONG 1301 (NIL LONG))
		   (SELECT-FIELD SPARE-BIT 1201 (NIL SPARE-BIT))
		   (TYPE-FIELD INDEX-LENGTH 0012 NIL)
))

(SETQ CC-INST-DESC '( (SELECT-FIELD OP-CODE 1104
		       (CALL CALL0 MOVE CAR
			CDR CADR CDDR CDAR
			CAAR NIL NIL NIL
			NIL MISC T T))
		      (COND OP-CODE 1104 (CC-I-DEST-DESC CC-I-DEST-DESC CC-I-DEST-DESC 
			CC-I-DEST-DESC CC-I-DEST-DESC CC-I-DEST-DESC 
			CC-I-DEST-DESC CC-I-DEST-DESC CC-I-DEST-DESC 
			CC-I-11-DESC CC-I-12-DESC CC-I-13-DESC
			CC-I-BR-DESC CC-I-DEST-DESC NIL NIL))
		      (COND SUB-OP 1104 (CC-I-ADDR-DESC CC-I-ADDR-DESC CC-I-ADDR-DESC 
			CC-I-ADDR-DESC CC-I-ADDR-DESC CC-I-ADDR-DESC 
			CC-I-ADDR-DESC CC-I-ADDR-DESC CC-I-ADDR-DESC 
			CC-I-ADDR-DESC CC-I-ADDR-DESC CC-I-ADDR-DESC 
			NIL CC-I-15-DESC NIL NIL))
))

(SETQ CC-I-DEST-DESC '( (SELECT-FIELD DEST 1503
			  (IGNORE STACK NEXT LAST
			   RETURN NEXTQ LASTQ NEXT-LIST))
))

(SETQ CC-I-ADDR-DESC '( (COND ADR-TYPE 1001 (CC-I-ADDR-F-DESC CC-I-ADDR-R-DESC))
))

(SETQ CC-I-ADDR-F-DESC '( (TYPE FEF) (NUM 0010) ))

(SETQ CC-I-ADDR-R-DESC '( (SELECT-FIELD ADR 0602 (QTPG LCL ARG PDL))
			  (NUM 0006)
))

(SETQ CC-I-BR-DESC '( (SELECT-FIELD BRANCH 1503
		       (BR BR-NIL BR-NOT-NIL BR-NIL-OR-POP
			BR-NOT-NIL-OR-POP BR-ATOM BR-NOT-ATOM T))
		      (NUM 0011)  ;FOR NOW, DOESN'T INTERPRET BRANCH DELTA
))

(SETQ CC-I-11-DESC '( (SELECT-FIELD OP 1503
			(T + - * // LOGAND LOGXOR LOGIOR))
))

(SETQ CC-I-12-DESC '( (SELECT-FIELD OP 1503
			(= > < EQ SCDR SCDDR 1+ 1-))
))

(SETQ CC-I-13-DESC '( (SELECT-FIELD OP 1503
			(BIND BINDNIL BINDPOP SETNIL SETZERO PUSH-E MOVEM POP))
))

(SETQ CC-I-15-DESC '( (TYPE-FIELD MISC-OP 0011 NIL)
		      (OUTPUT (CALL CC-I-MISC-NAME 0011)) ))	

(DEFUN CC-I-MISC-NAME (DISP IGNORE IGNORE)
  (COND ((< DISP 100) (FORMAT T "LIST ~D long " DISP))
	((< DISP 200) (FORMAT T "LIST-IN-AREA ~D long " (- DISP 100)))
	((< DISP 220) (FORMAT T "UNBIND ~D bindings " (- DISP 177)))
	((< DISP 240) (FORMAT T "POP-PDL ~D times " (- DISP 217)))
	(T
	  #Q (LET ((OP (MICRO-CODE-SYMBOL-NAME-AREA (- DISP 200))))  ;uses local machine's
	       (COND (OP (FORMAT T "~A " OP)))))))                   ; context.

;_V microinstruction type-out and type-in,
;for use in hardware debugging.  In CADRD.

;Functions used by the descriptors for _U output and input.

;(CALL CC-BYTE-FIELD-OUT 00nn always-reflect-mrot length-is-minus-one)
;nn should be 05 for a jump insn where the length minus one is zero.
;nn it is 12 for a byte insn which has 5 bits of mrot and 5 bits of length minus one.
(DEFPROP CC-BYTE-FIELD-OUT CC-BYTE-FIELD-IN INPUT)
(DEFUN CC-BYTE-FIELD-OUT (VAL WD ITEMREST)
    (PRINC '|(BYTE-FIELD |)
    (PRIN1-THEN-SPACE
       (COND ((CADR ITEMREST)
	      (1+ (LOGLDB 0505 VAL)))
	     (T (LOGLDB 0505 VAL))))
    (LET ((TEM (LOGLDB 0005 VAL)))
	 (COND ((ZEROP TEM))
	       ((OR (CAR ITEMREST) (= 1 (LOGLDB 1402 WD)))
		(SETQ TEM (- 32. TEM))))
	 (PRIN1 TEM))
    (PRINC '|) |))

(DEFUN CC-BYTE-FIELD-IN (WD WD-BITS-SET TYPE-OVER ITEMREST)
  (PROG (TEM)
    (PRINC '|(BYTE-FIELD |)
    (COND ((= (CAR ITEMREST) 0005) (PRINC '|WIDTH 1 |))
	  (T
	   (SETQ TEM (LOGLDB 0505 (LOGLDB (CAR ITEMREST) WD)))
	   (AND (CADDR ITEMREST) (SETQ TEM (1+ TEM)))
	   (SETQ TEM (CC-TYPE-IN '((TYPE-FIELD WIDTH 0006 NIL))
				 (COND (TYPE-OVER
					TEM)
				       (T 0))
				 TYPE-OVER))
	   (TYO 10) (TYO 10) (TYO 10) (TYO 10)
	   (AND (CADDR ITEMREST) (SETQ TEM (1- TEM)))
	   (SETQ WD (LOGDPB (LOGLDB (CAR ITEMREST) (LOGDPB TEM 0505 WD))
			    (CAR ITEMREST) WD))
	   (SETQ WD-BITS-SET (LOGDPB (LOGLDB (CAR ITEMREST) (LOGDPB -1 0505 WD-BITS-SET))
				     (CAR ITEMREST) WD-BITS-SET))))
    (SETQ TEM (CC-TYPE-IN '((TYPE-FIELD POSITION 0005 NIL))
			  (COND ((NOT TYPE-OVER) 0)
				((OR (CADR ITEMREST) (= 1 (LOGLDB 1402 WD)))
				 (LOGAND 37 (- 40 (LOGLDB 0005 WD))))
				(T (LOGLDB 0005 WD)))
			  TYPE-OVER))
    (TYO 10) (TYO 10) (TYO 10) (TYO 10)
    
    (COND ((OR (CADR ITEMREST) (= 1 (LOGLDB 1402 WD)))
	   (SETQ TEM (- 32. TEM))))
    (SETQ WD (LOGDPB TEM 0005 WD))
    (SETQ WD-BITS-SET (LOGDPB -1 0005 WD-BITS-SET))
    (PRINC '|) |)
    (RETURN (LIST WD WD-BITS-SET))))

(DEFUN CC-TYPE-JUMP-CONDITION (NUMBER IGNORE1 IGNORE2)
  IGNORE1 IGNORE2
  (PROG (TEM)
    (PRINC (NTH (LSH (LOGAND 1400 NUMBER) -10) '(JUMP CALL POPJ CALL-POPJ-??)))
    (COND ((ZEROP (LOGAND 40 NUMBER))
	   (PRINC '|-IF-BIT-|)
	   (COND ((ZEROP (LOGAND 100 NUMBER))
		  (PRINC 'SET))
		 (T (PRINC 'CLEAR)))
	   (COND ((ZEROP (LOGAND 200 NUMBER))
		  (PRINC '|-XCT-NEXT|)))
	   (PRINC '| (BYTE-FIELD 1 |)
	   (PRIN1 (- 32. (LOGAND 37 NUMBER)))
	   (PRINC '|)|))
	  (T
	   (SETQ TEM (NTH (COND ((ZEROP (LOGAND 100 NUMBER)) (LOGAND 7 NUMBER))
				(T (+ 10 (LOGAND 7 NUMBER))))
			  '(T -LESS-THAN -LESS-OR-EQUAL -EQUAL
			      -IF-PAGE-FAULT -IF-PAGE-FAULT-OR-INTERRUPT
			      -IF-SEQUENCE-BREAK NIL
			      T -GREATER-OR-EQUAL -GREATER-THAN -NOT-EQUAL
			      -IF-NO-PAGE-FAULT -IF-NO-PAGE-FAULT-OR-INTERRUPT
			      -IF-NO-SEQUENCE-BREAK -NEVER)))
	   (COND ((EQ TEM T)
		  (COND ((ZEROP (LOGAND 200 NUMBER))
			 (PRINC '|-XCT-NEXT|)))
		  (PRINC '| JUMP-CONDITION |) (PRIN1 (LOGAND 7 NUMBER))
		  (OR (ZEROP (LOGAND 100 NUMBER))
		      (PRINC '| (INVERTED)|)))
		 (T
		  (AND TEM (PRINC TEM))
		  (COND ((ZEROP (LOGAND 200 NUMBER))
			 (PRINC '|-XCT-NEXT|)))))))
    (PRINC '/ )))

(DECLARE (SPECIAL ART-STRING %FEFHI-FCTN-NAME 
		  Q-DATA-TYPES CC-SEXP-PRINLEVEL CC-SEXP-PRINLENGTH 
		  %%ARRAY-TYPE-FIELD))

(DEFUN CC-Q-PRINT-TOPLEV-1 (TYPED-POINTER WD ITEMREST)
    WD ITEMREST
    (CC-Q-PRINT TYPED-POINTER CC-SEXP-PRINLEVEL))

(DECLARE (SPECIAL SI:*IOLST SI:*IOCH))

(DEFUN CC-Q-EXPLODE (X &AUX (SI:*IOLST NIL) (SI:*IOCH T))
  (LET ((CC-OUTPUT-STREAM (FUNCTION SI:EXPLODE-STREAM)))
    (CC-Q-PRINT-TOPLEV X))
  (NREVERSE SI:*IOLST))

(DEFUN CC-Q-PRINT-TOPLEV (TYPED-POINTER)
    (CC-Q-PRINT TYPED-POINTER CC-SEXP-PRINLEVEL))

(COND ((NULL (BOUNDP 'CC-SEXP-PRINLEVEL))
	(SETQ CC-SEXP-PRINLEVEL 20)))

(COND ((NULL (BOUNDP 'CC-SEXP-PRINLENGTH))
	(SETQ CC-SEXP-PRINLENGTH 100)))

(DEFUN CC-Q-PRINT (TYPED-POINTER I-PRINLEVEL)
  (PROG (PRINLENGTH-COUNT DATA-TYPE Q-POINTER HEADER TEM)
	(SETQ PRINLENGTH-COUNT 0)
	(SETQ DATA-TYPE (LOGLDB %%Q-DATA-TYPE TYPED-POINTER))
	(SETQ Q-POINTER (LOGLDB %%Q-POINTER TYPED-POINTER))
	(COND ((CC-Q-ATOM TYPED-POINTER)
	       (COND ((= DATA-TYPE DTP-SYMBOL)
		      (RETURN (CC-Q-PRINT-STRING (CC-MEM-READ Q-POINTER))))
		     ((= DATA-TYPE DTP-FIX)
		      (RETURN (CC-Q-PRINT-FIX Q-POINTER)))
		     (T (GO BOMB))))
	      ((= DATA-TYPE DTP-STACK-GROUP)
	       (PRINC '|<Stack Group |)
	       (SETQ TEM (QF-ARRAY-LEADER (QF-MAKE-Q (QF-POINTER TYPED-POINTER)
						     DTP-ARRAY-POINTER)
					  SG-NAME)
		     DATA-TYPE (LOGLDB %%Q-DATA-TYPE TEM))
	       (COND ((= DATA-TYPE DTP-ARRAY-POINTER)) ;a string?
		     ((= DATA-TYPE DTP-SYMBOL)
		      (SETQ TEM (CC-MEM-READ TEM))) ;get-pname
		     (T (ERROR '|SG name has a bad type -- CC-Q-PRINT|)))
	       (CC-Q-PRINT-STRING TEM)
	       (PRINC '|>|)
	       (RETURN NIL))
	      ((= DATA-TYPE DTP-ARRAY-POINTER)
	       (SETQ HEADER (CC-MEM-READ Q-POINTER)) ;get array header following forwarding ptr
	       (COND ((= (MASK-FIELD-FROM-FIXNUM %%ARRAY-TYPE-FIELD HEADER)
			 ART-STRING)
		      (PRINC '/")
		      (CC-Q-PRINT-STRING Q-POINTER)
		      (PRINC '/")
		      (RETURN NIL))
		     ((NOT (ZEROP (MASK-FIELD-FROM-FIXNUM %%ARRAY-NAMED-STRUCTURE-FLAG
							  HEADER)))
		      ;; The array is a named-structure.
		      (LET ((NSS NIL) (NAME NIL))
			(COND ((NOT (ZEROP (MASK-FIELD-FROM-FIXNUM
					    %%ARRAY-LEADER-BIT HEADER)))
			       (SETQ NSS (QF-ARRAY-LEADER TYPED-POINTER 1))
			       (SETQ NAME (QF-ARRAY-LEADER TYPED-POINTER 2)))
			      (T (SETQ NSS (QF-AR-1 TYPED-POINTER 0))
				 (SETQ NAME (QF-AR-1 TYPED-POINTER 1))))
			(PRINC '|#<|)
			(CC-Q-PRINT NSS I-PRINLEVEL)
			(PRINC '| |)
			(CC-Q-PRINT NAME I-PRINLEVEL)
			(PRINC '| |)
			(PRIN1 (LOGLDB %%Q-POINTER TYPED-POINTER))
			(PRINC '|>|))
		      (RETURN NIL))
		     (T (GO BOMB))))
	      ((= DATA-TYPE DTP-U-ENTRY)
	       (RETURN (CC-Q-PRINT-U-ENTRY TYPED-POINTER I-PRINLEVEL)))
	      ((= DATA-TYPE DTP-FEF-POINTER)
	       (RETURN (CC-Q-PRINT-FRAME TYPED-POINTER I-PRINLEVEL)))
	      ((NOT (= DATA-TYPE DTP-LIST))
	       (GO BOMB))
	      ((= I-PRINLEVEL 0)
	       (PRINC '/#)
	       (RETURN NIL)))
	(PRINC '/()
    L	(CC-Q-PRINT (QF-CAR TYPED-POINTER) (1- I-PRINLEVEL))
	(SETQ TYPED-POINTER (QF-CDR TYPED-POINTER))
	(COND ((CC-Q-NULL TYPED-POINTER)
	       (PRINC '/))
	       (RETURN NIL)))
	(PRINC '/ )
	(COND ((NOT (= DTP-LIST (SETQ DATA-TYPE (LOGLDB %%Q-DATA-TYPE TYPED-POINTER))))
	       (PRINC '/./ )
	       (CC-Q-PRINT TYPED-POINTER (1- I-PRINLEVEL))
	       (PRINC '/))
	       (RETURN NIL))
	      ((> (SETQ PRINLENGTH-COUNT (1+ PRINLENGTH-COUNT)) CC-SEXP-PRINLENGTH)
	       (PRINC '/./././))
	       (RETURN NIL)))
	(GO L)

   BOMB	(RETURN (CC-Q-PRINT-BOMB TYPED-POINTER))
))

;;;*** This knows that NIL is at location zero.
(DEFUN CC-Q-NULL (TYPED-POINTER)
   (COND ((AND (= 0 (LOGLDB %%Q-POINTER TYPED-POINTER))
	       (= (LOGLDB %%Q-DATA-TYPE TYPED-POINTER) 
	           DTP-SYMBOL))
	    T)))

(DEFUN CC-Q-ATOM (TYPED-POINTER)
  (PROG (DATA-TYPE)
	(SETQ DATA-TYPE (LOGLDB %%Q-DATA-TYPE TYPED-POINTER))
	(COND ((OR (= DATA-TYPE DTP-SYMBOL)
		   (= DATA-TYPE DTP-FIX)
		   (= DATA-TYPE DTP-EXTENDED-NUMBER))
		(RETURN T)))
	(RETURN NIL)))

(DEFUN CC-Q-PRINT-FIX (Q-NUM)
  (COND ((NOT (ZEROP (LOGLDB 2701 Q-NUM)))
	  (SETQ Q-NUM #M (BOOLE 15 Q-NUM 77777777)
		      #Q (%LOGDPB 1 2701 (LDB 0027 Q-NUM)))))
  (PRIN1 Q-NUM))

(DECLARE (SPECIAL QF-ARRAY-DATA-ORIGIN QF-ARRAY-LENGTH
		  QF-ARRAY-HAS-LEADER-P QF-ARRAY-HEADER-ADDRESS))

;;; Print a string.  Note that it is truncated to at most 200 characters to
;;; avoid printing infinite garbage
(DEFUN CC-Q-PRINT-STRING (ADR)
  (QF-ARRAY-SETUP (QF-MAKE-Q (QF-POINTER ADR) DTP-ARRAY-POINTER))
  (DO ((LEN (COND (QF-ARRAY-HAS-LEADER-P
		   (QF-POINTER (QF-MEM-READ (- QF-ARRAY-HEADER-ADDRESS 2))))
		  (T QF-ARRAY-LENGTH)))
       (ADR QF-ARRAY-DATA-ORIGIN)
       (I 0 (1+ I))
       (CH)
       (WD))
      ((OR (>= I LEN) (= I 200))
       (AND (< I LEN) (PRINC '/././.) CC-OUTPUT-STREAM)
       NIL)
    (DECLARE (FIXNUM LEN ADR I WD))
    (COND ((ZEROP (LOGAND 3 I))	;Get next word
	   (SETQ WD (QF-MEM-READ ADR)
		 ADR (1+ ADR))))
    (SETQ CH (LOGAND 377 WD)
	  WD (CC-SHIFT WD -8))
    (TYO CH CC-OUTPUT-STREAM)))	;On the pdp10, should check for format-effectors
		
(DEFUN CC-Q-PRINT-U-ENTRY (TYPED-POINTER I-PRINLEVEL)
  (PROG (TEM)
	(SETQ TEM (QF-INITIAL-AREA-ORIGIN 'MICRO-CODE-ENTRY-NAME-AREA))
	(COND ((= TEM 0)
		(RETURN (CC-Q-PRINT-BOMB TYPED-POINTER))))
	(PRIN1-THEN-SPACE 'DTP-U-ENTRY)
	(CC-Q-PRINT (QF-MEM-READ (+ TEM (LOGLDB %%Q-POINTER TYPED-POINTER))) I-PRINLEVEL)))

(DEFUN CC-Q-PRINT-FRAME (TYPED-POINTER I-PRINLEVEL)
  (PROG (TEM)
	(SETQ TEM (CC-MEM-READ (+ %FEFHI-FCTN-NAME (LOGLDB %%Q-POINTER TYPED-POINTER))))
	(PRINC '/#/<DTP-FEF-POINTER/ )
	(CC-Q-PRINT TEM I-PRINLEVEL)
	(PRINC '/ )
	(PRIN1 (QF-POINTER TYPED-POINTER))
	(PRINC '/>)))

(DEFUN CC-Q-PRINT-BOMB (TYPED-POINTER)
  (PROG (DATA-TYPE Q-POINTER)
	(SETQ DATA-TYPE (LOGLDB %%Q-DATA-TYPE TYPED-POINTER))
	(SETQ Q-POINTER (LOGLDB %%Q-POINTER TYPED-POINTER))
	(PRINC '/#<)
	(PRIN1 (NTH DATA-TYPE Q-DATA-TYPES))
	(PRINC '/ )
	(PRIN1 Q-POINTER)
	(COND ((= DATA-TYPE DTP-NULL)
	       (TYO 40)
	       (CC-Q-PRINT-STRING (CC-MEM-READ TYPED-POINTER)))
	      ((= DATA-TYPE DTP-SYMBOL-HEADER)
	       (TYO 40)
	       (CC-Q-PRINT-STRING TYPED-POINTER))
	      ((= DATA-TYPE DTP-FEF-POINTER)
	       (TYO 40)
	       (CC-Q-PRINT-STRING (CC-MEM-READ (+ %FEFHI-FCTN-NAME TYPED-POINTER)))))
	(PRIN1 '>)
	(RETURN T)))

(DEFUN CC-MEM-READ (ADDR)
    (DO ((X (QF-MEM-READ ADDR) (QF-MEM-READ ADDR))
	 (DTP))
	(NIL)
      (SETQ DTP (QF-DATA-TYPE X))
      (COND ((= DTP DTP-BODY-FORWARD)
	     (LET ((OFFSET (- (QF-POINTER ADDR) (QF-POINTER X))))
	       (SETQ X (+ (QF-MEM-READ X) OFFSET))))
	    ((OR (= DTP DTP-HEADER-FORWARD)
		 (= DTP DTP-ONE-Q-FORWARD)
		 (= DTP DTP-EXTERNAL-VALUE-CELL-POINTER))) ;loop
            (T (RETURN X)))
      (SETQ ADDR X)))

(DEFUN CC-TYPE-OUT (WD DESC PROMPTP *DONT-TOUCH-MACHINE*)
  (PROG (DC ITEM VAL TEM SYM-BASE)
	(SETQ DC (COND ((ATOM DESC) (SYMEVAL DESC))
		       (T DESC)))
    L	(COND ((NULL DC) (RETURN T)))
	(SETQ ITEM (CAR DC))
	(COND ((EQ (CAR ITEM) 'TYPE-FIELD)
		(GO T-F))
	      ((EQ (CAR ITEM) 'SELECT-FIELD)
	       (SETQ VAL (LOGLDB (CADDR ITEM) WD))
	       (SETQ TEM (NTH VAL (CADDDR ITEM)))
	       (OR (ATOM TEM)
		   (SETQ TEM (COND (PROMPTP (AND (CADR TEM) (CAR TEM)))
				   (T (CAR TEM)))))
	       (COND ((NULL TEM))
		     ((EQ TEM T)
		       (AND PROMPTP (PRIN1-THEN-SPACE (CADR ITEM)))
		       (PRIN1-THEN-SPACE (LOGLDB (CADDR ITEM) WD))) 
		     (T(AND (EQ PROMPTP 'ALL) (PRIN1-THEN-SPACE (CADR ITEM)))
		       (PRIN1-THEN-SPACE TEM))))
	      ((EQ (CAR ITEM) 'SUB-FIELD)
		(CC-TYPE-OUT WD (CADR ITEM) PROMPTP *DONT-TOUCH-MACHINE*))
	      ((EQ (CAR ITEM) 'COND)
		(GO COND))
	      ((EQ (CAR ITEM) 'IF-EQUAL)
	       (SETQ TEM (CDDDDR ITEM))
	       (COND ((NOT (= (LOGLDB (CADDR ITEM) WD) (CADDDR ITEM)))
		      (SETQ TEM (CDR TEM))))
	       (AND (CAR TEM)
		    (CC-TYPE-OUT WD (CAR TEM) PROMPTP *DONT-TOUCH-MACHINE*)))
	      ((EQ (CAR ITEM) 'CALL)
		(FUNCALL (CADR ITEM) (LOGLDB (CADDR ITEM) WD) WD (CDDDR ITEM)))
	      ((EQ (CAR ITEM) 'TYPE)
		(PRIN1-THEN-SPACE (CADR ITEM)))
	      ((EQ (CAR ITEM) 'CTYPE)
		(TYO 10)
		(PRINC (CADR ITEM)))
	      ((EQ (CAR ITEM) 'NUM)
		(PRIN1-THEN-SPACE (LOGLDB (CADR ITEM) WD)))
	      ((EQ (CAR ITEM) 'CONSTANT))
	      ((EQ (CAR ITEM) 'INPUT))
	      ((EQ (CAR ITEM) 'OUTPUT)
	       (CC-TYPE-OUT WD (CDR ITEM) PROMPTP *DONT-TOUCH-MACHINE*))
	      ((EQ (CAR ITEM) 'BITS)
	       (CC-PRINT-BITS WD))
	      (T (PRINT (LIST (CAR ITEM) 'IN ITEM  'UNKNOWN-DESCRIPTOR))))
    L1	(SETQ DC (CDR DC))
	(GO L)

  T-F   (SETQ VAL (LOGLDB (CADDR ITEM) WD))
	(COND ((NULL (CADDDR ITEM))			;3RD ARG IS NIL - PRINT NUMBER.
	       (AND PROMPTP (PRIN1-THEN-SPACE (CADR ITEM)))
	       (PRIN1-THEN-SPACE VAL)
	       (GO L1)))
	(SETQ SYM-BASE (SYMEVAL (CADDDR ITEM)))
	(COND ((MEMQ (CADDDR ITEM) '(RACMO RADMO))
	       (CC-C-OR-D-ADR-OUT (CADR ITEM) VAL SYM-BASE))
	      (T (CC-A-OR-M-ADR-OUT (CADR ITEM) VAL SYM-BASE)))
	(GO L1)

  COND	(SETQ VAL (LOGLDB (CADDR ITEM) WD))
	(SETQ TEM (CADDDR ITEM))
  C-1	(COND ((NULL TEM) (GO L1))
	      ((= VAL 0) (GO C-2)))
	(SETQ TEM (CDR TEM))
	(SETQ VAL (1- VAL))
	(GO C-1)
  C-2	(CC-TYPE-OUT WD (CAR TEM) PROMPTP *DONT-TOUCH-MACHINE*)
	(GO L1)
))

(DEFUN CC-C-OR-D-ADR-OUT (TYPE VAL SYM-BASE)
    TYPE
    (PRIN1-THEN-SPACE (OR (CC-FIND-CLOSEST-SYM (+ SYM-BASE VAL)) VAL)))

(DEFUN CC-A-OR-M-ADR-OUT (TYPE VAL SYM-BASE)
    (PROG (TEM)
	(COND ((ZEROP VAL) (RETURN NIL))
	      ((OR (AND (SETQ TEM (CC-FIND-CLOSEST-SYM (+ SYM-BASE VAL)))
			(ATOM TEM))
		   *DONT-TOUCH-MACHINE*)
	       (COND ((NULL TEM)
		      (FORMAT T "~S@A" VAL))
		     (T
		      (PRIN1 TEM))))
	      (T
	       (PRIN1 VAL)
	       (PRINC '/@)
	       (PRINC TYPE)
	       (PRINC '/[)
	       (PRIN1 (CC-REGISTER-EXAMINE (+ VAL SYM-BASE)))
	       (PRINC '/])))
	(PRINC '/ )))

(IF-FOR-MACLISP 
(DEFUN PRIN1-THEN-SPACE (X)
      (PRIN1 X)
      (PRINC '/ )))

(DEFUN TYI-UPPERCASIFY NIL 
  (PROG (CH)
	(SETQ CH (TYI CC-INPUT-STREAM))
	(COND ((AND (NOT (< CH 141))
		    (NOT (> CH 172)))
	       (SETQ CH (- CH 40))))
	(RETURN CH)))

(DEFUN CC-TYPE-IN (DESC WD TYPE-OVER)
 (PROG (DESC-STACK SYL N TEM CH ITEM WD-BITS-SET)
  (PRINC '/ )
  (SETQ WD-BITS-SET 0)		;MASK FOR BITS SET THIS TIME AROUND
A (AND (ATOM DESC) (SETQ DESC (SYMEVAL DESC)))
B (SETQ ITEM '(OUTPUT))
  (CC-TI-CONTROL-SEQUENCE)
AA
  (COND ((NULL ITEM)
	 (PRINC '|   |)
	 (RETURN WD)))
  ;;DEAL WITH STANDARD CONTROL-SEQUENCE DESCRIPTORS IN STANDARD WAY.
  (AND (CC-TI-CONTROL-SEQUENCE) (GO AA))
  ;MAYBE THIS DESCRIPTION ITEM DOESN'T CALL FOR TYPE-IN?  OR NEEDS PROMPT
  (COND ((EQ (CAR ITEM) 'CONSTANT)
	 (SETQ WD (PLUS WD (LOGDPB (CADDR ITEM) (CADR ITEM) 0)))
	 (SETQ WD-BITS-SET (LOGDPB -1 (CADR ITEM) WD-BITS-SET))
	 (GO B))
	((EQ (CAR ITEM) 'CALL)
	 (COND ((SETQ CH (GET (CADR ITEM) 'INPUT))
		(SETQ CH (FUNCALL CH WD WD-BITS-SET TYPE-OVER (CDDR ITEM)))
		(SETQ WD (CAR CH) WD-BITS-SET (CADR CH))
		(GO B))
	       (T (PRINC '|I can't hack this |)
		  (RETURN NIL))))
	;; We require that an IF-EQUAL either be determined from bits already set
	;; or have only one non-empty alternative (which we always take).
	((EQ (CAR ITEM) 'IF-EQUAL)
	  (PUSH DESC DESC-STACK)
	  (COND ((NOT (ZEROP (LOGLDB (CADDR ITEM) WD-BITS-SET)))
		 (SETQ DESC (COND ((= (CADDDR ITEM) (LOGLDB (CADDR ITEM) WD))
				   (CAR (CDDDDR ITEM)))
				  (T (CADR (CDDDDR ITEM))))))
		((NULL (CAR (CDDDDR ITEM)))
		 (SETQ DESC (CADR (CDDDDR ITEM))))
		((NULL (CADR (CDDDDR ITEM)))
		 (SETQ DESC (CAR (CDDDDR ITEM))))
		(T (BREAK 'BAD-IF-EQUAL-DESC-FOR-INPUT)))
	  (GO B))
	((AND (EQ (CAR ITEM) 'COND)	;COND THAT DEPENDS ON PREVIOUS TYPE-IN
	      (NOT (ZEROP (LOGLDB (CADDR ITEM) WD-BITS-SET))))
	 (SETQ ITEM `(SUB-FIELD ,(NTH (LOGLDB (CADDR ITEM) WD) (CADDDR ITEM))))
	 (CC-TI-CONTROL-SEQUENCE)
	 (GO AA))
	((MEMQ (CAR ITEM) '(SELECT-FIELD TYPE-FIELD COND))
	 (PRIN1-THEN-SPACE (CADR ITEM)))
	((EQ (CAR ITEM) 'NUM)
	 (PRINC '|#: |)))
  (SETQ SYL NIL)

  ;ITEM IS A DESCRIPTOR, SYL HAS TYPE-IN SO FAR.
  ;HERE TO READ MORE.
C (COND (CC-LOW-LEVEL-FLAG (CC-REPLACE-STATE)))
  (SETQ CH (TYI-UPPERCASIFY))
  (COND ((OR (= CH 77) (= CH 40) (= CH 33)) (GO D))
	((< CH 40)
	 (TERPRI)
	 (MAPC 'TYO SYL))
	((= CH 177)
	 (OR SYL (RETURN (PROGN (PRINC '|??  |) NIL)))
	 (SETQ SYL (NREVERSE (CDR (NREVERSE SYL))))
	 (CURSORPOS 'X))
	((NULL SYL) (SETQ SYL (LIST CH)))
	((RPLACD (LAST SYL) (LIST CH))))
  (GO C)

  ;HAVE SOME TYPE-IN, CH HAS DELIMITER.
  D
  (COND ((AND TYPE-OVER (= CH 40) (NULL SYL))
	 (GO K)))
 
  ;FIRST SET TEM TO LIST OF POSSIBLE COMPLETIONS
  (SETQ TEM (ELIMINATE-DUPLICATES (CC-TI-POSSIBILITIES SYL ITEM)))
  (COND ((NULL TEM)
	 (PRINC '-IMPOSS-))
	((= CH 77)
	 (MAPC 'PRIN1-THEN-SPACE TEM))
	((= CH 33)
	 (GO F))
	((COND ((NULL SYL)		;CHECK FOR AMBIGUITY,
		(NOT (MEMQ NIL TEM)))	;HACKING DEFAULT AND EXACT-MATCH
	       ((AND (> (LENGTH TEM) 1)
		     (NOT (AND (MEMQ (SETQ CH (READLIST SYL)) TEM)
			       (SETQ TEM (CONS CH TEM)))) )))
	 (PRINC '-AMBIG-))
	((GO H)))
  ;RETYPE THE SYLLABLE AND READ MORE.
  (MAPC 'TYO SYL)
  (GO C)

  ;HERE TO DO COMPLETION, SYL HAS LIST OF CHARS TYPED SO FAR,
  ;TEM HAS LIST OF POSSIBILITIES, TYPE OUT ALL CHARS THAT ARE FORCED.
F (CURSORPOS 'X)	;UNECHO THE ALTMODE
  (AND (NUMBERP (CAR TEM))
       (GO C))		;CAN'T COMPLETE PURE-NUMERIC TYPEIN
G (SETQ N (1+ (LENGTH SYL))) ;INDEX OF CHAR TO LOOK AT
  (SETQ CH (GETCHARN (CAR TEM) N))
  (AND (= CH 0) (GO C))
  ;CH HAS PROPOSED CHARACTER, SEE IF ALL POSSIBILITIES AGREE
  (AND (DO TEM (CDR TEM) (CDR TEM) (NULL TEM)
	(OR (= CH (GETCHARN (CAR TEM) N))
	    (RETURN T)))
       (GO C))		;DISAGREEMENT, STOP HERE
  (TYO CH)
  (COND ((NULL SYL) (SETQ SYL (LIST CH)))
	((RPLACD (LAST SYL) (LIST CH))))
  (GO G)

  ;TYPEIN HAS BEEN COMPLETED AND ACCEPTED, DIGEST IT.
H (SETQ TEM (AND SYL (CAR TEM)))
  (CURSORPOS 'B)	;UNSPACE
  (OR (NUMBERP TEM)
      (NULL SYL)
      (DO ((CH)		;DO FINAL STAGE OF COMPLETION
	   (N (1+ (LENGTH SYL)) (1+ N)))
	  (NIL)
	(AND (= 0 (SETQ CH (GETCHARN TEM N)))
	     (RETURN NIL))
	(TYO CH)))
  (PRINC '/ )		;SPACE AFTER FIELD
I (COND ((EQ (CAR ITEM) 'TYPE))
	((EQ (CAR ITEM) 'SELECT-FIELD)
	 (SETQ TEM (COND ((NUMBERP TEM) TEM)
			 ((AND (NULL SYL)
			       (MEMQ NIL (CDR (MEMQ NIL (CADDDR ITEM)))))
			  (GO B))		;MULTIPLE NILS, DEFER DECISION
			 ((DO ((L (CADDDR ITEM) (CDR L)) (I 0 (1+ I))) ((NULL L) NIL)
			     (AND (OR (EQ (CAR L) TEM)
				      (AND (NOT (ATOM (CAR L))) (MEMQ TEM (CAR L))))
				  (RETURN I))))))
	 (SETQ WD (LOGDPB TEM (CADDR ITEM) WD))
	 (SETQ WD-BITS-SET (LOGDPB -1 (CADDR ITEM) WD-BITS-SET)))
	((EQ (CAR ITEM) 'TYPE-FIELD)
	 (SETQ TEM (COND ((NUMBERP TEM) TEM)
			 ((NULL TEM) 0)
			 ((DIFFERENCE (CC-LOOKUP-NAME TEM)
				      (SYMEVAL (CADDDR ITEM))))))
	 (SETQ WD (LOGDPB TEM (CADDR ITEM) WD))
	 (SETQ WD-BITS-SET (LOGDPB -1 (CADDR ITEM) WD-BITS-SET)))
	((EQ (CAR ITEM) 'NUM)
	 (SETQ WD (LOGDPB TEM (CADR ITEM) WD))
	 (SETQ WD-BITS-SET (LOGDPB -1 (CADR ITEM) WD-BITS-SET)))
	((EQ (CAR ITEM) 'COND)
	 (DO ((DL (CADDDR ITEM) (CDR DL))
	      (N 0 (1+ N)))
	     ((NULL DL) (BREAK COND-BARF T))
	    (SETQ CH `(SUB-FIELD ,(CAR DL)))
	    (COND ((MEMQ TEM (CC-TI-POSSIBILITIES SYL CH))
		   (SETQ WD (LOGDPB N (CADDR ITEM) WD))
		   (SETQ WD-BITS-SET (LOGDPB -1 (CADDR ITEM) WD-BITS-SET))
		   (RETURN NIL))))
	 (SETQ ITEM CH)
	 (CC-TI-CONTROL-SEQUENCE)
	 (GO I))
	((CC-TI-CONTROL-SEQUENCE) (GO I))
	(T (BREAK INPUT-LOSSAGE-GOBBLING)))
  (PRINC '/ )
  (GO B)

  ;LEAVE THIS FIELD WITH SAME VALUE AS BEFORE
K (TYO 10)     ;Don't leave two spaces on the screen.
KK
  (COND ((EQ (CAR ITEM) 'TYPE))
	((MEMQ (CAR ITEM) '(SELECT-FIELD TYPE-FIELD))
	 (SETQ WD-BITS-SET (LOGDPB -1 (CADDR ITEM) WD-BITS-SET)))
	((EQ (CAR ITEM) 'NUM)
	 (SETQ WD-BITS-SET (LOGDPB -1 (CADR ITEM) WD-BITS-SET)))
	((EQ (CAR ITEM) 'COND)
	 (SETQ ITEM `(SUB-FIELD ,(NTH (LOGLDB (CADDR ITEM) WD) (CADDDR ITEM))))
	 (CC-TI-CONTROL-SEQUENCE)
	 (GO KK))
	((CC-TI-CONTROL-SEQUENCE)
	 (GO KK))
	(T (BREAK INPUT-LOSSAGE-SPACE)))
  (CC-TYPE-OUT WD (LIST ITEM) NIL NIL) ;RE-TYPE THE THING
  (PRINC '/ )
  (GO B)
))

(DEFUN CC-TI-CONTROL-SEQUENCE ()
  (PROG ()
    (SELECTQ (CAR ITEM)
	((SUB-FIELD INPUT)
	  (PUSH DESC DESC-STACK)
	  (SETQ DESC (COND ((EQ (CAR ITEM) 'INPUT) (CDR ITEM)) (T (CADR ITEM)))))
	(OUTPUT)
	(CTYPE
	  (TYO 10)
	  (PRINC (CADR ITEM)))
	(OTHERWISE (RETURN NIL)))
    LOOP
    (COND ((AND DESC (ATOM DESC))
	   (SETQ DESC (SYMEVAL DESC))
	   (GO LOOP))
	  (DESC)
	  (DESC-STACK (SETQ DESC (POP DESC-STACK))
		      (GO LOOP)))
    (SETQ ITEM (POP DESC))
    (RETURN T)))

;Given a desc item ITEM, and given DESC and DESC-STACK as they are,
;compute the matches of the list of characters SYL against ITEM or the
;items that follow it/are called by it.
(DEFUN CC-TI-POSSIBILITIES (SYL ITEM)
  (LET ((DESC DESC) (DESC-STACK DESC-STACK))
     (PROG ()
	 LOOP
	 (RETURN (COND
	       ((CC-TI-CONTROL-SEQUENCE) (GO LOOP))
	       ((AND SYL (EVERY SYL '(LAMBDA (CH) (AND (> CH 57) (< CH 72)))))
		(LIST (READLIST SYL)))	;IT IS, ONLY POSSIBILITY IS THAT NUMBER
	       ((EQ (CAR ITEM) 'TYPE)
		(AND (CC-TI-MATCH SYL (CADR ITEM)) (CDR ITEM)))
	       ((EQ (CAR ITEM) 'SELECT-FIELD)
		(CC-TI-SELECT-FIELD-POSSIBILITIES SYL (CADDDR ITEM)))
	       ((EQ (CAR ITEM) 'NUM)
		NIL)	;ONLY NUMBERS ALLOWED?
	       ((EQ (CAR ITEM) 'TYPE-FIELD)
		(COND ((NULL (CADDDR ITEM)) NIL) ;ONLY NUMBERS ALLOWED?
		      ((NULL SYL)
		       (LIST NIL (IMPLODE (APPEND (EXPLODE (CADR ITEM)) '(- M E M - A D R)))))
		      (T	;HACK COMPLETIONS OF REGISTER ADDRESSES
		       (LET ((FROM-I 0) (TO-I 0))
			    (COND ((NULL SYL)
				   (SETQ FROM-I 0 TO-I CC-SYMBOLS-SIZE))
				  (T (LET ((SYL+1 (APPEND SYL NIL)))
					  (LET ((L (LAST SYL+1)))
					       (RPLACA L (1+ (CAR L))))
					  (SETQ FROM-I (CC-FIND-NAME (IMPLODE SYL))
						TO-I (CC-FIND-NAME (IMPLODE SYL+1))))))
			    (DO ((I FROM-I (1+ I))
				 (ANS NIL))
				((NOT (< I TO-I)) (NREVERSE ANS))
				(LET ((E (ARRAYCALL T CC-SYMBOLS-NAME I)))
				     (AND (CC-ADR-CLOSE-ENOUGH
					   (CADDDR ITEM)
					   (GET (CC-FIND-REG-ADR-RANGE (CDR E))
						'CC-LOWEST-ADR))
					  (SETQ ANS (CONS (CAR E) ANS))))))
		       )))
	       ((EQ (CAR ITEM) 'COND)	;HAIR....
		(PUSH DESC DESC-STACK)
		(MAPCAN (FUNCTION (LAMBDA (DESC)
			    (AND (ATOM DESC) (SETQ DESC (SYMEVAL DESC)))
			    (COND ((NULL DESC) NIL)
				  ((APPEND (CC-TI-POSSIBILITIES SYL (POP DESC)) NIL)))))
			(CADDDR ITEM)))
	       (T
		 #Q (LOCAL-DECLARE ((SPECIAL ARGS))
		      ((LAMBDA (ARGS) (BREAK CC-TI-POSSIBILITIES-LOSES T))
		       ITEM))
		 #M (BREAK FUCKUP) ))))))

;Find the possible matches for SYL in a symbol or list of symbols or lists of ...
(DEFUN CC-TI-SELECT-FIELD-POSSIBILITIES (SYL SYM)
    (COND ((ATOM SYM)
	   (AND (CC-TI-MATCH SYL SYM)
		(LIST SYM)))
	  (T
	   (DO ((SYM SYM (CDR SYM)) (RESULT))
	       ((NULL SYM) RESULT)
	      (SETQ RESULT (NCONC (CC-TI-SELECT-FIELD-POSSIBILITIES SYL (CAR SYM)) RESULT))))))

;Match the list of characters SYL against the head of the symbol SYM.
(DEFUN CC-TI-MATCH (SYL SYM)
 (COND ((EQ SYM T) NIL)		;T ISN'T REALLY A SYMBOL!
       ((DO ((SYL SYL (CDR SYL))
	     (N 1 (1+ N)))
            ((NULL SYL) T)
	 (OR (= (CAR SYL) (GETCHARN SYM N))
	     (RETURN NIL))))))

(DEFUN ELIMINATE-DUPLICATES (L)
  (COND ((NULL L) NIL)
	((MEMQ (CAR L) (CDR L))
	 (ELIMINATE-DUPLICATES (CDR L)))
	((CONS (CAR L) (ELIMINATE-DUPLICATES (CDR L))))))

(DEFUN CC-ADR-CLOSE-ENOUGH (TARGET POSSIBILITY)
  (OR (EQ TARGET POSSIBILITY)
      (AND (EQ TARGET 'RAMMO) (EQ POSSIBILITY 'RAFSO)) ;FUNC SRCS ARE OK AS M MEMORY
    ))

;;;MICRO-LOADER

(DEFVAR CC-FILE-SYMBOLS-LOADED-FROM NIL)

(IF-FOR-MACLISP
(DEFUN CC-LOAD-UCODE FEXPR (L)
  (CC-UCODE-LOADER NIL L NIL))

(DEFUN CC-LOAD-BOOTSTRAP FEXPR (L)		;LOAD WITHOUT SYMBOLS AND DONT AFFECT
  (CC-UCODE-LOADER 'LOAD-WITHOUT-SYMBOLS L NIL)); CURRENT SYMTAB

(DEFUN CC-LOAD-UCODE-SYMBOLS FEXPR (L)
  (CC-UCODE-LOADER 'LOAD-SYMBOLS L NIL))

(DEFUN CC-COMPARE-UCODE FEXPR (L)
  (CC-UCODE-LOADER 'COMPARE L NIL))
)

(IF-FOR-LISPM
(DEFUN CC-LOAD-UCODE (FILE &OPTIONAL MERGEP)
  (CC-UCODE-LOADER NIL FILE MERGEP)
  (SETQ CC-FILE-SYMBOLS-LOADED-FROM FILE))

(DEFUN CC-LOAD-BOOTSTRAP (FILE)
  (CC-UCODE-LOADER 'LOAD-WITHOUT-SYMBOLS FILE NIL))

(DEFUN CC-LOAD-UCODE-SYMBOLS (FILE &OPTIONAL MERGEP)
  (COND ((EQUAL FILE CC-FILE-SYMBOLS-LOADED-FROM))
	((AND (NULL MERGEP)
	      (CC-SELECT-SYMBOL-TABLE FILE)))
	(T
	 (CC-UCODE-LOADER 'LOAD-SYMBOLS FILE MERGEP)
	 (CC-RECORD-SYMBOL-TABLE (SETQ CC-FILE-SYMBOLS-LOADED-FROM FILE)))))

(DEFUN CC-COMPARE-UCODE (FILE)
  (CC-UCODE-LOADER 'COMPARE FILE NIL))
)  ;end if for LISPM

(DEFUN WORDEX MACRO (X)
  (SUBST (+ 16. (* -16. (CADR X))) 'BAR
    (SUBST (CADDR X) 'FOO
	'(BOOLE 1 177777 (LSH FOO BAR)) )))

;(DEFUN CC-MAIN-MEMORY-BLOCK-WRITE (ADR DATA)
;	(CNSWDB RACMWD (WORDEX 3 DATA) (WORDEX 2 DATA) (WORDEX 1 DATA))
;	(CNSWDB RACPMW (WORDEX 3 ADR) (WORDEX 2 ADR) (WORDEX 1 ADR)))

(DEFUN READ-FIXNUM (FILE)		;HOPEFULLY FAST FIXNUM-ONLY READER, NCALL'ABLE
  (PROG (CH NUM SGN)
	(DECLARE (FIXNUM CH NUM SGN))
	(SETQ NUM 0 SGN 1)
  A	(AND (< (SETQ CH (TYI FILE)) 41)  ;IGNORE LEADING GARBAGE
	     (GO A))
        (AND (> CH 177) (GO A))
	(COND ((= CH 55)
	       (SETQ SGN -1))
	      ((GO C)))
  B	(SETQ CH (TYI FILE))
  C	(COND ((= CH 137)
	       (RETURN (* SGN (LSH NUM (READ-FIXNUM FILE)))))
	      ((AND (> CH 57) (< CH 70))
	       (SETQ NUM (+ (LSH NUM 3) CH -60))
	       (GO B))
	      ((> CH 40)
	       (ERROR CH 'RANDOM-CHAR-IN-READ-FIXNUM 'FAIL-ACT)))
	(RETURN (* SGN NUM))))


(DEFUN CC NIL			;MAIN LOOP OF CONS CONSOLE PROGRAM
  (PROG (CC-ARG CC-SYL CC-VAL CC-UPDATE-DISPLAY-FLAG CC-OPEN-REGISTER 
	  CC-LAST-OPEN-REGISTER CC-LAST-VALUE-TYPED COM-CH TEM) 
	(SETQ QF-SWAP-IN-LOOP-CHECK NIL)
	#M (CNSPMI)  ;FLUSH LOOKBEHIND ON CC-UNIBUS-MAP
	(IF-FOR-MACLISP (CC-SET-TTY-STATUS))
	(CC-CONSOLE-INIT)
	#Q (AND (EQ DBG-ACCESS-PATH 'DL11)
		(USER:INIT-DL11-UNIBUS-CHANNEL))
	(LET #Q ((STANDARD-OUTPUT CC-OUTPUT-STREAM)) #M ()
	  (PROG ()
	     (TERPRI)
	     (PRINC (COND (CC-FULL-SAVE-VALID
			   '|CC contains saved state, type control-S to flush it.|)
			  (T '|Getting fresh state from machine.|)))
	     (TERPRI)
	     (CC-CONSOLE-STATUS-DISPLAY T)    ;dont touch machine since saved state may
					      ;not be valid
         L0  (SETQ CC-ARG NIL)
	     (AND (SIGNP GE (- (CAR (CURSORPOS)) CC-FIRST-STATUS-LINE))
		  (PROGN (CURSORPOS 'Z) (TERPRI)))
	 L   (SETQ CC-SYL (CC-GETSYL-READ-TOKEN))
	     (COND ((NUMBERP CC-SYL)
		    (GO L1))
		   ((EQ CC-SYL '*RUB*) ;OVER RUB-OUT
		    (GO ERR1))
		   ((EQ CC-SYL '/@)	;VARIOUS REG ADDR SPACES + MISC COMMANDS
		    (GO COM))
		   ((EQ CC-SYL '/)	;EXIT TO LISP
		    (GO X))
		   ((EQ CC-SYL '/_)	;VARIOUS TYPE-OUT MODES
		    (GO UND))
		   ((EQ CC-SYL '/`)	;VARIOUS TYPE-IN MODES
		    (GO IND))
		   ((EQ CC-SYL '/')	;TYPE-IN OVER EXISTING FIELDS
		    (GO INDOV))
		   ((EQ CC-SYL '/.)	;"POINT"
		    (SETQ CC-SYL CC-LAST-OPEN-REGISTER)
		    (GO L1))
		   ((EQ CC-SYL '/:)	;VARIOUS SYMBOLIC COMMANDS
		    (GO CLN))
		   ((SETQ TEM (CC-LOOKUP-NAME CC-SYL))
		    (SETQ CC-SYL TEM)
		    (GO L1)))
            L2     (COND ((SETQ TEM (GET CC-SYL 'CC-COMMAND))
			  (GO COM1)))
            ERR   (PRIN1 CC-SYL)
            ERR1  (PRINC '??/ / )
	    (GO L0)

  L1	(COND ((NUMBERP CC-ARG)
		(SETQ CC-ARG (PLUS CC-ARG CC-SYL)))
	      (T (SETQ CC-ARG CC-SYL)))
	(GO L)

  COM	(SETQ COM-CH (ASCII (CC-CHAR-UPCASE (CC-GETSYL-RCH))))
	(COND ((SETQ TEM (GET COM-CH 'CC-LOWEST-ADR))
		(COND ((NULL CC-ARG) (SETQ CC-ARG 0)))
		(SETQ CC-ARG (+ CC-ARG (SYMEVAL TEM)))
		(GO L)))
	(SETQ CC-SYL COM-CH)
	(GO L2)
  COM1	(SETQ CC-VAL (FUNCALL TEM CC-ARG))
	(COND (CC-UPDATE-DISPLAY-FLAG 
		(CC-CONSOLE-STATUS-DISPLAY NIL)
		(SETQ CC-UPDATE-DISPLAY-FLAG NIL)))
	(COND ((NUMBERP CC-VAL)
		(SETQ CC-ARG CC-VAL)
		(GO L))
	      (T (GO L0)))

  UND	(SETQ CC-SYL (CC-CHAR-UPCASE (CC-GETSYL-RCH)))	;VARIOUS TYPEOUT COMMANDS
	(OR CC-ARG (SETQ CC-ARG CC-LAST-VALUE-TYPED))
	(COND ((OR (AND (> CC-SYL 57) (< CC-SYL 72))
		   (= CC-SYL 55))
		(SETQ CC-GETSYL-UNRCH CC-SYL		;IF DIGIT OR MINUS,
		      CC-SYL (CC-GETSYL-READ-TOKEN))	;READ WHOLE NUMBER
		(SETQ CC-SYL (LOGAND 37 CC-SYL))	;AND LEFT-ROTATE BY THAT
		(SETQ CC-ARG
		      (LOGIOR (LOGLDB (+ CC-SYL (CC-SHIFT (- 40 CC-SYL) 6)) CC-ARG)
			      (CC-SHIFT (LOGLDB (- 40 CC-SYL) CC-ARG) CC-SYL)))
		(AND (EQ CC-GETSYL-UNRCH-TOKEN '/ )
		     (SETQ CC-GETSYL-UNRCH-TOKEN '=))
		(GO L)))	;N_N<SPACE> TYPES OUT, OTHERWISE IS TYPE-IN!
	(PRINC '/ )
	(OR (SETQ COM-CH (ASSQ (SETQ CC-SYL (ASCII CC-SYL))
			       CC-MODE-DESC-TABLE))
	    (GO ERR))
	(CC-TYPE-OUT CC-ARG (CDR COM-CH) T NIL)
	(SETQ CC-LAST-VALUE-TYPED CC-ARG)	
	(PRINC '/ / )
	(GO L0)

  IND	(SETQ CC-SYL (ASCII (CC-CHAR-UPCASE (CC-GETSYL-RCH))))	;VARIOUS TYPEIN COMMANDS
	(PRINC '/ )
	(OR (SETQ COM-CH (ASSQ CC-SYL CC-MODE-DESC-TABLE))
	    (GO ERR))
	(SETQ CC-SYL (CC-TYPE-IN (CDR COM-CH) 0 NIL))
	(GO L1)

  INDOV	(SETQ CC-SYL (ASCII (CC-CHAR-UPCASE (CC-GETSYL-RCH))))
	(PRINC '|/
[EDIT] | )
	(OR (SETQ COM-CH (ASSQ CC-SYL CC-MODE-DESC-TABLE))
	    (GO ERR))
	(SETQ CC-SYL (CC-TYPE-IN (CDR COM-CH) CC-LAST-VALUE-TYPED T))
	(GO L1)

  X	(IF-FOR-MACLISP (CC-RESTORE-TTY-STATUS))
	(RETURN T)

  CLN	(SETQ CC-SYL (CC-GETSYL-READ-TOKEN))  ;:FOOBAR ETC.
	(OR (SETQ TEM (GET CC-SYL 'CC-COLON-CMD))
	    (GO ERR1))
	(GO COM1)
))))

(DEFUN CC-CHAR-UPCASE (CHAR)
    (DECLARE (FIXNUM CHAR))
    (COND ((AND (> CHAR 140)
		(< CHAR 173))
	   (LOGXOR 40 CHAR))
	  (T CHAR)))

(DEFUN CC-PRINT-REG-ADR-CONTENTS (ADR)
 (PROG (RANGE DATA PCPART)
	(SETQ RANGE (CC-FIND-REG-ADR-RANGE ADR))
	(SETQ DATA (COND ((EQ RANGE 'RAIDR)
			  (CC-RAID-REG (- ADR RARDRO)))	;RAIDR RANGE IS IN 10
			 ((CC-REGISTER-EXAMINE ADR))))
	(SETQ CC-LAST-VALUE-TYPED DATA)
	(COND ((OR (MEMQ RANGE '(C CIB)) (= ADR RAIR) (= ADR RASIR))
	       (CC-TYPE-OUT DATA CC-UINST-DESC T NIL))
	      ((MEMQ RANGE '(U OPC))
	       (SETQ PCPART (\ DATA (- RACME RACMO)))
	       (CC-PRINT-ADDRESS (+ PCPART RACMO))	;PCP PART SYMBOLICALLY
	       (COND ((NOT (= DATA PCPART))		;RESIDUE, IF ANY, NUMERICALLY
		      (PRINC '| + |)
		      (PRIN1 (- DATA PCPART))))
	       (PRINC '/ ))
	      ((EQ RANGE 'RAIDR)
		(CC-PRINT-ADDRESS DATA) (PRINC '/ ))
	      (T (PRIN1-THEN-SPACE DATA)))
	(PRINC '/ / )))

;RETURNS:  NIL IF NONE FOUND CLOSER THAN 20 TO DESIRED REG ADR
;	   SYMBOL  IF EXACT MATCH FOUND
;	   (LIST SYMBOL DIFFERENCE)  IF ONE FOUND CLOSER THAN 20

(DEFUN CC-FIND-REG-ADR-RANGE (REG-ADR)
	(COND ((< REG-ADR RACMO) 'TOO-LOW)
	      ((< REG-ADR RACME) 'C)
	      ((< REG-ADR RADME) 'D)
	      ((< REG-ADR RAPBE) 'P)
	      ((< REG-ADR RAM1E) '/1)
	      ((< REG-ADR RAM2E) '/2)
	      ((< REG-ADR RAAME) 'A)
	      ((< REG-ADR RAUSE) 'U)
	      ((< REG-ADR RAMME) 'M)
	      ((< REG-ADR RAFSE) 'FS)
	      ((< REG-ADR RAFDE) 'FD)
	      ((< REG-ADR RARGE) 'CC)
	      ((< REG-ADR RACSWE) 'CSW)
	      ((< REG-ADR RARDRE) 'RAIDR)
	      ((< REG-ADR RACIBE) 'CIB)
	      ((< REG-ADR RAOPCE) 'OPC)
	      ((< REG-ADR CC-REG-ADR-PHYS-MEM-OFFSET) 'TOO-HIGH)
	      ((< REG-ADR CC-REG-ADR-VIRT-MEM-OFFSET) 'PHYSICAL)
	      (T 'VIRTUAL)))

(DEFPROP C RACMO CC-LOWEST-ADR)
(DEFPROP D RADMO CC-LOWEST-ADR)
(DEFPROP P RAPBO CC-LOWEST-ADR)
(DEFPROP /1 RAM1O CC-LOWEST-ADR)
(DEFPROP /2 RAM2O CC-LOWEST-ADR)
(DEFPROP A RAAMO CC-LOWEST-ADR)
(DEFPROP U RAUSO CC-LOWEST-ADR)
(DEFPROP M RAMMO CC-LOWEST-ADR)
(DEFPROP FS RAFSO CC-LOWEST-ADR)
(DEFPROP FD RAFDO CC-LOWEST-ADR)
(DEFPROP CC RARGO CC-LOWEST-ADR)
(DEFPROP CSW RACSWO CC-LOWEST-ADR)
(DEFPROP RAIDR RARDRO CC-LOWEST-ADR)
(DEFPROP CIB RACIBO CC-LOWEST-ADR)
(DEFPROP OPC RAOPCO CC-LOWEST-ADR)

(DEFPROP C C CC-@-NAME)
(DEFPROP D D CC-@-NAME)
(DEFPROP P P CC-@-NAME)
(DEFPROP /1 1 CC-@-NAME)
(DEFPROP /2 2 CC-@-NAME)
(DEFPROP A A CC-@-NAME)
(DEFPROP U U CC-@-NAME)
(DEFPROP M M CC-@-NAME)

(DEFUN CC-PRINT-ADDRESS-1 (REG-ADR WD ITEMREST)
    WD ITEMREST
    (CC-PRINT-ADDRESS REG-ADR))

(DEFUN CC-PRINT-ADDRESS (REG-ADR)
  (PROG (RANGE-NAME RANGE-BASE @-NAME TEM)
	(SETQ RANGE-NAME (CC-FIND-REG-ADR-RANGE REG-ADR))
	(COND ((AND (SETQ TEM (CC-FIND-CLOSEST-SYM REG-ADR))
		    (OR (ATOM TEM)
			(EQ RANGE-NAME 'C)
			(EQ RANGE-NAME 'D)))
		(PRIN1 TEM))
	      ((SETQ RANGE-BASE (GET RANGE-NAME 'CC-LOWEST-ADR))
		(COND ((SETQ @-NAME (GET RANGE-NAME 'CC-@-NAME))
			(PRIN1 (- REG-ADR (SYMEVAL RANGE-BASE)))
			(PRINC '@)
			(PRIN1 @-NAME))
		      (T (PRIN1 RANGE-NAME)
			 (PRINC '/ )
			 (PRIN1 (- REG-ADR (SYMEVAL RANGE-BASE))))))
	      (T (PRIN1 REG-ADR)))
     X	(RETURN T)
))

(DEFUN CC-CONSOLE-STATUS-DISPLAY (DONT-TOUCH-MACHINE) 
   (PROG (SAVE-CURSOR-POS PC IR)
	(SETQ SAVE-CURSOR-POS (CURSORPOS))
	(CURSORPOS CC-FIRST-STATUS-LINE 0)
	(CURSORPOS 'E)
  	(PRINC '***********************************************)
	(TERPRI)
	(CC-ENTER)
	(PRINC 'PC=)
	(PRINC (SETQ PC (CC-REGISTER-EXAMINE RAPC)))
	(PRINC '/ / / )
	(SETQ IR (CC-REGISTER-EXAMINE RASIR))
	(PRINC 'OBUS=)
	(PRINC (CC-REGISTER-EXAMINE RAOBS))
	(PRINC '|   |)
	(PRIN1 (CC-FIND-CLOSEST-SYM (+ PC RACMO)))	;PRINT SYMBOLIC PC
	(TERPRI)
	(PRINC 'IR=)
   ;if comming in at top level, dont print contents of M or A mem location that does
   ;not have symbolic name.  Problem is that examining does CC-NOOP-CLOCK which results
   ;in loss of state, increments PC, etc etc.
	(CC-TYPE-OUT IR CC-UINST-DESC T DONT-TOUCH-MACHINE)
	(TERPRI)
	(PRINC 'ERROR-STATUS)
	(PRINC '/ )
	(CC-PRINT-ERROR-STATUS  (CC-REGISTER-EXAMINE RASTS))
	(DBG-PRINT-STATUS)	;PRINT UNIBUS, XBUS  PARERRS, NXM
	(TERPRI)
	(CC-RAID)
	(CURSORPOS (CAR SAVE-CURSOR-POS) (CDR SAVE-CURSOR-POS))	;RESTORE CURSOR POS
))

(DEFUN CC-PRINT-SET-BITS (NUM BIT-LIST)
  (PROG (BIT-NUM THIS-BIT-SET)
	(SETQ BIT-NUM 0)
   L	(COND ((OR (= 0 NUM)
		   (NULL BIT-LIST))
		(RETURN T)))
	(COND ((SETQ THIS-BIT-SET (NOT (= 0 (LOGLDB (+ (LSH BIT-NUM 6) 0001) NUM))))
               (SETQ NUM (LOGDPB 0 (+ (LSH BIT-NUM 6) 0001) NUM))))
	(COND ((NULL (CAR BIT-LIST)))
	      ((NOT (ATOM (CAR BIT-LIST)))
		(COND ((FUNCALL (CAAR BIT-LIST) THIS-BIT-SET)
			(PRIN1 (CADAR BIT-LIST))
			(PRINC '/ ))))
	      (THIS-BIT-SET (PRIN1 (CAR BIT-LIST))
			    (PRINC '/ )))
	(SETQ BIT-NUM (1+ BIT-NUM))
	(SETQ BIT-LIST (CDR BIT-LIST))
	(GO L)))


(DEFUN CC-STORE (REG-ADR QUAN)
   (COND ((EQ 'RAIDR (CC-FIND-REG-ADR-RANGE REG-ADR))
	  (STORE (CC-RAID-REG (- REG-ADR RARDRO)) QUAN))
	 (T (CC-REGISTER-DEPOSIT REG-ADR QUAN))))

(DEFUN CC-RAID ()
  (DO ((I 0 (1+ I))
       (TEM)
       (CC-LAST-VALUE-TYPED))
      ((= I 8))
    (COND ((NOT (ZEROP (SETQ TEM (CC-RAID-REG I))))
	   (CC-PRINT-ADDRESS TEM)
	   (PRINC '|//   |)
	   (CC-PRINT-REG-ADR-CONTENTS TEM)
	   (SETQ TEM (CURSORPOS))
	   (COND ((< (CDR TEM) 40.)
		  (CURSORPOS (CAR TEM) 40.))
		 ((TERPRI))) ))))

(DEFPROP // CC-SLASH CC-COMMAND)

(DEFUN CC-SLASH (ADR)
  (PROG NIL 
	(COND ((NULL ADR) (RETURN NIL)))
	(SETQ CC-OPEN-REGISTER ADR)
	(SETQ CC-LAST-OPEN-REGISTER CC-OPEN-REGISTER)
	(PRINC '/ / / )
	(CC-PRINT-REG-ADR-CONTENTS ADR)
))

(PUTPROP (ASCII 15) 'CC-CR 'CC-COMMAND)

(DEFUN CC-CR (QUAN)
  (PROG NIL 
	(COND ((AND QUAN CC-OPEN-REGISTER)
	       (CC-STORE CC-OPEN-REGISTER QUAN)))
	(SETQ CC-OPEN-REGISTER NIL)
	(TERPRI)
))

(PUTPROP (ASCII 12) 'CC-LF 'CC-COMMAND)

(DEFUN CC-LF (QUAN)
  (PROG (TEM) 
	(CC-CR QUAN)
	(CC-PRINT-ADDRESS (SETQ TEM (1+ CC-LAST-OPEN-REGISTER)))
	(PRINC '//)
	(CC-SLASH TEM) ))

(DEFPROP ^ CC-UPAR CC-COMMAND)

(DEFUN CC-UPAR (QUAN)
  (PROG (TEM)
	(CC-CR QUAN)
	(CC-PRINT-ADDRESS (SETQ TEM (1- CC-LAST-OPEN-REGISTER)))
	(PRINC '//)
	(CC-SLASH TEM) ))

(PUTPROP (ASCII 40) 'CC-SPACE 'CC-COMMAND)
(PUTPROP '+ 'CC-SPACE 'CC-COMMAND)

(DEFUN CC-SPACE (ARG)
   ARG)

(PUTPROP (ASCII 14) 'CC-FORM 'CC-COMMAND)

(DEFUN CC-FORM (QUAN)
	QUAN
	(SETQ CC-UPDATE-DISPLAY-FLAG T)
	NIL)

(DEFPROP = CC-EQUALS CC-COMMAND)

(DEFUN CC-EQUALS (QUAN)
	(AND QUAN (SETQ CC-LAST-VALUE-TYPED QUAN))
	#M (SETQ QUAN (STATUS _))
	#M (SSTATUS _ NIL)
	(PRIN1 CC-LAST-VALUE-TYPED)
	#M (SSTATUS _ QUAN)
	(PRINC '/ / / )
	NIL)

(DEFPROP G CC-GO CC-COMMAND)

(DEFUN CC-GO (QUAN)
       (CC-REGISTER-DEPOSIT RASA QUAN)
       (SETQ CC-UPDATE-DISPLAY-FLAG T)
       (TERPRI)
	NIL)

(DEFPROP / CC-STEP CC-COMMAND)

(DEFUN CC-STEP (QUAN)
 (PROG (QN)
	(SETQ QN (OR QUAN 1))
 	(CC-REGISTER-DEPOSIT RASTEP QN)
	(SETQ CC-UPDATE-DISPLAY-FLAG T)
	(AND QUAN (TERPRI))
	(RETURN NIL)))

(DEFPROP / CC-RESET CC-COMMAND)

(DEFUN CC-RESET (QUAN)
	(CC-REGISTER-DEPOSIT RARS (OR QUAN 0))
	(SETQ CC-UPDATE-DISPLAY-FLAG T)
	(TERPRI)
	NIL)

(DEFPROP Q CC-Q CC-COMMAND)

(DEFUN CC-Q (QUAN)
	(PLUS (OR QUAN 0) CC-LAST-VALUE-TYPED))

(DEFPROP / CC-STOP CC-COMMAND)

(DEFUN CC-STOP (QUAN)
  (SETQ CC-PASSIVE-SAVE-VALID NIL)
  (SETQ CC-FULL-SAVE-VALID NIL)		;ASSURE READING FRESH STUFF FROM HARDWARE
  (CC-REGISTER-DEPOSIT RASTOP QUAN)
  (SETQ CC-UPDATE-DISPLAY-FLAG T)
  (TERPRI) )

(DEFPROP / CC-PROCEED CC-COMMAND)

(DEFUN CC-PROCEED (QUAN)
  (PROG (CHAR)
	(QF-CLEAR-CACHE NIL)	;CLEAR PHT CACHE
	(CC-REGISTER-DEPOSIT RAGO (OR QUAN 0))
	(PRINC #Q "RUN"
	       #M (COND ((MEMQ 'SAIL (STATUS FILEMODE TYO))
			 '|RUN|)
			(T '--RUN--)))
   L	(COND (#Q (SETQ CHAR (KBD-TYI-NO-HANG)) #M (NOT (ZEROP (LISTEN))) (GO X1))
	      ((ZEROP (CC-REGISTER-EXAMINE RAGO)) (GO X)))
     #Q (PROCESS-SLEEP 30.)         ;WHY WAIT AS LONG?
     #M (SLEEP 2)
	(GO L)      
   X1 #M(TYI)
        (COND ((= CHAR 203) (BREAK CC-MACHINE-RUNNING) (GO L)))
   X	(PRINC 'STOP)
	(CC-REGISTER-DEPOSIT RASTOP 0)
	(SETQ CC-UPDATE-DISPLAY-FLAG T)
	(TERPRI) ))

(PUTPROP (INTERN (ASCII 11)) 'CC-TAB 'CC-COMMAND)

(DEFUN CC-TAB (QUAN)
  (PROG (TEM)
	(CC-CR NIL) ;DON'T CLOBBER OPEN REGISTER
	(SETQ TEM (OR QUAN CC-LAST-VALUE-TYPED))
	(SETQ TEM (PLUS CC-REG-ADR-VIRT-MEM-OFFSET (QF-POINTER TEM)))
	(CC-PRINT-ADDRESS TEM)
	(PRINC '//)
	(CC-SLASH TEM) ))

(DEFPROP FOOBAR CC-FOO-BAR CC-COMMAND)

(DEFUN CC-FOO-BAR (QUAN)
  (COND ((EQUAL QUAN 105)
	 (CC-REGISTER-DEPOSIT RARS 0)
	 (CC-GO 1)
	 (CC-PROCEED NIL))
	(T (PRINC '|FOOBAR??  |) NIL)))

(DEFPROP / CC-REMOTE-CONSOLE CC-COMMAND)

;REMOTE CONSOLE MODE
(DEFUN CC-REMOTE-CONSOLE (N)
 (PROG ()
   (QF-CLEAR-CACHE NIL)		;CLEAR PHT CACHE
   (CC-REGISTER-DEPOSIT RAGO 1)
   (SETQ CC-UPDATE-DISPLAY-FLAG T)
 A (AND #M (ZEROP (LISTEN)) #Q (NOT (KBD-TYI-NO-HANG)) (GO B))
   (SETQ N (TYI))
   (AND (= N 23) (GO Y))
   (AND (< N 40) (TYO N))
   (AND (= N 15) (TYO 12))   ;NEWIO IS FUCKED UP
   (CC-REGISTER-DEPOSIT RARCON N)
   (GO A)

 B (SETQ N (CC-REGISTER-EXAMINE RARCON))
   (OR (ZEROP N) (GO D))
   (AND (ZEROP (CC-REGISTER-EXAMINE RAGO)) (GO X))
   (SLEEP 1)
   (GO A)

 C (AND (ZEROP (SETQ N (CC-REGISTER-EXAMINE RARCON)))
	(GO A))
 D (TYO N)
   (AND (= N 15) (TYO 12))   ;NEWIO IS FUCKED UP
   (GO C)
 X (PRINC '|/
Machine Stopped/
|)
 Y (CC-REGISTER-DEPOSIT RASTOP 0)
   (SETQ CC-UPDATE-DISPLAY-FLAG T)
))

;;;HIGHER LEVEL STUFF

(DEFPROP AREAS CC-DESCRIBE-AREAS CC-COLON-CMD)

(DEFUN CC-REGISTER-EXAMINE-FIXNUM (ADR)
  (LET ((CONTENTS (CC-REGISTER-EXAMINE ADR)))
    (OR (= (LOGLDB %%Q-DATA-TYPE CONTENTS) DTP-FIX)
	(ERROR (LIST ADR CONTENTS) 'SHOULD-BE-Q-FIXNUM 'FAIL-ACT))
    (LOGAND 77777777 CONTENTS) ))

(DEFUN CC-REGISTER-EXAMINE-PTR (ADR)
  (LOGAND 77777777 (CC-REGISTER-EXAMINE ADR)))

(DEFUN CC-SYMBOLIC-EXAMINE-REGISTER (REG)
  (LET ((ADR (CC-LOOKUP-NAME REG)))
    (COND ((NULL ADR)
	   (PRINT REG) (PRINC '| is undefined.|) (TERPRI)
	   0)
	  (T (CC-REGISTER-EXAMINE ADR)))))

(DEFUN CC-SYMBOLIC-DEPOSIT-REGISTER (REG VAL)
  (LET ((ADR (CC-LOOKUP-NAME REG)))
    (COND ((NULL ADR)
	   (PRINT REG) (PRINC '| is undefined.|) (TERPRI))
	  (T (CC-REGISTER-DEPOSIT ADR VAL)))))

(DEFUN CC-DESCRIBE-AREAS (IGNORE)
  (TERPRI)
  (LET ((A-N (QF-INITIAL-AREA-ORIGIN 'AREA-NAME))
	(A-RL (QF-INITIAL-AREA-ORIGIN 'AREA-REGION-LIST))
	(A-RS (QF-INITIAL-AREA-ORIGIN 'AREA-REGION-SIZE))
	(A-MS (QF-INITIAL-AREA-ORIGIN 'AREA-MAXIMUM-SIZE))

	(R-O (QF-INITIAL-AREA-ORIGIN 'REGION-ORIGIN))
	(R-L (QF-INITIAL-AREA-ORIGIN 'REGION-LENGTH))
	(R-B (QF-INITIAL-AREA-ORIGIN 'REGION-BITS))
	(R-FP (QF-INITIAL-AREA-ORIGIN 'REGION-FREE-POINTER))
	(R-GCP (QF-INITIAL-AREA-ORIGIN 'REGION-GC-POINTER))
	(R-LT  (QF-INITIAL-AREA-ORIGIN 'REGION-LIST-THREAD)))
   (DO ((AREA 0 (1+ AREA))
	(NAREAS 0) (BITS) (A-NAME))
       ((= AREA SIZE-OF-AREA-ARRAYS) (PRINC '|/
Number of active areas = |) (PRINC NAREAS) (TERPRI) )
       (SETQ A-NAME (QF-MEM-READ (+ A-N AREA)))
       (COND ((AND (= (LOGLDB %%Q-DATA-TYPE A-NAME) DTP-SYMBOL)
		   (NOT (ZEROP (LOGLDB %%Q-POINTER A-NAME))))
		(AND (SIGNP GE (- (CAR (CURSORPOS)) CC-FIRST-STATUS-LINE))
		     (PROGN (PRINC '**MORE**) (TYI)
			    (CURSORPOS 0 0) (CURSORPOS 'L)))
		(SETQ NAREAS (1+ NAREAS))
		(PRIN1 AREA)		;AREA NUMBER
		(TYO #M 11 #Q 211)
		(CC-Q-PRINT-TOPLEV A-NAME)  ;AREA-NAME
		(TYO #M 11 #Q 211)
		(PRINC '|Region-size |)
		(CC-Q-PRINT-TOPLEV (QF-MEM-READ (+ A-RS AREA)))
		(PRINC '| Maximum-size |)
		(CC-Q-PRINT-TOPLEV (QF-MEM-READ (+ A-MS AREA)))
		(DO ((RN (LOGLDB %%Q-POINTER (QF-MEM-READ (+ A-RL AREA)))
			 (LOGLDB %%Q-POINTER (QF-MEM-READ (+ R-LT RN)))))
		     ((NOT (ZEROP (LOGAND 1_23. RN)))
		      (COND ((NOT (= AREA (LOGAND RN 777777)))
			     (PRINC '|Region thread not linked back to AREA!!|))))
		   (TERPRI)
                   (PRINC '|   R |)
		   (PRINC RN) 
		   (PRINC '|: Origin |)
		   (PRINC (LOGLDB %%Q-POINTER (QF-MEM-READ (+ R-O RN))))
		   (PRINC '| Length |)
		   (PRINC (LOGLDB %%Q-POINTER (QF-MEM-READ (+ R-L RN))))
		   (PRINC '| Free-Ptr |)
		   (PRINC (LOGLDB %%Q-POINTER (QF-MEM-READ (+ R-FP RN))))
		   (PRINC '| GC-Ptr |)
		   (PRINC (LOGLDB %%Q-POINTER (QF-MEM-READ (+ R-GCP RN))))
                   (PRINC '| |)
                   (SETQ BITS (QF-MEM-READ (+ R-B RN)))
                   (PRINC (NTH (LOGLDB %%REGION-REPRESENTATION-TYPE BITS)
                               '(LIST STRUC 2 3)))
                   (PRINC '| |)
                   (PRINC (NTH (LOGLDB %%REGION-SPACE-TYPE BITS)
                               '(FREE OLD NEW STATIC FIXED EXITED EXIT EXTRA-PDL
                                 WIRED USER-PAGED COPY 13 14 15 16 17))))
		(TERPRI))))))


(DEFPROP ATOM CC-DESCRIBE-ATOM CC-COLON-CMD)

(DEFUN CC-DESCRIBE-ATOM (TEM)
  (SETQ CC-GETSYL-UNRCH NIL CC-GETSYL-UNRCH-TOKEN NIL) ;FLUSH DELIMITER
  (SETQ TEM (READ))	;GET NAME OF ATOM USING LISP SYNTAX
  (TERPRI)
  (SETQ TEM (QF-SYMBOL TEM))
  (COND ((< TEM 0)
	 (PRINC '|Not found.|)
	 (TERPRI))
	(T (CC-DESCRIBE-THIS-ATOM TEM)))
  (TERPRI)
  NIL)

(DEFUN CC-DESCRIBE-THIS-ATOM (ADR)
  (PRINT 'LOCATION)
  (PRIN1 (SETQ ADR (QF-POINTER ADR)))
  (PRINT 'VALUE)
  (CC-Q-PRINT-TOPLEV (CC-MEM-READ (1+ ADR)))
  (PRINT 'FUNCTION)
  (CC-Q-PRINT-TOPLEV (CC-MEM-READ (+ ADR 2)))
  (PRINT 'PLIST)
  (CC-Q-PRINT-TOPLEV (CC-MEM-READ (+ ADR 3)))
  (PRINT 'PACKAGE)
  (CC-Q-PRINT-TOPLEV (CC-MEM-READ (+ ADR 4)))
  (TERPRI))

(DEFPROP AREA CC-WHICH-AREA CC-COLON-CMD)
(DEFUN CC-WHICH-AREA (ARG)
   (SETQ ARG (OR ARG CC-LAST-VALUE-TYPED))
   (LET ((AREA-NUM (QF-AREA-NUMBER-OF-POINTER ARG)))
      (PRINC '|Area # = |) (PRINC AREA-NUM) (PRINC '| |)
      (CC-Q-PRINT-TOPLEV (QF-MEM-READ (+ (QF-INITIAL-AREA-ORIGIN 'AREA-NAME)
					 AREA-NUM)))
      (TERPRI)))

(DEFPROP MAPS CC-MAPS CC-COLON-CMD)
(DEFUN CC-MAPS (ARG)
   (SETQ ARG (OR ARG CC-LAST-VALUE-TYPED))
   (PROG (L1MAPADR L1VAL L2MAPADR L2BITS L2VAL)
	 (SETQ L1MAPADR (LOGLDB 1513 ARG))
	 (SETQ L2BITS (LOGLDB 0805 ARG))
	 (PRINC L1MAPADR) (PRINC '|@1// |) 
	  (PRIN1-THEN-SPACE (SETQ L1VAL (CC-REGISTER-EXAMINE (+ RAM1O L1MAPADR))))
	 (PRINC (SETQ L2MAPADR (+ (CC-SHIFT L1VAL 5) L2BITS))) (PRINC '|@2// |)
	  (PRIN1-THEN-SPACE (SETQ L2VAL (CC-REGISTER-EXAMINE (+ RAM2O L2MAPADR))))
	 (TERPRI)))

;;; STACK PRINTING STUFF

(DECLARE (SPECIAL CC-STACK-VIRTUAL-ADDRESS-MODE CC-STACK-M-AP CC-STACK-PP 
	CC-STACK-SPDL-FRAME-EXISTS  
	CC-STACK-A-QLBNDP CC-STACK-A-QLPDLO 
	CC-STACK-A-QLBNDO CC-STACK-CURRENT-FRAME-TYPE 
	CC-STACK-CURRENT-FRAME-CALL-STATE CC-STACK-CURRENT-FRAME-EXIT-STATE 
	CC-STACK-CURRENT-FRAME-ENTRY-STATE CC-STACK-CURRENT-FRAME-FCTN 
	CC-STACK-PREVIOUS-ACTIVE-FRAME 
        CC-STACK-PREVIOUS-OPEN-FRAME CC-STACK-A-QCSTKG 
	CC-STACK-MACRO-PC CC-STACK-USTACK-DATA))

(DECLARE (SPECIAL %%LP-EXS-EXIT-PC 
		%%LP-CLS-ADI-PRESENT 
		%%LP-CLS-DELTA-TO-ACTIVE-BLOCK 
		%%LP-CLS-DELTA-TO-OPEN-BLOCK 
		%%LP-EXS-BINDING-BLOCK-PUSHED 
		%%LP-EXS-MICRO-STACK-SAVED
		%%LP-ENS-NUM-ARGS-SUPPLIED))

;MODE = NIL -> USE PDL BUFFER ADDRESSES AND ONLY PRINT WHATS IN P.B.
;     = T   -> USE VIRTUAL ADDRESSES  (NOT IMPLEMENTED NOW)
(DEFUN CC-STACK-SET-VARS-FROM-MACHINE (MODE) 
  (PROG (PDL-BUFFER-HEAD PDL-BUFFER-VIRTUAL-ADDRESS)  
	(SETQ CC-STACK-VIRTUAL-ADDRESS-MODE MODE)
	(SETQ CC-STACK-USTACK-DATA (CC-GET-USTACK-DATA-LIST))
	(SETQ CC-STACK-M-AP (CC-SYMBOLIC-EXAMINE-REGISTER 'M-AP)
	      CC-STACK-PP (CC-SYMBOLIC-EXAMINE-REGISTER 'PP)
	      CC-STACK-A-QLBNDP (CC-SYMBOLIC-EXAMINE-REGISTER 'A-QLBNDP)
	      CC-STACK-A-QLBNDO (CC-SYMBOLIC-EXAMINE-REGISTER 'A-QLBNDO) )
	(COND ((NULL MODE)
		(SETQ CC-STACK-A-QLPDLO (CC-SYMBOLIC-EXAMINE-REGISTER 
						'A-PDL-BUFFER-HEAD))
		(SETQ CC-STACK-M-AP (+ CC-STACK-M-AP RAPBO))
		(SETQ CC-STACK-PP (+ CC-STACK-PP RAPBO))
		(SETQ CC-STACK-A-QLPDLO (+ CC-STACK-A-QLPDLO RAPBO)))
	      (T (SETQ CC-STACK-A-QLPDLO (CC-SYMBOLIC-EXAMINE-REGISTER 'A-QLPDLO))
		 (SETQ PDL-BUFFER-HEAD (LOGLDB %%Q-POINTER 
			(CC-SYMBOLIC-EXAMINE-REGISTER 'A-PDL-BUFFER-HEAD)))
		 (SETQ PDL-BUFFER-VIRTUAL-ADDRESS (LOGLDB %%Q-POINTER 
			(CC-SYMBOLIC-EXAMINE-REGISTER 'A-PDL-BUFFER-VIRTUAL-ADDRESS)))
		 (SETQ CC-STACK-M-AP (+ PDL-BUFFER-VIRTUAL-ADDRESS 
			(LOGAND 1777 (- CC-STACK-M-AP PDL-BUFFER-HEAD))))
		 (SETQ CC-STACK-PP (+ PDL-BUFFER-VIRTUAL-ADDRESS 
			(LOGAND 1777 (- CC-STACK-PP PDL-BUFFER-HEAD))))
		))   ;RELOCATE AP, ETC TO VIRTUAL ADDRESSES
	(CC-STACK-CURRENT-FRAME-SETUP MODE)
	(SETQ CC-STACK-SPDL-FRAME-EXISTS NIL)
	(COND ((EQ CC-STACK-CURRENT-FRAME-TYPE 'DTP-FEF-POINTER)
		(SETQ CC-STACK-SPDL-FRAME-EXISTS
			 (NOT (ZEROP (LOGLDB %%M-FLAGS-QBBFL 
					     (CC-SYMBOLIC-EXAMINE-REGISTER 
							'M-FLAGS)))))))
	(SETQ CC-STACK-MACRO-PC 
	      (QF-POINTER (CC-SHIFT (CC-SYMBOLIC-EXAMINE-REGISTER 'LC) -1)))
))

;INDEX BACK TO PREVIOUS STACK FRAME
(DEFUN CC-STACK-NEXT-FRAME-SETUP (MODE)
  (PROG ()
	(COND ((NULL CC-STACK-PREVIOUS-ACTIVE-FRAME) (RETURN NIL)))
	(SETQ CC-STACK-M-AP CC-STACK-PREVIOUS-ACTIVE-FRAME)
	(CC-STACK-CURRENT-FRAME-SETUP MODE)
	(SETQ CC-STACK-USTACK-DATA NIL)
	(COND ((NOT (ZEROP (LOGLDB %%LP-EXS-MICRO-STACK-SAVED
				   CC-STACK-CURRENT-FRAME-EXIT-STATE)))
	       (SETQ CC-STACK-USTACK-DATA (CC-STACK-XFER-USTACK))))
	(SETQ CC-STACK-SPDL-FRAME-EXISTS
	      (LOGLDB %%LP-EXS-BINDING-BLOCK-PUSHED CC-STACK-CURRENT-FRAME-EXIT-STATE))
	(RETURN T)
))


(DEFUN CC-STACK-XFER-USTACK ()
  (PROG (DATA LST)
	(SETQ CC-STACK-USTACK-DATA NIL)
   L	(COND ((NOT (> CC-STACK-A-QLBNDP CC-STACK-A-QLBNDO))
		(PRINT 'BIND-STACK-EXHAUSTED-DURING-USTACK-XFER)
		(RETURN NIL)))
	(SETQ LST (CONS (SETQ DATA (QF-MEM-READ CC-STACK-A-QLBNDP))
			LST))
	(SETQ CC-STACK-A-QLBNDP (1- CC-STACK-A-QLBNDP))
	(COND ((= 0 (LOGLDB %%Q-FLAG-BIT DATA))	;NFLAGB
	       (GO L)))
	(RETURN LST)
))

;SET UP VARS TO FRAME CC-STACK-M-AP POINTS
(DEFUN CC-STACK-CURRENT-FRAME-SETUP (MODE) 
  (PROG (TEM) 
    (COND ((NULL MODE) 
	(SETQ CC-STACK-CURRENT-FRAME-CALL-STATE 
		(CC-REGISTER-EXAMINE (- CC-STACK-M-AP 3)))
	(SETQ CC-STACK-CURRENT-FRAME-EXIT-STATE 
		(CC-REGISTER-EXAMINE (- CC-STACK-M-AP 2)))
	(SETQ CC-STACK-CURRENT-FRAME-ENTRY-STATE 
		(CC-REGISTER-EXAMINE (- CC-STACK-M-AP 1)))
	(SETQ CC-STACK-CURRENT-FRAME-FCTN (CC-REGISTER-EXAMINE CC-STACK-M-AP)) )
    (T 	(SETQ CC-STACK-CURRENT-FRAME-CALL-STATE 
		(CC-MEM-READ (- CC-STACK-M-AP 3)))
	(SETQ CC-STACK-CURRENT-FRAME-EXIT-STATE 
		(CC-MEM-READ (- CC-STACK-M-AP 2)))
	(SETQ CC-STACK-CURRENT-FRAME-ENTRY-STATE 
		(CC-MEM-READ (- CC-STACK-M-AP 1)))
	(SETQ CC-STACK-CURRENT-FRAME-FCTN (CC-MEM-READ CC-STACK-M-AP)) ))

	(SETQ CC-STACK-CURRENT-FRAME-TYPE (NTH (LOGLDB %%Q-DATA-TYPE 
						       CC-STACK-CURRENT-FRAME-FCTN)
						Q-DATA-TYPES))
	(SETQ TEM (LOGLDB %%LP-CLS-DELTA-TO-ACTIVE-BLOCK 
			  CC-STACK-CURRENT-FRAME-CALL-STATE))
	(SETQ CC-STACK-PREVIOUS-ACTIVE-FRAME 
		(COND ((= TEM 0) NIL)
		      (T (- CC-STACK-M-AP TEM)) ))
	(SETQ TEM (LOGLDB %%LP-CLS-DELTA-TO-OPEN-BLOCK 
			  CC-STACK-CURRENT-FRAME-CALL-STATE))
	(SETQ CC-STACK-PREVIOUS-OPEN-FRAME 
		(- CC-STACK-M-AP TEM))
	(SETQ CC-STACK-MACRO-PC	(COND ((EQ CC-STACK-CURRENT-FRAME-TYPE 'DTP-FEF-POINTER)
				       (LOGLDB %%LP-EXS-EXIT-PC 
					       CC-STACK-CURRENT-FRAME-EXIT-STATE))
				      (T -1)))
))

(DEFUN CC-STACK-SPACE-BIND-STACK NIL 
 (PROG (TEM)
	(COND ((EQ CC-STACK-CURRENT-FRAME-TYPE 'DTP-FEF-POINTER)
		(COND (CC-STACK-SPDL-FRAME-EXISTS 
			(CC-STACK-SPACE-BINDING-BLOCK))))
	      ((EQ CC-STACK-CURRENT-FRAME-TYPE 'DTP-U-ENTRY)
		(GO L1)))
   X	(RETURN T)
   L1	(SETQ TEM CC-STACK-USTACK-DATA)
   L2	(COND ((NULL TEM) (GO X))
	      ((NOT (ZEROP (LOGAND (CAR CC-STACK-USTACK-DATA) 40000))) ;PPBSPC
		(CC-STACK-SPACE-BINDING-BLOCK)))
	(SETQ TEM (CDR TEM))
	(GO L2)))

(DEFUN CC-STACK-SPACE-BINDING-BLOCK NIL 
  (PROG (BOUND-LOC-POINTER PREV-CONTENTS)
   L	(COND ((NOT (> CC-STACK-A-QLBNDP CC-STACK-A-QLBNDO))
	       (RETURN T)))
	(SETQ BOUND-LOC-POINTER (QF-MEM-READ CC-STACK-A-QLBNDP))
	(COND ((NOT (= DTP-LOCATIVE (LOGLDB %%Q-DATA-TYPE BOUND-LOC-POINTER)))
	       (PRINT 'BOUND-LOC-POINTER-NOT-LOCATIVE)))
	(SETQ PREV-CONTENTS (QF-MEM-READ (1- CC-STACK-A-QLBNDP)))
	(SETQ CC-STACK-A-QLBNDP (- CC-STACK-A-QLBNDP 2))
	(COND ((NOT (= 0 (LOGLDB %%Q-FLAG-BIT PREV-CONTENTS)))	;NFLAGB
	       (GO L)))
	(RETURN T)
))

(DEFPROP STKP CC-PRINT-PDL CC-COLON-CMD)

(DEFUN CC-PRINT-PDL (CNT) (CC-PRINT-PDL-1 CNT T))

(DEFPROP BAKTRACE CC-BAKTRACE CC-COLON-CMD)
(DEFPROP BACKTRACE CC-BAKTRACE CC-COLON-CMD)

(DEFUN CC-BAKTRACE (CNT) (CC-PRINT-PDL-1 CNT NIL))

(DEFUN CC-PRINT-PDL-1 (CNT PRINT-ARGS-FLAG)	;ARG IS NUMBER OF BLOCKS TO PRINT
 (PROG (MODE)
	(SETQ MODE T)
        (TERPRI)
	(COND ((NULL CNT) (SETQ CNT 100005))
	      ((< CNT 0) (SETQ CNT (- 0 CNT)) (SETQ MODE NIL))) ;NEG NUMBER OF BLOCKS DOESNT
			;GO THRU PAGING HAIR, ETC.
	(CC-STACK-SET-VARS-FROM-MACHINE MODE)
    L	(CC-STACK-PRINT-STACK-FRAME MODE PRINT-ARGS-FLAG)
	(COND ((< (SETQ CNT (1- CNT)) 0) (RETURN T))
	      (#M (NOT (ZEROP (LISTEN)))
	       #Q (KBD-TYI-NO-HANG)
	       (RETURN 'LISTEN)))
	(CC-STACK-SPACE-BIND-STACK)
	(COND ((NULL (CC-STACK-NEXT-FRAME-SETUP MODE)) (RETURN T)))
	(GO L)
))

(DEFUN CC-STACK-PRINT-STACK-FRAME (MODE PRINT-ARGS-FLAG) 
  (PROG (ADR CNT) ;TEM ADL-POINTER ARG-DESC VAR-NAME SV-LIST-POINTER
	(DECLARE (FIXNUM ADR CNT)) ;TEM ADL-POINTER ARG-DESC SV-LIST-POINTER
	(PRIN1-THEN-SPACE CC-STACK-M-AP)
	(CC-Q-PRINT-TOPLEV CC-STACK-CURRENT-FRAME-FCTN)
	(PRINC '/[)
	(PRIN1 (LOGLDB %%LP-EXS-EXIT-PC CC-STACK-CURRENT-FRAME-EXIT-STATE))
	(PRINC '/])
	(COND ((NULL PRINT-ARGS-FLAG) (TERPRI) (RETURN T)))
	(SETQ ADR (1+ CC-STACK-M-AP))
	(SETQ CNT (LOGLDB %%LP-ENS-NUM-ARGS-SUPPLIED 
			  CC-STACK-CURRENT-FRAME-ENTRY-STATE))
;	(COND ((AND (EQ CC-STACK-CURRENT-FRAME-TYPE 'DTP-FEF-POINTER)
;		    (NOT (= 0 (LOGLDB-FROM-FIXNUM %%FEFH-SV-BIND 
;				     (QF-MEM-READ (LOGLDB-FROM-FIXNUM %%Q-POINTER 
;							  CC-STACK-CURRENT-FRAME-FCTN))))))
;		(SETQ ADL-POINTER (LOGLDB-FROM-FIXNUM %%Q-POINTER 
;		   (+ CC-STACK-CURRENT-FRAME-FCTN (LOGLDB-FROM-FIXNUM %%FEFHI-MS-ARG-DESC-ORG 
;		      (QF-MEM-READ (LOGLDB-FROM-FIXNUM %%Q-POINTER 
;			(+ CC-STACK-CURRENT-FRAME-FCTN %FEFHI-MISC)))))))
;		(SETQ SV-LIST-POINTER (LOGLDB-FROM-FIXNUM %%Q-POINTER 
;		   (+ CC-STACK-CURRENT-FRAME-FCTN %FEFHI-SPECIAL-VALUE-CELL-PNTRS)))))
    L	(COND ((= CNT 0) (TERPRI) (RETURN T)))
	(PRINC '/ )
;	(COND (ADL-POINTER 
;		(SETQ ARG-DESC (QF-MEM-READ ADL-POINTER))
;		(SETQ VAR-NAME NIL)
;		(SETQ ADL-POINTER 
;		  (+ ADL-POINTER 
;		     1 
;		     (COND ((= 0 (LOGLDB-FROM-FIXNUM %%FEF-NAME-PRESENT ARG-DESC)) 0) 
;			    (T (SETQ VAR-NAME (QF-MEM-READ (+ ADL-POINTER 1))) 1))
;		     (COND ((OR (= (SETQ TEM (LOGLDB-FROM-FIXNUM %%FEF-INIT-OPTION ARG-DESC))
;				   FEF-INI-PNTR)
;				(= TEM FEF-INI-C-PNTR)
;				(= TEM FEF-INI-OPT-SA)
;				(= TEM FEF-INI-EFF-ADR))
;			      1)
;			    (T 0)))) ))
	(CC-Q-PRINT-TOPLEV (COND ((NULL MODE) (CC-REGISTER-EXAMINE ADR))
				 (T (CC-MEM-READ ADR))))
	(SETQ CNT (1- CNT) ADR (1+ ADR))
	(GO L)
 ))

(DEFUN CC-GET-USTACK-DATA-LIST NIL   ;RETURNS A LIST OF CONTENTS OF USTACK
   (PROG (USP DATA)		     ; CAR OF RESULT WOULD BE POPJ ED TO FIRST
	(SETQ USP (CC-SYMBOLIC-EXAMINE-REGISTER 'USP))
    L	(COND ((NOT (> USP 0)) (RETURN (NREVERSE DATA))))
	(SETQ DATA (CONS (CC-REGISTER-EXAMINE (+ USP RAUSO))
			 DATA))
	(SETQ USP (1- USP))
	(GO L)))


;;; BREAKPOINTS	

(DEFPROP HERE CC-HERE CC-COLON-CMD)

(DEFUN CC-HERE (QUAN)
  (SETQ CC-REMOTE-CONSOLE-MODE T)
  QUAN)

(DEFPROP THERE CC-THERE CC-COLON-CMD)

(DEFUN CC-THERE (QUAN)
  (SETQ CC-REMOTE-CONSOLE-MODE NIL)
  QUAN)

(DEFPROP LISTB CC-LIST-BREAKPOINTS CC-COLON-CMD)

(DEFUN CC-LIST-BREAKPOINTS (QUAN)
  QUAN
  (AND CC-BREAKPOINT-LIST (PRINT 'PERMANENT-BREAKPOINTS))
  (DO X CC-BREAKPOINT-LIST (CDR X) (NULL X)
    (TERPRI) (CC-PRINT-ADDRESS (CAR X)))
  (AND CC-TEMPORARY-BREAKPOINT-LIST (PRINT 'TEMPORARY-BREAKPOINTS))
  (DO X CC-TEMPORARY-BREAKPOINT-LIST (CDR X) (NULL X)
    (TERPRI) (CC-PRINT-ADDRESS (CDR X)))
  (AND (NULL CC-BREAKPOINT-LIST) (NULL CC-TEMPORARY-BREAKPOINT-LIST)
       (PRINT 'NONE))
  (CC-CR NIL))

(DEFPROP B CC-SET-PERM-BKPT CC-COLON-CMD)

(DEFUN CC-SET-PERM-BKPT (QUAN)
  (CC-SET-BREAKPOINT (OR QUAN CC-LAST-OPEN-REGISTER) T)
  (CC-CR NIL))

(DEFPROP TB CC-SET-TEMP-BKPT CC-COLON-CMD)

(DEFUN CC-SET-TEMP-BKPT (QUAN)
  (CC-SET-BREAKPOINT (OR QUAN CC-LAST-OPEN-REGISTER) NIL)
  (CC-CR NIL))

(DEFPROP TBP CC-SET-TEMP-BKPT-CONTIN CC-COLON-CMD)

(DEFUN CC-SET-TEMP-BKPT-CONTIN (QUAN)
  (CC-SET-BREAKPOINT (OR QUAN CC-LAST-OPEN-REGISTER) NIL)
  (CC-CONTIN NIL))

(DEFPROP G CC-LOAD-ADDR-CONTIN CC-COLON-CMD)

(DEFUN CC-LOAD-ADDR-CONTIN (QUAN)
  (CC-GO 1)
  (CC-CONTIN QUAN))

(DEFPROP P CC-CONTIN CC-COLON-CMD)

(DEFUN CC-CONTIN (QUAN)
  QUAN
  (COND (CC-REMOTE-CONSOLE-MODE
	 (CC-REMOTE-CONSOLE 0))
	(T (CC-PROCEED NIL)))
  (COND ((NOT (NULL CC-TEMPORARY-BREAKPOINT-LIST))
	 (MAPC 'CC-UNSET-BREAKPOINT CC-TEMPORARY-BREAKPOINT-LIST)
	 (PRINT '(TEMPORARY-BREAKPOINTS-REMOVED))))
  (CC-CR NIL))

(DEFPROP UB CC-UNSET-BKPT CC-COLON-CMD)

(DEFUN CC-UNSET-BKPT (LOC)
  (OR LOC (SETQ LOC CC-LAST-OPEN-REGISTER))
  (COND ((OR (MEMBER LOC CC-BREAKPOINT-LIST)
	     (MEMBER LOC CC-TEMPORARY-BREAKPOINT-LIST))
	   (CC-UNSET-BREAKPOINT (OR LOC CC-LAST-OPEN-REGISTER)))
	(T (PRINT 'NO-BREAKPOINT-AT)
	   (CC-PRINT-ADDRESS LOC)))
  (CC-CR NIL))

(DEFPROP UAB CC-UNSET-ALL-BKPTS CC-COLON-CMD)
  
(DEFUN CC-UNSET-ALL-BKPTS (QUAN)
  QUAN
  (MAPC 'CC-UNSET-BREAKPOINT CC-BREAKPOINT-LIST)
  (MAPC 'CC-UNSET-BREAKPOINT CC-TEMPORARY-BREAKPOINT-LIST)
  (CC-CR NIL))

(DEFUN CC-SET-BREAKPOINT (LOC PERMANENT)
  (PROG (CONTENTS MF)
    (OR (EQ 'C (CC-FIND-REG-ADR-RANGE LOC))
	(RETURN (PRINT 'BKPT-NOT-IN-C-MEM)))
    (SETQ CONTENTS (CC-REGISTER-EXAMINE LOC))
    (SETQ MF (LOGLDB 1202 CONTENTS))
    (COND ((= MF 0))
	  ((= MF 1) (PRINT '(WARNING/, BKPT ALREADY SET)))
	  (T (RETURN (PRINT '(SORRY/, MF FIELD ALREADY IN USE THIS INSTRUCTION)))))
    (CC-REGISTER-DEPOSIT LOC (LOGDPB 1 1202 CONTENTS))
    (COND (PERMANENT
	   (SETQ CC-BREAKPOINT-LIST (CONS LOC CC-BREAKPOINT-LIST)))
	  ((SETQ CC-TEMPORARY-BREAKPOINT-LIST (CONS LOC CC-TEMPORARY-BREAKPOINT-LIST))))
   ))

(DEFUN CC-UNSET-BREAKPOINT (LOC)
  (PROG (CONTENTS MF)
    (OR (EQ 'C (CC-FIND-REG-ADR-RANGE LOC))
	(RETURN (PRINT 'BKPT-NOT-IN-C-MEM)))
    (SETQ CONTENTS (CC-REGISTER-EXAMINE LOC))
    (SETQ MF (LOGLDB 1202 CONTENTS))
    (COND ((NOT (= MF 1))
	   (PRINT 'BREAKPOINT-CLOBBERED)
	   (CC-PRINT-ADDRESS LOC))
	  (T (CC-REGISTER-DEPOSIT LOC (LOGDPB 0 1202 CONTENTS))))
    (SETQ CC-BREAKPOINT-LIST (DELETE LOC CC-BREAKPOINT-LIST))
    (SETQ CC-TEMPORARY-BREAKPOINT-LIST (DELETE LOC CC-TEMPORARY-BREAKPOINT-LIST))
   ))

(DEFPROP CHECK-MAP CC-CHECK-MAP CC-COLON-CMD)

(ARRAY CC-LEVEL-1-MAP-FREQUENCIES FIXNUM 32.)
(ARRAY CC-LEVEL-1-REVERSE-MAP FIXNUM 32.)

(DEFUN CC-CHECK-MAP (TEM)
 (PROG (NUM-CHECKED-OK L1-MAP PRINT-MAP-FREQS)
  (SETQ NUM-CHECKED-OK 0)
  (DO I 0 (1+ I) (= I 32.)
      (STORE (CC-LEVEL-1-MAP-FREQUENCIES I) 0))
  (DO ((ADR RAM1O (1+ ADR)))
      ((= ADR RAM1E))
    (SETQ L1-MAP (CC-REGISTER-EXAMINE ADR))
    (COND ((OR (< L1-MAP 0) (> L1-MAP 31.))
	   (PRINT (LIST 'BAD-LEVEL-1-MAP-VALUE ADR L1-MAP)))
	  (T
	    (STORE (CC-LEVEL-1-MAP-FREQUENCIES L1-MAP)
		   (SETQ TEM (1+ (CC-LEVEL-1-MAP-FREQUENCIES L1-MAP))))
	    (COND ((AND (NOT (= L1-MAP 37))	 ;NO LEVEL 1 MAP ENTRYS SHOULD BE DUPLICATED
			(NOT (= TEM 1)))	 ; EXCEPT 37
		   (FORMAT T "~%TRIGGERED ON L1-MAP ~O, FREQ ~O" L1-MAP TEM)
		   (SETQ PRINT-MAP-FREQS T)))
	    (STORE (CC-LEVEL-1-REVERSE-MAP L1-MAP)
		   (- ADR RAM1O)))))
  (COND (PRINT-MAP-FREQS
	  (DO ((I 0 (1+ I)))
	      ((= I 32.))
	    (PRINT (LIST 'LEVEL-1-MAP-VALUE I 'APPEARS (CC-LEVEL-1-MAP-FREQUENCIES I))))))
  (DO ((I 0 (1+ I))
       (NOT-37S 0))
      ((= I 31.)
       (PRINT (LIST 'NOT-37S NOT-37S)))
    (SETQ TEM (CC-LEVEL-1-MAP-FREQUENCIES I))
    (SETQ NOT-37S (+ NOT-37S TEM))
    (COND ((ZEROP TEM)
	   (PRINT (LIST 'LEVEL-2-MAP-BLOCK I 'NOT-USED))
	   (GO E))
	  ((> TEM 1)
	   (PRINT (LIST 'LEVEL-2-MAP-BLOCK I 'USED TEM 'TIMES))))
    (SETQ NUM-CHECKED-OK 
	  (+ (CC-CHECK-LEVEL-2-BLOCK (CC-SHIFT (CC-LEVEL-1-REVERSE-MAP I) 13.)
				     I)
	     NUM-CHECKED-OK))
    E)
  (PRINT (LIST NUM-CHECKED-OK 'MAP-ENTRIES-CHECKED-OK))
  (RETURN NIL)))

(DECLARE (SPECIAL %%PHT2-PHYSICAL-PAGE-NUMBER %%PHT2-META-BITS %%PHT2-MAP-ACCESS-CODE))

(DEFUN CC-CHECK-LEVEL-2-BLOCK (VIRTUAL-BASE LEVEL-2-BLOCK-NUMBER)
 (PROG (L2M PHT-ADR PHT-VALUE VIR-ADR NUM-CHECKED-OK L2MAP-SA)
   (SETQ NUM-CHECKED-OK 0)
   (SETQ L2MAP-SA (+ RAM2O (LSH LEVEL-2-BLOCK-NUMBER 5)))
   (DO ADR-IN-BLOCK 0 (1+ ADR-IN-BLOCK) (= ADR-IN-BLOCK 32.)
       (SETQ L2M (CC-REGISTER-EXAMINE (+ ADR-IN-BLOCK L2MAP-SA)))
       (COND ((>= (LOGLDB %%PHT2-MAP-STATUS-CODE L2M)    ;IF LEVEL 2 MAP SET UP ...
		  %PHT-MAP-STATUS-READ-ONLY)
	      (SETQ PHT-ADR (QF-PAGE-HASH-TABLE-LOOKUP
			     (SETQ VIR-ADR (+ VIRTUAL-BASE (LSH ADR-IN-BLOCK 8)))))
	      (COND ((< PHT-ADR 0)
		     (COND ((< VIR-ADR A-MEMORY-VIRTUAL-ADDRESS)
			    (PRINT (LIST 'MAP-ENTRY-AT-VIRTUAL-ADDRESS
					 VIR-ADR 'NOT-FOUND-IN-PHT))
			    (PRINT (LIST 'MAP-VALUE L2M
					 'LEVEL-2-MAP-ADR (+ (LSH LEVEL-2-BLOCK-NUMBER
								  5)
							     ADR-IN-BLOCK))))))
		    ((NOT (= (LOGLDB %%PHT2-PHYSICAL-PAGE-NUMBER L2M)
			     (LOGLDB %%PHT2-PHYSICAL-PAGE-NUMBER
				     (SETQ PHT-VALUE (PHYS-MEM-READ (1+ PHT-ADR))))))
		     (PRINT (LIST 'MAP-ENTRY-AT-VIRTUAL-ADDRESS VIR-ADR 'DIFFERS-FROM-PHT))
		     (PRINT (LIST 'MAP-VALUE L2M
				  'PHT-VALUE PHT-VALUE 
				  'PHT-ADR PHT-ADR 
				  'LEVEL-2-MAP-ADR (+ (LSH LEVEL-2-BLOCK-NUMBER
							   5)
						      ADR-IN-BLOCK))))
		    (T (SETQ NUM-CHECKED-OK (1+ NUM-CHECKED-OK)))))))
   (RETURN NUM-CHECKED-OK)))

(DEFPROP MEMSTAT CC-DESCRIBE-MEMORY CC-COLON-CMD)

;:MEMSTAT TELL ALL ABOUT ALL PAGES THAT ARE SWAPPED IN
(DEFUN CC-DESCRIBE-MEMORY (TEM)
  ;GET LIST OF REVERSE LISTS (FOR CONTIG AREAS) OF LISTS
  ;EACH 3RD LEVEL LIST IS VIRTUAL ADDR, AREA NUMBER, SWAP STATUS, PHYS ADDR,
  ; META BITS, MAP STATUS, ACCESS CODE 
  (DO L (CC-DESCRIBE-MEMORY-COLLECT-CONTIG (CC-DESCRIBE-MEMORY-COPY-OUT-PHT) NIL)
      (CDR L) (NULL L)					;PROCESS EACH CONTIG AREA
    (DO LL (CC-DESCRIBE-MEMORY-CONTIG-SPLITUP (NREVERSE (CAR L)) NIL)
	   (CDR LL) (NULL LL)
      (PRINT (CAAAR LL))				;FIRST VIRTUAL ADDRESS
      (TYO 11)
      (COND ((< (CADAAR LL) (LENGTH AREA-LIST))		;PRINT AREA NAME, TRYING TO BE
	     (PRIN1-THEN-SPACE (NTH (CADAAR LL) AREA-LIST)))	;QUICK ABOUT IT
	    (T (CC-Q-PRINT-TOPLEV (QF-MEM-READ (+ (CADAAR LL)
						  (QF-INITIAL-AREA-ORIGIN 'AREA-NAME))))
	       (TYO 40)))
      (PRIN1 (// (- (CAAAR LL)
		     (SETQ TEM (QF-POINTER		;AREA NUMBER TO AREA ORIGIN
				 (QF-MEM-READ (+ (CADAAR LL)
						 (QF-INITIAL-AREA-ORIGIN 'AREA-ORIGIN))))))
		  PAGE-SIZE))				;FIRST RELATIVE PAGE NUM
      (COND ((> (LENGTH (CAR LL)) 1)			;IF MULTI PAGES CONTIG
	     (PRINC '-)
	     (PRIN1 (// (- (CAAR (LAST (CAR LL))) TEM) PAGE-SIZE))))
      (PRINC '|  -->  |)				;MAPS ONTO
      (CC-DESCRIBE-MEMORY-PRINT-ATTRIB (CAAR LL))))	;SAY WHAT IT MAPS ONTO
  (TERPRI)
  NIL)

(DEFUN CC-DESCRIBE-MEMORY-PRINT-ATTRIB (X)
  (PRIN1-THEN-SPACE (CADDDR X))				;PHYSICAL ADDRESS
  (AND (CADDR X) (PRIN1-THEN-SPACE (CADDR X)))		;SWAP STATUS IF ABNORMAL
  (PRIN1-THEN-SPACE (CADR (CDDDDR X)))			;MAP STATUS
  (AND (CADDR (CDDDDR X))
       (PRIN1-THEN-SPACE (CADDR (CDDDDR X))))		;ACCESS IF ANY
  (OR (= 0 (CAR (CDDDDR X)))
      (PRIN1 'META-BITS=)
      (PRIN1 (CAR (CDDDDR X)))))			;META BITS IF NON-ZERO

  ;GET LIST OF REVERSE LISTS (FOR CONTIG AREAS) OF LISTS
  ;EACH 3RD LEVEL LIST IS VIRTUAL ADDR, AREA NUMBER, SWAP STATUS, PHYS ADDR,
  ; META BITS, MAP STATUS, ACCESS CODE 
;CONVERT ONE LIST OF PAGES INTO N, FOR THE CONTIGUOUS SUBSETS
(DEFUN CC-DESCRIBE-MEMORY-CONTIG-SPLITUP (LL PREV-CONTIG)
  (COND ((NULL PREV-CONTIG)
	 (CC-DESCRIBE-MEMORY-CONTIG-SPLITUP (CDR LL) (LIST (CAR LL))))
	((NULL LL)
	 (LIST (NREVERSE PREV-CONTIG)))
	((AND (= (- (CADDDR (CAR LL)) PAGE-SIZE)
		 (CADDDR (CAR PREV-CONTIG)))	;PHYS ADDRS AGREE
	      (EQ (CADDR (CAR LL)) (CADDR (CAR PREV-CONTIG)))	;SWAP STATUS AGREE
	      (EQUAL (CDDDDR (CAR LL)) (CDDDDR (CAR PREV-CONTIG)))) ;OTHER STUFF AGREES
	 (CC-DESCRIBE-MEMORY-CONTIG-SPLITUP (CDR LL) (CONS (CAR LL) PREV-CONTIG)))
	(T							;START NEW CONTIG FROB
	 (CONS (NREVERSE PREV-CONTIG)
	       (CC-DESCRIBE-MEMORY-CONTIG-SPLITUP (CDR LL) (LIST (CAR LL)))))))

(DEFUN CC-DESCRIBE-MEMORY-COLLECT-CONTIG (SORTED-PHT-LIST PREVIOUS-CONTIG-LIST)
  (COND ((NULL SORTED-PHT-LIST)
	 (AND PREVIOUS-CONTIG-LIST (LIST PREVIOUS-CONTIG-LIST)))
	((NULL PREVIOUS-CONTIG-LIST)
	 (CC-DESCRIBE-MEMORY-COLLECT-CONTIG (CDR SORTED-PHT-LIST)
					    (LIST (CAR SORTED-PHT-LIST))))
	((AND (= (CADAR SORTED-PHT-LIST) (CADAR PREVIOUS-CONTIG-LIST)) ;SAME AREA
	      (= (CAAR SORTED-PHT-LIST)
		 (+ PAGE-SIZE (CAAR PREVIOUS-CONTIG-LIST)))) ;NEXT VIR ADR
	 (CC-DESCRIBE-MEMORY-COLLECT-CONTIG (CDR SORTED-PHT-LIST)
					    (CONS (CAR SORTED-PHT-LIST) PREVIOUS-CONTIG-LIST)))
	(T
	 (CONS PREVIOUS-CONTIG-LIST
	       (CC-DESCRIBE-MEMORY-COLLECT-CONTIG SORTED-PHT-LIST NIL)))))

(DECLARE (SPECIAL %PHT-DUMMY-VIRTUAL-ADDRESS %%PHT1-VIRTUAL-PAGE-NUMBER))

(DEFUN CC-DESCRIBE-MEMORY-COPY-OUT-PHT NIL 
  (SORTCAR
    (DO ((PHTP (QF-POINTER (PHYS-MEM-READ (+ PAGE-SIZE %SYS-COM-PAGE-TABLE-PNTR))) (+ PHTP 2))
	 (COUNT (// (QF-POINTER (PHYS-MEM-READ (+ PAGE-SIZE %SYS-COM-PAGE-TABLE-SIZE))) 2)
		(1- COUNT))
	 (PHT1)
	 (PHT2)
	 (VIRAD)
	 (LST NIL))
	((= 0 COUNT) LST)
      (DECLARE (FIXNUM PHTP COUNT PHT1 PHT2 VIRAD))
      (COND ((AND (NOT (= 0 (LOGAND 100 (SETQ PHT1 (PHYS-MEM-READ PHTP)))))
		  (NOT (= %PHT-DUMMY-VIRTUAL-ADDRESS (LOGLDB %%PHT1-VIRTUAL-PAGE-NUMBER PHT1))))
	     (SETQ LST (CONS (LIST (SETQ VIRAD (* PAGE-SIZE		;VIRTUAL ADDRESS
						  (LOGLDB %%PHT1-VIRTUAL-PAGE-NUMBER PHT1)))

				   (QF-AREA-NUMBER-OF-POINTER VIRAD)	;AREA NUMBER
				   (NTH (LOGLDB %%PHT1-SWAP-STATUS-CODE PHT1)
					'(SWAP-STATUS-ZERO?
					  NIL FLUSHABLE SWAP-STATUS-PDL-BUFFER
					  AGE-TRAP WIRED SWAP-STATUS-6?
					  SWAP-STATUS-7?))
				   (* PAGE-SIZE		;PHYSICAL ADDRESS
				      (LOGLDB %%PHT2-PHYSICAL-PAGE-NUMBER
					      (SETQ PHT2 (PHYS-MEM-READ (1+ PHTP)))))
				   (LOGLDB %%PHT2-META-BITS PHT2)
				   (NTH (LOGLDB %%PHT2-MAP-STATUS-CODE PHT2)
					'(LEVEL-1-MAP-NOT-VALID?
					  LEVEL-2-MAP-NOT-VALID?
					  READ-ONLY READ-WRITE-FIRST READ-WRITE
					  MAP-STATUS-PDL-BUFFER
					  MAP-STATUS-6? MAP-STATUS-7?))
				   (NTH (LOGLDB %%PHT2-MAP-ACCESS-CODE PHT2)
					'(NIL NIL R-ACCESS R-W-ACCESS)))
			     LST)))))
    (FUNCTION <)))

(DEFPROP RELPC CC-RELPC CC-COLON-CMD)

(DEFUN CC-RELPC (IGNORE)
  (PROG (M-AP LC)
	(SETQ M-AP (CC-REGISTER-EXAMINE (+ (CC-SYMBOLIC-EXAMINE-REGISTER 'M-AP)
					   RAPBO)))
	(CC-Q-PRINT-TOPLEV M-AP)
	(COND ((= (LOGLDB %%Q-DATA-TYPE M-AP)
		  DTP-FEF-POINTER)
	       (SETQ LC
		     (QF-POINTER (CC-SHIFT (CC-SYMBOLIC-EXAMINE-REGISTER 'LC) -1)))
	       (TYO 40)
	       (PRIN1 (- (QF-POINTER LC)
			 (* 2 (QF-POINTER M-AP)))) ))))

(DECLARE (SPECIAL %%LP-EXS-EXIT-PC %%LP-CLS-DELTA-TO-ACTIVE-BLOCK 
		  %%LP-CLS-DELTA-TO-OPEN-BLOCK 
		  %%LP-ENS-NUM-ARGS-SUPPLIED))

(DEFUN (TRACE CC-COLON-CMD) (COUNT)
     (CC-TRACE-COMMAND COUNT T))

(DEFUN (TRACEN CC-COLON-CMD) (COUNT)
     (CC-TRACE-COMMAND COUNT NIL))

(DEFUN CC-TRACE-COMMAND (COUNT PRINT-ARGS-P)
    (TERPRI)
    (CC-TRACE-THE-STACK (COND ((NULL COUNT)
			       (SETQ COUNT 7777777)
			       (COND ((AND CC-LAST-VALUE-TYPED
					   (= (QF-DATA-TYPE CC-LAST-VALUE-TYPED)
					      DTP-STACK-GROUP))
				      CC-LAST-VALUE-TYPED)
				     (T T)))
			      ((MINUSP COUNT)
			       (SETQ COUNT (- COUNT))
			       NIL)
			      (T T))
			PRINT-ARGS-P
			COUNT))

;; First argument, MODE, is NIL to use the current stack group from the pdl buffer,
;; T for the current stack group from memory, or a stack group to trace.
;; Second argument, PRINT-ARGS-P, is T if you want the arguments to be printed
;; for each frame.
(DEFUN CC-TRACE-THE-STACK (MODE PRINT-ARGS-P COUNT)
    (PROG (M-AP CALL-WORD EXIT-WORD ENTRY-WORD FUNCTION-WORD FRAME-TYPE TIMES)
	  (SETQ TIMES 0)
	  (SETQ M-AP
		(QF-POINTER
		 (COND ((NULL MODE)
			(+ RAPBO (CC-SYMBOLIC-EXAMINE-REGISTER 'M-AP)))
		       ((EQ MODE T)
			(+ (QF-POINTER (CC-SYMBOLIC-EXAMINE-REGISTER
					'A-PDL-BUFFER-VIRTUAL-ADDRESS))
			   (LOGAND 1777 (- (CC-SYMBOLIC-EXAMINE-REGISTER 'M-AP)
					   (QF-POINTER (CC-SYMBOLIC-EXAMINE-REGISTER
							'A-PDL-BUFFER-HEAD))))))
		       (T (SETQ MODE (QF-MAKE-Q MODE DTP-ARRAY-POINTER))
			  (LET ((RP (QF-ARRAY-LEADER MODE SG-REGULAR-PDL)))
			    (+ RP
			       (QF-ARRAY-LEADER MODE SG-AP)
			       1
			       (LOGLDB %%ARRAY-LONG-LENGTH-FLAG (CC-MEM-READ RP))))))))
	LOOP
	  (OR #M (ZEROP (LISTEN)) #Q (NOT (KBD-TYI-NO-HANG)) (RETURN NIL))
	  (COND ((NULL MODE) 
		 (SETQ CALL-WORD (CC-REGISTER-EXAMINE (- M-AP 3)))
		 (SETQ EXIT-WORD (CC-REGISTER-EXAMINE (- M-AP 2)))
		 (SETQ ENTRY-WORD (CC-REGISTER-EXAMINE (- M-AP 1)))
		 (SETQ FUNCTION-WORD (CC-REGISTER-EXAMINE M-AP)))
		(T
		 (SETQ CALL-WORD (CC-MEM-READ (- M-AP 3)))
		 (SETQ EXIT-WORD (CC-MEM-READ (- M-AP 2)))
		 (SETQ ENTRY-WORD (CC-MEM-READ (- M-AP 1)))
		 (SETQ FUNCTION-WORD (CC-MEM-READ M-AP))))
	  (SETQ FRAME-TYPE (NTH (LOGLDB %%Q-DATA-TYPE FUNCTION-WORD) Q-DATA-TYPES))

	  ;;; Print out info about this frame.
	  (PRIN1-THEN-SPACE M-AP)
	  (CC-Q-PRINT-TOPLEV FUNCTION-WORD)
	  (COND ((AND (EQ FRAME-TYPE 'DTP-FEF-POINTER)
		      (NOT (AND (ZEROP TIMES)
				(MEMQ MODE '(T NIL)))))
		 (PRINC '/[)
		 (PRIN1 (LOGLDB %%LP-EXS-EXIT-PC EXIT-WORD))
		 (PRINC '/])))
	  (COND (PRINT-ARGS-P
		 (DO ((ADR (1+ M-AP) (1+ ADR))
		      (CC-SEXP-PRINLEVEL 2)
		      (CC-SEXP-PRINLENGTH 3)
		      (CNT (LOGLDB %%LP-ENS-NUM-ARGS-SUPPLIED ENTRY-WORD) (1- CNT)))
		     ((ZEROP CNT))
		   (DECLARE (FIXNUM ADR CNT))
		   (PRINC '| |)
		   (CC-Q-PRINT-TOPLEV (COND ((NULL MODE) (CC-REGISTER-EXAMINE ADR))
					    (T (CC-MEM-READ ADR)))))))
	  (TERPRI)
	  (OR (< (SETQ TIMES (1+ TIMES)) COUNT)
	      (RETURN NIL))
	  (LET ((DELTA (LOGLDB %%LP-CLS-DELTA-TO-ACTIVE-BLOCK CALL-WORD)))
	    (COND ((ZEROP DELTA) (RETURN NIL))
		  (T (SETQ M-AP (- M-AP DELTA))
		     (GO LOOP))))
	  ))

(DECLARE (SPECIAL %%FEFH-PC %FEFHI-IPC %FEFHI-STORAGE-LENGTH))

(DECLARE (NOTYPE (CC-CODE-INSN FIXNUM FIXNUM FIXNUM)))

(DEFUN (CODE CC-COLON-CMD) (ARG)
   (TERPRI)
   (LET ((PC (QF-POINTER (CC-SHIFT (CC-SYMBOLIC-EXAMINE-REGISTER 'LC) -1)))
	 (FEF (CC-REGISTER-EXAMINE (+ (CC-SYMBOLIC-EXAMINE-REGISTER 'M-AP)
				      RAPBO))))
     (DECLARE (FIXNUM PC FEF))
     (COND ((NOT (= (QF-DATA-TYPE FEF) DTP-FEF-POINTER))
	    (PRINC '|The current function is not a FEF.|) (TERPRI))
	   (T (SETQ FEF (QF-POINTER FEF))
	      (PRINC '|Current FEF is |)
	      (CC-Q-PRINT-TOPLEV (CC-MEM-READ (+ %FEFHI-FCTN-NAME FEF)))
	      (TERPRI)
	      (LET ((RELPC (- PC (* 2 FEF))))
		(DECLARE (FIXNUM RELPC))
		(COND ((OR (< RELPC 10) (> RELPC 10000))
		       (PRINC '|The PC does not seem to be pointer to the running FEF.|)
		       (TERPRI))
		      (T (CC-DISASSEMBLE-FEF FEF (COND ((EQ ARG 1) NIL)
						       (T RELPC))))))))))

(DEFUN (DISASSEMBLE-FEF CC-COLON-CMD) (ARG)
  ARG
  (AND CC-LAST-VALUE-TYPED
       (= (QF-DATA-TYPE CC-LAST-VALUE-TYPED) DTP-FEF-POINTER)
       (PROGN (PRINC '|Type center PC or NIL|)
	      (CC-DISASSEMBLE-FEF CC-LAST-VALUE-TYPED (READ)))))

(DECLARE (NOTYPE (CC-DISASSEMBLE-FEF FIXNUM NOTYPE)))

(DEFUN CC-DISASSEMBLE-FEF (FEF CENTER-PC)
  (LET ((FROM-RELPC 0) (TO-RELPC 0)
	(INIT-RELPC (LOGLDB %%FEFH-PC
			    (CC-MEM-READ (+ FEF %FEFHI-IPC))))
	(LIM-RELPC (* 2 (LOGLDB %%Q-POINTER
				(CC-MEM-READ
				  (+ FEF %FEFHI-STORAGE-LENGTH))))))
    (DECLARE (FIXNUM FROM-RELPC TO-RELPC INIT-RELPC LIM-RELPC))
    (SETQ FROM-RELPC
	  (LOGAND -2 (COND ((NULL CENTER-PC) INIT-RELPC)
			   (T (MAX (- 10 CENTER-PC) INIT-RELPC)))))
    (SETQ TO-RELPC
	  (COND ((NULL CENTER-PC) LIM-RELPC)
		(T (MIN (+ 3 CENTER-PC) LIM-RELPC))))
    (COND ((NULL CENTER-PC) (SETQ CENTER-PC -1)))
    (DO ((I FROM-RELPC (+ 2 I)))
	((NOT (< I TO-RELPC)))
      (DECLARE (FIXNUM I))
      (LET ((WORD (QF-MEM-READ (+ (LSH I -1) FEF))))
	(DECLARE (FIXNUM WORD))
	(CC-CODE-INSN (LOGLDB 0020 WORD) I CENTER-PC)
	(CC-CODE-INSN (LOGLDB 2020 WORD) (1+ I) CENTER-PC)))))

(DEFUN CC-CODE-INSN (INST ADDR CENTER-PC)
    (PRINC (COND ((= CENTER-PC ADDR) '|=> |)
		 (T '|   |)))
    (PRINC ADDR) (PRINC '| |)
    (CC-TYPE-OUT INST CC-INST-DESC T NIL)
    (TERPRI)
    NIL)

(DEFUN (PF CC-COLON-CMD) (IGNORE)
  (COND ((NULL CC-OPEN-REGISTER)
	 (PRINC '|NO REGISTER OPEN (SHOULD BE AN LP-FEF WORD OF A FRAME)|))
	(T 
    (LET ((CALL-WORD (CC-REGISTER-EXAMINE (- CC-OPEN-REGISTER 3)))
	  (EXIT-WORD (CC-REGISTER-EXAMINE (- CC-OPEN-REGISTER 2)))
	  (ENTRY-WORD (CC-REGISTER-EXAMINE (- CC-OPEN-REGISTER 1)))
	  (FUNCTION-WORD (CC-REGISTER-EXAMINE CC-OPEN-REGISTER)))
       (TERPRI) (CC-TYPE-OUT FUNCTION-WORD 'CC-SEXP-DESC T NIL)
       (TERPRI) (CC-TYPE-OUT CALL-WORD 'CALL-WORD-DESC 'ALL NIL)
       (TERPRI) (CC-TYPE-OUT EXIT-WORD 'EXIT-WORD-DESC 'ALL NIL)
       (TERPRI) (CC-TYPE-OUT ENTRY-WORD 'ENTRY-WORD-DESC 'ALL NIL)
     (COND ((NOT (ZEROP (LOGLDB %%LP-CLS-ADI-PRESENT CALL-WORD)))
	    (DO ((ADR (- CC-OPEN-REGISTER 4) (- ADR 2))
		 (W1)(W2))
		(NIL)
	      (DECLARE (FIXNUM ADR W1 W2))
	      (SETQ W1 (CC-REGISTER-EXAMINE ADR)
		    W2 (CC-REGISTER-EXAMINE (- ADR 1)))
	      (TERPRI)
	      (CC-TYPE-OUT W1 'ADI-W1-DESC 'ALL NIL)
	      (TERPRI)
	      (CC-TYPE-OUT W2 'ADI-W2-DESC 'ALL NIL)
	      (AND (ZEROP (LOGLDB %%Q-FLAG-BIT W2)) (RETURN NIL))))))))
  (TERPRI))

(SETQ CALL-WORD-DESC '(
    (TYPE CALL-WORD)
    (SELECT-FIELD DOWNWARD-CLOSURE-PUSHED 2501 (NIL DOWNWARD-CLOSURE-PUSHED))
    (SELECT-FIELD ADI-PRESENT 2401 (NIL ADI-PRESENT))
    (SELECT-FIELD S-DEST 2004 (D-INDS D-PDL D-NEXT D-LAST D-RETURN T T D-NEXT-LIST D-MICRO))
    (TYPE-FIELD DELTA-TO-OPEN-BLOCK 1010 NIL)
    (TYPE-FIELD DELTA-TO-ACTIVE-BLOCK 0010 NIL)))

(SETQ EXIT-WORD-DESC '(
    (TYPE EXIT-WORD)
    (SELECT-FIELD MICRO-STACK-SAVED 2101 (NIL MICRO-STACK-SAVED))
    (SELECT-FIELD BINDING-BLOCK-PUSHED 2001 (NIL BINDING-BLOCK-PUSHED))
    (TYPE-FIELD SAVED-PC 0017 NIL)))

(SETQ ENTRY-WORD-DESC '(
    (TYPE ENTRY-WORD)
    (TYPE-FIELD NUM-ARGS 1006 NIL)
    (TYPE-FIELD LOC-BLOCK-ORIGIN 0010 NIL)))

(SETQ ADI-W1-DESC '(
    (TYPE ADI-W1)
    (SELECT-FIELD FLAG-BIT 3501 (NO-FLAG-BIT-ERROR NIL))
    (SELECT-FIELD ADI-TYPE 2403 (ERR RETURN-INFO RESTART-PC FEXPR-CALL LEXPR-CALL 
				   BIND-STACK-LEVEL T USED-UP-RETURN-INFO))
    (SELECT-FIELD STORING-OPT 2103 (ERR BLOCK LIST MAKE-LIST INDIRECT T T T))
    (TYPE-FIELD NUM-VALS-EXPECTING 0006 NIL)))

(SETQ ADI-W2-DESC '(
    (TYPE ADI-W2)
    (TYPE-FIELD FLAG-BIT 3501 NIL)
    (TYPE-FIELD W2 0030 NIL)))

;Search physical memory (ie currently swapped in stuff) for arg.
(DEFUN (PHYS-MEM-WORD-SEARCH CC-COLON-CMD) (QUAN)
  (DECLARE (FIXNUM ADR TEM))
  (DO ((ADR 0 (1+ ADR))
       (TEM))
      ((OR (= ADR 400000) #Q (KBD-TYI-NO-HANG) #M NIL))   ;SEARCHES 128K  **CROCK**
      (COND ((= QUAN (SETQ TEM (PHYS-MEM-READ ADR)))
	     (FORMAT T '|~%~S/	~S  | ADR TEM)))))

(DECLARE (SPECIAL CONS-FUNC-DEST-INT-CNTRL)) ;CADMAC does not get included in compilation

;:INTOFF disables hardware interrupts and sequence breaks
(DEFUN (INTOFF CC-COLON-CMD) (IGNORE)
  (CC-WRITE-FUNC-DEST CONS-FUNC-DEST-INT-CNTRL 0))

(DEFUN (DESCRIBE CC-COLON-CMD) (IGNORE)
  (AND CC-LAST-VALUE-TYPED 
       (LET ((DT (QF-DATA-TYPE CC-LAST-VALUE-TYPED)))
	 (COND ((= DT DTP-STACK-GROUP)
		(CC-DESCRIBE-STACK-GROUP CC-LAST-VALUE-TYPED))
	       ((OR (= DT DTP-CLOSURE)
		    (= DT DTP-ENTITY))
		(CC-DESCRIBE-CLOSURE CC-LAST-VALUE-TYPED))
	       ((= DT DTP-FEF-POINTER)
		(CC-DESCRIBE-FEF CC-LAST-VALUE-TYPED))))))

(IF-FOR-MACLISP
(DECLARE (SPECIAL SG-NAME SG-STATE %%SG-ST-IN-SWAPPED-STATE %%SG-ST-FOOTHOLD-EXECUTING
		  %%SG-ST-PROCESSING-ERROR %%SG-ST-CURRENT-STATE SG-STATES SG-SAVED-M-FLAGS
		  SG-REGULAR-PDL-POINTER SG-SPECIAL-PDL SG-SPECIAL-PDL-POINTER)))

(DEFUN CC-DESCRIBE-STACK-GROUP (SG)
  (PROG (PNTR)
	(SETQ PNTR (QF-POINTER SG))
	(FORMAT T "~%Stack group: " )
	(CC-Q-PRINT-TOPLEV (CC-MEM-READ (- PNTR 2 SG-NAME)))
	(LET ((STATE (CC-MEM-READ (- PNTR 2 SG-STATE))))
	   (COND ((NOT (ZEROP (LOGLDB %%SG-ST-IN-SWAPPED-STATE STATE)))
		  (FORMAT T "~% Variables currently swapped out")))
	   (COND ((NOT (ZEROP (LOGLDB %%SG-ST-FOOTHOLD-EXECUTING STATE)))
		  (FORMAT T "~% Foothold currently executing")))
	   (COND ((NOT (ZEROP (LOGLDB %%SG-ST-PROCESSING-ERROR STATE)))
		  (FORMAT T "~% Currently processing an error")))
	   (FORMAT T ", State ~S" (NTH (LOGLDB %%SG-ST-CURRENT-STATE STATE) SG-STATES)))
	(DO ((L STACK-GROUP-HEAD-LEADER-QS (CDR L))
	     (A (- PNTR 2) (1- A))
	     (WD))
	    ((NULL L))
	  (FORMAT T "~%~O~10T~A:~30T" A (CAR L))
	  (SETQ WD (CC-MEM-READ A))
	  (CC-TYPE-OUT WD CC-Q-DESC NIL NIL)
	  (TYO #\TAB)
	  (ERRSET (CC-Q-PRINT-TOPLEV WD)))
	(TERPRI)))

(DEFUN CC-DESCRIBE-CLOSURE (CLOS)
    (FORMAT T "~%CLOSED-FUNCTION ")
    (CC-Q-PRINT-TOPLEV (QF-CAR CLOS))
    (DO ((L (QF-CDR CLOS) (QF-CDR (QF-CDR L))))
	((CC-Q-NULL L))
      (FORMAT T "~%SYM: ")
      (CC-Q-PRINT-TOPLEV (1- (QF-SMASH-DATA-TYPE (QF-CAR L) DTP-SYMBOL)))
      (FORMAT T " VALUE:")
      (CC-Q-PRINT-TOPLEV (QF-CAR (QF-CAR (QF-CDR L))))))

(DEFUN CC-P-LDB-OFFSET (PPSS PNTR OFF)
  (LOGLDB PPSS (CC-MEM-READ (+ PNTR OFF))))

(DEFUN CC-P-CONTENTS-OFFSET (PNTR OFF)
       (LOGLDB %%Q-TYPED-POINTER (CC-MEM-READ (+ PNTR OFF))))

(DEFUN CC-DESCRIBE-FEF (FEF &AUX HEADER NAME FAST-ARG SV MISC LENGTH DBI)
  (SETQ HEADER (CC-P-LDB-OFFSET %%HEADER-REST-FIELD FEF %FEFHI-IPC))
  (SETQ LENGTH (CC-P-CONTENTS-OFFSET FEF %FEFHI-STORAGE-LENGTH))
  (SETQ NAME (CC-P-CONTENTS-OFFSET FEF %FEFHI-FCTN-NAME))
  (SETQ FAST-ARG (CC-P-CONTENTS-OFFSET FEF %FEFHI-FAST-ARG-OPT))
  (SETQ SV (CC-P-CONTENTS-OFFSET FEF %FEFHI-SV-BITMAP))
  (SETQ MISC (CC-P-CONTENTS-OFFSET FEF %FEFHI-MISC))
  (FORMAT T "~%FEF for function ") (CC-Q-PRINT-TOPLEV NAME) (TERPRI)
  (FORMAT T "Initial relative PC: ~S halfwords.~%" (LOGLDB %%FEFH-PC HEADER))
; -- Print out the fast arg option
  (FORMAT T "The Fast Argument Option is ~A"
	  (IF (ZEROP (LOGLDB %%FEFH-FAST-ARG HEADER))
	      "not active, but here it is anyway:"
	      "active:"))
  (SI:DESCRIBE-NUMERIC-DESCRIPTOR-WORD FAST-ARG)
; -- Randomness.
  (FORMAT T "~%The length of the local block is ~S~%"
	  (LOGLDB %%FEFHI-MS-LOCAL-BLOCK-LENGTH MISC))
  (FORMAT T "The total storage length of the FEF is ~S~%"
	  LENGTH)
; -- Special variables
  (COND ((ZEROP (LOGLDB %%FEFH-SV-BIND HEADER))
	 (PRINC "There are no special variables present."))
	(T (PRINC "There are special variables, ")
	   (TERPRI)
	   (COND ((ZEROP (LOGLDB %%FEFHI-SVM-ACTIVE SV))
		  (PRINC "but the S-V bit map is not active. "))
		 (T (FORMAT T "and the S-V bit map is active and contains: ~O"
			    (LOGLDB %%FEFHI-SVM-BITS SV))))))
  (TERPRI)
; -- ADL.
  (COND ((ZEROP (LOGLDB %%FEFH-NO-ADL HEADER))
	 (FORMAT T "There is an ADL:  It is ~S long, and starts at ~S"
		 (LOGLDB %%FEFHI-MS-BIND-DESC-LENGTH MISC)
			   (LDB %%FEFHI-MS-ARG-DESC-ORG MISC))
	 (CC-DESCRIBE-ADL (CC-GET-MACRO-ARG-DESC-POINTER FEF))
	 )
	(T (PRINC "There is no ADL.")))
  (TERPRI)
  DBI
; (COND ((SETQ DBI (FUNCTION-DEBUGGING-INFO FEF))
;	 (FORMAT T "Debugging info:~%")
;	 (DOLIST (ITEM DBI)
;		 (FORMAT T "  ~S~%" ITEM))))
  )
   
(DEFUN CC-GET-MACRO-ARG-DESC-POINTER (FEF-POINTER &AUX ORIGIN)
   (COND ((= 0 (SETQ ORIGIN
		     (CC-P-LDB-OFFSET %%FEFHI-MS-ARG-DESC-ORG FEF-POINTER %FEFHI-MISC)))
	  (CC-MAKE-POINTER DTP-SYMBOL 0))
	 (T (CC-MAKE-POINTER-OFFSET DTP-LIST FEF-POINTER ORIGIN))))

(DEFUN CC-MAKE-POINTER (DT PNTR)
       (LOGDPB DT %%Q-DATA-TYPE PNTR))

(DEFUN CC-MAKE-POINTER-OFFSET (DT PNTR OFF)
       (LOGDPB DT %%Q-DATA-TYPE (+ PNTR OFF)))

(DEFUN CC-DESCRIBE-ADL (ADL)
  (PROG (OPT-Q INIT-OPTION)
    L	(COND ((CC-Q-NULL ADL) (RETURN NIL)))
    	(SETQ OPT-Q (QF-CAR ADL) ADL (QF-CDR ADL))
	(TERPRI)
	(COND ((NOT (ZEROP (LOGAND OPT-Q %FEF-NAME-PRESENT)))
	       (PRINC "NAME ")
	       (CC-Q-PRINT-TOPLEV (QF-CAR ADL))
	       (SETQ ADL (QF-CDR ADL))))
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
	       (CC-Q-PRINT-TOPLEV (QF-CAR ADL))
	       (SETQ ADL (QF-CDR ADL))))
	(GO L)
))


(DEFUN (FLAGS CC-COLON-CMD) (QUAN)
  (CC-TYPE-OUT (OR QUAN (CC-SYMBOLIC-EXAMINE-REGISTER 'M-FLAGS)) 'M-FLAGS-DESC 'ALL NIL))

(SETQ M-FLAGS-DESC '(
    (TYPE M-FLAGS)
    (TYPE-FIELD M-QBFFL 0001 NIL)
    (SELECT-FIELD CAR-SYMBOL-MODE 0102 (ERROR NIL->NIL NIL ERROR))
    (SELECT-FIELD CAR-NUMBER-MODE 0302 (ERROR NIL ERROR ERROR))
    (SELECT-FIELD CDR-SYMBOL-MODE 0502 (ERROR NIL->NIL NIL PLIST))
    (SELECT-FIELD CDR-NUMBER-MODE 0702 (ERROR NIL ERROR ERROR))
    (SELECT-FIELD DONT-SWAP-IN 1101 (NIL DONT-SWAP-IN))
    (TYPE-FIELD TRAP-ENABLE 1201 NIL)
    (SELECT-FIELD MAR-MODE 1302 (NIL READ WRITE READ-AND-WRITE))
    (SELECT-FIELD PGF-WRITE 1501 (NIL PGF-WRITE))
    (SELECT-FIELD INTERRUPT 1601 (NIL INTERRUPT))
    (SELECT-FIELD SCAVENGE 1701 (NIL SCAVENGE))
    (SELECT-FIELD TRANSPORT 2001 (NIL TRANSPORT))
    (SELECT-FIELD STACK-GROUP-SWITCH 2101 (NIL STACK-GROUP-SWITCH))
    (SELECT-FIELD DEFERRED-SEQUENCE-BREAK 2201 (NIL DEFERRED-SEQUENCE-BREAK)) ))

