;;; This is file of definitions of the format of stack groups.   dlw 10/15/77 -*-LISP-*-
;;; It will be used by error handlers.
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;If this file is changed, it goes without saying that you need to make a new cold load.
;  Also LISPM; QCOM and LISPM2;GLOBAL must be changed to agree.
;  LISPM2;SYSTEM has a list of the symbols defined here which belong in the SYSTEM package
;The microcode must be reassembled, and at least the following must be recompiled:
;    LISPM2;EH >, LISPM2;EHR >, LISPM;SGFCTN >, LISPM; QMISC > (for DESCRIBE)
;    LISPM2;PROCES >, LMIO;KBD > (for PROCESS-WAIT), LISPM2;QTRACE (for FUNCTION-ACTIVE-P)
;
;also LMCONS;CC >

(DEFSTRUCT (STACK-GROUP :ARRAY-LEADER (:CONSTRUCTOR NIL))
      SG-NAME			    ;String with the name of the stack group
      SG-REGULAR-PDL		    ;Regular PDL array, 0=base of stack
      SG-REGULAR-PDL-LIMIT	    ;Max depth before trap
      SG-SPECIAL-PDL		    ;Special PDL array, 0=base of stack
      SG-SPECIAL-PDL-LIMIT	    ;Max depth before trap
      SG-INITIAL-FUNCTION-INDEX	    ;Index into PDL of initial M-AP 
				    ;   (3 unless initial call has adi)
      SG-UCODE			    ;   (unused)
      SG-TRAP-TAG		    ;Symbolic tag corresponding to SG-TRAP-MICRO-PC.
				    ; gotten via MICROCODE-ERROR-TABLE, etc.  Properties
				    ; off this symbol drive error recovery.
      SG-RECOVERY-HISTORY	    ;Available for hairy SG munging routines to attempt to
				    ; leave tracks.
      SG-FOOTHOLD-DATA		    ;During error recovery, contains pointer to a stack frame
				    ; which contains saved status of the main stack group.
				    ; (Preventing it from being lost when the foothold is 
				    ; running.)
      ((SG-STATE)
       (SG-CURRENT-STATE 0006)	    ;state of this stack group
       (SG-FOOTHOLD-EXECUTING-FLAG 0601)	;not used
       (SG-PROCESSING-ERROR-FLAG 0701)		;taking error trap (detect recursive errors)
       (SG-PROCESSING-INTERRUPT-FLAG 1001)	;not used
       (SG-SAFE 1101)
       (SG-INST-DISP 1202)	    ;the instruction dispatch we are using
       (SG-IN-SWAPPED-STATE 2601)   ;we are in the swapped state
       (SG-SWAP-SV-ON-CALL-OUT 2501)	    ;if this is on in the caller, or
       (SG-SWAP-SV-OF-SG-THAT-CALLS-ME 2401))	    ;   this in the callee, then swap
      SG-PREVIOUS-STACK-GROUP	    ;Stack group who just ran
      SG-CALLING-ARGS-POINTER	    ;Pointer into previous stack group's REGPDL to
      SG-CALLING-ARGS-NUMBER	    ;   the args passed to us.
      SG-TRAP-AP-LEVEL		    ;Locative to a location in PDL Buffer, below which 
				    ;traps occur
      SG-REGULAR-PDL-POINTER	    ;Saved pdl pointer (as index into regular-pdl array)
      SG-SPECIAL-PDL-POINTER	    ;Saved A-QLBNDP (as index into special-pdl array)
      SG-AP			    ;Saved M-AP
      SG-IPMARK			    ;Saved A-IPMARK
      SG-TRAP-MICRO-PC		    ;Address of last call to TRAP
;     SG-ERROR-HANDLING-SG	    ;Having these part of the SG would be nice, but
;     SG-INTERRUPT-HANDLING-SG      ; it doesnt buy anything for the time being, and costs
				    ; a couple microinstructions.
      SG-SAVED-QLARYH		    ;Saved A-QLARYH
      SG-SAVED-QLARYL		    ;Saved A-QLARYL
      ((SG-SAVED-M-FLAGS)	    ;Saved M-FLAGS
	;The below doesn't work due to a cretinous misfeature in DEFSTRUCT
;       (SG-FLAGS-QBBFL %%M-FLAGS-QBBFL)	    ; Binding-block-pushed flag
;       (SG-FLAGS-CAR-SYM-MODE %%M-FLAGS-CAR-SYM-MODE)  ;UPDATE PRINT-ERROR-MODE IN QMISC
;       (SG-FLAGS-CAR-NUM-MODE %%M-FLAGS-CAR-NUM-MODE)  ;  IF ADD ANY..
;       (SG-FLAGS-CDR-SYM-MODE %%M-FLAGS-CDR-SYM-MODE) 
;       (SG-FLAGS-CDR-NUM-MODE %%M-FLAGS-CDR-NUM-MODE) 
;       (SG-FLAGS-DONT-SWAP-IN %%M-FLAGS-DONT-SWAP-IN)
;       (SG-FLAGS-TRAP-ENABLE %%M-FLAGS-TRAP-ENABLE)
;       (SG-FLAGS-MAR-MODE %%M-FLAGS-MAR-MODE)
;       (SG-FLAGS-PGF-WRITE %%M-FLAGS-PGF-WRITE)
       (SG-FLAGS-QBBFL 0001)	    ; Binding-block-pushed flag
       (SG-FLAGS-CAR-SYM-MODE 0102)  ;UPDATE PRINT-ERROR-MODE IN QMISC
       (SG-FLAGS-CAR-NUM-MODE 0302)  ;  IF ADD ANY..
       (SG-FLAGS-CDR-SYM-MODE 0502) 
       (SG-FLAGS-CDR-NUM-MODE 0702) 
       (SG-FLAGS-DONT-SWAP-IN 1101)
       (SG-FLAGS-TRAP-ENABLE 1201)
       (SG-FLAGS-MAR-MODE 1302)
       (SG-FLAGS-PGF-WRITE 1501)
       )
      SG-AC-K
      SG-AC-S
      SG-AC-J 
      SG-AC-I
      SG-AC-Q
      SG-AC-R
      SG-AC-T
      SG-AC-E
      SG-AC-D
      SG-AC-C 
      SG-AC-B
      SG-AC-A
      SG-AC-ZR
      SG-AC-2			;Pointer field of M-2 as fixnum
      SG-AC-1			;Pointer field of M-1 as fixnum
      SG-VMA-M1-M2-TAGS		;Tag fields of VMA, M-1, M-2 packed into a fixnum
      SG-SAVED-VMA		;Pointer field of VMA as a locative
      SG-PDL-PHASE		;If you mung the sg's stack pointer, do same to this
				;This is the actual value of PDL-BUFFER-POINTER reg.
      )

(DEFSTRUCT (REGULAR-PDL :ARRAY-LEADER (:CONSTRUCTOR NIL))
      REGULAR-PDL-SG)

(DEFSTRUCT (SPECIAL-PDL :ARRAY-LEADER (:CONSTRUCTOR NIL))
      SPECIAL-PDL-SG)


;; Macros for accessing the Regular Pdl.

(DEFMACRO RP-CALL-WORD     (RP L) `(AR-1 ,RP (+ ,L %LP-CALL-STATE)))
(DEFMACRO RP-EXIT-WORD     (RP L) `(AR-1 ,RP (+ ,L %LP-EXIT-STATE)))
(DEFMACRO RP-ENTRY-WORD    (RP L) `(AR-1 ,RP (+ ,L %LP-ENTRY-STATE)))
(DEFMACRO RP-FUNCTION-WORD (RP L) `(AR-1 ,RP (+ ,L %LP-FEF)))


; (DEFINE-RP-MACROS ((RP-FOO %%FOO) (RP-BAR %%BAR)) RP-CALL-WORD)

; produces

; (PROGN 'COMPILE
;	 (MACRO RP-FOO (X)
;		`(LDB %%FOO (RP-CALL-WORD ,(CADR X) ,(CADDR X))))
;	 (MACRO RP-BAR (X)
;		`(LDB %%BAR (RP-CALL-WORD ,(CADR X) ,(CADDR X)))))

(DEFMACRO DEFINE-RP-MACROS (SPEC-LIST WORD-MACRO)
    (DO ((L SPEC-LIST (CDR L))
         (BYTE)
         (NAME)
         (ACCUM))
        ((NULL L) `(PROGN 'COMPILE ,@ACCUM))
      (SETQ NAME (CAAR L) BYTE (CADAR L))
      (PUSH `(MACRO ,NAME (X)
	       `(LDB ,',BYTE (,',WORD-MACRO ,(CADR X) ,(CADDR X))))
            ACCUM)))

(DEFINE-RP-MACROS ((RP-DOWNWARD-CLOSURE-PUSHED %%LP-CLS-DOWNWARD-CLOSURE-PUSHED)
		   (RP-ADI-PRESENT %%LP-CLS-ADI-PRESENT)
		   (RP-DESTINATION %%LP-CLS-DESTINATION)
		   (RP-DELTA-TO-OPEN-BLOCK %%LP-CLS-DELTA-TO-OPEN-BLOCK)
		   (RP-DELTA-TO-ACTIVE-BLOCK %%LP-CLS-DELTA-TO-ACTIVE-BLOCK))
		  RP-CALL-WORD)

(DEFINE-RP-MACROS ((RP-MICRO-STACK-SAVED %%LP-EXS-MICRO-STACK-SAVED)
		   (RP-PC-STATUS %%LP-EXS-PC-STATUS)
		   (RP-BINDING-BLOCK-PUSHED %%LP-EXS-BINDING-BLOCK-PUSHED)	;Same as above
		   (RP-EXIT-PC %%LP-EXS-EXIT-PC))
		  RP-EXIT-WORD)

(DEFINE-RP-MACROS ((RP-NUMBER-ARGS-SUPPLIED %%LP-ENS-NUM-ARGS-SUPPLIED) ;Only for macro frames
		   (RP-LOCAL-BLOCK-ORIGIN				; can this be extended?
		    %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN))	;Only for macro frames
		  RP-ENTRY-WORD)


;; Macros for accessing fields of the headers of Function Entry Frames.

(MACRO DEFINE-OFFSET-BYTE-MACROS (X)
   (DO ((LL (CDR X) (CDR LL))
        (ACCUM)
        (INDEX 0 (1+ INDEX)))
       ((NULL LL) `(PROGN 'COMPILE ,@ACCUM))
      (DO ((L (CAR LL) (CDR L))
           (NAME)
           (BYTE))
          ((NULL L))
         (SETQ NAME (CAAR L) BYTE (CADAR L))
         (PUSH `(MACRO ,NAME (X)
                 `(%P-LDB-OFFSET ,',BYTE ,(CADR X) ,',INDEX))
               ACCUM))))

(DEFINE-OFFSET-BYTE-MACROS
 ((FEF-INITIAL-PC %%FEFH-PC)
  (FEF-NO-ADL-P %%FEFH-NO-ADL)
  (FEF-FAST-ARGUMENT-OPTION-P %%FEFH-FAST-ARG)
  (FEF-SPECIALS-BOUND-P %%FEFH-SV-BIND))
 ((FEF-LENGTH %%Q-POINTER))
 ()
 ((FEF-FAST-ARGUMENT-OPTION-WORD %%Q-POINTER))
 ((FEF-BIT-MAP-P %%FEFHI-SVM-ACTIVE)
  (FEF-BIT-MAP %%FEFHI-SVM-BITS))
 ((FEF-NUMBER-OF-LOCALS %%FEFHI-MS-LOCAL-BLOCK-LENGTH)
  (FEF-ADL-ORIGIN %%FEFHI-ARG-DESC-ORG)
  (FEF-ADL-LENGTH %%FEFHI-BIND-DESC-LENGTH)))

(DEFMACRO FEF-NAME (FEF) `(%P-CONTENTS-OFFSET ,FEF %FEFHI-FCTN-NAME))

;; Randomness.
;   %%US-RPC						;RETURN PC
;   %%US-PPBMIA						;ADI ON MICRO-TO-MICRO-CALL
;   %%US-PPBMAA						;ADI ON MACRO-TO-MICRO-CALL
;   %%US-PPBSPC						;BINDING BLOCK PUSHED

;%%ADI-TYPE						;ADI-KINDS
;    ADI-RETURN-INFO
;       %%ADI-RET-STORING-OPTION				;ADI-STORING-OPTIONS
;          ADI-ST-BLOCK ADI-ST-LIST 
;	  ADI-ST-MAKE-LIST
;	  ADI-ST-INDIRECT
;       %%ADI-RET-SWAP-SV
;       %%ADI-RET-NUM-VALS-EXPECTING 
;    ADI-RESTART-PC
;       %%ADI-RPC-MICRO-STACK-LEVEL
;    ADI-FEXPR-CALL 
;    ADI-LEXPR-CALL
;    ADI-BIND-STACK-LEVEL
;    ADI-USED-UP-RETURN-INFO


