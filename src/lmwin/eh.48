;;; New error handler.       DLW 1/5/78  -*-Mode:LISP; Package:EH-*-

(DEFVAR ERROR-MESSAGE-PRINLEVEL 2)	;These are used when printing error messages
(DEFVAR ERROR-MESSAGE-PRINLENGTH 4)	; and values of variables in frames.

;; The error table, read from LISPM1;UCADR nnnTBL into MICROCODE-ERROR-TABLE,
;; describes the symbolic meaning of certain microcode pcs.
;; Its data is rearranged into other variables below.
;; ERROR-TABLE relates the micro pc to a symbolic name of error,
;; called an ETE, which will then have properties saying how to handle
;; the error.  The properties are defined in LISPM2;EHR.
;; ETE stands for error table entry, although that is only accurate for
;; microcode errors.  It is a list whose car is a symbol which has
;; some interesting properties for recovering from and reporting the error.
;; They are set up in the file LISPM2;EHR > (Error Handler Routines...)

;; Actual error table read in from LISPM1;LCADR nnnTBL.
(DEFVAR MICROCODE-ERROR-TABLE)
;; Ucode version number to which the loaded value of MICROCODE-ERROR-TABLE pertains.
(DEFVAR MICROCODE-ERROR-TABLE-VERSION-NUMBER 0)

;; ASSURE-TABLE-PROCESSED looks at MICROCODE-ERROR-TABLE
;; and produces these lists.
(DEFVAR CALLS-SUB-LIST)			;Alist of micropcs to symbols.
(DEFVAR RESTART-LIST)			;Alist of symbols to micropcs.
(DEFVAR ERROR-TABLE)			;List of ETEs.
(DEFVAR ERROR-TABLE-NUMBER -1)		;Microcode version number for ERROR-TABLE.

;; An error immediately runs the first level error handler stack group
;; whose job is to initialize a second level error handler stack group
;; in which the error handler actually runs.

;; SECOND-LEVEL-COUNT is a count for giving each second level error handler a distinct name.
(DEFVAR SECOND-LEVEL-ERROR-HANDLER-COUNT 0)
;; Error handler stack groups that were exited and can be reused.
(DEFVAR FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST NIL)
;; Last second level error handler to be running.
;; This is so that each of them can tell
;; when it is returned to whether some other one
;; has been running in the meanwhile.
(DEFVAR LAST-SECOND-LEVEL-ERROR-HANDLER-SG NIL)
;; This variable is bound to T in every second-level error handler to identify them.
(DEFVAR ERROR-HANDLER-RUNNING T)
;; Controls whether the error message is reprinted in RUN-SG
(DEFVAR ERROR-HANDLER-REPRINT-ERROR T)

(DEFVAR ERROR-HANDLER-IO NIL)	;If non-NIL, stream EH should use

;; Conditions.  Condition handlers are run by the second level error handler.
;; These variables are part of the mechanism by which condition handlers
;; ask to proceed from the error.
(DEFVAR CONDITION-PROCEED-FLAG)		;Communicate from condition handlers to PROCEED.
(DEFVAR CONDITION-PROCEED-VALUE)	;See READ-OBJECT.

;; ERRSET-STATUS is T within an errset.
;; ERRSET-PRINT-MSG is T if the error message should be printed anyway.
;; ERRSET is T if the error handler should be entered despite being in an errset.
(DEFVAR ERRSET-STATUS NIL)
(DEFVAR ERRSET-PRINT-MSG NIL)
(DEFVAR ERRSET NIL)
(REMPROP 'ERRSET ':SOURCE-FILE-NAME)  ;Avoid error message when macro defined

;; This is funcalled after printing the error message.
(DEFVAR ERROR-MESSAGE-HOOK NIL)

;; Here are the error handler's main operating parameters.
(DEFVAR ERROR-SG)		;The stack group that got the error.
(DEFVAR CURRENT-FRAME)		;The SG-AP of the frame that the error handler is looking at.
(DEFVAR ORIGINAL-FRAME)		;The SG-AP of the frame that got the error.

;; This is a random gensymmed object which is returned
;; from SG-EVAL to indicate that an error occurred within.
(DEFVAR ERROR-FLAG (NCONS NIL))

;; Number of levels of backtrace to print automatically upon error.
(DEFVAR ERROR-MESSAGE-BACKTRACE-LENGTH 3)

;; Number of instructions to disassemble for M-L, etc., if we
;; can't determine the amount of room on the screen.
(DEFVAR DISASSEMBLE-INSTRUCTION-COUNT 20)

;; Calls to these functions should not be mentioned as frames
;; when stack-censoring is going on in interpreted functions.
;; This should include all functions that have &QUOTE args and are open-compiled.
;; *EVAL and APPLY-LAMBDA are there for peculiar reasons.
(DEFVAR UNINTERESTING-FUNCTIONS '(SI:*EVAL SI:APPLY-LAMBDA COND SETQ PROG GO DO DO-NAMED
				  MULTIPLE-VALUE MULTIPLE-VALUE-LIST
				  MULTIPLE-VALUE-RETURN AND OR STORE))

;;; These datatypes are OK to call print on
(DEFVAR GOOD-DATA-TYPES '(DTP-SYMBOL DTP-FIX DTP-EXTENDED-NUMBER DTP-SMALL-FLONUM 
			  DTP-LIST DTP-U-ENTRY DTP-FEF-POINTER DTP-ARRAY-POINTER
			  DTP-STACK-GROUP DTP-CLOSURE DTP-ENTITY DTP-INSTANCE))
;;; These point to something (as opposed to being Inums)
(DEFVAR POINTER-TYPES '(DTP-NULL DTP-SYMBOL DTP-SYMBOL-HEADER DTP-EXTENDED-NUMBER
			DTP-GC-FORWARD DTP-EXTERNAL-VALUE-CELL-POINTER DTP-ONE-Q-FORWARD
			DTP-HEADER-FORWARD DTP-LOCATIVE DTP-LIST
			DTP-FEF-POINTER DTP-ARRAY-POINTER DTP-STACK-GROUP
			DTP-CLOSURE DTP-SELECT-METHOD DTP-INSTANCE DTP-INSTANCE-HEADER
			DTP-ENTITY))

;;; Table of stack groups being stepped and stack groups stepping them
(DEFVAR SG-STEPPING-TABLE NIL)

;; This is a temporary kludge, which will be fixed by modifying the installed
;; LISP-REINITIALIZE when this thing gets installed.

(SETQ %INITIALLY-DISABLE-TRAPPING NIL)

;; Save a stack group's state on its stack so we can use it and then restore the state.
;; The information goes on the pdl in a fake frame belonging to the function FOOTHOLD.
;; Each Q is saved as 2 words (pointer and tag) to avoid data type problems.
;; You must call this before pushing a call block, even if calling SG-RUN-GOODBYE,
;; in order to clean up the QBBFL and the U-STACK Q's.
(DEFUN SG-SAVE-STATE (SG &AUX P NEW-AP RP PP)
  (SG-MAYBE-GROW-PDLS SG)		;Make sure there is room to do this
  (SETQ RP (SG-REGULAR-PDL SG)
	PP (SG-REGULAR-PDL-POINTER SG))
  (SETQ NEW-AP (+ PP %LP-CALL-BLOCK-LENGTH))
  (ASET (DPB (- NEW-AP (SG-IPMARK SG)) %%LP-CLS-DELTA-TO-OPEN-BLOCK
	     (DPB (- NEW-AP (SG-AP SG)) %%LP-CLS-DELTA-TO-ACTIVE-BLOCK
		  0))
	RP (+ NEW-AP %LP-CALL-STATE))
  (ASET 0 RP (+ NEW-AP %LP-EXIT-STATE))
  (ASET 0 RP (+ NEW-AP %LP-ENTRY-STATE))
  (ASET #'FOOTHOLD RP (+ NEW-AP %LP-FEF))
  (SETQ PP (1+ NEW-AP))
  (DO I 0 (1+ I) (> I SG-PDL-PHASE)
      (SETQ P (AP-LEADER SG I))
      (ASET (IF (MEMQ (Q-DATA-TYPES (%P-DATA-TYPE P)) POINTER-TYPES)
		(%P-CONTENTS-AS-LOCATIVE P)
		(%P-POINTER P))
	    RP PP)
      (ASET (%P-LDB %%Q-ALL-BUT-POINTER P)
	    RP (1+ PP))
      (SETQ PP (+ PP 2)))
  (SETF (SG-REGULAR-PDL-POINTER SG) (1- PP))	;Index of last valid word
  (SETF (SG-FLAGS-QBBFL SG) 0)			;Clear QBBFL left over from previous frame
  (SETF (SG-IPMARK SG) NEW-AP)
  (SETF (SG-AP SG) NEW-AP))

;;; This function isn't called, it just exists to name state-save frames
(DEFUN FOOTHOLD () NIL)

;; Pop the saved state from the pdl into the current state.
(DEFUN SG-RESTORE-STATE (SG)
  (LET ((PP (SG-PREVIOUS-ACTIVE SG (SG-PREVIOUS-ACTIVE SG (SG-AP SG))))
	(RP (SG-REGULAR-PDL SG)))
    (AND (NULL PP)
	 (FERROR NIL "~S state not saved" SG))
    (OR (EQ (AREF RP PP) #'FOOTHOLD)
	(FERROR NIL "Saved state for ~S at ~S[~S] clobbered." SG RP PP))
    (SETQ PP (1+ PP))
    (DO I 0 (1+ I) (> I SG-PDL-PHASE)
      (%P-STORE-TAG-AND-POINTER (AP-LEADER SG I)
				(AREF RP (1+ PP))
				(AREF RP PP))
      (SETQ PP (+ PP 2)))))

;;; Low level routines for manipulating the stacks of a stack group.
;;; Call SG-SAVE-STATE before calling any of these.

(DEFUN SG-REGPDL-PUSH (X SG &AUX PP)
  (SETQ PP (1+ (SG-REGULAR-PDL-POINTER SG)))
  (ASET X (SG-REGULAR-PDL SG) PP)
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG) PP) CDR-NEXT)
  (SETF (SG-REGULAR-PDL-POINTER SG) PP)
  (SETF (SG-PDL-PHASE SG) (1+ (SG-PDL-PHASE SG)))
  X)

(DEFUN SG-REGPDL-POP (SG &AUX PP)
  (SETF (SG-PDL-PHASE SG) (1- (SG-PDL-PHASE SG)))
  (SETQ PP (SG-REGULAR-PDL-POINTER SG))
  (SETF (SG-REGULAR-PDL-POINTER SG) (1- PP))
  (AREF (SG-REGULAR-PDL SG) PP))

(DEFUN SG-SPECPDL-PUSH (X SG FLAG &AUX PP PDL)
  (SETQ PP (1+ (SG-SPECIAL-PDL-POINTER SG)))
  (SETF (SG-SPECIAL-PDL-POINTER SG) PP)
  (SETQ PDL (SG-SPECIAL-PDL SG))
  (ASET X PDL PP)
  (%P-STORE-FLAG-BIT (ALOC PDL PP) FLAG)
  X)

(DEFUN SG-SPECPDL-POP (SG &AUX PP)
  (SETQ PP (SG-SPECIAL-PDL-POINTER SG))
  (SETF (SG-SPECIAL-PDL-POINTER SG) (1- PP))
  (AREF (SG-SPECIAL-PDL SG) PP))

;; This simulates the CBM (or P3ZERO) routine in the microcode.
;; It is what a CALL instruction does.
;; You must call SG-SAVE-STATE before calling this.
(DEFUN SG-OPEN-CALL-BLOCK (SG DESTINATION FUNCTION &AUX PP NEW-IPMARK)
  (SETQ PP (SG-REGULAR-PDL-POINTER SG))
  (SETQ NEW-IPMARK (+ PP %LP-CALL-BLOCK-LENGTH))
  (SG-REGPDL-PUSH (DPB (- NEW-IPMARK (SG-IPMARK SG)) %%LP-CLS-DELTA-TO-OPEN-BLOCK
		       (DPB (- NEW-IPMARK (SG-AP SG)) %%LP-CLS-DELTA-TO-ACTIVE-BLOCK
			    (DPB DESTINATION %%LP-CLS-DESTINATION 0)))
		  SG)
  (SG-REGPDL-PUSH 0 SG)
  (SG-REGPDL-PUSH 0 SG)
  (SG-REGPDL-PUSH FUNCTION SG)
  (SETF (SG-IPMARK SG) NEW-IPMARK))

;; Running things in the other stack group.

;; Call a function in another stack group and return the value it "returns".
;; Actually, the function should call this stack group back with the "value" as argument.
;; If the value is the symbol LOSE, we throw to QUIT.
;; The call block and args should already be on the regpdl of the other stack group,
;; hence SG-SAVE-STATE should be outside this function.
;; Nothing will automatically make the function know who you are;
;; provide your own stack group as an argument to it if necessary.
;; Before returning, SG-RESTORE-STATE is done since it's generally desired.
(DEFUN RUN-SG (SG &AUX RESULT)
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG)	;Terminate arg list assumed there
			   (SG-REGULAR-PDL-POINTER SG))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
  (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP)
  (SETF (SG-FLAGS-MAR-MODE SG) 0)			;Turn off the MAR (why??)
  (FUNCALL SG)
  (SETQ RESULT (CAR %CURRENT-STACK-GROUP-CALLING-ARGS-POINTER))
  (SG-RESTORE-STATE SG)
  (COND ((AND ERROR-HANDLER-RUNNING ERROR-HANDLER-REPRINT-ERROR)
	 (COND ((NEQ %CURRENT-STACK-GROUP LAST-SECOND-LEVEL-ERROR-HANDLER-SG)
	        (TERPRI)
	        (PRINT-ERROR-MESSAGE SG (SG-TRAP-TAG SG) T)))
	 (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP)))
  (COND ((EQ RESULT 'LOSE)
	 (*THROW 'QUIT NIL)))
  RESULT)

;; Restart a stack group and mark the error handler stack group as free.
;; This is used for throwing, etc.
(DEFUN SG-RUN-GOODBYE (SG)
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG)	;Terminate arg list
			   (SG-REGULAR-PDL-POINTER SG))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
  (WITHOUT-INTERRUPTS
    (AND ERROR-HANDLER-RUNNING (FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP))
    (FUNCALL SG)))

;;Smash a SG so that it reinvokes its current function with the same args when resumed.
;; For the time being, the function had better not have pushed a micro-stack.
;; Any binding block pushed will not get unwound immediately, but will be
;;  "concatenated."  For the moment, tho, PROCESS-WAIT is the only fctn this should
;; be used on, and these screws shouldnt affect it.
(DEFUN SG-REINVOKE (SG &AUX RP AP NEW-AP NARGS)
  (PROG ()
      (SETQ RP (SG-REGULAR-PDL SG)
	    AP (SG-AP SG)
	    NEW-AP (SG-PREVIOUS-ACTIVE SG AP)
	    NARGS (LDB %%LP-ENS-NUM-ARGS-SUPPLIED (AREF RP (+ AP %LP-ENTRY-STATE))))
    L (COND ((> (SG-REGULAR-PDL-POINTER SG) (+ AP NARGS))  ;Pop any extra stuff beyond args.
	     (SG-REGPDL-POP SG)
	     (GO L)))
      (SETF (SG-IPMARK SG) AP)
      (SETF (SG-AP SG) NEW-AP)
      (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)))
  
;; Mark a second level error handler stack group as available for re-use.
(DEFUN FREE-SECOND-LEVEL-ERROR-HANDLER-SG (SG)
  (WITHOUT-INTERRUPTS
    (PUSH SG FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
    (AND CURRENT-PROCESS (FUNCALL CURRENT-PROCESS ':REVOKE-RUN-REASON SG))))

;; Unwind the stack group until the M-AP is DEST-AP.
;; If GOODBYE-P is T, it returns the specified value from that frame,
;; otherwise it comes back to the EH.
(DEFUN SG-UNWIND-TO-FRAME (SG DEST-AP GOODBYE-P &OPTIONAL VALUE (LABEL T) &AUX N)
  (SETQ N (DO ((AP (SG-AP SG) (SG-PREVIOUS-ACTIVE SG AP))
	       (N 1 (1+ N)))
	      ((= AP DEST-AP) N)))
  (SG-UNWIND SG LABEL VALUE N (IF GOODBYE-P NIL %CURRENT-STACK-GROUP)
	     (IF GOODBYE-P 'FREE 'CALL))
  (COND ((NULL GOODBYE-P)		;Flush the call back to this SG, and try to get
	 (SETF (SG-AP SG)			; things in phase again.
	       (SETQ CURRENT-FRAME
		     (SETQ ORIGINAL-FRAME (SG-PREVIOUS-ACTIVE SG (SG-AP SG)))))
	 (DOTIMES (I 4)
	   (SG-REGPDL-POP SG)))))

;; The CONTINUATION is a function called with one argument in the newly-reset
;; stack-group.  ARGUMENT is that argument.
;; If PROCESS-P, rather than doing it now, in this process, we simply
;; leave the stack-group in such a state that the next time it is called,
;; e.g. by the scheduler, it will do it.
(DEFUN UNWIND-SG (SG CONTINUATION ARGUMENT PROCESS-P)
  (SETF (SG-INST-DISP SG) 0)  ;SG-MAIN-DISPATCH
  (LET ((ST (SG-CURRENT-STATE SG)))
    (COND ((NOT (OR (= ST SG-STATE-AWAITING-INITIAL-CALL)
		    (= ST 0)))
	   (SG-UNWIND SG T ARGUMENT NIL CONTINUATION (IF PROCESS-P 'SETUP 'CALL)))
	  (T	;SG has not been run, don't unwind, but do leave in same state
	   (STACK-GROUP-PRESET SG CONTINUATION ARGUMENT)
	   (OR PROCESS-P (FUNCALL SG))))
    (OR PROCESS-P (SETF (SG-CURRENT-STATE SG) SG-STATE-EXHAUSTED))))

;; Eval a form in a specified stack group using a foothold.
(DEFUN SG-EVAL (SG FORM &OPTIONAL REBIND-STREAMS &AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
  (SG-SAVE-STATE SG)
  (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
  (SG-OPEN-CALL-BLOCK SG 0 (IF REBIND-STREAMS 'FH-STREAM-BINDING-EVALER 'FH-EVALER))
  (SG-REGPDL-PUSH FORM SG)
  (SG-REGPDL-PUSH + SG)
  (SG-REGPDL-PUSH * SG)
  (SG-REGPDL-PUSH %CURRENT-STACK-GROUP SG)
  (SG-REGPDL-PUSH ERROR-HANDLER-RUNNING SG)
  (SG-REGPDL-PUSH PREV-FH SG)
  (AND REBIND-STREAMS (SG-REGPDL-PUSH TERMINAL-IO SG))
  (RUN-SG SG))

(DEFUN SG-FUNCALL (SG FUNCTION &REST ARGUMENTS)
  (SG-APPLY SG FUNCTION ARGUMENTS))

(DEFUN SG-APPLY (SG FUNCTION ARGUMENTS &AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
  (SG-SAVE-STATE SG)
  (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
  (SG-OPEN-CALL-BLOCK SG 0 'FH-APPLIER)
  (SG-REGPDL-PUSH FUNCTION SG)
  (SG-REGPDL-PUSH ARGUMENTS SG)
  (SG-REGPDL-PUSH + SG)
  (SG-REGPDL-PUSH * SG)
  (SG-REGPDL-PUSH %CURRENT-STACK-GROUP SG)
  (SG-REGPDL-PUSH ERROR-HANDLER-RUNNING SG)
  (SG-REGPDL-PUSH PREV-FH SG)
  (RUN-SG SG))

(DEFUN SG-THROW (SG LABEL VALUE &OPTIONAL IGNORE)
  (SG-SAVE-STATE SG)
  (SG-OPEN-CALL-BLOCK SG 0 'FH-THROWER)
  (SG-REGPDL-PUSH LABEL SG)
  (SG-REGPDL-PUSH VALUE SG)
  (SG-RUN-GOODBYE SG))

(DEFUN SG-UNWIND (SG LABEL VALUE COUNT ACTION DISPOSAL)
  "DISPOSAL is SETUP just to set up the call, CALL to make the call and not free the EH,
   FREE to make the call and free the EH"
  (SG-SAVE-STATE SG)
  (AND COUNT (SETQ COUNT (1+ COUNT)))  ;Make up for the frame pushed by SG-SAVE-STATE.
  (SG-OPEN-CALL-BLOCK SG 0 'FH-UNWINDER)
  (SG-REGPDL-PUSH LABEL SG)
  (SG-REGPDL-PUSH VALUE SG)
  (SG-REGPDL-PUSH COUNT SG)
  (SG-REGPDL-PUSH ACTION SG)
  (%P-STORE-CDR-CODE (ALOC (SG-REGULAR-PDL SG)	;Terminate arg list
			   (SG-REGULAR-PDL-POINTER SG))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
  (WITHOUT-INTERRUPTS
    (AND ERROR-HANDLER-RUNNING (EQ DISPOSAL 'FREE)
	 (FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP))
    (OR (EQ DISPOSAL 'SETUP) (FUNCALL SG))))

;; The FH- functions are those intended to run in the other stack group.
;; Those that come back should be started up with RUN-SG.
;; They must be given the error handler stack group as an argument
;; so that they can call it back.  This they must do without making any other
;; intervening active call blocks on the stack, so that the foothold data
;; can be found from the SG-AP when it returns.  They must also be given ERROR-HANDLER-RUNNING
;; as an argument, so that if it is T they can do an unwind protect that
;; does FREE-SECOND-LEVEL-ERROR-HANDLER-SG on the stack group that they aren't going to return
;; to in that case.  They must also be given the previous foothold's offset so that
;; SG-FOOTHOLD-DATA can be reset in case of a throw.

;; Those that do not come back should be started up with SG-RUN-GOODBYE.

(DEFUN FH-APPLIER (FN ARGS NEW-+ NEW-* SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET ((+ NEW-+) (* NEW-*) EVALHOOK)
      (*CATCH 'FOOTHOLD
	      (FUNCALL SG (MULTIPLE-VALUE-LIST (APPLY FN ARGS))))
      ;; This is in case the catch catches.
      (FUNCALL SG 'LOSE))
    ;; This is reached only if we throw through this frame.
    (SETF (SG-FOOTHOLD-DATA %CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

(DEFUN FH-EVALER (FORM NEW-+ NEW-* SG EH-P PREV-FH)
  (UNWIND-PROTECT
    (LET ((+ NEW-+) (* NEW-*) EVALHOOK)
      (*CATCH 'FOOTHOLD
	      (FUNCALL SG (MULTIPLE-VALUE-LIST (EVAL FORM))))
      ;; This is in case the catch catches.
      (FUNCALL SG 'LOSE))
    (SETF (SG-FOOTHOLD-DATA %CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

(DEFUN FH-STREAM-BINDING-EVALER (FORM NEW-+ NEW-* SG EH-P PREV-FH EH-TERMINAL-IO)
  (DECLARE (SPECIAL OLD-TERMINAL-IO OLD-STANDARD-OUTPUT OLD-STANDARD-INPUT))
  (UNWIND-PROTECT
    (LET ((OLD-TERMINAL-IO TERMINAL-IO) 
	  (OLD-STANDARD-OUTPUT STANDARD-OUTPUT) (OLD-STANDARD-INPUT STANDARD-INPUT)
	  (+ NEW-+) (* NEW-*) EVALHOOK WIN-P RESULT)
      (LET ((TERMINAL-IO EH-TERMINAL-IO)
	    (STANDARD-INPUT 'SI:TERMINAL-IO-SYN-STREAM)
	    (STANDARD-OUTPUT 'SI:TERMINAL-IO-SYN-STREAM))
        (*CATCH 'FOOTHOLD
		(SETQ RESULT (MULTIPLE-VALUE-LIST (EVAL FORM))
		      WIN-P T)))
      (COND (WIN-P
	     (SETQ TERMINAL-IO OLD-TERMINAL-IO
		   STANDARD-OUTPUT OLD-STANDARD-OUTPUT
		   STANDARD-INPUT OLD-STANDARD-INPUT)
	     (FUNCALL SG RESULT))
	    (T (FUNCALL SG 'LOSE))))
    (SETF (SG-FOOTHOLD-DATA %CURRENT-STACK-GROUP) PREV-FH)
    (AND EH-P (FREE-SECOND-LEVEL-ERROR-HANDLER-SG SG))))

(DEFUN FH-THROWER (LABEL VALUE)
  (*THROW LABEL VALUE))

(DEFUN FH-UNWINDER (LABEL VALUE COUNT ACTION)
  (*UNWIND-STACK LABEL VALUE COUNT ACTION))

;; Various utility ANALYSIS functions.

;; These functions take an SG and an AP, and return the AP
;; for the previous open or active stack frame.
;Result is NIL if this is the bottom frame
(DEFUN SG-PREVIOUS-OPEN (SG AP)
  (LET ((DELTA (RP-DELTA-TO-OPEN-BLOCK (SG-REGULAR-PDL SG) AP)))
    (IF (ZEROP DELTA) NIL (- AP DELTA))))

;Result is NIL if this is the bottom frame
(DEFUN SG-PREVIOUS-ACTIVE (SG AP)
  (LET ((DELTA (RP-DELTA-TO-ACTIVE-BLOCK (SG-REGULAR-PDL SG) AP)))
    (IF (ZEROP DELTA) NIL (- AP DELTA))))

;; Returns NIL if there is no next.
(DEFUN SG-NEXT-OPEN (SG AP)
  (DO ((THIS-AP (SG-AP SG) (SG-PREVIOUS-OPEN SG THIS-AP))
       (NEXT-AP NIL THIS-AP))
      ((= THIS-AP AP) NEXT-AP)))

;; Returns NIL if there is no next.
(DEFUN SG-NEXT-ACTIVE (SG AP)
  (DO ((THIS-AP (SG-AP SG) (SG-PREVIOUS-ACTIVE SG THIS-AP))
       (NEXT-AP NIL THIS-AP))
      ((= THIS-AP AP) NEXT-AP)))

;; Scan several active frames up or down from a given one.
;; We return two values; the first is the offset of the frame found,
;; and the second is T if the specified number of frames were found
;; before the top or bottom of the stack.
(DEFUN SG-NEXT-NTH-ACTIVE (SG FRAME &OPTIONAL (COUNT 1))
  (COND ((= COUNT 0) FRAME)
	((MINUSP COUNT)
	 (DO ((P FRAME (SG-PREVIOUS-ACTIVE SG P))
	      (I 0 (1- I))
	      (PP NIL P))
	     (())
	   (AND (OR (NULL P) (= I COUNT))
		(RETURN (OR P PP) P))))
	(T (DO ((P FRAME (SG-NEXT-ACTIVE SG P))
		(I 0 (1+ I))
		(PP NIL P))
	       (())
	     (AND (OR (NULL P) (= I COUNT))
		  (RETURN (OR P PP) P))))))

(DEFUN FEF-INSTRUCTION (FEF PC)
  (LET ((IDX (// PC 2)))
    (COND ((ZEROP (LOGAND 1 PC))
	   (%P-LDB-OFFSET %%Q-LOW-HALF FEF IDX))
	  ((%P-LDB-OFFSET %%Q-HIGH-HALF FEF IDX)))))

;Takes a functional object, and returns a Lisp object which is its "name".
(DEFUN FUNCTION-NAME (FUNCTION)
  (SELECT (%DATA-TYPE FUNCTION)
    (DTP-FEF-POINTER (FEF-NAME FUNCTION))
    (DTP-U-ENTRY (MICRO-CODE-ENTRY-NAME-AREA (%POINTER FUNCTION)))
    (DTP-LIST (COND ((EQ (CAR FUNCTION) 'NAMED-LAMBDA)
		     (IF (ATOM (CADR FUNCTION)) (CADR FUNCTION)
			 (CAADR FUNCTION)))
		    (T FUNCTION)))
    (DTP-CLOSURE (FUNCTION-NAME (CAR (%MAKE-POINTER DTP-LIST FUNCTION))))
    (DTP-STACK-GROUP (SG-NAME FUNCTION))
    (DTP-SYMBOL FUNCTION)
    (OTHERWISE FUNCTION)))

;; Scan several active frames up or down from a given one,
;; being smart about calls to interpreted functions.
;; We return two values; the first is the offset of the frame found,
;; and the second is T if the specified number of frames were found
;; before the top or bottom of the stack.
(DEFUN SG-NEXT-NTH-INTERESTING-ACTIVE (SG FRAME &OPTIONAL (COUNT 1))
  (COND ((= COUNT 0) FRAME)
	((MINUSP COUNT)
	 (DO ((P FRAME (SG-PREVIOUS-INTERESTING-ACTIVE SG P))
	      (I 0 (1- I))
	      (PP NIL P))
	     (())
	   (AND (OR (NULL P) (= I COUNT))
		(RETURN (OR P PP) P))))
	(T (DO ((P FRAME (SG-NEXT-INTERESTING-ACTIVE SG P))
		(I 0 (1+ I))
		(PP NIL P))
	       (())
	     (AND (OR (NULL P) (= I COUNT))
		  (RETURN (OR P PP) P))))))

;; Return the next frame, counting all the actual frames of parts of an
;; interpreted function as if they were one frame.
(DEFUN SG-NEXT-INTERESTING-ACTIVE (SG AP &AUX (RP (SG-REGULAR-PDL SG)))
  (COND ((ATOM (RP-FUNCTION-WORD RP AP))
	 (SG-NEXT-ACTIVE SG AP))
	(T (DO ((NEW-AP (SG-NEXT-ACTIVE SG AP) (SG-NEXT-ACTIVE SG NEW-AP)))
	       ((OR (NULL NEW-AP)
		    (NOT (MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD RP NEW-AP))
			       UNINTERESTING-FUNCTIONS)))
		NEW-AP)))))

(DEFUN SG-PREVIOUS-INTERESTING-ACTIVE (SG AP)
  (SG-OUT-TO-INTERESTING-ACTIVE SG (SG-PREVIOUS-ACTIVE SG AP)))

;; Given a frame, find out if it is one of the frames of a call to an interpreted function.
;; If so, return the outermost frame of this call to the interpreted function.
;; If not, return the original frame.
(DEFUN SG-OUT-TO-INTERESTING-ACTIVE (SG AP &AUX (RP (SG-REGULAR-PDL SG)))
  (COND ((NULL AP) NIL)
	((NOT (MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD RP AP)) UNINTERESTING-FUNCTIONS))
	 AP)
	(T (DO ((NEW-AP AP (SG-PREVIOUS-ACTIVE SG NEW-AP)))
	       ((OR (NULL NEW-AP)
		    (NOT (MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD RP NEW-AP))
			       UNINTERESTING-FUNCTIONS)))
		(COND ((NULL NEW-AP) AP)
		      ((ATOM (RP-FUNCTION-WORD RP NEW-AP)) AP)
		      (T NEW-AP)))))))

;; Assuming that AP points to the outermost actual frame
;; of a call to an interpreted function, return its innermost active *EVAL frame.
(DEFUN SG-INNERMOST-UNINTERESTING-ACTIVE-EVAL (SG AP &AUX (RP (SG-REGULAR-PDL SG)))
  (DO ((NEW-AP AP FOLLOWING-AP)
       (FOLLOWING-AP) (LAST-EVAL-AP))
      (())
    (AND (EQ (RP-FUNCTION-WORD RP NEW-AP) #'SI:*EVAL)
	 (SETQ LAST-EVAL-AP NEW-AP))
    (SETQ FOLLOWING-AP (SG-NEXT-ACTIVE SG NEW-AP))
    (OR (AND FOLLOWING-AP
	     (MEMQ (FUNCTION-NAME (RP-FUNCTION-WORD RP FOLLOWING-AP))
		   UNINTERESTING-FUNCTIONS))
	(RETURN LAST-EVAL-AP))))

;; Return a name for the "function" to tell the user about
;; corresponding to the macro instruction in which an error happened.
(DEFUN SG-ERRING-FUNCTION (SG)
  (LET ((AP (SG-AP SG))
	(RP (SG-REGULAR-PDL SG)))
    (LET ((FUNCTION (RP-FUNCTION-WORD RP AP))
	  (PC (1- (RP-EXIT-PC RP AP))))
      (SELECT (%DATA-TYPE FUNCTION)
	(DTP-U-ENTRY
	  (MICRO-CODE-ENTRY-NAME-AREA (%POINTER FUNCTION)))
	(DTP-FEF-POINTER 
	  (LET ((INST (FEF-INSTRUCTION FUNCTION PC)))
	    (LET ((OP (LDB 1104 INST))
		  (DEST (LDB 1503 INST))
		  (DISP (LDB 0011 INST)))
	      (COND ((< OP 11)
		     (NTH OP '(FUNCALL FUNCALL MOVE-INSTRUCTION CAR CDR CADR CDDR CDAR CAAR)))
		    ((= OP 11)
		     (NTH DEST '(ND1-UNUSED *PLUS *DIF *TIMES	;*'s to avoid confusion with
				 *QUO *LOGAND *LOGXOR *LOGIOR)));argument-number
		    ((= OP 12)
		     (NTH DEST '(= > < EQ CDR CDDR 1+ 1-)))
		    ((= OP 13)
		     (NTH DEST '(ND3-UNUSED BIND BIND SET-NIL SET-ZERO PUSH-E MOVEM POP)))
		    ((= OP 14)
		     'A-BRANCH-INSTRUCTION)
		    ((< DISP 100) 'LIST)
		    ((< DISP 200) 'LIST-IN-AREA)
		    ((< DISP 220) 'UNBIND)
		    ((< DISP 240) 'A-POP-PDL-INSTRUCTION)
		    (T (MICRO-CODE-SYMBOL-NAME-AREA (- DISP 200)))))))
	(OTHERWISE FUNCTION)))))

;; Return the name of the localno'th local of function, or nil if unavailable or none such.
;; This is only meaningful for fefs if localno > 0.
;; If localno = 0 it will get the name of the rest arg, if there is one,
;; for any type of function.
(DEFUN LOCAL-NAME (FUNCTION LOCALNO &AUX ARGL)
  (COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	 (COMPILER:DISASSEMBLE-LOCAL-NAME FUNCTION LOCALNO))
	((AND (ZEROP LOCALNO)
	      (SETQ ARGL (COND ((LISTP FUNCTION)
				(SELECTQ (CAR FUNCTION)
				  (LAMBDA (CADR FUNCTION))
				  (NAMED-LAMBDA (CADDR FUNCTION))))
			       (T (ARGLIST FUNCTION T)))))
	 (CADR (MEMQ '&REST ARGL)))))

;; Return the name of the argno'th arg of function, or nil if
;; not known or function doesn't want that many args.
;; Rest args don't count.
(DEFUN ARG-NAME (FUNCTION ARGNO &AUX ARGL)
  (COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	 (COMPILER:DISASSEMBLE-ARG-NAME FUNCTION ARGNO))
	((SETQ ARGL (COND ((LISTP FUNCTION)
			   (SELECTQ (CAR FUNCTION)
			     (LAMBDA (CADR FUNCTION))
			     (NAMED-LAMBDA (CADDR FUNCTION))))
			  (T (ARGLIST FUNCTION T))))
	 (DO ((ARGL ARGL (CDR ARGL))
	      (I ARGNO))
	     ((OR (NULL ARGL) (EQ (CAR ARGL) '&AUX) (EQ (CAR ARGL) '&REST)))
	   (OR (MEMQ (CAR ARGL) LAMBDA-LIST-KEYWORDS)
	       (COND (( I 0)
		      (RETURN (CAR ARGL)))
		     (T (SETQ I (1- I)))))))))

;; Get the value of the rest arg in a given frame.
;; The first value is the value of the rest arg (nil if the frame has none).
;; The second value is T if the function expects to have one.
;; The third value indicates a rest arg explicitly passed as one;
;; it can conceivably be T even if the second is nil, if something strange
;; has happened, and an extraneous rest arg has been passed.
(DEFUN SG-REST-ARG-VALUE (SG FRAME &AUX
			     (RP (SG-REGULAR-PDL SG))
			     (AP FRAME)
			     LEXPR-CALL ARGS-INFO REST-ARG NARGS-EXPECTED
			     (FUNCTION (RP-FUNCTION-WORD RP AP))
			     (NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP)))
  (COND ((OR (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER) (LISTP FUNCTION))
	 (SETQ ARGS-INFO (ARGS-INFO FUNCTION))
	 (SETQ REST-ARG (LDB-TEST 2402 ARGS-INFO))
	 (SETQ NARGS-EXPECTED (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))))
  (AND (NOT (ZEROP (RP-ADI-PRESENT RP AP)))
       (DO I (- AP %LP-CALL-BLOCK-LENGTH) (- I 2) NIL
	   (SELECT (LDB %%ADI-TYPE (AREF RP I))
	     ((ADI-FEXPR-CALL ADI-LEXPR-CALL)
	      (RETURN (SETQ LEXPR-CALL T))))	;Last arg supplied is a rest arg
	   (AND (ZEROP (%P-FLAG-BIT (ALOC RP (1- I))))
		(RETURN NIL))))
  (PROG () (RETURN 
	     (COND (LEXPR-CALL (AREF RP (+ AP NARGS-SUPPLIED)))
		   ((LISTP FUNCTION)
		    (COND ((> NARGS-SUPPLIED NARGS-EXPECTED)
			   (%MAKE-POINTER DTP-LIST
					  (ALOC RP (+ AP NARGS-EXPECTED 1))))
			  (T NIL)))
		   (T (AREF RP (+ AP (RP-LOCAL-BLOCK-ORIGIN RP AP)))))
	     REST-ARG
	     LEXPR-CALL)))

;; Return the number of spread args present in a given frame.
;; This will not count any args which are part of a rest arg.
(DEFUN SG-NUMBER-OF-SPREAD-ARGS (SG FRAME &AUX
				    (RP (SG-REGULAR-PDL SG)) (AP FRAME)
				    ARGS-INFO REST-ARG-P NARGS-EXPECTED NARGS-VISIBLE
				    (FUNCTION (RP-FUNCTION-WORD RP AP))
				    (NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP)))
  (COND ((OR (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER) (LISTP FUNCTION))
	 (SETQ ARGS-INFO (ARGS-INFO FUNCTION))
	 (SETQ REST-ARG-P (LDB-TEST 2402 ARGS-INFO))
	 (SETQ NARGS-EXPECTED (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO))))
  ;; See if this is a lexpr-call.  If so, the last "arg" is a rest arg, so decrement nargs.
  (AND (NOT (ZEROP (RP-ADI-PRESENT RP AP)))
       (DO I (- AP %LP-CALL-BLOCK-LENGTH) (- I 2) NIL
	   (SELECT (LDB %%ADI-TYPE (AREF RP I))
	     ((ADI-FEXPR-CALL ADI-LEXPR-CALL)
	      (RETURN (SETQ NARGS-SUPPLIED (1- NARGS-SUPPLIED)))))
	   (AND (ZEROP (%P-FLAG-BIT (ALOC RP (1- I))))
		(RETURN NIL))))
  ;; The args that can be asked for are the ones supplied,
  ;; except that FEFs make slots for all args they expect whether supplied or not.
  (SETQ NARGS-VISIBLE
	(COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	       (MAX NARGS-SUPPLIED NARGS-EXPECTED))
	      (T NARGS-SUPPLIED)))
  ;; If function is known to take a rest arg, any unexpected args
  ;; are part of it, so they don't count as there this way.
  (AND REST-ARG-P (> NARGS-SUPPLIED NARGS-EXPECTED)
       (SETQ NARGS-VISIBLE NARGS-EXPECTED))
  NARGS-VISIBLE)

;; Return the value of the argno'th spread arg in a given frame, or nil if there is none.
;; The second value is T if that number arg is present in the frame.
(DEFUN SG-ARG-VALUE (SG FRAME ARGNO &AUX (RP (SG-REGULAR-PDL SG)) (AP FRAME))
  (PROG () (OR (MINUSP ARGNO) ( ARGNO (SG-NUMBER-OF-SPREAD-ARGS SG FRAME))
	       (RETURN (AREF RP (+ AP ARGNO 1)) T))))

;; These functions know about the location tags used in the ERROR-TABLE
;; entries, and how to creates locatives to them, fetch from them,
;; and store into them.
;;   There is the issue that the contents may be illegal datatypes.
;; Have to think about if there are screw cases, etc.

; Analysis
(DEFUN SG-CONTENTS (SG LOC)
  (SELECTQ LOC
    (M-A (SG-AC-A SG))
    (M-B (SG-AC-B SG))
    (M-C (SG-AC-C SG))
    (M-D (SG-AC-C SG))
    (M-E (SG-AC-E SG))
    (M-T (SG-AC-T SG))
    (M-R (SG-AC-R SG))
    (M-Q (SG-AC-Q SG))
    (M-I (SG-AC-I SG))
    (M-J (SG-AC-J SG))
    (M-S (SG-AC-S SG))
    (M-K (SG-AC-K SG))
    (A-QCSTKG SG)
    (A-SG-PREVIOUS-STACK-GROUP (SG-PREVIOUS-STACK-GROUP SG))
    (PP (AREF (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
    (RMD (%P-CONTENTS-OFFSET (SG-SAVED-VMA SG) 0))
    (OTHERWISE
      (COND ((AND (LISTP LOC) (EQ (CAR LOC) 'PP))
	     (AREF (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
	    ((BAD-HACKER LOC "Unknown tag"))))))

;; Metamorphosis
(DEFUN SG-STORE (X SG LOC)
  (SELECTQ LOC
    (M-A (SETF (SG-AC-A SG) X))
    (M-B (SETF (SG-AC-B SG) X))
    (M-C (SETF (SG-AC-C SG) X))
    (M-D (SETF (SG-AC-C SG) X))
    (M-E (SETF (SG-AC-E SG) X))
    (M-T (SETF (SG-AC-T SG) X))
    (M-R (SETF (SG-AC-R SG) X))
    (M-Q (SETF (SG-AC-Q SG) X))
    (M-I (SETF (SG-AC-I SG) X))
    (M-J (SETF (SG-AC-J SG) X))
    (M-S (SETF (SG-AC-S SG) X))
    (M-K (SETF (SG-AC-K SG) X))
    (A-QCSTKG (ERROR T "You can't store in this!"))
    (A-SG-PREVIOUS-STACK-GROUP (SETF (SG-PREVIOUS-STACK-GROUP SG) X))
    (PP (ASET X (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
    (RMD (%P-STORE-CONTENTS (SG-SAVED-VMA SG) X))	;Offset???
    (OTHERWISE
      (COND ((AND (LISTP LOC) (EQ (CAR LOC) 'PP))
	     (ASET X (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
	    ((BAD-HACKER LOC "Unknown tag"))))))

;; Getllocativepointersis
(DEFUN SG-LOCATE (SG LOC)
  (SELECTQ LOC
    (M-A (LOCF (SG-AC-A SG)))
    (M-B (LOCF (SG-AC-B SG)))
    (M-C (LOCF (SG-AC-C SG)))
    (M-D (LOCF (SG-AC-D SG)))
    (M-E (LOCF (SG-AC-E SG)))
    (M-T (LOCF (SG-AC-T SG)))
    (M-R (LOCF (SG-AC-R SG)))
    (M-Q (LOCF (SG-AC-Q SG)))
    (M-I (LOCF (SG-AC-I SG)))
    (M-J (LOCF (SG-AC-J SG)))
    (M-S (LOCF (SG-AC-S SG)))
    (M-K (LOCF (SG-AC-K SG)))
    (A-QCSTKG (%MAKE-POINTER DTP-LOCATIVE SG))
    (A-SG-PREVIOUS-STACK-GROUP (LOCF (SG-PREVIOUS-STACK-GROUP SG)))
    (PP (ALOC (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
    (RMD (%MAKE-POINTER DTP-LOCATIVE (SG-SAVED-VMA SG)))
    (OTHERWISE
      (COND ((AND (LISTP LOC) (EQ (CAR LOC) 'PP))
	     (ALOC (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
	    ((BAD-HACKER LOC "Unknown tag"))))))

;Get the special-pdl pointer for the running SG
(DEFUN GET-OWN-SPECIAL-PDL-POINTER (SP)
  (- (1- (%STRUCTURE-BOXED-SIZE SP))
     (+ (ARRAY-LEADER-LENGTH SP) 3 (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG SP 0))))

;An ALOC that only works for 1-dimensional arrays.  It avoids referencing the
;word pointed to since if the special-pdl pointer being used is confused that
;might be an external-value-cell-pointer, causing an error.  This doesn't
;do bounds checking.
(DEFUN ALOC-CAREFUL (ARRAY INDEX)
  (%MAKE-POINTER-OFFSET DTP-LOCATIVE
			ARRAY
			(+ INDEX 1 (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))))

;;; Find the value of a symbol in the binding environment of a specified stack group.
;;; Note that this cannot get an error even if the sg is in some funny state, unlike
;;; SG-EVAL.
(DEFUN SYMEVAL-IN-STACK-GROUP (SYM SG)
  (COND ((EQ SG %CURRENT-STACK-GROUP) (SYMEVAL SYM))
	(T
	 (DO-NAMED RESULT
		   ((VCL (VALUE-CELL-LOCATION SYM))
		    (SP (SG-SPECIAL-PDL SG))
		    (SPP (SG-SPECIAL-PDL-POINTER SG)))
		   ()
	   (OR	( SPP 0)
		(ZEROP (SG-IN-SWAPPED-STATE SG))	;If its bindings are swapped out
		(DO ((I SPP (1- I))		;then search through them
		     (P))
		    (( I 0))
		  (SETQ P (ALOC-CAREFUL SP I))
		  (SELECT (%P-DATA-TYPE P)
		    (DTP-LOCATIVE		;If this is a binding pair
		     (SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		     (IF (EQ (AREF SP I) VCL)	;and is for this variable, then return
			 (RETURN-FROM RESULT (CAR P))	;the saved value, invz'ing if necc
			 (SETQ I (1- I))))	;Space over second Q of binding pair
		    (OTHERWISE ))))		;Ignore non-binding blocks
	   ;; The variable isn't bound in that stack group, so we want its global value.
	   ;; Must ignore bindings in our own stack group.
	   (SETQ SP (SG-SPECIAL-PDL %CURRENT-STACK-GROUP)
		 SPP (GET-OWN-SPECIAL-PDL-POINTER SP))
	   (DO ((VAL (SYMEVAL SYM))
		(I SPP (1- I))
		(P))
	       (( I 0) (RETURN-FROM RESULT VAL))
	     (SETQ P (ALOC-CAREFUL SP I))
	     (SELECT (%P-DATA-TYPE P)
	       (DTP-LOCATIVE
		(SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		(IF (EQ (AREF SP I) VCL)
		    (SETQ VAL (CAR P)))
		(SETQ I (1- I)))
	       (OTHERWISE )))))))

(DEFUN USE-COLD-LOAD-STREAM (STRING)
  (SETQ TERMINAL-IO TV:COLD-LOAD-STREAM)
  (FUNCALL TERMINAL-IO ':HOME-CURSOR)
  (FUNCALL TERMINAL-IO ':CLEAR-EOL)
  (FORMAT TERMINAL-IO "--> ~A, using the cold load stream <--~2%" STRING))

(DEFMACRO PRINT-CAREFULLY (TYPE . BODY)
  `(OR (ERRSET (PROGN . ,BODY) NIL)
       (OR (ERRSET (FORMAT T "<<Error printing ~A>>" ,TYPE) NIL)
	   (USE-COLD-LOAD-STREAM (FORMAT NIL "<<Error printing ~A>>" ,TYPE)))))

;; Various initialization routines.

(DEFUN ASSURE-TABLE-LOADED ()
  (COND ((NOT (= MICROCODE-ERROR-TABLE-VERSION-NUMBER %MICROCODE-VERSION-NUMBER))
	 (LOAD-ERROR-TABLE)
	 (OR (= MICROCODE-ERROR-TABLE-VERSION-NUMBER %MICROCODE-VERSION-NUMBER)
	     (BREAK 'CANNOT-GET-ERROR-TABLE T)))))

(DEFUN LOAD-ERROR-TABLE (&AUX (FUDGED-USER-ID-FLAG NIL))
  (COND ((OR (NULL USER-ID) (STRING-EQUAL USER-ID ""))
	 (SETQ FUDGED-USER-ID-FLAG T)
	 (LOGIN "ERROR" T)))
  (LOAD (FORMAT NIL "DSK:LISPM1;UCADR ~DTBL" %MICROCODE-VERSION-NUMBER) "EH")
  (AND FUDGED-USER-ID-FLAG (LOGOUT)))

;; Divides up MICROCODE-ERROR-TABLE into CALLS-SUB-LIST, RESTART-LIST, and ERROR-TABLE.
(DEFUN ASSURE-TABLE-PROCESSED ()
  (COND ((NOT (= MICROCODE-ERROR-TABLE-VERSION-NUMBER ERROR-TABLE-NUMBER))
	 (SETQ ERROR-TABLE NIL
	       CALLS-SUB-LIST NIL
	       RESTART-LIST NIL)
	 (DO ET MICROCODE-ERROR-TABLE (CDR ET) (NULL ET)
	     (SELECTQ (CADAR ET)
	       (RESTART (PUSH (CONS (CADDAR ET) (1+ (CAAR ET))) RESTART-LIST))
	       (CALLS-SUB (PUSH (CONS (CAAR ET) (CADDAR ET)) CALLS-SUB-LIST))
	       (OTHERWISE (PUSH (CAR ET) ERROR-TABLE))))
	 (SETQ ERROR-TABLE-NUMBER MICROCODE-ERROR-TABLE-VERSION-NUMBER))))

;; Call this when it is apparent that some hacker set things up wrong.
(DEFUN BAD-HACKER (&REST ARGS)
  (FORMAT T "~%~%Foo, a hacker has screwn up somewhere.  Error:~%")
  (DO AL ARGS (CDR AL) (NULL AL) (PRINC (CAR AL)) (TYO #\SP))
  (TERPRI) (TERPRI))

;; Turn on error trapping mode.
(DEFUN ENABLE-TRAPPING (&OPTIONAL (X 1))
  (SETQ %MODE-FLAGS (DPB X %%M-FLAGS-TRAP-ENABLE %MODE-FLAGS)))

(DEFUN TRAPPING-ENABLED-P NIL 
  (NOT (ZEROP (LDB %%M-FLAGS-TRAP-ENABLE %MODE-FLAGS))))

(DEFUN P-PRIN1-CAREFUL (LOCATIVE &AUX)
  (LET ((DTP (Q-DATA-TYPES (%P-DATA-TYPE LOCATIVE))))
    (COND ((MEMQ DTP GOOD-DATA-TYPES)
	   (PRINT-CAREFULLY "printing" (PRIN1 (CAR LOCATIVE))))
	  (T (FORMAT T "#<~A ~O>" DTP (%P-POINTER LOCATIVE))))))

;; Initialize the error handler at warm boot time.
(ADD-INITIALIZATION "ERROR-HANDLER-INITIALIZE" '(INITIALIZE) '(WARM))

;;; Waiting until the first error to do these things loses
;;; because they let other processes run, which could get errors and crash the machine.
(DEFUN INITIALIZE ()
  (SETQ ERROR-HANDLER-RUNNING NIL)
  (SETQ ERRSET-STATUS NIL)		;Set to T if an errset exists and should be obeyed
  (ASSURE-TABLE-LOADED)			;Gets the right UCONS/UCADR TABLE file loaded.
  (ASSURE-TABLE-PROCESSED)		;Processes the contents of UCONS/UCADR TABLE.
  )

;; This is the function that runs in the first level error handler
;; It is called only at boot time.  From then on it just keeps coroutining.
(DEFUN LISP-ERROR-HANDLER (&AUX M SG SG2 ETE CONDITION (INHIBIT-SCHEDULING-FLAG T))
  ;; Return to boot code.  We are called back by the first error.
  (SETQ M (FUNCALL %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP NIL))
  (DO ((ERRSET-FLAG NIL NIL)	;These must be reinitialized each time through the loop!
       (ERRSET-PRINT-MSG NIL NIL))
      (NIL)			;Do forever, once for each error
    (SETQ SG %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)
    (ASSURE-DISPATCH-SET-UP)	;Set up command dispatch table.
    ;; Compute and store the ETE for this error.
    (SETF (SG-TRAP-TAG SG)
	  (SETQ ETE (OR M (CDR (ASSQ (SG-TRAP-MICRO-PC SG) ERROR-TABLE)))))
    (SETF (SG-PROCESSING-ERROR-FLAG SG) 0) ;Re-enable error trapping in that SG
    (SETF (SG-INST-DISP SG) 0)	;Turn off single-step mode (for foothold)
    (SETQ CONDITION (GET (CAR ETE) 'SIGNAL))
    (AND CONDITION (SETQ CONDITION (FUNCALL CONDITION SG ETE)))
    ;; All branches of this COND must end in funcalling some other SG.
    (SETQ M
	  (COND ((AND (EQ (CAR ETE) 'STEP-BREAK)
		      (SETQ SG2 (CDR (ASSQ SG SG-STEPPING-TABLE))))
		 (SETF (SG-CURRENT-STATE SG) SYS:SG-STATE-RESUMABLE)
		 (FUNCALL SG2 SG))
		((AND (NOT (SYMEVAL-IN-STACK-GROUP 'ERRSET SG))
		      (SETQ ERRSET-FLAG (SYMEVAL-IN-STACK-GROUP 'ERRSET-STATUS SG))
		      (NOT (SETQ ERRSET-PRINT-MSG
				 (SYMEVAL-IN-STACK-GROUP 'ERRSET-PRINT-MSG SG)))
		      (NOT (AND CONDITION (SG-CONDITION-HANDLED-P SG (CAR CONDITION)))))
		 ;; If we are in an errset, and don't want the message, throw now.
		 (SG-THROW SG 'ERRSET-CATCH NIL T))
		(T
		 ;; Otherwise, obtain a second level error handler sg
		 ;; and tell it what to work on.
		 (SETQ SG2 (OR (POP FREE-SECOND-LEVEL-ERROR-HANDLER-SG-LIST)
			       (MAKE-STACK-GROUP
				 (FORMAT NIL "SECOND-LEVEL-ERROR-HANDLER-~D"
					 (SETQ SECOND-LEVEL-ERROR-HANDLER-COUNT
					       (1+ SECOND-LEVEL-ERROR-HANDLER-COUNT)))
				 ':SAFE 0)))
		 (STACK-GROUP-PRESET SG2 'SECOND-LEVEL-ERROR-HANDLER
				     SG M ERRSET-FLAG ERRSET-PRINT-MSG CONDITION
				     (COND ((EQ SG SI:SCHEDULER-STACK-GROUP)
					    "Error in the scheduler")
					   ((AND (BOUNDP 'TV:KBD-PROCESS)
						 (EQ SG (PROCESS-STACK-GROUP TV:KBD-PROCESS)))
					    "Error in the keyboard process")
					   ((AND (BOUNDP 'TV:MOUSE-PROCESS)
						 (EQ SG (PROCESS-STACK-GROUP TV:MOUSE-PROCESS)))
					    "Error in the mouse process")))
		 (FUNCALL SG2))))))

;; Invoke the error handler to look at a particular stack group.
;; A window or process may also be supplied, and a stack group found from it.
;; Supplying NIL means find a process which is waiting to be looked at.
;; If a process is supplied or known, it is arrested while we are invoked.
;; This works differently from real errors; it just runs the error handler
;; in the same stack group and process that EH is called in
;; ERROR-HANDLER-RUNNING is NOT set.
;; The catch tag EXIT is used to return from EH.
(DEFUN EH (&OPTIONAL PROCESS
	   &AUX PKG SG ARREST-REASON
		ORIGINAL-FRAME CURRENT-FRAME CONDITION-PROCEED-VALUE CONDITION-PROCEED-FLAG)

  (AND (NULL PROCESS)
       (SETQ PROCESS (TV:FIND-PROCESS-IN-ERROR)))
  (COND ((NULL PROCESS) "cannot find a process")
	(T
	 ;; If arg is a window or stream, extract process from it.
	 (OR (TYPEP PROCESS ':STACK-GROUP) (TYPEP PROCESS 'SI:PROCESS)
	     (SETQ PROCESS (FUNCALL PROCESS ':PROCESS)))
	 ;; If arg is process or was converted to one, stop it.
	 (COND ((TYPEP PROCESS 'SI:PROCESS)
		(FUNCALL PROCESS ':ARREST-REASON CURRENT-PROCESS)
		(SETQ ARREST-REASON CURRENT-PROCESS)
		(SETQ SG (PROCESS-STACK-GROUP PROCESS)))
	       (T (SETQ SG PROCESS PROCESS NIL)))
	 (OR (TYPEP SG ':STACK-GROUP) (FERROR NIL "~S not a stack group" SG))
	 (SETQ ORIGINAL-FRAME (SG-AP SG))
	 (SETQ CURRENT-FRAME (SG-OUT-TO-INTERESTING-ACTIVE SG ORIGINAL-FRAME))
	 ;; Although we get the package each time around the r-e-p loop, we must get it
	 ;; here as well, so that when the error message is printed it will be in the
	 ;; right package.
	 (SETQ PKG (SYMEVAL-IN-STACK-GROUP 'PACKAGE SG))
	 (UNWIND-PROTECT
	   (*CATCH 'QUIT
	     (*CATCH 'SI:TOP-LEVEL
	       (PKG-BIND (IF (EQ (TYPEP PKG) 'PACKAGE) PKG "USER")
		 (PRINT-CAREFULLY "frame"
		   (FORMAT T "~&~S  Backtrace: " SG)
		   (SHORT-BACKTRACE SG NIL 3)
		   (SHOW-FUNCTION-AND-ARGS SG)))))
	   (*CATCH 'EXIT (COMMAND-LOOP SG (SG-TRAP-TAG SG))))
	 (AND ARREST-REASON (FUNCALL PROCESS ':REVOKE-ARREST-REASON ARREST-REASON)))))

;; What CURRENT-PROCESS was at entry to SECOND-LEVEL-ERROR-HANDLER
;; which may have bound it to NIL.
(DEFVAR REAL-CURRENT-PROCESS)
(DEFVAR ERRSET-INSIDE-ERROR NIL)
 
;; This is the function at the top level in each second level error handler sg.
(DEFUN SECOND-LEVEL-ERROR-HANDLER (SG M ERRSET-FLAG ERRSET-PRINT-MSG CONDITION MSG
				   &AUX PKG (ERRSET ERRSET-INSIDE-ERROR)
				   (PACKAGE SI:PKG-USER-PACKAGE)
					(ERROR-HANDLER-RUNNING T)
					(ERROR-HANDLER-REPRINT-ERROR T)
					(ETE (SG-TRAP-TAG SG)) BREAK-FLAG
					(TERMINAL-IO (OR ERROR-HANDLER-IO
							 (SYMEVAL-IN-STACK-GROUP
							   'TERMINAL-IO SG)))
					(STANDARD-INPUT SI:SYN-TERMINAL-IO)
					(STANDARD-OUTPUT SI:SYN-TERMINAL-IO)
					(QUERY-IO SI:SYN-TERMINAL-IO)
					;; In case we want to set CURRENT-PROCESS to nil.
					(CURRENT-PROCESS CURRENT-PROCESS)
					;; And some things will wonder what it had been.
					(REAL-CURRENT-PROCESS CURRENT-PROCESS)
					ORIGINAL-FRAME CURRENT-FRAME
					CONDITION-PROCEED-VALUE CONDITION-PROCEED-FLAG)
  (COND ((EQ (CAR M) ':BREAK)
	 (SG-RESTORE-STATE SG)		;Restore state saved at FORCE-BREAK
	 (COND ((CADR M)		;If re-invoke, do so.
		(SG-REINVOKE SG)))
	 (SETF (SG-TRAP-TAG SG) M)))	;Remember that we are handling a break
  (*CATCH 'QUIT
    (*CATCH 'SI:TOP-LEVEL
      (PROG (CONDITION-RESULT)
	;; If we have a condition to signal, do so (in the debugged stack group)
	;; and maybe return or restart if it says so.
	(AND CONDITION
	     (LET ((CONDITION-PROCEED-FLAG T)
		   CONDITION-PROCEED-VALUE)
	       (SETQ CONDITION-RESULT (SG-APPLY SG #'SIGNAL CONDITION))
	       (COND ((EQ (CAR CONDITION-RESULT) 'RETURN)
		      (SETQ CONDITION-PROCEED-VALUE (CADR CONDITION-RESULT))
		      (COM-PROCEED SG ETE))
		     ((EQ (CAR CONDITION-RESULT) 'ERROR-RESTART)
		      (COM-ERROR-RESTART SG ETE))
		     ((EQ (CAR CONDITION-RESULT) 'RETURN-VALUE)
		      (SG-UNWIND-TO-FRAME SG (SG-AP SG) T (CADR CONDITION-RESULT))))))
	;; If non-printing errset, throw to it once condition is processed.
	(AND ERRSET-FLAG (NOT ERRSET-PRINT-MSG) (NEQ (CAR M) ':BREAK)
	     (NEQ (CAR ETE) 'PDL-OVERFLOW)	;Shouldn't be caught by ERRSET, and mustn't
	     (SG-THROW SG 'ERRSET-CATCH NIL))	; type out yet.
	;; Otherwise, decide whether to break or to go to top level.
	(SETQ BREAK-FLAG (SG-BREAK-P SG ETE CONDITION))
	(SETQ ORIGINAL-FRAME (SG-AP SG))
	(SETQ CURRENT-FRAME
	      (SG-NEXT-NTH-ACTIVE SG ORIGINAL-FRAME
				  (- (OR (GET (CAR M) 'BACKTRACE-SKIP) 0))))
	(SETQ CURRENT-FRAME (SG-OUT-TO-INTERESTING-ACTIVE SG CURRENT-FRAME))
	(AND MSG (USE-COLD-LOAD-STREAM MSG))
	;; If not running in the scheduler, give us a run reason in case we died after
	;; becoming inactive, before getting back to the scheduler.
	(OR (NULL CURRENT-PROCESS)
	    (FUNCALL CURRENT-PROCESS ':RUN-REASON %CURRENT-STACK-GROUP))
	;; Try to see if TERMINAL-IO is reasonable and if not fix it.
	(LET ((WO (ERRSET (FUNCALL TERMINAL-IO ':WHICH-OPERATIONS) NIL))
	      (ERROR-HANDLER-REPRINT-ERROR NIL))
	  (IF (NULL WO) (USE-COLD-LOAD-STREAM "TERMINAL-IO clobbered")
	      (COND ((MEMQ ':NOTIFY (CAR WO))
		     (LET ((OLD-TIO TERMINAL-IO))	;:NOTIFY can change it
		       (FUNCALL TERMINAL-IO ':NOTIFY ':ERROR)	;Do this in non-erring stack
		       (COND ((NEQ TERMINAL-IO OLD-TIO)
			      (SG-FUNCALL SG #'SET 'TERMINAL-IO TERMINAL-IO))))))))
	;; Although we get the package each time around the r-e-p loop, we must get it
	;; here as well, so that when the error message is printed it will be in the
	;; right package.
	(SETQ PKG (SYMEVAL-IN-STACK-GROUP 'PACKAGE SG))
	(PKG-BIND (IF (EQ (TYPEP PKG) 'PACKAGE) PKG SI:PKG-USER-PACKAGE)
	  (PRINT-CAREFULLY "error message"
	    ;; Print a brief message if not going to eh command level, else a long msg
	    (IF (OR (EQ (CAR M) ':BREAK) (AND BREAK-FLAG (NOT ERRSET-FLAG)))
		(SHOW SG ETE)
		(PRINT-ERROR-MESSAGE SG ETE T)))) 
	(AND ERRSET-FLAG (NEQ (CAR M) ':BREAK)	;Should PDL-OVERFLOW be catchable by ERRSET?
	     (SG-THROW SG 'ERRSET-CATCH NIL))
	;; If this error isn't interesting to break on,
	;; return to top level, or to innermost error break loop.
	(OR BREAK-FLAG (COM-THROW-ONE-ERROR SG ETE)))))
  (SETQ LAST-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP)
  ;;SG-TRAP-TAG is part of the state restored by SG-RESTORE-STATE in case of BREAK.
  ;;Thus, it does not win to have COMMAND-LOOP refetch it.
  (COMMAND-LOOP SG ETE))

;; Decide whether an error break loop is useful for this error.
;; It is unless the error was a simple error in the form immediately
;; typed in to the error handler (not inside any functions).
(DEFUN SG-BREAK-P (SG IGNORE CONDITION)
  (OR (NULL (SG-FOOTHOLD-DATA SG))
      (NOT (SELECTQ (CAR CONDITION)
	     (:WRONG-NUMBER-OF-ARGUMENTS
	       (EQ (LET ((AP (SG-PREVIOUS-ACTIVE SG (SG-PREVIOUS-ACTIVE SG (SG-AP SG))))
			 (RP (SG-REGULAR-PDL SG)))
		     (RP-FUNCTION-WORD RP AP))
		   #'FH-STREAM-BINDING-EVALER))
	     ((:UNDEFINED-VARIABLE :UNDEFINED-FUNCTION)
	      (EQ (LET ((AP (SG-PREVIOUS-ACTIVE SG (SG-AP SG)))
			(RP (SG-REGULAR-PDL SG)))
		    (RP-FUNCTION-WORD RP AP))
		  #'FH-STREAM-BINDING-EVALER))))))
