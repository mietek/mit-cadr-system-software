;;; -*-Mode:LISP; Package:SYSTEM-INTERNALS-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;; Error handler.       DLW 1/5/78 

;; Note: ETE = Error Table Entry.  Documented in EHR.

(DECLARE (SPECIAL EH-REAL-CURRENT-PROCESS	;What CURRENT-PROCESS was at entry
				        ;to EH-SECOND-LEVEL, which may have bound it to NIL.
		  ))
 
(DEFVAR EH-PRINLEVEL 2)		;These are used when printing error messages
(DEFVAR EH-PRINLENGTH 4)	; and values of variables in frames.

;; The error table, read from LISPM1;UCADR nnnTBL into MICROCODE-ERROR-TABLE,
;; describes the symbolic meaning of certain microcode pcs.
;; Its data is rearranged into other variables below.
;; EH-ERROR-TABLE relates the micro pc to a symbolic name of error,
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

;; EH-ASSURE-TABLE-PROCESSED looks at MICROCODE-ERROR-TABLE
;; and produces these lists.
(DEFVAR EH-CALLS-SUB-LIST)		;Alist of micropcs to symbols.
(DEFVAR EH-RESTART-LIST)		;Alist of symbols to micropcs.
(DEFVAR EH-ERROR-TABLE)			;List of ETEs.
(DEFVAR EH-ERROR-TABLE-NUMBER -1)	;Microcode version number for EH-ERROR-TABLE.

;; An error immediately runs the first level error handler stack group
;; whose job is to initialize a second level error handler stack group
;; in which the error handler actually runs.
;; EH-SECOND-LEVEL-COUNT is a count for giving each second level error handler a distinct name.
(DEFVAR EH-SECOND-LEVEL-COUNT 0)
;; Error handler stack groups that were exited and can be reused.
(DEFVAR EH-FREE-SECOND-LEVEL-SG-LIST NIL)
;; Last second level error handler to be running.
;; This is so that each of them can tell
;; when it is returned to whether some other one
;; has been running in the meanwhile.
(DEFVAR EH-LAST-SECOND-LEVEL-SG NIL)
;; This variable is bound to T in every second-level error handler to identify them.
(DEFVAR EH-RUNNING T)

(DEFVAR ERROR-HANDLER-IO NIL)	;If non-NIL, stream EH should use

;; Conditions.  Condition handlers are run by the second level error handler.
;; These variables are part of the mechanism by which condition handlers
;; ask to proceed from the error.
(DEFVAR EH-CONDITION-PROCEED-FLAG)	;Communicate from condition handlers to EH-PROCEED.
(DEFVAR EH-CONDITION-PROCEED-VALUE)	;See EH-GET-OBJECT.

;; EH-ERRSET-STATUS is T within an errset.
;; EH-ERRSET-PRINT-MSG is T if the error message should be printed anyway.
;; ERRSET is T if the error handler should be entered despite being in an errset.
(DEFVAR EH-ERRSET-STATUS NIL)
(DEFVAR EH-ERRSET-PRINT-MSG NIL)
(DEFVAR ERRSET NIL)

;; This is funcalled after printing the error message.
(DEFVAR ERROR-MESSAGE-HOOK NIL)

;; This is the dispatch table of error handler commands, a 4 by 220 array.
(DEFVAR EH-DISPATCH)
;; This is a list, setq'd in this file, from which EH-DISPATCH is initialized.
(DEFVAR EH-DISPATCH-LIST)
;; This file will set EH-DISPATCH-LIST; make sure EH-DISPATCH is recomputed from it
;; by EH-ASSURE-DISPATCH-SET-UP.
(MAKUNBOUND 'EH-DISPATCH)

;; Here are the error handler's main operating parameters.
(DEFVAR EH-SG)				;The stack group that got the error.
(DEFVAR EH-CURRENT-FRAME)		;The SG-AP of the frame that the error handler is looking at.
(DEFVAR EH-ORIGINAL-FRAME)		;The SG-AP of the frame that got the error.

;; This is a random gensymmed object which is returned
;; from EH-EVAL to indicate that an error occurred within.
(DEFVAR EH-ERROR-FLAG (NCONS NIL))

;; Number of levels of backtrace to print automatically upon error.
(DEFVAR EH-ERROR-MESSAGE-BACKTRACE-LENGTH 3)

;; Number of instructions to disassemble for M-L, etc., if we
;; can't determine the amount of room on the screen.
(DEFVAR EH-DISASSEMBLE-INSTRUCTION-COUNT 20)

;; Calls to these functions should not be mentioned as frames
;; when stack-censoring is going on in interpreted functions.
;; This should include all functions that have &QUOTE args and are open-compiled.
;; *EVAL and APPLY-LAMBDA are there for peculiar reasons.
(DEFVAR EH-UNINTERESTING-FUNCTIONS '(*EVAL APPLY-LAMBDA COND SETQ PROG GO DO DO-NAMED
				     MULTIPLE-VALUE MULTIPLE-VALUE-LIST
				     MULTIPLE-VALUE-RETURN AND OR STORE))

;;; These datatypes are OK to call print on
(DEFVAR EH-GOOD-TYPES '(DTP-SYMBOL DTP-FIX DTP-EXTENDED-NUMBER  
				   DTP-SMALL-FLONUM 
				   DTP-LIST DTP-U-ENTRY DTP-FEF-POINTER DTP-ARRAY-POINTER 
				   DTP-STACK-GROUP DTP-CLOSURE DTP-ENTITY))
;;; These point to something (as opposed to being Inums)
(DEFVAR EH-POINTER-TYPES '(DTP-NULL DTP-SYMBOL DTP-SYMBOL-HEADER DTP-EXTENDED-NUMBER
			   DTP-GC-FORWARD DTP-EXTERNAL-VALUE-CELL-POINTER DTP-ONE-Q-FORWARD
			   DTP-HEADER-FORWARD DTP-LOCATIVE DTP-LIST 
			   DTP-FEF-POINTER DTP-ARRAY-POINTER DTP-STACK-GROUP
			   DTP-CLOSURE DTP-SELECT-METHOD DTP-INSTANCE DTP-INSTANCE-HEADER
			   DTP-ENTITY))

;; This is a temporary kludge, which will be fixed by modifying the installed
;; LISP-REINITIALIZE when this thing gets installed.

(SETQ %INITIALLY-DISABLE-TRAPPING NIL)

;; Save a stack group's state on its stack so we can use it and then restore the state.
;; The information goes on the pdl in a fake frame belonging to the function EH-FOOTHOLD.
;; Each Q is saved as 2 words (pointer and tag) to avoid data type problems.
;; You must call this before pushing a call block, even if calling EH-RUN-SG-GOODBYE,
;; in order to clean up the QBBFL and the U-STACK Q's.
(DEFUN EH-SAVE-SG-STATE (SG &AUX P NEW-AP RP PP)
	 (SETQ RP (SG-REGULAR-PDL SG)
	       PP (SG-REGULAR-PDL-POINTER SG))
	 (SETQ NEW-AP (+ PP %LP-CALL-BLOCK-LENGTH))
	 (ASET (DPB (- NEW-AP (SG-IPMARK SG)) %%LP-CLS-DELTA-TO-OPEN-BLOCK
		    (DPB (- NEW-AP (SG-AP SG)) %%LP-CLS-DELTA-TO-ACTIVE-BLOCK
			 0))
	       RP (+ NEW-AP %LP-CALL-STATE))
	 (ASET 0 RP (+ NEW-AP %LP-EXIT-STATE))
	 (ASET 0 RP (+ NEW-AP %LP-ENTRY-STATE))
	 (ASET #'EH-FOOTHOLD RP (+ NEW-AP %LP-FEF))
	 (SETQ PP (1+ NEW-AP))
	 (DO I 0 (1+ I) (> I SG-PDL-PHASE)
	   (SETQ P (AP-LEADER SG I))
	   (ASET (IF (MEMQ (Q-DATA-TYPES (%P-DATA-TYPE P)) EH-POINTER-TYPES)
		     (%P-CONTENTS-AS-LOCATIVE P)
		     (%P-POINTER P))
		 RP PP)
	   (ASET (%P-LDB %%Q-ALL-BUT-POINTER P)
		 RP (1+ PP))
	   (SETQ PP (+ PP 2)))
	 (SETF (SG-REGULAR-PDL-POINTER SG) (1- PP))	;Index of last valid word
	 (SETF (SG-FLAGS-QBBFL SG) 0)		;Clear QBBFL left over from previous frame
	 (SETF (SG-IPMARK SG) NEW-AP)
	 (SETF (SG-AP SG) NEW-AP))

;;; This function isn't called, it just exists to name state-save frames
(DEFUN EH-FOOTHOLD () NIL)

;; Pop the saved state from the pdl into the current state.
(DEFUN EH-RESTORE-SG-STATE (SG)
  (LET ((PP (EH-PREVIOUS-ACTIVE SG (EH-PREVIOUS-ACTIVE SG (SG-AP SG))))
	(RP (SG-REGULAR-PDL SG)))
    (AND (NULL PP)
	 (FERROR NIL "~S state not saved" SG))
    (OR (EQ (AREF RP PP) #'EH-FOOTHOLD)
	(FERROR NIL "Saved state for ~S at ~S[~S] clobbered." SG RP PP))
    (SETQ PP (1+ PP))
    (DO I 0 (1+ I) (> I SG-PDL-PHASE)
      (%P-STORE-TAG-AND-POINTER (AP-LEADER SG I)
				(AREF RP (1+ PP))
				(AREF RP PP))
      (SETQ PP (+ PP 2)))))

;;; Low level routines for manipulating the stacks of a stack group.
;;; Call EH-SAVE-SG-STATE before calling any of these.

(DEFUN EH-REGPDL-PUSH (X SG &AUX PP)
    (SETQ PP (1+ (SG-REGULAR-PDL-POINTER SG)))
    (AS-1 X (SG-REGULAR-PDL SG) PP)
    (%P-STORE-CDR-CODE (AP-1 (SG-REGULAR-PDL SG) PP) CDR-NEXT)
    (SETF (SG-REGULAR-PDL-POINTER SG) PP)
    (SETF (SG-PDL-PHASE SG) (1+ (SG-PDL-PHASE SG)))
    X)

(DEFUN EH-REGPDL-POP (SG &AUX PP)
    (SETF (SG-PDL-PHASE SG) (1- (SG-PDL-PHASE SG)))
    (SETQ PP (SG-REGULAR-PDL-POINTER SG))
    (SETF (SG-REGULAR-PDL-POINTER SG) (1- PP))
    (AR-1 (SG-REGULAR-PDL SG) PP))

(DEFUN EH-SPECPDL-PUSH (X FLAG SG &AUX PP PDL)
    (SETQ PP (1+ (SG-SPECIAL-PDL-POINTER SG)))
    (SETF (SG-SPECIAL-PDL-POINTER SG) PP)
    (SETQ PDL (SG-SPECIAL-PDL SG))
    (AS-1 X PDL PP)
    (%P-STORE-FLAG-BIT (AP-1 PDL PP) FLAG)
    X)

(DEFUN EH-SPECPDL-POP (SG &AUX PP)
    (SETQ PP (SG-SPECIAL-PDL-POINTER SG))
    (SETF (SG-SPECIAL-PDL-POINTER SG) (1- PP))
    (AR-1 (SG-SPECIAL-PDL SG) PP))

;; This simulates the CBM (or P3ZERO) routine in the microcode.
;; It is what a CALL instruction does.
;; You must call EH-SAVE-SG-STATE before calling this.
(DEFUN EH-OPEN-CALL-BLOCK (SG DESTINATION FUNCTION &AUX PP NEW-IPMARK)
    (SETQ PP (SG-REGULAR-PDL-POINTER SG))
    (SETQ NEW-IPMARK (+ PP %LP-CALL-BLOCK-LENGTH))
    (EH-REGPDL-PUSH (DPB (- NEW-IPMARK (SG-IPMARK SG)) %%LP-CLS-DELTA-TO-OPEN-BLOCK
                         (DPB (- NEW-IPMARK (SG-AP SG)) %%LP-CLS-DELTA-TO-ACTIVE-BLOCK
                              (DPB DESTINATION %%LP-CLS-DESTINATION 0)))
                    SG)
    (EH-REGPDL-PUSH 0 SG)
    (EH-REGPDL-PUSH 0 SG)
    (EH-REGPDL-PUSH FUNCTION SG)
    (SETF (SG-IPMARK SG) NEW-IPMARK))

;; Running things in the other stack group.

;; Call a function in another stack group and return the value it "returns".
;; Actually, the function should call this stack group back with the "value" as argument.
;; If the value is the symbol LOSE, we throw to EH-QUIT.
;; The call block and args should already be on the regpdl of the other stack group,
;; hence EH-SAVE-SG-STATE should be outside this function.
;; Nothing will automatically make the function know who you are;
;; provide your own stack group as an argument to it if necessary.
;; Before returning, EH-RESTORE-SG-STATE is done since it's generally desired.
(DEFUN EH-RUN-SG (SG &AUX RESULT)
    (%P-STORE-CDR-CODE (AP-1 (SG-REGULAR-PDL SG)	;Terminate arg list assumed there
			     (SG-REGULAR-PDL-POINTER SG))
		       CDR-NIL)
    (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
    (SETQ EH-LAST-SECOND-LEVEL-SG %CURRENT-STACK-GROUP)
    (SETF (SG-FLAGS-MAR-MODE SG) 0)			;Turn off the MAR (why??)
    (FUNCALL SG)
    (SETQ RESULT (CAR %CURRENT-STACK-GROUP-CALLING-ARGS-POINTER))
    (EH-RESTORE-SG-STATE SG)
    (COND (EH-RUNNING
	   (COND ((NEQ %CURRENT-STACK-GROUP EH-LAST-SECOND-LEVEL-SG)
		  (TERPRI)
		  (EH-PRINT-ERROR-MESSAGE SG (SG-TRAP-TAG SG) T)))
	   (SETQ EH-LAST-SECOND-LEVEL-SG %CURRENT-STACK-GROUP)))
    (COND ((EQ RESULT 'LOSE)
	   (*THROW 'EH-QUIT NIL)))
    RESULT)

;; Restart a stack group and mark the error handler stack group as free.
;; This is used for throwing, etc.
(DEFUN EH-RUN-SG-GOODBYE (SG)
    (%P-STORE-CDR-CODE (AP-1 (SG-REGULAR-PDL SG)	;Terminate arg list
			     (SG-REGULAR-PDL-POINTER SG))
		       CDR-NIL)
    (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
    (WITHOUT-INTERRUPTS
      (COND (EH-RUNNING
             (EH-FREE %CURRENT-STACK-GROUP)))
      (FUNCALL SG)))

;;Smash a SG so that it reinvokes its current function with the same args when resumed.
;; For the time being, the function had better not have pushed a micro-stack.
;; Any binding block pushed will not get unwound immediately, but will be
;;  "concatenated."  For the moment, tho, PROCESS-WAIT is the only fctn this should
;; be used on, and these screws shouldnt affect it.
(DEFUN EH-REINVOKE (SG &AUX RP AP NEW-AP NARGS)
 (PROG NIL
  (SETQ RP (SG-REGULAR-PDL SG)
	AP (SG-AP SG)
	NEW-AP (EH-PREVIOUS-ACTIVE SG AP)
	NARGS (LDB %%LP-ENS-NUM-ARGS-SUPPLIED (AR-1 RP (+ AP %LP-ENTRY-STATE))))
;  (COND ((NOT (ZEROP (LDB %%LP-EXS-MICRO-STACK-SAVED (AR-1 RP (+ AP %LP-EXIT-STATE))))) ))
;  (COND ((NOT (ZEROP (LDB %%LP-EXS-BINDING-BLOCK-PUSHED (AR-1 RP (+ AP %LP-EXIT-STATE))))) 
;         ))
 L (COND ((> (SG-REGULAR-PDL-POINTER SG) (+ AP NARGS))  ;Pop any extra stuff beyond args.
	  (EH-REGPDL-POP SG)
	  (GO L)))
   (SETF (SG-IPMARK SG) AP)
   (SETF (SG-AP SG) NEW-AP)
   (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)))
  
;; Mark a second level error handler stack group as available for re-use.
(DEFUN EH-FREE (SG)
    (WITHOUT-INTERRUPTS
     (PUSH SG EH-FREE-SECOND-LEVEL-SG-LIST)
     (AND CURRENT-PROCESS (<- CURRENT-PROCESS ':REVOKE-RUN-REASON SG))))

;; Unwind the stack group until the M-AP is DEST-AP.
;; If GOODBYE-P is T, it returns the specified value from that frame,
;; otherwise it comes back to the EH.
(DEFUN EH-UNWIND-TO-FRAME (SG DEST-AP GOODBYE-P &OPTIONAL VALUE (LABEL T) &AUX N)
    (SETQ N (DO ((AP (SG-AP SG) (EH-PREVIOUS-ACTIVE SG AP))
                 (N 1 (1+ N)))
                ((= AP DEST-AP) N)))
    (EH-UNWIND SG LABEL VALUE N (IF GOODBYE-P NIL %CURRENT-STACK-GROUP)
	       (IF GOODBYE-P 'FREE 'CALL))
    (cond ((null goodbye-p)	;Flush the call back to this SG, and try to get
	   (setf (sg-ap sg)     ; things in phase again.
		 (setq eh-current-frame
		       (setq eh-original-frame (eh-previous-active sg (sg-ap sg)))))
	   (dotimes (i 4)
	     (eh-regpdl-pop sg))))
    )

;; The CONTINUATION is a function called with one argument in the newly-reset
;; stack-group.  ARGUMENT is that argument.
;; If PROCESS-P, rather than doing it now, in this process, we simply
;; leave the stack-group in such a state that the next time it is called,
;; e.g. by the scheduler, it will do it.
(DEFUN EH-UNWIND-SG (SG CONTINUATION ARGUMENT PROCESS-P)
    (LET ((ST (SG-CURRENT-STATE SG)))
      (COND ((NOT (OR (= ST SG-STATE-AWAITING-INITIAL-CALL)
		      (= ST 0)))
	     (EH-UNWIND SG T ARGUMENT NIL CONTINUATION (IF PROCESS-P 'SETUP 'CALL)))
	    (T	;SG has not been run, don't unwind, but do leave in same state
	     (STACK-GROUP-PRESET SG CONTINUATION ARGUMENT)
	     (OR PROCESS-P (FUNCALL SG))))
      (OR PROCESS-P (SETF (SG-CURRENT-STATE SG) SG-STATE-EXHAUSTED))))

;; Eval a form in a specified stack group using a foothold.
(DEFUN EH-EVAL (SG FORM &AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
    (EH-SAVE-SG-STATE SG)
    (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
    (EH-OPEN-CALL-BLOCK SG 0 'EH-FH-EVALER)
    (EH-REGPDL-PUSH FORM SG)
    (EH-REGPDL-PUSH + SG)
    (EH-REGPDL-PUSH * SG)
    (EH-REGPDL-PUSH %CURRENT-STACK-GROUP SG)
    (EH-REGPDL-PUSH EH-RUNNING SG)
    (EH-REGPDL-PUSH PREV-FH SG)
    (EH-RUN-SG SG))

(DEFUN EH-APPLY (SG FUNCTION ARGUMENTS &AUX (PREV-FH (SG-FOOTHOLD-DATA SG)))
    (EH-SAVE-SG-STATE SG)
    (SETF (SG-FOOTHOLD-DATA SG) (SG-AP SG))
    (EH-OPEN-CALL-BLOCK SG 0 'EH-FH-APPLIER)
    (EH-REGPDL-PUSH FUNCTION SG)
    (EH-REGPDL-PUSH ARGUMENTS SG)
    (EH-REGPDL-PUSH + SG)
    (EH-REGPDL-PUSH * SG)
    (EH-REGPDL-PUSH %CURRENT-STACK-GROUP SG)
    (EH-REGPDL-PUSH EH-RUNNING SG)
    (EH-REGPDL-PUSH PREV-FH SG)
    (EH-RUN-SG SG))

(DEFUN EH-THROW (SG LABEL VALUE &OPTIONAL IGNORE)
    (EH-SAVE-SG-STATE SG)
    (EH-OPEN-CALL-BLOCK SG 0 'EH-FH-THROWER)
    (EH-REGPDL-PUSH LABEL SG)
    (EH-REGPDL-PUSH VALUE SG)
    (EH-RUN-SG-GOODBYE SG))

(DEFUN EH-UNWIND (SG LABEL VALUE COUNT ACTION DISPOSAL)
  "DISPOSAL is SETUP just to set up the call, CALL to make the call and not free the EH,
   FREE to make the call and free the EH"
    (EH-SAVE-SG-STATE SG)
    (AND COUNT (SETQ COUNT (1+ COUNT)))  ;Make up for the frame pushed by EH-SAVE-SG-STATE.
    (EH-OPEN-CALL-BLOCK SG 0 'EH-FH-UNWINDER)
    (EH-REGPDL-PUSH LABEL SG)
    (EH-REGPDL-PUSH VALUE SG)
    (EH-REGPDL-PUSH COUNT SG)
    (EH-REGPDL-PUSH ACTION SG)
    (%P-STORE-CDR-CODE (AP-1 (SG-REGULAR-PDL SG)	;Terminate arg list
			     (SG-REGULAR-PDL-POINTER SG))
		       CDR-NIL)
    (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
    (WITHOUT-INTERRUPTS
      (AND EH-RUNNING (EQ DISPOSAL 'FREE)
	   (EH-FREE %CURRENT-STACK-GROUP))
      (OR (EQ DISPOSAL 'SETUP) (FUNCALL SG))))

;; The EH-FH functions are those intended to run in the other stack group.
;; Those that come back should be started up with EH-RUN-SG.
;; They must be given the error handler stack group as an argument
;; so that they can call it back.  This they must do without making any other
;; intervening active call blocks on the stack, so that the foothold data
;; can be found from the SG-AP when it returns.  They must also be given EH-RUNNING as
;; an argument, so that if it is T they can do an unwind protect that
;; does EH-FREE on the stack group that they aren't going to return to in that case.
;; They must also be given the previous foothold's offset so that
;; SG-FOOTHOLD-DATA can be reset in case of a throw.

;; Those that do not come back should be started up with EH-RUN-SG-GOODBYE.

(DEFUN EH-FH-APPLIER (FN ARGS NEW-+ NEW-* SG EH-P PREV-FH)
    (UNWIND-PROTECT
      (LET ((KBD-SIMULATED-CLOCK-FCN-LIST '(TV-BLINKER-CLOCK))
	    (+ NEW-+) (* NEW-*) EVALHOOK)
	(*CATCH 'EH-FOOTHOLD
	   (FUNCALL SG (MULTIPLE-VALUE-LIST (APPLY FN ARGS))))
	;; This is in case the catch catches.
	(FUNCALL SG 'LOSE))
      ;; This is reached only if we throw through this frame.
      (SETF (SG-FOOTHOLD-DATA %CURRENT-STACK-GROUP) PREV-FH)
      (AND EH-P (EH-FREE SG))))

(DEFUN EH-FH-EVALER (FORM NEW-+ NEW-* SG EH-P PREV-FH)
    (UNWIND-PROTECT
      (LET ((KBD-SIMULATED-CLOCK-FCN-LIST '(TV-BLINKER-CLOCK))
	    (+ NEW-+) (* NEW-*) EVALHOOK)
	(*CATCH 'EH-FOOTHOLD
	   (FUNCALL SG (MULTIPLE-VALUE-LIST (EVAL FORM))))
	;; This is in case the catch catches.
	(FUNCALL SG 'LOSE))
      (SETF (SG-FOOTHOLD-DATA %CURRENT-STACK-GROUP) PREV-FH)
      (AND EH-P (EH-FREE SG))))

(DEFUN EH-FH-THROWER (LABEL VALUE)
    (*THROW LABEL VALUE))

(DEFUN EH-FH-UNWINDER (LABEL VALUE COUNT ACTION)
   (*UNWIND-STACK LABEL VALUE COUNT ACTION))

;; Various utility ANALYSIS functions.

;; These functions take an SG and an AP, and return the AP
;; for the previous open or active stack frame.
;Result is NIL if this is the bottom frame
(DEFUN EH-PREVIOUS-OPEN (SG AP)
  (LET ((DELTA (RP-DELTA-TO-OPEN-BLOCK (SG-REGULAR-PDL SG) AP)))
    (COND ((ZEROP DELTA) NIL)
	  (T (- AP DELTA)))))

;Result is NIL if this is the bottom frame
(DEFUN EH-PREVIOUS-ACTIVE (SG AP)
  (LET ((DELTA (RP-DELTA-TO-ACTIVE-BLOCK (SG-REGULAR-PDL SG) AP)))
    (COND ((ZEROP DELTA) NIL)
	  (T (- AP DELTA)))))

;; Returns NIL if there is no next.
(DEFUN EH-NEXT-OPEN (SG AP)
    (DO ((THIS-AP (SG-AP SG) (EH-PREVIOUS-OPEN SG THIS-AP))
	 (NEXT-AP NIL THIS-AP))
	((= THIS-AP AP) NEXT-AP)))

;; Returns NIL if there is no next.
(DEFUN EH-NEXT-ACTIVE (SG AP)
    (DO ((THIS-AP (SG-AP SG) (EH-PREVIOUS-ACTIVE SG THIS-AP))
	 (NEXT-AP NIL THIS-AP))
	((= THIS-AP AP) NEXT-AP)))

;; Scan several active frames up or down from a given one.
;; We return two values; the first is the offset of the frame found,
;; and the second is T if the specified number of frames were found
;; before the top or bottom of the stack.
(DEFUN EH-NEXT-NTH-ACTIVE (SG FRAME &OPTIONAL (COUNT 1))
    (COND ((= COUNT 0) FRAME)
	  ((MINUSP COUNT)
	   (DO ((P FRAME (EH-PREVIOUS-ACTIVE SG P))
		(I 0 (1- I))
		(PP NIL P))
	       (())
	     (AND (OR (NULL P) (= I COUNT))
		  (RETURN (OR P PP) P))))
	  (T (DO ((P FRAME (EH-NEXT-ACTIVE SG P))
		  (I 0 (1+ I))
		  (PP NIL P))
		 (())
	       (AND (OR (NULL P) (= I COUNT))
		    (RETURN (OR P PP) P))))))

(DEFUN EH-FEF-INSTRUCTION (FEF PC)
    (LET ((IDX (// PC 2)))
	 (COND ((ZEROP (LOGAND 1 PC))
		(%P-LDB-OFFSET %%Q-LOW-HALF FEF IDX))
	       ((%P-LDB-OFFSET %%Q-HIGH-HALF FEF IDX)))))

;Takes a functional object, and returns a Lisp object which is its "name".
(DEFUN EH-FUNCTION-NAME (FUNCTION)
    (SELECT (%DATA-TYPE FUNCTION)
       (DTP-FEF-POINTER (FEF-NAME FUNCTION))
       (DTP-U-ENTRY (MICRO-CODE-ENTRY-NAME-AREA (%POINTER FUNCTION)))
       (DTP-LIST (COND ((EQ (CAR FUNCTION) 'NAMED-LAMBDA)
			(IF (ATOM (CADR FUNCTION)) (CADR FUNCTION)
			    (CAADR FUNCTION)))
		       (T FUNCTION)))
       ((DTP-CLOSURE DTP-ENTITY) (EH-FUNCTION-NAME (%MAKE-POINTER DTP-LIST (CAR FUNCTION))))
       (DTP-STACK-GROUP (SG-NAME FUNCTION))
       (DTP-SYMBOL FUNCTION)
       (OTHERWISE FUNCTION)))

;; Scan several active frames up or down from a given one,
;; being smart about calls to interpreted functions.
;; We return two values; the first is the offset of the frame found,
;; and the second is T if the specified number of frames were found
;; before the top or bottom of the stack.
(DEFUN EH-NEXT-NTH-INTERESTING-ACTIVE (SG FRAME &OPTIONAL (COUNT 1))
    (COND ((= COUNT 0) FRAME)
	  ((MINUSP COUNT)
	   (DO ((P FRAME (EH-PREVIOUS-INTERESTING-ACTIVE SG P))
		(I 0 (1- I))
		(PP NIL P))
	       (())
	     (AND (OR (NULL P) (= I COUNT))
		  (RETURN (OR P PP) P))))
	  (T (DO ((P FRAME (EH-NEXT-INTERESTING-ACTIVE SG P))
		  (I 0 (1+ I))
		  (PP NIL P))
		 (())
	       (AND (OR (NULL P) (= I COUNT))
		    (RETURN (OR P PP) P))))))

;; Return the next frame, counting all the actual frames of parts of an
;; interpreted function as if they were one frame.
(DEFUN EH-NEXT-INTERESTING-ACTIVE (SG AP &AUX (RP (SG-REGULAR-PDL SG)))
    (COND ((ATOM (RP-FUNCTION-WORD RP AP))
	   (EH-NEXT-ACTIVE SG AP))
	  (T (DO ((NEW-AP (EH-NEXT-ACTIVE SG AP) (EH-NEXT-ACTIVE SG NEW-AP)))
		 ((OR (NULL NEW-AP)
		      (NOT (MEMQ (EH-FUNCTION-NAME (RP-FUNCTION-WORD RP NEW-AP))
				 EH-UNINTERESTING-FUNCTIONS)))
		  NEW-AP)))))

(DEFUN EH-PREVIOUS-INTERESTING-ACTIVE (SG AP)
    (EH-OUT-TO-INTERESTING-ACTIVE SG (EH-PREVIOUS-ACTIVE SG AP)))

;; Given a frame, find out if it is one of the frames of a call to an interpreted function.
;; If so, return the outermost frame of this call to the interpreted function.
;; If not, return the original frame.
(DEFUN EH-OUT-TO-INTERESTING-ACTIVE (SG AP &AUX (RP (SG-REGULAR-PDL SG)))
    (COND ((NULL AP) NIL)
	  ((NOT (MEMQ (EH-FUNCTION-NAME (RP-FUNCTION-WORD RP AP)) EH-UNINTERESTING-FUNCTIONS))
	   AP)
	  (T (DO ((NEW-AP AP (EH-PREVIOUS-ACTIVE SG NEW-AP)))
		 ((OR (NULL NEW-AP)
		      (NOT (MEMQ (EH-FUNCTION-NAME (RP-FUNCTION-WORD RP NEW-AP))
				 EH-UNINTERESTING-FUNCTIONS)))
		  (COND ((NULL NEW-AP) AP)
			((ATOM (RP-FUNCTION-WORD RP NEW-AP)) AP)
			(T NEW-AP)))))))

;; Assuming that AP points to the outermost actual frame
;; of a call to an interpreted function, return its innermost active *EVAL frame.
(DEFUN EH-INNERMOST-UNINTERESTING-ACTIVE-EVAL (SG AP &AUX (RP (SG-REGULAR-PDL SG)))
    (DO ((NEW-AP AP FOLLOWING-AP)
	 (FOLLOWING-AP) (LAST-EVAL-AP))
	(())
      (AND (EQ (RP-FUNCTION-WORD RP NEW-AP) (FUNCTION *EVAL))
	   (SETQ LAST-EVAL-AP NEW-AP))
      (SETQ FOLLOWING-AP (EH-NEXT-ACTIVE SG NEW-AP))
      (OR (AND FOLLOWING-AP
	       (MEMQ (EH-FUNCTION-NAME (RP-FUNCTION-WORD RP FOLLOWING-AP))
		     EH-UNINTERESTING-FUNCTIONS))
	  (RETURN LAST-EVAL-AP))))

;; Return a name for the "function" to tell the user about
;; corresponding to the macro instruction in which an error happened.
(DEFUN EH-ERRING-FUNCTION (SG)
    (LET ((AP (SG-AP SG))
	  (RP (SG-REGULAR-PDL SG)))
      (LET ((FUNCTION (RP-FUNCTION-WORD RP AP))
	    (PC (1- (RP-EXIT-PC RP AP))))
	(SELECT (%DATA-TYPE FUNCTION)
	   (DTP-U-ENTRY
	    (MICRO-CODE-ENTRY-NAME-AREA (%POINTER FUNCTION)))
	   (DTP-FEF-POINTER 
	    (LET ((INST (EH-FEF-INSTRUCTION FUNCTION PC)))
	      (LET ((OP (LDB 1104 INST))
		    (DEST (LDB 1503 INST))
		    (DISP (LDB 0011 INST)))
		(COND ((< OP 11)
		       (NTH OP '(FUNCALL FUNCALL MOVE-INSTRUCTION CAR
				 CDR CADR CDDR CDAR CAAR)))
		      ((= OP 11)
		       (NTH DEST '(ND1-UNUSED *PLUS *DIF *TIMES ;*'s to avoid confusion with
				   *QUO *LOGAND *LOGXOR *LOGIOR))) ;argument-number
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
(DEFUN EH-LOCAL-NAME (FUNCTION LOCALNO &AUX ARGL)
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
(DEFUN EH-ARG-NAME (FUNCTION ARGNO &AUX ARGL)
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
(DEFUN EH-REST-ARG-VALUE (SG FRAME &AUX
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
					    (AP-1 RP (+ AP NARGS-EXPECTED 1))))
			    (T NIL)))
		     (T (AREF RP (+ AP (RP-LOCAL-BLOCK-ORIGIN RP AP)))))
	       REST-ARG
	       LEXPR-CALL)))

;; Return the number of spread args present in a given frame.
;; This will not count any args which are part of a rest arg.
(DEFUN EH-NUMBER-OF-SPREAD-ARGS (SG FRAME &AUX
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
(DEFUN EH-ARG-VALUE (SG FRAME ARGNO &AUX (RP (SG-REGULAR-PDL SG)) (AP FRAME))
    (PROG ()
	  (OR (MINUSP ARGNO) ( ARGNO (EH-NUMBER-OF-SPREAD-ARGS SG FRAME))
	      (RETURN (AREF RP (+ AP ARGNO 1)) T))))

;; These functions know about the location tags used in the ERROR-TABLE
;; entries, and how to creates locatives to them, fetch from them,
;; and store into them.
;;   There is the issue that the contents may be illegal datatypes.
;; Have to think about if there are screw cases, etc.

; Analysis
(DEFUN EH-CONTENTS (LOC SG)
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
	     (PP (AR-1 (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
	     (RMD (%P-CONTENTS-OFFSET (SG-SAVED-VMA SG) 0))
	     (OTHERWISE
	        (COND ((AND (LISTP LOC) (EQ (CAR LOC) 'PP))
		       (AR-1 (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
		      ((EH-BAD-HACKER LOC "Unknown tag"))))))

;; Metamorphosis
(DEFUN EH-STORE (X LOC SG)
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
	     (PP (AS-1 X (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
	     (RMD (%P-STORE-CONTENTS (SG-SAVED-VMA SG) X))	;Offset???
	     (OTHERWISE
		(COND ((AND (LISTP LOC) (EQ (CAR LOC) 'PP))
		       (AS-1 X (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
		      ((EH-BAD-HACKER LOC "Unknown tag"))))))

;; Getllocativepointersis
(DEFUN EH-LOCATE (LOC SG)
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
	     (PP (AP-1 (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG)))
	     (RMD (%MAKE-POINTER DTP-LOCATIVE (SG-SAVED-VMA SG)))
	     (OTHERWISE
		(COND ((AND (LISTP LOC) (EQ (CAR LOC) 'PP))
		       (AP-1 (SG-REGULAR-PDL SG) (+ (SG-REGULAR-PDL-POINTER SG) (CADR LOC))))
		      ((EH-BAD-HACKER LOC "Unknown tag"))))))

;; Various initialization routines.

(DEFUN EH-ASSURE-TABLE-LOADED ()
    (COND ((NOT (AND (BOUNDP 'MICROCODE-ERROR-TABLE-VERSION-NUMBER)
		     (= MICROCODE-ERROR-TABLE-VERSION-NUMBER %MICROCODE-VERSION-NUMBER)))
	   (EH-LOAD-ERROR-TABLE)
	   (OR (AND (BOUNDP 'MICROCODE-ERROR-TABLE-VERSION-NUMBER)
		     (= MICROCODE-ERROR-TABLE-VERSION-NUMBER %MICROCODE-VERSION-NUMBER))
	       (BREAK 'CANNOT-GET-ERROR-TABLE T)))))

(DEFUN EH-LOAD-ERROR-TABLE (&AUX (FUDGED-USER-ID-FLAG NIL))
  (COND ((OR (NULL USER-ID) (STRING-EQUAL USER-ID ""))
	 (SETQ FUDGED-USER-ID-FLAG T)
	 (LOGIN "ERROR" T)))
  (LOAD (FORMAT NIL "DSK:LISPM1;UCADR ~DTBL" %MICROCODE-VERSION-NUMBER)
	"SYSTEM-INTERNALS")
  (AND FUDGED-USER-ID-FLAG (LOGOUT)))

;; Divides up MICROCODE-ERROR-TABLE into EH-CALLS-SUB-LIST, EH-RESTART-LIST,
;; and EH-ERROR-TABLE.
(DEFUN EH-ASSURE-TABLE-PROCESSED ()
    (COND ((NOT (= MICROCODE-ERROR-TABLE-VERSION-NUMBER EH-ERROR-TABLE-NUMBER))
	   (SETQ EH-ERROR-TABLE NIL
		 EH-CALLS-SUB-LIST NIL
		 EH-RESTART-LIST NIL)
	   (DO ET MICROCODE-ERROR-TABLE (CDR ET) (NULL ET)
	     (SELECTQ (CADAR ET)
		      (RESTART (PUSH (CONS (CADDAR ET) (1+ (CAAR ET))) EH-RESTART-LIST))
		      (CALLS-SUB (PUSH (CONS (CAAR ET) (CADDAR ET)) EH-CALLS-SUB-LIST))
		      (OTHERWISE (PUSH (CAR ET) EH-ERROR-TABLE))))
	   (SETQ EH-ERROR-TABLE-NUMBER MICROCODE-ERROR-TABLE-VERSION-NUMBER)
	   )))

(DEFUN EH-ASSURE-DISPATCH-SET-UP ()
    (COND ((NOT (BOUNDP 'EH-DISPATCH))
	   (SETQ EH-DISPATCH (MAKE-ARRAY NIL 'ART-Q '(4 220)))
	   (SETUP-KEYBOARD-DISPATCH-TABLE EH-DISPATCH EH-DISPATCH-LIST))))

;; Call this when it is apparent that some hacker set things up wrong.
(DEFUN EH-BAD-HACKER (&REST ARGS)
    (FORMAT T "~%~%Foo, a hacker has screwn up somewhere.  Error:~%")
    (DO AL ARGS (CDR AL) (NULL AL) (PRINC (CAR AL)) (TYO #\SP))
    (TERPRI) (TERPRI))

;; Turn on error trapping mode.
(DEFUN ENABLE-TRAPPING (&OPTIONAL (X 1))
  (SETQ %MODE-FLAGS (DPB X %%M-FLAGS-TRAP-ENABLE %MODE-FLAGS)))

(DEFUN TRAPPING-ENABLED-P NIL 
  (NOT (ZEROP (LDB %%M-FLAGS-TRAP-ENABLE %MODE-FLAGS))))

(DEFUN EH-P-PRIN1 (LOCATIVE &AUX)
    (LET ((DTP (Q-DATA-TYPES (%P-DATA-TYPE LOCATIVE))))
      (COND ((MEMQ DTP EH-GOOD-TYPES)
	     (OR (LISTP (ERRSET (PRIN1 (CAR LOCATIVE)) NIL))
		 (PRINC "<<Error in printing!>>")))
	    (T (FORMAT T "#<~A ~O>" DTP (%P-POINTER LOCATIVE))))))

;; Initialize the error handler at warm boot time.
(ADD-INITIALIZATION "EH-INITIALIZE" '(EH-INITIALIZE) '(WARM))

(DEFUN EH-INITIALIZE ()
  (SETQ EH-RUNNING NIL)
  (SETQ EH-ERRSET-STATUS NIL)		;Set to T if an errset exists and should be obeyed
  (EH-ASSURE-TABLE-LOADED)		;Gets the right UCONS/UCADR TABLE file loaded.
  (EH-ASSURE-TABLE-PROCESSED)		;Processes the contents of UCONS/UCADR TABLE.
;;; Waiting until the first error to do these things loses
;;; because they let other processes run, which could get errors and crash the machine.
  )

;; This is the function that runs in the first level error handler
;; It is called only at boot time.  From then on it just keeps coroutining.
(DEFUN LISP-ERROR-HANDLER (&AUX M SG SG2 CONDITION
                                (INHIBIT-SCHEDULING-FLAG T))
    ;; Return to boot code.  We are called back by the first error.
    (SETQ M (FUNCALL %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP NIL))
    (DO ((ERRSET-FLAG NIL NIL)	;These must be reinitialized each time through the loop!
	 (ERRSET-PRINT-MSG NIL NIL))
	(NIL)			;Do forever, once for each error
      (SETQ SG %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)
      (EH-ASSURE-DISPATCH-SET-UP)		;Set up command dispatch table.
      ;; Compute and store the ETE for this error.
      (SETF (SG-TRAP-TAG SG)
	    (COND (M M)
		  (T (CDR (ASSQ (SG-TRAP-MICRO-PC SG) EH-ERROR-TABLE)))))
      (SETF (SG-PROCESSING-ERROR-FLAG SG) 0) ;Re-enable error trapping in that SG
      (SETQ CONDITION (GET (CAR (SG-TRAP-TAG SG)) 'EH-SIGNAL))
      (AND CONDITION (SETQ CONDITION (FUNCALL CONDITION SG (SG-TRAP-TAG SG))))
      ;; All branches of this COND must end in funcalling some other SG.
      (SETQ M
	    (COND ((AND (NOT (SYMEVAL-IN-STACK-GROUP 'ERRSET SG))
			(SETQ ERRSET-FLAG (SYMEVAL-IN-STACK-GROUP 'EH-ERRSET-STATUS SG))
			(NOT (SETQ ERRSET-PRINT-MSG
                                   (SYMEVAL-IN-STACK-GROUP 'EH-ERRSET-PRINT-MSG SG)))
                        (NOT (AND CONDITION (EH-CONDITION-HANDLED-P SG (CAR CONDITION)))))
		   ;; If we are in an errset, and don't want the message, throw now.
		   (EH-THROW SG 'ERRSET-CATCH NIL T))
		  (T
		   ;; Otherwise, obtain a second level error handler sg
		   ;; and tell it what to work on.
                   (SETQ SG2 (OR (POP EH-FREE-SECOND-LEVEL-SG-LIST)
				 (MAKE-STACK-GROUP
                                     (STRING-APPEND "SECOND-LEVEL-ERROR-HANDLER-"
                                                    (FORMAT NIL "~D"
                                                            (SETQ EH-SECOND-LEVEL-COUNT
                                                                  (1+ EH-SECOND-LEVEL-COUNT))))
                                     ':SAFE 0
				     ':SPECIAL-PDL-SIZE 1000)))  ;Otherwise ZDT loses.
		   (STACK-GROUP-PRESET SG2 'EH-SECOND-LEVEL
                                       SG M ERRSET-FLAG ERRSET-PRINT-MSG CONDITION)
		   (FUNCALL SG2))))))

;; Invoke the error handler to look at a particular stack group.
;; A window or process may also be supplied, and a stack group found from it.
;; Supplying NIL means find a process which is waiting to be looked at.
;; If a process is supplied or known, it is arrested while we are invoked.
;; This works differently from real errors; it just runs the error handler
;; in the same stack group and process that EH is called in;  EH-RUNNING is NOT set.
;; The catch tag EH-EXIT is used to return from EH.
(DEFUN EH (&OPTIONAL PROCESS &AUX PKG SG ARREST-REASON
	   EH-ORIGINAL-FRAME EH-CURRENT-FRAME
	   EH-CONDITION-PROCEED-VALUE EH-CONDITION-PROCEED-FLAG)
  (COND ((NULL PROCESS)
	 (DOLIST (P ACTIVE-PROCESSES)
	   (COND ((STRING-EQUAL (PROCESS-WHOSTATE (CAR P)) "Error")
		  (FORMAT T "~&Selecting ~S~%" (CAR P))
		  (<- (CAR P) ':SELECT)
		  (PROCESS-WAIT "Select" #'(LAMBDA () (EQ CURRENT-PROCESS SELECTED-PROCESS)))
		  (RETURN NIL)))))
	(T
	 ;; If arg is a window or stream, extract process from it.
	 (OR (MEMQ (TYPEP PROCESS) '(ARRAY PROCESS))
	     (SETQ PROCESS (FUNCALL PROCESS ':PROCESS)))
	 ;; If arg is process or was converted to one, stop it.
	 (COND ((EQ (TYPEP PROCESS) 'PROCESS)
		(<- PROCESS ':ARREST-REASON CURRENT-PROCESS)
		(SETQ ARREST-REASON CURRENT-PROCESS)
		(SETQ SG (PROCESS-STACK-GROUP PROCESS)))
	       (T (SETQ SG PROCESS PROCESS NIL)))
	 (SETQ EH-ORIGINAL-FRAME (SG-AP SG))
	 (SETQ EH-CURRENT-FRAME (EH-OUT-TO-INTERESTING-ACTIVE SG EH-ORIGINAL-FRAME))
	 ;; Although we get the package each time around the r-e-p loop, we must get it
	 ;; here as well, so that when the error message is printed it will be in the
	 ;; right package.
	 (SETQ PKG (SYMEVAL-IN-STACK-GROUP 'PACKAGE SG))	;Use same package as at point of error
	 (UNWIND-PROTECT
	   (PROGN
	     (*CATCH 'EH-QUIT
	       (*CATCH 'TOP-LEVEL
		 (PKG-BIND (IF (EQ (TYPEP PKG) 'PACKAGE) PKG	;If it looks reasonably trustworthy
			       "USER")
			   (OR (LISTP
				(ERRSET (PROGN
					 (FORMAT T "~&~S  Backtrace: " SG)
					 (EH-SHORT-BACKTRACE SG NIL 3)
					 (EH-SHOW-FUNCTION-AND-ARGS SG))))
			       (PRINC "<<Error printing frame!>>")))))
	     (*CATCH 'EH-EXIT (EH-COMMAND-LOOP SG (SG-TRAP-TAG SG))))
	   (AND ARREST-REASON (<- PROCESS ':REVOKE-ARREST-REASON ARREST-REASON))))))

;; This is the function at the top level in each second level error handler sg.
(DEFUN EH-SECOND-LEVEL (SG M ERRSET-FLAG ERRSET-PRINT-MSG CONDITION
                           &AUX PKG (ERRSET NIL) (EH-RUNNING T) (ETE (SG-TRAP-TAG SG))
			   ;; Prevent blowup if PACKAGE is bad.
			   (PACKAGE PKG-USER-PACKAGE)
			   BREAK-FLAG
			   (TERMINAL-IO (OR ERROR-HANDLER-IO
					    (SYMEVAL-IN-STACK-GROUP 'TERMINAL-IO SG)))
			   (STANDARD-INPUT SYN-TERMINAL-IO)
			   (STANDARD-OUTPUT SYN-TERMINAL-IO)
			   (QUERY-IO SYN-TERMINAL-IO)
			   ;; In case we want to set CURRENT-PROCESS to nil.
			   (CURRENT-PROCESS CURRENT-PROCESS)
			   ;; And some things will wonder what it had been.
			   (EH-REAL-CURRENT-PROCESS CURRENT-PROCESS)
			   EH-ORIGINAL-FRAME EH-CURRENT-FRAME
                           EH-CONDITION-PROCEED-VALUE EH-CONDITION-PROCEED-FLAG)
    ;; If error was in the scheduler, set CURRENT-PROCESS to nil
    ;; so that KBD-TYI won't wait for selection, etc.
    (COND ((EQ SG SCHEDULER-STACK-GROUP)
	   (SETQ CURRENT-PROCESS NIL)))
    (OR (NULL CURRENT-PROCESS)    ;If not running in the scheduler, give us a run reason.
				  ; in case we died after becoming inactive, before
				  ; getting back to the scheduler.
	(<- CURRENT-PROCESS ':RUN-REASON %CURRENT-STACK-GROUP))
    ;; Try to see if TERMINAL-IO is reasonable and if not fix it.
    (COND ((NULL (ERRSET (FUNCALL TERMINAL-IO ':WHICH-OPERATIONS) NIL))
	   (SETQ TERMINAL-IO (TV-MAKE-STREAM CONSOLE-IO-PC-PPR))
	   (SETF (PC-PPR-OUTPUT-HOLD-FLAG CONSOLE-IO-PC-PPR) 0)
	   (TV-HOME CONSOLE-IO-PC-PPR)
	   (TV-CLEAR-EOL CONSOLE-IO-PC-PPR)
	   (FORMAT T " ---> TERMINAL-IO clobbered, using CONSOLE-IO-PC-PPR <---~%")))
    (COND ((EQ (CAR M) ':BREAK)
	   (EH-RESTORE-SG-STATE SG)  ;Restore state saved at FORCE-BREAK
	   (COND ((CADR M)	     ;If re-invoke, do so.
		  (EH-REINVOKE SG)))))
    (*CATCH 'EH-QUIT
      (*CATCH 'TOP-LEVEL
        (PROGN
          ;; If we have a condition to signal, do so (in the debugged stack group)
	  ;; and maybe return or restart if it says so.
	  (AND CONDITION
	       (LET ((TEM1 (EH-APPLY SG #'SIGNAL CONDITION))
		     (EH-CONDITION-PROCEED-FLAG T)
		     EH-CONDITION-PROCEED-VALUE)
		   (COND ((EQ (CAR TEM1) 'RETURN)
                          (SETQ EH-CONDITION-PROCEED-VALUE (CADR TEM1))
			  (EH-PROCEED SG ETE))
			 ((EQ (CAR TEM1) 'ERROR-RESTART)
			  (EH-ERROR-RESTART SG ETE)))))
	  ;; If non-printing errset, throw to it once condition is processed.
          (AND ERRSET-FLAG (NOT ERRSET-PRINT-MSG)
               (EH-THROW SG 'ERRSET-CATCH NIL))
	  ;; Otherwise, decide whether to break or to go to top level.
	  (SETQ BREAK-FLAG (OR (NOT (FBOUNDP 'EH-BREAK-P)) (EH-BREAK-P SG ETE CONDITION)))
          (SETQ EH-ORIGINAL-FRAME (SG-AP SG))
          (SETQ EH-CURRENT-FRAME
		(EH-NEXT-NTH-ACTIVE SG EH-ORIGINAL-FRAME
				    (- (OR (GET (CAR M) 'EH-BACKTRACE-SKIP) 0))))
	  (SETQ EH-CURRENT-FRAME (EH-OUT-TO-INTERESTING-ACTIVE SG EH-CURRENT-FRAME))
          ;; Although we get the package each time around the r-e-p loop, we must get it
          ;; here as well, so that when the error message is printed it will be in the
          ;; right package.
          (SETQ PKG (SYMEVAL-IN-STACK-GROUP 'PACKAGE SG)) ;Use same package as at point of error
          (PKG-BIND (IF (EQ (TYPEP PKG) 'PACKAGE) PKG    ;If it looks reasonably trustworthy
                        PKG-USER-PACKAGE)
                    (OR (LISTP
			 (ERRSET (PROGN (COND ((EQ (CAR M) ':BREAK)
					       (TERPRI)
					       (PRINC ">>BREAK")
					       (TERPRI))
					      ((OR ERRSET-FLAG (NULL BREAK-FLAG))
					       (TERPRI)
					       (EH-PRINT-ERROR-MESSAGE SG ETE T))
					      (T (EH-SHOW SG ETE))))
				 NIL))
			(PRINC "<<Error printing error message!>>")))
        (AND ERRSET-FLAG (NOT (EQ (CAR M) ':BREAK)) 
             (EH-THROW SG 'ERRSET-CATCH NIL))
	;; If this error isn't interesting to break on,
	;; return to top level, or to innermost error break loop.
	(OR BREAK-FLAG (EH-THROW-ONE-ERROR SG ETE)))))
    (SETQ EH-LAST-SECOND-LEVEL-SG %CURRENT-STACK-GROUP)
    (EH-COMMAND-LOOP SG ETE))  ;SG-TRAP-TAG is part of the state restored by 
			       ;EH-RESTORE-SG-STATE in case of BREAK.  Thus, it does
			       ;not win to have EH-COMMAND-LOOP refetch it.

;; Decide whether an error break loop is useful for this error.
(DEFUN EH-BREAK-P (SG IGNORE CONDITION)
    (OR (NULL (SG-FOOTHOLD-DATA SG))
	(NOT (SELECTQ (CAR CONDITION)
	       (:WRONG-NUMBER-OF-ARGUMENTS
		 (EQ (LET ((AP (EH-PREVIOUS-ACTIVE SG (SG-AP SG)))
			   (RP (SG-REGULAR-PDL SG)))
		       (RP-FUNCTION-WORD RP AP))
		     (FUNCTION *EVAL)))
	       ((:UNDEFINED-VARIABLE :UNDEFINED-FUNCTION)
		(EQ (LET ((AP (SG-AP SG))
			  (RP (SG-REGULAR-PDL SG)))
		      (RP-FUNCTION-WORD RP AP))
		    (FUNCTION *EVAL)))))))

(DEFUN EH-COMMAND-LOOP (EH-SG ETE &AUX CHAR FUNCTION SEXP 
			   (EVALHOOK NIL) PKG
			   (BASE 8) (IBASE 8) (*NOPOINT NIL) (PACKAGE PACKAGE)
			   (KBD-TYI-HOOK 'EH-KBD-TYI-HOOK)
			   ;; If you know why this is done, make this comment say why
			   (KBD-SIMULATED-CLOCK-FCN-LIST '(TV-BLINKER-CLOCK)))
    ;; If it was the mouse process that got the error, the user isn't
    ;; likely to be able to select this process, so just grab the keyboard
    ;; so as to have a chance of debugging.  Also if an error occurs while
    ;; initializing the system, grab the keyboard since normal mechanisms
    ;; such as the mouse may be broken.
    (COND ((OR (NOT (BOUNDP 'MOUSE-PROCESS))	;Error during system booting
	       (EQ CURRENT-PROCESS MOUSE-PROCESS)
	       SYSTEM-BEING-INITIALIZED-FLAG)
	   (FORMAT T "~%---> Changing SELECTED-PROCESS from ~S to ~S <---~%"
		     SELECTED-PROCESS CURRENT-PROCESS)
	   (SETQ SELECTED-PROCESS CURRENT-PROCESS)))
    ;; Discard type-ahead - unless we are going to wait for the user to say (EH).
    (AND (OR (NULL CURRENT-PROCESS)
	     (EQ CURRENT-PROCESS SELECTED-PROCESS))
	 (FUNCALL STANDARD-INPUT ':CLEAR-INPUT))
    (DO ((NUMERIC-ARG)
	 (-)
	 (+ (SYMEVAL-IN-STACK-GROUP '- EH-SG))
	 (* (SYMEVAL-IN-STACK-GROUP '* EH-SG)))
        (())
      (SETQ PKG (SYMEVAL-IN-STACK-GROUP 'PACKAGE EH-SG)) ;Get package again in case user
      (SETQ PACKAGE (IF (EQ (TYPEP PKG) 'PACKAGE) PKG    ; does a PKG-GOTO
                        OKG-USER-PACKAGE))
      ;; Wait until we are selected, in a way that shows up in PEEK.
      (COND ((AND CURRENT-PROCESS
		  (NEQ CURRENT-PROCESS SELECTED-PROCESS))
	     (PROCESS-WAIT "Error" #'(LAMBDA () (EQ CURRENT-PROCESS SELECTED-PROCESS)))
	     (OR (LISTP
		   (ERRSET (PROGN (EH-SHOW EH-SG ETE) NIL)))
		 (PRINC "<<Error printing error message!>>"))))
      (*CATCH 'TOP-LEVEL
        (*CATCH 'EH-QUIT
	 (PROGN
	  (OR NUMERIC-ARG
	      (FORMAT T "~&"))
	  ;; Read the next control character or sexp, with combined rubout processing.
	  (MULTIPLE-VALUE (CHAR SEXP)
	    (EH-COMMAND-LOOP-READ))
	  ;; If it's a control character, look it up.
	  (SETQ FUNCTION (AND CHAR (EH-COMMAND-LOOKUP CHAR)))
	  ;; If it's a character, execute the definition or complain.
	  (COND ((EQ FUNCTION 'EH-NUMBER-COMMAND)
		 (SETQ NUMERIC-ARG
		      (LET ((DIGIT (- (LDB %%KBD-CHAR CHAR) #/0)))
			(COND ((NULL NUMERIC-ARG) DIGIT)
			      (T (+ DIGIT (* 10. NUMERIC-ARG)))))))
		(FUNCTION
		 (COND (NUMERIC-ARG
			 (FUNCALL FUNCTION EH-SG ETE NUMERIC-ARG))
		       (T (FUNCALL FUNCTION EH-SG ETE)))
		 (SETQ NUMERIC-ARG NIL))
		(CHAR (PRINC "??"))
		;; If there was no char, there was a sexp, so eval it.
		(T
		 (LET ((RESULTS (EH-EVAL EH-SG (SETQ - SEXP))))
		   (SETQ + -)
		   (COND ((NEQ RESULTS EH-ERROR-FLAG)
			  (SETQ * (CAR RESULTS))
			  (MAPC 'PRINT RESULTS))))))
	  )))))

;; This is what KBD-TYI should do with all control characters read by anyone
;; inside the error handler.  BREAK breaks, C-G throws to the eh top level,
;; C-Z is not special (unlike the usual situation).
(DEFUN EH-KBD-TYI-HOOK (CH)
  (AND (OR (= CH (+ 400 #/G)) (= CH (+ 400 #/g)))
       (PRINC "G") (THROW NIL TOP-LEVEL))
  (COND ((= CH 201) (BREAK BREAK T) NIL)
	(T CH)))

;; Read from STANDARD-INPUT either a control-character (or ? or Help)
;; or a s-expression.  Return CHAR or return NIL and the s-expression.
(DEFUN EH-COMMAND-LOOP-READ ()
  (PROG (CHAR SEXP FLAG)
   RETRY
    ;; Read a character.
    (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI))
    ;; Now, if the char is special, echo and return it.
    (COND ((OR (LDB-TEST %%KBD-CONTROL-META CHAR)
	       (= CHAR #\HELP)
	       (= CHAR #/?))
	   (FORMAT T "~C" CHAR)
	   (RETURN CHAR))
	  ((= CHAR #\RUBOUT) (GO RETRY)))	;Ignore rubouts
    ;; Otherwise, unread it and read an s-exp instead.
    (FUNCALL STANDARD-INPUT ':UNTYI CHAR)
    (COND ((AND (NOT RUBOUT-HANDLER)
		(MEMQ ':RUBOUT-HANDLER (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS)))
	   (MULTIPLE-VALUE (SEXP FLAG)
	     (FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '((:FULL-RUBOUT :FULL-RUBOUT))
		      #'READ-FOR-TOP-LEVEL))
	   (AND (EQ FLAG ':FULL-RUBOUT) (GO RETRY))
	   (RETURN NIL SEXP))
	  ;; If stream has no rubout handler, degrade gracefully.
	  ((RETURN NIL (READ-FOR-TOP-LEVEL))))))

(DEFUN EH-COMMAND-LOOKUP (CHAR &AUX TEM)
    (SETQ TEM (AR-2 EH-DISPATCH (LDB %%KBD-CONTROL-META CHAR) (LDB %%KBD-CHAR CHAR)))
    (DO ()
	((NLISTP TEM) TEM)
      (SETQ TEM (AR-2 EH-DISPATCH (FIRST TEM) (SECOND TEM)))))

;CONDITION-HANDLERS is a list of handling specs, each of which
;contains first either a condition name, a list of such names, or
;NIL meaning all condition names, and second
;a function to call to handle the condition.
;When you signal a condition with (SIGNAL condition-name info info info...)
;condition-handlers is searched for an element that applies to this
;condition name, and that element's function is called
;with the same arguments that signal was given (however many there were).
;If the function's first value is NIL, this means that the condition
;has not been handled, and the remaining handlers on the list should
;be given the chance to look at it.  Otherwise, the function's one
;or two values are returned by SIGNAL.

(declare (special condition-handlers))
(setq condition-handlers nil)

(defun signal (&rest args)
       (multiple-value-call (signal-1 condition-handlers args)))

(defun signal-1 (handler-list condition-list &aux (cname (car condition-list)) tem1 tem2)
    (do ((handler-list handler-list (cdr handler-list))
	 (h))
	((null handler-list) nil)
	(setq h (car handler-list))
	(cond ((cond ((null (car h)) T)
		     ((nlistp (car h))
		      (eq (car h) cname))
		     (T (memq cname (car h))))
	       (multiple-value (tem1 tem2)
			       (apply (cadr h) condition-list))
	       (and tem1 (return tem1 tem2))))))

(defmacro-displace condition-bind (handlers . body)
    `(let ((condition-handlers (append ',handlers condition-handlers)))
	  . ,body))

;; Does a stack group have anything that could try to handle this condition?
(DEFUN EH-CONDITION-HANDLED-P (SG CONDITION)
    (DOLIST (H (SYMEVAL-IN-STACK-GROUP 'CONDITION-HANDLERS SG))
      (AND (COND ((NULL (CAR H)) T)
                 ((NLISTP (CAR H)) (EQ (CAR H) CONDITION))
                 (T (MEMQ CONDITION (CAR H))))
           (RETURN T))))

;; Utility function used by the top level, and various commands.

;; Print out the error message of the error.
(DEFUN EH-PRINT-ERROR-MESSAGE (SG ETE &OPTIONAL BRIEF-FLAG
			       &AUX (PRINLEVEL EH-PRINLEVEL)
			            (PRINLENGTH EH-PRINLENGTH)
				    INFORM)
    (EH-PRINT-DEBUGGING-ERROR-MESSAGE SG ETE)		;For debugging only.
    (SETQ INFORM (GET (CAR ETE) 'EH-INFORM))
    (COND ((NULL INFORM)
	   (FORMAT T "There is no error message provided for this error!~%"))
	  (T (FUNCALL INFORM SG ETE)))
    (COND ((NOT BRIEF-FLAG)
	   (FORMAT T "~&While in the function ")
	   (EH-SHORT-BACKTRACE SG 
			       NIL
			       (OR (GET (CAR ETE) 'EH-BACKTRACE-LENGTH)
				   EH-ERROR-MESSAGE-BACKTRACE-LENGTH)
			       (OR (GET (CAR ETE) 'EH-BACKTRACE-SKIP)
				   0))
	   (TERPRI)
	   (AND ;Check for user message hook
	    (SETQ INFORM (SYMEVAL-IN-STACK-GROUP 'ERROR-MESSAGE-HOOK SG))
	    (FUNCALL INFORM))))		;Call user function to explain
	   (FORMAT T "~&"))		;Compensate if it forgot to CRLF

(DEFUN EH-PRINT-DEBUGGING-ERROR-MESSAGE (SG ETE &AUX TEM FLAG)
    (COND ((NEQ (CAR ETE) 'FERROR)
	   (FORMAT T ">>TRAP ~A ~A"
			  (SG-TRAP-MICRO-PC SG) ETE)
           (COND ;; Process the micro-stack of the active frame, if any
		 ((LDB-TEST %%LP-EXS-MICRO-STACK-SAVED
			    (RP-EXIT-WORD (SG-REGULAR-PDL SG) (SG-AP SG)))
		  (DO I (SG-SPECIAL-PDL-POINTER SG) (1- I) NIL
		    (LET ((PC (AREF (SG-SPECIAL-PDL SG) I)))
		      (SETQ TEM (ASSQ (1- (%POINTER PC)) EH-CALLS-SUB-LIST))
		      (COND (TEM
			      (OR FLAG (FORMAT T " ->"))
			      (SETQ FLAG T)
			      (FORMAT T "  ~A " (CDR TEM)))))
		    (OR (ZEROP (%P-FLAG-BIT (ALOC (SG-SPECIAL-PDL SG) I)))
			(RETURN NIL)))))
	   (TERPRI))
          (T (FORMAT T ">>ERROR: "))))

(DEFUN EH-CONSTANT-FORM-P (X)
    (COND ((SYMBOLP X)
	   (OR (EQ X 'T) (NULL X)))
	  ((LISTP X) (EQ (CAR X) 'QUOTE))
	  (T T)))

;; This is the function used by proceed routines to ask for
;; a form to read and eval.  When proceeding is done under control
;; of a condition handler, this function arranges to get the
;; object supplied by the handler instead of asking.
(DEFUN EH-GET-OBJECT (PROMPT &AUX FORM OBJECT)
    (COND ((EQ EH-CONDITION-PROCEED-FLAG T)
           (SETQ EH-CONDITION-PROCEED-FLAG 'GOBBLED)
           EH-CONDITION-PROCEED-VALUE)
          ((EQ EH-CONDITION-PROCEED-FLAG 'GOBBLED)
           (FERROR NIL "EH-GET-OBJECT called twice by proceed routine for ~S"
		   (CAR (SG-TRAP-TAG EH-SG))))
          (T
           (DO ()
               ((PROGN
                 (FORMAT T "~A~%" PROMPT)
                 (SETQ FORM (READ-FOR-TOP-LEVEL))
                 (SETQ OBJECT (CAR (EH-EVAL EH-SG FORM)))
                 (TERPRI)
                 (COND ((EH-CONSTANT-FORM-P FORM) T)
                       ((NEQ CURRENT-PROCESS SELECTED-PROCESS) T)
                       (T (FORMAT T "The object is ~S, ok? " OBJECT)
                          (Y-OR-N-P))))
                OBJECT)))))

;; Backtrace.
;; These initial implementations make no attempt to censor the stack,
;; but clearly something will be needed for interpreted functions.

;; Both versions take arguments the same way.  The first is the SG, and the
;; second is ignored so that the functions may be used as commands.
;; They will print no more than N frames if N is present.
;; If SKIP is nonzero, the first that many frames are skipped before the
;; N frames are printed.  Used to put them in curly brackets, but that was
;; fairly useless.

;; This prints out like the Maclisp BAKTRACE, and does not TERPRI at beginning
;; nor end.
(DEFUN EH-SHORT-BACKTRACE (SG IGNORE &OPTIONAL (N 777777) (SKIP 0) UNINTERESTING-FLAG)
  (DO ((AP (SG-AP SG) (EH-PREVIOUS-ACTIVE SG AP))
       (RP (SG-REGULAR-PDL SG)))
      ((OR (NULL AP) (MINUSP (SETQ SKIP (1- SKIP))))
       (DO ((AP AP (FUNCALL (COND (UNINTERESTING-FLAG 'EH-PREVIOUS-ACTIVE)
				  (T 'EH-PREVIOUS-INTERESTING-ACTIVE))
			    SG AP))
	    (I 0 (1+ I)))
	   ((OR (>= I N) (NULL AP)) NIL)
	 (OR (ZEROP I) (PRINC "  "))
	 (PRIN1 (EH-FUNCTION-NAME (RP-FUNCTION-WORD RP AP)))))))

(DEFUN EH-FULL-BACKTRACE (SG IGNORE &OPTIONAL (N 777777) (SKIP 0) UNINTERESTING-FLAG)
  (DO ((AP (SG-AP SG) (EH-PREVIOUS-ACTIVE SG AP)))
      ((OR (NULL AP) (MINUSP (SETQ SKIP (1- SKIP))))
       (DO ((AP AP (FUNCALL (COND (UNINTERESTING-FLAG 'EH-PREVIOUS-ACTIVE)
				  (T 'EH-PREVIOUS-INTERESTING-ACTIVE))
			    SG AP))
	    (I 0 (1+ I)))
	   ((OR (>= I N) (NULL AP)) NIL)
	 (EH-PRINT-FUNCTION-AND-ARGS SG AP)))))

(DEFUN EH-FULL-BACKTRACE-UNINTERESTING (SG IGNORE &OPTIONAL (N 777777) (SKIP 0))
    (EH-FULL-BACKTRACE SG NIL N SKIP T))

;Return list of the function and args that were invoked (as best as it can).
(DEFUN EH-GET-FRAME-FUNCTION-AND-ARGS (SG AP
			    &AUX FUNCTION NARGS-SUPPLIED 
				 (RP (SG-REGULAR-PDL SG))
				 LEXPR-CALL REST-ARG-P REST-ARG-VALUE ANS)
      (SETQ FUNCTION (RP-FUNCTION-WORD RP AP)
	    NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP))  ;Really slots on stack
      (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-P LEXPR-CALL)
	(EH-REST-ARG-VALUE SG AP))
      ;; Print the individual args.
      (DO ((I NARGS-SUPPLIED (1- I)))   ;CONS THEM UP IN REVERSE ORDER
	  ((ZEROP I))
	(SETQ ANS (CONS (AREF RP (+ AP I)) ANS)))   ;+1 -1
      ;; NCONC the rest arg if any.
      (COND ((OR REST-ARG-P LEXPR-CALL)
	     (SETQ ANS (NCONC ANS (APPEND REST-ARG-VALUE NIL)))))
      (CONS FUNCTION ANS))

(DEFUN EH-PRINT-FUNCTION-AND-ARGS (SG AP &AUX FUNCTION (RP (SG-REGULAR-PDL SG)))
  (SETQ FUNCTION (EH-FUNCTION-NAME (RP-FUNCTION-WORD RP AP)))
  (ERRSET (FORMAT T "~%~S:" FUNCTION) NIL)
  (AND (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
       (FORMAT T " (P.C. = ~O)"		;Note that this displays the return-pc,
	       (RP-EXIT-PC RP AP)))	; which is one greater than the D-LAST.  
  (TERPRI)
  (EH-PRINT-FRAME-ARGS SG AP 3))

;; Returns T if it displayed the rest arg, NIL otherwise.  See EH-SHOW-ALL-MACRO.
(DEFUN EH-PRINT-FRAME-ARGS (SG AP INDENT
			    &AUX (PRINLEVEL EH-PRINLEVEL)
				 (PRINLENGTH EH-PRINLENGTH)
				 FUNCTION NARGS-SUPPLIED NARGS-TO-PRINT
				 (RP (SG-REGULAR-PDL SG))
				 NARGS-EXPECTED NARGS-REQUIRED
				 LEXPR-CALL REST-ARG-P REST-ARG-VALUE)
      (SETQ FUNCTION (RP-FUNCTION-WORD RP AP)
	    NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP))
      (COND ((OR (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER) (LISTP FUNCTION))
	     (SETQ NARGS-REQUIRED
		   (LDB %%ARG-DESC-MIN-ARGS (ARGS-INFO FUNCTION)))
	     (SETQ NARGS-EXPECTED
		   (LDB %%ARG-DESC-MAX-ARGS (ARGS-INFO FUNCTION)))))
      (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-P LEXPR-CALL)
	(EH-REST-ARG-VALUE SG AP))
      (SETQ NARGS-TO-PRINT (EH-NUMBER-OF-SPREAD-ARGS SG AP))
      ;; Print the individual args.
      (DOTIMES (I NARGS-TO-PRINT)
	(AND (= I NARGS-SUPPLIED)
	     (COND ((AND NARGS-REQUIRED (< I NARGS-REQUIRED))
		    (FORMAT T "   --Missing args:--~%"))
		   (T
		    (FORMAT T "   --Defaulted args:--~%")))) ;These "args" weren't supplied
	(AND NARGS-EXPECTED (= I NARGS-EXPECTED)    ;Called with too many args
	     (FORMAT T "   --Extraneous args:--~%"))
	(FORMAT T "~VTArg ~D" INDENT I)
	(EH-DISPLAY-ARG-NAME " (~A)" FUNCTION I)
	;; Print the arg value unless the arg is missing (val is garbage).
	(OR (AND NARGS-REQUIRED
		 (> NARGS-REQUIRED NARGS-SUPPLIED)
		 ( I NARGS-SUPPLIED))
	    (PROGN (PRINC ": ")
		   (ERRSET (PRIN1 (AREF RP (+ AP I 1))) NIL)))
	(TERPRI))
      ;; Print the rest arg if any.
      (COND (REST-ARG-P
	     (FORMAT T "~VTRest arg" INDENT)
	     (EH-DISPLAY-LOCAL-NAME " (~A)" FUNCTION 0)
	     (PRINC ": "))
	    (LEXPR-CALL
	     (FORMAT T "~VTExtraneous Rest Arg: " INDENT)))
      (COND ((OR REST-ARG-P LEXPR-CALL)
	     (ERRSET (PRIN1 REST-ARG-VALUE) NIL)
	     (TERPRI)))
      REST-ARG-P)

(DEFUN EH-DISPLAY-LOCAL-NAME (FORMAT-STRING FUNCTION LOCALNO &AUX NAME)
  (SETQ NAME (EH-LOCAL-NAME FUNCTION LOCALNO))
  (AND NAME (FORMAT T FORMAT-STRING NAME)))

(DEFUN EH-DISPLAY-ARG-NAME (FORMAT-STRING FUNCTION ARGNO &AUX NAME)
    (SETQ NAME (EH-ARG-NAME FUNCTION ARGNO))
    (AND NAME (FORMAT T FORMAT-STRING NAME)))

;; Commands in the dispatch table.  These are given the SG and the ETE.
;; Any command which wants to return out of the error handler should
;; do a throw to EH-FINISHED after restarting the erring stack group.

;; Basic commands for inspecting specific stack frames.
;; UP means closer to the top of the stack, DOWN means the base of the stack.

;; Control-P, <^>
(DEFUN EH-UP-STACK (SG IGNORE &OPTIONAL COUNT SHOW-ALL-FLAG REVERSE-FLAG UNINTERESTING-FLAG
		       &AUX AP COUNT1)
    (SETQ COUNT1 (OR COUNT 1))
    (AND REVERSE-FLAG (SETQ COUNT1 (- COUNT1)))
    (SETQ AP (FUNCALL (COND (UNINTERESTING-FLAG 'EH-NEXT-NTH-ACTIVE)
			    (T 'EH-NEXT-NTH-INTERESTING-ACTIVE))
		      SG EH-CURRENT-FRAME COUNT1))
    (COND ((= AP EH-CURRENT-FRAME)
	   (FORMAT T (COND (REVERSE-FLAG
			    "You are already at the bottom of the stack.~%")
			   (T "You are already at the top of the stack.~%"))))
	  (T (SETQ EH-CURRENT-FRAME AP)
	     (COND ((NOT SHOW-ALL-FLAG) (EH-SHOW-FUNCTION-AND-ARGS SG))
		   (T (EH-SHOW-ALL SG)))))
    NIL)

;; Control-N, <line>
(DEFUN EH-DOWN-STACK (SG ETE &OPTIONAL COUNT)
    (EH-UP-STACK SG ETE COUNT NIL T))

;; Meta-P.
(DEFUN EH-UP-STACK-ALL (SG ETE &OPTIONAL COUNT)
    (EH-UP-STACK SG ETE COUNT T))

;; Meta-N.
(DEFUN EH-DOWN-STACK-ALL (SG ETE &OPTIONAL COUNT)
    (EH-UP-STACK SG ETE COUNT T T))

;; Meta->.
(DEFUN EH-TOP-STACK (SG &REST IGNORE)
    (SETQ EH-CURRENT-FRAME (EH-OUT-TO-INTERESTING-ACTIVE SG (SG-AP SG)))
    (EH-SHOW-FUNCTION-AND-ARGS SG)
    NIL)

;; Meta-<.
(DEFUN EH-BOTTOM-STACK (SG &REST IGNORE)
    (SETQ EH-CURRENT-FRAME
	  (DO ((AP (SG-AP SG) (EH-PREVIOUS-ACTIVE SG AP))
	       (PREV-AP NIL AP))
	      ((NULL AP) PREV-AP)))
    (EH-SHOW-FUNCTION-AND-ARGS SG)
    NIL)

;; Control-Meta-P.
(DEFUN EH-UP-STACK-UNINTERESTING (SG ETE &OPTIONAL COUNT)
    (EH-UP-STACK SG ETE COUNT NIL NIL T))

;; Control-Meta-N.
(DEFUN EH-DOWN-STACK-UNINTERESTING (SG ETE &OPTIONAL COUNT)
    (EH-UP-STACK SG ETE COUNT NIL T T))

;; Control-Meta-U.
(DEFUN EH-UP-TO-INTERESTING (SG IGNORE &OPTIONAL IGNORE)
    (SETQ EH-CURRENT-FRAME (EH-OUT-TO-INTERESTING-ACTIVE SG EH-CURRENT-FRAME))
    (EH-SHOW-FUNCTION-AND-ARGS SG)
    NIL)

;; Control-L, form.
(DEFUN EH-CLEAR-AND-SHOW (SG ETE &REST IGNORE)
    (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
    (EH-PRINT-ERROR-MESSAGE SG ETE)
    (EH-SHOW-FUNCTION-AND-ARGS SG)
    NIL)

;; This is how the error message is printed when the error handler starts up.
(DEFUN EH-SHOW (SG ETE &REST IGNORE)
    (TERPRI)
    (EH-PRINT-ERROR-MESSAGE SG ETE)
    (EH-SHOW-FUNCTION-AND-ARGS SG)
    NIL)

;; Meta-L.
(DEFUN EH-CLEAR-AND-SHOW-ALL (SG &REST IGNORE)
    (EH-SHOW-ALL SG)
    NIL)

;; EH-SHOW-FUNCTION-AND-ARGS is reguar printing tty stuff.
;; EH-SHOW-ALL clears the screen and then fill is up.

;; The guts of the commands on the previous page.

(DEFUN EH-SHOW-FUNCTION-AND-ARGS (SG)
    (EH-PRINT-FUNCTION-AND-ARGS SG EH-CURRENT-FRAME))

(DEFUN EH-SHOW-ALL (SG &AUX RP FUNCTION)
    (SETQ RP (SG-REGULAR-PDL SG)
	  FUNCTION (RP-FUNCTION-WORD RP EH-CURRENT-FRAME))
    (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
    (SELECT (%DATA-TYPE FUNCTION)
       (DTP-FEF-POINTER (EH-SHOW-ALL-MACRO SG RP FUNCTION))
       (OTHERWISE (EH-SHOW-FUNCTION-AND-ARGS SG))))
	  
(DEFUN EH-SHOW-ALL-MACRO (SG RP FUNCTION &AUX N-LOCALS PC-NOW NAME REST-ARG-PRINTED
			     PC-PPR LH PC-PPR-HEIGHT TEM LIM-PC)
    (SETQ N-LOCALS (FEF-NUMBER-OF-LOCALS FUNCTION)
	  NAME (FEF-NAME FUNCTION)
	  PC-NOW (RP-EXIT-PC RP EH-CURRENT-FRAME)
	  LIM-PC (COMPILER:DISASSEMBLE-LIM-PC FUNCTION))
    ;; Print the header, including the underlined function name
    (FORMAT T "~14XMacro-compiled frame.  Frame address = ~O~%~%~S~2%"
		   EH-CURRENT-FRAME NAME)
    ;; Print the arguments, including the rest-arg which is the first local
    (SETQ REST-ARG-PRINTED (EH-PRINT-FRAME-ARGS SG EH-CURRENT-FRAME 0))
    ;; Print the rest of the locals
    (DO ((I 0 (1+ I))
	 (J (+ (RP-LOCAL-BLOCK-ORIGIN RP EH-CURRENT-FRAME) EH-CURRENT-FRAME) (1+ J)))
	((>= I N-LOCALS))
      (COND ((NOT (AND REST-ARG-PRINTED (ZEROP I)))	;Don't show rest arg twice
	     (FORMAT T "Local ~D" I)
	     (EH-DISPLAY-LOCAL-NAME " (~A)" FUNCTION I)
	     (LET ((PRINLEVEL EH-PRINLEVEL)
		   (PRINLENGTH EH-PRINLENGTH))
	       (FORMAT T ": ~S~%" (AREF RP J))))))
    (FORMAT T "~%Disassembled code:")
    ;; Figure out how many instructions will fit in the pc ppr we are using.
    (COND ((MEMQ ':PC-PPR (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
	   (SETQ PC-PPR (FUNCALL STANDARD-OUTPUT ':PC-PPR))
	   (SETQ PC-PPR-HEIGHT (- (PC-PPR-BOTTOM-LIMIT PC-PPR)	;The -20 is to prevent
				  (PC-PPR-TOP-MARGIN PC-PPR)	; **MORE** from going off.
				  20)
		 LH (PC-PPR-LINE-HEIGHT PC-PPR))
	   (MULTIPLE-VALUE (NIL TEM) (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':PIXEL))
	   (SETQ TEM (- (// (- PC-PPR-HEIGHT TEM) LH) 3))))	;Number of lines to play with.
    ;; Disassemble assuming we have TEM lines of space to use.
    (SETQ TEM (MAX EH-DISASSEMBLE-INSTRUCTION-COUNT (OR TEM 0)))
    (DO ((I 0 (1+ I))
	 (PC (MAX (FEF-INITIAL-PC FUNCTION) (- PC-NOW (// TEM 2)))
	     (+ PC (COMPILER:DISASSEMBLE-INSTRUCTION-LENGTH FUNCTION PC))))
	((OR (>= I TEM) (>= PC LIM-PC))
	 (COND ((= PC PC-NOW)				;If arrow should point after all code,
		(TERPRI) (PRINC "=> ")))
	 )
      (TERPRI)
      (PRINC (COND ((= PC PC-NOW) "=> ")
		   (T "   ")))
      (COMPILER:DISASSEMBLE-INSTRUCTION FUNCTION PC)))

;; Other commands.

;; Control-Z.
(DEFUN EH-TOP-LEVEL-THROW (SG ETE &OPTIONAL (COUNT 1))
   ETE COUNT
   (COND ((NULL EH-RUNNING)
	  ;; Explicitly invoked error handler => return from it.
	  (*THROW 'EH-EXIT NIL))
	 ((EQ SG SCHEDULER-STACK-GROUP)
	  (FORMAT T "~&Restarting the scheduler.")
	  (COND (EH-REAL-CURRENT-PROCESS
		 (PROCESS-BLAST EH-REAL-CURRENT-PROCESS)
		 (FORMAT T "~%Blasting ~S so this won't happen again" EH-REAL-CURRENT-PROCESS)))
	  (STACK-GROUP-PRESET SG (FUNCTION PROCESS-SCHEDULER))
	  (EH-RUN-SG-GOODBYE SG))
	 ((EQ CURRENT-PROCESS KBD-INTERRUPT-PROCESS)
	  (FORMAT T "~&Restarting the keyboard interrupt process.")
	  (PROCESS-PRESET KBD-INTERRUPT-PROCESS 'KBD-PROCESS-TOP-LEVEL)
	  (EH-RUN-SG-GOODBYE SG))
	 (T
	  (OR (EQ SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS))
	      (EH-UNWIND-SG SG %CURRENT-STACK-GROUP NIL NIL))
	  (EH-THROW (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS) 'TOP-LEVEL NIL)))
   NIL)

;; Meta-Z command.  Throw one level of error handler in this stack group.
(DEFUN EH-THROW-ONE-ERROR (SG ETE)
    (COND ((NULL EH-RUNNING) (*THROW 'EH-QUIT NIL))
	  ((SG-FOOTHOLD-DATA SG)
	   (EH-THROW SG 'EH-FOOTHOLD NIL))
	  (T (EH-TOP-LEVEL-THROW SG ETE))))

;; Control-T.
(DEFUN EH-THROW-COMMAND (SG &REST IGNORE)
    (COND ((NULL EH-RUNNING) (*THROW 'EH-QUIT NIL)))
    (FORMAT T "Throw a value to a tag.~%")
    (EH-THROW SG
	      (EH-GET-OBJECT "What is the tag?")
	      (EH-GET-OBJECT "What value do you wish to throw?"))
    NIL)

;; ?, <help>
(DEFUN EH-HELP (&REST IGNORE)
  (FORMAT T "~&~A~%"
"    You are in the error handler.  If you type in a Lisp form, it will be
evaluated, and the result printed.  You may also type one of the following:
<help> or ? gets this text.  To just get back to top level, type a Z.
    N or <line> goes down a frame, P or <return> goes up.  N and P
are similar but give more info.  L or <form> clears screen and retypes info,
L clears screen and types more info.  < goes to top of stack, >
goes to the bottom.
    B gives a brief backtrace, B a fuller one.  A prints the arglist of the
function in the current frame.
    N, P and B resemble N, P and B except that they show
all the internal EVALs, PROGs, CONDs, etc. of interpreted code.
    A prints an argument to the current function, and sets * to be that argument
to let you do more complicated things with it.  + is set to a locative to that argument,
should you want to modify it.  L does likewise for the function's locals.
    R returns a value from the current frame.  R offers to reinvoke the current
frame with the originally supplied arguments (as best as they can be determined).
    T throws to a specific tag.
    C corrects the error and continues; it may ask you to input a value.
    C continues from an ERROR-RESTART special form.
    Z is like Z, but when there are recursive errors it only pops one level.
While in the error hander, G quits back to the error handler top level."
  ))

;; Control-R.
(DEFUN EH-RETURN-A-VALUE (SG &REST IGNORE)
    (COND ((NULL EH-RUNNING) (*THROW 'EH-QUIT NIL)))
    (FORMAT T "Return a value from the function ~S.~%"
	    (EH-FUNCTION-NAME (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) EH-CURRENT-FRAME)))
    (EH-UNWIND-TO-FRAME SG EH-CURRENT-FRAME T
			(EH-GET-OBJECT "Form to evaluate and return:"))
    NIL)

;; Meta-R.
(DEFUN EH-RETURN-MANY-VALUES (&REST IGNORE)
);WRITE THIS

;; Control-Meta-R
(DEFUN EH-RETURN-REINVOKATION (SG &REST IGNORE &AUX FORM OBJECT)
  (COND ((NULL EH-RUNNING) (*THROW 'EH-QUIT NIL)))
  (FORMAT T "Evaluating ~S, OK?"
	  (SETQ FORM (EH-GET-FRAME-FUNCTION-AND-ARGS SG EH-CURRENT-FRAME)))
  (COND ((Y-OR-N-P)
	 (EH-UNWIND-TO-FRAME SG EH-CURRENT-FRAME NIL NIL)  ;Unwind other crap, so as to
           ;unlock things, establish correct environment, etc.  Unfortunately, it doesnt
	   ;quite win, ie the frame being returnned from is not unwound until
	   ;after the new value has been obtained.
	 (SETQ OBJECT (CAR (EH-APPLY EH-SG (CAR FORM) (CDR FORM))))	   ;reinvoke the frob.
	 (EH-UNWIND-TO-FRAME SG EH-CURRENT-FRAME T OBJECT))) ;Return the value.
  NIL)

;; Control-A.
(DEFUN EH-ARGLIST (SG &REST IGNORE)
    (LET ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) EH-CURRENT-FRAME)))
      (FORMAT T "Argument list for ~S is ~A.~%"
	      (EH-FUNCTION-NAME FUNCTION)
	      (ARGLIST FUNCTION)))
    NIL)

;; Control-C.
(DEFUN EH-PROCEED (SG ETE &REST IGNORE &AUX TEM OBJ)
    (COND ((NULL EH-RUNNING) (*THROW 'EH-QUIT NIL)))
    (COND ((AND (EQ (CAR ETE) 'FERROR)
		(CADR ETE))
	   (SETQ OBJ (COND ((SETQ TEM (GET (FOURTH ETE) 'EH-PROCEED))	;Condition's property
			    (FUNCALL TEM SG ETE))
			   (T
			     (AND (EQ (CADR ETE) T)
				  (EH-GET-OBJECT
				    "Form to evaluate and return from FERROR//CERROR:")))))
	   ;; Now restart the stack group, causing CERROR to return OBJ
	   (WITHOUT-INTERRUPTS
	     (EH-FREE %CURRENT-STACK-GROUP)
	     (FUNCALL SG OBJ)))
	  ((AND (EQ (CAR ETE) ':BREAK)		;Break
		(CADR ETE))			; and want retry.
	   (WITHOUT-INTERRUPTS			;Everything set at EH-SECOND-LEVEL. Do it.
	     (EH-FREE %CURRENT-STACK-GROUP)
	     (FUNCALL SG OBJ)))
	  (T
	    (LET ((FUNCTION (GET (FIRST ETE) 'EH-PROCEED)))
	      (COND ((NULL FUNCTION)
		     (FORMAT T "There is no way to proceed from this error.~%"))
		    (T ;; Call the specific proceed routine.  It should call EH-RESTART
		       ;; to mung the micro-stack appropriately.
		       (FUNCALL FUNCTION SG ETE)
		       ;; Now restart the stack group, as that routine left it.
		       (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
		       (WITHOUT-INTERRUPTS
			 (EH-FREE %CURRENT-STACK-GROUP)
			 (FUNCALL SG)))))))
    NIL)

;; Meta-C.
(DEFUN EH-ERROR-RESTART (SG &REST IGNORE)
    (COND ((NULL EH-RUNNING) (*THROW 'EH-QUIT NIL)))
    (COND ((OR EH-CONDITION-PROCEED-FLAG
	       (Y-OR-N-P "Are you SURE you want to restart? "))
	   (EH-THROW SG 'ERROR-RESTART NIL)))
    (FORMAT T "Flushed.~%"))

;; Control-S.
(DEFUN EH-SEARCH (SG IGNORE &OPTIONAL IGNORE FLAG &AUX KEY AP)
    (FORMAT T "String to search for (end with RETURN):~%")
    (SETQ KEY (READLINE))
    (SETQ AP 
	 (DO ((AP (SG-AP SG) (EH-PREVIOUS-ACTIVE SG AP))
	      (RP (SG-REGULAR-PDL SG))
	      (NAME)
	      )
	     ((NULL AP) NIL)
	   (SETQ NAME (EH-FUNCTION-NAME (RP-FUNCTION-WORD RP AP)))
	   (SETQ NAME
		 (COND ((STRINGP NAME) NAME)
		       ((SYMBOLP NAME) (STRING NAME))
		       (T (FORMAT NIL "~S" NAME))))
	   (AND (STRING-SEARCH KEY NAME)
		(RETURN AP))))
    (COND ((NULL AP)
	   (FORMAT T "Search failed.~%"))
	  (T
	   (SETQ EH-CURRENT-FRAME AP)
	   (COND ((NOT FLAG) (EH-SHOW-FUNCTION-AND-ARGS SG))
		 (T (EH-SHOW-ALL SG))))))

;; Meta-S.
(DEFUN EH-SEARCH-AND-SHOW-ALL (SG ETE &OPTIONAL (COUNT 1))
   (EH-SEARCH SG ETE COUNT T))

;; Control-Meta-A
(DEFUN EH-GET-ARG (SG IGNORE &OPTIONAL (ARG 0) &AUX RP TEM)
   (SETQ RP (SG-REGULAR-PDL SG)
	 TEM (+ ARG 1 EH-CURRENT-FRAME)
	 + (AP-1 RP TEM)
	 * (AR-1 RP TEM))
   (FORMAT T "~&~S" *))

;; Control-Meta-L
(DEFUN EH-GET-LOCAL (SG IGNORE &OPTIONAL (ARG 0) &AUX RP TEM)
   (SETQ RP (SG-REGULAR-PDL SG)
	 TEM (+ ARG EH-CURRENT-FRAME (RP-LOCAL-BLOCK-ORIGIN RP EH-CURRENT-FRAME))
	 + (AP-1 RP TEM)
	 * (AR-1 RP TEM))
   (FORMAT T "~&~S" *))


;; The initial dispatch table.
(SETQ EH-DISPATCH-LIST '(
       ( (:REPEAT 77 NIL)	;Sail graphics.
	 EH-HELP		;?
	 (:REPEAT 105 NIL)
	 NIL			;Backnext.
	 EH-HELP		;Help.
	 NIL			;Rubout.
	 NIL			;Backspace.
	 NIL			;Tab.
	 EH-DOWN-STACK		;Line.
	 NIL			;Vt.
	 EH-CLEAR-AND-SHOW	;Form.
	 EH-UP-STACK		;Return.
	 (:REPEAT 2 NIL))
       ;; Control characters
       (
	(:REPEAT 40 NIL)
	(:REPEAT 10 NIL)
	NIL		      ;(
	NIL		      ;)
	(:REPEAT 6 NIL)
	(:REPEAT 12 EH-NUMBER-COMMAND)	      ;digits
	(:REPEAT 2 NIL)
	NIL		      ;<
	NIL		      ;=
	NIL		      ;>
	NIL		      ;?
	NIL		      ;@
	EH-ARGLIST	      ;A
	EH-SHORT-BACKTRACE    ;B
	EH-PROCEED	      ;C
	NIL		      ;D
	NIL		      ;E
	NIL		      ;F
	NIL		      ;G
	NIL		      ;H
	NIL		      ;I
	NIL		      ;J
	NIL		      ;K
	EH-CLEAR-AND-SHOW     ;L
	NIL		      ;M
	EH-DOWN-STACK	      ;N
	NIL		      ;O
	EH-UP-STACK	      ;P
	NIL		      ;Q
	EH-RETURN-A-VALUE     ;R
	EH-SEARCH	      ;S
	EH-THROW-COMMAND      ;T
	NIL		      ;U
	NIL		      ;V
	NIL		      ;W
	NIL		      ;X
	NIL		      ;Y
	EH-TOP-LEVEL-THROW    ;Z
	(:REPEAT 6 NIL)
	(:REPEAT-EVAL 32 (LIST 1 (+ 101 SI:RPCNT)))      ;Map lower case into upper case.
	(:REPEAT 5 NIL)
	(:REPEAT 7 NIL)
	NIL
	(:REPEAT 5 NIL)
	NIL
	(:REPEAT 2 NIL))
       ;; Meta characters
       (
	(:REPEAT 40 NIL)
	(:REPEAT 10 NIL)
	NIL		      ;(
	NIL		      ;)
	(:REPEAT 6 NIL)
	(:REPEAT 12 EH-NUMBER-COMMAND)	      ;digits
	(:REPEAT 2 NIL)
	EH-TOP-STACK	      ;<
	NIL		      ;=
	EH-BOTTOM-STACK	      ;>
	NIL		      ;?
	NIL		      ;@
	NIL		      ;A
	EH-FULL-BACKTRACE     ;B
	EH-ERROR-RESTART      ;C
	NIL		      ;D
	NIL		      ;E
	NIL		      ;F
	NIL		      ;G
	NIL		      ;H
	NIL		      ;I
	NIL		      ;J
	NIL		      ;K
	EH-CLEAR-AND-SHOW-ALL ;L
	NIL		      ;M
	EH-DOWN-STACK-ALL     ;N
	NIL		      ;O
	EH-UP-STACK-ALL	      ;P
	NIL		      ;Q
	EH-RETURN-MANY-VALUES ;R
	EH-SEARCH-AND-SHOW-ALL	      ;S
	NIL		      ;T
	NIL		      ;U
	NIL		      ;V
	NIL		      ;W
	NIL		      ;X
	NIL		      ;Y
	EH-THROW-ONE-ERROR    ;Z
	(:REPEAT 6 NIL)
	(:REPEAT-EVAL 32 (LIST 2 (+ 101 SI:RPCNT)))      ;Map lower case into upper case.
	(:REPEAT 5 NIL)
	(:REPEAT 7 NIL)
	NIL
	(:REPEAT 5 NIL)
	NIL
	(:REPEAT 2 NIL))
       ;Control-Meta characters
       (
	(:REPEAT 40 NIL)
	(:REPEAT 10 NIL)
	NIL		      ;(
	NIL		      ;)
	(:REPEAT 6 NIL)
	(:REPEAT 12 EH-NUMBER-COMMAND)	      ;digits
	(:REPEAT 2 NIL)
	NIL		      ;<
	NIL		      ;=
	NIL		      ;>
	NIL		      ;?
	NIL		      ;@
	EH-GET-ARG	      ;A
	EH-FULL-BACKTRACE-UNINTERESTING   ;B
	NIL		      ;C
	NIL		      ;D
	NIL		      ;E
	NIL		      ;F
	NIL		      ;G
	NIL		      ;H
	NIL		      ;I
	NIL		      ;J
	NIL		      ;K
	EH-GET-LOCAL	      ;L
	NIL		      ;M
	EH-DOWN-STACK-UNINTERESTING  ;N
	NIL		      ;O
	EH-UP-STACK-UNINTERESTING    ;P
	NIL		      ;Q
	EH-RETURN-REINVOKATION       ;R
	NIL		      ;S
	NIL		      ;T
	EH-UP-TO-INTERESTING  ;U
	NIL		      ;V
	NIL		      ;W
	NIL		      ;X
	NIL		      ;Y
	NIL		      ;Z
	(:REPEAT 6 NIL)
	(:REPEAT-EVAL 32 (LIST 3 (+ 101 SI:RPCNT)))      ;MAP LOWER CASE INTO UPPER CASE
	(:REPEAT 5 NIL)
	(:REPEAT 7 NIL)
	NIL
	(:REPEAT 5 NIL)
	NIL
	(:REPEAT 2 NIL))
       ))

;;; Find the value of a symbol in the binding environment of a specified stack group.
;;; Note that this cannot get an error even if the sg is in some funny state, unlike
;;; EH-EVAL.

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
		  (SETQ P (AP-1-CAREFUL SP I))
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
	     (SETQ P (AP-1-CAREFUL SP I))
	     (SELECT (%P-DATA-TYPE P)
	       (DTP-LOCATIVE
		(SETQ P (%MAKE-POINTER-OFFSET DTP-LOCATIVE P -1))
		(IF (EQ (AREF SP I) VCL)
		    (SETQ VAL (CAR P)))
		(SETQ I (1- I)))
	       (OTHERWISE )))))))

;Get the special-pdl pointer for the running SG
(DEFUN GET-OWN-SPECIAL-PDL-POINTER (SP)
  (- (1- (%STRUCTURE-BOXED-SIZE SP))
     (+ (ARRAY-LEADER-LENGTH SP) 3 (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG SP 0))))

;An ALOC that only works for 1-dimensional arrays.  It avoids referencing the
;word pointed to since if the special-pdl pointer being used is confused that
;might be an external-value-cell-pointer, causing an error.  This doesn't
;do bounds checking.
(DEFUN AP-1-CAREFUL (ARRAY INDEX)
  (%MAKE-POINTER-OFFSET DTP-LOCATIVE
			ARRAY
			(+ INDEX 1 (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))))
