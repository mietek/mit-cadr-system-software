;;; -*-Mode:LISP; Package:SYSTEM-INTERNALS; BASE: 8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;Lisp machine processes and scheduler.
;Note that this file is not in the cold-load, but is loaded in as part of
;the "inner system", after which scheduling is turned on.

;; The following structure is a "process".  It is not the same as a stack group.
;; A collection of stack groups usually form a process, and we simply
;; remember which stack group of the process was the one running when
;; the process was descheduled.
;; A process MUST be a named structure, because the scheduler uses it as one.

(DEFSTRUCT (PROCESS :ARRAY :NAMED)
    PROCESS-NAME		    ;Print name
    PROCESS-STACK-GROUP		    ;Stack group currently executing on behalf of this process
    (PROCESS-WAIT-FUNCTION	    ;Predicate to determine if process is runnable
        (FUNCTION FALSE))
    PROCESS-WAIT-ARGUMENT-LIST	    ;Arguments passed to above (use an arg to avoid a closure)
				    ;This will often be a rest argument in somebody's stack,
				    ;but it will always be used in a safe manner.
    (PROCESS-WHOSTATE "JUST CREATED")	    ;The "WHOSTATE" string for the who line or PEEK
    PROCESS-INITIAL-STACK-GROUP	    ;The stack group which PROCESS-RESET (q.v.) will reset to.
    PROCESS-INITIAL-FORM	    ;Form to preset the initial sg to when process is reset.
    PROCESS-RUN-REASONS		    ;List of run reasons for this process.
    PROCESS-ARREST-REASONS	    ;List of arrest reasons for this process.
    PROCESS-ERROR-STOP-PROCESSES    ;Processes or parallelisms to be stopped if
				    ;this process gets an error.
    PROCESS-PLIST		    ;Random properties.
    )

(DEFVAR CURRENT-PROCESS)	    ;The process which is currently executing.
(DEFVAR TOP-PROCESS)		    ;The process that the machine starts up in.
(DEFVAR WARM-BOOTED-PROCESS)	    ;The process that was warm-booted out of.
(DEFVAR INHIBIT-SCHEDULING-FLAG)    ;Inhibits clock and process-switching
(DEFVAR CLOCK-FUNCTION-LIST NIL)    ;Every 60th of a second, each elt is funcalled on nothing.
(DEFVAR SCHEDULER-STACK-GROUP)	    ;The stack group in which the scheduler runs.
(DEFVAR SCHEDULER-EXISTS)	    ;T if the scheduler and processes are set up.
(DEFVAR SCHEDULER-HOOK NIL)	    ;If non-NIL, called with arg of process about to be run.
(DEFVAR INHIBIT-IDLE-SCAVENGING-FLAG T) ;If NIL scavenger runs when no processes runnable
(DEFVAR GC-IDLE-SCAVENGE-QUANTUM 10.)   ;Argument to %GC-SCAVENGE used in that case

;;  An ACTIVE process is one which is runnable.
;;   A process is runnable if it has at least one run-reason
;;   and no arrest-reasons.
;;  The CURRENT process is the one which is running.
;;   If current process is NIL, it
;;   means we are running CLOCK-FUNCTION-LIST or certain other overhead
;;   functions within the scheduler.
;;
;; ACTIVE-PROCESSES	An alist of all processes that are runnable.
;;			A process is runnable if it has at least one run
;;			reason, and no arrest reasons.  This list is maintained
;;			because it is considered too expensive to have the
;;			scheduler inspect each process' run and arrest reasons.
;; Each element on ACTIVE-PROCESSES looks like:
;;	(process wait-function wait-arglist <slots for wait args>)
;;	wait-arglist is usually a tail of this list
;;	NIL for a process means an unused slot
;; This stuff is all carefully packed into one page
(DEFVAR ACTIVE-PROCESSES-ELEMENT-SIZE 8)
(DEFVAR ACTIVE-PROCESSES-PREFIX-SIZE 3) ;Process, wait-function, wait-arglist

(DEFUN MAKE-ACTIVE-PROCESSES (LEN &AUX AP)
  (WITHOUT-INTERRUPTS
    ;; Make sure that list gets allocated contiguously
    (SETQ AP (MAKE-LIST PERMANENT-STORAGE-AREA LEN))
    (DO ((L AP (CDR L)))
	((NULL L) AP)
      (RPLACA L (MAKE-LIST PERMANENT-STORAGE-AREA ACTIVE-PROCESSES-ELEMENT-SIZE)))))

;Allocate contiguous space for 20. processes which should be more than enough.
(DEFVAR ACTIVE-PROCESSES (MAKE-ACTIVE-PROCESSES 20.))

;Make an entry for this process in ACTIVE-PROCESSES, with its current wait condition,
;when it first becomes runnable.  Try not to cons.
(DEFUN PROCESS-ACTIVE-ENTRY (PROC &AUX AENTRY)
  (WITHOUT-INTERRUPTS
    (OR (SETQ AENTRY (ASSQ PROC ACTIVE-PROCESSES))
	(SETQ AENTRY (ASSQ NIL ACTIVE-PROCESSES))
	(RPLACD (LAST ACTIVE-PROCESSES)
		(NCONS (SETQ AENTRY (MAKE-LIST PERMANENT-STORAGE-AREA
					       ACTIVE-PROCESSES-ELEMENT-SIZE)))))
    (SETF (FIRST AENTRY) PROC)
    (SET-PROCESS-WAIT PROC (PROCESS-WAIT-FUNCTION PROC) (PROCESS-WAIT-ARGUMENT-LIST PROC))))

;Remove this process's ACTIVE-PROCESSES entry, moving other entries up
;to fill in the gap.
(DEFUN PROCESS-REMOVE-ACTIVE-ENTRY (PROC)
  (WITHOUT-INTERRUPTS
    (LET ((L (MEM #'(LAMBDA (PROC ELEM) (EQ (CAR ELEM) PROC)) PROC ACTIVE-PROCESSES)))
      (IF (NULL L)
	  ;; Not found, just return
	  NIL

	  ;; Null this entry
	  (SETF (FIRST (CAR L)) NIL)
	  ;; Now, remove all null entries
	  (DO ((NULL L (CDR NULL)))
	      ((NULL NULL) T)
	    (AND (NULL (FIRST (CAR L)))
		 ;; A null entry, find next non-null and exchange
		 (LET ((NON-NULL (DO ((L (CDR NULL) (CDR L)))
				     ((OR (FIRST (CAR L)) (NULL L))
				      L)))
		       (TEM (CAR NULL)))
		   (AND (NULL NON-NULL) (RETURN T))
		   (SETF (CAR NULL) (CAR L))
		   (SETF (CAR L) TEM))))))))

;Set up a process's wait condition, in both places. 
(DEFUN SET-PROCESS-WAIT (PROC FUN ARGS &AUX IDX APE)
  (WITHOUT-INTERRUPTS
    (SETF (PROCESS-WAIT-FUNCTION PROC) FUN)
    (SETF (PROCESS-WAIT-ARGUMENT-LIST PROC) ARGS)
    (COND ((NULL (SETQ APE (ASSQ PROC ACTIVE-PROCESSES))))
	  (T
	    (SETF (SECOND APE) FUN)
	    (COND (( (SETQ IDX (- ACTIVE-PROCESSES-ELEMENT-SIZE (LENGTH ARGS)))
		      ACTIVE-PROCESSES-PREFIX-SIZE)
		   (LET ((L (NTHCDR IDX APE)))
		     (SETF (THIRD APE) L)
		     (DO ((L L (CDR L))
			  (ARGS ARGS (CDR ARGS)))
			 ((NULL ARGS))
		       (RPLACA L (CAR ARGS)))))
		  (T (SETF (THIRD APE) ARGS)))))))

(DEFSTRUCTCLASS PROCESS OBJECT-CLASS)

;This function is the named-structure handler for processes
(DEFUN PROCESS (OP &OPTIONAL X &REST ARGS)
    (SELECTQ OP
	     (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
	     ((:PRINT :PRINT-SELF)
	      (FORMAT (CAR ARGS) "#<PROCESS ~A ~O>"
				 (PROCESS-NAME X)
				 (%POINTER X)))
	     (OTHERWISE (ERROR OP "I have never heard of "))))

;The fundamental methods of processes are :RESET, :FORCE-BREAK,
; :ACTIVE-P, :RUN-REASON, :REVOKE-RUN-REASON, :ARREST-REASON,
; :REVOKE-ARREST-REASON.

;The following global variables pertain to processes
;and are maintained by their methods.
;They should not be changed except with interrupts off.

;ACTIVE-PROCESSES is the alist of all runable processes.
;  Since this is used by the scheduler, everything on it must be a real
;  live process, not just something that accepts these messages.

;The defstruct components of processes are not used outside of this page
;except in the functions PROCESS-SCHEDULER and PROCESS-WAIT, and in LTOP.
;[Above is probably not true.]

(DEFUN PROCESS-CREATE (NAME &OPTIONAL IGNORE &REST OPTIONS &AUX PROC)
    (SETQ PROC (MAKE-PROCESS PROCESS-NAME NAME
			     PROCESS-STACK-GROUP
			     (LEXPR-FUNCALL (FUNCTION MAKE-STACK-GROUP)
					    NAME ':SAFE 0 OPTIONS)))
    (SETF (PROCESS-INITIAL-STACK-GROUP PROC)
	  (PROCESS-STACK-GROUP PROC))
    PROC)

(DEFUN PROCESS-PRESET (PROCESS INITIAL-FUNCTION &REST ARGUMENTS)
    (LEXPR-FUNCALL '<- PROCESS ':PRESET INITIAL-FUNCTION ARGUMENTS))

;This used to not unwind the stack-group of the process, but I
;thought that was a bad idea, so I took it out.  --Moon
(DEFMETHOD (PROCESS-CLASS :PRESET) (INITIAL-FUNCTION &REST ARGUMENTS)
    (SETF (PROCESS-INITIAL-FORM SELF)
	  (CONS INITIAL-FUNCTION (APPEND ARGUMENTS NIL)))
    (<- SELF ':RESET))

;This is the real initial function of all processes' initial stack groups.
;Its purpose is to make sure that the error handler C-Z command works.
;It also prevents anything bad from happening if the specified top-level returns
;and arranges for typing out to do "background" stuff.
(DEFUN PROCESS-TOP-LEVEL (&OPTIONAL IGNORE)
    (DO ((TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))
	(NIL)
       (*CATCH 'TOP-LEVEL
	       (PROGN (APPLY (CAR (PROCESS-INITIAL-FORM CURRENT-PROCESS))
                             (CDR (PROCESS-INITIAL-FORM CURRENT-PROCESS)))
		      (PROCESS-FLUSH-BACKGROUND-STREAM)
		      (PROCESS-WAIT-FOREVER)))))

(DEFUN PROCESS-FLUSH-BACKGROUND-STREAM ()
  (COND ((AND (NEQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM)
	      (TYPEP TERMINAL-IO 'TV:SHEET))
	 (AND (GET-HANDLER-FOR TERMINAL-IO ':WAIT-UNTIL-SEEN)
	      (FUNCALL TERMINAL-IO ':WAIT-UNTIL-SEEN))
	 (FUNCALL TERMINAL-IO ':DEACTIVATE)
	 (DEALLOCATE-RESOURCE 'TV:BACKGROUND-LISP-INTERACTORS TERMINAL-IO)
	 (SETQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))))

;This is a list of processes which may be recycled by PROCESS-RUN-FUNCTION
;It exists to avoid excess consing of stacks and reclaiming of them via
;the ordinary garbage collector.
(DEFVAR PROCESS-RUN-FUNCTION-SPARE-PROCESSES NIL)

;; Run a function in its own process
(DEFUN PROCESS-RUN-FUNCTION (NAME FUNCTION &REST ARGS &AUX PROCESS)
  (SETQ PROCESS (WITHOUT-INTERRUPTS (OR (POP PROCESS-RUN-FUNCTION-SPARE-PROCESSES)
					(PROCESS-CREATE NAME NIL
							':SPECIAL-PDL-SIZE 4000
							':REGULAR-PDL-SIZE 15000))))
  (SETF (PROCESS-NAME PROCESS) NAME)
  (SETF (SG-NAME (PROCESS-INITIAL-STACK-GROUP PROCESS)) NAME)
  (LEXPR-FUNCALL (FUNCTION PROCESS-PRESET)
                 PROCESS
                 (FUNCTION PROCESS-RUN-FUNCTION-INTERNAL)
                 FUNCTION
                 ARGS)
  (PROCESS-ENABLE PROCESS)
  PROCESS)

(DEFUN PROCESS-RUN-FUNCTION-INTERNAL (FUNCTION &REST ARGS)
  (*CATCH 'TOP-LEVEL (APPLY FUNCTION ARGS))
  ;; When the function returns, disable this process and make it available
  ;; for re-use.
  (PROCESS-FLUSH-BACKGROUND-STREAM)
  (LET ((PROCESS CURRENT-PROCESS))
    (WITHOUT-INTERRUPTS
      (PROCESS-DISABLE PROCESS)
      (PUSH PROCESS PROCESS-RUN-FUNCTION-SPARE-PROCESSES))))

(DEFUN PROCESS-RESET (PROCESS) (<- PROCESS ':RESET))

;This really ought to find all of the process's stack-groups and unwind them.
(DEFMETHOD (PROCESS-CLASS :RESET) (&OPTIONAL NOUNWIND &AUX SG)
  (WITHOUT-INTERRUPTS
    (COND ((NEQ SELF CURRENT-PROCESS)
	   ;; Wake up
	   (SET-PROCESS-WAIT SELF #'TRUE NIL)
	   (SETF (PROCESS-WHOSTATE SELF) "RUN")
	   ;; Reset a few minor things
	   (SETF (PROCESS-STACK-GROUP SELF) (SETQ SG (PROCESS-INITIAL-STACK-GROUP SELF)))
	   (TV:WHO-LINE-PROCESS-CHANGE SELF)
	   ;; Note -- the following code is not logically necessary, however
	   ;; it is necessary to make the cold-load come up when EH:UNWIND-SG
	   ;; is not loaded yet.  We avoid unwinding the stack-group if it
	   ;; has just been created.
	   (LET ((ST (SG-CURRENT-STATE SG)))
	     (COND ((OR (= ST SG-STATE-AWAITING-INITIAL-CALL) (= ST 0))
		    (SETQ NOUNWIND T))))
	   ;; Cause it, when next scheduled, to unwind itself and
	   ;; call its initial function.
           (COND ((EQ %CURRENT-STACK-GROUP SG)
		  ) ;Something fishy going on, ignore
		 ((NOT NOUNWIND)
		  (EH:UNWIND-SG SG (FUNCTION PROCESS-TOP-LEVEL) NIL T))
		 (T
		  (STACK-GROUP-PRESET SG (FUNCTION PROCESS-TOP-LEVEL))))))))

(DEFMETHOD (PROCESS-CLASS :FORCE-BREAK) ()
 (WITHOUT-INTERRUPTS
   (LET ((SG (PROCESS-STACK-GROUP SELF)))
     (LET ((STATE (SG-CURRENT-STATE SG)))
       (SELECT STATE
	  (SG-STATE-RESUMABLE
	    (FORCE-BREAK SG NIL))
	  (SG-STATE-AWAITING-RETURN
	    (COND ((PROCESS-WAIT-FUNCTION SELF)
		   (FORCE-BREAK SG T)		;Call PROCESS-WAIT again
		   (SET-PROCESS-WAIT SELF NIL NIL))
		  (T (FORCE-BREAK SG NIL))))
	  (OTHERWISE STATE))))))		;In case you're tracing this method

;REINVOKE-P will cause currently executing function to get reinvoked with the
; same args it has now.
(DEFUN FORCE-BREAK (SG REINVOKE-P)
  (EH:SG-SAVE-STATE SG)
  (EH:SG-OPEN-CALL-BLOCK SG 0 'FORCED-BREAK-HANDLER)
  (EH:SG-REGPDL-PUSH REINVOKE-P SG)
  (%P-STORE-CDR-CODE (AP-1 (SG-REGULAR-PDL SG)	;Terminate arg list
			   (SG-REGULAR-PDL-POINTER SG))
		     CDR-NIL)
  (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN))

(DEFUN FORCED-BREAK-HANDLER (REINVOKE-P)   ;Call error handler. This call frame is flushed
   (FUNCALL %ERROR-HANDLER-STACK-GROUP  ;by the EH:RESTORE-SG-STATE done by EH:SECOND-LEVEL
	    `(:BREAK ,REINVOKE-P)))	 ;when it sees a :BREAK message.

;; Make a process ready to run (but don't make it active if it wasn't).
;; Returns T always.
;; Doesn't update the who-line, because it is desirable for that usually to
;; happen INSIDE the process the who-line is watching, so that the correct
;; bindings of symbols such as PACKAGE will be in effect.
(DEFUN PROCESS-RUN-STATE (PROC)
    (SETF (PROCESS-WHOSTATE PROC) "RUN")
    (SET-PROCESS-WAIT PROC NIL NIL)
    T)

;Make a process wait until it is reset.
;This is useful in message handlers that want to synchronize
;with processes internal to the object receiving the message.
(DEFMETHOD (PROCESS-CLASS :FLUSH) ()
    (COND ((EQ SELF CURRENT-PROCESS))
	  (T
	   (SET-PROCESS-WAIT SELF #'FALSE NIL)
	   (SETF (PROCESS-WHOSTATE SELF) "FLUSHED"))))

;; This function is useful in the error handler when
;; a process's wait function causes an error in the scheduler.
;; By blasting it (it is the current process at that time),
;; you can prevent the error from happening again.
;; C-Z will restart the scheduler, and then you can type CALL.
(DEFUN PROCESS-BLAST (&OPTIONAL (PROC CURRENT-PROCESS))
  (SET-PROCESS-WAIT PROC #'FALSE NIL))

(DEFUN PROCESS-ENABLE (PROCESS &OPTIONAL (REASON ':USER))
    (<- PROCESS ':RUN-REASON REASON))

(DEFMETHOD (PROCESS-CLASS :ACTIVE-P) ()
    (ASSQ SELF ACTIVE-PROCESSES))

(DEFMETHOD (PROCESS-CLASS :RUNNABLE-P) ()
    (ASSQ SELF ACTIVE-PROCESSES))

(DEFMETHOD (PROCESS-CLASS :RUN-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (OR (MEMQ REASON (PROCESS-RUN-REASONS SELF))
	(PROGN (PUSH REASON (PROCESS-RUN-REASONS SELF))
	       (OR (PROCESS-ARREST-REASONS SELF)
		   (ASSQ SELF ACTIVE-PROCESSES)
		   (PROGN (PROCESS-ACTIVE-ENTRY SELF)
			  ;; If process's stack group is in a bad state,
			  ;; make it wait instead of actually running (unless it's current!).
			  ;; ACTIVE is a bad state for a process which isn't running!
			  (AND (LET ((STATE (SG-CURRENT-STATE (PROCESS-STACK-GROUP SELF))))
				 (OR (= STATE SG-STATE-ERROR)
				     (= STATE SG-STATE-ACTIVE)
				     (= STATE SG-STATE-EXHAUSTED)))
			       CURRENT-PROCESS ;Prevents lossage in PROCESS-INITIALIZE.
			       (<- SELF ':FLUSH))
			  (TV:WHO-LINE-PROCESS-CHANGE SELF)))))))

(DEFUN PROCESS-DISABLE (PROCESS &OPTIONAL (REASON ':USER))
    (<- PROCESS ':REVOKE-RUN-REASON REASON))

(DEFMETHOD (PROCESS-CLASS :REVOKE-RUN-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (OR (SETF (PROCESS-RUN-REASONS SELF)
	      (DELQ REASON (PROCESS-RUN-REASONS SELF)))
	(PROGN (PROCESS-REMOVE-ACTIVE-ENTRY SELF)
	       (TV:WHO-LINE-PROCESS-CHANGE SELF)))))

(DEFMETHOD (PROCESS-CLASS :RUN-REASONS) ()
  (PROCESS-RUN-REASONS SELF))

(DEFMETHOD (PROCESS-CLASS :ARREST-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (OR (MEMQ REASON (PROCESS-ARREST-REASONS SELF))
	(PROGN (PUSH REASON (PROCESS-ARREST-REASONS SELF))
	       (PROCESS-REMOVE-ACTIVE-ENTRY SELF)
	       (TV:WHO-LINE-PROCESS-CHANGE SELF)))))

(DEFMETHOD (PROCESS-CLASS :REVOKE-ARREST-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
      (AND (NOT (SETF (PROCESS-ARREST-REASONS SELF)
		      (DELQ REASON (PROCESS-ARREST-REASONS SELF))))
	   (PROCESS-RUN-REASONS SELF)
	   (NOT (ASSQ SELF ACTIVE-PROCESSES))
	   (PROGN (PROCESS-ACTIVE-ENTRY SELF)
		  ;; If process's stack group is in a bad state,
		  ;; make it wait instead of actually running (unless it's current!).
		  ;; ACTIVE is a bad state for a process which isn't running!
		  (AND (LET ((STATE (SG-CURRENT-STATE (PROCESS-STACK-GROUP SELF))))
			 (OR (= STATE SG-STATE-ERROR)
			     (= STATE SG-STATE-ACTIVE)
			     (= STATE SG-STATE-EXHAUSTED)))
		       (<- SELF ':FLUSH))
		  (TV:WHO-LINE-PROCESS-CHANGE SELF)))))

(DEFMETHOD (PROCESS-CLASS :ARREST-REASONS) ()
  (PROCESS-ARREST-REASONS SELF))

;A Parallelism is a way of grouping several processes so that they
;can be turned on and off at once, as if they were one process.
;Make a parallelism out of several processes, and hand it to someone
;who expects to deal with only a single process.

;Of the processes in the parallelism, any subset may be enabled.
;Only those will be run when the parallelism is told to run.
;However, arresting the parallelism will arrest all processes in it,
;enabled or not.
;One of the processes in the parallelism is the input process.
;It is the one which can read input for the parallelism,
;and which is described in the who-line.

(DEFCLASS PARALLELISM-CLASS OBJECT-CLASS
	  (NAME PROCESSES ENABLED-PROCESSES RUN-REASONS ARREST-REASONS))

(DEFMETHOD (PARALLELISM-CLASS :NAME) () NAME)
(DEFMETHOD (PARALLELISM-CLASS :PROCESSES) () PROCESSES)
(DEFMETHOD (PARALLELISM-CLASS :ENABLED-PROCESSES) () ENABLED-PROCESSES)
(DEFMETHOD (PARALLELISM-CLASS :RUN-REASONS<-) (&REST IGNORE)
  (FERROR NIL ":RUN-REASONS<- is not allowed"))
(DEFMETHOD (PARALLELISM-CLASS :ARREST-REASONS<-) (&REST IGNORE)
  (FERROR NIL ":ARREST-REASONS<- is not allowed"))

(DEFMETHOD (PARALLELISM-CLASS :PROCESSES<-) (PROCS)
    (COND (ARREST-REASONS
	   (DOLIST (P PROCESSES)
	     (OR (MEMQ P PROCS)
		 (<- P ':REVOKE-ARREST-REASON SELF)))
	   (SETQ PROCESSES PROCS)
	   (DOLIST (P PROCESSES)
	     (<- P ':ARREST-REASON SELF)))
	  (T (SETQ PROCESSES PROCS))))

(DEFMETHOD (PARALLELISM-CLASS :ENABLED-PROCESSES<-) (PROCS)
    (COND (RUN-REASONS
	   (DOLIST (P ENABLED-PROCESSES)
	     (<- P ':REVOKE-RUN-REASON SELF))))
    (SETQ ENABLED-PROCESSES NIL)
    (DOLIST (P PROCS)
      (FUNCALL SELF ':ENABLE P)))

(DEFMETHOD (PARALLELISM-CLASS :ADD-PROCESS) (PROCESS)
    (OR (MEMQ PROCESS PROCESSES)
	(PROGN (PUSH PROCESS PROCESSES)
	       (AND ARREST-REASONS
		    (<- PROCESS ':ARREST-REASON SELF)))))

(DEFMETHOD (PARALLELISM-CLASS :RESET) ()
    (DO L PROCESSES (CDR L) (NULL L)
       (<- (CAR L) ':RESET)))

(DEFMETHOD (PARALLELISM-CLASS :TO-ALL-ENABLED) (&REST MESSAGE)
    (DO L ENABLED-PROCESSES (CDR L) (NULL L)
	(LEXPR-FUNCALL '<- (CAR L) MESSAGE)))

(DEFMETHOD (PARALLELISM-CLASS :TO-ALL-PROCESSES) (&REST MESSAGE)
    (DO L PROCESSES (CDR L) (NULL L)
	(LEXPR-FUNCALL '<- (CAR L) MESSAGE)))

(DEFMETHOD (PARALLELISM-CLASS :ENABLE) (PROCESS)
    (<- SELF ':ADD-PROCESS PROCESS)
    (COND ((MEMQ PROCESS ENABLED-PROCESSES))
	  (T (PUSH PROCESS ENABLED-PROCESSES)
	     (AND RUN-REASONS
		  (<- PROCESS ':RUN-REASON SELF)))))

(DEFMETHOD (PARALLELISM-CLASS :DISABLE) (PROCESS)
    (AND (MEMQ PROCESS ENABLED-PROCESSES)
	 (PROGN (SETQ ENABLED-PROCESSES
		      (DELQ PROCESS ENABLED-PROCESSES))
		(<- PROCESS ':REVOKE-RUN-REASON SELF))))

(DEFMETHOD (PARALLELISM-CLASS :RUN-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (OR RUN-REASONS
	(<-AS PARALLELISM-CLASS ':TO-ALL-ENABLED ':RUN-REASON SELF))
    (OR (MEMQ REASON RUN-REASONS)
	(PUSH REASON RUN-REASONS))))

(DEFMETHOD (PARALLELISM-CLASS :REVOKE-RUN-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (OR (SETQ RUN-REASONS (DELQ REASON RUN-REASONS))
	(<-AS PARALLELISM-CLASS ':TO-ALL-ENABLED ':REVOKE-RUN-REASON SELF))))

(DEFMETHOD (PARALLELISM-CLASS :ARREST-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (OR ARREST-REASONS
	(<-AS PARALLELISM-CLASS ':TO-ALL-PROCESSES ':ARREST-REASON SELF))
    (OR (MEMQ REASON ARREST-REASONS)
	(PUSH REASON ARREST-REASONS))))

(DEFMETHOD (PARALLELISM-CLASS :REVOKE-ARREST-REASON) (&OPTIONAL (REASON ':USER))
  (WITHOUT-INTERRUPTS
    (OR (SETQ ARREST-REASONS (DELQ REASON ARREST-REASONS))
	(<-AS PARALLELISM-CLASS ':TO-ALL-PROCESSES ':REVOKE-ARREST-REASON SELF))))

(DEFMETHOD (PARALLELISM-CLASS :ACTIVE-P) ()
    (AND RUN-REASONS (NOT ARREST-REASONS)))

(DEFMETHOD (PARALLELISM-CLASS :RUNNABLE-P) ()
    (AND RUN-REASONS (NOT ARREST-REASONS)))

;; The scheduler.

;; This is the function which runs in the scheduler stack group.
;; It cycles circularly around the processes performing two phases:
;; (I) If the process is runnable (and if there are any processes), run it.
;; (II) Run the simulated-clock list if it is time to do so.
;; Note that if there are no processes, it still runs the clock list.

;; This code makes NO attempt to know about interrupts nor
;; sequence-breaks; there is no pre-emption.  It is intended for the
;; prototype machine.  The who-line is updated once a second.

(DEFUN PROCESS-SCHEDULER ()
    (DO ((PROCL NIL (COND ((NULL PROCL) ACTIVE-PROCESSES)
                          (T (CDR PROCL))))
	 (INHIBIT-SCHEDULING-FLAG T)   ;The scheduler stack group must always have
				       ; this binding in effect.  Otherwise, a
				       ; recursive sequence-break could try to happen
				       ; (which would cause a ILLOP).
	 (IDLE-P NIL)
         (LAST-TIME 0)		;Last time at which we ran the clock functions.
	 (LAST-WHO-TIME 0)	;Last time the who-line was updated
	 (PROC))
	(NIL)	     
      ;; Phase I of the scheduler cycle: see if the process is runnable, and, if so,
      ;; run it and wait until it comes back.
      (COND ((OR (NULL PROCL)	;If got to end of process list, then if no processes
		 (NULL (CAR (SETQ PROC (CAR PROCL)))))
             (AND IDLE-P        ; had been runnable, run the garbage collector
                  (NOT INHIBIT-IDLE-SCAVENGING-FLAG)
                  (%GC-SCAVENGE GC-IDLE-SCAVENGE-QUANTUM))
             (SETQ IDLE-P T))
            (T                ;Only do Phase I if there is a process.
	       (SETQ CURRENT-PROCESS (CAR PROC))	;Wait functions can look at this.
	       (COND ((OR (NULL (SECOND PROC))
			  (AND (APPLY (SECOND PROC) (THIRD PROC))
			       ;; Predicate returned truth, so it is runnable.
			       (PROCESS-RUN-STATE (CAR PROC))))
		      (COND (SCHEDULER-HOOK
			      (FUNCALL SCHEDULER-HOOK (CAR PROC))))
		      (%XBUS-WRITE TV:WHO-LINE-RUN-LIGHT-LOC 37777777)
		      (FUNCALL (PROCESS-STACK-GROUP (CAR PROC)))
		      (%XBUS-WRITE TV:WHO-LINE-RUN-LIGHT-LOC 0)
                      (SETQ IDLE-P NIL)

		      ;; Remember which stack-group he was in when he called us.
		      (SETF (PROCESS-STACK-GROUP CURRENT-PROCESS)
			    %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)))))

      (SETQ CURRENT-PROCESS NIL)
      ;In case we take a page fault, the microcode will turn the run light on.
      ;So turn it back off.  This is a kind of kludge, but...
      (%XBUS-WRITE TV:WHO-LINE-RUN-LIGHT-LOC 0)

      ;; Phase II of the scheduler cycle:  if enough time has past,
      ;; do the forms on the simulated-clock list.
      (LET ((NOW (TIME)))
	(COND ((NOT (= NOW LAST-TIME))
	       (DO L CLOCK-FUNCTION-LIST (CDR L) (NULL L)
		   (FUNCALL (CAR L)))
	       (SETQ LAST-TIME NOW)))
	(COND ((AND ( (TIME-DIFFERENCE NOW LAST-WHO-TIME) 60.) (BOUNDP 'TV:WHO-LINE-LIST))
	       (TV:WHO-LINE-UPDATE)
	       (SETQ LAST-WHO-TIME NOW))))
      ;; If a chaosnet packet has been read in by ucode, gobble it down.
      ;; This avoids switching to a separate RECEIVER processor, and thus
      ;; touches a few less pages.
      (COND ((AND (BOUNDP 'CHAOS:ENABLE)
		  CHAOS:ENABLE
		  (CHAOS:INT-RECEIVE-LIST))
	     (CHAOS:RECEIVE-ANY-FUNCTION)))
      ))

;If anyone wants this they should convert it to the new window-system
;(DEFUN DEFAULT-SCHEDULER-HOOK-FUNCTION (PROC)
;  (LET ((X (- (SCREEN-WIDTH TV-CPT-SCREEN) 200))
;	(Y (- (SCREEN-HEIGHT TV-CPT-SCREEN) 40)))
;    (TV-SELECT-SCREEN TV-CPT-SCREEN)
;    (TV-ERASE 200 14 X Y TV-ALU-ANDCA)
;    (TV-STRING-OUT-EXPLICIT (PROCESS-NAME PROC)
;			    (+ X 2) (+ Y 2))))

;;; Miscellaneous process synchronization functions

(DEFUN PROCESS-ALLOW-SCHEDULE ()
   (FUNCALL SCHEDULER-STACK-GROUP)
   (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS))	;Mark as running in who-line

;; Takes one argument, a number of 60ths of a second for which to sleep.
(DEFUN PROCESS-SLEEP (INTERVAL)
  (PROCESS-WAIT "SLEEP" #'(LAMBDA (START-TIME INTERVAL)
			     ( (TIME-DIFFERENCE (TIME) START-TIME)
			        INTERVAL))
		        (TIME) INTERVAL))

;PROCESS-WAIT is in LTOP since it has to be in the cold load.

(DEFUN PROCESS-WAIT-FOREVER ()
    (PROCESS-WAIT "Wait forever" (FUNCTION FALSE)))

;; A lock may be any cell.  When a lock is in the unlocked state, the cell
;; contains NIL; otherwise the cell contains the process which locked the lock.
;; A lock is referred to by a locative pointer to the cell.

;; Lock the given lock, blocking until it is sucessfully locked.
(DEFUN PROCESS-LOCK (LOCATIVE-POINTER &OPTIONAL (LOCK-VALUE CURRENT-PROCESS))
  (DO ((LOCKER (CAR LOCATIVE-POINTER) (CAR LOCATIVE-POINTER)))
      ((%STORE-CONDITIONAL LOCATIVE-POINTER NIL LOCK-VALUE))
    (AND (EQ LOCKER LOCK-VALUE)
	 (FERROR NIL "Lock ~S already locked by this process" LOCATIVE-POINTER))
    (PROCESS-WAIT "LOCK"
		  (FUNCTION (LAMBDA (BAD-CONTENTS POINTER)
				    (NEQ (CAR POINTER) BAD-CONTENTS)))
		  LOCKER
		  LOCATIVE-POINTER)))

;; Unlock the given lock.  The unlocker must be the same as the locker.
(DEFUN PROCESS-UNLOCK (LOCATIVE-POINTER &OPTIONAL (LOCK-VALUE CURRENT-PROCESS))
  (OR (%STORE-CONDITIONAL LOCATIVE-POINTER LOCK-VALUE NIL)
      (FERROR NIL "Attempt to unlock ~S, which you don't have locked" LOCATIVE-POINTER)))

;; This function initializes the processes and scheduler.
(DEFUN PROCESS-INITIALIZE ()
  (COND ((NOT SCHEDULER-EXISTS)
	 (SETQ SCHEDULER-STACK-GROUP (MAKE-STACK-GROUP "SCHEDULER" ':SAFE 0))
	 (SETQ TOP-PROCESS
	       (MAKE-PROCESS PROCESS-NAME "TOP-PROCESS"
			     PROCESS-STACK-GROUP %CURRENT-STACK-GROUP
			     PROCESS-INITIAL-STACK-GROUP %CURRENT-STACK-GROUP
			     PROCESS-INITIAL-FORM '(LISP-TOP-LEVEL2)
			     ))))
  ;; Below here is done every time the machine starts up.
  ;; Totally discard all the state in the process that was active
  ;; before a warm boot, because it's garbage now.
  (COND ((SETQ WARM-BOOTED-PROCESS (AND (BOUNDP 'CURRENT-PROCESS)
					CURRENT-PROCESS))
	 (<- (PROG1 CURRENT-PROCESS (SETQ CURRENT-PROCESS NIL)) ':RESET 'NOUNWIND)))
  (SETQ CURRENT-PROCESS NIL)	;Prevent :RESET, :FLUSH from losing.
  ;; Don't let any process run again until someone blesses it.
  (DOLIST (P ACTIVE-PROCESSES) (AND (CAR P) (<- (CAR P) ':FLUSH)))
  (<- TOP-PROCESS ':RESET)
  (<- TOP-PROCESS ':RUN-REASON ':USER)
  (SETF (PROCESS-ARREST-REASONS TOP-PROCESS) NIL)
  (PROCESS-RUN-STATE TOP-PROCESS)
  (SETQ %SCHEDULER-STACK-GROUP SCHEDULER-STACK-GROUP)	;Tell microcode
  (STACK-GROUP-PRESET SCHEDULER-STACK-GROUP (FUNCTION PROCESS-SCHEDULER))
	;Do next line before calling the scheduler if you expect it ever to return
  (SETQ SCHEDULER-EXISTS T)	     ;Everything is set up now, PROCESS-WAIT may schedule
  (FUNCALL SCHEDULER-STACK-GROUP)    ;Get it into awaiting-return so PROCESS-WAIT will
				     ; be happy
  (SETQ INHIBIT-SCHEDULING-FLAG NIL)
  (SB-ON ':CLOCK))

(DEFUN SB-ON (&OPTIONAL (WHEN 'JUST-SHOW-CURRENT-STATE)
	      &AUX MASK TEM
	      (ALIST '( (:CALL . 1) (:KEYBOARD . 2) (:CHAOS . 4) (:CLOCK . 10) )))
  "Sets the sequence break enable flags:
	The argument can be a keyword, a list of keywords, or a numeric mask.
	Keywords are: :CALL, :KEYBOARD, :CHAOS, :CLOCK
	With no argument, just returns a list of keywords for what is enabled.
	Argument of NIL means turn off sequence breaks."
  (COND ((NUMBERP WHEN) (SETQ MASK WHEN))
	((NULL WHEN) (SETQ MASK 0))
	((EQ WHEN 'JUST-SHOW-CURRENT-STATE) (SETQ MASK %SEQUENCE-BREAK-SOURCE-ENABLE))
	((ATOM WHEN)
	 (OR (SETQ MASK (CDR (ASSQ WHEN ALIST)))
	     (FERROR NIL "~S invalid keyword.  Use :CALL, :KEYBOARD, :CHAOS, or :CLOCK"
		         WHEN)))
	(T (SETQ MASK 0)
	   (DOLIST (KWD WHEN)
	     (IF (SETQ TEM (CDR (ASSQ KWD ALIST)))
		 (SETQ MASK (LOGIOR MASK TEM))
		 (FERROR NIL "~S invalid keyword.  Use :CALL, :KEYBOARD, :CHAOS, or :CLOCK"
			     KWD)))))
  ;; Warm booting turns off sequence breaks, so reset them when this happens.
  (COND ((EQ WHEN 'JUST-SHOW-CURRENT-STATE))
	((NULL WHEN) (DELETE-INITIALIZATION "Sequence Breaks" '(:WARM)))
	(T (ADD-INITIALIZATION "Sequence Breaks" `(SI:SB-ON ,MASK)) '(:WARM)))
  (SETQ %SEQUENCE-BREAK-SOURCE-ENABLE MASK)
  (DO ((L NIL)
       (B 1 (LSH B 1)))
      ((ZEROP MASK) L)
    (AND (BIT-TEST B MASK)
	 (PUSH (IF (SETQ TEM (CAR (RASSOC B ALIST))) TEM B) L))
    (SETQ MASK (BOOLE 2 B MASK))))

;NORMAL is to prevent this from going off during loading.
(ADD-INITIALIZATION "PROCESS" '(PROCESS-INITIALIZE) '(:SYSTEM :NORMAL))
