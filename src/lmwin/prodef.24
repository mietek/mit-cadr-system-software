;;; -*- Mode: LISP;  Package: SYSTEM-INTERNALS;  Base: 8 -*-

;;; Processes

(DEFVAR CURRENT-PROCESS)		;The process which is currently executing.
(DEFVAR INITIAL-PROCESS)		;The first process made
;DEFVAR'ed in PROCES
;(DEFVAR ACTIVE-PROCESSES)		;Alist of all processes being considered for running
					; and their wait conditions.  This list structure is
					; all in contiguous memory to decrease the size of
					; the scheduler's working set.
(DEFVAR ALL-PROCESSES NIL)		;A list of all processes that have not bee "killed"
(DEFVAR PROCESS-ACTIVE-LENGTH 30.)	;Initial length of ACTIVE-PROCESSES
(DEFVAR WARM-BOOTED-PROCESS NIL)	;When you warm boot

;;; Scheduling

(DEFVAR INHIBIT-SCHEDULING-FLAG)	;Inhibits clock and process-switching
(DEFVAR CLOCK-FUNCTION-LIST NIL)	;At clock time, each element is funcalled on the
					; number of 60ths that have elapsed recently.
(DEFVAR SCHEDULER-STACK-GROUP)		;The stack group in which the scheduler runs.
(DEFVAR SCHEDULER-EXISTS NIL)		;T if the scheduler and processes are set up.
(DEFVAR INHIBIT-IDLE-SCAVENGING-FLAG)	;If NIL scavenger runs when no processes runnable
(DEFVAR GC-IDLE-SCAVENGE-QUANTUM 10.)	;Argument to %GC-SCAVENGE used in that case
(DEFVAR DEFAULT-QUANTUM 60.)		;Defaultly run each process for at least one second

(DEFVAR SYSTEM-BEING-INITIALIZED-FLAG T) ;T while coming up, mainly for error-handler

;;; Processes
(DEFFLAVOR PROCESS
 (NAME				;Print name
  STACK-GROUP			;Stack group currently executing on behalf of this process
  (WAIT-FUNCTION (FUNCTION FALSE)) ;Predicate to determine if process is runnable
  (WAIT-ARGUMENT-LIST NIL)	;Arguments passed to above (use an arg to avoid a closure)
				; This will often be a rest argument in somebody's stack,
				; but it will always be used in a safe manner.
  (WHOSTATE "Just Created")	;The "WHOSTATE" string for the who line, etc.
  INITIAL-STACK-GROUP		;The stack group which PROCESS-RESET (q.v.) will reset to.
  INITIAL-FORM			;Form to preset the initial stack group to when proc is reset.
				; Really cons of function and evaluated args.
  (RUN-REASONS NIL)		;List of run reasons for this process.
  (ARREST-REASONS NIL)		;List of arrest reasons for this process.
  (QUANTUM DEFAULT-QUANTUM)	;Number of ticks process should run at most before
				; running another process.
  (QUANTUM-REMAINING 0)		;Amount of time remaining for this process to run.
  (PRIORITY 0)			;Absolute priority of this process.  The larger the number,
				; the more this process wants to run.  It will never be
				; run for more than its quantum, though.
  (WARM-BOOT-ACTION		;Thing to do to this process if it is active when the
   'PROCESS-WARM-BOOT-RESTART)	; machine is warm-booted.  NIL means the default action
				; (flush it).  If non-NIL, gets funcalled with the process
				; as its argument.
  (SIMPLE-P NIL)		;T if the process is simple (has no stack group)
  )
  ()
  :ORDERED-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:SETTABLE-INSTANCE-VARIABLES WARM-BOOT-ACTION)
  :INITABLE-INSTANCE-VARIABLES
  (:INIT-KEYWORDS :FLAVOR
		  ;; Keywords for stack group
		  :SG-AREA :REGULAR-PDL-AREA :SPECIAL-PDL-AREA :REGULAR-PDL-SIZE
		  :SPECIAL-PDL-SIZE :CAR-SYM-MODE :CAR-NUM-MODE :CDR-SYM-MODE :CDR-NUM-MODE
		  :SWAP-SV-ON-CALL-OUT :SWAP-SV-OF-SG-THAT-CALLS-ME :TRAP-ENABLE :SAFE))

;;; Special kind of process that does not a require a stack group
(DEFFLAVOR SIMPLE-PROCESS () (PROCESS)
  (:DEFAULT-INIT-PLIST :SIMPLE-P T
    		       :WAIT-FUNCTION #'TRUE))

;;; Processes that do coroutining
(DEFFLAVOR COROUTINING-PROCESS ((COROUTINE-STACK-GROUPS NIL)) (PROCESS)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES)
