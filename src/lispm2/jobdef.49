;;; -*-LISP-*- Machine Process, Job, Window, Screen definitions
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;; Associates names with jobs.  Not all jobs need be on it.
(DEFVAR JOB-ALIST NIL)

;; The window that is selected when you hit CALL.
(DEFVAR TOP-WINDOW)
;; The window which has been selected by the user.
;; The process of that window is the SELECTED-PROCESS.
(DEFVAR SELECTED-WINDOW)
;; The list of windows trying to be displayed.
;; The order of this list is "highest in the pile on the desk" first.
(DEFVAR ACTIVE-WINDOWS)

;; The list of windows which are at the top surface, i.e.
;;  their entire area is visible.  The order of this
;;  list is most likely to be displaced first.
(DEFVAR EXPOSED-WINDOWS)

;; This turns off the copying of bits to and from
;; the screen when windows are exposed and deexposed.
;; Deexposed windows are not saved, and nothing is done
;; to the screen when a window is exposed.
(DEFVAR INHIBIT-SCREEN-SAVING-FLAG NIL)

;; This makes windows which are exposed be just cleaned,
;; but deexposed windows are still saved.
(DEFVAR INHIBIT-SCREEN-RESTORATION-FLAG NIL)

;; This turns off screen blting globally.
;; It is suitable to set at top level.
;; Some things that reshape windows bind it to NIL.
(DEFVAR INHIBIT-ALL-SCREEN-BLTING-FLAG NIL)

;; Bind to T to prevent automatic exposing of non-hidden windows
;; and automatic selection of top window in the pile.
(DEFVAR INHIBIT-AUTOEXPOSE-FLAG NIL)

;; Assoc with each screen a pc-ppr
;; which is used to print labels of windows.
(DEFVAR SCREEN-PC-PPR-ALIST NIL)

;; Lock for changing screen layout.  Manipulate only via LOCK-SCREEN-LAYOUT.
(DEFVAR SCREEN-LAYOUT-LOCK)

;; Flag for WINDOW-UPDATE (q.v.) to do full redisplay.
(DEFVAR WINDOW-COMPLETE-REDISPLAY NIL)

;; Processes

(DEFVAR CURRENT-PROCESS)	    ;The process which is currently executing.
(DEFVAR SELECTED-PROCESS)	    ;The process which is allowed to use the keyboard.
(DEFVAR TOP-PROCESS)		    ;The process that the machine starts up in.
				    ;This is TOP-WINDOW's process.
				    ;Used only by PROCESS-INITIALIZE and WINDOW-INITIALIZE.
(DEFVAR TRIVIAL-PROCESS)	    ;A special process for trivial windows.  DON'T USE IT.
(DEFVAR PROCESS-ALIST NIL)	    ;Associates names with processes or parallelisms.  GOING AWAY.
(DEFVAR ACTIVE-PROCESSES)	    ;A list of all processes being considered for running.
(DEFVAR WHO-LINE-FROZEN NIL)	    ;T => don't move who-line around as new processes get selected.

;; The keyboard process

(DEFVAR KBD-INTERRUPT-PROCESS)	    ;The kbd-interrupt handler.
(DEFVAR KBD-INTERRUPT-KLUDGE)	    ;If non-nil, character or job to be selected, awakens k-p
(DEFVAR KBD-BUFFER)		    ;If non-nil, character read out of hardware but not
				    ; given to anyone yet.  Raw keyboard code.

;;Scheduling

(DEFVAR INHIBIT-SCHEDULING-FLAG)    ;Inhibits clock and process-switching
(DEFVAR KBD-SIMULATED-CLOCK-FCN-LIST)  ; At clock time, each elt is funcalled on nothing.
(DEFVAR SCHEDULER-STACK-GROUP)	    ;The stack group in which the scheduler runs.
(DEFVAR SCHEDULER-EXISTS)	    ;T if the scheduler and processes are set up.
(DEFVAR SCHEDULER-HOOK)		    ;If non-NIL, called with arg of process about to be run.
(DEFVAR INHIBIT-IDLE-SCAVENGING-FLAG) ;If NIL scavenger runs when no processes runnable
(DEFVAR GC-IDLE-SCAVENGE-QUANTUM)   ;Argument to %GC-SCAVENGE used in that case

(DEFVAR SYSTEM-BEING-INITIALIZED-FLAG)	;T while coming up, mainly for error-handler

;; The following structure is a "process".  It is not the same as a stack group.
;; A collection of stack groups usually form a process, and we simply
;; remember which stack group of the process was the one running when
;; the process was descheduled.
;; A process MUST be a named structure, because the scheduler uses it as one.

(DEFSTRUCT (PROCESS :ARRAY :NAMED)
    PROCESS-NAME		;Print name
    PROCESS-STACK-GROUP		;Stack group currently executing on behalf of this process
    (PROCESS-WAIT-FUNCTION	;Predicate to determine if process is runnable
        (FUNCTION FALSE))
    PROCESS-WAIT-ARGUMENT-LIST	;Arguments passed to above (use an arg to avoid a closure)
				;This will often be a rest argument in somebody's stack,
				;but it will always be used in a safe manner.
    (PROCESS-WHOSTATE "JUST CREATED")	;The "WHOSTATE" string for the who line or PEEK or whatever.
    PROCESS-INITIAL-STACK-GROUP ;The stack group which PROCESS-RESET (q.v.) will reset to.
    PROCESS-INITIAL-FORM	;Form to preset the initial stack group to when proc is reset.
    PROCESS-INPUT-PROCESS	;Non-NIL => it is a process (not a parallelism) to which
				;this process donates its permission to use the keyboard.
				;Often, processes in a parallelism should all point this way
				;to the one among them which reads input.
    PROCESS-FORCED-INPUT	;String or character of forced terminal input
    PROCESS-FORCED-INPUT-INDEX	;Index into above string, saying how far has already been read.
    PROCESS-RUN-REASONS		;List of run reasons for this process.
    PROCESS-ARREST-REASONS	;List of arrest reasons for this process.
    PROCESS-ERROR-STOP-PROCESSES
				;Processes or parallelisms to be stopped if
				;this process gets an error.
    PROCESS-PLIST		;Random properties.
    )

(DEFMACRO LOCK-SCREEN-LAYOUT BODY
    `(LET ((LOCK-SCREEN-LAYOUT (NEQ (CDR SCREEN-LAYOUT-LOCK) CURRENT-PROCESS)))
      (UNWIND-PROTECT
        (PROGN (AND LOCK-SCREEN-LAYOUT (PROCESS-LOCK SCREEN-LAYOUT-LOCK))
	       . ,BODY)
	(AND LOCK-SCREEN-LAYOUT (PROCESS-UNLOCK SCREEN-LAYOUT-LOCK)))))

(DEFMACRO MAYBE-LOCK-SCREEN-LAYOUT (CONDITION . BODY)
  `(LET ((LOCK-SCREEN-LAYOUT (AND ,CONDITION (NEQ (CDR SCREEN-LAYOUT-LOCK) CURRENT-PROCESS))))
     (UNWIND-PROTECT
       (PROGN (AND LOCK-SCREEN-LAYOUT (PROCESS-LOCK SCREEN-LAYOUT-LOCK))
	      . ,BODY)
       (AND LOCK-SCREEN-LAYOUT (PROCESS-UNLOCK SCREEN-LAYOUT-LOCK)))))

(DEFMACRO CHECK-LAYOUT-LOCKED ()
  '(OR (EQ (CDR SCREEN-LAYOUT-LOCK) CURRENT-PROCESS)
       (FERROR NIL "Not inside a LOCK-SCREEN-LAYOUT")))

(DEFCLASS WINDOW-CLASS OBJECT-CLASS
    (NAME SCREEN LEFT TOP RIGHT BOTTOM PROCESS STATUS BIT-SAVE-ARRAY FRAME))

(DEFCLASS WINDOW-WITH-BOX-CLASS WINDOW-CLASS
    (LABEL LABEL-POSITION LABEL-NEEDS-UPDATING-FLAG MARGINS))

(DEFCLASS WINDOW-WITH-PC-PPR-CLASS WINDOW-CLASS (PC-PPR))

(DEFCLASS WINDOW-WITH-PC-PPR-AND-BOX-CLASS WINDOW-WITH-PC-PPR-CLASS
    (LABEL LABEL-POSITION LABEL-NEEDS-UPDATING-FLAG MARGINS))

(DEFCLASS POP-UP-TEXT-WINDOW-CLASS WINDOW-WITH-PC-PPR-AND-BOX-CLASS
	(STREAM))  

(DEFCLASS JOB-CLASS OBJECT-CLASS (NAME WINDOWS ENABLED-WINDOWS))
(SPECIAL JOB-CLASS)

(DEFCLASS LISP-LISTENER-CLASS WINDOW-WITH-PC-PPR-CLASS (STREAM DEFAULT-PROCESS))

;See LISPM2;FRAME for documentation of these.
(DEFCLASS WINDOW-FRAME-CLASS WINDOW-CLASS
    (PANES SELECTED-PANE RESET-PANES RESET-SELECTED-PANE ACTIVE-FLAG
     LABEL LABEL-POSITION LABEL-NEEDS-UPDATING-FLAG MARGINS))

(DEFMACRO WINDOW-FRAME-SAVE-PANES (FRAME . BODY)
    `(PROG ((WINDOW-FRAME-SAVE-PANES-FRAME ,FRAME)
	    (WINDOW-FRAME-SAVE-PANES-OLD-PANES
	     (<- WINDOW-FRAME-SAVE-PANES-FRAME ':SAVE-PANES)))
	   (RETURN (UNWIND-PROTECT (PROGN . ,BODY)
				   (<- WINDOW-FRAME-SAVE-PANES-FRAME
				       ':RESTORE-PANES WINDOW-FRAME-SAVE-PANES-OLD-PANES)))))

(DEFCLASS WINDOW-SINGLE-FRAME-CLASS WINDOW-FRAME-CLASS ())

;See LISPM2;SCROLL for documentation of these.
(DEFCLASS SCROLL-TEXT-WINDOW-CLASS WINDOW-WITH-PC-PPR-CLASS
	  (LINE-TABLE
	   TOP-SCREEN-LINE
	   SCREEN-LINES
	   REDISPLAY-NEEDED				;T => UPDATE must do full hair.
	   STREAM))					;A stream for our pc-ppr.

(DEFSUBST SCROLL-ITEM-FUNCTION (ITEM) (CAAR ITEM))
(DEFSUBST SCROLL-ITEM-ARGLIST (ITEM) (CDAR ITEM))
(DEFSUBST SCROLL-ITEM-WIDTH (ITEM) (CADR ITEM))
(DEFSUBST SCROLL-ITEM-STARTING-X (ITEM) (CADDR ITEM))
(DEFSUBST SCROLL-ITEM-SCREEN-LINE (ITEM) (CADDDR ITEM))
(DEFSUBST SCROLL-ITEM-PREVIOUS-VALUE (ITEM) (CADR (CDDDR ITEM)))

(DEFSUBST SCROLL-LINE-CONTENTS (LTE) (CAR LTE))
(DEFSUBST SCROLL-LINE-SCREEN-LINE (LTE) (CADR LTE))
(DEFSUBST SCROLL-LINE-TOPIC (LTE) (CADDR LTE))
