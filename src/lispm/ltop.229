;;; -*-Mode:LISP; Package:SYSTEM-INTERNALS-*-
;;; Initialization & top-level READ-EVAL-PRINT loop

(DECLARE (SPECIAL ERROR-STACK-GROUP %ERROR-HANDLER-STACK-GROUP
		  G P TRACE-LEVEL SYN-TERMINAL-IO + - * // ++ +++ ** ***
		  INITIAL-READTABLE RUBOUT-HANDLER
		  BUILD-INITIAL-OBARRAY-FLAG COLD-INITIALIZATION-LIST WARM-INITIALIZATION-LIST
		  ONCE-ONLY-INITIALIZATION-LIST SYSTEM-INITIALIZATION-LIST
		  LISP-TOP-LEVEL-INSIDE-EVAL TV:INITIAL-LISP-LISTENER))

;Come here when machine starts.  Provides a base frame.
(DEFUN LISP-TOP-LEVEL NIL
  (LISP-REINITIALIZE NIL)			;(Re)Initialize critical variables and things
  (LISP-TOP-LEVEL1 (OR TV:INITIAL-LISP-LISTENER TERMINAL-IO))
  ;;Never returns
  )

;Called when the main process is reset.
(DEFUN LISP-TOP-LEVEL2 ()
  (LISP-TOP-LEVEL1 (OR TV:INITIAL-LISP-LISTENER TERMINAL-IO)))

;Function to reset various things, do initialization that's inconvenient in cold load, etc.
(DEFUN LISP-REINITIALIZE (&OPTIONAL (CALLED-BY-USER T))
  (SETQ INHIBIT-SCHEDULING-FLAG T)		;In case called by the user
  (SETQ ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL)
  (COND ((NOT CALLED-BY-USER)
	 (AND (FBOUNDP 'COMPILER:MA-RESET)	;Unload microcompiled defs, because they are gone!
	     (COMPILER:MA-RESET))		; Hopefully manage to do this before any gets called.
	 ;; Set up the TV sync program as soon as possible; until it is set up
	 ;; read references to the TV buffer can get NXM errors which cause a
	 ;; main-memory parity error halt.  Who-line updating can do this.
	 (AND (BOUNDP 'TV:DEFAULT-SCREEN) (SETUP-CPT))))
  (OR (FBOUNDP 'INTERN) (FSET 'INTERN #'INTERN-OLD))
  (OR (FBOUNDP 'FSET-CAREFULLY) (FSET 'FSET-CAREFULLY #'FSET-CAREFULLY-COLD-LOAD))
  (SETQ DEFAULT-CONS-AREA WORKING-STORAGE-AREA)	;Reset default areas.
  (AND (FBOUNDP 'NUMBER-GC-ON) (NUMBER-GC-ON))	;This seems to work now, make it the default
  (SETQ EH:CONDITION-HANDLERS NIL)
  (COND ((NOT (BOUNDP 'BUILD-INITIAL-OBARRAY-FLAG))
	 (BUILD-INITIAL-OBARRAY)
	 (SETQ BUILD-INITIAL-OBARRAY-FLAG T)))

  (COND ((NOT (BOUNDP 'CURRENT-PROCESS))	;Very first time around
	 (SETQ SCHEDULER-EXISTS NIL
	       CURRENT-PROCESS NIL
	       TV:WHO-LINE-PROCESS NIL
	       TV:LAST-WHO-LINE-PROCESS NIL)
	 (OR (FBOUNDP 'TV:WHO-LINE-RUN-STATE-UPDATE)
	     (FSET 'TV:WHO-LINE-RUN-STATE-UPDATE #'(LAMBDA (&REST IGNORE) NIL)))
	 (KBD-INITIALIZE)))
  (INITIALIZE-WIRED-KBD-BUFFER)

  ;Get the right readtable.
  (OR (BOUNDP 'INITIAL-READTABLE)
      (SETQ INITIAL-READTABLE READTABLE))
  (SETQ READTABLE INITIAL-READTABLE)

  ;; Initialize the rubout handler.
  (SETQ	RUBOUT-HANDLER NIL)			;We're not in it now

  ;; Initialize the error handler.
  (OR (BOUNDP 'ERROR-STACK-GROUP)
      (SETQ ERROR-STACK-GROUP (MAKE-STACK-GROUP 'ERROR-STACK-GROUP ':SAFE 0)))
  (SETQ %ERROR-HANDLER-STACK-GROUP ERROR-STACK-GROUP)
  (STACK-GROUP-PRESET ERROR-STACK-GROUP 'LISP-ERROR-HANDLER)	;May not be defined yet 
  (SETF (SG-FOOTHOLD-DATA %INITIAL-STACK-GROUP) NIL)	;EH depends on this
  (AND (FBOUNDP 'LISP-ERROR-HANDLER)
       (FUNCALL ERROR-STACK-GROUP '(INITIALIZE)))
  (COND ((AND (BOUNDP '%INITIALLY-DISABLE-TRAPPING)
	      (NULL %INITIALLY-DISABLE-TRAPPING)
	      (FBOUNDP 'LISP-ERROR-HANDLER)
	      (FBOUNDP 'ENABLE-TRAPPING))
	 (ENABLE-TRAPPING)))
  (SETQ EH:ERRSET-STATUS NIL)			;Turn off possible spurious errset

  ;And all kinds of randomness...

  (SETQ TRACE-LEVEL 0)
  (SETQ INSIDE-TRACE NIL)
  (SETQ G '?? P '??)
  (SETQ + NIL * NIL - NIL ;In case of error during first read/eval/print cycle
	// NIL ++ NIL +++ NIL ;or if their values were unprintable or obscene
	** NIL *** NIL)  ;and to get global values in case of break in a non-lisp-listener
  (SETQ LISP-TOP-LEVEL-INSIDE-EVAL NIL)
  (OR (BOUNDP 'PRIN1) (SETQ PRIN1 NIL))
  (SETQ EVALHOOK NIL)
  (FSET' EVAL (FUNCTION *EVAL))
  (SETQ IBASE 8 BASE 8 *NOPOINT NIL)
  (SETQ XR-CORRESPONDENCE-FLAG NIL		;Prevent the reader from doing random things
	XR-CORRESPONDENCE NIL)
  (SETQ *RSET T)				;In case any MACLISP programs look at it
  (SETQ PROGDESCLIST NIL RETPROGDESC NIL)
  (SETQ SYS:UNDO-DECLARATIONS-FLAG NIL)		;Don't get screwed by MACRO!
  (SETQ FDEFINE-FILE-SYMBOL NIL)
  (SETQ LOCAL-DECLARATIONS NIL FILE-LOCAL-DECLARATIONS NIL)
  (SETQ COMPILER:QC-FILE-IN-PROGRESS NIL COMPILER:QC-FILE-READ-IN-PROGRESS NIL)
  (AND (FBOUNDP 'PKG-FIND-PACKAGE)		;If package system is present
       (SETQ PACKAGE (PKG-FIND-PACKAGE "USER")))

  ;; The first time, this does top-level SETQ's from the cold-load files
  (AND (BOUNDP 'LISP-CRASH-LIST)
       (MAPC (FUNCTION EVAL) LISP-CRASH-LIST))
  (SETQ LISP-CRASH-LIST NIL)

  ;Reattach IO streams.  Note that TERMINAL-IO will be fixed later to go to a window.
  (OR (BOUNDP 'SYN-TERMINAL-IO) (SETQ SYN-TERMINAL-IO (MAKE-SYN-STREAM 'TERMINAL-IO)))
  (OR CALLED-BY-USER
      (SETQ TERMINAL-IO	    COLD-LOAD-STREAM
	    STANDARD-OUTPUT SYN-TERMINAL-IO
	    STANDARD-INPUT  SYN-TERMINAL-IO
	    QUERY-IO        SYN-TERMINAL-IO
	    TRACE-OUTPUT    (MAKE-SYN-STREAM 'QUERY-IO)
	    ERROR-OUTPUT    TRACE-OUTPUT
	    ))

  (SETQ TV:MOUSE-WINDOW NIL)  ;This gets looked at before the mouse process is turned on

  ;; These are initializations that have to be done before other initializations
  (INITIALIZATIONS 'SYSTEM-INITIALIZATION-LIST T)

  (AND CURRENT-PROCESS
       (FUNCALL CURRENT-PROCESS ':RUN-REASON 'LISP-INITIALIZE))

  (INITIALIZATIONS 'COLD-INITIALIZATION-LIST)

  (INITIALIZATIONS 'WARM-INITIALIZATION-LIST T)

  (COND ((AND (FBOUNDP 'FORMAT) (FBOUNDP 'CHAOS:HOST-DATA))
         (FORMAT T "~A~%" (CHAOS:HOST-DATA CHAOS:MY-ADDRESS)))
        (T (PRINC "Lisp Machine cold load environment, beware!")))

  ;; This process no longer needs to be able to run except for the usual reasons.
  (COND ((FBOUNDP 'TV:WINDOW-INITIALIZE)
	 (MULTIPLE-VALUE-BIND (X Y)
	     (FUNCALL TERMINAL-IO ':READ-CURSORPOS)
	   (SETF (TV:SHEET-OUTPUT-HOLD-FLAG TV:INITIAL-LISP-LISTENER) 0)
	   (FUNCALL TV:INITIAL-LISP-LISTENER ':SET-CURSORPOS X Y)))
	(T (SETQ TV:INITIAL-LISP-LISTENER NIL)))	;Not created yet


  (AND CURRENT-PROCESS
       (FUNCALL CURRENT-PROCESS ':REVOKE-RUN-REASON 'LISP-INITIALIZE))

  ;; The global value of TERMINAL-IO is a stream which goes to an auto-exposing
  ;; window.  Some processes, such as Lisp listeners, rebind it to something else.
  ;; CALLED-BY-USER is T if called from inside one of those.
  (COND ((AND (NOT CALLED-BY-USER)
	      (FBOUNDP TV:DEFAULT-BACKGROUND-STREAM))
	 (SETQ TERMINAL-IO TV:DEFAULT-BACKGROUND-STREAM))))

; The real top level.  Note that the stream to use is passed as an argument and
; bound to the special variable TERMINAL-IO.
(DEFUN LISP-TOP-LEVEL1 (TERMINAL-IO)
  (DO ((*) (+) (-)	;Bind these so that they are per-stack-group
       (//) (++) (+++) (**) (***)
       (THROW-FLAG))	;Gets non-NIL if throw to TOP-LEVEL (e.g. quitting from an error)
      (NIL)		;Do forever
    (MULTIPLE-VALUE (NIL THROW-FLAG)
      (*CATCH 'TOP-LEVEL
	      (PROGN (TERPRI)
		     (SETQ - (READ-FOR-TOP-LEVEL))
		     (LET ((LISP-TOP-LEVEL-INSIDE-EVAL T))
		       (SETQ // (MULTIPLE-VALUE-LIST (EVAL -))))
		     (SETQ *** **	;Save first value, propagate old saved values
			   ** *
			   * (CAR //))
		     (DOLIST (VALUE //)
		       (TERPRI)
		       (FUNCALL (OR PRIN1 #'PRIN1) VALUE)))))
    (AND THROW-FLAG (PRINT '*))	;Signal return to top level
    (SETQ +++ ++ ++ + + -)))			;Save last three input forms

;Note that BREAK binds RUBOUT-HANDLER to NIL so that a new level of catch
;will be established.  Before returning it clears the old rubout handler's buffer.
;Changed 3/3/80 by Moon not to bind *, +, and -.
(DEFUN BREAK (&QUOTE TAG &OPTIONAL &EVAL (CONDITIONAL T)
	      &AUX (RUBOUT-HANDLER NIL)
	           (READ-PRESERVE-DELIMITERS NIL)
	           ;Next line commented out since it causes more trouble in than out
		   ;(IBASE 8) (BASE 8)
		   (OLD-STANDARD-INPUT STANDARD-INPUT)
		   (STANDARD-INPUT 'SI:TERMINAL-IO-SYN-STREAM)
		   (STANDARD-OUTPUT 'SI:TERMINAL-IO-SYN-STREAM)
	           (EH:ERRSET-STATUS NIL)	;"Condition Wall" for errsets
		   (EH:CONDITION-HANDLERS NIL)	; and for conditions
		   )
  (COND (CONDITIONAL
	 ;; Deal with keyboard multiplexing in a way similar to the error-handler.
	 ;; If we break in the scheduler, set CURRENT-PROCESS to NIL.
	 ;; If this is not the scheduler process, make sure it has a run reason
	 ;; in case we broke in the middle of code manipulating process data.
	 ;; If INHIBIT-SCHEDULING-FLAG is set, turn it off and print a warning.
	 (COND ((EQ %CURRENT-STACK-GROUP SCHEDULER-STACK-GROUP)
		(SETQ CURRENT-PROCESS NIL)))
	 (AND (NOT (NULL CURRENT-PROCESS))
	      (NULL (FUNCALL CURRENT-PROCESS ':RUN-REASONS))
	      (FUNCALL CURRENT-PROCESS ':RUN-REASON 'BREAK))
	 (COND (INHIBIT-SCHEDULING-FLAG
		(FORMAT T "~%---> Turning off INHIBIT-SCHEDULING-FLAG, you may lose. <---~%")
		(SETQ INHIBIT-SCHEDULING-FLAG NIL)))
	 (FORMAT T "~&;BKPT ~A~%" TAG)
	 (LET ((VALUE
		 (DO ()
		     (NIL)		;Do forever (until explicit return)
		   (TERPRI)
		   (SETQ - (READ-FOR-TOP-LEVEL))
		   (COND ((EQ - 'P)	;Altmode-P proceeds from BREAK
			  (RETURN NIL))
			 ((AND (SYMBOLP -)
			       (STRING-EQUAL - "’"))  ;so does Resume
			  (RETURN NIL))
			 ((EQ - 'G)	;Altmode-G quits to top level (semi-obsolete)
			  (*THROW 'TOP-LEVEL NIL))
			 ((AND (LISTP -) (EQ (CAR -) 'RETURN))	;(RETURN form) proceeds
			  (RETURN (EVAL (CADR -)))))
		   (SETQ // (MULTIPLE-VALUE-LIST (EVAL -)))
		   (SETQ *** **
			 ** *
			 * (CAR //))	;Save first value
		   (DOLIST (VALUE //)
		     (TERPRI)
		     (FUNCALL (OR PRIN1 #'PRIN1) VALUE))
		   (SETQ +++ ++ ++ + + -))))
	   ;; Before returning, clear rubout handler's buffer to avoid problems
	   (AND (MEMQ ':CLEAR-INPUT (FUNCALL OLD-STANDARD-INPUT ':WHICH-OPERATIONS))
		(FUNCALL OLD-STANDARD-INPUT ':CLEAR-INPUT))
	   VALUE))))

;;; Initialization stuff

;;; Init lists have entries of the form (name form flag)
;;; If flag is non-NIL init has been run.

;; Some code relies on INIT-NAME being the CAR of the init entry.  **DO NOT CHANGE THIS**
(DEFMACRO INIT-NAME (INIT) `(CAR ,INIT))
(DEFMACRO INIT-FORM (INIT) `(CADR ,INIT))
(DEFMACRO INIT-FLAG (INIT) `(CADDR ,INIT))

(DEFMACRO INIT-LIST-CHECK (NAME) `(OR (BOUNDP ,NAME) (SET ,NAME NIL)))

;;; Run the inits in the specified list.
;;; If init has been run before it will only be run again if the second arg is non-NIL.
;;; The third arg is the flag to be RLACA'd into the flag slot.  If it is NIL it will
;;;  look as if the inits have never been run.  This may be useful for some applications.
(DEFUN INITIALIZATIONS (LIST-NAME &OPTIONAL (REDO-FLAG NIL) (FLAG T))
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((INIT (SYMEVAL LIST-NAME) (CDR INIT)))
      ((NULL INIT))
      (COND ((OR (NULL (INIT-FLAG (CAR INIT))) REDO-FLAG)
             (EVAL (INIT-FORM (CAR INIT)))
             (SETF (INIT-FLAG (CAR INIT)) FLAG)))))

;;; Adds a new init to the list.
;;; Keywords are:
;;; NOW		Run the init now
;;; FIRST	Run the init now if this is the first entry for the specified name
;;; NORMAL	Do the "normal" thing (init when initializations normally run)
;;; REDO	Do nothing now, but set up things so init gets redone
;;; COLD	Use the cold boot list
;;; WARM	Use the warm boot list
;;; ONCE	Use the once-only list
;;; SYSTEM	Use the system list
;;; If neither WARM nor COLD are specified, warm is assumed.  If a fourth argument
;;; is given, then it is the list to use.  WARM and COLD will override the fourth argument.
(DEFUN ADD-INITIALIZATION (NAME FORM &OPTIONAL KEYWORDS (LIST-NAME 'WARM-INITIALIZATION-LIST)
                                     &AUX (WHEN NIL) INIT)
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((L KEYWORDS (CDR L))
       (V))
      ((NULL L))
      (SETQ V (GET-PNAME (CAR L)))
      (COND ((STRING-EQUAL "NOW" V) (SETQ WHEN 'NOW))
            ((STRING-EQUAL "FIRST" V) (SETQ WHEN 'FIRST))
            ((STRING-EQUAL "NORMAL" V) (SETQ WHEN NIL))
            ((STRING-EQUAL "REDO" V) (SETQ WHEN 'REDO))
            ((STRING-EQUAL "WARM" V) (SETQ LIST-NAME 'WARM-INITIALIZATION-LIST))
            ((STRING-EQUAL "COLD" V) (SETQ LIST-NAME 'COLD-INITIALIZATION-LIST))
            ((STRING-EQUAL "SYSTEM" V)
             (SETQ LIST-NAME 'SYSTEM-INITIALIZATION-LIST)
             (SETQ WHEN 'FIRST))
            ((STRING-EQUAL "ONCE" V)
             (SETQ LIST-NAME 'ONCE-ONLY-INITIALIZATION-LIST)
             (SETQ WHEN 'FIRST))
            (T (FERROR NIL "Illegal keyword ~S" (CAR L)))))
  (SETQ INIT
        (DO ((L (SYMEVAL LIST-NAME) (CDR L)))
            ((NULL L)
             (COND ((NULL (SYMEVAL LIST-NAME))
                    (CAR (SET LIST-NAME (NCONS (LIST NAME FORM NIL)))))
                   (T (CADR (RPLACD (LAST (SYMEVAL LIST-NAME))
                                    (NCONS (LIST NAME FORM NIL)))))))
            (COND ((STRING-EQUAL (INIT-NAME (CAR L)) NAME)
                   (SETF (INIT-FORM (CAR L)) FORM)
                   (RETURN (CAR L))))))
  (COND ((EQ WHEN 'REDO) (SETF (INIT-FLAG INIT) NIL))
        ((OR (EQ WHEN 'NOW)
             (AND (EQ WHEN 'FIRST) (NULL (INIT-FLAG INIT))))
         (EVAL (INIT-FORM INIT))
         (SETF (INIT-FLAG INIT) T))))

;;; Deletes an init from the list.
;;; Keywords are:
;;; COLD	Use the cold boot list
;;; WARM	Use the warm boot list
;;; ONCE	Use the once-only list
;;; SYSTEM	Use the system list
;;; If neither WARM nor COLD are specified, warm is assumed.  If a third argument
;;; is given, then it is the list to use.  WARM and COLD will override the third argument.
(DEFUN DELETE-INITIALIZATION (NAME &OPTIONAL KEYWORDS (LIST-NAME 'WARM-INITIALIZATION-LIST))
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((L KEYWORDS (CDR L))
       (V))
      ((NULL L))
      (SETQ V (GET-PNAME (CAR L)))
      (COND ((STRING-EQUAL "WARM" V) (SETQ LIST-NAME 'WARM-INITIALIZATION-LIST))
            ((STRING-EQUAL "COLD" V) (SETQ LIST-NAME 'COLD-INITIALIZATION-LIST))
            ((STRING-EQUAL "ONCE" V) (SETQ LIST-NAME 'ONCE-ONLY-INITIALIZATION-LIST))
            ((STRING-EQUAL "SYSTEM" V) (SETQ LIST-NAME 'SYSTEM-INITIALIZATION-LIST))
            (T (FERROR NIL "Illegal keyword ~S" (CAR L)))))
  (DO ((L (SYMEVAL LIST-NAME) (CDR L)))
      ((NULL L))
      (COND ((STRING-EQUAL (INIT-NAME (CAR L)) NAME)
             (SET LIST-NAME (DELQ (CAR L) (SYMEVAL LIST-NAME)))))))

(DEFUN RESET-INITIALIZATIONS (LIST-NAME)
  (INIT-LIST-CHECK LIST-NAME)
  (DO ((L (SYMEVAL LIST-NAME) (CDR L)))
      ((NULL L))
      (SETF (INIT-FLAG (CAR L)) NIL)))

(ADD-INITIALIZATION "LTOP-CLEAR-SCREEN" '(FUNCALL COLD-LOAD-STREAM ':CLEAR-SCREEN) '(COLD))


;Small version of FSET-CAREFULLY to be used until all the full
;mechanisms are there (bootstrapping from cold-load)
(DEFUN FSET-CAREFULLY-COLD-LOAD (FUNCTION-SPEC DEFINITION &OPTIONAL FORCE-FLAG)
  FORCE-FLAG ;ignored by his simple version
  (OR (SYMBOLP FUNCTION-SPEC) (FERROR NIL "~S must be a symbol at this point" FUNCTION-SPEC))
  (AND FDEFINE-FILE-SYMBOL
       (PUTPROP FUNCTION-SPEC FDEFINE-FILE-SYMBOL ':SOURCE-FILE-NAME))
  (FSET FUNCTION-SPEC DEFINITION))

;;; Stuff which has to go somewhere, to be around in the cold-load,
;;; and doesn't have any logical place where it belongs (this used to
;;; be in LMIO;KBD)

(DEFVAR USER-ID "")	;Not logged in


;; This is here rather than with the scheduler because it has to be
;; in the cold-load.  It checks for the non-existence of a scheduler
;; and does it itself in that case.

;; Takes a predicate and arguments to it.  The process becomes blocked
;; until the application of the predicate to those arguments returns T.
;; Note that the function is run in the SCHEDULER stack group, not the
;; process's stack group!  This means that bindings in effect at the
;; time PROCESS-WAIT is called will not be in effect; don't refer to
;; variables "freely" if you are binding them.
;;    Kludge:  if the scheduler seems broken, or we ARE the scheduler
;; (i.e. a clock function tries to block), then loop-wait (no blinkers...)

;; In case of a sequence-break while waiting, this function can get "reinvoked".
;; Therefore, it must not modify its arguments, and must observe other restrictions.
;; see EH:REINVOKE.
(DEFUN PROCESS-WAIT (WHOSTATE FUNCTION &REST ARGUMENTS)
  (COND ((APPLY FUNCTION ARGUMENTS)	;Test condition before doing slow stack-group switch
	 NIL)				;Hmm, no need to wait after all
	((OR (NOT SCHEDULER-EXISTS)
	     (EQ SCHEDULER-STACK-GROUP %CURRENT-STACK-GROUP)
	     (NULL CURRENT-PROCESS)
	     (LET ((STATE (SG-CURRENT-STATE SCHEDULER-STACK-GROUP)))
	       (NOT (OR (= STATE SG-STATE-AWAITING-INITIAL-CALL)
			(= STATE SG-STATE-AWAITING-RETURN)))))
	 (DO () (NIL)
	   (AND (APPLY FUNCTION ARGUMENTS)
		(RETURN NIL))))
	(T
	 (SETF (PROCESS-WHOSTATE CURRENT-PROCESS) WHOSTATE)
	 (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS)
	 (WITHOUT-INTERRUPTS	;Dont allow below frobs to get reset by SB
	   (SET-PROCESS-WAIT CURRENT-PROCESS FUNCTION ARGUMENTS)
	   (FUNCALL SCHEDULER-STACK-GROUP))
	 (TV:WHO-LINE-PROCESS-CHANGE CURRENT-PROCESS))))
