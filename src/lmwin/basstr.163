;;; -*- Mode: LISP; Package: TV; Base: 8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains:  IO buffers, keyboard process

;;; IO buffers (definition in NTVDEF)

(DEFUN IO-BUFFER (OP BUFFER &REST ARGS)
  "Printer for IO-BUFFER named structures"
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
    ((:PRINT :PRINT-SELF)
	    (FORMAT (CAR ARGS) "#<IO-BUFFER ~O: " (%POINTER BUFFER))
	    (COND ((= (IO-BUFFER-INPUT-POINTER BUFFER)
		      (IO-BUFFER-OUTPUT-POINTER BUFFER))
		   (PRINC "empty, " (CAR ARGS)))
		  (T (FORMAT (CAR ARGS) "~D entr~:@P, "
			     (LET ((DIFF (- (IO-BUFFER-INPUT-POINTER BUFFER)
					    (IO-BUFFER-OUTPUT-POINTER BUFFER))))
			       (IF (< DIFF 0)
				   (+ DIFF (IO-BUFFER-SIZE BUFFER))
				   DIFF)))))
	    (FORMAT (CAR ARGS) "State: ~A>" (IO-BUFFER-STATE BUFFER)))
    (OTHERWISE (FORMAT T "I don't know about ~S" OP))))


(DEFUN MAKE-IO-BUFFER (SIZE &OPTIONAL IN-FUN OUT-FUN PLIST STATE &AUX BUFFER)
  "Create a new IO buffer of specified size"
  (SETQ BUFFER (MAKE-ARRAY NIL 'ART-Q SIZE NIL (GET 'IO-BUFFER 'SI:DEFSTRUCT-SIZE) NIL T))
  (STORE-ARRAY-LEADER 'IO-BUFFER BUFFER 1)
  (SETF (IO-BUFFER-FILL-POINTER BUFFER) 0)
  (SETF (IO-BUFFER-SIZE BUFFER) SIZE)
  (SETF (IO-BUFFER-INPUT-POINTER BUFFER) 0)
  (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER) 0)
  (SETF (IO-BUFFER-INPUT-FUNCTION BUFFER) IN-FUN)
  (SETF (IO-BUFFER-OUTPUT-FUNCTION BUFFER) OUT-FUN)
  (SETF (IO-BUFFER-STATE BUFFER) STATE)
  (SETF (IO-BUFFER-PLIST BUFFER) PLIST)
  BUFFER)

(DEFUN MAKE-DEFAULT-IO-BUFFER ()
  (MAKE-IO-BUFFER 100 NIL 'KBD-DEFAULT-OUTPUT-FUNCTION))

(DEFUN IO-BUFFER-PUT (BUFFER ELT &OPTIONAL (NO-HANG-P NIL))
  "Store a new element in an IO buffer"
  (DO ((INHIBIT-SCHEDULING-FLAG T T)
       (IGNORE-P)
       (INPUT-POINTER)
       (IN-FUN (IO-BUFFER-INPUT-FUNCTION BUFFER)))
      (())
    (COND ((OR (NULL (IO-BUFFER-STATE BUFFER))
	       (EQ (IO-BUFFER-STATE BUFFER) ':INPUT))
	   (COND (IN-FUN
		  ;; Call function with INHIBIT-SCHEDULING-FLAG turned on and bound.
		  ;; Since this function may change the state of the buffer either directly
		  ;; or indirectly, loop in order to check the state.  Set the function to
		  ;; NIL, though, so it won't be run again
		  (MULTIPLE-VALUE (ELT IGNORE-P)
		    (FUNCALL IN-FUN BUFFER ELT))
		  (AND IGNORE-P (RETURN T))
		  (SETQ IN-FUN NIL))
		 (T
		  (COND ((NOT (IO-BUFFER-FULL-P BUFFER))
			 (SETF (IO-BUFFER-LAST-INPUT-PROCESS BUFFER) CURRENT-PROCESS)
			 (SETQ INPUT-POINTER (IO-BUFFER-INPUT-POINTER BUFFER))
			 (ASET ELT BUFFER INPUT-POINTER)
			 (SETF (IO-BUFFER-INPUT-POINTER BUFFER)
			       (\ (1+ INPUT-POINTER) (IO-BUFFER-SIZE BUFFER)))
			 (RETURN T))
			(NO-HANG-P (RETURN NIL))
			(T
			  (SETQ INHIBIT-SCHEDULING-FLAG NIL)
			  (PROCESS-WAIT "Buffer full" #'(LAMBDA (BUF)
							  (NOT (IO-BUFFER-FULL-P BUF)))
					BUFFER))))))
	  (NO-HANG-P (RETURN NIL))
	  (T
	   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	   (PROCESS-WAIT "Buffer state" #'(LAMBDA (BUF)
					    (OR (NULL (IO-BUFFER-STATE BUF))
						(EQ (IO-BUFFER-STATE BUF) ':INPUT)))
			 BUFFER)))))

(DEFUN IO-BUFFER-GET (BUFFER &OPTIONAL (NO-HANG-P NIL))
  "Get an element from an IO buffer.  First value is ele, second is T if got one, else NIL"
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS BUFFER) CURRENT-PROCESS)
  (DO ((INHIBIT-SCHEDULING-FLAG T T)
       (ELT)
       (IGNORE-P)
       (OUTPUT-POINTER)
       (OUT-FUN (IO-BUFFER-OUTPUT-FUNCTION BUFFER)))
      (())
    (COND ((OR (NULL (IO-BUFFER-STATE BUFFER))
	       (EQ (IO-BUFFER-STATE BUFFER) ':OUTPUT))
	   (COND ((NOT (IO-BUFFER-EMPTY-P BUFFER))
		  (SETQ OUTPUT-POINTER (IO-BUFFER-OUTPUT-POINTER BUFFER))
		  (SETQ ELT (AREF BUFFER OUTPUT-POINTER))
		  (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER)
			(\ (1+ OUTPUT-POINTER) (IO-BUFFER-SIZE BUFFER)))
		  (COND ((AND OUT-FUN
			      ;; Call function with INHIBIT-SCHEDULING-FLAG on and bound.
			      ;; If element is to be ignored, loop back, else return element
			      (PROG2
			        (MULTIPLE-VALUE (ELT IGNORE-P)
				  (FUNCALL OUT-FUN BUFFER ELT))
				IGNORE-P)))
			(T (RETURN ELT T))))
		 (NO-HANG-P (RETURN NIL NIL))
		 (T
		  (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		  (PROCESS-WAIT "Buffer empty" #'(LAMBDA (BUF)
						   (NOT (IO-BUFFER-EMPTY-P BUF)))
				BUFFER))))
	  (NO-HANG-P (RETURN NIL NIL))
	  (T
	   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	   (PROCESS-WAIT "Buffer state" #'(LAMBDA (BUF)
					    (OR (NULL (IO-BUFFER-STATE BUF))
						(EQ (IO-BUFFER-STATE BUF) ':OUTPUT)))
			 BUFFER)))))

(DEFUN IO-BUFFER-UNGET (BUFFER ELT)
  "Return ELT to the IO-BUFFER by backing up the pointer.  ELT should be the last thing
read from the buffer."
  (WITHOUT-INTERRUPTS
    (LET ((OUTPUT-POINTER (1- (IO-BUFFER-OUTPUT-POINTER BUFFER))))
      (AND (< OUTPUT-POINTER 0)
	   (SETQ OUTPUT-POINTER (1- (IO-BUFFER-SIZE BUFFER))))
      (OR (EQ ELT (AREF BUFFER OUTPUT-POINTER))
	  (FERROR NIL
	    "Attempt to un-get something different then last element gotten from IO-BUFFER"))
      (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER) OUTPUT-POINTER))))

(DEFUN IO-BUFFER-CLEAR (BUFFER)
  "Clears out an IO buffer"
  (WITHOUT-INTERRUPTS
    (SETF (IO-BUFFER-INPUT-POINTER BUFFER) 0)
    (SETF (IO-BUFFER-OUTPUT-POINTER BUFFER) 0)
    T))

(DEFUN PROCESS-TYPEAHEAD (IO-BUFFER FUNCTION)
  (DO ((INPUT-POINTER (IO-BUFFER-INPUT-POINTER IO-BUFFER))
       (CH))
      ((= INPUT-POINTER (IO-BUFFER-OUTPUT-POINTER IO-BUFFER)))
    (AND (SETQ CH (FUNCALL FUNCTION (IO-BUFFER-GET IO-BUFFER T)))
	 (IO-BUFFER-PUT IO-BUFFER CH T))))

(DEFVAR KBD-IO-BUFFER (MAKE-IO-BUFFER 1000))	;Intermediate buffer so char is read out of
						; hardware immediatly
(DEFVAR KBD-ESC-HAPPENED NIL)			;An escape was typed
(DEFVAR KBD-ESC-TIME NIL)	;If non-NIL, this is the time we started processing
				;an escape (Terminal or System) which is still in process.
				;We try not to look at the keyboard while one is still
				;in process to provide more predictable behavior with
				;typeahead.  However, we don't wait forever so that if
				;the process hangs forever the system doesn't "die".

(DEFUN KBD-PROCESS-MAIN-LOOP ()
  "This function runs in the keyboard process.  It is responsible for reading characters
from the hardware, and performing any immediate processing associated with the character."
  (DO () (NIL)
    (*CATCH 'SI:TOP-LEVEL
      (PROGN
	(IO-BUFFER-CLEAR KBD-IO-BUFFER)
	(SETQ KBD-ESC-HAPPENED NIL)
	(DO () (NIL)
	  (PROCESS-WAIT "Keyboard"
			#'(LAMBDA ()
			    (OR KBD-ESC-HAPPENED
				(AND (NOT (IO-BUFFER-FULL-P KBD-IO-BUFFER))
				     (KBD-HARDWARE-CHAR-AVAILABLE)))))
	  (COND (KBD-ESC-HAPPENED
		  (FUNCALL KBD-ESC-HAPPENED)
		  (PROCESS-WAIT "ESC Finish"
				#'(LAMBDA () (LET ((X KBD-ESC-TIME))
					       (OR (NULL X)	;Wait at most 10 seconds
						   (> (TIME-DIFFERENCE (TIME) X) 600.)))))
		  (SETQ KBD-ESC-HAPPENED NIL)))
	  (KBD-PROCESS-MAIN-LOOP-INTERNAL))))))

;Note that KBD-CONVERT-TO-SOFTWARE-CHAR must be called in order,
;since for the new keyboards it does shifts and keeps state.

(DEFUN KBD-PROCESS-MAIN-LOOP-INTERNAL (&AUX BUFFER PLIST RAW-P)
  (WITHOUT-INTERRUPTS
    (COND ((SETQ BUFFER (KBD-GET-IO-BUFFER))
	   (SETQ PLIST (LOCF (IO-BUFFER-PLIST BUFFER)))
	   (SETQ RAW-P (GET PLIST ':RAW))))
    (DO ((CHAR)
	 (SOFT-CHAR)
	 (IFUN (GET PLIST ':INTERRUPT-FUNCTION)))
	((OR KBD-ESC-HAPPENED
	     (NOT (KBD-HARDWARE-CHAR-AVAILABLE))))
      (SETQ CHAR (KBD-GET-HARDWARE-CHAR))
      (COND (RAW-P
	     (COND ((NOT (IO-BUFFER-FULL-P BUFFER))
		    (AND IFUN (FUNCALL IFUN BUFFER CHAR))
		    (IO-BUFFER-PUT BUFFER CHAR))))
	    (T
	     (SETQ SOFT-CHAR (KBD-CONVERT-TO-SOFTWARE-CHAR CHAR))
	     (COND ((NULL SOFT-CHAR))			;Unreal char
		   (T (SETQ CHAR (LDB %%KBD-CHAR SOFT-CHAR)	;No bucky bits
			    KBD-LAST-ACTIVITY-TIME (TIME)
			    SI:WHO-LINE-JUST-COLD-BOOTED-P NIL)
		      (COND ((= SOFT-CHAR #\ESC)	;Must have no bucky bits--for supdup
			     (SETQ KBD-ESC-HAPPENED #'KBD-ESC))
			    ((= SOFT-CHAR #\SYSTEM)
			     (SETQ KBD-ESC-HAPPENED #'KBD-SYS))
			    ((AND (= CHAR #\CALL)
				  (NOT (GET PLIST ':SUPER-IMAGE)))
			     (KBD-CALL BUFFER))
			    ((AND (= CHAR #\ABORT)
				  (NOT (GET PLIST ':SUPER-IMAGE)))
			     (KBD-ABORT BUFFER))
			    ((NOT (IO-BUFFER-FULL-P KBD-IO-BUFFER))
			     (AND IFUN (FUNCALL IFUN BUFFER SOFT-CHAR))
			     (IO-BUFFER-PUT KBD-IO-BUFFER SOFT-CHAR))))))))))

(DEFUN KBD-IO-BUFFER-GET (BUFFER &OPTIONAL (NO-HANG-P NIL) (WHOSTATE "TYI"))
  (DO ((INHIBIT-SCHEDULING-FLAG T T)
       (UPDATE-STATE-P (NEQ CURRENT-PROCESS (IO-BUFFER-LAST-OUTPUT-PROCESS BUFFER)))
       (OK)
       (ELT))
      (())
    (MULTIPLE-VALUE (ELT OK)
      (IO-BUFFER-GET BUFFER T))
    ;; If new process reading, better update wholine run state
    (AND UPDATE-STATE-P (EQ BUFFER SELECTED-IO-BUFFER)
	 (WHO-LINE-RUN-STATE-UPDATE))
    ;; Got something from the normal buffer, just return it
    (AND OK (RETURN ELT))
    ;; OK is NIL here.  If we aren't selected, don't look at system's io buffer
    (AND (EQ BUFFER SELECTED-IO-BUFFER)
	 (MULTIPLE-VALUE (ELT OK)
	   (IO-BUFFER-GET KBD-IO-BUFFER T)))
    (COND (OK
	   ;; Got something from the kbd buffer, put it into the normal buffer and loop
	   (IO-BUFFER-PUT BUFFER ELT T))	;Can't hang, but...
	  ;; Nothing for baby!!!  What should we do?
	  (T
	   (AND (NOT (IO-BUFFER-FULL-P KBD-IO-BUFFER))
		(KBD-HARDWARE-CHAR-AVAILABLE)
		;; If there is a possibility that a character of interest exists in
		;; the hardware, get it
		(KBD-PROCESS-MAIN-LOOP-INTERNAL))
	   (IF (OR (NOT (IO-BUFFER-EMPTY-P BUFFER))
		   (AND (EQ BUFFER (KBD-GET-IO-BUFFER))
			(NOT (IO-BUFFER-EMPTY-P KBD-IO-BUFFER))))
	       NIL				;Have a character, so loop and get it
	       (AND NO-HANG-P (RETURN NIL))
	       (SETQ INHIBIT-SCHEDULING-FLAG NIL)
	       (PROCESS-WAIT WHOSTATE #'(LAMBDA (BUFFER)
					  (OR (NOT (IO-BUFFER-EMPTY-P BUFFER))
					      (AND (EQ BUFFER (KBD-GET-IO-BUFFER))
						   (NOT (IO-BUFFER-EMPTY-P KBD-IO-BUFFER)))))
			     BUFFER))))))

(DEFUN KBD-SNARF-INPUT (BUFFER &OPTIONAL NO-HARDWARE-CHARS-P)
  (WITHOUT-INTERRUPTS
    (COND ((EQ BUFFER (KBD-GET-IO-BUFFER))
	   ;; There is potentially input for us
	   (OR NO-HARDWARE-CHARS-P (KBD-PROCESS-MAIN-LOOP-INTERNAL))
	   (DO ((OK)
		(ELT))
	       ((IO-BUFFER-EMPTY-P KBD-IO-BUFFER))
	     (MULTIPLE-VALUE (ELT OK)
	       (IO-BUFFER-GET KBD-IO-BUFFER T))
	     (OR OK (RETURN NIL))		;Some ignored characters, we are done
	     (AND ELT (IO-BUFFER-PUT BUFFER ELT T)))))))

;;; This is a crock, but I suppose someone might want to...
(DEFVAR KBD-TYI-HOOK NIL)

(DEFUN KBD-DEFAULT-OUTPUT-FUNCTION (IGNORE CHAR)
  "System standard IO-BUFFER output function.  Must be called with INHIBIT-SCHEDULING-FLAG
bound to T, and this may SETQ it to NIL."
  ;; Default IO-BUFFER-OUTPUT-FUNCTION for keyboard io buffers.  Implements control-Z.
  (PROG ()
    (IF (AND KBD-TYI-HOOK (FUNCALL KBD-TYI-HOOK CHAR))
	(RETURN CHAR T)
	(SETQ INHIBIT-SCHEDULING-FLAG NIL)
	(SELECTQ CHAR
	  ((#/Z #/z) (*THROW 'SI:TOP-LEVEL NIL))
	  (#\BREAK
;;;        (FUNCALL-SELF ':BREAK)
	   (BREAK BREAK T)
	   (RETURN CHAR T))))
    (RETURN CHAR NIL)))

(DEFVAR KBD-PROCESS)
(DEFUN INSTALL-MY-KEYBOARD ()
  (OR (BOUNDP 'KBD-PROCESS)
      (SETQ KBD-PROCESS (PROCESS-RUN-FUNCTION "Keyboard" 'KBD-PROCESS-MAIN-LOOP)))
  (SI:PROCESS-RESET-AND-ENABLE KBD-PROCESS))

(DEFUN KBD-GET-SOFTWARE-CHAR (&OPTIONAL (WHOSTATE "Keyboard"))
  "Returns the next char from the hardware converted to software codes.  This
is meant to be used only by things that run in the keyboard process, and not by
any user code."
  (DO ((CH)) (NIL)
    (PROCESS-WAIT WHOSTATE #'KBD-HARDWARE-CHAR-AVAILABLE)
    (AND (SETQ CH (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR)))
	 (RETURN CH))))

(DEFUN KBD-CHAR-TYPED-P (&AUX (BUFFER (KBD-GET-IO-BUFFER)))
  "Kludge to return T when a character has been typed.  First checks the selected window's
IO buffer, and if it is empty then checks the microcode's buffer.  This is useful for
programs which want to stop when a character is typed, but don't want to allow
interrupts and scheduling."
  (OR (AND BUFFER (NOT (IO-BUFFER-EMPTY-P BUFFER)))
      (KBD-HARDWARE-CHAR-AVAILABLE)))

(DEFUN KBD-CLEAR-IO-BUFFER ()
  "Clear the keyboard buffer and the hardware buffer"
  (IO-BUFFER-CLEAR KBD-IO-BUFFER)
  (DO () ((NOT (KBD-HARDWARE-CHAR-AVAILABLE)))
    ;; Call this to process shifts
    (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR))))

(DEFUN KBD-CLEAR-SELECTED-IO-BUFFER ()
  "Flush the selected io buffer"
  (SETQ SELECTED-IO-BUFFER NIL))

(DEFUN KBD-GET-IO-BUFFER ()
  "Returns the current IO buffer.  If there is no current buffer, the selected window
is interrogated.  If there is no selected window, or the window has no buffer, returns NIL."
  (COND ((NULL SELECTED-WINDOW)
	 ;; This shouldn't be necessary, but try not to lose too big
	 (SETQ SELECTED-IO-BUFFER NIL))
	(SELECTED-IO-BUFFER SELECTED-IO-BUFFER)
	(T (PROG1 (SETQ SELECTED-IO-BUFFER (FUNCALL SELECTED-WINDOW ':IO-BUFFER))
		  (WHO-LINE-RUN-STATE-UPDATE)))))	;May have just switched processes

(DEFUN KBD-CALL (BUFFER)
  BUFFER					;Not used
  (IO-BUFFER-CLEAR KBD-IO-BUFFER)		;Forget chars typed before "call"
  (PROCESS-RUN-FUNCTION "Call" #'(LAMBDA (WINDOW)
				   (IF WINDOW
				       (FUNCALL WINDOW ':CALL)
				       (SETQ WINDOW (KBD-DEFAULT-CALL-WINDOW))
				       (FUNCALL WINDOW ':MOUSE-SELECT)))
			SELECTED-WINDOW))

(DEFUN KBD-DEFAULT-CALL-WINDOW (&OPTIONAL (SCREEN DEFAULT-SCREEN) &AUX PREVIOUS-WINDOW)
  (IF (AND (SETQ PREVIOUS-WINDOW (AREF PREVIOUSLY-SELECTED-WINDOWS 0))
	   (EQ (FUNCALL PREVIOUS-WINDOW ':LISP-LISTENER-P) ':IDLE))
      ;; CALL should always get a Lisp Listener, but try to be smart about
      ;; the one that it really gets
      PREVIOUS-WINDOW
      (FUNCALL SCREEN ':IDLE-LISP-LISTENER)))

(DEFUN KBD-ABORT (BUFFER)
  BUFFER					;Not used
  (KBD-ESC-CLEAR NIL)  ;Forget chars typed before "abort", even those inside window's iob
  (AND SELECTED-WINDOW
       (PROCESS-RUN-FUNCTION "Abort" SELECTED-WINDOW ':ABORT)))

(DEFUN KBD-BREAK (BUFFER)
  BUFFER					;Not used
  (AND SELECTED-WINDOW
       (PROCESS-RUN-FUNCTION "Break" SELECTED-WINDOW ':BREAK)))

;Return the state of a key, T if it is depressed, NIL if it is not.
;This only works on new keyboards; on old keyboards it always returns NIL.
;A key is specified by either a number which is the ascii code of the
;key (the character you get when you type that key with no shifts),
;or a symbol which is the symbolic name of a shift key (see below).
(DEFUN KEY-STATE (KEY &AUX TEM)
  (KBD-PROCESS-MAIN-LOOP-INTERNAL)
  (COND ((NUMBERP KEY) (NOT (ZEROP (AREF SI:KBD-KEY-STATE-ARRAY KEY))))
	((SETQ TEM (ASSQ KEY '((:SHIFT 100) (:LEFT-SHIFT 0) (:RIGHT-SHIFT 40)
			       (:GREEK 101) (:LEFT-GREEK 1) (:RIGHT-GREEK 41)
			       (:TOP 102) (:LEFT-TOP 2) (:RIGHT-TOP 42)
			       (:CONTROL 104) (:LEFT-CONTROL 4) (:RIGHT-CONTROL 44)
			       (:META 105) (:LEFT-META 5) (:RIGHT-META 45)
			       (:SUPER 106) (:LEFT-SUPER 6) (:RIGHT-SUPER 46)
			       (:HYPER 107) (:LEFT-HYPER 7) (:RIGHT-HYPER 47)
			       (:CAPS-LOCK 3) (:ALT-LOCK 10) (:MODE-LOCK 11)
			       (:REPEAT 12))))
	 (BIT-TEST (LSH 1 (LOGAND (SETQ TEM (CADR TEM)) 37))
		   (COND ((< TEM 40) SI:KBD-LEFT-SHIFTS)
			 ((< TEM 100) SI:KBD-RIGHT-SHIFTS)
			 (T (LOGIOR SI:KBD-LEFT-SHIFTS SI:KBD-RIGHT-SHIFTS)))))
	(T (FERROR NIL "~S illegal key; must be character or symbol for shift key" KEY))))

;;; "Escape key"

; A list of elements (char function documentation . options).
; Typing [terminal] char activates this element.  If function is a list it is
; evaluated, otherwise it is a function to be applied to one argument, which
; is NIL or the numeric-arg typed by the user.  In either case it happens
; in a separate process.  documentation is a form to evaluate to get the
; documentation, a string or NIL to leave this key undocumented.
; Documentation can be a list of strings to go on separate lines.
; The following options in the CDDDR of the list are:
;    :TYPEAHEAD - copy the contents of the
;	software buffer into the currently selected IO-BUFFER.  This has the
;	effect of treating everything typed before the ESC as typeahead to
;	the currently selected window.  Useful for ESC commands that
;	change the selected window.
;    :KEYBOARD-PROCESS - run the function in the keyboard process instead of starting
;	a new process for it.
; Unknown or misspelled keywords are ignored.
(DEFVAR *ESCAPE-KEYS*
     '( (#\BREAK (AND SELECTED-WINDOW (FUNCALL SELECTED-WINDOW ':BREAK))
	 "Force process into error-handler")
	(#\CLEAR KBD-ESC-CLEAR "Discard type-ahead" :KEYBOARD-PROCESS)
	(#\FORM (KBD-SCREEN-REDISPLAY) "Clear and redisplay all windows")
	(#/A KBD-ESC-ARREST
	     "Arrest process in who-line (minus means unarrest)" :KEYBOARD-PROCESS)
	(#/C (COMPLEMENT-BOW-MODE) "Complement video black-on-white state" :KEYBOARD-PROCESS)
	(#/D (CHAOS:BUZZ-DOOR) (AND (CHAOS:TECH-SQUARE-FLOOR-P 9) "Open the door"))
	(#/E (CHAOS:CALL-ELEVATOR) (AND (OR (CHAOS:TECH-SQUARE-FLOOR-P 8)
					    (CHAOS:TECH-SQUARE-FLOOR-P 9))
					"Call the elevator"))
	(#/F KBD-FINGER "Finger (AI, or arg=1:Lisp machines, 2 MC, 3 AI+MC, 0 ask)"
			:TYPEAHEAD)
	(#/M KBD-ESC-MORE "**MORE** enable (complement, or arg=1:on, 0 off)"
			  :KEYBOARD-PROCESS)
	(#/O KBD-OTHER-EXPOSED-WINDOW "Select another exposed window" :TYPEAHEAD)
	(#/Q (SI:SCREEN-XGP-HARDCOPY-BACKGROUND DEFAULT-SCREEN)
	     "Hardcopy the screen on the XGP")
	(#/S KBD-SWITCH-WINDOWS
	 '("Select the most recently selected window.  With an argument, select the nth"
	   "previously selected window and rotate the top n windows.  (Default arg is 2)."
	   "With an arg of 1, rotate through all the windows.  With a negative arg rotate"
	   "in the other direction.  With an argument of 0, select a window that wants"
	   "attention, e.g. to report an error.")
	   :TYPEAHEAD)
	(#/W KBD-ESC-W
	 '("Switch which process the wholine looks at.  Default is just to refresh it"
	   " 1 means selected-window's process, 2 means freeze on this process,"
	   " 3 means rotate right in active-processes, 4 means rotate left.")
	   :KEYBOARD-PROCESS)
	(#\HOLD-OUTPUT KBD-ESC-OUTPUT-HOLD "Expose window on which we have /"Output Hold/"")
	(#/? KBD-ESC-HELP NIL :TYPEAHEAD)
	(#\HELP KBD-ESC-HELP NIL :TYPEAHEAD)
	(NIL) ;Ones after here are "for wizards"
	(#\CALL (KBD-USE-COLD-LOAD-STREAM) "Get to cold-load stream" :TYPEAHEAD)
	(#\CLEAR KBD-CLEAR-LOCKS "Clear window-system locks")
	(#/T KBD-CLEAR-TEMPORARY-WINDOWS "Flush temporary windows")
	(#/G (BEEP) "Beep the beeper")))  ;Should this be flushed now?
	
(DEFUN KBD-ESC (&AUX CH ARG MINUS FCN)
  "Handle ESC typed on keyboard"
  (LET-GLOBALLY ((WHO-LINE-PROCESS CURRENT-PROCESS))
    (WHO-LINE-RUN-STATE-UPDATE)  ;Necessary to make above take effect
    (DO () (NIL)
      (SETQ CH (CHAR-UPCASE (KBD-GET-SOFTWARE-CHAR "Terminal-")))
      (COND ((= CH #\ESC)					;Typed another ESC, reset
	     (SETQ ARG NIL MINUS NIL))
	    ((AND ( CH #/0) ( CH #/9))
	     (SETQ ARG (+ (* (OR ARG 0) 8.) (- CH #/0))))
	    ((= CH #/-) (SETQ MINUS T))
	    (T (RETURN)))))
  (WHO-LINE-RUN-STATE-UPDATE)	;Switch LAST-WHO-LINE-PROCESS back
  (AND MINUS (SETQ ARG (MINUS (OR ARG 1))))
  (COND ((SETQ CH (ASSQ CH *ESCAPE-KEYS*))
	 (WITHOUT-INTERRUPTS
	   (AND (MEMQ ':TYPEAHEAD (CDDDR CH)) (KBD-GET-IO-BUFFER)
		(KBD-SNARF-INPUT SELECTED-IO-BUFFER T)))
	 (SETQ FCN (SECOND CH))
	 (AND (LISTP FCN) (SETQ ARG FCN FCN #'EVAL))
	 (COND ((MEMQ ':KEYBOARD-PROCESS (CDDDR CH))
		(FUNCALL FCN ARG))
	       (T (SETQ KBD-ESC-TIME (TIME))
		  (PROCESS-RUN-FUNCTION "KBD ESC"
					#'(LAMBDA (FCN ARG)
					    (FUNCALL FCN ARG)
					    (SETQ KBD-ESC-TIME NIL))
					FCN ARG))))))

(DEFUN KBD-ESC-MORE (ARG) ;esc M
  (SETQ MORE-PROCESSING-GLOBAL-ENABLE
	(COND ((NULL ARG) (NOT MORE-PROCESSING-GLOBAL-ENABLE))
	      ((< ARG 1) NIL)			;ESC 0 M, ESC - M, MORE PROC OFF
	      (T T))))				;ESC 1 M, MORE PROC ON

(DEFUN KBD-ESC-CLEAR (TEM) ;esc clear-input
  (AND (SETQ TEM (KBD-GET-IO-BUFFER))
       (IO-BUFFER-CLEAR TEM))
  (IO-BUFFER-CLEAR KBD-IO-BUFFER))

(DEFUN KBD-ESC-ARREST (ARG &AUX P)
  (COND ((NULL (SETQ P LAST-WHO-LINE-PROCESS)) (BEEP))
	((AND ARG (MINUSP ARG))
	 (DOLIST (R (FUNCALL P ':ARREST-REASONS))
	   (FUNCALL P ':REVOKE-ARREST-REASON R)))
	(T (FUNCALL P ':ARREST-REASON ':USER))))

(DEFUN KBD-OTHER-EXPOSED-WINDOW (IGNORE)
  ;; ESC O selects the least recently-selected window that is exposed.
  ;; Thus repeated esc O cycles among all the selectable exposed windows 
  ;; on all the screens.  Real useful with split-screen!
  (DO ((I 0 (1+ I))
       (N (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
       (TEM)
       (WINDOW NIL))
      (( I N)
       (IF WINDOW (FUNCALL WINDOW ':MOUSE-SELECT)
	   (BEEP)))
    (AND (SETQ TEM (AREF PREVIOUSLY-SELECTED-WINDOWS I))
	 (EQ (FUNCALL TEM ':STATUS) ':EXPOSED)
	 (NOT (NULL (FUNCALL TEM ':NAME-FOR-SELECTION)))
	 (SETQ WINDOW TEM))))

(DEFUN KBD-SWITCH-WINDOWS (ARG &AUX TEM) ;esc S
  ;; ESC n S rotates the n most recently selected windows, selecting the nth
  ;; ESC S = ESC 2 S
  ;; ESC 1 S selects the next most recent window but rotates all the windows
  ;; ESC -n S rotates the same set of windows in the other direction
  ;; ESC 0 S selects a window which has an error pending (or otherwise wants attention)
  (OR ARG (SETQ ARG 2))
  (COND ((= ARG 0) (AND (SETQ TEM (FIND-INTERESTING-WINDOW))
			(FUNCALL TEM ':MOUSE-SELECT)))
	(T (DELAYING-SCREEN-MANAGEMENT		;Inhibit auto-selection
	     (COND ((SETQ TEM SELECTED-WINDOW)	;Put current window on front of array
		    (FUNCALL TEM ':DESELECT NIL)
		    (AND (SETQ TEM (FUNCALL TEM ':IO-BUFFER))
			 (KBD-SNARF-INPUT TEM T))))
	     (WITHOUT-INTERRUPTS		;Get rid of any non-mouse-selectable ones
	       (DOTIMES (I (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
		 (OR (SETQ TEM (AREF PREVIOUSLY-SELECTED-WINDOWS I)) (RETURN))
		 (COND ((NOT (FUNCALL TEM ':NAME-FOR-SELECTION))
			(REMOVE-FROM-PREVIOUSLY-SELECTED-WINDOWS TEM)
			(SETQ I (1- I)))))
	       (ROTATE-TOP-OF-ARRAY PREVIOUSLY-SELECTED-WINDOWS ARG))
	     (AND (SETQ TEM (AREF PREVIOUSLY-SELECTED-WINDOWS 0))
		  (FUNCALL TEM ':MOUSE-SELECT))))))

;This is like ZWEI:ROTATE-TOP-OF-LIST but for a NIL-padded array
;Rotate nth (1-origin!) element to the front of the array, rotating the
;part of the array before it.  With a negative arg rotate the same amount
;backwards.  With an arg of 1 rotate the whole array BACKWARDS, i.e. bring
;up the same element as with an arg of 2 but store the old front at the back.
;Zero arg is undefined, do nothing I guess.  Note that 2 and -2 do the same thing.
;Doesn't barf if N is too big.
(DEFUN ROTATE-TOP-OF-ARRAY (ARRAY N &AUX (LENGTH (ARRAY-LENGTH ARRAY)))
  (DO () ((ZEROP LENGTH))
    (AND (AREF ARRAY (1- LENGTH)) (RETURN))
    (SETQ LENGTH (1- LENGTH)))
  (AND (= (ABS N) 1) (SETQ N (* N -1 LENGTH)))
  (COND ((PLUSP N)
	 (SETQ N (MIN LENGTH N))
	 (DO ((I 0 (1+ I))
	      (NTH (AREF ARRAY (1- N)) OLD)
	      (OLD))
	     (( I N))
	   (SETQ OLD (AREF ARRAY I))
	   (ASET NTH ARRAY I)))
	((MINUSP N)
	 (SETQ N (MIN LENGTH (MINUS N)))
	 (DO ((I 1 (1+ I))
	      (FRONT (AREF ARRAY 0)))
	     (( I N) (ASET FRONT ARRAY (1- I)))
	   (ASET (AREF ARRAY I) ARRAY (1- I)))))
  ARRAY)

(DEFUN KBD-SCREEN-REDISPLAY (&OPTIONAL (SCREEN MOUSE-SHEET))
  "Like SCREEN-REDISPLAY, but goes over windows by hand, and never waits for a lock."
  (DOLIST (I (SHEET-EXPOSED-INFERIORS SCREEN))
    (AND (SHEET-CAN-GET-LOCK I)
	 (FUNCALL I ':REFRESH)))
  (WHO-LINE-CLOBBERED)
  (AND (NEQ DEFAULT-SCREEN MOUSE-SHEET)
       (FUNCALL DEFAULT-SCREEN ':SCREEN-MANAGE))
  (FUNCALL MOUSE-SHEET ':SCREEN-MANAGE))

(DEFUN KBD-CLEAR-LOCKS (IGNORE) ;esc c-clear
  (SHEET-CLEAR-LOCKS))

(DEFUN KBD-CLEAR-TEMPORARY-WINDOWS (IGNORE) ;esc c-T
  (MAP-OVER-SHEETS #'(LAMBDA (SHEET)
		       (AND (SHEET-TEMPORARY-P SHEET)
			    (SHEET-EXPOSED-P SHEET)
			    (SHEET-CAN-GET-LOCK SHEET)
			    (ERRSET (FUNCALL SHEET ':DEEXPOSE) NIL)))))

(DEFUN KBD-USE-COLD-LOAD-STREAM ()
  (FUNCALL COLD-LOAD-STREAM ':HOME-CURSOR)
  (FUNCALL COLD-LOAD-STREAM ':CLEAR-EOL)
  (*CATCH 'SI:TOP-LEVEL
    (LET ((INHIBIT-SCHEDULING-FLAG NIL)		;NIL or BREAK would complain
	  (TERMINAL-IO COLD-LOAD-STREAM))
      (PRINT PACKAGE COLD-LOAD-STREAM)
      (BREAK COLD-LOAD-STREAM))))

(DEFUN KBD-ESC-OUTPUT-HOLD (IGNORE &AUX P W)
  (COND ((AND (SETQ P LAST-WHO-LINE-PROCESS)
	      (EQUAL (PROCESS-WHOSTATE P) "Output Hold")
	      (TYPEP (SETQ W (CAR (PROCESS-WAIT-ARGUMENT-LIST P))) 'SHEET)
	      (SHEET-OUTPUT-HOLD-FLAG W))
	 (SHEET-FREE-TEMPORARY-LOCKS W)
	 (FUNCALL W ':EXPOSE))
	((BEEP))))

(DEFVAR POP-UP-FINGER-WINDOW)
(DEFUN KBD-FINGER (ARG)
  (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG POP-UP-FINGER-WINDOW) 1)
  (FUNCALL POP-UP-FINGER-WINDOW ':SET-LABEL (COND ((NULL ARG)
						   "Who's on AI")
						  ((= ARG 0)
						   "Finger")
						  ((= ARG 1)
						   "Who's on Lisp Machines")
						  ((= ARG 2)
						   "Who's on MC")
						  (T
						   "Who's on AI and MC")))
  (FUNCALL POP-UP-FINGER-WINDOW ':SET-PROCESS CURRENT-PROCESS)
  (WINDOW-CALL (POP-UP-FINGER-WINDOW :DEACTIVATE)
    (SETQ KBD-ESC-TIME NIL)	;Window configuration stable now, let kbd process proceed
    (COND ((NULL ARG)
	   (CHAOS:FINGER "@AI" POP-UP-FINGER-WINDOW))
	  ((= ARG 0)
	   (FORMAT POP-UP-FINGER-WINDOW "~&Finger:~%")
	   (FUNCALL #'CHAOS:FINGER (READLINE POP-UP-FINGER-WINDOW) POP-UP-FINGER-WINDOW))
	  ((= ARG 1)
	   (CHAOS:FINGER-ALL-LMS POP-UP-FINGER-WINDOW T))
	  ((= ARG 2)
	   (CHAOS:FINGER "//L@MC" POP-UP-FINGER-WINDOW))
	  (T
	   (CHAOS:FINGER "@AI" POP-UP-FINGER-WINDOW)
	   (TERPRI POP-UP-FINGER-WINDOW)
	   (CHAOS:FINGER "@MC" POP-UP-FINGER-WINDOW)))
    (FORMAT POP-UP-FINGER-WINDOW "~&~%Type a space to flush: ")
    (FUNCALL POP-UP-FINGER-WINDOW ':TYI)))

(DEFUN KBD-ESC-W (ARG &AUX PROC)
  (SETQ PROC LAST-WHO-LINE-PROCESS)
  (SELECTQ ARG
    (NIL (SHEET-CLEAR WHO-LINE-WINDOW)
	 (WHO-LINE-CLOBBERED))
    (1 (SETQ WHO-LINE-PROCESS NIL))
    (2 (SETQ WHO-LINE-PROCESS PROC))
    (3 (SETQ WHO-LINE-PROCESS (DO ((L ACTIVE-PROCESSES (CDR L)))
				  ((NULL L) (CAAR ACTIVE-PROCESSES))
				(AND (EQ (CAAR L) PROC)
				     (RETURN (OR (CAADR L) (CAAR ACTIVE-PROCESSES)))))))
    (4 (SETQ WHO-LINE-PROCESS (OR (DO ((L ACTIVE-PROCESSES (CDR L))
				       (OL NIL L))
				      ((NULL L) NIL)
				    (AND (EQ (CAAR L) PROC)
					 (RETURN (CAAR OL))))
				  (DO ((L ACTIVE-PROCESSES (CDR L))
				       (OL NIL L))
				      ((NULL (CAR L)) (CAAR OL)))))))
  (WHO-LINE-RUN-STATE-UPDATE)
  (WHO-LINE-UPDATE))

(DEFUN KBD-ESC-HELP (IGNORE &AUX DOC (INDENT 15.))
  (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG POP-UP-FINGER-WINDOW) 0)
  (FUNCALL POP-UP-FINGER-WINDOW ':SET-LABEL "Keyboard documentation")
  (WINDOW-MOUSE-CALL (POP-UP-FINGER-WINDOW :DEACTIVATE)
     (FORMAT POP-UP-FINGER-WINDOW "~25TType Terminal//Escape followed by:

0-9, -~VTNumeric argument to following command~%" INDENT)
     (DOLIST (X *ESCAPE-KEYS*)
       (COND ((NULL (CAR X))
	      (SETQ INDENT 20.)
	      (FORMAT POP-UP-FINGER-WINDOW "~%~5XThese are for wizards:~2%"))
	     ((SETQ DOC (EVAL (CADDR X)))
	      (FORMAT POP-UP-FINGER-WINDOW "~:C~VT~A~%" (CAR X) INDENT
		      (IF (ATOM DOC) DOC (CAR DOC)))
	      (OR (ATOM DOC) (DOLIST (LINE (CDR DOC))
			       (FORMAT POP-UP-FINGER-WINDOW "~VT~A~%" INDENT LINE))))))
     (FORMAT POP-UP-FINGER-WINDOW "~3%~25TNew-keyboard function keys:

Macro		Keyboard macros (ed)		Abort		Kill running program
Terminal	The above commands		Break		Get read-eval-print loop
System		Select a Program		Resume		Continue from break/error
Network		Supdup//Telnet commands		Call		Stop program, get a Lisp
Quote		(not used)			Status		(not used)
Overstrike	/"backspace/"			Delete		(not used)
Clear-Input	Forget typein			End		Terminate input
Clear-Screen	Refresh screen			Help		Print documentation
Hold-Output	(not used)			Return		Carriage return
Stop-Output	(not used)			Line		Next line and indent (ed)
")
     (FORMAT POP-UP-FINGER-WINDOW "~%Type a space to flush: ")
     (FUNCALL POP-UP-FINGER-WINDOW ':TYI)))

;Keys you can type after SYSTEM.
;Each element is a list (character flavor documentation-string create-p)
;create-p is T if OK to create a window of that flavor, NIL if only select existing ones.
;If create-p is a list, it is the form to evaluate in a separate process to create
;one, otherwise the window is created the default way and assumed to provide its
;own process and whatever else it may require.
;In place of the flavor you may also have the window itself.
(DEFVAR *SYSTEM-KEYS*
     '(	(#/E NZWEI:ZMACS-FRAME "Editor" T)
	(#/I INSPECT-FRAME "Inspector" (TV:INSPECT))
	(#/L LISP-LISTENER "Lisp" T)
	(#/P PEEK "Peek" T)
	(#/R EH:ERROR-HANDLER-FRAME "Window error-handler" NIL)
	(#/S SUPDUP:SUPDUP "Supdup" T)
	(#/T SUPDUP:TELNET "Telnet" T) ))

(DEFUN KBD-SYS (&AUX CH)
  (LET-GLOBALLY ((WHO-LINE-PROCESS CURRENT-PROCESS))
    (WHO-LINE-RUN-STATE-UPDATE)  ;Necessary to make above take effect
    (SETQ CH (CHAR-UPCASE (KBD-GET-SOFTWARE-CHAR "System-"))))
  (WHO-LINE-RUN-STATE-UPDATE)	;Switch LAST-WHO-LINE-PROCESS back
  ;; Anything typed before the System belongs to the currently selected window
  ;; Anything typed after this belongs to the new window we are going to get to.
  (WITHOUT-INTERRUPTS
    (AND (KBD-GET-IO-BUFFER)
	 (KBD-SNARF-INPUT SELECTED-IO-BUFFER T)))
  (SETQ KBD-ESC-TIME (TIME))
  (PROCESS-RUN-FUNCTION "KBD SYS" #'KBD-SYS-1 CH))

(DEFUN KBD-SYS-1 (CH &AUX E W SW)
  (COND ((OR (= CH #/?) (= CH #\HELP))
	 (SETF (SHEET-TRUNCATE-LINE-OUT-FLAG POP-UP-FINGER-WINDOW) 0)
	 (FUNCALL POP-UP-FINGER-WINDOW ':SET-LABEL "Keyboard system commands")
	 (WINDOW-CALL (POP-UP-FINGER-WINDOW :DEACTIVATE)
	   (FORMAT POP-UP-FINGER-WINDOW
		   "Type ~:@C followed by one of these letters to select the corresponding ~
		    program:~2%~:{~C~8T~*~A~%~}"
		   #\SYSTEM *SYSTEM-KEYS*)
	   (FORMAT POP-UP-FINGER-WINDOW "~%Type a space to flush: ")
	   (FUNCALL POP-UP-FINGER-WINDOW ':TYI)))
	((SETQ E (ASSQ CH *SYSTEM-KEYS*))
	 ;; Find the most recently selected window of the desired type.
	 ;; If it is the same type as the selected window, make that the
	 ;; least recently selected so as to achieve the cycling-through effect.
	 ;; Otherwise the currently selected window becomes the most recently
	 ;; selected as usual, and esc S will return to it.
	 ;; In any case, we must fake out :MOUSE-SELECT's typeahead action since
	 ;; that has already been properly taken care of and we don't want to snarf
	 ;; any characters already typed after the [SYSTEM] command.
	 (DELAYING-SCREEN-MANAGEMENT	;Inhibit auto selection
	   (COND ((= (%DATA-TYPE (SECOND E)) DTP-INSTANCE)
		  (AND (SETQ SW SELECTED-WINDOW) (FUNCALL SW ':DESELECT NIL))
		  (FUNCALL (SECOND E) ':MOUSE-SELECT))
		 ((SETQ W (FIND-WINDOW-OF-FLAVOR (SECOND E)))	;Already exists?
		  (COND ((SETQ SW SELECTED-WINDOW)
			 (FUNCALL SW ':DESELECT NIL)
			 (AND (TYPEP SW (SECOND E))
			      (ADD-TO-PREVIOUSLY-SELECTED-WINDOWS SW T))))
		  (FUNCALL W ':MOUSE-SELECT))
		 ((TYPEP SELECTED-WINDOW (SECOND E))	;Already got one, don't make more
		  (BEEP))
		 ((NULL (FOURTH E)) (BEEP))	;Cannot create
		 ((EQ (FOURTH E) T)
		  (AND (SETQ SW SELECTED-WINDOW) (FUNCALL SW ':DESELECT NIL))
		  (FUNCALL (WINDOW-CREATE (SECOND E)) ':MOUSE-SELECT))
		 (T (EVAL (FOURTH E))))))
	(( CH #\RUBOUT) (BEEP)))
  (SETQ KBD-ESC-TIME NIL))

(DEFUN FIND-WINDOW-OF-FLAVOR (FLAVOR)
  ;; Only looks at PREVIOUSLY-SELECTED-WINDOWS, but that should have all the ones
  ;; of any interest.
  (DOTIMES (I (ARRAY-LENGTH PREVIOUSLY-SELECTED-WINDOWS))
    (LET ((W (AREF PREVIOUSLY-SELECTED-WINDOWS I)))
      (AND W (TYPEP W FLAVOR) (FUNCALL W ':NAME-FOR-SELECTION)
	   (RETURN W)))))

;;; Background stream

;(DEFVAR DEFAULT-BACKGROUND-STREAM 'BACKGROUND-STREAM)  ;in COLD
(DEFVAR BACKGROUND-STREAM-BELL-COUNT 3)
(DEFVAR PROCESS-IS-IN-ERROR NIL)
(DEFVAR BACKGROUND-INTERESTING-WINDOWS NIL)

(DEFMACRO MAKE-SELF-INTERESTING ()     
  `(PROGN
     (WITHOUT-INTERRUPTS
       (OR (MEMQ SELF BACKGROUND-INTERESTING-WINDOWS)
	   (PUSH SELF BACKGROUND-INTERESTING-WINDOWS)))
     (FUNCALL-SELF ':ACTIVATE)))

(DEFFLAVOR BACKGROUND-LISP-INTERACTOR () (LISP-INTERACTOR)
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION '(:BACKGROUND-TYPEOUT)))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :BEFORE :INIT) (PLIST)
  (PUTPROP PLIST T ':SAVE-BITS))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :SET-PROCESS) (NP)
  (SETF (IO-BUFFER-LAST-OUTPUT-PROCESS IO-BUFFER) NP)
  (SETQ PROCESS NP))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :BACKGROUND-TYPEOUT) ()
  (MAKE-SELF-INTERESTING)
  (WITHOUT-INTERRUPTS
    (AND SCREEN-ARRAY (SETF (SHEET-OUTPUT-HOLD-FLAG) 0)))
  (NOTIFY-USER "wants the TTY" SELF))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :AFTER :SELECT) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
    (SETQ BACKGROUND-INTERESTING-WINDOWS (DELQ SELF BACKGROUND-INTERESTING-WINDOWS))))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :AFTER :DEACTIVATE) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
    (SETQ BACKGROUND-INTERESTING-WINDOWS (DELQ SELF BACKGROUND-INTERESTING-WINDOWS))))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :WAIT-UNTIL-SEEN) ()
  ;; If we have typed out since we were selected last, then wait until we get seen
  (PROCESS-WAIT "Seen" #'(LAMBDA (S)
			   (NOT (MEMQ S BACKGROUND-INTERESTING-WINDOWS)))
		SELF)
  ;; Then wait until we are deselected
  (PROCESS-WAIT "No Longer Seen" #'(LAMBDA (S) (NEQ S SELECTED-WINDOW)) SELF))

(DEFMETHOD (BACKGROUND-LISP-INTERACTOR :BEFORE :TYI) (&REST IGNORE)
  (COND ((OR (FUNCALL-SELF ':LISTEN)
	     EXPOSED-P))
	(T (MAKE-SELF-INTERESTING)
	   (NOTIFY-USER "wants typein" SELF))))

(DEFVAR BACKGROUND-STREAM-WHICH-OPERATIONS)

(DEFUN BACKGROUND-STREAM (OP &REST ARGS)
  "This function is defaultly used as TERMINAL-IO for all processes.  If it gets called
at all, it turns TERMINAL-IO into a lisp listener window, and notifies the user that
the process wants the terminal."
  (IF (EQ TERMINAL-IO DEFAULT-BACKGROUND-STREAM)
      (SELECTQ OP
	(:WHICH-OPERATIONS 
	  ;; Get the which-operations once, but after the flavor has been compiled
	  (OR (BOUNDP 'BACKGROUND-STREAM-WHICH-OPERATIONS)
	      (SETQ BACKGROUND-STREAM-WHICH-OPERATIONS
		    (APPEND '(:NOTIFY :BEEP)
			    (FUNCALL (CAR BACKGROUND-LISP-INTERACTORS) ':WHICH-OPERATIONS))))
	  BACKGROUND-STREAM-WHICH-OPERATIONS)
	  ;; If the stream hasn't changed since the process was started, do default action
	(:BEEP
	 (LET ((W (WITHOUT-INTERRUPTS
		    (IF SELECTED-WINDOW
			(SHEET-GET-SCREEN SELECTED-WINDOW)
			DEFAULT-SCREEN))))
	   (LEXPR-FUNCALL W ':BEEP ARGS)))
	(OTHERWISE
	  (SETQ TERMINAL-IO (ALLOCATE-RESOURCE 'BACKGROUND-LISP-INTERACTORS))
	  (SHEET-FORCE-ACCESS (TERMINAL-IO :NO-PREPARE)
	    (FUNCALL TERMINAL-IO ':SET-LABEL (STRING-APPEND (PROCESS-NAME CURRENT-PROCESS)
							    " Background Stream"))
	    (FUNCALL TERMINAL-IO ':SET-PROCESS CURRENT-PROCESS)
	    (FUNCALL TERMINAL-IO ':CLEAR-SCREEN))
	  (FUNCALL TERMINAL-IO ':ACTIVATE)
	  (DOTIMES (I BACKGROUND-STREAM-BELL-COUNT) (BEEP))
	  (IF (EQ OP ':NOTIFY)
	      (NOTIFY-USER (CAR ARGS))
	      (LEXPR-FUNCALL TERMINAL-IO OP ARGS))))
      (SETQ TERMINAL-IO DEFAULT-BACKGROUND-STREAM)
      (LEXPR-FUNCALL TERMINAL-IO OP ARGS)))

(DEFUN NOTIFY-USER (MESSAGE &OPTIONAL (WINDOW TERMINAL-IO) &AUX NOTIFY-STREAM ERROR-P)
  (COND ((EQ MESSAGE ':ERROR)
	 (SETQ ERROR-P T)
	 (SETQ MESSAGE "got an error")))
  (OR (AND ERROR-P (SHEET-EXPOSED-P WINDOW))
      (DO ((INHIBIT-SCHEDULING-FLAG T T))
	  (SELECTED-WINDOW
	    ;; Notify the user only if he didn't select our window
	    (SETQ NOTIFY-STREAM (AND (OR (NOT ERROR-P) (NEQ SELECTED-WINDOW WINDOW))
				     ;; Notify user if the specified window isn't selected,
				     ;; or if it is a non-error notification
				     (FUNCALL SELECTED-WINDOW
					      ':NOTIFY-STREAM WINDOW))))
	(SETQ INHIBIT-SCHEDULING-FLAG NIL)
	(PROCESS-WAIT "A Selected Window" #'(LAMBDA () SELECTED-WINDOW))))
  (COND (NOTIFY-STREAM
	 (TV:BEEP)
	 (FORMAT NOTIFY-STREAM "~&[Process ~A ~A]~%" (PROCESS-NAME CURRENT-PROCESS) MESSAGE)))
  (AND ERROR-P
       (NOT (SHEET-EXPOSED-P WINDOW))
       ;; If notifying for an error, remain "in error" until selected
       (LET ((PROCESS-IS-IN-ERROR WINDOW))
	 (PROCESS-WAIT "Selected" #'(LAMBDA (W) (EQ SELECTED-WINDOW W)) WINDOW))))

;;; This is like the above function, without the various extra
;;; features.  It just gives a background process a stream on
;;; which it may print something it has to say.  By convention
;;; we usually enclose such messages and brackets and do a beep.
(DEFUN GET-NOTIFICATION-STREAM ()
 (DO ((INHIBIT-SCHEDULING-FLAG T T))
     (SELECTED-WINDOW
       (FUNCALL SELECTED-WINDOW ':NOTIFY-STREAM NIL))
   (SETQ INHIBIT-SCHEDULING-FLAG NIL)
   (PROCESS-WAIT "A Selected Window" #'(LAMBDA () SELECTED-WINDOW))))

(DEFUN FIND-PROCESS-IN-ERROR (&AUX WINDOW SG)
  (WITHOUT-INTERRUPTS
    (DOLIST (P ACTIVE-PROCESSES)
      (AND (SETQ P (CAR P))
	   (TYPEP (SETQ SG (PROCESS-STACK-GROUP P)) ':STACK-GROUP)
	   (SETQ WINDOW (SYMEVAL-IN-STACK-GROUP 'PROCESS-IS-IN-ERROR SG))
	   (RETURN P WINDOW)))))

(DEFUN FIND-INTERESTING-WINDOW ()
  (MULTIPLE-VALUE-BIND (NIL W)
      (FIND-PROCESS-IN-ERROR)
    (OR W (CAR BACKGROUND-INTERESTING-WINDOWS))))

;;; More or less innocuous functions from the old window system that are called all over the
;;; place.
(DEFUN KBD-TYI (&REST IGNORE) (FUNCALL TERMINAL-IO ':TYI))

(DEFUN KBD-TYI-NO-HANG (&REST IGNORE) (FUNCALL TERMINAL-IO ':TYI-NO-HANG))

(DEFUN KBD-CHAR-AVAILABLE (&REST IGNORE) (FUNCALL TERMINAL-IO ':LISTEN))
