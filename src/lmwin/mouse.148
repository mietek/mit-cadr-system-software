;;; -*- Mode:LISP; Package:TV; Base:8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Lisp Machine Mouse Handler
;;;  This doesn't use the hairier features of the CADR mouse interface.

(DEFVAR MOUSE-H1 -1)			;First hardware word (to detect status change)
(DEFVAR MOUSE-H2 -1)			;Second hardware word (..)
(DEFVAR MOUSE-X-SCALE '(2 . 3))		;Numerator . Denominator
(DEFVAR MOUSE-Y-SCALE '(3 . 5))
(DEFVAR MOUSE-LAST-X 0)			;To compute deltas without truncation error
(DEFVAR MOUSE-LAST-Y 0)
(DEFVAR MOUSE-LAST-BUTTONS 0)		;To compute change in buttons
(DEFVAR KBD-BUTTONS 0)			;buttons input via roman numerials on new keyboard.
					;These get IORed with buttons from the mouse.
(DEFVAR MOUSE-BOUNCE-TIME 200)		;Delay for bounce elimination
(DEFVAR MOUSE-DOUBLE-CLICK-TIME 6000)	;Delay for user to push button again
;(DEFVAR MOUSE-RECONSIDER)		;T => mouse process should return to overseer
					;and decide anew which window should get the mouse.
					;For use by :MOUSE-MOVES methods, etc.
(DEFVAR MOUSE-REG1 764104)		;Unibus addresses
(DEFVAR MOUSE-REG2 764106)

(DECLARE (SPECIAL FONTS:MOUSE))		;New 1980's mouse characters

;;; Low-level routines

;;; MOUSE-INPUT blocks until the mouse status changes (it moves or a button
;;; is depressed or raised).  It then returns 4 values: delta-X, delta-Y,
;;; buttons-newly-pushed, and buttons-newly-raised.
;;; There are 3 coordinate systems involved:
;;;  Table coordinates - physical motion of the mouse
;;;  Mouse coordinates - (only deltas exist here)  These are the table
;;;			 coordinates after (possibly non-linear) scaling.
;;;  Screen coordinates - these are where the mouse-blinker appears on
;;;			 the TV screen; the same as mouse coordinates except
;;;			 for non-simple geometry caused by e.g. scroll bars.

(DEFUN MOUSE-INPUT (&OPTIONAL (WAIT-FLAG T))
  (PROG (NEW-X NEW-Y X-MODULUS Y-MODULUS DELTA-X DELTA-Y NEW-BUTTONS CHANGED-BUTTONS)
    ;; Await a change in hardware status from what it was last time
    (COND (WAIT-FLAG
           ;; Due to the slowness of the mouse tracking operation, and the high speed
           ;; with which the mouse can interrupt, it's possible to hog the whole
           ;; machine.  The next line prevents that while still tracking reasonably.
           (PROCESS-ALLOW-SCHEDULE)
           (PROCESS-WAIT "MOUSE"
                         #'(LAMBDA (&AUX (NH1 (%UNIBUS-READ MOUSE-REG1)))
				   ;; Wait till mouse moves.
				   (OR MOUSE-RECONSIDER
				       ( MOUSE-LAST-BUTTONS
					  (LOGIOR KBD-BUTTONS (LDB 1403 NH1)))
				       ( (LOGAND 7777 (- MOUSE-H1 NH1)) 0)
				       ( (LOGAND 7777
						  (- MOUSE-H2
						     (%UNIBUS-READ MOUSE-REG2)))
					  0))))))
    ;; Get new position, converting from table coordinates to mouse coordinates
    ;; Also save the hardware status at this point
    (SETQ NEW-X (// (* (LOGAND 7777 (SETQ MOUSE-H2 (%UNIBUS-READ MOUSE-REG2)))
		       (CAR MOUSE-X-SCALE))
		    (CDR MOUSE-X-SCALE))
	  NEW-Y (// (* (LOGAND 7777 (SETQ MOUSE-H1 (%UNIBUS-READ MOUSE-REG1)))
		       (CAR MOUSE-Y-SCALE))
		    (CDR MOUSE-Y-SCALE)))
    ;; Compute moduli for wrap-around (these are constants unless scale is changed)
    (SETQ X-MODULUS (ABS (// (* 10000 (CAR MOUSE-X-SCALE)) (CDR MOUSE-X-SCALE)))
	  Y-MODULUS (ABS (// (* 10000 (CAR MOUSE-Y-SCALE)) (CDR MOUSE-Y-SCALE))))
    ;; Compute delta X and Y, allowing for wrap-around in the original table coordinates
    (SETQ DELTA-X (\ (- NEW-X MOUSE-LAST-X) X-MODULUS)
	  DELTA-Y (\ (- NEW-Y MOUSE-LAST-Y) Y-MODULUS))
    (AND ( DELTA-X (LSH X-MODULUS -1)) (SETQ DELTA-X (- DELTA-X X-MODULUS)))
    (AND ( DELTA-X (- (LSH X-MODULUS -1))) (SETQ DELTA-X (+ DELTA-X X-MODULUS)))
    (AND ( DELTA-Y (LSH Y-MODULUS -1)) (SETQ DELTA-Y (- DELTA-Y Y-MODULUS)))
    (AND ( DELTA-Y (- (LSH Y-MODULUS -1))) (SETQ DELTA-Y (+ DELTA-Y Y-MODULUS)))
    (SETQ MOUSE-LAST-X NEW-X MOUSE-LAST-Y NEW-Y)
    ;; Compute change in button status
    (SETQ NEW-BUTTONS (MOUSE-BUTTONS)
	  CHANGED-BUTTONS (LOGXOR NEW-BUTTONS MOUSE-LAST-BUTTONS)
	  MOUSE-LAST-BUTTONS NEW-BUTTONS)
    (RETURN DELTA-X
	    DELTA-Y
	    (LOGAND NEW-BUTTONS CHANGED-BUTTONS)
	    (BOOLE 2 NEW-BUTTONS CHANGED-BUTTONS)))) ;BOOLE 2 is ANDCA

;;; MOUSE-BUTTONS returns a word with a 1 for each button currently held down
(DEFUN MOUSE-BUTTONS ()
  (LOGIOR KBD-BUTTONS (LDB 1403 (%UNIBUS-READ MOUSE-REG1))))

;;; MOUSE-BUTTON-ENCODE
;;; When a mouse button has been pushed, and you want to support
;;; double-clicking, call this function.  It returns NIL if no
;;; button is pushed, or 2000 + 8 N + B, where B is the bit number
;;; in the button word, and N is one less than the number of clicks.
;;; Timing is computed by looping; in the future interrupts should
;;; be masked or timing should be computed by a clock.
;;; This function deliberately does not track the mouse, since
;;; motion while pushing buttons is probably unintentional.
;;; The argument, BD, is which buttons were just pushed, supplied by the caller
;;; who presumably got it from MOUSE-INPUT for the sake of good rollover.
(DEFUN MOUSE-BUTTON-ENCODE (BD &AUX BUTTON MASK CH)
  (COND (( (SETQ BUTTON (1- (HAULONG BD))) 0)  ;Pick a button that was just pushed
	 (SETQ MASK (LSH 1 BUTTON)
	       CH (DPB 1 %%KBD-MOUSE BUTTON))
	 (*CATCH 'MOUSE-BUTTON-ENCODE		;Throw CH here when ready to return
	   (DO () (NIL)				;Do forever (until guy's finger wears out)
	     (DO I MOUSE-BOUNCE-TIME (1- I) (= I 0))  ;Bounce delay
	     (DO I MOUSE-DOUBLE-CLICK-TIME (1- I) NIL ;Look for button to be lifted 
	       (COND ((= I 0)
		      (*THROW 'MOUSE-BUTTON-ENCODE CH))	;Timed out, button is still down
		     ((ZEROP (LOGAND (SETQ MOUSE-LAST-BUTTONS (MOUSE-BUTTONS)) MASK))
		      (RETURN NIL))))		;Button came up, enter next loop
	     (DO I MOUSE-BOUNCE-TIME (1- I) (= I 0))  ;Bounce delay		 
	     (DO I MOUSE-DOUBLE-CLICK-TIME (1- I) NIL ;Look for button to be pushed again
	       (COND ((= I 0)
		      (*THROW 'MOUSE-BUTTON-ENCODE CH))	;Timed out, button is still up
		     ((NOT (ZEROP (LOGAND (SETQ MOUSE-LAST-BUTTONS (MOUSE-BUTTONS)) MASK)))
		      (SETQ CH (+ CH 8))	;Button pushed again, this is a double click
		      (RETURN NIL)))))))))	;Continue scanning <for triple click!>

;;; Middle-level routines

(DEFVAR MOUSE-PROCESS)			;This global-process is in charge of the mouse
(DEFVAR MOUSE-BLINKER NIL)		;This blinker shows where the mouse "is"
(DEFVAR MOUSE-BLINKER-NAME NIL)		;The type of MOUSE-BLINKER
;(DEFVAR MOUSE-WINDOW)			;Window controlling the mouse, NIL if none
;(DEFVAR WINDOW-OWNING-MOUSE)		;NIL, or window which has seized the mouse, or
;					;T if someone has seized the mouse and can't identify
;					;himself as any particular window,
;					;or STOP to make the mouse process do nothing.
;(DEFVAR MOUSE-X)			;X coordinate of MOUSE-BLINKER
;(DEFVAR MOUSE-Y)			;Y coordinate of MOUSE-BLINKER
(DEFVAR MOUSE-SHEET)			;Which sheet MOUSE-BLINKER lives on
(OR MOUSE-SHEET (SETQ MOUSE-SHEET DEFAULT-SCREEN))

(DEFUN MOUSE-STANDARD-BLINKER (&OPTIONAL (WINDOW (WINDOW-OWNING-MOUSE)))
  (AND (SYMBOLP WINDOW)
       (SETQ WINDOW MOUSE-SHEET))
  (FUNCALL WINDOW ':MOUSE-STANDARD-BLINKER))

(DEFMETHOD (SHEET :MOUSE-STANDARD-BLINKER) ()
  (FUNCALL SUPERIOR ':MOUSE-STANDARD-BLINKER))

(DEFMETHOD (SCREEN :MOUSE-STANDARD-BLINKER) ()
  (MOUSE-SET-BLINKER-DEFINITION ':CHARACTER 0 0 ':ON
				':SET-CHARACTER 6 'FONTS:MOUSE))

(DEFMETHOD (SCREEN :MOUSE-FONT) () (FUNCALL-SELF ':PARSE-FONT-DESCRIPTOR 'FONTS:MOUSE))

(DEFMETHOD (MOUSE-BLINKER-MIXIN :OFFSETS) ()
  (PROG () (RETURN X-OFFSET Y-OFFSET)))

(DEFMETHOD (MOUSE-BLINKER-MIXIN :SET-OFFSETS) (X Y)
  (SETQ X-OFFSET X
	Y-OFFSET Y))

(DEFFLAVOR MOUSE-CHARACTER-BLINKER () (MOUSE-BLINKER-MIXIN CHARACTER-BLINKER))
(DEFFLAVOR MOUSE-RECTANGULAR-BLINKER () (MOUSE-BLINKER-MIXIN RECTANGULAR-BLINKER))
(DEFFLAVOR MOUSE-HOLLOW-RECTANGULAR-BLINKER
	()
	(MOUSE-BLINKER-MIXIN HOLLOW-RECTANGULAR-BLINKER))

(DEFVAR MOUSE-BLINKER-TYPES NIL)
(DEFUN MOUSE-DEFINE-BLINKER-TYPE (TYPE CREATION-FUN)
  (SETQ MOUSE-BLINKER-TYPES (DELQ (ASSQ TYPE MOUSE-BLINKER-TYPES) MOUSE-BLINKER-TYPES))
  (PUSH (CONS TYPE CREATION-FUN) MOUSE-BLINKER-TYPES)
  (MOUSE-GET-BLINKER TYPE))

(DEFUN MOUSE-GET-BLINKER (TYPE &OPTIONAL (SHEET MOUSE-SHEET)
			       &AUX (SCREEN (SHEET-GET-SCREEN SHEET))
			       BLINKERS)
  (LET ((BE (ASSQ TYPE MOUSE-BLINKER-TYPES)))
    (OR BE (FERROR NIL "~A is unknown mouse blinker type" TYPE))
    (LET ((BL (CDR (ASSQ TYPE (SETQ BLINKERS (FUNCALL SCREEN ':MOUSE-BLINKERS))))))
      (COND ((NULL BL)
	     (SETQ BL (FUNCALL (CDR BE) SCREEN))
	     (PUSH (CONS TYPE BL) BLINKERS)
	     (FUNCALL SCREEN ':SET-MOUSE-BLINKERS BLINKERS)))
      (BLINKER-SET-SHEET BL SHEET)
      BL)))

(DEFUN MOUSE-SET-BLINKER-DEFINITION (TYPE X-OFF Y-OFF VISIBILITY
				     MESSAGE &REST MESSAGE-ARGS)
  (LET ((BL (MOUSE-GET-BLINKER TYPE)))
    (AND MOUSE-BLINKER (NEQ BL MOUSE-BLINKER) (FUNCALL MOUSE-BLINKER ':SET-VISIBILITY NIL))
    (LEXPR-FUNCALL BL MESSAGE MESSAGE-ARGS)
    (FUNCALL BL ':SET-OFFSETS X-OFF Y-OFF)
    (FUNCALL BL ':SET-VISIBILITY (IF (EQ VISIBILITY ':ON) T VISIBILITY))
    (SETQ MOUSE-BLINKER BL
	  MOUSE-BLINKER-NAME TYPE)
    BL))

(DEFUN MOUSE-SET-BLINKER (TYPE &OPTIONAL X-OFF Y-OFF)
  (LET ((BL (MOUSE-GET-BLINKER TYPE)))
    (AND (NEQ BL MOUSE-BLINKER) (FUNCALL MOUSE-BLINKER ':SET-VISIBILITY NIL))
    (AND X-OFF (FUNCALL BL ':SET-OFFSETS X-OFF Y-OFF))
    (FUNCALL BL ':SET-VISIBILITY T)
    (SETQ MOUSE-BLINKER BL
	  MOUSE-BLINKER-NAME TYPE)
    BL))

(MOUSE-DEFINE-BLINKER-TYPE ':CHARACTER
			   #'(LAMBDA (SCREEN)
			       (DEFINE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
				 ':VISIBILITY T
				 ':FONT (FUNCALL SCREEN ':MOUSE-FONT)
				 ':CHAR 6)))
				 
(MOUSE-DEFINE-BLINKER-TYPE ':RECTANGLE-BLINKER
			   #'(LAMBDA (SCREEN)
			       (DEFINE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
				 ':VISIBILITY NIL
				 ':FONT (FUNCALL SCREEN ':MOUSE-FONT)
				 ':CHAR 21)))

(DEFUN MOUSE-SET-BLINKER-CURSORPOS (&REST IGNORE)
  (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
      (FUNCALL MOUSE-BLINKER ':OFFSETS)
    (BLINKER-SET-CURSORPOS MOUSE-BLINKER (- MOUSE-X X-OFF) (- MOUSE-Y Y-OFF))))

(DEFUN MOUSE-CALL-SYSTEM-MENU (&OPTIONAL (SUP MOUSE-SHEET))
  (PROCESS-RUN-FUNCTION "System Menu"
			#'(LAMBDA (SUP &AUX MENU)
			    (SETQ MENU (GET-A-SYSTEM-WINDOW 'SYSTEM-MENU SUP NIL))
			    (AND MENU (FUNCALL MENU ':CHOOSE)))
			SUP))


;;; This function is called during loading and from LISP-REINITIALIZE
;;; to initialize the mouse process and associated variable.
(DEFUN MOUSE-INITIALIZE (&OPTIONAL (SHEET DEFAULT-SCREEN))
  (OR (BOUNDP 'MOUSE-PROCESS)	;If first time loaded, initialize everything
      (SETQ MOUSE-PROCESS (PROCESS-CREATE "Mouse" ':SPECIAL-PDL-SIZE 2000.)))
  (SETQ MOUSE-WINDOW NIL
	WINDOW-OWNING-MOUSE NIL
	MOUSE-X 0
	MOUSE-Y 0
	MOUSE-SHEET SHEET)
  (AND MOUSE-BLINKER (BLINKER-SET-VISIBILITY MOUSE-BLINKER NIL))
  (MOUSE-STANDARD-BLINKER)
  (MOUSE-WARP (- (SHEET-INSIDE-WIDTH MOUSE-SHEET) 8)
	      (- (SHEET-INSIDE-HEIGHT MOUSE-SHEET) 16.))
  ;; Call MOUSE-INPUT once to flush any pending motion and update variables, but don't wait.
  (MOUSE-INPUT NIL)
  ;; Start up the mouse process
  (FUNCALL MOUSE-PROCESS ':PRESET 'MOUSE-OVERSEER)
  (FUNCALL MOUSE-PROCESS ':RUN-REASON))

;;; This function changes the MOUSE-SHEET
(DEFUN MOUSE-SET-SHEET (NEW-SHEET)
  (WITH-MOUSE-USURPED
    (SETQ MOUSE-SHEET NEW-SHEET)
    (MULTIPLE-VALUE (MOUSE-X MOUSE-Y)
      (FUNCALL MOUSE-BLINKER ':READ-CURSORPOS))
    (MOUSE-STANDARD-BLINKER)))

;;; This function "warps" the mouse to a specified place
(DEFUN MOUSE-WARP (X Y)
  (COND ((OR ( MOUSE-X X) ( MOUSE-Y Y))
	 (SETQ MOUSE-X (MAX 0 (MIN (1- (SHEET-INSIDE-WIDTH MOUSE-SHEET)) X)))
	 (SETQ MOUSE-Y (MAX 0 (MIN (1- (SHEET-INSIDE-HEIGHT MOUSE-SHEET)) Y)))
	 (MOUSE-SET-BLINKER-CURSORPOS)
	 (MOUSE-WAKEUP))))			;Make sure the mouse tracker process notices

;;; This returns the lowest window under the mouse prepared to handle the given operation.
(DEFUN WINDOW-UNDER-MOUSE (&OPTIONAL METHOD (ACTIVE-CONDITION ':ACTIVE))
  (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y METHOD ACTIVE-CONDITION))

;;; This is the window or special thing that owns the mouse
(DEFUN WINDOW-OWNING-MOUSE ()
  (OR WINDOW-OWNING-MOUSE
      (WINDOW-UNDER-MOUSE ':HANDLE-MOUSE ':EXPOSED)))

;;; Use this to tell whether a certain window owns the mouse, or one of its inferiors does.
(DEFUN WINDOW-OWNS-MOUSE-P (WINDOW)
    (COND ((EQ WINDOW T)
	   (EQ WINDOW-OWNING-MOUSE T))
          (WINDOW-OWNING-MOUSE
	    (DO W WINDOW (SHEET-SUPERIOR W) (NULL W)
	      (AND (EQ W WINDOW-OWNING-MOUSE) (RETURN T))))
	  (T
	   (AND (SHEET-EXPOSED-P WINDOW)
		(SHEET-CONTAINS-SHEET-POINT-P WINDOW MOUSE-SHEET MOUSE-X MOUSE-Y)))))

;;; A window's process should call this function when it wishes to seize
;;; the mouse to use it to get input that can range over the whole screen.
;;; To free the mouse again, setq WINDOW-OWNING-MOUSE to NIL again.
(DEFUN MOUSE-SEIZE ()
  (SETQ WINDOW-OWNING-MOUSE SELECTED-WINDOW))

;;; Call this function to awaken the mouse process
;;; whenever the layout of the screen changes, so that it can call WINDOW-OWNING-MOUSE
;;; and find out that the mouse should be under control of a different window.
(DEFUN MOUSE-WAKEUP ()
  (SETQ MOUSE-H1 -1))  ;This should be a sufficiently kludgey way of kicking the mouse.

;;; MOUSE-OVERSEER
;;; This is the top-level function of the mouse process.  It tracks the
;;; mouse and does default things with the buttons.  If the mouse enters
;;; the region of the screen occupied by a window that has its own mouse handler,
;;; that mouse handler is called.  It then has control of the mouse until
;;; it returns.  Both this function and specialized mouse handlers are
;;; to call the above low-level routines.  MOUSE-WINDOW is the window which
;;; has control of the mouse, or NIL if there is none.  Note that window
;;; need not be selected, nor exposed.  It does have to be current.
;;; Mouse handlers are free to mung the mouse blinker however they like.
;;; The mouse overseer is guaranteed to put it back.

;;; Most mouse handlers will return whenever WINDOW-OWNING-MOUSE says that the
;;; mouse has moved outside of the visible part of that window.  Some however,
;;; will not return until they feel like it.
;;; The convention to be used is up to the individual handler.

;;; Frobbing with MOUSE-RECONSIDER is for race-free interface with WITH-MOUSE-GRABBED
(DEFUN MOUSE-OVERSEER ()
  (DO () (NIL)
    (*CATCH 'SI:TOP-LEVEL
      (DO () (NIL)
	(MOUSE-SET-BLINKER-CURSORPOS)	      
	(COND ((EQ (SETQ MOUSE-WINDOW (WINDOW-OWNING-MOUSE)) 'STOP)
	       (SETQ MOUSE-RECONSIDER NIL)
	       (PROCESS-WAIT "Usurped"
			     #'(LAMBDA () (OR MOUSE-RECONSIDER	;This can happen randomly 
					      (NEQ WINDOW-OWNING-MOUSE 'STOP)))))
	      ((NOT (SYMBOLP MOUSE-WINDOW))
	       (SETQ MOUSE-RECONSIDER NIL)
	       (FUNCALL MOUSE-WINDOW ':HANDLE-MOUSE))
	      (T
	       ;; Standardize the blinker if no one else will
	       (OR MOUSE-WINDOW (MOUSE-STANDARD-BLINKER MOUSE-SHEET))
	       (SETQ MOUSE-RECONSIDER NIL)
	       (MOUSE-DEFAULT-HANDLER MOUSE-WINDOW)))))))

;; Magic adjustments

;; Magic top-level values for scroll bar parameters
(DEFVAR SCROLL-BAR-MAX-SPEED 10.)		;10 inches per second, speed at which
						; scroll bar gets ignored
(DEFVAR SCROLL-BAR-RELUCTANCE 10.)		;10 pixels before entering scroll bar
(DEFVAR SCROLL-BAR-WIDTH 3)			;3 pixels wide
(DEFVAR SCROLL-BAR-EXIT-RELUCATANCE 300.)	;Hard to get out of scroll bar

(DEFVAR MOUSE-FAST-MOTION-SPEED 30.)		;Moving faster than 30. inches per second
(DEFVAR MOUSE-FAST-MOTION-CROSS-SIZE 40.)	; triggers a cross 1 cm in diameter
(DEFVAR MOUSE-FAST-MOTION-CROSS-TIME 2000.)	; which lasts this long (DO-loop units)
(DEFVAR MOUSE-SPEED 0)				; Speed mouse is travelling

;; This mouse handler serves for windows which want to do things the simple way.
;; A second argument of T says that the window should have a scroll bar.
;; This function is also used to track the mouse when it isn't inside any window,
;; by calling it with an argument of NIL.
;; An arg of T is used when the mouse has been seized by a process not
;; for any specific window.
(DEFUN MOUSE-DEFAULT-HANDLER (WINDOW &OPTIONAL SCROLL-BAR-FLAG
				     &AUX MOVE-HANDLER
				     (MIN-X 0) (MIN-Y 0)
				     (MAX-X (1- (SHEET-INSIDE-WIDTH MOUSE-SHEET)))
				     (MAX-Y (1- (SHEET-INSIDE-HEIGHT MOUSE-SHEET)))
				     (WINDOW-X-OFFSET 0) (WINDOW-Y-OFFSET 0)
				     WINDOW-X WINDOW-Y
				     (MOVE-METHOD (COND ((EQ SCROLL-BAR-FLAG ':IN)
							 ':MOUSE-MOVES-SCROLL)
							(T ':MOUSE-MOVES)))
				     (BUTTONS-METHOD (COND ((EQ SCROLL-BAR-FLAG ':IN)
							    ':MOUSE-BUTTONS-SCROLL)
							   (T ':MOUSE-BUTTONS))))
  (SETQ MOVE-HANDLER
	(OR (AND (NOT (SYMBOLP WINDOW)) (GET-HANDLER-FOR WINDOW MOVE-METHOD))
	    #'MOUSE-SET-BLINKER-CURSORPOS))
  (OR (SYMBOLP WINDOW)
      (MULTIPLE-VALUE (WINDOW-X-OFFSET WINDOW-Y-OFFSET)
	(SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET)))
  (SETQ WINDOW-X (- MOUSE-X WINDOW-X-OFFSET)
	WINDOW-Y (- MOUSE-Y WINDOW-Y-OFFSET))
  (FUNCALL MOVE-HANDLER MOVE-METHOD WINDOW-X WINDOW-Y)
  (DO ((DX) (DY) (BU) (BD) (HAND)
       (OLD-OWNER WINDOW-OWNING-MOUSE WINDOW-OWNING-MOUSE)
       (X-OFFSET 0)
       (LAST-TIME NIL THIS-TIME) (THIS-TIME))
      (MOUSE-RECONSIDER)
    (MULTIPLE-VALUE (DX DY BD BU) (MOUSE-INPUT))
    ;; If asked to reconsider, do so now.  But don't lose mouse motion that
    ;; happened since we last woke up, which may be a considerable time ago
    ;; when things requiring reconsideration are happening.
    ;; Don't bother updating blinker since it is likely to change soon, and
    ;; in any case we are going to be called back shortly.
    (COND (MOUSE-RECONSIDER
	   (SETQ MOUSE-X (MIN MAX-X (+ MOUSE-X DX))
		 MOUSE-Y (MAX MIN-Y (MIN MAX-Y (+ MOUSE-Y DY))))
	   (RETURN NIL)))
    ;; Approximate speed of the mouse in inches per second
    (SETQ THIS-TIME (TIME:FIXNUM-MICROSECOND-TIME)
	  MOUSE-SPEED (IF LAST-TIME (// (* (+ (ABS DX) (ABS DY)) 1.0s4)
					(MAX (TIME-DIFFERENCE THIS-TIME LAST-TIME) 1))
			  0))
    (COND ((OR OLD-OWNER (ZEROP BD))
	   ;; If no buttons pushed, or mouse is seized, track the motion of the mouse
	   ;; Update mouse's new location.
	   (SETQ MOUSE-X (MIN MAX-X (+ MOUSE-X DX))
		 MOUSE-Y (MAX MIN-Y (MIN MAX-Y (+ MOUSE-Y DY)))
		 WINDOW-X (- MOUSE-X WINDOW-X-OFFSET)
		 WINDOW-Y (- MOUSE-Y WINDOW-Y-OFFSET))
	   ;; If the mouse is moving incredibly fast, flash up something to
	   ;; help the user find it.  Thus if you can't find the mouse, just jerk it.
	   (COND ((> MOUSE-SPEED MOUSE-FAST-MOTION-SPEED)
		  (LET ((REAL-MOUSE-X (MAX MOUSE-X MIN-X)))	;Compensate for kludge
		    (LET ((XTOP (MAX (- MOUSE-Y MOUSE-FAST-MOTION-CROSS-SIZE) MIN-Y))
			  (XBOTTOM (MIN (+ MOUSE-Y MOUSE-FAST-MOTION-CROSS-SIZE 20) MAX-Y))
			  (XLEFT (MAX (- REAL-MOUSE-X MOUSE-FAST-MOTION-CROSS-SIZE) MIN-X))
			  (XRIGHT (MIN (+ REAL-MOUSE-X MOUSE-FAST-MOTION-CROSS-SIZE 20)
				       MAX-X))
			  (XX (MIN REAL-MOUSE-X (- MAX-X 20)))
			  (YY (MIN MOUSE-Y (- MAX-Y 20))))
		      (WITHOUT-INTERRUPTS
		        (%DRAW-RECTANGLE 20 (- XBOTTOM XTOP) XX XTOP ALU-XOR MOUSE-SHEET)
			(%DRAW-RECTANGLE (- XRIGHT XLEFT) 20 XLEFT YY ALU-XOR MOUSE-SHEET)
			(DOTIMES (I MOUSE-FAST-MOTION-CROSS-TIME) )
			(%DRAW-RECTANGLE 20 (- XBOTTOM XTOP) XX XTOP ALU-XOR MOUSE-SHEET)
			(%DRAW-RECTANGLE (- XRIGHT XLEFT) 20 XLEFT YY ALU-XOR MOUSE-SHEET))
		      ))))
	   ;; If there is a scroll bar and we are entering it, activate it.
	   ;; However, the mouse must move at least a certain distance past the left
	   ;; edge of the window in order to qualify for scrolling (this is set by
	   ;; the SCROLL-BAR-RELUCTANCE variable in the window).  Before entering
	   ;; scroll bar, send a :MOUSE-MOVES message in order to let the window know
	   ;; what's happening.
	   (COND ((AND (EQ SCROLL-BAR-FLAG 'T)
		       (NOT OLD-OWNER)
		       (NOT WINDOW-OWNING-MOUSE)
		       (< MOUSE-X (- WINDOW-X-OFFSET (MIN SCROLL-BAR-RELUCTANCE 0))))
		  (SETQ X-OFFSET
			(+ X-OFFSET (MAX 0 (- WINDOW-X-OFFSET MOUSE-X))))
		  (FUNCALL MOVE-HANDLER MOVE-METHOD WINDOW-X WINDOW-Y)
		  (COND ((AND SCROLL-BAR-MAX-SPEED
			      (> MOUSE-SPEED SCROLL-BAR-MAX-SPEED))
			 (RETURN NIL))	 ;Too fast, pass right through
			((> X-OFFSET SCROLL-BAR-RELUCTANCE)
			 (RETURN (FUNCALL WINDOW ':HANDLE-MOUSE-SCROLL)))
			(T (SETQ MOUSE-X WINDOW-X-OFFSET))))
		 ((AND (EQ SCROLL-BAR-FLAG ':IN)
		       (NOT OLD-OWNER)
		       (NOT WINDOW-OWNING-MOUSE))
		  (COND ((AND (> WINDOW-X 0) ( WINDOW-X SCROLL-BAR-WIDTH))
			 (SETQ X-OFFSET 0))
			((AND SCROLL-BAR-MAX-SPEED
			      (> MOUSE-SPEED SCROLL-BAR-MAX-SPEED))			      
			 ;; Moving like a bat, let the guy out of the scroll bar
			 (RETURN NIL))
			(T
			 (SETQ X-OFFSET (+ X-OFFSET WINDOW-X))
			 ;; Moved too far, let him out
			 (AND (> (ABS X-OFFSET) SCROLL-BAR-EXIT-RELUCATANCE)
			      (RETURN T))
			 (SETQ MOUSE-X (IF ( WINDOW-X 0)
					   WINDOW-X-OFFSET
					   (+ WINDOW-X-OFFSET SCROLL-BAR-WIDTH))))))
		 (T (SETQ X-OFFSET 0)))
	   ;; Check for left margin after scroll bar, so windows on edge work
	   (SETQ MOUSE-X (MAX MIN-X MOUSE-X))
	   ;; Put the mouse blinker where the mouse is, with offset
	   ;; to put the center of the X at the right spot.
	   ;; Call the move handler before checking for exiting the window,
	   ;; so that it gets a chance to do final things, and so that
	   ;; it can warp the mouse back into the window if it chooses to.
	   (SETQ WINDOW-X (- MOUSE-X WINDOW-X-OFFSET)
		 WINDOW-Y (- MOUSE-Y WINDOW-Y-OFFSET))
	   (FUNCALL MOVE-HANDLER MOVE-METHOD WINDOW-X WINDOW-Y)
	   ;; If mouse has moved out of this window, return to overseer
	   (COND ((OR WINDOW-OWNING-MOUSE (EQ WINDOW T))
		  (OR (WINDOW-OWNS-MOUSE-P WINDOW) (RETURN NIL)))
		 (WINDOW
		   (OR (AND (SHEET-EXPOSED-P WINDOW)
			    (SHEET-CONTAINS-SHEET-POINT-P WINDOW MOUSE-SHEET
							  MOUSE-X MOUSE-Y))
		       (RETURN NIL)))
		 ;; If wasn't in any window, but has moved into one or been seized,
		 ;; return to overseer
		 ((WINDOW-OWNING-MOUSE) (RETURN NIL)))
	   ;; Check for slipping into an inferior
	   (AND (NOT (SYMBOLP WINDOW))
		(DOLIST (INFERIOR (SHEET-EXPOSED-INFERIORS WINDOW))
		  (AND (SHEET-CONTAINS-SHEET-POINT-P INFERIOR WINDOW WINDOW-X WINDOW-Y)
		       (GET-HANDLER-FOR INFERIOR ':HANDLE-MOUSE)
		       (RETURN T)))
		(RETURN NIL)))
	  ;; Update idle time when buttons pushed
	  ((PROG1 NIL (SETQ KBD-LAST-ACTIVITY-TIME (TIME))))
	  ;; We do nothing with buttons when seized nonspecifically.
	  ((EQ WINDOW T))
	  ;; If over an exposed window, send it the button-push
	  (WINDOW (FUNCALL WINDOW BUTTONS-METHOD BD WINDOW-X WINDOW-Y))
	  ;; If argument is NIL, see if was really over an exposed window.
	  ((SETQ HAND (WINDOW-UNDER-MOUSE BUTTONS-METHOD ':EXPOSED))
	   (MULTIPLE-VALUE-BIND (XOFF YOFF)
	       (SHEET-CALCULATE-OFFSETS HAND MOUSE-SHEET)
	     (FUNCALL HAND BUTTONS-METHOD BD (- MOUSE-X XOFF) (- MOUSE-Y YOFF))))
	  ;; If not over an exposed window, see if over one which can be
	  ;; exposed and selected by the mouse.  If so, call the default buttons handler.
	  ((SETQ HAND (WINDOW-UNDER-MOUSE ':MOUSE-SELECT))
	   (MOUSE-BUTTONS-DEFAULT BD HAND))
	  ;; Nothing willing to handle the buttons, just call the system menu
	  (T (MOUSE-CALL-SYSTEM-MENU)))))

;It is often useful for mouse commands to keep doing things in the window's process
;as MOUSE-DEFAULT-HANDLER keeps tracking the mouse and updating MOUSE-X and MOUSE-Y
;in the mouse process.  The mouse command can use this function to wait for
;something to happen.  For best results, to avoid timing errors,
;the command should examine the values of MOUSE-X and MOUSE-Y,
;use them, and supply the same values as arguments to this function.
(DEFUN MOUSE-WAIT (&OPTIONAL (OLD-X MOUSE-X) (OLD-Y MOUSE-Y) (OLD-BUTTONS MOUSE-LAST-BUTTONS))
  (PROCESS-WAIT "MOUSE"
    (FUNCTION (LAMBDA (OLD-X OLD-Y OLD-BUTTONS)
		(OR ( MOUSE-X OLD-X)
		    ( MOUSE-Y OLD-Y)
		    ( MOUSE-LAST-BUTTONS OLD-BUTTONS))))
    OLD-X OLD-Y OLD-BUTTONS))

(DEFFLAVOR ESSENTIAL-MOUSE () ()
  (:INCLUDED-FLAVORS ESSENTIAL-WINDOW))

(DEFMETHOD (ESSENTIAL-MOUSE :HANDLE-MOUSE) ()
  (MOUSE-STANDARD-BLINKER SELF)
  (MOUSE-DEFAULT-HANDLER SELF (FUNCALL-SELF ':SCROLL-BAR-P)))

(DEFMETHOD (ESSENTIAL-MOUSE :SET-MOUSE-CURSORPOS) (X Y)
  (FUNCALL-SELF ':SET-MOUSE-POSITION (+ (SHEET-INSIDE-LEFT) X) (+ (SHEET-INSIDE-TOP) Y)))

(DEFMETHOD (ESSENTIAL-MOUSE :SET-MOUSE-POSITION) (X Y &AUX X-OFF Y-OFF)
  (COND ((SHEET-ME-OR-MY-KID-P SELF MOUSE-SHEET)
	 (MULTIPLE-VALUE (X-OFF Y-OFF)
	   (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
	 (SETQ X (IF X
		     (+ X X-OFF)
		     MOUSE-X)
	       Y (IF Y
		     (+ Y Y-OFF)
		     MOUSE-Y))
	 (MOUSE-WARP X Y))
	(T (FERROR NIL
	     "Attempt to :SET-MOUSE-POSITION on a ~S, which is not related to MOUSE-SHEET."
	     SELF))))

(DEFMETHOD (ESSENTIAL-MOUSE :MOUSE-BUTTONS) (BD IGNORE IGNORE)
  (MOUSE-BUTTONS-DEFAULT BD))

(DEFUN MOUSE-BUTTONS-DEFAULT (BD &OPTIONAL (WINDOW SELF))
  (COND ((BIT-TEST 1 BD) (PROCESS-RUN-FUNCTION "Mouse Select" #'MOUSE-SELECT WINDOW))
	((BIT-TEST 2 BD) (FUNCALL WINDOW ':DOCUMENT))
	((BIT-TEST 4 BD) (MOUSE-CALL-SYSTEM-MENU))))

(DEFUN MOUSE-SELECT (WINDOW)
  (OR (FUNCALL WINDOW ':MOUSE-SELECT NIL)
      (BEEP)))

(DEFMETHOD (ESSENTIAL-MOUSE :DOCUMENT) () (BEEP))

(DEFMETHOD (ESSENTIAL-MOUSE :SCROLL-BAR-P) () NIL)

(DEFFLAVOR KBD-MOUSE-BUTTONS-MIXIN () ()
  (:INCLUDED-FLAVORS ESSENTIAL-MOUSE)
  (:DOCUMENTATION :MIXIN "Sticks clicks in input buffer as characters
Clicking on the window when it is not selected will select it; mouse-right-twice
calls the system menu; any other number of mouse clicks is stuck send as a fixnum
via :force-kdb-input, %%kbd-mouse-button is button clicked on, %%kbd-mouse-n-clicks
the number of click."))

(DEFMETHOD (KBD-MOUSE-BUTTONS-MIXIN :MOUSE-BUTTONS) (BD X Y)
  X Y
  (SETQ KBD-LAST-ACTIVITY-TIME (TIME))
  (LET ((BUTTONS (MOUSE-BUTTON-ENCODE BD)))
    (COND ((= BUTTONS #\MOUSE-3-2)
	   (MOUSE-CALL-SYSTEM-MENU))
	  ((NEQ SELF SELECTED-WINDOW)
	   (PROCESS-RUN-FUNCTION "Mouse Select" SELF ':SELECT))
	  (T
	   (FUNCALL-SELF ':FORCE-KBD-INPUT BUTTONS)))))

(DEFFLAVOR HYSTERETIC-WINDOW-MIXIN ((HYSTERESIS 25.)) ()
  (:INCLUDED-FLAVORS ESSENTIAL-WINDOW)
  (:SETTABLE-INSTANCE-VARIABLES HYSTERESIS)
  (:DOCUMENTATION :MIXIN "Controls mouse for small area outside of itself too.
The hysteresis instance variable is the number of pixels outside of its own
area within the :handle-mouse method still retain control."))

(DEFMETHOD (HYSTERETIC-WINDOW-MIXIN :HANDLE-MOUSE) ()
  (LET (LEFT-LIM TOP-LIM
	RIGHT-LIM BOTTOM-LIM)
    (MULTIPLE-VALUE (LEFT-LIM TOP-LIM)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
    (SETQ RIGHT-LIM (+ LEFT-LIM WIDTH HYSTERESIS)
	  BOTTOM-LIM (+ TOP-LIM HEIGHT HYSTERESIS)
	  LEFT-LIM (- LEFT-LIM HYSTERESIS)
	  TOP-LIM (- TOP-LIM HYSTERESIS))
    (MOUSE-STANDARD-BLINKER SELF)
    (DO () (())
      ;; Let the mouse out of the window only if it moves more than <hysteresis> away
      (AND (OR MOUSE-RECONSIDER
	       (< MOUSE-X LEFT-LIM)
	       (> MOUSE-X RIGHT-LIM)
	       (< MOUSE-Y TOP-LIM)
	       (> MOUSE-Y BOTTOM-LIM))
	 (RETURN T))
      (MOUSE-DEFAULT-HANDLER SELF (FUNCALL-SELF ':SCROLL-BAR-P)))))

;;; The Scroll-Bar

;;; Moving out the left edge of a window that has a scroll bar causes the
;;; window's mouse handler to call this routine.
;;; While the mouse is inside the scroll bar, the mouse buttons are used
;;; to scroll the window for which the scroll bar was brought up.
;;; You can tell that you are in the scroll bar, because the mouse cursor
;;; changes to a fat doubleheaded arrow and part of the left margin of
;;; of the window is darkened, showing where in the buffer is the visible portion.

;;; The commands in the scroll bar are:
;;;   Left: Move the line next to the mouse to the top.
;;;   Left-double: Move the line next to the mouse to the bottom.
;;;   Right: Move the top line to the place where the mouse is.
;;;   Right-double: Move the bottom line to the place where the mouse is.
;;;		Note that this does not call the system menu as you would
;;;		normally expect.
;;;   Middle: Jump to a place in the "buffer" as far (proportionally) from
;;;	     the beginning as the mouse is from the top.
;;;   Middle-double: not used


;;; The window should handle the :SCROLL-POSITION message by returning these four values:
;;;  TOP-LINE-NUM - the line-number of the line currently at the top of the window
;;;  TOTAL-LINES - the total number of lines available to scroll through.
;;;  LINE-HEIGHT - the height (in pixels) of a line
;;;  N-ITEMS - the number of items displayed on the screen (there are occaisions where
;;;	       this is not trivially calcuable from the other information)
;;; The window should handle the message :SCROLL-TO line-number by scrolling
;;;  to that line, or trying to.  This handler should truncate its arg into range.

;;; A window which can use the scroll bar should send a :NEW-SCROLL-POSITION
;;;  message to itself whenever it scrolls, for any reason, mouse-related or not.
;;; This causes the scroll bar to update its display.
(DEFFLAVOR BASIC-SCROLL-BAR
	   ((SCROLL-BAR T) (SCROLL-BAR-ALWAYS-DISPLAYED NIL) (SCROLL-BAR-IN NIL))
           ()
  (:INCLUDED-FLAVORS ESSENTIAL-WINDOW)
  (:REQUIRED-METHODS :SCROLL-TO)
  (:GETTABLE-INSTANCE-VARIABLES SCROLL-BAR SCROLL-BAR-ALWAYS-DISPLAYED)
  (:INITABLE-INSTANCE-VARIABLES SCROLL-BAR SCROLL-BAR-ALWAYS-DISPLAYED))

(DEFMETHOD (BASIC-SCROLL-BAR :BEFORE :INIT) (INIT-PLIST)
  (ADJUST-MARGINS 'SCROLL-BAR ':PARSE-SCROLL-BAR-SPEC INIT-PLIST NIL))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR) (NEW-SCROLL-BAR
					       &AUX (PLIST (LIST ':SCROLL-BAR
								 NEW-SCROLL-BAR)))
  (FUNCALL-SELF ':REDEFINE-MARGINS (LOCF PLIST)))

(DEFMETHOD (BASIC-SCROLL-BAR :AFTER :REFRESH-MARGINS) ()
  (AND (OR (EQ SCROLL-BAR-IN T) SCROLL-BAR-ALWAYS-DISPLAYED)
       (SCROLL-BAR-DRAW)))

(DEFMETHOD (BASIC-SCROLL-BAR :BEFORE :REDEFINE-MARGINS) (PLIST)
  (ADJUST-MARGINS 'SCROLL-BAR ':PARSE-SCROLL-BAR-SPEC PLIST ':SCROLL-BAR))

(DEFMETHOD (BASIC-SCROLL-BAR :PARSE-SCROLL-BAR-SPEC) (SPEC LM TM RM BM &AUX BAR-WIDTH)
  (COND (SPEC
	 (AND (EQ SPEC T) (SETQ SPEC 1))
	 (IF (NUMBERP SPEC)
	     (SETQ BAR-WIDTH SPEC SPEC (MAKE-LIST NIL 4))
	     (SETQ BAR-WIDTH (- (THIRD SPEC) (FIRST SPEC))))
	 (SETF (FIRST SPEC) LM)
	 (SETF (SECOND SPEC) TM)
	 (SETF (THIRD SPEC) (SETQ LM (+ LM BAR-WIDTH)))
	 (SETF (FOURTH SPEC) (- BM))))
  (PROG () (RETURN SPEC LM TM RM BM)))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-BAR-P) () (NOT (NULL SCROLL-BAR)))

;;; Next two methods are defaults, some flavors do these more efficiently
(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-MORE-ABOVE) ()
  (PLUSP (FUNCALL-SELF ':SCROLL-POSITION)))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-MORE-BELOW) ()
  (MULTIPLE-VALUE-BIND (TOP-LINE N-LINES LINE-HEIGHT N-SCREEN-LINES)
      (FUNCALL-SELF ':SCROLL-POSITION)
    ;; Some bag-chompers forget to return this value
    (OR N-SCREEN-LINES
	(SETQ N-SCREEN-LINES (// (SHEET-INSIDE-HEIGHT) LINE-HEIGHT)))
    (< (+ TOP-LINE N-SCREEN-LINES) N-LINES)))

(DEFMETHOD (BASIC-SCROLL-BAR :SET-SCROLL-BAR-ALWAYS-DISPLAYED) (NEW)
  (SETQ SCROLL-BAR-ALWAYS-DISPLAYED NEW)
  (COND ((EQ SCROLL-BAR-IN T))
	(SCROLL-BAR-ALWAYS-DISPLAYED (SCROLL-BAR-DRAW))
	(T (SCROLL-BAR-ERASE))))

(DEFMETHOD (BASIC-SCROLL-BAR :HANDLE-MOUSE-SCROLL) (&AUX Y-OFF BOTTOM)
  "Called when the mouse enters the scroll bar"
  (SCROLL-BAR-DRAW)
  (SETQ SCROLL-BAR-IN T)
  (FUNCALL-SELF ':SET-MOUSE-POSITION (// SCROLL-BAR-WIDTH 2) NIL)
  (MOUSE-SET-BLINKER-DEFINITION ':CHARACTER 0 7 ':ON
				':SET-CHARACTER 14)
  (DO () (())
    (MOUSE-DEFAULT-HANDLER SELF ':IN)
    (MULTIPLE-VALUE (NIL Y-OFF)
      (SHEET-CALCULATE-OFFSETS SELF MOUSE-SHEET))
    (COND ((< MOUSE-Y Y-OFF)
	   (MOUSE-WARP MOUSE-X Y-OFF))
	  (( MOUSE-Y (SETQ BOTTOM (+ Y-OFF HEIGHT)))
	   (MOUSE-WARP MOUSE-X (1- BOTTOM)))
	  (T (RETURN T))))
  (WITHOUT-INTERRUPTS
    (OR SCROLL-BAR-ALWAYS-DISPLAYED
	;;There is this funny case where the sheet could be locked by the person waiting
	;; for us to back out.  For us to block here would be a disaster, so undraw the
	;; scroll bar in another process
	(COND ((SHEET-CAN-GET-LOCK SELF)
	       (SHEET-FORCE-ACCESS (SELF) (SCROLL-BAR-ERASE))
	       (SETQ SCROLL-BAR-IN NIL))
	      (T (SETQ SCROLL-BAR-IN ':CLEAR)
		 (PROCESS-RUN-FUNCTION "Undraw Scroll Bar"
				       #'(LAMBDA (W)
					   (FUNCALL W ':FUNCALL-INSIDE-YOURSELF
						      #'(LAMBDA ()
							  (SHEET-FORCE-ACCESS (SELF)
							    (SCROLL-BAR-ERASE))
							  (SETQ SCROLL-BAR-IN NIL))))
				       SELF))))))

(DEFMETHOD (BASIC-SCROLL-BAR :MOUSE-BUTTONS-SCROLL) (BD IGNORE Y &AUX CHAR TOP BOTTOM SHEIGHT)
  (SETQ CHAR (MOUSE-BUTTON-ENCODE BD))
  (MULTIPLE-VALUE (NIL TOP NIL BOTTOM)
    (DECODE-SCROLL-BAR SCROLL-BAR))
  (SETQ SHEIGHT (- BOTTOM TOP)
	Y (- Y TOP))
  (SELECTQ CHAR
    (#\MOUSE-1-1
      ;; Left: Here to top
      (FUNCALL-SELF ':SCROLL-RELATIVE Y ':TOP))
    (#\MOUSE-3-1
      ;; Right: Top to here
      (FUNCALL-SELF ':SCROLL-RELATIVE ':TOP Y))
    (#\MOUSE-1-2
      ;; Double Left: Here to bottom
      (FUNCALL-SELF ':SCROLL-RELATIVE Y ':BOTTOM))
    (#\MOUSE-3-2
      ;; Double right: Bottom to here
      (FUNCALL-SELF ':SCROLL-RELATIVE ':BOTTOM Y))
    (#\MOUSE-2-1
      ;; Middle: Jump to a proportional place in the "buffer"
      ;; If we are n% of the window down, we want the point
      ;; n% through the buffer to appear at the top of the window.
      (MULTIPLE-VALUE-BIND (IGNORE TOTAL-ITEMS)
	  (FUNCALL-SELF ':SCROLL-POSITION)
	(FUNCALL-SELF ':SCROLL-ABSOLUTE
		 (FIX (+ .5s0 (// (* TOTAL-ITEMS Y) (SMALL-FLOAT SHEIGHT)))))))
    (OTHERWISE (BEEP))))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-RELATIVE) (FROM TO)
  "Put the FROM Y-position on the TO Y-position.  This assumes that each item is LINE-HEIGHT
high, and that there is a :SCROLL-TO message which accepts a line number to scroll to,
or a relative number of lines to scroll by."
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE ITEM-HEIGHT)
      (FUNCALL-SELF ':SCROLL-POSITION)
    (SETQ FROM (COND ((EQ FROM ':TOP) 0)
		     ((EQ FROM ':BOTTOM) (// (- (SHEET-INSIDE-HEIGHT) (// ITEM-HEIGHT 2))
					     ITEM-HEIGHT))
		     ((NUMBERP FROM) (// (- FROM TOP-MARGIN-SIZE) ITEM-HEIGHT))
		     (T (FERROR NIL "~A illegal arg to :SCROLL-RELATIVE" FROM)))
	  TO  (COND ((EQ TO ':TOP) 0)
		    ((EQ TO ':BOTTOM) (// (- (SHEET-INSIDE-HEIGHT) (// ITEM-HEIGHT 2))
					  ITEM-HEIGHT))
		    ((NUMBERP TO) (// (- TO TOP-MARGIN-SIZE) ITEM-HEIGHT))
		    (T (FERROR NIL "~A illegal arg to :SCROLL-RELATIVE" TO))))
    ;; We now know what item we are scrolling from, and what item we are scrolling to.
    ;; Scroll that relative amount.
    (FUNCALL-SELF ':SCROLL-TO (- FROM TO) ':RELATIVE)))

(DEFMETHOD (BASIC-SCROLL-BAR :SCROLL-ABSOLUTE) (TO)
  "Scroll to the specified item"
  (FUNCALL-SELF ':SCROLL-TO TO ':ABSOLUTE))

(DEFMETHOD (BASIC-SCROLL-BAR :MOUSE-MOVES-SCROLL) (&REST IGNORE)
  (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
      (FUNCALL MOUSE-BLINKER ':OFFSETS)
    (BLINKER-SET-CURSORPOS MOUSE-BLINKER
			   (MAX 0 (- MOUSE-X X-OFF))
			   (- MOUSE-Y Y-OFF))))

(DEFMETHOD (BASIC-SCROLL-BAR :AFTER :NEW-SCROLL-POSITION) (&OPTIONAL IGNORE)
  (AND (OR SCROLL-BAR-IN SCROLL-BAR-ALWAYS-DISPLAYED)
       (SCROLL-BAR-DRAW)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-SCROLL-BAR)
(DEFUN DECODE-SCROLL-BAR (SPEC)
  (PROG () (RETURN (FIRST SPEC) (SECOND SPEC) (THIRD SPEC) (+ (FOURTH SPEC) HEIGHT)))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-SCROLL-BAR)
(DEFUN SCROLL-BAR-DRAW (&AUX ITEM-HEIGHT TOP-ITEM-NUM TOTAL-ITEMS LEFT TOP RIGHT BOTTOM
			     BAR-HEIGHT N-ITEMS BAR-TOP BAR-BOTTOM BAR-WIDTH)
  (AND (EQ SCROLL-BAR-IN ':CLEAR)
       (PROCESS-WAIT "Scroll Bar Cleared"
		     #'(LAMBDA (SBI) (NEQ (CAR SBI) ':CLEAR))
		     (LOCATE-IN-INSTANCE SELF 'SCROLL-BAR-IN)))
  (MULTIPLE-VALUE (TOP-ITEM-NUM TOTAL-ITEMS ITEM-HEIGHT N-ITEMS)
    (FUNCALL-SELF ':SCROLL-POSITION))
  (SETQ TOTAL-ITEMS (MAX TOP-ITEM-NUM TOTAL-ITEMS))	;In case we get a bad number, don't
							; blow the whole mouse process away
  (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
    (DECODE-SCROLL-BAR SCROLL-BAR))
  (SETQ BAR-HEIGHT (- BOTTOM TOP))
  (OR N-ITEMS (SETQ N-ITEMS (// (SHEET-INSIDE-HEIGHT) ITEM-HEIGHT)))
  (SETQ BAR-TOP (FIX (+ TOP .5S0
			(* BAR-HEIGHT (IF (ZEROP TOTAL-ITEMS)
					  0
					  (// TOP-ITEM-NUM (SMALL-FLOAT TOTAL-ITEMS)))))))
  (SETQ BAR-BOTTOM (FIX (+ TOP .5S0
			   (* BAR-HEIGHT (IF (ZEROP TOTAL-ITEMS)
					     1
					     (// (+ TOP-ITEM-NUM N-ITEMS)
					     (SMALL-FLOAT TOTAL-ITEMS)))))))
  (SETQ BAR-WIDTH (- RIGHT LEFT))
  (PREPARE-SHEET (SELF)
    (%DRAW-RECTANGLE BAR-WIDTH (- BOTTOM TOP) LEFT TOP ALU-ANDCA SELF)
    (%DRAW-RECTANGLE BAR-WIDTH (- (MIN BOTTOM BAR-BOTTOM) BAR-TOP)
		     LEFT BAR-TOP ALU-IOR SELF)))
)

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-SCROLL-BAR)
(DEFUN SCROLL-BAR-ERASE (&AUX LEFT TOP RIGHT BOTTOM)
  (MULTIPLE-VALUE (LEFT TOP RIGHT BOTTOM)
    (DECODE-SCROLL-BAR SCROLL-BAR))
  (PREPARE-SHEET (SELF)
    (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF)))
)

;Flashy Scrolling

;If you move the mouse slowly out the top or bottom of a window that
;has this flavor, it gets scrolled up or down by a line, and the mouse
;jumps back in so that if you keep moving it, it keeps getting scrolled.
(DEFFLAVOR FLASHY-SCROLLING-MIXIN
	((FLASHY-SCROLLING-REGION '((40 0.80 :RIGHT) (40 0.80 :RIGHT)))
	 ;*** I'm not sure there's any point to making this an instance variable --Moon ***
	 (FLASHY-SCROLLING-MAX-SPEED 6)	;Default to 6 inches per second
	 (FLASHY-SCROLLING-BLINKER NIL))
	()
  (:INITABLE-INSTANCE-VARIABLES FLASHY-SCROLLING-REGION FLASHY-SCROLLING-MAX-SPEED)
  (:INCLUDED-FLAVORS WINDOW)
  (:REQUIRED-METHODS :SCROLL-TO)
  (:DOCUMENTATION :MIXIN "Automatic scrolling when moving over the margins
Moving slowly out of the top or bottom of a window that includes this and keep moving,
and it will scroll up or down by a single line and the mouse will be moved back."))

(MOUSE-DEFINE-BLINKER-TYPE 'FLASHY-CHARACTER
			   #'(LAMBDA (SCREEN)
			       (DEFINE-BLINKER SCREEN 'MOUSE-CHARACTER-BLINKER
				 ':VISIBILITY NIL
				 ':FONT (FUNCALL SCREEN ':MOUSE-FONT)
				 ':CHAR 10)))

;Arguments are window-relative position of the mouse
;This only does the rest of the processing if the flashy scrolling didn't happen
(DEFWRAPPER (FLASHY-SCROLLING-MIXIN :MOUSE-MOVES) ((X Y) . BODY)
  `(COND ((NOT (FLASHY-SCROLLING-MOUSE-MOVES SELF X Y))
	  . ,BODY)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (FLASHY-SCROLLING-MIXIN)
(DEFUN FLASHY-SCROLLING-MOUSE-MOVES (W X Y &AUX REGION TOP-P)
  (COND ((AND ( X 0) (< X WIDTH))
	 (SETQ REGION (IF (SETQ TOP-P (< Y (// HEIGHT 2)))
			  (FIRST FLASHY-SCROLLING-REGION)
			  (SECOND FLASHY-SCROLLING-REGION)))
	 ;; Make sure is within the appropriate region
	 (COND ((AND (FUNCALL-SELF ':SCROLL-BAR-P)
		     (IF TOP-P
			 (< Y (FIRST REGION))
			 (> Y (- HEIGHT (FIRST REGION))))
		     (> X (FLASHY-SCROLLING-PARSE-X-SPEC (SECOND REGION)))
		     ( X (FLASHY-SCROLLING-PARSE-X-SPEC (THIRD REGION)))
		     (FUNCALL-SELF (IF TOP-P ':SCROLL-MORE-ABOVE ':SCROLL-MORE-BELOW)))
		(OR FLASHY-SCROLLING-BLINKER
		    (SETQ FLASHY-SCROLLING-BLINKER MOUSE-BLINKER-NAME))
		(COND (TOP-P
		       (MOUSE-SET-BLINKER-DEFINITION 'FLASHY-CHARACTER 6 0 ':ON
						     ':SET-CHARACTER 10))
		      (T
		       (MOUSE-SET-BLINKER-DEFINITION 'FLASHY-CHARACTER 6 15 ':ON
						     ':SET-CHARACTER 12)))
		(AND ;; If mouse is moving slowly enough
		  (OR (NULL FLASHY-SCROLLING-MAX-SPEED)
		      (< MOUSE-SPEED FLASHY-SCROLLING-MAX-SPEED))
		  ;; and out the top or bottom
		  (OR (SETQ TOP-P ( Y 0)) ( Y (1- HEIGHT)))
		  ;; then warp the mouse and send the appropriate message and return T
		  (MULTIPLE-VALUE-BIND (IGNORE WINDOW-Y-OFFSET)
		      (SHEET-CALCULATE-OFFSETS W MOUSE-SHEET)
		    (MOUSE-WARP MOUSE-X (+ (IF TOP-P 10. (- HEIGHT 10.)) WINDOW-Y-OFFSET))
		    ;; Express scrolling 1 line up or down by relations of lines 0 and 1
		    (FUNCALL-SELF ':SCROLL-TO (IF TOP-P -1 +1) ':RELATIVE))
		  T))
	       (FLASHY-SCROLLING-BLINKER
		(MOUSE-SET-BLINKER FLASHY-SCROLLING-BLINKER)
		(SETQ FLASHY-SCROLLING-BLINKER NIL)
		NIL))))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (FLASHY-SCROLLING-MIXIN)
(DEFUN FLASHY-SCROLLING-PARSE-X-SPEC (SPEC)
  (COND ((FLOATP SPEC) (FIX (* SPEC WIDTH)))
	((FIXP SPEC) SPEC)
	((EQ SPEC ':RIGHT) WIDTH)
	((EQ SPEC ':LEFT) 0)
	(T (FERROR NIL "~A is illegal X position specification for flashy scrolling" SPEC)))))

(COMPILE-FLAVOR-METHODS MOUSE-CHARACTER-BLINKER MOUSE-RECTANGULAR-BLINKER
			MOUSE-HOLLOW-RECTANGULAR-BLINKER)

;;; Get everything going.  This has to be at the end of the file.
(ADD-INITIALIZATION "MOUSE" '(MOUSE-INITIALIZE) '(:WARM :FIRST))

