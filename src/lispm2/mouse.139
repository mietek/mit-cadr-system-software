;;; -*-Mode:LISP; Package:SYSTEM-INTERNALS-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; Lisp Machine Mouse Handler
;;; NOTES:
;;;  For simplicity, tablet stuff is currently deleted, add back later, perhaps
;;;   as an overlay.
;;;  This doesn't use the hairier features of the CADR mouse interface.

;;; Low-level routines

(DECLARE (SPECIAL MOUSE-H1		;First hardware word (to detect status change)
		  MOUSE-H2		;Second hardware word (..)
		  MOUSE-X-SCALE		;Numerator . Denominator
		  MOUSE-Y-SCALE
		  MOUSE-LAST-X		;To compute deltas without truncation error
		  MOUSE-LAST-Y
		  MOUSE-LAST-BUTTONS	;To compute change in buttons
		  MOUSE-BOUNCE-TIME	;Delay for bounce elimination
		  MOUSE-DOUBLE-CLICK-TIME ;Delay for user to push button again
		  MOUSE-RECONSIDER	;T => mouse process should return to overseer
					;and decide anew which window should get the mouse.
					;For use by :MOUSE-MOVES methods, etc.
		  MOUSE-X-OFFSET	;Offset in dots for displaying the mouse cursor.
		  MOUSE-Y-OFFSET	;Used so that the center of the X, rather than the
					;corner, appears where the mouse really is.
					;The optimum value depends on the character.

		  MOUSE-REG1 MOUSE-REG2 ;Unibus addresses
		  FONTS:ARROW		;Special character fonts
                  SCROLL-BAR		;The scroll bar window.
))

(SETQ MOUSE-H1 -1 MOUSE-H2 -1 MOUSE-LAST-X 0 MOUSE-LAST-Y 0 MOUSE-LAST-BUTTONS 0
      MOUSE-X-SCALE '(2 . 3) MOUSE-Y-SCALE '(3 . 5)
      MOUSE-BOUNCE-TIME 200 MOUSE-DOUBLE-CLICK-TIME 6000)

(SETQ MOUSE-REG1 764104 MOUSE-REG2 764106)

(ENDF HEAD)

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
				   (OR ( MOUSE-LAST-BUTTONS
					  (LDB 1403 NH1))
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
  (LDB 1403 (%UNIBUS-READ MOUSE-REG1)))

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
	       CH (+ 2000 BUTTON))
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

(DECLARE (SPECIAL MOUSE-PROCESS		;This global-process is in charge of the mouse
		  MOUSE-BLINKER		;This blinker shows where the mouse "is"
		  MOUSE-RECTANGLE-BLINKER	;In MOUSE-SPECIFY-RECTANGLE this shows
					;the upper left corner while the lower right is
					;being asked for.
		  MOUSE-WINDOW		;window controlling the mouse, NIL if none
		  WINDOW-OWNING-MOUSE	;NIL, or window which has seized the mouse, or
					;T if someone has seized the mouse and can't identify
					; himself as any particular window,
					;or STOP to make the mouse process do nothing.
		  MOUSE-X		;X coordinate of MOUSE-BLINKER
		  MOUSE-Y		;Y coordinate of MOUSE-BLINKER
		  MOUSE-SCREEN		;Which screen MOUSE-BLINKER is on 
		  MOUSE-DEFAULT-BUTTON-TABLE  ;Command table for buttons pushed in random windows
					      ;  First subscript is button, second is clicks-1
))

(DEFUN MOUSE-SET-X-BLINKER-CURSORPOS (&REST IGNORE)
    (TV-SET-BLINKER-CURSORPOS MOUSE-BLINKER
                              (- MOUSE-X MOUSE-X-OFFSET)
                              (- MOUSE-Y MOUSE-Y-OFFSET)))

;;; Double-click-right when not on any window - offer to create a window.
(DECLARE (SPECIAL WINDOW-CREATION-MENU))

(DEFUN MOUSE-CALL-SYSTEM-MENU ()
       (AND (BOUNDP 'WINDOW-CREATION-MENU)
	    (<- WINDOW-CREATION-MENU ':CHOOSE)))

;;; This function is called during loading and from LISP-REINITIALIZE
;;; to initialize the mouse process and associated variable.
(DEFUN MOUSE-INITIALIZE (&OPTIONAL (SCREEN TV-DEFAULT-SCREEN))

  (OR (BOUNDP 'MOUSE-PROCESS)	;If first time loaded, initialize everything
      (SETQ MOUSE-PROCESS (PROCESS-CREATE "Mouse" NIL ':SPECIAL-PDL-SIZE 1400)))
  (OR (AND (BOUNDP 'MOUSE-SCREEN)
	   (EQ MOUSE-SCREEN SCREEN))
      (SETQ MOUSE-BLINKER (TV-DEFINE-BLINKER NIL ':ROVING-P T ':VISIBILITY T ;No blink
			     ':FUNCTION 'TV-CHARACTER-BLINKER
			     ':ARG1 FONTS:ARROW
			     ':ARG2 #/X)))
  (OR (BOUNDP 'MOUSE-RECTANGLE-BLINKER)
      (SETQ MOUSE-RECTANGLE-BLINKER
	    (TV-DEFINE-BLINKER NIL ':ROVING-P T ':VISIBILITY T ;No blink
			       ':FUNCTION 'TV-CHARACTER-BLINKER
			       ':ARG1 FONTS:ARROW
			       ':ARG2 3)))

  ;; Here are the commands you can give when the mouse is not on a window.
  (COND ((NOT (BOUNDP 'MOUSE-DEFAULT-BUTTON-TABLE))
	 (SETQ MOUSE-DEFAULT-BUTTON-TABLE (MAKE-ARRAY NIL 'ART-Q '(3 2))) ;3 buttons, 2 clicks
	 ;; Presently the only command is double-click-right gets you the system menu
	 (AS-2 'MOUSE-CALL-SYSTEM-MENU MOUSE-DEFAULT-BUTTON-TABLE 2 1)
	 ))
  (SETQ MOUSE-WINDOW NIL
	WINDOW-OWNING-MOUSE NIL
	MOUSE-X 0
	MOUSE-Y 0
	MOUSE-SCREEN SCREEN)
  ;; Make sure the mouse blinker is winning, and put it in the lower-right-hand corner
  (TV-SET-BLINKER-VISIBILITY MOUSE-BLINKER T)
  (TV-SET-BLINKER-FUNCTION MOUSE-BLINKER 'TV-CHARACTER-BLINKER
			   FONTS:ARROW #/X)
  (SETQ MOUSE-X-OFFSET 3 MOUSE-Y-OFFSET 3)
  (MOUSE-WARP (- (SCREEN-X2 MOUSE-SCREEN) 5) (- (SCREEN-Y2 MOUSE-SCREEN) 15.))
  ;; Make sure the other blinker used by MOUSE-SPECIFY-RECTANGLE is off.
  (TV-SET-BLINKER-VISIBILITY MOUSE-RECTANGLE-BLINKER NIL)
  (TV-SET-BLINKER-FUNCTION MOUSE-RECTANGLE-BLINKER 'TV-CHARACTER-BLINKER
			   FONTS:ARROW 3)
  ;; Call MOUSE-INPUT once to flush any pending motion and update variables, but don't wait.
  (MOUSE-INPUT NIL)
  ;; Start up the mouse process
  (<- MOUSE-PROCESS ':PRESET 'MOUSE-OVERSEER)
  (<- MOUSE-PROCESS ':RUN-REASON))

;;; This function "warps" the mouse to a specified place
(DEFUN MOUSE-WARP (X Y)
       (SETQ MOUSE-X (MAX (SCREEN-X1 MOUSE-SCREEN)
			  (MIN (1- (SCREEN-X2 MOUSE-SCREEN)) X)))
       (SETQ MOUSE-Y (MAX (SCREEN-Y1 MOUSE-SCREEN)
			  (MIN (1- (SCREEN-Y2 MOUSE-SCREEN)) Y)))
       (MOUSE-SET-X-BLINKER-CURSORPOS)
       (MOUSE-WAKEUP))  ;Make sure the mouse tracker process notices

;; Call this and get back a list of four co-ordinates of a rectangle which
;; the user has specified by moving the mouse.
;; Specifying a rectangle of zero or negative size instead gives the full screen.
;; Our arguments are where to start the corners out:
;; The upper left corner goes at LEFT and TOP, or where the mouse is if they are NIL;
;; the lower right corner goes near the other one by default, unless all
;; four args are present, in which case it starts off so as to make a rectangle
;; congruent to the one specified by the arguments.
(DEFUN MOUSE-SPECIFY-RECTANGLE (&OPTIONAL LEFT TOP RIGHT BOTTOM &AUX LEFT1 TOP1)
    (AND (EQ CURRENT-PROCESS MOUSE-PROCESS)
	 (FERROR NIL "MOUSE-SPECIFY-RECTANGLE cannot be called in the mouse process"))
    (LET-GLOBALLY ((WINDOW-OWNING-MOUSE T))
      (MOUSE-WAKEUP)
      ;; Make the mouse process notice that the mouse has been nonspecifically seized,
      ;; and get inside an appropriate call to MOUSE-DEFAULT-HANDLER on T
      ;; so that we can change the blinker without timing error.
      (PROCESS-ALLOW-SCHEDULE)
      (TV-SET-BLINKER-FUNCTION MOUSE-BLINKER 'TV-CHARACTER-BLINKER
			       FONTS:ARROW 3)
      (SETQ MOUSE-X-OFFSET 0 MOUSE-Y-OFFSET 0)
      (MOUSE-WARP (OR LEFT MOUSE-X) (OR TOP MOUSE-Y))
      ;; In case this was called in response to a mouse click, wait for
      ;; the buttons to be released.
      (PROCESS-WAIT "Release Button" #'(LAMBDA () (ZEROP MOUSE-LAST-BUTTONS)))
      (PROCESS-WAIT "Button" #'(LAMBDA () (NOT (ZEROP MOUSE-LAST-BUTTONS))))
      (PROCESS-WAIT "Release Button" #'(LAMBDA () (ZEROP MOUSE-LAST-BUTTONS)))
      ;; The first click determines the upper left corner.
      (SETQ LEFT1 MOUSE-X TOP1 MOUSE-Y)
      ;; Set up the mouse for finding the lower right corner.
      (TV-SET-BLINKER-FUNCTION MOUSE-BLINKER 'TV-CHARACTER-BLINKER
			       FONTS:ARROW 6)
      (SETQ MOUSE-X-OFFSET 12. MOUSE-Y-OFFSET 12.)
      (COND ((AND LEFT TOP RIGHT BOTTOM)
	     (MOUSE-WARP (+ LEFT1 (- RIGHT LEFT)) (+ TOP1 (- BOTTOM TOP))))
	    (T (MOUSE-WARP (+ MOUSE-X 20.) (+ MOUSE-Y 20.))))
      ;; Leave the auxiliary blinker behind to continue to show the first corner.
      (TV-SET-BLINKER-CURSORPOS MOUSE-RECTANGLE-BLINKER LEFT1 TOP1)
      (TV-SET-BLINKER-VISIBILITY MOUSE-RECTANGLE-BLINKER T)
      ;; The next click fixes the lower right corner.
      (PROCESS-WAIT "Button" #'(LAMBDA () (NOT (ZEROP MOUSE-LAST-BUTTONS))))
      (TV-SET-BLINKER-VISIBILITY MOUSE-RECTANGLE-BLINKER NIL)
      (COND ((AND (< LEFT1 MOUSE-X) (< TOP1 MOUSE-Y))
	     (LIST LEFT1 TOP1 (1+ MOUSE-X) (1+ MOUSE-Y)))
	    ((LIST (SCREEN-X1 MOUSE-SCREEN) (SCREEN-Y1 MOUSE-SCREEN)
		   (SCREEN-X2 MOUSE-SCREEN) (SCREEN-Y2 MOUSE-SCREEN))))))

;;; This function returns the active window which the mouse is over, or NIL.
;;; It is not used to decide which window should handle mouse tracking,
;;; but it is used to decide which window should receive a mouse click
;;; when the mouse was not owned by any window.
(DEFUN WINDOW-UNDER-MOUSE ()
    (WINDOW-UNDER-POINT MOUSE-X MOUSE-Y MOUSE-SCREEN))

;;; This function is used to decide which window should own the mouse.
;;; If this is different from MOUSE-WINDOW, it would be good human-engineering to switch windows.
;;; A window can set WINDOW-OWNING-MOUSE to itself when it is selected and wants
;;; you to use the mouse to specify another window to it.
(DEFUN WINDOW-OWNING-MOUSE ()
  (OR WINDOW-OWNING-MOUSE
    (DO ((L EXPOSED-WINDOWS (CDR L))) ((NULL L) NIL)
       (AND (<- (CAR L) ':CONTAINS-POINT-P MOUSE-SCREEN MOUSE-X MOUSE-Y)
	    (RETURN (CAR L))))))

;;; Use this to tell whether a certain window owns the mouse.
;;; It is not precisely equivalent to comparing (WINDOW-OWNING-MOUSE) with
;;; that window.  If the mouse is seized by a pane in a frame,
;;; then (WINDOW-OWNING-MOUSE) will be the pane, but this function will
;;; return T for both the pane and the frame.
;;; If the mouse is not seized, and is inside the pane, this function will again
;;; return T for both of them.
;;; In addition, this function is much much faster.
(DEFUN WINDOW-OWNS-MOUSE-P (W)
    (COND ((EQ W T) (EQ WINDOW-OWNING-MOUSE T))
          (WINDOW-OWNING-MOUSE
	    (DO ((W1 W (<- W1 ':FRAME))) ((NULL W1) NIL)
	      (AND (EQ WINDOW-OWNING-MOUSE W1) (RETURN T))))
	  (T (AND (<- W ':STATUS)
		  (<- W ':CONTAINS-POINT-P MOUSE-SCREEN MOUSE-X MOUSE-Y)))))

;;; A window's process should call this function when it wishes to seize
;;; the mouse to use it to get input that can range over the whole screen.
;;; To free the mouse again, setq WINDOW-OWNING-MOUSE to NIL again.
(DEFUN MOUSE-SEIZE ()
    (PROCESS-WAIT "MOUSE" (FUNCTION (LAMBDA () (EQ CURRENT-PROCESS SELECTED-PROCESS))))
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

(DEFUN MOUSE-OVERSEER ()
    (DO () (())
	(SETQ MOUSE-RECONSIDER NIL)
	(TV-SET-BLINKER-FUNCTION MOUSE-BLINKER 'TV-CHARACTER-BLINKER
              FONTS:ARROW #/X)
	(SETQ MOUSE-X-OFFSET 3 MOUSE-Y-OFFSET 3)
	(MOUSE-SET-X-BLINKER-CURSORPOS)	      
	(COND ((EQ (SETQ MOUSE-WINDOW (WINDOW-OWNING-MOUSE)) 'STOP)
	       (PROCESS-WAIT "Usurped"
			(FUNCTION (LAMBDA () (NEQ WINDOW-OWNING-MOUSE 'STOP)))))
	      ((NOT (MEMQ MOUSE-WINDOW '(() T)))
	       (<- MOUSE-WINDOW ':HANDLE-MOUSE))
	      (T
	       (MOUSE-DEFAULT-HANDLER MOUSE-WINDOW)))))

;; Magic adjustments

;; Distance mouse must try to move before it penetrates into the scroll bar.
;; This parameter prevents you from getting into the scroll bar too easily.
(DEFVAR MOUSE-DISTANCE-TO-SCROLL-BAR 12.)	;1/8 inch barrier between window and scrollbar
;; This parameter prevents the scroll bar from popping up if you move the mouse
;; fast through the left edge of a window.
(DEFVAR MOUSE-SCROLL-BAR-MAX-SPEED 10.)		;10 inches per second

(DEFVAR SCROLL-BAR-LEFT-HYSTERESIS 33.)		;1/3 inch
(DEFVAR SCROLL-BAR-RIGHT-HYSTERESIS 12.)	;1/8 inch
(DEFVAR MOUSE-FAST-MOTION-SPEED 20.)		;Moving faster than 20. inches per second
(DEFVAR MOUSE-FAST-MOTION-CROSS-SIZE 40.)	; triggers a cross 1 cm in diameter
(DEFVAR MOUSE-FAST-MOTION-CROSS-TIME 2000.)	; which lasts this long (DO-loop units)
(DEFVAR MOUSE-SPEED)				;Make this special so MOUSE-MOVES can see it

;; This mouse handler serves for windows which want to do things the simple way.
;; A second argument of T says that the window should have a scroll bar.
;; This function is also used to track the mouse when it isn't inside any window,
;; by calling it with an argument of NIL.
;; An arg of T is used when the mouse has been seized by a process not
;; for any specific window.
(DEFUN MOUSE-DEFAULT-HANDLER (WINDOW &OPTIONAL SCROLL-BAR-FLAG
				     &AUX MOVE-HANDLER PANES-HANDLER SCROLLED-WINDOW
				     CONTAINS-POINT-P-HANDLER min-x max-x min-y max-y)
    (LOCAL-DECLARE ((SPECIAL LEFT STATUS))  ;COMES FROM THE WINDOW
      (SETQ MOVE-HANDLER
	    (OR (AND WINDOW (NEQ WINDOW T) (<- WINDOW ':HANDLER-FOR ':MOUSE-MOVES))
		(FUNCTION MOUSE-SET-X-BLINKER-CURSORPOS)))
      (COND ((NOT (SYMBOLP WINDOW))  ;Not T or NIL.
	     (SETQ PANES-HANDLER (<- WINDOW ':HANDLER-FOR ':PANES))
	     (SETQ CONTAINS-POINT-P-HANDLER
		   (<- WINDOW ':HANDLER-FOR ':CONTAINS-POINT-P))))
      ;; If the mouse is in the scroll bar, keep checking the scrolled window
      ;; so we can get rid of the scroll bar if that window goes away.
      (AND (EQ WINDOW SCROLL-BAR)
	   (SETQ SCROLLED-WINDOW (<- WINDOW ':SCROLLED-WINDOW))
	   ;; But not if the scroll bar hides that window!
	   ;; In that case, we would go away instantly!
	   (LEXPR-FUNCALL WINDOW ':OVERLAPS-P MOUSE-SCREEN (<- SCROLLED-WINDOW ':EDGES))
	   (SETQ SCROLLED-WINDOW NIL))
      (FUNCALL MOVE-HANDLER ':MOUSE-MOVES MOUSE-X MOUSE-Y)
      (DO ((DX) (DY) (BU) (BD) (HAND)
	   (OLD-OWNER WINDOW-OWNING-MOUSE WINDOW-OWNING-MOUSE)
	   (X-OFFSET-TOWARD-SCROLL-BAR 0)
	   (LAST-TIME NIL THIS-TIME) (THIS-TIME))
          (MOUSE-RECONSIDER)
	(MULTIPLE-VALUE (DX DY BD BU) (MOUSE-INPUT))
	;; Approximate speed of the mouse in inches per second
	(setq min-x (screen-x1 mouse-screen) max-x (1- (screen-x2 mouse-screen))
	      min-y (screen-y1 mouse-screen) max-y (1- (screen-y2 mouse-screen)))
	(SETQ THIS-TIME (FIXNUM-MICROSECOND-TIME)
	      MOUSE-SPEED (IF LAST-TIME (// (* (+ (ABS DX) (ABS DY)) 1.0s4)
					    (MAX (TIME-DIFFERENCE THIS-TIME LAST-TIME) 1))
			      0))
	(COND (SCROLLED-WINDOW
	       ;; If we are in the scroll bar and the scrolled window is deexposed,
	       ;; get rid of the scroll bar.
	       (OR (<- SCROLLED-WINDOW ':STATUS) (RETURN T))))
	(COND ((OR (AND OLD-OWNER
			(NOT (EQ OLD-OWNER WINDOW-OWNING-MOUSE)))
		   (ZEROP BD))
	       ;; If no buttons pushed, or mouse is seized, track the motion of the mouse
               ;; Update mouse's new location.
               (SETQ MOUSE-X (MIN MAX-X (+ MOUSE-X DX))
                     MOUSE-Y (MAX MIN-Y (MIN MAX-Y (+ MOUSE-Y DY))))
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
			    (TV-OPEN-SCREEN)
			    (TV-SELECT-SCREEN MOUSE-SCREEN)
			    (TV-ERASE 20 (- XBOTTOM XTOP) XX XTOP TV-ALU-XOR)
			    (TV-ERASE (- XRIGHT XLEFT) 20 XLEFT YY TV-ALU-XOR)
			    (DOTIMES (I MOUSE-FAST-MOTION-CROSS-TIME) )
			    (TV-ERASE 20 (- XBOTTOM XTOP) XX XTOP TV-ALU-XOR)
			    (TV-ERASE (- XRIGHT XLEFT) 20 XLEFT YY TV-ALU-XOR))))))
	       ;; If there is a scrol bar and we have entered it, do scrolling.
	       ;; However, the mouse must move at least MOUSE-DISTANCE-TO-SCROLL-BAR
	       ;; to the left (while appearing to remain stationary) before we do so.
	       ;; First, call the :MOUSE-MOVES handler so it can notice what is
	       ;; about to happen (from the value of MOUSE-X) and clean up.
	       (COND ((AND SCROLL-BAR-FLAG
                           (NOT OLD-OWNER)
			   (NOT WINDOW-OWNING-MOUSE)
			   (< MOUSE-X LEFT))
		      ;; Don't count accidental horizontal motion during mainly vertical
		      (AND (> (ABS DX) (ABS DY))
			   (SETQ X-OFFSET-TOWARD-SCROLL-BAR
				 (+ X-OFFSET-TOWARD-SCROLL-BAR (- LEFT MOUSE-X))))
		      (FUNCALL MOVE-HANDLER ':MOUSE-MOVES MOUSE-X MOUSE-Y)
		      (COND ((> MOUSE-SPEED MOUSE-SCROLL-BAR-MAX-SPEED)
			     (RETURN NIL)) ;Pass right through scroll bar without popping up
			    ((> X-OFFSET-TOWARD-SCROLL-BAR MOUSE-DISTANCE-TO-SCROLL-BAR)
			     (RETURN (<- SCROLL-BAR ':GO-TO-WORK SELF)))
			    (T (SETQ MOUSE-X LEFT))))
		     (T (SETQ X-OFFSET-TOWARD-SCROLL-BAR 0)))
	       ;; Check for left margin after scroll bar, so windows on the edge work
	       (SETQ MOUSE-X (MAX MIN-X MOUSE-X))
	       ;; If mouse has moved out of this window, return to overseer.
	       (COND ((OR WINDOW-OWNING-MOUSE (EQ WINDOW T))
		       (OR (WINDOW-OWNS-MOUSE-P WINDOW) (RETURN NIL)))
		     (WINDOW
		       (OR (AND STATUS
				(FUNCALL CONTAINS-POINT-P-HANDLER ':CONTAINS-POINT-P
					 MOUSE-SCREEN MOUSE-X MOUSE-Y))
			   (RETURN NIL)))
		     ;; If wasn't in any window, but has moved into one or been seized,
		     ;; return to overseer.
		     ((WINDOW-OWNING-MOUSE) (RETURN NIL)))
	       ;; If this window, is a frame, and the mouse was not in any pane,
	       ;; see if it has moved into one.
	       (COND (PANES-HANDLER
		      (LET ((PANES (FUNCALL PANES-HANDLER ':PANES)))
			(AND (DOLIST (PANE PANES)
			       (AND (<- PANE ':CONTAINS-POINT-P MOUSE-SCREEN MOUSE-X MOUSE-Y)
				    (RETURN T)))
			     (RETURN NIL)))))
               ;; Put the mouse blinker where the mouse is, with offset
               ;; to put the center of the X at the right spot.
	       (FUNCALL MOVE-HANDLER ':MOUSE-MOVES MOUSE-X MOUSE-Y))
	      ;; We do nothing with buttons when seized nonspecifically.
	      ((EQ WINDOW T))
	      (WINDOW (<- WINDOW ':MOUSE-BUTTONS BD MOUSE-X MOUSE-Y))
	      ((SETQ HAND (WINDOW-UNDER-MOUSE))
	       (<- HAND ':MOUSE-BUTTONS BD MOUSE-X MOUSE-Y))
	      (T
	       (SETQ HAND (MOUSE-BUTTON-ENCODE BD))	;Button pushed, execute command
	       (AND (SETQ HAND (AR-2 MOUSE-DEFAULT-BUTTON-TABLE
				     (LDB 0003 HAND)
				     (MIN 1 (LDB 0303 HAND))))
		    (FUNCALL HAND)))))))

;This function is here just because this wanted to call it and it didn't exist
;any place else.
(DEFUN FIXNUM-MICROSECOND-TIME ()
  (LET ((LOW (%UNIBUS-READ 764120))  ;Hardware synchronizes if you read this one first
	(HIGH (%UNIBUS-READ 764122)))
    (DPB HIGH 2007 LOW)))

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

;Wait until a mouse button is pressed and then released,
;in the user's own process, assuming mouse-default-handler or something similar is running.
;Wait for the button to be pressed, and then wait for the mouse-character
;to arrive as keyboard input and return it.
(DEFUN MOUSE-SELECTION-WAIT ()
    (DO () (NIL)
	(MOUSE-WAIT)
	(OR (ZEROP MOUSE-LAST-BUTTONS) (RETURN (KBD-TYI)))))

;;; The Scroll-Bar

;;; Moving out the left edge of a window that has a scroll bar causes the
;;; window's mouse handler to call this routine.  The scroll bar, a window,
;;; pops up and remains as long as the mouse stays inside it.
;;; While the mouse is inside the scroll bar, the mouse buttons are used
;;; to scroll the window for which the scroll bar was brought up.

;;; The commands in the scroll bar are:
;;;   Left: Move the line next to the mouse to the top.
;;;   Left-double: Move the line next to the mouse to the bottom.
;;;   Middle: Move the top line to the place where the mouse is.
;;;   Middle-double: Move the bottom line to the place where the mouse is.
;;;   Right: Jump to a place in the "buffer" as far (proportionally) from
;;;	     the beginning as the mouse is from the top.
;;;   Right-double: calls system menu on the window being scrolled,
;;;          after making scroll bar go away and putting the mouse back in the window.

;;; To turn on the scroll bar for a class of window, do
;;;  (DEFMETHOD (mumble-class :HANDLE-MOUSE) () (MOUSE-DEFAULT-HANDLER SELF condition))
;;;  which will make the scroll bar available if condition (evaluated only once) is non-NIL.
;;; The window on which this function is run should handle the :LINE-HEIGHT message
;;;  by returning the height of the unit of scrolling.
;;; It should handle the :MARGINS message by returning a list of the four margins
;;;  in the canonical left, top, right, bottom order, so that we can set two variables:
;;;  TOP-MARGIN - the number of bits from the top of the window to the top of
;;;    the first line.
;;;  BOTTOM-MARGIN - the number of bits from the bottom of the window to the
;;;    bottom of the last line.
;;; The window should handle the :SCROLL-POSITION message by returning these two values:
;;;  TOP-LINE-NUM - the line-number of the line currently at the top of the window
;;;  TOTAL-LINES - the total number of lines available to scroll through.
;;; The window should handle the message :SCROLL-POSITION<- line-number by scrolling
;;;  to that line, or trying to.  This handler should truncate its arg into range.

;;; A window which can use the scroll bar should send SCROLL-BAR a :SCROLLING-DONE
;;;  message whenever it scrolls, for any reason, mouse-related or not.
;;;  Actually, it should send that message whenever the value which :SCROLL-POSITION
;;;  would return is changed.  This will cause the scroll bar to update its display.

(DEFVAR SCROLL-BAR-WIDTH 60.)		;Width of scroll bar offset, normally.

(DEFCLASS SCROLL-BAR-CLASS WINDOW-WITH-BOX-CLASS (HIDDEN-WINDOW SCROLLED-WINDOW))

(DEFMETHOD (SCROLL-BAR-CLASS :LABEL-HEIGHT) () 0)
(DEFMETHOD (SCROLL-BAR-CLASS :PRINT-LABEL) () ())
(DEFMETHOD (SCROLL-BAR-CLASS :UPDATE-LABEL) () ())

;; It works to clobber our screen and clean us, since cleaning us
;; completely displays us.
(DEFMETHOD (SCROLL-BAR-CLASS :CLOBBER-SCREEN) () T)

(DEFVAR SCROLL-BAR (<- SCROLL-BAR-CLASS ':NEW))

(DEFMETHOD (SCROLL-BAR-CLASS :GO-TO-WORK) (WINDOW &AUX W-OR-F)
    (SETQ SCREEN (<- WINDOW ':SCREEN))
    (LOCK-SCREEN-LAYOUT
      (DO ((TOP-W WINDOW F) (F WINDOW)) (())
	(SETQ F (<- F ':FRAME))
	(OR F (RETURN (SETQ W-OR-F TOP-W))))
      (LET ((LEFT (MAX (SCREEN-X1 SCREEN) (- (<- W-OR-F ':LEFT) SCROLL-BAR-WIDTH)))
	    (TOP (<- WINDOW ':TOP))
	    (BOTTOM (<- WINDOW ':BOTTOM)))
	(<- SELF ':EDGES<- LEFT TOP (+ LEFT SCROLL-BAR-WIDTH) BOTTOM)
	(SETQ SCROLLED-WINDOW WINDOW)
	;; If we hide the selected window, remember it in HIDDEN-WINDOW.
	(SETQ HIDDEN-WINDOW SELECTED-WINDOW)
	(LET ((INHIBIT-SCREEN-RESTORATION-FLAG T))
	  ;; We should be just cleaned, not screen-restored.
	  (FUNCALL SELF ':EXPOSE))
	;; Position the mouse at the right-hand edge of the scroll bar,
	;; since it is probably moving to the left still.
	;; Do this after the time delay of exposing the scroll-bar and
	;; after allowing scheduling so that any accumulated mouse motion
	;; is discarded first.
	(PROCESS-ALLOW-SCHEDULE)
	(MOUSE-WARP (1- RIGHT) MOUSE-Y)
	(AND SELECTED-WINDOW (SETQ HIDDEN-WINDOW NIL)))))

(DEFMETHOD (SCROLL-BAR-CLASS :CLEAN) (&AUX LINE-HEIGHT TOP-LINE-NUM TOTAL-LINES
					   WINDOW-TOP WINDOW-BOTTOM
					   WINDOW-HEIGHT WINDOW-N-LINES
					   BAR-TOP BAR-BOTTOM BAR-WIDTH)
    (SETQ LINE-HEIGHT (<- SCROLLED-WINDOW ':LINE-HEIGHT))
    (MULTIPLE-VALUE (TOP-LINE-NUM TOTAL-LINES)
      (FUNCALL SCROLLED-WINDOW ':SCROLL-POSITION))
    (SETQ WINDOW-TOP TOP WINDOW-BOTTOM BOTTOM)
    (SETQ WINDOW-HEIGHT (- WINDOW-BOTTOM WINDOW-TOP))
    (SETQ WINDOW-N-LINES (// WINDOW-HEIGHT LINE-HEIGHT))
    (SETQ BAR-TOP (FIX (+ WINDOW-TOP .5S0
			  (* WINDOW-HEIGHT (// TOP-LINE-NUM (FLOAT TOTAL-LINES))))))
    (SETQ BAR-BOTTOM (FIX (+ WINDOW-TOP .5S0
			     (* WINDOW-HEIGHT (// (+ TOP-LINE-NUM WINDOW-N-LINES)
						  (FLOAT TOTAL-LINES))))))
    (SETQ BAR-WIDTH (FIX (// (- RIGHT LEFT) 3.0S0)))
    (<-AS WINDOW-WITH-BOX-CLASS ':CLEAN)
    (WITHOUT-INTERRUPTS
      (TV-OPEN-SCREEN)
      (TV-SELECT-SCREEN SCREEN)
      (TV-ERASE BAR-WIDTH (- (MIN BOTTOM BAR-BOTTOM) BAR-TOP)
		(+ LEFT BAR-WIDTH) BAR-TOP TV-ALU-IOR)))

(DEFMETHOD (SCROLL-BAR-CLASS :HANDLE-MOUSE) (&AUX SYS-MENU-FLAG HIDDEN-FLAG)
    (SETQ HIDDEN-FLAG (NOT (<- SCROLLED-WINDOW ':STATUS)))
    (TV-SET-BLINKER-FUNCTION MOUSE-BLINKER 'TV-CHARACTER-BLINKER
			     FONTS:ARROW 1)
    (SETQ MOUSE-X-OFFSET 14 MOUSE-Y-OFFSET 5)
    (DO ()
	;; This tag is thrown to with value T when Right-double is pressed.
	((SETQ SYS-MENU-FLAG (*CATCH 'SCROLL-BAR (PROGN (MOUSE-DEFAULT-HANDLER SELF) NIL))))
      (AND MOUSE-RECONSIDER (RETURN (MOUSE-WARP (<- SCROLLED-WINDOW ':LEFT) MOUSE-Y)))
      ;; Let the mouse move out of the scroll bar via the left and right edges.
      (COND ((< MOUSE-X LEFT) (RETURN T))
	    (( MOUSE-X RIGHT)
	     ;; If it moves out the right edge, move back to left edge of scrolled window.
	     (MOUSE-WARP (+ (<- SCROLLED-WINDOW ':LEFT) (- MOUSE-X RIGHT))
			 MOUSE-Y)
	     (RETURN T)))
      ;; If the scrolled window has been deexposed, that's it.
      (OR HIDDEN-FLAG (<- SCROLLED-WINDOW ':STATUS) (RETURN T))
      ;; Don't let it move out the top or bottom of the scroll bar.
      (AND (< MOUSE-Y TOP) (MOUSE-WARP MOUSE-X TOP))
      (AND ( MOUSE-Y BOTTOM) (MOUSE-WARP MOUSE-X (1- BOTTOM))))

    (LET ((INHIBIT-SCREEN-SAVING-FLAG T))
      (LOCK-SCREEN-LAYOUT (FUNCALL SELF ':DEEXPOSE)))
    ;; Restore screen under us, and reexpose windows under us.
    (FUNCALL SELF ':DEACTIVATE)
    ;; Reselect appropriate window if we deselected it.
    (AND HIDDEN-WINDOW (WINDOW-SELECT HIDDEN-WINDOW))
    (COND (SYS-MENU-FLAG
           ;; Call system menu, but first make sure that the mouse is back in the real window.
           (MOUSE-WARP (MAX MOUSE-X (<- SCROLLED-WINDOW ':LEFT)) MOUSE-Y)
           (<- (<- SCROLLED-WINDOW ':POP-UP-MENU) ':CHOOSE))))

(DEFMETHOD (SCROLL-BAR-CLASS :MOUSE-BUTTONS) (BD X Y &AUX LINE-HEIGHT
				    TOTAL-LINES TOP-LINE-NUM CUR-LINE-NUM CHAR
				    WINDOW-TOP WINDOW-BOTTOM WINDOW-HEIGHT WINDOW-N-LINES)
    X ;Doesn't care where the mouse is horizontally
    (SETQ CHAR (MOUSE-BUTTON-ENCODE BD))
    (SETQ LINE-HEIGHT (<- SCROLLED-WINDOW ':LINE-HEIGHT))
    (MULTIPLE-VALUE (TOP-LINE-NUM TOTAL-LINES)
                    (FUNCALL SCROLLED-WINDOW ':SCROLL-POSITION))
    (SETQ WINDOW-TOP TOP WINDOW-BOTTOM BOTTOM)
    (SETQ WINDOW-HEIGHT (- WINDOW-BOTTOM WINDOW-TOP))
    (SETQ WINDOW-N-LINES (// WINDOW-HEIGHT LINE-HEIGHT))
    (SETQ CUR-LINE-NUM (+ TOP-LINE-NUM (// (- Y WINDOW-TOP) LINE-HEIGHT)))
    (<- SCROLLED-WINDOW ':SCROLL-POSITION<-
        (SELECTQ (LOGAND 17 CHAR)	;10=2 clicks, low digit is which button.
          (0  ;Left: Move the line next to the mouse to the top.
           CUR-LINE-NUM)
          (10 ;Left-double: Move the line next to the mouse to the bottom.
           (- CUR-LINE-NUM (1- WINDOW-N-LINES)))
          (1  ;Middle: Move the top line to the place where the mouse is.
           (- TOP-LINE-NUM (- CUR-LINE-NUM TOP-LINE-NUM)))
          (11 ;Middle-double: Move the bottom line to here
           (+ TOP-LINE-NUM (- (+ TOP-LINE-NUM (1- WINDOW-N-LINES))
                              CUR-LINE-NUM)))
          (2  ;Right: Jump to a proportional place in the "buffer"
           ;If we are n% of the window down, we want the point
           ;n% through the buffer to appear at the top of the window.
           (FIX (+ .5S0 (// (* TOTAL-LINES (- Y WINDOW-TOP))
			    (FLOAT WINDOW-HEIGHT)))))
          (12 ;Right-double: flush the scroll bar from the screen
              ;and call the system menu.
           (*THROW 'SCROLL-BAR T))
          (OTHERWISE (TV-BEEP))))
    ;; If the scroll bar is hiding the window it is scrolling,
    ;; it should vanish after one command so the window can come back and scroll.
    ;; This isn't ideal, but it's better than not doing so.
    (LET (W-OR-F)
      (DO ((TOP-W SCROLLED-WINDOW F) (F SCROLLED-WINDOW)) (())
	(SETQ F (<- F ':FRAME))
	(OR F (RETURN (SETQ W-OR-F TOP-W))))
      (COND ((<- W-OR-F ':OVERLAPS-P SCREEN LEFT TOP RIGHT BOTTOM)
	     (SETQ MOUSE-RECONSIDER T)))))

;; Whenever a window is scrolled, it lets us know, so that if we are
;; now scrolling it, our black inner bar can be moved.
(DEFMETHOD (SCROLL-BAR-CLASS :SCROLLING-DONE) (WINDOW)
    (AND (EQ WINDOW SCROLLED-WINDOW)
	 STATUS
	 (FUNCALL SELF ':CLEAN)))



;;; Conventions which are recommended but not required.  These should be violated
;;; if and only if there is a benefit to the user to be gained.

;;; Doing something "substantial" to a window with the mouse selects it
;;; and gives its window the keyboard.

;;; Double-click-right calls the system menu.  Commands from this menu mostly
;;; spawn windows.  The screen editor is one exception which runs in the mouse process.

;;; Clicks other than single-click, single-click-hold-down, and double-click
;;; work but are significantly harder to do and should not be used for most things.
;;; Temporary menus don't require you to hold down a button and select when you
;;; lift it; instead you get one click which selects and deletes the menu,
;;; but you can first move the mouse around with no buttons held down.  This
;;; is different from certain other systems but seems like the right way.
;;; It is bad for one kind of menu to have different button conventions than another kind.

;;; The use of "non-simple" geometry should be kept within reasonable bounds.
;;; Currently it is planned to use this only for scroll bars.

;;; The mouse is not really an easy device to use for accurate positioning,
;;; so the user should generally not be required to position it precisely.
;;; The mouse is more likely to spazz than the keyboard, so mouse commands
;;; should be reversible, or be harmless, or require confirmation.  Being able
;;; to reverse the operation by pushing a different button, without moving the
;;; mouse, is desirable.

;;; Get everything going.  This has to be at the end of the file.
(ADD-INITIALIZATION "MOUSE" '(MOUSE-INITIALIZE) '(WARM FIRST))
