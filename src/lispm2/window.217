;-*-Mode:Lisp; Package:SI-*-

;Message-passing definitions for windows.
;; LISPM2; JOBDEF > contains data definitions for this package.
;; Documentation of the functions in it and how to use them
;; are in the manual, where you would expect them.

;; NEW TERMINOLOGY:
;;  An ACTIVE window is one which is potentially partially visible,
;;   or is a candidate for becoming visible.
;;  An EXPOSED window is one which is completely visible.
;;   Normally, an exposed window's process is automatically runnable.
;;  The SELECTED window is a single distinguished window which is somehow different
;;   from the point of view of the user; e.g. commands refer to it or go to it.

;; ENABLING:
;;  A window may or may not be enabled with respect to any job it is in.
;;  When a job is selected, all enabled windows in it go to the front of the
;;  active list and probably become exposed.

;Real definitions in LISPM2;JOBDEF
;(DEFCLASS WINDOW-CLASS OBJECT-CLASS (NAME SCREEN LEFT TOP RIGHT BOTTOM
;					   PROCESS STATUS BIT-SAVE-ARRAY FRAME))

;; Sent to all active windows when screen system is reinitialized.
(DEFMETHOD (WINDOW-CLASS :INITIALIZE) ()
  (FUNCALL SELF ':RUN-REASON)
  (SETQ STATUS NIL))

;You can specify the name of a window to be created either explicitly
;or by asking one to be generated.  Do that by giving (:NEW FOO)
;as the specified name.  FOO-G0069 will be generated.

;When creating a window, you must specify the process
;to be run to manage the window.  You can give an existing process,
;specify :TRIVIAL meaning to use TRIVIAL-PROCESS, or ask for a
;new process to be created by giving a list starting with :NEW, as in
;(:NEW FOO MUMBLE 100).  This supplies MUMBLE 100 as options
;to PROCESS-CREATE, and presets the process to do (FUNCALL 'FOO this-window).
;The process's name will be the same as the window's.

;A window may have a piece of paper that occupies the same screen area
;for use in typing out in the window.  It will be moved and activated
;automatically at the right times, independent of what the user-supplied
;window handler does.  To do this, make the window of class WINDOW-WITH-PC-PPR-CLASS
;or a subclass of it.  You can either provide an existing piece of paper
;with the :PC-PPR<- message or have it make a new one with :NEW-PC-PPR.

;It is desired that specifying T for :FRAME will cause a frame to be
;made for the window.  However, this may not really work as written
;for all types of windows.  It will lose if the class of window is not
;able to handle a :edges<- message before it is finished being born.
;How can this be fixed?
(DEFMETHOD (WINDOW-CLASS :BORN) ()
   (AND (LISTP NAME) (EQ (CAR NAME) ':NEW)
	(SETQ NAME (STRING-APPEND (CADR NAME) "-" (GENSYM))))
   (OR NAME (SETQ NAME (STRING-APPEND (CLASS-NAME SELF)
				      "-"
				      (GENSYM))))
   (AND (EQ PROCESS ':TRIVIAL) (SETQ PROCESS TRIVIAL-PROCESS))
   (AND (LISTP PROCESS)
	(EQ (CAR PROCESS) ':NEW)
	(LET ((PRESET PROCESS))
	   (PROCESS-PRESET
	     (SETQ PROCESS (LEXPR-FUNCALL 'PROCESS-CREATE NAME NIL (CDDR PROCESS)))
	     (CADR PRESET) SELF)))
   (OR SCREEN (SETQ SCREEN TV-DEFAULT-SCREEN))
   (LET ((SPECD-FRAME FRAME))
     (SETQ FRAME NIL)
     (COND ((EQ SPECD-FRAME T)
	    (SETQ SPECD-FRAME (<- WINDOW-SINGLE-FRAME-CLASS ':NEW))
	    (<- SPECD-FRAME ':EDGES<-
		(OR LEFT (SETQ LEFT (SCREEN-X1 SCREEN)))
		(OR TOP (SETQ TOP (SCREEN-Y1 SCREEN)))
		(OR RIGHT (SETQ RIGHT (SCREEN-X2 SCREEN)))
		(OR BOTTOM (SETQ BOTTOM (SCREEN-Y2 SCREEN))))))
     (COND (SPECD-FRAME (<- SPECD-FRAME ':PANE<- SELF))
	   (T
	     (OR LEFT (SETQ LEFT (SCREEN-X1 SCREEN)))
	     (OR TOP (SETQ TOP (SCREEN-Y1 SCREEN)))
	     (OR RIGHT (SETQ RIGHT (SCREEN-X2 SCREEN)))
	     (OR BOTTOM (SETQ BOTTOM (SCREEN-Y2 SCREEN)))))))

;;; Position on the screen.

(DEFMETHOD (WINDOW-CLASS :EDGES) ()
    (LIST LEFT TOP RIGHT BOTTOM))

(DEFMETHOD (WINDOW-CLASS :EDGES-MVR) ()
    (PROG () (RETURN LEFT TOP RIGHT BOTTOM)))

;; :MARGINS returns the widths of the margins inside the edges of the window.
;; The window may be a pane in a frame, but that's irrelevant;
;; what you get from this says what portion of the window's own region
;; is margin belonging to this window itself.
(DEFMETHOD (WINDOW-CLASS :MARGINS) ()
  '(0 0 0 0))

;Return T if the specified edges would be ok to use
;Following values are edges truncated into range.
(DEFMETHOD (WINDOW-CLASS :EDGES-OK-P) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (SETQ NEW-LEFT (TRUNCATE-TO-SCREEN-H SCREEN NEW-LEFT))
    (SETQ NEW-TOP (TRUNCATE-TO-SCREEN-V SCREEN NEW-TOP))
    (SETQ NEW-RIGHT (1+ (TRUNCATE-TO-SCREEN-H SCREEN (1- NEW-RIGHT))))
    (SETQ NEW-BOTTOM (1+ (TRUNCATE-TO-SCREEN-V SCREEN (1- NEW-BOTTOM))))
    (PROG ()
	  (RETURN (AND (> (- NEW-RIGHT NEW-LEFT) 0) (> (- NEW-BOTTOM NEW-TOP) 0))
		  NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))) 

;Set the edges of the window to specified size, with the position not mattering.
;Normally used when not exposed.  This finds some position that will make edges
;of that size legal.
(DEFMETHOD (WINDOW-CLASS :SIZE<-) (WIDTH HEIGHT)
  (LET ((NEW-LEFT (TRUNCATE-TO-SCREEN-H SCREEN (MIN LEFT (- (SCREEN-X2 SCREEN) WIDTH))))
	(NEW-TOP (TRUNCATE-TO-SCREEN-V SCREEN (MIN TOP (- (SCREEN-Y2 SCREEN) HEIGHT)))))
    (LET ((NEW-RIGHT (1+ (TRUNCATE-TO-SCREEN-H SCREEN (+ NEW-LEFT WIDTH -1))))
	  (NEW-BOTTOM (1+ (TRUNCATE-TO-SCREEN-V SCREEN (+ NEW-TOP HEIGHT -1)))))
      (OR (AND (= (- NEW-RIGHT NEW-LEFT) WIDTH)
	       (= (- NEW-BOTTOM NEW-TOP) HEIGHT))
	  (FERROR NIL "Cannot set window size as specified"))
      (<- SELF ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))))

;; Set a window's edges and select it, all at once to optimize screen updating.
;; Must be inside a LOCK-SCREEN-LAYOUT.
(DEFMETHOD (WINDOW-CLASS :SELECT-AND-EDGES<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
  (LET ((INHIBIT-AUTOEXPOSE-FLAG T))
    (FUNCALL SELF ':DEACTIVATE)
    (FUNCALL SELF ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (LET ((INHIBIT-SCREEN-RESTORATION-FLAG T))
      (FUNCALL SELF ':SELECT)))
  (WINDOWS-AUTOEXPOSE))

(DEFMETHOD (WINDOW-CLASS :EDGES<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM
					     &AUX O-LEFT O-TOP O-RIGHT O-BOTTOM)
  (LET ((OLD-STATUS STATUS))
    (LOCK-SCREEN-LAYOUT
      (MULTIPLE-VALUE (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
	(WINDOW-CHECK-EDGES SCREEN NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))
      (AND OLD-STATUS (NULL FRAME) (<- SELF ':DEEXPOSE))
      (SETQ O-LEFT LEFT O-TOP TOP O-RIGHT RIGHT O-BOTTOM BOTTOM)
      (SETQ LEFT NEW-LEFT TOP NEW-TOP RIGHT NEW-RIGHT BOTTOM NEW-BOTTOM)
      ;; If shape or size has changed, regenerate screen data if possible.
      (OR (AND (= (- O-RIGHT O-LEFT) (- RIGHT LEFT))
	       (= (- O-BOTTOM O-TOP) (- BOTTOM TOP)))
	  (FUNCALL SELF ':CLOBBER-SCREEN))
      (COND ((AND OLD-STATUS (NULL FRAME))
	     ;; Bring us back.
	     (FUNCALL SELF ':EXPOSE)
	     ;; Bring back the right stuff into areas we are departing.
	     (WINDOW-RESTORE-PARTIAL SELF SCREEN
				     O-LEFT O-TOP O-RIGHT O-BOTTOM)
	     (AND (EQ OLD-STATUS ':SELECTED)
		  (FUNCALL SELF ':SELECT))))
      ;; Expose windows we used to but no longer cover.
      (OR FRAME
	  (WINDOWS-AUTOEXPOSE)))))

(DEFUN WINDOW-CHECK-EDGES (SCREEN NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (SETQ NEW-LEFT (TRUNCATE-TO-SCREEN-H SCREEN NEW-LEFT))
    (SETQ NEW-TOP (TRUNCATE-TO-SCREEN-V SCREEN NEW-TOP))
    (SETQ NEW-RIGHT (1+ (TRUNCATE-TO-SCREEN-H SCREEN (1- NEW-RIGHT))))
    (SETQ NEW-BOTTOM (1+ (TRUNCATE-TO-SCREEN-V SCREEN (1- NEW-BOTTOM))))
    (LET ((WIDTH (- NEW-RIGHT NEW-LEFT)) (HEIGHT (- NEW-BOTTOM NEW-TOP)))
	(OR (> WIDTH 0)
	    (FERROR NIL "Ridiculous width ~D specified for window ~S" WIDTH SELF))
	(OR (> HEIGHT 0)
	    (FERROR NIL "Ridiculous height ~D specified for window ~S" HEIGHT SELF)))
    (PROG () (RETURN NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)))

(DEFUN TRUNCATE-TO-SCREEN-H (SCREEN POS)
    (MIN (MAX POS (SCREEN-X1 SCREEN)) (1- (SCREEN-X2 SCREEN))))

(DEFUN TRUNCATE-TO-SCREEN-V (SCREEN POS)
    (MIN (MAX POS (SCREEN-Y1 SCREEN)) (1- (SCREEN-Y2 SCREEN))))

(DEFMETHOD (WINDOW-CLASS :FULL-SCREEN) ()
    (COND (FRAME (FUNCALL FRAME ':FULL-SCREEN))
	  (T (<- SELF ':EDGES<-
		 (SCREEN-X1 SCREEN) (SCREEN-Y1 SCREEN)
		 (SCREEN-X2 SCREEN) (SCREEN-Y2 SCREEN)))))

(DEFMETHOD (WINDOW-CLASS :FIND-SPACE) (MIN-X MIN-Y &OPTIONAL MAX-X MAX-Y)
	 (LEXPR-FUNCALL '<- SELF ':EDGES<-
			(SCREEN-ALLOCATE SCREEN MIN-X MIN-Y MAX-X MAX-Y)))

;; Try to put the "center" of the window at a certain spot.
;; Where the center is depends on the class;  the default is the geometric center.
;; We turn into a :MOVE-RELATIVE because that makes passing on to a frame work right:
;; it moves the frame so that the center of this pane is at the desired spot.
(DEFMETHOD (WINDOW-CLASS :MOVE-NEAR) (XPOS YPOS)
   (LET ((OLD-X-CENTER (// (+ LEFT RIGHT) 2))
	 (OLD-Y-CENTER (// (+ TOP BOTTOM) 2)))
	(FUNCALL SELF ':MOVE-RELATIVE (- XPOS OLD-X-CENTER) (- YPOS OLD-Y-CENTER))))

(DEFMETHOD (WINDOW-CLASS :MOVE-RELATIVE) (DX DY)
    (COND (FRAME (FUNCALL FRAME ':MOVE-RELATIVE DX DY))
	  (T (LET ((WIDTH (- RIGHT LEFT))
		   (HEIGHT (- BOTTOM TOP))
		   NEW-LEFT NEW-TOP)
		  (SETQ NEW-LEFT (MIN (MAX (SCREEN-X1 SCREEN) (+ LEFT DX))
				      (- (SCREEN-X2 SCREEN) WIDTH))
			NEW-TOP (MIN (MAX (SCREEN-Y1 SCREEN) (+ TOP DY))
				     (- (SCREEN-Y2 SCREEN) HEIGHT)))
		  (<- SELF ':EDGES<- NEW-LEFT NEW-TOP (+ NEW-LEFT WIDTH) (+ NEW-TOP HEIGHT))
		  (LIST (+ NEW-LEFT (// WIDTH 2)) (+ NEW-TOP (// HEIGHT 2)))))))

;;; Status changes

(DEFUN WINDOW-SELECT (WINDOW)
  (LOCK-SCREEN-LAYOUT
     (<- WINDOW ':SELECT)))

(DEFMETHOD (WINDOW-CLASS :SELECT) ()
  (CHECK-LAYOUT-LOCKED)
  (OR (EQ STATUS ':SELECTED)
      (PROGN
	(<- SELF ':EXPOSE)
	(SETQ STATUS ':SELECTED)
	(COND (FRAME (<- FRAME ':SELECT-PANE SELF))
	      (T (AND SELECTED-WINDOW (<- SELECTED-WINDOW ':DESELECT))
		 (SETQ ACTIVE-WINDOWS (CONS SELF (DELQ SELF ACTIVE-WINDOWS)))
		 (SETQ SELECTED-WINDOW SELF)))
	(AND PROCESS (<- PROCESS ':SELECT)))))

(DEFMETHOD (WINDOW-CLASS :DESELECT) ()
  (CHECK-LAYOUT-LOCKED)
  (AND (EQ STATUS ':SELECTED)
       (PROGN
	(AND PROCESS (<- PROCESS ':DESELECT))
	(COND (FRAME (FUNCALL FRAME ':DESELECT-PANE SELF))
	      ((EQ SELF SELECTED-WINDOW)
	       (SETQ SELECTED-WINDOW NIL)))
	(SETQ STATUS ':EXPOSED))))

(DEFUN WINDOW-EXPOSE (WINDOW)
  (LOCK-SCREEN-LAYOUT
    (<- WINDOW ':EXPOSE)))

(DEFMETHOD (WINDOW-CLASS :EXPOSE) ()
    (CHECK-LAYOUT-LOCKED)
    (COND ((NULL STATUS)
	   (AND PROCESS (ARRAYP PROCESS) (EQ (PROCESS-WAIT-FUNCTION PROCESS) #'FALSE)
		(<- PROCESS ':RESET))
	   (SETQ STATUS ':EXPOSED)
	   (COND (FRAME (FUNCALL FRAME ':EXPOSE))
		 (T
		  (LET ((INHIBIT-SCREEN-SAVING-FLAG T))
		    (FUNCALL SELF ':ACTIVATE))
		  ;; Deexpose any windows that overlap this one.
		  (DO L EXPOSED-WINDOWS (CDR L) (NULL L)
		      (AND (<- (CAR L) ':OVERLAPS-P SCREEN LEFT TOP RIGHT BOTTOM)
			   (<- (CAR L) ':DEEXPOSE SELF)))
		  ;; Move the exposed window to the front of ACTIVE-WINDOWS,
		  ;; but not before the selected window.
		  (SETQ ACTIVE-WINDOWS
			(COND ((AND SELECTED-WINDOW (NEQ SELECTED-WINDOW SELF))
			       (CONS SELECTED-WINDOW
				     (CONS SELF
					   (DELQ SELF
						 (DELQ SELECTED-WINDOW
						       ACTIVE-WINDOWS)))))
			      (T (CONS SELF (DELQ SELF ACTIVE-WINDOWS)))))
		  (SETQ EXPOSED-WINDOWS (CONS SELF (DELQ SELF EXPOSED-WINDOWS)))))
	   (OR FRAME (<- SELF ':RESTORE-SCREEN))
	   (MOUSE-WAKEUP)
	   (OR INHIBIT-SCREEN-SAVING-FLAG
	       (AND (OR INHIBIT-SCREEN-RESTORATION-FLAG
			INHIBIT-ALL-SCREEN-BLTING-FLAG)
		    (FUNCALL SELF ':CLOBBER-SCREEN))))))

;; This operation is semi-obsolete.
(DEFMETHOD (WINDOW-CLASS :EXPOSE-CLEAN) ()
    (FUNCALL SELF ':CLOBBER-SCREEN)
    (FUNCALL SELF ':EXPOSE))

;; Deexpose should not be sent to a pane in a frame except by that frame,
;; or by setting the edges of that pane.
(DEFMETHOD (WINDOW-CLASS :DEEXPOSE) (&OPTIONAL IGNORE)
  (CHECK-LAYOUT-LOCKED)
  (AND (EQ STATUS ':SELECTED) (<- SELF ':DESELECT))
  (AND STATUS
       (COND (FRAME (SETQ STATUS NIL)
		    (<- FRAME ':DEEXPOSE))
	     (T
	      (<- SELF ':SAVE-SCREEN)
	      (SETQ STATUS NIL)
	      (SETQ EXPOSED-WINDOWS (DELQ SELF EXPOSED-WINDOWS))
	      (MOUSE-WAKEUP)))))

;; Mark as exposed any windows which are active and not at all covered.
;; If there is no selected window, select one
(DEFUN WINDOWS-AUTOEXPOSE (&AUX EDGES SCREEN)
  (OR INHIBIT-AUTOEXPOSE-FLAG
      (PROGN
	(DOLIST (W (REVERSE ACTIVE-WINDOWS))
	  (COND ((NOT (MEMQ W EXPOSED-WINDOWS))
		 (SETQ EDGES (<- W ':EDGES))
		 (SETQ SCREEN (<- W ':SCREEN))
		 (DOLIST (E ACTIVE-WINDOWS)
		   (AND (EQ E W)
			(RETURN
			  (LET ((INHIBIT-SCREEN-SAVING-FLAG T))
			    ;; This window is already properly on the screen.
			    (WINDOW-EXPOSE W))))
		   (AND (LEXPR-FUNCALL E ':OVERLAPS-P SCREEN EDGES)
			(RETURN T))))))
	;; If no window selected, select one.  Go down ACTIVE-WINDOWS and pick
	;; the first one that is exposed and allows itself to be selected.
	(DO ((L ACTIVE-WINDOWS (CDR L)))
	    ((OR (NULL L) (NOT (NULL SELECTED-WINDOW))))
	  (AND (<- (CAR L) ':STATUS)
	       (WINDOW-SELECT (CAR L)))))))

;; Saving and restoring screen bits.
;; This is normally done by exposing and deexposing.
;; However, WINDOW-RESTORE, WINDOW-RESTORE-PARTIAL and WINDOW-RESTORE-UNDERNEATH
;; also restore screen bits and WINDOW-SAVE also saves them.
;; Panes don't actually save or restore bits.  Their frames take
;; care of that for them.  The panes have no bit save arrays.

;; INHIBIT-SCREEN-RESTORATION-FLAG makes restore-screen clean
;; instead of actually restoring.
;; save-screen and restore-partial-screen are not affected.
;; INHIBIT-SCREEN-SAVING-FLAG makes all these operations no-ops.
;; It is for use when exposing or deexposing, if the screen is going
;; to be updated specially with a short-cut.
;; INHIBIT-ALL-SCREEN-BLTING-FLAG turns off the feature of saving bits.
;; It is defined to be such that setting it globally to T works reasonably.
;; It may be bound to NIL by special operations that need saving on.

(DEFMETHOD (WINDOW-CLASS :SAVE-SCREEN) ()
    (CHECK-LAYOUT-LOCKED)
    (COND (INHIBIT-SCREEN-SAVING-FLAG)
          (INHIBIT-ALL-SCREEN-BLTING-FLAG)
          (FRAME (<- FRAME ':SAVE-SCREEN))
          (T
           (LET ((HEIGHT (- BOTTOM TOP))
                 (WIDTH (- RIGHT LEFT))
                 (INHIBIT-SCHEDULING-FLAG T))
             (COND ((AND BIT-SAVE-ARRAY
                         (<= WIDTH (ARRAY-DIMENSION-N 1 BIT-SAVE-ARRAY))
                         (<= HEIGHT (ARRAY-DIMENSION-N 2 BIT-SAVE-ARRAY))))
                   (T (SETQ BIT-SAVE-ARRAY
                            (MAKE-ARRAY NIL ART-1B
                                        (LIST (LOGAND -32. (+ 31. WIDTH)) HEIGHT)))))
             (PAGE-IN-STRUCTURE BIT-SAVE-ARRAY)
	     (DOLIST (BLINKER TV-ROVING-BLINKER-LIST)
	       (TV-OPEN-BLINKER BLINKER))
             ;; Zero any extra space left in array
             ;; so that if window is made bigger the new space comes up as empty.
             (LET ((SA-W (ARRAY-DIMENSION-N 1 BIT-SAVE-ARRAY))
                   (SA-H (ARRAY-DIMENSION-N 2 BIT-SAVE-ARRAY)))
               (COND ((OR (NOT (= SA-W WIDTH))
                          (NOT (= SA-H HEIGHT)))
                      (BITBLT TV-ALU-XOR SA-W SA-H
                              BIT-SAVE-ARRAY 0 0 BIT-SAVE-ARRAY 0 0))))
             ;; Then store our current contents in the array.
             (BITBLT TV-ALU-SETA WIDTH HEIGHT
                     (SCREEN-BUFFER-PIXEL-ARRAY SCREEN) LEFT TOP
                     BIT-SAVE-ARRAY 0 0)
	     (PAGE-OUT-STRUCTURE BIT-SAVE-ARRAY)))))

(DEFMETHOD (WINDOW-CLASS :RESTORE-SCREEN) ()
    (CHECK-LAYOUT-LOCKED)
    (COND (INHIBIT-SCREEN-SAVING-FLAG)
          (FRAME (<- FRAME ':RESTORE-SCREEN))
          ((AND BIT-SAVE-ARRAY (NOT INHIBIT-SCREEN-RESTORATION-FLAG)
		(NOT INHIBIT-ALL-SCREEN-BLTING-FLAG))
           (PAGE-IN-STRUCTURE BIT-SAVE-ARRAY)
	   (LET ((HEIGHT (- BOTTOM TOP))
                 (WIDTH (- RIGHT LEFT))
                 (INHIBIT-SCHEDULING-FLAG T))
	     (DOLIST (BLINKER TV-ROVING-BLINKER-LIST)
	       (TV-OPEN-BLINKER BLINKER))
	     (BITBLT TV-ALU-SETA
		     (MIN WIDTH (ARRAY-DIMENSION-N 1 BIT-SAVE-ARRAY))
		     (MIN HEIGHT (ARRAY-DIMENSION-N 2 BIT-SAVE-ARRAY))
		     BIT-SAVE-ARRAY 0 0
		     (SCREEN-BUFFER-PIXEL-ARRAY SCREEN) LEFT TOP))
	   (PAGE-OUT-STRUCTURE BIT-SAVE-ARRAY))
          (T (<- SELF ':CLEAN))))

;; Restore a part of the screen area belonging to a window.
;; NIL specified for one of the edges means all the way to that edge of the window.
;; NO-SWAP-OUT-FLAG means leave the bit array in core.
(DEFMETHOD (WINDOW-CLASS :RESTORE-PARTIAL-SCREEN)
           (&OPTIONAL PART-LEFT PART-TOP PART-RIGHT PART-BOTTOM NO-SWAP-OUT-FLAG
	    &AUX (INHIBIT-SCHEDULING-FLAG T))
    (CHECK-LAYOUT-LOCKED)
    (COND (INHIBIT-SCREEN-SAVING-FLAG)
	  (INHIBIT-ALL-SCREEN-BLTING-FLAG)
          (FRAME (FUNCALL FRAME ':RESTORE-PARTIAL-SCREEN
                          PART-LEFT PART-TOP PART-RIGHT PART-BOTTOM))
	  (T
	   (SETQ PART-LEFT (MAX (OR PART-LEFT LEFT) LEFT)
		 PART-TOP (MAX (OR PART-TOP TOP) TOP)
		 PART-RIGHT (MIN (OR PART-RIGHT RIGHT) RIGHT)
		 PART-BOTTOM (MIN (OR PART-BOTTOM BOTTOM) BOTTOM))
	   (LET ((WIDTH (- PART-RIGHT PART-LEFT))
		 (HEIGHT (- PART-BOTTOM PART-TOP)))
		(AND (> WIDTH 0) (> HEIGHT 0)
		     (COND (BIT-SAVE-ARRAY
			    ;; Page in the part of the array that we need.
			    (PAGE-IN-BIT-ARRAY BIT-SAVE-ARRAY
					       (- PART-LEFT LEFT) (- PART-TOP TOP)
					       (- PART-RIGHT LEFT) (- PART-BOTTOM TOP))
			    (DOLIST (BLINKER TV-ROVING-BLINKER-LIST)
			      (TV-OPEN-BLINKER BLINKER))
			    ;; Copy it into the right place on the screen.
			    (BITBLT TV-ALU-SETA (MIN WIDTH (ARRAY-DIMENSION-N 1 BIT-SAVE-ARRAY))
				    (MIN HEIGHT (ARRAY-DIMENSION-N 2 BIT-SAVE-ARRAY))
				    BIT-SAVE-ARRAY
				    (- PART-LEFT LEFT) (- PART-TOP TOP)
				    (SCREEN-BUFFER-PIXEL-ARRAY SCREEN)
				    PART-LEFT PART-TOP)
			    ;; Page out most of the array.
			    ;; But leave the very front in core, in case
			    ;; we are restoring several parts of this window.
			    (OR NO-SWAP-OUT-FLAG
				(PAGE-OUT-WORDS (+ 5 (%POINTER BIT-SAVE-ARRAY))
						(- (%STRUCTURE-TOTAL-SIZE BIT-SAVE-ARRAY) 5))))
			   (T (TV-OPEN-SCREEN)
			      (TV-SELECT-SCREEN SCREEN)
			      (TV-ERASE WIDTH HEIGHT PART-LEFT PART-TOP TV-ALU-ANDCA))))))))

;; Make a window accessible to the mouse.
;; It goes on the bottom of the heap, so it may become partly visible
;; if the already existing windows do not cover the screen.
;; The layout need not be locked for this operation.
;; This does not try to expose the window in the case it is not covered.
(DEFMETHOD (WINDOW-CLASS :ACTIVATE) ()
   (IF FRAME (FUNCALL FRAME ':ACTIVATE)
       (LOCK-SCREEN-LAYOUT
	 (OR (MEMQ SELF ACTIVE-WINDOWS)
	     (PROGN
	       (FUNCALL SELF ':RUN-REASON)
	       ;; Lock here just so activates on different windows don't clobber each other
	       ;; Put it on ACTIVE-WINDOWS twice to control WINDOW-RESTORE-UNDERNEATH
	       (SETQ ACTIVE-WINDOWS (NCONC ACTIVE-WINDOWS (CONS SELF (NCONS SELF))))
	       ;; Make window appear in places where it shows through.
	       (WINDOW-RESTORE-UNDERNEATH SELF SCREEN LEFT TOP RIGHT BOTTOM)
	       ;; Set ACTIVE-WINDOWS up right.
	       (SETQ ACTIVE-WINDOWS (NCONC (DELQ SELF ACTIVE-WINDOWS) (NCONS SELF))))))))
    
;; Take a window completely off the screen.
;; It is inaccessible to the mouse and not even partly visible.
;; The layout need not be locked for this operation.
(DEFMETHOD (WINDOW-CLASS :DEACTIVATE) ()
    (COND (FRAME (FUNCALL FRAME ':DEACTIVATE))
	  (T ;; Redisplay what was under this window, or nothing, where the window was.
	     (LOCK-SCREEN-LAYOUT
	       (AND STATUS (<- SELF ':DEEXPOSE))
 	       (COND ((MEMQ SELF ACTIVE-WINDOWS)
		      (WINDOW-RESTORE-UNDERNEATH SELF SCREEN LEFT TOP RIGHT BOTTOM)
		      (FUNCALL SELF ':REVOKE-RUN-REASON)
		      (SETQ ACTIVE-WINDOWS (DELQ SELF ACTIVE-WINDOWS)))))
	     (WINDOWS-AUTOEXPOSE))))

;; Tell a window it is being discarded for good.
(DEFMETHOD (WINDOW-CLASS :KILL) ()
  (AND PROCESS (<- PROCESS ':RESET))
  (FUNCALL SELF ':DEACTIVATE))

;; Move a window to the bottom of the heap.
;; It may still be partly visible if no other window overlaps it.
;; The layout need not be locked for this operation.
(DEFMETHOD (WINDOW-CLASS :BURY) ()
    (COND (FRAME (FUNCALL FRAME ':BURY))
	  (T
	   (LOCK-SCREEN-LAYOUT
	     (COND (STATUS (<- SELF ':DEEXPOSE)))
	     (COND ((MEMQ SELF ACTIVE-WINDOWS)
		    (SETQ ACTIVE-WINDOWS (NCONC ACTIVE-WINDOWS (NCONS SELF)))
		    ;; Redisplay anything which had been under this window as over it.
		    (WINDOW-RESTORE-UNDERNEATH SELF SCREEN LEFT TOP RIGHT BOTTOM)
		    (SETQ ACTIVE-WINDOWS
			  (NCONC (DELQ SELF ACTIVE-WINDOWS) (NCONS SELF)))))
	     (WINDOWS-AUTOEXPOSE)))))

(DEFMETHOD (WINDOW-CLASS :OVERLAPS-P) (OTHER-SCREEN OTHER-LEFT OTHER-TOP OTHER-RIGHT OTHER-BOTTOM)
    (AND (EQ SCREEN OTHER-SCREEN)
	 (NOT (OR (>= LEFT OTHER-RIGHT)
		  (>= OTHER-LEFT RIGHT)
		  (>= TOP OTHER-BOTTOM)
		  (>= OTHER-TOP BOTTOM)))))

(DEFMETHOD (WINDOW-CLASS :CONTAINS-P) (OTHER-SCREEN OTHER-LEFT OTHER-TOP OTHER-RIGHT OTHER-BOTTOM)
    (AND (EQ SCREEN OTHER-SCREEN)
	 (<= LEFT OTHER-LEFT)
	 (<= OTHER-RIGHT RIGHT)
	 (<= TOP OTHER-TOP)
	 (<= OTHER-BOTTOM BOTTOM)))

(DEFMETHOD (WINDOW-CLASS :CONTAINS-POINT-P) (OTHER-SCREEN X Y)
    (AND (EQ SCREEN OTHER-SCREEN)
	 (<= LEFT X)
	 (< X RIGHT)
	 (<= TOP Y)
	 (< Y BOTTOM)))

(DEFMETHOD (WINDOW-CLASS :RESET) ()
	 (AND PROCESS (<- PROCESS ':RESET)))

(DEFMETHOD (WINDOW-CLASS :NOTE-INPUT) () NIL)

(DEFMETHOD (WINDOW-CLASS :RUN-REASON) ()
	 (AND PROCESS (<- PROCESS ':RUN-REASON SELF)))

(DEFMETHOD (WINDOW-CLASS :REVOKE-RUN-REASON) ()
	 (AND PROCESS (<- PROCESS ':REVOKE-RUN-REASON SELF)))

(DEFMETHOD (WINDOW-CLASS :ARREST-REASON) (&OPTIONAL (REASON ':USER))
	 (AND PROCESS (<- PROCESS ':ARREST-REASON REASON)))

(DEFMETHOD (WINDOW-CLASS :REVOKE-ARREST-REASON) (&OPTIONAL (REASON ':USER))
	 (AND PROCESS (<- PROCESS ':REVOKE-ARREST-REASON REASON)))

(DEFMETHOD (WINDOW-CLASS :UPDATE) () NIL)
;; NOTE: A CLOBBER-SCREEN method should return T
;; if the window is really going to be able to regenerate its bits.
(DEFMETHOD (WINDOW-CLASS :CLOBBER-SCREEN) () NIL)

;Draw a vector
;Shouldn't draw if the window isn't exposed.
(DEFMETHOD (WINDOW-CLASS :DRAW-LINE) (XO YO X Y &OPTIONAL (TV-ALU TV-ALU-IOR))
  (LET ((MARGINS (<- SELF ':MARGINS)))
    (LET ((LLIM (+ LEFT (CAR MARGINS))) (TLIM (+ TOP (CADR MARGINS)))
	  (RLIM (- RIGHT (CADDR MARGINS))) (BLIM (- BOTTOM (CADDDR MARGINS))))
      (TV-DRAW-LINE-CLIPPED (+ XO LLIM) (+ YO TLIM) (+ X LLIM) (+ Y TLIM)
			    TV-ALU SCREEN LLIM TLIM RLIM BLIM))))

(LOCAL-DECLARE ((SPECIAL LEFT TOP RIGHT BOTTOM))
(DEFUN TV-DRAW-LINE-CLIPPED (FROM-X FROM-Y TO-X TO-Y ALU &OPTIONAL (SCREEN TV-DEFAULT-SCREEN)
				                                   LEFT TOP RIGHT BOTTOM)
  (OR LEFT (SETQ LEFT (SCREEN-X1 SCREEN) TOP (SCREEN-Y1 SCREEN) RIGHT (SCREEN-X2 SCREEN)
		 BOTTOM (SCREEN-Y2 SCREEN)))
  (DO ((FROM-VISIBILITY (TV-CLIP-VISIBILITY FROM-X FROM-Y) (TV-CLIP-VISIBILITY FROM-X FROM-Y))
       (TO-VISIBILITY (TV-CLIP-VISIBILITY TO-X TO-Y)))
      ;;When completely visible, draw the line
      ((AND (ZEROP FROM-VISIBILITY) (ZEROP TO-VISIBILITY))
       (TV-DRAW-LINE FROM-X FROM-Y TO-X TO-Y ALU SCREEN))
    ;;If all off the screen, dont draw anything
    (OR (ZEROP (LOGAND FROM-VISIBILITY TO-VISIBILITY)) (RETURN NIL))
    ;;Exchange points to try to make to point visible
    (AND (ZEROP FROM-VISIBILITY)
	 (PSETQ FROM-X TO-X TO-X FROM-X FROM-Y TO-Y TO-Y FROM-Y
		FROM-VISIBILITY TO-VISIBILITY TO-VISIBILITY FROM-VISIBILITY))
    ;;If TO-X = FROM-X then FROM-VISIBILITY = 0, 4 or 8 so there is no danger
    ;; of divide by zero in the next "Push"
    (COND ((LDB-TEST 0001 FROM-VISIBILITY)		;Push toward left edge
	   (SETQ FROM-Y (+ FROM-Y (// (* (- TO-Y FROM-Y) (- LEFT FROM-X)) (- TO-X FROM-X)))
		 FROM-X LEFT))
	  ((LDB-TEST 0101 FROM-VISIBILITY)		;Push toward right edge
	   (SETQ FROM-Y (+ FROM-Y (// (* (- TO-Y FROM-Y) (- RIGHT FROM-X)) (- TO-X FROM-X)))
		 FROM-X RIGHT)))
    (COND ((LDB-TEST 0201 FROM-VISIBILITY)		;Push toward top
	   ;;It is possible that TO-Y = FROM-Y at this point because of the effects of
	   ;; the last "Push", but in that case TO-X is probably equal to FROM-X as well
	   ;; (or at least close to it) so we needn't draw anything:
	   (AND (= TO-Y FROM-Y) (RETURN NIL))
	   (SETQ FROM-X (+ FROM-X (// (* (- TO-X FROM-X) (- TOP FROM-Y)) (- TO-Y FROM-Y)))
		 FROM-Y TOP))
	  ((LDB-TEST 0301 FROM-VISIBILITY)		;Push toward bottom
	   ;; Same:
	   (AND (= TO-Y FROM-Y) (RETURN NIL))
	   (SETQ FROM-X (+ FROM-X (// (* (- TO-X FROM-X) (- BOTTOM FROM-Y)) (- TO-Y FROM-Y)))
		 FROM-Y BOTTOM))))))

(DEFUN TV-CLIP-VISIBILITY (POINT-X POINT-Y &AUX VISIBILITY)
  (LOCAL-DECLARE ((SPECIAL LEFT TOP RIGHT BOTTOM))
    (SETQ VISIBILITY (COND ((< POINT-X LEFT) 1)
			   ((> POINT-X RIGHT) 2)
			   (T 0)))
    (COND ((< POINT-Y TOP) (DPB 1 0201 VISIBILITY))
	  ((> POINT-Y BOTTOM) (DPB 1 0301 VISIBILITY))
	  (T VISIBILITY))))

(DEFMETHOD (WINDOW-CLASS :CLEAN) (&aux (bits-per-pixel (screen-bits-per-pixel screen)))
    (LET ((INHIBIT-SCHEDULING-FLAG T))
         (TV-OPEN-SCREEN)
	 (TV-SELECT-SCREEN SCREEN)
         (TV-ERASE (* bits-per-pixel (- RIGHT LEFT)) (- BOTTOM TOP)
                   LEFT TOP TV-ALU-ANDCA)))

(DEFMETHOD (WINDOW-CLASS :HANDLE-MOUSE) () (MOUSE-DEFAULT-HANDLER SELF))

(DEFMETHOD (WINDOW-CLASS :MOUSE-MOVES) (X Y) X Y (MOUSE-SET-X-BLINKER-CURSORPOS))

(DEFMETHOD (WINDOW-CLASS :MOUSE-BUTTONS) (BD X Y)
    X Y ;position of mouse is ignored
	   ;;    If left button pushed, select SELF
           ;;    Middle, send ':DOCUMENTATION message to SELF under mouse.
           ;;    Right button twice, show system menu.
    (LET ((BUTTONS (MOUSE-BUTTON-ENCODE BD)))
	(SELECTQ BUTTONS
	   (2000 (OR (EQ SELF SELECTED-WINDOW)
		     (WINDOW-SELECT SELF)))
	   (2001 (<- SELF ':DOCUMENTATION))
	   (2012 (<- (<- SELF ':POP-UP-MENU) ':CHOOSE)))))

(DEFMETHOD (WINDOW-CLASS :FORCE-KBD-INPUT) (INPUT)
    (AND PROCESS (<- PROCESS ':FORCE-KBD-INPUT INPUT)))

(DEFMETHOD (WINDOW-CLASS :PRINT-LABEL) (PC-PPR-1)
    (TV-STRING-OUT PC-PPR-1 NAME))

(DEFMETHOD (WINDOW-CLASS :LABEL-HEIGHT) ()
    (FONT-CHAR-HEIGHT (SCREEN-DEFAULT-FONT SCREEN)))

(DEFMETHOD (WINDOW-CLASS :CLOBBER-LABEL) ()
    (AND FRAME (<- FRAME ':CLOBBER-LABEL)))

(DEFMETHOD (WINDOW-CLASS :UPDATE-LABEL) ()
    (AND FRAME (<- FRAME ':UPDATE-LABEL)))

(DEFMETHOD (WINDOW-CLASS :UPDATE-LABEL-IF-NECESSARY) ()
    (AND FRAME (<- FRAME ':UPDATE-LABEL-IF-NECESSARY)))

;; in lieu of any other knowledge, say we scroll in units of dots.
(DEFMETHOD (WINDOW-CLASS :LINE-HEIGHT) () 1)

;; In lieu of any other knowledge, say we have nothing to scroll through
;; and ignore requests to scroll.  In general, this operation would
;; return the top line number and the total lines to scroll through.
(DEFMETHOD (WINDOW-CLASS :SCROLL-POSITION) ()
    (PROG () (RETURN 0 0)))
(DEFMETHOD (WINDOW-CLASS :SCROLL-POSITION<-) (NEW-LINE-NUM)
    NEW-LINE-NUM ;ignored
    NIL)

(DEFMETHOD (WINDOW-CLASS :SUPERVISORY-SIGNAL) (TYPE)
    (<- TOP-WINDOW ':RESET)	;**THIS NEEDS TO BE UPDATED**
    (TOP-WINDOW))

(DECLARE (SPECIAL COMPLEMENT-LABELS-FLAG HALFTONE-LABELS-FLAG))
(SETQ COMPLEMENT-LABELS-FLAG NIL HALFTONE-LABELS-FLAG NIL)

;(DEFCLASS WINDOW-WITH-BOX-CLASS WINDOW-CLASS
;    (LABEL LABEL-POSITION LABEL-NEEDS-UPDATING-FLAG MARGINS))

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :LABEL<-) (NEW-LABEL)
    (SETQ LABEL NEW-LABEL
	  LABEL-NEEDS-UPDATING-FLAG T))

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :CLOBBER-LABEL) ()
    (SETQ LABEL-NEEDS-UPDATING-FLAG T))

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :CLEAN) ()
    (COND ((ZEROP (CAR MARGINS))
	   (<-AS WINDOW-CLASS ':CLEAN))
	  (T (SI:SCREEN-WINDOW-BOX SELF)))
    (<- SELF ':UPDATE-LABEL))

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :UPDATE) ()
    (<-AS WINDOW-CLASS ':UPDATE)
    (COND (LABEL-NEEDS-UPDATING-FLAG
	   (<- SELF ':UPDATE-LABEL))))

(DECLARE (SPECIAL LABELS-HALFTONE LABELS-HALFTONE-ALU))
(COND ((NOT (BOUNDP 'LABELS-HALFTONE))
       (SETQ LABELS-HALFTONE (MAKE-ARRAY NIL 'ART-16B '(2 4)))
       (ASET 52525 LABELS-HALFTONE 0 0)  ;We use 50% halftone and white on black
       (ASET 52525 LABELS-HALFTONE 1 0)  ;You're welcome to patch this to something nicer
       (ASET 125252 LABELS-HALFTONE 0 1)
       (ASET 125252 LABELS-HALFTONE 1 1)
       (ASET 52525 LABELS-HALFTONE 0 2)
       (ASET 52525 LABELS-HALFTONE 1 2)
       (ASET 125252 LABELS-HALFTONE 0 3) ;
       (ASET 125252 LABELS-HALFTONE 1 3)
       (SETQ LABELS-HALFTONE-ALU 100) ;NOR
       ))

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :UPDATE-LABEL) ()
  (COND ((NOT (ZEROP (<- SELF ':LABEL-HEIGHT)))
    (LET (PC-PPR-1)
	 (SETQ PC-PPR-1 (PER-SCREEN-PC-PPR SCREEN LABEL-POSITION))
         (TV-CLEAR-PC-PPR PC-PPR-1)
	 (*CATCH 'UPDATE-LABEL
	    (<- SELF ':PRINT-LABEL PC-PPR-1))
	 (AND HALFTONE-LABELS-FLAG
              (TV-PREPARE-PC-PPR (PC-PPR-1)
		  (BITBLT LABELS-HALFTONE-ALU
			  (- (PC-PPR-CURRENT-X PC-PPR-1) (PC-PPR-LEFT-MARGIN PC-PPR-1))
			  (- (PC-PPR-BOTTOM-MARGIN PC-PPR-1) (PC-PPR-TOP-MARGIN PC-PPR-1))
			  LABELS-HALFTONE 0 0
			  (SCREEN-BUFFER-PIXEL-ARRAY SCREEN)
			  (PC-PPR-LEFT-MARGIN PC-PPR-1)
			  (PC-PPR-TOP-MARGIN PC-PPR-1))))
	 (AND COMPLEMENT-LABELS-FLAG
	      (TV-PREPARE-PC-PPR (PC-PPR-1)
		  (TV-ERASE (- (PC-PPR-CURRENT-X PC-PPR-1) ;Only complement part printed
			       (PC-PPR-LEFT-MARGIN PC-PPR-1))
			    (- (PC-PPR-BOTTOM-MARGIN PC-PPR-1)
			       (PC-PPR-TOP-MARGIN PC-PPR-1))
			    (PC-PPR-LEFT-MARGIN PC-PPR-1)
			    (PC-PPR-TOP-MARGIN PC-PPR-1)
			    TV-ALU-XOR))))))
  (SETQ LABEL-NEEDS-UPDATING-FLAG NIL))

(DEFUN PER-SCREEN-PC-PPR-END-LINE-FCN (IGNORE) (*THROW 'UPDATE-LABEL NIL))

(DEFUN PER-SCREEN-PC-PPR (SCREEN LABEL-POSITION &AUX FONT PC-PPR-1)
       (SETQ FONT (SCREEN-DEFAULT-FONT SCREEN))
       (OR (BOUNDP 'SCREEN-PC-PPR-ALIST)
	   (SETQ SCREEN-PC-PPR-ALIST NIL))
       (SETQ PC-PPR-1 (CADR (ASSQ SCREEN SCREEN-PC-PPR-ALIST)))
       (OR PC-PPR-1
	   (PROGN (SETQ PC-PPR-1
			(TV-DEFINE-PC-PPR "WINDOW-label"
					  (LIST FONT)
					  ':SCREEN SCREEN
					  ':MORE-P NIL
					  ':BLINKER-P NIL))
		  (PUSH (LIST SCREEN PC-PPR-1) SCREEN-PC-PPR-ALIST)))
       (SETF (PC-PPR-END-LINE-FCN PC-PPR-1) 'PER-SCREEN-PC-PPR-END-LINE-FCN)
       (TV-REDEFINE-PC-PPR PC-PPR-1
			   ':LEFT (FIRST LABEL-POSITION)
			   ':TOP (SECOND LABEL-POSITION)
			   ':RIGHT (THIRD LABEL-POSITION)
			   ':BOTTOM (FOURTH LABEL-POSITION))
       PC-PPR-1)

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :PRINT-LABEL) (PC-PPR-1)
	 (TV-STRING-OUT PC-PPR-1 (OR LABEL NAME)))

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :EDGES<-)
	   (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM &AUX NEW-MARGINS)
    (MULTIPLE-VALUE (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
		    (WINDOW-CHECK-EDGES SCREEN NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))
    (COND ((AND (EQ NEW-LEFT (SCREEN-X1 SCREEN))
		(EQ NEW-TOP (SCREEN-Y1 SCREEN))
		(EQ NEW-RIGHT (SCREEN-X2 SCREEN))
		(EQ NEW-BOTTOM (SCREEN-Y2 SCREEN)))
	   (SETQ NEW-MARGINS (LIST 0 0 0 (<- SELF ':LABEL-HEIGHT))))
	  (T
	   (SETQ NEW-MARGINS (LIST 2 2 2 (+ 2 (<- SELF ':LABEL-HEIGHT))))))
    (<- SELF ':EDGES-INSIDE-BOX<-
	(+ NEW-LEFT (CAR NEW-MARGINS))
	(+ NEW-TOP (CADR NEW-MARGINS))
	(- NEW-RIGHT (CADDR NEW-MARGINS))
	(- NEW-BOTTOM (CADDDR NEW-MARGINS)))
    (SETQ LABEL-POSITION
	  (LIST (+ NEW-LEFT (CAR NEW-MARGINS))
		(- NEW-BOTTOM (CADDDR NEW-MARGINS))
		(- NEW-RIGHT (CADDR NEW-MARGINS))
		(- NEW-BOTTOM (CADR NEW-MARGINS))))
    (SETQ MARGINS NEW-MARGINS)
    (<-AS WINDOW-CLASS ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))

(DEFMETHOD (WINDOW-WITH-BOX-CLASS :EDGES-INSIDE-BOX<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM ;ignored
    NIL)

;(DEFCLASS WINDOW-WITH-PC-PPR-CLASS WINDOW-CLASS (PC-PPR))

;If a pc-ppr is supplied, take our edges from it.
;If pc-ppr is supplied but is a list, not a pc-ppr,
;use that list as the font-map (nil for the default one) and options and create one.
;Otherwise, create a default-style pc-ppr.
(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :BORN) ()
    (COND ((AND PC-PPR (ATOM PC-PPR))
	   (OR LEFT (SETQ LEFT (PC-PPR-LEFT PC-PPR)))
	   (OR TOP (SETQ TOP (PC-PPR-TOP PC-PPR)))
	   (OR RIGHT (SETQ RIGHT (PC-PPR-RIGHT PC-PPR)))
	   (OR BOTTOM (SETQ BOTTOM (PC-PPR-BOTTOM PC-PPR)))))
    (<-AS WINDOW-CLASS ':BORN)
    (OR (AND PC-PPR (ATOM PC-PPR))
	(LEXPR-FUNCALL SELF ':NEW-PC-PPR PC-PPR)))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :PC-PPR<-) (NEW-PC-PPR)
	 (OR (EQ (PC-PPR-SCREEN NEW-PC-PPR) SCREEN)
	      (FERROR NIL "~S and ~S live on different screens" SELF PC-PPR))
	 (SETQ PC-PPR NEW-PC-PPR)
	 (SETF (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR) (IF (NULL STATUS) 1 0))
	 (COND (BOTTOM
		(LEXPR-FUNCALL '<- SELF ':EDGES<- (<- SELF ':EDGES)))
	       (T (<- SELF ':EDGES<-
		      (PC-PPR-LEFT PC-PPR)
		      (PC-PPR-TOP PC-PPR)
		      (PC-PPR-RIGHT PC-PPR)
		      (PC-PPR-BOTTOM PC-PPR)))))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :NEW-PC-PPR) (&OPTIONAL FONT-MAP &REST OPTIONS)
	 (LET ((PC-PPR-1
		(LEXPR-FUNCALL 'TV-DEFINE-PC-PPR NAME
			      (OR FONT-MAP
				  (LIST (SCREEN-DEFAULT-FONT SCREEN)))
			      ':SCREEN SCREEN
			      OPTIONS)))
	      (<- SELF ':PC-PPR<- PC-PPR-1)))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :SELECT) ()
	 (<-AS WINDOW-CLASS ':SELECT)
	 (TV-SELECT-PC-PPR PC-PPR))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :DESELECT) ()
	 (<-AS WINDOW-CLASS ':DESELECT)
	 (TV-DESELECT-PC-PPR PC-PPR))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :EXPOSE) (&AUX CLEAR)
	 (<-AS WINDOW-CLASS ':EXPOSE)
	 ;; If, due to changes in size, our cursorpos is no longer in range,
	 ;; arrange to reinit it (later, when output is working again).
	 (OR (AND ( (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-RIGHT-LIMIT PC-PPR))
		  ( (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BOTTOM-LIMIT PC-PPR)))
	     (PROGN (SETQ CLEAR T)))
	 ;; If we did a :CLEAN instead of restoring the bits, then our blinkers
	 ;; aren't already present on the screen though they think they are.
	 (AND INHIBIT-SCREEN-RESTORATION-FLAG
	      (DOLIST (BLINKER (PC-PPR-BLINKER-LIST PC-PPR))
		(SETF (TV-BLINKER-PHASE BLINKER) NIL)))
	 ;; Connect blinkers to screen again.  Now TV-PREPARE-PC-PPR will work
	 (TV-EXPOSE-PC-PPR PC-PPR)
	 ;; so it's ok to do output now.  But don't let anyone else do it till after us.
	 (WITHOUT-INTERRUPTS
	   (SETF (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR) 0)
	   (COND (CLEAR (TV-SET-CURSORPOS PC-PPR 0 0)
			(TV-CLEAR-EOL PC-PPR)))))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :DEEXPOSE) (&OPTIONAL REASON)
	(SETF (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR) 1)
	(TV-DEEXPOSE-PC-PPR PC-PPR)
	(<-AS WINDOW-CLASS ':DEEXPOSE REASON))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :RESET) ()
	 (SETF (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR) (IF (NULL STATUS) 1 0))
	 (<-AS WINDOW-CLASS ':RESET))

;; All windows receive this message whenever something waits for input from the user.
;; What we do with it is postpone all --more--ing in our pc-ppr
;; until a long time from now.
(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :NOTE-INPUT) ()
    (COND ((NULL (PC-PPR-MORE-VPOS PC-PPR)))	;Unless MORE inhibited entirely
	  ((< (* (- (PC-PPR-BOTTOM PC-PPR)	;See if near bottom
		    (PC-PPR-CURRENT-Y PC-PPR))
		 4)
	      (- (PC-PPR-BOTTOM PC-PPR) (PC-PPR-TOP PC-PPR)))
	   (SETF (PC-PPR-MORE-VPOS PC-PPR)	;Near bottom, wrap around before MOREing
		 (+ 100000 (- (PC-PPR-CURRENT-Y PC-PPR)	; just above this line
			      (PC-PPR-LINE-HEIGHT PC-PPR)))))
	  ((SETF (PC-PPR-MORE-VPOS PC-PPR)	;Arrange to MORE at the bottom
		 (1+ (- (PC-PPR-BOTTOM-LIMIT PC-PPR) (PC-PPR-LINE-HEIGHT PC-PPR)))))))

;; This must be careful to get any errors before making any changes.
(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :EDGES<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (MULTIPLE-VALUE (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
		    (WINDOW-CHECK-EDGES SCREEN NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))
    (AND PC-PPR
	 (TV-REDEFINE-PC-PPR PC-PPR ':LEFT NEW-LEFT ':TOP NEW-TOP
				    ':RIGHT NEW-RIGHT ':BOTTOM NEW-BOTTOM))
    (<-AS WINDOW-CLASS ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))

(DEFMETHOD (WINDOW-WITH-PC-PPR-CLASS :LINE-HEIGHT) ()
    (PC-PPR-LINE-HEIGHT PC-PPR))

;(DEFCLASS WINDOW-WITH-PC-PPR-AND-BOX-CLASS WINDOW-WITH-PC-PPR-CLASS
;    (LABEL LABEL-POSITION LABEL-NEEDS-UPDATING-FLAG MARGINS))

(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :CLEAN) ()
	 (<-AS WINDOW-WITH-BOX-CLASS ':CLEAN))
(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :LABEL-HEIGHT) ()
	 (<-AS WINDOW-WITH-BOX-CLASS ':LABEL-HEIGHT))
(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :CLOBBER-LABEL) ()
	 (<-AS WINDOW-WITH-BOX-CLASS ':CLOBBER-LABEL))
(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :PRINT-LABEL) (PC-PPR-1)
	 (<-AS WINDOW-WITH-BOX-CLASS ':PRINT-LABEL PC-PPR-1))
(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :UPDATE-LABEL) ()
	 (<-AS WINDOW-WITH-BOX-CLASS ':UPDATE-LABEL))
(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :UPDATE) ()
	 (<-AS WINDOW-WITH-BOX-CLASS ':UPDATE))

(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :EDGES<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
	 (<-AS WINDOW-WITH-BOX-CLASS ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))

;; This must be careful to get any errors before making any changes.
(DEFMETHOD (WINDOW-WITH-PC-PPR-AND-BOX-CLASS :EDGES-INSIDE-BOX<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (AND PC-PPR
	 (LET ((OX (- (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR)))
	       (OY (- (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR))))
	      (AND PC-PPR
		   (TV-REDEFINE-PC-PPR PC-PPR
			     ':LEFT NEW-LEFT ':TOP NEW-TOP
			     ':RIGHT NEW-RIGHT ':BOTTOM NEW-BOTTOM))
	      ;; Keep relative cursorpos the same even if that puts cursor outside pc ppr.
	      ;; If that happens, the expose method will take care of it.
	      (SETF (PC-PPR-CURRENT-X PC-PPR)
		    (+ OX (PC-PPR-LEFT-MARGIN PC-PPR)))
	      (SETF (PC-PPR-CURRENT-Y PC-PPR)
		    (+ OY (PC-PPR-TOP-MARGIN PC-PPR))))))

;(DEFCLASS LISP-LISTENER-CLASS WINDOW-WITH-PC-PPR-CLASS (STREAM DEFAULT-PROCESS))

(DEFMETHOD (LISP-LISTENER-CLASS :BORN) ()
    (OR PROCESS (SETQ PROCESS `(:NEW LISP-LISTEN-LOOP
			             :REGULAR-PDL-SIZE 40000
				     :SPECIAL-PDL-SIZE 4000)))
    (OR PC-PPR (SETQ PC-PPR '(NIL :BLINKER-P T)))
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':BORN)
    (SETQ DEFAULT-PROCESS PROCESS)
    (SETQ STREAM (TV-MAKE-STREAM PC-PPR)))

(DEFUN LISP-LISTEN-LOOP (WINDOW)
  (LISP-TOP-LEVEL1 (<- WINDOW ':STREAM)))

(DEFMETHOD (LISP-LISTENER-CLASS :SUPERVISORY-SIGNAL) (TYPE)
    (COND ((EQ TYPE ':CALL-KEY)
	   (SETQ PROCESS DEFAULT-PROCESS)
	   (<- PROCESS ':SELECT)
	   (<- PROCESS ':RESET))
	  ((EQ TYPE ':BREAK)
	   (<- PROCESS ':FORCE-BREAK))))

(DEFMETHOD (LISP-LISTENER-CLASS :DESELECT) ()
  (SETQ PROCESS SELECTED-PROCESS)
  (<-AS WINDOW-WITH-PC-PPR-CLASS ':DESELECT))

;;; Jobs.

;(DEFCLASS JOB-CLASS OBJECT-CLASS (NAME WINDOWS ENABLED-WINDOWS))

(DEFMETHOD (JOB-CLASS :WINDOWS<-) (WINDOW-LIST)
      (SETQ WINDOWS WINDOW-LIST)
      (SETQ ENABLED-WINDOWS NIL))
;Add a window to a job.  It starts out not enabled.
(DEFMETHOD (JOB-CLASS :ADD-WINDOW) (WINDOW)
    (OR (MEMQ WINDOW WINDOWS) (PUSH WINDOW WINDOWS)))

;Jobs are usually used through the global job name data base, JOB-ALIST.
;JOB-CREATE makes a job and puts it on the alist.  JOB-KILL removes it.
;JOB-DECODE finds the job with a specified name.
;JOB-FIND-OR-CREATE makes one if there isn't one.
;JOB-SELECT selects the windows in a job with a specified name.  JOB-BURY buries them.

(DEFUN JOB-CREATE (&OPTIONAL NAME &AUX JOB)
    (SETQ JOB (<- JOB-CLASS ':NEW))
    (COND (NAME (<- JOB ':NAME<- NAME)
		(PUSH (CONS NAME JOB) JOB-ALIST)))
    JOB)

(DEFUN JOB-FIND-OR-CREATE (NAME)
    (OR (CDR (ASS 'STRING-EQUAL NAME JOB-ALIST))
	(JOB-CREATE NAME)))

(DEFUN JOB-KILL (JOB)
    (SETQ JOB (JOB-DECODE JOB))
    (SETQ JOB-ALIST (DEL 'EQ-TO-CDR-P JOB JOB-ALIST)))

;Accept a job or the symbolic name of one.
;Return the job given or named.
(DEFUN JOB-DECODE (JOB)
    (PROG ()
	  LOOP
	  (COND ((SYMBOLP JOB)
		 (RETURN (OR (CADR (ASSQ JOB JOB-ALIST))
			     (PROGN (SETQ JOB (CERROR T NIL NIL
						      "Undefined Job name: ~S"
						      JOB))
				    (GO LOOP))))))
	  (RETURN JOB)))

(DEFUN EQ-TO-CAR-P (X Y)
    (EQ X (CAR Y)))

(DEFUN EQ-TO-CDR-P (X Y)
    (EQ X (CDR Y)))

(DEFUN JOB-SELECT (JOB-NAME)
    (<- (JOB-DECODE JOB-NAME) ':SELECT))

(DEFUN JOB-BURY (JOB-NAME)
    (<- (JOB-DECODE JOB-NAME) ':BURY))

;;; To select a job, select all enabled windows in it.
(DEFMETHOD (JOB-CLASS :SELECT) ()
  (LOCK-SCREEN-LAYOUT
    (DO L (REVERSE ENABLED-WINDOWS) (CDR L) (NULL L)
      (<- (CAR L) ':SELECT))))

(DEFMETHOD (JOB-CLASS :EXPOSE) ()
  (LOCK-SCREEN-LAYOUT
    (DO L ENABLED-WINDOWS (CDR L) (NULL L)
      (<- (CAR L) ':EXPOSE))))

;;; To bury a job, bury all windows in it.
(DEFMETHOD (JOB-CLASS :BURY) () 
  (LET ((INHIBIT-AUTOEXPOSE-FLAG T))
    (DO L WINDOWS (CDR L) (NULL L)
	(<- (CAR L) ':BURY))))

;;; Enabling and disabling the windows in a job.
(DEFMETHOD (JOB-CLASS :ENABLE) (WINDOW)
    (<- SELF ':ADD-WINDOW WINDOW)
    (OR (MEMQ WINDOW ENABLED-WINDOWS)
	(PUSH WINDOW ENABLED-WINDOWS)))

(DEFMETHOD (JOB-CLASS :DISABLE) (WINDOW)
    (SETQ ENABLED-WINDOWS (DELQ WINDOW ENABLED-WINDOWS)))

;;; Implement the :FIND-SPACE method for windows.
;;; Find space that isn't in use by any exposed window.

(DECLARE (SPECIAL SCREEN MIN-X MIN-Y MAX-X MAX-Y))

;;; Allocate at least MIN-X, MIN-Y space, expanding up to MAX-X, MAX-Y.
;;; Return the allocated area as a 4-list (LEFT TOP RIGHT BOTTOM)
(DEFUN SCREEN-ALLOCATE (SCREEN MIN-X MIN-Y MAX-X MAX-Y)
       (LET ((X-SIZE (- (SCREEN-X2 SCREEN) (SCREEN-X1 SCREEN)))
	     (Y-SIZE (- (SCREEN-Y2 SCREEN) (SCREEN-Y1 SCREEN))))
	 (CHECK-ARG MIN-X ( MIN-X X-SIZE) "in range")
	 (CHECK-ARG MIN-Y ( MIN-Y Y-SIZE) "in range")
	 (OR MAX-X (SETQ MAX-X X-SIZE))
	 (OR MAX-Y (SETQ MAX-Y Y-SIZE))
	 (LET ((LIST
		(*CATCH 'FOUND
			(DO ((EXP EXPOSED-WINDOWS (CDR EXP)))
			    (NIL)
			    (TRY-SPACE (SCREEN-X1 SCREEN) (SCREEN-Y1 SCREEN)
				       (SCREEN-X2 SCREEN) (SCREEN-Y2 SCREEN)
				       EXP)))))
	   (LET ((LEFT (FIRST LIST)) (TOP (SECOND LIST)))
	      (LET ((RIGHT (MIN (THIRD LIST) (+ LEFT MAX-X)))
		    (BOTTOM (MIN (FOURTH LIST) (+ TOP MAX-Y))))
		   (LIST LEFT TOP RIGHT BOTTOM))))))

(DEFUN TRY-SPACE (LEFT TOP RIGHT BOTTOM EXP)  ;EXP is list of windows to be left exposed
    (COND ((AND ( MIN-X (- RIGHT LEFT)) ;If it has any hope of fitting here
		( MIN-Y (- BOTTOM TOP)))
	   (DO () ((NULL EXP)	    ;See if it overlaps any window on EXP
		   (*THROW 'FOUND   ;It doesn't, we've won, return
                           (LIST LEFT TOP RIGHT BOTTOM)))
	     (COND ((<- (CAR EXP) ':OVERLAPS-P SCREEN LEFT TOP RIGHT BOTTOM)
		    (RETURN NIL))
		   (T (SETQ EXP (CDR EXP)))))
	   (LET ((REST (CDR EXP))
		 (EDGES (<- (CAR EXP) ':EDGES))) ;Get edges of window overlapping proposed area
	     ;; Try all four rectangles into which window breaks the proposed area
	     (TRY-SPACE LEFT TOP (CAR EDGES) BOTTOM REST)
	     (TRY-SPACE LEFT TOP RIGHT (SECOND EDGES) REST)
	     (TRY-SPACE (THIRD EDGES) TOP RIGHT BOTTOM REST)
	     (TRY-SPACE LEFT (FOURTH EDGES) RIGHT BOTTOM REST)))))

(DECLARE (UNSPECIAL SCREEN MIN-X MIN-Y MAX-X MAX-Y))

;; Redisplay.

;;; If WINDOW-COMPLETE-REDISPLAY is T, then make t NIL and call WINDOW-COMPLETE-REDISPLAY.
;;; Otherwise, send UPDATE messages to all exposed WINDOWs.
(DEFUN WINDOW-UPDATE ()
    (COND (WINDOW-COMPLETE-REDISPLAY
	   (WINDOW-COMPLETE-REDISPLAY))
	  (T
	   (LOCK-SCREEN-LAYOUT
	     (DO L EXPOSED-WINDOWS (CDR L) (NULL L)
	       (<- (CAR L) ':UPDATE))))))

;;; Redisplay the entire screen.
(DEFUN WINDOW-COMPLETE-REDISPLAY ()
  (LOCK-SCREEN-LAYOUT
    ;; Must lock out interrupts from before saving screens to after restoring them.
    (WITHOUT-INTERRUPTS
      (DOLIST (W EXPOSED-WINDOWS)
	(OR (<- W ':CLOBBER-SCREEN)
	    (<- W ':SAVE-SCREEN)))
      (WINDOW-RESTORE T)	      ;Restore partially visible ones, clobber exposed ones.
      (SETQ WINDOW-COMPLETE-REDISPLAY NIL)))
  (WINDOW-UPDATE))		      ;Finish updating the exposed ones.

;;; WINDOW-SAVE makes all exposed windows save their screen
;;; so that operations can be performed on the locations of windows
;;; and then WINDOW-RESTORE can be done.
(DEFUN WINDOW-SAVE (&AUX (INHIBIT-ALL-SCREEN-BLTING-FLAG NIL))
  (CHECK-LAYOUT-LOCKED)
  (DO L EXPOSED-WINDOWS (CDR L) (NULL L)
    (<- (CAR L) ':SAVE-SCREEN)))

(DEFUN WINDOW-CLEAN () (WINDOW-RESTORE))

;;; WINDOW-RESTORE copies the saved contents of all active windows
;;; back onto the screen.  Only the visible part of any window is copied.
;;; We process every screen that has active windows on it, one screen at a time.
;;; A WINDOW-SAVE should have been done first.
;;; CLOBBER-EXPOSED-WINDOWS-FLAG says to clean the exposed windows instead
;;; of restoring their old contents.  This should be followed by a (WINDOW-UPDATE).
;;; All windows which cannot regenerate themselves should have been sent :SAVE-SCREEN.
(DEFUN WINDOW-RESTORE (&OPTIONAL CLOBBER-EXPOSED-WINDOWS-FLAG &AUX SCREENS
				 (INHIBIT-ALL-SCREEN-BLTING-FLAG NIL))
  (LOCK-SCREEN-LAYOUT
    (DO L ACTIVE-WINDOWS (CDR L) (NULL L)
       (LET ((S (<- (CAR L) ':SCREEN)))
	   (COND ((MEMQ S SCREENS))
		 (T
		  (PUSH S SCREENS)
		  (TV-CLEAR-SCREEN S)
		  ;; Each exposed window on this screen gets either
		  ;; clobber-screen and clean, or restore-screen.
		  (DOLIST (W EXPOSED-WINDOWS)
		    (AND (EQ S (<- W ':SCREEN))
			 (COND ((AND CLOBBER-EXPOSED-WINDOWS-FLAG
				     (<- W ':CLOBBER-SCREEN))
				(<- W ':CLEAN))
			       (T (<- W ':RESTORE-SCREEN)))))
		  ;; Now bring back the parts of the nonexposed windows.
		  (WINDOW-RESTORE-1 S ACTIVE-WINDOWS NIL
                           (SCREEN-X1 S) (SCREEN-Y1 S)
                           (SCREEN-X2 S) (SCREEN-Y2 S))))))))

;; We operate only on unexposed (partially visible) windows in WINDOWS-LEFT.
;; If SKIP-FLAG is SKIP, we don't actually restore the
;; first window on the list.  But it does prevent any other windows from
;; being restored into its area.  We clear the rest of the area.
;; If SKIP-FLAG is CLEAR, then we clear the space
;; we have been told to restore, first thing.
;; If SKIP-FLAG is a window,
;; then we don't do anything until we pass that window in WINDOWS-LEFT;
;; that is, we operate only on the part of the screen not obscured by windows
;; that were all along higher up than that window.
;; If we find the same window again later on in the list, we can restore parts of it then;
;; otherwise, it will not be restored from at all.  This is used when burying and deactivating.
(DEFUN WINDOW-RESTORE-1 (SCREEN WINDOWS-LEFT SKIP-FLAG LEFT TOP RIGHT BOTTOM) 
   (PROG (REST) LOOP
       (SETQ REST (CDR WINDOWS-LEFT))
       (COND ((<= RIGHT LEFT))
	     ((<= BOTTOM TOP))
	     ((NULL WINDOWS-LEFT) NIL)
	     ((<- (CAR WINDOWS-LEFT) ':OVERLAPS-P SCREEN LEFT TOP RIGHT BOTTOM)
              (COND ((EQ SKIP-FLAG ':CLEAR)
                     (LET ((INHIBIT-SCHEDULING-FLAG T))
                          (TV-OPEN-SCREEN)
			  (TV-SELECT-SCREEN SCREEN)
                          (TV-ERASE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP TV-ALU-ANDCA))
                     (SETQ SKIP-FLAG NIL)))
	      (COND ((ENTITYP SKIP-FLAG)
		     (COND ((EQ SKIP-FLAG (CAR WINDOWS-LEFT))
			    (SETQ SKIP-FLAG ':CLEAR)
			    (SETQ WINDOWS-LEFT REST)
			    (GO LOOP))))
		    ((EQ SKIP-FLAG ':SKIP)
		     (SETQ SKIP-FLAG ':CLEAR))
		    ((<- (CAR WINDOWS-LEFT) ':STATUS))
		    (T (<- (CAR WINDOWS-LEFT) ':RESTORE-PARTIAL-SCREEN LEFT TOP RIGHT BOTTOM)))
	      (LET ((EDGES (<- (CAR WINDOWS-LEFT) ':EDGES)))
		;; Try all four rectangles into which window breaks the proposed area
		(WINDOW-RESTORE-1 SCREEN REST SKIP-FLAG
				  LEFT TOP RIGHT (SECOND EDGES))
		(WINDOW-RESTORE-1 SCREEN REST SKIP-FLAG
				  LEFT (FOURTH EDGES) RIGHT BOTTOM)
		;; The next two are slightly different,
		;; because if they were the same we would be covering four subrectangles twice.
		(WINDOW-RESTORE-1 SCREEN REST SKIP-FLAG
				  LEFT (MAX TOP (SECOND EDGES))
				  (FIRST EDGES) (MIN BOTTOM (FOURTH EDGES)))
		(WINDOW-RESTORE-1 SCREEN REST SKIP-FLAG
				  (THIRD EDGES) (MAX TOP (SECOND EDGES))
				  RIGHT (MIN BOTTOM (FOURTH EDGES)))))
	     (T (SETQ WINDOWS-LEFT REST) (GO LOOP)))))

;; WINDOW-RESTORE just a part of a screen.
;; WINDOW should be either NIL,
;; or a window whose territory should not be touched.
(DEFUN WINDOW-RESTORE-PARTIAL (WINDOW SCREEN LEFT TOP RIGHT BOTTOM)
    (CHECK-LAYOUT-LOCKED)
    (LET (RELEVANT-WINDOWS)
	;; Filter out irrelevant windows
	;; so that they only get tested once, not down each recursion path.
	(DOLIST (W ACTIVE-WINDOWS)
	    (AND (<- W ':OVERLAPS-P SCREEN LEFT TOP RIGHT BOTTOM)
		 (PUSH W RELEVANT-WINDOWS)))
	(SETQ RELEVANT-WINDOWS (NREVERSE RELEVANT-WINDOWS))
	(AND WINDOW (PUSH WINDOW RELEVANT-WINDOWS))
	(WINDOW-RESTORE-1 SCREEN RELEVANT-WINDOWS
			  (COND (WINDOW ':SKIP) (T ':CLEAR))
                          LEFT TOP RIGHT BOTTOM)))

;; Redisplay only the part of the screen in which a certain window
;; now shows through (intersected with another rectangle).
;; Redisplay that part of the screen as if that window were not there.
;; If the window occurs twice in ACTIVE-WINDOWS then what to display
;; is determined from the position of its first occurrence but it
;; may itself be displayed when its second occurrence is reached.
;; (burying calls this function with two occurrences).
(DEFUN WINDOW-RESTORE-UNDERNEATH (WINDOW SCREEN LEFT TOP RIGHT BOTTOM)
    (CHECK-LAYOUT-LOCKED)
    (LET (RELEVANT-WINDOWS)
	;; Filter out irrelevant windows
	;; so that they only get tested once, not down each recursion path.
	(DOLIST (W ACTIVE-WINDOWS)
	    (AND (OR (EQ W WINDOW)
		     (<- W ':OVERLAPS-P SCREEN LEFT TOP RIGHT BOTTOM))
		 (PUSH W RELEVANT-WINDOWS)))
	(SETQ RELEVANT-WINDOWS (NREVERSE RELEVANT-WINDOWS))
	(COND ((NOT (MEMQ WINDOW RELEVANT-WINDOWS))
	       (FERROR NIL "~S is not active!" WINDOW)))
	(WINDOW-RESTORE-1 SCREEN RELEVANT-WINDOWS
			  WINDOW
                          LEFT TOP RIGHT BOTTOM)))

;;; Call this before doing non-window-oriented type out or display.
;;; When finished, do a WINDOW-RESTORE (NOT a WINDOW-COMPLETE-REDISPLAY).
;;; DON'T de-expose any of the windows after doing this.
(DEFUN WINDOW-TURN-OFF ()
    (WINDOW-SAVE)
    ;This should not be necessary
    ;(SETF (PC-PPR-OUTPUT-HOLD-FLAG CONSOLE-IO-PC-PPR) 0)
    )

;;; BOXERY

;;; This function draws outlines in a specified rectangle, possibly ragged.
;;; Does not yet handle sideways and inverse-video screens.
;;; Also cannot draw ragged vertical lines yet
;;; Also ragged edges currently assumes ALU function is IOR.
(DEFUN DRAW-BOX (SCREEN LEFT TOP RIGHT BOTTOM
			LEFT-RAGGED-P TOP-RAGGED-P RIGHT-RAGGED-P BOTTOM-RAGGED-P
		 &OPTIONAL (ALU TV-ALU-IOR) TRUNCATIONS
		 &AUX C-LEFT C-TOP C-RIGHT C-BOTTOM)
  LEFT-RAGGED-P RIGHT-RAGGED-P ;ignored since we don't have vertical raggeds yet
  (LET ((INHIBIT-SCHEDULING-FLAG T))	;Since we are opening the screen down below.
    ;; Put corners of rectangle into canonical order
    (AND (< RIGHT LEFT)
	 (SETQ RIGHT (PROG1 LEFT (SETQ LEFT RIGHT))))
    (AND (< BOTTOM TOP)
	 (SETQ BOTTOM (PROG1 TOP (SETQ TOP BOTTOM))))
    ;; Compute the clipped edges of the box.  These are used as the endpoints
    ;; of the line in the coordinate parallel to the line.
    (SETQ C-LEFT (MAX LEFT (SCREEN-X1 SCREEN) (OR (FIRST TRUNCATIONS) LEFT)))
    (SETQ C-TOP (MAX TOP (SCREEN-Y1 SCREEN) (OR (SECOND TRUNCATIONS) TOP)))
    (SETQ C-RIGHT (MIN RIGHT (SCREEN-X2 SCREEN) (OR (THIRD TRUNCATIONS) RIGHT)))
    (SETQ C-BOTTOM (MIN BOTTOM (SCREEN-Y2 SCREEN) (OR (FOURTH TRUNCATIONS) BOTTOM)))
    (COND ((AND (> C-RIGHT (1+ C-LEFT))
		(> C-BOTTOM (1+ C-TOP)))		;Don't draw illegal or invisible boxes.
	   (TV-OPEN-SCREEN)
	   (TV-SELECT-SCREEN SCREEN)
	   ;; Any line which is entirely off the screen, don't draw at all.
	   ;; The line on edge mumble is off the screen if ( mumble C-mumble).
	   (COND (( TOP C-TOP))
		 ((NOT TOP-RAGGED-P)
		  (TV-ERASE (- C-RIGHT C-LEFT) 1 C-LEFT TOP ALU))
		 (T (DRAW-RAGGED-EDGE C-LEFT C-RIGHT (1+ TOP))))
	   (COND (( BOTTOM C-BOTTOM))
		 ((NOT BOTTOM-RAGGED-P)
		  (TV-ERASE (- C-RIGHT C-LEFT) 1 C-LEFT (1- BOTTOM) ALU))
		 (T (DRAW-RAGGED-EDGE C-LEFT C-RIGHT (- BOTTOM 2))))
	   (AND (= LEFT C-LEFT)
		(TV-ERASE 1 (- (- C-BOTTOM C-TOP) 2) LEFT (1+ C-TOP) ALU))
	   (AND (= RIGHT C-RIGHT)
		(TV-ERASE 1 (- (- C-BOTTOM C-TOP) 2) (1- RIGHT) (1+ C-TOP) ALU))))))

;;; Draw a jagged line on the currently-selected plane.  Assume screen is already selected.
(DEFUN DRAW-RAGGED-EDGE (LEFT RIGHT Y-POS)
  (DO ((X LEFT (1+ X))
       (Y Y-POS (COND ((BIT-TEST 2 PHASE) (1- Y)) (T (1+ Y))))
       (PHASE 1 (1+ PHASE)))
      (( X RIGHT))
    (AS-2 1 TV-BUFFER X Y)))

;; Erase the interior of a window and draw a box around it.
;; MARGIN if nonzero is how much space to leave around the window inside the box.
;; TRUNCATIONS are limits for truncation (l, r, t, b).
(DEFMETHOD (WINDOW-CLASS :DRAW-BOX) (&OPTIONAL (MARGIN 0) &REST TRUNCATIONS)
    (LET ((INHIBIT-SCHEDULING-FLAG T)
	  (BLEFT (- LEFT MARGIN)) (BTOP (- TOP MARGIN))
	  (BRIGHT (+ RIGHT MARGIN)) (BBOTTOM (+ BOTTOM MARGIN)))
      (TV-OPEN-SCREEN)
      (TV-SELECT-SCREEN SCREEN)
      (COND (TRUNCATIONS
	     (TV-ERASE-TRUNCATED TRUNCATIONS
				 (- BRIGHT BLEFT) (- BBOTTOM BTOP) BLEFT BTOP TV-ALU-ANDCA 1))
	    (T
	     (TV-ERASE (- BRIGHT BLEFT) (- BBOTTOM BTOP) BLEFT BTOP TV-ALU-ANDCA)))
      (DRAW-BOX SCREEN BLEFT BTOP BRIGHT BBOTTOM NIL NIL NIL NIL TV-ALU-IOR TRUNCATIONS)))

(DEFUN SCREEN-WINDOW-BOX (WINDOW) (<- WINDOW ':DRAW-BOX))

;Random junk for windows

;;; Return the window underneath point X,Y on SCREEN, or NIL if there is none.
(DEFUN WINDOW-UNDER-POINT (X Y &OPTIONAL (SCREEN TV-DEFAULT-SCREEN))
  (DO L ACTIVE-WINDOWS (CDR L) (NULL L)
    (AND (<- (CAR L) ':CONTAINS-POINT-P SCREEN X Y)
	 (RETURN (CAR L)))))

;; Only do this once, so the user can override it permanently.

(setq inhibit-all-screen-blting-flag nil)

;; this function initializes the screen manager and connects the
;; scheduler up to it.  (process-initialize) should already have been done.

(defun window-initialize ()
  (setq inhibit-autoexpose-flag nil)
  (cond ((or (not (boundp 'top-window))
             (null top-window))
	 (setq job-alist nil
               active-windows nil)
	 (setq screen-layout-lock (locf (car (list nil))))
	 (setq top-window (<- lisp-listener-class ':new
			      ':pc-ppr console-io-pc-ppr
			      ':name "Initial-Lisp-Listener"
			      ':process top-process))
	 (<- (<- window-single-frame-class ':new) ':pane<- top-window)))
  ;; below here is done every time the machine starts up.
  (<- top-window ':reset)
  (setq inhibit-screen-saving-flag nil
	inhibit-screen-restoration-flag nil)
  (rplacd screen-layout-lock current-process)
  (and (boundp 'exposed-windows)
       (mapc #'(lambda (window)
		 (or (eq window top-window)
		     (<- window ':save-screen)))
             exposed-windows))
  (and (boundp 'active-windows)
       (mapc (function (lambda (window)
		 (<- window ':initialize)))
	     active-windows))
  (setq selected-window nil
	exposed-windows nil)
  (<- top-window ':select)		  ;Now that top-process is getting top-window as a
  (rplacd screen-layout-lock nil)
  (<- top-process ':revoke-run-reason))	  ;run-reason, flush :user as a run-reason.

(add-initialization "WINDOW" '(window-initialize) '(SYSTEM))

(defun top-window (&optional nowait)
    (lock-screen-layout
      (<- top-window ':select)
      (<- (<- top-window ':process) ':select))
    (or nowait (process-allow-schedule)))

;;; Pop-up text windows.

;(DEFCLASS POP-UP-TEXT-WINDOW-CLASS WINDOW-WITH-PC-PPR-AND-BOX-CLASS
;	(STREAM))			; i/o stream that uses this window

;;; This is just to create a stream
(DEFMETHOD (POP-UP-TEXT-WINDOW-CLASS :BORN) ()
  (<-AS WINDOW-WITH-PC-PPR-AND-BOX-CLASS ':BORN)
  (SETQ STREAM (TV-MAKE-STREAM PC-PPR)))

;;; Expose and select the window
;;; No need to restore the bits, we are popping up a clean window
(DEFMETHOD (POP-UP-TEXT-WINDOW-CLASS :POP-UP) (&OPTIONAL (PROC-TO-SELECT CURRENT-PROCESS))
  ;; Expose self, and remember what windows we deexpose.
  (LOCK-SCREEN-LAYOUT
    (LET ((INHIBIT-SCREEN-RESTORATION-FLAG T))
      (WINDOW-EXPOSE SELF))
    (TV-HOME PC-PPR)
    (<- SELF ':PROCESS<- PROC-TO-SELECT)
    (<- SELF ':SELECT)))

;;; De-select, de-expose, de-activate the window, restore prior screen.
;; Don't save bits when we pop down, since we will regenerate them next time.
(DEFMETHOD (POP-UP-TEXT-WINDOW-CLASS :POP-DOWN) ()
  (LOCK-SCREEN-LAYOUT
    (LET ((INHIBIT-SCREEN-SAVING-FLAG T))
      (<-AS WINDOW-WITH-PC-PPR-AND-BOX-CLASS ':DEEXPOSE))
    (<-AS WINDOW-WITH-PC-PPR-AND-BOX-CLASS ':DEACTIVATE)))

;;; Define window which background processes use for terminal i/o (typically for error
;;; and trace output).

(DEFVAR BACKGROUND-TERMINAL-IO-STREAM)		;Stream that goes to background window

(LOCAL-DECLARE ((SPECIAL WINDOW))
(DEFUN CREATE-BACKGROUND-WINDOW ()		;Normally only called once
  (LET ((WINDOW (<- WINDOW-WITH-PC-PPR-CLASS ':NEW
		    ':NAME "Background processes"
		    ':LEFT (SCREEN-X1 TV-DEFAULT-SCREEN) ':RIGHT (SCREEN-X2 TV-DEFAULT-SCREEN)
		    ':TOP (// (SCREEN-Y2 TV-DEFAULT-SCREEN) 3)
		    ':BOTTOM (* 2 (// (SCREEN-Y2 TV-DEFAULT-SCREEN) 3))
		    ':FRAME T
		    ':PROCESS NIL))
	(PC-PPR)
	(STREAM))
    (SETQ PC-PPR (<- WINDOW ':PC-PPR)
	  STREAM (TV-MAKE-STREAM PC-PPR))
    (SETF (PC-PPR-OUTPUT-HOLD-FCN PC-PPR)
	  (CLOSURE '(WINDOW)
		   #'(LAMBDA (PC-PPR)		;Any attempt to type out exposes this window
		       (<- WINDOW ':PROCESS<- CURRENT-PROCESS)	;In case someone selects it
		       (LET ((FRAME (<- WINDOW ':FRAME)))
			 (<- FRAME ':LABEL<-
			     (FORMAT NIL "Background process: ~S" CURRENT-PROCESS))
			 (LET ((INHIBIT-SCREEN-RESTORATION-FLAG T)) ;Come up blank
			   (WINDOW-EXPOSE WINDOW))
			 (TV-HOME PC-PPR)	;I shouldn't have to do this
			 (TV-CLEAR-EOL PC-PPR)
			 (<- FRAME ':UPDATE-LABEL)))))	;Crock doesn't fix label itself
    (<- WINDOW ':ACTIVATE)
    (SETQ BACKGROUND-TERMINAL-IO-STREAM STREAM)))
)
