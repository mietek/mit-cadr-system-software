;;;  This file is part of ZWEI.   -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file provides functions dealing with redisplay:
;;; MUST-REDISPLAY  - Tell a window that redisplay is needed.
;;; MUNG-LINE       - Tell redisplay that a line has changed.
;;; REDISPLAY       - Update the image of a window.

;;;   Functions in this file will NOT touch any part of the window
;;; that is in the margins of the window;  the caller may do as he
;;; wishes with the margins.

;;;   This function tells a window that at least a certain amount of
;;; redisplay is needed.  The value you give will be "max'ed" into
;;; the value saved in the window.  The first argument is the window.
;;; The second argument is a keyword explaining the degree to which the
;;; parameters of the text being displayed have changed.  It should the
;;; value of one of the DIS- symbols:
;;;    DIS- code:         Meaning:
;;;      DIS-NONE           No text has changed, no bps have moved.
;;;      DIS-MARK-GOES      The state of existence of the region may have changed.
;;;      DIS-BPS            Any BPs may have moved.
;;;      DIS-LINE           See below.
;;;      DIS-TEXT           Any text may have been changed.
;;;      DIS-ALL            The window may be clobbered, don't trust information
;;;                           in the window data structure at all.
;;;   Each condition includes all of the conditions above.
;;;   When giving DIS-LINE as the argument, a third and fourth argument
;;; should also be given, and they should be a line and an index.  DIS-LINE
;;; means that text has changed, but only on that line after that index.

(DEFUN MUST-REDISPLAY (WINDOW DEGREE &OPTIONAL LINE INDEX FOO)
  (LET ((W-DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW)))
    (COND ((= DEGREE DIS-LINE)
	   (COND ((= W-DEGREE DIS-LINE)
		  (COND ((EQ (WINDOW-REDISPLAY-LINE WINDOW) LINE)
			 (SETF (WINDOW-REDISPLAY-INDEX WINDOW)
			       (MIN INDEX (WINDOW-REDISPLAY-INDEX WINDOW))))
			(T 
			  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-TEXT))))
		 ((< W-DEGREE DIS-LINE)
		  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-LINE)
		  (SETF (WINDOW-REDISPLAY-LINE WINDOW) LINE)
		  (SETF (WINDOW-REDISPLAY-INDEX WINDOW) INDEX))))
	  (T (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) (MAX W-DEGREE DEGREE)))))
  (OR FOO (MUST-REDISPLAY-OTHER-WINDOWS (WINDOW-INTERVAL WINDOW) WINDOW DEGREE LINE INDEX)))

;;; Also redisplay all other windows than WINDOW which point to INTERVAL
(DEFUN MUST-REDISPLAY-OTHER-WINDOWS (INTERVAL WINDOW DEGREE &OPTIONAL LINE INDEX)
  (DOLIST (W *WINDOW-LIST*)
    (AND (NEQ W WINDOW) (EQ INTERVAL (WINDOW-INTERVAL W))
	 (MUST-REDISPLAY W DEGREE LINE INDEX T))))

;;; Redisplay all the exposed windows associated with this editor,
;;; unless typeahead prevents, in which case T is returned.
(DEFUN REDISPLAY-ALL-WINDOWS (&OPTIONAL (FORCE-TO-COMPLETION-P NIL) (SELECT-P T))
  (COND ((AND (NOT FORCE-TO-COMPLETION-P)
	      (FUNCALL STANDARD-INPUT ':LISTEN))	;Suppress redisplay if typeahead
	 T)
        (T
	 (AND SELECT-P (FUNCALL (WINDOW-SHEET *WINDOW*) ':FINISH-DELAYED-SELECT))
	 (DOLIST (WINDOW *WINDOW-LIST*)
            (AND (WINDOW-READY-P WINDOW SELECT-P)
                 (REDISPLAY WINDOW)))
         (REDISPLAY-MODE-LINE)
	 NIL)))

;;;  This function is called to tell redisplay that the text of a given
;;; line has just changed.
(DEFUN MUNG-LINE (LINE)
  (SETF (LINE-CONTENTS-PLIST LINE) NIL)
  (SETF (LINE-TICK LINE) (TICK)))

;;;  This generates and returns a new value of TICK.
(DEFUN TICK ()
  (SETQ *TICK* (1+ *TICK*)))

;;; Set the *CENTERING-FRACTION* based on the sign of X.
(DEFUN SET-CENTERING-FRACTION (X)
  (SETQ *CENTERING-FRACTION*
	(IF (MINUSP X) *MAX-RESET-FRACTION* *MIN-RESET-FRACTION*)))

;;; This is a function for commands like C-V to call to scroll the window some
(DEFUN RECENTER-WINDOW-RELATIVE (WINDOW NLINES)
  (AND (> (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-MARK-GOES)	;If there is pending redisplay
       (NULL (PLINE-OF-POINT T WINDOW (WINDOW-POINT WINDOW)))	;and point no longer valid
       (RECENTER-WINDOW WINDOW ':ABSOLUTE))	;First correct point and start of window
  (RECENTER-WINDOW WINDOW ':RELATIVE NLINES))

;;; Change the window-start-bp of a window for the next redisplay
;;; See the function REDISPLAY for what our arguments do.
;;; Recentering-types :NONE and :POINT are not meaningful 
;;; This should ALWAYS leave point within the range of things that will be
;;; displayed according to the start-bp that we set up.
(DEFUN RECENTER-WINDOW (WINDOW RECENTER-TYPE &OPTIONAL RC1 RC2 &AUX TOP-LINE TOP-INDEX
                               POINT-PLINE POINT SHEET INTERVAL LH FIRST-BP LAST-BP DEGREE
                               N-PLINES START-BP)
  (SETQ SHEET (WINDOW-SHEET WINDOW)
	LH (TV:SHEET-LINE-HEIGHT SHEET)
	DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW)
	N-PLINES (WINDOW-N-PLINES WINDOW)
	POINT (WINDOW-POINT WINDOW)
	INTERVAL (WINDOW-INTERVAL WINDOW)
	START-BP (WINDOW-START-BP WINDOW)
	FIRST-BP (INTERVAL-FIRST-BP INTERVAL)
	LAST-BP (INTERVAL-LAST-BP INTERVAL))
  (SELECTQ RECENTER-TYPE
    (:ABSOLUTE
      (LET ((GOAL-RASTER (FIX (* (OR RC1 *CENTERING-FRACTION*)
				 (TV:SHEET-INSIDE-HEIGHT SHEET)))))
	(SETQ POINT-PLINE (// GOAL-RASTER LH))))
    (:START
      ;; The new start has been specified explicitly.
      (IF RC2
	  (SETQ TOP-LINE RC1 TOP-INDEX RC2)
	  (SETQ TOP-LINE (BP-LINE RC1) TOP-INDEX (BP-INDEX RC1)))
      (MOVE-BP START-BP TOP-LINE TOP-INDEX)
      (SETF (WINDOW-REDISPLAY-DEGREE WINDOW)
	    (SETQ DEGREE (MAX (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-TEXT)))
      (LET ((P (PLINE-OF-POINT T WINDOW POINT)))
	(COND ((NULL P)
	       (MOVE-BP POINT TOP-LINE TOP-INDEX)
	       (SETQ POINT-PLINE 0)))))
    (:RELATIVE
      ;; Move POINT by RC1 plines.
      (COND ((ZEROP RC1))
	    ((AND ( DEGREE DIS-BPS)
		  ( RC1 0)
		  (< RC1 N-PLINES)
		  (PLINE-LINE WINDOW RC1))
	     ;; What luck! No text has changed, and the goal PLINE is in the window.
	     (SETQ TOP-LINE (PLINE-LINE WINDOW RC1)
		   TOP-INDEX (PLINE-FROM-INDEX WINDOW RC1))
	     (COND ((< (WINDOW-LAST-POINT-PLINE WINDOW) RC1)
		    (MOVE-BP POINT TOP-LINE TOP-INDEX)
		    (SETQ POINT-PLINE 0))))
	    (T
	     (SETQ TOP-INDEX (WINDOW-START-BP WINDOW))
	     (SETQ TOP-LINE (BP-LINE TOP-INDEX) TOP-INDEX (BP-INDEX TOP-INDEX))
	     (MOVE-BP START-BP TOP-LINE TOP-INDEX)
	     (LET ((P (PLINE-OF-POINT NIL WINDOW POINT)))
	       (SETQ POINT-PLINE (- P RC1)))))))
  (COND (POINT-PLINE
	 (MULTIPLE-VALUE (TOP-LINE TOP-INDEX POINT-PLINE)
	   (PUT-POINT-AT-PLINE SHEET (BP-LINE POINT) (BP-INDEX POINT) POINT-PLINE
			       FIRST-BP LAST-BP))
	 ;; If recentering pushes point out the top or bottom, pull it back
	 ;; just far enough to be back inside.  Also update POINT-PLINE for how point moves.
	 (COND ((MINUSP POINT-PLINE)
		(SETQ POINT-PLINE 0)
		(MOVE-BP POINT TOP-LINE TOP-INDEX))
	       (( POINT-PLINE N-PLINES)
		(SETQ POINT-PLINE (1- N-PLINES))
		(MULTIPLE-VALUE-BIND (POINT-LINE POINT-INDEX NIL)
		    (PUT-POINT-AT-PLINE SHEET TOP-LINE TOP-INDEX (- POINT-PLINE)
					FIRST-BP LAST-BP)
		  (MOVE-BP POINT POINT-LINE POINT-INDEX))))
	 (SETF (WINDOW-LAST-POINT-PLINE WINDOW) POINT-PLINE)))
  (MOVE-BP START-BP TOP-LINE TOP-INDEX)
  (NOTIFY-SCROLL-BAR WINDOW)
  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) (MAX DEGREE DIS-TEXT)))

;;;   This function is the only entrypoint into redisplay proper.  It does
;;; anything anyone would ever need (ha ha).
;;; The first argument is the window to be redisplayed.
;;; The second argument is a keyword which says what kind of recentering
;;; is desired, and the third and fourth arguments are parameters whose meanings depend
;;; on the second argument.  Valid keywords are:
;;;   Recentering type:       Parameters:      Meaning:
;;;         :NONE             (none)           Do not recenter at all.
;;;         :POINT            raster_fraction  Keep POINT's blinker on the window.  If
;;;                                            neccesary, recenter.  Recenter in such a
;;;                                            way that the blinker ends up near the given
;;;                                            raster line.  If raster_fraction is NIL, 
;;;                                            the *CENTERING-FRACTION* is used.
;;;         :START            line, index      Recenter so that the position (line, index)
;;;                            (or a bp)       appears at the top of the window.
;;;         :RELATIVE         fixnum           Recenter to scroll window up <fixnum> lines.
;;;                                            If <fixnum> is negative, scroll down.
;;;         :ABSOLUTE         fixnum           Recenter so that point is at raster <fixnum>.

;;; The elements of a window PLINE are the:
;;; PLINE-LINE			;Editor line displayed, NIL if blank
;;; PLINE-FROM-INDEX		;First character displayed
;;; PLINE-TO-INDEX		;Last character displayed+1
;;; PLINE-TICK			;TICK as of last time pline updated on display
;;; PLINE-MARKING-LEFT		;NIL no marking, or X coord of start of region-marking
;;; PLINE-MARKING-WIDTH		;Horizontal extent of marking
;;; PLINE-TEXT-WIDTH		;Horizontal extent of text
;;; Note that for non-continuation lines, PLINE-TEXT-WIDTH includes a little
;;; extra for the pseudo-space at the end of the line which corresponds to the #\CR.
;;; But for continuation lines, it does not include the ! at the end of the line.
;;; (It does now, but that should be regarded as a bug in SHEET-LINE-OUT)
;;; PLINE-TEXT-WIDTH is used only for region marking.

(DEFUN REDISPLAY (WINDOW &OPTIONAL (RECENTER-TYPE ':POINT)
                         RC1 RC2 (FORCE-TO-COMPLETION-P NIL))
  (PREPARE-WINDOW-FOR-REDISPLAY WINDOW)
  (LET ((N-PLINES (WINDOW-N-PLINES WINDOW))
	(POINT (WINDOW-POINT WINDOW))
	(SHEET (WINDOW-SHEET WINDOW))
	(DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW))
	(POINT-BLINKER (WINDOW-POINT-BLINKER WINDOW))
	(INTERVAL (WINDOW-INTERVAL WINDOW))
	(START-BP (WINDOW-START-BP WINDOW))
	(NOW (TICK))
	POINT-PLINE)
    (PROG ABORT-REDISPLAY
	  ((LH (TV:SHEET-LINE-HEIGHT SHEET))
	   (POINT-LINE (BP-LINE POINT))
	   (POINT-INDEX (BP-INDEX POINT))
	   (TOP-LINE (BP-LINE START-BP))
	   (TOP-INDEX (BP-INDEX START-BP))
	   (LAST-BP (INTERVAL-LAST-BP INTERVAL))
	   (INITIAL-DEGREE DEGREE)
	   ;; Bind *INTERVAL* in case we decide to call any primitives, e.g. inside the
	   ;; special-blinker which blinks matching parens.  This is an implicit argument.
	   (*INTERVAL* INTERVAL))
      ;; :POINT recentering is a conditional sort of :ABSOLUTE recentering.
      ;; So decide here whether :ABSOLUTE recentering should be done.
      (AND (EQ RECENTER-TYPE ':POINT)
	   (COND (( DEGREE DIS-MARK-GOES))
		 ;; When typing at the end of the line, dont try to compute POINT-PLINE yet,
		 ;; but wait till after we have faked out the pline-text-width correctly.
		 ;; Otherwise it will be much, much slower
		 ((AND (= DEGREE DIS-LINE)
		       (EQ (WINDOW-REDISPLAY-LINE WINDOW) POINT-LINE)
		       (NEQ POINT-LINE (PLINE-LINE WINDOW (1- N-PLINES)))
		       (OR ( (1+ (WINDOW-REDISPLAY-INDEX WINDOW)) POINT-INDEX)
			   (= (MULTIPLE-VALUE-BIND (NIL Y)
				  (TV:SHEET-COMPUTE-MOTION SHEET 0 0 POINT-LINE 0 POINT-INDEX
							   T)
				Y)
			      0))))
		 ((SETQ POINT-PLINE (PLINE-OF-POINT T WINDOW POINT)))
		 (T (SETQ RECENTER-TYPE ':ABSOLUTE))))
      ;; If recentering is needed, do it, and see what changes it made.
      (COND ((MEMQ RECENTER-TYPE '(:NONE :POINT)))
	    (T (RECENTER-WINDOW WINDOW RECENTER-TYPE RC1 RC2)
	       (SETQ DEGREE (WINDOW-REDISPLAY-DEGREE WINDOW)
		     START-BP (WINDOW-START-BP WINDOW)
		     TOP-LINE (BP-LINE START-BP)
		     TOP-INDEX (BP-INDEX START-BP)
		     POINT-LINE (BP-LINE POINT)
		     POINT-INDEX (BP-INDEX POINT))
	       ;; Gobble point-pline as computed by recenter-window
	       ;; if it is accurate.
	       (SETQ POINT-PLINE (WINDOW-LAST-POINT-PLINE WINDOW))
	       (OR (AND (EQ POINT-LINE (PLINE-LINE WINDOW POINT-PLINE))
			( (PLINE-FROM-INDEX WINDOW POINT-PLINE) POINT-INDEX)
			(< POINT-INDEX (PLINE-TO-INDEX WINDOW POINT-PLINE)))
		   (SETQ POINT-PLINE NIL))))
      ;; Now we have TOP-LINE and TOP-INDEX, and possibly POINT-PLINE.

      ;; First, handle the case where just one line needs to be updated.
      (AND (= DEGREE DIS-LINE)
	   (LET ((LINE (WINDOW-REDISPLAY-LINE WINDOW))
		 (INDEX (WINDOW-REDISPLAY-INDEX WINDOW)))
	     (LET ((P (FIND-BP-IN-WINDOW WINDOW LINE INDEX))
		   (LINE-LENGTH (IF (EQ LINE (BP-LINE LAST-BP)) (BP-INDEX LAST-BP)
				    (LINE-LENGTH LINE)))
		   LEN DWID)
	       ;; LEN gets the raster position in the pline P
	       ;; of the character in LINE at position INDEX.
	       (AND P (SETQ LEN (STRING-WIDTH LINE (PLINE-FROM-INDEX WINDOW P) INDEX)))
	       (COND ((AND P
			   ;; If P and LEN say we are at the start of a continuation line,
			   ;; then maybe they are wrong
			   ;; (if the contin line has been exactly deleted).
			   (OR (NOT (ZEROP LEN))
			       (ZEROP INDEX)))
		      ;; Go to the place in the line where changes start. Clear from there.
		      ;; This means that any region marking from there on is gone now.
		      (COND ((AND (PLINE-MARKING-LEFT WINDOW P)
				  (< (PLINE-MARKING-LEFT WINDOW P) LEN))
			     (SETF (PLINE-MARKING-WIDTH WINDOW P)
				   (MIN (- LEN (PLINE-MARKING-LEFT WINDOW P))
					(PLINE-MARKING-WIDTH WINDOW P))))
			    (T (SETF (PLINE-MARKING-LEFT WINDOW P) NIL)
			       (SETF (PLINE-MARKING-WIDTH WINDOW P) NIL)))
		      ;; If the character is wider than it claims to be, draw an extra
		      ;; character, since the clear-eol will erase data.
		      (OR (ZEROP INDEX)
			  (LET ((CH (AREF LINE (1- INDEX))))
			    (AND (< CH 200)
				 (LET ((FONT (AREF (TV:SHEET-FONT-MAP SHEET)
						   (LDB %%CH-FONT CH)))
				       CWT)
				   (AND (SETQ CWT (FONT-CHAR-WIDTH-TABLE FONT))
					(LET ((CWID (AREF CWT (SETQ CH (LDB %%CH-CHAR CH))))
					      (RWID (FED:FONT-CHAR-MIN-RASTER-WIDTH FONT CH)))
					  (AND (> RWID CWID) (SETQ DWID CWID))))))))
		      (MULTIPLE-VALUE-BIND (I TW)
			  (TV:SHEET-LINE-OUT SHEET LINE INDEX LINE-LENGTH LEN (* LH P) DWID)
			;; We have output the first PLINE of this line
			(SETF (PLINE-TO-INDEX WINDOW P) I)
			(SETF (PLINE-TEXT-WIDTH WINDOW P)
			      (IF ( I LINE-LENGTH) TW	;Continuation needed
				  (+ TW (TV:SHEET-CHAR-WIDTH SHEET)))) ;Allow for CR
			(SETF (PLINE-TICK WINDOW P) NOW)
			;; See if plines below this need to be redisplayed, due
			;; to line-continuation issues
			(COND ((AND (< (1+ P) N-PLINES)
				    (OR ( I LINE-LENGTH)
					( (+ TW (TV:SHEET-INSIDE-LEFT SHEET))
					   (TV:SHEET-INSIDE-RIGHT SHEET))
					(EQ (PLINE-LINE WINDOW (1+ P)) LINE)))
			       (SETQ DEGREE DIS-TEXT POINT-PLINE NIL)
			       ;; If we are just creating a new continuation line, make it
			       ;; still look munged, so REDISPLAY-BLT can understand.
			       (OR (EQ (PLINE-LINE WINDOW (1+ P)) LINE)
				   (SETF (PLINE-TICK WINDOW P) -1))))))
		     (T
		      (SETQ DEGREE DIS-TEXT POINT-PLINE NIL))))))
	  ;; If all the window should be redisplayed, mark each pline as unknown.
	  (COND (( DEGREE DIS-ALL)
		 (DO I 0 (1+ I) (= I N-PLINES)
		     (SETF (PLINE-TICK WINDOW I) -1))))
	  (COND (( DEGREE DIS-TEXT)
		 ;; In case we abort before we are done, don't forget what's needed.
		 (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-TEXT)
		 (SETF (WINDOW-LAST-BP-DISPLAYED-P WINDOW) NIL)
		 ;; Abort now if input available
		 (AND (NOT FORCE-TO-COMPLETION-P)
		      (FUNCALL STANDARD-INPUT ':LISTEN)
		      (RETURN-FROM ABORT-REDISPLAY NIL))
		 ;; Attempt to do insert and delete line cleverness.
		 (REDISPLAY-BLT WINDOW)
		 ;; This might have invalidated the value of POINT-PLINE.
		 ;; It won't be hard to recompute, so do so.
		 (SETQ POINT-PLINE NIL)
		 ;; First loop over actual lines.
		 (DO-NAMED LINES
			   ((LINE TOP-LINE (LINE-NEXT LINE))
			    (FROM-INDEX TOP-INDEX 0)
			    (TO-INDEX)
			    (PLINE 0)
			    (STOP-LINE (BP-LINE LAST-BP)))
			   (NIL)
		   ;; Between lines, check for input available and abort if so.
		   (AND (NOT FORCE-TO-COMPLETION-P)
			(ZEROP (\ PLINE 30.))
			(FUNCALL STANDARD-INPUT ':LISTEN)
			(RETURN-FROM ABORT-REDISPLAY NIL))
		   (SETQ TO-INDEX (IF (EQ LINE STOP-LINE) (BP-INDEX LAST-BP)
				      (LINE-LENGTH LINE)))
		   ;; Now loop over the plines of this line.
		   (DO NIL (NIL)
		     (AND ( PLINE N-PLINES) (RETURN-FROM LINES))
		     ;; Check for a line that has not been changed.
		     (COND ((AND (EQ LINE (PLINE-LINE WINDOW PLINE))
				 (> (PLINE-TICK WINDOW PLINE) (LINE-TICK LINE))
				 (= (PLINE-FROM-INDEX WINDOW PLINE) FROM-INDEX))
			    (SETQ FROM-INDEX (PLINE-TO-INDEX WINDOW PLINE)))
			   (T
			     ;; This should work differently
			     (LET ((FROB (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM)) I TW)
			       (COND (FROB
				       (TV:SHEET-SET-CURSORPOS SHEET 0 (* LH PLINE))
				       (TV:SHEET-CLEAR-EOL SHEET)
				       (FUNCALL FROB ':DRAW LINE SHEET)
				       (SETQ I 1 TW 0))
				     (T
				      (MULTIPLE-VALUE (I TW)
					(TV:SHEET-LINE-OUT SHEET LINE
							   FROM-INDEX TO-INDEX
							   0 (* LH PLINE)))))
			       (SETF (PLINE-LINE WINDOW PLINE) LINE)
			       (SETF (PLINE-FROM-INDEX WINDOW PLINE) FROM-INDEX)
			       (SETF (PLINE-TO-INDEX WINDOW PLINE) I)
			       (SETF (PLINE-TICK WINDOW PLINE) NOW)
			       (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL)
			       (SETF (PLINE-TEXT-WIDTH WINDOW PLINE)
				     (IF ( I (LINE-LENGTH LINE)) TW	;Continuation needed
					 (+ TW (TV:SHEET-CHAR-WIDTH SHEET)))) ;Allow for CR
			       (SETQ FROM-INDEX I))))
		     (SETQ PLINE (1+ PLINE))
		     ;; This is >, not , because if line isn't cont'd then PLINE-TO-PLINE
		     ;; counts the phony CR which is output by SHEET-LINE-OUT.
		     (AND (> FROM-INDEX TO-INDEX) (RETURN)))
		   ;; Check for the last line in the interval.
		   (COND ((EQ LINE STOP-LINE)
			  (SETF (WINDOW-LAST-BP-DISPLAYED-P WINDOW) T)
			  (OR (< PLINE N-PLINES) (RETURN-FROM LINES))
			  (AND (NULL (PLINE-LINE WINDOW PLINE))
			       (PLINE-TICK WINDOW PLINE) (> (PLINE-TICK WINDOW PLINE) 0)
			       (RETURN-FROM LINES)) ;Return if screen already blanked
			  ;; Clean out the rest of the window beneath it.  Then exit.
			  (TV:SHEET-SET-CURSORPOS SHEET 0 (* LH PLINE))
			  (TV:SHEET-CLEAR-EOF SHEET)
			  (DO PLINE PLINE (1+ PLINE) ( PLINE N-PLINES)
			      (SETF (PLINE-LINE WINDOW PLINE) NIL)
			      (SETF (PLINE-TICK WINDOW PLINE) NOW)
			      (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL))
			  (RETURN-FROM LINES))))))
	  (COND (( DEGREE DIS-BPS)
		 ;; BPs have moved.  Reposition the POINT blinker.
		 (OR POINT-PLINE
		     (SETQ POINT-PLINE (FIND-BP-IN-WINDOW WINDOW POINT-LINE POINT-INDEX))
		     (EQ RECENTER-TYPE ':NONE)
		     (IF (AND (= INITIAL-DEGREE DIS-LINE) (= DEGREE DIS-TEXT))
			 ;;Somewhat anomalous case, try again with greater redisplay degree
			 (RETURN (REDISPLAY WINDOW RECENTER-TYPE RC1 RC2
					    FORCE-TO-COMPLETION-P))
			 (FERROR NIL "Recenter type ~S left point outside the window"
				 RECENTER-TYPE)))
		 (COND ((NULL POINT-PLINE)
			;; POINT is not on the window, so make it go away.
			(TV:BLINKER-SET-VISIBILITY POINT-BLINKER NIL))
		       (T
			;; POINT is on the window, find its Y position.
			(TV:BLINKER-SET-VISIBILITY
			  POINT-BLINKER (IF (EQ SHEET TV:SELECTED-WINDOW) ':BLINK
					    (TV:BLINKER-DESELECTED-VISIBILITY POINT-BLINKER)))
			(COND ((NOT (EQ POINT-LINE (PLINE-LINE WINDOW POINT-PLINE)))
			       (DPRINT POINT-LINE POINT-PLINE (PLINE-LINE WINDOW POINT-PLINE))
			       (FERROR NIL "Position of POINT on window is screwed up:")))
			(SET-BLINKER-SIZE POINT WINDOW POINT-BLINKER
					  (TV:SHEET-COMPUTE-MOTION SHEET 0 0 POINT-LINE
							(PLINE-FROM-INDEX WINDOW POINT-PLINE)
							POINT-INDEX)
					  (* LH POINT-PLINE) SHEET)
			(SETF (WINDOW-LAST-POINT-PLINE WINDOW) POINT-PLINE)))
		 ;; Blink the parens, etc.
		 (DOLIST (BL (WINDOW-SPECIAL-BLINKER-LIST WINDOW))
		   (FUNCALL (CAR BL) (CDR BL) WINDOW POINT START-BP))))
	  (COND (( DEGREE DIS-MARK-GOES)
		 ;; The region marking may have changed.
		 (UPDATE-REGION-MARKING WINDOW)))
	  ;;The character under the mouse also
	  (AND ( DEGREE DIS-BPS) (MOUSE-RETHINK WINDOW))
	  (AND ( DEGREE DIS-TEXT) (NOTIFY-SCROLL-BAR WINDOW))
	  (SETF (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-NONE)
	  )))

(DEFUN REDISPLAY-BLT (WINDOW)
  (PROG* REDISPLAY-BLT
	 ((START-BP (WINDOW-START-BP WINDOW))
	  (N-PLINES (WINDOW-N-PLINES WINDOW))
	  (NEW-FIRST-LINE (BP-LINE START-BP))
	  (FIRST-CHANGED-LINE NEW-FIRST-LINE) (FIRST-CHANGED-PLINE 0)
	  (SHEET (WINDOW-SHEET WINDOW))
	  LAST-OCCUPIED-PLINE
	  FIRST-UNCHANGED-PLINE FIRST-UNCHANGED-LINE
	  FIRST-UNCHANGED-LINE-NEW-PLINE)
    (COND ((AND (EQ NEW-FIRST-LINE (PLINE-LINE WINDOW 0))
		(= (BP-INDEX START-BP) (PLINE-FROM-INDEX WINDOW 0)))
	   ;; Find the first place in the window at which anything is changed.
	   ;; FIRST-CHANGED-LINE gets the NEW line that should be displayed there.
	   ;; Make sure that FIRST-CHANGED-PLINE gets the FIRST pline of that line!
	   ;; When a character is inserted into a line which is continued,
	   ;; REDISPLAY's DIS-LINE processing updates the first pline including tick
	   ;; before noticing the continuation.  Then, the continuation pline would
	   ;; be the first mismatch!
	   (DO ((PLINE 0 (1+ PLINE))
		(LINE NEW-FIRST-LINE))
	       ((OR ( PLINE N-PLINES)
		    (NEQ (PLINE-LINE WINDOW PLINE) LINE)
		    (< (PLINE-TICK WINDOW PLINE)
		       (LINE-TICK LINE))))
	     (AND (> (PLINE-TO-INDEX WINDOW PLINE)
		     (LINE-LENGTH LINE))
		  (NOT (SETQ LINE (LINE-NEXT LINE)
			     FIRST-CHANGED-PLINE (1+ PLINE)
			     FIRST-CHANGED-LINE LINE))
		  (RETURN-FROM REDISPLAY-BLT)))))

      ;; Now find the last non-null line (that used to be) in the window.
      ;; LAST-OCCUPIED-PLINE says where to find it in the window.
      (DO ((PLINE (1- N-PLINES) (1- PLINE)))
          ((PLINE-LINE WINDOW PLINE)
           (SETQ LAST-OCCUPIED-PLINE PLINE))
        (AND (ZEROP PLINE) (RETURN-FROM REDISPLAY-BLT)))
      ;; Now scan upward from there till we find a change.
      (DO ((PLINE LAST-OCCUPIED-PLINE (1- PLINE))
           (LINE (PLINE-LINE WINDOW LAST-OCCUPIED-PLINE)))
          ((OR (MINUSP PLINE)
               (NEQ (PLINE-LINE WINDOW PLINE) LINE)
               ;; Give up if we come across a deleted line.
               ;; That tells us that these lines are no longer relevant.
               (AND (EQ (LINE-TICK LINE) 'DELETED)
                    (RETURN-FROM REDISPLAY-BLT))
               (< (PLINE-TICK WINDOW PLINE)
                  (LINE-TICK LINE))))
        ;; If we have reached the line which will now occupy the first pline,
        ;; then if it will be split across the top of the window,
        ;; we must not include it in the blt.
        ;; If we are going to include it, we will exit after doing so
        ;; via the RETURN in the COND below.
        (AND (EQ LINE NEW-FIRST-LINE)
             (NOT (ZEROP (BP-INDEX START-BP)))
             (RETURN))
        ;; When we come move back past the start of a line in the window,
        ;; do so also in the interval,
        ;; and include the line we have moved over in the blt.
        ;; This way, a line which used to be split over the top of the window
        (COND ((ZEROP (PLINE-FROM-INDEX WINDOW PLINE))
               (SETQ FIRST-UNCHANGED-PLINE PLINE
                     FIRST-UNCHANGED-LINE LINE)
               (AND (EQ LINE NEW-FIRST-LINE)
                    (RETURN))
               (SETQ LINE (LINE-PREVIOUS LINE)))))
      ;; FIRST-UNCHANGED-LINE is the first line of those to be blt'ed.
      ;; But make sure that it is still in the interval, and not too far away,
      ;; before we do anything to it.  Note, maybe we passed it in the last DO.
      (DO ((I N-PLINES (1- I))
           (LINE FIRST-UNCHANGED-LINE (LINE-PREVIOUS LINE)))
          ((OR (ZEROP I) (NULL LINE)) (RETURN-FROM REDISPLAY-BLT))
        (AND (EQ LINE FIRST-CHANGED-LINE) (RETURN NIL)))
      ;; Now we know we can win, so find out where to blt FIRST-UNCHANGE-LINE to.
      (SETQ FIRST-UNCHANGED-LINE-NEW-PLINE
	    (+ FIRST-CHANGED-PLINE
	       (MULTIPLE-VALUE-BIND (NIL NIL PLINE-OFFSET)
		   (PUT-POINT-AT-PLINE SHEET FIRST-UNCHANGED-LINE 0
				       (- N-PLINES FIRST-CHANGED-PLINE)
				       (IF (ZEROP FIRST-CHANGED-PLINE) START-BP
					   (CREATE-BP FIRST-CHANGED-LINE
					     (PLINE-FROM-INDEX WINDOW FIRST-CHANGED-PLINE)))
				       NIL)
		 PLINE-OFFSET)))
      (AND ( FIRST-UNCHANGED-LINE-NEW-PLINE N-PLINES) (RETURN NIL))
      (AND (= FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE) (RETURN NIL))
      ;; If the number of lines to be preserved is less than 1/4 of the distance they move,
      ;; don't bother moving them, since it looks ugly anyway.
      (AND (< (* 4 (- N-PLINES (MAX FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)))
	      (ABS (- FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)))
           (RETURN NIL))
      ;; Now do the actual moving of text on the screen.
      (TV:SHEET-SET-CURSORPOS SHEET 0
			      (* (TV:SHEET-LINE-HEIGHT SHEET)
				 (MIN FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)))
      (IF (< FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)
	  ;; Copying upward.
	  (TV:SHEET-DELETE-LINE SHEET
				(- FIRST-UNCHANGED-PLINE FIRST-UNCHANGED-LINE-NEW-PLINE))
	  ;; Copying downward.
	  (TV:SHEET-INSERT-LINE SHEET
				(- FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE)))
      ;; Now copy the contents of the window array just as we moved the bits.
      (LET ((INC (IF (< FIRST-UNCHANGED-LINE-NEW-PLINE FIRST-UNCHANGED-PLINE) 1 -1))
	    (NEW-START FIRST-UNCHANGED-LINE-NEW-PLINE))
	(AND (MINUSP INC) (SETQ NEW-START (1- N-PLINES)))
	(DO ((NEW-PLINE NEW-START (+ INC NEW-PLINE))
	     (OLD-PLINE (+ NEW-START (- FIRST-UNCHANGED-PLINE FIRST-UNCHANGED-LINE-NEW-PLINE))
			(+ INC OLD-PLINE)))
	    ((OR (= OLD-PLINE N-PLINES)
		 (< OLD-PLINE FIRST-UNCHANGED-PLINE)))
	  (SETF (PLINE-LINE WINDOW NEW-PLINE)
		(PLINE-LINE WINDOW OLD-PLINE))
	  (SETF (PLINE-FROM-INDEX WINDOW NEW-PLINE)
		(PLINE-FROM-INDEX WINDOW OLD-PLINE))
	  (SETF (PLINE-TO-INDEX WINDOW NEW-PLINE)
		(PLINE-TO-INDEX WINDOW OLD-PLINE))
	  (SETF (PLINE-TEXT-WIDTH WINDOW NEW-PLINE)
		(PLINE-TEXT-WIDTH WINDOW OLD-PLINE))
	  (SETF (PLINE-MARKING-LEFT WINDOW NEW-PLINE)
		(PLINE-MARKING-LEFT WINDOW OLD-PLINE))
	  (SETF (PLINE-MARKING-WIDTH WINDOW NEW-PLINE)
		(PLINE-MARKING-WIDTH WINDOW OLD-PLINE))
	  (SETF (PLINE-TICK WINDOW NEW-PLINE)
		(PLINE-TICK WINDOW OLD-PLINE)))
	;; Mark as clear the lines cleared by the insert or delete.
	(COND ((MINUSP INC)
	       (DO ((PLINE FIRST-UNCHANGED-PLINE (1+ PLINE)))
		   ((= PLINE FIRST-UNCHANGED-LINE-NEW-PLINE))
		 (SETF (PLINE-LINE WINDOW PLINE) NIL)))
	      (T (DO ((PLINE (1- N-PLINES) (1- PLINE))
		      (I FIRST-UNCHANGED-LINE-NEW-PLINE (1+ I)))
		     ((= I FIRST-UNCHANGED-PLINE))
		 (SETF (PLINE-LINE WINDOW PLINE) NIL)
		 (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NIL)))))))

;;; This is an internal function of REDISPLAY.
;;; Find the PLINE on which POINT should be displayed, given the current
;;; window-start-bp.  If the PLINE is out of the window, then
;;; (if IN-BOUNDS-P, return NIL; else return the PLINE anyway).
;;; POINT can be a BP or just a line.
(DEFUN PLINE-OF-POINT (IN-BOUNDS-P WINDOW POINT &AUX (START-BP (WINDOW-START-BP WINDOW)))
  (LET (POINT-LINE POINT-INDEX
       (TOP-LINE (BP-LINE START-BP))
       (TOP-INDEX (BP-INDEX START-BP))
       (SHEET (WINDOW-SHEET WINDOW))
       POINT-PLINE)
    (COND ((LISTP POINT)
	   (SETQ POINT-LINE (BP-LINE POINT)
		 POINT-INDEX (BP-INDEX POINT)))
	  (T (SETQ POINT-LINE POINT POINT-INDEX 0)))
    (COND ((AND ( (WINDOW-REDISPLAY-DEGREE WINDOW) DIS-BPS)
		;; This clause is a short-cut, to avoid SHEET-COMPUTE-MOTION.
		;; No text has changed.  If we can find POINT in the old
		;; state of the window, then that is its current position.
		;; Otherwise: if IN-BOUNDS-P, return NIL, else try slow way.
		(OR
		 (SETQ POINT-PLINE (FIND-BP-IN-WINDOW WINDOW POINT-LINE POINT-INDEX))
		 IN-BOUNDS-P))
	   POINT-PLINE)
	  ;; Some text has changed, the existing WINDOW state is useless.
	  ;; Assume we were to redisplay with the same TOP-LINE and TOP-INDEX
	  ;; and figure out where that would put POINT.
	  ((AND (COND (IN-BOUNDS-P
		       (DO ((LINE TOP-LINE (LINE-NEXT LINE))
			    (END-LINE (BP-LINE (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW))))
			    (N-PLINES (WINDOW-N-PLINES WINDOW))
			    (I 0 (1+ I)))
			   (( I N-PLINES) NIL)
			 (AND (EQ LINE POINT-LINE) (RETURN T))
			 (AND (EQ LINE END-LINE) (RETURN NIL))))
		      (T (SEARCH-FOR-LINE POINT-LINE TOP-LINE)))
		(OR (NEQ POINT-LINE TOP-LINE)
		    ( POINT-INDEX TOP-INDEX)))
	   ;; POINT is past the top-line, top-index position.
	   (MULTIPLE-VALUE (NIL NIL POINT-PLINE)
	     (PUT-POINT-AT-PLINE SHEET POINT-LINE POINT-INDEX
				 (IF IN-BOUNDS-P (1+ (WINDOW-N-PLINES WINDOW)) 777777)
				 START-BP NIL))
	   (AND (NOT (AND IN-BOUNDS-P ( POINT-PLINE (WINDOW-N-PLINES WINDOW))))
		POINT-PLINE))
	   ;; The POINT-LINE is behind TOP-LINE.
	  (IN-BOUNDS-P NIL)
	  (T
	   ;; It's above the top and we really want to know exactly where.
	   ;; Amazingly, we can just ask to display point infinitely far before
	   ;; the place which is the top, and see where it would manage to appear!
	   (MULTIPLE-VALUE (NIL NIL POINT-PLINE)
	       (PUT-POINT-AT-PLINE SHEET
				   POINT-LINE POINT-INDEX -777777
				   NIL START-BP))
	   POINT-PLINE))))

;;; This is an internal function of REDISPLAY, PLINE-OF-POINT, UPDATE-REGION-MARKING.
;;; Search in WINDOW for BP.  BP may be given, or LINE and INDEX.
;;; Assumes that the window's display information is up to date.
(DEFUN FIND-BP-IN-WINDOW (WINDOW BP-LINE &OPTIONAL BP-INDEX)
  (COND ((NULL BP-INDEX)
	 (SETQ BP-INDEX (BP-INDEX BP-LINE) BP-LINE (BP-LINE BP-LINE))))
  (LET ((N-PLINES (WINDOW-N-PLINES WINDOW))
	(HINT (WINDOW-LAST-POINT-PLINE WINDOW)))
    (COND ((AND (EQ BP-LINE (PLINE-LINE WINDOW HINT))
		( (PLINE-FROM-INDEX WINDOW HINT) BP-INDEX)
		(< BP-INDEX (PLINE-TO-INDEX WINDOW HINT)))
	   ;; The hint from last time payed off!
	   HINT)
	  (T
	   ;; The hint didn't do it, search for the pline.
	   (DO ((PLINE 0 (1+ PLINE)))
	       (( PLINE N-PLINES)
		NIL)
	     (COND ((AND (EQ BP-LINE (PLINE-LINE WINDOW PLINE))
			 ( (PLINE-FROM-INDEX WINDOW PLINE) BP-INDEX)
			 (< BP-INDEX (PLINE-TO-INDEX WINDOW PLINE)))
		    (RETURN PLINE))))))))

;;; This is an internal function of REDISPLAY.
;;; Figures out where to start redisplay so that POINT ends up on or near POINT-PLINE.
;;; Returns a LINE and an INDEX indicating where to start redisplay, and the
;;; real value of POINT-PLINE.  Usually this third value equals the POINT-PLINE,
;;; but sometimes it will be smaller because you cannot start redisplay before
;;; the beginning of the interval (unlike MagicSix TV, may it rest in peace).
(DEFUN PUT-POINT-AT-PLINE (SHEET POINT-LINE POINT-INDEX POINT-PLINE FIRST-BP LAST-BP
				 &AUX (LH (TV:SHEET-LINE-HEIGHT SHEET)))
  (PROG KLUDGE () ;Kludge for multiple-value-return.
    (COND (( POINT-PLINE 0)
	   ;; Algorithm: first find LINE, which will be the new TOP-LINE,
	   ;; by scanning backwards.  Then knock off plines from the front
	   ;; of it until POINT ends up at POINT-PLINE.
	   ;; P is the number of plines between POINT and the beginning
	   ;; of the current LINE.
	   (DO ((LINE POINT-LINE)
		;; P is the point-pline if we start at the beginning of LINE.
		(P (MULTIPLE-VALUE-BIND (NIL FY)
		       ;; Compute which continuation line of POINT-LINE point is on.
		       (TV:SHEET-COMPUTE-MOTION SHEET 0 0 POINT-LINE
						(IF (EQ POINT-LINE (BP-LINE FIRST-BP))
						    (BP-INDEX FIRST-BP) 0)
						POINT-INDEX)
		     (// FY LH)))
		(STOP-LINE (BP-LINE FIRST-BP)))
	       (( P POINT-PLINE)
		;; We have found the new TOP-LINE.  Now find TOP-INDEX.
		(RETURN-FROM KLUDGE
		   LINE
		   (LET ((DIFFERENCE (- P POINT-PLINE)))
		     (COND ((ZEROP DIFFERENCE) 0)
			   (T ;; Compute motion to move Y down DIFFERENCE plines.
			    (MULTIPLE-VALUE-BIND (NIL NIL NC)
				(TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE
							 (IF (EQ LINE STOP-LINE)
							     (BP-INDEX FIRST-BP) 0)
							 NIL NIL
							 0 (* DIFFERENCE LH))
			      (IF (EQ LINE STOP-LINE)
				  (MAX NC (BP-INDEX FIRST-BP))
				  NC)))))
		   POINT-PLINE))
	     (COND ((EQ LINE STOP-LINE)
		    (RETURN-FROM KLUDGE LINE 0 P)))
	     (SETQ LINE (LINE-PREVIOUS LINE))
	     (SETQ P (+ P (MULTIPLE-VALUE-BIND (NIL FY)
			      ;; Compute downward motion of this line, with fake CR.
			      (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE
						       (IF (EQ LINE STOP-LINE)
							   (BP-INDEX FIRST-BP) 0)
						       NIL T 0 177777 177777)
			    (// FY LH))))))
	  (T
	   ;; POINT-PLINE is negative, do the same thing in reverse.
	   (DO ((LINE POINT-LINE)
		;; P is the point-pline if we display from beg of (LINE-NEXT LINE).
                ;; This line is the one if P is too far.
		(P (MULTIPLE-VALUE-BIND (NIL FY)
			   (TV:SHEET-COMPUTE-MOTION SHEET 0 0 POINT-LINE POINT-INDEX
						    (COND ((EQ POINT-LINE (BP-LINE LAST-BP))
							   (MIN (1+ (BP-INDEX LAST-BP))
								(LINE-LENGTH POINT-LINE)))
							  (T (LINE-LENGTH POINT-LINE)))
						    (NEQ POINT-LINE (BP-LINE LAST-BP)))
		       (- (// FY LH))))
		(STOP-LINE (BP-LINE LAST-BP)))
	       ((< P POINT-PLINE)
		;; We have found the new TOP-LINE.  Now find TOP-INDEX.
		(RETURN-FROM KLUDGE
		   LINE
		   (LET ((DIFFERENCE (- P POINT-PLINE)))
		     (COND ((= DIFFERENCE -1) 0)
			   (T (MULTIPLE-VALUE-BIND (NIL HEIGHT)
				  (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE 0 NIL T)
				(MULTIPLE-VALUE-BIND (NIL NIL NC)
				    (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE 0 NIL T
							  0 (+ HEIGHT (* DIFFERENCE LH)))
				  (IF (EQ LINE STOP-LINE)
				      (MIN NC (BP-INDEX LAST-BP))
				      NC))))))
		   POINT-PLINE))
	     (COND ((EQ LINE STOP-LINE)
		    (RETURN-FROM KLUDGE LINE 0 P)))
	     (SETQ LINE (LINE-NEXT LINE))
	     (SETQ P (- P (MULTIPLE-VALUE-BIND (NIL FY)
			      (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE 0
						       (AND (EQ LINE STOP-LINE)
							    (MIN (1+ (BP-INDEX LAST-BP))
								 (LINE-LENGTH STOP-LINE)))
						       (NEQ LINE STOP-LINE))
			    (// FY LH)))))))))

;;; This is an internal function of REDISPLAY.
(DEFUN UPDATE-REGION-MARKING (WINDOW &AUX (SHEET (WINDOW-SHEET WINDOW)) (*VSP* 2))
  (LET ((MARK-P (WINDOW-MARK-P WINDOW))
	(BP1 (WINDOW-POINT WINDOW))
	(BP2 (WINDOW-MARK WINDOW))
	(N-PLINES (WINDOW-N-PLINES WINDOW))
	(LAST-PLINE (1- (WINDOW-N-PLINES WINDOW)))
	(SHEET-INSIDE-LEFT (TV:SHEET-INSIDE-LEFT SHEET))
	(LH (TV:SHEET-LINE-HEIGHT SHEET))
	(TOP (TV:SHEET-INSIDE-TOP SHEET))
	HEIGHT OFFSET
	PLINE-1 X-1 PLINE-2 X-2)
    ;; The four variables above designate what stuff should be marked.
    (SELECTQ *REGION-MARKING-MODE*
       (:UNDERLINE
	(SETQ OFFSET (+ TOP (- LH *VSP* 1)) HEIGHT 1)) ;In the highest of the VSP lines
       (:REVERSE-VIDEO
	(SETQ OFFSET TOP HEIGHT (- LH *VSP*))))
    (COND ((> HEIGHT 0)
	   ;; That is, if marking is turned on.
	   (COND ((NOT MARK-P)
		  (REGION-UNMARK-RANGE WINDOW SHEET SHEET-INSIDE-LEFT
				       0 N-PLINES HEIGHT OFFSET LH))
		 (T
		  (LET ((LINE-1 (BP-LINE BP1))
			(INDEX-1 (BP-INDEX BP1))
			(LINE-2 (BP-LINE BP2))
			(INDEX-2 (BP-INDEX BP2))
			(LAST-LINE (PLINE-LINE WINDOW LAST-PLINE))
			P1 P2)
		    ;; Each BP may be before the window, after the window, or in the window.
		    (SETQ P1 (FIND-BP-IN-WINDOW WINDOW LINE-1 INDEX-1))
		    (SETQ P2 (FIND-BP-IN-WINDOW WINDOW LINE-2 INDEX-2))
		    ;; Hold on to your hats!  Here we effectively do a 9-way dispatch, based
		    ;; on whether each of the two bps is in, before, or after the window.
		    ;; If PLINE-n is left NIL, it and X-n will be set to zero.
		    ;; If PLINE-n is set but X-n isn't, X-n will come from
		    ;;  SHEET-COMPUTE-MOTION.
		    (COND ((NULL P1)
			   ;; Line 1 is not on the screen, which way did he go?
			   (COND ((AND LAST-LINE
				       (SEARCH-FOR-LINE LINE-1 LAST-LINE))
				  ;; Line 1 is ahead of the screen, check out Line 2.
				  (COND ((NULL P2)
					 ;; Line 2 isn't on the window either.
					 (COND ((AND LAST-LINE
						     (SEARCH-FOR-LINE LINE-2 LAST-LINE))
						;; ** They are both ahead, no display.
						)
					       (T ; ** Line 2 is behind, mark all.
						(SETQ X-2 (PLINE-TEXT-WIDTH WINDOW LAST-PLINE)
						      PLINE-2 LAST-PLINE))))
					(T ; ** Line 2 is on, Line 1 is ahead.
					 (SETQ PLINE-1 P2 LINE-1 LINE-2 INDEX-1 INDEX-2
					       PLINE-2 LAST-PLINE
					       X-2 (PLINE-TEXT-WIDTH WINDOW LAST-PLINE)))))
				 (T ;; Line 1 is behind the window, check out Line 2.
				  (COND ((NULL P2)
					 ;; Line 2 isn't on the screen either.
					 (COND ((AND LAST-LINE
						     (SEARCH-FOR-LINE LINE-2 LAST-LINE))
						;; ** Line 2 is ahead, mark all.
						;; ** Otherwise no marking.
						(SETQ X-2 (PLINE-TEXT-WIDTH WINDOW LAST-PLINE)
						      PLINE-2 LAST-PLINE))))
					(T ; ** Line 2 is on, Line 1 is behind.
					 (SETQ PLINE-2 P2))))))
			  (T ; Line 1 is on the window, check out Line 2.
			   (COND ((NULL P2)
				  ;; Line 2 is not on the window.
				  (COND ((AND LAST-LINE
					      (SEARCH-FOR-LINE LINE-2 LAST-LINE))
					 ;; ** Line 2 is ahead and Line 1 is on.
					 (SETQ PLINE-1 P1 PLINE-2 LAST-PLINE
					       X-2 (PLINE-TEXT-WIDTH WINDOW LAST-PLINE)))
					(T ; ** Line 2 is behind, Line 1 is on.
					 (SETQ PLINE-2 P1 LINE-2 LINE-1 INDEX-2 INDEX-1))))
				 (T ; ** Both are on.
				  (COND ((OR (NOT (SEARCH-FOR-LINE LINE-1 LINE-2))
					     (AND (EQ LINE-1 LINE-2)
						  (< INDEX-1 INDEX-2)))
					 ;; Line 1 is behind Line 2.
					 (SETQ PLINE-1 P1 PLINE-2 P2))
					(T
					 (SETQ LINE-1 (PROG1 LINE-2 (SETQ LINE-2 LINE-1)))
					 (SETQ INDEX-1 (PROG1 INDEX-2
							      (SETQ INDEX-2 INDEX-1)))
					 (SETQ PLINE-1 P2 PLINE-2 P1)))))))
		    (COND ((NULL PLINE-1)
			   (SETQ PLINE-1 0 X-1 0))
			  ((NULL X-1)
			   (SETQ X-1 (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE-1 0 INDEX-1))))
		    (COND ((NULL PLINE-2)
			   (SETQ PLINE-2 0 X-2 0))
			  ((NULL X-2)
			   (SETQ X-2 (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE-2 0 INDEX-2))))
		    ;; Now PLINE-1, X-1 and PLINE-2, X-2 are set up.
		    (REGION-UNMARK-RANGE WINDOW SHEET SHEET-INSIDE-LEFT
					 0 PLINE-1 HEIGHT OFFSET LH)
		    (COND ((EQ PLINE-1 PLINE-2)
			   (REGION-MARK-PLINE WINDOW SHEET SHEET-INSIDE-LEFT
					      PLINE-1 HEIGHT (+ OFFSET (* LH PLINE-1))
					      X-1 X-2))
			  (T
			   (REGION-MARK-PLINE WINDOW SHEET SHEET-INSIDE-LEFT
					      PLINE-1 HEIGHT (+ OFFSET (* LH PLINE-1))
					      X-1 (PLINE-TEXT-WIDTH WINDOW PLINE-1))
			   (DO ((P (1+ PLINE-1) (1+ P))
				(Y-POS (+ OFFSET (* LH (1+ PLINE-1))) (+ Y-POS LH)))
			       (( P PLINE-2))
			     (REGION-MARK-PLINE WINDOW SHEET SHEET-INSIDE-LEFT P HEIGHT
						Y-POS 0 (PLINE-TEXT-WIDTH WINDOW P)))
			   (REGION-MARK-PLINE WINDOW SHEET SHEET-INSIDE-LEFT
					      PLINE-2 HEIGHT (+ OFFSET (* LH PLINE-2))
					      0 X-2)))
		    (REGION-UNMARK-RANGE WINDOW SHEET SHEET-INSIDE-LEFT
					 (1+ PLINE-2) N-PLINES HEIGHT OFFSET LH))))))))

;;; This is an internal function of UPDATE-REGION-MARKING.
(DEFUN REGION-MARK-PLINE (WINDOW SHEET SHEET-INSIDE-LEFT PLINE HEIGHT Y-POS
				 NEW-LEFT NEW-RIGHT)
  (LET ((PML (PLINE-MARKING-LEFT WINDOW PLINE))
	(PMW (PLINE-MARKING-WIDTH WINDOW PLINE))
	(NEW-WIDTH (MAX 0 (- NEW-RIGHT NEW-LEFT)))) ;A negative number here would lose badly
    (COND ((NOT (AND (EQ PML NEW-LEFT)
		     (EQ PMW NEW-WIDTH)))
	   (AND PML
		(TV:%DRAW-RECTANGLE PMW HEIGHT (+ SHEET-INSIDE-LEFT PML) Y-POS
				    TV:ALU-XOR SHEET))
	   (TV:%DRAW-RECTANGLE
	     NEW-WIDTH HEIGHT (+ SHEET-INSIDE-LEFT NEW-LEFT) Y-POS TV:ALU-XOR SHEET)
	   (SETF (PLINE-MARKING-LEFT WINDOW PLINE) NEW-LEFT)
	   (SETF (PLINE-MARKING-WIDTH WINDOW PLINE) NEW-WIDTH)))))

;;; This is an internal function of UPDATE-REGION-MARKING.
(DEFUN REGION-UNMARK-RANGE (WINDOW SHEET SHEET-INSIDE-LEFT FROM-PLINE TO-PLINE HEIGHT OFFSET
				   LH)
  (DO ((P FROM-PLINE (1+ P))
       (Y-POS (+ (* FROM-PLINE LH) OFFSET) (+ Y-POS LH)))
      (( P TO-PLINE))
    (LET ((PML (PLINE-MARKING-LEFT WINDOW P)))
      (COND (PML
	     (TV:%DRAW-RECTANGLE (PLINE-MARKING-WIDTH WINDOW P) HEIGHT
				 (+ SHEET-INSIDE-LEFT PML)
				 Y-POS TV:ALU-XOR SHEET)
	     (SETF (PLINE-MARKING-LEFT WINDOW P) NIL))))))

(DEFUN REDISPLAY-MODE-LINE ()
  (FUNCALL *MODE-LINE-WINDOW* ':REDISPLAY *MODE-LINE-LIST*))

;Set the size of BLINKER to be appropriate to the char BP points at,
;assuming the font map of WINDOW;  then position it on the character
;which is assumed to be located at X Y.  Where the char is really located
;on the screen depends on the baseline of the font.
(DEFUN SET-BLINKER-SIZE (BP WINDOW BLINKER X Y SHEET &AUX CHAR FONT CHAR2)
  WINDOW					;Not used
  (SETQ CHAR (BP-CHAR BP))
  (SETQ FONT (COND (( CHAR #\CR)
		    (LDB %%CH-FONT CHAR))
		   (( (SETQ CHAR2 (BP-CHAR-BEFORE BP)) #\CR)
		    (LDB %%CH-FONT CHAR2))
		   (T
		    *FONT*)))			;At start of empty line use insert default
  (SETQ FONT (AREF (TV:SHEET-FONT-MAP SHEET) FONT)
	CHAR (LDB %%CH-CHAR CHAR))
  (COND ((= CHAR #\CR)
	 ;; At end of line, make blinker the width of a space.
	 (SETQ CHAR #\SP))
	((AND (= CHAR #\TAB) *TAB-BLINKER-FLAG*)
	 ;; Some people find blinkers over tabs annoying.
	 (SETQ CHAR #\SP)))
  ;; Set the blinker position, adjusting for the difference between
  ;; this font's baseline and other fonts' baselines.
  (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
  (TV:BLINKER-SET-CURSORPOS BLINKER X Y)		;Assure blinker overlaps char

  ;; Set the blinker size to be right for this character and font.
  (TV:SHEET-SET-CURSORPOS SHEET X Y) ;Needed to make TABs work right in SHEET-CHARACTER-WIDTH.
  (TV:BLINKER-SET-SIZE BLINKER (ABS (TV:SHEET-CHARACTER-WIDTH SHEET CHAR FONT))
		       (FONT-RASTER-HEIGHT FONT)))

;; Return position at which BP occurs in WINDOW relative to its SHEET
;; or NIL NIL if BP is not on the screen.
;; Assumes that the window's display information is up to date.
(DEFUN FIND-BP-IN-WINDOW-COORDS (BP WINDOW)
  (LET ((PLINE (FIND-BP-IN-WINDOW WINDOW BP)))
    (COND ((NULL PLINE) NIL)
	  (T
	   (LET ((SHEET (WINDOW-SHEET WINDOW)))
	     (TV:SHEET-COMPUTE-MOTION SHEET 0 (* PLINE (TV:SHEET-LINE-HEIGHT SHEET))
				      (BP-LINE BP) (PLINE-FROM-INDEX WINDOW PLINE)
				      (BP-INDEX BP)))))))

;;; Cause the matching paren to flash, START-BP is the beginning of the window, and
;;; gets passed as a magic argument to FORWARD-SEXP to tell it not to try to go past
;;; that
(DEFUN BLINK-MATCHING-PAREN (BLINKER WINDOW POINT WINDOW-START-BP &AUX BP X Y)
  (COND ((AND (CHAR-EQUAL (BP-CHAR-BEFORE POINT) #/))
	      *FLASH-MATCHING-PAREN*
	      (SETQ BP (FORWARD-SEXP POINT -1 NIL 0 WINDOW-START-BP))
	      (PROGN (MULTIPLE-VALUE (X Y) (FIND-BP-IN-WINDOW-COORDS BP WINDOW)) X))
	 (LET ((SHEET (WINDOW-SHEET WINDOW))
	       (CHAR (BP-CHAR BP)))
	   (LET ((FONT (AREF (TV:SHEET-FONT-MAP SHEET) (LDB %%CH-FONT CHAR))))
	     (SETQ Y (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
	     (WITHOUT-INTERRUPTS
	       (TV:BLINKER-SET-CHARACTER BLINKER FONT (LDB %%CH-CHAR CHAR))
	       (TV:BLINKER-SET-CURSORPOS BLINKER X Y)
	       (TV:BLINKER-SET-VISIBILITY BLINKER ':BLINK)))))
	(T (TV:BLINKER-SET-VISIBILITY BLINKER NIL))))

;; Returns the width (in pixels) of the given substring of STRING
;; when displayed on SHEET.  FROM and TO default to the beginning and
;; ending of the string.
(DEFUN STRING-WIDTH (STRING &OPTIONAL (FROM 0)
			    (TO (STRING-LENGTH STRING))
			    (SHEET (WINDOW-SHEET *WINDOW*)))
  (TV:SHEET-STRING-LENGTH SHEET STRING FROM TO NIL (AREF (TV:SHEET-FONT-MAP SHEET) *FONT*)))

;;; Put the given bp on the given pline.  NOT-IF-DISPLAYED-P means don't do anything if it
;;; is already on the screen.  This is useful when given something off the point pdl e.g.
(DEFUN REDISPLAY-POINT-ON-PLINE (BP WINDOW PLINE &OPTIONAL (NOT-IF-DISPLAYED-P T))
  (OR (AND (FIND-BP-IN-WINDOW WINDOW BP) NOT-IF-DISPLAYED-P)
      (MULTIPLE-VALUE-BIND (LINE INDEX)
	  (PUT-POINT-AT-PLINE (WINDOW-SHEET WINDOW) (BP-LINE BP) (BP-INDEX BP) PLINE
			      (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW))
			      (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW)))
	(RECENTER-WINDOW WINDOW ':START LINE INDEX))))

;;; Things dealing with windows
;;; This takes a window, and creates a new window on top of it in the default font.
(DEFVAR *OVERLYING-WINDOW-LIST* NIL)		;This buys a little speed

(DEFUN CREATE-OVERLYING-WINDOW (WINDOW &AUX (SHEET (WINDOW-SHEET WINDOW)) NEW-WINDOW)
  (IF (SETQ NEW-WINDOW (CDR (ASSQ WINDOW *OVERLYING-WINDOW-LIST*)))
      (LEXPR-FUNCALL (WINDOW-SHEET NEW-WINDOW) ':SET-EDGES
		     (MULTIPLE-VALUE-LIST (FUNCALL SHEET ':EDGES)))
      (SETQ NEW-WINDOW (CREATE-WINDOW (TYPEP SHEET) ':EDGES-FROM SHEET
				      ':IO-BUFFER (FUNCALL SHEET ':IO-BUFFER)
				      ':SUPERIOR (TV:SHEET-SUPERIOR SHEET)))
      (PUSH (CONS WINDOW NEW-WINDOW) *OVERLYING-WINDOW-LIST*))
  NEW-WINDOW)

(DEFUN CREATE-WINDOW (SHEET &REST OPTIONS &AUX NEW-SHEET-P)
  (COND ((SYMBOLP SHEET)
	 (SETQ SHEET (LEXPR-FUNCALL #'TV:WINDOW-CREATE (OR SHEET 'ZWEI-WINDOW)
				    ':ZWEI-WINDOW NIL
				    OPTIONS)
	       NEW-SHEET-P T)))
  (LET ((N-PLINES (// (TV:SHEET-INSIDE-HEIGHT SHEET) (TV:SHEET-LINE-HEIGHT SHEET)))
	(PAREN-BLINKER (TV:DEFINE-BLINKER SHEET 'TV:CHARACTER-BLINKER
					  ':VISIBILITY NIL ':HALF-PERIOD 8
					  ':DESELECTED-VISIBILITY ':OFF
					  ':FONT (TV:SHEET-CURRENT-FONT SHEET) ':CHAR #/()))
    (LET ((WINDOW (MAKE-WINDOW MAKE-ARRAY (NIL 'ART-Q (LIST *NUMBER-OF-PLINE-PARAMETERS*
							    N-PLINES))
			       WINDOW-N-PLINES N-PLINES
			       WINDOW-REDISPLAY-DEGREE DIS-ALL
			       WINDOW-LAST-POINT-PLINE 0
			       WINDOW-SHEET SHEET
			       WINDOW-POINT-BLINKER (CAR (LAST
							   (TV:SHEET-BLINKER-LIST SHEET)))
			       WINDOW-SPECIAL-BLINKER-LIST `((BLINK-MATCHING-PAREN
							       . ,PAREN-BLINKER)))))
      (AND NEW-SHEET-P (SET-IN-INSTANCE SHEET 'ZWEI-WINDOW WINDOW))
      WINDOW)))

;;; Scrollable file viewing
(DEFUN VIEW-WINDOW (ZWEI-WINDOW &OPTIONAL STREAM RETURN-IF-NO-MORE &AUX CH)
  (DO ((N-LINES (1- (WINDOW-N-PLINES ZWEI-WINDOW)))
       (FIRST-P T NIL)
       (AT-END-P))
      (NIL)
    (MULTIPLE-VALUE (AT-END-P STREAM)
      (VIEW-WINDOW-DISPLAY ZWEI-WINDOW STREAM FIRST-P))
    (AND FIRST-P RETURN-IF-NO-MORE AT-END-P (RETURN NIL))
    (SELECTQ (SETQ CH (WITHOUT-IO-BUFFER-OUTPUT-FUNCTION
			(FUNCALL STANDARD-INPUT ':TYI)))
      ((#\SP #/V #/v #\HAND-DOWN)
       (AND AT-END-P (RETURN NIL))
       (RECENTER-WINDOW-RELATIVE ZWEI-WINDOW N-LINES))
      ((#\BS #/V #/v #\HAND-UP)
       (RECENTER-WINDOW-RELATIVE ZWEI-WINDOW (- N-LINES)))
      (#\SP
       (KBD-SCROLL (WINDOW-SHEET ZWEI-WINDOW)
		   #'(LAMBDA (IGNORE N-LINES STREAM ZWEI-WINDOW)
		       (REDISPLAY ZWEI-WINDOW ':RELATIVE N-LINES)
		       (VIEW-WINDOW-DISPLAY ZWEI-WINDOW STREAM))
		   STREAM ZWEI-WINDOW))
      (OTHERWISE
       (OR (= CH #\RUBOUT)
	   (FUNCALL STANDARD-INPUT ':UNTYI CH))
       (RETURN NIL))))
  (MVRETURN (COPY-BP (WINDOW-POINT ZWEI-WINDOW)) CH))

(DEFUN VIEW-WINDOW-DISPLAY (ZWEI-WINDOW STREAM &OPTIONAL FORCE-P &AUX AT-END-P N-PLINES SHEET
								      LAST-BP PLINE X Y Y-POS)
  (SETQ N-PLINES (WINDOW-N-PLINES ZWEI-WINDOW)
	LAST-BP (INTERVAL-LAST-BP (WINDOW-INTERVAL ZWEI-WINDOW)))
  (AND STREAM (SETQ PLINE (PLINE-OF-POINT NIL ZWEI-WINDOW LAST-BP))
       (DO ((I PLINE (1+ I))
	    (LINE-SIZE (GET 'LINE 'SI:DEFSTRUCT-SIZE))
	    (AT-LINE (BP-LINE LAST-BP))
	    (LINE) (EOF))
	   (( I N-PLINES))
	 (MULTIPLE-VALUE (LINE EOF)
	   (FUNCALL STREAM ':LINE-IN LINE-SIZE))
	 (AND LINE (INSERT-LINE-WITH-LEADER LINE AT-LINE))
	 (AND EOF (RETURN (SETQ AT-END-P T
				STREAM NIL)))))
  (MUST-REDISPLAY ZWEI-WINDOW DIS-TEXT)
  (REDISPLAY ZWEI-WINDOW ':POINT NIL NIL FORCE-P)
  (OR STREAM AT-END-P
      (SETQ AT-END-P (FIND-BP-IN-WINDOW ZWEI-WINDOW LAST-BP)))
  (SETQ SHEET (WINDOW-SHEET ZWEI-WINDOW)
	Y (* N-PLINES (TV:SHEET-LINE-HEIGHT SHEET)))
  (SYS:%DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH SHEET)	;Erase anything left over
		       (MULTIPLE-VALUE-BIND (NIL HEIGHT)
			   (FUNCALL SHEET ':LABEL-SIZE)
			 (- (+ (TV:SHEET-INSIDE-BOTTOM SHEET) HEIGHT) Y))
		       (TV:SHEET-INSIDE-LEFT SHEET) Y (TV:SHEET-ERASE-ALUF SHEET) SHEET)
  (BIND (LOCF (TV:SHEET-BOTTOM-MARGIN-SIZE SHEET)) 0)
  (AND AT-END-P
       (MULTIPLE-VALUE (X Y-POS)
	 (FIND-BP-IN-WINDOW-COORDS LAST-BP ZWEI-WINDOW)))
  (COND ((OR (NOT AT-END-P) (NULL Y-POS))
	 (TV:SHEET-LINE-OUT SHEET "--More--" 0 NIL 0 Y)
	 (MULTIPLE-VALUE (X Y-POS)
	   (TV:SHEET-READ-CURSORPOS SHEET))))
  (LET ((BLINKER (WINDOW-POINT-BLINKER ZWEI-WINDOW)))
    (TV:BLINKER-SET-CURSORPOS BLINKER X Y-POS)
    (TV:BLINKER-SET-VISIBILITY BLINKER ':BLINK))
  (MVRETURN AT-END-P STREAM))

(DEFVAR *KBD-SCROLL-WAIT-TIME* (* 30. 60.))
(DEFUN KBD-SCROLL (&OPTIONAL (WINDOW TV:SELECTED-WINDOW) SCROLL-FUNCTION &REST ARGS)
  (OR SCROLL-FUNCTION
      (SETQ SCROLL-FUNCTION #'(LAMBDA (WINDOW NLINES)
				(FUNCALL WINDOW ':SCROLL-TO NLINES ':RELATIVE))))
  (DO ((N-LINES (// (* (TV:SHEET-HEIGHT WINDOW) 3) (* (TV:SHEET-LINE-HEIGHT WINDOW) 4)))
       (WAIT-P) (FULL-P) (BACKWARD-P) (FORWARD-P)
       (KBD-SHIFTS))
      (NIL)
    (SETQ KBD-SHIFTS (LOGIOR SI:KBD-LEFT-SHIFTS SI:KBD-RIGHT-SHIFTS))
    (SETQ FULL-P (LDB-TEST 0701 KBD-SHIFTS)	;Hyper
	  WAIT-P (LDB-TEST 0601 KBD-SHIFTS)	;Super
	  BACKWARD-P (LDB-TEST 0501 KBD-SHIFTS)	;Meta
	  FORWARD-P (LDB-TEST 0401 KBD-SHIFTS))	;Ctrl
    (PROCESS-WAIT "TYI" #'(LAMBDA (WINDOW START-TIME DELTA-TIME)
			    (OR (FUNCALL WINDOW ':LISTEN)
				( (TIME-DIFFERENCE (TIME) START-TIME) DELTA-TIME)))
		  WINDOW (TIME) (IF WAIT-P *KBD-SCROLL-WAIT-TIME* 1))
    (AND (FUNCALL WINDOW ':LISTEN) (RETURN NIL))
    (AND (OR FORWARD-P BACKWARD-P)
	 (LEXPR-FUNCALL SCROLL-FUNCTION WINDOW
			(* (IF BACKWARD-P -1 1) (IF FULL-P N-LINES 1)) ARGS))))

;;; Given a window, prints out its contents for debugging to STANDARD-OUTPUT.
(DEFUN PRINT-WINDOW (WINDOW)
  (DO ((PLINE 0 (1+ PLINE))
       (N-PLINES (WINDOW-N-PLINES WINDOW)))
      (( PLINE N-PLINES))
    (FORMAT T "/#~S Tick=~S  From ~S to ~S  Marking from ~S width ~S.  Text width ~S~%"
	    PLINE (PLINE-TICK WINDOW PLINE) (PLINE-FROM-INDEX WINDOW PLINE)
	    (PLINE-TO-INDEX WINDOW PLINE) (PLINE-MARKING-LEFT WINDOW PLINE)
	    (PLINE-MARKING-WIDTH WINDOW PLINE) (PLINE-TEXT-WIDTH WINDOW PLINE))
    (FORMAT T "~S~%" (PLINE-LINE WINDOW PLINE))))

;;; Verify that all lines are linked together properly
(DEFUN CHECK-INTERVAL-LINES (BP1 &OPTIONAL BP2 IN-ORDER-P TICK)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((PREV NIL LINE)
       (LINE (BP-LINE BP1) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE BP2))
       (PREV-1) (BAD-POINTERS-P) (DELETED-P) (LINE-TICK))
      (NIL)
    (SETQ PREV-1 (IF LINE (LINE-PREVIOUS LINE) LAST-LINE)
	  LINE-TICK (AND LINE (LINE-TICK LINE)))
    (SETQ BAD-POINTERS-P (NEQ PREV PREV-1)
	  DELETED-P (AND LINE-TICK (EQ LINE-TICK ':DELETED)))
    (COND ((OR BAD-POINTERS-P DELETED-P)
	   (FORMAT T "~&Line: ~S~%" LINE)
	   (AND BAD-POINTERS-P
		(FORMAT T " line previous of line ~S, previous line ~S~%" PREV-1 PREV))
	   (AND (OR DELETED-P (AND TICK LINE-TICK (> LINE-TICK TICK)))
		(FORMAT T " line is ~:[modified~;deleted~]" DELETED-P))))
    (AND (OR (EQ PREV LAST-LINE) (NULL LINE))
	 (RETURN))))

(DEFUN CHECK-BUFFER-LINES (BUFFER)
  (CHECK-INTERVAL-LINES BUFFER NIL T (AND (BUFFER-FILE-ID BUFFER) (BUFFER-TICK BUFFER))))

(DEFUN CHECK-ALL-BUFFER-LINES ()
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (FORMAT T "~2&~A:~%" (BUFFER-NAME BUFFER))
    (CHECK-BUFFER-LINES BUFFER)))

;;; Prompt line and typein line.
(DEFUN PROMPT-LINE (STRING &REST ARGS)
  (FUNCALL *MODE-LINE-WINDOW* ':CLOBBER)
  (TV:SHEET-HOME *MODE-LINE-WINDOW*)
  (TV:SHEET-CLEAR-EOL *MODE-LINE-WINDOW*)
  (LEXPR-FUNCALL #'FORMAT *MODE-LINE-WINDOW* STRING ARGS))

(DEFUN PROMPT-LINE-MORE (STRING &REST ARGS)
  (FUNCALL *MODE-LINE-WINDOW* ':CLOBBER)
  (LEXPR-FUNCALL #'FORMAT *MODE-LINE-WINDOW* STRING ARGS))

(DEFUN PROMPT-LINE-WITH-REDISPLAY (STRING &REST ARGS)
  (REDISPLAY *WINDOW* ':NONE)
  (LEXPR-FUNCALL #'PROMPT-LINE STRING ARGS))

(DEFUN TYPEIN-LINE (STRING &REST ARGS)
  (IF (TYPEP *TYPEIN-WINDOW* 'ECHO-AREA-WINDOW)
      (LET* ((MINI-BUFFER-SHEET (WINDOW-SHEET *MINI-BUFFER-WINDOW*))
	     (MINI-BUFFER-IN-USE (TV:SHEET-EXPOSED-P MINI-BUFFER-SHEET)))
	(FUNCALL *TYPEIN-WINDOW* ':EXPOSE)
	(FUNCALL *TYPEIN-WINDOW* ':CLEAR-SCREEN)
	(AND MINI-BUFFER-IN-USE (FUNCALL MINI-BUFFER-SHEET ':START-DELAYED-SELECT)))
      (FUNCALL *TYPEIN-WINDOW* ':FRESH-LINE))
  (LEXPR-FUNCALL #'FORMAT *TYPEIN-WINDOW* STRING ARGS))

(DEFUN TYPEIN-LINE-MORE (STRING &REST ARGS)
  (LEXPR-FUNCALL #'FORMAT *TYPEIN-WINDOW* STRING ARGS))

(DEFUN TYPEIN-LINE-WITH-REDISPLAY (STRING &REST ARGS)
  (AND (WINDOW-READY-P *WINDOW*)	;E.g. searching inside mini-buffer
       (REDISPLAY *WINDOW* ':NONE))
  (LEXPR-FUNCALL #'TYPEIN-LINE STRING ARGS))

;;; TYPEIN-LINE-ACTIVATE is in MACROS

(DEFUN TYPEIN-LINE-READLINE (CTL-STRING &REST ARGS &AUX INTERVAL PROMPT)
  (SETQ PROMPT (IF (NULL ARGS) CTL-STRING
		   (LEXPR-FUNCALL #'FORMAT NIL CTL-STRING ARGS)))
  (MULTIPLE-VALUE (NIL NIL INTERVAL)
    (EDIT-IN-MINI-BUFFER *MINI-BUFFER-COMTAB* NIL NIL (AND PROMPT (NCONS PROMPT))))
  (STRING-INTERVAL INTERVAL))

(DEFUN TYPEIN-LINE-READ (CTL-STRING &REST ARGS &AUX INTERVAL PROMPT)
  (SETQ PROMPT (IF (NULL ARGS) CTL-STRING
		   (LEXPR-FUNCALL #'FORMAT NIL CTL-STRING ARGS)))
  (MULTIPLE-VALUE (NIL NIL INTERVAL)
    (EDIT-IN-MINI-BUFFER *MINI-BUFFER-COMTAB* NIL NIL (AND PROMPT (NCONS PROMPT))))
  (READ (INTERVAL-STREAM INTERVAL) '*EOF*))
