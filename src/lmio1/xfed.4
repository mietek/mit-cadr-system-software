;;; -*-Mode:LISP;Package:FED-*-

(SPECIAL FED-WINDOW FED-FD-ALIST FED-WINDOW-CLASS)

(OR (BOUNDP 'FED-WINDOW)
    (SETQ FED-WINDOW NIL))

(ENDF HEAD)

(DECLARE (SPECIAL BOX-X-SIZE BOX-Y-SIZE MARGINS
            FED-CURSOR-X FED-CURSOR-Y FED-CURSOR-ON
	    WINDOW-X-POS WINDOW-Y-POS
	    WINDOW-X-SIZE WINDOW-Y-SIZE
            MIN-CHANGED-X MIN-CHANGED-Y MAX-CHANGED-X MAX-CHANGED-Y
	    CHAR-BOX-X1 CHAR-BOX-X2 CHAR-BOX-Y1 CHAR-BOX-Y2 CHAR-BOX-Y3
	    DISPLAYED-CHAR-BOX-X1 DISPLAYED-CHAR-BOX-X2
	    DISPLAYED-CHAR-BOX-Y1 DISPLAYED-CHAR-BOX-Y2 DISPLAYED-CHAR-BOX-Y3
	    TYPEOUT-STREAM CLOBBERED-P
	    WINDOW-ARRAY CHARACTER CHARACTER-ARRAY SAMPLE-STRING))

(DEFCLASS FED-WINDOW-CLASS WINDOW-WITH-PC-PPR-CLASS
	   (BOX-X-SIZE BOX-Y-SIZE
	    LEFT-MARGIN TOP-MARGIN RIGHT-MARGIN BOTTOM-MARGIN MARGINS
            FED-CURSOR-X FED-CURSOR-Y FED-CURSOR-ON
	    WINDOW-X-POS WINDOW-Y-POS
	    WINDOW-X-SIZE WINDOW-Y-SIZE
            MIN-CHANGED-X MIN-CHANGED-Y MAX-CHANGED-X MAX-CHANGED-Y
	    CHAR-BOX-X1 CHAR-BOX-X2 CHAR-BOX-Y1 CHAR-BOX-Y2 CHAR-BOX-Y3
	    DISPLAYED-CHAR-BOX-X1 DISPLAYED-CHAR-BOX-X2
	    DISPLAYED-CHAR-BOX-Y1 DISPLAYED-CHAR-BOX-Y2 DISPLAYED-CHAR-BOX-Y3
	    TYPEOUT-STREAM CLOBBERED-P
	    WINDOW-ARRAY FONT CHARACTER CHARACTER-ARRAY SAMPLE-STRING))

;This is the top level of FED.
;We create a FED WINDOW and select it, so that type-in is directed at it.
;All type in is then handled by FED-COMMAND.
(DEFUN FED (&OPTIONAL (SCREEN TV-DEFAULT-SCREEN) (WIDTH 1000) (HEIGHT 1000) &AUX FRAME)
    (OR FED-WINDOW
	(PROGN (SETQ FED-WINDOW
		     (<- FED-WINDOW-CLASS ':NEW ':SCREEN SCREEN))
	       (SETQ FRAME (<- SI:WINDOW-SINGLE-FRAME-CLASS ':NEW))
	       (<- FRAME ':FIND-SPACE WIDTH HEIGHT WIDTH HEIGHT)
	       (<- FRAME ':PANE<- FED-WINDOW)))
    (WINDOW-SELECT FED-WINDOW)
    (PROCESS-WAIT "Select" #'(LAMBDA () (EQ CURRENT-PROCESS SELECTED-PROCESS))))

;Create a FED WINDOW.
(DEFMETHOD (FED-WINDOW-CLASS :BORN) ()
    (OR SI:PROCESS (SETQ SI:PROCESS '(:NEW FED-TOP-LEVEL)))
    (SETQ FED-CURSOR-X 0 FED-CURSOR-Y 0)
    (SETQ BOX-X-SIZE 14 BOX-Y-SIZE 14)
    (SETQ WINDOW-X-SIZE 0 WINDOW-Y-SIZE 0)
    (SETQ WINDOW-X-POS 0 WINDOW-Y-POS 0)
    (SETQ MIN-CHANGED-X WINDOW-X-SIZE
          MIN-CHANGED-Y WINDOW-Y-SIZE
          MAX-CHANGED-X 0 MAX-CHANGED-Y 0)
    (OR MARGINS (SETQ MARGINS (LIST NIL NIL NIL NIL)))
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':BORN)
    (SETQ TYPEOUT-STREAM
	  (SI:MAKE-WINDOW-TYPEOUT-STREAM SELF NIL))
    (<- SELF ':WINDOW-UPDATE)
    (<- SELF ':ERASE-ALL))

(DEFUN FED-TOP-LEVEL (WINDOW)
    (DO () (()) (<- WINDOW ':COMMAND (KBD-TYI))))

(DEFMETHOD (FED-WINDOW-CLASS :ERASE-ALL) ()
    (FED-ERASE-ALL SELF T))

;; Recompute the size of the window in boxes based on everything.
;; If necessary, allocate a new window array and clean the window.
(DEFMETHOD (FED-WINDOW-CLASS :WINDOW-UPDATE) ()
    (<- SELF ':EDGES<- SI:LEFT SI:TOP SI:RIGHT SI:BOTTOM)
    (AND SI:FRAME (LEXPR-FUNCALL '<- SI:FRAME ':EDGES<- (<- SI:FRAME ':EDGES))))

(DEFMETHOD (FED-WINDOW-CLASS :EDGES<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (SETQ LEFT-MARGIN NEW-LEFT TOP-MARGIN NEW-TOP)
    ;; Round size in both directions down to multiple of box.
    ;; If size has changed, make a new window array.
    ;; Both SETQs must be done even if the value of the AND is determined by the first!
    (SETQ WINDOW-X-SIZE (// (- NEW-RIGHT NEW-LEFT) BOX-X-SIZE))
    (SETQ WINDOW-Y-SIZE (// (- NEW-BOTTOM NEW-TOP) BOX-Y-SIZE))
    (SETQ RIGHT-MARGIN (+ LEFT-MARGIN (* BOX-X-SIZE WINDOW-X-SIZE))
	  BOTTOM-MARGIN (+ TOP-MARGIN (* BOX-Y-SIZE WINDOW-Y-SIZE)))
    ;; Update the list MARGINS.
    (SETF (FIRST MARGINS) LEFT-MARGIN)
    (SETF (SECOND MARGINS) TOP-MARGIN)
    (SETF (THIRD MARGINS) RIGHT-MARGIN)
    (SETF (FOURTH MARGINS) BOTTOM-MARGIN))

(DEFMETHOD (FED-WINDOW-CLASS :CLOBBER-SCREEN) ()
  (SETQ CLOBBERED-P T))

;; When we expose, make a new WINDOW-ARRAY if our size has changed since last use.
;; In that case, clean the window.
(DEFMETHOD (FED-WINDOW-CLASS :EXPOSE) ()
    (OR (AND WINDOW-ARRAY
	     (= WINDOW-X-SIZE (ARRAY-DIMENSION-N 1 WINDOW-ARRAY))
	     (= WINDOW-Y-SIZE (ARRAY-DIMENSION-N 2 WINDOW-ARRAY)))
        (SETQ WINDOW-ARRAY (MAKE-ARRAY NIL ART-4B (LIST WINDOW-X-SIZE WINDOW-Y-SIZE))))
    (LET ((INHIBIT-SCREEN-RESTORATION-FLAG
	    (OR INHIBIT-SCREEN-RESTORATION-FLAG CLOBBERED-P)))
      (<-AS WINDOW-WITH-PC-PPR-CLASS ':EXPOSE)))

;Methods concerned with redisplaying a fed window.

(DEFMETHOD (FED-WINDOW-CLASS :LABEL-HEIGHT) ()
    (MAX (<-AS WINDOW-CLASS ':LABEL-HEIGHT)
	 (COND (FONT (FD-LINE-SPACING (FED-GET-FD FONT))) (T 0))))

(DEFMETHOD (FED-WINDOW-CLASS :PRINT-LABEL) (LABEL-PC-PPR)
    (BIND (LOCF (PC-PPR-LINE-HEIGHT LABEL-PC-PPR)) (FUNCALL SELF ':LABEL-HEIGHT))
    (COND (FONT
	    (BIND (LOCF (PC-PPR-BASELINE LABEL-PC-PPR))
		  (MAX (PC-PPR-BASELINE LABEL-PC-PPR)
		       (FD-BASELINE (FED-GET-FD FONT))))))
    ;; Now display which font and which character we are editing.
    (TV-SET-FONT LABEL-PC-PPR (SCREEN-DEFAULT-FONT SI:SCREEN))
    (TV-STRING-OUT LABEL-PC-PPR "Font: ")
    (TV-STRING-OUT LABEL-PC-PPR (GET-PNAME FONT))
    (COND (CHARACTER
	   (TV-STRING-OUT LABEL-PC-PPR "  Char: ")
	   (TV-TYO LABEL-PC-PPR (+ #/0 (LDB 0603 CHARACTER)))
	   (TV-TYO LABEL-PC-PPR (+ #/0 (LDB 0303 CHARACTER)))
	   (TV-TYO LABEL-PC-PPR (+ #/0 (LDB 0003 CHARACTER)))
	   (TV-TYO LABEL-PC-PPR #/ )
	   (COND ((= CHARACTER #/ )
		  (TV-STRING-OUT LABEL-PC-PPR "Space"))
		 (T (TV-TYO LABEL-PC-PPR CHARACTER)))
	   (TV-TYO LABEL-PC-PPR #/ )
	   (COND ((AND (BOUNDP FONT) (SYMEVAL FONT))
		  (TV-SET-FONT LABEL-PC-PPR (SYMEVAL FONT))
		  (FED-TYO LABEL-PC-PPR CHARACTER)))))
    (SI:TV-MOVE-BITPOS LABEL-PC-PPR 10 0)
    (COND ((AND SAMPLE-STRING (BOUNDP FONT) (SYMEVAL FONT))
	   (TV-SET-FONT LABEL-PC-PPR (SYMEVAL FONT))
	   (DOTIMES (I (STRING-LENGTH SAMPLE-STRING))
	      (FED-TYO LABEL-PC-PPR (AR-1 SAMPLE-STRING I))))))

(DEFMETHOD (FED-WINDOW-CLASS :UPDATE) (&AUX TEM (INHIBIT-SCHEDULING-FLAG T))
    (AND (FUNCALL TYPEOUT-STREAM ':DEACTIVATE)
	 (SETQ CLOBBERED-P T))
    (TV-OPEN-SCREEN)
    (TV-SELECT-SCREEN SI:SCREEN)
    ;; If dots or character box have changed, must reprint the label.
    (AND (OR (> MAX-CHANGED-X -1)
	     CLOBBERED-P
	     (NOT (AND (= DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
		       (= DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
		       (= DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
		       (= DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
		       (= DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3))))
	 (<- SELF ':UPDATE-LABEL))
    (COND (CLOBBERED-P
           (SETQ CLOBBERED-P NIL)
	   (TV-CLEAR-PC-PPR SI:PC-PPR)
           ;; Now add in the lines representing the edges of the character frame,
           ;; except in scale 1.
           (COND ((OR (= BOX-X-SIZE 1) (= BOX-Y-SIZE 1)))
                 (T
                  (SETQ DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
                  (SETQ DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
                  (SETQ DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
                  (SETQ DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
                  (SETQ DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3)
                  (FUNCALL SELF ':DISPLAY-CHAR-BOX)))
           ;; Now add in the grid points, unless the grid is too small.
           (OR (< BOX-X-SIZE 6) (< BOX-Y-SIZE 6)
               (DO ((I 0 (1+ I))) ((> I WINDOW-X-SIZE))
                 (DO ((J 0 (1+ J))) ((> J WINDOW-Y-SIZE))
                   (TV-ERASE-TRUNCATED MARGINS 2 2
                                       (+ (CAR MARGINS) (* BOX-X-SIZE I) -1)
                                       (+ (CADR MARGINS) (* BOX-Y-SIZE J) -1)
                                       TV-ALU-XOR))))
           ;; Every box is now clear on the screen
           (DO ((I 0 (1+ I))) ((= I WINDOW-X-SIZE))
             (DO ((J 0 (1+ J))) ((= J WINDOW-Y-SIZE))
               (AS-2 0 WINDOW-ARRAY I J)))
           ;; but every box must be checked for redisplay.
           (SETQ MIN-CHANGED-X 0 MIN-CHANGED-Y 0
                 MAX-CHANGED-X (1- WINDOW-X-SIZE)
                 MAX-CHANGED-Y (1- WINDOW-Y-SIZE))))
    ;; Take advantage of knowing that there can't be any points in nonexistent part of plane.
    (SETQ MIN-CHANGED-X (MAX MIN-CHANGED-X (- (FIRST (PLANE-ORIGIN CHARACTER-ARRAY))
                                              WINDOW-X-POS)))
    (SETQ MIN-CHANGED-Y (MAX MIN-CHANGED-Y (- (SECOND (PLANE-ORIGIN CHARACTER-ARRAY))
                                              WINDOW-Y-POS)))
    (SETQ MAX-CHANGED-X (MIN MAX-CHANGED-X (+ (FIRST (PLANE-ORIGIN CHARACTER-ARRAY))
                                              (- WINDOW-X-POS)
                                              (FIRST (ARRAY-DIMENSIONS CHARACTER-ARRAY)))))
    (SETQ MAX-CHANGED-Y (MIN MAX-CHANGED-Y (+ (SECOND (PLANE-ORIGIN CHARACTER-ARRAY))
                                              (- WINDOW-Y-POS)
                                              (SECOND (ARRAY-DIMENSIONS CHARACTER-ARRAY)))))
    ;; Now, for each box which isn't already displayed in the right state,
    ;; update it.
    (DO ((I MIN-CHANGED-X (1+ I))) ((> I MAX-CHANGED-X))
	(DO ((J MIN-CHANGED-Y (1+ J))) ((> J MAX-CHANGED-Y))
	    (COND ((= (AR-2 WINDOW-ARRAY I J)
                      (SETQ TEM (PLANE-AR-N CHARACTER-ARRAY
                                            (+ I WINDOW-X-POS)
                                            (+ J WINDOW-Y-POS)))))
                  (T
                   (TV-ERASE BOX-X-SIZE BOX-Y-SIZE
                             (+ (* I BOX-X-SIZE) (CAR MARGINS))
                             (+ (* J BOX-Y-SIZE) (CADR MARGINS))
                             TV-ALU-XOR)
                   (AS-2 TEM WINDOW-ARRAY I J)))))
    ;; If character frame lines aren't in the right place,
    ;; clear them out and redraw them in the right place.
    (COND ((OR (= BOX-X-SIZE 1) (= BOX-Y-SIZE 1)))
          ((AND (= DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
		(= DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
		(= DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
		(= DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
		(= DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3)))
	  (T
           (FUNCALL SELF ':DISPLAY-CHAR-BOX)
	   (SETQ DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
	   (SETQ DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
	   (SETQ DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
	   (SETQ DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
	   (SETQ DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3)
	   (FUNCALL SELF ':DISPLAY-CHAR-BOX)))
    ;; Say that the range of boxes needing consideration for redisplay is now empty.
    (SETQ MIN-CHANGED-X WINDOW-X-SIZE
          MIN-CHANGED-Y WINDOW-Y-SIZE
          MAX-CHANGED-X -1 MAX-CHANGED-Y -1)
    ;; If we are supposed to display the cursor, do so.
    (COND (FED-CURSOR-ON
           (TV-SET-BLINKER-CURSORPOS MOUSE-BLINKER
                     (+ (CAR MARGINS)
                        (* BOX-X-SIZE FED-CURSOR-X))
                     (+ (CADR MARGINS)
                        (* BOX-Y-SIZE FED-CURSOR-Y)))
           (TV-SET-BLINKER-VISIBILITY MOUSE-BLINKER 'BLINK))))

;Subroutines of redisplaying a fed window.

;Either write or remove (xor) the five lines displaying the character box and baseline
;at lattice (not dot) positions specified by DISPLAYED-CHAR-BOX-X1, etc.
;This is a method so that the class variables will be locally special within,
;when that gets implemented.
(DEFMETHOD (FED-WINDOW-CLASS :DISPLAY-CHAR-BOX) (&AUX X1 Y1 X2 Y2 Y3)
       (SETQ X1 (+ LEFT-MARGIN (* BOX-X-SIZE (- DISPLAYED-CHAR-BOX-X1 WINDOW-X-POS)) -1))
       (SETQ Y1 (+ TOP-MARGIN (* BOX-Y-SIZE (- DISPLAYED-CHAR-BOX-Y1 WINDOW-Y-POS)) -1))
       (SETQ X2 (+ LEFT-MARGIN (* BOX-X-SIZE (- DISPLAYED-CHAR-BOX-X2 WINDOW-X-POS)) -1))
       (SETQ Y2 (+ TOP-MARGIN (* BOX-Y-SIZE (- DISPLAYED-CHAR-BOX-Y2 WINDOW-Y-POS)) -1))
       (SETQ Y3 (+ TOP-MARGIN (* BOX-Y-SIZE (- DISPLAYED-CHAR-BOX-Y3 WINDOW-Y-POS)) -1))
       (TV-ERASE-TRUNCATED MARGINS
	   2 (- Y2 Y1) X1 Y1
	   TV-ALU-XOR)
       (COND ((= X1 X2))
	     (T
	      (TV-ERASE-TRUNCATED MARGINS
		  (- X2 X1) 2 (+ 2 X1) Y1
		  TV-ALU-XOR)
	      (TV-ERASE-TRUNCATED MARGINS
		  2 (- Y2 Y1) X2 (+ 2 Y1)
		  TV-ALU-XOR)
	      (TV-ERASE-TRUNCATED MARGINS
		  (- X2 X1) 2 X1 Y2
		  TV-ALU-XOR)
	      (OR (= Y2 Y3)
		  (TV-ERASE-TRUNCATED MARGINS 
		      (- X2 -2 X1) 2 X1 Y3
		      TV-ALU-XOR)))))

;; Print a character on pc-ppr, assuming that pc-ppr is set up to the
;; font being edited.  If the character is the one being edited,
;; the picture being edited is displayed.
(DEFUN FED-TYO (PC-PPR CH)
    (LOCAL-DECLARE ((SPECIAL CHARACTER CHARACTER-ARRAY CHAR-BOX-X1 CHAR-BOX-X2 CHAR-BOX-Y1))
      (COND ((AND CHARACTER (= CH CHARACTER))
	     (LET (;; Offset from horiz idx in plane to hpos of dot on screen.
		   (LEFT (+ (- (PC-PPR-CURRENT-X PC-PPR) CHAR-BOX-X1)
                            (FIRST (PLANE-ORIGIN CHARACTER-ARRAY))))
		   ;; Offset from vert idx in plane to vpos of dot on screen.
                   (TOP (+ (- (PC-PPR-CURRENT-Y PC-PPR) CHAR-BOX-Y2)
                           (PC-PPR-BASELINE PC-PPR)
                           (SECOND (PLANE-ORIGIN CHARACTER-ARRAY))))
                   (PLANE-WIDTH (FIRST (ARRAY-DIMENSIONS CHARACTER-ARRAY)))
		   ;; First vertical idx to print from in plane.
		   (PLANE-TOP (MAX 0 (- CHAR-BOX-Y1
					(SECOND (PLANE-ORIGIN CHARACTER-ARRAY)))))
		   ;; Last+1 vertical idx to print from in plane.
                   (PLANE-BOTTOM (MIN (SECOND (ARRAY-DIMENSIONS CHARACTER-ARRAY))
				      (- CHAR-BOX-Y3
					 (SECOND (PLANE-ORIGIN CHARACTER-ARRAY))))))
               (DOTIMES (HPOS PLANE-WIDTH)
                 (DO ((VPOS PLANE-TOP (1+ VPOS)))
                     ((>= VPOS PLANE-BOTTOM))
                   (OR (ZEROP (AR-2 CHARACTER-ARRAY HPOS VPOS))
                       (TV-ERASE 1 1 (+ HPOS LEFT) (+ VPOS TOP) TV-ALU-IOR))))
               (SI:TV-MOVE-BITPOS PC-PPR (- CHAR-BOX-X2 CHAR-BOX-X1) 0)))
            (T (TV-TYO PC-PPR CH)))))

;This function processes one command for a FED WINDOW.
;The "trivial" process associated with the FED window loops reading a
;character and handing it with a COMMAND command to the window,
;and that calls this function.
;Digits are accumulated as an argument for the next command.
;Not all commands use such an arg, but all non-digits flush any arg.
(DEFMETHOD (FED-WINDOW-CLASS :COMMAND) (COMMAND &AUX
						(STANDARD-OUTPUT TYPEOUT-STREAM)
						(STANDARD-INPUT TYPEOUT-STREAM))
  (PROG ((ARG 1) ARG-P)
    LOOP
    (COND ((AND (>= COMMAND #/0) (<= COMMAND #/9))
           (SETQ ARG (+ COMMAND -60 (* 10. (COND (ARG-P ARG) (T 0)))))
           (SETQ ARG-P T)
           (SETQ COMMAND (KBD-TYI))
           (GO LOOP)))
    (COND ((NOT (ZEROP (LDB %%KBD-MOUSE COMMAND)))
	   (SETQ FED-CURSOR-ON NIL)
	   (SELECTQ (LOGAND 77 COMMAND)
	      (0 (FED-MOUSE-MARK-SQUARES T))
	      (1 (FED-MOUSE-MOVE-CHAR-BOX))
	      (2 (FED-MOUSE-MARK-SQUARES NIL))
	      (OTHERWISE (TV-BEEP))))
          (T (SELECTQ (CHAR-UPCASE (LDB %%KBD-CHAR COMMAND))
                ((#/ #/ #/ 13)
                 (FED-SHIFT-WINDOW COMMAND ARG-P ARG))
                ((#/[ #/] #/\ #// )
                 (FED-SHIFT-CURSOR COMMAND ARG-P ARG))
		((0 #/ ) NIL)          	;0 is used to cause a redisplay!
                (#/H (FED-HOME))
                (#/@ (FED-SCALE ARG-P ARG))
                (#/F (FED-SPECIFY-FONT))
                (#/C (FED-SPECIFY-CHARACTER COMMAND))
		(#/M (FED-MERGE-CHARACTER COMMAND))
                (#/S (FED-SAVE-CHARACTER))
		(#/Z (FED-ERASE-REGION))
		(#/E (FED-ERASE-ALL SELF))
		(#/P (FED-SET-FONT-PARAMETERS))
                (#/B (BREAK FED T))
                (#/X (FED-SET-X ARG))
                (#/Y (FED-SET-Y ARG))
		(#/D (FED-DISPLAY-FONT))
		(#/V (FED-SET-SAMPLE))
		(#/ (FED-REFLECT-COMMAND ARG))
		(15 ;Circle-plus
		  (FED-ROTATE-CHARACTER-RIGHT))
                (#/R (FED-READ-KST-FILE FONT))
                (#/W (FED-WRITE-KST-FILE FONT))
                (#/. (COND (FED-CURSOR-ON
			     (FED-ALTER-SQUARE T FED-CURSOR-X FED-CURSOR-Y))
			    (T (TV-BEEP))))
                (#/, (COND (FED-CURSOR-ON
			     (FED-ALTER-SQUARE NIL FED-CURSOR-X FED-CURSOR-Y))
			    (T (TV-BEEP))))
		((#/? 206) (FED-HELP))
                (214 (<- SELECTED-WINDOW ':CLEAN)  ;Not self, but our frame instead.
		     (<- SELF ':CLOBBER-SCREEN))
		(OTHERWISE (TV-BEEP)))))
    (COND ((FUNCALL TYPEOUT-STREAM ':INCOMPLETE-P)
	   ;; If dots or character box have changed, must reprint the label.
	   (AND (OR (> MAX-CHANGED-X -1)
		    CLOBBERED-P
		    (NOT (AND (= DISPLAYED-CHAR-BOX-X1 CHAR-BOX-X1)
			      (= DISPLAYED-CHAR-BOX-X2 CHAR-BOX-X2)
			      (= DISPLAYED-CHAR-BOX-Y1 CHAR-BOX-Y1)
			      (= DISPLAYED-CHAR-BOX-Y2 CHAR-BOX-Y2)
			      (= DISPLAYED-CHAR-BOX-Y3 CHAR-BOX-Y3))))
		(<- SELF ':UPDATE-LABEL))
	   (LET ((NEXTCH (FUNCALL TYPEOUT-STREAM ':TYI)))
	     (FUNCALL TYPEOUT-STREAM ':MAKE-COMPLETE)
	     (COND ((NOT (= NEXTCH #/ ))
		    (FUNCALL SELF ':COMMAND NEXTCH))))))
    (OR (KBD-CHAR-AVAILABLE) (<- SELF ':UPDATE))))

(DEFUN FED-SET-SAMPLE ()
    (LOCAL-DECLARE ((SPECIAL FONT SAMPLE-STRING))
      (FORMAT T "~%String to display in ~A: " FONT)
      (SETQ SAMPLE-STRING (READLINE))
      (FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
      (AND (ZEROP (STRING-LENGTH SAMPLE-STRING)) (SETQ SAMPLE-STRING NIL))))

(DEFUN FED-HELP ()
    (PRINC "Mouse-Left - set square   Mouse-Right - clear square
Mouse-Middle - move edge of character box
F - select Font   C - select Character
S - Store back edited character   E - Erase all dots
Z - erase (Zap) connected region where the cursor is
R - Read KST file   W - Write KST file
P - set font Parameters   M - Merge in character
X - set X position of non-mouse cursor   Y - set Y
 - reflect character   015 - rotate character
[, ], \, // - move non-mouse cursor
. - set dot under non-mouse cursor   , - clear it
, , 013,  - move window   H - move window to Home
@ - set scale (size of box) to numeric arg
D - Display entire font   V - set sample string
[, ], \, //, , , 013,  take numeric arg or meta bits
"))

;Alter the square which the mouse is on.
(DEFUN FED-ALTER-SQUARE (SETP &OPTIONAL X Y)
    (LOCAL-DECLARE ((SPECIAL LEFT-MARGIN TOP-MARGIN CHARACTER-ARRAY
                             BOX-X-SIZE BOX-Y-SIZE WINDOW-X-POS WINDOW-Y-POS
                             MIN-CHANGED-X MIN-CHANGED-Y MAX-CHANGED-X MAX-CHANGED-Y))
      (OR X (SETQ X (// (- MOUSE-X LEFT-MARGIN) BOX-X-SIZE)
                  Y (// (- MOUSE-Y TOP-MARGIN) BOX-Y-SIZE)))
      (SETQ X (+ WINDOW-X-POS X) Y (+ WINDOW-Y-POS Y))
      (PLANE-AS-N (COND (SETP 1) (T 0)) CHARACTER-ARRAY X Y)
      (SETQ MIN-CHANGED-X (MIN MIN-CHANGED-X (- X WINDOW-X-POS)))
      (SETQ MIN-CHANGED-Y (MIN MIN-CHANGED-Y (- Y WINDOW-Y-POS)))
      (SETQ MAX-CHANGED-X (MAX MAX-CHANGED-X (- X WINDOW-X-POS)))
      (SETQ MAX-CHANGED-Y (MAX MAX-CHANGED-Y (- Y WINDOW-Y-POS)))))

;Erase a region connected by vertical or horizontal (but not diagonal) adjacency.

(DEFUN FED-ERASE-REGION (&OPTIONAL X Y)
  (LOCAL-DECLARE ((SPECIAL CHARACTER-ARRAY LEFT-MARGIN TOP-MARGIN BOX-X-SIZE BOX-Y-SIZE))
      (OR X (SETQ X (// (- MOUSE-X LEFT-MARGIN) BOX-X-SIZE)
                  Y (// (- MOUSE-Y TOP-MARGIN) BOX-Y-SIZE)))
      (SETQ X (+ WINDOW-X-POS X) Y (+ WINDOW-Y-POS Y))
      (FED-ERASE-REGION-1 X Y)))

(DEFUN FED-ERASE-REGION-1 (X Y)
  (LOCAL-DECLARE ((SPECIAL CHARACTER-ARRAY MIN-CHANGED-X MIN-CHANGED-Y
			   MAX-CHANGED-X MAX-CHANGED-Y))
    (COND ((NOT (ZEROP (PLANE-AR-N CHARACTER-ARRAY X Y)))
	   (PLANE-AS-N 0 CHARACTER-ARRAY X Y)
	   (SETQ MIN-CHANGED-X (MIN MIN-CHANGED-X (- X WINDOW-X-POS)))
	   (SETQ MIN-CHANGED-Y (MIN MIN-CHANGED-Y (- Y WINDOW-Y-POS)))
	   (SETQ MAX-CHANGED-X (MAX MAX-CHANGED-X (- X WINDOW-X-POS)))
	   (SETQ MAX-CHANGED-Y (MAX MAX-CHANGED-Y (- Y WINDOW-Y-POS)))
	   (FED-ERASE-REGION-1 (1- X) Y)
	   (FED-ERASE-REGION-1 (1+ X) Y)
	   (FED-ERASE-REGION-1 X (1- Y))
	   (FED-ERASE-REGION-1 X (1+ Y))))))	   


(DEFUN FED-ERASE-ALL (IGNORE &OPTIONAL DONT-ASK-FLAG &AUX FD)
    (LOCAL-DECLARE ((SPECIAL FONT CHARACTER-ARRAY TYPEOUT-STREAM
                             CHAR-BOX-X1 CHAR-BOX-X2 CHAR-BOX-X3
                             CHAR-BOX-Y1 CHAR-BOX-Y2))
      (COND ((OR DONT-ASK-FLAG
		 (PROG1 (Y-OR-N-P "Erase all these dots? " TYPEOUT-STREAM)
			(FUNCALL TYPEOUT-STREAM ':MAKE-COMPLETE)))
             (SETQ CHARACTER-ARRAY (MAKE-PLANE ART-4B 2 0 10))
             (SETQ CHAR-BOX-X1 0 CHAR-BOX-Y1 0
                   CHAR-BOX-X2 7 CHAR-BOX-Y2 11 CHAR-BOX-Y3 14)
             (COND (FONT
                    (SETQ FD (FED-GET-FD FONT))
                    (SETQ CHAR-BOX-Y2 (FD-BASELINE FD)
                          CHAR-BOX-X2 (FD-SPACE-WIDTH FD)
                          CHAR-BOX-Y3 (FD-LINE-SPACING FD))))
             (FED-HOME)))))

;; Display all of the characters of the font being edited, to show what they look like.
;; Above each one is the corresponding character of CPTFONT, so you
;; can see which character is which in non-alphabetic fonts.
(DEFUN FED-DISPLAY-FONT ()
    (LOCAL-DECLARE ((SPECIAL FONT TYPEOUT-STREAM SI:SCREEN CHARACTER))
      (COND ((AND (BOUNDP FONT) (SYMEVAL FONT))
             (LET ((PP (FUNCALL TYPEOUT-STREAM ':PC-PPR))
                   (DF (SCREEN-DEFAULT-FONT SI:SCREEN)))
	       (FUNCALL TYPEOUT-STREAM ':CLEAR-SCREEN)
               (FORMAT TYPEOUT-STREAM "Font ~A:~%" FONT)
               (DO ((CH 0) (OCH)) ((= CH 128.))
                 (TV-CRLF PP)
                 (SETQ OCH CH)
                 ;; Output one line of chars in the default font,
                 ;; spaced so that they lie above the corresponding chars in the next line.
                 ;; Stop at margin, or when we reach a char code that's a multiple of 32.
                 (DO ()
                     ((> (+ (PC-PPR-CURRENT-X PP) (FED-CHAR-WIDTH (SYMEVAL FONT) CH))
                         (PC-PPR-RIGHT-MARGIN PP)))
                   (COND ((OR (AND (AR-1 (FED-GET-FD FONT) CH)
				   (NOT (ZEROP (FED-CHAR-WIDTH (SYMEVAL FONT) CH))))
			      (AND CHARACTER (= CH CHARACTER)))
                          (TV-TYO PP CH)
                          (SI:TV-MOVE-BITPOS PP
                                             (- (MAX (FED-CHAR-WIDTH (SYMEVAL FONT) CH)
                                                     (FED-CHAR-WIDTH DF CH))
                                                (FED-CHAR-WIDTH DF CH))
                                             0)))
                   (SETQ CH (1+ CH))
                   (AND (ZEROP (\ CH 32.)) (RETURN)))
                 (TV-CRLF PP)
                 ;; Clear out what we will move down over with TV-MOVE-BITPOS.
                 (TV-ERASE (- (PC-PPR-RIGHT-MARGIN PP) (PC-PPR-LEFT-MARGIN PP))
                           (FONT-CHAR-HEIGHT (SYMEVAL FONT))
                           (PC-PPR-LEFT-MARGIN PP)
                           (+ (PC-PPR-CURRENT-Y PP) (PC-PPR-LINE-HEIGHT PP))
                           TV-ALU-ANDCA)
                 ;; Now output the corresponding chars in the font being edited.
                 ;; First leave space so it won't overlap if font is taller.
                 (SI:TV-MOVE-BITPOS PP 0 (- (FONT-BASELINE (SYMEVAL FONT))
                                            (PC-PPR-BASELINE PP)))
                 (TV-SET-FONT PP (SYMEVAL FONT))
                 (DO ()
                     ((> (+ (PC-PPR-CURRENT-X PP) (FED-CHAR-WIDTH (SYMEVAL FONT) OCH))
                         (PC-PPR-RIGHT-MARGIN PP)))
                   (COND ((OR (AND (AR-1 (FED-GET-FD FONT) OCH)
				   (NOT (ZEROP (FED-CHAR-WIDTH (SYMEVAL FONT) OCH))))
			      (EQ CH CHARACTER))
                          (FED-TYO PP OCH)
                          (SI:TV-MOVE-BITPOS PP
                                             (- (MAX (FED-CHAR-WIDTH (SYMEVAL FONT) OCH)
                                                     (FED-CHAR-WIDTH DF OCH))
                                                (FED-CHAR-WIDTH (SYMEVAL FONT) OCH))
                                             0)))
                   (SETQ OCH (1+ OCH))
                   (AND (ZEROP (\ OCH 32.)) (RETURN)))
                 (TV-SET-FONT PP DF)
                 ;; Move down, leaving space for font's descenders.
                 (SI:TV-MOVE-BITPOS PP 0 (- (FONT-CHAR-HEIGHT (SYMEVAL FONT))
                                            (- (FONT-BASELINE (SYMEVAL FONT))
                                               (PC-PPR-BASELINE PP)))))
               (SETF (PC-PPR-CURRENT-X PP) (PC-PPR-LEFT-MARGIN PP))))
            (T (TV-BEEP)))))

;; Return the width of a given char in a given font.
(DEFUN FED-CHAR-WIDTH (FONT CHAR)
    (LET ((CWT (FONT-CHAR-WIDTH-TABLE FONT)))
      (COND (CWT (AR-1 CWT CHAR))
	    (T (FONT-CHAR-WIDTH FONT)))))

;Set the position of the cursor, which is used as an alternate to the mouse
;for complementing squares.  Also say that the cursor ought to be displayed.
(DEFUN FED-SET-X (XPOS)
    (COND ((OR (< XPOS 0) (>= XPOS WINDOW-X-SIZE))
           (TV-BEEP)))
    (SETQ FED-CURSOR-X (MAX 0 (MIN (1- WINDOW-X-SIZE) XPOS))))

(DEFUN FED-SET-Y (YPOS)
    (COND ((OR (< YPOS 0) (>= YPOS WINDOW-Y-SIZE))
           (TV-BEEP)))
    (SETQ FED-CURSOR-Y (MAX 0 (MIN (1- WINDOW-Y-SIZE) YPOS))))

(DEFUN FED-SHIFT-CURSOR (COMMAND ARG-P DISTANCE &AUX DX DY ARROW)
    (OR ARG-P (SETQ DISTANCE (LSH 1 (LDB %%KBD-CONTROL-META COMMAND))))
    (SETQ ARROW (LDB %%KBD-CHAR COMMAND))
    (SETQ DX (* DISTANCE (OR (CADR (ASSQ ARROW '((#/[ -1) (#/] 1)))) 0)))
    (SETQ DY (* DISTANCE (OR (CADR (ASSQ ARROW '((#/\ -1) (#// 1)))) 0)))
    (FED-SET-X (+ FED-CURSOR-X DX))
    (FED-SET-Y (+ FED-CURSOR-Y DY))
    (SETQ FED-CURSOR-ON T))

(DEFUN FED-SHIFT-WINDOW (COMMAND ARG-P DISTANCE &AUX DX DY ARROW)
    (OR ARG-P (SETQ DISTANCE (LSH 1 (LDB %%KBD-CONTROL-META COMMAND))))
    (SETQ ARROW (LDB %%KBD-CHAR COMMAND))
    (SETQ DX (* DISTANCE (OR (CADR (ASSQ ARROW '((#/ 1) (#/ -1)))) 0)))
    (SETQ DY (* DISTANCE (OR (CADR (ASSQ ARROW '((13 1) (#/ -1)))) 0)))
    (FED-SET-WINDOW-POS (+ WINDOW-X-POS DX)
                        (+ WINDOW-Y-POS DY)))

;Set the box-size (in both X and Y) of the fed-window to SCALE.
;We try to keep the center of the window in the center.
(DEFUN FED-SCALE (ARG-P SCALE)
    (LOCAL-DECLARE ((SPECIAL LEFT-MARGIN TOP-MARGIN RIGHT-MARGIN BOTTOM-MARGIN
                             BOX-X-SIZE BOX-Y-SIZE))
      (PROG ()
            (OR ARG-P (SETQ SCALE 14))
            (OR (AND (> SCALE 0)
                     (< SCALE (// (- RIGHT-MARGIN LEFT-MARGIN) 2))
                     (< SCALE (// (- BOTTOM-MARGIN TOP-MARGIN) 2)))
                (RETURN (TV-BEEP)))
            (SETQ BOX-X-SIZE SCALE BOX-Y-SIZE SCALE)
            (<- SELF ':WINDOW-UPDATE))))

;Return the window of the fed window to home position.
(DEFUN FED-HOME ()
    (FED-SET-WINDOW-POS CHAR-BOX-X1 CHAR-BOX-Y1)
    (SETQ FED-CURSOR-X 0 FED-CURSOR-Y 0))

;Set the window position of the fed window.
(DEFUN FED-SET-WINDOW-POS (X Y)
    (SETQ FED-CURSOR-X (MAX 0 (MIN WINDOW-X-SIZE (- FED-CURSOR-X (- X WINDOW-X-POS)))))
    (SETQ FED-CURSOR-Y (MAX 0 (MIN WINDOW-Y-SIZE (- FED-CURSOR-Y (- Y WINDOW-Y-POS)))))
    (SETQ WINDOW-X-POS X WINDOW-Y-POS Y)
    (<- SELF ':CLOBBER-SCREEN))

;Read the name of a font and select it.
(DEFUN FED-SPECIFY-FONT (&AUX NEW-FONT TEM)
    (LOCAL-DECLARE ((SPECIAL FONT TYPEOUT-STREAM CHARACTER))
      (FUNCALL TYPEOUT-STREAM ':FRESH-LINE)
      (PRINC "Font: ")
      (SETQ TEM (READLINE))
      (COND ((ZEROP (STRING-LENGTH TEM))
	     (TV-BEEP))
	    (T
	      (SETQ NEW-FONT (INTERN (STRING-TRIM '(#\SP) TEM) "FONTS"))
	      (COND ((OR (BOUNDP NEW-FONT)
			 (Y-OR-N-P "This font does not exist.  Create it? "
				   TYPEOUT-STREAM))
		     ;; Creating a font: make sure we have an FD for it before FED-SET-FONT-PARAMETERS is called.
		     (FED-GET-FD (SETQ FONT NEW-FONT))
		     (SETQ CHARACTER NIL)))))
      (FUNCALL TYPEOUT-STREAM ':HOME-CURSOR)
      (COND ((AND NEW-FONT (BOUNDP NEW-FONT))
             (FED-DISPLAY-FONT)
             (<- SELF ':WINDOW-UPDATE)))))

;Get the font descriptor corresponding to the specified font.
;If we haven't made one yet, make one, and remember it on FED-FD-ALIST.
;If the font is a nonexistent one (being created), make a default empty FD.
(DEFUN FED-GET-FD (FONT &AUX FD)
    (COND ((BOUNDP FONT)
           (FONT-NAME-FONT-DESCRIPTOR FONT))
          (T (SETQ FD (MAKE-FONT-DESCRIPTOR FD-LINE-SPACING 14
                                            FD-BASELINE 11
                                            FD-BLINKER-HEIGHT 14
                                            FD-BLINKER-WIDTH 7
                                            FD-SPACE-WIDTH 7))
             (AS-1 (MAKE-CHAR-DESCRIPTOR
                    MAKE-ARRAY (NIL ART-4B '(11 7))
                    CD-CHAR-WIDTH 7
                    CD-CHAR-LEFT-KERN 0)
                   FD #/ )
             (PUTPROP FONT FD 'FONT-DESCRIPTOR)
             (SET FONT NIL)
             (PUTPROP FONT NIL 'FONT-DESCRIBED)
             FD)))

;Set various per-font (as opposed to per-character) parameters of the current font.
;This command is necessary because editing one character is not allowed to
;change anything pertaining to the whole font.
(DEFUN FED-SET-FONT-PARAMETERS (&AUX FD (IBASE 10.) TEM FNT)
  (LOCAL-DECLARE ((SPECIAL FONT))
    ;; If we have no FD format array for this font, make one.
    (SETQ FD (FED-GET-FD FONT))
    (SETQ FNT (AND (BOUNDP FONT) (SYMEVAL FONT)))
    (FORMAT T "~%Font line spacing (now ~D) = " (FD-LINE-SPACING FD))
    (SETQ TEM (READLINE))
    (OR (ZEROP (STRING-LENGTH TEM))
	(NOT (NUMBERP (SETQ TEM (READ-FROM-STRING TEM))))
	(PROGN (AND FNT (SETF (FONT-CHAR-HEIGHT FNT) TEM))
	       (SETF (FD-LINE-SPACING FD) TEM)))
    (FORMAT T "Font baseline (now ~D) = " (FD-BASELINE FD))
    (SETQ TEM (READLINE))
    (OR (ZEROP (STRING-LENGTH TEM))
	(NOT (NUMBERP (SETQ TEM (READ-FROM-STRING TEM))))
	(PROGN (AND FNT (SETF (FONT-BASELINE FNT) TEM))
	       (SETF (FD-BASELINE FD) TEM)))
    (FORMAT T "Font blinker height (now ~D) = " (FD-BLINKER-HEIGHT FD))
    (SETQ TEM (READLINE))
    (OR (ZEROP (STRING-LENGTH TEM))
	(NOT (NUMBERP (SETQ TEM (READ-FROM-STRING TEM))))
	(PROGN (AND FNT (SETF (FONT-BLINKER-HEIGHT FNT) TEM))
	       (SETF (FD-BLINKER-HEIGHT FD) TEM)))
    (FORMAT T "Font blinker width (now ~D) = " (FD-BLINKER-WIDTH FD))
    (SETQ TEM (READLINE))
    (OR (ZEROP (STRING-LENGTH TEM))
	(NOT (NUMBERP (SETQ TEM (READ-FROM-STRING TEM))))
	(PROGN (AND FNT (SETF (FONT-BLINKER-WIDTH FNT) TEM))
	       (SETF (FD-BLINKER-WIDTH FD) TEM)))
    (SETQ CHAR-BOX-Y1 (- CHAR-BOX-Y2 (FD-BASELINE FD))
	  CHAR-BOX-Y3 (+ CHAR-BOX-Y1 (FD-LINE-SPACING FD)))
    (FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
    (<- SELF ':WINDOW-UPDATE)))

(DEFUN FED-READ-KST-FILE (FONT &AUX FD FILENAME)
    (SETQ FD (READ-KST-INTO-FONT-DESCRIPTOR
                (SETQ FILENAME (FED-READ-KST-FILENAME FONT)) FONT))
    (PUTPROP FONT FILENAME 'KST-FILE)
    (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONT FD))

(DEFUN FED-WRITE-KST-FILE (FONT &AUX FILENAME)
    (WRITE-FONT-INTO-KST FONT (SETQ FILENAME (FED-READ-KST-FILENAME FONT)))
    (PUTPROP FONT FILENAME 'KST-FILE))

(DEFUN FED-READ-KST-FILENAME (FONT &AUX TEM TEM1 SPEC)
    (SETQ TEM (STRING-APPEND "LMFONT;"
                             (COND ((< (STRING-LENGTH FONT) 7) (STRING FONT))
                                   (T (SUBSTRING FONT 0 6)))
                             " KST"))
    (AND (SETQ TEM1 (GET FONT 'KST-FILE))
         (SETQ TEM (SI:FILE-MERGE-PATHNAMES TEM1 TEM)))
    (PRINC "KST file name: ")
    (SETQ SPEC (READLINE))
    (FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
    (SI:FILE-MERGE-PATHNAMES SPEC TEM))

;C => Read the name of a character and select it in the current font.
;C-C => Read name of character and select it, keeping data in fed-buffer
;instead of gobbling the current definition of the new character.
;Typing a control or mouse character as the arg to the C command aborts it.
(DEFUN FED-SPECIFY-CHARACTER (COMMAND-CHAR &AUX CH)
  (LOCAL-DECLARE ((SPECIAL FONT CHARACTER))
    (PRINC "Character: ")
    (SETQ CH (FUNCALL STANDARD-INPUT ':TYI))
    (COND ((= CH (LOGAND 177 CH))
	   (SETQ CHARACTER CH)
	   (FORMAT T "~:C~%" CHARACTER)
	   (FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
	   (AND (ZEROP (LDB %%KBD-CONTROL COMMAND-CHAR))
		(FED-GOBBLE-CHARACTER FONT CHARACTER)))
	  (T (TV-BEEP)))))

;Copy the data from character CHAR in font FONT
;into the fed window to be edited.
(DEFUN FED-GOBBLE-CHARACTER (FONT CHAR &AUX FD CD)
  (PROG ()
    ;; If we have no FD format array for this font, make one.
    (SETQ FD (FED-GET-FD FONT))
    ;; Get the character descriptor for the desired character out of the FD.
    (OR (AND (SETQ CD (AR-1 FD CHAR))
             (NOT (ZEROP (ARRAY-DIMENSION-N 2 CD))))
        (RETURN (FED-ERASE-ALL SELF T)))
    (SETQ CHARACTER-ARRAY (MAKE-PLANE ART-4B 2 0 10))
    ;; Put sides of character frame at right place, according to char width and left kern.
    (SETQ CHAR-BOX-X1 (CD-CHAR-LEFT-KERN CD)
          CHAR-BOX-X2 (+ (CD-CHAR-WIDTH CD) (CD-CHAR-LEFT-KERN CD)))
    ;; Put top of character at top of font line, and bottom at baseline
    ;; so that descenders go below the "bottom".
    (SETQ CHAR-BOX-Y1 0
          CHAR-BOX-Y2 (FD-BASELINE FD)
          CHAR-BOX-Y3 (FD-LINE-SPACING FD))
    ;; Now XWIDTH and YWIDTH get the size of the character's raster,
    ;; and copy the data into the plane in CHARACTER-ARRAY.
    (LET ((XWIDTH (SECOND (ARRAY-DIMENSIONS CD)))
          (YWIDTH (FIRST (ARRAY-DIMENSIONS CD))))
         (DO I 0 (1+ I) (= I XWIDTH)
            (DO J 0 (1+ J) (= J YWIDTH)
               (PLANE-AS-N (AR-2 CD J I) CHARACTER-ARRAY I J))))
    ;; Now put the window at home position, causing a full redisplay.
    (FED-HOME)))

;M => Read the name of a character and merge it into the data already there. 
;Typing a control or mouse character as the arg to the C command aborts it.
(DEFUN FED-MERGE-CHARACTER (IGNORE &AUX CH)
  (LOCAL-DECLARE ((SPECIAL FONT))
    (PRINC "Character to merge: ")
    (SETQ CH (FUNCALL STANDARD-INPUT ':TYI))
    (COND ((= CH (LOGAND 177 CH))
	   (FORMAT T "~:C~%" CH)
	   (FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
	   (FED-MERGE-CHARACTER-1 FONT CH)))))

(DEFUN FED-MERGE-CHARACTER-1 (FONT CHAR &AUX FD CD)
  (PROG ((XOFFS (+ FED-CURSOR-X WINDOW-X-POS)) (YOFFS (+ FED-CURSOR-Y WINDOW-Y-POS)))
    ;; If we have no FD format array for this font, make one.
    (SETQ FD (FED-GET-FD FONT))
    ;; Get the character descriptor for the desired character out of the FD.
    (OR (AND (SETQ CD (AR-1 FD CHAR))
             (NOT (ZEROP (ARRAY-DIMENSION-N 2 CD))))
        (RETURN NIL))
    (SETQ XOFFS (+ (- XOFFS (CD-CHAR-LEFT-KERN CD)) CHAR-BOX-X1))
    ;; Now XWIDTH and YWIDTH get the size of the character's raster,
    ;; and copy the data into the plane in CHARACTER-ARRAY.
    (LET ((XEND (+ XOFFS (SECOND (ARRAY-DIMENSIONS CD))))
          (YEND (+ YOFFS (FIRST (ARRAY-DIMENSIONS CD)))))
      (DO I XOFFS (1+ I) (= I XEND)
	  (DO J YOFFS (1+ J) (= J YEND)
	      (PLANE-AS-N (LOGIOR (PLANE-AR-N CHARACTER-ARRAY I J)
				  (AR-2 CD (- J YOFFS) (- I XOFFS)))
			  CHARACTER-ARRAY I J)))
      (SETQ MIN-CHANGED-X (MIN MIN-CHANGED-X XOFFS))
      (SETQ MAX-CHANGED-X (MAX MAX-CHANGED-X XEND))
      (SETQ MIN-CHANGED-Y (MIN MIN-CHANGED-Y YOFFS))
      (SETQ MAX-CHANGED-Y (MAX MAX-CHANGED-Y YEND)))))

(DEFUN FED-REFLECT-COMMAND (IGNORE)
  (PROG (AXIS)
	(PRINC "Line to reflect in (X, Y, XY or X-Y): ")
	(SETQ AXIS (STRING-UPCASE (READLINE)))
	(OR (MEMBER AXIS '("X" "Y" "XY" "X-Y"))
	    (RETURN (TV-BEEP)))
	(FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
	(FED-REFLECT-CHARACTER AXIS)))

(DEFUN FED-REFLECT-CHARACTER (AXIS &AUX NEW-CHAR ORIGINS EXTENTS)
  (SETQ NEW-CHAR (MAKE-PLANE ART-4B 2 0 10))
  (SETQ ORIGINS (PLANE-ORIGIN CHARACTER-ARRAY))
  (SETQ EXTENTS (ARRAY-DIMENSIONS CHARACTER-ARRAY))
  (DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
       (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
      (( HPOS HEND))
    (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	 (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	(( VPOS VEND))
      (LET ((NEWVPOS VPOS) (NEWHPOS HPOS))
	(COND ((EQUAL AXIS "X")
	       (SETQ NEWVPOS
		     (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS)))
	      ((EQUAL AXIS "Y")
	       (SETQ NEWHPOS
		     (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS)))
	      ((EQUAL AXIS "X-Y")
	       (SETQ NEWHPOS (+ CHAR-BOX-X1 (- VPOS CHAR-BOX-Y1))
		     NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1))))
	      ((EQUAL AXIS "XY")
	       ;; Invert in the origin, then reflect in X-Y.
	       (SETQ NEWVPOS
		     (- (+ CHAR-BOX-Y1 CHAR-BOX-Y3 -1) VPOS))
	       (SETQ NEWHPOS
		     (- (+ CHAR-BOX-X1 CHAR-BOX-X2 -1) HPOS))
	       (PSETQ NEWHPOS (+ CHAR-BOX-X1 (- NEWVPOS CHAR-BOX-Y1))
		      NEWVPOS (+ CHAR-BOX-Y1 (- NEWHPOS CHAR-BOX-X1)))))
	(PLANE-AS-N (PLANE-AR-N CHARACTER-ARRAY HPOS VPOS)
		    NEW-CHAR NEWHPOS NEWVPOS))))
  (SETQ CHARACTER-ARRAY NEW-CHAR)
  (<- SELF ':CLOBBER-SCREEN))

(DEFUN FED-ROTATE-CHARACTER-RIGHT (&AUX NEW-CHAR ORIGINS EXTENTS)
  (SETQ NEW-CHAR (MAKE-PLANE ART-4B 2 0 10))
  (SETQ ORIGINS (PLANE-ORIGIN CHARACTER-ARRAY))
  (SETQ EXTENTS (ARRAY-DIMENSIONS CHARACTER-ARRAY))
  (DO ((HPOS (FIRST ORIGINS) (1+ HPOS))
       (HEND (+ (FIRST ORIGINS) (FIRST EXTENTS))))
      (( HPOS HEND))
    (DO ((VPOS (SECOND ORIGINS) (1+ VPOS))
	 (VEND (+ (SECOND ORIGINS) (SECOND EXTENTS))))
	(( VPOS VEND))
      (LET ((NEWVPOS (+ CHAR-BOX-Y1 (- HPOS CHAR-BOX-X1)))
	    (NEWHPOS (- CHAR-BOX-X2 1 (- VPOS CHAR-BOX-Y1))))
	(PLANE-AS-N (PLANE-AR-N CHARACTER-ARRAY HPOS VPOS)
		    NEW-CHAR NEWHPOS NEWVPOS))))
  (SETQ CHARACTER-ARRAY NEW-CHAR)
  (<- SELF ':CLOBBER-SCREEN))

(DEFUN FED-REGENERATE-FONT ()
    (LOCAL-DECLARE ((SPECIAL FONT CHARACTER))
      (AND CHARACTER (FED-STORE-CD FONT CHARACTER NIL))
      (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONT (FED-GET-FD FONT))))

;Save the editing that has been done on the current character.
(DEFUN FED-SAVE-CHARACTER ()
    (LOCAL-DECLARE ((SPECIAL FONT CHARACTER))
      (COND (CHARACTER
             (FED-STORE-CD FONT CHARACTER)
             (<- SELF ':UPDATE-LABEL))
            (T (TV-BEEP)))))

;Store the current FED data buffer into character CHAR of the font descriptor
;array for font FONT.
(DEFUN FED-STORE-CD (FONT CHAR &OPTIONAL (UPDATE-FONT-FLAG T)
			  &AUX FD CD YSTART XSTART YWIDTH XWIDTH KERN
                          PLANE-X1 PLANE-Y1 PLANE-WIDTH PLANE-HEIGHT)
  (PROG FED-STORE-CD ()
    ;; Find the FD format array for this font.
    (SETQ FD (FED-GET-FD FONT))
    ;; Warn if char box now displayed is incompatible with the font.
    (COND ((OR ( (- CHAR-BOX-Y2 CHAR-BOX-Y1) (FD-BASELINE FD))
	       ( (- CHAR-BOX-Y3 CHAR-BOX-Y1) (FD-LINE-SPACING FD)))
	   (OR (Y-OR-N-P "/
Character height and baseline are incompatible with font.
If actually stored, the character will be aligned by the top of its box.
Proceed to store anyway?"
			 TYPEOUT-STREAM)
	       (RETURN-FROM FED-STORE-CD NIL))))
    ;; What are the regions of the fed data plane which actually are stored?
    (SETQ PLANE-X1 (FIRST (PLANE-ORIGIN CHARACTER-ARRAY)))
    (SETQ PLANE-Y1 (SECOND (PLANE-ORIGIN CHARACTER-ARRAY)))
    (SETQ PLANE-WIDTH (FIRST (ARRAY-DIMENSIONS CHARACTER-ARRAY)))
    (SETQ PLANE-HEIGHT (SECOND (ARRAY-DIMENSIONS CHARACTER-ARRAY)))
    ;; Figure out what portion of the plane holding the fed data is really nonzero.
    ;; XSTART and YSTART get the indices in CHARACTER-ARRAY (as an array, not as a plane!)
    ;; of what is going to go into the upper left corner of the CD.
    ;; XWIDTH and YWIDTH get the dimensions which the CD will need to hold all nonzero data.
    ;; XSTART is determined by the leftmost nonzero data, and its distance from
    ;; CHAR-BOX-X1 determines the left kern.  YSTART has to correspond to CHAR-BOX-Y1
    ;; because that is not a per-character parameter.
    (SETQ YSTART (MAX 0 (- CHAR-BOX-Y1 PLANE-Y1)) YWIDTH 0)
    (DO J YSTART (1+ J) (= J PLANE-HEIGHT)
       (DO I 0 (1+ I) (= I PLANE-WIDTH)
          (OR (ZEROP (AR-2 CHARACTER-ARRAY I J))
              (SETQ YWIDTH (1+ (- J YSTART))))))
    (SETQ XSTART NIL XWIDTH 0)
    (DO I 0 (1+ I) (= I PLANE-WIDTH)
       (DO J YSTART (1+ J) (= J PLANE-HEIGHT)
          (COND ((NOT (ZEROP (AR-2 CHARACTER-ARRAY I J)))
                 (OR XSTART (SETQ XSTART I))
                 (SETQ XWIDTH (1+ (- I XSTART)))))))
    ;; Make sure XSTART isn't NIL, and neither width is zero.
    (COND ((NULL XSTART)
           (SETQ XSTART 0 XWIDTH 1)))
    (AND (ZEROP YWIDTH) (SETQ YWIDTH 1))
    ;; Warn about dots to be lost above YSTART.
    (PROG FOO ()
      (DO I 0 (1+ I) (= I PLANE-WIDTH)
         (DO J 0 (1+ J) (= J YSTART)
	    (OR (ZEROP (AR-2 CHARACTER-ARRAY I J))
		(COND ((Y-OR-N-P "/
Dots above character top will be lost.  Store anyway? "
				 TYPEOUT-STREAM)
		       (RETURN-FROM FOO NIL))
		      (T (RETURN-FROM FED-STORE-CD NIL)))))))
    (SETQ KERN (- CHAR-BOX-X1 (+ XSTART PLANE-X1)))
    ;; Copy the data in the FED buffer into a CD
    (SETQ CD (MAKE-CHAR-DESCRIPTOR
                      MAKE-ARRAY (NIL ART-4B (LIST YWIDTH XWIDTH))
                      CD-CHAR-WIDTH (- CHAR-BOX-X2 CHAR-BOX-X1)
                      CD-CHAR-LEFT-KERN KERN))
    (DO I 0 (1+ I) (= I XWIDTH)
       (DO J 0 (1+ J) (= J YWIDTH)
          (AS-2 (AR-2 CHARACTER-ARRAY (+ I XSTART) (+ J YSTART))
                CD J I)))
    (COND (UPDATE-FONT-FLAG
           ;; Use the CD just made to update the font itself,or make a new font.
           (FONT-NAME-STORE-CD FONT CD CHAR))
          (T
           ;; Store the CD in the FD.
           (AS-1 CD FD CHAR)
           (AND (= CHAR #/ )
                (SETF (FD-SPACE-WIDTH FD) (CD-CHAR-WIDTH CD)))))))

(DEFMETHOD (FED-WINDOW-CLASS :MOUSE-BUTTONS) (BD X Y)
  ;; First, see if the mouse is where we have typed stuff out.
  ;; If so, it has no effect (since we use no typeout items)
  ;; unless it is getting the system menu.
  (OR (FUNCALL TYPEOUT-STREAM ':MOUSE-BUTTONS BD X Y)
      ;; Not in typeout => decode double clicks
      ;; and pass along as an input character to the fed process
      ;; unless it's getting the system menu.
      (LET ((BUTTONS (MOUSE-BUTTON-ENCODE BD)))
        (COND ((= BUTTONS 2012)
               (<- (<- SELF ':POP-UP-MENU) ':CHOOSE))
              (T (<- SI:PROCESS ':FORCE-KBD-INPUT (DPB 1 %%KBD-MOUSE BUTTONS)))))))

;Set or clear the square under the mouse, and then as long as you hold the button down
;whenever the mouse is moved to a new square that square is set or clear also.
(DEFUN FED-MOUSE-MARK-SQUARES (SETP &AUX X Y OLD-M-X OLD-M-Y)
    (LOCAL-DECLARE ((SPECIAL LEFT-MARGIN TOP-MARGIN BOX-X-SIZE BOX-Y-SIZE
                             WINDOW-X-SIZE WINDOW-Y-SIZE FED-CURSOR-X FED-CURSOR-Y))
      (DO ((FIRST T NIL))
          ((AND (NOT FIRST) (ZEROP (SI:MOUSE-BUTTONS))))
        (OR FIRST (MOUSE-WAIT OLD-M-X OLD-M-Y))
        (SETQ OLD-M-X MOUSE-X OLD-M-Y MOUSE-Y)
        (SETQ X (// (- MOUSE-X LEFT-MARGIN) BOX-X-SIZE))
        (SETQ Y (// (- MOUSE-Y TOP-MARGIN) BOX-Y-SIZE))
        (OR (AND (LESSP -1 X WINDOW-X-SIZE) (LESSP -1 Y WINDOW-Y-SIZE))
            (RETURN NIL))
        (OR (AND (NOT FIRST)
                 (= X FED-CURSOR-X) (= Y FED-CURSOR-Y))
            (PROGN (FED-ALTER-SQUARE SETP X Y)
                   (<- SELF ':UPDATE)))
        (SETQ FED-CURSOR-X X FED-CURSOR-Y Y))))

;Push this button when the mouse is near an edge or corner of the character box,
;and then as long as you hold the button down you are moving that corner.
(DEFUN FED-MOUSE-MOVE-CHAR-BOX (&AUX X-POS-NAME Y-POS-NAME)
    (LOCAL-DECLARE ((SPECIAL LEFT-MARGIN TOP-MARGIN
                             WINDOW-X-POS WINDOW-Y-POS
                             BOX-X-SIZE BOX-Y-SIZE
                             CHAR-BOX-X1 CHAR-BOX-X2
                             CHAR-BOX-Y1 CHAR-BOX-Y2 CHAR-BOX-Y3
                             WINDOW-X-SIZE WINDOW-Y-SIZE))
      (PROG ()
            ;; Decide which corner or edge of the character box we will move
            ;; (or maybe we aren't in range of any of them).
            (COND ((< (ABS (- MOUSE-X (* (- CHAR-BOX-X1 WINDOW-X-POS)
                                         BOX-X-SIZE) LEFT-MARGIN))
                      (// BOX-X-SIZE 2))
                   (SETQ X-POS-NAME 'CHAR-BOX-X1))
                  ((< (ABS (- MOUSE-X (* (- CHAR-BOX-X2 WINDOW-X-POS)
                                         BOX-X-SIZE) LEFT-MARGIN))
                      (// BOX-X-SIZE 2))
                   (SETQ X-POS-NAME 'CHAR-BOX-X2)))
            (COND ((< (ABS (- MOUSE-Y (* (- CHAR-BOX-Y1 WINDOW-Y-POS)
                                         BOX-Y-SIZE) TOP-MARGIN))
                      (// BOX-Y-SIZE 2))
                   (SETQ Y-POS-NAME 'CHAR-BOX-Y1))
                  ((< (ABS (- MOUSE-Y (* (- CHAR-BOX-Y2 WINDOW-Y-POS)
                                         BOX-Y-SIZE) TOP-MARGIN))
                      (// BOX-Y-SIZE 2))
                   (SETQ Y-POS-NAME 'CHAR-BOX-Y2))
                  ((< (ABS (- MOUSE-Y (* (- CHAR-BOX-Y3 WINDOW-Y-POS)
                                         BOX-Y-SIZE) TOP-MARGIN))
                      (// BOX-Y-SIZE 2))
                   (SETQ Y-POS-NAME 'CHAR-BOX-Y3)))
            ;; If not in range to move any edge, beep.
            (OR X-POS-NAME Y-POS-NAME (RETURN (TV-BEEP)))
            (DO ((NOT-FIRST NIL T) (X) (Y) (OX) (OY) (OLD-M-X) (OLD-M-Y))
                ((AND NOT-FIRST (ZEROP MOUSE-LAST-BUTTONS)))
              (AND NOT-FIRST (MOUSE-WAIT OLD-M-X OLD-M-Y))
              (OR (SI:WINDOW-OWNS-MOUSE-P SELF)
                  (RETURN NIL))
              (SETQ OLD-M-X MOUSE-X OLD-M-Y MOUSE-Y)
              (SETQ X (// (+ (// BOX-X-SIZE 2) (- MOUSE-X LEFT-MARGIN)) BOX-X-SIZE))
              (SETQ Y (// (+ (// BOX-Y-SIZE 2) (- MOUSE-Y TOP-MARGIN)) BOX-Y-SIZE))
              ;; Exit if mouse is outside of FED grid area.
              (OR (AND (LESSP -1 X (1+ WINDOW-X-SIZE)) (LESSP -1 Y (1+ WINDOW-Y-SIZE)))
                  (RETURN NIL))
              (SETQ X (+ X WINDOW-X-POS) Y (+ Y WINDOW-Y-POS))
              ;; Try moving the edges, remember where they used to be.
              (SETQ OX (SYMEVAL X-POS-NAME) OY (SYMEVAL Y-POS-NAME))
              (AND Y-POS-NAME (SET Y-POS-NAME Y))
              (AND X-POS-NAME (SET X-POS-NAME X))
              ;; Don't move an edge past or up to its opposite edge.
              (OR (AND ( CHAR-BOX-X1 CHAR-BOX-X2)
                       (< CHAR-BOX-Y1 CHAR-BOX-Y2)
                       ( CHAR-BOX-Y2 CHAR-BOX-Y3))
                  (PROGN (SET X-POS-NAME OX)
                         (SET Y-POS-NAME OY)
                         (TV-BEEP)))
              ;; If we are really moving an edge to a new place, redisplay.
              (OR (AND (OR (NOT X-POS-NAME)
                           (= (SYMEVAL X-POS-NAME) OX))
                       (OR (NOT Y-POS-NAME)
                           (= (SYMEVAL Y-POS-NAME) OY)))
                  (<- SELF ':UPDATE))))))
