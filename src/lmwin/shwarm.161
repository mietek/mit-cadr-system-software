;;; -*- Mode: LISP; Package: TV; Base: 8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;;Miscellaneous user functions
(DEFUN SCREEN-CLEAR (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  "This function is obsolete, but may still be called."
  ;; It isn't really obsolete, the initialization right below calls it
  (WITHOUT-INTERRUPTS
    (PREPARE-SHEET (SCREEN)
      (%DRAW-RECTANGLE (SHEET-WIDTH SCREEN) (SHEET-HEIGHT SCREEN)
		       0 0
		       ALU-ANDCA SCREEN))
    (AND (FBOUNDP 'WHO-LINE-CLOBBERED)
	 (WHO-LINE-CLOBBERED))
    (AND (FBOUNDP 'SCREEN-MANAGE-FLUSH-KNOWLEDGE)
	 (SCREEN-MANAGE-FLUSH-KNOWLEDGE SCREEN))))

;Upon a cold-boot, after exposing the main-screen and the initial-lisp-listener,
;we erase the main-screen, including the who-line which is in its margin.
;This serves two purposes; one it gets rid of unsightly bits that may have
;been left in the hardware, and two it gets rid of old typeout that may
;have been in the initial lisp listener at the time the band was saved,
;if that lisp listener was not exposed at the time.
(ADD-INITIALIZATION "Clear Screen" '(SCREEN-CLEAR) '(:COLD))

(DEFUN SCREEN-REDISPLAY (&OPTIONAL (TYPE ':COMPLETE-REDISPLAY) (SCREEN DEFAULT-SCREEN))
  (FUNCALL SCREEN ':REFRESH TYPE)
  (WHO-LINE-CLOBBERED))

(DEFMETHOD (SCREEN :BEEP) (&OPTIONAL BEEP-TYPE)
  "Beep the beeper."
  BEEP-TYPE  ;We wanted to make this soo hairy, that we punted until we could do it right
  (WITHOUT-INTERRUPTS  ;otherwise might quit out and leave screen complemented
    (AND (MEMQ BEEP '(:FLASH T)) (COMPLEMENT-BOW-MODE SELF))
    (AND (MEMQ BEEP '(:BEEP T)) (%BEEP BEEP-WAVELENGTH BEEP-DURATION))
    (AND (MEMQ BEEP '(:FLASH T)) (COMPLEMENT-BOW-MODE SELF))))

(DEFMETHOD (SHEET :BEEP) (&OPTIONAL BEEP-TYPE)
  (AND SUPERIOR (FUNCALL SUPERIOR ':BEEP BEEP-TYPE)))

(DEFUN BEEP (&OPTIONAL BEEP-TYPE)
  (FUNCALL TERMINAL-IO ':BEEP BEEP-TYPE))

(DEFUN BLACK-ON-WHITE (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGIOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))

(DEFUN WHITE-ON-BLACK (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGAND -5 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN))))) ;1's comp of 4

(DEFUN COMPLEMENT-BOW-MODE (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGXOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))

(DEFUN SHEET-INCREMENT-BITPOS (SHEET DX DY &AUX X Y MORE-VPOS)
  "Increment cursor X and cursor Y, keeping within sheet.  Sets exception flags
according to new positions"
  (SETF (SHEET-CURSOR-X SHEET)
	(SETQ X (MAX (+ DX (SHEET-CURSOR-X SHEET)) (SHEET-INSIDE-LEFT SHEET))))
  (SETF (SHEET-CURSOR-Y SHEET)
	(SETQ Y (MAX (+ DY (SHEET-CURSOR-Y SHEET)) (SHEET-INSIDE-TOP SHEET))))
  (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
       (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
  (AND (SETQ MORE-VPOS (SHEET-MORE-VPOS SHEET))
       ( Y MORE-VPOS)
       (SETF (SHEET-MORE-FLAG SHEET) 1))
  NIL)

(DEFUN SHEET-TAB (SHEET)
  "Output a tab to a sheet"
  (PREPARE-SHEET (SHEET)
     (OR (ZEROP (SHEET-EXCEPTIONS SHEET)) (SHEET-HANDLE-EXCEPTIONS SHEET))
     (LET ((TAB-WIDTH (* 8 (SHEET-CHAR-WIDTH SHEET))))
       (SHEET-INCREMENT-BITPOS SHEET (- TAB-WIDTH (\ (- (SHEET-CURSOR-X SHEET)
							(SHEET-INSIDE-LEFT SHEET))
						     TAB-WIDTH))
			       0))))

(DEFUN SHEET-SET-FONT (SHEET FONT)
  "Change a sheet's current font"
  (SETF (SHEET-CURRENT-FONT SHEET) FONT)
  (SETF (SHEET-BASELINE-ADJ SHEET) (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))

(DEFUN SHEET-SET-CURSORPOS (SHEET X Y)
  "Set 'cursor' position of a sheet in terms of raster units.  Cursorposes are relative
to the left and top margins.  Cursorpos is `clipped' to stay inside the sheet-inside."
  (DO ((INHIBIT-SCHEDULING-FLAG T T)  ;Keep trying until we get the lock
       (LOCK) (BL))
      ((AND (SETQ LOCK (SHEET-CAN-GET-LOCK SHEET))
	    (NOT (SHEET-OUTPUT-HELD-P SHEET)))
       (SETQ X (IF X (MIN (+ (MAX (FIX X) 0) (SHEET-INSIDE-LEFT SHEET))
			  (SHEET-INSIDE-RIGHT SHEET))
		   (SHEET-CURSOR-X SHEET)))
       (SETQ Y (IF Y (MIN (+ (MAX (FIX Y) 0) (SHEET-INSIDE-TOP SHEET))
			  (SHEET-INSIDE-BOTTOM SHEET))
		   (SHEET-CURSOR-Y SHEET)))
       (AND (= (SHEET-CURSOR-X SHEET) X) (= (SHEET-CURSOR-Y SHEET) Y)
	    (RETURN NIL))			;Not moving, don't open the blinker
       (AND (SETQ BL (SHEET-FOLLOWING-BLINKER SHEET))
	    (OPEN-BLINKER BL))
       (AND (SHEET-MORE-VPOS SHEET)		;If more processing enabled, delay until
						; bottom of sheet
	    (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
       (SETF (SHEET-CURSOR-X SHEET) X)
       (SETF (SHEET-CURSOR-Y SHEET) Y)
       (SETF (SHEET-EXCEPTIONS SHEET) 0)
       (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
	    (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
       T)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (IF LOCK
	(FUNCALL SHEET ':OUTPUT-HOLD-EXCEPTION)
	(PROCESS-WAIT "Lock" #'SHEET-CAN-GET-LOCK SHEET))))

(DEFUN SHEET-READ-CURSORPOS (SHEET)
  "Read the cursor position in raster units relative to margins"
  (PROG ()
    (RETURN (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
	    (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)))))

(DEFUN SHEET-HOME (SHEET)
  "Go to upper left edge of sheet (Home up)"
  (PREPARE-SHEET (SHEET)
    (AND (SHEET-MORE-VPOS SHEET)		;If MORE processing, put it off 'til last line
	 (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SETF (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))
    (SETF (SHEET-EXCEPTIONS SHEET) 0)))

(DEFUN SHEET-CRLF (SHEET)
  "Crlf and clear next line"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))	;Handle exceptions first
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SHEET-INCREMENT-BITPOS SHEET 0 (SHEET-LINE-HEIGHT SHEET))
    (SHEET-CLEAR-EOL SHEET)))

(DEFUN SHEET-SPACE (SHEET &OPTIONAL CHAR)
  "Space forward"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS SHEET
			    (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
							    (SHEET-CURRENT-FONT SHEET))
				     (SHEET-CHAR-WIDTH SHEET))
			    0)))

(DEFUN SHEET-BACKSPACE (SHEET &OPTIONAL CHAR)
  "Space backwards"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS SHEET
			    (- (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
							       (SHEET-CURRENT-FONT SHEET))
					(SHEET-CHAR-WIDTH SHEET)))
			    0)))

(DEFUN SHEET-CLEAR-CHAR (SHEET &OPTIONAL CHAR)
  "Clear current character position"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
						     (SHEET-CURRENT-FONT SHEET))
			      (SHEET-CHAR-WIDTH SHEET))
		     (SHEET-LINE-HEIGHT SHEET)
		     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFUN SHEET-CLEAR-EOL (SHEET)
  "Clear to end of current line"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (MAX (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET))
			  0)
		     (MIN (- (SHEET-INSIDE-BOTTOM SHEET) (SHEET-CURSOR-Y SHEET))
			  (SHEET-LINE-HEIGHT SHEET))
		     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFUN SHEET-CLEAR-BETWEEN-CURSORPOSES (SHEET START-X START-Y END-X END-Y
					&AUX (ALUF (SHEET-ERASE-ALUF SHEET)) MID-Y)
  "Erase from starting pos to ending pos
   Does nothing if start is after end on the same line, but if on different
   lines, assumes screen wrap-around"
  (SETQ START-X (MIN (+ START-X (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
	START-Y (MIN (+ START-Y (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
	END-X (MIN (+ END-X (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
	END-Y (MIN (+ END-Y (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET)))
  (PREPARE-SHEET (SHEET)
    (COND ((= START-Y END-Y)
	   (COND ((< START-X END-X)
		  (%DRAW-RECTANGLE (- END-X START-X)
				   (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y)
					(SHEET-LINE-HEIGHT SHEET))
				   START-X START-Y ALUF SHEET))))
	  (T (%DRAW-RECTANGLE (- (SHEET-INSIDE-RIGHT SHEET) START-X) 
			      (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y)
				   (SHEET-LINE-HEIGHT SHEET))
			      START-X START-Y ALUF SHEET)
	     (SETQ MID-Y (+ START-Y (SHEET-LINE-HEIGHT SHEET)))
	     (%DRAW-RECTANGLE END-X (MIN (- (SHEET-INSIDE-BOTTOM SHEET) END-Y)
					 (SHEET-LINE-HEIGHT SHEET))
			      (SHEET-INSIDE-LEFT SHEET) END-Y ALUF SHEET)
	     (IF (< START-Y END-Y)
		 (AND (< MID-Y END-Y)
		      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (- END-Y MID-Y)
				       (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET))
		 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET)
				  (- (SHEET-INSIDE-BOTTOM SHEET) MID-Y)
				  (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET)
		 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET)
				  (- END-Y (SHEET-INSIDE-TOP SHEET))
				  (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)
				  ALUF SHEET))))))

(DEFMETHOD (SHEET :CLEAR-SCREEN) (&OPTIONAL (MARGINS-P NIL))
  (SHEET-CLEAR SELF MARGINS-P))

(DEFUN SHEET-CLEAR (SHEET &OPTIONAL (MARGINS-P NIL))
  (PREPARE-SHEET (SHEET)
    (SHEET-HOME SHEET)				;Handles any exceptions
    (IF MARGINS-P
	(%DRAW-RECTANGLE (SHEET-WIDTH SHEET) (SHEET-HEIGHT SHEET)
			 0 0
			 (SHEET-ERASE-ALUF SHEET) SHEET)
	(%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (SHEET-INSIDE-HEIGHT SHEET)
			 (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)
			 (SHEET-ERASE-ALUF SHEET) SHEET))
    (SCREEN-MANAGE-FLUSH-KNOWLEDGE SHEET)))

(DEFUN SHEET-CLEAR-EOF (SHEET &AUX HT TEM)
  "Clear from cursor to end of sheet"
  (PREPARE-SHEET (SHEET)
    (SHEET-CLEAR-EOL SHEET)			;Will process exceptions
    (AND (PLUSP (SETQ HT (- (SHEET-INSIDE-BOTTOM SHEET)
			    (SETQ TEM (+ (SHEET-CURSOR-Y SHEET) (SHEET-LINE-HEIGHT SHEET))))))
	 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) HT
			  (SHEET-INSIDE-LEFT SHEET) TEM
			  (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-HOME-DOWN (SHEET)
  "Place cursor at bottom of sheet"
  (SHEET-SET-CURSORPOS SHEET 0 (- (SHEET-INSIDE-HEIGHT SHEET) (SHEET-LINE-HEIGHT SHEET))))

(DEFUN SHEET-INSERT-LINE (SHEET &OPTIONAL (LINE-COUNT 1))
  "Make room for a line before the line the cursor is currently on"
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (WIDTH (SHEET-INSIDE-WIDTH SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  HEIGHT
	  DELTA-HEIGHT)
      (SETQ HEIGHT (* LINE-COUNT LINE-HEIGHT))
      ;; Compute minus height of block to BLT
      (SETQ DELTA-HEIGHT
	    (- HEIGHT (- (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES SHEET))
			 (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)))))
      (OR ( DELTA-HEIGHT 0)			;If some bits to move, move them
	  (BITBLT ALU-SETA
		  WIDTH DELTA-HEIGHT
		  ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
		  ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)))
      (%DRAW-RECTANGLE WIDTH HEIGHT
		       (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-DELETE-LINE (SHEET &OPTIONAL (LINE-COUNT 1))
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (WIDTH (SHEET-INSIDE-WIDTH SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  HEIGHT
	  DELTA-HEIGHT)
      (SETQ HEIGHT (* LINE-COUNT LINE-HEIGHT))
      (AND (PLUSP (SETQ DELTA-HEIGHT
			(- (+ (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)) HEIGHT)
			   (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES SHEET)))))
	   (FERROR NIL "Illegal line-count ~S for ~S" LINE-COUNT SHEET))
      (BITBLT ALU-SETA WIDTH (- DELTA-HEIGHT)
	      ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)
	      ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH HEIGHT
		       (SHEET-INSIDE-LEFT SHEET) (- (SHEET-CURSOR-Y SHEET) DELTA-HEIGHT)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-INSERT-CHAR (SHEET &OPTIONAL (CHAR-COUNT 1) (TYPE ':CHARACTER))
  "Make room for characters after cursor.  Is only correct for fixed width fonts"
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  (WIDTH (IF (EQ TYPE ':PIXEL) CHAR-COUNT
		     (* CHAR-COUNT (SHEET-CHAR-WIDTH SHEET)))))
      (BITBLT ALU-SETA
	      (- WIDTH (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET)))
	      LINE-HEIGHT
	      ARRAY (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
	      ARRAY (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH LINE-HEIGHT
		       (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-DELETE-CHAR (SHEET &OPTIONAL (CHAR-COUNT 1) (TYPE ':CHARACTER))
  "Delete characters after cursor.  Is only correct for fixed width fonts"
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  (WIDTH (IF (EQ TYPE ':PIXEL) CHAR-COUNT
		     (* CHAR-COUNT (SHEET-CHAR-WIDTH SHEET)))))
      (BITBLT ALU-SETA
	      (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET) WIDTH)
	      LINE-HEIGHT
	      ARRAY (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-CURSOR-Y SHEET)
	      ARRAY (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH LINE-HEIGHT
		       (- (SHEET-INSIDE-RIGHT SHEET) WIDTH)
		       (SHEET-CURSOR-Y SHEET)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-INSERT-STRING (SHEET STRING &OPTIONAL (START 0) END (TYPE-TOO T) &AUX LEN)
  (SETQ LEN (IF (NUMBERP STRING)
		(SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET))
		(SHEET-STRING-LENGTH SHEET STRING START END)))
  (SHEET-INSERT-CHAR LEN ':PIXEL)
  (AND TYPE-TOO (SHEET-STRING-OUT SHEET STRING START END)))

(DEFUN SHEET-DELETE-STRING (SHEET STRING &OPTIONAL (START 0) END &AUX LEN)
  (SETQ LEN (IF (NUMBERP STRING)
		(SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET))
		(SHEET-STRING-LENGTH SHEET STRING START END)))
  (SHEET-DELETE-CHAR LEN ':PIXEL))

(DEFUN SHEET-TYO (SHEET CHAR)
  "Draw a printing character in a sheet, or execute a special function"
  (IF ( CHAR 200)
      (COND ((= CHAR #\CR)
             (SHEET-CRLF SHEET))
            ((= CHAR #\TAB)
             (SHEET-TAB SHEET))
            ((AND (= CHAR #\BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
             (SHEET-BACKSPACE SHEET))
            ((< CHAR 240)			;Invisible format effector
	     (SHEET-DISPLAY-LOSENGED-STRING SHEET
	       (IF (= CHAR #\FORM) "PAGE"	;Rather than "CLEAR-SCREEN"
		   (STRING (CAR (RASSOC CHAR SI:XR-SPECIAL-CHARACTER-NAMES)))))))
      (PREPARE-SHEET (SHEET)
        (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	    (SHEET-HANDLE-EXCEPTIONS SHEET))
        (LET* ((FONT (SHEET-CURRENT-FONT SHEET))
	       (CHAR-WIDTHS (FONT-CHAR-WIDTH-TABLE FONT))
	       (FIT (FONT-INDEXING-TABLE FONT))
	       (WIDTH)
	       (KERN 0)
	       (KERN-TABLE)
	       (XPOS (SHEET-CURSOR-X SHEET))
	       (RIGHT-LIM (SHEET-INSIDE-RIGHT SHEET)))
	  (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
	      (SETQ RIGHT-LIM (- RIGHT-LIM (SHEET-CHAR-WIDTH SHEET))))
	  (SETQ WIDTH (IF CHAR-WIDTHS
			  (AREF CHAR-WIDTHS CHAR)
			  (FONT-CHAR-WIDTH FONT)))
	  (COND ((> (+ XPOS WIDTH) RIGHT-LIM)
		 (FUNCALL SHEET ':END-OF-LINE-EXCEPTION)
		 (SETQ XPOS (SHEET-CURSOR-X SHEET))))
	  (AND (SETQ KERN-TABLE (FONT-LEFT-KERN-TABLE FONT))
	       (SETQ KERN (AREF KERN-TABLE CHAR)))
	  (COND ((NULL FIT)
		 (%DRAW-CHAR FONT CHAR (- XPOS KERN)
			     (+ (SHEET-CURSOR-Y SHEET) (SHEET-BASELINE-ADJ SHEET))
			     (SHEET-CHAR-ALUF SHEET)
			     SHEET))
		;; Wide character, draw several columns
		(T
		  (DO ((CH (AREF FIT CHAR) (1+ CH))
		       (LIM (AREF FIT (1+ CHAR)))
		       (BPP (SHEET-BITS-PER-PIXEL SHEET))
		       (XPOS (- XPOS KERN)
			     (+ XPOS (// (FONT-RASTER-WIDTH FONT) BPP)))
		       (YPOS (+ (SHEET-CURSOR-Y SHEET) (SHEET-BASELINE-ADJ SHEET)))
		       (ALUF (SHEET-CHAR-ALUF SHEET)))
		      ((= CH LIM))
		    (%DRAW-CHAR FONT CH XPOS YPOS ALUF SHEET))))
	  (SETF (SHEET-CURSOR-X SHEET) (+ XPOS WIDTH)))))
  CHAR)

(DEFUN SHEET-STRING-OUT (SHEET STRING &OPTIONAL (START 0) (END NIL))
       "Routine to print a string on a sheet. Understands format effectors (special
keys 200-237).  Optional starting and ending indicies may be supplied.  Default is
to output the whole string"
  (PREPARE-SHEET (SHEET)
    (AND (SYMBOLP STRING)		;Convert symbols to strings for output
	 (SETQ STRING (GET-PNAME STRING)))
    (PROG ((I START)
	   (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	   (FONT (SHEET-CURRENT-FONT SHEET))
	   XPOS YPOS XLIM ALUF WIDTH CH FWT LKT)
       TOP
	  (AND ( I N) (RETURN NIL))		        ;No exception if done anyway
	  (AND (NULL (FONT-INDEXING-TABLE FONT))
	       (GO EZ))					;Handle easy case fast
       HD (SHEET-TYO SHEET (AREF STRING I))
	  (AND (< (SETQ I (1+ I)) N)
	       (GO TOP))
	  (RETURN NIL)

       EZ (OR (ZEROP (SHEET-EXCEPTIONS SHEET))		;End of page, MORE
	      (SHEET-HANDLE-EXCEPTIONS SHEET))
	  (SETQ XPOS (SHEET-CURSOR-X SHEET)
		YPOS (+ (SHEET-CURSOR-Y SHEET) (SHEET-BASELINE-ADJ SHEET))
	        ALUF (SHEET-CHAR-ALUF SHEET)
		WIDTH (FONT-CHAR-WIDTH FONT)
		XLIM (SHEET-INSIDE-RIGHT SHEET))
	  (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
	      (SETQ XLIM (- XLIM (SHEET-CHAR-WIDTH SHEET))))
	  (AND (OR (FONT-CHAR-WIDTH-TABLE FONT) (FONT-LEFT-KERN-TABLE FONT))
	       (GO VW))					;Variable-width is a little slower
       EZ1						;This is the fast loop
	  (COND ((< (SETQ CH (AREF STRING I)) 200)	;Printing char
		 (COND ((> (+ XPOS WIDTH) XLIM)		;Room for it before right margin?
			(SETF (SHEET-CURSOR-X SHEET) XPOS)
			(FUNCALL SHEET ':END-OF-LINE-EXCEPTION)
			(GO TOP)))
		 (%DRAW-CHAR FONT CH XPOS YPOS ALUF SHEET)
		 (SETQ XPOS (+ XPOS WIDTH))
		 (AND (< (SETQ I (1+ I)) N)
		      (GO EZ1))
		 (SETF (SHEET-CURSOR-X SHEET) XPOS)
		 (RETURN NIL))
		(T					;Format effector
		 (SETF (SHEET-CURSOR-X SHEET) XPOS)
		 (GO HD)))

       VW  (SETQ FWT (FONT-CHAR-WIDTH-TABLE FONT)	;This is the medium speed loop 
		 LKT (FONT-LEFT-KERN-TABLE FONT))
       VW1 (COND ((< (SETQ CH (AREF STRING I)) 200)	;Printing char
		  (AND FWT (SETQ WIDTH (AREF FWT CH)))
		  (COND ((> (+ WIDTH XPOS) XLIM)	;Room before margin?
			 (SETF (SHEET-CURSOR-X SHEET) XPOS)
			 (FUNCALL SHEET ':END-OF-LINE-EXCEPTION)
			 (GO TOP)))
		  (%DRAW-CHAR FONT CH (IF LKT (- XPOS (AREF LKT CH)) XPOS) YPOS ALUF SHEET)
		  (SETQ XPOS (+ XPOS WIDTH))
		  (AND (< (SETQ I (1+ I)) N)
		       (GO VW1))
		  (SETF (SHEET-CURSOR-X SHEET) XPOS)
		  (RETURN NIL))
		 (T					;Format effector
		  (SETF (SHEET-CURSOR-X SHEET) XPOS)
		  (GO HD))))))

;;; Editor's line redisplay primitive, output STRING from START to END,
;;; first setting position to (SET-XPOS,SET-YPOS) and doing a clear-eol
;;; DWIDTH is a special hack for DIS-LINE redisplay of italic fonts, it means
;;; draw an extra character starting one character back, since the clear-eol
;;; will have erased part of the last character where it sticks out past its width.
;;; (If this can really happen, it's going to mean trouble with the margins, too!)
;;; This function never does more than one line; it stops rather than wrapping around.
;;; If you put a carriage return in the string, above may not be true.
;;; Where this leaves the sheet's actual cursorpos is undefined (somewhere on the line)
(DEFUN SHEET-LINE-OUT (SHEET STRING &OPTIONAL (START 0) (END NIL) SET-XPOS SET-YPOS DWIDTH)
 (DECLARE (RETURN-LIST I XPOS))	;Returns index of next character to do and where cursor got to
 				;Except the first value can be incremented, to show that
				;the line was completed (as if it counted the carriage return)
 (PROG CROCK ()		;Multiple values do not propogate out of an unwind protect
  (PREPARE-SHEET (SHEET)
    (PROG ((I START)
	   (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	   (RIGHT-LIMIT (SHEET-INSIDE-RIGHT SHEET))
	   (MARGIN-FLAG (NOT (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))))
	   XPOS YPOS OYPOS ALUF WIDTH FWT LKT CH FONT FONTX TEM)
      (AND MARGIN-FLAG (SETQ RIGHT-LIMIT (- RIGHT-LIMIT (SHEET-CHAR-WIDTH SHEET))))
      (COND (SET-XPOS
	     (SETF (SHEET-CURSOR-X SHEET)
		   (SETQ SET-XPOS (MIN (+ SET-XPOS (SHEET-INSIDE-LEFT SHEET))
				       (SHEET-INSIDE-RIGHT SHEET))))))
      (COND (SET-YPOS
	     (AND (SHEET-MORE-VPOS SHEET)
		  (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
	     (SETF (SHEET-CURSOR-Y SHEET)
		   (SETQ SET-YPOS (MIN (+ SET-YPOS (SHEET-INSIDE-TOP SHEET))
				       (SHEET-INSIDE-BOTTOM SHEET))))
	     (SETF (SHEET-EXCEPTIONS SHEET) 0)
	     (AND (> (+ SET-YPOS (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
		  (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
	     (SETQ OYPOS SET-YPOS))
	    (T (SETQ OYPOS (SHEET-CURSOR-Y SHEET))))

      (OR (ZEROP (SHEET-EXCEPTIONS SHEET)) (SHEET-HANDLE-EXCEPTIONS SHEET))

      ;; If we set the cursor then do a clear to end of line
      (AND (OR SET-XPOS SET-YPOS)
	   (%DRAW-RECTANGLE (- (SHEET-INSIDE-RIGHT SHEET)
			       (SETQ SET-XPOS (OR SET-XPOS (SHEET-CURSOR-X SHEET))))
			    (SHEET-LINE-HEIGHT SHEET)
			    SET-XPOS OYPOS
			    (SHEET-ERASE-ALUF SHEET) SHEET))
      ;; If special case of italic line, move back and decrement starting index
      (COND (DWIDTH
	     (SETF (SHEET-CURSOR-X SHEET) (- SET-XPOS DWIDTH))
	     (SETQ I (1- I))))

  HD  (AND ( I N)
	   (RETURN-FROM CROCK (1+ I) (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))))
      (SETQ CH (AREF STRING I))
      (OR (EQ (SETQ TEM (LDB %%CH-FONT CH)) FONTX)	;Changing to a new font
	  (SHEET-SET-FONT SHEET (SETQ FONT (AREF (SHEET-FONT-MAP SHEET) (SETQ FONTX TEM)))))
      (SETQ WIDTH (SHEET-CHARACTER-WIDTH SHEET (SETQ CH (LDB %%CH-CHAR CH)) FONT))
      (COND ((> (+ (SHEET-CURSOR-X SHEET) WIDTH) RIGHT-LIMIT)	;This char won't fit
	     (AND MARGIN-FLAG (SHEET-TYO-RIGHT-MARGIN-CHARACTER SHEET
						(SHEET-CURSOR-X SHEET) OYPOS #/!))
	     (RETURN-FROM CROCK
			  (IF (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG SHEET)) I (1+ N))
			  (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET)))))
      (COND ((NULL (FONT-INDEXING-TABLE FONT))		;Let SHEET-TYO do big fonts
	     (AND (NULL (FONT-CHAR-WIDTH-TABLE FONT))
		  (NULL (FONT-LEFT-KERN-TABLE FONT))
		  (GO EZ))				;Handle easy fixed-width case fast
	     (GO VW)))					;Variable-width is a little slower
  HD1 (SHEET-TYO SHEET CH)
      (SETQ I (1+ I))
      (GO HD)
  
      ;;This loop is for simple fonts that don't need full hair of SHEET-TYO
  EZ  (SETQ XPOS (SHEET-CURSOR-X SHEET)
	    ALUF (SHEET-CHAR-ALUF SHEET))
  EZ0 (SETQ WIDTH (FONT-CHAR-WIDTH FONT)
	    YPOS (+ (SHEET-CURSOR-Y SHEET) (SHEET-BASELINE-ADJ SHEET)))
  EZ1 (OR (< CH 200) (GO EZX))				;Format effector, call full TYO
      (COND ((> (+ XPOS WIDTH) RIGHT-LIMIT)		;Form continuation line
	     (AND MARGIN-FLAG (SHEET-TYO-RIGHT-MARGIN-CHARACTER SHEET XPOS YPOS #/!))
	     (RETURN-FROM CROCK
			  (IF (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG SHEET)) I (1+ N))
			  (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (%DRAW-CHAR FONT CH XPOS YPOS ALUF SHEET)
      (SETQ XPOS (+ XPOS WIDTH))
      ;;Get next character, if any left
      (COND (( (SETQ I (1+ I)) N)
	     (SETF (SHEET-CURSOR-X SHEET) XPOS)		;Necessary?
	     (RETURN-FROM CROCK (1+ I) (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (SETQ CH (AREF STRING I)
	    TEM (LDB %%CH-FONT CH)
	    CH (LDB %%CH-CHAR CH))
      (AND (EQ TEM FONTX)
	   (GO EZ1))
      (SETQ FONTX TEM)					;Changing to a new font
      (SHEET-SET-FONT SHEET (SETQ FONT (AREF (SHEET-FONT-MAP SHEET) FONTX)))
      (AND (NULL (FONT-LEFT-KERN-TABLE FONT))
	   (NULL (FONT-INDEXING-TABLE FONT))
	   (NULL (FONT-CHAR-WIDTH-TABLE FONT))
	   (GO EZ0))					;Handle easy case fast
  EZX (SETF (SHEET-CURSOR-X SHEET) XPOS)
      (GO HD1)						;Go type out char and enter HD loop

      ;;This loop is for variable-width fonts
  VW  (SETQ XPOS (SHEET-CURSOR-X SHEET)
	    ALUF (SHEET-CHAR-ALUF SHEET)
	    FWT (FONT-CHAR-WIDTH-TABLE FONT)
	    LKT (FONT-LEFT-KERN-TABLE FONT)
	    YPOS (+ (SHEET-CURSOR-Y SHEET) (SHEET-BASELINE-ADJ SHEET)))
  VW1 (OR (< CH 200) (GO EZX))				;Format effector, call full TYO
      (AND FWT (SETQ WIDTH (AREF FWT CH)))
      (COND ((> (+ WIDTH XPOS) RIGHT-LIMIT)		;Won't fit in line
	     (AND MARGIN-FLAG (SHEET-TYO-RIGHT-MARGIN-CHARACTER SHEET XPOS YPOS #/!))
	     (RETURN-FROM CROCK
			  (IF (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG SHEET)) I (1+ N))
			  (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (%DRAW-CHAR FONT CH (IF LKT (- XPOS (AREF LKT CH)) XPOS) YPOS ALUF SHEET)
      (SETQ XPOS (+ XPOS WIDTH))
      ;;Get next character, if any left
      (COND (( (SETQ I (1+ I)) N)
	     (SETF (SHEET-CURSOR-X SHEET) XPOS)		;Necessary?
	     (RETURN-FROM CROCK (1+ I) (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (SETQ CH (AREF STRING I)
	    TEM (LDB %%CH-FONT CH)
	    CH (LDB %%CH-CHAR CH))
      (AND (EQ TEM FONTX) (GO VW1))
      (SETF (SHEET-CURSOR-X SHEET) XPOS)
      (GO HD)
      ))))

;Compute the motion that would be caused by outputting a string.
;This is used by the editor.
;Note that this does not use the "case shift" flavor of font hacking.
; Instead, it uses the 16-bit-character flavor that the editor uses.
; This means that if you give it an ordinary 8-bit string it will
; be assumed to be all in font 0.
;*** Actually the code seems to be confused about this. ***
;Args are: sheet, X and Y position to start at (NILs here use the current
; position of the sheet), string, and optionally the starting and ending indices
; and a flag saying to fake a CRLF at end of the string.
; Optionally you can give two additional arguments which are the X and Y to stop at,
; if not given these default to the end of the sheet.
;Returns 3 values: FINAL-X, FINAL-Y, and an indication of how far down the
; string it got.  this is NIL if the whole string (including the fake
; carriage return, if any) was processed without
; reaching the stopping point, or the index of the next character to be
; processed when the stopping point was reached, or T if the stopping point
; was reached after the fake carriage return.
;*** The interface to this crock should be redesigned.  Also note that the
;*** exact treatment of STOP-X and STOP-Y does not agree with SHEET-STRING-LENGTH.

(DEFUN SHEET-COMPUTE-MOTION (SHEET X Y STRING &OPTIONAL (START 0) (END NIL) (CR-AT-END-P NIL)
							(STOP-X 0) (STOP-Y NIL) BOTTOM-LIMIT)
  (PROG (RIGHT-LIMIT CWA CW CH FONT FONTX TEM I N NN II MARGIN-FLAG)
    (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET)) (SETQ MARGIN-FLAG T))
    (AND (NULL X) (SETQ X (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))))
    (AND (NULL Y) (SETQ Y (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))
    (AND (NULL STOP-Y)
	 (SETQ STOP-Y (1+ (SHEET-INSIDE-HEIGHT SHEET))))
		    ;   ^-- THIS 1+ IS SO CAN USE  RATHER THAN >
    (SETQ RIGHT-LIMIT (SHEET-INSIDE-WIDTH SHEET))
    (AND MARGIN-FLAG (SETQ RIGHT-LIMIT (- RIGHT-LIMIT (SHEET-CHAR-WIDTH SHEET))))
    (AND (NULL BOTTOM-LIMIT)
	 (SETQ BOTTOM-LIMIT (- (SHEET-INSIDE-HEIGHT SHEET) (SHEET-LINE-HEIGHT SHEET))))
    (SETQ I START
 	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  FONT (SHEET-CURRENT-FONT SHEET)
	  CW (FONT-CHAR-WIDTH FONT))
    ;;At this point, decide whether we can use the fast version
    (AND (= (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0)
	    ART-STRING)					;no font changes
	 (NULL (SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))	;and fixed width
	 (GO FAST))
    ;;This is the slow version.
SLOW
    (COND ((AND ( Y STOP-Y) ( X STOP-X))	;Reached sticking-point
	   (RETURN X Y I))
	  ((NOT (< I N))			;If string exhausted
	   (COND (CR-AT-END-P
		  (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))	;CRLF if told to
		  (AND (> Y BOTTOM-LIMIT) (SETQ Y 0))))
	   (RETURN X Y (AND ( X STOP-X) ( Y STOP-Y)))))
    (SETQ CH (LDB %%CH-CHAR (SETQ TEM (AREF STRING I))))
    (COND ((NEQ (SETQ TEM (LDB %%CH-FONT TEM)) FONTX)	;Changing fonts
	   (SETQ FONTX TEM
		 FONT (AREF (SHEET-FONT-MAP SHEET) FONTX)
		 CWA (FONT-CHAR-WIDTH-TABLE FONT)
		 CW (FONT-CHAR-WIDTH FONT))))
    (COND ((= CH #\CR)
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  ((< CH 200)				;Printing character
	   (SETQ X (+ (COND (CWA (AREF CWA CH)) (T CW)) X))) ;do char width
	  ((= CH #\TAB)				;Tab (have to do here since x-dependent)
	   (SETQ TEM (* 8 (SHEET-CHAR-WIDTH SHEET)))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  (T					;Format effector
	   (SETQ X (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)))))
    (COND ((> X RIGHT-LIMIT)			;If this character doesn't fit, crlf
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))	; and do it again
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  (T (SETQ I (1+ I))))
    (GO SLOW)

    ;;Here is the fast loop.  The basic idea is to scan as fast as possible
    ;;over printing characters, with all checking outside the loop.
FAST 
    ;;First, decide the most characters we want to scan over in a whack
    (SETQ NN (MIN (+ (// (- (COND (( Y STOP-Y)	;Stop-point is in this line
				   STOP-X)
				  (T RIGHT-LIMIT))	;Stop for this line is margin
			    X)
			 CW)
		     I)
		  N))				;NN is limiting value of I
    ;Now, scan over printing characters.
    (AND ( (SETQ II I) NN)			;Save initial I, and check for null loop
	 (GO SCX))
    (SETQ TEM 200)				;This is really a ridiculous bum
SCN (AND (< (AREF STRING I) TEM)		;If this is a printing character
	 (< (SETQ I (1+ I)) NN)			; and we haven't reached stop point
	 (GO SCN))				; then continue to loop (9 instructions)
    (SETQ X (+ (* (- I II) CW) X))		;Account for the motion of those chars
SCX (SETQ NN X)
    (COND ((AND ( Y STOP-Y) ( X STOP-X))	;If reached sticking-point, done.
	   (RETURN X Y I))
	  ((NOT (< I N))			;If string exhausted
	   (COND (CR-AT-END-P			;Do return X off end of line
		  (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))	;crlf if told to
		  (AND (> Y BOTTOM-LIMIT) (SETQ Y 0))))
	   (RETURN X Y (AND ( X STOP-X) ( Y STOP-Y)))))
    (COND ((= (SETQ CH (AREF STRING I)) #\CR)
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  ((< CH 200)				;Printing character
	   (SETQ X (+ CW X)))
	  ((= CH #\TAB)				;Tab (have to do here since x-dependent)
	   (SETQ TEM (* 8 (SHEET-CHAR-WIDTH SHEET)))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  (T					;Format effector
	   (SETQ X (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)))))
    (COND ((> X RIGHT-LIMIT)			;If this char didn't fit, crlf and do again
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  (T (SETQ I (1+ I))))
    (GO FAST)
))

(DEFUN SHEET-CHARACTER-WIDTH (SHEET CH FONT &AUX TEM)
  "Returns the width of a character, in raster units.
For backspace, it can return a negative number.
For tab, the number returned depends on the current cursor position.
For return, the result is zero."
  (COND ((< CH 200)				;Ordinary printing character
	 (COND ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT)) (AREF TEM CH))
	       (T (FONT-CHAR-WIDTH FONT))))
	((= CH #\CR) 0)				        ;Return
	((= CH #\TAB)				        ;Tab
	 (SETQ TEM (* 8 (SHEET-CHAR-WIDTH SHEET)))
	 (- (* (// (+ (SHEET-CURSOR-X SHEET) TEM) TEM) TEM)
	    (SHEET-CURSOR-X SHEET)))
	((AND (= CH #\BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
	 (MINUS (SHEET-CHAR-WIDTH SHEET)))		;Backspace
	((= CH #\FORM)					;Displays specially as <PAGE>
	 34.)
	((< CH 240)				        ;Misc invisible format effector
	 (+ (* (STRING-LENGTH (CAR (RASSOC CH SI:XR-SPECIAL-CHARACTER-NAMES))) 6) 10.))
	(T 0)))					        ;Font change or something

;;; This is like SHEET-COMPUTE-MOTION, but in one dimension only.
;;; Returned values are X and I, a cursor position and the index of the next
;;; character to be processed; I is the length of the string (or END) unless
;;; STOP-X is specified in which case it may be the index of the character
;;; which would have made the cursor > STOP-X.
(DEFUN SHEET-STRING-LENGTH (SHEET STRING &OPTIONAL (START 0) (END NIL) (STOP-X NIL)
						   (FONT (SHEET-CURRENT-FONT SHEET))
						   (START-X 0))
  (PROG (CWA CW CH FONTX TEM I N NN II STRINGP (X START-X))
    (SETQ I START
	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  CW (FONT-CHAR-WIDTH FONT))
    ;At this point, decide whether we can use the fast version
SLOW
    (AND (SETQ STRINGP (= (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0)
			  ART-STRING))			;i.e. no font changes
	 (NULL (SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))	;and fixed width
	 (GO FAST))
SLOW0
    (OR (< I N) (RETURN X I))			;If string exhausted
    (SETQ CH (LDB %%CH-CHAR (SETQ TEM (AREF STRING I))))
    (COND ((AND (NOT STRINGP)				;Changing fonts
		(NEQ (SETQ TEM (LDB %%CH-FONT TEM)) FONTX))
	   (SETQ FONTX TEM
		 FONT (AREF (SHEET-FONT-MAP SHEET) FONTX)
		 CWA (FONT-CHAR-WIDTH-TABLE FONT)
		 CW (FONT-CHAR-WIDTH FONT))))
    (COND ((< CH 200)					;Printing character
	   (SETQ NN (IF CWA (AREF CWA CH) CW)))
	  ((= CH #\TAB)
	   (SETQ TEM (* 8 (SHEET-CHAR-WIDTH SHEET)))
	   (SETQ NN (- (* (// (+ X TEM) TEM) TEM) X)))
	  ((AND (= CH #\BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
	   (SETQ NN (- (MAX 0 (- X (SHEET-CHAR-WIDTH SHEET))) X)))
	  ((= CH #\CR))					;Ignore it
	  ((< CH 240)					;Invisible format effector
	   (SETQ NN (SHEET-CHARACTER-WIDTH SHEET CH FONT))))
    (SETQ X (+ X NN))
    (AND STOP-X (> X STOP-X)			;If char doesn't fit, stop before it
	 (RETURN (- X NN) I))
    (SETQ I (1+ I))
    (GO SLOW)

    ;Here is the fast loop.  The basic idea is to scan as fast as possible
    ;over printing characters, with all checking outside the loop.
FAST 
    ;First, decide the most characters we want to scan over in a whack
    (SETQ NN (COND ((NULL STOP-X) N)		;NN is limiting value of I
                   ((MIN (+ (// (- STOP-X X)
                                CW)
                            I)
                         N))))
    ;Now, scan over printing characters.
    (AND ( (SETQ II I) NN)			;Save initial I, and check for null loop
	 (GO SLOW0))
    (SETQ TEM 200)				;This is really a ridiculous bum
SCN (AND (< (AREF STRING I) TEM)		;If this is a printing character
	 (< (SETQ I (1+ I)) NN)			; and we haven't reached stop point
	 (GO SCN))				; then continue to loop (9 instructions)
    (SETQ X (+ (* (- I II) CW) X))		;Account for the motion of those chars
    (GO SLOW0)					;Either string exhausted, non-printing,
						; or reached stop-x
))

(DEFUN SHEET-STRING-OUT-EXPLICIT (SHEET STRING X Y XLIM FONT ALU
					&OPTIONAL (START 0) (END NIL)
					&AUX FIT FWT LKT)
  "Output a special string (like a label) without exceptions or anything like that."
  (SETQ FIT (FONT-INDEXING-TABLE FONT)
	FWT (FONT-CHAR-WIDTH-TABLE FONT)
	LKT (FONT-LEFT-KERN-TABLE FONT))
  (PREPARE-SHEET (SHEET)
    (DO ((I START (1+ I))
	 (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	 (WIDTH (FONT-CHAR-WIDTH FONT))
	 (X X (+ X WIDTH))
	 (CH))
	((OR ( I N) ( (+ X WIDTH) XLIM))
	 (PROG () (RETURN X I)))
      (SETQ CH (AREF STRING I))
      (AND (> CH 200) (FERROR NIL "SHEET-STRING-OUT-EXPICIT cannot handle ~C" CH))
      (AND LKT (SETQ X (- X (AREF LKT CH))))
      (IF FIT
	  (DO ((CH (AREF FIT CH) (1+ CH))
	       (LIM (AREF FIT (1+ CH)))
	       (BPP (SHEET-BITS-PER-PIXEL SHEET))
	       (X X (+ X (// (FONT-RASTER-WIDTH FONT) BPP))))
	      (( CH LIM))
	    (%DRAW-CHAR FONT CH X Y ALU SHEET))
	  (%DRAW-CHAR FONT CH X Y ALU SHEET))
      (AND FWT (SETQ WIDTH (AREF FWT CH))))))

;;; This function displays a string centered between two X coordinates, truncated if necessary
;;; The arguments are relative to the margins, as usual.
(DEFUN SHEET-DISPLAY-CENTERED-STRING (SHEET STRING
				      &OPTIONAL (LEFT 0) (RIGHT (SHEET-INSIDE-WIDTH SHEET))
				                (Y-POS (- (SHEET-CURSOR-Y SHEET)
							  (SHEET-INSIDE-TOP SHEET)))
				      &AUX WID SWID SLEN)
  (SETQ WID (- RIGHT LEFT)
	STRING (STRING STRING))
  (MULTIPLE-VALUE (SWID SLEN)  ;Compute how wide the string is, and whether to truncate
     (SHEET-STRING-LENGTH SHEET STRING 0 NIL WID))
  ;; SHEET-SET-CURSORPOS takes arguments in a different coordinate system
  (SHEET-SET-CURSORPOS SHEET (+ LEFT (MAX (// (- WID SWID) 2) 0)) Y-POS)
  (SHEET-STRING-OUT SHEET STRING 0 SLEN))

(DEFUN SHEET-DISPLAY-X-Y-CENTERED-STRING (SHEET STRING
					  &OPTIONAL (LEFT 0) (TOP 0)
					            (RIGHT (SHEET-INSIDE-WIDTH SHEET))
						    (BOTTOM (SHEET-INSIDE-HEIGHT SHEET))
						    (FNT (SHEET-CURRENT-FONT SHEET)))
  "Display a string centered in both X and Y.
  Note that the coordinates of the box in which it is centered are relative to the margins"
  (LET ((HT (FONT-BASELINE FNT))
	(WID (- RIGHT LEFT)))
    (MULTIPLE-VALUE-BIND (SWID SLEN)
	(SHEET-STRING-LENGTH SHEET STRING 0 NIL WID FNT)
      (SHEET-STRING-OUT-EXPLICIT SHEET STRING
				 (+ (SHEET-INSIDE-LEFT SHEET) LEFT
				    (MAX (// (- WID SWID) 2) 0))
				 (+ (SHEET-INSIDE-TOP SHEET)
				    (MAX (- (// (+ TOP BOTTOM) 2) (// HT 2)) TOP))
				 (+ (SHEET-INSIDE-LEFT SHEET) RIGHT)
				 FNT (SHEET-CHAR-ALUF SHEET) 0 SLEN))))

(DEFUN SHEET-DISPLAY-LOSENGED-STRING (SHEET STRING)
  (LET ((WIDTH (+ 10. (* (STRING-LENGTH STRING) 6))))
    ;; Make sure there is enough room on the line, if not CRLF and
    ;; hope the sheet isn't too narrow.  Relies on the fact that handling
    ;; of all exceptions leaves you no further to the right than you were
    ;; (usually at the left margin).
    (PREPARE-SHEET (SHEET)
      (OR ( (+ (SHEET-CURSOR-X SHEET) WIDTH)
	     (IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET)) (SHEET-INSIDE-RIGHT SHEET)
		 (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-WIDTH SHEET))))
	  (FUNCALL SHEET ':END-OF-LINE-EXCEPTION))
      (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	  (SHEET-HANDLE-EXCEPTIONS SHEET))
      ;; Put the string then the box around it
      (LET ((X0 (SHEET-CURSOR-X SHEET))
	    (Y0 (1+ (SHEET-CURSOR-Y SHEET)))
	    (X1 (+ (SHEET-CURSOR-X SHEET) (1- WIDTH)))
	    (Y1 (+ (SHEET-CURSOR-Y SHEET) 9))
	    (ALUF (SHEET-CHAR-ALUF SHEET)))
	(SHEET-STRING-OUT-EXPLICIT SHEET STRING (+ X0 5) (+ Y0 2)
				   X1
				   (FUNCALL (SHEET-GET-SCREEN SHEET)
					    ':PARSE-FONT-DESCRIPTOR FONTS:5X5)
				   (SHEET-CHAR-ALUF SHEET))
	(%DRAW-RECTANGLE (- WIDTH 8) 1 (+ X0 4) Y0 ALUF SHEET)
	(%DRAW-RECTANGLE (- WIDTH 8) 1 (+ X0 4) Y1 ALUF SHEET)
	(%DRAW-LINE X0 (+ Y0 4) (+ X0 3) (1+ Y0) ALUF T SHEET)
	(%DRAW-LINE (1+ X0) (+ Y0 5) (+ X0 3) (1- Y1) ALUF T SHEET)
	(%DRAW-LINE X1 (+ Y0 4) (- X1 3) (1+ Y0) ALUF T SHEET)
	(%DRAW-LINE (1- X1) (+ Y0 5) (- X1 3) (1- Y1) ALUF T SHEET)
	(SETF (SHEET-CURSOR-X SHEET) (1+ X1))))))

;Before making our first screen, compile any methods it requires
;Also do Sheet now, since it actually does get instantiated, e.g. for the who-line
(COMPILE-FLAVOR-METHODS SHEET SCREEN)

;;;Set things up
(DEFUN INITIALIZE ()
  (SHEET-CLEAR-LOCKS)
  (AND (BOUNDP 'WHO-LINE-WINDOW) (SHEET-CLEAR-LOCKS-INTERNAL WHO-LINE-WINDOW))
  ;; Set up screen and sheet for the main monitor (CPT typically)
  (AND (NOT (BOUNDP 'MAIN-SCREEN))
       (SETQ MAIN-SCREEN
	     (WINDOW-CREATE 'SCREEN
			    ':NAME "MAIN-SCREEN"
			    ':DEFAULT-FONT FONTS:CPTFONT
			    ':BUFFER (LSH 77 18.)
			    ':CONTROL-ADDRESS 377760
			    ':PROPERTY-LIST '(:VIDEO :BLACK-AND-WHITE
					      :CONTROLLER :SIMPLE)
			    ':HEIGHT 896. ':WIDTH 768.)))
  (SETQ MOUSE-SHEET MAIN-SCREEN)
  (OR (BOUNDP 'ALL-THE-SCREENS)
      (SETQ ALL-THE-SCREENS NIL))
  (OR (MEMQ MAIN-SCREEN ALL-THE-SCREENS)
      (PUSH MAIN-SCREEN ALL-THE-SCREENS))
  (FUNCALL MAIN-SCREEN ':EXPOSE)
  (AND (BOUNDP 'COLOR:COLOR-SCREEN)	;Decide whether we have color hardware before using it!
       (FUNCALL COLOR:COLOR-SCREEN ':EXPOSE))
  (SETQ DEFAULT-SCREEN MAIN-SCREEN
	SCREEN-MANAGER-TOP-LEVEL T
	SCREEN-MANAGER-QUEUE NIL)
  ;Screen-manage commented out because it only causes trouble
  ;(DOLIST (S ALL-THE-SCREENS)
  ;  (FUNCALL S ':SCREEN-MANAGE))
  )

(DEFUN DEFINE-SCREEN (FLAVOR NAME &REST ARGS)
  (LET ((SCREEN (LEXPR-FUNCALL #'WINDOW-CREATE FLAVOR ':NAME NAME ARGS)))
    (PUSH SCREEN ALL-THE-SCREENS)
    (FUNCALL SCREEN ':EXPOSE)
    SCREEN))

(ADD-INITIALIZATION "SHEET" '(INITIALIZE) '(:ONCE))

(DEFVAR INITIAL-LISP-LISTENER)

;This function is called from an initialization in COMETH
(DEFUN WINDOW-INITIALIZE ()
  (INITIALIZE)
  (WHO-LINE-CLOBBERED)
  (SETQ KBD-TYI-HOOK NIL PROCESS-IS-IN-ERROR NIL)
  (OR (EQ WHO-LINE-PROCESS SI:INITIAL-PROCESS)	;So it stays latched here during loading
      (SETQ WHO-LINE-PROCESS NIL))
  (OR INITIAL-LISP-LISTENER	;Set to NIL in LTOP
      (SETQ INITIAL-LISP-LISTENER (WINDOW-CREATE 'LISP-LISTENER
						 ':PROCESS SI:INITIAL-PROCESS)))
  (FUNCALL INITIAL-LISP-LISTENER ':SELECT)
  (FUNCALL TERMINAL-IO ':HOME-CURSOR)
  (FUNCALL TERMINAL-IO ':CLEAR-EOL)
  (AND SI:WARM-BOOTED-PROCESS
       (FORMAT T "Warm boot while running ~S.~%Its variable bindings remain in effect; ~
 its unwind-protects have been lost.~%"
	       SI:WARM-BOOTED-PROCESS))
  (OR (MEMQ 'TV:BLINKER-CLOCK SI:CLOCK-FUNCTION-LIST)
      (PUSH 'TV:BLINKER-CLOCK SI:CLOCK-FUNCTION-LIST))
  (INSTALL-MY-KEYBOARD))

;;; HAIRY WHO-LINE SYSTEM.

; WHO-LINE-LIST is a list of fields, each field has the following
; format, the WHO-LINE-ITEM defstruct:
;	(func state left right ...)
; func is a function to call, given the field as an argument.  If the state
; is NIL it always updates, otherwise it updates if the "state has changed".
; The ... is additional info depending on the func.  left and right are the
; portion of the who line occupied by this field.

(DEFUN WHO-LINE-SETUP ()
  (COND ((NOT (BOUNDP 'WHO-LINE-WINDOW))
	 (WITHOUT-SCREEN-MANAGEMENT
	  (LET ((HEIGHT (SHEET-HEIGHT DEFAULT-SCREEN)))
	   (SETQ WHO-LINE-WINDOW
		 (WINDOW-CREATE 'SHEET
				':NAME "WHO-LINE"
				':TOP (- HEIGHT (FONT-CHAR-HEIGHT
						  (SHEET-CURRENT-FONT DEFAULT-SCREEN)))
				':BOTTOM HEIGHT ':VSP 0 ':BLINKER-P NIL ':MORE-P NIL
				':DEEXPOSED-TYPEOUT-ACTION ':EXPOSE)))
	  (PUSH WHO-LINE-WINDOW (SHEET-INFERIORS DEFAULT-SCREEN))
	  (FUNCALL WHO-LINE-WINDOW ':EXPOSE)
	 ;; This is so updating the who line does not cause all blinkers to flicker
	 ;; it will tend to leave turds in the who line, but this is not as bad.
	  (SETF (SHEET-INFERIORS DEFAULT-SCREEN)
		(DELQ WHO-LINE-WINDOW (SHEET-INFERIORS DEFAULT-SCREEN)))
	  (SETF (SHEET-EXPOSED-INFERIORS DEFAULT-SCREEN)
		(DELQ WHO-LINE-WINDOW (SHEET-EXPOSED-INFERIORS DEFAULT-SCREEN)))
	  (SETF (SHEET-SUPERIOR WHO-LINE-WINDOW) NIL)
	  (SETF (SHEET-BOTTOM-MARGIN-SIZE DEFAULT-SCREEN) (SHEET-HEIGHT WHO-LINE-WINDOW)))))
  ;; SETQ'ing WHO-LINE-LIST enables the scheduler to start updating the who-line
  (SETQ WHO-LINE-LIST
	'(		;0-144. SHOULD BE THE TIME  (18 chars)
	  (WHO-LINE-USER-OR-PROCESS NIL 144. 248.)	;(13 chars plus 1 space)
	  (WHO-LINE-PACKAGE NIL 256. 400.)		;(18 chars)
	  (WHO-LINE-STRING NIL 400. 480. WHO-LINE-RUN-STATE)	;(10 chars)
	  (FS:WHO-LINE-FILE-STATE NIL 480. 768. NIL NIL))))	;(36 chars)

;This function updates all fields of the who-line that need it.
(DEFUN WHO-LINE-UPDATE (&OPTIONAL RUN-STATE-ONLY-P &AUX RL)
  (OR INHIBIT-WHO-LINE
      (SHEET-LOCK WHO-LINE-WINDOW)	;Don't block inside scheduler 
      (NOT (ZEROP (SHEET-OUTPUT-HOLD-FLAG WHO-LINE-WINDOW)))
      (WITHOUT-INTERRUPTS
	(SETQ RL (%XBUS-READ WHO-LINE-RUN-LIGHT-LOC))	;Don't clobber run light
	(DOLIST (ITEM WHO-LINE-LIST)			;For each field of the who line
	  (AND (OR (NOT RUN-STATE-ONLY-P)
		   (EQ (WHO-LINE-STRING-ITEM-SYMBOL ITEM) 'WHO-LINE-RUN-STATE))
	       (FUNCALL (CAR ITEM) ITEM)))
	(%XBUS-WRITE WHO-LINE-RUN-LIGHT-LOC RL)))
  T)

;This function clears and positions, in preparation for update of a who-line field
(DEFUN WHO-LINE-PREPARE-FIELD (ITEM)
  (WITHOUT-INTERRUPTS
    (SHEET-SET-CURSORPOS WHO-LINE-WINDOW (WHO-LINE-ITEM-LEFT ITEM) 0)
    (%DRAW-RECTANGLE (- (WHO-LINE-ITEM-RIGHT ITEM) (WHO-LINE-ITEM-LEFT ITEM))
		     (SHEET-LINE-HEIGHT WHO-LINE-WINDOW)
		     (WHO-LINE-ITEM-LEFT ITEM) 0
		     (SHEET-ERASE-ALUF WHO-LINE-WINDOW)
		     WHO-LINE-WINDOW)))

(DEFUN WHO-LINE-CLOBBERED ()
  (DOLIST (ITEM WHO-LINE-LIST)
    (SETF (WHO-LINE-ITEM-STATE ITEM) NIL)))

;Update Functions.
(DEFSTRUCT (WHO-LINE-STRING-ITEM :LIST (:INCLUDE WHO-LINE-ITEM) (:CONSTRUCTOR NIL))
  WHO-LINE-STRING-ITEM-SYMBOL)

(DEFUN WHO-LINE-STRING (ITEM &OPTIONAL (VAL (SYMEVAL (WHO-LINE-STRING-ITEM-SYMBOL ITEM))))
  (COND ((NEQ (WHO-LINE-ITEM-STATE ITEM) VAL)
	 (WHO-LINE-PREPARE-FIELD ITEM)
	 (SHEET-STRING-OUT WHO-LINE-WINDOW VAL
			0 (MIN (STRING-LENGTH VAL)
			       (// (- (WHO-LINE-ITEM-RIGHT ITEM)
				      (WHO-LINE-ITEM-LEFT ITEM))
				   (SHEET-CHAR-WIDTH WHO-LINE-WINDOW))))
	 (SETF (WHO-LINE-ITEM-STATE ITEM) VAL))))

(DEFUN WHO-LINE-USER-OR-PROCESS (ITEM)
  (WHO-LINE-STRING ITEM (IF WHO-LINE-PROCESS (PROCESS-NAME WHO-LINE-PROCESS) USER-ID)))

(DEFUN WHO-LINE-PACKAGE (ITEM &AUX VAL SG)
  (LET ((PKG (COND ((SETQ LAST-WHO-LINE-PROCESS (OR WHO-LINE-PROCESS
						    (AND SELECTED-IO-BUFFER
							 (IO-BUFFER-LAST-OUTPUT-PROCESS
							   SELECTED-IO-BUFFER))))
		    (SETQ SG (PROCESS-STACK-GROUP LAST-WHO-LINE-PROCESS))
		    (COND ((EQ SG %CURRENT-STACK-GROUP) PACKAGE)
			  ((TYPEP SG ':STACK-GROUP) (SYMEVAL-IN-STACK-GROUP 'PACKAGE SG))
			  (T PACKAGE))))))
    (COND ((AND PKG (ARRAYP PKG)
		(NEQ (WHO-LINE-ITEM-STATE ITEM) (SETQ VAL (PKG-NAME PKG))))
	   (WHO-LINE-PREPARE-FIELD ITEM)
	   (SHEET-STRING-OUT WHO-LINE-WINDOW VAL
			     0 (MIN (STRING-LENGTH VAL)
				    (1- (// (- (WHO-LINE-ITEM-RIGHT ITEM)
					       (WHO-LINE-ITEM-LEFT ITEM))
					    (SHEET-CHAR-WIDTH WHO-LINE-WINDOW)))))
	   (SHEET-TYO WHO-LINE-WINDOW #/:)
	   (SETF (WHO-LINE-ITEM-STATE ITEM) VAL)))))

(DEFUN WHO-LINE-RUN-STATE-UPDATE (&AUX P)  ;Separate variable since other can be setq'ed
					   ;asynchronously by other processes
  (SETQ LAST-WHO-LINE-PROCESS
	(SETQ P (OR WHO-LINE-PROCESS
		    (AND SELECTED-IO-BUFFER
			 (IO-BUFFER-LAST-OUTPUT-PROCESS SELECTED-IO-BUFFER)))))
  (SETQ WHO-LINE-RUN-STATE (COND ((NULL P) "NIL")
				 ((ASSQ P ACTIVE-PROCESSES)
				  (PROCESS-WHOSTATE P))
				 ((NOT (NULL (SI:PROCESS-ARREST-REASONS P)))
				  "ARREST")
				 (T "STOP")))
  (WHO-LINE-UPDATE T))

(ADD-INITIALIZATION "WHO LINE" '(WHO-LINE-SETUP) '(:ONCE))

(DEFUN SET-TV-SPEED (FREQUENCY)
  "Set the TV refresh rate.  The default is 64.69.  Returns the number of display lines."
  ;; Try not to burn up the monitor
  (CHECK-ARG FREQUENCY (AND (> FREQUENCY 54.) (< FREQUENCY 76.))
	     "a number between 55. and 75.")
  ;; Here each horizontal line is 32. sync clocks, or 16.0 microseconds with a 64 MHz clock.
  ;; The number of lines per frame is 70. overhead lines plus enough display lines
  ;; to give the desired rate.
  (LET ((N-LINES (- (FIX (// 1e6 (* 16. FREQUENCY))) 70.))
	(OLD-BOTTOM (SHEET-INSIDE-HEIGHT DEFAULT-SCREEN)) NEW-BOTTOM)
    (SI:SETUP-CPT
      (APPEND '(1.  (1 33) (5 13) 12 12 (11. 12 12) 212 113)	;VERT SYNC, CLEAR TVMA
	      '(53. (1 33) (5 13) 12 12 (11. 12 12) 212 13)	;VERT RETRACE
	      '(8.  (1 31)  (5 11) 11 10 (11. 0 0) 200 21)	;8 LINES OF MARGIN
	      (DO ((L NIL (APPEND L `(,DN (1 31) (5 11) 11 50 (11. 0 40) 200 21)))
		   (N N-LINES (- N DN))
		   (DN))
		  ((ZEROP N) L)
		(SETQ DN (MIN 255. N)))
	      '(7. (1 31) (5 11) 11 10 (11. 0 0) 200 21)
	      '(1. (1 31) (5 11) 11 10 (11. 0 0) 300 23))
      (SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)
      T)
    ;; Move the who-line, and change the dimensions of default screen
    (SETF (SHEET-Y-OFFSET WHO-LINE-WINDOW)
	  (- N-LINES (FONT-CHAR-HEIGHT (SHEET-CURRENT-FONT WHO-LINE-WINDOW))))
    (SETF (SHEET-HEIGHT DEFAULT-SCREEN) N-LINES)
    (%P-STORE-CONTENTS-OFFSET (// (* (SHEET-WIDTH DEFAULT-SCREEN) N-LINES) 16.)
			      (SYMEVAL-IN-INSTANCE DEFAULT-SCREEN 'BUFFER-HALFWORD-ARRAY)
			      3)
    (%P-STORE-CONTENTS-OFFSET (* (SHEET-WIDTH DEFAULT-SCREEN) N-LINES)
			      (SHEET-SCREEN-ARRAY DEFAULT-SCREEN) 3)
    (SETQ NEW-BOTTOM (SHEET-INSIDE-HEIGHT DEFAULT-SCREEN))
    (AND (> NEW-BOTTOM OLD-BOTTOM)
	 ;; If screen got bigger, erase old who-line and newly-appeared space
	 (%DRAW-RECTANGLE (SHEET-WIDTH DEFAULT-SCREEN) (- N-LINES OLD-BOTTOM)
			  0 OLD-BOTTOM ALU-SETZ DEFAULT-SCREEN))
    ;;*** This doesn't work because it can make the size negative.  The :VERIFY option
    ;;*** does not detect this, but it blows out later.  This needs to be done over
    ;;*** to do some hairier hacking of the edges, but I don't want to work on it more now.
    (DOLIST (W (SHEET-INFERIORS DEFAULT-SCREEN))
      (LET ((BOTTOM (+ (SHEET-Y-OFFSET W) (SHEET-HEIGHT W))))
	(AND ( BOTTOM OLD-BOTTOM)
	     (MEMQ ':SET-SIZE (FUNCALL W ':WHICH-OPERATIONS))
	     (FUNCALL W ':SET-SIZE (SHEET-WIDTH W) (- NEW-BOTTOM (SHEET-Y-OFFSET W)) ':VERIFY)
	     (FUNCALL W ':SET-SIZE (SHEET-WIDTH W) (- NEW-BOTTOM (SHEET-Y-OFFSET W))))))
    (SETQ %DISK-RUN-LIGHT (+ (- (* N-LINES (SHEET-LOCATIONS-PER-LINE DEFAULT-SCREEN)) 15)
			     (LSH 77 18.)))
    (WHO-LINE-CLOBBERED)
    N-LINES))
