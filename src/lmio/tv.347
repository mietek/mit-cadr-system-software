;;; LISP MACHINE TV ROUTINES			-*-Mode:Lisp; Package:SI-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;THIS FILE IS THE STUFF TO GO IN THE COLD LOAD, TVWARM IS THE FILE
;CONTAINING STUFF TO BE LOADED LATER SINCE IT ISN'T NEEDED TO GET THINGS STARTED.

;SEE FILE LMIO; TVDEFS > FOR STRUCTURES (SEPARATE FILE SO CAN BE AVAILABLE TO COMPILER)
;      THE FILE ALSO CONTAINS MOST SPECIAL VARIABLE DECLARATIONS

;THIS IS THE NEW (8/27/78) VERSION, PRIMARILY FOR 32-BIT TV'S SUCH AS C.P.T.
;--- HERE INSERT NOTES ON BASIC WAY THIS WORKS, ESP. BLINKER STUFF ---

;SUPER-OLD NOTES:
;WHAT HAPPENS ON LINE/SCREEN WRAP-AROUND.
;	LINE TRUNCATION/AUTO CRLF/AUTO CRLF WITH INDICATION/HORIZONTAL WINDOWING
;	SCREEN WRAP-AROUND/MORE PROCESSING/SCROLLING (DONE SOMEWHAT)
;  WE'RE PREPARING FOR THIS WITH PC-PPR-FLAGS
;SUBROUTINE TO SWITCH PC PPR BETWEEN NORMAL AND REVERSE VIDEO.
; REQUIRES DOING A CLEAR-EOL.

;MICROCODE PRIMITIVES:
;(TV-SELECT-SCREEN SCREEN) SETS VARIOUS VARIABLES NEEDED BY THE TWO MICROCODE
;FUNCTIONS BELOW.  TV-CURRENT-SCREEN IS A VARIABLE WHICH YOU SHOULDN'T WRITE
;EXCEPT BY CALLING TV-SELECT-SCREEN.  ALSO (AS IS CURRENTLY PLANNED ANYWAY)
;YOU SHOULDN'T CALL THIS INTERRUPTIBLY, OTHERWISE MORE HAIR WILL BE NEEDED.
;FOR 16-BIT TVS THIS TAKES CARE OF SELECTING THE RIGHT PLANE.

;(TV-DRAW-CHAR FONT-ARRAY-PNTR CHAR-CODE X-BIT-POS Y-BIT-POS ALU-FUNC)
;THE X-BIT-POS AND Y-BIT-POS ARE OF THE TOP LEFT CORNER OF THE CHARACTER.
; (0,0) IS THE TOP LEFT CORNER OF THE SCREEN
;THE ALU-FUNC IS SUITABLE FOR OA-REG-LOW.  GOOD VALUES ARE TV-ALU-IOR AND LIKE THAT.

;(TV-ERASE WIDTH HEIGHT X-BIT-POS Y-BIT-POS ALU-FUNC)
;WIDTH AND HEIGHT ARE IN BITS.  A RECTANGLE OF THE INDICATED
;SIZE, OF ALL 1S, IS CREATED AND STORED INTO THE SPECIFIED
;PART OF THE TV BUFFER USING THE SPECIFIED ALU-FUNC.  USUALLY
;THE ANDCA FUNCTION IS USED FOR ERASING, BUT XOR COULD BE USED
;FOR THE BLINKING CURSOR ETC.  NOTE HEIGHT MUST BE > 0.

(DECLARE (SPECIAL FONTS:CPTFONT))

;;; Make a blinker temporarily disappear from the screen.
;;; Anything that moves it or changes its parameters should call this.
;;; When the next clock interrupt happens with INHIBIT-SCHEDULING-FLAG clear,
;;; the blinker will come back on.  This is independent of the time until next
;;; blink, in order to provide the appearance of fast response.
;;; Anyone who calls this should have lambda-bound INHIBIT-SCHEDULING-FLAG to T.
(DEFUN TV-OPEN-BLINKER (BLINKER)
  (OR INHIBIT-SCHEDULING-FLAG			;These 2 lines are temporary ---
      (FERROR NIL "INHIBIT-SCHEDULING-FLAG off in TV-OPEN-BLINKER"))
  (COND ((TV-BLINKER-PHASE BLINKER)		;If blinker is visible,
	 (TV-BLINK BLINKER NIL)			; make it invisible.
	 (SETF (TV-BLINKER-TIME-UNTIL-BLINK BLINKER) 0))))	;Come back on asap.

;;; This routine shuts off all blinkers, preparatory to arbitrary mungage of the screen.
(DEFUN TV-OPEN-SCREEN ()
  (MAPC (FUNCTION TV-OPEN-BLINKER) TV-ROVING-BLINKER-LIST)	;This should do it
  (MAPC #'(LAMBDA (PP)
	    (MAPC #'TV-OPEN-BLINKER (PC-PPR-BLINKER-LIST PP)))
	EXPOSED-PC-PPRS)
  (MAPC #'(LAMBDA (PP)
	    (MAPC #'TV-OPEN-BLINKER (PC-PPR-BLINKER-LIST PP)))
	SELECTED-PC-PPRS)
  T)

;;; This routine is called from the clock every 60th (currently) of a second,
;;; except when INHIBIT-SCHEDULING-FLAG is set.  It is consequently uninterruptible.
;;; Any blinkers which are supposed to be on but are off are turned on.
;;; Any blinkers which are supposed to be flashed are flashed if it is time.
;;; Note: we depend on the fact that blinkers temporarily turned off
;;; have their TV-BLINKER-TIME-UNTIL-BLINK fields set to 0.
(DEFUN TV-BLINKER-CLOCK ()
  ;; For a merely exposed pc-ppr, just turn on blinkers if off.
  ;; If blinker wants to blink, just turn it on, don't blink it.
  (DO ((PPL EXPOSED-PC-PPRS (CDR PPL))) ((NULL PPL))
    (DO ((BL (PC-PPR-BLINKER-LIST (CAR PPL)) (CDR BL))) ((NULL BL))
      (OR (EQ (NOT (NULL (TV-BLINKER-VISIBILITY (CAR BL))))
	      (TV-BLINKER-PHASE (CAR BL)))
	  (TV-BLINK (CAR BL) (NOT (NULL (TV-BLINKER-VISIBILITY (CAR BL))))))))
  ;; For blinkers of selected pc-pprs, and for roving blinkers,
  ;; turn the blinker on or flash it, whichever the blinker says.
  (DO ((PPL SELECTED-PC-PPRS (CDR PPL))) ((NULL PPL))
    (DO ((BL (PC-PPR-BLINKER-LIST (CAR PPL)) (CDR BL))) ((NULL BL))
      (LET ((TEM (TV-BLINKER-TIME-UNTIL-BLINK (CAR BL))))
	(COND ((NULL TEM))			;Not enabled for blinking, ignore this one
	      ((<= (SETF (TV-BLINKER-TIME-UNTIL-BLINK (CAR BL))
			 (1- TEM))		;If it's time to blink, blink it appropriately
		   0)
	       (TV-BLINK (CAR BL) (TV-BLINKER-VISIBILITY (CAR BL))))))))
  (DO ((BL TV-ROVING-BLINKER-LIST (CDR BL))) ((NULL BL))
    (LET ((TEM (TV-BLINKER-TIME-UNTIL-BLINK (CAR BL))))
      (COND ((NULL TEM))			;Not enabled for blinking, ignore this one
	    ((<= (SETF (TV-BLINKER-TIME-UNTIL-BLINK (CAR BL))
		       (1- TEM))		;If it's time to blink, blink it appropriately
		 0)
	     (TV-BLINK (CAR BL) (TV-BLINKER-VISIBILITY (CAR BL))))))))

;;; This is the routine to blink a blinker.  It must not be called interruptibly,
;;; i.e. either call it from the TV-BLINKER-CLOCK or with INHIBIT-SCHEDULING-FLAG set.
;;; The second argument is one of the symbols NIL, T, or BLINK, to make
;;; it invisible, make it solidly visible, or take it to its next step
;;; of blinkage.  This routine takes care of common things and calls
;;; a routine dependent on the specific type of blinker.
;;; We let the specific routine handle the TV-BLINKER-PHASE in case it does something weird.
(DEFUN TV-BLINK (BLINKER NEW-PHASE &AUX X Y PC-PPR)
   (SETF (TV-BLINKER-TIME-UNTIL-BLINK BLINKER)	;Schedule next blink
	 (AND (EQ NEW-PHASE 'BLINK)			;No blinking if NEW-PHASE not BLINK
	      (TV-BLINKER-HALF-PERIOD BLINKER)))
   (COND ((SETQ X (TV-BLINKER-X-POS BLINKER))	;Find where it is, may track pc ppr.
	  (SETQ Y (TV-BLINKER-Y-POS BLINKER)))
	 (T
	  (SETQ PC-PPR (TV-BLINKER-PC-PPR BLINKER))
	  (SETQ X (MIN (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-RIGHT-LIMIT PC-PPR)))
	  (SETQ Y (MIN (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BOTTOM-LIMIT PC-PPR)))))
   (TV-SELECT-SCREEN (TV-BLINKER-SCREEN BLINKER))	;Select proper screen
   ;; Unless blinker is already in proper phase, tell it to switch.
   (OR (EQ (TV-BLINKER-PHASE BLINKER) NEW-PHASE)
       (FUNCALL (TV-BLINKER-FUNCTION BLINKER) BLINKER NEW-PHASE X Y)))

;TV-BLINKER-FUNCTION function for rectangular blinkers (the default).
;Ignores the NEW-PHASE, just complements.
(DEFUN TV-RECTANGULAR-BLINKER (BLINKER NEW-PHASE X Y)
  NEW-PHASE
  (SETF (TV-BLINKER-PHASE BLINKER) (NOT (TV-BLINKER-PHASE BLINKER)))
  (COND ((NOT (TV-BLINKER-SIDEWAYS-P BLINKER))
	 (TV-ERASE (TV-BLINKER-WIDTH BLINKER)
		   (TV-BLINKER-HEIGHT BLINKER)
		   X
		   Y
		   TV-ALU-XOR))
	(T (TV-ERASE (TV-BLINKER-HEIGHT BLINKER)
		     (TV-BLINKER-WIDTH BLINKER)
		     (- (SCREEN-HEIGHT (TV-BLINKER-SCREEN BLINKER))
			Y
			(TV-BLINKER-HEIGHT BLINKER))
		     X
		     TV-ALU-XOR))))

;;; Draw a printing character, or execute a special format character.
(DEFUN TV-TYO (PC-PPR CHAR &AUX TEM FONT KERN-TABLE (KERN 0))
  (IF ( CHAR 200)	;Don't get screwed by things like CR at end of line
      (COND ((= CHAR 215)
             (TV-CRLF PC-PPR))
            ((= CHAR 210)
             (TV-BACKSPACE PC-PPR))
            ((= CHAR 211)
             (TV-TAB PC-PPR))
            ((< CHAR 240)			;Invisible format effector
             (SETQ FONT (PC-PPR-CURRENT-FONT PC-PPR))
             (TV-SET-FONT PC-PPR FONTS:ARROW)	;Display a magic character
	     ;; These characters are wider than normal, so the normal right-margin
	     ;; checking won't work.  Don't bother with special bottom-margin checking,
	     ;; they are only higher than a ridiculously-small font.
	     (AND (> (+ (PC-PPR-CURRENT-X PC-PPR)
			(AREF (FONT-CHAR-WIDTH-TABLE FONTS:ARROW) (- CHAR 40)))
		     (PC-PPR-RIGHT-MARGIN PC-PPR))
		  (SETF (PC-PPR-END-LINE-FLAG PC-PPR) 1))
             (TV-TYO PC-PPR (- CHAR 40))	;200-237 -> 140-177
             (TV-SET-FONT PC-PPR FONT))		;Restore font
            ((< CHAR 250)			;Mapped font change
             (TV-SET-FONT PC-PPR (AR-1 (PC-PPR-FONT-MAP PC-PPR) (- CHAR 240))))
            ((= CHAR 250) )			;Absolute font change
            )
      (TV-PREPARE-PC-PPR (PC-PPR)
	(OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR))
            (TV-EXCEPTION PC-PPR))
        (SETQ FONT (PC-PPR-CURRENT-FONT PC-PPR))
        (AND (SETQ KERN-TABLE (FONT-LEFT-KERN-TABLE FONT))
             (SETQ KERN (AREF KERN-TABLE CHAR)))
        (COND ((NULL (SETQ TEM (FONT-INDEXING-TABLE FONT)))
               (COND ((ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))
                      (TV-DRAW-CHAR FONT CHAR (- (PC-PPR-CURRENT-X PC-PPR) KERN)
                                    (+ (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BASELINE-ADJ PC-PPR))
                                    (PC-PPR-CHAR-ALUF PC-PPR)))
                     ((TV-DRAW-CHAR FONT CHAR
                                    (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR))
                                       (+ (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BASELINE-ADJ PC-PPR))
                                       KERN
                                       (FONT-RASTER-WIDTH FONT))
                                    (PC-PPR-CURRENT-X PC-PPR)
                                    (PC-PPR-CHAR-ALUF PC-PPR)))))
              ;; Wide character, draw several columns
              ((ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))
               (DO ((CH (AR-1 TEM CHAR) (1+ CH))
                    (LIM (AR-1 TEM (1+ CHAR)))
                    (XPOS (- (PC-PPR-CURRENT-X PC-PPR) KERN)
                          (+ XPOS (FONT-RASTER-WIDTH FONT)))
                    (YPOS (+ (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BASELINE-ADJ PC-PPR)))
                    (ALUF (PC-PPR-CHAR-ALUF PC-PPR)))
                   ((= CH LIM))
                 (TV-DRAW-CHAR FONT CH XPOS YPOS ALUF)))
              (T		;Wide character sideways
               (DO ((CH (AR-1 TEM CHAR) (1+ CH))
                    (LIM (AR-1 TEM (1+ CHAR)))
                    (YPOS (PC-PPR-CURRENT-X PC-PPR))
                    (XPOS (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR))
                             (+ (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BASELINE-ADJ PC-PPR))
                             KERN
                             (* (FONT-RASTER-WIDTH FONT)	;Hairily find top left corner
                                (- (AR-1 TEM (1+ CHAR)) (AR-1 TEM CHAR))))
                          (+ XPOS (FONT-RASTER-WIDTH FONT)))
                    (ALUF (PC-PPR-CHAR-ALUF PC-PPR)))
                   ((= CH LIM))
                 (TV-DRAW-CHAR FONT CH XPOS YPOS ALUF))))
        (TV-MOVE-BITPOS PC-PPR (COND ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT))
                                      (AR-1 TEM CHAR))
                                     (T (FONT-CHAR-WIDTH FONT)))
                        0)))
  CHAR)

;;; Move current X, current Y on piece of paper, keeping inside boundaries.
(DEFUN TV-MOVE-BITPOS (PC-PPR DELTA-X DELTA-Y &AUX X Y TEM)
  (SETF (PC-PPR-CURRENT-X PC-PPR)
	(SETQ X (MAX (+ DELTA-X (PC-PPR-CURRENT-X PC-PPR)) (PC-PPR-LEFT PC-PPR))))
  (SETF (PC-PPR-CURRENT-Y PC-PPR)
	(SETQ Y (MAX (+ DELTA-Y (PC-PPR-CURRENT-Y PC-PPR)) (PC-PPR-TOP PC-PPR))))
  (AND (> X (PC-PPR-RIGHT-LIMIT PC-PPR))
       (SETF (PC-PPR-END-LINE-FLAG PC-PPR) 1))
  (AND (> Y (PC-PPR-BOTTOM-LIMIT PC-PPR))
       (SETF (PC-PPR-END-PAGE-FLAG PC-PPR) 1))
  (AND (SETQ TEM (PC-PPR-MORE-VPOS PC-PPR))
       (>= Y TEM)
       (SETF (PC-PPR-MORE-FLAG PC-PPR) 1))
  NIL)

;;; Call here with an exception on a piece of paper.  The appropriate trap routine is called.
(DEFUN TV-EXCEPTION (PC-PPR)
  (OR (ZEROP (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR))
      (FUNCALL (PC-PPR-OUTPUT-HOLD-FCN PC-PPR) PC-PPR))  ;Block
  (OR (ZEROP (PC-PPR-END-LINE-FLAG PC-PPR))
      (FUNCALL (PC-PPR-END-LINE-FCN PC-PPR) PC-PPR))  ;Line wrap-around, or whatever
  (OR (ZEROP (PC-PPR-END-PAGE-FLAG PC-PPR))
      (FUNCALL (PC-PPR-END-SCREEN-FCN PC-PPR) PC-PPR))  ;Screen wrap-around, or whatever
  (OR (ZEROP (PC-PPR-MORE-FLAG PC-PPR))
      (COND (TV-MORE-PROCESSING-GLOBAL-ENABLE
	     (FUNCALL (PC-PPR-MORE-FCN PC-PPR) PC-PPR))  ;MORE processing
	    (T (SETF (PC-PPR-MORE-FLAG PC-PPR) 0))))
  (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR))
      (FERROR NIL "Exceptions (~O) on pc ppr ~S won't go away, you are losing badly"
	          (PC-PPR-EXCEPTIONS PC-PPR)
		  PC-PPR))
  NIL)

;;; Default end of line function, mostly just CRLF, but for TV-LINE-OUT
;;; be careful of getting off the end of the pc ppr and wiping out other parts of the screen
;;; which can happen if the PC-PPR-END-SCREEN-FCN is a no-op.  (And if it wasn't
;;; we would wipe out the top line of the pc ppr which would be worse.)
(DEFUN TV-END-LINE-DEFAULT (PC-PPR)
  (SETF (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
  (SETF (PC-PPR-END-LINE-FLAG PC-PPR) 0)	;Not at end of line any more
  (TV-MOVE-BITPOS PC-PPR 0 (PC-PPR-LINE-HEIGHT PC-PPR)) ;To next line
  (OR (> (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BOTTOM-LIMIT PC-PPR))
      (TV-CLEAR-EOL PC-PPR)))

;;; Default end of screen function, wraps around (MORE processing already done if needed)
(DEFUN TV-END-SCREEN-DEFAULT (PC-PPR &AUX MORE-VPOS)
  ;; First pick up the MORE-VPOS, before TV-HOME bashes it
  (SETQ MORE-VPOS (PC-PPR-MORE-VPOS PC-PPR))
  ;; Now wrap around
  (TV-HOME PC-PPR)
  (TV-CLEAR-EOL PC-PPR)
  ;; Arrange for more processing next time around
  (COND ((NULL MORE-VPOS))			;No MORE processing on this PC PPR
	((>= MORE-VPOS 100000)
	 (SETF (PC-PPR-MORE-VPOS PC-PPR)	;MORE processing to happen next time
	       (- MORE-VPOS 100000)))
	(T					;Otherwise set to happen at end of screen
	 (SETF (PC-PPR-MORE-VPOS PC-PPR)	;I.E. when there isn't room for two more
		 (1+ (- (PC-PPR-BOTTOM-LIMIT PC-PPR)	; lines.
			(PC-PPR-LINE-HEIGHT PC-PPR)))))))

;;; Default MORE processor, types out **MORE** and waits for input.
;;; Note that interrupts are enabled while waiting, then disabled again,
;;; and then the blinkers have to be opened again.
(DEFUN TV-MORE-DEFAULT (PC-PPR &AUX SAVEX TEM)
  (SETF (PC-PPR-MORE-FLAG PC-PPR) 0)		;Won't need MORE processing no more
  (SETQ SAVEX (PC-PPR-CURRENT-X PC-PPR))	;Save address of beginning of **MORE**
  (SETF (PC-PPR-MORE-VPOS PC-PPR)		;Defer mores while typing **MORE**
	(+ 100000 (PC-PPR-MORE-VPOS PC-PPR)))
  (TV-CLEAR-EOL PC-PPR)
  (TV-STRING-OUT PC-PPR "**MORE**")		;Say no **MORE**
  (LET ((INHIBIT-SCHEDULING-FLAG NIL)		;<+><+><+><+> Re-enable interrupts <+><+><+><+>
	(KBD-TYI-HOOK 'DEFAULT-KBD-TYI-HOOK))	;Let user have a chance of quitting
    (SETQ TEM (KBD-TYI "MORE")))
						;<-><-><-><-> Turn off interrupts <-><-><-><->
  (TV-PREPARE-PC-PPR (PC-PPR) )			; Open blinkers, select screen.
						; Must do it before setting the xpos.
						; No body, our caller is in body!
  (SETF (PC-PPR-CURRENT-X PC-PPR) SAVEX)	;Wipe out the **MORE**
  (TV-CLEAR-EOL PC-PPR)
  (COND ((> (+ (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-LINE-HEIGHT PC-PPR))
	    (PC-PPR-BOTTOM-LIMIT PC-PPR))
	 (SETF (PC-PPR-MORE-VPOS PC-PPR) 0)	;(This line probably fixes a bug)
	 (FUNCALL (PC-PPR-END-SCREEN-FCN PC-PPR) PC-PPR))
						;We're at bottom, wrap around (or scroll)
						;Next MORE will happen at same place.
	(T (TV-NOTE-INPUT))))			;Otherwise, MORE one line up next time.

;;; This will get hairier...
(DEFUN TV-OUTPUT-HOLD-DEFAULT (PC-PPR)
  (PROCESS-WAIT "Output Hold" (FUNCTION (LAMBDA (PC-PPR)
					   (ZEROP (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR))))
			      PC-PPR))

;;; Input routine calls here.  Arrange for a MORE not to happen until this
;;; line is reached again; except, if this line is far from the bottom, we prefer
;;; to MORE at the bottom (otherwise wouldn't MORE until wrapped around and
;;; reached this line.)  This makes MOREing usually be at the bottom.
;;; We apply the algorithm to every piece of paper on the theory that
;;; if the user stops and types, he must have read everything interesting on the screen.
(DEFUN TV-NOTE-INPUT ()
    (AND (BOUNDP 'EXPOSED-WINDOWS)
	 (DOLIST (W EXPOSED-WINDOWS)
	   (<- W ':NOTE-INPUT))))

;;; Home up
(DEFUN TV-HOME (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (OR (ZEROP (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR)) ;This is the only exception that applies
	 (TV-EXCEPTION PC-PPR))
     (AND (PC-PPR-MORE-VPOS PC-PPR)		;If MORE processing enabled, put it off until
	  (SETF (PC-PPR-MORE-VPOS PC-PPR)	;bottom - i.e. last line that will fit
		(1+ (- (PC-PPR-BOTTOM-LIMIT PC-PPR) (PC-PPR-LINE-HEIGHT PC-PPR)))))
     (SETF (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
     (SETF (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR))
     (SETF (PC-PPR-EXCEPTIONS PC-PPR) 0)))	;Not at end line, nor end screen, nor MORE

;;; Crlf and clear next line
(DEFUN TV-CRLF (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (SETF (PC-PPR-END-LINE-FLAG PC-PPR) 0)	;We won't be needing this
     (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR)) (TV-EXCEPTION PC-PPR))
     (SETF (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
     (TV-MOVE-BITPOS PC-PPR 0 (PC-PPR-LINE-HEIGHT PC-PPR))
     (TV-CLEAR-EOL PC-PPR)))

;;; Space forward
(DEFUN TV-SPACE (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR)) (TV-EXCEPTION PC-PPR))
     (TV-MOVE-BITPOS PC-PPR (PC-PPR-CHAR-WIDTH PC-PPR) 0)))

;;; Space backward
(DEFUN TV-BACKSPACE (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (SETF (PC-PPR-END-LINE-FLAG PC-PPR) 0)
     (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR)) (TV-EXCEPTION PC-PPR))
     (TV-MOVE-BITPOS PC-PPR (- (PC-PPR-CHAR-WIDTH PC-PPR)) 0)))

;;; Clear current character position
(DEFUN TV-CLEAR-CHAR (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR)) (TV-EXCEPTION PC-PPR))
     (COND ((ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))
	    (TV-ERASE (PC-PPR-CHAR-WIDTH PC-PPR)
		      (PC-PPR-LINE-HEIGHT PC-PPR)
		      (PC-PPR-CURRENT-X PC-PPR)
		      (PC-PPR-CURRENT-Y PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR)))
	   ((TV-ERASE (PC-PPR-LINE-HEIGHT PC-PPR)
		      (PC-PPR-CHAR-WIDTH PC-PPR)
		      (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR))
			 (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-LINE-HEIGHT PC-PPR))
		      (PC-PPR-CURRENT-X PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR))))))

;;; Clear to end of current line
(DEFUN TV-CLEAR-EOL (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (OR (ZEROP (LOGAND -2 (PC-PPR-EXCEPTIONS PC-PPR))) ;not END-LINE
	 (TV-EXCEPTION PC-PPR))
     (COND ((ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))
	    (TV-ERASE (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-CURRENT-X PC-PPR))
		      (MIN (PC-PPR-LINE-HEIGHT PC-PPR)
                           (- (PC-PPR-BOTTOM-MARGIN PC-PPR) (PC-PPR-CURRENT-Y PC-PPR)))
		      (PC-PPR-CURRENT-X PC-PPR)
		      (PC-PPR-CURRENT-Y PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR)))
	   ((TV-ERASE (MIN (PC-PPR-LINE-HEIGHT PC-PPR)
                           (- (PC-PPR-BOTTOM-MARGIN PC-PPR) (PC-PPR-CURRENT-Y PC-PPR)))
		      (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-CURRENT-X PC-PPR))
		      (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR))
			 (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-LINE-HEIGHT PC-PPR))
		      (PC-PPR-CURRENT-X PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR))))))

;;; Clear whole piece of paper
(DEFUN TV-CLEAR-PC-PPR (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (TV-HOME PC-PPR)	;Will handle exception business
			;Now clear all including margins
     (COND ((ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))
	    (TV-ERASE (- (PC-PPR-RIGHT PC-PPR) (PC-PPR-LEFT PC-PPR))
		      (- (PC-PPR-BOTTOM PC-PPR) (PC-PPR-TOP PC-PPR))
		      (PC-PPR-LEFT PC-PPR)
		      (PC-PPR-TOP PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR)))
	   ((TV-ERASE (- (PC-PPR-BOTTOM PC-PPR) (PC-PPR-TOP PC-PPR))
		      (- (PC-PPR-RIGHT PC-PPR) (PC-PPR-LEFT PC-PPR))
		      (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR))
			 (PC-PPR-BOTTOM PC-PPR))
		      (PC-PPR-LEFT PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR))))))

(DEFUN TV-CLEAR-SCREEN (&OPTIONAL (SCREEN TV-DEFAULT-SCREEN))
  (LET ((INHIBIT-SCHEDULING-FLAG T))
     (TV-OPEN-SCREEN)		;Opens all screens, actually.
     (TV-SELECT-SCREEN SCREEN)
   (LET ((PM (SCREEN-PLANE-MASK SCREEN)))
     (COND ((ZEROP PM)
	    (TV-ERASE (* (SCREEN-WIDTH SCREEN) (SCREEN-BITS-PER-PIXEL SCREEN))
		      (SCREEN-HEIGHT SCREEN) 0 0 TV-ALU-ANDCA))
	   ((= PM 1)
	    (TV-ERASE (SCREEN-WIDTH SCREEN) (SCREEN-HEIGHT SCREEN) 0 0 TV-ALU-ANDCA))
	   (T (LET ((OPM (%UNIBUS-READ 776660)))  ;MICROCODE NOT HAIRY ENUF TO WIN FOR
		(DO ((PM PM (LSH PM -1))	  ; THIS SO FIX IT JUST ENUF TO
		     (P 0 (1+ P)))		  ; WIN FOR OLD GREY LEVEL DISPLAY ON CONS
		    ((ZEROP PM))
		  (COND ((LOGAND PM 1)
			 (%UNIBUS-WRITE 776660 (LSH P 8))
			 (TV-ERASE (SCREEN-WIDTH SCREEN) (SCREEN-HEIGHT SCREEN)
				   0 0 TV-ALU-ANDCA))))
		(%UNIBUS-WRITE 776660 OPM)))))
     (AND (BOUNDP 'TV-WHO-LINE-LIST)		;Whole who-line will need redisplay
       (DO L TV-WHO-LINE-LIST (CDR L) (NULL L)
	 (SETF (TV-WHO-LINE-ITEM-STATE (CAR L)) NIL)))))

;;; Routine to print a string on TV
;;; Understands format effectors (special keys 200-237).
;;; Optional starting and ending indices may be supplied.  If unsupplied,
;;; the whole string is done.
(DEFUN TV-STRING-OUT (PC-PPR STRING &OPTIONAL (START 0) (END NIL))
  (TV-PREPARE-PC-PPR (PC-PPR)
     (AND (SYMBOLP STRING)
	  (SETQ STRING (GET-PNAME STRING)))
     (PROG ((I START)
	    (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	    (FONT (PC-PPR-CURRENT-FONT PC-PPR))
	    (SIDEWAYS-P (NOT (ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))))
	    XPOS YPOS YCROC XLIM ALUF WIDTH CH)
   TOP (AND (>= I N) (RETURN NIL))		;No exception if done anyway
       (AND (NULL (FONT-CHAR-WIDTH-TABLE FONT)) ;Handle easy case fast
	    (NULL (FONT-INDEXING-TABLE FONT))
            (NULL (FONT-LEFT-KERN-TABLE FONT))
            (< (AREF STRING I) 200)             ;Not if first char a format effector!
	    (GO EZ))                            ; This makes CRLF at right margin work
       (TV-TYO PC-PPR (AREF STRING I))
       (AND (< (SETQ I (1+ I)) N)
            (GO TOP))
       (RETURN NIL)

   EZ  (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR)) (TV-EXCEPTION PC-PPR))
       (SETQ XPOS (PC-PPR-CURRENT-X PC-PPR)
	     YPOS (+ (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BASELINE-ADJ PC-PPR))
             YCROC (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR)) YPOS (FONT-RASTER-WIDTH FONT))
	     ALUF (PC-PPR-CHAR-ALUF PC-PPR)
	     WIDTH (FONT-CHAR-WIDTH FONT)
	     XLIM (PC-PPR-RIGHT-LIMIT PC-PPR))

   EZ1 (COND ((>= I N)
	      (TV-MOVE-BITPOS PC-PPR (- XPOS (PC-PPR-CURRENT-X PC-PPR)) 0)
	      (RETURN NIL))
	     ((< (SETQ CH (AR-1 STRING I)) 200)	;Printing character
	      (COND ((NOT SIDEWAYS-P)
		     (TV-DRAW-CHAR FONT CH XPOS YPOS ALUF))
		    ((TV-DRAW-CHAR FONT CH YCROC XPOS ALUF)))
	      (SETQ XPOS (+ XPOS WIDTH))
	      (COND ((> XPOS XLIM)		;If running off margin
		     (TV-MOVE-BITPOS PC-PPR	; call full routine
				     (- XPOS (PC-PPR-CURRENT-X PC-PPR)) 0)
		     (SETQ I (1+ I))
		     (GO TOP))))		;Reget parameters, check exceptions
	     ((AND (> CH 237) (< CH 250))	;Mapped font change
	      (TV-SET-FONT PC-PPR (SETQ FONT (AR-1 (PC-PPR-FONT-MAP PC-PPR) (- CH 240))))
	      (TV-MOVE-BITPOS PC-PPR (- XPOS (PC-PPR-CURRENT-X PC-PPR)) 0)
	      (GO TOP))
	     ((= CH 250) )			;Absolute font change
             (T					;Format effector, call full TYO
	      (TV-MOVE-BITPOS PC-PPR (- XPOS (PC-PPR-CURRENT-X PC-PPR)) 0)
              (TV-TYO PC-PPR CH)
	      (SETQ I (1+ I))
	      (GO TOP)))			;Reget parameters, check exceptions
       (SETQ I (1+ I))				;Loop
       (GO EZ1))))

;;; Routine to initialize the TV system upon startup.
;;; This is called every time the machine is 105 foobared.
;;; Note that the cold-load defines TVFONT and CPTFONT so the we won't have to do
;;; any file I/O before typing out.
;;; The cold-load also creates the funny arrays such as TV-BUFFER, for now anyway.
(DEFVAR TV-CPT-PC-PPR)

(DEFUN TV-INITIALIZE ()
  ;; Make CPT monitor happy
  (TV-SETUP-CPT)
  (SETQ TV-DEFAULT-SCREEN NIL)	;Prevent unbound variable
  ;; Set up screens and pcs ppr for Ball and CPT monitors
  (COND ((NOT (BOUNDP 'TV-CPT-PC-PPR))
	 (SETQ TV-CPT-SCREEN (TV-DEFINE-SCREEN "CPT" ':DEFAULT-FONT FONTS:CPTFONT
					             ':PLANE-MASK 0 ':BUFFER (LSH 77 18.)
						     ':CONTROL-ADDRESS 377760
                                                     ':PROPERTIES '(:VIDEO :BLACK-AND-WHITE
                                                                    :CONTROLLER :SIMPLE)
                                                     ':HEIGHT 896. ':WIDTH 768.)
	       TV-CPT-PC-PPR (TV-DEFINE-PC-PPR "CPT" (LIST FONTS:CPTFONT)
					             ':SCREEN TV-CPT-SCREEN))))
  (SETQ TV-DEFAULT-SCREEN TV-CPT-SCREEN
	CONSOLE-IO-PC-PPR TV-CPT-PC-PPR)
  ;; Set up named structure functions
  (COND ((NOT (FBOUNDP 'TV-BLINKER))
	 (FSET' FONT 'TV-NAMED-STRUCTURE-HANDLER)
	 (FSET' SCREEN 'TV-NAMED-STRUCTURE-HANDLER)
	 (FSET' PC-PPR 'TV-NAMED-STRUCTURE-HANDLER)
	 (FSET' TV-BLINKER 'TV-NAMED-STRUCTURE-HANDLER)))

  ;; Prevent unbound errors since TVDEFS isn't loaded yet.
  (OR (BOUNDP 'EXPOSED-PC-PPRS) (SETQ EXPOSED-PC-PPRS NIL))
  (OR (BOUNDP 'SELECTED-PC-PPRS) (SETQ SELECTED-PC-PPRS NIL))
  (OR (BOUNDP 'TV-ROVING-BLINKER-LIST) (SETQ TV-ROVING-BLINKER-LIST NIL))
  (OR (BOUNDP 'TV-MORE-PROCESSING-GLOBAL-ENABLE) (SETQ TV-MORE-PROCESSING-GLOBAL-ENABLE T))
  (OR (BOUNDP 'TV-BEEP) (SETQ TV-BEEP T))
  (OR (BOUNDP 'TV-BEEP-DURATION) (SETQ TV-BEEP-DURATION 400000))
  (OR (BOUNDP 'TV-BEEP-WAVELENGTH) (SETQ TV-BEEP-WAVELENGTH 1350))
  ;; Take all non-roving blinkers off the screen.
  ;; Pc-pprs which belong to windows which will be exposed or selected
  ;; will get put back in the right state by WINDOW-INITIALIZE.
  (MAPC #'TV-DEACTIVATE-PC-PPR EXPOSED-PC-PPRS)
  (MAPC #'TV-DEACTIVATE-PC-PPR SELECTED-PC-PPRS)
  ;; Take the roving blinkers off the screen,
  (TV-OPEN-SCREEN)
  ;; Make them invisible, which means that they are not even
  ;; still on TV-ROVING-BLINKER-LIST until their owners make them visible again.
  (MAPC (FUNCTION (LAMBDA (BL) (TV-SET-BLINKER-VISIBILITY BL NIL)))
	TV-ROVING-BLINKER-LIST))

;;; Routine to define a screen
;;; The reason for the colons in the local variables here is that
;;; the compiler outputs symbols with the names of the local variables
;;; and having two symbols with the same name in different packages
;;; in the cold-load messes things up.
(DEFUN TV-DEFINE-SCREEN (NAME &REST KEYWORDS
			 &AUX (PLANE-MASK 0)
			      (HEIGHT NIL)
			      (WIDTH NIL)
			      :LOCATIONS-PER-LINE
			      (:BUFFER NIL)
			      (:CONTROL-ADDRESS NIL)
                              (:BITS-PER-PIXEL 1)
			      (:PROPERTIES NIL)
			      (:FONT-ALIST NIL)
			      :DEFAULT-FONT
			      (:WHO-LINE-P T)
			      (:X1 0) (:Y1 0) :X2 :Y2
			      SCREEN)
  ;; Process options
  (DO L KEYWORDS (CDDR L) (NULL L)
    (SELECTQ (CAR L)
      (:PLANE-MASK (SETQ PLANE-MASK (CADR L)))
      (:HEIGHT (SETQ HEIGHT (CADR L)))
      (:WIDTH (SETQ WIDTH (CADR L)))
      (:LOCATIONS-PER-LINE (SETQ :LOCATIONS-PER-LINE (CADR L)))
      (:BUFFER (SETQ :BUFFER (CADR L)))
      (:CONTROL-ADDRESS (SETQ :CONTROL-ADDRESS (CADR L)))
      (:WHO-LINE-P (SETQ :WHO-LINE-P (CADR L)))
      (:BITS-PER-PIXEL (SETQ :BITS-PER-PIXEL (CADR L)))
      (:PROPERTIES (SETQ :PROPERTIES (CADR L)))
      (:FONT-ALIST (SETQ :FONT-ALIST (CADR L)))
      (:DEFAULT-FONT (SETQ :DEFAULT-FONT (CADR L)))
      (:X1 (SETQ :X1 (CADR L)))
      (:Y1 (SETQ :Y1 (CADR L)))
      (:X2 (SETQ :X2 (CADR L)))
      (:Y2 (SETQ :Y2 (CADR L)))
      (OTHERWISE (FERROR NIL "~S is an unknown keyword argument" (CAR L)))))
  (OR :DEFAULT-FONT			;Default the default font, a little kludgily
      (SETQ :DEFAULT-FONT FONTS:CPTFONT))
  ;; Default X2 and Y2 if user hasn't specified
  (COND ((NOT (NULL :X2)) )
	(T (SETQ :X2 WIDTH)))
  (COND ((NOT (NULL :Y2)) )
	((NOT :WHO-LINE-P) (SETQ :Y2 HEIGHT))
	(T (SETQ :Y2 (- HEIGHT (FONT-CHAR-HEIGHT :DEFAULT-FONT)))))
  (OR :LOCATIONS-PER-LINE
      (SETQ :LOCATIONS-PER-LINE (// (* WIDTH :BITS-PER-PIXEL)
				   (COND ((ZEROP PLANE-MASK) 32.) (T 16.)))))
  ;; Make the screen
  (SETQ SCREEN (MAKE-SCREEN SCREEN-NAME NAME
			    SCREEN-PLANE-MASK PLANE-MASK
			    SCREEN-HEIGHT HEIGHT
			    SCREEN-WIDTH WIDTH
			    SCREEN-BITS-PER-PIXEL :BITS-PER-PIXEL
			    SCREEN-PROPERTY-LIST :PROPERTIES
			    SCREEN-FONT-ALIST :FONT-ALIST
			    SCREEN-X1 :X1
			    SCREEN-Y1 :Y1
			    SCREEN-X2 :X2
			    SCREEN-Y2 :Y2
			    SCREEN-BUFFER :BUFFER
			    SCREEN-CONTROL-ADDRESS :CONTROL-ADDRESS
                            SCREEN-LOCATIONS-PER-LINE :LOCATIONS-PER-LINE
			    SCREEN-BUFFER-PIXEL-ARRAY
				  ;; Note, pixels not supported on 16-bit TV any more.
			          (MAKE-ARRAY NIL  ;CONTROL-TABLES?
					      (COND ((NOT (ZEROP PLANE-MASK)) 'ART-TVB)
						    ((= :BITS-PER-PIXEL 1) 'ART-1B)
						    ((= :BITS-PER-PIXEL 2) 'ART-2B)
						    ((= :BITS-PER-PIXEL 4) 'ART-4B)
						    ((= :BITS-PER-PIXEL 8) 'ART-8B)
						    (T 'ART-1B)) ;Sigh, shouldn't bomb I guess
					      (LIST WIDTH HEIGHT) ;Dimensions
					      :BUFFER) ;Displaced to actual video buffer
			    SCREEN-BUFFER-HALFWORD-ARRAY
			          (MAKE-ARRAY NIL  ;CONTROL-TABLES?
					      (COND ((NOT (ZEROP PLANE-MASK)) 'ART-32B)
							;Old TV, bits reversed, 16 per word
						    (T 'ART-16B))
					      (// (* WIDTH HEIGHT :BITS-PER-PIXEL) 16.)
					      :BUFFER) ;Displaced to actual video buffer 
			   SCREEN-DEFAULT-FONT :DEFAULT-FONT))
  SCREEN)

;;; Routine to define a piece of paper, returns it.
;;; OPTIONS is a "PROPERTY LIST" (not an ASSOC list)
;;; Note that local vars with colons in them are there for a reason, commented elsewhere in
;;; this file.
(DEFUN TV-DEFINE-PC-PPR (NAME :FONT-MAP &REST &EVAL OPTIONS
			 &AUX TOP BOTTOM LEFT RIGHT (SCREEN TV-DEFAULT-SCREEN)
			      (BLINKER-P T) (REVERSE-VIDEO-P NIL) (MORE-P T)
			      (VSP 2) (LEFT-MARGIN 0) (TOP-MARGIN 0)
			      (RIGHT-MARGIN 0) (BOTTOM-MARGIN 0)
			      (END-LINE-FCN 'TV-END-LINE-DEFAULT)
			      (END-SCREEN-FCN 'TV-END-SCREEN-DEFAULT)
			      ;; See comment above TV-DEFINE-SCREEN about colon
			      (:OUTPUT-HOLD-FCN 'TV-OUTPUT-HOLD-DEFAULT)
			      (MORE-FCN 'TV-MORE-DEFAULT) (BLINK-FCN 'TV-RECTANGULAR-BLINKER)
			      SIDEWAYS-P (INTEGRAL-P NIL)
			      PC-PPR)
  ;; First, find out what screen so we can default other things
  (DO OP OPTIONS (CDDR OP) (NULL OP)
    (AND (EQ (CAR OP) ':SCREEN)
	 (SETQ SCREEN (CADR OP))))
  (AND (FBOUNDP 'TYPEP) ;Sigh
       (CHECK-ARG SCREEN (EQ (TYPEP SCREEN) 'SCREEN) "a screen"))
  (SETQ LEFT (SCREEN-X1 SCREEN)
	RIGHT (SCREEN-X2 SCREEN)
	TOP (SCREEN-Y1 SCREEN)
	BOTTOM (SCREEN-Y2 SCREEN)
	SIDEWAYS-P (GET (LOCF (SCREEN-PROPERTY-LIST SCREEN)) ':SIDEWAYS))
  ;; Process options
  (DO OP OPTIONS (CDDR OP) (NULL OP)
    (SELECTQ (CAR OP)
	(:TOP (SETQ TOP (CADR OP)))
	(:BOTTOM (SETQ BOTTOM (CADR OP)))
	(:LEFT (SETQ LEFT (CADR OP)))
	(:RIGHT (SETQ RIGHT (CADR OP)))
	(:FONT-MAP (SETQ :FONT-MAP (CADR OP)))
	(:SCREEN )
	(:BLINKER-P (SETQ BLINKER-P (CADR OP)))
	(:REVERSE-VIDEO-P (SETQ REVERSE-VIDEO-P (CADR OP)))
	(:MORE-P (SETQ MORE-P (CADR OP)))
	(:VSP (SETQ VSP (CADR OP)))
	(:LEFT-MARGIN (SETQ LEFT-MARGIN (CADR OP)))
	(:TOP-MARGIN (SETQ TOP-MARGIN (CADR OP)))
	(:RIGHT-MARGIN (SETQ RIGHT-MARGIN (CADR OP)))
	(:BOTTOM-MARGIN (SETQ BOTTOM-MARGIN (CADR OP)))
	(:END-LINE-FCN (SETQ END-LINE-FCN (CADR OP)))
	(:END-SCREEN-FCN (SETQ END-SCREEN-FCN (CADR OP)))
	(:OUTPUT-HOLD-FCN (SETQ :OUTPUT-HOLD-FCN (CADR OP)))
	(:MORE-FCN (SETQ MORE-FCN (CADR OP)))
	(:BLINK-FCN (SETQ BLINK-FCN (CADR OP)))
	(:SIDEWAYS-P (SETQ SIDEWAYS-P (CADR OP)))
	(:INTEGRAL-P (SETQ INTEGRAL-P (CADR OP)))
	(OTHERWISE (FERROR NIL "~S is not a recognized option" (CAR OP)))))
  (OR :FONT-MAP (SETQ :FONT-MAP (LIST (SCREEN-DEFAULT-FONT SCREEN))))
  (TV-SET-PC-PPR-PARAMETERS (SETQ PC-PPR (MAKE-PC-PPR))
		:FONT-MAP TOP TOP-MARGIN BOTTOM BOTTOM-MARGIN
		LEFT LEFT-MARGIN RIGHT RIGHT-MARGIN VSP INTEGRAL-P MORE-P)
  (SETF (PC-PPR-NAME PC-PPR) NAME)
  (SETF (PC-PPR-SCREEN PC-PPR) SCREEN)
  (AND SIDEWAYS-P (SETF (PC-PPR-SIDEWAYS-P PC-PPR) 1))
  (SETF (PC-PPR-END-LINE-FCN PC-PPR) END-LINE-FCN)
  (SETF (PC-PPR-END-SCREEN-FCN PC-PPR) END-SCREEN-FCN)
  (SETF (PC-PPR-OUTPUT-HOLD-FCN PC-PPR) :OUTPUT-HOLD-FCN)
  (SETF (PC-PPR-MORE-FCN PC-PPR) MORE-FCN)

  (COND ((NOT REVERSE-VIDEO-P)
	 (SETF (PC-PPR-CHAR-ALUF PC-PPR) TV-ALU-IOR)
	 (SETF (PC-PPR-ERASE-ALUF PC-PPR) TV-ALU-ANDCA))
	(T
	 (SETF (PC-PPR-CHAR-ALUF PC-PPR) TV-ALU-ANDCA)
	 (SETF (PC-PPR-ERASE-ALUF PC-PPR) TV-ALU-IOR)))
  (SETF (PC-PPR-BLINKER-LIST PC-PPR) NIL)
  (AND BLINKER-P
       (TV-DEFINE-BLINKER PC-PPR ':FUNCTION BLINK-FCN
			  ':FOLLOW-P T
			  ':SIDEWAYS-P SIDEWAYS-P))
  PC-PPR)

;This function sets the various geometrical parameters and the font map
;of a piece of paper.  It knows about all the peculiar consistency rules.
(DEFUN TV-SET-PC-PPR-PARAMETERS (PC-PPR :FONT-MAP TOP TOP-MARGIN BOTTOM BOTTOM-MARGIN
				 LEFT LEFT-MARGIN RIGHT RIGHT-MARGIN VSP INTEGRAL-P MORE-P
				 &AUX (MAXWIDTH 0) (MAXHEIGHT 0) (MAXBASE 0) LINE-HEIGHT
				      RIGHT-LIMIT BOTTOM-LIMIT TEM)
  ;First standardize the font map
  (COND ((ARRAYP :FONT-MAP))
	((LISTP :FONT-MAP)
	 (SETQ TEM (MAKE-ARRAY NIL 'ART-Q '(26.) NIL NIL))
	 (DO ((I 0 (1+ I))
	      (L :FONT-MAP (OR (CDR L) L)))
	     ((= I 26.))
	  (AS-1 (CAR L) TEM I))
	 (SETQ :FONT-MAP TEM))
	((FERROR NIL "~S is not a valid FONT-MAP" :FONT-MAP)))

  ;Now, find out the character dimensions of this set of fonts
  (DO I 0 (1+ I) (= I 26.)
    (AND (> (SETQ TEM (FONT-CHAR-HEIGHT (AR-1 :FONT-MAP I))) MAXHEIGHT)
	 (SETQ MAXHEIGHT TEM))
    (AND (> (SETQ TEM (FONT-BASELINE (AR-1 :FONT-MAP I))) MAXBASE)
	 (SETQ MAXBASE TEM))
    (COND ((NULL (SETQ TEM (FONT-CHAR-WIDTH-TABLE (AR-1 :FONT-MAP I))))
	   (AND (> (SETQ TEM (FONT-CHAR-WIDTH (AR-1 :FONT-MAP I))) MAXWIDTH)
		(SETQ MAXWIDTH TEM)))
	  (T (DO J 0 (1+ J) (= J 200)
	       (AND (> (AR-1 TEM J) MAXWIDTH)
		    (SETQ MAXWIDTH (AR-1 TEM J)))))))
  (SETQ LINE-HEIGHT (+ VSP MAXHEIGHT))

  ;Convert margins from relative to absolute
  ;BOTTOM-LIMIT is always an integral number of lines down from TOP-MARGIN so that
  ;it is precisely the position of the last allowable line.
  (SETQ TOP-MARGIN (+ TOP TOP-MARGIN)
	BOTTOM-LIMIT (+ TOP-MARGIN
			(* LINE-HEIGHT (// (- BOTTOM BOTTOM-MARGIN LINE-HEIGHT TOP-MARGIN)
					   LINE-HEIGHT)))
	LEFT-MARGIN (+ LEFT LEFT-MARGIN)
	RIGHT-MARGIN (- RIGHT RIGHT-MARGIN)
	RIGHT-LIMIT (- RIGHT-MARGIN MAXWIDTH))
  ;If requested, make the text area an integral number of lines by moving the bottom up
  (AND INTEGRAL-P
       (SETQ BOTTOM (+ BOTTOM-LIMIT LINE-HEIGHT BOTTOM-MARGIN)))
  (SETQ BOTTOM-MARGIN (- BOTTOM BOTTOM-MARGIN))

  (OR (AND (>= RIGHT-LIMIT LEFT-MARGIN)		;MAKE CONSISTENCY CHECKS
	   (>= BOTTOM-LIMIT (+ TOP-MARGIN (COND (MORE-P LINE-HEIGHT) (T 0)))))
      (FERROR NIL "Inconsistent parameters in PC-PPR ~S" PC-PPR))

  ;Store the results into the pc ppr
  (SETF (PC-PPR-TOP PC-PPR) TOP)
  (SETF (PC-PPR-BOTTOM PC-PPR) BOTTOM)
  (SETF (PC-PPR-LEFT PC-PPR) LEFT)
  (SETF (PC-PPR-RIGHT PC-PPR) RIGHT)
  (SETF (PC-PPR-TOP-MARGIN PC-PPR) TOP-MARGIN)
  (SETF (PC-PPR-LEFT-MARGIN PC-PPR) LEFT-MARGIN)
  (SETF (PC-PPR-BOTTOM-MARGIN PC-PPR) BOTTOM-MARGIN)
  (SETF (PC-PPR-RIGHT-MARGIN PC-PPR) RIGHT-MARGIN)
  (SETF (PC-PPR-CURRENT-Y PC-PPR) TOP-MARGIN)
  (SETF (PC-PPR-CURRENT-X PC-PPR) LEFT-MARGIN)
  (SETF (PC-PPR-FONT-MAP PC-PPR) :FONT-MAP)
  (SETF (PC-PPR-CURRENT-FONT PC-PPR) (SETQ TEM (AR-1 :FONT-MAP 0)))
  (SETF (PC-PPR-CHAR-WIDTH PC-PPR) (FONT-CHAR-WIDTH TEM))
  (SETF (PC-PPR-BASELINE PC-PPR) MAXBASE)
  (SETF (PC-PPR-BASELINE-ADJ PC-PPR) 0)
  (SETF (PC-PPR-LINE-HEIGHT PC-PPR) LINE-HEIGHT)
  (SETF (PC-PPR-MORE-VPOS PC-PPR) (AND MORE-P (1- (- BOTTOM-LIMIT LINE-HEIGHT))))
  (SETF (PC-PPR-RIGHT-LIMIT PC-PPR) RIGHT-LIMIT)
  (SETF (PC-PPR-BOTTOM-LIMIT PC-PPR) BOTTOM-LIMIT))

;;; Define a blinker on a piece of paper
(DEFUN TV-DEFINE-BLINKER (PC-PPR &REST &EVAL OPTIONS
			  &AUX (FONT (AND PC-PPR (AR-1 (PC-PPR-FONT-MAP PC-PPR) 0)))
			       (FUNCTION 'TV-RECTANGULAR-BLINKER)
			       (HEIGHT (AND FONT (FONT-BLINKER-HEIGHT FONT)))
			       (WIDTH (AND FONT (FONT-BLINKER-WIDTH FONT)))
			       (VISIBILITY 'BLINK)
			       (FOLLOW-P NIL)
			       (ROVING-P NIL)
			       (HALF-PERIOD 15.)
			       SCREEN SIDEWAYS-FLAG
			       BLINKER)
  (DO OP OPTIONS (CDDR OP) (NULL OP)
    (SELECTQ (CAR OP)
      ((:HEIGHT :ARG2) (SETQ HEIGHT (CADR OP)))
      ((:WIDTH :ARG1) (SETQ WIDTH (CADR OP)))
      (:FUNCTION (SETQ FUNCTION (CADR OP)))
      (:VISIBILITY (SETQ VISIBILITY (CADR OP)))
      (:FOLLOW-P (SETQ FOLLOW-P (CADR OP) ROVING-P NIL))
      (:ROVING-P (SETQ ROVING-P (CADR OP) FOLLOW-P NIL))
      (:HALF-PERIOD (SETQ HALF-PERIOD (CADR OP)))
      (:SCREEN (SETQ SCREEN (CADR OP)))
      (:SIDEWAYS-P (SETQ SIDEWAYS-FLAG (COND ((CADR OP) 1) (T 0))))
      (OTHERWISE (FERROR NIL "~S is not a recognized option" (CAR OP)))))
  ;; Process old and new flavor of screen specifications
  (SETQ SCREEN (COND (SCREEN)
		     (PC-PPR (PC-PPR-SCREEN PC-PPR))
		     (T TV-DEFAULT-SCREEN)))
  (SETQ SIDEWAYS-FLAG (COND (SIDEWAYS-FLAG)
			    (PC-PPR (PC-PPR-SIDEWAYS-P PC-PPR))
			    ((GET (LOCF (SCREEN-PROPERTY-LIST SCREEN)) ':SIDEWAYS) 1)
			    (T 0)))
  (SETQ BLINKER (MAKE-TV-BLINKER))
  (COND (ROVING-P
	 (SETF (TV-BLINKER-X-POS BLINKER) 0)
	 (SETF (TV-BLINKER-Y-POS BLINKER) 0))
	((NOT FOLLOW-P)
	 (SETF (TV-BLINKER-X-POS BLINKER) (PC-PPR-CURRENT-X PC-PPR))
	 (SETF (TV-BLINKER-Y-POS BLINKER) (PC-PPR-CURRENT-Y PC-PPR))))
  (SETF (TV-BLINKER-PC-PPR BLINKER) (AND (NOT ROVING-P) PC-PPR)) ;NIL IF ROVING
  (SETF (TV-BLINKER-WIDTH BLINKER) WIDTH)
  (SETF (TV-BLINKER-HEIGHT BLINKER) HEIGHT)
  (SETF (TV-BLINKER-FUNCTION BLINKER) FUNCTION)
  (SETF (TV-BLINKER-HALF-PERIOD BLINKER) HALF-PERIOD)
  (SETF (TV-BLINKER-TIME-UNTIL-BLINK BLINKER) 0)
  (SETF (TV-BLINKER-PHASE BLINKER) NIL)
  (SETF (TV-BLINKER-VISIBILITY BLINKER) VISIBILITY)
  (SETF (TV-BLINKER-SAVE-VISIBILITY BLINKER) VISIBILITY)
  (SETF (TV-BLINKER-SIDEWAYS-P BLINKER) (NOT (ZEROP SIDEWAYS-FLAG)))
  (SETF (TV-BLINKER-SCREEN BLINKER) SCREEN)
  (OR ROVING-P (PUSH BLINKER (PC-PPR-BLINKER-LIST PC-PPR)))
  (AND ROVING-P VISIBILITY
       (WITHOUT-INTERRUPTS
	 (PUSH BLINKER TV-ROVING-BLINKER-LIST)))
  BLINKER)

;;; Make a pc-ppr be selected.  This means its blinkers can actually blink.
(DEFUN TV-SELECT-PC-PPR (PC-PPR)
  (WITHOUT-INTERRUPTS
    (SETQ EXPOSED-PC-PPRS (DELQ PC-PPR EXPOSED-PC-PPRS))	;This is NOT exposed.
    (COND ((MEMQ PC-PPR SELECTED-PC-PPRS))	;It IS selected.
	  (T (PUSH PC-PPR SELECTED-PC-PPRS)
	     ;; Make blinkers that want to blink start blinking right away.
	     (DOLIST (B (PC-PPR-BLINKER-LIST PC-PPR))
	       (SETF (TV-BLINKER-TIME-UNTIL-BLINK B) 0))))))

;; Make this pc-ppr be exposed, which means that it can be typed on:
;; typeout will make the blinkers temporarily invisible,
;; and the blinker system will put them back on again soon after.
(DEFUN TV-EXPOSE-PC-PPR (PC-PPR)
  (WITHOUT-INTERRUPTS
    (OR (MEMQ PC-PPR EXPOSED-PC-PPRS)
	(PUSH PC-PPR EXPOSED-PC-PPRS))
    (SETQ SELECTED-PC-PPRS (DELQ PC-PPR SELECTED-PC-PPRS))))

;; Make the blinker system forget about a pc-ppr
;; (ie, not check for blinkers made temporarily invisible or needing to be blinked)
;; but leave the blinkers visible on the screen.
(DEFUN TV-DEEXPOSE-PC-PPR (PC-PPR)
  (WITHOUT-INTERRUPTS
    (SETQ EXPOSED-PC-PPRS (DELQ PC-PPR EXPOSED-PC-PPRS))
    (SETQ SELECTED-PC-PPRS (DELQ PC-PPR SELECTED-PC-PPRS))
    ;; Make sure all the blinkers are actually visible on the screen.
    ;; Then, since this pc-ppr is no longer exposed or selected,
    ;; the blinker system won't know the blinkers are on the screen
    ;; and will just leave them there.
    (DOLIST (B (PC-PPR-BLINKER-LIST PC-PPR))
      (AND (TV-BLINKER-VISIBILITY B)
	   (TV-BLINK B T)))))

;; If pc-ppr is selected, make it only exposed, else leave it alone
(DEFUN TV-DESELECT-PC-PPR (PC-PPR)
  (WITHOUT-INTERRUPTS
    (AND (MEMQ PC-PPR SELECTED-PC-PPRS) (TV-EXPOSE-PC-PPR PC-PPR))))

;; These are semi-obsolete ways of turning a pc-ppr's blinkers on and off.
(DEFUN TV-ACTIVATE-PC-PPR (PC-PPR)
  (TV-SELECT-PC-PPR PC-PPR))

;; This is like TV-DEEXPOSE-PC-PPR except that blinkers are left off, not on.
(DEFUN TV-DEACTIVATE-PC-PPR (PC-PPR)
  (WITHOUT-INTERRUPTS
    (SETQ EXPOSED-PC-PPRS (DELQ PC-PPR EXPOSED-PC-PPRS))
    (SETQ SELECTED-PC-PPRS (DELQ PC-PPR SELECTED-PC-PPRS))
    ;; Take all the blinkers off the screen before we stop checking for them.
    (DOLIST (B (PC-PPR-BLINKER-LIST PC-PPR))
      (TV-OPEN-BLINKER B))))

;;; Support for "Simple TV" (32-bit TV system)

;;; Read and write the sync program
(DEFUN TV-READ-SYNC (ADR &OPTIONAL (TV-ADR 377760))
	(%XBUS-WRITE (+ TV-ADR 2) ADR)	;Set pointer
	(LOGAND 377 (%XBUS-READ (+ TV-ADR 1))))

(DEFUN TV-WRITE-SYNC (ADR DATA &OPTIONAL (TV-ADR 377760))
	(%XBUS-WRITE (+ TV-ADR 2) ADR)	;Set pointer
	(%XBUS-WRITE (+ TV-ADR 1) DATA))

;;; Start and stop the sync program
(DEFUN TV-START-SYNC (CLOCK BOW VSP &OPTIONAL (TV-ADR 377760))
	(%XBUS-WRITE TV-ADR (+ (LSH BOW 2) CLOCK))
	(%XBUS-WRITE (+ TV-ADR 3) (+ 200 VSP)))

(DEFUN TV-STOP-SYNC (&OPTIONAL (TV-ADR 377760))
	(%XBUS-WRITE (+ TV-ADR 3) 200))		;Disable output of sync

;;; Write into the sync program from a list with repeat-counts
;;; Sub-lists are repeated <car> times.
(DEFUN TV-FILL-SYNC (L &OPTIONAL (ADR 0) (TV-ADR 377760) &AUX X)
  (DO ((L L (CDR L))) ((NULL L) ADR)
    (SETQ X (CAR L))
    (COND ((ATOM X) (TV-WRITE-SYNC ADR X TV-ADR) (SETQ ADR (1+ ADR)))
	  (T (DO N (CAR X) (1- N) (ZEROP N)
	       (SETQ ADR (TV-FILL-SYNC (CDR X) ADR TV-ADR)))))))

(DECLARE (SPECIAL TV-CPT-SYNC TV-CPT-SYNC1 TV-CPT-SYNC2 TV-COLOR-SYNC TV-CPT-SYNC-60HZ))

;;; Set up sync for CPT monitor 768. x 896.
(DEFUN TV-SETUP-CPT (&OPTIONAL (SYNC-PROG TV-CPT-SYNC2) (TV-ADR 377760) FORCE-P)
  ;; Always turn on vertical sync interrupts if this is the first TV controller.
  ;; The microcode relies on these as a periodic clock for various purposes.
  ;; If not the first controller, leave the interrupt enable the way it is.
  (WITHOUT-INTERRUPTS
    (LET ((INTERRUPT-ENABLE (IF (= TV-ADR 377760) 10	;This is the number UCADR knows
				(LOGAND (%XBUS-READ TV-ADR) 10)))
	  (STATUS (%XBUS-READ TV-ADR)))
      (COND ((AND (NOT FORCE-P) (BIT-TEST STATUS 200))		;Running in PROM
	     (%XBUS-WRITE TV-ADR (+ 4 INTERRUPT-ENABLE)))
	    (T (TV-STOP-SYNC TV-ADR)
	       (TV-FILL-SYNC SYNC-PROG 0 TV-ADR)
	       (TV-START-SYNC INTERRUPT-ENABLE 1 0 TV-ADR))))))

(DEFUN TV-PROM-SETUP (&OPTIONAL (TV-ADR 377760))
  (%XBUS-WRITE (+ TV-ADR 3) 0))

(DEFUN TV-60HZ ()
    (TV-SETUP-CPT TV-CPT-SYNC-60HZ 377760 T)
    (DO I 0 (1+ I) (= I 100000) (%XBUS-WRITE I 0)))

;sync program bits
;1  HSYNC
;2  VSYNC
;4  COMPOSITE - (not used really, encode on HSYNC)
;10  BLANKING
;     0  PROC CYC
;     20  REFRESH
;     40  INC TVMA
;     60  STEP TVMA
;     0    --
;     100  CLR TVMA
;     200  EOL
;     300  EOP
;Assume 60MHZ bit clock, therefore 15Mhz (66.7 ns) TTL clock, 533ns SYNC clk
; 30. sync clks per line, 10. for horz sync, 
;41.6 lines for 666 usec vertical
;1037 lines per 16.66 ms frame


; 640. X 896.
(SETQ TV-CPT-SYNC '(
   1.  (2 33) (8 13) (18. 12) 212 112
   45.  (2 33) (8 13) (18. 12) 212 12
   8.  (2 33)  (6 13) 13 12 (18. 2) 202 2
   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   131. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   8. (2 31) (6 11) 11 10 (8. 0 0) 0 0 300 0
))

;704. x 896.
(SETQ TV-CPT-SYNC1 '(
   1.  (1 33) (5 13) 12 12 (10. 12 12) 212 113			;VERT SYNC, CLEAR TVMA
   53.  (1 33) (5 13) 12 12 (10. 12 12) 212 13			;VERT RETRACE
   8.  (1 31)  (5 11) 11 10 (10. 0 0) 200 21		;8 LINES OF MARGIN
   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   131. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   7. (1 31) (5 11) 11 10 (10. 0 0) 200 21
   1. (1 31) (5 11) 11 10 (10. 0 0) 300 23
))

;Sync for 64 Mhz crystal,  768. x 896.

(SETQ TV-CPT-SYNC2 '(
   1.  (1 33) (5 13) 12 12 (11. 12 12) 212 113			;VERT SYNC, CLEAR TVMA
   53.  (1 33) (5 13) 12 12 (11. 12 12) 212 13			;VERT RETRACE
   8.  (1 31)  (5 11) 11 10 (11. 0 0) 200 21		;8 LINES OF MARGIN
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   131. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   7. (1 31) (5 11) 11 10 (11. 0 0) 200 21
   1. (1 31) (5 11) 11 10 (11. 0 0) 300 23))

(SETQ TV-CPT-SYNC-60hz '(
   1.  (1 33) (5 13) 12 12 (11. 12 12) 212 113			;VERT SYNC, CLEAR TVMA
   53.  (1 33) (5 13) 12 12 (11. 12 12) 212 13			;VERT RETRACE
   8.  (1 31)  (5 11) 11 10 (11. 0 0) 200 21		;8 LINES OF MARGIN
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   200. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   7. (1 31) (5 11) 11 10 (11. 0 0) 200 21
   1. (1 31) (5 11) 11 10 (11. 0 0) 300 23
))

;;; This is the CPT-SYNC2 program (locations are in octal, repeats in decimal)
;Loc	Rpt	Hsync	Vsync	Comp	Blank	Other1		Other2
;0	1
;1		X	X		X	Refresh
;2-6		X	X		X
;7-36			X		X
;37			X		X			Eol
;40		X	X		X			Clr MA
;41	53.
;42		X	X		X	Refresh
;43-47		X	X		X
;50-77			X		X
;100			X		X			Eol
;101		X	X		X			Clr MA
;102	8.
;103		X			X	Refresh
;104
;;; to be continued
