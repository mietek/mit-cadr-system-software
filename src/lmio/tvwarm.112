;;; LISP MACHINE TV ROUTINES			-*-Mode:Lisp; Package:SI-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; New (8/28/78) version for 32-bit TVs

;THIS FILE IS THE STUFF WHICH DOESN'T GO IN THE COLD LOAD

;SEE FILE LMIO; TVDEFS > FOR STRUCTURES (SEPARATE FILE SO CAN BE AVAILABLE TO COMPILER)
;      THE FILE ALSO CONTAINS MOST SPECIAL VARIABLE DECLARATIONS

(DEFUN TV-HOLLOW-RECTANGULAR-BLINKER (BLINKER NEW-PHASE X Y &AUX W H)
  NEW-PHASE
  (SETF (TV-BLINKER-PHASE BLINKER) (NOT (TV-BLINKER-PHASE BLINKER)))
  (AND (ZEROP X) (SETQ X (1+ X)))			;AVOID RUNNING OFF LEFT EDGE OF SCREEN 
  (AND (ZEROP Y) (SETQ Y (1+ Y)))			;AVOID RUNNING OFF TOP EDGE OF SCREEN
  (COND ((NOT (TV-BLINKER-SIDEWAYS-P BLINKER))
	 (SETQ W (TV-BLINKER-WIDTH BLINKER)
	       H (TV-BLINKER-HEIGHT BLINKER)))
	((SETQ W (TV-BLINKER-HEIGHT BLINKER)
	       H (TV-BLINKER-WIDTH BLINKER)
	       X (PROG1 (SETQ Y X) (- TV-SCREEN-WIDTH Y W)))))
  (TV-ERASE W H X Y TV-ALU-XOR)			;DRAW SMALL RECTANGLE
  (TV-ERASE (+ W 2) (+ H 2) (1- X) (1- Y) TV-ALU-XOR))	;XOR BIG RECTANGLE, LEAVING OUTLINE

(DEFUN TV-IBEAM-BLINKER (BLINKER NEW-PHASE X Y &AUX X0 H)
  NEW-PHASE
  (SETF (TV-BLINKER-PHASE BLINKER) (NOT (TV-BLINKER-PHASE BLINKER)))
  (COND ((NOT (TV-BLINKER-SIDEWAYS-P BLINKER))
	 (SETQ H (TV-BLINKER-HEIGHT BLINKER))
	 (TV-ERASE 2 H (MAX 0 (1- X)) Y TV-ALU-XOR)	;VERTICAL LINE
	 (SETQ X0 (MAX 0 (- X 4)))
	 (TV-ERASE (- (+ X 5) X0) 2 X0 (MAX 0 (- Y 2)) TV-ALU-XOR)	;TOP CROSSBAR
	 (TV-ERASE (- (+ X 5) X0) 2 X0 (+ Y H) TV-ALU-XOR))	;BOTTOM CROSSBAR
	(T
	 (SETQ H (TV-BLINKER-HEIGHT BLINKER)
	       X (PROG1 (SETQ Y X) (- (SCREEN-HEIGHT (TV-BLINKER-SCREEN BLINKER)) Y H)))
	 (TV-ERASE H 2 X (MAX 0 (1- Y)) TV-ALU-XOR)		;VERTICAL LINE
	 (SETQ X0 (MAX 0 (- Y 4)))
	 (TV-ERASE 2 (- (+ Y 5) X0) (MAX 0 (- X 2)) X0 TV-ALU-XOR)	;TOP BAR
	 (TV-ERASE 2 (- (+ Y 5) X0) (+ Y H) X0 TV-ALU-XOR))))	;BOTTOM BAR

(DEFUN TV-CHARACTER-BLINKER-SET-SIZE (WIDTH HEIGHT) WIDTH HEIGHT NIL)
(DEFPROP TV-CHARACTER-BLINKER TV-CHARACTER-BLINKER-SET-SIZE TV-BLINKER-SET-SIZE-FUNCTION)
;BECAUSE TV-CHARACTER-BLINKER USES THE HEIGHT AND WIDTH FOR FUNNY THINGS,
;TV-SET-BLINKER-SIZE SHOULDN'T JUST CLOBBER THEM.

;WIDTH IS FONT, HEIGHT IS CHARACTER.  CURRENTLY THIS DOESN'T CHECK FOR OFF-THE-SCREEN.
(DEFUN TV-CHARACTER-BLINKER (BLINKER NEW-PHASE X Y
			     &AUX (FONT (TV-BLINKER-WIDTH BLINKER))
			          (CHAR (TV-BLINKER-HEIGHT BLINKER)) TEM)
  NEW-PHASE
  (SETF (TV-BLINKER-PHASE BLINKER) (NOT (TV-BLINKER-PHASE BLINKER)))
  (COND ((NULL (SETQ TEM (FONT-INDEXING-TABLE FONT)))
	 (COND ((NOT (TV-BLINKER-SIDEWAYS-P BLINKER))
		(TV-DRAW-CHAR FONT CHAR X Y TV-ALU-XOR))
	       ((TV-DRAW-CHAR FONT CHAR
			      (- (SCREEN-HEIGHT (TV-BLINKER-SCREEN BLINKER))
				 Y
				 (FONT-RASTER-WIDTH FONT))
			      X TV-ALU-XOR))))
	((NOT (TV-BLINKER-SIDEWAYS-P BLINKER))
	 (DO ((CH (AR-1 TEM CHAR) (1+ CH))
	      (LIM (AR-1 TEM (1+ CHAR)))
	      (XPOS X (+ XPOS (FONT-RASTER-WIDTH FONT))))
	     ((= CH LIM))
	     (TV-DRAW-CHAR FONT CH XPOS Y TV-ALU-XOR)))
	(T						;WIDE CHARACTER SIDEWAYS
	 (DO ((CH (AR-1 TEM CHAR) (1+ CH))
	      (LIM (AR-1 TEM (1+ CHAR)))
	      (YPOS X)
	      (XPOS (- (SCREEN-HEIGHT (TV-BLINKER-SCREEN BLINKER))
		       Y
		       (* (FONT-RASTER-WIDTH FONT)	;HAIRILY FIND TOP LEFT CORNER
			  (- (AR-1 TEM (1+ CHAR)) (AR-1 TEM CHAR))))
		    (+ XPOS (FONT-RASTER-WIDTH FONT))))
	     ((= CH LIM))
	     (TV-DRAW-CHAR FONT CH XPOS YPOS TV-ALU-XOR)))))

;;; Horizontal tab.   For now, 8 columns.  May want tab stops table?
(DEFUN TV-TAB (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR)) (TV-EXCEPTION PC-PPR))
     ((LAMBDA (TAB-WIDTH)
       (TV-MOVE-BITPOS PC-PPR (- TAB-WIDTH (\ (- (PC-PPR-CURRENT-X PC-PPR)
						 (PC-PPR-LEFT-MARGIN PC-PPR))
					      TAB-WIDTH))
		       0))
      (* 8 (PC-PPR-CHAR-WIDTH PC-PPR)))))

;;; Change the current font, adjusting the baseline etc.
;;; Should it TV-PREPARE-PC-PPR in case blinker or other process looks at font?
(DEFUN TV-SET-FONT (PC-PPR FONT)
  (SETF (PC-PPR-CURRENT-FONT PC-PPR) FONT)
  (SETF (PC-PPR-BASELINE-ADJ PC-PPR) (- (PC-PPR-BASELINE PC-PPR) (FONT-BASELINE FONT))))

;;; Routine to set "cursor" position of a piece of paper
;;; in raster units (-NOT- character units!)
;;; Note all cursorposes are in raster units relative to left and top margins.
(DEFUN TV-SET-CURSORPOS (PC-PPR X Y)
  (TV-PREPARE-PC-PPR (PC-PPR)
       (OR (ZEROP (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR)) (TV-EXCEPTION PC-PPR))
       (AND (PC-PPR-MORE-VPOS PC-PPR)		;If MORE processing enabled, put it off until
	    (SETF (PC-PPR-MORE-VPOS PC-PPR)	; bottom - i.e. last line that will fit
		  (1+ (- (PC-PPR-BOTTOM-LIMIT PC-PPR) (PC-PPR-LINE-HEIGHT PC-PPR)))))
       (SETF (PC-PPR-CURRENT-X PC-PPR)
	     (MIN (+ (PC-PPR-LEFT-MARGIN PC-PPR) X)
		  (PC-PPR-RIGHT-LIMIT PC-PPR)))
       (SETF (PC-PPR-CURRENT-Y PC-PPR)
	     (MIN (+ (PC-PPR-TOP-MARGIN PC-PPR) Y)
		  (PC-PPR-BOTTOM-LIMIT PC-PPR)))
       (SETF (PC-PPR-EXCEPTIONS PC-PPR) 0)))	;No exceptions since user can't move to end

;;; Returns two values, relative X and relative Y
(DEFUN TV-READ-CURSORPOS (PC-PPR)
  (PROG NIL
    (RETURN (- (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
	    (- (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR)))))

;;; Set the cursor position of a blinker relative to a pc ppr.
;;; If this blinker was following the pc ppr's cursor, it won't any more.
(DEFUN TV-SET-BLINKER-CURSORPOS (BLINKER X Y &AUX (PC-PPR (TV-BLINKER-PC-PPR BLINKER)))
  (WITHOUT-INTERRUPTS
    (LET ((OLD-PHASE (TV-BLINKER-PHASE BLINKER)))
      (COND (PC-PPR
	     (SETQ X (MIN (+ (PC-PPR-LEFT-MARGIN PC-PPR) X) (PC-PPR-RIGHT-LIMIT PC-PPR)))
	     (SETQ Y (MIN (+ (PC-PPR-TOP-MARGIN PC-PPR) Y) (PC-PPR-BOTTOM-LIMIT PC-PPR)))))
      (COND ((AND (= X (TV-BLINKER-X-POS BLINKER))	;Don't blink if not really moving
		  (= Y (TV-BLINKER-Y-POS BLINKER)))) 
	    ((OR (NULL PC-PPR) (ZEROP (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR)))
	     (TV-OPEN-BLINKER BLINKER)
	     (SETF (TV-BLINKER-X-POS BLINKER) X)
	     (SETF (TV-BLINKER-Y-POS BLINKER) Y)
	     (AND (TV-BLINKER-VISIBILITY BLINKER)
		  (NEQ (TV-BLINKER-VISIBILITY BLINKER) 'BLINK)	;If non-blinking, don't
		  OLD-PHASE				;disappear for a long time.
		  (TV-BLINK BLINKER T)))
	    (T
	     (SETF (TV-BLINKER-X-POS BLINKER) X)
	     (SETF (TV-BLINKER-Y-POS BLINKER) Y))))))

;;; Read the cursor position of a blinker relative to a pc ppr
(DEFUN TV-READ-BLINKER-CURSORPOS (BLINKER &AUX (PC-PPR (TV-BLINKER-PC-PPR BLINKER)))
  (PROG NIL
    (OR PC-PPR (RETURN (TV-BLINKER-X-POS BLINKER) (TV-BLINKER-Y-POS BLINKER)))
    (RETURN (- (OR (TV-BLINKER-X-POS BLINKER) (PC-PPR-CURRENT-X PC-PPR))
	       (PC-PPR-LEFT-MARGIN PC-PPR))
	    (- (OR (TV-BLINKER-Y-POS BLINKER) (PC-PPR-CURRENT-Y PC-PPR))
	       (PC-PPR-TOP-MARGIN PC-PPR)))))

;;; Carefully alter the visibility of a blinker
;;; VISIBILITY is NIL (off), T (on), or BLINK
(DEFUN TV-SET-BLINKER-VISIBILITY (BLINKER VISIBILITY)
  (OR (EQ (TV-BLINKER-VISIBILITY BLINKER) VISIBILITY)
      (WITHOUT-INTERRUPTS
	(OR VISIBILITY (TV-OPEN-BLINKER BLINKER))
	(SETF (TV-BLINKER-VISIBILITY BLINKER) VISIBILITY)
	(COND ((NULL (TV-BLINKER-PC-PPR BLINKER))
	       ;; A roving blinker should be on TV-ROVING-BLINKER-LIST only if visible.
	       (COND (VISIBILITY
		       (OR (MEMQ BLINKER TV-ROVING-BLINKER-LIST)
			   (PUSH BLINKER TV-ROVING-BLINKER-LIST)))
		     (T (SETQ TV-ROVING-BLINKER-LIST
			      (DELQ BLINKER TV-ROVING-BLINKER-LIST))))))
	;; Arrange for TV-BLINKER-CLOCK to fix the screen.
	(SETF (TV-BLINKER-TIME-UNTIL-BLINK BLINKER) 0))))

;;; Change the function to be called by a blinker, and the arguments
(DEFUN TV-SET-BLINKER-FUNCTION (BLINKER FUNCTION
				&OPTIONAL (ARG1 (TV-BLINKER-WIDTH BLINKER))
				          (ARG2 (TV-BLINKER-HEIGHT BLINKER))
				&AUX OLD-PHASE)
  (WITHOUT-INTERRUPTS
    (COND ((NOT (AND (EQ (TV-BLINKER-FUNCTION BLINKER) FUNCTION)
		     (EQ (TV-BLINKER-WIDTH BLINKER) ARG1)
		     (EQ (TV-BLINKER-HEIGHT BLINKER) ARG2)))
	   (SETQ OLD-PHASE (TV-BLINKER-PHASE BLINKER))
	   (TV-OPEN-BLINKER BLINKER)
	   (SETF (TV-BLINKER-FUNCTION BLINKER) FUNCTION)
	   (SETF (TV-BLINKER-WIDTH BLINKER) ARG1)
	   (SETF (TV-BLINKER-HEIGHT BLINKER) ARG2)
	   (AND (TV-BLINKER-VISIBILITY BLINKER)
		(NEQ (TV-BLINKER-VISIBILITY BLINKER) 'BLINK)	;If non-blinking, don't
		OLD-PHASE				;disappear for a long time.
		(TV-BLINK BLINKER T))))))

(DEFUN TV-SET-BLINKER-SIZE (BLINKER WIDTH HEIGHT &AUX FUNCTION)
  (WITHOUT-INTERRUPTS
    (COND ((NOT (AND (= (TV-BLINKER-WIDTH BLINKER) WIDTH)
		     (= (TV-BLINKER-HEIGHT BLINKER) HEIGHT)))
	   (TV-OPEN-BLINKER BLINKER)
	   (SETQ FUNCTION (GET (TV-BLINKER-FUNCTION BLINKER) 'TV-SET-BLINKER-SIZE-FUNCTION))
	   (COND (FUNCTION (FUNCALL FUNCTION WIDTH HEIGHT))
		 (T (SETF (TV-BLINKER-WIDTH BLINKER) WIDTH)
		    (SETF (TV-BLINKER-HEIGHT BLINKER) HEIGHT)))))))

;;; Clear character area only, not margins
(DEFUN TV-CLEAR-PC-PPR-EXCEPT-MARGINS (PC-PPR)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (TV-HOME PC-PPR)			;Will take care of exceptions
     ;; Now clear all but margins
     (COND ((ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))
	    (TV-ERASE (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
		      (- (PC-PPR-BOTTOM-MARGIN PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR))
		      (PC-PPR-LEFT-MARGIN PC-PPR)
		      (PC-PPR-TOP-MARGIN PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR)))
	   ((TV-ERASE (- (PC-PPR-BOTTOM-MARGIN PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR))
		      (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
		      (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR))
			 (PC-PPR-BOTTOM-MARGIN PC-PPR))
		      (PC-PPR-LEFT-MARGIN PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR))))))

;;; Clear to end of pc ppr
(DEFUN TV-CLEAR-EOF (PC-PPR &AUX HT TEM)
  (TV-PREPARE-PC-PPR (PC-PPR)
     (TV-CLEAR-EOL PC-PPR)		;Will take care of exceptions
     (COND ((<= (SETQ HT (- (PC-PPR-BOTTOM-MARGIN PC-PPR)
			    (SETQ TEM (+ (PC-PPR-CURRENT-Y PC-PPR)
					 (PC-PPR-LINE-HEIGHT PC-PPR)))))
		0))	;Just the one line to be done
	   ((ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))
	    (TV-ERASE (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
		      HT 
		      (PC-PPR-LEFT-MARGIN PC-PPR)
		      TEM
		      (PC-PPR-ERASE-ALUF PC-PPR)))
	   ((TV-ERASE HT 
		      (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
		      (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR)) TEM HT)
		      (PC-PPR-LEFT-MARGIN PC-PPR)
		      (PC-PPR-ERASE-ALUF PC-PPR))))))

(DEFUN TV-HOME-DOWN (PC-PPR)
  (TV-SET-CURSORPOS PC-PPR 0 (- (PC-PPR-BOTTOM-LIMIT PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR))))

(DEFUN TV-INSERT-LINE (PC-PPR &OPTIONAL (LINE-COUNT 1))
 (TV-PREPARE-PC-PPR (PC-PPR)
  (LET ((SCREEN-ARRAY (SCREEN-BUFFER-PIXEL-ARRAY (PC-PPR-SCREEN PC-PPR)))
        (WIDTH (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR)))
        (HEIGHT (* LINE-COUNT (PC-PPR-LINE-HEIGHT PC-PPR)))
        DELTA-HEIGHT)
       ;; Compute minus height of block to blt.
       (SETQ DELTA-HEIGHT (- HEIGHT (- (PC-PPR-BOTTOM-MARGIN PC-PPR)
                                       (PC-PPR-CURRENT-Y PC-PPR))))
       (OR (PLUSP DELTA-HEIGHT)
           (BITBLT TV-ALU-SETA
                   WIDTH
                   DELTA-HEIGHT
                   SCREEN-ARRAY
                   (PC-PPR-LEFT-MARGIN PC-PPR)
                   (PC-PPR-CURRENT-Y PC-PPR)
                   SCREEN-ARRAY
                   (PC-PPR-LEFT-MARGIN PC-PPR)
                   (+ (PC-PPR-CURRENT-Y PC-PPR) HEIGHT)))
       (TV-ERASE WIDTH HEIGHT (PC-PPR-LEFT-MARGIN PC-PPR) (PC-PPR-CURRENT-Y PC-PPR)
                 (PC-PPR-ERASE-ALUF PC-PPR)))))

(DEFUN TV-DELETE-LINE (PC-PPR &OPTIONAL (LINE-COUNT 1))
 (TV-PREPARE-PC-PPR (PC-PPR)
  (LET ((SCREEN-ARRAY (SCREEN-BUFFER-PIXEL-ARRAY (PC-PPR-SCREEN PC-PPR)))
        (WIDTH (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR)))
        (HEIGHT (* LINE-COUNT (PC-PPR-LINE-HEIGHT PC-PPR)))
        DELTA-HEIGHT)
       (OR (MINUSP (SETQ DELTA-HEIGHT
                         (- (+ (PC-PPR-CURRENT-Y PC-PPR) HEIGHT)
                            (PC-PPR-BOTTOM-MARGIN PC-PPR))))
           (FERROR NIL "Illegal line-count ~S for ~S" LINE-COUNT PC-PPR))
       (BITBLT TV-ALU-SETA
               WIDTH
               (- DELTA-HEIGHT)
               SCREEN-ARRAY
               (PC-PPR-LEFT-MARGIN PC-PPR)
               (+ (PC-PPR-CURRENT-Y PC-PPR) HEIGHT)
               SCREEN-ARRAY
               (PC-PPR-LEFT-MARGIN PC-PPR)
               (PC-PPR-CURRENT-Y PC-PPR))
       (TV-ERASE WIDTH
                 HEIGHT
                 (PC-PPR-LEFT-MARGIN PC-PPR)
                 (- (PC-PPR-CURRENT-Y PC-PPR) DELTA-HEIGHT)
                 (PC-PPR-ERASE-ALUF PC-PPR)))))

;INSERT A CHARACTER AT THE CURRENT CURSOR POSITION.  ONLY WORKS FOR FIXED WIDTH FONTS!
(DEFUN TV-INSERT-CHAR (PC-PPR &OPTIONAL (CHAR-COUNT 1))
 (TV-PREPARE-PC-PPR (PC-PPR)
  (LET ((SCREEN-ARRAY (SCREEN-BUFFER-PIXEL-ARRAY (PC-PPR-SCREEN PC-PPR)))
        (LINE-HEIGHT (PC-PPR-LINE-HEIGHT PC-PPR))
        (WIDTH (* CHAR-COUNT (PC-PPR-CHAR-WIDTH PC-PPR))))
       (BITBLT TV-ALU-SETA
               (- WIDTH (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-CURRENT-X PC-PPR)))
               LINE-HEIGHT
               SCREEN-ARRAY
               (PC-PPR-CURRENT-X PC-PPR)
               (PC-PPR-CURRENT-Y PC-PPR)
               SCREEN-ARRAY
               (+ (PC-PPR-CURRENT-X PC-PPR) WIDTH)
               (PC-PPR-CURRENT-Y PC-PPR))
       (TV-ERASE WIDTH
                 LINE-HEIGHT
                 (PC-PPR-CURRENT-X PC-PPR)
                 (PC-PPR-CURRENT-Y PC-PPR)
                 (PC-PPR-ERASE-ALUF PC-PPR)))))

;DELETE A CHARACTER AT THE CURRENT CURSOR POSITION.  ONLY WORKS FOR FIXED WIDTH FONTS!
(DEFUN TV-DELETE-CHAR (PC-PPR &OPTIONAL (CHAR-COUNT 1))
 (TV-PREPARE-PC-PPR (PC-PPR)
  (LET ((SCREEN-ARRAY (SCREEN-BUFFER-PIXEL-ARRAY (PC-PPR-SCREEN PC-PPR)))
        (LINE-HEIGHT (PC-PPR-LINE-HEIGHT PC-PPR))
        (WIDTH (* CHAR-COUNT (PC-PPR-CHAR-WIDTH PC-PPR))))
       (BITBLT TV-ALU-SETA
               (- (- (PC-PPR-RIGHT-MARGIN PC-PPR) (PC-PPR-CURRENT-X PC-PPR)) WIDTH)
               LINE-HEIGHT
               SCREEN-ARRAY
               (+ (PC-PPR-CURRENT-X PC-PPR) WIDTH)
               (PC-PPR-CURRENT-Y PC-PPR)
               SCREEN-ARRAY
               (PC-PPR-CURRENT-X PC-PPR)
               (PC-PPR-CURRENT-Y PC-PPR))
       (TV-ERASE WIDTH
                 LINE-HEIGHT
                 (- (PC-PPR-RIGHT-MARGIN PC-PPR) WIDTH)
                 (PC-PPR-CURRENT-Y PC-PPR)
                 (PC-PPR-ERASE-ALUF PC-PPR)))))

(DEFUN TV-BEEP ()
  (OR TV-BEEP (TV-COMPLEMENT-BOW-MODE))
  (%BEEP TV-BEEP-WAVELENGTH TV-BEEP-DURATION)
  (OR TV-BEEP (TV-COMPLEMENT-BOW-MODE)))

;;; The following no longer accept plane masks, and no longer support 16-bit TV's.
;;; They need to find the TV control register from the screen; for now they
;;; just work on the main controller.
(DEFUN TV-BLACK-ON-WHITE (&OPTIONAL (SCREEN TV-DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGIOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))

(DEFUN TV-WHITE-ON-BLACK (&OPTIONAL (SCREEN TV-DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGAND -5 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN))))) ;1's comp of 4

(DEFUN TV-COMPLEMENT-BOW-MODE (&OPTIONAL (SCREEN TV-DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGXOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))

(DEFUN TV-WHITE-ON-BLACK-STATE (&OPTIONAL (SCREEN TV-DEFAULT-SCREEN))
  (ZEROP (LOGAND 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))

;This routine is used by the editor's display routines to output one line.
;The argument is a string of either 8-bit or 16-bit characters (usually
;this is an ED-LINE, but the leader is not touched except for the fill pointer.)
;The %%CH-FONT byte of each character is the index into the font map
;of the font in which that character is to be displayed.  8-bit chars use font 0.
;There are optional starting and ending indices; if these are omitted the
;whole string is specified.
;If the specified area of the string exceeds a physical line, typeout stops
;and the index of the next character to be output is returned.  At this point,
;the pc ppr is pointing at the beginning of the next line (i.e. PC-PPR-END-LINE-FCN
;has been invoked.)  If the whole string is successfully output, NIL is returned,
;and the pc ppr is pointing somewhere in the middle of the line.
;Understands format effectors (special keys 200-237).

(DEFUN TV-LINE-OUT (PC-PPR STRING &OPTIONAL (START 0) (END NIL))
  (TV-PREPARE-PC-PPR (PC-PPR)
     (PROG ((I START)
	    (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	    (LINECHECK (PC-PPR-CURRENT-Y PC-PPR))
            KERN-TABLE KERN
	    (SIDEWAYS-P (NOT (ZEROP (PC-PPR-SIDEWAYS-P PC-PPR))))
	    XPOS YPOS YCROC XLIM ALUF WIDTH CH FONT FONTX TEM)
   HD  (OR (ZEROP (PC-PPR-EXCEPTIONS PC-PPR)) (TV-EXCEPTION PC-PPR))
       (AND (>= I N) (RETURN NIL))
       (SETQ CH (AR-1 STRING I) I (1+ I))
       (COND ((NEQ FONTX (SETQ TEM (LDB %%CH-FONT CH)))	    ;CHANGING TO A NEW FONT
	      (SETQ FONTX TEM)
	      (TV-SET-FONT PC-PPR (SETQ FONT (AR-1 (PC-PPR-FONT-MAP PC-PPR) FONTX)))
	      (AND (NULL (FONT-INDEXING-TABLE FONT))
		   (NULL (FONT-CHAR-WIDTH-TABLE FONT))
		   (GO EZ))			    ;HANDLE EASY CASE FAST
	      ))
   HD1 (TV-TYO PC-PPR (LDB %%CH-CHAR CH))
       (OR (= (PC-PPR-CURRENT-Y PC-PPR) LINECHECK) (RETURN I)) ;RAN OFF END OF LINE
       (GO HD)
   
       ;THIS LOOP IS FOR SIMPLE FONTS THAT DON'T NEED FULL HAIR OF TVO-TYO.
   EZ  (SETQ XPOS (PC-PPR-CURRENT-X PC-PPR)
	     ALUF (PC-PPR-CHAR-ALUF PC-PPR)
	     XLIM (PC-PPR-RIGHT-LIMIT PC-PPR))
   EZ0 (SETQ WIDTH (FONT-CHAR-WIDTH FONT)
             KERN-TABLE (FONT-LEFT-KERN-TABLE FONT)
	     YPOS (+ (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-BASELINE-ADJ PC-PPR)))
       (SETQ YCROC (- (SCREEN-HEIGHT (PC-PPR-SCREEN PC-PPR))
                      YPOS (FONT-RASTER-WIDTH FONT)))
   EZ1 (COND ((< (SETQ CH (LDB %%CH-CHAR CH)) 200)	    ;PRINTING CHARACTER
              (SETQ KERN (COND (KERN-TABLE (AR-1 KERN-TABLE CH)) (T 0)))
	      (COND ((NOT SIDEWAYS-P)
		     (TV-DRAW-CHAR FONT CH (- XPOS KERN) YPOS ALUF))
		    ((TV-DRAW-CHAR FONT CH YCROC (- XPOS KERN) ALUF)))
	      (COND ((> (SETQ XPOS (+ XPOS WIDTH)) XLIM)	    ;CHECK FOR RUNNING OFF END OF LINE
		     (TV-MOVE-BITPOS PC-PPR (- XPOS (PC-PPR-CURRENT-X PC-PPR)) 0)
		     (RETURN I))))
	     (T (GO EZX)))				    ;FORMAT EFFECTOR, CALL FULL TYO
       ;GET NEXT CHARACTER, IF ANY LEFT
       (AND (>= I N) (GO EX))
       (SETQ CH (AR-1 STRING I) I (1+ I))
       (COND ((NEQ FONTX (SETQ TEM (LDB %%CH-FONT CH)))	    ;CHANGING TO A NEW FONT
	      (SETQ FONTX TEM)
	      (TV-SET-FONT PC-PPR (SETQ FONT (AR-1 (PC-PPR-FONT-MAP PC-PPR) FONTX)))
	      (AND (NULL (FONT-INDEXING-TABLE FONT))
		   (NULL (FONT-CHAR-WIDTH-TABLE FONT))
		   (GO EZ0))			    ;HANDLE EASY CASE FAST
	      (GO EZX)))
       (GO EZ1)
   
   EZX (TV-MOVE-BITPOS PC-PPR (- XPOS (PC-PPR-CURRENT-X PC-PPR)) 0) ;ESCAPE TO HARD CASE
       (SETQ FONTX NIL)				    ;HAVE TO RESELECT FONT
       (GO HD1)					    ;GO TYPE OUT CHAR AND ENTER HD LOOP
   
   EX  (TV-MOVE-BITPOS PC-PPR (- XPOS (PC-PPR-CURRENT-X PC-PPR)) 0) ;DONE
       (RETURN NIL))))

;COMPUTE THE MOTION THAT WOULD BE CAUSED BY OUTPUTTING A STRING.
;THIS IS USED BY THE EDITOR.
;NOTE THAT THIS DOES NOT USE THE "CASE SHIFT" FLAVOR OF FONT HACKING.
; INSTEAD, IT USES THE 16-BIT-CHARACTER FLAVOR THAT THE EDITOR USES.
; THIS MEANS THAT IF YOU GIVE IT AN ORDINARY 8-BIT STRING IT WILL
; BE ASSUMED TO BE ALL IN FONT 0.
;ARGS ARE: THE PIECE OF PAPER, THE X AND Y POSITION TO START AT (NILS HERE USE THE CURRENT
; POSITION OF THE PC PPR), THE STRING, AND OPTIONALLY THE STARTING AND ENDING INDICES
; AND A FLAG SAYING TO FAKE A CRLF AT END OF THE STRING.
; OPTIONALLY YOU CAN GIVE TWO ADDITIONAL ARGUMENTS WHICH ARE THE X AND Y TO STOP AT,
; IF NOT GIVEN THESE DEFAULT TO THE END OF THE SCREEN.
;RETURNS 3 VALUES: FINAL-X, FINAL-Y, AND AN INDICATION OF HOW FAR DOWN THE
; STRING IT GOT.  THIS IS NIL IF THE WHOLE STRING (INCLUDING THE FAKE
; CARRIAGE RETURN, IF ANY) WAS PROCESSED WITHOUT
; REACHING THE STOPPING POINT, OR THE INDEX OF THE NEXT CHARACTER TO BE
; PROCESSED WHEN THE STOPPING POINT WAS REACHED, OR T IF THE STOPPING POINT
; WAS REACHED AFTER THE FAKE CARRIAGE RETURN.
;ALL COORDINATES HEREIN ARE RELATIVE TO THE MARGINS, LIKE THE USER COORDINATE
; SYSTEM AND UNLIKE THE INTERNAL COORDINATE SYSTEM THE OTHER FUNCTIONS USE.

;New 3 times faster version
(DEFUN TV-COMPUTE-MOTION (PC-PPR X Y STRING
			  &OPTIONAL (START 0) (END NIL) (CR-AT-END-P NIL)
			  (STOP-X 0) (STOP-Y NIL))
  (PROG (RIGHT-LIMIT CWA CW CH FONT FONTX TEM I N NN II EOL-FLAG)
    (AND (NULL X) (SETQ X (- (PC-PPR-CURRENT-X PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))))
    (AND (NULL Y) (SETQ Y (- (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR))))
    (AND (NULL STOP-Y)
	 (SETQ STOP-Y (1+ (- (PC-PPR-BOTTOM-LIMIT PC-PPR) (PC-PPR-TOP-MARGIN PC-PPR)))))
		    ;   ^-- THIS 1+ IS SO CAN USE >= RATHER THAN >
    (SETQ RIGHT-LIMIT (- (PC-PPR-RIGHT-LIMIT PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR))
	  I START
	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  FONT (PC-PPR-CURRENT-FONT PC-PPR)
	  CW (FONT-CHAR-WIDTH FONT))
    ;At this point, decide whether we can use the fast version
    (AND (= (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0) ART-STRING) ;i.e. no font changes
	 (NULL (SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))		  ;and fixed width
	 (GO FAST))
    ;This is the slow version, about the same as before.  Not a DO anymore.
SLOW
    (COND ((AND (>= Y STOP-Y) (>= X STOP-X))	;REACHED STICKING-POINT
	   (RETURN X Y I))
	  ((NOT (< I N))			;IF STRING EXHAUSTED
	   (AND (OR CR-AT-END-P EOL-FLAG)	;DON'T RETURN X OFF END OF LINE
		(SETQ X 0 Y (+ Y (PC-PPR-LINE-HEIGHT PC-PPR))))
	   (RETURN X Y (AND (>= X STOP-X) (>= Y STOP-Y)))))
    (SETQ CH (LDB %%CH-CHAR (SETQ TEM (AR-1 STRING I))))
    (COND ((NEQ (SETQ TEM (LDB %%CH-FONT TEM)) FONTX)	;CHANGING FONTS
	   (SETQ FONTX TEM
		 FONT (AR-1 (PC-PPR-FONT-MAP PC-PPR) FONTX)
		 CWA (FONT-CHAR-WIDTH-TABLE FONT)
		 CW (FONT-CHAR-WIDTH FONT))))
    (COND ((= CH 215)				;CRLF
	   (SETQ EOL-FLAG NIL
		 X 0 Y (+ Y (PC-PPR-LINE-HEIGHT PC-PPR))))
	  (EOL-FLAG				;FORCE CRLF WITHOUT GOBBLING A CHAR
	   (SETQ EOL-FLAG NIL
		 X 0 Y (+ Y (PC-PPR-LINE-HEIGHT PC-PPR)))
	   (GO SLOW))
	  ((< CH 200)					;PRINTING CHARACTER
	   (SETQ X (+ (COND (CWA (AR-1 CWA CH)) (T CW)) X))) ;DO CHAR WIDTH
	  ((= CH 210)					;BACKSPACE
	   (SETQ X (MAX 0 (- X (PC-PPR-CHAR-WIDTH PC-PPR)))))
	  ((= CH 211)					;TAB
	   (SETQ TEM (* 8 (PC-PPR-CHAR-WIDTH PC-PPR)))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  ((< CH 240)					;INVISIBLE FORMAT EFFECTOR
	   (SETQ X (+ X (AR-1 (FONT-CHAR-WIDTH-TABLE FONTS:ARROW) (- CH 40)))))
	  )						;FONT CHANGES NOT ALLOWED.
    (SETQ I (1+ I))					;NEXT LOOP
    (COND ((> X RIGHT-LIMIT)				;AUTO CRLF AT LINE END
	   (SETQ EOL-FLAG T)))
    (GO SLOW)

    ;Here is the fast loop.  The basic idea is to scan as fast as possible
    ;over printing characters, with all checking outside the loop.
FAST 
    ;First, decide the most characters we want to scan over in a whack
    (SETQ NN (MIN (+ (// (- (COND ((>= Y STOP-Y)	;Stop-point is in this line
				   STOP-X)
				  (T RIGHT-LIMIT))	;Stop for this line is margin
			    X)
			 CW)
		     I)
		  N))				;NN is limiting value of I
    ;Now, scan over printing characters.
    (AND (>= (SETQ II I) NN)			;Save initial I, and check for null loop
	 (GO SCX))
    (SETQ TEM 200)				;This is really a ridiculous bum
SCN (AND (< (AR-1 STRING I) TEM)		;If this is a printing character
	 (< (SETQ I (1+ I)) NN)			; and we haven't reached stop point
	 (GO SCN))				; then continue to loop (9 instructions)
    (AND (> (SETQ X (+ (* (- I II) CW) X))	;Account for the motion of those chars
	    RIGHT-LIMIT)			;Check for right margin
	 (SETQ EOL-FLAG T))			;We will need crlf before any other chars
SCX (COND ((AND (>= Y STOP-Y) (>= X STOP-X))	;If reached sticking-point, done.
	   (RETURN X Y I))
	  ((NOT (< I N))			;If string exhausted
	   (AND (OR CR-AT-END-P EOL-FLAG)	;Don't return X off end of line
		(SETQ X 0 Y (+ Y (PC-PPR-LINE-HEIGHT PC-PPR))))
	   (RETURN X Y (AND (>= X STOP-X) (>= Y STOP-Y)))))
    (COND ((= (SETQ CH (AR-1 STRING I)) 215)	;CRLF
	   (SETQ EOL-FLAG NIL
		 X 0 Y (+ Y (PC-PPR-LINE-HEIGHT PC-PPR))))
	  (EOL-FLAG				;FORCE CRLF WITHOUT GOBBLING A CHAR
	   (SETQ EOL-FLAG NIL
		 X 0 Y (+ Y (PC-PPR-LINE-HEIGHT PC-PPR)))
	   (GO SCX))
	  ((< CH 200)				;PRINTING CHARACTER
	   (SETQ X (+ CW X)))			;DO CHAR WIDTH
	  ((= CH 210)				;BACKSPACE
	   (SETQ X (MAX 0 (- X (PC-PPR-CHAR-WIDTH PC-PPR)))))
	  ((= CH 211)				;TAB
	   (SETQ TEM (* 8 (PC-PPR-CHAR-WIDTH PC-PPR)))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  ((< CH 240)				;INVISIBLE FORMAT EFFECTOR
	   (SETQ X (+ X (AR-1 (FONT-CHAR-WIDTH-TABLE FONTS:ARROW) (- CH 40)))))
	  )					;FONT CHANGES NOT ALLOWED
    (SETQ I (1+ I))				;NEXT LOOP
    (COND ((> X RIGHT-LIMIT)
	   (SETQ EOL-FLAG T)
	   (GO SCX)))
    (GO FAST)
))

;This function returns the width of a character, in raster units.
;For backspace, it can return a negative number.
;For tab, the number returned depends on the current cursor position.
;For return, the result is zero.
(DEFUN TV-CHAR-WIDTH (PC-PPR CH FONT &AUX TEM)
  (COND ((< CH 200)  ;ORDINARY PRINTING CHARACTER
	 (COND ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT)) (AR-1 TEM CH))
	       (T (FONT-CHAR-WIDTH FONT))))
	((= CH 215) 0)	;RETURN
	((= CH 210) (MINUS (PC-PPR-CHAR-WIDTH PC-PPR)))	;BACKSPACE
	((= CH 211)	;TAB
	 (SETQ TEM (* 8 (PC-PPR-CHAR-WIDTH PC-PPR)))
	 (- (* (// (+ (PC-PPR-CURRENT-X PC-PPR) TEM) TEM) TEM)
	    (PC-PPR-CURRENT-X PC-PPR)))
	((< CH 240)	;MISC INVISIBLE FORMAT EFFECTOR
	 (AR-1 (FONT-CHAR-WIDTH-TABLE FONTS:ARROW) (- CH 40)))
	(T 0)))		;FONT CHANGE OR SOMETHING

;;; This is like TV-COMPUTE-MOTION, but in one dimension only
;;; Returned values are X and I.
(DEFUN TV-STRING-LENGTH (PC-PPR STRING &OPTIONAL (START 0) (END NIL) (STOP-X NIL))
  (PROG (CWA CW CH FONT FONTX TEM I N NN II (X 0))
    (SETQ I START
	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  FONT (PC-PPR-CURRENT-FONT PC-PPR)
	  CW (FONT-CHAR-WIDTH FONT))
    ;At this point, decide whether we can use the fast version
    (AND (= (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0) ART-STRING) ;i.e. no font changes
	 (NULL (SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))		  ;and fixed width
	 (GO FAST))
SLOW
    (COND ((OR (AND STOP-X (>= X STOP-X))		;If reached limit or string exhausted
	       (NOT (< I N)))
	   (RETURN X I)))
    (SETQ CH (LDB %%CH-CHAR (SETQ TEM (AR-1 STRING I))))
    (COND ((NEQ (SETQ TEM (LDB %%CH-FONT TEM)) FONTX)	;CHANGING FONTS
	   (SETQ FONTX TEM
		 FONT (AR-1 (PC-PPR-FONT-MAP PC-PPR) FONTX)
		 CWA (FONT-CHAR-WIDTH-TABLE FONT)
		 CW (FONT-CHAR-WIDTH FONT))))
    (COND ((< CH 200)					;PRINTING CHARACTER
	   (SETQ X (+ (COND (CWA (AR-1 CWA CH)) (T CW)) X))) ;DO CHAR WIDTH
	  ((= CH 210)					;BACKSPACE
	   (SETQ X (MAX 0 (- X (PC-PPR-CHAR-WIDTH PC-PPR)))))
	  ((= CH 211)					;TAB
	   (SETQ TEM (* 8 (PC-PPR-CHAR-WIDTH PC-PPR)))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  ((= CH 215)					;CRLF
	   ) ;Ignore it, I don't know what else to do
	  ((< CH 240)					;INVISIBLE FORMAT EFFECTOR
	   (SETQ X (+ X (AR-1 (FONT-CHAR-WIDTH-TABLE FONTS:ARROW) (- CH 40)))))
	  )						;FONT CHANGES NOT ALLOWED.
    (SETQ I (1+ I))					;NEXT LOOP
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
    (AND (>= (SETQ II I) NN)			;Save initial I, and check for null loop
	 (GO SCX))
    (SETQ TEM 200)				;This is really a ridiculous bum
SCN (AND (< (AR-1 STRING I) TEM)		;If this is a printing character
	 (< (SETQ I (1+ I)) NN)			; and we haven't reached stop point
	 (GO SCN))				; then continue to loop (9 instructions)
    (SETQ X (+ (* (- I II) CW) X))		;Account for the motion of those chars
SCX (COND ((OR (AND STOP-X (>= X STOP-X))	;If string exhausted or reached limit
               (NOT (< I N)))
	   (RETURN X I))
	  ((= (SETQ CH (AR-1 STRING I)) 215)	;CRLF
	   ) ;I don't know what to do
	  ((< CH 200)				;PRINTING CHARACTER
	   (SETQ X (+ CW X)))			;DO CHAR WIDTH
	  ((= CH 210)				;BACKSPACE
	   (SETQ X (MAX 0 (- X (PC-PPR-CHAR-WIDTH PC-PPR)))))
	  ((= CH 211)				;TAB
	   (SETQ TEM (* 8 (PC-PPR-CHAR-WIDTH PC-PPR)))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  ((< CH 240)				;INVISIBLE FORMAT EFFECTOR
	   (SETQ X (+ X (AR-1 (FONT-CHAR-WIDTH-TABLE FONTS:ARROW) (- CH 40)))))
	  )					;FONT CHANGES NOT ALLOWED
    (SETQ I (1+ I))				;NEXT LOOP
    (GO FAST)
))

;REDEFINE SOME OF THE PARAMETERS OF A PC PPR
;FOR NOW, ONLY THE BOUNDARIES AND MARGINS AND FONTS CAN BE CHANGED, AND MORE-P
;CURRENTLY, IT FORGETS THE OLD INTEGRAL-P
;SINCE IT IS NOT INDEPENDENTLY REMEMBERED IN THE PIECE OF PAPER.
;HAIR ADDED 9/3/78 TO DEDUCE WHAT OLD VSP MUST HAVE BEEN
(DEFUN TV-REDEFINE-PC-PPR (PC-PPR &REST &EVAL OPTIONS
			   &AUX (TOP (PC-PPR-TOP PC-PPR))
			        (BOTTOM (PC-PPR-BOTTOM PC-PPR))
				(LEFT (PC-PPR-LEFT PC-PPR))
				(RIGHT (PC-PPR-RIGHT PC-PPR))
				(TOP-MARGIN (- (PC-PPR-TOP-MARGIN PC-PPR)
					       (PC-PPR-TOP PC-PPR)))
				(BOTTOM-MARGIN (- (PC-PPR-BOTTOM PC-PPR)
						  (PC-PPR-BOTTOM-MARGIN PC-PPR)))
				(LEFT-MARGIN (- (PC-PPR-LEFT-MARGIN PC-PPR)
						(PC-PPR-LEFT PC-PPR)))
				(RIGHT-MARGIN (- (PC-PPR-RIGHT PC-PPR)
						 (PC-PPR-RIGHT-MARGIN PC-PPR)))
				(INTEGRAL-P NIL)
				(MORE-P (PC-PPR-MORE-VPOS PC-PPR))
				(OLD-X (- (PC-PPR-CURRENT-X PC-PPR)
					  (PC-PPR-LEFT-MARGIN PC-PPR)))
				(OLD-Y (- (PC-PPR-CURRENT-Y PC-PPR)
					  (PC-PPR-TOP-MARGIN PC-PPR)))
				FONT-MAP VSP SCREEN OLD-LEFT OLD-TOP)
  (SETQ VSP (- (PC-PPR-LINE-HEIGHT PC-PPR)
               (DO ((I 0 (1+ I))
                    (N (ARRAY-DIMENSION-N 1 (PC-PPR-FONT-MAP PC-PPR)))
                    (H 0))
                   ((= I N) H)
                (SETQ H (MAX H (FONT-CHAR-HEIGHT (AR-1 (PC-PPR-FONT-MAP PC-PPR) I)))))))
  (DO OP OPTIONS (CDDR OP) (NULL OP)
      (SELECTQ (CAR OP)
	(:FONTS (SETQ FONT-MAP (CADR OP)))
	(:VSP (SETQ VSP (CADR OP)
		    FONT-MAP (OR FONT-MAP (PC-PPR-FONT-MAP PC-PPR))))
	(:TOP (SETQ TOP (CADR OP)))
	(:BOTTOM (SETQ BOTTOM (CADR OP)))
	(:LEFT (SETQ LEFT (CADR OP)))
	(:RIGHT (SETQ RIGHT (CADR OP)))
	(:TOP-MARGIN (SETQ TOP-MARGIN (CADR OP)))
	(:BOTTOM-MARGIN (SETQ BOTTOM-MARGIN (CADR OP)))
	(:LEFT-MARGIN (SETQ LEFT-MARGIN (CADR OP)))
	(:RIGHT-MARGIN (SETQ RIGHT-MARGIN (CADR OP)))
	(:INTEGRAL-P (SETQ INTEGRAL-P (CADR OP)))
	(:MORE-P (SETQ MORE-P (CADR OP)))
	(:SCREEN (SETQ SCREEN (CADR OP)))
	(OTHERWISE (FERROR NIL "~S is not a recognized option" (CAR OP)))))
  ;; Open blinkers while changing pc ppr, if no output-hold.
  ;; This is necessary because the position might change or the blinker's
  ;; position might end up outside the pc ppr.
  ;; Don't use TV-PREPARE-PC-PPR because that waits if we have output-hold.
  (WITHOUT-INTERRUPTS
    (AND (ZEROP (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR))
	 (MAPC #'TV-OPEN-BLINKER (PC-PPR-BLINKER-LIST PC-PPR)))
    (SETQ OLD-LEFT (PC-PPR-LEFT-MARGIN PC-PPR)
	  OLD-TOP (PC-PPR-TOP-MARGIN PC-PPR))
    (TV-SET-PC-PPR-PARAMETERS PC-PPR
			      (OR FONT-MAP (PC-PPR-FONT-MAP PC-PPR))
			      TOP TOP-MARGIN BOTTOM BOTTOM-MARGIN
			      LEFT LEFT-MARGIN RIGHT RIGHT-MARGIN VSP INTEGRAL-P MORE-P)
    ;; Relocate the cursor, keeping the same position relative to the top-left corner
    (SETF (PC-PPR-CURRENT-X PC-PPR) (MIN (+ (PC-PPR-LEFT-MARGIN PC-PPR) OLD-X)
					 (PC-PPR-RIGHT-LIMIT PC-PPR)))
    (SETF (PC-PPR-CURRENT-Y PC-PPR) (MIN (+ (PC-PPR-TOP-MARGIN PC-PPR) OLD-Y)
					 (PC-PPR-BOTTOM-LIMIT PC-PPR)))
    ;; Relocate all non-cursor-following blinkers of this pc-ppr.
    ;; They remain fixed relative to the upper left corner.
    (DO L (PC-PPR-BLINKER-LIST PC-PPR) (CDR L) (NULL L)
      (COND ((TV-BLINKER-X-POS (CAR L))
	     ;; Relocate blinkers
	     (TV-SET-BLINKER-CURSORPOS (CAR L)
				       (- (TV-BLINKER-X-POS (CAR L)) OLD-LEFT)
				       (- (TV-BLINKER-Y-POS (CAR L)) OLD-TOP)))))
    (COND (SCREEN		;Switching screens.  I wonder if this code works.
	   (SETF (PC-PPR-SCREEN PC-PPR) SCREEN)
	   (DO L (PC-PPR-BLINKER-LIST PC-PPR) (CDR L) (NULL L)
	     (SETF (TV-BLINKER-SCREEN (CAR L)) SCREEN)))))
  PC-PPR)


(EVAL-WHEN (COMPILE)
	   (SPECIAL PC-PPR TAG INIT-X INIT-Y MAX-X MAX-Y)
)

(DEFUN TV-MAKE-LIMIT-STREAM (PC-PPR TAG)
  (LET ((INIT-X 0) (INIT-Y 0) (MAX-X 0) (MAX-Y 0))
     (CLOSURE '(PC-PPR TAG INIT-X INIT-Y MAX-X MAX-Y)
	      (FUNCTION TV-LIMIT-STREAM))))

(DEFUN TV-LIMIT-STREAM (OP &OPTIONAL ARG1 &REST REST &AUX X Y)
	   (SELECTQ OP
	     (TYO
	      (MULTIPLE-VALUE (X Y)
		  (TV-READ-CURSORPOS PC-PPR))
	      (COND ((OR (< Y INIT-Y) (> Y MAX-Y)
			 (AND (= Y MAX-Y) (> X MAX-X)))
		     (*THROW TAG NIL)))
	      (TV-TYO PC-PPR ARG1))
	     (INITIALIZE
	      (MULTIPLE-VALUE (INIT-X INIT-Y)
		  (TV-READ-CURSORPOS PC-PPR))
	      (SETQ MAX-X (+ INIT-X ARG1)
		    MAX-Y (+ INIT-Y (CAR REST))))
	     (WHICH-OPERATIONS
	      '(TYO INITIALIZE))
	     (T (MULTIPLE-VALUE-CALL (STREAM-DEFAULT-HANDLER 'TV-LIMIT-STREAM OP ARG1 REST)))))

(EVAL-WHEN (COMPILE)
	   (UNSPECIAL PC-PPR TAG INIT-X INIT-Y MAX-X MAX-Y))

;;; HAIRY WHO-LINE SYSTEM.

; TV-WHO-LINE-LIST is a list of fields, each field has the following
; format, the TV-WHO-LINE-ITEM defstruct:
;	(func state left right ...)
; func is a function to call, given the field as an argument.  If the state
; is NIL it always updates, otherwise it updates if the "state has changed".
; The ... is additional info depending on the func.  left and right are the
; portion of the who line occupied by this field.

(OR (BOUNDP 'TV-WHO-LINE-LIST)
    (SETQ TV-WHO-LINE-LIST
	  ;narrow screen
;	  '( ;0-108. SHOULD BE THE TIME  (18 chars)
;	    (TV-WHO-LINE-STRING NIL 108. 162. USER-ID)	;(9 chars)
;	    (TV-WHO-LINE-PACKAGE NIL 162. 300.)
;	    (TV-WHO-LINE-STRING NIL 300. 400. TV-WHO-LINE-RUN-STATE)
;	    (TV-WHO-LINE-FILE-STATE NIL 400. 576.))
	  ;wide screen, bigger font
	  '( ;0-144. SHOULD BE THE TIME  (18 chars)
	    (TV-WHO-LINE-STRING NIL 144. 216. USER-ID)	;(9 chars)
	    (TV-WHO-LINE-PACKAGE NIL 216. 360.)  ;(18 chars)
	    (TV-WHO-LINE-STRING NIL 360. 480. TV-WHO-LINE-RUN-STATE) ;(15 chars)
	    (TV-WHO-LINE-FILE-STATE NIL 480. 768.))))  ;(36 chars)

;THIS HAS TO BE A FUNCTION AND NOT A LOAD TIME EVAL BECAUSE WHEN THIS READS IN
; THERES NOT ENUF OF THE SYSTEM LOADED TO WIN (THE SCREEN DEFSTRUCT IS NOT IN, FOR EXAMPLE).
(DEFUN TV-SETUP-TV-WHO-LINE-PC-PPR NIL 
 (LET ((H (SCREEN-HEIGHT TV-DEFAULT-SCREEN)))
  (SETQ TV-WHO-LINE-PC-PPR 
	(TV-DEFINE-PC-PPR 'TV-WHO-LINE-PC-PPR NIL
			  'TOP (- H (FONT-CHAR-HEIGHT
				     (SCREEN-DEFAULT-FONT TV-DEFAULT-SCREEN)))
			  'BOTTOM H 'VSP 0
			  'BLINKER-P NIL 'MORE-P NIL))))

(OR (BOUNDP 'TV-WHO-LINE-PC-PPR)
    (TV-SETUP-TV-WHO-LINE-PC-PPR))

(OR (BOUNDP 'TV-WHO-LINE-STREAM)
    (SETQ TV-WHO-LINE-STREAM (TV-MAKE-STREAM TV-WHO-LINE-PC-PPR)))

;Update Functions.
(DEFUN TV-WHO-LINE-STRING (ITEM &AUX VAL)
  (COND ((NEQ (TV-WHO-LINE-ITEM-STATE ITEM)
	      (SETQ VAL (SYMEVAL (FIFTH ITEM))))
	 (TV-WHO-LINE-PREPARE-FIELD ITEM)
	 (TV-STRING-OUT TV-WHO-LINE-PC-PPR VAL
			0 (MIN (STRING-LENGTH VAL)
			       (// (- (TV-WHO-LINE-ITEM-RIGHT ITEM)
				      (TV-WHO-LINE-ITEM-LEFT ITEM))
				   (PC-PPR-CHAR-WIDTH TV-WHO-LINE-PC-PPR))))
	 (SETF (TV-WHO-LINE-ITEM-STATE ITEM) VAL))))

(DEFUN TV-WHO-LINE-PACKAGE (ITEM &AUX VAL)
  (LET ((PKG (COND ((OR (NULL TV-WHO-LINE-PROCESS)
			(NOT (FBOUNDP 'SYMEVAL-IN-STACK-GROUP)))  ;MAY NOT BE LOADED YET
		    PACKAGE)
		   ((EQ (PROCESS-STACK-GROUP TV-WHO-LINE-PROCESS)
			%CURRENT-STACK-GROUP)
		    PACKAGE)
		   (T (SYMEVAL-IN-STACK-GROUP 'PACKAGE
					      (PROCESS-STACK-GROUP TV-WHO-LINE-PROCESS))))))
    (COND ((AND (ARRAYP PKG)
		(NEQ (TV-WHO-LINE-ITEM-STATE ITEM) (SETQ VAL (PKG-NAME PKG))))
	   (TV-WHO-LINE-PREPARE-FIELD ITEM)
	   (TV-STRING-OUT TV-WHO-LINE-PC-PPR VAL
			  0 (MIN (STRING-LENGTH VAL)
				 (1- (// (- (TV-WHO-LINE-ITEM-RIGHT ITEM)
					    (TV-WHO-LINE-ITEM-LEFT ITEM))
					 (PC-PPR-CHAR-WIDTH TV-WHO-LINE-PC-PPR)))))
	   (TV-TYO TV-WHO-LINE-PC-PPR #/:)
	   (SETF (TV-WHO-LINE-ITEM-STATE ITEM) VAL)))))

(OR (FBOUNDP'TV-WHO-LINE-FILE-STATE)
    (FSET'TV-WHO-LINE-FILE-STATE 'CAR))	;This is a KLUDGE! Function is in NQFILE

;This function clears and positions, in preparation for update of a who-line field
(DEFUN TV-WHO-LINE-PREPARE-FIELD (ITEM)
  (WITHOUT-INTERRUPTS
    (TV-SET-CURSORPOS TV-WHO-LINE-PC-PPR (TV-WHO-LINE-ITEM-LEFT ITEM) 0)
    (TV-SELECT-SCREEN (PC-PPR-SCREEN TV-WHO-LINE-PC-PPR))
    (TV-ERASE (- (TV-WHO-LINE-ITEM-RIGHT ITEM) (TV-WHO-LINE-ITEM-LEFT ITEM))
	      (PC-PPR-LINE-HEIGHT TV-WHO-LINE-PC-PPR)
	      (TV-WHO-LINE-ITEM-LEFT ITEM)
	      (PC-PPR-TOP TV-WHO-LINE-PC-PPR)
	      (PC-PPR-ERASE-ALUF TV-WHO-LINE-PC-PPR))))

(DEFUN TV-WHO-LINE-RUN-STATE-UPDATE ()
    (SETQ TV-WHO-LINE-RUN-STATE
	  (COND ((NULL TV-WHO-LINE-PROCESS) "NIL")
		((ASSQ TV-WHO-LINE-PROCESS ACTIVE-PROCESSES)
		 (PROCESS-WHOSTATE TV-WHO-LINE-PROCESS))
		((NOT (NULL (PROCESS-ARREST-REASONS TV-WHO-LINE-PROCESS)))
		 "ARREST")
		(T "STOP")))
    (TV-WHO-LINE-UPDATE T))

;This function updates all fields of the who-line that need it.
(DEFUN TV-WHO-LINE-UPDATE (&OPTIONAL RUN-STATE-ONLY-P
			   &AUX RL (TV-ROVING-BLINKER-LIST NIL)) ;Who-line does not attempt
						;to synchronize to blinkers anyway, you
						;shouldn't put them down there.  This binding
						;gets rid of twinkling of the mouse blinker.
  (WITHOUT-INTERRUPTS
    (SETQ RL (%XBUS-READ TV-WHO-LINE-RUN-LIGHT-LOC))	;Don't clobber run light
    (DOLIST (ITEM TV-WHO-LINE-LIST)
      (AND (OR (NOT RUN-STATE-ONLY-P)
	       (EQ (FIFTH ITEM) 'TV-WHO-LINE-RUN-STATE))
	   (FUNCALL (CAR ITEM) ITEM)))
    (%XBUS-WRITE TV-WHO-LINE-RUN-LIGHT-LOC RL))
  T)

;Some people like to call this.  It's hard to make it work so I'm just going
;to make it clear the default screen.  It can be fixed later.  But we need
;a defined mechanism to find all the real screens.
(FSET' TV-CLEAR-ALL 'TV-CLEAR-SCREEN)

;This function is the named-structure handler for all TV defstructs.
;When the new class system is installed it can probably go away.
(DEFUN TV-NAMED-STRUCTURE-HANDLER (OP &OPTIONAL X &REST ARGS)
    (SELECTQ OP
	     (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
	     ((:PRINT :PRINT-SELF)
	      (FORMAT (CAR ARGS) "#<TV ~A ~O ~A>"
				 (TYPEP X)
				 (%POINTER X)
				 (COND ((EQ (TYPEP X) 'TV-BLINKER)
					(LET ((PC-PPR (TV-BLINKER-PC-PPR X)))
					  (COND ((NULL PC-PPR) "Roving")
						(T (PC-PPR-NAME PC-PPR)))))
				       ((ARRAY-HAS-LEADER-P X)
					(ARRAY-LEADER X 2))
				       (T (AR-1 X 1)))))
	     (OTHERWISE (ERROR OP "I have never heard of "))))

;Output a string without using a piece of paper.
;Specify the font and the position of the top left corner of the first character.
;Whether to output sideways (in which case the supplied co-ordinates are sideways)
;can also be spec'd, as can the starting and ending positions in the string.
(DEFUN TV-STRING-OUT-EXPLICIT (STRING XPOS YPOS
			        &OPTIONAL (START 0) END
					  (FONT FONTS:CPTFONT)
					  (ALU-FUNCTION TV-ALU-XOR)
					  (SCREEN TV-CPT-SCREEN)
					  SIDEWAYS-FLAG)
 (SETQ STRING (STRING STRING))
 (WITHOUT-INTERRUPTS
   (TV-SELECT-SCREEN SCREEN)
   (PROG ((I START)
	  (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	  CHAR TEM)
    (AND SIDEWAYS-FLAG (PSETQ XPOS YPOS YPOS XPOS))
    LOOP
    (AND (>= I N) (RETURN NIL))
    (SETQ CHAR (AR-1 STRING I))
    (COND ((< CHAR 200)					;PRINTING CHARACTER
;; Handles kerning wrong - the kern should not affect the ultimate cursor position.
	 (COND ((SETQ TEM (FONT-LEFT-KERN-TABLE FONT))
		(COND (SIDEWAYS-FLAG
		       (SETQ YPOS (MAX 0 (- YPOS (AR-1 TEM CHAR)))))
		      (T (SETQ XPOS (MAX 0 (- XPOS (AR-1 TEM CHAR))))))))
	 (COND ((NULL (SETQ TEM (FONT-INDEXING-TABLE FONT)))
		(TV-DRAW-CHAR FONT CHAR
			      (COND (SIDEWAYS-FLAG
				     (- (SCREEN-WIDTH SCREEN)
					XPOS
					(FONT-RASTER-WIDTH FONT)))
				    (T XPOS))
			      YPOS ALU-FUNCTION))
	       (T
		(DO ((CH (AR-1 TEM CHAR) (1+ CH))
		     (LIM (AR-1 TEM (1+ CHAR)))
		     (XPOS (COND (SIDEWAYS-FLAG (- (SCREEN-WIDTH SCREEN)
						   XPOS	;HAIRILY FIND TOP LEFT CORNER
						   (* (FONT-RASTER-WIDTH FONT)
						      (- (AR-1 TEM (1+ CHAR)) (AR-1 TEM CHAR)))))
				 (T XPOS))
			   (+ XPOS (FONT-RASTER-WIDTH FONT))))
		    ((= CH LIM))
		  (TV-DRAW-CHAR FONT CH XPOS YPOS ALU-FUNCTION))))
	 (SETQ TEM
	       (COND ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT))
		      (AR-1 TEM CHAR))
		     (T (FONT-CHAR-WIDTH FONT))))
	 (COND (SIDEWAYS-FLAG
		(SETQ YPOS (+ YPOS TEM)))
	       (T (SETQ XPOS (+ XPOS TEM))))))
    (SETQ I (1+ I))
    (GO LOOP))))


;TV-ERASE a region, but truncate it to what is inside some specified bounds.
;Optionally also leave untouched a fixed-width MARGIN inside the bounds.
(DEFUN TV-ERASE-TRUNCATED (BOUNDS XWIDTH YWIDTH XSTART YSTART ALU &OPTIONAL MARGIN)
    (LET ((X1 (FIRST BOUNDS))
	  (Y1 (SECOND BOUNDS))
	  (X2 (THIRD BOUNDS))
	  (Y2 (FOURTH BOUNDS)))
	 (AND MARGIN (SETQ X1 (+ X1 MARGIN) Y1 (+ Y1 MARGIN)
			   X2 (- X2 MARGIN) Y2 (- Y2 MARGIN)))
	 (SETQ X1 (MAX X1 XSTART) Y1 (MAX Y1 YSTART))
	 (SETQ X2 (MIN X2 (+ XWIDTH XSTART)))
	 (SETQ Y2 (MIN Y2 (+ YWIDTH YSTART)))
         (OR (< X2 X1) (< Y2 Y1)
             (TV-ERASE (- X2 X1) (- Y2 Y1) X1 Y1 ALU))))

