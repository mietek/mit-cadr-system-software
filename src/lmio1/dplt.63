;-*- MODE:LISP; PACKAGE:DPLT; BASE:8 -*-

;;; Send hardcopies of PLT files from SUDS to the Dover

;;; Not supported: rotated chars, PADs (other PC related stuff?)

;;; To print files use:  (PRINT-FILE file1 file2 keyword1 keyword2 ...)
;;; where file1, file2, etc are strings which are the names of files to be printed
;;; (second filename defaults to PLT) and keyword1, keyword2, etc are keywords
;;; indicated below.  Filenames and keywords may be interspersed in any order.
;;; The default action is that all the files specified are printed in a single
;;; Dover queue request by writing a file MC:.DOVR.;DPLT > (thus, the printing
;;; is indirect).  This can be overridden by specifying a keyword :FILE followed
;;; by a string giving and alternate filename or NIL, indicating that the files
;;; should be directly sent to the Dover over the ChaosNet (in this case, files
;;; are printed one at a time).  The keyword :SCALE can be used to scale the
;;; drawings (default is 1) and :BLANK-PAGE (with no argument) is used to
;;; supply an extra blank page at the end (used when the Dover leaves spots on
;;; the last page).  :COPIES can be used to get more than one copy.

;;; Todo: get additional rotated fonts, run scale test again
;;        author name

;;; User variables
(DEFVAR SCALE 1)				;The user can change this
(DEFVAR COPIES 1)				;number of copies
(DEFVAR SPOOL-FILENAME "MC:.DOVR.;DPLT >")	;default is to spool
(DEFVAR BLANK-PAGE NIL)				;in case the dover is printing bird shit

;;; Constants (should use # for these?)
(DEFVAR MICAS-PER-INCH 2540.)
(DEFVAR DOVER-PAGE-WIDTH (FIX (* 8.5 MICAS-PER-INCH)))	;8.5 inch page
(DEFVAR DPLT-TO-MICAS-SCALE-FACTOR 3)	;2540. micas/in / 1600. pts/in
(DEFVAR TEXT-X-OFFSET -35.)			;random bums
(DEFVAR TEXT-Y-OFFSET 18.)

(DEFSTRUCT (DPLT-FONT :LIST (:CONSTRUCTOR NIL))			;simple lists
	   FNAME FONT-TYPEFACE FONT-SIZE FONT-HEIGHT FONT-WIDTH FONT-INDEX)

;;; Definitely could use more SAIL fonts (e.g. sizes 5,7 9,10,12,14,18,20)

(DEFVAR TEXT-FONTS '(("SAIL" "" 6. NIL NIL 0)
		     ("SAIL" "" 8. NIL NIL 0)
		     ("Gacha" "" 10. NIL NIL 0)
		     ("Gacha" "" 12. NIL NIL 0)
		     ("Helvetica" "" 18. NIL NIL 0)))

(DEFVAR SYMBOL-FONTS '(("SAIL" "" 6. NIL NIL 0)
		       ("SAIL" "" 8. NIL NIL 0)))

(DEFVAR DIAMOND-FONT '("Math" "" 10. NIL NIL 0))
(DEFVAR TITLE-FONT '("Helvetica" "B" 12. NIL NIL 0))

;;; Define the box frame for the drawing and text such as titles and filename.
(DEFSTRUCT (DPLT-FRAME :LIST (:CONSTRUCTOR NIL))	;simple list
	   FLEFT FRIGHT FTOP FBOTTOM LABEL-TOP DATE-LEFT FILE-LEFT
	   TITLE1 TITLE2 DATE FILE BASELINE)	;these are all locations

(DEFVAR FRAME (MAPCAR #'(LAMBDA(X) (FIX (* MICAS-PER-INCH X)))
		      '(0 10.2 8.2 .4 .7 6.0 8.05
			.5 3.0 6.14 8.2 .48)))

;; Load the LOOP macro
(EVAL-WHEN (:COMPILE :EVAL)
     (OR (FBOUNDP 'LOOP) (LOAD "AI:LIBLSP;LOOP")))

(DEFUN PRINT-FILE (&REST FILE-LIST
		   &AUX (SCALE SCALE) (SPOOL-FILENAME SPOOL-FILENAME) (BLANK-PAGE BLANK-PAGE)
		        (COPIES COPIES))
  (LOOP WITH NEW-FILE-LIST
	FOR L ON FILE-LIST
	DO (IF (NOT (SYMBOLP (CAR L)))
	       (SETQ NEW-FILE-LIST (PUSH (CAR L) NEW-FILE-LIST))
	       (SELECTQ (CAR L)
		   (:SCALE (SETQ SCALE (CADR L)))
		   (:COPIES (SETQ COPIES (CADR L)))
		   (:FILE  (SETQ SPOOL-FILENAME
				 (AND (CADR L)
				      (SI:FILE-MERGE-PATHNAMES (CADR L) SPOOL-FILENAME))))
		   (:BLANK-PAGE (SETQ BLANK-PAGE T))
		   (T (FERROR NIL "~%~A Unknown keyword: DPLT:PRINT-FILE" (CAR L))))
	       (SETQ L (REST1 L)))
	FINALLY (SETQ FILE-LIST (NREVERSE NEW-FILE-LIST)))
  (IF (NULL SPOOL-FILENAME) (PRESS:PRINT-DOVER-STATUS))	;Let the user know the possibilities..
  (LET ((PRESS:DOVER-X0 0.) (PRESS:DOVER-Y2 1100.)	;Y0 = .4 real margin x 2540.
	(PRESS:LINE-WIDTH 20.)
	(PRESS:DIAGONAL-LINE-WIDTH 12.))
    (IF SPOOL-FILENAME (START-OUTPUT))
    (DOLIST (FILE FILE-LIST)
      (IF (NULL (PROBEF FILE)) (FORMAT T "~% File ~S not found, continuing..." FILE)
	  (IF (NULL SPOOL-FILENAME) (START-OUTPUT))
	  (PRESS:PRESS-START-PAGE)
	  (OUTPUT-FILE FILE)
	  (PRESS:PRESS-END-PAGE)
	  (IF (NULL SPOOL-FILENAME) (END-OUTPUT))))
    (IF SPOOL-FILENAME (END-OUTPUT)))
  "Plot files printed")

(GLOBALIZE 'DPLT-PRINT-FILE)
(FSET 'DPLT-PRINT-FILE #'PRINT-FILE)

(DEFUN START-OUTPUT ()
  (PRESS:PRESS-START-FILE (OR SPOOL-FILENAME PRESS:DOVER-ADDRESS))
  ;; define fonts
  (DOLIST (FONT-LIST `(,TEXT-FONTS ,SYMBOL-FONTS (,TITLE-FONT)))
    (DOLIST (F FONT-LIST)
      (SETF (FONT-INDEX F)
	    (PRESS:PRESS-DEFINE-FONT (FNAME F) (FONT-TYPEFACE F)
				     (FONT-SIZE F) 5400.))))	;rotated 90 deg.
  ;; special kludge for diamonds
  (SETF (FONT-INDEX DIAMOND-FONT)
	(PRESS:PRESS-DEFINE-FONT (FNAME DIAMOND-FONT) (FONT-TYPEFACE DIAMOND-FONT)
				 (FONT-SIZE DIAMOND-FONT) 0.)))

(DEFUN END-OUTPUT ()
  (IF BLANK-PAGE
      (PROGN (PRESS:PRESS-START-PAGE)			;blank page for bird shits
	     (PRESS:PRESS-END-PAGE)))
  (PRESS:PRESS-END-FILE "SUDS Plots" (TIME:WHAT-TIME NIL) COPIES))

;;; PLT files are stored in a 36-bit format, so do the best we can...
;;; Read in 18. bit bytes (so they remain as small integers)

(DEFMACRO IN18 (STREAM)				;18. bit input, 9 bit bytes
  `(+ (ASH (FUNCALL ,STREAM ':TYI) 9.) (FUNCALL ,STREAM ':TYI)))

;;; Occassionally need 36. bits at a time here

(DEFMACRO IN36 (STREAM)
  `(+ (ASH (IN18 ,STREAM) 18.) (IN18 ,STREAM)))

(DEFVAR STREAM)
(DEFVAR PASS)					;Indicates sizing pass or printing pass
(DEFVAR X-OFFSET) (DEFVAR Y-OFFSET)		;initialized during prescan
(DEFVAR MIN-X) (DEFVAR MIN-Y) (DEFVAR MAX-X) (DEFVAR MAX-Y)

;;; Process a DPLT file.  Since plots are not centered, must prescan to find
;;; the range of the plot to compute its offsets.

(DEFUN OUTPUT-FILE (FILE)
  (UNWIND-PROTECT 
    (PROGN
      (SETQ STREAM (OPEN (FUNCALL (FS:FILE-PARSE-NAME FILE)
				  ':COPY-WITH-TYPE
				  "PLT")
			 '(:IN :FIXNUM :BYTE-SIZE 9.))
	    X-OFFSET 0 Y-OFFSET 0			;no offsets initially
	    MIN-X  #.(ASH 1 16.)			;init to largest and smallest possible
	    MIN-Y  #.(ASH 1 16.)
	    MAX-X  #.(- (ASH 1 16.))
	    MAX-Y  #.(- (ASH 1 16.)))
      (PROCESS-FILE 'PASS1)				;prescan to find max, min
      (SETQ X-OFFSET (- (// (+ (FLEFT FRAME) (FRIGHT FRAME)) 2)
			(// (+ MAX-X MIN-X) 2))
	    Y-OFFSET (- (// (+ (LABEL-TOP FRAME) (FTOP FRAME)) 2)
			(// (+ MAX-Y MIN-Y) 2)))
      (FUNCALL STREAM ':SET-POINTER 0)			;backup for reread
      (PROCESS-FILE 'PASS2)
      (DRAW-FRAME-AND-TITLE))
    (CLOSE STREAM)))

(DEFUN PROCESS-FILE (PASS &AUX X Y TYPE LH RH)
  (LOOP INITIALLY (IN36 STREAM)			;skip first word (SUDS version number)
	          (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ))	;first item
	UNTIL (EQ 'END TYPE)
	DO (SELECTQ TYPE			;each DRAW function returns next values
	     (VECTOR  (MULTIPLE-VALUE (X Y TYPE LH RH) (DRAW-VECTORS X Y)))
	     (TEXT    (MULTIPLE-VALUE (X Y TYPE LH RH) (DRAW-TEXT X Y RH)))	; RH has size
	     (DIAMOND (MULTIPLE-VALUE (X Y TYPE LH RH) (DRAW-DIAMOND X Y)))
	     (OTHERWISE (FERROR NIL "~% DPLT Lossage, shouldn't get here")))))

;;; Read a 36. bit word (IN18 2 halves) and decode the item type.
;;; Read and translate coordinates.

(DEFUNP DPLT-READ (&AUX (LH (IN18 STREAM)) (RH (IN18 STREAM))
			(X (+ (DPLT-TO-MICAS LH) X-OFFSET))
			(Y (+ (DPLT-TO-MICAS RH) Y-OFFSET)))
  (RETURN X Y (COND ((= 400001 LH) 'END)	;end of display items
		    ((BIT-TEST 1 RH) 'CONTINUE)	;proceed in the current mode
		    ((NOT (BIT-TEST 1 LH)) 'VECTOR)
		    (T (SETQ LH (IN18 STREAM) RH (IN18 STREAM))	;read both halves
		       (SELECTQ LH		;decode off LH
			 (0 'TEXT)
			 (2 'DIAMOND)		;dont hack pads...
			 (OTHERWISE (FERROR NIL "~% Invalid data in file.")))))
	  LH RH))				;Return this for some commands to reparse

;;; Convert 17. bit numbers which are left adjusted in 18. bits and
;;; are in 2's complement form, rather than LISPM number form.

(DEFUN DPLT-TO-MICAS (RAW)
  (IF (BIT-TEST #.(ASH 1 17.) RAW) (SETQ RAW (- (1+ (LOGXOR 777777 RAW)))))	;convert sign
  (FIX (* DPLT-TO-MICAS-SCALE-FACTOR (ASH RAW -1))))

;;; Sort of a crock - rotate the page and do the user's scaling

(DEFMACRO DPLT-TO-DOVER (X Y)
  `(LET ((SAVE-X ,X))
     (SETQ ,X (* SCALE (- DOVER-PAGE-WIDTH ,Y))
	   ,Y (* SCALE SAVE-X))))

;;; Draw a line, converting to rotated Dover coords

(DEFUN LINE (X1 Y1 X2 Y2)
  (DPLT-TO-DOVER X1 Y1)
  (DPLT-TO-DOVER X2 Y2)
  (PRESS:PRESS-LINE X1 Y1 X2 Y2))

;;; Update the range of the plot (used during PASS1 scan of the file)

(DEFUN MIN-MAX-TEST (X Y)
  (IF (< X MIN-X) (SETQ MIN-X X))
  (IF (> X MAX-X) (SETQ MAX-X X))
  (IF (< Y MIN-Y) (SETQ MIN-Y Y))
  (IF (> Y MAX-Y) (SETQ MAX-Y Y)))

;;; Select a font from a font list, given a DPLT character size

(DEFUN SELECT-FONT (CSIZE FONT-LIST)
  (LOOP WITH LAST-F = (FIRST FONT-LIST)
	FOR F IN FONT-LIST
	DO (IF (< CSIZE (* .6 (// (* 1600. (FONT-SIZE F)) 72.)))  (RETURN LAST-F))
	(SETQ LAST-F F)
	FINALLY (RETURN (CAR (LAST FONT-LIST)))))

;;; DRAWING routines for various plot items

;;; Vector Mode (loop until mode changes)

(DEFUNP DRAW-VECTORS (X Y &AUX TYPE LH RH)
  (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y))
  (LOOP FOR LAST-X = X THEN X AND LAST-Y = Y THEN Y
	DO  (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ))
	WHILE (MEMQ TYPE '(CONTINUE VECTOR))
	DOING (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y)
		  (IF (NOT (EQ TYPE 'VECTOR))	;if VECTOR, just a set point
		      (LINE LAST-X LAST-Y X Y))))
  (RETURN X Y TYPE LH RH))

(DEFUNP DRAW-TEXT (X Y RH &AUX LH TYPE TEXT-FONT SYMBOL-FONT  ;; (ROT (LDB 2101 RH))
		   FONT-WIDTH FONT-HEIGHT (X0 X) CSIZE CURRENT-FONT)
  (SETQ CSIZE (LDB 0120 RH)
	TEXT-FONT (SELECT-FONT CSIZE TEXT-FONTS)
	SYMBOL-FONT (SELECT-FONT CSIZE SYMBOL-FONTS))
  (DOLIST (FONT `(,TEXT-FONT ,SYMBOL-FONT))
    (SETF (FONT-WIDTH FONT)			;font width expected by SUDS
	  (// (* CSIZE MICAS-PER-INCH) 1600.))
    (IF (NULL (FONT-HEIGHT FONT))
	(SETF (FONT-HEIGHT FONT)
	      (CADR (PRESS:GET-FONT-WIDTH-AND-HEIGHT
		      (FNAME FONT) (FONT-TYPEFACE FONT) (FONT-SIZE FONT))))))
  (SETQ FONT-HEIGHT (FONT-HEIGHT TEXT-FONT) FONT-WIDTH (FONT-WIDTH TEXT-FONT)
	CURRENT-FONT TEXT-FONT)
  (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y)
      (PRESS:PRESS-SELECT-FONT (FONT-INDEX CURRENT-FONT)))
  (LOOP WITH X1 = (+ X TEXT-X-OFFSET) AND Y1 = (+ Y TEXT-Y-OFFSET)
	DO (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ))
        WHILE (EQ TYPE 'CONTINUE)
	DOING
	  (LOOP WITH WORD = (+ (ASH LH 18.) RH)
		FOR BP FROM 3507 DOWNTO 0107 BY 0700
	        FOR CH = (LDB BP WORD) THEN (LDB BP WORD)
		UNLESS (ZEROP CH)
		DO
		 (IF (= CH #\CR) (PROGN (SETQ X1 X0 Y1 (- Y1 FONT-HEIGHT))
					(MIN-MAX-TEST X1 Y1))
		     (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X1 (+ Y1 FONT-HEIGHT))
			 (LET ((X X1)(Y Y1)
			       (FONT (IF (> CH 37) TEXT-FONT SYMBOL-FONT)))
			   (AND (NOT (EQ FONT CURRENT-FONT))
				(SETQ CURRENT-FONT FONT)
				(PRESS:PRESS-SELECT-FONT (FONT-INDEX CURRENT-FONT)))
			   (DPLT-TO-DOVER X Y)
			   (PRESS:PRESS-SET-CURSOR X Y)
			   (PRESS:PRESS-CHAR CH)))
		     (SETQ X1 (+ X1 FONT-WIDTH)))))
  (RETURN X Y TYPE LH RH))

(DEFUNP DRAW-DIAMOND (X Y &AUX TYPE LH RH)
  (IF (EQ PASS 'PASS1) (MIN-MAX-TEST X Y)
      (DPLT-TO-DOVER X Y)			;otherwise, on PASS2 print a diamond
      (PRESS:PRESS-SELECT-FONT (FONT-INDEX DIAMOND-FONT))
      (PRESS:PRESS-SET-CURSOR (- X 70.) (- Y 130.))
      (PRESS:PRESS-CHAR 017))
  (LOOP DO  (MULTIPLE-VALUE (X Y TYPE LH RH) (DPLT-READ))	;shouldn't be needed...
	WHILE (EQ 'CONTINUE TYPE))
  (RETURN X Y TYPE LH RH))

;;; Draw the frame and plot titles

(DEFUN DRAW-FRAME-AND-TITLE (&AUX
     (AUTHOR-LIST (READ-ASCIZ-STRING))
     (TITLE1      (READ-ASCIZ-STRING))
     (TITLE2      (READ-ASCIZ-STRING))
     (BINARY-DATE (IN18 STREAM))
     (BINARY-TIME (IN18 STREAM))
     (SIXBIT-FN1  (IN36 STREAM))
     (SIXBIT-FN2  (IN36 STREAM))
     (SIXBIT-DIR  (IN36 STREAM))
     (DATE-TIME   (FORMAT NIL "~D-~[JAN~;FEB~;MAR~;APR~;MAY~;JUN~;JUL~;AUG~;SEP~;OCT~;NOV~;DEC~]-~D  ~2,'0D:~2,'0D"
			  (1+ (\ BINARY-DATE 31.))
			  (\ (// BINARY-DATE 31.) 12.)
			  (+ 1964. (// BINARY-DATE (* 12. 31.)))
			  (// BINARY-TIME 60.)
			  (\ BINARY-TIME 60.)))
     (FILE-NAME (FUNCALL (FUNCALL STREAM ':FILENAME) ':STRING-FOR-PRINTING))
     (LEFT   (FLEFT FRAME))
     (RIGHT  (FRIGHT FRAME))
     (TOP    (FTOP FRAME))
     (BOTTOM (FBOTTOM FRAME))
     (LTOP   (LABEL-TOP FRAME))
     (DATEL  (DATE-LEFT FRAME))
     (FILEL  (FILE-LEFT FRAME))
     (BASEL  (BASELINE FRAME))
     (PRESS:LINE-WIDTH (* 2 PRESS:LINE-WIDTH)))	;doubly thick lines here
  (LINE LEFT  BOTTOM LEFT  TOP)
  (LINE LEFT  TOP    RIGHT TOP)
  (LINE RIGHT TOP    RIGHT BOTTOM)
  (LINE RIGHT BOTTOM LEFT  BOTTOM)
  (LINE LEFT  LTOP   RIGHT  LTOP)
  (LINE DATEL LTOP   DATEL BOTTOM)
  (LINE FILEL LTOP   FILEL BOTTOM)
  (PRESS:PRESS-SELECT-FONT (FONT-INDEX TITLE-FONT))
  (SETQ AUTHOR-LIST AUTHOR-LIST)		;maybe print this someday
  (PRINT-STRING TITLE1 (TITLE1 FRAME) BASEL)
  (PRINT-STRING TITLE2 (TITLE2 FRAME) BASEL)
  (PRINT-STRING DATE-TIME (DATE FRAME) BASEL)
  SIXBIT-FN2 ;not used, avoid compiler warning
  (PRINT-STRING FILE-NAME (+ (FILE FRAME) (* (- 17. (STRING-LENGTH FILE-NAME)) 180.)) BASEL))

(DEFUN READ-ASCIZ-STRING ()
  (APPLY ':STRING-APPEND
	 (LOOP FOR WORD = (IN36 STREAM) THEN (IN36 STREAM)
	       NCONCING (LOOP FOR BP FROM 3507 DOWNTO 0107 BY 0700
			      FOR CH = (LDB BP WORD) THEN (LDB BP WORD)
			      UNLESS (ZEROP CH)
			      COLLECTING CH)
	       UNTIL (ZEROP (LDB 0107 WORD)))))

(DEFUN PRINT-STRING (STRING X Y)
  (DPLT-TO-DOVER X Y)
  (PRESS:PRESS-SET-CURSOR X Y)
  (PRESS:PRESS-STRING STRING))

(DEFUN SIXBIT-TO-STRING (NUM)
  (DO ((PPSS 3606 (- PPSS 0600))
       (L NIL))
      ((MINUSP PPSS)
       (DO () ((OR (NULL L) (NOT (= (CAR L) 40))))
	 (SETQ L (CDR L)))
       (APPLY #'STRING-APPEND (NREVERSE L)))
   (PUSH (+ (LDB PPSS NUM) 40) L)))
