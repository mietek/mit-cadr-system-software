;;;-*-Mode:LISP; Package:FED-*-

;The functions in this file
;are used to convert between the various formats for fonts as used on the LISP
;Machine.  There are currently three formats supported:
;	KST format is used for communication with the PDP-10.
;	FD (or Font Descriptor) Format is used as a machine resident format
;		which is easily manipulated.  The format consists of a 200
;		element array with a leader.  The elements of this array are
;		themselves two dimensional arrays which contain the actual
;		pixel values for the character.
;	FONT (or internal) Format is the format actually used by the tv display
;		routines.  The format is fairly complicated and its direct
;		use is not recommended when a conversion to FD format would
;		be better.
;	AL format is used for ALTO fonts.

;First some helping functions:

;Maximum raster width of an FD format font
(DEFUN MAX-RASTER-WIDTH (FONT-DESCRIPTOR &AUX (GUESS 0) TEMP)
       (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
	   (( CHAR-CODE 200) GUESS)
	   (COND ((SETQ TEMP (AR-1 FONT-DESCRIPTOR CHAR-CODE))
		  (SETQ GUESS (MAX GUESS (ARRAY-DIMENSION-N 2 TEMP)))))))

;Maximum raster height of an FD format font
(DEFUN MAX-RASTER-HEIGHT (FONT-DESCRIPTOR &AUX (GUESS 0) TEMP)
       (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
	   (( CHAR-CODE 200) GUESS)
	   (COND ((SETQ TEMP (AR-1 FONT-DESCRIPTOR CHAR-CODE))
		  (SETQ GUESS (MAX GUESS (ARRAY-DIMENSION-N 1 TEMP)))))))

;;; Memoizing version of FONT-INTO-FONT-DESCRIPTOR
;;; that wants a font name (symbol in FONTS:) rather than the font itself.
;;; The FONT-DESCRIPTOR property of the symbol holds the descriptor.
;;; The FONT-DESCRIBED property holds the font itself which the descriptor matches.
;;; If anyone changes the font, we can see that the old descriptor is no good.
(defun font-name-font-descriptor (fontname &aux fd)
    (setq fd (get fontname 'font-descriptor))
    (cond ((and fd (eq (get fontname 'font-described) (symeval fontname))))
	  (t (setq fd (font-into-font-descriptor (symeval fontname)))
	     (putprop fontname (symeval fontname) 'font-described)
	     (putprop fontname fd 'font-descriptor)))
    fd)

;;; Set a font given a font descriptor.  Keep the descriptor around.
;;; Forward the old definition of the font to the new one.
(defun font-name-set-font-and-descriptor (fontname font-descriptor)
    (let ((oldfont (and (boundp fontname) (symeval fontname))))
      (set fontname (font-descriptor-into-font font-descriptor))
      (and oldfont (structure-forward oldfont (symeval fontname)))
      (putprop fontname font-descriptor 'font-descriptor)
      (putprop fontname (symeval fontname) 'font-described)
      font-descriptor))

;Store a character in a font.  Given a font and corresponding FD, both are
;updated by storing a given CD for a given character code.
;If the CD can be stored into the existing font, that is done.
;Otherwise, a new font is made from the updated FD and the old font forwarded to it.
(defun font-name-store-cd (fontname cd char-code &aux font)
    (let ((width (array-dimension-n 2 cd))
          (height (array-dimension-n 1 cd))
          tem fd)
      (setq fd (font-name-font-descriptor fontname))
      (as-1 cd fd char-code)
      (and (= char-code #/ )
           (setf (fd-space-width fd) (cd-char-width cd)))
      (cond ((or (not (boundp fontname))
                 (null (setq font (symeval fontname)))
                 (> width
                    (cond ((setq tem (font-indexing-table font))
                           (* (font-raster-width font)
                              (- (ar-1 tem (1+ char-code))
                                 (ar-1 tem char-code))))
                          (t (font-raster-width font))))
                 (> height (font-raster-height font)))
             (font-name-set-font-and-descriptor fontname fd))
            (t (store-cd-in-font cd font char-code nil)))))

;Functions for referring to specified pixels of characters in an internal format font.

;ROW and COL are measured from top/left as usual.  An alternative would be:
;	COL is measured from the left, with Kerning hacked.
;	ROW is positive above the baseline and negative below.
;  (SETQ ROW (- (FONT-BASELINE FONT) ROW))
;  (AND (SETQ TEM (FONT-LEFT-KERN-TABLE FONT))
;       (SETQ COL (+ COL (AR-1 TEM CHAR))))
;However it looks like this would cause more trouble than it would save.
;Attempts to reference outside of the raster return 0, or barf if storing.
;Conceivably it might be good to not barf at attempts to store 0 out of bounds?

(DEFUN FONT-GET-PIXEL (FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  (COND ((OR (< ROW 0)
	     (>= ROW (FONT-RASTER-HEIGHT FONT))
	     (< COL 0)
	     (COND ((SETQ TEM (FONT-INDEXING-TABLE FONT))
		    (SETQ CHAR (+ (AR-1 TEM CHAR) (// COL (FONT-RASTER-WIDTH FONT))))
		    (SETQ COL (\ COL (FONT-RASTER-WIDTH FONT)))
		    (>= CHAR (AR-1 TEM NEXTCHAR)))
		   ((>= COL (FONT-RASTER-WIDTH FONT)))))
	 0)  ;out of bounds, return 0
	(T
	 (DO ((FONT FONT (FONT-NEXT-PLANE FONT))
	      (PIXEL 0)
	      (PLANENUM 0 (1+ PLANENUM)))
	     ((NULL FONT) PIXEL)
	   (SETQ PIXEL
		 (+ PIXEL (LSH (AR-1 FONT
				     (+ (* 32. (+ (* (FONT-WORDS-PER-CHAR FONT) CHAR)
						  (// ROW (FONT-RASTERS-PER-WORD FONT))))
                                        (+ (* (FONT-RASTER-WIDTH FONT)
                                              (\ ROW (FONT-RASTERS-PER-WORD FONT)))
                                           COL)))
			       PLANENUM)))))))

(DEFUN FONT-SET-PIXEL (PIXEL FONT CHAR ROW COL &AUX TEM (NEXTCHAR (1+ CHAR)))
  (COND ((OR (< ROW 0)
	     (>= ROW (FONT-RASTER-HEIGHT FONT))
	     (< COL 0)
	     (COND ((SETQ TEM (FONT-INDEXING-TABLE FONT))
		    (SETQ CHAR (+ (AR-1 TEM CHAR) (// COL (FONT-RASTER-WIDTH FONT))))
		    (SETQ COL (\ COL (FONT-RASTER-WIDTH FONT)))
		    (>= CHAR (AR-1 TEM NEXTCHAR)))
		   ((>= COL (FONT-RASTER-WIDTH FONT)))))
	 (FERROR NIL "Store of ~C in ~S at ~O,~O out of character bounds" CHAR FONT ROW COL))
	(T
	 (DO ((FONT FONT (FONT-NEXT-PLANE FONT))
	      (BIT PIXEL (LSH BIT -1)))
	     ((NULL FONT) PIXEL)
	     (AS-1 BIT FONT
		   (+ (* 32. (+ (* (FONT-WORDS-PER-CHAR FONT) CHAR)
				(// ROW (FONT-RASTERS-PER-WORD FONT))))
                      (+ (* (FONT-RASTER-WIDTH FONT)
                            (\ ROW (FONT-RASTERS-PER-WORD FONT)))
                         COL)))))))

;This function takes an FD format font and creates an internal format
;	font from it.  All of the hairy formats of the stored font
;	are taken care of by this function so the user doesn't have
;	to worry about them.

(DEFUN FONT-DESCRIPTOR-INTO-FONT (FONT-DESCRIPTOR
	   &OPTIONAL (NBR-PLANES-OUT NIL)
	   &AUX (FONT-OUT NIL)
		(COL-INCR (COND ((FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR) 2)
				(T 1)))
		(SPACE-WIDTH (OR (FD-SPACE-WIDTH FONT-DESCRIPTOR) 0))
		(WIDTH (// SPACE-WIDTH COL-INCR))
		(HEIGHT (FD-LINE-SPACING FONT-DESCRIPTOR))
		(BASELINE (FD-BASELINE FONT-DESCRIPTOR))
		(RASTER-WIDTH (// (+ (MAX-RASTER-WIDTH FONT-DESCRIPTOR)
				     (1- COL-INCR))
				  COL-INCR))
		(RASTER-HEIGHT (MAX-RASTER-HEIGHT FONT-DESCRIPTOR))
		(RASTERS-PER-WORD (// 32. (MIN 32. RASTER-WIDTH)))
		(WORDS-PER-RASTER-ELEMENT (1+ (// (1- RASTER-HEIGHT) RASTERS-PER-WORD)))
		(TOTAL-RASTER-ELEMENTS 200)
		(BLINKER-WIDTH (// (FD-BLINKER-WIDTH FONT-DESCRIPTOR) COL-INCR))
		(BLINKER-HEIGHT (FD-BLINKER-HEIGHT FONT-DESCRIPTOR))
		(INDEXING-TABLE NIL)
		(CHARS-EXIST-TABLE (MAKE-ARRAY NIL ART-1B 200))
		TEMP					;General temporary
		)

;Set up NBR-PLANES-OUT if defaulted
       (COND ((NULL NBR-PLANES-OUT)
	      (SETQ NBR-PLANES-OUT COL-INCR)))

;Create INDEXING-TABLE if needed
       (COND ((> RASTER-WIDTH 32.)
	        (SETQ INDEXING-TABLE (MAKE-ARRAY NIL 'ART-16B 201))
		(AS-1 0 INDEXING-TABLE 0)
		(DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
		    (( CHAR-CODE 200) (SETQ TOTAL-RASTER-ELEMENTS (AR-1 INDEXING-TABLE 200)))
		    (SETQ TEMP (AR-1 FONT-DESCRIPTOR CHAR-CODE))
		    (AS-1 (+ (AR-1 INDEXING-TABLE CHAR-CODE)
                             (COND ((NULL TEMP) 0)
                                   (T (// (+ (ARRAY-DIMENSION-N 2 TEMP) 31.) 32.))))
			  INDEXING-TABLE (1+ CHAR-CODE)))
                (SETQ RASTER-WIDTH 32.)))

;set up all the planes of the font
       (DO ((I NBR-PLANES-OUT (1- I)))
	   ((ZEROP I))

;Make up a (one-plane) font and make it's next plane be the last one we made
	   (SETQ TEMP (TV:MAKE-FONT MAKE-ARRAY (NIL 'ART-1B
                                                    (* TOTAL-RASTER-ELEMENTS
                                                       WORDS-PER-RASTER-ELEMENT 32.))))
	   (SETF (FONT-NEXT-PLANE TEMP) FONT-OUT)
	   (SETQ FONT-OUT TEMP)

;Now set all the other fields in the leader
           (SETF (FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
	   (SETF (FONT-CHAR-WIDTH FONT-OUT) WIDTH)
	   (SETF (FONT-CHAR-HEIGHT FONT-OUT) HEIGHT)
	   (SETF (FONT-RASTER-WIDTH FONT-OUT) RASTER-WIDTH)
	   (SETF (FONT-RASTER-HEIGHT FONT-OUT) RASTER-HEIGHT)
	   (SETF (FONT-RASTERS-PER-WORD FONT-OUT) RASTERS-PER-WORD)
	   (SETF (FONT-WORDS-PER-CHAR FONT-OUT) WORDS-PER-RASTER-ELEMENT)
	   (SETF (FONT-BASELINE FONT-OUT) BASELINE)
	   (SETF (FONT-BLINKER-WIDTH FONT-OUT) BLINKER-WIDTH)
	   (SETF (FONT-BLINKER-HEIGHT FONT-OUT) BLINKER-HEIGHT)
	   (SETF (FONT-NAME FONT-OUT) (FD-NAME FONT-DESCRIPTOR))
	   (SETF (FONT-CHARS-EXIST-TABLE FONT-OUT) CHARS-EXIST-TABLE)
	   (SETF (FONT-INDEXING-TABLE FONT-OUT) INDEXING-TABLE))
       (DO ((CHAR-CODE 0 (1+ CHAR-CODE))) (( CHAR-CODE 200))
	   (SETQ TEMP (AR-1 FONT-DESCRIPTOR CHAR-CODE))
	   (COND (TEMP
		  (STORE-CD-IN-FONT TEMP FONT-OUT CHAR-CODE
				    (FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR)))))
       FONT-OUT)

;Store the data in CD into character number CHAR-CODE of FONT.
;It is assumed that the dimensions of the CD fit within the raster dimensions of the font.
;This is not recommended for users to call.
(DEFUN STORE-CD-IN-FONT (CD FONT CHAR-CODE &OPTIONAL (DOUBLE-WIDTH-P NIL) &AUX
                            (WIDTH (ARRAY-DIMENSION-N 2 CD))
                            (HEIGHT (ARRAY-DIMENSION-N 1 CD))
			    (FONT-HEIGHT (FONT-RASTER-HEIGHT FONT))
			    (FONT-WIDTH (FONT-RASTER-WIDTH FONT))
                            PIXEL
                            (COL-INCR (COND (DOUBLE-WIDTH-P 2) (T 1))))
    ;; Update the font's char-width-table, creating one if necessary.
    (LET ((CW (// (+ (CD-CHAR-WIDTH CD)
		     (1- COL-INCR))
		  COL-INCR))
	  (FCW (FONT-CHAR-WIDTH FONT))
	  (FCWT (FONT-CHAR-WIDTH-TABLE FONT)))
	(COND (FCWT
	       (AS-1 CW FCWT CHAR-CODE))
	      ((NOT (= CW FCW))
	       (SETF (FONT-CHAR-WIDTH-TABLE FONT)
		     (SETQ FCWT (MAKE-ARRAY NIL ART-8B '(200))))
               (AND DOUBLE-WIDTH-P
                    (SETF (FONT-CHAR-WIDTH-TABLE (FONT-NEXT-PLANE FONT))
                          FCWT))
	       (DO I 0 (1+ I) (= I 200)
		  (AS-1 FCW FCWT I))
	       (AS-1 CW FCWT CHAR-CODE)))
	(AND (= CHAR-CODE #/ )
	     (SETF (FONT-CHAR-WIDTH FONT) CW)))
    ;; Update the font's left-kern table, creating one if necessary.
    (LET ((CK (CD-CHAR-LEFT-KERN CD))
	  (FCKT (FONT-LEFT-KERN-TABLE FONT)))
	(COND (FCKT (AS-1 CK FCKT CHAR-CODE))
	      ((NOT (ZEROP CK))
	       (SETF (FONT-LEFT-KERN-TABLE FONT)	;MUST BE ART-32B BECAUSE LEFT-KERN
		     (SETQ FCKT (MAKE-ARRAY NIL ART-32B '(200))))  ;CAN BE NEGATIVE
               (AND DOUBLE-WIDTH-P
                    (SETF (FONT-LEFT-KERN-TABLE (FONT-NEXT-PLANE FONT))
                          FCKT))
	       (AS-1 CK FCKT CHAR-CODE))))
    ;; Tell the font this char exists.
    (ERRSET (AS-1 1 (FONT-CHARS-EXIST-TABLE FONT) CHAR-CODE) NIL)
    ;; In wide fonts, the raster width depends on the character, and is a multiple of 32.
    (COND ((FONT-INDEXING-TABLE FONT)
           (SETQ FONT-WIDTH (* (// (+ (ARRAY-DIMENSION-N 2 CD) 31.) 32.) 32.))))
    ;; Now copy the data.
    (DO ((ROW 0 (1+ ROW)))
	(( ROW FONT-HEIGHT))
	(DO ((COL 0 (+ COL COL-INCR))
	     (PIXEL-COL 0 (1+ PIXEL-COL)))
	    (( PIXEL-COL FONT-WIDTH))
	    (SETQ PIXEL (COND ((OR (>= COL WIDTH) (>= ROW HEIGHT)) 0)
			      (DOUBLE-WIDTH-P
			       (+ (COND ((>= (1+ COL) WIDTH) 0)
					(T (AR-2 CD ROW (1+ COL))))
				  (* 2 (AR-2 CD ROW COL))))
			      (T (AR-2 CD ROW COL))))
	    (FONT-SET-PIXEL PIXEL FONT CHAR-CODE
                            ROW PIXEL-COL))))

;Create an FD format font from an internal format font

(DEFUN FONT-INTO-FONT-DESCRIPTOR (FONT &OPTIONAL (DBL-WIDTH-P NIL)
		   &AUX (FONT-DESCRIPTOR (MAKE-FONT-DESCRIPTOR))
			(LINE-SPACING (FONT-CHAR-HEIGHT FONT))
			(RASTER-HEIGHT (FONT-RASTER-HEIGHT FONT))
			(BASELINE (FONT-BASELINE FONT))
			(BLINKER-HEIGHT (FONT-BLINKER-HEIGHT FONT))
			(BLINKER-WIDTH (FONT-BLINKER-WIDTH FONT))
			(SPACE-WIDTH (FONT-CHAR-WIDTH FONT))
			FONT-CHARS-EXIST-TABLE
			TEMP RASTER-WIDTH CHARACTER-WIDTH LEFT-KERN PIXEL
			)
       (ERRSET (SETQ FONT-CHARS-EXIST-TABLE (FONT-CHARS-EXIST-TABLE FONT)) NIL)
       (SETF (FD-NAME FONT-DESCRIPTOR) (FONT-NAME FONT))
       (SETF (FD-LINE-SPACING FONT-DESCRIPTOR) LINE-SPACING)
       (SETF (FD-BASELINE FONT-DESCRIPTOR)BASELINE)
       (SETF (FD-BLINKER-HEIGHT FONT-DESCRIPTOR) BLINKER-HEIGHT)
       (SETF (FD-BLINKER-WIDTH FONT-DESCRIPTOR) BLINKER-WIDTH)
       (SETF (FD-SPACE-WIDTH FONT-DESCRIPTOR) SPACE-WIDTH)
       (SETF (FD-DOUBLE-WIDTH-P FONT-DESCRIPTOR) DBL-WIDTH-P)
       (DO ((CHAR-CODE 0 (1+ CHAR-CODE)))
	   (( CHAR-CODE 200))
	   (AND FONT-CHARS-EXIST-TABLE
		(ZEROP (AR-1 FONT-CHARS-EXIST-TABLE CHAR-CODE))
		(GO SKIP-CHAR))
	   (SETQ CHARACTER-WIDTH (COND ((SETQ TEMP (FONT-CHAR-WIDTH-TABLE FONT))
					(AR-1 TEMP CHAR-CODE))
				       (T (FONT-CHAR-WIDTH FONT))))
           (SETQ RASTER-WIDTH
                 (FONT-CHAR-MIN-RASTER-WIDTH FONT CHAR-CODE))
	   (SETQ LEFT-KERN (COND ((SETQ TEMP (FONT-LEFT-KERN-TABLE FONT))
				  (AR-1 TEMP CHAR-CODE))
				 (T 0)))
	   (SETQ TEMP (MAKE-CHAR-DESCRIPTOR
			       MAKE-ARRAY (NIL 'ART-4B (LIST RASTER-HEIGHT RASTER-WIDTH))
			       CD-CHAR-WIDTH CHARACTER-WIDTH
			       CD-CHAR-LEFT-KERN LEFT-KERN))
	   (AS-1 TEMP FONT-DESCRIPTOR CHAR-CODE)
	   (COND (DBL-WIDTH-P (DO ((ROW 0 (1+ ROW)))
                                  (( ROW RASTER-HEIGHT))
                                  (DO ((COLI 0 (1+ COLI))
                                       (COL 0 (+ 2 COL)))
                                      (( COL RASTER-WIDTH))
                                      (SETQ PIXEL (FONT-GET-PIXEL FONT CHAR-CODE ROW COLI))
                                      (AS-2 PIXEL TEMP ROW COL)
                                      (AS-2 (LSH PIXEL -1) TEMP ROW (1+ COL)))))
                 (T (DO ((ROW 0 (1+ ROW)))
                        (( ROW RASTER-HEIGHT))
                        (DO ((COL 0 (1+ COL)))
                            (( COL RASTER-WIDTH))
                            (AS-2 (FONT-GET-PIXEL FONT CHAR-CODE ROW COL)
                                  TEMP ROW COL)))))
	   SKIP-CHAR)
       FONT-DESCRIPTOR)

;; Read in a kst file and make and return a FONT-DESCRIPTOR,
;; which is an alternate convenient representation for a font.
(defun read-kst-into-font-descriptor (filename &optional fontname &aux stream fd)
  (setq filename (fs:file-parse-name filename nil t ':kst))
  (or fontname (setq fontname (funcall filename ':name)))
  (and (stringp fontname) (setq fontname (intern fontname "FONTS")))
  (setq stream (open filename '(:fixnum :in :byte-size 9.)))
  (setq fd (make-font-descriptor fd-name fontname))
  ;; Discard KSTID.
  (dotimes (i 4) (funcall stream ':tyi))
  ;; Discard column position adjust until I find out what it means.
  (or (zerop (funcall stream ':tyi))
      (ferror nil
	      "Nonzero column-position-adjust in font ~A -- what does that mean?"
	      fontname))
  (setf (fd-space-width fd) 0)			;Just in case no space character.
  (setf (fd-baseline fd) (funcall stream ':tyi))
  (setf (fd-line-spacing fd) (read-kst-halfword stream))
  (setf (fd-blinker-height fd)
	(fd-line-spacing fd))
  (setf (fd-name fd) fontname)
  (let (kern char-code raster-width char-width byte-list byte-list-head cd tem
	     (line-height (fd-line-spacing fd)))
    (do () ((= (logand (read-kst-halfword stream) (read-kst-halfword stream)) -1))
      (setq kern (read-kst-halfword stream))
      (setq char-code (read-kst-halfword stream))
      (setq raster-width (read-kst-halfword stream))
      (setq char-width (read-kst-halfword stream))
      (setq cd (make-char-descriptor
		 make-array (nil art-1b (list line-height raster-width))))
      (setf (cd-char-width cd) char-width)
      (setf (cd-char-left-kern cd) kern)
      (as-1 cd fd char-code)
      (and (= char-code #/ )
	   (setf (fd-space-width fd) char-width))
      ;; read in the bits of the character
      (setq byte-list nil
	    byte-list-head (list nil nil nil nil))
      (dotimes (vpos line-height)
	;; Read in the next row.
	(dotimes (hpos raster-width)
	  ;; If byte is exhausted, get next byte into (car byte-list)
	  (cond ((zerop (\ hpos 8))
		 (setq byte-list (read-kst-bytes stream byte-list byte-list-head))))
	  (setq tem (logand 1 (lsh (car byte-list) (- (\ hpos 8)))))
	  (as-2 tem cd vpos hpos)))))
  (setf (fd-fill-pointer fd) 200)
  ;; Set width of blinker and space fields from the space character.
  (setf (fd-blinker-width fd)
	(fd-space-width fd))
  (funcall stream ':close)
  fd)

;; Read in a kst file and define a font.
;; The font name defaults from the file name.
(defun read-kst-into-font (filename &optional fontname
				    &aux stream font chars-exist-table
				    raster-width raster-height
				    rasters-per-word words-per-char)
    (setq filename (fs:file-parse-name filename nil t ':kst))
    (or fontname (setq fontname (funcall filename ':name)))
    (and (stringp fontname) (setq fontname (intern fontname "FONTS")))
    ;; Read file once to determine font parameters.
    (multiple-value (raster-width raster-height)
		    (read-kst-max-raster-width filename))
    ;; If this is a hairy wide font, then instead of writing it directly
    ;; make a font-descriptor and turn it into a font.
    (cond ((> raster-width 32.)
           (font-name-set-font-and-descriptor
               fontname
               (read-kst-into-font-descriptor filename fontname))
           fontname)
	  (t
           (setq rasters-per-word (// 32. raster-width))
	   (setq words-per-char (// (+ raster-height rasters-per-word -1) rasters-per-word))
	   ;; Now that we know the parameters, allocate the font.
	   (setq font (tv:make-font make-array (nil art-1b (* words-per-char 32. 200))))
	   (setf (font-rasters-per-word font) rasters-per-word)
	   (setf (font-words-per-char font) words-per-char)
	   (setf (font-raster-width font) raster-width)
	   (setf (font-raster-height font) raster-height)
	   (setf (font-char-height font) raster-height)
	   (setf (font-blinker-height font) raster-height)
	   (setf (font-name font) fontname)
	   (setq chars-exist-table (make-array nil art-1b 200))
	   (setf (font-chars-exist-table font) chars-exist-table)
	   ;; Now actually read in the data of the font.
	   (setq stream (open filename '(:fixnum :in :byte-size 9.)))
	   ;; Discard KSTID.
	   (dotimes (i 4) (funcall stream ':tyi))
	   ;; Discard column position adjust until I find out what it means.
	   (or (zerop (funcall stream ':tyi))
	       (ferror nil
		       "Nonzero column-position-adjust in font ~A -- what does that mean?"
		       fontname))
	   (setf (font-baseline font) (funcall stream ':tyi))
	   ;; Discard line height (already determined).
	   (read-kst-halfword stream)
	   (let (kern char-code char-width char-raster-width
                      byte-list byte-list-head tem bit-pos word-pos
		      (line-height raster-height))
	       (do () ((= (logand (read-kst-halfword stream) (read-kst-halfword stream)) -1))
		   (setq kern (read-kst-halfword stream))
		   (setq char-code (read-kst-halfword stream))
                   ;; While all chars have the same raster width in the lisp machine font,
                   ;; we need the raster width stored in the kst file to read the kst file.
		   (setq char-raster-width (read-kst-halfword stream))
		   (setq char-width (read-kst-halfword stream))
		   (as-1 1 chars-exist-table char-code)
		   ;; Now store the char width and left kern, creating the tables if nec.
		   (cond ((null (font-char-width font))
			  (setf (font-char-width font) char-width))
			 ((font-char-width-table font)
			  (as-1 char-width (font-char-width-table font) char-code))
			 ((= char-width (font-char-width font)))
			 (t (setf (font-char-width-table font)
				  (make-array nil art-16b 200))
			    (as-1 char-width (font-char-width-table font) char-code)))
		   (and (= char-code #/ )
			(setf (font-char-width font) char-width))
		   (cond ((not (zerop kern))
			  (or (font-left-kern-table font)
			      (setf (font-left-kern-table font)
                                    ;; Use art-32b so can hold both signs.
				    (make-array nil art-32b 200)))
			  (as-1 kern (font-left-kern-table font) char-code)))
		   ;; read in the bits of the character
		   (setq byte-list nil
			 byte-list-head (list nil nil nil nil))
		   (setq word-pos (* char-code words-per-char)
			 bit-pos 0)
		   (dotimes (vpos line-height)
		      ;; Find next row in font - advance to word boundary if nec.
		      (and (> (+ bit-pos raster-width) 32.)
			   (setq bit-pos 0 word-pos (1+ word-pos)))
		      ;; Read in that row.
		      (dotimes (hpos char-raster-width)
			 ;; If byte is exhausted, get next byte into (car byte-list)
			 (cond ((zerop (\ hpos 8))
				(setq byte-list (read-kst-bytes stream byte-list byte-list-head))))
			 (setq tem (logand 1 (lsh (car byte-list) (- (\ hpos 8)))))
			 (as-1 tem font (+ (lsh word-pos 5) bit-pos hpos)))
		      ;; Advance past this row in the font.
		      (setq bit-pos (+ bit-pos raster-width)))
		   ))
	   ;; Set width of blinker and space fields from the space character.
	   (setf (font-blinker-width font)
		 (font-char-width font))
	   (set fontname font)
	   (putprop fontname filename 'kst-file)
	   (funcall stream ':close)
	   fontname)))

;; Scan a kst file and return two values which are the
;; raster width and raster height needed in a TV format font to contain that font.
(defun read-kst-max-raster-width (filename &aux stream
						raster-height (raster-width 0)
						char-raster-width)
  (setq stream (open (fs:file-parse-name filename nil t ':kst) '(:fixnum :in :byte-size 9.)))
  ;; Discard KSTID.
  (dotimes (i 4) (funcall stream ':tyi))
  ;; Discard column-position-adjust
  (funcall stream ':tyi)
  ;; Discard baseline.
  (funcall stream ':tyi)
  ;; Remember font line height as raster height.
  (setq raster-height (read-kst-halfword stream))
  ;; Keep maxing raster widths of characters into raster-width
  (setq raster-width 0)
  (do () ((= (logand (read-kst-halfword stream) (read-kst-halfword stream)) -1))
    ;; Ignore char's left kern.
    (read-kst-halfword stream)
    ;; Ignore its character code.
    (read-kst-halfword stream)
    ;; Max in its raster width
    (setq char-raster-width (read-kst-halfword stream))
    (setq raster-width (max raster-width char-raster-width))
    ;; Ignore its character width.
    (read-kst-halfword stream)
    ;; Skip the bits of the character
    (prog ((bytes (* raster-height (// (+ char-raster-width 7) 8))))
	  (setq bytes (* 4 (// (+ bytes 3) 4)))
	  (dotimes (i bytes)
	    (funcall stream ':tyi))))
  (funcall stream ':close)
  (prog () (return raster-width raster-height)))

;; Fetch the next 8-bit byte where stream is a 9-bit byte stream.
;; byte-list-head should be a list of 4 things we can clobber.
;; byte-list is a tail of it.
;; We advance it, and if it is empty we fill byte-list-head
;; with four more 8-bit bytes and return that.
;; The car of our value is the next byte.
;; Save the value for the byte-list arg next time.
(defun read-kst-bytes (stream byte-list byte-list-head)
    (or (cdr byte-list)
	;; Exhausted the word - read another.
	(let ((hwd1 (read-kst-halfword stream))
	      (hwd2 (read-kst-halfword stream)))
	     (setq byte-list byte-list-head)
	     ;; Turn it into 4 8-bit bytes in byte-list.
	     (rplaca byte-list (ldb 1210 hwd1))
	     (rplaca (cdr byte-list) (ldb 0210 hwd1))
	     (rplaca (cddr byte-list)
		     (+ (lsh (ldb 0002 hwd1) 6)
			(ldb 1406 hwd2)))
	     (rplaca (cdddr byte-list) (ldb 0410 hwd2))
	     byte-list)))

;; Read two 9-bit bytes from stream, make an 18-bit halfword,
;; and sign-extend it.
(defun read-kst-halfword (stream &aux hwd)
    (setq hwd (+ (* (funcall stream ':tyi) 1000)
		 (funcall stream ':tyi)))
    (cond ((bit-test hwd 400000)
	   (logior hwd -400000))
	  (t hwd)))

;; It would be good to check for chars that are all zero and
;; flush them, and also to compute the actual needed raster width and use it.
(defun write-font-into-kst (fontname &optional filename &aux stream font)
  (and (stringp fontname) (setq fontname (intern fontname "FONTS")))
  (setq filename (fs:file-parse-name filename nil t ':kst))
  (setq font (symeval fontname))
  (cond ((font-indexing-table font)
	 (let ((fd (font-name-font-descriptor fontname)))
	   (write-font-descriptor-into-kst fd filename)))
	(t
	 (and (> (font-raster-height font)
		 (font-char-height font))
	      (format t "Warning: font raster height exceeds line height"))
	 (setq stream (open filename '(:fixnum :out :byte-size 9.)))
	 ;; Write KSTID as 0.
	 (dotimes (i 4) (funcall stream ':tyo 0))
	 ;; Write column position adjust as 0.
	 (funcall stream ':tyo 0)
	 ;; Write baseline and height into second header word.
	 (funcall stream ':tyo (font-baseline font))
	 (write-kst-halfword stream (font-char-height font))
	 ;; Then write out all the characters.
	 (let (kern-table char-width-table chars-exist-table
			  word-pos bit-pos byte-count byte
			  char-raster-width byte-list byte-list-head)
	   (setq kern-table (font-left-kern-table font)
		 char-width-table (font-char-width-table font))
	   (errset (setq chars-exist-table (font-chars-exist-table font)) nil)
	   (dotimes (char-code 200)
	     (and chars-exist-table
		  (zerop (ar-1 chars-exist-table char-code))
		  (go skip-char))
	     ;; Each char must start with a word containing a 1.
	     (write-kst-halfword stream 0)
	     (write-kst-halfword stream 1)
	     ;; left kern and char code fill the next word.
	     (write-kst-halfword stream
				 (or (and kern-table (ar-1 kern-table char-code)) 0))
	     (write-kst-halfword stream char-code)
	     ;; Raster width and char width are the next word.
	     (setq char-raster-width (max 1 (font-char-min-raster-width font char-code)))
	     (write-kst-halfword stream char-raster-width)
	     (write-kst-halfword stream 
				 (cond (char-width-table (or (ar-1 char-width-table char-code) 0))
				       (t (font-char-width font))))
	     ;; Write out the bits of the character
	     ;; Word-pos and bit-pos are used to point at a bit in the font.
	     (setq word-pos (* (font-words-per-char font) char-code))
	     (setq bit-pos 0 byte-count 0)
	     ;; Byte-list and its head are used to accumulate 4 bytes
	     ;; and then output them at once as a word.
	     ;; This is needed because the stream wants 9-bit bytes.
	     (setq byte-list-head (list nil nil nil nil))
	     (setq byte-list byte-list-head)
	     (dotimes (vpos (font-char-height font))
	       ;; Prepare to extract next row of char from font.
	       (and (> (+ bit-pos (font-raster-width font)) 32.)
		    (setq word-pos (1+ word-pos) bit-pos 0))
	       (setq byte 0)
	       ;; Get the row a bit at a time and fill up 8-bit bytes.
	       ;; Output the bytes when full.  Output the excess at the end.
	       ;; Count the bytes output with byte-count
	       (dotimes (hpos char-raster-width)
		 (cond ((and (= (\ hpos 8) 0) (not (zerop hpos)))
			(setq byte-count (1+ byte-count))
			(setq byte-list
			      (write-kst-byte stream byte byte-list byte-list-head))
			(setq byte 0)))
		 (or ( vpos (font-raster-height font))
		     (setq byte (+ byte (lsh (ar-1 font
						   (+ (* 32. word-pos) hpos bit-pos))
					     (\ hpos 8))))))
	       (setq byte-count (1+ byte-count))
	       (setq byte-list (write-kst-byte stream byte byte-list byte-list-head))
	       (setq bit-pos (+ bit-pos (font-raster-width font))))
	     ;; Pad to a word boundary.
	     (do () ((zerop (\ byte-count 4)))
	       (setq byte-list (write-kst-byte stream 0 byte-list byte-list-head))
	       (setq byte-count (1+ byte-count)))
	     skip-char)
	   ;; Mark end of file with two -1 words.
	   (dotimes (i 8)
	     (funcall stream ':tyo -1)))
	 (close stream))))

(defun write-font-descriptor-into-kst (fd filename &aux stream)
    (setq stream (open (fs:file-parse-name filename nil t ':kst)
		       '(:fixnum :out :byte-size 9.)))
    ;; Write KSTID as 0.
    (dotimes (i 4) (funcall stream ':tyo 0))
    ;; Write column position adjust as 0.
    (funcall stream ':tyo 0)
    ;; Write baseline and height into second header word.
    (funcall stream ':tyo (fd-baseline fd))
    (write-kst-halfword stream (fd-line-spacing fd))
    ;; Then write out all the characters.
    (let (cd char-height byte-count byte byte-list byte-list-head)
       (dotimes (char-code 200)
          (cond ((and (setq cd (ar-1 fd char-code))
                      ;; Wide fonts without chars-exist-tables can have 0-width chars.
                      (or (not (zerop (array-dimension-n 2 cd)))
                          (not (zerop (cd-char-width cd)))))
            ;; Each char must start with a word containing a 1.
            (write-kst-halfword stream 0)
            (write-kst-halfword stream 1)
            ;; left kern and char code fill the next word.
            (write-kst-halfword stream (cd-char-left-kern cd))
            (write-kst-halfword stream char-code)
            ;; Raster width and char width are the next word.
            (write-kst-halfword stream (array-dimension-n 2 cd))
            (write-kst-halfword stream (cd-char-width cd))
	    ;; Write out the bits of the character
            ;; Byte-list and its head are used to accumulate 4 bytes
            ;; and then output them at once as a word.
            ;; This is needed because the stream wants 9-bit bytes.
            (setq byte-list-head (list nil nil nil nil))
            (setq byte-list byte-list-head)
            (setq byte-count 0)
            (setq char-height (array-dimension-n 1 cd))
            (and (> char-height (fd-line-spacing fd))
                 (ferror nil "Character ~C height exceeds font line height in KST file"
                         char-code))
            (dotimes (vpos (fd-line-spacing fd))
               ;; Prepare to extract next row of char from font.
               (setq byte 0)
               ;; Get the row a bit at a time and fill up 8-bit bytes.
               ;; Output the bytes when full.  Output the excess at the end.
               ;; Count the bytes output with byte-count
               (dotimes (hpos (array-dimension-n 2 cd))
                   (cond ((and (= (\ hpos 8) 0) (not (zerop hpos)))
                          (setq byte-count (1+ byte-count))
                          (setq byte-list
                                (write-kst-byte stream byte byte-list byte-list-head))
                          (setq byte 0)))
                   (or ( vpos char-height)
                       (setq byte (+ byte (lsh (ar-2 cd vpos hpos) (\ hpos 8))))))
               (setq byte-count (1+ byte-count))
               (setq byte-list (write-kst-byte stream byte byte-list byte-list-head)))
            ;; Pad to a word boundary.
            (do () ((zerop (\ byte-count 4)))
               (setq byte-list (write-kst-byte stream 0 byte-list byte-list-head))
               (setq byte-count (1+ byte-count))))))
       ;; Mark end of file with two -1 words.
       (dotimes (i 8)
            (funcall stream ':tyo -1)))
    (close stream))

;; Write an 8-bit byte to the kst file.  We pack 4 bytes per word.
;; The stream is assumed to want 9-bit bytes.
;; Byte-list-head should be a list of length 4 we can clobber.
;; byte-list should initially be the same thing;  we return a new value to set it to.
(defun write-kst-byte (stream byte byte-list byte-list-head)
    (rplaca byte-list byte)
    (pop byte-list)
    (cond ((null byte-list)
           (setq byte-list byte-list-head)
           (write-kst-halfword stream
                  (+ (lsh (first byte-list) 10.)
                     (lsh (second byte-list) 2.)
                     (ldb 0602 (third byte-list))))
           (write-kst-halfword stream
                  (+ (lsh (ldb 0006 (third byte-list)) 12.)
                     (lsh (fourth byte-list) 4)))))
    byte-list)

(defun write-kst-halfword (stream halfword)
    (funcall stream ':tyo (ldb 1111 halfword))
    (funcall stream ':tyo (ldb 0011 halfword)))

;; Compute the smallest raster width needed to store the specified char
;; as defined by the specified font.
;; low-level means we are looking at one sub-character in a wide font.
(defun font-char-min-raster-width (font char-code &optional low-level
                                        &aux bit-pos word-pos tem
					min-raster-width f-raster-width raster-height)
    (cond ((and (not low-level)
                (setq tem (font-indexing-table font)))
           ;; If it's a wide font, go by the number of vertical stripes,
           ;; but also see how wide the rightmost stripe really needs to be.
           (max 0
                (+ (* 32. (- (ar-1 tem (1+ char-code)) (ar-1 tem char-code)))
                   -32.
                   (font-char-min-raster-width font (1- (ar-1 tem (1+ char-code))) t))))
	  (t (setq word-pos (* char-code (font-words-per-char font))
		   bit-pos 0
		   min-raster-width 0
		   f-raster-width (font-raster-width font)
		   raster-height (font-raster-height font))
	     (dotimes (vpos raster-height)
		 (and (> (+ bit-pos f-raster-width) 32.)
		      (setq bit-pos 0 word-pos (1+ word-pos)))
		 (do ((hpos 0 (1+ hpos)) (index (+ bit-pos (lsh word-pos 5)) (1+ index)))
		     ((= hpos f-raster-width))
		    (or (zerop (ar-1 font index))
			(setq min-raster-width (max (1+ hpos) min-raster-width))))
		 (setq bit-pos (+ f-raster-width bit-pos)))
	     min-raster-width)))

;; ALTO .AL format
;; Load an ALTO font file into a font, the easy way, via a font descriptor
(DEFUN READ-AL-INTO-FONT (FILENAME &OPTIONAL FONTNAME)
  (SETQ FILENAME (FS:FILE-PARSE-NAME FILENAME NIL T ':AL))
  (OR FONTNAME (SETQ FONTNAME (FUNCALL FILENAME ':NAME)))
  (AND (STRINGP FONTNAME) (SETQ FONTNAME (INTERN FONTNAME "FONTS")))
  (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME
				     (READ-AL-INTO-FONT-DESCRIPTOR FILENAME FONTNAME))
  (SYMEVAL FONTNAME))

;; Load an ALTO font file into a font descriptor
(DEFUN READ-AL-INTO-FONT-DESCRIPTOR (FILENAME &OPTIONAL FONTNAME
					      &AUX FD STREAM ARRAY LINE-HEIGHT)
  (SETQ FILENAME (FS:FILE-PARSE-NAME FILENAME NIL T ':AL))
  (OR FONTNAME (SETQ FONTNAME (FUNCALL FILENAME ':NAME)))
  (AND (STRINGP FONTNAME) (SETQ FONTNAME (INTERN FONTNAME "FONTS")))
  (SETQ FD (MAKE-FONT-DESCRIPTOR FD-NAME FONTNAME))
  (SETF (FD-NAME FD) FONTNAME)
  (SETQ STREAM (OPEN FILENAME '(:IN :FIXNUM)))
  (UNWIND-PROTECT
   (PROGN
    (SETQ LINE-HEIGHT (FUNCALL STREAM ':TYI))
    (SETF (FD-LINE-SPACING FD) LINE-HEIGHT)
    (SETF (FD-BLINKER-HEIGHT FD) LINE-HEIGHT)
    (LET ((BASELINE-AND-MAX-WIDTH (FUNCALL STREAM ':TYI)))
      (SETF (FD-BASELINE FD) (LDB 1007 BASELINE-AND-MAX-WIDTH))
      (SETF (FD-SPACE-WIDTH FD) (LDB 0010 BASELINE-AND-MAX-WIDTH)))
    (SETQ ARRAY (MAKE-ARRAY NIL 'ART-16B 1000. NIL '(0)))
    (DO CH (FUNCALL STREAM ':TYI) (FUNCALL STREAM ':TYI) (NULL CH)
      (ARRAY-PUSH-EXTEND ARRAY CH)))
   (FUNCALL STREAM ':CLOSE))
  (DO ((CH 0 (1+ CH))
       (CD)
       (CHAR-WIDTH))
      (( CH 200))
    (SETQ CHAR-WIDTH 0)
    (DO ((IDX CH)
	 (XW))
	(NIL)
      (SETQ IDX (+ IDX (AREF ARRAY IDX)))
      (SETQ XW (AREF ARRAY IDX))
      (IF (ZEROP (PROG1 (LOGAND XW 1) (SETQ XW (// XW 2))))
	  (SETQ CHAR-WIDTH (+ CHAR-WIDTH 16.)
		IDX XW)
	  (SETQ CHAR-WIDTH (+ CHAR-WIDTH XW))
	  (RETURN)))
    (SETQ CD (MAKE-CHAR-DESCRIPTOR MAKE-ARRAY (NIL ART-1B (LIST LINE-HEIGHT CHAR-WIDTH))))
    (SETF (CD-CHAR-WIDTH CD) CHAR-WIDTH)
    (AND (= CH #\SP) (SETF (FD-SPACE-WIDTH FD) CHAR-WIDTH))
    (SETF (CD-CHAR-LEFT-KERN CD) 0)
    (ASET CD FD CH)
    (READ-AL-INTO-FONT-DESCRIPTOR-1 CD ARRAY CH 0))
  (SETF (FD-FILL-POINTER FD) 200)
  ;; Set width of blinker and space fields from the space character.
  (SETF (FD-BLINKER-WIDTH FD) (FD-SPACE-WIDTH FD))
  FD)

;;;IDX is the address of the character-pointer
(DEFUN READ-AL-INTO-FONT-DESCRIPTOR-1 (CD ARRAY IDX XOFF &AUX XW HD-AND-XH)
  (SETQ IDX (+ IDX (AREF ARRAY IDX)))
  (SETQ XW (AREF ARRAY IDX)
	HD-AND-XH (AREF ARRAY (1+ IDX)))
  (DO ((I (- IDX (LDB 0010 HD-AND-XH)) (1+ I))
       (Y (LDB 1010 HD-AND-XH) (1+ Y)))
      ((= I IDX))
    (DO ((BITS (AREF ARRAY I) (LSH BITS 1))
	 (X XOFF (1+ X)))
	((ZEROP BITS))
      (AND (BIT-TEST 100000 BITS)
	   (ASET 1 CD Y X))))
  (OR (BIT-TEST 1 XW)
      (READ-AL-INTO-FONT-DESCRIPTOR-1 CD ARRAY (// XW 2) (+ XOFF 16.))))

(DEFUN THICKEN-FONT-DESCRIPTOR (FD &OPTIONAL NEW-NAME &AUX LEN NFD)
  (OR NEW-NAME (SETQ NEW-NAME (INTERN (STRING-APPEND (FD-NAME FD) #/B) "FONTS")))
  (SETQ LEN (ARRAY-LENGTH FD)
	NFD (MAKE-FONT-DESCRIPTOR MAKE-ARRAY (NIL ART-Q LEN)
				  FD-NAME NEW-NAME
				  FD-LINE-SPACING (FD-LINE-SPACING FD)
				  FD-BASELINE (FD-BASELINE FD)
				  FD-BLINKER-HEIGHT (FD-BLINKER-HEIGHT FD)
				  FD-BLINKER-WIDTH (FD-BLINKER-WIDTH FD)
				  FD-SPACE-WIDTH (FD-SPACE-WIDTH FD)))
  (DO ((I 0 (1+ I))
       (CD) (NCD))
      (( I LEN))
    (AND (SETQ CD (AREF FD I))
	 (LET ((WIDTH (ARRAY-DIMENSION-N 2 CD))
	       (HEIGHT (ARRAY-DIMENSION-N 1 CD)))
	   (SETQ NCD (MAKE-CHAR-DESCRIPTOR MAKE-ARRAY (NIL ART-4B (LIST HEIGHT (1+ WIDTH)))
					   CD-CHAR-WIDTH (1+ (CD-CHAR-WIDTH CD))
					   CD-CHAR-LEFT-KERN (CD-CHAR-LEFT-KERN CD)))
	   (COPY-ARRAY-CONTENTS CD NCD)
	   (DOTIMES (J HEIGHT)
	     (DOTIMES (I WIDTH)
	       (ASET (LOGIOR (AREF CD J I) (AREF NCD J (1+ I))) NCD J (1+ I))))
	   (ASET NCD NFD I))))
  NFD)

(DEFUN THICKEN-FONT (FONT-SYMBOL &AUX FD NFD NFS NFNT)
  (SETQ FD (FONT-NAME-FONT-DESCRIPTOR FONT-SYMBOL))
  (SETQ NFD (THICKEN-FONT-DESCRIPTOR FD))
  (SETQ NFS (FD-NAME NFD))
  (SETQ NFNT (FONT-DESCRIPTOR-INTO-FONT NFD))
  (SET NFS NFNT)
  (PUTPROP NFS NFD 'FONT-DESCRIPTOR)
  (PUTPROP NFS NFNT 'FONT-DESCRIBED)
  NFS)
