;;; -*- Mode:Lisp; Package:Press; base:8.; ibase:8. -*-

;;;; PRESS File and DOVER software

(DEFVAR PRESS-USE-EFTP NIL)	;T => EFTP, NIL => Chaos
(DEFVAR DOVER-ADDRESS 1002)	;2#2#
(DEFVAR 926-ALTO-ADDRESS 1140)	;Alto in Moon's office
				;CADR-6 is 1#36# as far as Xerox is concerned

(EVAL-WHEN (EVAL COMPILE LOAD)
     (OR (FBOUNDP 'CHAOS:GET-PUP) (LOAD "AI;LMIO1;EFTP QFASL")))

;Don't get too strung out by the little frob at the end of the message.
(DEFUN PRINT-DOVER-STATUS ()
  (DO ((N-RETRIES 10. (1- N-RETRIES))
       (PORT (CHAOS:GET-PORT-NUMBER))
       (PUP))
      ((ZEROP N-RETRIES)
       (FORMAT T "~&Dover is not responding (may be printing).~%"))
    (CHAOS:TRANSMIT-PUP (CHAOS:GET-PUP DOVER-ADDRESS 21 PORT 200 0) 0)
    (COND ((SETQ PUP (CHAOS:RECEIVE-PUP PORT))
	   (COND ((= (CHAOS:PUP-TYPE PUP) 201)
		  (FORMAT T "~&Dover status: ~[~;Spooler shut off.~;Spooler available.~;Spooler busy.~]  ~A~%"
			    (AREF PUP 22.)
			    (CHAOS:PUP-STRING PUP 2))
		  (CHAOS:FREE-INT-PKT PUP)
		  (RETURN T))
		 (T (CHAOS:RECEIVED-RANDOM-PUP PUP)))))))

;;; Routines for building Press pages and shipping them out an EFTP connection
;;; The state is all in special variables so you can only do one at a time
;;; Later this might be made into a more stream-like thing (as a "resource")

(DEFVAR PRESS-EFTP-STREAM)			;EFTP connection we send through
(DEFVAR PRESS-N-CHARS)				;Number of characters sent this part
(DEFVAR PRESS-CURRENT-RECORD-NUMBER)		;Record number within file
(DEFVAR PRESS-X)				;X position computed as characters sent
(DEFVAR PRESS-Y)				;Y ..
(DEFVAR PRESS-PAGE-NUMBER)			;Serial number of page
(DEFVAR PRESS-END-PAGE-HOOK   NIL)		;If non-NIL, function to call
(DEFVAR PRESS-PENDING-CHARS)			;Number of chars output but not yet known
						;about at the "entity" level

(DEFVAR PRESS-DATA-LIST-START)			;Value of PRESS-N-CHARS at start of entity
(DEFVAR PRESS-ENTITY-LIST-START)		;Value of (size of entity buffer) at ..

(DEFVAR PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE 4000.)
(DEFVAR PRESS-PAGE-ENTITY-BUFFER
	(MAKE-ARRAY NIL 'ART-8B PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE NIL '(0)))
						;This holds the "entity" portion of the
						;current page

(DEFVAR PRESS-PART-LIST) ;List of elements (part-type record-number n-records n-padding-words)
(DEFVAR PRESS-FONT-LIST) ;List of elements (family-name face-name point-size rotation
			 ;		    width height width-table)
(DEFVAR PRESS-CURRENT-FONT NIL)			;Element for selected font

(DEFVAR DOVER-X0 2000.)				;2 cm left margin
(DEFVAR DOVER-Y0 (FIX (* 9.8 2540.)))		;Where the page number goes
(DEFVAR DOVER-Y1 (FIX (* 9.5 2540.)))		;Where the text starts
(DEFVAR DOVER-Y2 (FIX (* 0.5 2540.)))		;Margin at the bottom of the page
(DEFVAR LINE-WIDTH 25.)				;Line width .01 inch
;(DEFVAR DIAGONAL-LINE-WIDTH 18.)		;Make darkness come out even
;This provides nice thin lines, for thinner lines you might want 2 instead of 4
(DEFVAR PRESS-LINE-FONT '(NEWVEC "" 4 0 0 0 NIL))
;The way these fonts work is that the point size is the thickness of the line,
;and NEWVEC has round ends, HNEWVEC has horizontal ends, and SNEWVEC has square
;ends (that is diamond on a 45-degree line).  The way the characters are organized
;is:  Consider the right half-box, and all its radii, that is lines proceeding
;clockwise from straight-up through straight-down.  The fonts contain vectors to
;all points with integral coordinates on half-boxes of various sizes.  The widths
;of the characters are set up so that the vectors chain properly.
;
;	000-100    The 16.-bit box
;	120-160    The 8.-bit box
;	170-210    The 4.-bit box
;	214-224    The 2.-bit box
;	226-232    The 1.-bit box
;	240        The 0-bit box (or isolated point).

;;;; Output to the Data and Entity Lists

;;; Macros to output things to the entity buffer

(DEFMACRO PRESS-ENTITY-BYTE (BYTE)
  `(ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER ,BYTE
		      PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE))

(DEFMACRO PRESS-ENTITY-WORD (WORD)
  (IF (ATOM WORD)
      `(PROGN (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB 1010 ,WORD)
				 PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
	      (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB 0010 ,WORD)
				 PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE))
      `(LET ((FOO ,WORD))
	 (PRESS-ENTITY-WORD FOO))))

(DEFMACRO PRESS-ENTITY-32WORD (WORD)
  `(LET ((FOO ,WORD))
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB 3010 FOO)
			PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB 2010 FOO)
			PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB 1010 FOO)
			PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)
     (ARRAY-PUSH-EXTEND PRESS-PAGE-ENTITY-BUFFER (LDB 0010 FOO)
			PRESS-PAGE-ENTITY-BUFFER-EXTENSION-SIZE)))


;;; Macros to output to the data list.  These do not catch format chars (see PRESS-CHAR).

(DEFMACRO PRESS-DATA-BYTE (BYTE)
  `(PROGN (FUNCALL PRESS-EFTP-STREAM ':TYO ,BYTE)
	  (SETQ PRESS-N-CHARS       (1+ PRESS-N-CHARS))  ))

(DEFMACRO PRESS-DATA-WORD (WORD)
  `(LET ((FOO ,WORD))
     (PRESS-DATA-BYTE (LDB 1010 FOO))
     (PRESS-DATA-BYTE (LDB 0010 FOO)) ))

(DEFMACRO PRESS-DATA-32WORD (WORD)
  `(LET ((FOO ,WORD))
     (PRESS-DATA-BYTE (LDB 3010 FOO))
     (PRESS-DATA-BYTE (LDB 2010 FOO))
     (PRESS-DATA-BYTE (LDB 1010 FOO))
     (PRESS-DATA-BYTE (LDB 0010 FOO))))


;;;; PRESS FORMAT DECLARATIONS


;;; Set up so #,<SET-X> turns into 356 -- except the compiler chokes!!

(DEFMACRO DEFPRESS (NAME . BODY)
  `(DEFVAR ,NAME ,(CAR BODY)))

;;; These are ENTITY LIST COMMANDS

(DEFPRESS <SHOW-CHARACTERS-SHORT>		  0 + (N-1 1))
(DEFPRESS <SKIP-CHARACTERS-SHORT>		 40 + (N-1 1))
(DEFPRESS <SHOW-CHARACTERS-AND-SKIP>		100 + (N-1 1))
(DEFPRESS <SET-SPACE-X-SHORT>			140 + (X 2))
(DEFPRESS <SET-SPACE-Y-SHORT>			150 + (Y 2))
(DEFPRESS <FONT>				160 + (FONT 1))
(DEFPRESS <SKIP-CONTROL-BYTES-IMMEDIATE>	353 (N 1))
(DEFPRESS <ALTERNATIVE>				354 (EL-TYPES 2) (EL-BYTES 4) (DL-BYTES 4))
(DEFPRESS <ONLY-ON-COPY>			355 (N 1))
(DEFPRESS <SET-X>				356 (X 2))
(DEFPRESS <SET-Y>				357 (Y 2))
(DEFPRESS <SHOW-CHARACTERS>			360 (N 1))
(DEFPRESS <SKIP-CHARACTERS>			361 (N 1))
(DEFPRESS <SKIP-CONTROL-BYTES>			362 (N 2) (TYPE 1))
(DEFPRESS <SHOW-CHARACTER-IMMEDIATE>		363 (CHAR 1))
(DEFPRESS <SET-SPACE-X>				364 (S 2))
(DEFPRESS <SET-SPACE-Y>				365 (S 2))
(DEFPRESS <RESET-SPACE>				366)
(DEFPRESS <SPACE>				367)
(DEFPRESS <SET-BRIGHTNESS>			370 (B 1))
(DEFPRESS <SET-HUE>				371 (H 1))
(DEFPRESS <SET-SATURATION>			372 (S 1))
(DEFPRESS <SHOW-OBJECT>				373 (N 2))
(DEFPRESS <SHOW-DOTS>				374 (N 4))
(DEFPRESS <SHOW-DOTS-OPAQUE>			375 (N 4))
(DEFPRESS <SHOW-RECTANGLE>			376 (WIDTH 2) (HEIGHT 2))
(DEFPRESS <NOP>					377)


;;; These are DATA LIST COMMANDS

(DEFPRESS <<MOVETO>>				  0)
(DEFPRESS <<DRAWTO>>				  1)
(DEFPRESS <<DRAWCURVE>>				  2)

(DEFPRESS <<SET-CODING>>			  1)
(DEFPRESS <<SET-WINDOW>>			  1)
(DEFPRESS <<SET-MODE>>				  2)
(DEFPRESS <<SET-SIZE>>				  2)
(DEFPRESS <<DOTS-FOLLOW>>			  3)
(DEFPRESS <<GET-DOTS-FROM-FILE>>		  4)
(DEFPRESS <<GET-DOTS-FROM-PRESS-FILE>>		  5)
(DEFPRESS <<SET-SAMPLING-PROPERTIES>>		  6)

(DEFPRESS <<SSP-INPUT-INTENSITY>>		  0)
(DEFPRESS <<SSP-OUTPUT-INTENSITY>>		  1)
(DEFPRESS <<SSP-SCREEN>>			  2)
(DEFPRESS <<SSP-DOT>>				  3)


;;;; Start Press File

;Start generating a press file.  Optional argument is EtherNet host address number
;or a filename string (for delayed or spooled printing)
;There really should be code in here like in the various dover programs,
;that doesn't try to send if Spruce is printing.  Then again, maybe not,
;since the Lisp machine is not really a timesharing system.

(DEFUN PRESS-START-FILE (&OPTIONAL (HOST-ADDRESS DOVER-ADDRESS))
  (OR (FBOUNDP 'LOAD-FONT-WIDTHS) (LOAD "AI:LMIO1;RFONTW QFASL"))
  (OR (BOUNDP 'FONT-WIDTH-DATA) (LOAD-FONT-WIDTHS))
  (SETQ PRESS-EFTP-STREAM
	(IF (NUMBERP HOST-ADDRESS)
	    (IF PRESS-USE-EFTP (CHAOS:MAKE-EFTP-WRITE-STREAM HOST-ADDRESS T)
		(LET ((CONN (CHAOS:CONNECT 426 "DOVER")))
		  (AND (STRINGP CONN)
		       (FERROR NIL "~A - cannot connect to DOVER server at AI-CHAOS-11" CONN))
		  (CHAOS:STREAM CONN)))
	    (OPEN (SI:FILE-MERGE-PATHNAMES
		    HOST-ADDRESS
		    (FORMAT NIL "MC:.DOVR.;~A >" USER-ID))
		  '(:WRITE :FIXNUM :BYTE-SIZE 8)))
	PRESS-CURRENT-RECORD-NUMBER 0
	PRESS-PART-LIST NIL
	PRESS-FONT-LIST NIL			;this is a crock ***
	PRESS-PAGE-NUMBER 1
	))


;;;; Finish Press File

;Output font directory, part directory, document directory
(DEFUN PRESS-END-FILE (FILE-NAME CREATION-DATE
		       &OPTIONAL (N-COPIES 1) (USER-NAME USER-ID))
  ;; The font directory part
  (STORE-ARRAY-LEADER 0 PRESS-PAGE-ENTITY-BUFFER 0)
  (DO ((L PRESS-FONT-LIST (CDR L))		; *** crock
       (FONT-NUMBER 0 (1+ FONT-NUMBER))
       (FONT))
      ((NULL L))
    (SETQ FONT (CAR L))
    (PRESS-ENTITY-WORD 16.)			;Length in words
    (PRESS-ENTITY-BYTE 0)			;Font set 0 **** crock
    (PRESS-ENTITY-BYTE FONT-NUMBER)
    (PRESS-ENTITY-BYTE 0)			;First char
    (PRESS-ENTITY-BYTE 177)			;Last char
    (PRESS-ENTITY-BCPL-STRING (STRING-UPCASE (FIRST FONT)) 20.)	;Family
    (PRESS-ENTITY-BYTE (ENCODE-PRESS-FACE (SECOND FONT)))	;Face code
    (PRESS-ENTITY-BYTE 0)			;Source (same as first char)
    (PRESS-ENTITY-WORD (THIRD FONT))		;Positive is points, negative is micas
    (PRESS-ENTITY-WORD (FOURTH FONT)))		;Rotation in minutes of arc anticlockwise
  (PRESS-ENTITY-WORD 0)				;End mark
  (FUNCALL PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  (PRESS-FINISH-PART (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 1)
  ;; That took care of the font directory, now the part directory
  (STORE-ARRAY-LEADER 0 PRESS-PAGE-ENTITY-BUFFER 0)
  (DOLIST (X (REVERSE PRESS-PART-LIST))		;NOT nreverse!
    (PRESS-ENTITY-WORD (FIRST X))		;Part type
    (PRESS-ENTITY-WORD (SECOND X))		;Starting record number
    (PRESS-ENTITY-WORD (THIRD X))		;Number of records
    (PRESS-ENTITY-WORD (FOURTH X)))		;Amount of padding
  (FUNCALL PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  (PRESS-FINISH-PART (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 'FOO)
  ;; The document directory
  (STORE-ARRAY-LEADER 0 PRESS-PAGE-ENTITY-BUFFER 0)
  (PRESS-ENTITY-WORD 27183.)			;Password
  (PRESS-ENTITY-WORD (1+ PRESS-CURRENT-RECORD-NUMBER))	;File size
  (PRESS-ENTITY-WORD (1- (LENGTH PRESS-PART-LIST)))	;Number of parts
  (PRESS-ENTITY-WORD (SECOND (CAR PRESS-PART-LIST)))	;Record number of part directory
  (PRESS-ENTITY-WORD (THIRD (CAR PRESS-PART-LIST)))	;Number of records in part dir
  (PRESS-ENTITY-WORD 0)				;Back-pointer
  (PRESS-ENTITY-32WORD 0)			;[Date]
  (PRESS-ENTITY-WORD 1)				;First copy to print
  (PRESS-ENTITY-WORD N-COPIES)			;Last copy to print
  (PRESS-ENTITY-WORD -1)			;Print all pages
  (PRESS-ENTITY-WORD -1)			;..
  (PRESS-ENTITY-WORD -1)			;Default printing mode
  (DOTIMES (I (- 200 13.))			;Padding
    (PRESS-ENTITY-WORD -1))
  (PRESS-ENTITY-BCPL-STRING FILE-NAME 52.)
  (PRESS-ENTITY-BCPL-STRING USER-NAME 32.)
  (PRESS-ENTITY-BCPL-STRING CREATION-DATE 40.)
  (FUNCALL PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  (PRESS-FINISH-PART (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0) 1)
  ;; Except for :CLOSE itself this is superfluous except
  ;; when going through the AI-CHAOS-11 dover server
  (AND (MEMQ ':EOF (FUNCALL PRESS-EFTP-STREAM ':WHICH-OPERATIONS))
       (FUNCALL PRESS-EFTP-STREAM ':EOF))
  (FUNCALL PRESS-EFTP-STREAM ':CLOSE))

;;;; Pages

;Start a page

(DEFUN PRESS-OPEN-PAGE ()
  (STORE-ARRAY-LEADER 0 PRESS-PAGE-ENTITY-BUFFER 0)
  (SETQ PRESS-N-CHARS 0))


;;; Finish a page.

(DEFUN PRESS-CLOSE-PAGE ()
  (IF PRESS-END-PAGE-HOOK
      (FUNCALL PRESS-END-PAGE-HOOK))		; User must open his own entity
  ;; Make the length of the data buffer a multiple of a word
  (COND ((ODDP PRESS-N-CHARS)
	 (FUNCALL PRESS-EFTP-STREAM ':TYO 0)
	 (SETQ PRESS-N-CHARS (1+ PRESS-N-CHARS))))
  ;; Output a zero word between the data list and the entity list
  (FUNCALL PRESS-EFTP-STREAM ':TYO 0)
  (FUNCALL PRESS-EFTP-STREAM ':TYO 0)
  ;; Output the entity buffer
  (FUNCALL PRESS-EFTP-STREAM ':STRING-OUT PRESS-PAGE-ENTITY-BUFFER)
  ;; Pad to a record (512-byte) boundary, and advance PRESS-CURRENT-RECORD-NUMBER
  (PRESS-FINISH-PART (+ PRESS-N-CHARS 2 (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER)) 0)
  (SETQ PRESS-PAGE-NUMBER (1+ PRESS-PAGE-NUMBER))
  )

;Hair shared between page parts and other parts
(DEFUN PRESS-FINISH-PART (NBYTES PART-TYPE &AUX NWORDS NRECORDS PADDING)
  (CHECK-ARG NBYTES EVENP "an even number of bytes")
  (SETQ NWORDS (// NBYTES 2))
  (SETQ PADDING (\ (- 256. (\ NWORDS 256.)) 256.))
  (SETQ NWORDS (+ NWORDS PADDING))
  (SETQ NRECORDS (// NWORDS 256.))
  (DOTIMES (I (* PADDING 2))
    (FUNCALL PRESS-EFTP-STREAM ':TYO 0))
  (PUSH (LIST PART-TYPE PRESS-CURRENT-RECORD-NUMBER NRECORDS PADDING)
	PRESS-PART-LIST)
  (SETQ PRESS-CURRENT-RECORD-NUMBER (+ PRESS-CURRENT-RECORD-NUMBER NRECORDS)))


;;; press-start-page is an ungodly crock that is here just for existing programs
;;; using press-open-page and explicitly opening entities is cleaner
(DEFUN PRESS-START-PAGE ()
  (PRESS-OPEN-PAGE)
  (PRESS-START-ENTITY)
  (PRESS-SET-CURSOR 0 DOVER-Y1))		;Put cursor at top of page

; This is also here for existing programs
(DEFUN PRESS-END-PAGE ()
  (AND PRESS-END-PAGE-HOOK (FUNCALL PRESS-END-PAGE-HOOK))	;Let user put titles etc.
  (PRESS-END-ENTITY)
  (LET ((PRESS-END-PAGE-HOOK NIL))
    (PRESS-CLOSE-PAGE)))


;;;; Entities

;;;Start an entity

(DEFUN PRESS-OPEN-ENTITY ()
  (SETQ PRESS-DATA-LIST-START   PRESS-N-CHARS
	PRESS-ENTITY-LIST-START (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER)
	PRESS-PENDING-CHARS     0))

;Finish the current entity.  You can start another if you like.
(DEFUN PRESS-CLOSE-ENTITY (&OPTIONAL (X-OFF DOVER-X0) (Y-OFF DOVER-Y2)
				     (WIDTH (*  8. 2540.)) (HEIGHT (* 11. 2540.)))
  (PRESS-PUT-PENDING-CHARS)
  ;; Pad entity to word boundary with NOP
  (AND (ODDP (ARRAY-LEADER PRESS-PAGE-ENTITY-BUFFER 0))
       (PRESS-ENTITY-BYTE 377))
  ;; Entity trailer
  (PRESS-ENTITY-BYTE 0)				;Type     **** crocks
  (PRESS-ENTITY-BYTE 0)				;Font set ****
  (PRESS-ENTITY-32WORD PRESS-DATA-LIST-START)	;Begin-byte
  (PRESS-ENTITY-32WORD (- PRESS-N-CHARS PRESS-DATA-LIST-START))	;Byte-length
  (PRESS-ENTITY-WORD X-OFF)			;X offset (left margin)
  (PRESS-ENTITY-WORD Y-OFF)			;Y offset (bottom margin)
  (PRESS-ENTITY-WORD 0)				;Left     ****
  (PRESS-ENTITY-WORD 0)				;Bottom   ****
  (PRESS-ENTITY-WORD WIDTH)			;Width
  (PRESS-ENTITY-WORD HEIGHT)			;Height
  (PRESS-ENTITY-WORD				;Entity length
    (// (- (+ (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER) 2)
	   PRESS-ENTITY-LIST-START)
	2)))

;Finish the current entity if the entity buffer is getting full.
;This will cause you to lose your cursor position and selected font.
;The problem is that if you have more than 32768 bytes in an entity,
;the Alto suffers from 16-bit brain-rot.
(DEFUN PRESS-MAYBE-NEW-ENTITY ()
  (COND ((> (ARRAY-ACTIVE-LENGTH PRESS-PAGE-ENTITY-BUFFER) 25000.)
	 (PRESS-END-ENTITY)
	 (PRESS-START-ENTITY))))

;;; grandfather version of PRESS-OPEN-ENTITY
(DEFUN PRESS-START-ENTITY (&AUX TEM)
  (PRESS-OPEN-ENTITY)
  (AND PRESS-CURRENT-FONT			;Restore font
       (SETQ TEM (FIND-POSITION-IN-LIST PRESS-CURRENT-FONT PRESS-FONT-LIST))
       (PRESS-SELECT-FONT TEM)))

(DEFUN PRESS-END-ENTITY ()
  (PRESS-CLOSE-ENTITY))

;;;; Random Functions

;Set (X,Y) position on the page
(DEFUN PRESS-SET-CURSOR (X Y)
  (PRESS-PUT-PENDING-CHARS)
  (PRESS-ENTITY-BYTE 356)	;Set-X
  (PRESS-ENTITY-WORD X)
  (PRESS-ENTITY-BYTE 357)	;Set-Y
  (PRESS-ENTITY-WORD Y)
  (SETQ PRESS-X X PRESS-Y Y))

;Put show-chars command for any pending characters
(DEFUN PRESS-PUT-PENDING-CHARS ()
  (COND ((ZEROP PRESS-PENDING-CHARS) NIL)
	(( PRESS-PENDING-CHARS 40) (PRESS-ENTITY-BYTE (1- PRESS-PENDING-CHARS)))
	(T (DO () ((< PRESS-PENDING-CHARS 400))
	     (PRESS-ENTITY-BYTE 360)
	     (PRESS-ENTITY-BYTE 377)
	     (SETQ PRESS-PENDING-CHARS (- PRESS-PENDING-CHARS 377)))
	   (PRESS-ENTITY-BYTE 360)
	   (PRESS-ENTITY-BYTE PRESS-PENDING-CHARS)))
  (SETQ PRESS-PENDING-CHARS 0))

;Output a character.  May even be a format effector.
(DEFUN PRESS-CHAR (CHAR)
  (COND ((< CHAR 200)				;Printing
	 (FUNCALL PRESS-EFTP-STREAM ':TYO CHAR)
	 (SETQ PRESS-N-CHARS (1+ PRESS-N-CHARS))
	 (SETQ PRESS-PENDING-CHARS (1+ PRESS-PENDING-CHARS))
	 (LET ((WIDTH (AREF (SEVENTH PRESS-CURRENT-FONT) CHAR)))
	   (IF (MINUSP WIDTH) (FORMAT ERROR-OUTPUT
				      "~&~C (~O) undefined character in ~A~D~A~%"
				      CHAR CHAR
				      (FIRST PRESS-CURRENT-FONT)
				      (THIRD PRESS-CURRENT-FONT)
				      (SECOND PRESS-CURRENT-FONT))
	       (SETQ PRESS-X (+ WIDTH PRESS-X)))))
	((= CHAR #\TAB)
	 ;; The bounding box seems to be wedged, it's not the same as the character
	 ;; width in fixed-width fonts.  So use the width of space.
	 (LET ((TAB-WIDTH (* 8 (AREF (SEVENTH PRESS-CURRENT-FONT) #\SP))))
	   (PRESS-SET-CURSOR (* (1+ (// PRESS-X TAB-WIDTH)) TAB-WIDTH) PRESS-Y)))
	((= CHAR #\CR)
	 (LET ((Y (- PRESS-Y (SIXTH PRESS-CURRENT-FONT))))
	   (IF (MINUSP Y) (PRESS-CHAR #\FORM)
	       (PRESS-SET-CURSOR 0 Y))))
	((= CHAR #\FORM)
	 (PRESS-END-PAGE)
	 (PRESS-START-PAGE)))
  NIL)

;Output a string.  May contain format effectors.
(DEFUN PRESS-STRING (STRING &OPTIONAL (FROM 0) TO)
  (SETQ STRING (STRING STRING))
  (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
  (DO I FROM (1+ I) ( I TO)
      (PRESS-CHAR (AREF STRING I))))

;;;; Font Stuff

;;; These should be rewritten to consider font-set ****
;;; Also, want to define font numbers randomly instead of sequentially

;Add a font to the font set and return its font number
(DEFUN PRESS-DEFINE-FONT (FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
  (LET ((WIDTH (GET-FONT-WIDTH-AND-HEIGHT FAMILY-NAME FACE-NAME POINT-SIZE))
	HEIGHT WIDTH-ARRAY FONT-DESC FONT-NUMBER)
    (SETQ HEIGHT (CADR WIDTH) WIDTH (CAR WIDTH))	;Bounding box for font
    (SETQ WIDTH-ARRAY (GET-FONT-WIDTH-DATA FAMILY-NAME FACE-NAME POINT-SIZE))
    (SETQ FONT-DESC (LIST FAMILY-NAME FACE-NAME POINT-SIZE ROTATION
			  WIDTH HEIGHT WIDTH-ARRAY))
    (OR (MEMBER FONT-DESC PRESS-FONT-LIST)
	(SETQ PRESS-FONT-LIST (NCONC PRESS-FONT-LIST (NCONS FONT-DESC))))
    (SETQ FONT-NUMBER (FIND-POSITION-IN-LIST-EQUAL FONT-DESC PRESS-FONT-LIST))
    (AND ( FONT-NUMBER 16.) (FERROR NIL "Maximum of 16 fonts allowed."))
    FONT-NUMBER))

;Similar to above, but works when there is no Fonts Widths data.  The
;assumed size is completely bogus, don't format lines depending on it.
;Actually, I use the size data from TIMESROMAN18.
;Second value is T if font not found in Fonts Widths.
(DEFUN PRESS-DEFINE-FONT-FAKE (FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
  (IF (ERRSET (FIND-FONT-DATA FAMILY-NAME FACE-NAME POINT-SIZE) NIL)
      (PRESS-DEFINE-FONT FAMILY-NAME FACE-NAME POINT-SIZE ROTATION)
      (LET ((WIDTH 633.) (HEIGHT 698.) WIDTH-ARRAY FONT-DESC FONT-NUMBER)
	(SETQ WIDTH-ARRAY (MAKE-ARRAY NIL 'ART-16B 400))
	(FILLARRAY WIDTH-ARRAY '(633.))
	(SETQ FONT-DESC (LIST FAMILY-NAME FACE-NAME POINT-SIZE ROTATION
			      WIDTH HEIGHT WIDTH-ARRAY))
	(OR (MEMBER FONT-DESC PRESS-FONT-LIST)
	    (SETQ PRESS-FONT-LIST (NCONC PRESS-FONT-LIST (NCONS FONT-DESC))))
	(SETQ FONT-NUMBER (FIND-POSITION-IN-LIST-EQUAL FONT-DESC PRESS-FONT-LIST))
	(AND ( FONT-NUMBER 16.) (FERROR NIL "Maximum of 16 fonts allowed."))
	(PROG () (RETURN FONT-NUMBER T)))))

;Select a font, by number
(DEFUN PRESS-SELECT-FONT (FONT-NUMBER)
  (PRESS-PUT-PENDING-CHARS)
  (OR (SETQ PRESS-CURRENT-FONT (NTH FONT-NUMBER PRESS-FONT-LIST))
      (FERROR NIL "Font number ~D not defined" FONT-NUMBER))
  (PRESS-ENTITY-BYTE (+ 160 FONT-NUMBER)))

;Single-letters in the string select features as follows:
; B bold, L light
; I italic
; C condensed
; E expanded
(DEFUN ENCODE-PRESS-FACE (STR)
  (SETQ STR (STRING STR))
  (DO ((FACE-CODE 0)
       (I 0 (1+ I))
       (N (STRING-LENGTH STR))
       (CH))
      ((= I N) FACE-CODE)
    (SETQ CH (CHAR-UPCASE (AREF STR I)))
    (SETQ FACE-CODE (+ FACE-CODE
		       (SELECTQ CH
			 (#/B 2)
			 (#/L 4)
			 (#/I 1)
			 (#/C 6)
			 (#/E 12.)
			 (OTHERWISE (FERROR NIL "~C illegal character in face name /"~A/""
					    CH STR)))))))


(DEFUN PRESS-ENTITY-BCPL-STRING (STRING NBYTES &AUX REAL-LENGTH)
  (PRESS-ENTITY-BYTE (SETQ REAL-LENGTH (MIN (STRING-LENGTH STRING) (1- NBYTES))))
  (DOTIMES (I REAL-LENGTH)
    (PRESS-ENTITY-BYTE (AREF STRING I)))
  (DOTIMES (I (- NBYTES (1+ REAL-LENGTH)))
    (PRESS-ENTITY-BYTE 0)))


(DEFVAR NEWVEC-SLOPE-TABLE)
(DEFVAR NEWVEC-DX-TABLE)
(DEFVAR NEWVEC-DY-TABLE)

(DEFUN MAKE-NEWVEC-TABLES ()
  (DO ((TBL (MAKE-ARRAY NIL 'ART-Q 101))
       (XTBL (MAKE-ARRAY NIL 'ART-Q 101))
       (YTBL (MAKE-ARRAY NIL 'ART-Q 101))
       (BITS-TO-MICAS (// 2540.0s0 384.))
       (I 0 (1+ I))
       (DX 0)
       (DY 16.))
      ((= I 101)
       (SETQ NEWVEC-SLOPE-TABLE TBL
	     NEWVEC-DX-TABLE XTBL
	     NEWVEC-DY-TABLE YTBL))
    (ASET (COND ((= I 0) 1s18)
		((= I 100) -1s18)
		(T (// (SMALL-FLOAT DY) DX))) TBL I)
    (ASET (* DX BITS-TO-MICAS) XTBL I)
    (ASET (* DY BITS-TO-MICAS) YTBL I)
    (COND ((< I 20) (SETQ DX (1+ DX)))
	  ((< I 60) (SETQ DY (1- DY)))
	  (T (SETQ DX (1- DX))))))

(MAKE-NEWVEC-TABLES)

;;;;Draw a line, using rectangles for straight lines and font for diagonal lines.
;;;    Coordinates in micas of course.

(DEFUN PRESS-LINE (X0 Y0 X1 Y1 &AUX (DX (ABS (- X0 X1))) (DY (ABS (- Y0 Y1))) FONT-NUMBER)
  (PRESS-PUT-PENDING-CHARS)
  (PRESS-MAYBE-NEW-ENTITY)			;This should make DPLT work better
  (COND ((= X0 X1)				;Vertical line
	 (PRESS-SET-CURSOR (- X0 (// LINE-WIDTH 2)) (MIN Y0 Y1))	;Lower left corner
	 (PRESS-SHOW-RECT LINE-WIDTH DY))
	((= Y0 Y1)				;Horizontal line
	 (PRESS-SET-CURSOR (MIN X0 X1) (- Y0 (// LINE-WIDTH 2)))	;Lower left corner
	 (PRESS-SHOW-RECT DX LINE-WIDTH))
	(T					;Diagonal line, use the font
	 (OR (MEMQ PRESS-LINE-FONT PRESS-FONT-LIST)
	     (SETQ PRESS-FONT-LIST (NCONC PRESS-FONT-LIST (NCONS PRESS-LINE-FONT))))
	 (SETQ FONT-NUMBER (FIND-POSITION-IN-LIST PRESS-LINE-FONT PRESS-FONT-LIST))
	 (AND ( FONT-NUMBER 16.) (FERROR NIL "Maximum of 16 fonts allowed."))
	 (OR (EQ PRESS-CURRENT-FONT PRESS-LINE-FONT)
	     (PRESS-SELECT-FONT FONT-NUMBER))
	 (IF (< X1 X0) (PSETQ X0 X1 Y0 Y1 X1 X0 Y1 Y0))	;(X0,Y0) are left end
	 (PRESS-SET-CURSOR X0 Y0)		;Proceed inevitably toward the right
	 (AND (< Y1 Y0) (SETQ DY (- DY)))
	 ;; Always use 2 characters of the largest size except for finishing up
	 (DO ((CH2 1 (1+ CH2))
	      (CH1 0 CH2)
	      (SLOPE (// (SMALL-FLOAT DY) DX)))
	     ((OR (= CH2 100)
		  (< (AREF NEWVEC-SLOPE-TABLE CH2) SLOPE))
	      (DO ((X X0 (+ X XINC))
		   (Y Y0 (+ Y YINC))
		   (CH) (XINC) (YINC) (STOP NIL)
		   (CDX1 (AREF NEWVEC-DX-TABLE CH1))
		   (CDY1 (AREF NEWVEC-DY-TABLE CH1))
		   (CDX2 (AREF NEWVEC-DX-TABLE CH2))
		   (CDY2 (AREF NEWVEC-DY-TABLE CH2)))
		  ((OR ( X X1) STOP))
		;; If Y would be below the line, use CH1 else use CH2
		(IF (< (// (SMALL-FLOAT (- (+ Y CDY2) Y0)) (- (+ X CDX2) X0)) SLOPE)
		    (SETQ CH CH1 XINC CDX1 YINC CDY1)
		    (SETQ CH CH2 XINC CDX2 YINC CDY2))
		;; If getting too close to the endpoint, use a shorter line
		(DO ((STRTL '(0 120 170 214 226) (CDR STRTL))
		     (I CH)
		     (D 2 (* D 2)))
		    ((OR ( (+ X XINC) X1) (SETQ STOP (NULL (CDR STRTL)))))
		  (SETQ CH (+ (// (- CH (CAR STRTL)) 2) (CADR STRTL))
			I (* (// I 2) 2)
			XINC (// (AREF NEWVEC-DX-TABLE I) D)
			YINC (// (AREF NEWVEC-DY-TABLE I) D)))
		(FUNCALL PRESS-EFTP-STREAM ':TYO CH)
		(SETQ PRESS-N-CHARS (1+ PRESS-N-CHARS))
		(SETQ PRESS-PENDING-CHARS (1+ PRESS-PENDING-CHARS))))))))

;Subroutine for the above
(DEFUN PRESS-SHOW-RECT (WIDTH HEIGHT)
  (PRESS-ENTITY-BYTE 376)
  (PRESS-ENTITY-WORD WIDTH)
  (PRESS-ENTITY-WORD HEIGHT))

;;;; Output a BITMAP off a window

;;; Output bitmap as an entity on the current page
;;;    assumes page is open but not an entity
;;; This uses pixel array because the halfword array requires a bitreverse
;;;      due to the DOVERs inflexibility
;;; U, V define the lower left edge of bitmap on page                [micas]
;;; SX0, SY0, SX1, SY1 define the left, top, right, bottom of screen [pixels]

(DEFUN PRESS-TV-BITMAP (U V &OPTIONAL (WINDOW TV:DEFAULT-SCREEN)
				      (SX0 0) (SY0 0) (SX1 (TV:SHEET-WIDTH WINDOW))
				      (SY1 (TV:SHEET-HEIGHT WINDOW)))
  (IF (> SX0 SX1) (PSETQ SX0 SX1 SX1 SX0))
  (IF (> SY0 SY1) (PSETQ SY0 SY1 SY1 SY0))
  (LET ((ARRAY  (TV:SHEET-SCREEN-ARRAY WINDOW))
	(START  0)
	(LINES  (LOGAND (+ 15. (- SY1 SY0)) 1760))
	(DOTS   (LOGAND (+ 15. (- SX1 SX0)) 1760))
	(SIZE   32.)				; a dot is 32 x 32 micas
	(WIDTH  0)
	(HEIGHT 0))
    (SETQ WIDTH  (* DOTS  SIZE)
	  HEIGHT (* LINES SIZE))
    (PRESS-OPEN-ENTITY)
    (PRESS-PUT-PENDING-CHARS)
    (PRESS-SET-CURSOR U V)
    (IF (ODDP PRESS-N-CHARS)
	(PROGN (PRESS-ENTITY-BYTE <SKIP-CHARACTERS-SHORT>)
	       (PRESS-DATA-BYTE 0)))
    (SETQ START PRESS-N-CHARS)			; remember where we are in DL
    (PROGN (PRESS-DATA-BYTE <<SET-CODING>>)
	   (PRESS-DATA-BYTE 0)
	   (PRESS-DATA-WORD DOTS)
	   (PRESS-DATA-WORD LINES))
    (PROGN (PRESS-DATA-BYTE <<SET-MODE>>)
	   (PRESS-DATA-BYTE 3.))		; Dover Requires this direction
    (PROGN (PRESS-DATA-WORD <<SET-SIZE>>)
	   (PRESS-DATA-WORD WIDTH)
	   (PRESS-DATA-WORD HEIGHT))
    (PROGN (PRESS-DATA-WORD <<DOTS-FOLLOW>>)
	   (DO ((Y      SY0 (1+ Y))
		(LAST-Y (+ LINES SY0)))
	       ((>= Y LAST-Y))
	     (DO ((X      SX0 (+ X 16.))
		  (LAST-X (+ DOTS SX0)))
		 ((>= X LAST-X))
	       (PRESS-DATA-WORD
		 (DO ((I 0 (1+ I))
		      (R 0))
		     ((>= I 16.) R)
		   (SETQ R (+ (LSH R 1) (AREF ARRAY (+ (MIN X SX1) I) (MIN Y SY1)))))))))
    (PROGN (PRESS-ENTITY-BYTE   <SHOW-DOTS>)
	   (PRESS-ENTITY-32WORD (// (- PRESS-N-CHARS START) 2)))
    (PRESS-CLOSE-ENTITY U V WIDTH HEIGHT)
    ))


;;;; Print a file

(DEFUN PRINT-FILE (FILE-NAME &OPTIONAL (FONT-NAME "TIMESROMAN")
			     (FACE-NAME "")
			     (FONT-SIZE 10.)
			     (PAGE-HEADINGS T)
			     (N-COPIES 1)
			     (SEND-TO-MOONS-ALTO-P NIL)
			     (HOST-ADDRESS DOVER-ADDRESS)
			     &AUX INPUT-STREAM (PRESS-USE-EFTP PRESS-USE-EFTP))
  (AND SEND-TO-MOONS-ALTO-P (SETQ PRESS-USE-EFTP T))
  (UNWIND-PROTECT 
    (LOCAL-DECLARE ((SPECIAL FILE-NAME CREATION-DATE))
      (SETQ INPUT-STREAM (OPEN FILE-NAME ':READ))
      (LET ((FILE-NAME (FUNCALL INPUT-STREAM ':GET ':UNIQUE-ID))
	    (CREATION-DATE (FORMAT NIL "~A ~A" (FUNCALL INPUT-STREAM ':GET ':CREATION-DATE)
				   (FUNCALL INPUT-STREAM ':GET ':CREATION-TIME)))
	    (PRESS-END-PAGE-HOOK
	      (IF PAGE-HEADINGS
		  #'(LAMBDA ()
		      (FORMAT T " page ~D " PRESS-PAGE-NUMBER)
		      (PRESS-SET-CURSOR 0 DOVER-Y0)
		      (PRESS-STRING (FORMAT NIL "~A~10X~A" FILE-NAME CREATION-DATE))
		      (PRESS-SET-CURSOR 15000. DOVER-Y0)
		      (PRESS-STRING (FORMAT NIL "Page ~D" PRESS-PAGE-NUMBER)))
		  #'(LAMBDA ()
		      (FORMAT T " page ~D " PRESS-PAGE-NUMBER)))))
	(OR SEND-TO-MOONS-ALTO-P (STRINGP HOST-ADDRESS) (PRINT-DOVER-STATUS))
	(PRESS-START-FILE (IF SEND-TO-MOONS-ALTO-P 926-ALTO-ADDRESS HOST-ADDRESS))
	(PRESS-START-PAGE)
	(PRESS-SELECT-FONT (PRESS-DEFINE-FONT FONT-NAME FACE-NAME FONT-SIZE 0))
	(DO ((CH (FUNCALL INPUT-STREAM ':TYI) (FUNCALL INPUT-STREAM ':TYI)))
	    ((NULL CH))
	  (PRESS-CHAR CH))
	(PRESS-END-PAGE)
	(PRESS-END-FILE FILE-NAME CREATION-DATE N-COPIES)))
    (CLOSE INPUT-STREAM)))

;Spool a file via MC's spooler
(DEFUN SPOOL-FILE (FILE-NAME &OPTIONAL (FONT-NAME "TIMESROMAN")
			     (FACE-NAME "")
			     (FONT-SIZE 10.)
			     (PAGE-HEADINGS T)
			     (N-COPIES 1)
			     (FILE (FORMAT NIL "MC:.DOVR.;~A >" USER-ID)))
  (PRINT-FILE FILE-NAME FONT-NAME FACE-NAME FONT-SIZE PAGE-HEADINGS N-COPIES NIL FILE))


;;;;Font sampling

;Each element in font-list is (family-name face-name point-size rotation)
; rotation is optional and defaults to 0
(DEFUN SAMPLE-FONTS (FONT-LIST &OPTIONAL (UPPER-HALF NIL)
					 (SEND-TO-MOONS-ALTO-P NIL)
					 (HOST-ADDRESS DOVER-ADDRESS)
			       &AUX (PRESS-USE-EFTP PRESS-USE-EFTP) FOO CH)
  (AND SEND-TO-MOONS-ALTO-P (SETQ PRESS-USE-EFTP T))
  (OR SEND-TO-MOONS-ALTO-P (STRINGP HOST-ADDRESS) (PRINT-DOVER-STATUS))
  (PRESS-START-FILE (IF SEND-TO-MOONS-ALTO-P 926-ALTO-ADDRESS HOST-ADDRESS))
  (LET ((LABEL-FONT (PRESS-DEFINE-FONT "TIMESROMAN" "" 10. 0)) THIS-FONT NOT-IN-FONTS-WIDTHS)
    (DO ((L FONT-LIST (CDR L))
	 (FONT) (ROTATION)
	 (I 1 (1+ I)))
	((NULL L)
	 (PRESS-END-FILE "Font samples" ""))
      (SETQ FONT (CAR L))
      (COND ((= I 16.)				;Got to make a new file
	     (PRESS-END-FILE "Font samples" "")
	     (RETURN (SAMPLE-FONTS L UPPER-HALF SEND-TO-MOONS-ALTO-P))))
      (PRESS-START-PAGE)
      (MULTIPLE-VALUE (THIS-FONT NOT-IN-FONTS-WIDTHS)
	  (PRESS-DEFINE-FONT-FAKE (CAR FONT) (CADR FONT) (CADDR FONT)
				  (SETQ ROTATION (OR (CADDDR FONT) 0))))
      (PRESS-SET-CURSOR 6500. 25400.)
      (PRESS-SELECT-FONT LABEL-FONT)
      (PRESS-STRING
	(FORMAT NIL "Font ~A, ~:[Face ~A, ~;~*~]Point size ~D~:[, rotated ~D degrees~;~*~]~:[~; (not in Fonts.Widths)~]"
		    (CAR FONT) (STRING-EQUAL (CADR FONT) "") (CADR FONT)
		    (CADDR FONT) (ZEROP ROTATION) (// ROTATION 60.)
		    NOT-IN-FONTS-WIDTHS))
      (DOTIMES (COL 10)
	(DOTIMES (ROW 20)
	  (PRESS-SET-CURSOR (+ (* COL 2200.) 300.) (- (* 10. 2540.) 1250. (* ROW 1000.)))
	  (PRESS-SELECT-FONT LABEL-FONT)
	  (SETQ CH (+ (IF UPPER-HALF 200 0) (* COL 20) ROW))
	  (DO PPSS 0603 (- PPSS 0300) (MINUSP PPSS)
	      (PRESS-CHAR (+ (LDB PPSS CH) #/0)))
	  (PRESS-STRING "   ")
	  (PRESS-SELECT-FONT THIS-FONT)
	  ;; See if char defined in font
	  (COND ((MINUSP (AREF (SEVENTH PRESS-CURRENT-FONT) CH))
		 (PRESS-SELECT-FONT LABEL-FONT)
		 (PRESS-STRING "und"))
		(T (FUNCALL PRESS-EFTP-STREAM ':TYO CH)
		   (SETQ PRESS-N-CHARS (1+ PRESS-N-CHARS))
		   (SETQ PRESS-PENDING-CHARS (1+ PRESS-PENDING-CHARS))))))
      ;8150. next
      (PRESS-SELECT-FONT THIS-FONT)
      (PRESS-SET-CURSOR 0 8000.)
      (PRESS-CHAR-SEQ #/A #/Z #\CR)
      (PRESS-CHAR-SEQ #/a #/z #\CR)
      (PRESS-CHAR-SEQ #/0 #/9 #\CR)
      (PRESS-CHAR-SEQ #/! #/?)
      (PRESS-CHAR-SEQ #/[ #/_)
      (PRESS-CHAR-SEQ #/{ #/‡ #\CR)
      (PRESS-CHAR-SEQ #/  #/)
      (PRESS-SET-CURSOR 0 4150.)
      (PRESS-STRING "/"The time has come,/" the Walrus said,
   /"To talk of many things;
Of shoes, and ships, and sealing wax,
")
      (SETQ FOO PRESS-Y)
      (PRESS-STRING "   Of cabbages and kings,
And why the sea is boiling hot,
   And whether pigs have wings./"")
      (PRESS-SET-CURSOR 8750. FOO)
      (PRESS-STRING "(DEFUN APPEND (X Y)
")
      (PRESS-SET-CURSOR 8750. PRESS-Y)
      (PRESS-STRING "       (COND ((NULL X) Y)
")
      (PRESS-SET-CURSOR 8750. PRESS-Y)
      (PRESS-STRING "             (T (CONS (CAR X) (APPEND (CDR X) Y)))))")
      (PRESS-END-PAGE))))

(DEFUN PRESS-CHAR-SEQ (FIRST LAST &OPTIONAL EXTRA)
  (DO CH FIRST (1+ CH) (> CH LAST)
    (OR (MINUSP (AREF (SEVENTH PRESS-CURRENT-FONT) CH)) (PRESS-CHAR CH)))
  (AND EXTRA (PRESS-CHAR EXTRA)))

;This one is driven off of the widths file, assumed to be already loaded
(DEFUN SAMPLE-ALL-FONTS (&AUX FONT-LIST NAME FACE POINTS ROT)
  (DOLIST (F FONT-WIDTH-DATA)
    (SETQ NAME (CAR F) FACE (CADR F) POINTS (CADDR F) ROT (CADDDR F))
    (COND ((PLUSP POINTS) (PUSH (LIST NAME FACE (// (* 10. POINTS) 352.) ROT) FONT-LIST))
	  (T (FORMAT T "~&Type list of point sizes for ~A~Arot~D: "
		     NAME FACE (// ROT 60.))
	     (DOLIST (POINTS (READ))
	       (PUSH (LIST NAME FACE POINTS ROT) FONT-LIST)))))
  (SAMPLE-FONTS FONT-LIST))

;;;; List of all fonts on MIT Dover as 12/21/79 [also there are rotated fonts]

(SETQ ALL-DOVER-FONTS '(
  (TIMESROMAN || 6.) (TIMESROMAN || 7.) (TIMESROMAN || 8.)
  (TIMESROMAN || 10.) (TIMESROMAN || 12.) (TIMESROMAN || 18.)

  (TIMESROMAN B 6.) (TIMESROMAN B 7.) (TIMESROMAN B 8.)
  (TIMESROMAN B 10.) (TIMESROMAN B 12.) (TIMESROMAN B 18.)

  (TIMESROMAN I 6.) (TIMESROMAN I 7.) (TIMESROMAN I 8.)
  (TIMESROMAN I 10.) (TIMESROMAN I 12.)

  (TIMESROMAN BI 6.) (TIMESROMAN BI 7.) (TIMESROMAN BI 8.)
  (TIMESROMAN BI 10.) (TIMESROMAN BI 12.)

  (HELVETICA || 6.) (HELVETICA || 7.) (HELVETICA || 8.)
  (HELVETICA || 10.)  (HELVETICA || 12.) (HELVETICA || 18.)

  (HELVETICA B 6.) (HELVETICA B 7.) (HELVETICA B 8.)
  (HELVETICA B 10.) (HELVETICA B 12.) (HELVETICA B 18.)

  (HELVETICA BI 6.) (HELVETICA BI 7.) (HELVETICA BI 8.)
  (HELVETICA BI 10.) (HELVETICA BI 12.)

  (HELVETICA I 6.) (HELVETICA I 7.) (HELVETICA I 8.)
  (HELVETICA I 10.) (HELVETICA I 12.)

  (MATH || 6) (MATH || 8.) (MATH || 10.)

  (CREAM || 10.)
  (CREAM I 10.)
  (CREAM B 10.)

  (GACHA || 8.)
  (GACHA || 10.)

  (HIPPO || 8.)
  (HIPPO || 10.)

  (ELITE || 10.)

  (ARROWS || 10.)

  (DOTS || 7.)

  (GATES || 32.)

  (TEMPLATE || 64.) ))

(COMMENT ;hacks

(DEFUN SPIRAL-HACK ()
  (PRESS-START-FILE DOVER-ADDRESS)
  (PRESS-START-PAGE)
  (PRESS-SELECT-FONT (PRESS-DEFINE-FONT "timesroman" "" 10. 0))
  (DO ((X 8000.) (Y 8000.) (ANG 0 (+ ANG ANGINC)) (ANGINC 0.3)
       (LEN 200. (+ LEN LENINC)) (LENINC 50.) (MAXLEN 1000.))
      ((> LEN MAXLEN))
    (PRESS-LINE X Y (SETQ X (+ X (FIX (* (SIN ANG) LEN))))
		    (SETQ Y (+ Y (FIX (* (COS ANG) LEN))))))
  (PRESS-END-PAGE)
  (PRESS-END-FILE "Spiral" ""))

(DEFUN FOO (POLY ADDRESS)
  (PRESS-START-FILE ADDRESS)
  (PRESS-START-PAGE)
  (PRESS-SELECT-FONT (PRESS-DEFINE-FONT "timesroman" "" 10. 0))
  (DO ((X (CAAR POLY))
       (Y (CADAR POLY))
       (L (CDR POLY) (CDR L)))
      ((NULL L))
    (PRESS-LINE X Y (SETQ X (CAAR L)) (SETQ Y (CADAR L))))
  (PRESS-END-PAGE)
  (PRESS-END-FILE "Lines" ""))
);comment
