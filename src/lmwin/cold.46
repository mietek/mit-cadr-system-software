;;; -*- Mode:LISP; Package:SI; Base:8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains those portions of the window system that need
;;; to be in the cold-load, including the basic keyboard software and
;;; the cold load stream.

;;; Note that this file has to be in the SYSTEM-INTERNALS (SI) package
;;; rather than TV because it is part of the cold-load.

TV:(PROGN 'COMPILE
(DEFVAR DEFAULT-BACKGROUND-STREAM 'TV:BACKGROUND-STREAM)
(DEFVAR WHO-LINE-RUN-LIGHT-LOC 51765)	;Where the run-light goes, in Xbus I/O space
(DEFVAR KBD-LAST-ACTIVITY-TIME 0)	;Time user last typed a key or clicked mouse
(DEFVAR ALU-SETA #.ALU-SETA)
(DEFVAR ALU-XOR #.ALU-XOR)
(DEFVAR ALU-ANDCA #.ALU-ANDCA)
(DEFVAR ALU-IOR #.ALU-IOR)
(DEFVAR ALU-SETZ #.ALU-SETZ)
);TV:

;;; Call this when the state of a process may have changed.
;;; In the cold-load because called by process stuff, loaded before window stuff.
TV:
(DEFUN WHO-LINE-PROCESS-CHANGE (PROC)
  (AND (FBOUNDP 'WHO-LINE-RUN-STATE-UPDATE) (EQ PROC LAST-WHO-LINE-PROCESS)
       (WHO-LINE-RUN-STATE-UPDATE)))

;;; Copy from LISPM2;FLAVOR, needed for setting up the sync program
(DEFUN SYMEVAL-IN-INSTANCE (INSTANCE PTR &OPTIONAL NO-ERROR-P)
  (AND (SYMBOLP PTR) (SETQ PTR (VALUE-CELL-LOCATION PTR)))
  (LET ((N (FIND-POSITION-IN-LIST PTR (%P-CONTENTS-OFFSET
					(%P-CONTENTS-AS-LOCATIVE-OFFSET INSTANCE 0)
					%INSTANCE-DESCRIPTOR-BINDINGS))))
    (COND (N (%INSTANCE-REF INSTANCE (1+ N)))
	  (NO-ERROR-P NIL)
	  (T
	   (FERROR NIL "The variable ~S is not an instance variable of ~S"
		   (%FIND-STRUCTURE-HEADER PTR) INSTANCE)))))

;;; Macros for cold-load construction

(DEFMACRO DEFINSTANCE-IMMEDIATE (NAME . INSTANCE-VARIABLES)
  (LET ((INSTANCE-VARIABLE-LIST-NAME (INTERN (STRING-APPEND NAME "-INSTANCE-VARIABLES")))
	(METHOD-LIST-NAME (INTERN (STRING-APPEND NAME "-WHICH-OPERATIONS"))))
    (SET METHOD-LIST-NAME NIL)
    (SET INSTANCE-VARIABLE-LIST-NAME INSTANCE-VARIABLES)
    `(PROGN 'COMPILE
       (DEFPROP ,NAME T SI:FLAVOR)		;This makes M-. work
       (DEFVAR ,NAME))))

(DEFMACRO DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES ((NAME) . BODY)
  (LET ((INSTANCE-VARIABLE-LIST-NAME (INTERN (STRING-APPEND NAME "-INSTANCE-VARIABLES"))))
    `(LOCAL-DECLARE ((SPECIAL . ,(SYMEVAL INSTANCE-VARIABLE-LIST-NAME)))
       . ,BODY)))

(DEFMACRO DEFMETHOD-IMMEDIATE ((NAME MESSAGE) ARGLIST . BODY)
  (LET ((METHOD-LIST-NAME (INTERN (STRING-APPEND NAME "-WHICH-OPERATIONS")))
	(METHOD-NAME (INTERN (STRING-APPEND NAME #/- MESSAGE "-METHOD"))))
    (OR (MEMQ MESSAGE (SYMEVAL METHOD-LIST-NAME))
	(PUSH MESSAGE (SYMEVAL METHOD-LIST-NAME)))
    `(DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES (,NAME)
     (DEFUN ,METHOD-NAME (IGNORE . ,ARGLIST)
       . ,BODY))))

(DEFMACRO MAKE-INSTANCE-IMMEDIATE (NAME &REST INIT-PLIST)
  (LET ((INSTANCE-VARIABLE-LIST-NAME (INTERN (STRING-APPEND NAME "-INSTANCE-VARIABLES")))
	(METHOD-LIST-NAME (INTERN (STRING-APPEND NAME "-WHICH-OPERATIONS")))
	(METHOD-LIST 'SI:UNCLAIMED-MESSAGE))
    (DOLIST (MESSAGE (SYMEVAL METHOD-LIST-NAME))
      (LET ((METHOD-NAME (INTERN (STRING-APPEND NAME #/- MESSAGE "-METHOD"))))
	(PUSH (CONS MESSAGE METHOD-NAME) METHOD-LIST)))
    (PUSH (CONS ':WHICH-OPERATIONS METHOD-LIST-NAME) METHOD-LIST)
    `(PROGN 'COMPILE
       (DEFUN ,METHOD-LIST-NAME (IGNORE)
	 ',(SYMEVAL METHOD-LIST-NAME))
       (SETQ ,NAME (FAKE-UP-INSTANCE ',NAME ',(SYMEVAL INSTANCE-VARIABLE-LIST-NAME)
				     ',METHOD-LIST ',INIT-PLIST)))))

(DEFINSTANCE-IMMEDIATE COLD-LOAD-STREAM
  ARRAY						;The array into which bits go
  LOCATIONS-PER-LINE				;Number of words in a screen line
  HEIGHT					;Height of screen
  WIDTH						;Width of screen
  CURSOR-X					;Current x position
  CURSOR-Y					;Current y position
  FONT						;The one and only font
  CHAR-WIDTH					;Width of a character
  LINE-HEIGHT					;Height of line, including vsp
  BUFFER					;The hardward buffer location
  TV:CONTROL-ADDRESS				;Hardware controller address
  UNRCHF					;For :UNTYI
  RUBOUT-HANDLER-BUFFER				;For :RUBOUT-HANDLER
  )

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :PRINT) (STREAM &REST IGNORE)
  (FORMAT STREAM "#<~A ~O>" (TYPEP SELF) (%POINTER SELF)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :INIT) (PLIST)
  (OR (BOUNDP 'KBD-TRANSLATE-TABLE)
      (KBD-INITIALIZE))
  (OR (BOUNDP 'TV:DEFAULT-SCREEN)
      (SETQ TV:DEFAULT-SCREEN SELF))
  (SETQ CURSOR-X 0 CURSOR-Y 0
	FONT (OR (GET PLIST ':FONT) FONTS:CPTFONT)
	UNRCHF NIL
	WIDTH (GET PLIST ':WIDTH)
	HEIGHT (GET PLIST ':HEIGHT)
	BUFFER (GET PLIST ':BUFFER)
	TV:CONTROL-ADDRESS (GET PLIST ':CONTROL-ADDRESS)
	ARRAY (MAKE-ARRAY NIL 'ART-1B (LIST WIDTH HEIGHT) BUFFER)
	LOCATIONS-PER-LINE (// WIDTH 32.)
	CHAR-WIDTH (FONT-CHAR-WIDTH FONT)
	LINE-HEIGHT (+ 2 (FONT-CHAR-HEIGHT FONT))
	RUBOUT-HANDLER-BUFFER (MAKE-ARRAY NIL ART-STRING 1000 NIL '(0 0))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :READ-CURSORPOS) (&OPTIONAL (UNITS ':PIXEL)
					       &AUX (X CURSOR-X) (Y CURSOR-Y))
  (AND (EQ UNITS ':CHARACTER)
       (SETQ X (// X CHAR-WIDTH)
	     Y (// Y LINE-HEIGHT)))
  (PROG () (RETURN X Y)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :SET-CURSORPOS) (X Y &OPTIONAL (UNITS ':PIXEL))
  (AND (NUMBERP UNITS)				;***CROCK***, flush when format fixed
       (PSETQ UNITS X X Y Y UNITS))
  (AND (EQ UNITS ':CHARACTER)
       (SETQ X (* X CHAR-WIDTH)
	     Y (* Y LINE-HEIGHT)))
  (SETQ CURSOR-X (MAX 0 (MIN WIDTH X))
	CURSOR-Y (MAX 0 (MIN HEIGHT Y))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :HOME-CURSOR) ()
  (SETQ CURSOR-X 0 CURSOR-Y 0))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :HANDLE-EXCEPTIONS) ())

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :TYO) (CH)
  (COND ((< CH 200)
	 (LET ((CHAR-WIDTHS (FONT-CHAR-WIDTH-TABLE FONT))
	       (FIT-ENTRY (FONT-INDEXING-TABLE FONT))
	       (DELTA-X))
	   (SETQ DELTA-X (IF CHAR-WIDTHS (AREF CHAR-WIDTHS CH) (FONT-CHAR-WIDTH FONT)))
	   (AND (> (+ CURSOR-X DELTA-X) WIDTH)	;End of line exception
		(FUNCALL-SELF ':TYO #\CR))
	   (IF (NULL FIT-ENTRY)
	       (%DRAW-CHAR FONT CH CURSOR-X CURSOR-Y TV:ALU-IOR SELF)
	       (DO ((CH (AREF FIT-ENTRY CH) (1+ CH))
		    (LIM (AREF FIT-ENTRY (1+ CH)))
		    (XPOS CURSOR-X (+ XPOS (FONT-RASTER-WIDTH FONT))))
		   ((= CH LIM))
		 (%DRAW-CHAR FONT CH XPOS CURSOR-Y TV:ALU-IOR SELF)))
	   (SETQ CURSOR-X (+ CURSOR-X DELTA-X))))
	((= CH #\CR)
	 (SETQ CURSOR-X 0
	       CURSOR-Y (+ CURSOR-Y LINE-HEIGHT))
	 (COND (( CURSOR-Y (- HEIGHT LINE-HEIGHT))	;End of page exception
		(FUNCALL-SELF ':STRING-OUT "**MORE**")
		(FUNCALL-SELF ':TYI)
		(SETQ CURSOR-X 0)
		(FUNCALL-SELF ':CLEAR-EOL)
		(SETQ CURSOR-Y 0)))
	 (FUNCALL-SELF ':CLEAR-EOL))
	((= CH #\TAB)
	 (DOTIMES (I (- 8 (\ (// CURSOR-X CHAR-WIDTH) 8)))
	   (FUNCALL-SELF ':TYO #\SP)))
	((AND (< CH 240) (BOUNDP 'FONTS:5X5))
	 ;; This won't work in the initial cold-load environment, hopefully no one
	 ;; will touch those keys then, but if they do we just type nothing.
	 ;; This code is like SHEET-DISPLAY-LOSENGED-STRING
	 (LET* ((CHNAME (GET-PNAME (CAR (RASSOC CH XR-SPECIAL-CHARACTER-NAMES))))
		(CHWIDTH (+ (* (ARRAY-ACTIVE-LENGTH CHNAME) 6) 10.)))
	   (AND (> (+ CURSOR-X CHWIDTH) WIDTH)	;Won't fit on line
		(FUNCALL-SELF ':TYO #\CR))
	   ;; Put the string then the box around it
	   (LET ((X0 CURSOR-X)
		 (Y0 (1+ CURSOR-Y))
		 (X1 (+ CURSOR-X (1- CHWIDTH)))
		 (Y1 (+ CURSOR-Y 9)))
	     (DO ((X (+ X0 5) (+ X 6))
		  (I 0 (1+ I))
		  (N (ARRAY-ACTIVE-LENGTH CHNAME)))
		 (( I N))
	       (%DRAW-CHAR FONTS:5X5 (AREF CHNAME I) X (+ Y0 2) TV:ALU-IOR SELF))
	     (%DRAW-RECTANGLE (- CHWIDTH 8) 1 (+ X0 4) Y0 TV:ALU-IOR SELF)
	     (%DRAW-RECTANGLE (- CHWIDTH 8) 1 (+ X0 4) Y1 TV:ALU-IOR SELF)
	     (%DRAW-LINE X0 (+ Y0 4) (+ X0 3) (1+ Y0) TV:ALU-IOR T SELF)
	     (%DRAW-LINE (1+ X0) (+ Y0 5) (+ X0 3) (1- Y1) TV:ALU-IOR T SELF)
	     (%DRAW-LINE X1 (+ Y0 4) (- X1 3) (1+ Y0) TV:ALU-IOR T SELF)
	     (%DRAW-LINE (1- X1) (+ Y0 5) (- X1 3) (1- Y1) TV:ALU-IOR T SELF)
	     (SETQ CURSOR-X (1+ X1))))))
  CH)

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-EOL) ()
  (%DRAW-RECTANGLE (- WIDTH CURSOR-X) LINE-HEIGHT CURSOR-X CURSOR-Y TV:ALU-ANDCA SELF))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-SCREEN) ()
  (SETQ CURSOR-X 0 CURSOR-Y 0)
  (%DRAW-RECTANGLE WIDTH HEIGHT 0 0 TV:ALU-ANDCA SELF))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :FRESH-LINE) ()
  (OR (ZEROP CURSOR-X) (FUNCALL-SELF ':TYO #\CR)))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :STRING-OUT) (STRING &OPTIONAL (START 0) END)
  (DO ((I START (1+ I))
       (END (OR END (ARRAY-ACTIVE-LENGTH STRING))))
      (( I END))
    (FUNCALL-SELF ':TYO (AREF STRING I))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :LINE-OUT) (STRING &OPTIONAL (START 0) END)
  (FUNCALL-SELF ':STRING-OUT STRING START END)
  (FUNCALL-SELF ':TYO #\CR))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :UNTYI) (CH)
  (SETQ UNRCHF CH))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :LISTEN) ()
  (OR UNRCHF
      (DO () ((NOT (KBD-HARDWARE-CHAR-AVAILABLE)) NIL)
	(AND (SETQ UNRCHF (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR)))
	     (RETURN T)))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :TYI) (&AUX IDX (INHIBIT-SCHEDULING-FLAG T))
  (COND (UNRCHF
	 (PROG1 UNRCHF (SETQ UNRCHF NIL)))
	((NOT RUBOUT-HANDLER)
	 (DO () (())
	   (COLD-LOAD-STREAM-WAIT-FOR-CHAR)
	   (LET ((CHAR (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR))))
	     (SELECTQ CHAR
	       (NIL)				;Unreal character
	       (#\BREAK (BREAK T))
	       (OTHERWISE (RETURN CHAR))))))
	((> (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0)
	    (SETQ IDX (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 1)))
	 (STORE-ARRAY-LEADER (1+ IDX) RUBOUT-HANDLER-BUFFER 1)
	 (AREF RUBOUT-HANDLER-BUFFER IDX))
	(T
	 (COLD-LOAD-STREAM-RUBOUT-HANDLER))))

(DEFVAR COLD-LOAD-STREAM-BLINKER-TIME 15.)
(DEFVAR COLD-LOAD-STREAM-WAIT-TIME 1000.)
(DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES (COLD-LOAD-STREAM)
(DEFUN COLD-LOAD-STREAM-WAIT-FOR-CHAR ()
  (DO ((PHASE NIL)
       (BLINKER-COUNT 0))
      ((KBD-HARDWARE-CHAR-AVAILABLE)
       (AND PHASE
	    (%DRAW-RECTANGLE (FONT-BLINKER-WIDTH FONT) (FONT-BLINKER-HEIGHT FONT) CURSOR-X
			     CURSOR-Y TV:ALU-XOR SELF)))
    (COND ((MINUSP (SETQ BLINKER-COUNT (1- BLINKER-COUNT)))
	   (%DRAW-RECTANGLE (FONT-BLINKER-WIDTH FONT) (FONT-BLINKER-HEIGHT FONT) CURSOR-X
			     CURSOR-Y TV:ALU-XOR SELF)
	   (SETQ PHASE (NOT PHASE)
		 BLINKER-COUNT COLD-LOAD-STREAM-BLINKER-TIME)))
    (DOTIMES (I COLD-LOAD-STREAM-WAIT-TIME)))))

;;; Give a single character, or do rubout processing, throws to RUBOUT-HANDLER on editting.
(DECLARE-INSTANCE-IMMEDIATE-INSTANCE-VARIABLES (COLD-LOAD-STREAM)
(DEFUN COLD-LOAD-STREAM-RUBOUT-HANDLER ()
  (DO ((CH)
       (RUBBED-OUT-SOME)
       (LEN)
       (RUBOUT-HANDLER NIL))
      (NIL)
    (SETQ CH (FUNCALL-SELF ':TYI))
    (COND ((= CH #\CLEAR)			;CLEAR flushes all buffered input
	   (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 0)
	   (SETQ RUBBED-OUT-SOME T)		;Will need to throw out
	   (FUNCALL-SELF ':TYO CH)		;Echo and advance to new line
	   (FUNCALL-SELF ':TYO #\CR))
	  ((OR (= CH #\FORM) (= CH #\VT))	;Retype buffered input
	   (FUNCALL-SELF ':TYO CH)		;Echo it
	   (IF (= CH #\FORM) (FUNCALL-SELF ':CLEAR-SCREEN) (FUNCALL-SELF ':TYO #\CR))
	   (FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER))
	  ((= CH #\RUBOUT)
	   (COND ((NOT (ZEROP (SETQ LEN (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0))))
		  (SETQ CURSOR-X (MAX 0 (- CURSOR-X CHAR-WIDTH)))
		  (FUNCALL-SELF ':CLEAR-EOL)
		  (STORE-ARRAY-LEADER (SETQ LEN (1- LEN)) RUBOUT-HANDLER-BUFFER 0)
		  (SETQ RUBBED-OUT-SOME T)
		  (COND ((ZEROP LEN)
			 (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 1)
			 (*THROW 'RUBOUT-HANDLER T))))))
	  ((LDB-TEST %%KBD-CONTROL-META CH)
	   (%BEEP 1350 400000))
	  (T
	   (FUNCALL-SELF ':TYO CH)
	   ;; This is not in the cold load, fake it until it gets loaded
	   (OR (FBOUNDP 'ARRAY-PUSH-EXTEND)
	       (FSET 'ARRAY-PUSH-EXTEND 'ARRAY-PUSH))
	   (ARRAY-PUSH-EXTEND RUBOUT-HANDLER-BUFFER CH)
	   (COND (RUBBED-OUT-SOME
		  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 1)
		  (*THROW 'RUBOUT-HANDLER T))
		 (T
		  (STORE-ARRAY-LEADER (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0)
				      RUBOUT-HANDLER-BUFFER 1)
		  (RETURN CH))))))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :RUBOUT-HANDLER)
		     (RUBOUT-HANDLER-OPTIONS FUNCTION &REST ARGS)
  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 0)
  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 1)
  (COND (UNRCHF				;If there is unread input, force it in
	 (ARRAY-PUSH RUBOUT-HANDLER-BUFFER UNRCHF)
	 (FUNCALL-SELF ':TYO UNRCHF)
	 (SETQ UNRCHF NIL)))
  (DO ((RUBOUT-HANDLER T)			;Establish rubout handler
       (INHIBIT-SCHEDULING-FLAG T))		;Make sure all chars come here
      (NIL)
    (*CATCH 'RUBOUT-HANDLER			;Throw here when rubbing out
      (PROGN
	(ERRSET (RETURN (APPLY FUNCTION ARGS)))	;Call read type function
	(FUNCALL-SELF ':STRING-OUT RUBOUT-HANDLER-BUFFER)	;On error, retype buffered
	(DO () (NIL) (FUNCALL-SELF ':TYI))))		;and force user to edit it
    ;;Maybe return when user rubs all the way back
    (AND (ZEROP (ARRAY-LEADER RUBOUT-HANDLER-BUFFER 0))
	 (LET ((FULL-RUBOUT-OPTION (ASSQ ':FULL-RUBOUT RUBOUT-HANDLER-OPTIONS)))
	   (AND FULL-RUBOUT-OPTION (RETURN NIL (CADR FULL-RUBOUT-OPTION)))))))

(DEFMETHOD-IMMEDIATE (COLD-LOAD-STREAM :CLEAR-INPUT) ()
  (SETQ UNRCHF NIL)
  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 0)
  (STORE-ARRAY-LEADER 0 RUBOUT-HANDLER-BUFFER 1)
  (DO () ((NOT (KBD-HARDWARE-CHAR-AVAILABLE)))
    ;;Call the convert routine for up-shifts too
    (KBD-CONVERT-TO-SOFTWARE-CHAR (KBD-GET-HARDWARE-CHAR))))

(DEFUN KBD-HARDWARE-CHAR-AVAILABLE ()
  "Returns T if a character is available in the microcode interrupt buffer"
  ( (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
     (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))))

(DEFUN KBD-GET-HARDWARE-CHAR (&AUX P)
  "Returns the next character in the microcode interrupt buffer, and NIL if there is none"
  (COND (( (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
	    (SETQ P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))))
	 (PROG1 (%P-LDB %%Q-POINTER P)
		(SETQ P (1+ P))
		(AND (= P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-END)))
		     (SETQ P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-START))))
		(%P-DPB P %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))))))

(DEFVAR SHIFT-LOCK-XORS NIL)	;If T, both SHIFT LOCK and SHIFT is the same as neither
				; if the character is alphabetic

(DEFUN KBD-CONVERT-TO-SOFTWARE-CHAR (HARD-CHAR &AUX ASC SHIFT BUCKY)
  "Convert hardware character to software character, or NIL to ignore"
  (IF (= (LDB 2003 HARD-CHAR) 1) (KBD-CONVERT-NEW HARD-CHAR)
      (SETQ SHIFT (COND ((BIT-TEST 1400 HARD-CHAR) 2)	;TOP
			((BIT-TEST 300 HARD-CHAR) 1)	;SHIFT
			(T 0)))				;VANILLA
      (SETQ BUCKY (+ (IF (BIT-TEST 06000 HARD-CHAR) 0400 0)	;CONTROL
		     (IF (BIT-TEST 30000 HARD-CHAR) 1000 0)))	;META
      (SETQ ASC (AREF KBD-TRANSLATE-TABLE SHIFT (LOGAND 77 HARD-CHAR)))
      (AND (BIT-TEST 40000 HARD-CHAR)			;SHIFT LOCK
	   (IF (AND SHIFT-LOCK-XORS (BIT-TEST 300 HARD-CHAR))
	       (AND ( ASC #/A) ( ASC #/Z) (SETQ ASC (+ ASC 40)))
	       (AND ( ASC #/a) ( ASC #/z) (SETQ ASC (- ASC 40)))))
      (+ ASC BUCKY)))

;; Sys com locations 500-577 are reserved for the wired keyboard buffer:
;; Locations 501 through 511 contain the buffer header; 520-577 are the buffer (48. chars)
;;
;; This is called when the machine is booted, warm or cold.  It's not an
;; initialization because it has to happen before all other initializations.
(DEFUN INITIALIZE-WIRED-KBD-BUFFER ()
  (DO I 500 (1+ I) (= I 600)
    (%P-STORE-TAG-AND-POINTER I 0 0))
  (%P-DPB 260 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-VECTOR-ADDRESS))
  (%P-DPB (VIRTUAL-UNIBUS-ADDRESS 764112) %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-CSR-ADDRESS))
  (%P-DPB 40 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-CSR-BITS))
  (%P-DPB (VIRTUAL-UNIBUS-ADDRESS 764100) %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-DATA-ADDRESS))
  (%P-DPB 1 %%Q-FLAG-BIT (+ 500 %UNIBUS-CHANNEL-DATA-ADDRESS))
  (%P-DPB 520 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-START))
  (%P-DPB 600 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-END))
  (%P-DPB 520 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
  (%P-DPB 520 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST) 500)
  (%UNIBUS-WRITE 764112 4))	;Keyboard interrupt enable, local mouse

;; Translate from a Unibus address to a Lisp machine virtual address, returning a fixnum.
(DEFUN VIRTUAL-UNIBUS-ADDRESS (ADR)
  (%24-BIT-PLUS (LSH 7740 12.) (LSH ADR -1)))

(DEFVAR KBD-TRANSLATE-TABLE)
;Keyboard translate table is a 3 X 64 array.
;3 entries for each of 100 keys.  First is vanilla, second shift, third top.
;The function KBD-INITIALIZE is only called once, in order to setup this array.

(DEFUN KBD-INITIALIZE ()
  (SETQ KBD-TRANSLATE-TABLE (MAKE-ARRAY WORKING-STORAGE-AREA 'ART-8B '(3 100)))
  (DO ((I 0 (1+ I))  ;2ND DIMENSION
       (L '(
	#\BREAK	#\BREAK	#\NETWORK
	#\ESC	#\ESC	#\SYSTEM
	#/1	#/!	#/!
	#/2	#/"	#/"
	#/3	#/#	#/#
	#/4	#/$	#/$
	#/5	#/%	#/%
	#/6	#/&	#/&
	#/7	#/'	#/'
	#/8	#/(	#/(
	#/9	#/)	#/)
	#/0	#/_	#/_
	#/-	#/=	#/=
	#/@	#/`	#/`
	#/^	#/~	#/~
	#\BS	#\BS	#\BS
	#\CALL	#\CALL	#\ABORT
	#\CLEAR	#\CLEAR	#\CLEAR
	#\TAB	#\TAB	#\TAB
	#/	#/	#/
	#/q	#/Q	#/
	#/w	#/W	#/
	#/e	#/E	#/
	#/r	#/R	#/
	#/t	#/T	#/
	#/y	#/Y	#/
	#/u	#/U	#/
	#/i	#/I	#/
	#/o	#/O	#/
	#/p	#/P	#/
	#/[	#/{	#/{
	#/]	#/}	#/}
	#/\	#/|	#/|
	#//	#/	#/
	#/å	#/ç	#/ 
	#/ä	#/â	#/â
	#\FORM	#\FORM	#\FORM
	#\VT	#\VT	#\VT
	#\RUBOUT #\RUBOUT #\RUBOUT
	#/a	#/A	#/
	#/s	#/S	#/
	#/d	#/D	#/
	#/f	#/F	#/
	#/g	#/G	#/
	#/h	#/H	#\HELP
	#/j	#/J	#/
	#/k	#/K	#/
	#/l	#/L	#/
	#/;	#/+	#/+
	#/:	#/*	#/*
	#\CR	#\CR	#\END
	#\LINE	#\LINE	#\LINE
	#\BACK-NEXT #\BACK-NEXT #\BACK-NEXT
	#/z	#/Z	#/
	#/x	#/X	#/
	#/c	#/C	#/
	#/v	#/V	#/à
	#/b	#/B	#/
	#/n	#/N	#/
	#/m	#/M	#/
	#/,	#/<	#/<
	#/.	#/>	#/>
	#//	#/?	#/?
	#\SP	#\SP	#\SP
	) (CDDDR L)))
      ((NULL L))
    (AS-2 (CAR L) KBD-TRANSLATE-TABLE 0 I)
    (AS-2 (CADR L) KBD-TRANSLATE-TABLE 1 I)
    (AS-2 (CADDR L) KBD-TRANSLATE-TABLE 2 I)))

;Support for new keyboard

;These two variables are bit masks for the shifting keys held down.
;Bit numbers are the same as those in the all-keys-up code sent by the
;keyboard, i.e.
;	0 shift		3 caps lock	6 super		9 mode lock
;	1 greek		4 control	7 hyper		10 repeat
;	2 top		5 meta		8 alt lock
;There are two variables so that if both shifts of a given type are pushed
;down. then one is released, we can tell what's going on.

(DEFVAR KBD-LEFT-SHIFTS 0)
(DEFVAR KBD-RIGHT-SHIFTS 0)
(DEFVAR KBD-KEY-STATE-ARRAY			;1 if key with that ascii code is down
	(MAKE-ARRAY PERMANENT-STORAGE-AREA 'ART-1B 400))
(DEFVAR KBD-KEY-STATE-ARRAY-16B			;For fast clearing of above array
	(MAKE-ARRAY PERMANENT-STORAGE-AREA 'ART-16B 20 KBD-KEY-STATE-ARRAY))

(DECLARE (SPECIAL KBD-NEW-TABLE))	;Array used as translation table.
;The second dimension is 200 long and indexed by keycode.
;The first dimension is the shifts:
; 0 unshifted
; 1 shift
; 2 top
; 3 greek
; 4 shift greek
;Elements in the table are 16-bit unsigned numbers.
;Bit 15 on and bit 14 on means undefined code, ignore and beep.
;Bit 15 on and bit 14 off means low bits are shift for bit in KBD-SHIFTS
;   (40 octal for right-hand key of a pair)
;Bit 15 off is ordinary code.

;Can return NIL if character wasn't really a character.
(DEFUN KBD-CONVERT-NEW (CH)
  (COND ((BIT-TEST 1_15. CH)		;An all-keys-up code, just update shifts mask
	 (COPY-ARRAY-CONTENTS "" KBD-KEY-STATE-ARRAY-16B)	;Mark all keys up
	 (SETQ CH (LDB 0017 CH))	;Get bits for keys or key-pairs still down
	 (SETQ KBD-LEFT-SHIFTS (LOGAND KBD-LEFT-SHIFTS CH)
	       KBD-RIGHT-SHIFTS (LOGAND KBD-RIGHT-SHIFTS CH)
	       KBD-LEFT-SHIFTS		;This is for keys that are down that we thought
	         (LOGIOR		; were up, e.g. caps lock.  Boole 10 is NOR.
		   (LOGAND (BOOLE 10 KBD-LEFT-SHIFTS KBD-RIGHT-SHIFTS) CH)
		   KBD-LEFT-SHIFTS)
	      TV:KBD-BUTTONS 0)		;analogous to mouse buttons, set by roman numerals
	 NIL)
	(T (LET* ((KBD-SHIFTS (LOGIOR KBD-LEFT-SHIFTS KBD-RIGHT-SHIFTS))
		  (NCH (AREF KBD-NEW-TABLE	;NCH gets translate-table entry
			     (COND ((BIT-TEST 2 KBD-SHIFTS)	;Greek
				    (+ (LOGAND 1 KBD-SHIFTS) 3))
				   ((BIT-TEST 4 KBD-SHIFTS) 2)	;Top
				   ((BIT-TEST 1 KBD-SHIFTS) 1)	;Shift
				   (T 0))
			     (LDB 0007 CH)))
		  (NCH0 (AREF KBD-NEW-TABLE 0 (LDB 0007 CH))))
	     (COND ((BIT-TEST 1_15. NCH)	;Not a real character
		    (COND ((BIT-TEST 1_14. NCH)	;Undefined key, beep if key-down
			   (OR (BIT-TEST 1_8 CH)
			       (%BEEP 1350 400000)))
			  (T			;A shifting key, update KBD-SHIFTS
			    (LET ((BOOLE (IF (BIT-TEST 1_8 CH) 2 7))  ;Bit off, on
				  (BIT (LSH 1 (LOGAND NCH 37))))
			      (IF (BIT-TEST 40 NCH)
				  (SETQ KBD-RIGHT-SHIFTS (BOOLE BOOLE BIT KBD-RIGHT-SHIFTS))
				  (SETQ KBD-LEFT-SHIFTS (BOOLE BOOLE BIT KBD-LEFT-SHIFTS))))))
		    NIL)
		   ((BIT-TEST 1_8 CH)
		    (ASET 0 KBD-KEY-STATE-ARRAY NCH0)
		    (AND (BIT-TEST 1_9 KBD-SHIFTS)	 ;Mode lock
			 (SELECTQ NCH
			   (#\I (SETQ TV:KBD-BUTTONS (BOOLE 4 TV:KBD-BUTTONS 1)))
			   (#\II (SETQ TV:KBD-BUTTONS (BOOLE 4 TV:KBD-BUTTONS 2)))
			   (#\III (SETQ TV:KBD-BUTTONS (BOOLE 4 TV:KBD-BUTTONS 4)))))
		    NIL)	 ;Just an up-code
		   ((AND (BIT-TEST 1_9 KBD-SHIFTS)	 ;Mode lock
			 (MEMQ NCH '(#\I #\II #\III)))
		    (ASET 1 KBD-KEY-STATE-ARRAY NCH0)
		    (SETQ TV:KBD-BUTTONS (LOGIOR TV:KBD-BUTTONS
						 (SELECTQ NCH (#\I 1) (#\II 2) (T 4))))
		    NIL)
		   (T ;A real key depression.  Check for caps-lock.
		    (ASET 1 KBD-KEY-STATE-ARRAY NCH0)
		    (AND (BIT-TEST 10 KBD-SHIFTS)	;Caps lock
			 (IF (AND SHIFT-LOCK-XORS (BIT-TEST 1 KBD-SHIFTS))
			     (AND ( NCH #/A) ( NCH #/Z) (SETQ NCH (+ NCH 40)))
			     (AND ( NCH #/a) ( NCH #/z) (SETQ NCH (- NCH 40)))))
		    (DPB (LDB 0404 KBD-SHIFTS) ;Hyper, Super, Meta, Control
			 %%KBD-CONTROL-META NCH))))))) ;A real character pushed down

(DEFUN KBD-MAKE-NEW-TABLE ()
  (LET ((TBL (MAKE-ARRAY PERMANENT-STORAGE-AREA 'ART-16B '(5 200))))
    (DO ((J 0 (1+ J))
	 (L '( 
	()					 ;0 not used
	#\II					 ;1 Roman II
	#\IV					 ;2 Roman IV
	100011					 ;3 Mode lock
	()					 ;4 not used
	100006					 ;5 Left super
	()					 ;6 not used
	()					 ;7 not used
	()					 ;10 not used
	(#/4 #/$ #/$)				 ;11 Four
	(#/r #/R #/)				 ;12 R
	(#/f #/F)				 ;13 F
	(#/v #/V)				 ;14 V
	100008					 ;15 Alt Lock
	()					 ;16 not used
	#\HAND-RIGHT				 ;17 Hand Right
	100004					 ;20 Left control
	(#/: 14 14)				 ;21 plus-minus
	#\TAB					 ;22 tab
	#\RUBOUT				 ;23 rubout
	100000					 ;24 Left Shift
	100040					 ;25 Right Shift
	100044					 ;26 Right control
	()					 ;27 not used
	#\HOLD-OUTPUT				 ;30 hold output
	(#/8 #/* #/*)				 ;31 Eight
	(#/i #/I #/)				 ;32 I
	(#/k #/K #/)				 ;33 K
	(#/, #/< #/<)				 ;34 comma
	100041					 ;35 Right Greek
	#\LINE					 ;36 Line
	(#/\ #/| #/|)				 ;37 Backslash
	#\ESC					 ;40 terminal
	()					 ;41 not used
	#\NETWORK				 ;42 network
	()					 ;43 not used
	100001					 ;44 Left Greek
	100005					 ;45 Left Meta
	#\STATUS				 ;46 status
	#\RESUME				 ;47 resume
	#\FORM					 ;50 clear screen
	(#/6 #/^ #/^)				 ;51 Six
	(#/y #/Y #/)				 ;52 Y
	(#/h #/H #/)				 ;53 H
	(#/n #/N #/)				 ;54 N
	()					 ;55 not used
	()					 ;56 not used
	()					 ;57 not used
	()					 ;60 not used
	(#/2 #/@ #/@)				 ;61 Two
	(#/w #/W #/)				 ;62 W
	(#/s #/S)				 ;63 S
	(#/x #/X)				 ;64 X
	100046					 ;65 Right Super
	()					 ;66 not used
	#\ABORT					 ;67 Abort
	()					 ;70 not used
	(#/9 #/( #/( )				 ;71 Nine
	(#/o #/O #/)				 ;72 O
	(#/l #/L #/ 10)			 ;73 L/lambda
	(#/. #/> #/>)				 ;74 period
	()					 ;75 not used
	()					 ;76 not used
	(#/` #/~ #/~ #/)			 ;77 back quote
	#\BACK-NEXT				 ;100 macro
	#\I					 ;101 Roman I
	#\III					 ;102 Roman III
	()					 ;103 not used
	100002					 ;104 Left Top
	()					 ;105 not used
	#\HAND-UP				 ;106 Up Thumb
	#\CALL					 ;107 Call
	#\CLEAR					 ;110 Clear Input
	(#/5 #/% #/%)				 ;111 Five
	(#/t #/T #/)				 ;112 T
	(#/g #/G #/ 11)			 ;113 G/gamma
	(#/b #/B #/ #/)			 ;114 B
	()					 ;115 Repeat
	#\HELP					 ;116 Help
	(#\HAND-LEFT #\HAND-LEFT #\HAND-LEFT #/ #/) ;117 Hand Left
	#\QUOTE					 ;120 Quote
	(#/1 #/! #/!)				 ;121 One
	(#/q #/Q #/)				 ;122 Q
	(#/a #/A 140000 #/)			 ;123 A
	(#/z #/Z)				 ;124 Z
	100003					 ;125 Caps Lock
	(#/= #/+ #/+)				 ;126 Equals
	()					 ;127 not used
	()					 ;130 not used
	(#/- #/_ #/_)				 ;131 Minus
	(#/( #/[ #/[)				 ;132 Open parenthesis
	(#/' #/" #/" 0)				 ;133 Apostrophe/center-dot
	#\SP					 ;134 Space
	()					 ;135 not used
	#\CR					 ;136 Return
	(#/) #/] #/])				 ;137 Close parenthesis
	()					 ;140 not used
	#\SYSTEM				 ;141 system
	()					 ;142 not used
	#/					 ;143 Alt Mode
	()					 ;144 not used
	100007					 ;145 Left Hyper
	(#/} 140000 140000)			 ;146 }
	()					 ;147 not used
	()					 ;150 not used
	(#/7 #/& #/&)				 ;151 Seven
	(#/u #/U #/)				 ;152 U
	(#/j #/J #/)				 ;153 J
	(#/m #/M #/)				 ;154 M
	100042					 ;155 Right Top
	#\END					 ;156 End
	#\DELETE				 ;157 Delete
	#\BS					 ;160 Overstrike
	(#/3 #/# #/#)				 ;161 Three
	(#/e #/E #/ #/)			 ;162 E
	(#/d #/D 140000 12)			 ;163 D/delta
	(#/c #/C #/)				 ;164 C
	100045					 ;165 Right Meta
	(#/{ 140000 140000)			 ;166 {
	#\BREAK					 ;167 Break
	#\STOP-OUTPUT				 ;170 Stop Output
	(#/0 #/) #/))				 ;171 Zero
	(#/p #/P #/ #/)			 ;172 P
	(#/; #/: #/:)				 ;173 Semicolon
	(#// #/? #/? 177)			 ;174 Question/Integral
	100047					 ;175 Right Hyper
	(#\HAND-DOWN #\HAND-DOWN #\HAND-DOWN #/ä #/ä)	;176 Down Thumb
	()					 ;177 Not used
	      ) (CDR L)))
	((= J 200) (SETQ KBD-NEW-TABLE TBL))
      (DO ((I 0 (1+ I))
	   (K (CAR L)))
	  ((= I 5))
	(ASET (COND ((ATOM K) (OR K 140000))
		    ((NULL (CAR K)) 140000)
		    (T (CAR K)))
	      TBL I J)
	(AND (LISTP K) (SETQ K (CDR K)))))))

(DEFVAR KBD-NEW-TABLE (KBD-MAKE-NEW-TABLE))

;;; Hardware primitives
;;; Support for "Simple TV" (32-bit TV system)
;;;Some special variables used by the hardware routines
(DECLARE (SPECIAL CPT-SYNC CPT-SYNC1 CPT-SYNC2 COLOR-SYNC CPT-SYNC-60HZ))

;;; Read and write the sync program
(DEFUN READ-SYNC (ADR &OPTIONAL (TV-ADR TV:(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
  (%XBUS-WRITE (+ TV-ADR 2) ADR)		;Set pointer
  (LOGAND 377 (%XBUS-READ (+ TV-ADR 1))))

(DEFUN WRITE-SYNC (ADR DATA &OPTIONAL (TV-ADR TV:(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
  (%XBUS-WRITE (+ TV-ADR 2) ADR)		;Set pointer
  (%XBUS-WRITE (+ TV-ADR 1) DATA))

;;; Start and stop the sync program
(DEFUN START-SYNC (CLOCK BOW VSP
		   &OPTIONAL (TV-ADR TV:(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
  (%XBUS-WRITE TV-ADR (+ (LSH BOW 2) CLOCK))
  (%XBUS-WRITE (+ TV-ADR 3) (+ 200 VSP)))

(DEFUN STOP-SYNC (&OPTIONAL (TV-ADR TV:(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
  (%XBUS-WRITE (+ TV-ADR 3) 200))		;Disable output of sync

;;; Write into the sync program from a list with repeat-counts
;;; Sub-lists are repeated <car> times.
(DEFUN FILL-SYNC (L &OPTIONAL (ADR 0) (TV-ADR TV:(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN))
		    &AUX X)
  (DO ((L L (CDR L))) ((NULL L) ADR)
    (SETQ X (CAR L))
    (COND ((ATOM X) (WRITE-SYNC ADR X TV-ADR) (SETQ ADR (1+ ADR)))
	  (T (DO N (CAR X) (1- N) (ZEROP N)
		 (SETQ ADR (FILL-SYNC (CDR X) ADR TV-ADR)))))))

;;; Set up sync for CPT monitor 768. x 896.
(DEFUN SETUP-CPT (&OPTIONAL (SYNC-PROG CPT-SYNC2)
			    (TV-ADR TV:(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN))
			    (FORCE-P NIL))
  ;; Always turn on vertical sync interrupts if this is the first TV controller.
  ;; The microcode relies on these as a periodic clock for various purposes.
  ;; If not the first controller, leave the interrupt enable the way it is.
  (WITHOUT-INTERRUPTS
    (LET ((INTERRUPT-ENABLE (IF (= TV-ADR 377760) 10	;This is the number UCADR knows
				(LOGAND (%XBUS-READ TV-ADR) 10)))
	  (STATUS (%XBUS-READ TV-ADR)))
      (COND ((AND (NOT FORCE-P) (BIT-TEST STATUS 200))
	     ;; Running in PROM, so stay there
	     (%XBUS-WRITE TV-ADR (+ 4 INTERRUPT-ENABLE)))
	    (T (STOP-SYNC TV-ADR)
	       (FILL-SYNC SYNC-PROG 0 TV-ADR)
	       (START-SYNC INTERRUPT-ENABLE 1 0 TV-ADR))))))  ;Clock 0, BOW 1, VSP 0

(DEFUN PROM-SETUP (&OPTIONAL (TV-ADR TV:(SCREEN-CONTROL-ADDRESS DEFAULT-SCREEN)))
  (%XBUS-WRITE (+ TV-ADR 3) 0))

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
;(SETQ CPT-SYNC '(
;   1.  (2 33) (8 13) (18. 12) 212 112
;   45.  (2 33) (8 13) (18. 12) 212 12
;   8.  (2 33)  (6 13) 13 12 (18. 2) 202 2
;   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;   131. (2 31) (6 11) 11 50 (9. 0 40) 200 0
;   8. (2 31) (6 11) 11 10 (8. 0 0) 0 0 300 0
;))

;704. x 896.
;(SETQ CPT-SYNC1 '(
;   1.  (1 33) (5 13) 12 12 (10. 12 12) 212 113			;VERT SYNC, CLEAR TVMA
;   53.  (1 33) (5 13) 12 12 (10. 12 12) 212 13			;VERT RETRACE
;   8.  (1 31)  (5 11) 11 10 (10. 0 0) 200 21		;8 LINES OF MARGIN
;   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;   131. (1 31) (5 11) 11 50 (10. 0 40) 200 21
;   7. (1 31) (5 11) 11 10 (10. 0 0) 200 21
;   1. (1 31) (5 11) 11 10 (10. 0 0) 300 23
;))

;Sync for 64 Mhz crystal,  768. x 896.

(SETQ CPT-SYNC2 '(
   1.  (1 33) (5 13) 12 12 (11. 12 12) 212 113			;VERT SYNC, CLEAR TVMA
   53.  (1 33) (5 13) 12 12 (11. 12 12) 212 13			;VERT RETRACE
   8.  (1 31)  (5 11) 11 10 (11. 0 0) 200 21		;8 LINES OF MARGIN
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (11. 0 40) 200 21
   131. (1 31) (5 11) 11 50 (11. 0 40) 200 21
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

;This is used for making an instance in the cold-load environment,
;so that we can display on the TV in the cold-load stream.
;The instance variable slots get initialized to NIL.  Note that
;the method-alist gets used as the dtp-select-method, so it must have
;the unknown-message handler in cdr of its last.
(DEFUN FAKE-UP-INSTANCE (TYPENAME INSTANCE-VARIABLES METHOD-ALIST INIT-OPTIONS
			 &AUX SIZE BINDINGS INSTANCE DESCRIPTOR)
  ;; Note that the length of this array must agree with INSTANCE-DESCRIPTOR-OFFSETS in QCOM
  (SETQ DESCRIPTOR (MAKE-ARRAY PERMANENT-STORAGE-AREA 'ART-Q 5))
  (ASET (SETQ SIZE (1+ (LENGTH INSTANCE-VARIABLES)))
	DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-SIZE))
  (SETQ BINDINGS (MAKE-LIST PERMANENT-STORAGE-AREA (LENGTH INSTANCE-VARIABLES)))
  (DO ((B BINDINGS (CDR B))
       (L INSTANCE-VARIABLES (CDR L)))
      ((NULL L))
    (RPLACA B (VALUE-CELL-LOCATION (CAR L))))
  (ASET BINDINGS DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-BINDINGS))
  (ASET (%MAKE-POINTER DTP-SELECT-METHOD METHOD-ALIST)
	DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-FUNCTION))
  (ASET TYPENAME DESCRIPTOR (1- %INSTANCE-DESCRIPTOR-TYPENAME))
  (SETQ INSTANCE (%ALLOCATE-AND-INITIALIZE DTP-INSTANCE DTP-INSTANCE-HEADER
			   DESCRIPTOR NIL PERMANENT-STORAGE-AREA SIZE))
  (FUNCALL INSTANCE ':INIT (LOCF INIT-OPTIONS))
  INSTANCE)

(MAKE-INSTANCE-IMMEDIATE COLD-LOAD-STREAM
			 :WIDTH 1400 :HEIGHT 1600 :BUFFER #.(LSH 77 18.)
			 :CONTROL-ADDRESS 377760)

;:NORMAL to not do when the ADD-INITIALIZATION is executed, only when it is
;time to do the system-initializations
; SETUP-CPT is now called directly from LISP-REINITIALIZE.  Problem was, on a cold boot,
;references to the video buffer could happen before the sync program was set up.
;This caused main memory parity errors even though the TV sends back ignore parity!
;One path that caused trouble was PROCESS-INITIALIZE, (PROCESS-CLASS :RESET),
;TV:WHO-LINE-PROCESS-CHANGE, etc.
;(ADD-INITIALIZATION "CPT-SYNC" '(SETUP-CPT CPT-SYNC2) '(:SYSTEM :NORMAL))
