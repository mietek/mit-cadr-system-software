;;; -*- Mode: LISP; Package: TV; Base: 8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFVAR |*All-instance-variables-on-one-page/|*|
	'(BASELINE BASELINE-ADJ BIT-ARRAY BITS-PER-PIXEL BLANK-RECTANGLES
	  BLINKER-LIST BORDER-MARGIN-WIDTH BORDERS BOTTOM-ITEM BOTTOM-MARGIN-SIZE
	  BOTTOM-REACHED BUFFER BUFFER-HALFWORD-ARRAY CACHE CHAR CHAR-ALUF
	  CHAR-WIDTH CHOICE-TYPES CHOICE-VALUE CHOSEN-ITEM COLUMN-WIDTH COLUMNS
	  CONSTRAINTS CONTROL-ADDRESS CURRENT-DISPLAY CURRENT-FONT CURRENT-ITEM
	  CURRENT-OBJECT CURRENT-REGION CURSOR-X CURSOR-Y DEEXPOSED-TYPEOUT-ACTION
	  DEFAULT-FONT DESELECTED-VISIBILITY DISPLAY-ITEM DISPLAYED-ITEMS
	  DISPLAYING-LIST ERASE-ALUF EXPOSED-INFERIORS EXPOSED-P EXPOSED-PANES
	  FLAGS FLASHY-SCROLLING-BLINKER FLASHY-SCROLLING-MAX-SPEED
	  FLASHY-SCROLLING-REGION FOLLOW-P FONT FONT-ALIST FONT-MAP FROBS FUNCTION
	  GEOMETRY GRAY-ARRAY HAD-MOUSE-P HALF-PERIOD HAVE-EDGES HEIGHT HYSTERESIS
	  INCOMPLETE-P INFERIORS INSPECTORS INTERNAL-CONSTRAINTS INTERNAL-PANES
	  INVISIBLE-TO-MOUSE-P IO-BUFFER ITEM-BLINKER ITEM-LIST ITEM-LIST-POINTER
	  ITEM-NAME ITEM-TYPE-ALIST ITEMS LABEL LABEL-NEEDS-UPDATING LAST-ITEM
	  LEFT-MARGIN-SIZE LEVEL-COUNT LINE-HEIGHT LINE-OVERFLOW-ALLOWED
	  LIST-BLINKER LIST-ITEM LOCATIONS-PER-LINE LOCK LOCK-COUNT
	  MAKING-SCROLL-DECISION MARGIN-CHOICES MENU MODE-ALIST MODIFY-MODE
	  MORE-VPOS MOUSE-BLINKERS NAME NEEDS-REDISPLAY OLD-BORDERS OLD-LABEL
	  OLD-SCREEN-ARRAY OLD-TYPEAHEAD OLD-X OLD-Y OUTPUT-LOCK PANES
	  PARSED-CONSTRAINTS PHASE PRINT-FUNCTION PRINT-FUNCTION-ARG PRIORITY
	  PROCESS PROPERTY-LIST RECT-HEIGHT RECT-WIDTH RECTANGLE-LIST RECURSION
	  REGION-LIST RESTORED-BITS-P RIGHT-MARGIN-SIZE ROW-HEIGHT ROW-MAP
	  RUBOUT-HANDLER-BUFFER SCREEN-ARRAY SCREEN-IMAGE SCREEN-LINES
	  SCREEN-MANAGER-SCREEN-IMAGE SCREEN-ROWS SCROLL-BAR
	  SCROLL-BAR-ALWAYS-DISPLAYED SCROLL-BAR-IN SELECTED-PANE
	  SENSITIVE-ITEM-TYPES SET-EDGES-MODE SHEET SINGLE-RIGHT-MENU STACK-GROUP
	  STATE SUBSTITUTIONS SUPERIOR TARGET-TOP-ITEM TEMPORARY-BIT-ARRAY
	  TEMPORARY-WINDOWS-LOCKED TIME-UNTIL-BLINK TOP-ITEM TOP-MARGIN-SIZE
	  TOP-ROW TOTAL-ROWS TRI-HEIGHT TRI-WIDTH TRUNCATION TYPE-ALIST
	  TYPEOUT-WINDOW VALUE-ARRAY VISIBILITY WIDTH WINDOW-OF-INTEREST
	  WINDOW-UNDER-MENU X-OFFSET X-ORIGIN X-POS Y-OFFSET Y-ORIGIN
	  Y-POS))

(DEFVAR DEFAULT-SCREEN)
(DEFVAR ALL-THE-SCREENS NIL)
(DEFVAR MORE-PROCESSING-GLOBAL-ENABLE T)
(DEFVAR MAIN-SCREEN)
(DEFVAR BEEP T)
(REMPROP 'BEEP ':SOURCE-FILE-NAME)	;Avoid error message when function defined
(DEFVAR BEEP-DURATION 400000)
(DEFVAR BEEP-WAVELENGTH 1350)

;;;These are here because they are needed in more than one file
(DEFVAR SELECTED-WINDOW NIL)		;The currently selected window
(DEFVAR SELECTED-IO-BUFFER NIL)		;The IO-BUFFER that input is currently being
					; directed to
(DEFVAR KBD-TRANSLATE-TABLE)		;Translation from keyboard code to character
(DEFVAR WHO-LINE-PROCESS CURRENT-PROCESS)	;Process that the wholine is stuck on
(PUSH '(SETQ WHO-LINE-PROCESS NIL) LOGOUT-LIST)	;Freeze wholine during loading, unfreeze later
(DEFVAR LAST-WHO-LINE-PROCESS CURRENT-PROCESS)	;The last one which was displayed
(DEFVAR INHIBIT-WHO-LINE NIL)		;Set this to T with CC if who line is broken
(DEFVAR INHIBIT-SCREEN-MANAGEMENT NIL)	;If set to T causes screen management not to
					; automatically.  This is useful when it is
					; known that many operations are going to be done
					; on the screen.  The function should call
					; SCREEN-MANAGE-SHEET on the superior that is was
					; hacking windows on for correct behaviour.
(DEFVAR MOUSE-WINDOW)			;Window controlling the mouse, NIL if none
(DEFVAR MOUSE-RECONSIDER)		;T => mouse process should return to overseer
					;and decide anew which window should get the mouse.
					;For use by :MOUSE-MOVES methods, etc.
(DEFVAR WINDOW-OWNING-MOUSE)		;NIL, or window which has seized the mouse, or
					;T if someone has seized the mouse and can't identify
					;himself as any particular window,
					;or STOP to make the mouse process do nothing.
(REMPROP 'WINDOW-OWNING-MOUSE ':SOURCE-FILE-NAME)  ;Avoid error message when function defined
(DEFVAR MOUSE-X)			;X coordinate of MOUSE-BLINKER
(DEFVAR MOUSE-Y)			;Y coordinate of MOUSE-BLINKER
(DEFVAR MOUSE-SHEET NIL)		;Which sheet MOUSE-BLINKER lives on

;; Dummy function for load-time use
(OR (FBOUNDP 'MOUSE-WAKEUP)
    (FSET' MOUSE-WAKEUP '(LAMBDA () NIL)))

(DEFVAR SHEET-AREA (MAKE-AREA ':NAME 'SHEET-AREA))
(DEFMACRO SHEET-CONSING (&REST BODY)
  `(LET ((DEFAULT-CONS-AREA SHEET-AREA))
     . ,BODY))

(DEFVAR BLINKER-AREA (MAKE-AREA ':NAME 'BLINKER-AREA))

(DEFFLAVOR SHEET
	   ((SCREEN-ARRAY NIL)	;Array that output goes on.  Either a standard array
				; or a section of the physical screen.  May be null when
				;deexposed if no BIT-ARRAY. (microcode use)
	    LOCATIONS-PER-LINE	;Number of locations per raster line (microcode use)
	    OLD-SCREEN-ARRAY	;SCREEN-ARRAY when last exposed if there is no BIT-ARRAY
	    (BIT-ARRAY NIL)	;"In-core" array used when sheet not exposed (may be null)
	    
	    (NAME NIL)		;What this here sheet is called
	    (LOCK NIL)		;Lock cell, contains unique-id of owner of lock, or a list
				;of temporary locking unique-ids.
	    (LOCK-COUNT 0)	;Number of times lock is locked by this id
				;(lock is freed when 0)

	    (SUPERIOR MOUSE-SHEET) ;Null superior is top.
	    (INFERIORS NIL)
	    
	    (EXPOSED-P NIL)	;T when exposed, NIL otherwise.  In this context "exposed"
				;means that it is among the superior's exposed-inferiors
				;and the superior either has a bit-array or is exposed.
				;T here does not necessarily mean it's visible on the screen.
	    (EXPOSED-INFERIORS NIL)
	    
	    (X-OFFSET NIL)	;Position relative to position of superior
	    (Y-OFFSET NIL)
	    (WIDTH NIL)		;Size of sheet
	    (HEIGHT NIL)
	    
	    CURSOR-X		;Position at which to draw next character
	    CURSOR-Y
	    
	    MORE-VPOS		;Y passing here triggers MORE processing
	    
	    (TOP-MARGIN-SIZE 0)	;Reserved region around outside of sheet (for borders, etc.)
	    (BOTTOM-MARGIN-SIZE 0)
	    (LEFT-MARGIN-SIZE 0)
	    (RIGHT-MARGIN-SIZE 0)
	    
	    (FLAGS 0)		;A fixnum containing various flags
	    
	    ;;; Font information
	    BASELINE		;# raster lines from top of char cell to baseline.
	    FONT-MAP		;Map from font numbers to font arrays
	    CURRENT-FONT	;Currently selected font
	    BASELINE-ADJ	;Y offset for current font to align baseline
	    LINE-HEIGHT		;Total number of raster lines per character line
	    CHAR-WIDTH		;Character width for cursor blinker + (X,Y) positioning
	    CHAR-ALUF		;ALU function for drawing characters
	    ERASE-ALUF		;ALU function for erasing characters/lines/whole thing	    
	    (BLINKER-LIST NIL)	;Possibly null list of blinkers on this sheet

	    (DEEXPOSED-TYPEOUT-ACTION ':NORMAL)
	    (TEMPORARY-BIT-ARRAY NIL)
	    (TEMPORARY-WINDOWS-LOCKED NIL)
	    RESTORED-BITS-P
	    (INVISIBLE-TO-MOUSE-P NIL)
	    (SCREEN-MANAGER-SCREEN-IMAGE NIL)
	    (PRIORITY NIL)
	    )
	   ()
  :ORDERED-INSTANCE-VARIABLES
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  (:SETTABLE-INSTANCE-VARIABLES DEEXPOSED-TYPEOUT-ACTION)
  (:INITABLE-INSTANCE-VARIABLES
    NAME WIDTH HEIGHT BIT-ARRAY
    LEFT-MARGIN-SIZE TOP-MARGIN-SIZE RIGHT-MARGIN-SIZE BOTTOM-MARGIN-SIZE
    SUPERIOR FONT-MAP)
  (:INIT-KEYWORDS :TOP :Y :BOTTOM :LEFT :X :RIGHT :EDGES :BLINKER-P :REVERSE-VIDEO-P
		  :CHARACTER-WIDTH :CHARACTER-HEIGHT :INSIDE-SIZE :INSIDE-WIDTH :INSIDE-HEIGHT
		  :MORE-P :VSP :BLINKER-FUNCTION :BLINKER-DESELECTED-VISIBILITY :INTEGRAL-P
		  :SAVE-BITS :RIGHT-MARGIN-CHARACTER-FLAG :TRUNCATE-LINE-OUT-FLAG
		  :BACKSPACE-NOT-OVERPRINTING-FLAG)
  (:DOCUMENTATION :LOWLEVEL-MIXIN "A lowest level window type
This is the data structure known about by the microcode."))

;;;*****
(DEFF SHEET-X #'SHEET-X-OFFSET)
(DEFF SHEET-Y #'SHEET-Y-OFFSET)
;;;*****

(DEFFLAVOR SCREEN
	   ((BITS-PER-PIXEL 1)	;For gray or color
	    FONT-ALIST		;Associate from names to font objects.  NYI.
	    BUFFER		;Virtual memory address of video buffer
	    CONTROL-ADDRESS	;XBUS I/O address of control register
	    BUFFER-HALFWORD-ARRAY	;One-dimensional array of 16-bit buffer hunks
	    (DEFAULT-FONT FONTS:CPTFONT)
	    PROPERTY-LIST
	    (X-OFFSET 0)
	    (Y-OFFSET 0)
	    (SUPERIOR NIL)
	    LOCATIONS-PER-LINE
	    (LEVEL-COUNT 0)
	    (MOUSE-BLINKERS NIL)
	    )
	   (SHEET)
;  :ORDERED-INSTANCE-VARIABLES		;THIS CANNOT WORK
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES BUFFER-HALFWORD-ARRAY DEFAULT-FONT
					  CONTROL-ADDRESS PROPERTY-LIST
					  BITS-PER-PIXEL BUFFER MOUSE-BLINKERS)
  (:INITABLE-INSTANCE-VARIABLES
    BITS-PER-PIXEL FONT-ALIST BUFFER CONTROL-ADDRESS BUFFER-HALFWORD-ARRAY DEFAULT-FONT
    PROPERTY-LIST LOCATIONS-PER-LINE)
  (:GETTABLE-INSTANCE-VARIABLES MOUSE-BLINKERS)
  (:SETTABLE-INSTANCE-VARIABLES MOUSE-BLINKERS)
  (:DOCUMENTATION :SPECIAL-PURPOSE "The software data structure for the actual screen
The top of a window hierachy should be of this type.  There will be only one for each
hardware display."))

(DEFMACRO DEFINE-SHEET-FLAGS (&REST FLAGS)
  `(PROGN 'COMPILE
	  ,@(MAPCAR #'(LAMBDA (FLAG)
			`(DEFMACRO ,(INTERN (STRING-APPEND "SHEET-" (CAR FLAG)))
				   (&OPTIONAL SHEET)
			   `(LDB ,',(CADR FLAG)
				 ,(IF SHEET `(SHEET-FLAGS ,SHEET) 'FLAGS))))
		    FLAGS)))
	    
(DEFINE-SHEET-FLAGS
  (EXCEPTIONS 0104)		;Reasons why typeout can't happen:
   ;(END-LINE-FLAG 0101)	;(spare bit no longer used)
   (END-PAGE-FLAG 0201)		;Cursor is below bottom limit
   (MORE-FLAG 0301)		;More processing needs to happen
   (OUTPUT-HOLD-FLAG 0401)	;Output may not happen on this sheet
  (RIGHT-MARGIN-CHARACTER-FLAG 0501)   ;A special character (!) indicates wraparound
  (TRUNCATE-LINE-OUT-FLAG 0601)        ;SHEET-LINE-OUT should truncate rather than wrap around
  (DONT-BLINK-BLINKERS-FLAG 0701)      ;Don't blink blinkers on this sheet or its inferiors
  (BACKSPACE-NOT-OVERPRINTING-FLAG 1001))	;Backspace is another losenge character

;;;Sizes within margins
(DEFMACRO SHEET-INSIDE-LEFT (&OPTIONAL SHEET)
  (IF SHEET `(SHEET-LEFT-MARGIN-SIZE ,SHEET) 'LEFT-MARGIN-SIZE))

(DEFMACRO SHEET-INSIDE-TOP (&OPTIONAL SHEET)
  (IF SHEET `(SHEET-TOP-MARGIN-SIZE ,SHEET) 'TOP-MARGIN-SIZE))

(DEFMACRO SHEET-INSIDE-RIGHT (&OPTIONAL SHEET)
  (IF SHEET
      `(- (SHEET-WIDTH ,SHEET) (SHEET-RIGHT-MARGIN-SIZE ,SHEET))
      `(- WIDTH RIGHT-MARGIN-SIZE)))

(DEFMACRO SHEET-INSIDE-BOTTOM (&OPTIONAL SHEET)
  (IF SHEET
      `(- (SHEET-HEIGHT ,SHEET) (SHEET-BOTTOM-MARGIN-SIZE ,SHEET))
      `(- HEIGHT BOTTOM-MARGIN-SIZE)))

(DEFMACRO SHEET-INSIDE-WIDTH (&OPTIONAL SHEET)
  (IF SHEET
      `(- (SHEET-WIDTH ,SHEET) (SHEET-LEFT-MARGIN-SIZE ,SHEET)
	  (SHEET-RIGHT-MARGIN-SIZE ,SHEET))
      `(- WIDTH LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))

(DEFMACRO SHEET-INSIDE-HEIGHT (&OPTIONAL SHEET)
  (IF SHEET
      `(- (SHEET-HEIGHT ,SHEET) (SHEET-TOP-MARGIN-SIZE ,SHEET)
	  (SHEET-BOTTOM-MARGIN-SIZE ,SHEET))
      `(- HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE)))

(DEFMACRO SHEET-TEMPORARY-P (&OPTIONAL SHEET)
  `(NOT (NULL ,(IF SHEET `(SHEET-TEMPORARY-BIT-ARRAY ,SHEET) 'TEMPORARY-BIT-ARRAY))))

(DEFMACRO SHEET-SUPERIOR-SCREEN-ARRAY (&OPTIONAL SHEET)
  (LET ((SUPERIOR (IF SHEET `(SHEET-SUPERIOR ,SHEET) 'SUPERIOR)))
    `(OR (SHEET-SCREEN-ARRAY ,SUPERIOR) (SHEET-OLD-SCREEN-ARRAY ,SUPERIOR))))

(DEFMACRO SHEET-OUTPUT-HELD-P (&OPTIONAL SHEET)
  (IF SHEET
      `(OR (NOT (ZEROP (SHEET-OUTPUT-HOLD-FLAG ,SHEET)))
	   (LISTP (SHEET-LOCK ,SHEET)))
      '(OR (NOT (ZEROP (SHEET-OUTPUT-HOLD-FLAG)))
	   (LISTP LOCK))))

(DEFMACRO SHEET-BITS-PER-PIXEL (SHEET)
  ;; Only callable when the sheet can be output on -- BEWARE!
  `(LSH 1 (1- (%P-LDB %%ARRAY-TYPE-FIELD (SHEET-SCREEN-ARRAY ,SHEET)))))

(DEFMACRO SHEET-LINE-NO (&OPTIONAL SHEET CURSOR-Y)
  `(// (- ,(COND (CURSOR-Y CURSOR-Y)
		 (SHEET `(SHEET-CURSOR-Y ,SHEET))
		 (T 'CURSOR-Y))
	  (SHEET-INSIDE-TOP ,SHEET))
       ,(IF SHEET
	    `(SHEET-LINE-HEIGHT ,SHEET)
	    'LINE-HEIGHT)))

(DEFMACRO SHEET-NUMBER-OF-INSIDE-LINES (&OPTIONAL SHEET)
  `(// (SHEET-INSIDE-HEIGHT ,SHEET)
       ,(IF SHEET `(SHEET-LINE-HEIGHT ,SHEET) 'LINE-HEIGHT)))

;;;A blinker is an actor, described as follows:
(DEFFLAVOR BLINKER
	((X-POS NIL)		;X position of blinker (left) NIL if should follow sheet
	 (Y-POS NIL)		;Y position of blinker (top)
	 SHEET			;Sheet associated with
	 (VISIBILITY ':BLINK)	;NIL invisible, T visible, :BLINK blinking, :ON visible but
				; blinking when selected, :OFF invisibile but ...
	 (DESELECTED-VISIBILITY ':ON)	;Blinker's visibility when the sheet it is on is
				; not selected, reasonable values :ON, :OFF and :BLINK
	 (HALF-PERIOD 15.)	;Time interval (60ths) between phase blinks
	 (PHASE NIL)		;NIL not visible, anything else visible in some form
				; (Complementing blinker has only two phases, uses NIL, T)
	 (TIME-UNTIL-BLINK 0)	;Time interval until next blink.  NIL means not blinking,
				; the clock level should ignore this blinker.
	 (FOLLOW-P NIL)
	 )
	()
  :ORDERED-INSTANCE-VARIABLES :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  (:INITABLE-INSTANCE-VARIABLES X-POS Y-POS SHEET VISIBILITY FOLLOW-P)
  (:SETTABLE-INSTANCE-VARIABLES DESELECTED-VISIBILITY HALF-PERIOD)
  (:REQUIRED-METHODS :BLINK :SIZE)
  (:SELECT-METHOD-ORDER :BLINK))

(DEFMACRO BLINKER-SET-CURSORPOS (BLINKER X Y)
  `(FUNCALL ,BLINKER ':SET-CURSORPOS ,X ,Y))

(DEFMACRO BLINKER-READ-CURSORPOS (BLINKER)
  `(FUNCALL ,BLINKER ':READ-CURSORPOS))

(DEFMACRO BLINKER-SET-VISIBILITY (BLINKER VISIBILITY)
  `(FUNCALL ,BLINKER ':SET-VISIBILITY ,VISIBILITY))

(DEFMACRO BLINK (BLINKER)
  `(FUNCALL ,BLINKER ':BLINK))

(DEFMACRO BLINKER-SET-SIZE (BLINKER WIDTH HEIGHT)
  `(FUNCALL ,BLINKER ':SET-SIZE ,WIDTH ,HEIGHT))

(DEFMACRO BLINKER-SET-CHARACTER (BLINKER FONT CHAR)
  `(FUNCALL ,BLINKER ':SET-CHARACTER ,CHAR ,FONT))

(DEFMACRO BLINKER-SET-SHEET (BLINKER SHEET)
  `(FUNCALL ,BLINKER ':SET-SHEET ,SHEET))

(DEFFLAVOR RECTANGULAR-BLINKER
	((WIDTH NIL)		;The width
	 (HEIGHT NIL))
	(BLINKER)
  (:INITABLE-INSTANCE-VARIABLES WIDTH HEIGHT))

(DEFFLAVOR MOUSE-BLINKER-MIXIN ((X-OFFSET 0) (Y-OFFSET 0)) ())


;;;Who line variables
(DEFVAR WHO-LINE-WINDOW)	;Sheet used for writing the who line
(DEFVAR WHO-LINE-RUN-STATE "")	;Variable containing the current state (RUN, STOP, TYI, etc.)
;(DEFVAR WHO-LINE-RUN-LIGHT-LOC);Contains the address of the run light under the who line
		;in COLD actually
(DEFVAR WHO-LINE-LIST)		;List of WHO-LINE-ITEM's, see DEFSTRUCT below

(DEFSTRUCT (WHO-LINE-ITEM :LIST (:CONSTRUCTOR NIL))  ;One for each field of the who-line
   WHO-LINE-ITEM-FUNCTION	;Function to be called, see WHO-LINE-UPDATE
   WHO-LINE-ITEM-STATE		;Previous contents, to save time
   WHO-LINE-ITEM-LEFT		;Left-hand bit address
   WHO-LINE-ITEM-RIGHT)		;Right-hand bit address
				;More fields may exist, depending on the function

;Fonts.
;A font array may not be displaced or anything hairy like that, because
;it is looked at directly by microcode.
;Its array leader contains:

(DEFSTRUCT (FONT :NAMED :ARRAY-LEADER)
	FONT-FILL-POINTER	;0 not used in case might be fill pointer?
	FONT-NAME		;Name.  This is supposed to be the symbol in the FONTS
				; package whose value is this font.
	FONT-CHAR-HEIGHT	;Character cell height
	FONT-CHAR-WIDTH		;Character cell width (used if char-width-table is nil)
	FONT-RASTER-HEIGHT	;Raster height
	FONT-RASTER-WIDTH	;Raster width
	FONT-RASTERS-PER-WORD	;Floor 32./raster width (# rows per word)
	FONT-WORDS-PER-CHAR	;Ceiling raster height/#5 (# words per char)
	FONT-BASELINE		;# Raster lines down from top to align with other fonts
	FONT-CHAR-WIDTH-TABLE	;NIL or array pointer to character width table
	FONT-LEFT-KERN-TABLE	;NIL or array pointer to left kern table
	FONT-INDEXING-TABLE	;NIL or array pointer to index table.  This is
				;used for characters whose raster is > 16. wide.
				;Use real char code to look up char code for
				;raster purposes in the indexing table.  Draw
				;several columns, until raster for succeeding
				;character is reached.  Index table must be 201
				;entries long so as to handle end condition right.
	FONT-NEXT-PLANE		;NIL or font containing next higher plane of this font.
	FONT-BLINKER-WIDTH	;Default width for blinkers.
	FONT-BLINKER-HEIGHT	;Default height for blinkers.
	FONT-CHARS-EXIST-TABLE	;Array of bits saying which chars "really exist".
				;NIL means not in use, so all chars must be assumed to exist.
;If changes are made, be sure to change LMIO1;FCMP > and recompile all the fonts
)

;The data part of the array contains an integral number of words
;per character.  Each word contains an integral number of rows
;of raster, right adjusted and processed from right to left.
;All 32 bits of each Q in this array are used.  For easiest processing
;by Lisp programs, it should be of 1-bit byte array type.

;%DRAW-CHAR only works for raster widths of at most 32.
;because that is the most that can be shifted without overlapping 3 TV buffer words.
;For larger widths it traps to ILLOP.  Wider characters are drawn
;by drawing several narrow characters side by side.  See the comment
;next to FONT-INDEXING-TABLE for how this is done.

;Named-structure handler, this also makes (TYPEP x 'FONT) happy.
(DEFUN FONT (OP SLF &OPTIONAL ARG1 &REST IGNORE)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
    ((:PRINT :PRINT-SELF) (FORMAT ARG1 "#<FONT ~A ~O>" (FONT-NAME SLF) (%POINTER SLF)))
    (OTHERWISE (FERROR NIL "~S unknown message to ~S" OP SLF))))

;;; Grab the lock on a sheet
(DEFMACRO LOCK-SHEET ((SHEET) . BODY)
  `(UNWIND-PROTECT
     (PROGN (SHEET-GET-LOCK ,SHEET)
       . ,BODY)
     (SHEET-RELEASE-LOCK ,SHEET)))

;;; Open up a sheet
(DEFVAR PREPARED-SHEET NIL)
(DEFMACRO PREPARE-SHEET ((SHEET) . BODY)
  `(LET ((INHIBIT-SCHEDULING-FLAG T))
     (AND (OR (NEQ PREPARED-SHEET ,SHEET) (NOT (SHEET-CAN-GET-LOCK ,SHEET)))
	  (SHEET-PREPARE-SHEET-INTERNAL ,SHEET))
     (SETQ PREPARED-SHEET ,SHEET)
     . ,BODY))

;;; Redirects a screen array
(DEFMACRO REDIRECT-ARRAY (&REST ARGS)
  `(WITHOUT-INTERRUPTS
     (SETQ SYS:%CURRENT-SHEET NIL)
     (SI:CHANGE-INDIRECT-ARRAY . ,ARGS)))

;;; Force access to a sheet and execute the code within.  If access cannot be
;;; forced, then the code is not executed.  Forcing access means binding off
;;; the output hold flag if the sheet is deexposed and has a bit-save array.
;;; The code is also executed of the sheet is exposed and not output-held.
(DEFMACRO SHEET-FORCE-ACCESS ((SHEET DONT-PREPARE-SHEET) . BODY)
  `(LOCK-SHEET (,SHEET)
       ;; Sheet can't have temporary lock here as we own lock, so SHEET-OUTPUT-HELD-P not
       ;; required for proper operation
       (LET ((.OLD.OUTPUT.HOLD. (SHEET-OUTPUT-HOLD-FLAG ,SHEET)))
	 (COND ((SHEET-SCREEN-ARRAY ,SHEET)
		(UNWIND-PROTECT
		  (PROGN (SETF (SHEET-OUTPUT-HOLD-FLAG ,SHEET) 0)
			 . ,(IF DONT-PREPARE-SHEET
				BODY
				`((PREPARE-SHEET (,SHEET) . ,BODY))))
		  (SETF (SHEET-OUTPUT-HOLD-FLAG ,SHEET) .OLD.OUTPUT.HOLD.)))))))

;;; I/O buffer stuff
(DEFSTRUCT (IO-BUFFER :ARRAY-LEADER :NAMED (:CONSTRUCTOR NIL))
  IO-BUFFER-FILL-POINTER			;Fill pointer, unused
  IO-BUFFER-SIZE				;Size of IO buffer (max index + 1)
						; All ptr's are mod this
  IO-BUFFER-INPUT-POINTER			;Index in which data is next stored
  IO-BUFFER-OUTPUT-POINTER			;Index from which data is next to be taken
						; If out ptr = inp ptr, then the buffer
						; is empty.  If inp ptr + 1 = out ptr, then
						; the buffer is full (This wastes a location
						; when the buffer is full)
						;Actual pointer manipulation should be done
						; with interrupts disabled.
  IO-BUFFER-INPUT-FUNCTION			;If non-NIL, function to be run on inputing
						; data
  IO-BUFFER-OUTPUT-FUNCTION			;If non-NIL, function to be run when taking
						; data out
  IO-BUFFER-STATE				;NIL means ok to put data in
						;T means data may not be put in or taken out
						;:INPUT means data may only be put in
						;:OUTPUT means data may only be taken out
  IO-BUFFER-PLIST				;Property list used to hold various bits of
						; information about the buffer (e.g. whether
						; in super image mode)
  IO-BUFFER-LAST-INPUT-PROCESS			;The last process that did input to here
  IO-BUFFER-LAST-OUTPUT-PROCESS			;The last process that did output from here
  )

(DEFMACRO IO-BUFFER-EMPTY-P (BUFFER)
  `(= (IO-BUFFER-INPUT-POINTER ,BUFFER)
      (IO-BUFFER-OUTPUT-POINTER ,BUFFER)))

(DEFMACRO IO-BUFFER-FULL-P (BUFFER)
  ;; Always leave room for at one unget to be done
  `(= (\ (+ (IO-BUFFER-INPUT-POINTER ,BUFFER) 2) (IO-BUFFER-SIZE ,BUFFER))
      (IO-BUFFER-OUTPUT-POINTER ,BUFFER)))

(DEFMACRO WITH-SHEET-DEEXPOSED ((SHEET) . BODY)
  `(LET ((.STATUS. (FUNCALL ,SHEET ':STATUS)))
     (DELAYING-SCREEN-MANAGEMENT
      (UNWIND-PROTECT
	(PROGN (FUNCALL ,SHEET ':DEEXPOSE ':DEFAULT ':NOOP)
	       . ,BODY)
	(FUNCALL ,SHEET ':SET-STATUS .STATUS.)))))

(DEFMACRO WINDOW-BIND ((WINDOW NEW-TYPE . INIT-PAIRS) . BODY)
  "Change the type of a window within the body."
  (CHECK-ARG WINDOW SYMBOLP "a symbol which is set to a window")
    `(LET ((.O.WINDOW. ,WINDOW) (.N.WINDOW.) (,WINDOW ,WINDOW) (TERMINAL-IO TERMINAL-IO))
       (UNWIND-PROTECT
	 (PROGN (SETQ .N.WINDOW. (WINDOW-PUSH ,WINDOW ,NEW-TYPE . ,INIT-PAIRS))
		(SETQ ,WINDOW .N.WINDOW.)
		(AND (EQ .O.WINDOW. TERMINAL-IO) (SETQ TERMINAL-IO ,WINDOW))
	   . ,BODY)
	 (AND .N.WINDOW. (WINDOW-POP .O.WINDOW. .N.WINDOW.)))))

;;;Temporarily select a window
(DEFMACRO WINDOW-CALL ((WINDOW FINAL-ACTION . FINAL-ACTION-ARGS) . BODY)
  `(LET ((.CURRENT-WINDOW. SELECTED-WINDOW))
     (UNWIND-PROTECT
       (PROGN
	 (FUNCALL ,WINDOW ':SELECT)
	 . ,BODY)
       (AND .CURRENT-WINDOW. (FUNCALL .CURRENT-WINDOW. ':SELECT NIL))
       ,(AND FINAL-ACTION `(FUNCALL ,WINDOW ',FINAL-ACTION . ,FINAL-ACTION-ARGS)))))

(DEFMACRO WINDOW-MOUSE-CALL ((WINDOW FINAL-ACTION . FINAL-ACTION-ARGS) . BODY)
  `(LET ((.CURRENT-WINDOW. SELECTED-WINDOW))
     (UNWIND-PROTECT
       (PROGN
	 (FUNCALL ,WINDOW ':MOUSE-SELECT)
	 . ,BODY)
       (AND .CURRENT-WINDOW. (FUNCALL .CURRENT-WINDOW. ':SELECT NIL))
       ,(AND FINAL-ACTION `(FUNCALL ,WINDOW ',FINAL-ACTION . ,FINAL-ACTION-ARGS)))))

;;;Maybe this should go somewhere else
(DEFMACRO DOPLIST ((PLIST PROP IND) . BODY)
  `(DO ((PLIST ,PLIST (CDDR PLIST))
	(,PROP)
	(,IND))
       ((NULL PLIST))
     (SETQ ,IND (CAR PLIST)
	   ,PROP (CADR PLIST))
     . ,BODY))

(DEFMACRO DEFRESOURCE (NAME . CREATOR)
  (LET ((NOT-FIRST-TIME (AND (LISTP NAME) (SECOND NAME))))
    (AND (LISTP NAME) (SETQ NAME (FIRST NAME)))
    `(PROGN 'COMPILE
       (DECLARE (SPECIAL ,NAME))
       (SI:SETQ-IF-UNBOUND ,NAME NIL)
       (DEFUN ,NAME () . ,CREATOR)
       ,(OR NOT-FIRST-TIME
	    `(AND (NULL ,NAME) (SETQ ,NAME (NCONS (,NAME))))))))

(DEFUN ALLOCATE-RESOURCE (NAME &AUX VAL)
  (OR (AND (BOUNDP NAME) (FBOUNDP NAME))
      (FERROR NIL "~S is not a known resource" NAME))
  (OR (WITHOUT-INTERRUPTS
       (PROG1 (CAR (SETQ VAL (SYMEVAL NAME))) (SET NAME (CDR VAL))))
      (FUNCALL (FSYMEVAL NAME))))

(DEFUN DEALLOCATE-RESOURCE (NAME RESOURCE)
  (WITHOUT-INTERRUPTS
    (SET NAME (CONS RESOURCE (SYMEVAL NAME)))))

(DEFMACRO WITH-RESOURCE ((NAME VAR) . BODY)
  `(LET ((,VAR NIL))
     (UNWIND-PROTECT
       (PROGN
	(SETF ,VAR (ALLOCATE-RESOURCE ',NAME))
	. ,BODY)
       (AND ,VAR (DEALLOCATE-RESOURCE ',NAME ,VAR)))))

;;; Defintions for screen management
(DEFMACRO RECT-SOURCE (R) `(FIRST ,R))
(DEFMACRO RECT-LEFT (R) `(SECOND ,R))
(DEFMACRO RECT-TOP (R) `(THIRD ,R))
(DEFMACRO RECT-RIGHT (R) `(FOURTH ,R))
(DEFMACRO RECT-BOTTOM (R) `(FIFTH ,R))
(DEFMACRO RECT-WITHIN-RECT-P (R1 R2)
  "R1 within R2"
  `(AND ( (RECT-LEFT ,R1) (RECT-LEFT ,R2))
	( (RECT-RIGHT ,R1) (RECT-RIGHT ,R2))
	( (RECT-TOP ,R1) (RECT-TOP ,R2))
	( (RECT-BOTTOM ,R1) (RECT-BOTTOM ,R2))))

(DEFMACRO RECT-NOT-OVERLAP-RECT-P (R1 R2)
  `(OR ( (RECT-RIGHT ,R2) (RECT-LEFT ,R1))
       ( (RECT-RIGHT ,R1) (RECT-LEFT ,R2))
       ( (RECT-BOTTOM ,R2) (RECT-TOP ,R1))
       ( (RECT-BOTTOM ,R1) (RECT-TOP ,R2))))

(DEFVAR SCREEN-MANAGER-QUEUE NIL)
(DEFVAR SCREEN-MANAGER-TOP-LEVEL T)

;(DEFMACRO DELAYING-SCREEN-MANAGEMENT (&REST BODY)
;  "Collect any screen manages that get queued during its body,
;and force them to happen at the later.  This code is unwind-
;protected so that all pending manages get done, as they are
;necessary to have the screen look correct.  The code tries to
;remove duplicate screen manages when it finally does them, and
;after it finishes all the managing does an autoexpose on all
;superiors that it hacked."
;  `(LET ((INHIBIT-SCREEN-MANAGEMENT T))
;     (AND SCREEN-MANAGER-TOP-LEVEL
;	  (BIND (VALUE-CELL-LOCATION 'SCREEN-MANAGER-QUEUE) NIL))
;     (UNWIND-PROTECT
;       (LET ((SCREEN-MANAGER-TOP-LEVEL NIL))
;	 . ,BODY)
;       (AND SCREEN-MANAGER-TOP-LEVEL (SCREEN-MANAGE-DEQUEUE-DELAYED-ENTRIES)))))

(DEFMACRO DELAYING-SCREEN-MANAGEMENT (&REST BODY)
  "Collect any screen manages that get queued during its body,
and force them to happen at the later.  This code is unwind-
protected so that all pending manages get done, as they are
necessary to have the screen look correct.  The code tries to
remove duplicate screen manages when it finally does them, and
after it finishes all the managing does an autoexpose on all
superiors that it hacked."
  `(LET ((INHIBIT-SCREEN-MANAGEMENT T)
	 (.RESULT.)
	 (.EXIT.QUEUE. NIL))
     (LET ((SCREEN-MANAGER-QUEUE NIL)
	   (SCREEN-MANAGER-TOP-LEVEL NIL))
       (UNWIND-PROTECT
	 (SETQ .RESULT. (PROGN . ,BODY)
	       .EXIT.QUEUE. SCREEN-MANAGER-QUEUE)
	 (OR .EXIT.QUEUE.
	     (NULL SCREEN-MANAGER-QUEUE)
	     (SCREEN-MANAGE-DEQUEUE-DELAYED-ENTRIES))))
     (AND .EXIT.QUEUE.
	  (SCREEN-MANAGE-DELAYING-SCREEN-MANAGEMENT-INTERNAL .EXIT.QUEUE.))
     .RESULT.))

(DEFMACRO WITHOUT-SCREEN-MANAGEMENT (&REST BODY)
  "This causes any screen manages that get queued during its
body to get flushed if the body exits normally.  Abnormal exit
will cause the screen manages to remain on the queue so that they
do get done.  This is useful in circumstances when you know
you'll be doing screen management on the same stuff right away."
  `(LET ((.FLAG. T)
	 (INHIBIT-SCREEN-MANAGEMENT T))
     (LET ((SCREEN-MANAGER-QUEUE NIL)
	   (SCREEN-MANAGER-TOP-LEVEL NIL))
       (UNWIND-PROTECT
	 (PROGN
	   ,@BODY
	   ;; Body completed successfully, flag it
	   (SETQ .FLAG. NIL))
	 (AND .FLAG.
	      ;; Body didn't complete successfully, hack the queue unless delaying
	      (IF (NOT SCREEN-MANAGER-TOP-LEVEL)
		  (SETQ .FLAG. SCREEN-MANAGER-QUEUE)
		  (SCREEN-MANAGE-DEQUEUE-DELAYED-ENTRIES)
		  (SETQ .FLAG. NIL)))))
     (DOLIST (E .FLAG.)
       ;; Requeue entries
       (LEXPR-FUNCALL #'SCREEN-MANAGE-QUEUE (FIRST (FIRST E)) (CDR E)))))

;;; Macros to help out the squeaking furry things
;;; Stop handling (but continue tracking) the mouse.  Things that use this must set the mouse
;;; blinker right away, by (MOUSE-STANDARD-BLINKER) or otherwise.
(DEFMACRO WITH-MOUSE-GRABBED (&REST BODY)
  `(LET ((.OLD.VALUE. WINDOW-OWNING-MOUSE))
     (UNWIND-PROTECT
       (PROGN
	 (WITH-MOUSE-GRABBED-INTERNAL T)
	 . ,BODY)
       (SETQ WINDOW-OWNING-MOUSE .OLD.VALUE.)
       (SETQ MOUSE-RECONSIDER T))))

;;; Stop handling and tracking of the mouse completely
(DEFMACRO WITH-MOUSE-USURPED (&REST BODY)
  `(LET ((.OLD.VALUE. WINDOW-OWNING-MOUSE))
     (UNWIND-PROTECT
       (PROGN
	 (WITH-MOUSE-GRABBED-INTERNAL 'STOP)
	 . ,BODY)
       (SETQ WINDOW-OWNING-MOUSE .OLD.VALUE.)
       (SETQ MOUSE-RECONSIDER T))))

;; Tell the mouse process to switch "modes" and wait for it to do so
(DEFUN WITH-MOUSE-GRABBED-INTERNAL (WOM)
  (SETQ WINDOW-OWNING-MOUSE WOM)
  (SETQ MOUSE-RECONSIDER T)
  (MOUSE-WAKEUP)
  (PROCESS-WAIT "Grab Mouse" #'(LAMBDA (WOM) (AND (NULL MOUSE-RECONSIDER)
						  (EQ MOUSE-WINDOW WOM)))
			     WOM))
