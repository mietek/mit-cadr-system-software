;This file defines the data structures used by the TV routines.		-*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;A font array may not be displaced or anything hairy like that, because
;it is looked at directly by microcode.
;Note that 16-bit and 32-bit screens must use different fonts.  In the 16-bit
;case, both the bits within a raster row and the raster rows within a word
;are ordered left to right, which is contrary to the normal way we do things.
;For now, getting the right font is entirely up to the user.  Fixed up later. ---
;Its array leader contains:

(DEFSTRUCT (FONT :ARRAY-LEADER (:DEFAULT-POINTER FONT) :NAMED)
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
;of raster, left adjusted and processed from left to right.
;(Right-adjusted and processed right to left in the 32-bit case.)
;All 32 bits of each Q in this array are used.  For easiest processing
;by Lisp programs, it should be of 1-bit byte array type.

;TV-DRAW-CHAR only works for raster widths of at most 16 (decimal) (32 for 32-bit TVs).
;because that is the most that can be shifted without overlapping 3 TV buffer words.
;For larger widths it traps to ILLOP.  Wider characters are drawn
;by drawing several narrow characters side by side.  See the comment
;next to FONT-INDEXING-TABLE for how this is done.

;A screen is a data structure which represents a physical display monitor.
(DEFSTRUCT (SCREEN :ARRAY :NAMED)
	SCREEN-NAME
	SCREEN-PLANE-MASK	;Bits for planes.  0 => new 32-bit TV system.
	SCREEN-HEIGHT		;Number of bits high counting who-line
	SCREEN-WIDTH		;Number of bits wide
	SCREEN-BITS-PER-PIXEL	;For gray or color
	SCREEN-PROPERTY-LIST
	SCREEN-FONT-ALIST	;Associate from names to font objects.  NYI.
	SCREEN-X1		;These are the bounds in which things get
	SCREEN-Y1		;  auto-allocated.  The idea is that they do
	SCREEN-X2		;  not include the who-line.
	SCREEN-Y2
	SCREEN-BUFFER		;Virtual memory address of video buffer
	SCREEN-LOCATIONS-PER-LINE ;Number of locations per raster line (microcode use)
	SCREEN-BUFFER-PIXEL-ARRAY ;Two-dimensional width x height array mapped onto buffer
	SCREEN-BUFFER-HALFWORD-ARRAY ;One-dimensional array of 16-bit buffer hunks
				;On old 16-bit TVs, the bits in this array are reversed.
	SCREEN-DEFAULT-FONT	  ;Font to be used if none specified
        SCREEN-CONTROL-ADDRESS	;XBUS I/O address of control register
)

;********** IF YOU CHANGE THE FORMAT OF A PC-PPR OR *****************
;********** OF A BLINKER, YOU MAY HAVE TO RECOMPILE A LOT OF PROGRAMS

;A piece of paper (PC-PPR) is an area on the TV screen on which
;characters may be drawn, sequentially in imitation of a TTY.  There can
;be any number of pieces of paper, in various shapes (rectangular), sizes, fonts,
;and black-on-white modes.  Each PC PPR may have a number of blinking
;cursors.  Before a piece of paper can be written on, it must be
;"opened," which means turning off its blinkers.  See the macro TV-PREPARE-PC-PPR below.
;When done with the piece of paper, a clock routine will close it.

;A piece of paper is represented as an ordinary array whose elements are:
(DEFSTRUCT (PC-PPR :ARRAY :NAMED)
	PC-PPR-NAME		;Name
	PC-PPR-SCREEN		;Which screen it is on
	PC-PPR-TOP		;Raster line # of topmost line
	PC-PPR-TOP-MARGIN	;Raster line # of topmost line used to draw chars
	PC-PPR-BOTTOM		;Raster line # of bottomost line + 1
	PC-PPR-BOTTOM-MARGIN	;Raster line # of bottomost line for chars + 1
	PC-PPR-LEFT		;Bit # of leftmost bit
	PC-PPR-LEFT-MARGIN	;Bit # of leftmost bit used to draw chars
	PC-PPR-RIGHT		;Bit # of rightmost bit + 1
	PC-PPR-RIGHT-MARGIN	;Bit # of rightmost bit for drawing chars + 1
	PC-PPR-RIGHT-LIMIT	;Char may not start to right of here
	PC-PPR-BOTTOM-LIMIT	;Line may not start below here
	PC-PPR-CURRENT-X	;X position at which to draw next character (left)
	PC-PPR-CURRENT-Y	;Y position at which to draw next character (top)
	((PC-PPR-FLAGS NIL 0)	;A fixnum containing various bit flags
	 (PC-PPR-SIDEWAYS-P 0001) ;0 normal, 1 sideways (exch X & Y before calling microcode)
	 (PC-PPR-EXCEPTIONS 0104) ;Reasons why typeout cannot happen:
	  (PC-PPR-END-LINE-FLAG 0101) ;Cursor is to right of right-limit (must be low bit)
	  (PC-PPR-END-PAGE-FLAG 0201) ;Cursor is below bottom-limit
	  (PC-PPR-MORE-FLAG 0301) ;More processing needs to happen
	  (PC-PPR-OUTPUT-HOLD-FLAG 0401)) ;User-controllable
      	PC-PPR-MORE-VPOS	;Y passing here triggers MORE processing.
				;Add 100000 to this field to delay until after screen wrap.
				;Store NIL here to inhibit MORE processing.
	PC-PPR-BASELINE		;# raster lines from top of char cell to baseline.
	PC-PPR-FONT-MAP		;Map from font numbers to font arrays
	PC-PPR-CURRENT-FONT	;Currently selected font
	PC-PPR-BASELINE-ADJ	;Y offset for current font to align baseline
	PC-PPR-LINE-HEIGHT	;Total number of raster lines per character line
	PC-PPR-CHAR-WIDTH	;Character width for cursor blinker + (X,Y) positioning
	PC-PPR-CHAR-ALUF	;ALU function for drawing characters
	PC-PPR-ERASE-ALUF	;ALU function for erasing characters/lines/whole pc ppr
	PC-PPR-BLINKER-LIST	;Possibly null list of blinkers on this pc ppr
	PC-PPR-END-LINE-FCN	;Called when typeout reaches right limit
	PC-PPR-END-SCREEN-FCN	;Called when typeout reaches bottom limit
	PC-PPR-OUTPUT-HOLD-FCN	;Called when output-hold flag is encountered
	PC-PPR-MORE-FCN		;Called when more processing is triggered
)

;A blinker is an array, described as follows:
(DEFSTRUCT (TV-BLINKER :ARRAY (:DEFAULT-POINTER TV-BLINKER) :NAMED)
	TV-BLINKER-X-POS	;X position of blinker (left) NIL if should follow pc-ppr
	TV-BLINKER-Y-POS	;Y position of blinker (top)
	TV-BLINKER-PC-PPR	;PC PPR associated with
				;If this is NIL, it's a roving blinker, on whole screen
	TV-BLINKER-VISIBILITY	;NIL invisible, T visible, BLINK blinking
		;This next cell is no longer used and will be flushed some day
	TV-BLINKER-SAVE-VISIBILITY	;Visibility saved here when made invisible temporarily
	TV-BLINKER-HALF-PERIOD	;Time interval (60ths) between phase blinks
	TV-BLINKER-PHASE	;NIL not visible, anything else visible in some form
				; (Complementing blinker has only two phases, uses NIL, T)
	TV-BLINKER-TIME-UNTIL-BLINK ;Time interval until next blink.  NIL means not blinking,
				; the clock level should ignore this blinker.
	TV-BLINKER-FUNCTION	;Function to call to induce blinking, next two are for its use
				;Args: blinker, fcn={NIL (OFF),T (ON),BLINK}, X, Y
	TV-BLINKER-WIDTH	;Width in bits of area to complement if TV-RECTANGULAR-BLINKER
	TV-BLINKER-HEIGHT	;Height in raster lines of area to complement (..)
	TV-BLINKER-SCREEN	;Screen object on which it displays. (formerly PLANE-MASK)
	TV-BLINKER-SIDEWAYS-P	;Interchange X and Y before calling microcode
)

;Note:  If a blinker's TV-BLINKER-FUNCTION uses the TV-BLINKER-HEIGHT and TV-BLINKER-WIDTH
;in an unusual way (as does TV-CHARACTER-BLINKER),
;then the blinker function name must have a TV-SET-BLINKER-SIZE-FUNCTION property
;which is a function of (BLINKER HEIGHT WIDTH) that alters the blinker size to that specified,
;if that is possible, or does nothing.

(DEFVAR TV-ROVING-BLINKER-LIST NIL) ;List of all visible blinkers not connected to any pc ppr.
(DEFVAR SELECTED-PC-PPRS NIL)	;These pc-pprs can have their blinkers blink.
(DEFVAR EXPOSED-PC-PPRS NIL)	;These pc-pprs have their blinkers permanently on,
				;but we have to know about them, since output is allowed
				;on them and will turn the blinkers off temporarily,
				;and we must check and turn them on again.

(DEFVAR TV-DEFAULT-SCREEN)	;Windows are created on this screen unless otherwise forced.
(DEFVAR TV-CPT-SCREEN)		;This is the 32-bit TV screen.

(SPECIAL TV-ALU-IOR		;Op-code for tv output to turn bits on.
	 TV-ALU-ANDCA		;Op-code for tv output to turn bits off (erase, usually).
	 TV-ALU-XOR		;Op-code for tv output to flip bits over.
	 TV-ALU-SETA)		;Op-code for tv output for setting bits, ignoring prev. values

(DEFVAR TV-BEEP T)		;T => the function TV-BEEP should not flash the screen.
(DEFVAR TV-BEEP-DURATION 400000)
(DEFVAR TV-BEEP-WAVELENGTH 1350)

;If this is NIL, no pc-ppr does **MORE**.  Complemented by Esc M.
(DEFVAR TV-MORE-PROCESSING-GLOBAL-ENABLE T)

(DEFVAR RUBOUT-HANDLER-CONTROL-CHARACTER-HOOK)

;;;WHO LINES

(SPECIAL TV-WHO-LINE-PC-PPR	;PC PPR used for writing the who line
	 TV-WHO-LINE-STREAM	;Corresponding stream
	 TV-WHO-LINE-PROCESS	;This process is reflected in TV-WHO-LINE-RUN-STATE
	 TV-WHO-LINE-RUN-STATE	;Variable containing the current state (RUN, STOP, TYI, etc.)
	 TV-WHO-LINE-PROGRAM-NAME ;Variable containing the program name to appear
	 TV-WHO-LINE-RUN-LIGHT-LOC  ;Contains the address of the run light under the who line
	 TV-WHO-LINE-LIST	;List of TV-WHO-LINE-ITEM's, see DEFSTRUCT below
)

(DEFSTRUCT (TV-WHO-LINE-ITEM :LIST (:CONSTRUCTOR NIL))  ;One for each field of the who-line
   TV-WHO-LINE-ITEM-FUNCTION	;Function to be called, see TV-WHO-LINE-UPDATE
   TV-WHO-LINE-ITEM-STATE	;Previous contents, to save time
   TV-WHO-LINE-ITEM-LEFT	;Left-hand bit address
   TV-WHO-LINE-ITEM-RIGHT)	;Right-hand bit address
				;More fields may exist, depending on the function

;;; This macro is used to prepare to operate on a piece of paper.
;;; It selects the appropriate screen and makes sure that no blinkers
;;; can be visible in the area of the screen occupied by this pc ppr.
;;; Interrupts are inhibited so that the clock level cannot turn
;;; blinkers back on, and to keep other processes from trying to
;;; use the same pc ppr at the same time.

(DEFMACRO TV-PREPARE-PC-PPR ((PC-PPR) . BODY)	;PC-PPR is variable containing pc ppr
  `(LET ((INHIBIT-SCHEDULING-FLAG T))		;Prevents anyone messing over blinkers etc.
     (OR (ZEROP (PC-PPR-OUTPUT-HOLD-FLAG ,PC-PPR))
	 (FUNCALL (PC-PPR-OUTPUT-HOLD-FCN ,PC-PPR) ,PC-PPR))
     ;; Turn off all blinkers that can interfere with this pc ppr
     (DO L TV-ROVING-BLINKER-LIST (CDR L) (NULL L)
       (AND (TV-BLINKER-PHASE (CAR L))		;Check visibility here for speed
	    (TV-OPEN-BLINKER (CAR L))))		;Blinker is visible, get rid of him
     (DO L (PC-PPR-BLINKER-LIST ,PC-PPR) (CDR L) (NULL L)
       (AND (TV-BLINKER-PHASE (CAR L))		;Check visibility here for speed
	    (TV-OPEN-BLINKER (CAR L))))
     (TV-SELECT-SCREEN (PC-PPR-SCREEN ,PC-PPR))
     . ,BODY))
