;;; New (July 1978) Menu system				-*-Mode:Lisp; Package:SI-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;Documentation on menu item-lists:
;
;Each item in the item-list may be one of the following:
; 1. A string (or a symbol).  
; 2. Cons of a string (or a symbol) and an atom.
; 3. List of a string (or a symbol) and any object.  The list may
;    not be more than 2 long.
; 4. List of a string (or a symbol), a flavor keyword, and an argument.
;    After the first 3 elements of the list, the rest of the list is modifier keywords.
;The string (or symbol) is displayed in the menu to represent this item.
;The value returned by the :CHOOSE method is the item in case 1, the cdr
;in case 2, the cadr in case 3, and varies in case 4 depending on the flavor.
;Case 4 menu items can also have side-effects.
;The following are the permissible flavor keywords:
;		:VALUE - argument is returned by the :CHOOSE method
;		:EVAL - argument is evaluated then returned
;		:FUNCALL - argument is a function of no args to be called
;		:NO-SELECT - this item cannot be selected
;		:WINDOW-OP - argument is a function of one argument.  This
;			argument is a list of window, mouse-x, mouse-y as they
;			were before the menu popped up.
;		:KBD - argument is forced into keyboard input of appropriate process.
;		:MENU - argument is a new menu to choose from.
;The following are the known modifier keywords:
;		:NO-AUTOEXPOSE - in momentary menus, don't expose any windows
;			due to de-exposing the menu before executing the chosen item.
;			This is assumed by default if the flavor is :MENU.
;
;This stuff is largely although not entirely controlled by the :EXECUTE method,
;which you may redefine.

(DEFCLASS MENU-CLASS WINDOW-CLASS
	  (ITEM-LIST			;List of items being displayed.
	   CURRENT-ITEM			;Item being pointed at now.
	   LAST-ITEM			;The last item to have been selected.
	   CHOSEN-ITEM			;The same, but it's ok to set this to NIL
					;and wait for it to become non-NIL.
	   CHOICE-RESULT		;Value returned by :EXECUTE operation.
	   LEFT-MARGIN TOP-MARGIN	;Boundaries of menu not including the margins
	   RIGHT-MARGIN BOTTOM-MARGIN	;or the label.
	   LABEL			;The string to be printed as a label, or NIL.
	   LABEL-POSITION		;A list of left, top, right and bottom boundaries
					;of the region to hold the label.
	   SCREEN-ROWS			;Number of rows in menu on screen;
	   TOTAL-ROWS			;Total number of rows in menu.
					;If this is greater than SCREEN-ROWS, then the latter
					;represent a window on all the rows.
	   TOP-ROW			;This is first row visible now.
	   ROW-HEIGHT			;Height in dots of a row (including vsp).
	   ROW-MAP			;Array of tails of ITEM-LIST.  For each row
					;in the menu, points to first item on that row.
					;An extra element at the end is NIL.
					;The length is thus (1+ TOTAL-ROWS).
	   COLUMNS			;Number of columns (NIL in fill mode).
	   COLUMN-WIDTH			;Width in dots of a column (NIL in fill mode).
	   PC-PPR			;PC-PPR covering area inside box.
	   FILL-P			;T => fill mode.
	   ))

;A fill-mode menu doesn't have distinct columns;  it packs as many items as
;possible onto each row.

;;; These special variables exist so that there are less random numbers
;;; in the code, giving somewhat more chance of understanding it.
;;; You might even want to change them.
(DEFVAR MENU-INTERWORD-SPACING 12.)
(DEFVAR MENU-FILL-BREAKAGE 30.)

;Menus default to this font
(DECLARE (SPECIAL FONTS:MEDFNT))

(ENDF HEAD)

;; If you specify the item-list in the :NEW message, then the geometry
;; is computed automatically.  It uses the specified number of columns, or 1.
;; Fill mode is used if FILL-P was specified as non-NIL.
;; If you specify a left and right, it uses that width, in which case the
;; number of columns is deduced from that if it was not explicitly specified.
(DEFMETHOD (MENU-CLASS :BORN) (&AUX SPEC-WIDTH SPEC-HEIGHT)
    (AND LEFT RIGHT (SETQ SPEC-WIDTH (- RIGHT LEFT)))
    (AND TOP BOTTOM (SETQ SPEC-HEIGHT (- BOTTOM TOP)))
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':BORN)
    (AND ITEM-LIST
	 (FUNCALL SELF ':COMPUTE-GEOMETRY
		  SPEC-HEIGHT SPEC-WIDTH NIL
		  (OR COLUMNS (AND (NOT FILL-P) (NOT SPEC-WIDTH) 1)))))

(DEFMETHOD (MENU-CLASS :LABEL<-) (NEW-LABEL)
    (SETQ LABEL NEW-LABEL)
    (AND STATUS
	 (<- SELF ':UPDATE-LABEL)))

(DEFMETHOD (MENU-CLASS :LABEL-HEIGHT) ()
      (COND (LABEL (FONT-CHAR-HEIGHT (AR-1 (PC-PPR-FONT-MAP PC-PPR) 0)))
	    (T 0)))

;This is like the one for WINDOW-WITH-BOX-CLASS except that it puts the
;label in a different place, and adjusts margins differently.

;;; The vertical format is:
;;; Without a label:
;;;   4 dots of blank space (optionally containing a torn edge)
;;;   n rows of cruft
;;;   4 dots of blank space (optionally containing a torn edge)
;;; With a label:
;;;   2 dots of blank space
;;;   The label line, including its VSP
;;;   4 dots of blank space (optionally containing a torn edge)
;;;   n rows of cruft
;;;   4 dots of blank space (optionally containing a torn edge)
;;; Only :MARGINS knows the margins.  :EDGES<- knows how to figure out
;;; where the label goes.
(DEFMETHOD (MENU-CLASS :MARGINS) (&AUX (TEM (<- SELF ':LABEL-HEIGHT)))
    (LIST 2 (COND ((ZEROP TEM) 4) (T (+ TEM 6))) 2 4))

(DEFMETHOD (MENU-CLASS :EDGES<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM
				  &AUX (TEM (<- SELF ':LABEL-HEIGHT)))
    (<-AS WINDOW-CLASS ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (SETQ LABEL-POSITION
	  (COND (LABEL (LIST LEFT (+ TOP 2) RIGHT (+ TOP TEM 2)))
		(T (LIST LEFT TOP RIGHT TOP))))
    (LET ((MARGINS (<- SELF ':MARGINS)))
	 (<- SELF ':EDGES-INSIDE-BOX<-
	     (+ LEFT (CAR MARGINS))
	     (+ TOP (CADR MARGINS))
	     (- RIGHT (CADDR MARGINS))
	     (- BOTTOM (CADDDR MARGINS)))))

(DEFMETHOD (MENU-CLASS :ITEM-LIST<-) (NEW-ITEM-LIST)
   (SETQ ITEM-LIST NEW-ITEM-LIST
	 LAST-ITEM NIL
	 CURRENT-ITEM NIL)
   (AND LEFT-MARGIN
	(<- SELF ':EDGES-INSIDE-BOX<- LEFT-MARGIN TOP-MARGIN RIGHT-MARGIN BOTTOM-MARGIN)))

;Add an item to the menu.						;
(DEFMETHOD (MENU-CLASS :ADD-AN-ITEM) (NEW-ITEM &OPTIONAL DUPLICATES-OK
					       (GEOMETRY-PARMS (LIST NIL NIL NIL 1))
					       &AUX ITEM-NAME)
  (SETQ ITEM-NAME (COND ((OR (STRINGP NEW-ITEM)
			     (SYMBOLP NEW-ITEM)) NEW-ITEM)
			((CAR NEW-ITEM))))
  (<- SELF ':ITEM-LIST<-
      (CONS NEW-ITEM
	    (IF DUPLICATES-OK
		ITEM-LIST
		;; Remove any duplicates
		(DO ((ITEMS ITEM-LIST (CDR ITEMS))
		     (GOOD-ITEMS
		       NIL
		       (IF (STRING-EQUAL ITEM-NAME
					 (COND ((OR (STRINGP (CAR ITEMS))
						    (SYMBOLP (CAR ITEMS))) (CAR ITEMS))
					       ((CAAR ITEMS))))
			   
			   GOOD-ITEMS		;it was a duplicate, leave it out.
			   (CONS (CAR ITEMS) GOOD-ITEMS))))	;no duplicate.
		    ((NULL ITEMS) (NREVERSE GOOD-ITEMS))))))
  (LEXPR-FUNCALL '<- SELF ':COMPUTE-GEOMETRY GEOMETRY-PARMS))

;Delete an item from the menu.
(DEFMETHOD (MENU-CLASS :DELETE-AN-ITEM) (ITEM-NAME &OPTIONAL DELETE-ONLY-FIRST
						    (GEOMETRY-PARMS (LIST NIL NIL NIL 1)))
  (SETQ ITEM-NAME (STRING ITEM-NAME))
  (<- SELF ':ITEM-LIST<- (MENU-DELETE-ITEMS ITEM-NAME ITEM-LIST DELETE-ONLY-FIRST))
  (LEXPR-FUNCALL '<- SELF ':COMPUTE-GEOMETRY GEOMETRY-PARMS))

(DEFUN MENU-DELETE-ITEMS (ITEM-NAME ITEM-LIST DELETE-ONLY-FIRST)
  (COND ((NULL ITEM-LIST) NIL)
	((STRING-EQUAL ITEM-NAME
		       (COND ((OR (STRINGP (CAR ITEM-LIST))
				  (SYMBOLP (CAR ITEM-LIST))) (CAR ITEM-LIST))
			     ((CAAR ITEM-LIST))))
	 (COND (DELETE-ONLY-FIRST (CDR ITEM-LIST))
	       (T (MENU-DELETE-ITEMS ITEM-NAME (CDR ITEM-LIST) DELETE-ONLY-FIRST))))
	(T (CONS (CAR ITEM-LIST)
		 (MENU-DELETE-ITEMS ITEM-NAME (CDR ITEM-LIST) DELETE-ONLY-FIRST)))))

;; Args are just like those for WINDOW-WITH-PC-PPR-CLASS.
;; The defaults are different:  MEDFNT, no blinker, and no **MORE**.
(DEFMETHOD (MENU-CLASS :NEW-PC-PPR) (&OPTIONAL FONT-MAP &REST OPTIONS
                                     &AUX BLINKER)
    ;; Set up the pc-ppr now, but its dimensions are not yet established
    ;; This is just to get the font and the screen
    (SETQ PC-PPR (LEXPR-FUNCALL 'TV-DEFINE-PC-PPR
				NAME (OR (AND FONT-MAP	;crock to fix other crocks.
					      (COND ((ATOM FONT-MAP)(LIST FONT-MAP))
						    (T FONT-MAP)))
					 (LIST FONTS:MEDFNT))
				':BLINKER-P NIL
				':MORE-P NIL
				':SCREEN SCREEN
				OPTIONS))
    (SETQ ROW-HEIGHT (PC-PPR-LINE-HEIGHT PC-PPR))
    (SETF (PC-PPR-CHAR-ALUF PC-PPR) TV-ALU-XOR)
    ;; Make a blinker, for flashing text near the mouse
    (SETQ BLINKER (TV-DEFINE-BLINKER PC-PPR
				   ':FUNCTION 'TV-RECTANGULAR-BLINKER
				   ':VISIBILITY NIL ;Changed by :MOUSE-MOVES method.
				   ))
    (AND LEFT-MARGIN
	 (<- SELF ':EDGES-INSIDE-BOX<- LEFT-MARGIN TOP-MARGIN RIGHT-MARGIN BOTTOM-MARGIN)))

(DEFMETHOD (MENU-CLASS :COMPUTE-GEOMETRY) (&OPTIONAL HEIGHT WIDTH N-ROWS N-COLUMNS MAX-HEIGHT MAX-WIDTH)
    (OR ITEM-LIST (FERROR NIL "This menu has no ITEM-LIST yet"))
    (MULTIPLE-VALUE (HEIGHT WIDTH SCREEN-ROWS COLUMNS)
	    (MENU-DEDUCE-PARAMETERS HEIGHT WIDTH N-ROWS N-COLUMNS MAX-HEIGHT MAX-WIDTH))
    (LET ((MARGINS (<- SELF ':MARGINS)))
	 (SETQ HEIGHT (+ (CADR MARGINS) (CADDDR MARGINS) HEIGHT)
	       WIDTH (+ (CAR MARGINS) (CADDR MARGINS) WIDTH)))
    (<- SELF ':SIZE<- WIDTH HEIGHT)
    (LIST HEIGHT WIDTH))

;When the area available inside the box and label changes,
;we must recompute how many rows and columns we have, etc.
(DEFMETHOD (MENU-CLASS :EDGES-INSIDE-BOX<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM
					   &AUX (HEIGHT (- NEW-BOTTOM NEW-TOP))
						(WIDTH (- NEW-RIGHT NEW-LEFT)))
    (SETQ LEFT-MARGIN NEW-LEFT TOP-MARGIN NEW-TOP
	  RIGHT-MARGIN NEW-RIGHT BOTTOM-MARGIN NEW-BOTTOM)
    ;; Get the new N-ROWS and so forth.
    (MULTIPLE-VALUE (HEIGHT WIDTH SCREEN-ROWS COLUMNS)
		    (MENU-DEDUCE-PARAMETERS HEIGHT WIDTH NIL COLUMNS NIL NIL))
    (TV-REDEFINE-PC-PPR PC-PPR
			':LEFT LEFT-MARGIN ':TOP TOP-MARGIN
			':RIGHT RIGHT-MARGIN ':BOTTOM BOTTOM-MARGIN)
    ;; Correct the pc ppr
    (SETF (PC-PPR-BOTTOM-LIMIT PC-PPR)	;Compensate for damned variable-height lines
	  (PC-PPR-BOTTOM-MARGIN PC-PPR))
    ;; Recompute the row map and update the tvob
    (MULTIPLE-VALUE (ROW-MAP TOTAL-ROWS)
		    (MENU-COMPUTE-ROW-MAP))
    (SETQ TOP-ROW 0)
    (SETQ COLUMN-WIDTH
	  (AND (NOT FILL-P) (// (+ WIDTH MENU-INTERWORD-SPACING) COLUMNS))))

;;; This function, given a bunch of parameters some of which are NIL meaning
;;; unspecified, deduces the rest of the parameters from constraints.

;;; First, compute the geometry
;;;  (1) The user has supplied the width and the number of columns or fill-p, nothing special.
;;;  (2) The user has supplied the width, we compute the number of columns
;;;             by finding the widest string in the item-list.
;;;  (3) The user has not supplied the width, but has supplied n-columns, we compute width
;;;             again by finding the widest string in the item-list.
;;;  (4) The user has supplied neither width nor n-columns.
;;;    (4a) The user has, however, supplied height or n-rows, so we pick a suitable width
;;;         to make the entire menu come out to n-rows, depending on fill mode.  Then if
;;;         it doesn't fit, this width will be wider than the screen, and will be limited.
;;;    (4b) The user has supplied no geometry, it's up to us.
;;;         Compute the total width depending on fill-mode, then pick n-rows and
;;;         n-columns to make this a square array.  Then limit each to the available
;;;         area of the screen, in case the menu is too big to fit.

;;; Once the horizontal business has been straightened out, if we don't have the
;;; height already, we pick a height to make it all fit on the screen, and limit that
;;; if it is too big.  Note that fill-mode has a line-breakage problem, which will
;;; need to be solved here (may change shape "slightly" from square.)

;;; Arguments:
;;; PC-PPR, LABEL, ITEM-LIST and FILL-P are used freely.
;;; PC-PPR should have the right font, screen, vsp but not
;;;	   yet the right dimensions and location.
;;; NEW-LEFT, etc. are the boundaries of the area actually available for use.
;;;  Any margins have already been excluded.

(LOCAL-DECLARE ((SPECIAL PC-PPR ITEM-LIST FILL-P LABEL))
(DEFUN MENU-DEDUCE-PARAMETERS (HEIGHT WIDTH N-ROWS N-COLUMNS ;dependent
			       MAX-HEIGHT MAX-WIDTH
			       &AUX (LINE-HEIGHT (PC-PPR-LINE-HEIGHT PC-PPR))
			            COLUMN-WIDTH ;If non-null, N-COLUMNS was not user-supplied
						 ; so it may be recomputed if came out too wide
				    TEM
				    VWIDTH-FONT-SLOP
				    (N-ITEMS (LENGTH ITEM-LIST))
				    (SCREEN (PC-PPR-SCREEN PC-PPR)))
  ;;Calculate slop due to the PC-PPR-RIGHT-LIMIT for variable width fonts.
  ;;If we have a fixed width font then VWIDTH-FONT-SLOP is 0.
  (SETQ VWIDTH-FONT-SLOP (- (PC-PPR-RIGHT-MARGIN PC-PPR)
			  (PC-PPR-RIGHT-LIMIT PC-PPR)))
  ;; Realize any immediately clear implications
  (AND N-ROWS (NULL HEIGHT) (SETQ HEIGHT (* N-ROWS LINE-HEIGHT)))
  (AND HEIGHT (NULL N-ROWS) (SETQ N-ROWS (// HEIGHT LINE-HEIGHT)))
  (SETQ MAX-HEIGHT (MIN (OR HEIGHT MAX-HEIGHT 10000)
			(- (SCREEN-Y2 SCREEN) (SCREEN-Y1 SCREEN))))
  (SETQ MAX-WIDTH (MIN (OR WIDTH MAX-WIDTH 10000)
		       (- (SCREEN-X2 SCREEN) (SCREEN-X1 SCREEN))))

  ;; Compute the horizontal parameters.
  (COND ((AND WIDTH (OR N-COLUMNS FILL-P)) )		;It's fully-determined
	(WIDTH	;We have the width, and it's not in fill mode, compute N-COLUMNS
	 (SETQ N-COLUMNS (MAX (// (+ WIDTH MENU-INTERWORD-SPACING)
				  (+ (MENU-MAX-WIDTH PC-PPR ITEM-LIST)
				     MENU-INTERWORD-SPACING VWIDTH-FONT-SLOP))
			      1)))
	(N-COLUMNS  ;We don't have the width, but do know how many columns, compute width
	 (SETQ WIDTH (MIN (- (* (+ (MENU-MAX-WIDTH PC-PPR ITEM-LIST)
				   VWIDTH-FONT-SLOP MENU-INTERWORD-SPACING)
				N-COLUMNS)
			     MENU-INTERWORD-SPACING)
			  MAX-WIDTH)))
	(N-ROWS  ;We know how high, make it wide enough to come out this high
	 (COND (FILL-P (SETQ WIDTH (MIN (// (+ (MENU-FILL-WIDTH PC-PPR ITEM-LIST)
					       (1- N-ROWS))
					    N-ROWS)
					MAX-WIDTH)))
	       (T (SETQ N-COLUMNS (// (+ N-ITEMS (1- N-ROWS)) N-ROWS)
			WIDTH (- (* (SETQ COLUMN-WIDTH (+ (MENU-MAX-WIDTH PC-PPR ITEM-LIST)
							  VWIDTH-FONT-SLOP
							  MENU-INTERWORD-SPACING))
				    N-COLUMNS)
				 MENU-INTERWORD-SPACING)))))
	((NOT FILL-P) ;No geometry supplied, pick N-ROWS and N-COLUMNS to make it square
	 (SETQ TEM (* (SETQ COLUMN-WIDTH (+ VWIDTH-FONT-SLOP
					    (MENU-MAX-WIDTH PC-PPR ITEM-LIST)))
                      N-ITEMS
                      LINE-HEIGHT)
	       N-ROWS (// (ISQRT TEM) LINE-HEIGHT)
	       N-COLUMNS (// (+ N-ITEMS (1- N-ROWS)) N-ROWS)
               N-ROWS NIL		;Force recomputation.  May fix rounding errors.
	       WIDTH (- (* (SETQ COLUMN-WIDTH (+ COLUMN-WIDTH MENU-INTERWORD-SPACING))
			   N-COLUMNS)
			MENU-INTERWORD-SPACING)))
	(T	;No geometry supplied, and in fill mode, make it square
	 (SETQ WIDTH (ISQRT (* (MENU-FILL-WIDTH PC-PPR ITEM-LIST) LINE-HEIGHT)))))

  ;; Now figure out the vertical characteristics
  (COND (N-ROWS)
        ((NULL WIDTH) (BREAK 'MENU-DEDUCE-PARAMETERS))
        (FILL-P 
         (SETQ N-ROWS (// (+ (MENU-FILL-WIDTH PC-PPR ITEM-LIST) WIDTH -1) WIDTH)))
        (T (SETQ N-ROWS (// (+ N-ITEMS N-COLUMNS -1) N-COLUMNS))))
  (OR HEIGHT (SETQ HEIGHT (* N-ROWS LINE-HEIGHT)))

  ;; If there is a label, the menu must be at least wide enough to accomodate it
  (AND LABEL
       (SETQ WIDTH (MAX WIDTH (+ VWIDTH-FONT-SLOP
				 (TV-STRING-LENGTH PC-PPR (STRING LABEL))))))

  ;; If this came out too high or too wide, retrench
  (AND (> HEIGHT MAX-HEIGHT)
       (SETQ N-ROWS (// MAX-HEIGHT LINE-HEIGHT)
	     HEIGHT (* N-ROWS LINE-HEIGHT)))
  (COND ((> WIDTH MAX-WIDTH)
	 (SETQ WIDTH MAX-WIDTH)
	 (AND COLUMN-WIDTH  ;If N-COLUMNS was not user-supplied, recompute it
	      (SETQ N-COLUMNS (MAX (// (+ WIDTH MENU-INTERWORD-SPACING)
				       COLUMN-WIDTH)
				   1)))))

  ;; At this point, WIDTH, HEIGHT, N-COLUMNS (if not FILL-P), and N-ROWS
  ;; are all valid and consistent, and not bigger than the available area,
  ;; provided that the user's original parameters were not illegally huge.

  ;; Return all the dependent parameters as multiple values
  (PROG NIL (RETURN HEIGHT WIDTH N-ROWS N-COLUMNS))))

;;; This function computes the ROW-MAP, which determines how many strings per line, & c.
;;; The first value is the row-map and the second is the n-total-rows
(LOCAL-DECLARE ((SPECIAL RIGHT-MARGIN LEFT-MARGIN COLUMNS ITEM-LIST FILL-P PC-PPR))
(DEFUN MENU-COMPUTE-ROW-MAP (&AUX (INSIDE-WIDTH (- RIGHT-MARGIN LEFT-MARGIN))
			          (MAP (MAKE-ARRAY NIL 'ART-Q (1+ (LENGTH ITEM-LIST))))
				  WID)
  (DO ((ITEMS ITEM-LIST) (ROW 0 (1+ ROW)))
      ((NULL ITEMS)
       (PROG NIL (RETURN (ADJUST-ARRAY-SIZE MAP (1+ ROW))  ;Last element always contains NIL
	       ROW)))
    (AS-1 ITEMS MAP ROW)	;This is where this row starts
    (COND (FILL-P		;Fill mode, we have some hairy calculation to do
	   (DO ((SPACE INSIDE-WIDTH)
		(STR))
	       ((NULL ITEMS))
	     (SETQ STR (STRING (COND ((ATOM (CAR ITEMS)) (CAR ITEMS))
				     (T (CAAR ITEMS)))))
	     (SETQ WID (TV-STRING-LENGTH PC-PPR STR))
	     (COND ((> WID SPACE)	;This one won't fit, break the line
		    (AND (> WID INSIDE-WIDTH)
			 (FERROR NIL "The string /"~A/" is too wide for this fill-mode menu"
				     STR))
		    (RETURN NIL)))
	     (SETQ SPACE (- SPACE (+ WID MENU-INTERWORD-SPACING))
		   ITEMS (CDR ITEMS))))
	  (T			;Not in fill mode, take the next N
	   (SETQ ITEMS (NTHCDR COLUMNS ITEMS)))))))

;; Since cleaning us redisplays us, we can regenerate our dots.
(DEFMETHOD (MENU-CLASS :CLOBBER-SCREEN) () T)

;; Cleaning a menu displays all the items as well.
(DEFMETHOD (MENU-CLASS :CLEAN) (&AUX (INHIBIT-SCHEDULING-FLAG T))
    (TV-OPEN-SCREEN)
    (TV-SELECT-SCREEN SCREEN)
    (TV-ERASE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP TV-ALU-ANDCA)
    (DRAW-BOX SCREEN
	      LEFT
	      (FOURTH LABEL-POSITION)
	      RIGHT
	      BOTTOM
	      (NOT (ZEROP TOP-ROW))	;Top edge ragged if anything above it
	      NIL
	      (< (+ TOP-ROW SCREEN-ROWS)
		 TOTAL-ROWS)		;Bottom edge ragged if anything below it
	    NIL)
    (AND LABEL
         (<- SELF ':UPDATE-LABEL))
    (DO ((ROW TOP-ROW (1+ ROW))
	 (Y-POS 0 (+ Y-POS ROW-HEIGHT))
	 (LIM (MIN TOTAL-ROWS (+ TOP-ROW SCREEN-ROWS))))
	(( ROW LIM))
      (DO ((ITEMS (AR-1 ROW-MAP ROW) (CDR ITEMS))
	   (END-ITEM-LIST (AR-1 ROW-MAP (1+ ROW)))
	   (STR)
	   (X-POS 0))
	  ((EQ ITEMS END-ITEM-LIST))
	 (SETQ STR (STRING (COND ((ATOM (CAR ITEMS)) (CAR ITEMS))
				 (T (CAAR ITEMS)))))
	 (COND (FILL-P			;Filled, put string followed by spacing
		(TV-SET-CURSORPOS PC-PPR X-POS Y-POS)
		(TV-STRING-OUT PC-PPR STR)
		(SETQ X-POS (+ (TV-READ-CURSORPOS PC-PPR) MENU-INTERWORD-SPACING)))
	       (T			;Columnated, center text within column
		(MENU-DISPLAY-CENTERED-STRING PC-PPR STR
					      X-POS (- (SETQ X-POS (+ X-POS COLUMN-WIDTH))
						       MENU-INTERWORD-SPACING)
					      Y-POS))))))

(DEFMETHOD (MENU-CLASS :PRINT-LABEL) (PC-PPR-1)
    (LET ((FONT (AR-1 (PC-PPR-FONT-MAP PC-PPR) 0)))
	 (TV-SET-FONT PC-PPR-1 FONT)
	 (MENU-DISPLAY-CENTERED-STRING PC-PPR-1 LABEL
				       0 (- RIGHT-MARGIN LEFT-MARGIN)
                                       (SECOND LABEL-POSITION))))

(DEFMETHOD (MENU-CLASS :UPDATE-LABEL) ()
           (<-AS WINDOW-WITH-BOX-CLASS ':UPDATE-LABEL))

;;; This function displays a string centered between two X coordinates, truncated if necessary
(DEFUN MENU-DISPLAY-CENTERED-STRING (PC-PPR STRING LEFT RIGHT Y-POS &AUX WID SWID SLEN)
  (SETQ WID (- RIGHT LEFT)
	STRING (STRING STRING))
  (MULTIPLE-VALUE (SWID SLEN)  ;Compute how wide the string is, and whether to truncate
	   (TV-STRING-LENGTH PC-PPR STRING 0 NIL WID))
  (TV-SET-CURSORPOS PC-PPR (+ LEFT (MAX (// (- WID SWID) 2) 0)) Y-POS)
  (TV-STRING-OUT PC-PPR STRING 0 SLEN))

;;; Mouse handler for menus
(DEFMETHOD (MENU-CLASS :HANDLE-MOUSE) ()
    ;; Forget anything we knew before about the highlight, so it will really be positioned
    (SETQ CURRENT-ITEM NIL)
    ;; Position the menu rectangular highlight.
    (<- SELF ':MOUSE-MOVES MOUSE-X MOUSE-Y)
    ;; Track the mouse, with scroll bar if all is not on the screen.
    (MOUSE-DEFAULT-HANDLER SELF (NEQ SCREEN-ROWS TOTAL-ROWS))
    ;; When mouse leaves this window, stop flashing any item
    ;; and tell the menu (in case it wants to disappear).
    (TV-SET-BLINKER-VISIBILITY (CAR (PC-PPR-BLINKER-LIST PC-PPR)) NIL)
    (<- SELF ':MOUSE-MOVES MOUSE-X MOUSE-Y))

;;; The next three methods make scrolling with the scroll bar work.
(DEFMETHOD (MENU-CLASS :LINE-HEIGHT) () ROW-HEIGHT)

(DEFMETHOD (MENU-CLASS :SCROLL-POSITION) ()
    (PROG () (RETURN TOP-ROW TOTAL-ROWS)))

;;; Given a proposed new MENU-TOP-ROW, compute and store the real one,
;;; update the display, and return it.
(DEFMETHOD (MENU-CLASS :SCROLL-POSITION<-) (NEW-TOP)
  (SETQ TOP-ROW (MAX 0 (MIN NEW-TOP (1- TOTAL-ROWS))))
  (<- SELF ':CLEAN)
  (<- SCROLL-BAR ':SCROLLING-DONE SELF)
  NEW-TOP)

;;; Mouse-click handler for menus.
;;; The left button "selects".  The meaning of this depends on the type of menu.
;;; The middle button calls for documentation.
;;; The right button is reserved.
;;; There are no double-clicks and you can't get to the system command menu.
;;; Clicking when the menu is not exposed just exposes it.

;;; This should be changed to win with temporary menus for which you
;;; release a button to select.  Or should that be done by shadowing this definition?
(DEFMETHOD (MENU-CLASS :MOUSE-BUTTONS) (BD X Y)
    X Y ;ignored, we don't care where the mouse is, the :MOUSE-MOVES method took care of that
    (COND ((NULL STATUS)  ;Button pushed while not exposed, expose self.
	   (WINDOW-EXPOSE SELF))
	  ((BIT-TEST 2 BD)  ;Middle button, get documentation
	   (<- SELF ':DOCUMENT))
	  ((NULL CURRENT-ITEM))
	  (T		    ;Left or Right button, select item.
	   (SETQ LAST-ITEM CURRENT-ITEM)
	   (SETQ CHOSEN-ITEM CURRENT-ITEM)
	   (SETQ CHOICE-RESULT (<- SELF ':EXECUTE CURRENT-ITEM NIL)))))

(DEFMETHOD (MENU-CLASS :CHOOSE) ()
    (OR STATUS (<- SELF ':EXPOSE))
    (PROCESS-WAIT "Menu choose" (FUNCTION CDR)
                  (LOCATE-IN-CLOSURE SELF 'CHOSEN-ITEM))
    (SETQ CHOSEN-ITEM NIL)
    CHOICE-RESULT)

;; Decide what to return based on the item selected.  Also have side-effects
;; such as calling a function if the item says to.
(DEFMETHOD (MENU-CLASS :EXECUTE) (ITEM WINDOW-AND-POSITION &AUX OP ARG)
  (COND ((NLISTP ITEM) ITEM)
	((ATOM (CDR ITEM)) (CDR ITEM))
	((ATOM (CDDR ITEM)) (CADR ITEM))
	((EQ (SETQ ARG (CADDR ITEM)
		   OP (CADR ITEM))
	     ':VALUE)
	 ARG)
	((EQ OP ':EVAL) (EVAL ARG))
	((EQ OP ':FUNCALL) (FUNCALL ARG))
	((EQ OP ':WINDOW-OP) (FUNCALL ARG WINDOW-AND-POSITION))
	((EQ OP ':KBD) (<- PROCESS ':FORCE-KBD-INPUT ARG))
	((EQ OP ':MENU) (<- (IF (SYMBOLP ARG) (SYMEVAL ARG) ARG) ':CHOOSE))
	(T (FERROR NIL "~S unknown menu item-list flavor" OP))))

(DEFMETHOD (MENU-CLASS :SELECT) () (<- SELF ':EXPOSE))

(DEFMETHOD (MENU-CLASS :DOCUMENT) ()
  (TV-BEEP))

;;; Put a menu near another window.  This will normally try to put it just below
;;; it and give it the same width.
(DEFMETHOD (MENU-CLASS :MOVE-NEAR-WINDOW) (W)
  (LOCK-SCREEN-LAYOUT
    (LET ((W-EDGES (<- W ':EDGES)))
      (LET ((GEOM (<- SELF ':COMPUTE-GEOMETRY NIL (- (THIRD W-EDGES) (FIRST W-EDGES))))
	    (NEW-TOP (FOURTH W-EDGES)))      
	;If it won't fit below try putting it above
	(AND (> (+ NEW-TOP (CAR GEOM)) (SCREEN-Y2 SCREEN))
	     (SETQ NEW-TOP (MAX (- (SECOND W-EDGES) (CAR GEOM)) 0)))
	;Put it there
	(<- SELF ':EDGES<- (FIRST W-EDGES) NEW-TOP (THIRD W-EDGES) (+ NEW-TOP (CAR GEOM)))
	(<- SELF ':EXPOSE)))))

;; We must turn our pc-ppr on and off when we are exposed and deexposed.
;; Also, we should not ever restore our saved bits (if any).  Just clean.
(DEFMETHOD (MENU-CLASS :EXPOSE) ()
  (LET ((INHIBIT-SCREEN-RESTORATION-FLAG T))
    (<-AS WINDOW-CLASS ':EXPOSE))
  (TV-EXPOSE-PC-PPR PC-PPR))

;; Like WINDOW-WITH-PC-PPR-CLASS but don't output-hold
;; since our :CLEAN method works by typing out and would get hung!
;; Also, turns the item blinker (highlight) completely off rather than solid on.
(DEFMETHOD (MENU-CLASS :DEEXPOSE) (&OPTIONAL X)
  (TV-DEACTIVATE-PC-PPR PC-PPR)
  (<-AS WINDOW-CLASS ':DEEXPOSE X))

;;; This is the guts.  Given a menu and a set of coordinates, it finds
;;; the corresponding item, if any, sets CURRENT-ITEM to it, and sets up
;;; the blinker to mark that item.  If no item, the blinker is shut off.
(DEFMETHOD (MENU-CLASS :MOUSE-MOVES)
	   (X Y &AUX ITEM ITEMS ROW XREL BLINKER BLX BLWIDTH STR COLN)
  (MOUSE-SET-X-BLINKER-CURSORPOS)
  (SETQ ROW (// (- Y TOP-MARGIN) ROW-HEIGHT)
	XREL (- X LEFT-MARGIN)
	BLINKER (CAR (PC-PPR-BLINKER-LIST PC-PPR)))
  (COND (( 0 ROW)
         (COND ((NULL STATUS))  ;If not exposed, must not blink
               ((AND ( XREL 0)	;If inside the menu
                     (< X RIGHT-MARGIN))
                ;;If mouse is past the last displayed row, blink item on that row.
                (AND (OR (>= (+ TOP-ROW ROW) TOTAL-ROWS) (>= ROW SCREEN-ROWS))
                     (SETQ ROW (1- (MIN SCREEN-ROWS (- TOTAL-ROWS TOP-ROW)))))
                (SETQ ITEMS (AR-1 ROW-MAP (+ TOP-ROW ROW)))
                (COND (FILL-P	;Fill mode, cogitate
                       (SETQ BLX 0)
                       (DO ((L ITEMS (CDR L))
                            (OLDL NIL L)
			    (X 0 (+ X
                                    (SETQ BLWIDTH (TV-STRING-LENGTH PC-PPR STR))
                                    MENU-INTERWORD-SPACING)))
                           ((OR (NULL L)
                                (> X XREL))  ;If this string crosses the mouse, it's the one
                            (SETQ ITEM (CAR OLDL)
				  BLX (1- BLX)
				  BLWIDTH (+ BLWIDTH 2)))
                           (SETQ BLX X
                                 STR (STRING (COND ((ATOM (CAR L)) (CAR L))
                                                   (T (CAAR L)))))))
                      (T	;Columnated, find which column
                       (SETQ COLN (// XREL COLUMN-WIDTH))		;Column selected
                       (SETQ ITEM (CAR (NTHCDR COLN ITEMS)))		;This may be NIL
                       (SETQ STR (STRING (COND ((ATOM ITEM) ITEM) (T (CAR ITEM)))))
                       (SETQ BLWIDTH
                             (+ (TV-STRING-LENGTH PC-PPR STR 0 NIL COLUMN-WIDTH) 2))
                       (SETQ BLX (+ (* COLN COLUMN-WIDTH)		;Start of column
                                    (MAX 0 (// (- COLUMN-WIDTH	;Centering
                                                  MENU-INTERWORD-SPACING
                                                  BLWIDTH)
                                               2))))))))))
  ;; If this item is non-selectable, don't select it.
  (AND (NOT (ATOM ITEM)) (NOT (ATOM (CDR ITEM))) (NOT (ATOM (CDDR ITEM)))
       (EQ (CADR ITEM) ':NO-SELECT)
       (SETQ ITEM NIL))
  ;; Now make the blinker be where and what we have just found it should be.
  (TV-SET-BLINKER-VISIBILITY BLINKER (NOT (NULL ITEM)))
  (SETQ CURRENT-ITEM ITEM)
  (COND (ITEM
	 (TV-SET-BLINKER-CURSORPOS BLINKER BLX (1- (* ROW ROW-HEIGHT)))
	 (TV-SET-BLINKER-SIZE
	     BLINKER
	     BLWIDTH
	     (+ (FONT-CHAR-HEIGHT (AR-1 (PC-PPR-FONT-MAP PC-PPR) 0)) 2)))))

;;; This is the blinker function for menus.  The given cursorpos is where to flash
;;; the text in "TV-BLINKER-WIDTH".  The char alu function of the pc ppr is XOR.
;;; "TV-BLINKER-HEIGHT" is (cough, cough) the string length if it needs truncation.
;;; THIS IS NOT USED.  THE RECTANGULAR BLINKER IS BETTER.
(DEFUN MENU-BLINKER (BLINKER IGNORE X Y &AUX (PC-PPR (TV-BLINKER-PC-PPR BLINKER)))
   (SETF (TV-BLINKER-PHASE BLINKER) (NOT (TV-BLINKER-PHASE BLINKER)))
   (SETF (PC-PPR-CURRENT-X PC-PPR) X)  ;No need to bind since interrupt-inhibition
   (SETF (PC-PPR-CURRENT-Y PC-PPR) Y)
   (SETF (PC-PPR-EXCEPTIONS PC-PPR) 0) ;Mainly to get rid of end of line flag
   (BIND (LOCF (PC-PPR-BLINKER-LIST PC-PPR)) NIL) ;Don't blink me recursively.
   (TV-STRING-OUT PC-PPR (TV-BLINKER-WIDTH BLINKER) 0 (TV-BLINKER-HEIGHT BLINKER)))

;;; This function, given a menu item-list, returns the maximum width
;;; of any string in it.  Normally you want to add an allowance for interword spacing.
(DEFUN MENU-MAX-WIDTH (PC-PPR ITEM-LIST)
  (DO ((L ITEM-LIST (CDR L))
       (MXW 0 (MAX MXW (TV-STRING-LENGTH PC-PPR (STRING (COND ((ATOM (CAR L)) (CAR L))
							      (T (CAAR L))))))))
      ((NULL L) MXW)))

;;; This function, given a menu item-list, returns the estimated total width
;;; (product of WIDTH and N-ROWS, not counting borders) when the menu items
;;; are put in "fill mode" rather than in columns.  It is only an estimate
;;; because at this point we can't be sure about word breakage.
(DEFUN MENU-FILL-WIDTH (PC-PPR ITEM-LIST)
  (DO ((L ITEM-LIST (CDR L))
       (WID 0))
      ((NULL L) WID)
    (SETQ WID (+ (TV-STRING-LENGTH PC-PPR (STRING (COND ((ATOM (CAR L)) (CAR L))
							(T (CAAR L)))))
		 MENU-FILL-BREAKAGE
		 WID))))

;;; Here is how we make a menu appear with the last item chosen under the mouse.

;;; Return the x and y co-ordinates (relative to the top left corner)
;;; of the center of the specified item

(DEFMETHOD (MENU-CLASS :ITEM-CURSORPOS)
	   (ITEM &AUX (ROW 0) XPOS)
    ;; Scan down for the row that contains this item.
    (DO ((L ITEM-LIST (CDR L)))
	((NULL L) (FERROR NIL "Item ~S is not in ~S's item-list" ITEM SELF))
       (AND (EQ L (AR-1 ROW-MAP (1+ ROW)))
	    (SETQ ROW (1+ ROW)))
       (AND (EQ (CAR L) ITEM) (RETURN NIL)))
    (COND (FILL-P
	    (DO ((L (AR-1 ROW-MAP ROW) (CDR L))
		 (XSTART 0 (+ XSTART WIDTH MENU-INTERWORD-SPACING))
		 (WIDTH))
		(())
	       (SETQ WIDTH (TV-STRING-LENGTH PC-PPR
			       (STRING (COND ((ATOM (CAR L)) (CAR L))
					     (T (CAAR L))))))
	       (AND (EQ (CAR L) ITEM)
		    (RETURN (SETQ XPOS (+ XSTART (// WIDTH 2)))))))
	  (T
	    (SETQ XPOS (+ (// COLUMN-WIDTH 2)
			  (* COLUMN-WIDTH
			     (FIND-POSITION-IN-LIST ITEM (AR-1 ROW-MAP ROW)))))))
    (LIST XPOS (+ (// ROW-HEIGHT 2) (* ROW-HEIGHT ROW))))

;; When we move a menu to a spot, make it go so that the last item chosen
;; appears at that spot.
(DEFMETHOD (MENU-CLASS :MOVE-NEAR) (X Y &AUX (XI 0) (YI 0))
    (AND LAST-ITEM
	 ;; If we remember a previous choice,
	 ;; let XI and YI get the offsets from that item to the center.
	 (LET ((FOO (<- SELF ':ITEM-CURSORPOS LAST-ITEM)))
	      (SETQ XI (- (// (- RIGHT LEFT) 2) (CAR FOO))
		    YI (- (// (- BOTTOM TOP) 2) (CADR FOO)))))
    (MAPCAR '- (<-AS WINDOW-CLASS ':MOVE-NEAR (+ X XI) (+ Y YI))
	    (LIST XI YI)))

;;; Menus to be used for a momentary choice.
;;; Send a menu of this type a :CHOOSE message to use the menu.
;;; When the user selects an item, or moves the mouse off the menu,
;;; the menu will disappear, and whatever was underneath it will reappear.
;;; It will return the chosen item, or NIL.  If the item is not atomic
;;; and its cadr is non-NIL, the cadr will be called with no arguments.
;;; In this case, if the caddr of the item is also non-nil,
;;; no windows will be re-exposed before the cadr is called.

;;; This process runs momentary menus that would otherwise run in the mouse process.
(DEFVAR MOMENTARY-MENU-PROCESS)

(DEFCLASS MOMENTARY-MENU-CLASS MENU-CLASS ())

(DEFMETHOD (MOMENTARY-MENU-CLASS :BORN) ()
    (OR (BOUNDP 'MOMENTARY-MENU-PROCESS)
        (SETQ MOMENTARY-MENU-PROCESS (PROCESS-CREATE "Momentary Menu" NIL
						     ':SPECIAL-PDL-SIZE 1000
						     ':REGULAR-PDL-SIZE 4000)))
    (SETQ PROCESS MOMENTARY-MENU-PROCESS)
    (<-AS MENU-CLASS ':BORN))

(DEFMETHOD (MOMENTARY-MENU-CLASS :HANDLE-MOUSE) ()
    (SETQ WINDOW-OWNING-MOUSE NIL)
    ;; Position the menu rectangular highlight.
    (<- SELF ':MOUSE-MOVES MOUSE-X MOUSE-Y)
    ;; Track the mouse, with scroll bar if all is not on the screen.
    (DO () (())
      ;; Let the mouse out of the menu only if it moves more than 1/4 inch away
      (AND (OR MOUSE-RECONSIDER
	       (< MOUSE-X (- LEFT 25.))
	       (> MOUSE-X (+ RIGHT 25.))
	       (< MOUSE-Y (- TOP 25.))
	       (> MOUSE-Y (+ BOTTOM 25.)))
	   (RETURN T))
      (MOUSE-DEFAULT-HANDLER SELF (NEQ SCREEN-ROWS TOTAL-ROWS)))
    ;; Return here when mouse moves outside menu or selection is made.
    ;; When mouse leaves this window, stop flashing any item
    ;; and take the menu off the screen.
    (<- SELF ':MOUSE-MOVES MOUSE-X MOUSE-Y)
    (TV-SET-BLINKER-VISIBILITY (CAR (PC-PPR-BLINKER-LIST PC-PPR)) NIL)
    ;; If item so requests, don't re-expose anybody before it is run.
    (LET ((INHIBIT-AUTOEXPOSE-FLAG
	   (OR INHIBIT-AUTOEXPOSE-FLAG
	       (AND (NOT (ATOM CHOSEN-ITEM))
		    (NOT (ATOM (CDR CHOSEN-ITEM)))
		    (NOT (ATOM (CDDR CHOSEN-ITEM)))
		    (OR (EQ (CADR CHOSEN-ITEM) ':MENU)
			(MEMQ ':NO-AUTOEXPOSE (CDDDR CHOSEN-ITEM)))))))
      (FUNCALL SELF ':DEACTIVATE)))

;; Don't save bits when we pop down, since we will regenerate them next time.
(DEFMETHOD (MOMENTARY-MENU-CLASS :DEACTIVATE) ()
  (LOCK-SCREEN-LAYOUT
    (LET ((INHIBIT-SCREEN-SAVING-FLAG T))
      (<-AS WINDOW-CLASS ':DEEXPOSE))
    (<-AS WINDOW-CLASS ':DEACTIVATE)))

;;; This should be changed to win with temporary menus for which you
;;; release a button to select.  Or should that be done by shadowing this definition?
(DEFMETHOD (MOMENTARY-MENU-CLASS :MOUSE-BUTTONS) (BD X Y)
    X Y ;ignored, we don't care where the mouse is, the :MOUSE-MOVES method took care of that
    (COND ((BIT-TEST 2 BD)  ;Middle button, get documentation
	   (<- SELF ':DOCUMENT))
	  ((NULL CURRENT-ITEM))
	  (T		    ;Left or Right button, select item.
	   ;; Make MOUSE-DEFAULT-HANDLER return so menu gets deactivated.
	   (SETQ MOUSE-RECONSIDER T)
	   (SETQ LAST-ITEM CURRENT-ITEM)
	   (SETQ CHOSEN-ITEM CURRENT-ITEM))))

(DEFMETHOD (MOMENTARY-MENU-CLASS :WAIT-FOR-CHOICE) ()
    (PROCESS-WAIT "Menu choose" (FUNCTION
				  (LAMBDA (STATUS-LOC TIME-STARTED-WAITING)
;THE FOLLOWING IS A SUITABLY CROCKISH FIX.
; PROBLEM IS THAT MOUSE-OVERSEER MAY HAVE ALREADY CYCLED BY THE TIME WINDOW-OWNING-MOUSE
; GETS SET UP.  IF THE MOUSE IS ON TOP OF THE MENU WINDOW, THAT IS OK, BUT OTHERWISE,
; IT MAY GIVE MOUSE CONTROL TO SOME OTHER WINDOW.  IF THIS SEEMS TO BE HAPPENING, GIVE UP.
				    (AND (CDR STATUS-LOC)
					 WINDOW-OWNING-MOUSE
					 (> (TIME-DIFFERENCE (TIME) TIME-STARTED-WAITING)
					    600.)
					 (SETQ MOUSE-RECONSIDER T))
				    (NULL (CDR STATUS-LOC))))
		  (LOCATE-IN-CLOSURE SELF 'STATUS)
		  (TIME))
    CHOSEN-ITEM)

(DEFMETHOD (MOMENTARY-MENU-CLASS :CHOOSE) ()
    (COND ((EQ CURRENT-PROCESS MOUSE-PROCESS)
	   (SETQ PROCESS MOMENTARY-MENU-PROCESS)
	   (<- MOMENTARY-MENU-PROCESS ':PRESET
	       SELF ':PROCESS-CHOOSE T)
	   (<- MOMENTARY-MENU-PROCESS ':RUN-REASON))
	  (T (SETQ PROCESS CURRENT-PROCESS)
	     (<- SELF ':PROCESS-CHOOSE NIL))))

;; This is a continuation of the :CHOOSE processing; if we were in the mouse process
;; getting here means we've managed to switch processes.
;; The first argument is T if this is the top level of the momentary menu process,
;; and therefore should flush itself when it is done.  Yes, this is a gross kludge.
;; We autoexpose windows after we are exposed, because if we were called
;; from another menu it will have avoided autoexposing before exposing us.
;; The reason for wanting to autoexpos after instead of before bringing up
;; the other menu is to avoid autoexposing anything that will just get deexposed again.

(DEFMETHOD (MOMENTARY-MENU-CLASS :PROCESS-CHOOSE) (TOP-LEVEL-P
						   &AUX EDGES RESULT)
    (LET ((WINDOW-UNDER-MENU (WINDOW-UNDER-MOUSE))
	  (OLD-X MOUSE-X) (OLD-Y MOUSE-Y)) 
	 (LEXPR-FUNCALL 'MOUSE-WARP
			(<- SELF ':MOVE-NEAR MOUSE-X MOUSE-Y))
         (SETQ EDGES (<- SELF ':EDGES))
	 ;; Initialize to no choice made yet.
	 (SETQ CHOSEN-ITEM NIL
	       CURRENT-ITEM NIL)
	 ;; Expose self, and seize the mouse.
	 (WINDOW-EXPOSE SELF)
	 (WINDOWS-AUTOEXPOSE)
	 (SETQ WINDOW-OWNING-MOUSE SELF)
	 ;; Let the mouse-process notice that we have been seized.
	 ;; Our :HANDLE-MOUSE will unseize it.
	 (PROCESS-ALLOW-SCHEDULE)
	 ;; Wait for the :HANDLE-MOUSE method to exit and the choice to be made.
	 (FUNCALL SELF ':WAIT-FOR-CHOICE)
	 (SETQ RESULT (<- SELF ':EXECUTE CHOSEN-ITEM
			  (LIST WINDOW-UNDER-MENU OLD-X OLD-Y)))
	 (AND TOP-LEVEL-P
	      (PROGN (<- MOMENTARY-MENU-PROCESS ':REVOKE-RUN-REASON)
		     (PROCESS-ALLOW-SCHEDULE)))   ;Back to the scheduler
	 RESULT))

(DEFVAR MENU-CHOOSE-MENU)    ;Menu reused each time by MENU-CHOOSE.

;;; Pop up a menu and pick an item from the given alist
(DEFUN MENU-CHOOSE (ALIST)
    (OR ALIST (FERROR NIL "No items to choose among"))
    (COND ((NOT (BOUNDP 'MENU-CHOOSE-MENU))
	   (SETQ MENU-CHOOSE-MENU (<- MOMENTARY-MENU-CLASS ':NEW ':ITEM-LIST ALIST)))
	  ((NEQ ALIST (<- MENU-CHOOSE-MENU ':ITEM-LIST))        ;Reuse an old menu
	   (<- MENU-CHOOSE-MENU ':LAST-ITEM<- NIL)
	   (<- MENU-CHOOSE-MENU ':ITEM-LIST<- ALIST)
	   (<- MENU-CHOOSE-MENU ':COMPUTE-GEOMETRY)))
    (<- MENU-CHOOSE-MENU ':CHOOSE)		;Pick something
    (<- MENU-CHOOSE-MENU ':CHOSEN-ITEM))	;Return whole item, not its cdr or cadr
