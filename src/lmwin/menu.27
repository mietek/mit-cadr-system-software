;;; -*- Mode:Lisp; Package:TV; Base:8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; New menu system

;Documentation on menu item-lists:
;
;Each item in the item-list may be one of the following:
; 1. A string (or a symbol).  
; 2. Cons of a string (or a symbol) and an atom.
; 3. List of a string (or a symbol) and any object.  The list may
;    not be more than 2 long.
; 4. List of a string (or a symbol), a flavor keyword, and an argument.
;    After the first 3 elements of the list, the rest of the list is
;    a property list of modifier keywords and values.
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
;		:FONT - the font in which to display the item
;This stuff is largely although not entirely controlled by the :EXECUTE method,
;which you may redefine.

;;; These special variables exist so that there are less random numbers
;;; in the code, giving somewhat more chance of understanding it.
;;; You might even want to change them.
(DEFVAR MENU-INTERWORD-SPACING 27.)	;For fill mode.  3 characters in MEDFNT
(DEFVAR MENU-INTERCOLUMN-SPACING 10.)	;For column mode.
(DEFVAR MENU-FILL-BREAKAGE 60.)
(DEFVAR MENU-GOLDEN-RATIO 1.6s0)

(DEFFLAVOR BASIC-MENU
	   (ITEM-LIST			;List of items being displayed.
	    CURRENT-ITEM		;Item being pointed at now.
	    LAST-ITEM			;The last item to have been selected.
	    (CHOSEN-ITEM NIL)		;The same, but it's ok to set this to NIL
					;and wait for it to become non-NIL.
	    SCREEN-ROWS			;Number of rows in menu on screen
	    TOTAL-ROWS			;Total number of rows in menu.
					;If this is greater than SCREEN-ROWS, then the latter
					;represent a window on all the rows.
	    TOP-ROW			;This is first row visible now.
	    ROW-HEIGHT			;Height in dots of a row (including vsp).
	    ROW-MAP			;Array of tails of ITEM-LIST.  For each row
					;in the menu, points to first item on that row.
					;An extra element at the end is NIL.
					;The length is thus (1+ TOTAL-ROWS).
	    (COLUMNS NIL)		;Number of columns (NIL in fill mode).
	    COLUMN-WIDTH		;Width in dots of a column (NIL in fill mode).

	    ;; GEOMETRY is the user specified geometry.  It is a list of:
	    ;; Number of columns or 0 if FILL-P, number of rows, inside width, inside height,
	    ;; maximum width, maximum height.  NIL means it's free to change, as was not
	    ;; explicitly specified by the user.  Default is to leave everything free.
	    (GEOMETRY
	     (LIST NIL NIL NIL NIL NIL NIL))
	    (SET-EDGES-MODE NIL)	;Looked at by :CHANGE-OF-SIZE-OR-MARGINS method
					;NIL means not via SET-EDGES (margin changing, for
					; example)
					;T means via an internal call from the menu system
					;USER means from a user's :SET-EDGES
					;Any other means to recompute parameters, but don't
					; be sticky with respect to new sizes
	   )
	   (MENU-EXECUTE-MIXIN)
  (:INCLUDED-FLAVORS BASIC-SCROLL-BAR)
  (:GETTABLE-INSTANCE-VARIABLES ITEM-LIST CURRENT-ITEM LAST-ITEM CHOSEN-ITEM GEOMETRY)
  (:SETTABLE-INSTANCE-VARIABLES LAST-ITEM CHOSEN-ITEM)
  (:INITABLE-INSTANCE-VARIABLES ITEM-LIST GEOMETRY)
  (:INIT-KEYWORDS :ROWS :COLUMNS :FILL-P)  ;Set specific parts of geometry
  (:DOCUMENTATION :MIXIN "Regular menu messages
Provides methods and instance variables common to all menus, such as the item-list,
the geometry hacking, a default :choose message, and a scroll bar if necessary."))

(DEFSTRUCT (GEOMETRY :LIST (:CONSTRUCTOR NIL))
  GEOMETRY-N-COLUMNS
  GEOMETRY-N-ROWS
  GEOMETRY-INSIDE-WIDTH
  GEOMETRY-INSIDE-HEIGHT
  GEOMETRY-MAX-WIDTH
  GEOMETRY-MAX-HEIGHT)

(DEFMACRO GEOMETRY-FILL-P (GEO) `(AND (GEOMETRY-N-COLUMNS ,GEO)
				      (ZEROP (GEOMETRY-N-COLUMNS ,GEO))))

(EVAL-WHEN (COMPILE LOAD EVAL)
(DEFPROP GEOMETRY-FILL-P
	 ((GEOMETRY-FILL-P GEO) . (SETF (GEOMETRY-N-COLUMNS GEO) (IF SI:VAL 0 NIL)))
	 SETF))

(DEFFLAVOR MENU ((LABEL NIL)) (BASIC-MENU BORDERS-MIXIN TOP-BOX-LABEL-MIXIN MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "The simplest instantiatable menu.
Defaults to not having a label, a label whose position is not initially specified will
be at the top, in a small auxiliary box, unlike most windows."))

(DEFFLAVOR POP-UP-MENU () (TEMPORARY-WINDOW-MIXIN MENU)
  (:DOCUMENTATION :COMBINATION "A menu that is temporary
This is not a momentary menu, it must be exposed and deexposed normally, it does save
the state beneath itself when exposed."))

(DEFMETHOD (SCREEN :MENU-FONT) () (FUNCALL-SELF ':PARSE-FONT-DESCRIPTOR 'FONTS:MEDFNT))

(DEFMETHOD (BASIC-MENU :BEFORE :INIT) (INIT-PLIST &AUX (SUP SUPERIOR) TEM)
  (SETQ SUP (OR SUP (GET INIT-PLIST ':SUPERIOR) DEFAULT-SCREEN))
  (OR (BOUNDP 'FONT-MAP)
      (SETQ FONT-MAP (LIST (FUNCALL (SHEET-GET-SCREEN SUP) ':MENU-FONT))))
  (PUTPROP INIT-PLIST NIL ':MORE-P)
  (AND (GET INIT-PLIST ':FILL-P)
       ;(SETF (GEOMETRY-FILL-P GEOMETRY) T)  ;Compiler gives a gratuitous warning for this
       (SETF (GEOMETRY-N-COLUMNS GEOMETRY) 0))
  (AND (SETQ TEM (GET INIT-PLIST ':ROWS))
       (SETF (GEOMETRY-N-ROWS GEOMETRY) TEM))
  (AND (SETQ TEM (GET INIT-PLIST ':COLUMNS))
       (SETF (GEOMETRY-N-COLUMNS GEOMETRY) TEM))
  ;; We'll handle SAVE-BITS ourselves later
  ;; This is so the bit array doesn't get created until we know the size
  (PUTPROP INIT-PLIST (GET INIT-PLIST ':SAVE-BITS) ':MENU-SAVE-BITS)
  (PUTPROP INIT-PLIST NIL ':SAVE-BITS))

(DEFMETHOD (BASIC-MENU :AFTER :INIT) (INIT-PLIST)
  (SETF (BLINKER-VISIBILITY (CAR BLINKER-LIST)) NIL)
  (MENU-COMPUTE-GEOMETRY NIL)
  (FUNCALL-SELF ':SET-SAVE-BITS (GET INIT-PLIST ':MENU-SAVE-BITS)))

(DEFMETHOD (BASIC-MENU :AFTER :REFRESH) (&OPTIONAL TYPE)
  (OR (AND RESTORED-BITS-P (NEQ TYPE ':SIZE-CHANGED))
      (FUNCALL-SELF ':MENU-DRAW)))

(DEFMETHOD (BASIC-MENU :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (COND ((EQ SET-EDGES-MODE T))  ;Recursive call, caller will take care of it
	((EQ SET-EDGES-MODE ':USER)
	 ;; Some sort of explicit setting of edges -- make the new size sticky
	 (SETF (GEOMETRY-INSIDE-WIDTH GEOMETRY) (SHEET-INSIDE-WIDTH))
	 (SETF (GEOMETRY-INSIDE-HEIGHT GEOMETRY) (SHEET-INSIDE-HEIGHT))
	 (MENU-COMPUTE-GEOMETRY NIL))
	((EQ SET-EDGES-MODE NIL)
	 (MENU-COMPUTE-GEOMETRY NIL))
	(T
	 ;; Some change other than by user or margins or compute geometry -- recompute
	 ;; geometry, use current size, but don't make it sticky.
	 ;; E.g. :MOVE-NEAR-WINDOW
	 (MENU-COMPUTE-GEOMETRY NIL (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)))))

(DEFWRAPPER (BASIC-MENU :SET-EDGES) (IGNORE . BODY)
  `(LET-GLOBALLY ((SET-EDGES-MODE (OR SET-EDGES-MODE ':USER)))
     . ,BODY))

(DEFMETHOD (BASIC-MENU :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (SETQ ITEM-LIST NEW-ITEM-LIST
	LAST-ITEM NIL
	CURRENT-ITEM NIL)
  (MENU-COMPUTE-GEOMETRY T)		;Recompute parameters, and redraw menu
  NEW-ITEM-LIST)

(DEFMETHOD (BASIC-MENU :SET-GEOMETRY) (&REST NEW-GEOMETRY)
  (DECLARE (ARGLIST (&OPTIONAL N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT
			       MAX-WIDTH MAX-HEIGHT)))
  "NIL for an argument means make it unconstrained.  T or unsupplied means leave it alone"
  (OR ( (LENGTH NEW-GEOMETRY) (LENGTH GEOMETRY))
      (FERROR NIL "Too many args to :SET-GEOMETRY"))
  (DO ((G NEW-GEOMETRY (CDR G))
       (CG GEOMETRY (CDR CG)))
      ((NULL G))
    (IF (NEQ (CAR G) T)
	(RPLACA CG (CAR G))))
  (MENU-COMPUTE-GEOMETRY T))

(DEFMETHOD (BASIC-MENU :CURRENT-GEOMETRY) ()
  "Like :GEOMETRY but returns the current state rather than the default"
  (LIST (IF (GEOMETRY-FILL-P GEOMETRY) 0 COLUMNS) TOTAL-ROWS
	(SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT)
	(GEOMETRY-MAX-WIDTH GEOMETRY) (GEOMETRY-MAX-HEIGHT GEOMETRY)))

(DEFMETHOD (BASIC-MENU :FILL-P) () (GEOMETRY-FILL-P GEOMETRY))
(DEFMETHOD (BASIC-MENU :SET-FILL-P) (FILL-P)
  (FUNCALL-SELF ':SET-GEOMETRY (IF FILL-P 0 NIL)))

(DEFMETHOD (BASIC-MENU :MOUSE-STANDARD-BLINKER) ()
  ;; Change the mouse cursor to a small X so it doesn't get in the way
  (MOUSE-SET-BLINKER-DEFINITION ':CHARACTER 3 3 ':ON
				':SET-CHARACTER 7))

;;; Mouse handler for menus
(DEFMETHOD (BASIC-MENU :BEFORE :HANDLE-MOUSE) ()
  ;; Forget anything we knew before about the highlight, so it will really be positioned
  (SETQ CURRENT-ITEM NIL))

(DEFMETHOD (BASIC-MENU :AFTER :HANDLE-MOUSE) ()
  ;; When mouse leaves this window, stop flashing any item
  (BLINKER-SET-VISIBILITY (CAR BLINKER-LIST) NIL))

;;; Mouse-click handler for menus.
;;; The left button "selects".  The meaning of this depends on the type of menu.
;;; The middle button calls for documentation.
;;; The right button is reserved.
;;; There are no double-clicks and you can't get to the system command menu.
;;; Clicking when the menu is not exposed just exposes it.

(DEFMETHOD (BASIC-MENU :MOUSE-BUTTONS) (BD X Y)
  X Y ;ignored, we don't care where the mouse is, the :MOUSE-MOVES method took care of that
  (COND ((NOT EXPOSED-P)	;Button pushed while not exposed, expose self.
	 (FUNCALL-SELF ':EXPOSE))
	((BIT-TEST 2 BD)	;Middle button, get documentation
	 (FUNCALL-SELF ':DOCUMENT))
	((NULL CURRENT-ITEM))
	(T			;Left or Right button, select item.
	 (SETQ LAST-ITEM CURRENT-ITEM
	       CHOSEN-ITEM CURRENT-ITEM))))

(DEFMETHOD (BASIC-MENU :CHOOSE) ()
  (SETQ CHOSEN-ITEM NIL)
  (OR EXPOSED-P (FUNCALL-SELF ':EXPOSE))
  (PROCESS-WAIT "Menu choose" #'(LAMBDA (ITEM-LOC STATUS-LOC)
				  (OR (CAR ITEM-LOC) (NULL (CAR STATUS-LOC))))
		(LOCATE-IN-INSTANCE SELF 'CHOSEN-ITEM)
		(LOCF (SHEET-EXPOSED-P SELF)))
  (PROG1 (FUNCALL-SELF ':EXECUTE CHOSEN-ITEM)
	 (SETQ CHOSEN-ITEM NIL)))

;;; These understand the mapping between an item and its printed representation
(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-MENU)
(DEFUN MENU-ITEM-STRING (ITEM &AUX STRING FONT)
  (DECLARE (RETURN-LIST STRING FONT))
  (IF (ATOM ITEM)
      (SETQ STRING ITEM)
      (SETQ STRING (CAR ITEM))
      (AND (LISTP (CDR ITEM))
	   (SETQ FONT (GET (CDDR ITEM) ':FONT))))
  (COND ((NULL FONT) (SETQ FONT CURRENT-FONT))
	((SYMBOLP FONT)
	 (SETQ FONT (FUNCALL (SHEET-GET-SCREEN SELF) ':PARSE-FONT-DESCRIPTOR FONT)))
	((NUMBERP FONT) (SETQ FONT (AREF FONT-MAP FONT))))
  (PROG () (RETURN (STRING STRING) FONT))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-MENU)
(DEFUN MENU-ITEM-STRING-WIDTH (ITEM &OPTIONAL STOP-X)
  (MULTIPLE-VALUE-BIND (STRING FONT)
      (MENU-ITEM-STRING ITEM)
    (SHEET-STRING-LENGTH SELF STRING 0 NIL STOP-X FONT))))

;;; This is the guts.  Given a menu and a set of coordinates, it finds
;;; the corresponding item, if any, sets CURRENT-ITEM to it, and sets up
;;; the blinker to mark that item.  If no item, the blinker is shut off.
;;;*** This tvobish code should be rewritten ***
(DEFMETHOD (BASIC-MENU :MOUSE-MOVES) (X Y
				      &AUX ITEM ITEMS ROW XREL BLINKER BLX (BLWIDTH 0)
				           COLN STOP-ITEM
					   (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (MOUSE-SET-BLINKER-CURSORPOS)
  (SETQ ROW (// (- Y (SHEET-INSIDE-TOP)) ROW-HEIGHT)
	XREL (- X (SHEET-INSIDE-LEFT))
	BLINKER (CAR BLINKER-LIST))
  (COND (( 0 ROW)
         (COND ((NOT EXPOSED-P))	 ;If not exposed, must not blink
               ((AND ( XREL 0)	;If inside the menu
                     (< X (SHEET-INSIDE-RIGHT)))
                ;;If mouse is past the last displayed row, blink item on that row.
                (AND (OR (>= (+ TOP-ROW ROW) TOTAL-ROWS) (>= ROW SCREEN-ROWS))
                     (SETQ ROW (1- (MIN SCREEN-ROWS (- TOTAL-ROWS TOP-ROW)))))
                (IF (MINUSP ROW) (SETQ ITEMS NIL STOP-ITEM NIL) ;No items visible
		    (SETQ ITEMS (AREF ROW-MAP (+ TOP-ROW ROW))
			  STOP-ITEM (AREF ROW-MAP (+ TOP-ROW ROW 1))))
                (COND (FILL-P	;Fill mode, cogitate
                       (SETQ BLX 0)
                       (DO ((L ITEMS (CDR L))
                            (ITM) (OITM NIL ITM)
			    (X 0 (+ X
				    (SETQ BLWIDTH (MENU-ITEM-STRING-WIDTH ITM))
				    MENU-INTERWORD-SPACING)))
                           ((OR (NULL L)
                                (> X XREL))  ;If this string crosses the mouse, it's the one
                            (SETQ ITEM OITM
				  BLX (1- BLX)
				  BLWIDTH (+ BLWIDTH 1)))
			 (AND (EQ L STOP-ITEM)
			      ;; The next item on next line -- punt
			      (RETURN NIL))
			 (SETQ ITM (CAR L)
			       BLX X)))
                      (T	;Columnated, find which column
                       (SETQ COLN (// XREL COLUMN-WIDTH))		;Column selected
                       (SETQ ITEM (CAR (NTHCDR COLN ITEMS)))		;This may be NIL
                       (SETQ BLWIDTH (1+ (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH)))
                       (SETQ BLX (+ (* COLN COLUMN-WIDTH)		;Start of column
				    -1
                                    (MAX 0 (// (- COLUMN-WIDTH	;Centering
                                                  MENU-INTERCOLUMN-SPACING
                                                  BLWIDTH)
                                               2))))))))))
  ;; If this item is non-selectable, don't select it.
  (AND (NOT (ATOM ITEM)) (NOT (ATOM (CDR ITEM))) (NOT (ATOM (CDDR ITEM)))
       (EQ (CADR ITEM) ':NO-SELECT)
       (SETQ ITEM NIL))
  ;; Now make the blinker be where and what we have just found it should be.
  (BLINKER-SET-VISIBILITY BLINKER (NOT (NULL ITEM)))
  (SETQ CURRENT-ITEM ITEM)
  (COND (ITEM
	 (BLINKER-SET-CURSORPOS BLINKER BLX (1- (* ROW ROW-HEIGHT)))
	 (BLINKER-SET-SIZE BLINKER BLWIDTH
			   (+ (FONT-CHAR-HEIGHT (AREF FONT-MAP 0)) 2)))))

(DEFMETHOD (BASIC-MENU :SCROLL-POSITION) ()
  (PROG () (RETURN TOP-ROW TOTAL-ROWS ROW-HEIGHT)))

(DEFMETHOD (BASIC-MENU :SCROLL-TO) (LINE MODE)
  (SELECTQ MODE
    (:ABSOLUTE)
    (:RELATIVE (SETQ LINE (+ TOP-ROW LINE)))
    (OTHERWISE (FERROR NIL "Illegal scroll mode ~A" MODE)))
  (COND (( (SETQ LINE (MAX 0 (MIN LINE (1- TOTAL-ROWS)))) TOP-ROW)
	 ;; Actually changing something, update
	 (SETQ TOP-ROW LINE)
	 (FUNCALL-SELF ':MENU-DRAW)
	 (FUNCALL-SELF ':NEW-SCROLL-POSITION TOP-ROW))))

;;; Put a menu near another window.  This will normally try to put it just below
;;; it and give it the same width.
(DEFMETHOD (BASIC-MENU :MOVE-NEAR-WINDOW) (W)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
      (FUNCALL W ':EDGES)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE NEW-HEIGHT)
	(MENU-DEDUCE-PARAMETERS NIL NIL (- RIGHT LEFT) NIL NIL NIL)
      (SETQ NEW-HEIGHT (+ NEW-HEIGHT TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
      ;If it won't fit below try putting it above
      (AND (> (+ BOTTOM NEW-HEIGHT)
	      (SHEET-INSIDE-BOTTOM SUPERIOR))
	   (SETQ BOTTOM (MAX (- TOP NEW-HEIGHT) 0)))
      ;Put it there
      (LET-GLOBALLY ((SET-EDGES-MODE ':MOVE-NEAR))
	(FUNCALL-SELF ':SET-EDGES LEFT BOTTOM RIGHT (+ BOTTOM NEW-HEIGHT)))
      (FUNCALL-SELF ':EXPOSE))))

;;; This is used by othogonal things like hysteretic window
(DEFMETHOD (BASIC-MENU :SCROLL-BAR-P) () (< SCREEN-ROWS TOTAL-ROWS))

;;; MENU-EXECUTE-MIXIN flavor processes a menu-like item

(DEFFLAVOR MENU-EXECUTE-MIXIN () ()
  (:DOCUMENTATION :MIXIN "Processes a menu-like item
This is a part of every menu, it is a separate flavor so that it can be included in other
things which want to act like menus with regard to the format of an item passed to a
:execute message.  This message is what handles most of the interpretation of the
item-list instance variable."))

;;; Decide what to return based on the item selected.  Also have side-effects
;;; such as calling a function if the item says to.
(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE) (ITEM &AUX OP ARG)
  (COND ((ATOM ITEM) ITEM)
	((ATOM (CDR ITEM)) (CDR ITEM))
	((ATOM (CDDR ITEM)) (CADR ITEM))
	((EQ (SETQ ARG (CADDR ITEM)
		   OP (CADR ITEM))
	     ':VALUE)
	 ARG)
	((EQ OP ':EVAL) (EVAL ARG))
	((EQ OP ':FUNCALL) (FUNCALL ARG))
	((EQ OP ':WINDOW-OP) (FUNCALL-SELF ':EXECUTE-WINDOW-OP ARG))
	((EQ OP ':KBD) (AND SELECTED-WINDOW (FUNCALL SELECTED-WINDOW ':FORCE-KBD-INPUT ARG)))
	((EQ OP ':MENU) (FUNCALL (IF (SYMBOLP ARG) (SYMEVAL ARG) ARG) ':CHOOSE))
	(T (FERROR NIL "~S is unknown operation for :EXECUTE" OP))))

;Same as above but returns NIL if getting the value would require side-effects.
;This is used by MENU-HIGHLIGHTING-MIXIN
(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE-NO-SIDE-EFFECTS) (ITEM &AUX OP ARG)
  (COND ((ATOM ITEM) ITEM)
	((ATOM (CDR ITEM)) (CDR ITEM))
	((ATOM (CDDR ITEM)) (CADR ITEM))
	((EQ (SETQ ARG (CADDR ITEM)
		   OP (CADR ITEM))
	     ':VALUE)
	 ARG)
	(T NIL)))

(DEFMETHOD (MENU-EXECUTE-MIXIN :EXECUTE-WINDOW-OP) (FUNCTION) (FUNCALL FUNCTION))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-MENU)
(DEFUN MENU-COMPUTE-GEOMETRY (DRAW-P &OPTIONAL INSIDE-WIDTH INSIDE-HEIGHT)
  "This function is called whenever something related to the geometry changes.  The menu
is redrawn if DRAW-P is T."
  (COND ((BOUNDP 'ITEM-LIST)  ;Do nothing if item-list not specified yet
	 ;; Get the new N-ROWS and so forth.
	 (MULTIPLE-VALUE (COLUMNS SCREEN-ROWS INSIDE-WIDTH INSIDE-HEIGHT)
	   (MENU-DEDUCE-PARAMETERS NIL NIL INSIDE-WIDTH INSIDE-HEIGHT NIL NIL))
	 ;; Recompute the row map
	 (MULTIPLE-VALUE (ROW-MAP TOTAL-ROWS)
	   (MENU-COMPUTE-ROW-MAP INSIDE-WIDTH))
	 (SETQ TOP-ROW 0
	       ROW-HEIGHT LINE-HEIGHT)
	 (FUNCALL-SELF ':NEW-SCROLL-POSITION TOP-ROW)
	 (SETQ COLUMN-WIDTH
	       (AND (NOT (GEOMETRY-FILL-P GEOMETRY))
		    (// (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING) COLUMNS)))
	 (IF (OR ( INSIDE-HEIGHT (SHEET-INSIDE-HEIGHT))
		 ( INSIDE-WIDTH (SHEET-INSIDE-WIDTH)))
	     (LET-GLOBALLY ((SET-EDGES-MODE T))
	       (FUNCALL-SELF ':SET-INSIDE-SIZE INSIDE-WIDTH INSIDE-HEIGHT))
	     (AND DRAW-P
		  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
		    (FUNCALL-SELF ':MENU-DRAW))))))
  NIL))

;;; This function, given a bunch of parameters some of which are NIL meaning
;;; unspecified, deduces the rest of the parameters from constraints.
;;; For parameters passed in as NIL, the corresponding element of GEOMETRY
;;; is used.

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
;;;	    Not actually square but the prettiest looking shape.

;;; Once the horizontal business has been straightened out, if we don't have the
;;; height already, we pick a height to make it all fit on the screen, and limit that
;;; if it is too big.  Note that fill-mode has a line-breakage problem, which will
;;; need to be solved here (may change shape "slightly" from square.)

;;; Arguments:
;;; SELF, ITEM-LIST and GEOMETRY are used freely.
;;; SELF should have the right font, screen, vsp but not
;;;	  yet the right dimensions and location.
;;; NEW-LEFT, etc. are the boundaries of the area actually available for use.
;;;  Any margins have already been excluded.

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-MENU)
(DEFUN MENU-DEDUCE-PARAMETERS (N-COLUMNS N-ROWS INSIDE-WIDTH INSIDE-HEIGHT
			       MAX-WIDTH MAX-HEIGHT
			       &AUX TEM COLUMN-WIDTH ;NIL if N-COLUMNS not chosen in here
				    (N-ITEMS (LENGTH ITEM-LIST))
				    FILL-P)
  ;; Pick up default constraints from GEOMETRY
  (SETQ N-COLUMNS (OR N-COLUMNS (GEOMETRY-N-COLUMNS GEOMETRY))
	N-ROWS (OR N-ROWS (GEOMETRY-N-ROWS GEOMETRY))
	INSIDE-WIDTH (OR INSIDE-WIDTH (GEOMETRY-INSIDE-WIDTH GEOMETRY))
	INSIDE-HEIGHT (OR INSIDE-HEIGHT (GEOMETRY-INSIDE-HEIGHT GEOMETRY))
	MAX-WIDTH (OR MAX-WIDTH (GEOMETRY-MAX-WIDTH GEOMETRY))
	MAX-HEIGHT (OR MAX-HEIGHT (GEOMETRY-MAX-HEIGHT GEOMETRY)))
  ;; If any of the arguments was :UNCONSTRAINED, that means use NIL
  ;; even if the geometry is non-NIL, whereas if an argument was NIL
  ;; that means use any constraint that is in the geometry.
  (AND (EQ N-COLUMNS ':UNCONSTRAINED) (SETQ N-COLUMNS NIL))
  (AND (EQ N-ROWS ':UNCONSTRAINED) (SETQ N-ROWS NIL))
  (AND (EQ INSIDE-WIDTH ':UNCONSTRAINED) (SETQ INSIDE-WIDTH NIL))
  (AND (EQ INSIDE-HEIGHT ':UNCONSTRAINED) (SETQ INSIDE-HEIGHT NIL))
  (AND (EQ MAX-WIDTH ':UNCONSTRAINED) (SETQ MAX-WIDTH NIL))
  (AND (EQ MAX-HEIGHT ':UNCONSTRAINED) (SETQ MAX-HEIGHT NIL))
  ;; Decide whether it is fill mode or array mode
  (AND (SETQ FILL-P (AND N-COLUMNS (ZEROP N-COLUMNS)))
       (SETQ N-COLUMNS NIL))

  ;; Realize any immediately clear implications
  (AND N-ROWS (NULL INSIDE-HEIGHT) (SETQ INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))
  (AND INSIDE-HEIGHT (NULL N-ROWS) (SETQ N-ROWS (// INSIDE-HEIGHT LINE-HEIGHT)))
  (SETQ MAX-HEIGHT (MIN (OR INSIDE-HEIGHT MAX-HEIGHT 10000)
			(- (SHEET-INSIDE-HEIGHT SUPERIOR) TOP-MARGIN-SIZE BOTTOM-MARGIN-SIZE))
	MAX-WIDTH (MIN (OR INSIDE-WIDTH MAX-WIDTH 10000)
		       (- (SHEET-INSIDE-WIDTH SUPERIOR) LEFT-MARGIN-SIZE RIGHT-MARGIN-SIZE)))

  ;; Compute the horizontal parameters.
  (COND ((AND INSIDE-WIDTH (OR N-COLUMNS FILL-P)) )		;It's fully-determined
	(INSIDE-WIDTH	;We have the width, and it's not in fill mode, compute N-COLUMNS
	 (SETQ N-COLUMNS (MAX (// (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING)
				  (+ (MENU-MAX-WIDTH ITEM-LIST) MENU-INTERCOLUMN-SPACING))
			      1)))
	(N-COLUMNS  ;We don't have the width, but do know how many columns, compute width
	 (SETQ INSIDE-WIDTH (MIN (- (* (+ (MENU-MAX-WIDTH ITEM-LIST)
					  MENU-INTERCOLUMN-SPACING)
				       N-COLUMNS)
				    MENU-INTERCOLUMN-SPACING)
				 MAX-WIDTH)))
	(N-ROWS  ;We know how high, make it wide enough to come out this high
	 (IF FILL-P
	     (SETQ INSIDE-WIDTH (MIN (// (+ (MENU-FILL-WIDTH ITEM-LIST)
					    (1- N-ROWS))
					 N-ROWS)
				     MAX-WIDTH))
	     (SETQ N-COLUMNS (MAX (// (+ N-ITEMS (1- N-ROWS)) N-ROWS) 1)
		   INSIDE-WIDTH (- (* (SETQ COLUMN-WIDTH (+ (MENU-MAX-WIDTH ITEM-LIST)
							    MENU-INTERCOLUMN-SPACING))
				      N-COLUMNS)
				   MENU-INTERCOLUMN-SPACING))))
	((NOT FILL-P) ;No geometry supplied, pick N-ROWS and N-COLUMNS to make it look nice
		      ;Use the largest number of columns which does not make the ratio
		      ;of height to width less than the Golden ratio
	 (SETQ TEM (* (SETQ COLUMN-WIDTH (MENU-MAX-WIDTH ITEM-LIST))
                      N-ITEMS
                      LINE-HEIGHT)
	       COLUMN-WIDTH (+ COLUMN-WIDTH MENU-INTERCOLUMN-SPACING)
	       N-COLUMNS (MAX (// (ISQRT (FIX (// TEM MENU-GOLDEN-RATIO))) COLUMN-WIDTH) 1)
	       INSIDE-WIDTH (- (* COLUMN-WIDTH N-COLUMNS) MENU-INTERCOLUMN-SPACING)))
	(T	;No geometry supplied, and in fill mode, make it like above
	 (SETQ INSIDE-WIDTH (MAX (ISQRT (FIX (// (* (MENU-FILL-WIDTH ITEM-LIST)
						    LINE-HEIGHT)
						 MENU-GOLDEN-RATIO)))
				 40))))  ;Don't get zero, and don't get absurdly small

  ;; Now figure out the vertical characteristics
  (OR N-ROWS
      (SETQ N-ROWS (IF FILL-P 
		       (// (+ (MENU-FILL-WIDTH ITEM-LIST) INSIDE-WIDTH -1) INSIDE-WIDTH)
		       (// (+ N-ITEMS N-COLUMNS -1) N-COLUMNS))))
  (OR INSIDE-HEIGHT (SETQ INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))

  ;; If there is a label, the menu must be at least wide enough to accomodate it
  (LET ((L (GET-HANDLER-FOR SELF ':LABEL-SIZE)))
    (AND L (SETQ L (FUNCALL L ':LABEL-SIZE))
	 (SETQ INSIDE-WIDTH (MAX INSIDE-WIDTH L))))

  ;; If this came out too high or too wide, retrench
  (AND (> INSIDE-HEIGHT MAX-HEIGHT)
       (SETQ N-ROWS (// MAX-HEIGHT LINE-HEIGHT)
	     INSIDE-HEIGHT (* N-ROWS LINE-HEIGHT)))
  (COND ((> INSIDE-WIDTH MAX-WIDTH)
	 (SETQ INSIDE-WIDTH MAX-WIDTH)
	 (AND COLUMN-WIDTH  ;If N-COLUMNS was not user-supplied, recompute it
	      (SETQ N-COLUMNS (MAX (// (+ INSIDE-WIDTH MENU-INTERCOLUMN-SPACING)
				       COLUMN-WIDTH)
				   1)))))

  ;; At this point, INSIDE-WIDTH, INSIDE-HEIGHT, N-COLUMNS (if not FILL-P), and N-ROWS
  ;; are all valid and consistent, and not bigger than the available area,
  ;; provided that the user's original parameters were not illegally huge.

  ;; Return all the dependent parameters as multiple values
  (PROG () (RETURN (IF FILL-P 0 N-COLUMNS) N-ROWS INSIDE-WIDTH INSIDE-HEIGHT)))
)

;;; This function computes the ROW-MAP, which determines how many strings per line, & c.
;;; The first value is the row-map and the second is the n-total-rows
(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-MENU)
(DEFUN MENU-COMPUTE-ROW-MAP (&OPTIONAL (INSIDE-WIDTH (SHEET-INSIDE-WIDTH))
			     &AUX (MAP (MAKE-ARRAY NIL 'ART-Q
						   (1+ (LENGTH ITEM-LIST))))
				  WID
				  (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  (DO ((ITEMS ITEM-LIST) (ROW 0 (1+ ROW)))
      ((NULL ITEMS)
       (PROG () (RETURN (ADJUST-ARRAY-SIZE MAP (1+ ROW))  ;Last element always contains NIL
			ROW)))
    (ASET ITEMS MAP ROW)	;This is where this row starts
    (IF FILL-P		;Fill mode, we have some hairy calculation to do
	(DO ((SPACE INSIDE-WIDTH))
	    ((NULL ITEMS))
	  (SETQ WID (MENU-ITEM-STRING-WIDTH (CAR ITEMS)))
	  (COND ((> WID SPACE)	;This one won't fit, break the line
		 (AND (> WID INSIDE-WIDTH)
		      (FERROR NIL "The item /"~A/" is too wide for this fill-mode menu"
			      (CAR ITEMS)))
		 (RETURN NIL)))
	  (SETQ SPACE (- SPACE (+ WID MENU-INTERWORD-SPACING))
		ITEMS (CDR ITEMS)))
	(SETQ ITEMS (NTHCDR COLUMNS ITEMS)))))
)

(DEFMETHOD (BASIC-MENU :MENU-DRAW) (&AUX (FILL-P (GEOMETRY-FILL-P GEOMETRY)))
  ;; Make sure the mouse knows we're changing
  (AND EXPOSED-P (MOUSE-WAKEUP))
  (PREPARE-SHEET (SELF)
    (SHEET-CLEAR SELF)
    (DO ((ROW TOP-ROW (1+ ROW))
	 (Y-POS 0 (+ Y-POS ROW-HEIGHT))
	 (LIM (MIN TOTAL-ROWS (+ TOP-ROW SCREEN-ROWS))))
	(( ROW LIM))
      (DO ((ITEMS (AREF ROW-MAP ROW) (CDR ITEMS))
	   (END-ITEM-LIST (AREF ROW-MAP (1+ ROW)))
	   (STR) (FONT) (FLAG)
	   (X-POS 0))
	  ((EQ ITEMS END-ITEM-LIST))
	 (MULTIPLE-VALUE (STR FONT)
	   (MENU-ITEM-STRING (CAR ITEMS)))
	 (UNWIND-PROTECT
	   (PROGN
	     (AND (SETQ FLAG (AND (NEQ FONT CURRENT-FONT) CURRENT-FONT))
		  (SHEET-SET-FONT SELF FONT))
	     (COND (FILL-P			;Filled, put string followed by spacing
		    (SHEET-SET-CURSORPOS SELF X-POS Y-POS)
		    (SHEET-STRING-OUT SELF STR)
		    (SETQ X-POS (+ (SHEET-READ-CURSORPOS SELF) MENU-INTERWORD-SPACING)))
		   (T				;Columnated, center text within column
		    (SHEET-DISPLAY-CENTERED-STRING SELF STR
						   X-POS
						   (- (SETQ X-POS (+ X-POS COLUMN-WIDTH))
						      MENU-INTERCOLUMN-SPACING)
						   Y-POS))))
	   (AND FLAG (SHEET-SET-FONT SELF FLAG)))))))

;;; This function, given a menu item-list, returns the maximum width
;;; of any string in it.  Normally you want to add an allowance for interword spacing.
(DEFUN MENU-MAX-WIDTH (ITEM-LIST)
  (DO ((L ITEM-LIST (CDR L))
       (MXW 1 (MAX MXW (MENU-ITEM-STRING-WIDTH (CAR L)))))
      ((NULL L) MXW)))

;;; This function, given a menu item-list, returns the estimated total width
;;; (product of WIDTH and N-ROWS, not counting borders) when the menu items
;;; are put in "fill mode" rather than in columns.  It is only an estimate
;;; because at this point we can't be sure about word breakage.
(DEFUN MENU-FILL-WIDTH (ITEM-LIST)
  (DO ((L ITEM-LIST (CDR L))
       (WID 0))
      ((NULL L) WID)
    (SETQ WID (+ WID (MENU-ITEM-STRING-WIDTH (CAR L)) MENU-FILL-BREAKAGE))))

;;; Here is how we make a menu appear with the last item chosen under the mouse.

;;; Return the x and y co-ordinates (inside the margins)
;;; of the center of the specified item, NIL if scrolled off display
(DEFMETHOD (BASIC-MENU :ITEM-CURSORPOS) (ITEM)
  (DO ((ROW (+ TOP-ROW SCREEN-ROWS -1) (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (AND (MEMQ ITEM (AREF ROW-MAP ROW))
	 (RETURN (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
		     (+ (* (FIND-POSITION-IN-LIST ITEM (AREF ROW-MAP ROW)) COLUMN-WIDTH)
			(// COLUMN-WIDTH 2))
		     (DO ((L (AREF ROW-MAP ROW) (CDR L))
			  (XSTART 0 (+ XSTART SWIDTH MENU-INTERWORD-SPACING))
			  (SWIDTH))
			 (NIL)
		       (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH (CAR L)))
		       (AND (EQ (CAR L) ITEM)
			    (RETURN (+ XSTART (// SWIDTH 2))))))
		 (+ (* (- ROW TOP-ROW) ROW-HEIGHT) (// ROW-HEIGHT 2))))))

;;; Return the left, top, right, bottom coordinates (inside the margins)
;;; of the rectangle enclosing the specified item, including one bit of
;;; margin all around, or NIL if scrolled off the display.
;;; Note that because of the one bit of margin, returned values can be outside
;;; the window.
(DEFMETHOD (BASIC-MENU :ITEM-RECTANGLE) (ITEM &AUX (X 0) SWIDTH)
  (DO ((ROW (+ TOP-ROW SCREEN-ROWS -1) (1- ROW)))
      ((< ROW TOP-ROW) NIL)
    (COND ((MEMQ ITEM (AREF ROW-MAP ROW))
	   (IF (NOT (GEOMETRY-FILL-P GEOMETRY))
	       (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH ITEM COLUMN-WIDTH)
		     X (+ (* (FIND-POSITION-IN-LIST ITEM (AREF ROW-MAP ROW)) COLUMN-WIDTH)
			  (// (- COLUMN-WIDTH MENU-INTERCOLUMN-SPACING SWIDTH) 2)))
	       (DOLIST (IT (AREF ROW-MAP ROW))
		 (SETQ SWIDTH (MENU-ITEM-STRING-WIDTH IT))
		 (AND (EQ IT ITEM) (RETURN))
		 (SETQ X (+ X SWIDTH MENU-INTERWORD-SPACING))))
	   (RETURN (1- X) (1- (* (- ROW TOP-ROW) ROW-HEIGHT))
		   (+ X SWIDTH 1) (1- (* (1+ (- ROW TOP-ROW)) ROW-HEIGHT)))))))

;; When we move a menu to a spot, make it go so that the last item chosen
;; appears at that spot.
(DEFMETHOD (BASIC-MENU :CENTER-AROUND) (X Y &AUX (XI 0) (YI 0))
  (AND (BOUNDP 'LAST-ITEM)
       (MEMQ LAST-ITEM ITEM-LIST)
       ;; If we remember a previous choice,
       ;; let XI and YI get the offsets from that item to the center.
       (MULTIPLE-VALUE-BIND (X1 Y1)
	   (FUNCALL-SELF ':ITEM-CURSORPOS LAST-ITEM)
	 (SETQ XI (- (// WIDTH 2) X1)
	       YI (- (// HEIGHT 2) Y1))))
  (MULTIPLE-VALUE-BIND (X1 Y1)
      (CENTER-WINDOW-AROUND SELF (+ X XI) (+ Y YI))
    (PROG () (RETURN (- X1 XI) (- Y1 YI)))))

(DEFMETHOD (BASIC-MENU :COLUMN-ROW-SIZE) ()
  (PROG () (RETURN COLUMN-WIDTH ROW-HEIGHT)))

;; Permanent menus for giving "keyboard" commands from a menu alist
(DEFFLAVOR COMMAND-MENU-MIXIN (IO-BUFFER) ()
  (:INCLUDED-FLAVORS BASIC-MENU)
  (:SETTABLE-INSTANCE-VARIABLES IO-BUFFER))

(DEFMETHOD (COMMAND-MENU-MIXIN :AFTER :MOUSE-BUTTONS) (BD IGNORE IGNORE)
  (COND (CHOSEN-ITEM
	 (IO-BUFFER-PUT IO-BUFFER `(:MENU ,CHOSEN-ITEM ,BD ,SELF))
	 (SETQ CHOSEN-ITEM NIL))))

(DEFFLAVOR COMMAND-MENU () (COMMAND-MENU-MIXIN MENU))

(DEFFLAVOR MENU-HIGHLIGHTING-MIXIN ((HIGHLIGHTED-ITEMS NIL)) ()
  (:INCLUDED-FLAVORS TV:BASIC-MENU)
  (:GETTABLE-INSTANCE-VARIABLES HIGHLIGHTED-ITEMS)
  (:INITABLE-INSTANCE-VARIABLES HIGHLIGHTED-ITEMS)
  (:DEFAULT-INIT-PLIST :BLINKER-FUNCTION 'TV:HOLLOW-RECTANGULAR-BLINKER)
  (:DOCUMENTATION :MIXIN "Provides for highlighting of items with inverse video"))

; This does not remember it on the list, you probably don't want to use it yourself
(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :HIGHLIGHT-ITEM) (ITEM)
  (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM) (FUNCALL-SELF ':ITEM-RECTANGLE ITEM)
    (AND (NOT (NULL LEFT))
	 (PREPARE-SHEET (SELF)			;Clip but allow extension into margins
	   (SETQ LEFT (MAX (+ LEFT (SHEET-INSIDE-LEFT)) 0)
		 RIGHT (MIN (+ RIGHT (SHEET-INSIDE-LEFT)) WIDTH)
		 TOP (MAX (+ TOP (SHEET-INSIDE-TOP)) 0)
		 BOTTOM (MIN (+ BOTTOM (SHEET-INSIDE-TOP)) HEIGHT))
	   (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ALU-XOR SELF)))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :ADD-HIGHLIGHTED-ITEM) (ITEM)
  (COND ((NOT (MEMQ ITEM HIGHLIGHTED-ITEMS))
	 (PUSH ITEM HIGHLIGHTED-ITEMS)
	 (SHEET-FORCE-ACCESS (SELF T) (FUNCALL-SELF ':HIGHLIGHT-ITEM ITEM)))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :REMOVE-HIGHLIGHTED-ITEM) (ITEM)
  (COND ((MEMQ ITEM HIGHLIGHTED-ITEMS)
	 (SETQ HIGHLIGHTED-ITEMS (DELQ ITEM HIGHLIGHTED-ITEMS))
	 (SHEET-FORCE-ACCESS (SELF T) (FUNCALL-SELF ':HIGHLIGHT-ITEM ITEM)))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :SET-HIGHLIGHTED-ITEMS) (NEW-HIGHLIGHTED-ITEMS &AUX OLD)
  (SETQ OLD HIGHLIGHTED-ITEMS
	HIGHLIGHTED-ITEMS NEW-HIGHLIGHTED-ITEMS)
  (SHEET-FORCE-ACCESS (SELF T)
    (DOLIST (X OLD)
      (OR (MEMQ X NEW-HIGHLIGHTED-ITEMS) (FUNCALL-SELF ':HIGHLIGHT-ITEM X)))
    (DOLIST (X NEW-HIGHLIGHTED-ITEMS)
      (OR (MEMQ X OLD) (FUNCALL-SELF ':HIGHLIGHT-ITEM X)))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :AFTER :MENU-DRAW) ()
  (DOLIST (X HIGHLIGHTED-ITEMS)
    (FUNCALL-SELF ':HIGHLIGHT-ITEM X)))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :HIGHLIGHTED-VALUES) ()
  (MAPCAR #'(LAMBDA (X) (FUNCALL-SELF ':EXECUTE-NO-SIDE-EFFECTS X)) HIGHLIGHTED-ITEMS))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :SET-HIGHLIGHTED-VALUES) (VALUES &AUX ITEMS)
  (DOLIST (ITEM ITEM-LIST)
    (AND (MEMBER (FUNCALL-SELF ':EXECUTE-NO-SIDE-EFFECTS ITEM) VALUES)
	 (PUSH ITEM ITEMS)))
  (OR (= (LENGTH ITEMS) (LENGTH VALUES))
      (FERROR NIL "Missing or duplicate value"))
  (FUNCALL-SELF ':SET-HIGHLIGHTED-ITEMS ITEMS))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :ADD-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR NIL "Value not found"))
    (AND (EQUAL (FUNCALL-SELF ':EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
	 (RETURN (FUNCALL-SELF ':ADD-HIGHLIGHTED-ITEM (CAR L))))))

(DEFMETHOD (MENU-HIGHLIGHTING-MIXIN :REMOVE-HIGHLIGHTED-VALUE) (VALUE)
  (DO ((L ITEM-LIST (CDR L)))
      ((NULL L) (FERROR NIL "Value not found"))
    (AND (EQUAL (FUNCALL-SELF ':EXECUTE-NO-SIDE-EFFECTS (CAR L)) VALUE)
	 (RETURN (FUNCALL-SELF ':REMOVE-HIGHLIGHTED-ITEM (CAR L))))))


;******** This doesn't work because it has to tell the geometry guy to leave enough
;******** width for the margin choices to fit in.  As it stands it actually manages
;******** to draw outside of its assigned area of the screen.
(DEFFLAVOR MENU-MARGIN-CHOICE-MIXIN () (MARGIN-CHOICE-MIXIN)
  (:INCLUDED-FLAVORS BASIC-MENU)
  (:DOCUMENTATION :MIXIN "Puts choice boxes in the bottom margin of a menu.
Clicking on a choice box simulates clicking on a menu item")
  (:INIT-KEYWORDS :MENU-MARGIN-CHOICES))

;An element of :MENU-MARGIN-CHOICES is just like an element of :ITEM-LIST

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (LET ((CURRENT-FONT NIL))	;MENU-ITEM-STRING looks at this
    (SETQ MARGIN-CHOICES
	  (MAPCAR #'MENU-MARGIN-CHOICE-FROM-ITEM (GET INIT-PLIST ':MENU-MARGIN-CHOICES)))))

(DEFMETHOD (MENU-MARGIN-CHOICE-MIXIN :SET-MENU-MARGIN-CHOICES) (LIST)
  (FUNCALL-SELF ':SET-MARGIN-CHOICES (MAPCAR #'MENU-MARGIN-CHOICE-FROM-ITEM LIST)))

(DEFUN MENU-MARGIN-CHOICE-FROM-ITEM (X)
  (LIST (MENU-ITEM-STRING X) NIL 'MENU-MARGIN-CHOICE-FUNCTION NIL NIL X))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (MENU-MARGIN-CHOICE-MIXIN)
(DEFUN MENU-MARGIN-CHOICE-FUNCTION (CHOICE-BOX REGION Y-POS)
  REGION Y-POS ;ignored
  (SETQ CHOSEN-ITEM (SIXTH CHOICE-BOX))))

(DEFFLAVOR MULTIPLE-MENU-MIXIN (SPECIAL-CHOICE-ITEMS) (MENU-HIGHLIGHTING-MIXIN)
  (:INIT-KEYWORDS :SPECIAL-CHOICES)
  (:DEFAULT-INIT-PLIST :FONT-MAP '(FONTS:MEDFNT FONTS:HL12I)
		       :SPECIAL-CHOICES '(("Do It"
					   :EVAL (FUNCALL-SELF ':HIGHLIGHTED-VALUES))))
  (:DOCUMENTATION :MIXIN "A menu in which you can select more than one choice.
 HIGHLIGHTED-ITEMS is a list of those items in the ITEM-LIST that are currently
 selected.  SPECIAL-CHOICES are those items that don't highlight when
 you click on them, but instead are executed in the usual way.  The default
 special choice is just Done, which returns a list of the values of the highlighted
 items.  SPECIAL-CHOICES are displayed in italics at the top of the menu."))

(DEFMETHOD (MULTIPLE-MENU-MIXIN :BEFORE :INIT) (INIT-PLIST)
  (SETQ SPECIAL-CHOICE-ITEMS
	(MAPCAR #'(LAMBDA (X)
		    (APPEND (COND ((ATOM X) (LIST X ':VALUE X))
				  ((ATOM (CDR X)) (LIST (CAR X) ':VALUE (CDR X)))
				  ((NULL (CDDR X)) (LIST (CAR X) ':VALUE (CADR X)))
				  (T X))
			    '(:FONT FONTS:HL12I :SPECIAL-CHOICE T)))
		(GET INIT-PLIST ':SPECIAL-CHOICES)))
  (AND (BOUNDP 'ITEM-LIST)			;Only if items specified in init-plist
       (SETQ ITEM-LIST (MULTIPLE-MENU-HACK-ITEM-LIST ITEM-LIST (GET INIT-PLIST ':COLUMNS)))))

(DEFMETHOD (MULTIPLE-MENU-MIXIN :SET-ITEM-LIST) (NEW-ITEM-LIST)
  (SETQ ITEM-LIST (MULTIPLE-MENU-HACK-ITEM-LIST NEW-ITEM-LIST)
	LAST-ITEM NIL
	CURRENT-ITEM NIL)
  (MENU-COMPUTE-GEOMETRY T)		;Recompute parameters, and redraw menu
  ITEM-LIST)

;Insert the special-choices into the item-list
;Buglet - if n-columns is not specified explicitly, and turns out to be more than 1,
;there will not be automatic blank space inserted to keep the special-choices on
;a separate row.  There is probably nothing wrong with this.
(DECLARE-FLAVOR-INSTANCE-VARIABLES (MULTIPLE-MENU-MIXIN)
(DEFUN MULTIPLE-MENU-HACK-ITEM-LIST (ITM-LST &OPTIONAL N-COLUMNS)
  (SETQ N-COLUMNS (OR N-COLUMNS (CAR GEOMETRY) 1))
  (APPEND SPECIAL-CHOICE-ITEMS
	  (AND N-COLUMNS (> N-COLUMNS 1)
	       (DO ((N (\ (LENGTH SPECIAL-CHOICE-ITEMS) N-COLUMNS) (1+ N))
		    (L NIL (CONS '("" :NO-SELECT NIL) L)))
		   ((OR (ZEROP N) (= N N-COLUMNS)) L)))
	  ITM-LST)))

;Modified mouse-button handler.  Does normal thing for special-choices, otherwise
;just complements highlight state.
(DEFMETHOD (MULTIPLE-MENU-MIXIN :MOUSE-BUTTONS) (BD X Y &AUX ITEM)
  X Y ;ignored, we don't care where the mouse is, the :MOUSE-MOVES method took care of that
  (COND ((NOT EXPOSED-P)	;Button pushed while not exposed, expose self.
	 (FUNCALL-SELF ':EXPOSE))
	((BIT-TEST 2 BD)	;Middle button, get documentation
	 (FUNCALL-SELF ':DOCUMENT))
	((NULL (SETQ ITEM CURRENT-ITEM)))
	((AND (NOT (ATOM ITEM))	;Special-choice selected?
	      (NOT (ATOM (CDR ITEM)))
	      (GET (CDDR ITEM) ':SPECIAL-CHOICE))
	 (SETQ LAST-ITEM CURRENT-ITEM
	       CHOSEN-ITEM CURRENT-ITEM))
	(T			;Ordinary item, highlight or un-highlight it
	 (FUNCALL-SELF (IF (MEMQ ITEM HIGHLIGHTED-ITEMS) ':REMOVE-HIGHLIGHTED-ITEM
			   ':ADD-HIGHLIGHTED-ITEM)
		       ITEM))))

(DEFFLAVOR MULTIPLE-MENU () (MULTIPLE-MENU-MIXIN MENU))

(DEFFLAVOR MOMENTARY-MULTIPLE-MENU () (MULTIPLE-MENU-MIXIN MOMENTARY-MENU))

(DEFFLAVOR BASIC-MOMENTARY-MENU () (HYSTERETIC-WINDOW-MIXIN BASIC-MENU)
  (:DOCUMENTATION :MIXIN "A menu that holds control of the mouse.
Menus of this type handle the mouse for a small area outside of their
actual edges.  They also are automatically deactivated whenever an item
is chosen or the mouse moves even further, out of its control."))

(DEFMETHOD (BASIC-MOMENTARY-MENU :BEFORE :CHOOSE) ()
  (COND ((NOT EXPOSED-P)
	 (MULTIPLE-VALUE-BIND (X-OFF Y-OFF)
	     (SHEET-CALCULATE-OFFSETS SUPERIOR MOUSE-SHEET)
	   (MULTIPLE-VALUE-BIND (X Y)
	       (FUNCALL-SELF ':CENTER-AROUND (- MOUSE-X X-OFF) (- MOUSE-Y Y-OFF))
	     (MOUSE-WARP (+ X X-OFF) (+ Y Y-OFF))))
	 ;; Expose self, and seize the mouse.
	 (FUNCALL-SELF ':EXPOSE))))

;;; When no selection, but mouse moved out of range, deexpose menu
(DEFMETHOD (BASIC-MOMENTARY-MENU :AFTER :HANDLE-MOUSE) ()
  (OR CHOSEN-ITEM
      (AND MOUSE-RECONSIDER (EQ SELF (WINDOW-OWNING-MOUSE)))
      ;; This is called in the mouse process.  We don't want to take the chance that
      ;; we might go blocked, so run in another process.
      (PROCESS-RUN-FUNCTION "Menu Deactivate" SELF ':DEACTIVATE)))

;;; Make MOUSE-DEFAULT-HANDLER return so menu gets deactivated.
(DEFMETHOD (BASIC-MOMENTARY-MENU :AFTER :MOUSE-BUTTONS) (IGNORE IGNORE IGNORE)
  (SETQ MOUSE-RECONSIDER T))

(DEFMETHOD (BASIC-MOMENTARY-MENU :BEFORE :EXECUTE) (&REST IGNORE)
  (FUNCALL-SELF ':DEACTIVATE))

(DEFFLAVOR WINDOW-HACKING-MENU-MIXIN (WINDOW-UNDER-MENU OLD-X OLD-Y) ()
  (:DOCUMENTATION :MIXIN "Menu which handles :WINDOW-OP when called over another window
The window that the menu is exposed over is remembered when the :choose message is sent,
and then used if a :window-op type item is selected."))

(DEFMETHOD (WINDOW-HACKING-MENU-MIXIN :BEFORE :CHOOSE) ()
  (SETQ WINDOW-UNDER-MENU (LOWEST-SHEET-UNDER-POINT MOUSE-SHEET MOUSE-X MOUSE-Y)
	OLD-X MOUSE-X
	OLD-Y MOUSE-Y))

(DEFMETHOD (WINDOW-HACKING-MENU-MIXIN :EXECUTE-WINDOW-OP) (FUNCTION)
  (FUNCALL FUNCTION WINDOW-UNDER-MENU OLD-X OLD-Y))


(DEFFLAVOR DYNAMIC-ITEM-LIST-MIXIN ((ITEM-LIST-POINTER NIL)) ()
  :INITABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:INCLUDED-FLAVORS BASIC-MENU)
  (:DEFAULT-INIT-PLIST :ITEM-LIST NIL)
  (:DOCUMENTATION :MIXIN "Allows the menu to have an item list that's being dynamically
modified.  Causes the menu's item list to be updated at appropriate times."))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (DYNAMIC-ITEM-LIST-MIXIN)
(DEFUN DYNAMIC-ITEM-LIST ()
  (IF (SYMBOLP ITEM-LIST-POINTER)
      (SYMEVAL ITEM-LIST-POINTER)
      (EVAL ITEM-LIST-POINTER))))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :INIT) (IGNORE)
  (AND ITEM-LIST-POINTER
       (SETQ ITEM-LIST (DYNAMIC-ITEM-LIST))))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :UPDATE-ITEM-LIST) (&AUX NEW-ITEM-LIST)
  (AND ITEM-LIST-POINTER
       (OR (EQUAL ITEM-LIST (SETQ NEW-ITEM-LIST (DYNAMIC-ITEM-LIST)))
	   (FUNCALL-SELF ':SET-ITEM-LIST NEW-ITEM-LIST))))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :CHOOSE) (&REST IGNORE)
  (FUNCALL-SELF ':UPDATE-ITEM-LIST))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :MOVE-NEAR-WINDOW) (&REST IGNORE)
  (FUNCALL-SELF ':UPDATE-ITEM-LIST))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :CENTER-AROUND) (&REST IGNORE)
  (FUNCALL-SELF ':UPDATE-ITEM-LIST))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :SIZE) (&REST IGNORE)
  (FUNCALL-SELF ':UPDATE-ITEM-LIST))

(DEFMETHOD (DYNAMIC-ITEM-LIST-MIXIN :BEFORE :PANE-SIZE) (&REST IGNORE)
  (FUNCALL-SELF ':UPDATE-ITEM-LIST))

;;; Menus to be used for a momentary choice.
;;; Send a menu of this type a :CHOOSE message to use the menu.
;;; When the user selects an item, or moves the mouse off the menu,
;;; the menu will disappear, and whatever was underneath it will reappear.
;;; It will return the chosen item, or NIL.  If the item is not atomic
;;; and its cadr is non-NIL, the cadr will be called with no arguments.
;;; In this case, if the caddr of the item is also non-nil,
;;; no windows will be re-exposed before the cadr is called.
(DEFFLAVOR MOMENTARY-MENU ((LABEL NIL)) (BASIC-MOMENTARY-MENU TEMPORARY-WINDOW-MIXIN
					 TOP-LABEL-MIXIN BORDERS-MIXIN
					 MINIMUM-WINDOW)
  (:DOCUMENTATION :COMBINATION "Temporary menu that goes away after item it chosen"))


(DEFFLAVOR MOMENTARY-WINDOW-HACKING-MENU () (WINDOW-HACKING-MENU-MIXIN MOMENTARY-MENU)
  (:DOCUMENTATION :COMBINATION))

(DEFFLAVOR DYNAMIC-MOMENTARY-MENU () (DYNAMIC-ITEM-LIST-MIXIN MOMENTARY-MENU))
(DEFFLAVOR DYNAMIC-MOMENTARY-WINDOW-HACKING-MENU
	()
	(DYNAMIC-ITEM-LIST-MIXIN MOMENTARY-WINDOW-HACKING-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-MENU () (DYNAMIC-ITEM-LIST-MIXIN POP-UP-MENU))
(DEFFLAVOR DYNAMIC-POP-UP-COMMAND-MENU ()
	   (DYNAMIC-ITEM-LIST-MIXIN TEMPORARY-WINDOW-MIXIN COMMAND-MENU))


(DEFUN MENU-CHOOSE (ALIST &OPTIONAL (LABEL NIL) (NEAR-MODE '(:MOUSE)) DEFAULT-ITEM
		    &AUX (SUPERIOR MOUSE-SHEET))
  (AND (EQ (CAR NEAR-MODE) ':WINDOW) (SETQ SUPERIOR (SHEET-SUPERIOR (CADR NEAR-MODE))))
  (LET ((MENU (GET-A-SYSTEM-WINDOW 'MOMENTARY-MENU SUPERIOR)))
    (FUNCALL MENU ':SET-LABEL LABEL)
    (FUNCALL MENU ':SET-ITEM-LIST ALIST)
    (FUNCALL MENU ':SET-LAST-ITEM DEFAULT-ITEM)
    (EXPOSE-WINDOW-NEAR MENU NEAR-MODE)
    (AND DEFAULT-ITEM (NOT (MEMQ (CAR NEAR-MODE) '(:MOUSE :POINT)))
	 (MULTIPLE-VALUE-BIND (X Y)
	     (FUNCALL MENU ':ITEM-CURSORPOS DEFAULT-ITEM)
	   (FUNCALL MENU ':SET-MOUSE-POSITION X Y)))
    (PROG () (RETURN (FUNCALL MENU ':CHOOSE) (FUNCALL MENU ':LAST-ITEM)))))

;All types of menus, since they probably all get used
(COMPILE-FLAVOR-METHODS MENU POP-UP-MENU COMMAND-MENU MOMENTARY-MENU
			MOMENTARY-WINDOW-HACKING-MENU
			MULTIPLE-MENU MOMENTARY-MULTIPLE-MENU
			DYNAMIC-MOMENTARY-MENU DYNAMIC-MOMENTARY-WINDOW-HACKING-MENU
			DYNAMIC-POP-UP-MENU DYNAMIC-POP-UP-COMMAND-MENU)

