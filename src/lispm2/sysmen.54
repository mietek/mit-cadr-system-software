; -*-Mode:LISP; Package:SYSTEM-INTERNALS-*-

;; Operations for moving, reshaping and creating windows,
;; and menus to get them from.

;; The mouse process uses this to ask which menu to expose for right-double click.
(DEFMETHOD (WINDOW-CLASS :POP-UP-MENU) () WINDOW-DEFAULT-MENU)

;; This method is used to move a window around with the mouse.
;; BUTTON-UP-FLAG if T says that we run with buttons up, UNTIL a button is pressed.
;; Otherwise we run with a button down, until it is released.

(DEFMETHOD (WINDOW-CLASS :MOUSE-FOLLOW) (&OPTIONAL BUTTON-UP-FLAG
						   &AUX INHIBIT-ALL-SCREEN-BLTING-FLAG)
  (WINDOW-SELECT SELF)
  (LOCK-SCREEN-LAYOUT
    (FUNCALL SELF ':DEEXPOSE)
    (LET-GLOBALLY ((WINDOW-OWNING-MOUSE T))
       (LET (O-MOUSE-X O-MOUSE-Y O-LEFT O-TOP O-RIGHT O-BOTTOM)
	 (DO () (())
	   (SETQ O-MOUSE-X MOUSE-X O-MOUSE-Y MOUSE-Y)
	   (MOUSE-WAIT)
	   (OR (EQ (ZEROP MOUSE-LAST-BUTTONS) BUTTON-UP-FLAG) (RETURN NIL))
	   (SETQ O-LEFT LEFT O-TOP TOP O-RIGHT RIGHT O-BOTTOM BOTTOM)
	   (LET ((INHIBIT-AUTOEXPOSE-FLAG T))
	     (LEXPR-FUNCALL 'MOUSE-WARP
			    (<- SELF ':MOVE-RELATIVE
				(- MOUSE-X O-MOUSE-X)
				(- MOUSE-Y O-MOUSE-Y))))
	   ;; Deexpose every window we now overlap.
	   (DOLIST (W EXPOSED-WINDOWS)
	     (AND (<- W ':OVERLAPS-P SCREEN LEFT TOP RIGHT BOTTOM)
		  (<- W ':DEEXPOSE)))
	   (WITHOUT-INTERRUPTS
	     ;; Clear out where we used to be FIRST so the motion looks right.
	     (TV-ERASE (- O-RIGHT O-LEFT) (- O-BOTTOM O-TOP)
		       O-LEFT O-TOP TV-ALU-ANDCA))
	   ;; Blt back this window (restore-partial is faster!), redisplay where we were,
	   (FUNCALL SELF ':RESTORE-PARTIAL-SCREEN NIL NIL NIL NIL T)
	   (WINDOW-RESTORE-PARTIAL SELF SCREEN O-LEFT O-TOP O-RIGHT O-BOTTOM))
	 (FUNCALL SELF ':SELECT)
	 ;; Bring back any windows that can now be exposed.
	 (WINDOWS-AUTOEXPOSE)))))

;; This method can be used to reshape a window with the mouse.
;; Windows which sometimes do not want to be reshaped can redefine this method.
(DEFMETHOD (WINDOW-CLASS :MOUSE-RESHAPE) (&AUX INHIBIT-ALL-SCREEN-BLTING-FLAG)
  (LOCK-SCREEN-LAYOUT
    (LEXPR-FUNCALL SELF ':SELECT-AND-EDGES<-
		   (MOUSE-SPECIFY-RECTANGLE LEFT TOP RIGHT BOTTOM))))

(DEFVAR WINDOW-DEFAULT-MENU)
(SETQ WINDOW-DEFAULT-MENU
      (<- MOMENTARY-MENU-CLASS ':NEW
	  ':NAME "Default Menu for Windows"
	  ':ITEM-LIST '((" Create" :MENU WINDOW-CREATION-MENU)
			(" Select" :FUNCALL REDEFINE-ACTIVE-WINDOWS-MENU)
			(" Trace" :FUNCALL TRACE-VIA-MENUS)
			(" Split Screen" :FUNCALL SPLIT-SCREEN-VIA-MENUS)
			(" Layouts" :FUNCALL SAVE-RESTORE-SCREEN-LAYOUT)
			("Bury" :WINDOW-OP
				(LAMBDA (W) (AND (CAR W) (<- (CAR W) ':BURY))))
			("Move" :WINDOW-OP
				(LAMBDA (W &AUX (WINDOW (FIRST W)))
				  (AND WINDOW (<- WINDOW ':MOUSE-FOLLOW))))
			("Reshape" :WINDOW-OP
				   (LAMBDA (W &AUX (WINDOW (FIRST W)))
				     (AND WINDOW (<- WINDOW ':MOUSE-RESHAPE))))
			(" Other" :WINDOW-OP
				   (LAMBDA (W)
				     ;; Move mouse back to where it was when the first menu
				     ;; was requested, so that this menu's operations will
				     ;; apply to the same window.
				     (MOUSE-WARP (SECOND W) (THIRD W))
				     (<- WINDOW-AUXILIARY-MENU ':CHOOSE))))))

;; Other random less useful operations on windows, put here to keep default menu small.
(DEFVAR WINDOW-AUXILIARY-MENU)
(SETQ WINDOW-AUXILIARY-MENU
	(<- MOMENTARY-MENU-CLASS ':NEW
	    ':NAME "Auxiliary Menu for Windows"
	    ':ITEM-LIST '(("Arrest" :WINDOW-OP
				    (LAMBDA (W) (AND (CAR W) (<- (CAR W) ':ARREST-REASON))))
			  ("Un-Arrest" :WINDOW-OP
				       (LAMBDA (W)
						 (AND (CAR W)
						      (<- (CAR W) ':REVOKE-ARREST-REASON))))
			  ("Reset" :WINDOW-OP
				   (LAMBDA (W)
					     (AND (CAR W) (<- (CAR W) ':RESET))))
			  ("Kill" :WINDOW-OP
				  (LAMBDA (W)
				    (COND ((CAR W)
					   (<- (CAR W) ':KILL))))))))

(DEFVAR VIEW-ACTIVE-WINDOWS-MENU)
(DEFUN REDEFINE-ACTIVE-WINDOWS-MENU ()
       (LET ((ITEM-LIST
	      (MAPCAR #'(LAMBDA (WINDOW)
		           (LIST (OR (AND (<- WINDOW ':HANDLER-FOR ':LABEL)
					  (<- WINDOW ':LABEL))
				     (<- WINDOW ':NAME))
				 WINDOW))
		      ACTIVE-WINDOWS)))
	 (COND ((BOUNDP 'VIEW-ACTIVE-WINDOWS-MENU)
		(<- VIEW-ACTIVE-WINDOWS-MENU ':ITEM-LIST<- ITEM-LIST)
		(<- VIEW-ACTIVE-WINDOWS-MENU ':COMPUTE-GEOMETRY NIL NIL NIL 1))
	       (T (SETQ VIEW-ACTIVE-WINDOWS-MENU
			(<- MOMENTARY-MENU-CLASS ':NEW
			    ':NAME "View windows menu"
			    ':ITEM-LIST ITEM-LIST)))))
       (LET ((WINDOW (<- VIEW-ACTIVE-WINDOWS-MENU ':CHOOSE)))
	 (AND WINDOW (WINDOW-SELECT WINDOW))))

(DEFVAR WINDOW-CREATION-MENU)
(SETQ WINDOW-CREATION-MENU
      (<- MOMENTARY-MENU-CLASS ':NEW
	  ':NAME "Window Creation Menu"
	  ':ITEM-LIST '(("Supdup" :EVAL (CREATE-WINDOW-WITH-RESHAPING SUPDUP:SUPDUP-CLASS))
			("Telnet" :EVAL (CREATE-WINDOW-WITH-RESHAPING SUPDUP:TELNET-CLASS))
			("Lisp" :EVAL (CREATE-WINDOW-WITH-RESHAPING LISP-LISTENER-CLASS))
			("Edit" :EVAL (CREATE-WINDOW-WITH-RESHAPING ZWEI:ZWEI-WINDOW-CLASS))
			("Peek" :EVAL (CREATE-WINDOW-WITH-RESHAPING PEEK-WINDOW-CLASS)))))

(DEFMETHOD (CLASS-CLASS :CREATE-USING-MOUSE) ()
    (CREATE-WINDOW-WITH-RESHAPING SELF))

(DEFUN CREATE-WINDOW-WITH-RESHAPING (CLASS)
    (LET ((WINDOW (<- CLASS ':NEW ':SCREEN MOUSE-SCREEN))
	  (FRAME (<- WINDOW-SINGLE-FRAME-CLASS ':NEW)))
         (LEXPR-FUNCALL FRAME ':EDGES<- (MOUSE-SPECIFY-RECTANGLE))
	 (<- FRAME ':PANE<- WINDOW)
	 (WINDOW-SELECT FRAME)
         WINDOW))

(DEFUN CREATE-WINDOW-WITH-FRAME (CLASS SCREEN LEFT TOP RIGHT BOTTOM)
    (LET ((WINDOW (<- CLASS ':NEW ':SCREEN SCREEN))
	  (FRAME (<- WINDOW-SINGLE-FRAME-CLASS ':NEW)))
         (<- FRAME ':EDGES<- LEFT TOP RIGHT BOTTOM)
	 (<- FRAME ':PANE<- WINDOW)
         WINDOW))

;;; Stuff for setting up a screen layout.
;;; Suggested improvements:
;;;  Find out why it thrashes the disk for several seconds before coming up,
;;;   after displaying all the windows.
;;;  Link the windows into a paned structure so that expanding one would 
;;;   contract the adjacent ones.
;;;  Provide the ability to set up a split screen structure that uses only
;;;   part of the screen.
;;;  Provide the ability to edit saved screen layouts.
;;;  Aid the user with a prompt of what is expected and what has been specified
;;;   so far; too bad the menu system makes this difficult to do.

(DEFVAR SPLIT-SCREEN-VIA-MENUS-MENU)
(DEFVAR SPLIT-SCREEN-VIA-MENUS-ACTIVE-WINDOWS-MENU)
(SETQ SPLIT-SCREEN-VIA-MENUS-MENU	;This is slightly different from WINDOW-CREATION-MENU
      (<- MOMENTARY-MENU-CLASS ':NEW
	  ':NAME "Split Screen Menu"
	  ':LABEL "Split screen element:"
	  ':COLUMNS 2
	  ':ITEM-LIST '("Supdup" "Telnet"
			"New Lisp" "Existing Lisp"
			"Edit" "Peek"
			"Existing Window" ""
			"Do It" "Quit"))
      SPLIT-SCREEN-VIA-MENUS-ACTIVE-WINDOWS-MENU
      (<- MOMENTARY-MENU-CLASS ':NEW
	  ':NAME "SPLIT-SCREEN-VIA-MENUS-ACTIVE-WINDOWS-MENU"
	  ':LABEL "Split screen element:"
	  ':ITEM-LIST '("FOO")))

(DEFUN SPLIT-SCREEN-VIA-MENUS ()
  (DO ((WINDOW-TYPE-LIST NIL)
       (N-WINDOWS 0)
       (SCREEN TV-DEFAULT-SCREEN)
       (N-COLUMNS) (N-ROWS) (HEIGHT) (WIDTH) (RES))
      (NIL)
    (SETQ RES (<- SPLIT-SCREEN-VIA-MENUS-MENU ':CHOOSE))
    (COND ((NULL RES))				;Ignore spazzes outside the menu
	  ((EQUAL RES ""))			;Ignore the null element used for formatting
	  ((EQUAL RES "Quit") (RETURN NIL))
	  ((EQUAL RES "Existing Window")
	   (<- SPLIT-SCREEN-VIA-MENUS-ACTIVE-WINDOWS-MENU
	       ':ITEM-LIST<- (MAPCAR #'(LAMBDA (WINDOW)
					  (LIST (OR (AND (<- WINDOW ':HANDLER-FOR ':LABEL)
							 (<- WINDOW ':LABEL))
						    (<- WINDOW ':NAME))
						WINDOW))
				     ACTIVE-WINDOWS))
	   (<- SPLIT-SCREEN-VIA-MENUS-ACTIVE-WINDOWS-MENU ':COMPUTE-GEOMETRY NIL NIL NIL 1)
	   (AND (SETQ RES (<- SPLIT-SCREEN-VIA-MENUS-ACTIVE-WINDOWS-MENU ':CHOOSE))
		(PUSH RES WINDOW-TYPE-LIST))
	   (SETQ N-WINDOWS (1+ N-WINDOWS)))
	  ((NOT (EQUAL RES "Do It"))
	   (PUSH RES WINDOW-TYPE-LIST)
	   (SETQ N-WINDOWS (1+ N-WINDOWS)))
	  (T ;; We now have the list of windows, lay out the screen and set them up.
	     ;; The general rule for screen layout is that 2 or 3 windows stack vertically,
	     ;; 4 are in a square, 5 are a square with 1 below it, etc.
	     ;; To generalize, you have floor(n/2) rows in 2 columns and 1 below if n is odd.
	   (COND ((< N-WINDOWS 4)
		  (SETQ N-COLUMNS 1 N-ROWS N-WINDOWS))
		 ((SETQ N-COLUMNS 2 N-ROWS (// (1+ N-WINDOWS) 2))))
	   (SETQ WIDTH (// (- (SCREEN-X2 SCREEN) (SCREEN-X1 SCREEN)) N-COLUMNS)
		 HEIGHT (// (- (SCREEN-Y2 SCREEN) (SCREEN-Y1 SCREEN)) N-ROWS))
	   (LOCK-SCREEN-LAYOUT
	     (DOLIST (W EXPOSED-WINDOWS)
	       (<- W ':DEEXPOSE))
	     (TV-CLEAR-SCREEN SCREEN)
	     (DO ((L (NREVERSE WINDOW-TYPE-LIST) (CDR L))
		  (EXISTING (APPEND ACTIVE-WINDOWS NIL)) ;uppermost first
		  (I 0 (1+ I)) (LEFT)(RIGHT)(TOP)(BOTTOM))
		 ((NULL L))
	       (SETQ LEFT (+ (SCREEN-X1 SCREEN) (* (\ I N-COLUMNS) WIDTH))
		     RIGHT (+ LEFT WIDTH)
		     TOP (+ (SCREEN-Y1 SCREEN) (* (// I N-COLUMNS) HEIGHT))
		     BOTTOM (+ TOP HEIGHT))
	       ;; The bottom-most window can be extra wide if there are an odd number of them
	       (AND (NULL (CDR L)) (SETQ RIGHT (SCREEN-X2 SCREEN)))
	       (LET ((WINDOW
		      (COND ((NOT (STRINGP (CAR L)))	;Window himself
			     (<- (CAR L) ':EDGES<- LEFT TOP RIGHT BOTTOM)
			     (CAR L))			   
			    ((EQUAL (CAR L) "Existing Lisp")
			     (OR (DOLIST (W EXISTING)
				   (AND (EQ (CLASS W) LISP-LISTENER-CLASS)
					(EQ (<- W ':SCREEN) SCREEN)
					(RETURN (PROGN (<- W ':EDGES<- LEFT TOP RIGHT BOTTOM)
						       W))))
				 (CREATE-WINDOW-WITH-FRAME LISP-LISTENER-CLASS
							   SCREEN LEFT TOP RIGHT BOTTOM)))
			    (T
			     (CREATE-WINDOW-WITH-FRAME
				 (SYMEVAL (CDR (ASSOC (CAR L)
						      '(("Supdup" . SUPDUP:SUPDUP-CLASS)
							("Telnet" . SUPDUP:TELNET-CLASS)
							("New Lisp" . LISP-LISTENER-CLASS)
							("Edit" . ZWEI:ZWEI-WINDOW-CLASS)
							("Peek" . PEEK-WINDOW-CLASS)))))
				 SCREEN LEFT TOP RIGHT BOTTOM)))))
		 (SETQ EXISTING (DELQ WINDOW EXISTING)) ;Don't use anybody twice
		 (<- WINDOW ':EXPOSE-CLEAN)
		 (AND (ZEROP I) (<- WINDOW ':SELECT)))))
	   (RETURN NIL)))))

(DEFVAR SCREEN-LAYOUT-MENU)
(SETQ SCREEN-LAYOUT-MENU
      (<- MOMENTARY-MENU-CLASS ':NEW
	  ':NAME "Screen Layout Menu"
	  ':LABEL "Screen Layouts"
	  ':ITEM-LIST `(("Just Lisp" :VALUE ((,TOP-WINDOW 0 0 ,(SCREEN-X2 TV-DEFAULT-SCREEN)
					      ,(SCREEN-Y2 TV-DEFAULT-SCREEN))))
			("Save This" :EVAL (PROGN (SAVE-THIS-SCREEN-LAYOUT) NIL)))))

(DEFUN SAVE-RESTORE-SCREEN-LAYOUT ()
  (<- SCREEN-LAYOUT-MENU ':COMPUTE-GEOMETRY NIL NIL NIL 1)
  (LET ((X (<- SCREEN-LAYOUT-MENU ':CHOOSE)))
    (COND (X 
	   (LOCK-SCREEN-LAYOUT
	     (DOLIST (Y X)
	       (LET ((WINDOW (CAR Y))
		     (EDGES (CDR Y)))
		 (<- WINDOW ':EDGES<-
		     (FIRST EDGES) (SECOND EDGES)
		     (THIRD EDGES) (FOURTH EDGES))
		 (<- WINDOW ':EXPOSE-CLEAN)))
	     (<- (CAAR X) ':SELECT))))))

(DEFUN SAVE-THIS-SCREEN-LAYOUT ()
  (<- SCREEN-LAYOUT-MENU
      ':ITEM-LIST<- (CONS (LIST (GET-LINE-FROM-KEYBOARD "Name for this screen layout")
				':VALUE
				(MAPCAR #'(LAMBDA (W)
					      (CONS W (<- W ':EDGES)))
					EXPOSED-WINDOWS))
			  (<- SCREEN-LAYOUT-MENU ':ITEM-LIST))))

(DECLARE (SPECIAL GET-LINE-FROM-KEYBOARD-WINDOW))

;;; Pop up a window near where the mouse is, then read a line from it.
(DEFUN GET-LINE-FROM-KEYBOARD (PROMPT)
  (OR (BOUNDP 'GET-LINE-FROM-KEYBOARD-WINDOW)
      (SETQ GET-LINE-FROM-KEYBOARD-WINDOW (<- POP-UP-TEXT-WINDOW-CLASS ':NEW)))
  (UNWIND-PROTECT (LET ((STREAM (<- GET-LINE-FROM-KEYBOARD-WINDOW ':STREAM)))
		    (<- GET-LINE-FROM-KEYBOARD-WINDOW ':LABEL<- "Type a line")
			;Unfortunately can't turn the label off without redefining methods
		    (<- GET-LINE-FROM-KEYBOARD-WINDOW ':EDGES<- 0 0 500 120)
		    (<- GET-LINE-FROM-KEYBOARD-WINDOW ':MOVE-NEAR MOUSE-X MOUSE-Y)
		    (<- GET-LINE-FROM-KEYBOARD-WINDOW ':POP-UP)
		    (FORMAT STREAM "~A:~%" PROMPT)
		    (READLINE STREAM))
    (<- GET-LINE-FROM-KEYBOARD-WINDOW ':POP-DOWN)))
