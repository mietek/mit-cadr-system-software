;;; -*-MODE:LISP; PACKAGE:SYSTEM-INTERNALS-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Typeout window stream, does ordinary typeout, and allows specification of fields
;;; that can be moused in a menu-like fashion.
;;; Funny operations are (:ITEM ITEM-TYPE ITEM &OPTIONAL PRINTED-REPRESENTATION) which
;;; types another item on its own line, marking it as ITEM-TYPE for the menu handler
;;; and (:ITEM-LIST ITEM-TYPE LIST) which prints the list in columns and allows selection
;;; of individual items from it then.  :ACTIVE-P tells if the typeout window was just used.
;;; :CHOOSE-OR-TYI returns an item or a keyboard character, and un-activates the window

;;; Class definitions

;;; ITEM-ALIST is a list of the form (ITEM-TYPE DEFAULT-ACTION . MENU-ALIST), a
;;; list of (TYPEOUT-EXECUTE CHOSEN-ACTION CHOSEN-ITEM) is returned when something is
;;; selected.

;;; BOTTOM-REACHED is the vpos beyond which the typeout window hasn't touched anything yet.

;;; UNDER-WINDOW is the window which this typeout window belongs to.

(DEFCLASS TYPEOUT-WINDOW-CLASS WINDOW-WITH-PC-PPR-CLASS
	  (COMMAND-BUFFER TYPEOUT-ARRAY NLINES MENU-BLINKER STREAM MENU ITEM-ALIST
	   BOTTOM-REACHED UNDER-WINDOW))
(DEFCLASS TYPEOUT-MENU-CLASS MOMENTARY-MENU-CLASS (TYPEOUT-WINDOW ITEM))

;;; Define the typeout-window structures
(DEFMACRO DEFINE-TYPEOUT-ARRAY-MACROS (NAME-LIST)
  (DO ((L NAME-LIST (CDR L))
       (I 0 (1+ I))
       (RET NIL (CONS `(DEFMACRO ,(CAR L) (TYPEOUT-ARRAY LINE)
			 `(AREF ,TYPEOUT-ARRAY ,LINE ,',I))
		      RET)))
      ((NULL L)
       `(PROGN 'COMPILE
	       (DEFMACRO MAKE-TYPEOUT-ARRAY (N-LINES)
		 `(MAKE-ARRAY NIL 'ART-Q (LIST ,N-LINES ,',I)))
	     . ,RET))))

(DEFINE-TYPEOUT-ARRAY-MACROS (TYPEOUT-ARRAY-ITEM-TYPE TYPEOUT-ARRAY-ITEM
			       TYPEOUT-ARRAY-X0 TYPEOUT-ARRAY-X1))

(LOCAL-DECLARE ((SPECIAL *TYPEOUT-WINDOW* *TYPEOUT-ARRAY* *STREAM*
			 *PC-PPR* *ACTIVE-P*))

;;; Return a stream for a typeout window over the given window
(DEFUN MAKE-TYPEOUT-STREAM (WINDOW ITEM-ALIST &AUX EDGES)
  (SETQ EDGES (<- WINDOW ':EDGES))
  (LET ((*TYPEOUT-WINDOW* (<- TYPEOUT-WINDOW-CLASS ':NEW
			      ':LEFT (CAR EDGES) ':TOP (CADR EDGES)
			      ':RIGHT (CADDR EDGES) ':BOTTOM (CADDDR EDGES)
			      ':ITEM-ALIST ITEM-ALIST
			      ':BIT-SAVE-ARRAY ;Max size array so it doesn't cons millions
;The below doesn't work, because this function gets called with edges of NIL
;from a window that isn't really born yet
;			         (LET ((WIDTH (- (CADDR EDGES) (CAR EDGES)))
;				       (HEIGHT (- (CADDDR EDGES) (CADR EDGES))))
;				   (MAKE-ARRAY NIL ART-1B
;					       (LIST (LOGAND -32. (+ 31. WIDTH)) HEIGHT)))
			         (MAKE-ARRAY NIL ART-1B (LIST 1400 1600))
			      ':UNDER-WINDOW WINDOW
			      ':PROCESS (<- WINDOW ':PROCESS)
			      ':SCREEN (<- WINDOW ':SCREEN))))
    (LET ((*PC-PPR* (<- *TYPEOUT-WINDOW* ':PC-PPR))
	  (*ACTIVE-P* NIL))
      (LET ((*STREAM* (TV-MAKE-STREAM *PC-PPR*))
            (*NLINES* (// (- (PC-PPR-BOTTOM-MARGIN *PC-PPR*) (PC-PPR-TOP-MARGIN *PC-PPR*))
			  (PC-PPR-LINE-HEIGHT *PC-PPR*))))
	 (LET ((*TYPEOUT-ARRAY* (MAKE-TYPEOUT-ARRAY *NLINES*)))
	   (<- *TYPEOUT-WINDOW* ':NLINES<- *NLINES*)
	   (<- *TYPEOUT-WINDOW* ':TYPEOUT-ARRAY<- *TYPEOUT-ARRAY*)
	   (LET ((STREAM (CLOSURE '(*TYPEOUT-WINDOW* *TYPEOUT-ARRAY* *STREAM*
						     *PC-PPR* *ACTIVE-P*)
				  'TYPEOUT-STREAM-INTERNAL)))
	     (<- *TYPEOUT-WINDOW* ':STREAM<- STREAM)
	     STREAM))))))

(DEFUN TYPEOUT-STREAM-INTERNAL (OP &OPTIONAL ARG1 &REST REST &AUX TEM)
  (COND ((<- *TYPEOUT-WINDOW* ':STATUS))        ;On the screen now
        ((EQ OP ':ACTIVE-P)
         (SETQ *ACTIVE-P* NIL))
        ((MEMQ OP '(:WINDOW :PC-PPR :WHICH-OPERATIONS :DEACTIVATE)))
	(T
	 (LOCK-SCREEN-LAYOUT
	   (<- *TYPEOUT-WINDOW* ':CLOBBER-SCREEN)
	   (<- *TYPEOUT-WINDOW* ':EXPOSE))))
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:LISTEN :TYI :RUBOUT-HANDLER :UNTYI :TYO :LINE-OUT
                         :STRING-OUT :FRESH-LINE :READ-CURSORPOS :SET-CURSORPOS :PC-PPR
                         :WINDOW :CLEAN :ITEM :ITEM-LIST :CLEAR-SCREEN
                         :ACTIVE-P :DEACTIVATE :CHOOSE-OR-TYI :GROW-FULL-WINDOW))
    (:ACTIVE-P
     *ACTIVE-P*)
    (:DEACTIVATE
     (SETQ *ACTIVE-P* NIL))
    ((:LISTEN :TYI :UNTYI :RUBOUT-HANDLER)
     (LEXPR-FUNCALL *STREAM* OP ARG1 REST))
    ((:CLEAN :CLEAR-SCREEN)
     (<- *TYPEOUT-WINDOW* ':CLOBBER-SCREEN)
     (<- *TYPEOUT-WINDOW* ':CLEAN))
    (:ITEM-LIST
     (TYPEOUT-ITEM-LIST (CAR REST) ARG1)
     (SETQ *ACTIVE-P* T))
    (:ITEM
     (LET (LINE-NUM)
       (MULTIPLE-VALUE (TEM LINE-NUM) (TV-READ-CURSORPOS *PC-PPR*))
       (SETQ LINE-NUM (// LINE-NUM (PC-PPR-LINE-HEIGHT *PC-PPR*)))
       (SETF (TYPEOUT-ARRAY-ITEM-TYPE *TYPEOUT-ARRAY* LINE-NUM) ARG1)
       (SETF (TYPEOUT-ARRAY-ITEM *TYPEOUT-ARRAY* LINE-NUM) (CAR REST))
       (SETF (TYPEOUT-ARRAY-X0 *TYPEOUT-ARRAY* LINE-NUM) TEM)
       (IF (CDR REST) (LEXPR-FUNCALL #'FORMAT 'TYPEOUT-STREAM-INTERNAL (CDR REST))
	   (PRINC (CAR REST) 'TYPEOUT-STREAM-INTERNAL))
       (SETF (TYPEOUT-ARRAY-X1 *TYPEOUT-ARRAY* LINE-NUM)
	     (TV-READ-CURSORPOS *PC-PPR*))))
    (:TYO
     (TV-TYO *PC-PPR* ARG1)
     (SETQ *ACTIVE-P* T))
    (:STRING-OUT
     (SETQ *ACTIVE-P* T)
     (LEXPR-FUNCALL #'TV-STRING-OUT *PC-PPR* ARG1 REST))
    (:LINE-OUT
     (LEXPR-FUNCALL #'TV-STRING-OUT *PC-PPR* ARG1 REST)
     (TV-CRLF *PC-PPR*)
     (SETQ *ACTIVE-P* T))
    (:FRESH-LINE
     (OR (ZEROP (TV-READ-CURSORPOS *PC-PPR*)) (TV-TYO *PC-PPR* #\CR))
     (SETQ *ACTIVE-P* T))
    (:READ-CURSORPOS
     (MULTIPLE-VALUE-BIND (X Y)
			  (TV-READ-CURSORPOS *PC-PPR*)
         (SELECTQ ARG1
	     (:PIXEL
	      (PROG () (RETURN X Y)))
	     (:CHARACTER
	      (PROG () (RETURN (// X (PC-PPR-CHAR-WIDTH *PC-PPR*))
			       (// Y (PC-PPR-LINE-HEIGHT *PC-PPR*)))))
	     (OTHERWISE (FERROR NIL "~S is not a known unit." ARG1)))))
    (:SET-CURSORPOS
     (<- *TYPEOUT-WINDOW* ':UPDATE-BOTTOM-REACHED)
     (LET ((X (FIRST REST)) (Y (SECOND REST)))
       (SELECTQ ARG1
	 (:PIXEL
	  (TV-SET-CURSORPOS *PC-PPR* X Y))
	 (:CHARACTER
	  (TV-SET-CURSORPOS *PC-PPR*
			    (* X (PC-PPR-CHAR-WIDTH *PC-PPR*))
			    (* Y (PC-PPR-LINE-HEIGHT *PC-PPR*))))
	 (OTHERWISE (FERROR NIL "~S is not a known unit." ARG1)))))
    (:PC-PPR
     *PC-PPR*)
    (:WINDOW
     *TYPEOUT-WINDOW*)
    (:CHOOSE-OR-TYI
     (TYPEOUT-CHOOSE-OR-TYI))
    (:GROW-FULL-WINDOW
     (<- *TYPEOUT-WINDOW* ':GROW-FULL-WINDOW))
    (OTHERWISE
     (FUNCALL #'STREAM-DEFAULT-HANDLER 'TYPEOUT-STREAM-INTERNAL OP ARG1 REST))))

) ;LOCAL-DECLARE

;; This is the guts of the :ITEM-LIST operation on the typeout window stream.
;; We are given a list of items and an item type that applies to them all,
;; and pack them onto lines horizontally into columns.
(DEFUN TYPEOUT-ITEM-LIST (LIST ITEM-TYPE &AUX (MAXL 0) LINE-NUM YPOS STRING
			       WIDTH N WIDTH-PER)
  (LOCAL-DECLARE ((SPECIAL *TYPEOUT-ARRAY* *PC-PPR*))
    (AND (PLUSP (TV-READ-CURSORPOS *PC-PPR*))		;Assure new-line
	 (TYPEOUT-STREAM-INTERNAL ':TYO #\CR))
    ;; Compute the maximum width of any item, in dots (MAXL).
    (DOLIST (ITEM LIST)
      (SETQ STRING (STRING (IF (LISTP ITEM) (CAR ITEM) ITEM)))
      (SETQ MAXL (MAX (TV-STRING-LENGTH *PC-PPR* STRING) MAXL)))
    (SETQ WIDTH (- (PC-PPR-RIGHT-LIMIT *PC-PPR*) (PC-PPR-LEFT-MARGIN *PC-PPR*)))
    (AND (> MAXL WIDTH)
	 (FERROR NIL "Some item is too big for this typeout window"))
    ;; How many items go on each line (except the last)?
    (SETQ N (MIN (// WIDTH (+ MAXL (FONT-CHAR-WIDTH (PC-PPR-CURRENT-FONT *PC-PPR*))))
		 (LENGTH LIST)))
    ;; How much space horizontally does each item get?
    (SETQ WIDTH-PER (// WIDTH N))
    ;; Now print the items and store the data in the table.
    ;; Move to a new line when we exhaust a line, and at the end.
    ;; I counts from 1 thru N on each line.
    (DO ((I 1 (1+ I))
	 (LIST LIST (CDR LIST)))
	((NULL LIST))
      (COND ((= I 1)
	     ;; Start of a new line; make LINE-NUM correct.
	     (MULTIPLE-VALUE (NIL YPOS)
	       (TV-READ-CURSORPOS *PC-PPR*))
	     (SETQ LINE-NUM (// YPOS (PC-PPR-LINE-HEIGHT *PC-PPR*)))
	     ;; Set up the table data about this line.
	     (SETF (TYPEOUT-ARRAY-ITEM-TYPE *TYPEOUT-ARRAY* LINE-NUM) ITEM-TYPE)
	     (SETF (TYPEOUT-ARRAY-ITEM *TYPEOUT-ARRAY* LINE-NUM) LIST)
	     (SETF (TYPEOUT-ARRAY-X0 *TYPEOUT-ARRAY* LINE-NUM) NIL)
	     (SETF (TYPEOUT-ARRAY-X1 *TYPEOUT-ARRAY* LINE-NUM) MAXL)))
      (SETQ STRING (STRING (IF (LISTP (CAR LIST)) (CAAR LIST) (CAR LIST))))
      (TV-STRING-OUT *PC-PPR* STRING)
      (COND ((AND ( I N) (CDR LIST))
	     ;; Not end of line => space out for next item.
	     (TV-SET-CURSORPOS *PC-PPR* 
			       (* WIDTH-PER
				  (// (+ (1- WIDTH-PER) (TV-READ-CURSORPOS *PC-PPR*))
				      WIDTH-PER))
			       YPOS))
	    (T
	      ;; End of line.
	      (TV-TYO *PC-PPR* #\CR)
	      (SETQ I 0))))))

(DEFUN TYPEOUT-CHOOSE-OR-TYI (&AUX LOC)
  (LOCAL-DECLARE ((SPECIAL *TYPEOUT-WINDOW* *STREAM* *ACTIVE-P*))
    (SETQ LOC (LOCATE-IN-CLOSURE *TYPEOUT-WINDOW* 'COMMAND-BUFFER))
    (OR (CAR LOC)
	(FUNCALL *STREAM* ':LISTEN)
	(PROCESS-WAIT "TYI" #'(LAMBDA (BUFFER-LOC) (OR (CAR BUFFER-LOC) (KBD-CHAR-AVAILABLE)))
		      LOC))
    (COND ((CAR LOC)                              ;There is a command from the mouse
	   (<- *TYPEOUT-WINDOW* ':COMMAND))
	  (T
	    (SETQ *ACTIVE-P* NIL)                  ;Getting from stream, go away soon
	    (FUNCALL *STREAM* ':TYI)))))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :BORN) ()
  (<-AS WINDOW-WITH-PC-PPR-CLASS ':BORN)
  (SETQ MENU-BLINKER (TV-DEFINE-BLINKER PC-PPR ':VISIBILITY NIL ':HALF-PERIOD 8))
  (SETQ MENU (<- TYPEOUT-MENU-CLASS ':NEW ':ITEM-LIST '(FOO) ':TYPEOUT-WINDOW SELF)))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :COMMAND) ()
  (AND COMMAND-BUFFER
       (POP COMMAND-BUFFER)))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :COMMAND-PUSH) (THING)
  (PUSH THING COMMAND-BUFFER))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :EXPOSE) (&AUX UNDER-STATUS)
  (SETQ UNDER-STATUS (<- UNDER-WINDOW ':STATUS))
  (COND ((NULL STATUS) ;If not already exposed
	 (<-AS WINDOW-CLASS ':EXPOSE)
 	 (SETF (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR) 0)
	 (AND (= BOTTOM-REACHED TOP)
	      (TV-SET-CURSORPOS PC-PPR 0 0))
	 (TV-CLEAR-EOL PC-PPR)
	 (SETQ COMMAND-BUFFER NIL)
	 (TV-ACTIVATE-PC-PPR PC-PPR)))
  (AND UNDER-STATUS (FUNCALL SELF ':SELECT)))

;;; This shouldn't even clean itself when RESTORE-SCREEN is done,
;;; so that you can see what's in your buffer.
(DEFMETHOD (TYPEOUT-WINDOW-CLASS :RESTORE-SCREEN) ()
  NIL)

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :DEEXPOSE) (&OPTIONAL REASON)
  (TV-DEACTIVATE-PC-PPR PC-PPR)	;Gets rid of blinkers
  (SETF (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR) 1)
  (LET ((INHIBIT-SCREEN-SAVING-FLAG T))
    (<-AS WINDOW-CLASS ':DEEXPOSE REASON))	;don't bother saving the bits
  (FUNCALL SELF ':UPDATE-BOTTOM-REACHED)
  (COND ((NEQ REASON MENU)
	 (PUSH NIL COMMAND-BUFFER)
	 (SETQ ACTIVE-WINDOWS (DELQ SELF ACTIVE-WINDOWS)))))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :CLOBBER-SCREEN)
           (&AUX (NLINES (ARRAY-DIMENSION-N 1 TYPEOUT-ARRAY)))
  (SETQ BOTTOM-REACHED (COND (STATUS BOTTOM) (T TOP)))
  (DOTIMES (I NLINES)			     ;Forget anything that was there before
    (SETF (TYPEOUT-ARRAY-ITEM-TYPE TYPEOUT-ARRAY I) NIL))
  T)     ;WINDOW-COMPLETE-REDISPLAY should clean us.  After all, restore-screen won't work.

;; How far down has use of the typeout window reached?
;; Look at the cursor now, and also at BOTTOM-REACHED which remembers
;; previous downward excursions of the cursor.
(DEFMETHOD (TYPEOUT-WINDOW-CLASS :BOTTOM-REACHED) ()
  (MAX BOTTOM-REACHED
       (+ (PC-PPR-CURRENT-Y PC-PPR)
	  (PC-PPR-LINE-HEIGHT PC-PPR))))

;; Say that the use of the typeout window should be regarded as
;; having reached down to a specific point.
(DEFMETHOD (TYPEOUT-WINDOW-CLASS :BOTTOM-REACHED<-) (NEW-BOTTOM)
  (SETQ BOTTOM-REACHED (MAX BOTTOM-REACHED NEW-BOTTOM)))

;; Say we want to use the entire area of the typeout window (say, to draw a picture)
;; in a way that won't be recorded properly by the pc-ppr's cursor.
(DEFMETHOD (TYPEOUT-WINDOW-CLASS :GROW-FULL-WINDOW) ()
  (SETQ BOTTOM-REACHED BOTTOM))

;; Remember how far cursor has reached, in BOTTOM-REACHED,
;; when cursor is about to move upward.
(DEFMETHOD (TYPEOUT-WINDOW-CLASS :UPDATE-BOTTOM-REACHED) ()
  (SETQ BOTTOM-REACHED (MAX BOTTOM-REACHED
			    (+ (PC-PPR-CURRENT-Y PC-PPR)
			       (PC-PPR-LINE-HEIGHT PC-PPR)))))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :HANDLE-MOUSE) ()
  (MOUSE-DEFAULT-HANDLER SELF)
  (TV-SET-BLINKER-VISIBILITY MENU-BLINKER NIL))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :MOUSE-MOVES) (NEW-X NEW-Y &AUX ITEM-TYPE X0 X1 Y)
  (TV-ACTIVATE-PC-PPR PC-PPR)                ;Prevent lossage
  (MOUSE-SET-X-BLINKER-CURSORPOS)
  (MULTIPLE-VALUE (NIL ITEM-TYPE X0 X1 Y)
      (TYPEOUT-MOUSE-ITEM TYPEOUT-ARRAY PC-PPR NEW-X NEW-Y))
  (COND ((NULL ITEM-TYPE)
	 (TV-SET-BLINKER-VISIBILITY MENU-BLINKER NIL))
	(T
         (TV-SET-BLINKER-CURSORPOS MENU-BLINKER X0 Y)
	 (TV-SET-BLINKER-SIZE MENU-BLINKER (- X1 X0) (TV-BLINKER-HEIGHT MENU-BLINKER))
	 (TV-SET-BLINKER-VISIBILITY MENU-BLINKER 'BLINK))))

(DEFMETHOD (TYPEOUT-WINDOW-CLASS :MOUSE-BUTTONS) (BD X Y &AUX TEM ITEM ITEM-TYPE)
  (COND ((>= Y (FUNCALL SELF ':BOTTOM-REACHED))
	 (WINDOW-SELECT UNDER-WINDOW))
	;;Get double-clicks
	((= (SETQ TEM (MOUSE-BUTTON-ENCODE BD)) 2012)
	 (<- WINDOW-DEFAULT-MENU ':CHOOSE))
	(T
	  (MULTIPLE-VALUE (ITEM ITEM-TYPE)
	    (TYPEOUT-MOUSE-ITEM TYPEOUT-ARRAY PC-PPR X Y))
	  (COND ((AND ITEM (SETQ ITEM-TYPE (ASSQ ITEM-TYPE ITEM-ALIST)))
		 (AND (LISTP ITEM) (SETQ ITEM (CDR ITEM)))
		 (SELECTQ TEM
		   (2000 (PUSH (LIST ':TYPEOUT-EXECUTE (CADR ITEM-TYPE) ITEM) COMMAND-BUFFER))
		   (2002 (<- MENU ':CHOOSE ITEM (CDDR ITEM-TYPE)))
		   (OTHERWISE (TV-BEEP))))
		(T
		  (TV-BEEP))))))

;;; This returns the mouse item you are pointing to
(DEFUN TYPEOUT-MOUSE-ITEM (TYPEOUT-ARRAY PC-PPR MOUSE-X MOUSE-Y
					 &AUX X Y X0 X1 LINE-NUM TEM ITEM ITEM-TYPE
					 (NLINES (ARRAY-DIMENSION-N 1 TYPEOUT-ARRAY)))
    (SETQ X (- MOUSE-X (PC-PPR-LEFT-MARGIN PC-PPR))
	  Y (- MOUSE-Y (PC-PPR-TOP-MARGIN PC-PPR))
	  LINE-NUM (// Y (PC-PPR-LINE-HEIGHT PC-PPR)))
    (SETQ Y (* LINE-NUM  (PC-PPR-LINE-HEIGHT PC-PPR)))
    (COND ((OR ( LINE-NUM NLINES)
	       (NULL (SETQ ITEM-TYPE (TYPEOUT-ARRAY-ITEM-TYPE TYPEOUT-ARRAY LINE-NUM))))
	   (PROG () (RETURN NIL NIL X X Y)))
	  ((SETQ ITEM (TYPEOUT-ARRAY-ITEM TYPEOUT-ARRAY LINE-NUM)
		 X1 (TYPEOUT-ARRAY-X1 TYPEOUT-ARRAY LINE-NUM)
		 X0 (TYPEOUT-ARRAY-X0 TYPEOUT-ARRAY LINE-NUM))
	   (PROG () (RETURN ITEM (AND ( X X0) ( X X1) ITEM-TYPE) X0 X1 Y)))
	  (T
	    (SETQ TEM (// X X1)
		  X0 (* TEM X1)
		  ITEM (NTH TEM ITEM)
		  X1 (AND ITEM (+ (TV-STRING-LENGTH PC-PPR
						    (STRING (IF (LISTP ITEM) (CAR ITEM) ITEM)))
				  X0)))
	    (PROG () (RETURN ITEM (AND ITEM ( X X1) ITEM-TYPE) X0 X1 Y)))))

;;; Pop-up menus inside the typeout window
(DEFMETHOD (TYPEOUT-MENU-CLASS :CHOOSE) (TYPEOUT-ITEM ALIST)
  (SETQ ITEM TYPEOUT-ITEM)
  (COND ((NEQ ALIST ITEM-LIST)
         (SETQ ITEM-LIST ALIST
               CURRENT-ITEM NIL
               LAST-ITEM NIL)
         (<- SELF ':COMPUTE-GEOMETRY)))
  (<-AS MOMENTARY-MENU-CLASS ':CHOOSE))

(DEFMETHOD (TYPEOUT-MENU-CLASS :WAIT-FOR-CHOICE) ()
  ;; This kludge is about the only way to leave the selected item marked
  (LET ((BLINKER (<- TYPEOUT-WINDOW ':MENU-BLINKER))
        FLAG)
    (TV-SET-BLINKER-VISIBILITY BLINKER T)
    (OR (SETQ FLAG (MEMQ BLINKER TV-BLINKER-LIST)) (PUSH BLINKER TV-BLINKER-LIST))
    (<-AS MOMENTARY-MENU-CLASS ':WAIT-FOR-CHOICE)
    (OR FLAG (SETQ TV-BLINKER-LIST (DELQ BLINKER TV-BLINKER-LIST)))))

(DEFMETHOD (TYPEOUT-MENU-CLASS :EXECUTE) (MENU-ITEM WINDOW-AND-POSITION)
  (AND MENU-ITEM
       (<- TYPEOUT-WINDOW ':COMMAND-PUSH (LIST ':TYPEOUT-EXECUTE (CDR MENU-ITEM) ITEM))))

;;; This tries to put the menu in a place that wont interfere with the item selected
(DEFMETHOD (TYPEOUT-MENU-CLASS :MOVE-NEAR) (XPOS YPOS &AUX WIDTH HEIGHT SCREEN-X1 SCREEN-X2
                                            SCREEN-Y1 SCREEN-Y2 ITEM-X1 ITEM-X2 ITEM-Y
                                            ITEM-HEIGHT NEW-LEFT NEW-TOP)
  (SETQ WIDTH (- RIGHT LEFT)
        HEIGHT (- BOTTOM TOP)
        SCREEN-X1 (SCREEN-X1 SCREEN)
        SCREEN-X2 (SCREEN-X2 SCREEN)
        SCREEN-Y1 (SCREEN-Y1 SCREEN)
        SCREEN-Y2 (SCREEN-Y2 SCREEN))
  (LET ((TYPEOUT-ARRAY (<- TYPEOUT-WINDOW ':TYPEOUT-ARRAY))
	(PC-PPR (<- TYPEOUT-WINDOW ':PC-PPR)))
    (SETQ ITEM-HEIGHT (PC-PPR-LINE-HEIGHT PC-PPR))
    (MULTIPLE-VALUE (NIL NIL ITEM-X1 ITEM-X2 ITEM-Y)
      (TYPEOUT-MOUSE-ITEM TYPEOUT-ARRAY PC-PPR XPOS YPOS))
    (SETQ ITEM-X1 (+ ITEM-X1 (PC-PPR-LEFT-MARGIN PC-PPR))    ;Readjust to absolute coords
	  ITEM-X2 (+ ITEM-X2 (PC-PPR-LEFT-MARGIN PC-PPR))
	  ITEM-Y (+ ITEM-Y (PC-PPR-TOP-MARGIN PC-PPR))))
  (COND (( (+ ITEM-X2 WIDTH) SCREEN-X2)        ;Will fit to right of selected item?
         (SETQ NEW-LEFT ITEM-X2))
        ((< (SETQ NEW-LEFT (- ITEM-X1 WIDTH)) SCREEN-X1)
         (SETQ NEW-LEFT NIL)))                  ;Wont fit anywhere, put above or below later
  (COND (NEW-LEFT
         (SETQ NEW-TOP (MIN (MAX SCREEN-Y1 (- ITEM-Y (// HEIGHT 2)))
                            (- SCREEN-Y2 HEIGHT))))
        (T
         (SETQ NEW-LEFT (- SCREEN-X2 WIDTH))
         (AND (< (SETQ NEW-TOP (- ITEM-Y HEIGHT)) SCREEN-Y1)
              (SETQ NEW-TOP (MIN (+ ITEM-Y ITEM-HEIGHT) (- SCREEN-Y2 HEIGHT))))))
  (<- SELF ':EDGES<- NEW-LEFT NEW-TOP (+ NEW-LEFT WIDTH) (+ NEW-TOP HEIGHT))
  (LIST (+ NEW-LEFT (// WIDTH 2)) (+ NEW-TOP (// HEIGHT 2))))
