;;; -*-MODE:LISP; PACKAGE:SYSTEM-INTERNALS-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This is the class of the menus you get by mousing an item of typeout.
;;; This class definition is up here rather than later on with the methods
;;; so that the class name will be special.
(DEFCLASS WINDOW-TYPEOUT-MENU-CLASS MOMENTARY-MENU-CLASS (WINDOW ITEM STREAM ITEM-BLINKER))

;;; Now we describe the operations and closure variables of the window typeout stream:

;;; BUFFER is the buffer for rubout-processing of input.  UNRCHF is for
;;; unreading one character of input.  PC-PPR is used for output.
;;; WINDOW is the window which this typeout stream belongs to.

;;; The window typeout stream is "active" when part of the area of the window
;;; is in use by its typeout.  The pc-ppr's blinker is flashing at those times.
;;; BOTTOM-REACHED not NIL is the internal criterion for activity.
;;; The :ACTIVE-P operation returns non-NIL if the stream is active.
;;; The stream becomes active when output is done through it, or when rubout
;;; processing input is done through it.  It remains active until a :DEACTIVATE
;;; operation is done, which also returns non-NIL if the stream had been active.

;;; In addition to activeness, the stream's state may be "complete" or "incomplete".
;;; Output makes the stream incomplete.  It is made complete again by deactivation
;;; or with a :MAKE-COMPLETE operation.
;;; When the stream is incomplete, the window ought to leave its data on the screen
;;; until a character is read.  A space typed at that time should be ignored
;;; except for causing redisplay.
;;; The :INCOMPLETE-P operation can be used to ask about this.

;;; In addition to ordinary text, the window typeout stream can be used to put
;;; up ITEMs, which are strings of text printed on the screen which are sensitive
;;; to the mouse.  The ITEM-LIST variable of the stream is a list of these objects.

(DEFSTRUCT (WINDOW-TYPEOUT-ITEM LIST)
  ;; The TYPE is for looking up in the ITEM-TYPE-ALIST.
  WINDOW-TYPEOUT-ITEM-TYPE
  ;; The ITEM is the essential data.  When the item is seleced with the mouse,
  ;; it is what is used to tell the called which item was selected.
  WINDOW-TYPEOUT-ITEM-ITEM
  ;; The LEFT, TOP, RIGHT and BOTTOM define the screen area of the item.
  ;; All are relative to the PC-PPR that the stream uses.
  WINDOW-TYPEOUT-ITEM-LEFT
  WINDOW-TYPEOUT-ITEM-TOP
  WINDOW-TYPEOUT-ITEM-RIGHT
  WINDOW-TYPEOUT-ITEM-BOTTOM)

;; A typeout item is made with one of two operations.
;; :ITEM type item .. format info .. creates one item.
;; The text is made by printing <item>, or by passing the format info as
;; args to format, if the format info was provided (it is optional).
;; :ITEM-LIST type list  makes several items, arranged into columns,
;; but ordered horizontally first (maybe this should be changed).
;; It makes as many columns as will fit.  Each element of the list should
;; be either a string, which becomes the item value, of a pair of a string
;; and what ought to be the item value.

;;; When the mouse is over an item, ITEM-BLINKER is used to highlight that item.
;;; Each item has a "type"
;;; which says, indirectly, what to do if a mouse button is pressed when the mouse
;;; is over the item.  The type is interpreted by looking it up in the ITEM-TYPE-ALIST.
;;; This is used to determine something to push onto COMMAND-BUFFER.
;;; Alternatively, it may pop up the MENU.

;;; ITEM-TYPE-ALIST is a list of the form (ITEM-TYPE DEFAULT-ACTION . MENU-ALIST).
;;; When an item of that type is moused with left-single, DEFAULT-ACTION is used.
;;; When it is moused with right-single, the menu alist, a list of pairs of
;;; strings and actions, is put into the stream's menu to choose an action.
;;; However the action is determined, it is acted on by pushing on the command
;;; buffer a list (:TYPEOUT-EXECUTE action item-indicator), where item-indicator
;;; is "the thing to return" for the item that was selected.

;;; BOTTOM-REACHED is the vpos beyond which we have not typed out (rel to pc-ppr).
;;; When asked to handle mouse motion or mouse clicks, the stream handles them
;;; only when the cursor is above this point.  In that case, it returns T.
;;; Otherwise, it returns NIL.  The :MOUSE-MOVES and :MOUSE-BUTTONS methods of
;;; the underlying window ought to do (OR (FUNCALL STREAM ...) normal-actions)
;;; to interface with the stream properly.

;;; CURSOR-BLINKER is the blinker for pc-ppr's cursor, if any.
;;; We make it visible when we are active.  HIDDEN-BLINKERS is a list of
;;; all the other blinkers of this pc-ppr that were visible, consed with their
;;; visibilities.  Those blinkers are turned off when we activate, and restored
;;; when we deactivate.

;;; Return a stream for a typeout window over the given window
(DEF MAKE-WINDOW-TYPEOUT-STREAM
  (LOCAL-DECLARE ((SPECIAL BUFFER UNRCHF PC-PPR INCOMPLETE-P
			   ITEM-LIST ITEM-TYPE-ALIST CURSOR-BLINKER
			   ITEM-BLINKER HIDDEN-BLINKERS COMMAND-BUFFER
			   BOTTOM-REACHED MENU WINDOW))
    
    (DEFUN MAKE-WINDOW-TYPEOUT-STREAM (WINDOW ITEM-TYPE-ALIST &AUX EDGES PC-PPR)
	   (SETQ EDGES (<- WINDOW ':EDGES))
	   (SETQ PC-PPR (<- WINDOW ':PC-PPR))
	   (LET (BUFFER UNRCHF ITEM-LIST ITEM-BLINKER CURSOR-BLINKER HIDDEN-BLINKERS MENU
		 COMMAND-BUFFER BOTTOM-REACHED INCOMPLETE-P SELF)
	     (SETF (PC-PPR-END-SCREEN-FCN PC-PPR)
		   (CLOSURE '(BOTTOM-REACHED) 'WINDOW-TYPEOUT-STREAM-END-SCREEN))
	     (DOLIST (BL (PC-PPR-BLINKER-LIST PC-PPR))
	       (OR (TV-BLINKER-Y-POS BL)
		   ;; This blinker is where text is typed out.
		   (SETQ CURSOR-BLINKER BL)))
	     ;; This item-blinker is used by both the stream and the menu
	     ;; to highlight the selected item.
	     (SETQ ITEM-BLINKER (TV-DEFINE-BLINKER PC-PPR ':VISIBILITY NIL ':HALF-PERIOD 8))
	     (SETQ MENU (<- WINDOW-TYPEOUT-MENU-CLASS ':NEW
			    ':ITEM-BLINKER ITEM-BLINKER
			    ':ITEM-LIST NIL
			    ':WINDOW WINDOW))
	     (SETQ BUFFER (MAKE-ARRAY NIL 'ART-16B 1000 NIL '(0 0)))
	     (SETQ SELF
		   (CLOSURE '(BUFFER UNRCHF PC-PPR
			      ITEM-LIST ITEM-TYPE-ALIST ITEM-BLINKER CURSOR-BLINKER
			      HIDDEN-BLINKERS COMMAND-BUFFER BOTTOM-REACHED
			      MENU WINDOW SELF INCOMPLETE-P)
			    'WINDOW-TYPEOUT-STREAM-INTERNAL))
	     (<- MENU ':STREAM<- SELF)))))

(DEF WINDOW-TYPEOUT-STREAM-INTERNAL
  (LOCAL-DECLARE ((SPECIAL BUFFER UNRCHF PC-PPR
			   ITEM-LIST ITEM-TYPE-ALIST ITEM-BLINKER CURSOR-BLINKER
			   HIDDEN-BLINKERS COMMAND-BUFFER BOTTOM-REACHED MENU WINDOW
			   INCOMPLETE-P
			   TV-MAKE-STREAM-BUFFER TV-MAKE-STREAM-PC-PPR))
    
    (DEFUN WINDOW-TYPEOUT-STREAM-INTERNAL (OP &OPTIONAL ARG1 &REST REST &AUX TEM)
	   (COND (BOTTOM-REACHED)
		 ((MEMQ OP '(:WINDOW :PC-PPR :WHICH-OPERATIONS :DEACTIVATE :ACTIVE-P
			     :TYI :UNTYI :TYI-NO-HANG :LISTEN :CLEAR-INPUT
			     :MOUSE-MOVES :MOUSE-BUTTONS
			     :BOTTOM-REACHED :INCOMPLETE-P :MAKE-COMPLETE)))
		 (T
		   ;; Here if we are not active and should become active.

		   ;; Take the pc-ppr's normal blinkers off the screen.
		   ;; Note that their visibilities are not clobbered even temporarily,
		   ;; and can be changed without lossage while they are in this state.
		   (WITHOUT-INTERRUPTS
		     (MAPC #'TV-OPEN-BLINKER
			   (SETQ HIDDEN-BLINKERS (PC-PPR-BLINKER-LIST PC-PPR))))
		   ;; Shadow them with our list of blinkers.
		   (SETF (PC-PPR-BLINKER-LIST PC-PPR)
			 (CONS ITEM-BLINKER
			       (AND CURSOR-BLINKER (LIST CURSOR-BLINKER))))
		   ;; Turn on our own blinker.
		   (AND CURSOR-BLINKER
			(TV-SET-BLINKER-VISIBILITY CURSOR-BLINKER ':BLINK))
		   
		   ;; Home up, and clear the top line.
		   (TV-HOME PC-PPR)
		   (TV-CLEAR-EOL PC-PPR)
		   (MOUSE-WAKEUP)  ;In case mouse is now inside us.
		   (SETQ BOTTOM-REACHED 0)
		   (SETQ INCOMPLETE-P T)
		   (SETQ ITEM-LIST NIL)
		   (SETQ COMMAND-BUFFER NIL)))
	   (SELECTQ OP
	     (:WHICH-OPERATIONS '(:LISTEN :TYI :TYI-NO-HANG :CLEAR-INPUT
				  :RUBOUT-HANDLER :UNTYI
				  :TYO :LINE-OUT :STRING-OUT :FRESH-LINE :HOME-CURSOR
				  :READ-CURSORPOS :SET-CURSORPOS :PC-PPR
				  :WINDOW :CLEAN :ITEM :ITEM-LIST :CLEAR-SCREEN :CLEAR-EOF
				  :ACTIVE-P :DEACTIVATE :CHOOSE-OR-TYI
				  :BOTTOM-REACHED :INCOMPLETE-P :MAKE-COMPLETE
				  :COMMAND-PUSH :MOUSE-MOVES :MOUSE-BUTTONS))
	     ;; Important output operations come first.
	     (:TYO
	       (SETQ INCOMPLETE-P T)
	       (TV-TYO PC-PPR ARG1)
	       (MOUSE-WAKEUP))
	     (:STRING-OUT
	       (SETQ INCOMPLETE-P T)
	       (LEXPR-FUNCALL #'TV-STRING-OUT PC-PPR ARG1 REST)
	       (MOUSE-WAKEUP))
	     (:LINE-OUT
	       (SETQ INCOMPLETE-P T)
	       ;God damn it, how many times do I have to fix this bug?  This routine
	       ;is NOT supposed to be calling TV-LINE-OUT.  That function has nothing
	       ;to do with the LINE-OUT stream operation.  Please don't keep breaking
	       ;this by changing it to call TV-LINE-OUT!

	       ;; Maybe this proves that TV-LINE-OUT should have its name changed.
	       (LEXPR-FUNCALL #'TV-STRING-OUT PC-PPR ARG1 REST)	       
	       (TV-CRLF PC-PPR)
	       (MOUSE-WAKEUP))
	     (:FRESH-LINE
	       (SETQ INCOMPLETE-P T)
	       (OR (ZEROP (TV-READ-CURSORPOS PC-PPR)) (TV-CRLF PC-PPR))
	       (MOUSE-WAKEUP))
	     (:HOME-CURSOR
	       (SETQ INCOMPLETE-P T)
	       (SETQ BOTTOM-REACHED (MAX BOTTOM-REACHED
					 (- (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-TOP PC-PPR))))
	       (TV-HOME PC-PPR)
	       (TV-CLEAR-EOL PC-PPR)
	       (SETQ ITEM-LIST NIL))
	     ((:CLEAN :CLEAR-SCREEN)
	      (SETQ INCOMPLETE-P T)
	      (TV-CLEAR-PC-PPR PC-PPR)
	      (TV-HOME PC-PPR)
	      (SETQ BOTTOM-REACHED (- (PC-PPR-BOTTOM-LIMIT PC-PPR) (PC-PPR-TOP PC-PPR)))
	      (SETQ ITEM-LIST NIL)
	      (MOUSE-WAKEUP))
	     (:CLEAR-EOF
	      (SETQ INCOMPLETE-P T)
	      (TV-CLEAR-EOF PC-PPR)
	      (SETQ BOTTOM-REACHED (- (PC-PPR-BOTTOM-LIMIT PC-PPR) (PC-PPR-TOP PC-PPR))))
	     (:READ-CURSORPOS
	       (MULTIPLE-VALUE-BIND (X Y)
		   (TV-READ-CURSORPOS PC-PPR)
		 (SELECTQ ARG1
		   (:PIXEL
		     (PROG () (RETURN X Y)))
		   (:CHARACTER
		     (PROG () (RETURN (// X (PC-PPR-CHAR-WIDTH PC-PPR))
				      (// Y (PC-PPR-LINE-HEIGHT PC-PPR)))))
		   (OTHERWISE (FERROR NIL "~S is not a known unit of cursor position." ARG1)))))
	     (:SET-CURSORPOS
	       (SETQ INCOMPLETE-P T)
	       (SETQ BOTTOM-REACHED (MAX BOTTOM-REACHED
					 (- (PC-PPR-CURRENT-Y PC-PPR) (PC-PPR-TOP PC-PPR))))
	       (LET ((X (FIRST REST)) (Y (SECOND REST)))
		 (SELECTQ ARG1
		   (:PIXEL
		     (TV-SET-CURSORPOS PC-PPR X Y))
		   (:CHARACTER
		     (TV-SET-CURSORPOS PC-PPR
				       (* X (PC-PPR-CHAR-WIDTH PC-PPR))
				       (* Y (PC-PPR-LINE-HEIGHT PC-PPR))))
		   (OTHERWISE (FERROR NIL "~S is not a known unit of cursor position." ARG1))))	       (MOUSE-WAKEUP))
	     ;; Input operations.
	     (:TYI
	       (COND ;;Give buffered-back character, if any
		 (UNRCHF
		   (PROG1 UNRCHF
			  (SETQ UNRCHF NIL)))
		 ;;Give buffered input from rubout-processor, if using rubout processor
		 ((AND RUBOUT-HANDLER			; and unread buffered input exists
		       (> (ARRAY-LEADER BUFFER 0)
			  (SETQ TEM (ARRAY-LEADER BUFFER 1))))
		  (STORE-ARRAY-LEADER (1+ TEM) BUFFER 1)
		  (AR-1 BUFFER TEM))
		 ;;If the rubout processor is not being used, get from the keyboard with no echo
		 ((NOT RUBOUT-HANDLER) 
		  (KBD-TYI))
		 ;;Get more input via rubout processor.
		 (T
		   (LET ((TV-MAKE-STREAM-PC-PPR PC-PPR)
			 (TV-MAKE-STREAM-BUFFER BUFFER))
		     (TV-MAKE-STREAM-RUBOUT-HANDLER)))))
	     (:TYI-NO-HANG
	       (COND ;;Give buffered-back character, if any
		 (UNRCHF (PROG1 UNRCHF (SETQ UNRCHF NIL))) 
		 ;;Give buffered input from rubout-processor, if using rubout processor
		 ((AND RUBOUT-HANDLER			; and unread buffered input exists
		       (> (ARRAY-LEADER BUFFER 0)
			  (SETQ TEM (ARRAY-LEADER BUFFER 1))))
		  (STORE-ARRAY-LEADER (1+ TEM) BUFFER 1)
		  (AR-1 BUFFER TEM))
		 (T (KBD-TYI-NO-HANG))))
	     (:UNTYI
	       (SETQ UNRCHF ARG1))
	     (:LISTEN
	       (COND (UNRCHF)
		     ((NOT RUBOUT-HANDLER)
		      (KBD-CHAR-AVAILABLE))
		     ((> (ARRAY-LEADER BUFFER 0)
			 (SETQ TEM (ARRAY-LEADER BUFFER 1)))
		      (AR-1 BUFFER TEM))))
	     (:CLEAR-INPUT
	       (SETQ UNRCHF NIL)		;Clear :UNTYI buffer
	       (STORE-ARRAY-LEADER 0 BUFFER 0)	;Clear rubout-handler buffer
	       (STORE-ARRAY-LEADER 0 BUFFER 1)
	       (DO () ((NOT (KBD-CHAR-AVAILABLE)))	;Clear hardware/firmware buffers
		 (KBD-TYI)))
	     (:RUBOUT-HANDLER
	       (STORE-ARRAY-LEADER 0 BUFFER 0)	;Empty the buffer
	       (STORE-ARRAY-LEADER 0 BUFFER 1)
	       (COND (UNRCHF		;Absorb and echo any buffered-back char
		       (ARRAY-PUSH BUFFER UNRCHF)
		       (TV-TYO PC-PPR (LDB %%KBD-CHAR UNRCHF))
		       (SETQ UNRCHF NIL)))
	       ;;Prompt if desired
	       (AND (SETQ TEM (ASSQ ':PROMPT ARG1))
		    (FUNCALL (CADR TEM) #'WINDOW-TYPEOUT-STREAM-INTERNAL NIL))
	       (DO ((RUBOUT-HANDLER T)	;Establish rubout handler
		    (TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS ARG1))
		   (())			;Loop until normal (non-throw) exit
		 (*CATCH 'RUBOUT-HANDLER	;which will pass by this CATCH and DO
			 (PROGN
			   (ERRSET (RETURN (APPLY (CAR REST) (CDR REST))))
			   ;; On error, reprint contents of buffer so user can rub out ok.
			   (TV-STRING-OUT PC-PPR BUFFER)
			   ;; Swallow all input following error.  User must rub it out.
			   (DO () (NIL) (WINDOW-TYPEOUT-STREAM-INTERNAL ':TYI))))
		 ;; When a rubout or other editing operation is done, throws back to that
		 ;; catch to reread the input.  But if the :FULL-RUBOUT option was specified
		 ;; and everything was rubbed out, we return NIL and the specified value.
		 (AND (ZEROP (ARRAY-LEADER BUFFER 0))
		      (SETQ TEM (ASSQ ':FULL-RUBOUT TV-MAKE-STREAM-RUBOUT-HANDLER-OPTIONS))
		      (RETURN NIL (CADR TEM)))))
	     ;; Operations specific to window typeout streams.
	     (:ITEM-LIST
	       (WINDOW-TYPEOUT-ITEM-LIST ARG1 (CAR REST)))
	     (:ITEM
	       (SETQ INCOMPLETE-P T)   ;this is like typeout in this regard.
	       (LET (TOP-Y)
		 (MULTIPLE-VALUE (TEM TOP-Y) (TV-READ-CURSORPOS PC-PPR))
		 (IF (CDR REST)
		     (LEXPR-FUNCALL 'FORMAT 'WINDOW-TYPEOUT-STREAM-INTERNAL (CDR REST))
		     (PRINC (CAR REST) 'WINDOW-TYPEOUT-STREAM-INTERNAL))
		 (PUSH (LIST ARG1 (CAR REST)
			     TEM TOP-Y (TV-READ-CURSORPOS PC-PPR)
			     (+ TOP-Y (PC-PPR-LINE-HEIGHT PC-PPR)))
		       ITEM-LIST))
	       (MOUSE-WAKEUP))
	     (:PC-PPR
	       PC-PPR)
	     (:WINDOW
	       WINDOW)
	     (:INCOMPLETE-P INCOMPLETE-P)
	     (:MAKE-COMPLETE (SETQ INCOMPLETE-P NIL))
	     ((:ACTIVE-P :BOTTOM-REACHED)
	      (AND BOTTOM-REACHED
		   (+ (PC-PPR-LINE-HEIGHT PC-PPR)
		      (MAX (+ BOTTOM-REACHED (PC-PPR-TOP PC-PPR))
			   (PC-PPR-CURRENT-Y PC-PPR)))))
	     (:DEACTIVATE
	       (PROG1 (AND BOTTOM-REACHED
			   (+ (PC-PPR-LINE-HEIGHT PC-PPR)
			      (MAX (+ BOTTOM-REACHED (PC-PPR-TOP PC-PPR))
				   (PC-PPR-CURRENT-Y PC-PPR))))
		      (COND (BOTTOM-REACHED
			     ;; Take our blinkers off the screen.
			     (WITHOUT-INTERRUPTS
			       (MAPC #'TV-OPEN-BLINKER (PC-PPR-BLINKER-LIST PC-PPR)))
			     ;; Turn back on all the pc-ppr's usual blinkers.
			     ;; Once we put them in (PC-PPR-BLINKER-LIST PC-PPR),
			     ;; they will be put on the screen by TV-BLINKER-CLOCK.
			     (SETF (PC-PPR-BLINKER-LIST PC-PPR) HIDDEN-BLINKERS)))
		      ;; Turn off our own blinkers.
		      (TV-SET-BLINKER-VISIBILITY ITEM-BLINKER NIL)
		      (AND CURSOR-BLINKER
			   (TV-SET-BLINKER-VISIBILITY CURSOR-BLINKER NIL))
		      (SETQ BOTTOM-REACHED NIL)))
	     (:COMMAND-PUSH (PUSH ARG1 COMMAND-BUFFER))
	     (:CHOOSE-OR-TYI
	       (OR COMMAND-BUFFER
		   (WINDOW-TYPEOUT-STREAM-INTERNAL ':LISTEN)
		   (PROCESS-WAIT "TYI"
				 #'(LAMBDA (BUFFER-LOC) (OR (CAR BUFFER-LOC)
							    (KBD-CHAR-AVAILABLE)))
				 (LOCATE-IN-CLOSURE SELF 'COMMAND-BUFFER)))
	       (IF COMMAND-BUFFER (POP COMMAND-BUFFER)
		   (WINDOW-TYPEOUT-STREAM-INTERNAL ':TYI)))
	     (:MOUSE-MOVES
	       (COND ((NULL BOTTOM-REACHED) NIL)
		     ((< MOUSE-Y (+ (PC-PPR-LINE-HEIGHT PC-PPR)
				    (MAX (+ BOTTOM-REACHED (PC-PPR-TOP PC-PPR))
					 (PC-PPR-CURRENT-Y PC-PPR))))
		      (LET ((ITEM (WINDOW-TYPEOUT-MOUSE-ITEM ITEM-LIST PC-PPR MOUSE-X MOUSE-Y))
			    LEFT TOP RIGHT BOTTOM)
			(COND ((NULL ITEM)
			       (TV-SET-BLINKER-VISIBILITY ITEM-BLINKER NIL))
			      (T
				(SETQ LEFT (WINDOW-TYPEOUT-ITEM-LEFT ITEM))
				(SETQ TOP (WINDOW-TYPEOUT-ITEM-TOP ITEM))
				(SETQ RIGHT (WINDOW-TYPEOUT-ITEM-RIGHT ITEM))
				(SETQ BOTTOM (WINDOW-TYPEOUT-ITEM-BOTTOM ITEM))
				(OR (AND (TV-BLINKER-VISIBILITY ITEM-BLINKER)
					 (= LEFT (- (TV-BLINKER-X-POS ITEM-BLINKER)
						    (PC-PPR-LEFT-MARGIN PC-PPR)))
					 (= TOP (- (TV-BLINKER-Y-POS ITEM-BLINKER)
						   (PC-PPR-TOP-MARGIN PC-PPR)))
					 (= (- RIGHT LEFT)
					    (TV-BLINKER-WIDTH ITEM-BLINKER))
					 (= (- BOTTOM TOP)
					    (TV-BLINKER-HEIGHT ITEM-BLINKER)))
				    (PROGN
				      (TV-SET-BLINKER-CURSORPOS ITEM-BLINKER LEFT TOP)
				      (TV-SET-BLINKER-SIZE ITEM-BLINKER
							   (- RIGHT LEFT) (- BOTTOM TOP))
				      (TV-SET-BLINKER-VISIBILITY ITEM-BLINKER T))))))
		      T)
		     (T (TV-SET-BLINKER-VISIBILITY ITEM-BLINKER NIL) NIL)))
	     (:MOUSE-BUTTONS  ;Args are BD, X, Y, just as for the window op.
	       (COND ((NULL BOTTOM-REACHED) NIL)
		     (( (CADR REST)		;Return NIL if mouse not inside typeout area.
			 (+ (PC-PPR-LINE-HEIGHT PC-PPR)
			    (MAX (+ BOTTOM-REACHED (PC-PPR-TOP PC-PPR))
				 (PC-PPR-CURRENT-Y PC-PPR))))
		      NIL)
		     ;; If mouse is inside typeout area, return T always.
		     ;; Process double clicks.  Getting system menu works as always.
		     ((= (SETQ TEM (MOUSE-BUTTON-ENCODE ARG1)) 2012)
		      (<- WINDOW-DEFAULT-MENU ':CHOOSE)
		      T)
		     ;; Anything other than system menu puts a command on the
		     ;; COMMAND-BUFFER or runs our own menu MENU.
		     (T
		       (LET (ITEM ITEM-TYPE)
			 (SETQ ITEM
			       (WINDOW-TYPEOUT-MOUSE-ITEM ITEM-LIST PC-PPR (CAR REST) (CADR REST)))
			 (SETQ ITEM-TYPE (WINDOW-TYPEOUT-ITEM-TYPE ITEM))
			 (COND ((AND ITEM (SETQ ITEM-TYPE (ASSQ ITEM-TYPE ITEM-TYPE-ALIST)))
				(SELECTQ TEM
				  (2000 (PUSH (LIST ':TYPEOUT-EXECUTE (CADR ITEM-TYPE)
						    (WINDOW-TYPEOUT-ITEM-ITEM ITEM))
					      COMMAND-BUFFER))
				  (2002 (<- MENU ':CHOOSE ITEM (CDDR ITEM-TYPE)))
				  (OTHERWISE (TV-BEEP))))
			       (T
				 (TV-BEEP))))
		       T)))
	     (OTHERWISE
	       (FUNCALL 'STREAM-DEFAULT-HANDLER
			'WINDOW-TYPEOUT-STREAM-INTERNAL OP ARG1 REST))))))

;;; End of screen function.  Wraps around (MORE processing already done if needed).
;;; Also makes the typeout remember that the entire pc-ppr has been used for typeout
;;; even though the current y will not be at the bottom any longer.
;;; We are actually used inside a closure on BOTTOM-REACHED.
(DEFUN WINDOW-TYPEOUT-STREAM-END-SCREEN (PC-PPR)
  (LOCAL-DECLARE ((SPECIAL BOTTOM-REACHED))
    (SETQ BOTTOM-REACHED (- (PC-PPR-BOTTOM-LIMIT PC-PPR)
			    (PC-PPR-TOP PC-PPR))))
  (TV-END-SCREEN-DEFAULT PC-PPR))

;; This is the guts of the :ITEM-LIST operation on the typeout window stream.
;; We are given a list of items and an item type that applies to them all,
;; and pack them onto lines horizontally into columns.
(DEFUN WINDOW-TYPEOUT-ITEM-LIST (ITEM-TYPE LIST &AUX (MAXL 0) YPOS STRING
			       WIDTH N WIDTH-PER)
  (LOCAL-DECLARE ((SPECIAL ITEM-LIST PC-PPR))
    (AND (PLUSP (TV-READ-CURSORPOS PC-PPR))		;Assure new-line
	 (WINDOW-TYPEOUT-STREAM-INTERNAL ':TYO #\CR))
    (COND (LIST		;Do nothing if empty list
	   ;; Compute the maximum width of any item, in dots (MAXL).
	   (DO ((LIST LIST (CDR LIST)))
	       ((NULL LIST))
	     (SETQ STRING (STRING (IF (LISTP (CAR LIST)) (CAAR LIST) (CAR LIST))))
	     (SETQ MAXL (MAX (TV-STRING-LENGTH PC-PPR STRING) MAXL)))
	   (SETQ WIDTH (- (PC-PPR-RIGHT-LIMIT PC-PPR) (PC-PPR-LEFT-MARGIN PC-PPR)))
	   (AND (> MAXL WIDTH)
		(FERROR NIL "Some item is too big for this typeout window"))
	   ;; How many items go on each line (except the last)?
	   (SETQ N (MIN (// WIDTH (+ MAXL (FONT-CHAR-WIDTH (PC-PPR-CURRENT-FONT PC-PPR))))
			(LENGTH LIST)))
	   ;; How much space horizontally does each item get?
	   (SETQ WIDTH-PER (// WIDTH N))
	   ;; Now print the items and store the data in the table.
	   ;; Move to a new line when we exhaust a line, and at the end.
	   ;; I counts from 1 thru N on each line.
	   (DO ((I 1 (1+ I))
		(LIST LIST (CDR LIST)))
	       ((NULL LIST))
	     (MULTIPLE-VALUE (NIL YPOS)
	       (TV-READ-CURSORPOS PC-PPR))
	     ;; Actually make this item.
	     (IF (LISTP (CAR LIST))
		 (WINDOW-TYPEOUT-STREAM-INTERNAL ':ITEM ITEM-TYPE (CDAR LIST) "~A" (CAAR LIST))
		 (WINDOW-TYPEOUT-STREAM-INTERNAL ':ITEM ITEM-TYPE (CAR LIST)))
	     ;; Space out for next item, or move to new line.
	     (COND ((AND ( I N) (CDR LIST))
		    ;; Not end of line => space out for next item.
		    (TV-SET-CURSORPOS PC-PPR 
				      (* WIDTH-PER
					 (// (+ (1- WIDTH-PER) (TV-READ-CURSORPOS PC-PPR))
					     WIDTH-PER))
				      YPOS))
		   (T
		     ;; End of line.
		     (TV-CRLF PC-PPR)
		     (SETQ I 0))))))))

;;; This returns the mouse item you are pointing to
;;; Our value is just the WINDOW-TYPEOUT-ITEM
(DEFUN WINDOW-TYPEOUT-MOUSE-ITEM (ITEM-LIST PC-PPR X Y)
    (SETQ X (- X (PC-PPR-LEFT-MARGIN PC-PPR))
	  Y (- Y (PC-PPR-TOP-MARGIN PC-PPR)))
    (PROG FOO ()
	  (DOLIST (ITEM ITEM-LIST)
	    (AND ( Y (WINDOW-TYPEOUT-ITEM-TOP ITEM))
		 (< Y (WINDOW-TYPEOUT-ITEM-BOTTOM ITEM))
		 ( X (WINDOW-TYPEOUT-ITEM-LEFT ITEM))
		 (< X (WINDOW-TYPEOUT-ITEM-RIGHT ITEM))
		 (RETURN-FROM FOO ITEM)))))

;;; Pop-up menus for selecting actions to perform on a typeout item.

(DEFMETHOD (WINDOW-TYPEOUT-MENU-CLASS :CHOOSE) (WINDOW-TYPEOUT-ITEM ALIST)
  (SETQ ITEM WINDOW-TYPEOUT-ITEM)
  (COND ((NEQ ALIST ITEM-LIST)
         (SETQ ITEM-LIST ALIST
               CURRENT-ITEM NIL
               LAST-ITEM NIL)
         (<- SELF ':COMPUTE-GEOMETRY)))
  (<-AS MOMENTARY-MENU-CLASS ':CHOOSE))

(DEFMETHOD (WINDOW-TYPEOUT-MENU-CLASS :EXECUTE) (MENU-ITEM WINDOW-AND-POSITION)
  WINDOW-AND-POSITION		;This argument not used
  (AND MENU-ITEM (FUNCALL STREAM ':COMMAND-PUSH (LIST ':TYPEOUT-EXECUTE (CDR MENU-ITEM)
						      (WINDOW-TYPEOUT-ITEM-ITEM ITEM)))))

;;; This tries to put the menu in a place that won't interfere with the item selected
(DEFMETHOD (WINDOW-TYPEOUT-MENU-CLASS :MOVE-NEAR)
      (XPOS YPOS &AUX WIDTH HEIGHT NEW-LEFT NEW-TOP
	    SCREEN-X1 SCREEN-X2 SCREEN-Y1 SCREEN-Y2
	    ITEM-LEFT ITEM-TOP ITEM-RIGHT ITEM-BOTTOM)
  XPOS YPOS ;Completely ignores the specified position, puts near the selected typeout-item
  (SETQ WIDTH (- RIGHT LEFT)
        HEIGHT (- BOTTOM TOP)
        SCREEN-X1 (SCREEN-X1 SCREEN)
        SCREEN-X2 (SCREEN-X2 SCREEN)
        SCREEN-Y1 (SCREEN-Y1 SCREEN)
        SCREEN-Y2 (SCREEN-Y2 SCREEN))
  (LET ((PC-PPR (FUNCALL STREAM ':PC-PPR)))
    ;; Extract absolute coordinates of the item we are acting on.
    (SETQ ITEM-LEFT (+ (WINDOW-TYPEOUT-ITEM-LEFT ITEM)
		       (PC-PPR-LEFT-MARGIN PC-PPR)))
    (SETQ ITEM-TOP (+ (WINDOW-TYPEOUT-ITEM-TOP ITEM)
		      (PC-PPR-TOP-MARGIN PC-PPR)))
    (SETQ ITEM-RIGHT (+ (WINDOW-TYPEOUT-ITEM-RIGHT ITEM)
			(PC-PPR-LEFT-MARGIN PC-PPR)))
    (SETQ ITEM-BOTTOM (+ (WINDOW-TYPEOUT-ITEM-BOTTOM ITEM)
			 (PC-PPR-TOP-MARGIN PC-PPR)))    
    (COND (( (+ ITEM-RIGHT WIDTH) SCREEN-X2)	;Will fit to right of selected item?
	   (SETQ NEW-LEFT ITEM-RIGHT))
	  (( SCREEN-X1 (- ITEM-LEFT WIDTH))	;Will fit to left of selected item?
	   (SETQ NEW-LEFT (- ITEM-LEFT WIDTH))))
    (COND (NEW-LEFT
	    (SETQ NEW-TOP (MIN (MAX SCREEN-Y1 (- ITEM-TOP (// HEIGHT 2)))
			       (- SCREEN-Y2 HEIGHT))))
	  (T
	    (SETQ NEW-LEFT (- SCREEN-X2 WIDTH))
	    (AND (< (SETQ NEW-TOP (- ITEM-TOP HEIGHT)) SCREEN-Y1)
		 (SETQ NEW-TOP (MIN ITEM-BOTTOM (- SCREEN-Y2 HEIGHT))))))
    (<- SELF ':EDGES<- NEW-LEFT NEW-TOP (+ NEW-LEFT WIDTH) (+ NEW-TOP HEIGHT))
    (LIST (+ NEW-LEFT (// WIDTH 2)) (+ NEW-TOP (// HEIGHT 2)))))
