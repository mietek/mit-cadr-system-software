;-*- Mode:Lisp; Package: SYSTEM-INTERNALS -*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **
; A SCROLL-TEXT-WINDOW is, essentially, an arbitrarily long
;page of text which the user can scroll through and programs
;can update.
; The program always deals with the window in terms of virtual lines,
;each of which can be one or more screen lines long.  A virtual
;line can be more than one screen line either because it is too
;long to fit in just one (continuation, which is automatic) or
;because there are Return characters in it.
; The handlers for scroll text windows automatically convert from
;virtual lines to screen lines.  Scrolling is in terms of screen
;lines, so that the way the text is broken down in to virtual lines
;need not be visible to the user.  The :VIRTUAL-LINE message is
;used to find out the virtual equivalent of a given screen line.
; The simplest possible value for a virtual line is a string to be
;displayed.  The display will always move to a new screen line between
;one virtual line and the next, so that a virtual line which is
;the empty string causes a blank line on the screen.
; The program can change the contents of any virtual line with a
;:CHANGE-LINE message, or create or delete virtual lines at any point
;with a :INSERT-LINES or :DELETE-LINES message.  :RESET-LINES can
;be used to reinitialize the window contents.
; Since recomputing the entire contents of the window to display
;updated data can be time consuming, and since the program will have
;to recompute the entire window even if most of it is scrolled
;out of view (in case the user scrolls it back in again),
;we provide for automatically updated items.  A virtual line which
;is a list should contain a mixture of strings, to be printed
;literally, and automatically updated items, which are lists which
;describe how to compute data to be printed and what the data were
;when last displayed.  When the window updates its display, all
;automatically updating items which are in the visible region
;are recomputed and those that have changed are redisplayed.
;This avoids reprinting the constant and unchanged parts of
;the text and avoids recomputing things that are scrolled
;out of view.
; An automatically updated item should look like
;((function pre-evalled-args...) width starting-x screen-line previous-value)
;Printing is done by calling the function with arguments of
;the pc-ppr to print on, a stream for it, the width of the screen area for it,
; the old value, and the pre-evalled-args.
;At that time, the cursor of the pc-ppr is already set up.
;The function may, if it likes, erase the area and print something new.
;See SCROLL-ITEM-COMPARE-STRING-EQ for how to erase it.
;It should return a new value to replace previous-value.
;Width is the width of the screen area allocated to the item,
;and starting-x and screen-line say where that area starts
;(starting-x is in dots from the left margin;  screen-line, in
;screen lines from the start of this virtual line).
;These three are used only for bookkeeping and setting up.
;The function, pre-evalled-args and width should be provided by the program.
;The others should be supplied as NIL.  They will be filled in
;by the process of updating.
;A convenient function to use is SCROLL-ITEM-COMPARE-STRING-EQ
;which expects the first pre-evalled-arg to be a function to apply to the rest
;to produce a string.  If that string is not EQ to the previous-value,
;it replaces the old value and is printed.

;These macros are really defined in JOBDEF.
;(DEFSUBST SCROLL-ITEM-FUNCTION (ITEM) (CAAR ITEM))
;(DEFSUBST SCROLL-ITEM-ARGLIST (ITEM) (CDAR ITEM))
;(DEFSUBST SCROLL-ITEM-WIDTH (ITEM) (CADR ITEM))
;(DEFSUBST SCROLL-ITEM-STARTING-X (ITEM) (CADDR ITEM))
;(DEFSUBST SCROLL-ITEM-SCREEN-LINE (ITEM) (CADDDR ITEM))
;(DEFSUBST SCROLL-ITEM-PREVIOUS-VALUE (ITEM) (CADR (CDDDR ITEM)))

;The LINE-TABLE is the fundamental data base of the scroll window.
;Each element of the line table describes one virtual line.
;The car of the element is the contents of the line as set by :CHANGE-LINE, etc.
;The cadr is the number of screen lines occupied by all the preceding virtual lines.
;The caddr need not exist.  It is intended for use as the "topic" of the line
;so that someone can decide which lines to delete and which to insert.
;For example, when PEEK is displaying the status of active processes it sets
;the topic of the line for a process to be that process.
;Then, PEEK can use the topics to tell when a process is active and not in the list
;or vice versa.
;The last element of the LINE-TABLE is a dummy whose CONTENTS is NIL
;and whose starting screen line is the total number of screen lines used.

;(DEFSUBST SCROLL-LINE-CONTENTS (LTE) (CAR LTE))
;(DEFSUBST SCROLL-LINE-SCREEN-LINE (LTE) (CADR LTE))
;(DEFSUBST SCROLL-LINE-TOPIC (LTE) (CADDR LTE))

;TO PREVENT TIMING ERRORS
;it is necessary to take into account that a :SCROLL-POSITION<- message can
;be sent at any time by the mouse process to change TOP-SCREEN-LINE.
;Anyone who uses TOP-SCREEN-LINE several times must either lock out interrupts
;or copy it once and use the copy, if he depends on getting the same value each time.
;The same goes for anyone who reads and then writes TOP-SCREEN-LINE
;if he depends on any relationship between the old and new values.
;Since :SCROLL-POSITION<- uses the screen line number of the phony last virtual
;line to decide on the range of legitimate TOP-SCREEN-LINE values,
;changing that screen line number may also require locking out interrupts.

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :BORN) ()
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':BORN)
    (SETQ STREAM (TV-MAKE-STREAM PC-PPR))
    (<- SELF ':RESET-LINES 0))

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :RESET-LINES) (&OPTIONAL (LINES 0))
    (SETQ LINE-TABLE (LIST (LIST NIL 0)))
    (SETQ TOP-SCREEN-LINE 0)
    (SETQ REDISPLAY-NEEDED T)
    (OR (ZEROP LINES)
	(<- SELF ':INSERT-LINES 0 LINES))
    (<- SCROLL-BAR ':SCROLLING-DONE SELF))

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :PC-PPR<-) (NEW-PC-PPR)
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':PC-PPR<- NEW-PC-PPR)
    (SETQ SCREEN-LINES (// (- (PC-PPR-BOTTOM NEW-PC-PPR)
			      (PC-PPR-TOP NEW-PC-PPR))
			   (PC-PPR-LINE-HEIGHT NEW-PC-PPR)))
    (TV-REDEFINE-PC-PPR NEW-PC-PPR
	':BOTTOM
	(+ (PC-PPR-TOP NEW-PC-PPR) (* SCREEN-LINES (PC-PPR-LINE-HEIGHT NEW-PC-PPR))))
    (SETF (PC-PPR-END-SCREEN-FCN NEW-PC-PPR) 'SCROLL-TEXT-WINDOW-END-SCREEN-FCN)
    (SETF (PC-PPR-MORE-VPOS NEW-PC-PPR) NIL))

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :EDGES<-)
	   (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':EDGES<-
	  NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (SETQ REDISPLAY-NEEDED T)
    (COND (SI:PC-PPR
	   (SETQ SCREEN-LINES (// (- NEW-BOTTOM NEW-TOP)
				  (PC-PPR-LINE-HEIGHT SI:PC-PPR)))
	   (WITHOUT-INTERRUPTS
             (AND TOP-SCREEN-LINE LINE-TABLE
		  (SETQ TOP-SCREEN-LINE
			(MAX 0
			     (MIN TOP-SCREEN-LINE
				  (- (SCROLL-LINE-SCREEN-LINE (CAR (LAST LINE-TABLE)))
				     SCREEN-LINES))))))
	   (TV-REDEFINE-PC-PPR SI:PC-PPR
	       ':BOTTOM
	       (+ (PC-PPR-TOP SI:PC-PPR) (* SCREEN-LINES (PC-PPR-LINE-HEIGHT SI:PC-PPR))))))
    ;; Recompute continuation lines and positions of items in lines.
    (DO ((L LINE-TABLE (CDR L)) (I 0 (1+ I))) ((NULL L))
       (AND (STRINGP (SCROLL-LINE-CONTENTS (CAR L)))
	   (<- SELF ':CHANGE-LINE I (SCROLL-LINE-CONTENTS (CAR L))))))

(DEFUN SCROLL-TEXT-WINDOW-END-SCREEN-FCN (PP)
    (SETF (PC-PPR-END-PAGE-FLAG PP) 0)
    (*THROW 'SCROLL-REDISPLAY NIL))

;; Don't activate our PC-PPR when we are selected (or ever).
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :SELECT) ()
    (<-AS WINDOW-CLASS ':SELECT))

;For debugging only.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :PRINT-LINE-TABLE) ()
    (DO ((L LINE-TABLE (CDR L)) (I 0 (1+ I))) ((NULL L))
	(FORMAT T "~%~D ~S ~S" I
		(SCROLL-LINE-SCREEN-LINE (CAR L))
		(SCROLL-LINE-CONTENTS (CAR L)))))

;; Operations for changing the text being scrolled through.

;; Change a virtual line to a new string.
;; Automatically gobbles or releases screen lines as necessary.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :CHANGE-LINE) (LINE-NUMBER STRING &OPTIONAL TOPIC
						    &AUX LINE LINE-PTR SCREEN-LINE NEW-END)
    (AND (OR (< LINE-NUMBER 0)
	     (NULL (CDR (SETQ LINE-PTR (NTHCDR LINE-NUMBER LINE-TABLE)))))
	 (FERROR NIL "Virtual line number ~D out of range in ~S"
		 LINE-NUMBER SELF))
    (SETQ LINE (CAR LINE-PTR))
    (SETF (SCROLL-LINE-CONTENTS LINE) STRING)
    (AND TOPIC (NULL (CDDR LINE))
         (RPLACD (CDR LINE) (LIST NIL)))
    (AND (CDDR LINE)
         (SETF (SCROLL-LINE-TOPIC LINE) TOPIC))
    (SETQ SCREEN-LINE (SCROLL-LINE-SCREEN-LINE LINE))
    (COND ((STRINGP STRING)
	   ;; line is a string => see ehere it leaves us, in dots rel. to start.
	   ;; Include a CR at the end.  Including the CR here
	   ;; prevents superfluous continuation if it fits exactly.
	   (MULTIPLE-VALUE (NIL NEW-END)
	          (TV-COMPUTE-MOTION SI:PC-PPR 0 0 STRING
				     0 NIL T
				     0 10000)))
	  (T (SETQ NEW-END 0)
	     ;; A list of strings and items.  Process them cumulatively.
	     ;; NEW-END is vertical, in dots, and NEW-END-X is horizontal.
	     (DO ((S STRING (CDR S)) (NEW-END-X 0)) ((NULL S))
		 (COND ((STRINGP (CAR S))
			;; Include a CR after a string if string is last thing.
			(MULTIPLE-VALUE (NEW-END-X NEW-END)
			      (TV-COMPUTE-MOTION SI:PC-PPR NEW-END-X NEW-END (CAR S)
						 0 NIL (NULL (CDR S))
						 0 10000)))
		       (T (SETF (SCROLL-ITEM-STARTING-X (CAR S)) NEW-END-X)
			  (SETF (SCROLL-ITEM-SCREEN-LINE (CAR S))
				(// NEW-END (PC-PPR-LINE-HEIGHT SI:PC-PPR)))
			  (SETQ NEW-END-X (+ NEW-END-X (SCROLL-ITEM-WIDTH (CAR S))))
			  ;; Handle continuation before an item if it doesn't fit.
			  (OR (<= (+ NEW-END-X (PC-PPR-LEFT-MARGIN PC-PPR))
				  (PC-PPR-RIGHT-MARGIN PC-PPR))
			      (PROGN (SETQ NEW-END (+ NEW-END (PC-PPR-LINE-HEIGHT PC-PPR))
					   NEW-END-X (SCROLL-ITEM-WIDTH (CAR S)))
				     (SETF (SCROLL-ITEM-STARTING-X (CAR S)) 0)
				     (SETF (SCROLL-ITEM-SCREEN-LINE (CAR S))
					   (// NEW-END (PC-PPR-LINE-HEIGHT SI:PC-PPR)))))
			  ;; CR after the last item.
			  (OR (CDR S)
			      (SETQ NEW-END (+ NEW-END (PC-PPR-LINE-HEIGHT SI:PC-PPR)))))))))
    (SETQ NEW-END (+ SCREEN-LINE
		     (// NEW-END (PC-PPR-LINE-HEIGHT SI:PC-PPR))))
    (<- SELF ':GROW-LINE
	LINE-NUMBER
	(- NEW-END (SCROLL-LINE-SCREEN-LINE (CADR LINE-PTR))))
    (WITHOUT-INTERRUPTS
      (COND ((NULL SI:STATUS))
	    (REDISPLAY-NEEDED)
	    ((<= NEW-END TOP-SCREEN-LINE))
	    ((>= SCREEN-LINE (+ TOP-SCREEN-LINE SCREEN-LINES)))
	    ((NOT (STRINGP STRING))
	     (SETQ REDISPLAY-NEEDED T))
	    (T (LET (SPOS YPOS)
		 (SETQ YPOS (MAX SCREEN-LINE TOP-SCREEN-LINE))
		 (MULTIPLE-VALUE (NIL SPOS)
		   (FUNCALL (<- SELF ':HANDLER-FOR ':VIRTUAL-LINE)
			    NIL YPOS))
		 (TV-SET-CURSORPOS SI:PC-PPR 0
				   (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
				      (- YPOS TOP-SCREEN-LINE)))
		 (TV-CLEAR-EOL SI:PC-PPR)
		 (*CATCH 'SCROLL-REDISPLAY
			 (TV-STRING-OUT SI:PC-PPR STRING SPOS)))))))

;; Adjust the size in screen lines of one virtual line.
;; The increment may be of either sign.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :GROW-LINE) (LINE-NUMBER INCREMENT
						    &AUX LINE LINE-PTR SCREEN-LINE SCREEN-SIZE)
    (AND (OR (< LINE-NUMBER 0)
	     (NULL (CDR (SETQ LINE-PTR (NTHCDR LINE-NUMBER LINE-TABLE)))))
	 (FERROR NIL "Virtual line number ~D out of range in ~S"
		 LINE-NUMBER SELF))
    (SETQ LINE (CAR LINE-PTR))
    (SETQ SCREEN-LINE (SCROLL-LINE-SCREEN-LINE LINE))
    (SETQ SCREEN-SIZE (- (SCROLL-LINE-SCREEN-LINE (CADR LINE-PTR)) SCREEN-LINE))
    (SETQ SCREEN-SIZE (+ SCREEN-SIZE INCREMENT))
    (AND (MINUSP SCREEN-SIZE) (FERROR NIL "Screen size of line ~D in ~S made negative"
				     LINE-NUMBER SELF))
    (WITHOUT-INTERRUPTS
      (DO L (CDR LINE-PTR) (CDR L) (NULL L)
	  (SETF (SCROLL-LINE-SCREEN-LINE (CAR L))
		(+ INCREMENT (SCROLL-LINE-SCREEN-LINE (CAR L)))))
      (COND ((< SCREEN-LINE TOP-SCREEN-LINE)
	     (SETQ TOP-SCREEN-LINE (+ TOP-SCREEN-LINE INCREMENT))
	     (OR (< SCREEN-LINE TOP-SCREEN-LINE)
		 (SETQ REDISPLAY-NEEDED T))))
      (COND ((OR REDISPLAY-NEEDED (NULL SI:STATUS)))
	    ((< SCREEN-LINE TOP-SCREEN-LINE))
	    ((>= SCREEN-LINE (+ TOP-SCREEN-LINE SCREEN-LINES)))
	    ((ZEROP INCREMENT))
	    ((> INCREMENT 0)
	     (TV-SET-CURSORPOS SI:PC-PPR 0
			       (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
				  (- SCREEN-LINE TOP-SCREEN-LINE)))
	     (TV-INSERT-LINE SI:PC-PPR INCREMENT))
	    (T (SETQ REDISPLAY-NEEDED T)))))

;; Insert some virtual lines before a specified virtual line.
;; The inserted lines start out blank, using one screen line each.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :INSERT-LINES)
	   (LINE-NUMBER NUMBER-TO-INSERT &AUX OLD-LENGTH LINE-PTR INSERTION-SCREEN-LINE)
    (SETQ OLD-LENGTH (LENGTH LINE-TABLE))
    (AND (OR (< LINE-NUMBER 0)
	     (>= LINE-NUMBER OLD-LENGTH))
	 (FERROR NIL "Virtual line number ~D out of range in ~S"
		 LINE-NUMBER SELF))
    (AND (MINUSP NUMBER-TO-INSERT)
	 (FERROR NIL "Attempt to insert ~D lines, a negative number, in ~S"
		 NUMBER-TO-INSERT SELF))
    (SETQ LINE-PTR (NTHCDR LINE-NUMBER (VALUE-CELL-LOCATION 'LINE-TABLE)))
    (SETQ INSERTION-SCREEN-LINE (SCROLL-LINE-SCREEN-LINE (CADR LINE-PTR)))
    ;; LINE-PTR's CADR is the line before which we insert.
    ;; Make entries for the new lines and patch them into the list.
    (DO ((I 0 (1+ I)) (SL (SCROLL-LINE-SCREEN-LINE (CADR LINE-PTR)) (1+ SL)))
	((= I NUMBER-TO-INSERT))
       (RPLACD LINE-PTR (CONS (LIST "" SL) (CDR LINE-PTR)))
       (SETQ LINE-PTR (CDR LINE-PTR)))
    ;; Now LINE-PTR's CAR is the last of the new lines.  Its CADR is the same as before.
    ;; Update the screen lines of all the following virtual lines.
    (WITHOUT-INTERRUPTS
      (DO L (CDR LINE-PTR) (CDR L) (NULL L)
	  (SETF (SCROLL-LINE-SCREEN-LINE (CAR L))
		(+ NUMBER-TO-INSERT (SCROLL-LINE-SCREEN-LINE (CAR L)))))
      (SETQ INSERTION-SCREEN-LINE (- INSERTION-SCREEN-LINE TOP-SCREEN-LINE))
      (COND ((AND SI:STATUS (NOT REDISPLAY-NEEDED))
	     (COND ((< INSERTION-SCREEN-LINE 0)
		    (SETQ TOP-SCREEN-LINE (+ TOP-SCREEN-LINE NUMBER-TO-INSERT)))
		   ((= NUMBER-TO-INSERT 0) NIL)
		   ((>= INSERTION-SCREEN-LINE SCREEN-LINES) NIL)
		   (T
		    (TV-SET-CURSORPOS SI:PC-PPR 0
				      (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
					 INSERTION-SCREEN-LINE))
		    (TV-INSERT-LINE SI:PC-PPR NUMBER-TO-INSERT)))))))

;; Delete some virtual lines starting with a specified virtual line.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :DELETE-LINES)
	   (LINE-NUMBER NUMBER-TO-DELETE
	    &AUX LINE-PTR OLD-LENGTH SCREEN-LINE SCREEN-LINES-TO-DELETE)
    (SETQ OLD-LENGTH (LENGTH LINE-TABLE))
    (AND (OR (< LINE-NUMBER 0)
	     (>= (1+ LINE-NUMBER) OLD-LENGTH))
	 (FERROR NIL "Virtual line number ~D out of range in ~S"
		 LINE-NUMBER SELF))
    (AND (MINUSP NUMBER-TO-DELETE)
	 (FERROR NIL "Attempt to delete ~D lines, a negative number, in ~S"
		 NUMBER-TO-DELETE SELF))
    (SETQ NUMBER-TO-DELETE (MIN NUMBER-TO-DELETE (- OLD-LENGTH LINE-NUMBER 1)))
    (SETQ LINE-PTR (NTHCDR LINE-NUMBER (VALUE-CELL-LOCATION 'LINE-TABLE)))
    (SETQ SCREEN-LINE (SCROLL-LINE-SCREEN-LINE (CADR LINE-PTR)))
    (SETQ SCREEN-LINES-TO-DELETE
	  (- (SCROLL-LINE-SCREEN-LINE (CADR (NTHCDR NUMBER-TO-DELETE LINE-PTR)))
	     SCREEN-LINE))
    (RPLACD LINE-PTR (CDR (NTHCDR NUMBER-TO-DELETE LINE-PTR)))
    (WITHOUT-INTERRUPTS
      (DO L (CDR LINE-PTR) (CDR L) (NULL L)
	  (SETF (SCROLL-LINE-SCREEN-LINE (CAR L))
		(- (SCROLL-LINE-SCREEN-LINE (CAR L)) SCREEN-LINES-TO-DELETE)))
      (COND ((< SCREEN-LINE TOP-SCREEN-LINE)
	     (SETQ TOP-SCREEN-LINE (- TOP-SCREEN-LINE SCREEN-LINES-TO-DELETE)))
	    ((< SCREEN-LINE (+ TOP-SCREEN-LINE SCREEN-LINES))
	     (SETQ REDISPLAY-NEEDED T)))))

;; Redisplay itself.

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :EXPOSE) ()
    (SETF (PC-PPR-OUTPUT-HOLD-FLAG PC-PPR) 0)
    (LET ((INHIBIT-SCREEN-RESTORATION-FLAG
	   (OR INHIBIT-SCREEN-RESTORATION-FLAG
	       REDISPLAY-NEEDED)))
      (<-AS WINDOW-CLASS ':EXPOSE)))

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :CLOBBER-SCREEN) ()
    (SETQ REDISPLAY-NEEDED T))

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :UPDATE) (&AUX SPOS L STR
					       (TSL TOP-SCREEN-LINE))
    ;; We save TOP-SCREEN-LINE in TSL and use TSL for the whole update.
    ;; This is so that we are not screwed if another process sends us
    ;; a :SCROLL-POSITION<- message while we are updating.
    (COND (REDISPLAY-NEEDED
	   (SETQ REDISPLAY-NEEDED NIL)
	   (TV-CLEAR-PC-PPR SI:PC-PPR)
	   ;; Compute which virtual line the top screen line comes from,
	   ;; and how much of that virtual line is off the top of the screen.
	   (MULTIPLE-VALUE (L SPOS)
		(FUNCALL SELF ':VIRTUAL-LINE TSL))
	   ;; Display rest of that line, and all of following ones,
	   ;; till either end of data (string is NIL) or end of screen (throw).
	   (*CATCH 'SCROLL-REDISPLAY
	     (AND L
	       (DO L (NTHCDR L LINE-TABLE) (CDR L) ()
	         (OR (SETQ STR (SCROLL-LINE-CONTENTS (CAR L))) (RETURN NIL))
		 (AND (>= (SCROLL-LINE-SCREEN-LINE (CAR L))
			  (+ TSL SCREEN-LINES))
		      (RETURN NIL)) 
		 (TV-SET-CURSORPOS SI:PC-PPR 0
				   (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
				      (MAX 0 (- (SCROLL-LINE-SCREEN-LINE (CAR L))
						TSL))))
		 (COND ((STRINGP STR)
			(TV-STRING-OUT SI:PC-PPR STR SPOS NIL)
			(SETQ SPOS 0))
		       (T
			(AND (LISTP SPOS) (SETQ STR (CAR SPOS) SPOS (CADR SPOS)))
			(DO S STR (CDR S) (NULL S)
			    (COND ((STRINGP (CAR S))
				   (TV-STRING-OUT SI:PC-PPR (CAR S) SPOS NIL))
				  (T
				   (LET ((YPOS (* (PC-PPR-LINE-HEIGHT PC-PPR)
						  (- (+ (SCROLL-LINE-SCREEN-LINE (CAR L))
							(SCROLL-ITEM-SCREEN-LINE (CAR S)))
						     TSL))))
				       (TV-SET-CURSORPOS PC-PPR
					      (SCROLL-ITEM-STARTING-X (CAR S))
					      YPOS)
				       (TV-PREPARE-PC-PPR (PC-PPR)
				         (SETF (SCROLL-ITEM-PREVIOUS-VALUE (CAR S))
					     (LEXPR-FUNCALL
					        (SCROLL-ITEM-FUNCTION (CAR S))
						PC-PPR
						STREAM
						(SCROLL-ITEM-WIDTH (CAR S))
						NIL
						(SCROLL-ITEM-ARGLIST (CAR S)))))
				       (TV-SET-CURSORPOS PC-PPR
					       (+ (SCROLL-ITEM-STARTING-X (CAR S))
						  (SCROLL-ITEM-WIDTH (CAR S)))
					       YPOS))))
			    (SETQ SPOS 0))))))))
	  ;; If no redisplay needed, just updating, update any items that are changed.
	  (T
	   ;; Compute which virtual line the top screen line comes from,
	   ;; and how much of that virtual line is off the top of the screen.
	   (MULTIPLE-VALUE (L SPOS)
		(FUNCALL (<- SELF ':HANDLER-FOR ':VIRTUAL-LINE) NIL TSL))
	   (*CATCH 'SCROLL-REDISPLAY
	     (AND L
	       ;; Scan over all the lines that appear on the screen
	       (DO L (NTHCDR L LINE-TABLE) (CDR L) (NULL L)
	         (OR (SETQ STR (SCROLL-LINE-CONTENTS (CAR L))) (RETURN NIL))
		 (AND (>= (SCROLL-LINE-SCREEN-LINE (CAR L))
			  (+ TSL SCREEN-LINES)) (RETURN NIL)) 
		 (TV-SET-CURSORPOS SI:PC-PPR 0
				   (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
				      (MAX 0 (- (SCROLL-LINE-SCREEN-LINE (CAR L))
						TSL))))
		 (AND (LISTP SPOS) (SETQ STR (CAR SPOS) SPOS 0))
		 ;; If the line is not just a string,
		 ;; scan it for automatically updated items.
		 (OR (STRINGP STR)
		   (TV-PREPARE-PC-PPR (PC-PPR)
		     (DO S STR (CDR S) (NULL S)
			(COND ((NOT (STRINGP (CAR S)))
			       ;; Found one: set cursor, then call function to maybe print.
			       (TV-SET-CURSORPOS PC-PPR
				       (SCROLL-ITEM-STARTING-X (CAR S))
				       (* (PC-PPR-LINE-HEIGHT PC-PPR)
					  (+ (SCROLL-ITEM-SCREEN-LINE (CAR S))
					     (- (SCROLL-LINE-SCREEN-LINE (CAR L))
						TSL))))
			       (SETF (SCROLL-ITEM-PREVIOUS-VALUE (CAR S))
				     (LEXPR-FUNCALL
				        (SCROLL-ITEM-FUNCTION (CAR S))
					PC-PPR
					STREAM
					(SCROLL-ITEM-WIDTH (CAR S))
					(SCROLL-ITEM-PREVIOUS-VALUE (CAR S))
					(SCROLL-ITEM-ARGLIST (CAR S)))))))))
		 ))))))

;; This type of scroll item has a function to do the actual printing.
;; We assume that what will be printed will not change dynamically,
;; so printing is done only if the item is just coming onto the screen, etc.
(DEFUN SCROLL-ITEM-PRINTER-FUNCTION (PC-PPR STANDARD-OUTPUT WIDTH OLD FUNCTION &REST ARGS)
    (COND ((NULL OLD)
	   (TV-ERASE WIDTH
		     (PC-PPR-LINE-HEIGHT PC-PPR)
		     (PC-PPR-CURRENT-X PC-PPR)
		     (PC-PPR-CURRENT-Y PC-PPR)
		     TV-ALU-ANDCA)
	   (APPLY FUNCTION ARGS)))
    T)

(DEFUN SCROLL-ITEM-COMPARE-STRING-EQ (PC-PPR IGNORE WIDTH OLD FUNCTION &REST ARGS)
    (LET ((NEW (APPLY FUNCTION ARGS)))
	 (COND ((NEQ NEW OLD)
		(TV-ERASE WIDTH
			  (PC-PPR-LINE-HEIGHT PC-PPR)
			  (PC-PPR-CURRENT-X PC-PPR)
			  (PC-PPR-CURRENT-Y PC-PPR)
			  TV-ALU-ANDCA)
		(TV-STRING-OUT PC-PPR NEW)))
	 NEW))

(DEFUN SCROLL-ITEM-COMPARE-NUMBER (PC-PPR STREAM WIDTH OLD FUNCTION &REST ARGS)
    (LET ((NEW (APPLY FUNCTION ARGS)))
         (COND ((OR (NULL OLD) (NOT (= NEW OLD)))
		(TV-ERASE WIDTH
			  (PC-PPR-LINE-HEIGHT PC-PPR)
			  (PC-PPR-CURRENT-X PC-PPR)
			  (PC-PPR-CURRENT-Y PC-PPR)
			  TV-ALU-ANDCA)
                (PRIN1 NEW STREAM)))
         NEW))

;((SCROLL-ITEM-FORMAT args-to-be-evalled) width starting-x screen-line previous-values-list)
(DEFUN SCROLL-ITEM-FORMAT (PC-PPR STREAM WIDTH OLD &REST ARGS)
  (AND (NULL OLD)  ;Make smashable list to remember old values
       (SETQ OLD (MAKE-LIST WORKING-STORAGE-AREA (LENGTH ARGS))))
  (DO ((ARGS ARGS (CDR ARGS))
       (OLDS OLD (CDR OLDS))
       (VAL)
       (CHANGE-P NIL))
      ((NULL ARGS)
       (COND (CHANGE-P
	      (TV-ERASE WIDTH
			(PC-PPR-LINE-HEIGHT PC-PPR)
			(PC-PPR-CURRENT-X PC-PPR)
			(PC-PPR-CURRENT-Y PC-PPR)
			TV-ALU-ANDCA)
	      (LEXPR-FUNCALL #'FORMAT STREAM OLD)))
       OLD)
    (SETQ VAL (EVAL (CAR ARGS)))
    (COND ((NOT (EQUAL VAL (CAR OLDS)))
	   (RPLACA OLDS VAL)
	   (SETQ CHANGE-P T)))))

;; Return the number of the virtual line that contains a given screen line
;; (the latter being relative to the whole window, not just what's on the screen).
;; Our second value is the position in the string of the first character
;; which is on the specified screen line.
;; If the virtual line is a list of strings and items,
;; our second value is a list whose car is the tail of that list
;; starting at the first string or item which is partly on that screen line,
;; and whose cadr is the index in the string or item of the first
;; character on that screen line.
;; In order to get both values, you must use FUNCALL to send the message.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :VIRTUAL-LINE)
	   (SCREEN-LINE-NUMBER &AUX SPOS)
    (DO ((I 0 (1+ I)) (L LINE-TABLE (CDR L)))
	((NULL (CDR L)) NIL)
       (COND ((> (SCROLL-LINE-SCREEN-LINE (CADR L)) SCREEN-LINE-NUMBER)
	      (COND ((= (SCROLL-LINE-SCREEN-LINE (CAR L)) SCREEN-LINE-NUMBER)
                     (SETQ SPOS 0))
                    ((STRINGP (SCROLL-LINE-CONTENTS (CAR L)))
		     (MULTIPLE-VALUE (NIL NIL SPOS)
			   (TV-COMPUTE-MOTION SI:PC-PPR 0 0
				       (SCROLL-LINE-CONTENTS (CAR L))
				       0 NIL NIL
				       0 (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
                                            (- SCREEN-LINE-NUMBER
                                               (SCROLL-LINE-SCREEN-LINE (CAR L)))))))
		    (T ;; A list of strings and items.  Process them cumulatively.
		       ;; NEW-END is vertical, in dots, and NEW-END-X is horizontal.
		       (DO ((S (SCROLL-LINE-CONTENTS (CAR L)) (CDR S))
			    (NEW-END-X 0) (NEW-END 0))
			   ((NULL S)
			    (FERROR NIL "Line table inconsistency"))
			  (COND ((STRINGP (CAR S))
				 (MULTIPLE-VALUE (NEW-END-X NEW-END SPOS)
				      (TV-COMPUTE-MOTION SI:PC-PPR NEW-END-X NEW-END (CAR S)
							 0 NIL NIL
							 0 (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
							      (- SCREEN-LINE-NUMBER
								 (SCROLL-LINE-SCREEN-LINE (CAR L)))))))
				(T
				 (SETQ NEW-END (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
						  (SCROLL-ITEM-SCREEN-LINE (CAR S))))
				 (SETQ NEW-END-X (+ (SCROLL-ITEM-STARTING-X (CAR S))
						    (SCROLL-ITEM-WIDTH (CAR S))))
				 ;; Did we reach the desired screen line?
				 (COND ((>= NEW-END (* (PC-PPR-LINE-HEIGHT SI:PC-PPR)
						       (- SCREEN-LINE-NUMBER
							  (SCROLL-LINE-SCREEN-LINE (CAR L)))))
					(RETURN (SETQ SPOS (LIST S 0))))))))
		       (RETURN I SPOS)))
	      (OR (NUMBERP SPOS) (FERROR NIL "LOSSAGE"))
	      (RETURN I SPOS)))))

;; We want a scroll bar.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :HANDLE-MOUSE) ()
    (MOUSE-DEFAULT-HANDLER SELF T))

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :SCROLL-POSITION) ()
    (PROG () (RETURN TOP-SCREEN-LINE
		     (SCROLL-LINE-SCREEN-LINE (CAR (LAST LINE-TABLE))))))

;; This is the usual way to scroll a window, whether with the mouse or on command.
(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :SCROLL-POSITION<-)
	   (NEW-TOP-SCREEN-LINE)
    (SETQ REDISPLAY-NEEDED T)
    (WITHOUT-INTERRUPTS
      (SETQ TOP-SCREEN-LINE
	    (MAX 0 (MIN (- (SCROLL-LINE-SCREEN-LINE (CAR (LAST LINE-TABLE)))
			   SCREEN-LINES)
			NEW-TOP-SCREEN-LINE))))
    (<- SCROLL-BAR ':SCROLLING-DONE SELF))

(DEFMETHOD (SCROLL-TEXT-WINDOW-CLASS :LINE-TABLE<-) (NEW-LINE-TABLE)
    (SETQ LINE-TABLE NEW-LINE-TABLE)
    (SETQ TOP-SCREEN-LINE 0)
    (SETQ REDISPLAY-NEEDED T)
    (<- SCROLL-BAR ':SCROLLING-DONE SELF))
