;;; Some simple ZWEI command functions.     -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; DEFS contains a list of all free variables used by these commands.
;;; Any editor that wishes to use these commands must bind all of them.
;;; When adding any to this file, or to primitives called by functions
;;; in this file, update the list in DEFS.

;;; The caller of these functions should also provide a function
;;; called BARF, to report errors.  It should take arguments like
;;; the &REST to FORMAT.

;;; Commands in this file also use the PROMPT and TYPEIN lines
;;; to interact with the user.  In order to keep the implementation
;;; of these lines as flexible as possible, a very minimal interface
;;; is defined herein.  The following functions may be used to access
;;; these lines:

;;; (PROMPT-LINE <ctl-string> . <args>)
;;;    Do formatted output to the prompt line.  The line is cleared and homed first.

;;; (PROMPT-LINE-MORE <ctl-string> . <args>)
;;;    Do formatted output to the prompt line, without clearing and homeing.

;;; (TYPEIN-LINE <ctl-string> . <args>)
;;;    Do formatted output to the typein line.  The line is cleared and homed first.

;;; (TYPEIN-LINE-MORE <ctl-string> . <args>)
;;;    Do formatted output to the typein line, without clearing and homeing.

;;; (TYPEIN-LINE-Y-OR-N-P <ctl-string> . <args>)
;;;    Do formatted output to the typein line, and ask for a Y or N answer (see Y-OR-N-P).

;;; (TYPEIN-LINE-ACTIVATE . <body>)
;;;    This is a SPECIAL FORM.  Within the body, the blinker for the typein line
;;;    will be on, and input from the typein line is allowed by using the
;;;    value of TYPEIN-LINE-STREAM, on which regular stream input operations will
;;;    work.  None of the other TYPEIN line or PROMPT line functions should
;;;    be called while in the scope of a TYPEIN-LINE-ACTIVATE; all you may
;;;    do is read from the stream.

;;; (TYPEIN-LINE-READLINE <ctl-string> . <args>)
;;;    Read in a line from the typein line, with editting.  The arguments
;;;    are passed along to the prompt line.

;;; (TYPEIN-LINE-READ <ctl-string> . <args>)
;;;    Like the above, but does a READ instead of a READLINE.

;;; *TYPEOUT-WINDOW*
;;;    A larger, menu-like window for random stream output.

(ENDF HEAD)

(DEFCOM COM-SELF-INSERT "Inserts itself." (NM)
  (LET ((CHAR (IN-CURRENT-FONT *LAST-COMMAND-CHAR*))
	(POINT (POINT)))
    (LET ((LINE (BP-LINE POINT)) (INDEX (BP-INDEX POINT)))
	 (DOTIMES (I *NUMERIC-ARG*)
	   (INSERT-MOVING POINT CHAR))
	 (SETQ *CURRENT-COMMAND-TYPE* 'SELF-INSERT)
	 (MVRETURN DIS-LINE LINE INDEX))))

(DEFCOM COM-QUOTED-INSERT "Insert a quoted character" (NM)
  (TYPEIN-LINE "~:[~*~;~A ~]~:@C: "
	       *NUMERIC-ARG-P*
	       (FORMAT-ARGUMENT *NUMERIC-ARG-P* *NUMERIC-ARG*)
	       *LAST-COMMAND-CHAR*)
  (TYPEIN-LINE-ACTIVATE
    (SETQ *LAST-COMMAND-CHAR* (FUNCALL STANDARD-INPUT ':TYI)))
  (TYPEIN-LINE-MORE "~:@C" *LAST-COMMAND-CHAR*)
  (AND (LDB-TEST %%KBD-CONTROL-META *LAST-COMMAND-CHAR*) (BARF))
  (COM-SELF-INSERT))

(DEFCOM COM-FORWARD "Move one or more characters forward.
Move point one character forward.  With a numeric argument,
move point that many characters forward." (KM R)
  (LET ((POINT (POINT)))
    (MOVE-BP POINT (OR (FORWARD-CHAR POINT *NUMERIC-ARG*) (BARF))))
  (SET-CENTERING-FRACTION *NUMERIC-ARG*)
  DIS-BPS)

(DEFCOM COM-BACKWARD "Move one or more characters backward.
Move point one character backward.  With a numeric argument,
move point that many characters backward." (KM -R)
  (LET ((POINT (POINT)))
    (MOVE-BP POINT (OR (FORWARD-CHAR POINT (- *NUMERIC-ARG*)) (BARF))))
  (SET-CENTERING-FRACTION (- *NUMERIC-ARG*))
  DIS-BPS)

(DEFCOM COM-GOTO-CHARACTER "Move point to the nth character in the buffer.
With a negative argument, use the absolute value of the argument, and
count the characters the way ITS would count them, namely,
count newlines as two characters rather than one.  This is useful for interpreting
character counts returned by R and BOLIO." (KM)
  (LET ((DEST (FUNCALL (IF (MINUSP *NUMERIC-ARG*) #'FORWARD-ITS-CHAR #'FORWARD-CHAR)
		       (INTERVAL-FIRST-BP *INTERVAL*) (ABS *NUMERIC-ARG*))))
    (IF (NULL DEST)
	(BARF "There are fewer than ~D. characters in the buffer." *NUMERIC-ARG*)
	(MOVE-BP (POINT) DEST)))
  DIS-BPS)

(DEFCOM COM-DOWN-REAL-LINE "Move down vertically to next real line.
Moves as far as possible horizontally toward the goal column for successive
commands." (KM R)
  (DOWN-REAL-LINE *NUMERIC-ARG*))

(DEFCOM COM-UP-REAL-LINE "Move up vertically to previous real line.
Moves as far as possible horizontally toward the goal column for successive
commands." (KM -R)
  (DOWN-REAL-LINE (- *NUMERIC-ARG*)))

(DEFUN DOWN-REAL-LINE (N-LINES)
  (SETQ *CURRENT-COMMAND-TYPE* 'REAL-MOVE)
  (SET-CENTERING-FRACTION N-LINES)
  (LET ((POINT (POINT))
	(RET DIS-BPS))
    (LET ((DEST (FORWARD-LINE POINT N-LINES)))
      (COND ((NULL DEST)
	     ;; He overshot.
	     (COND ((MINUSP N-LINES)
		    ;; He was going backwards, go to beginnning.
		    (MOVE-BP POINT (INTERVAL-FIRST-BP *INTERVAL*))
		    (SETQ *REAL-LINE-GOAL-XPOS* 0))
		   ((NOT *NUMERIC-ARG-P*)
		    ;; No argument give, going down.  Create a line.
		    (SETQ RET DIS-TEXT)
		    (MOVE-BP POINT (INSERT (INTERVAL-LAST-BP *INTERVAL*) #\CR))
		    (SETQ *REAL-LINE-GOAL-XPOS* 0))
		   (T
		    ;; He was going forwards, go to end.
		    (MOVE-BP POINT (INTERVAL-LAST-BP *INTERVAL*))
		    (SETQ *REAL-LINE-GOAL-XPOS* (BP-INDENTATION POINT)))))
	    (T
	     (SETQ DEST (BP-LINE DEST))
	     (SETQ *REAL-LINE-GOAL-XPOS*
		   (COND (*PERMANENT-REAL-LINE-GOAL-XPOS*)
			 ((EQ *LAST-COMMAND-TYPE* 'REAL-MOVE)
			  *REAL-LINE-GOAL-XPOS*)
			 (T (BP-INDENTATION POINT))))
	     (LET ((INDEX (INDENTATION-INDEX DEST *REAL-LINE-GOAL-XPOS*)))
	       (MOVE-BP POINT DEST (OR INDEX (LINE-LENGTH DEST)))))))
    RET))

(DEFCOM COM-SET-GOAL-COLUMN "Sets the goal column for Up Real Line and Down Real Line." (KM)
  (SETQ *PERMANENT-REAL-LINE-GOAL-XPOS*
	(COND ((> *NUMERIC-ARG* 1) NIL)
	      (T (BP-INDENTATION (POINT)))))
  DIS-NONE)

(DEFCOM COM-RECENTER-WINDOW "Choose a new point in buffer to begin redisplay.
With no argument, center point on the screen.  An argument is the
line of the window to put point on.  Negative arguments count
up from the bottom." (KM)
  (OR *NUMERIC-ARG-P* (MUST-REDISPLAY *WINDOW* DIS-ALL))
  (LET ((N-PLINES (WINDOW-N-PLINES *WINDOW*)))
    (RECENTER-WINDOW *WINDOW*
	       ':ABSOLUTE
	       (IF *NUMERIC-ARG-P*
		   (// (RANGE (+ *NUMERIC-ARG*
				 (IF (MINUSP *NUMERIC-ARG*) N-PLINES 0))
			      0 (1- N-PLINES))
		       (SMALL-FLOAT N-PLINES))
		   *CENTER-FRACTION*)))
  DIS-NONE)

(DEFCOM COM-COMPLETE-REDISPLAY "Redisplay all windows." (KM)
  (FUNCALL *TYPEOUT-WINDOW* ':DEACTIVATE)
  (FUNCALL *MODE-LINE-WINDOW* ':REFRESH)
  (SELECT-WINDOW *WINDOW*)
  (DOLIST (WINDOW *WINDOW-LIST*)
    (AND (WINDOW-READY-P WINDOW)
	 (FUNCALL (WINDOW-SHEET WINDOW) ':REFRESH)))
  DIS-NONE)

(DEFCOM COM-NEXT-SCREEN "Move down to display next screenful of text.
With argument, move window down <arg> lines." (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (IF *NUMERIC-ARG-P*
					 *NUMERIC-ARG*
					 (- (WINDOW-N-PLINES *WINDOW*) 1)))
  DIS-NONE)

(DEFCOM COM-PREVIOUS-SCREEN "Move up to display previous screenful of text.
With argument, move window up <arg> lines." (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (IF *NUMERIC-ARG-P*
					 (- *NUMERIC-ARG*)
					 (- 1 (WINDOW-N-PLINES *WINDOW*))))
  DIS-NONE)

(DEFCOM COM-NEXT-SEVERAL-SCREENS "Move down argument screenfuls of text" (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (* *NUMERIC-ARG* (1- (WINDOW-N-PLINES *WINDOW*))))
  DIS-NONE)

(DEFCOM COM-PREVIOUS-SEVERAL-SCREENS "Move down argument screenfuls of text" (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (* *NUMERIC-ARG* (- 1 (WINDOW-N-PLINES *WINDOW*))))
  DIS-NONE)

(DEFCOM COM-BEGINNING-OF-LINE "Move to the beginning of the line." (KM)
  (MOVE-BP (POINT) (BEG-LINE (POINT) (1- *NUMERIC-ARG*)))
  DIS-BPS)

(DEFCOM COM-END-OF-LINE "Move to the end of the line." (KM)
  (MOVE-BP (POINT) (END-LINE (POINT) (1- *NUMERIC-ARG*)))
  DIS-BPS)

(DEFCOM COM-MOVE-TO-SCREEN-EDGE "Jump to top or bottom of screen.
A numeric argument specifies the screen line to go to, negative arguments count
up from the bottom." (KM)
  (REDISPLAY *WINDOW* ':POINT NIL NIL T)	;Force redisplay to completion first
  (LET ((N-PLINES (WINDOW-N-PLINES *WINDOW*)))
    (LET ((PLINE (RANGE (IF *NUMERIC-ARG-P*
			    (+ *NUMERIC-ARG*
			       (IF (MINUSP *NUMERIC-ARG*) N-PLINES 0))
			    (FIX (* *CENTER-FRACTION* N-PLINES)))
			0 N-PLINES)))
      (LET ((LINE (PLINE-LINE *WINDOW* PLINE)))
	(COND ((NOT (NULL LINE))
	       (MOVE-BP (POINT) LINE (PLINE-FROM-INDEX *WINDOW* PLINE)))
	      ((OR (NOT *NUMERIC-ARG-P*) (MINUSP *NUMERIC-ARG*))
	       (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*)))
	      (T (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*)))))))
  DIS-BPS)

(DEFCOM COM-GOTO-BEGINNING "Go to beginning of buffer.
With an argument from 0 to 10, goes that many tenths of the length of the buffer
down from the beginning." (KM PUSH)
  (COND ((NOT *NUMERIC-ARG-P*)
	 (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*)))
	(T (MOVE-FRACTIONALLY *NUMERIC-ARG*)))
  DIS-BPS)

(DEFCOM COM-GOTO-END "Go to the end of the buffer.
With an argument from 0 to 10, goes that many tenths of the length of the buffer
from the end." (KM PUSH)
  (COND ((NOT *NUMERIC-ARG-P*)
	 (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*)))
	(T (MOVE-FRACTIONALLY (- 10. *NUMERIC-ARG*))))
  DIS-BPS)

(DEFUN MOVE-FRACTIONALLY (TENTHS)
  (COND ((OR (> TENTHS 10.)
	     (< TENTHS 0))
	 (BARF "The argument must be between 0 and 10."))
	(T
	 (MOVE-BP (POINT)
	  (FORWARD-LINE
	   (INTERVAL-FIRST-BP *INTERVAL*)
	   (// (* (COUNT-LINES *INTERVAL*) TENTHS) 10.)
	   T)))))

(DEFCOM COM-MARK-BEGINNING "Put the mark at the beginning of the buffer." (SM)
  (MOVE-BP (MARK) (INTERVAL-FIRST-BP *INTERVAL*))
  DIS-BPS)

(DEFCOM COM-MARK-END "Put the mark at the end of the buffer." (SM)
  (MOVE-BP (MARK) (INTERVAL-LAST-BP *INTERVAL*))
  DIS-BPS)

(DEFCOM COM-SWAP-POINT-AND-MARK "Exchange point and the mark." (SM)
  (OR (EQ (BP-INTERVAL (POINT)) (BP-INTERVAL (MARK)))
      (BARF "Point and mark not in same buffer"))
  (SWAP-BPS (POINT) (MARK))
  DIS-BPS)

(DEFCOM COM-SET-POP-MARK "Sets or pops the mark.
With no U's, sets the mark at the point, and pushes point onto the point pdl.
With one U, pops the point pdl.
With two U's, pops the point pdl and throws it away" (KM)
  (COND (( *NUMERIC-ARG* 3)
	 (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
	 (MOVE-BP (MARK) (POINT))
	 (SETF (WINDOW-MARK-P *WINDOW*) T)
	 DIS-BPS)
	(( *NUMERIC-ARG* 17)
	 (MULTIPLE-VALUE-BIND (BP PLINE)
	     (POINT-PDL-POP *WINDOW*)
	   (POINT-PDL-MOVE BP PLINE))
	 DIS-BPS)
	(T
	 (POINT-PDL-POP *WINDOW*)
	 DIS-NONE)))

(DEFCOM COM-PUSH-POP-POINT-EXPLICIT "Push or pop point onto the point pdl.
With no argument, push point onto the point pdl.
With an argument, exchanges point with the nth position on the stack." (KM)
  (COND ((NOT *NUMERIC-ARG-P*)
	 (POINT-PDL-PUSH (POINT) *WINDOW* T NIL)
	 DIS-NONE)
	(T
	 (MULTIPLE-VALUE-BIND (BP PLINE)
	      (POINT-PDL-EXCH (POINT) *WINDOW* *NUMERIC-ARG-P* *NUMERIC-ARG*)
	   (POINT-PDL-MOVE BP PLINE))
	 DIS-BPS)))

(DEFCOM COM-MOVE-TO-PREVIOUS-POINT "Exchange point and top of point pdl.
A numeric argument rotates top arg entries of the point pdl (the default
numeric argument is 2).  An argument of 1 rotates the whole point pdl
and a negative argument rotates the other way." ()
  (ROTATE-POINT-PDL *WINDOW* (IF *NUMERIC-ARG-P* *NUMERIC-ARG* 2)))

(DEFVAR *DEFAULT-PREVIOUS-POINT-ARG* 3)
(DEFCOM COM-MOVE-TO-DEFAULT-PREVIOUS-POINT "Rotate the point pdl.
A numeric argument specifies the number of entries to rotate, and sets the new default." ()
  (AND *NUMERIC-ARG-P*
       (SETQ *DEFAULT-PREVIOUS-POINT-ARG* *NUMERIC-ARG*))
  (ROTATE-POINT-PDL *WINDOW* *DEFAULT-PREVIOUS-POINT-ARG*))

(DEFCOM COM-INSERT-CRS "Insert one or more newlines into the buffer." ()
  (LET ((POINT (POINT)))
    (LET ((NEXT-LINE (LINE-NEXT (BP-LINE POINT))))
       (COND ((AND (= (BP-INDEX POINT) (LINE-LENGTH (BP-LINE POINT)))
		   (NOT *NUMERIC-ARG-P*)
		   (NEQ (BP-LINE POINT) (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
		   (LINE-BLANK-P NEXT-LINE)
		   (OR (EQ NEXT-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
		       (LINE-BLANK-P (LINE-NEXT NEXT-LINE))))
	      (DELETE-INTERVAL (BEG-OF-LINE NEXT-LINE) (END-OF-LINE NEXT-LINE))
	      (MOVE-BP POINT (BEG-OF-LINE NEXT-LINE)))
	     (T
	      (SETQ *CURRENT-COMMAND-TYPE* 'INSERT-CR)
	      (DOTIMES (I *NUMERIC-ARG*)
		(INSERT-MOVING POINT #\CR))))))
  DIS-TEXT)

(DEFCOM COM-MAKE-ROOM "Insert one or more blank lines after point." ()
  (DOTIMES (I *NUMERIC-ARG*)
     (INSERT (POINT) #\CR))
  DIS-TEXT)

(DEFCOM COM-SPLIT-LINE "Move rest of current line down vertically.
Inserts a carriage-return and updates indentation of the new line to be below the
old position." ()
  (LET ((POINT (POINT)))
    (MOVE-BP POINT (FORWARD-OVER *BLANKS* POINT))
    (LET ((IND (BP-INDENTATION POINT))
	  (BP (COPY-BP POINT)))
      (DOTIMES (I (MAX *NUMERIC-ARG* 1))
	(INSERT-MOVING BP #\CR))
      (INDENT-LINE BP IND)))
  DIS-TEXT)

(DEFCOM COM-THIS-INDENTATION "Indent a new line to this point.
With arg of 0, indent this line to here.
With positive arg, make a new line indented like this one." ()
  (LET ((BP1 (FORWARD-OVER *BLANKS* (IF (OR (NOT *NUMERIC-ARG-P*) (ZEROP *NUMERIC-ARG*))
					(POINT) (BEG-LINE (POINT)))))
	(BP2 (IF (ZEROP *NUMERIC-ARG*) (POINT) (INSERT-MOVING (END-LINE (POINT)) #\CR))))
    (MOVE-BP (POINT) (INDENT-LINE BP2 (BP-INDENTATION BP1))))
  DIS-TEXT)

(DEFCOM COM-DELETE-INDENTATION "Delete CRLF and any indentation at front of line.
Leaves a space in place of them where appropriate.  A numeric argument means move
down a line first (killing the end of the current line)." ()
  (LET ((POINT (POINT)))
    (LET ((LINE (BP-LINE POINT)))
       (COND ((AND *NUMERIC-ARG-P*
		   (NOT (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))))
	      (SETQ LINE (LINE-NEXT LINE))))
       (MOVE-BP POINT LINE 0)
       (COND ((NOT (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
	      (DELETE-INTERVAL (END-OF-LINE (LINE-PREVIOUS LINE)) POINT)))
       (DELETE-AROUND *BLANKS* POINT)
       (LET ((SYNTAX-BEFORE (LIST-SYNTAX (BP-CHAR-BEFORE POINT))))
	 (OR (= (LIST-SYNTAX (BP-CHAR POINT)) LIST-CLOSE)
	     (= SYNTAX-BEFORE LIST-OPEN)
	     (= SYNTAX-BEFORE LIST-SINGLE-QUOTE)
	     (INSERT-MOVING POINT (IN-CURRENT-FONT #\SP))))))
  DIS-TEXT)

(DEFCOM COM-DELETE-FORWARD "Delete one or more characters forward." ()
  (LET ((POINT (POINT)))
    (LET ((BP (FORWARD-CHAR POINT *NUMERIC-ARG* T)))
      (COND ((EQ (BP-LINE POINT) (BP-LINE BP))
	     (MUST-REDISPLAY *WINDOW*
			     DIS-LINE
			     (BP-LINE BP)
			     (MIN (BP-INDEX BP) (BP-INDEX POINT))))
	    (T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
      (DELETE-INTERVAL BP POINT)))
  DIS-NONE)

(DEFCOM COM-RUBOUT "Delete one or more characters backward." ()
  (LET ((POINT (POINT)))
    (LET ((BP (FORWARD-CHAR POINT (- *NUMERIC-ARG*) T)))
      (COND ((EQ (BP-LINE POINT) (BP-LINE BP))
	     (MUST-REDISPLAY *WINDOW*
			     DIS-LINE
			     (BP-LINE BP)
			     (MIN (BP-INDEX BP) (BP-INDEX POINT))))
	    (T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
      (DELETE-INTERVAL BP POINT)))
  DIS-NONE)

(DEFCOM COM-KILL-LINE "Kill to end of line, or kill an end of line.
Before a CRLF, delete the blank line, otherwise clear the line.
With a numeric argument, always kills the specified number of lines." ()
  (LET ((POINT (POINT)))
    (COND ((AND (BP-= POINT (INTERVAL-LAST-BP *INTERVAL*)) (PLUSP *NUMERIC-ARG*))
	   (BARF "Attempt to kill past the end of the buffer."))
	  (T
	   (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
	   (COND (*NUMERIC-ARG-P*
		  (KILL-INTERVAL-ARG POINT
				     (BEG-LINE POINT *NUMERIC-ARG* T)
				     *NUMERIC-ARG*)
		  DIS-TEXT)
		 ((END-LINE-P (FORWARD-OVER *BLANKS* POINT))
		  (KILL-INTERVAL POINT (BEG-LINE POINT 1 T) T T)
		  DIS-TEXT)
		 (T
		  (KILL-INTERVAL POINT (END-LINE POINT) T T)
		  (MVRETURN DIS-LINE (BP-LINE POINT) (BP-INDEX POINT))))))))

(DEFCOM COM-CLEAR "Kill to the start of the current line." ()
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (LET ((POINT (POINT)))
    (LET ((BP (BEG-LINE POINT (COND (*NUMERIC-ARG-P* (- *NUMERIC-ARG*))
				    ((BEG-LINE-P POINT) -1)
				    (T 0)) T)))
      (KILL-INTERVAL BP POINT NIL NIL)))
  DIS-TEXT)

(DEFCOM COM-SAVE-REGION "Put region on kill-ring without deleting it." ()
  (REGION (BP1 BP2)
    (KILL-RING-SAVE-INTERVAL BP1 BP2 T))
  DIS-NONE)

(DEFCOM COM-KILL-REGION "Kill from point to mark.
Killed text is placed on the kill-ring for retrieval" ()
  (AND (EQ *LAST-COMMAND-TYPE* 'YANK)			;By special case.
       (SETF (WINDOW-MARK-P *WINDOW*) T))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (REGION (BP1 BP2)
     (KILL-INTERVAL BP1 BP2 T T))
  (CLEAN-POINT-PDL *WINDOW*)
  (LET ((PDL (WINDOW-POINT-PDL *WINDOW*)))
    (AND PDL (MOVE-BP (MARK) (CAAR PDL))))
  DIS-TEXT)

(DEFCOM COM-APPEND-NEXT-KILL "Make next kill command append text to previous one." (KM)
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-NONE)

(DEFCOM COM-YANK "Re-insert the last stuff killed.
Leaves point and mark around what is inserted.  A numeric argument means use the
n'th most recent kill from the ring." ()
  (OR *KILL-RING* (BARF))
  (LET ((ARG (IF (EQ *NUMERIC-ARG-P* ':CONTROL-U) 0 (1- *NUMERIC-ARG*))))
    (AND ( ARG (LENGTH *KILL-RING*)) (BARF))
    (SETQ *CURRENT-COMMAND-TYPE* 'YANK)
    (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
    (LET ((BP (INSERT-THING (POINT) (NTH ARG *KILL-RING*))))
      (COND ((EQ *NUMERIC-ARG-P* ':CONTROL-U)
	     (MOVE-BP (MARK) BP))
	    (T
	     (MOVE-BP (MARK) (POINT))
	     (MOVE-BP (POINT) BP)))))
  DIS-TEXT)

(DEFCOM COM-YANK-POP "Correct a Yank to use a previous kill.
Deletes between point and the mark and then inserts the previous kill from the
kill-ring, which is pulled to the top, so that successive attempts cycle through
the whole ring." ()
  ;; Need not check for MARK-P, by special case.
  (OR (EQ *LAST-COMMAND-TYPE* 'YANK) (BARF))
  (SETQ *CURRENT-COMMAND-TYPE* 'YANK)
  (DELETE-INTERVAL (POINT) (MARK))
  (OR (ZEROP *NUMERIC-ARG*)
      (MOVE-BP (POINT) (INSERT-THING (POINT) (KILL-RING-POP (1- *NUMERIC-ARG*)))))
  DIS-TEXT)

;;; If there was no arg at all, *NUMERIC-ARG-P* is NIL and *NUMERIC-ARG* is 1.
;;; If user just typed -, then *NUMERIC-ARG-P* is :SIGN and *NUMERIC-ARG* is -1.
;;; If numeric arg commands were typed, *NUMERIC-ARG-P* is :DIGITS and
;;; *NUMERIC-ARG* is the number.
;;; Note that - does not toggle negativeness, it turns it on.

(DEFCOM COM-QUADRUPLE-NUMERIC-ARG "Multiply the next command's numeric argument by 4." ()
  (SETQ *NUMERIC-ARG* (* *NUMERIC-ARG* 4)
	*NUMERIC-ARG-P* ':CONTROL-U)
  ':ARGUMENT)

(DEFCOM COM-NUMBERS "part of the next command's numeric argument." ()
  (LET ((FLAG NIL)
	(DIGIT (- (LDB %%KBD-CHAR *LAST-COMMAND-CHAR*) #/0)))
    (COND ((< *NUMERIC-ARG* 0)
	   (SETQ FLAG T)
	   (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*))))
    (SETQ *NUMERIC-ARG*
	  (IF (EQ *NUMERIC-ARG-P* ':DIGITS)
	      (+ (* 10. *NUMERIC-ARG*) DIGIT)
	      DIGIT))
    (AND FLAG (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*))))
  (SETQ *NUMERIC-ARG-P* ':DIGITS)
  ':ARGUMENT)

(DEFCOM COM-NEGATE-NUMERIC-ARG "Negate the next command's numeric argument." ()
  (SETQ *NUMERIC-ARG* (MINUS (ABS *NUMERIC-ARG*))
	*NUMERIC-ARG-P* ':SIGN)
  ':ARGUMENT)

(DEFCOM COM-SIMPLE-EXCHANGE-CHARACTERS
	"Interchange the characters before and after the cursor.
With a positive argument it interchanges the characters before and
after the cursor, moves right, and repeats the specified number of
times, dragging the character to the left of the cursor right.  With a
negative argument, it interchanges the two characters to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the characters at point and mark." ()
  (EXCHANGE-SUBR 'FORWARD-CHAR *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-CHARACTERS "Interchange the characters before and after the cursor.
With a positive argument it interchanges the characters before and
after the cursor, moves right, and repeats the specified number of
times, dragging the character to the left of the cursor right.  With a
negative argument, it interchanges the two characters to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the characters at point and mark.
No argument is like an argument of 1, except at the end of a line
the previous two characters are interchanged." ()
  (COND ((AND (NOT *NUMERIC-ARG-P*)
	      (= (BP-CHAR (POINT)) #\CR))
	 (MOVE-BP (POINT) (OR (FORWARD-CHAR (POINT) -1) (BARF)))))
  (EXCHANGE-SUBR 'FORWARD-CHAR *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-WORDS "Interchange the words before and after the cursor.
With a positive argument it interchanges the words before and
after the cursor, moves right, and repeats the specified number of
times, dragging the word to the left of the cursor right.  With a
negative argument, it interchanges the two words to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the words at point and mark." ()
  (EXCHANGE-SUBR 'FORWARD-WORD *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-LINES "Interchange the lines before and after the cursor.
With a positive argument it interchanges the lines before and
after the cursor, moves right, and repeats the specified number of
times, dragging the word to the left of the cursor right.  With a
negative argument, it interchanges the two lines to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the lines at point and mark." ()
  (EXCHANGE-SUBR 'FORWARD-LINE *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-SEXPS "Interchange the S-expressions before and after the cursor.
With a positive argument it interchanges the S-expressions before and
after the cursor, moves right, and repeats the specified number of
times, dragging the S-expression to the left of the cursor right.  With a
negative argument, it interchanges the two S-expressions to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the S-expressions at point and mark." ()
  (EXCHANGE-SUBR 'FORWARD-SEXP *NUMERIC-ARG*)
  DIS-TEXT)

;;;This is arranged so weirdly because it runs out of local variables as just one function.
(DEFUN EXCHANGE-SUBR (FN N &AUX BUF1 BUF2)
  (COND ((PLUSP N)
	 (EXCHANGE-SUBR-1 FN N))
	((MINUSP N)
	 (EXCHANGE-SUBR-2 FN N))
	(T
	 (REGION (BP1 BP2)
	   (WITH-BP (BP1 (OR (FUNCALL FN BP1 1) (BARF)) ':NORMAL)
	     (OR (SETQ BP1 (FUNCALL FN BP1 -1)) (BARF))
	     (WITH-BP (BP2 (OR (FUNCALL FN BP2 1) (BARF)) ':NORMAL)
	       (WITH-BP (BP3 (OR (FUNCALL FN BP2 -1) (BARF)) ':NORMAL)
		 (WITH-BP (BP4 (OR (FUNCALL FN BP1 1) (BARF)) ':NORMAL)
			  (SETQ BUF1 (COPY-INTERVAL BP3 BP2 T)
				BUF2 (COPY-INTERVAL BP1 BP4 T))
			  (DELETE-INTERVAL BP3 BP2 T)
			  (MOVE-BP (POINT) (INSERT-INTERVAL BP3 BUF2))
			  (MOVE-BP (MARK) (INSERT-INTERVAL BP4 BUF1))
			  (DELETE-INTERVAL BP1 BP4 T))))))
	   (SETQ *MARK-STAYS* T))))

(DEFUN EXCHANGE-SUBR-1 (FN N &AUX BP1 BUF1 BUF2)
  (OR (SETQ BP1 (FUNCALL FN (POINT) 1)) (BARF))
  (OR (SETQ BP1 (FUNCALL FN BP1 -2)) (BARF))
  (OR (SETQ BP1 (FUNCALL FN BP1 1)) (BARF))
  (MOVE-BP (POINT) BP1)
  (DOTIMES (I N)
    (WITH-BP (BP1 (POINT) ':NORMAL)
      (WITH-BP (BP2 (OR (FUNCALL FN BP1 1) (BARF)) ':NORMAL)
	(WITH-BP (BP3 (OR (FUNCALL FN BP2 -1) (BARF)) ':NORMAL)
	  (WITH-BP (BP4 (OR (FUNCALL FN BP1 -1) (BARF)) ':NORMAL)
		   (SETQ BUF1 (COPY-INTERVAL BP3 BP2 T)
			 BUF2 (COPY-INTERVAL BP4 BP1 T))
		   (DELETE-INTERVAL BP3 BP2 T)
		   (MOVE-BP (POINT) (INSERT-INTERVAL BP3 BUF2))
		   (INSERT-INTERVAL BP1 BUF1)
		   (DELETE-INTERVAL BP4 BP1 T)))))))

(DEFUN EXCHANGE-SUBR-2 (FN N &AUX BP1 BUF1 BUF2)
  (OR (SETQ BP1 (FUNCALL FN (POINT) -1)) (BARF))
  (OR (SETQ BP1 (FUNCALL FN BP1 1)) (BARF))
  (MOVE-BP (POINT) BP1)
  (DO I 0 (1- I) ( I N)
      (WITH-BP (BP1 (POINT) ':NORMAL)
	(WITH-BP (BP2 (OR (FUNCALL FN BP1 -2) (BARF)) ':NORMAL)
	  (WITH-BP (BP3 (OR (FUNCALL FN BP2 1) (BARF)) ':NORMAL)
	    (WITH-BP (BP4 (OR (FUNCALL FN BP1 -1) (BARF)) ':NORMAL)
	      (SETQ BUF1 (COPY-INTERVAL BP2 BP3 T)
		    BUF2 (COPY-INTERVAL BP4 BP1 T))
	      (DELETE-INTERVAL BP4 BP1 T)
	      (INSERT-INTERVAL BP4 BUF1)
	      (MOVE-BP (POINT) (INSERT-INTERVAL BP3 BUF2))
	      (DELETE-INTERVAL BP2 BP3 T)))))))

(DEFCOM COM-EXCHANGE-REGIONS "Exchange region delimited by point and last three marks." (KM)
  (OR (WINDOW-MARK-P *WINDOW*) (BARF "There is no region"))	;Avoid accidental lossage
  (LET ((POINT (POINT)) (MARK (MARK))
	BP1 BP2 BP3 BP4)
    (OR (BP-= MARK (CAAR (WINDOW-POINT-PDL *WINDOW*)))
	(BARF "Mark not at the same place as top of point pdl"))
    (SETQ BP1 POINT
	  BP2 (POINT-PDL-POP *WINDOW*)
	  BP3 (POINT-PDL-POP *WINDOW*)
	  BP4 (POINT-PDL-POP *WINDOW*))
    (LET ((LIST (LIST BP1 BP2 BP3 BP4)))
      (SETQ LIST (SORT LIST #'(LAMBDA (BP1 BP2)
				(AND (EQ (BP-INTERVAL BP1) (BP-INTERVAL BP2))
				     (BP-< BP1 BP2)))))
      (SETQ BP1 (FIRST LIST)
	    BP2 (SECOND LIST)
	    BP3 (THIRD LIST)
	    BP4 (FOURTH LIST)))
    (OR (AND (EQ (BP-INTERVAL BP1) (BP-INTERVAL BP2))
	     (EQ (BP-INTERVAL BP3) (BP-INTERVAL BP4)))
	(BARF "Regions are not both within single buffers"))
    (WITH-BP (NBP2 (INSERT-INTERVAL BP2 BP3 BP4 T) ':NORMAL)
      (WITH-BP (NBP4 (INSERT-INTERVAL BP4 BP1 BP2 T) ':NORMAL)
	(DELETE-INTERVAL BP1 BP2 T)
	(DELETE-INTERVAL BP3 BP4 T)
	(POINT-PDL-PUSH BP1 *WINDOW*)    
	(POINT-PDL-PUSH NBP2 *WINDOW*)
	(POINT-PDL-PUSH BP3 *WINDOW*)
	(MOVE-BP MARK BP3)
	(MOVE-BP POINT NBP4))))
  DIS-TEXT)

(DEFUN REVERSE-SUBR (FN N &AUX (POINT (POINT)) BP-LIST)
  (AND (MINUSP N)
       (SETQ POINT (FUNCALL FN POINT N)
	     N (- N)))
  (UNWIND-PROTECT
    (PROGN
      (DO ((I 0 (1+ I))
	   (START-BP POINT END-BP)
	   (END-BP))
	  (( I N)
	   (UNDO-SAVE POINT END-BP T "Reverse"))
	(SETQ END-BP (OR (FUNCALL FN START-BP 1) (BARF))
	      START-BP (OR (FUNCALL FN END-BP -1) (BARF)))
	(PUSH (LIST (COPY-BP START-BP ':MOVES) (COPY-BP END-BP ':NORMAL)) BP-LIST))
      (DO ((I 0 (1+ I))
	   (N (// N 2))
	   (LIST-FROM-THE-RIGHT BP-LIST (CDR LIST-FROM-THE-RIGHT))
	   (LIST-FROM-THE-LEFT (REVERSE BP-LIST) (CDR LIST-FROM-THE-LEFT))
	   (RIGHT-START-BP) (RIGHT-END-BP)
	   (LEFT-START-BP) (LEFT-END-BP))
	  (( I N))
	(SETQ LEFT-START-BP (CAAR LIST-FROM-THE-LEFT)
	      LEFT-END-BP (CADAR LIST-FROM-THE-LEFT))
	(SETQ RIGHT-START-BP (CAAR LIST-FROM-THE-RIGHT)
	      RIGHT-END-BP (CADAR LIST-FROM-THE-RIGHT))
	(INSERT-INTERVAL LEFT-START-BP RIGHT-START-BP RIGHT-END-BP T)
	(DELETE-INTERVAL RIGHT-START-BP RIGHT-END-BP T)
	(INSERT-INTERVAL RIGHT-START-BP LEFT-START-BP LEFT-END-BP T)
	(DELETE-INTERVAL LEFT-START-BP LEFT-END-BP T)))
    (DO ((BPS BP-LIST (CDR BPS)))
	((NULL BPS))
      (FLUSH-BP (CAAR BPS))
      (FLUSH-BP (CADAR BPS)))))

(DEFCOM COM-REVERSE-LINES "Reverse the order of the specified number of lines" ()
  (REVERSE-SUBR 'FORWARD-LINE *NUMERIC-ARG*)
  DIS-TEXT)

(DEFUN KILL-COMMAND-INTERNAL (FUNCTION ARG &AUX (POINT (POINT)))
  (KILL-INTERVAL-ARG POINT
		     (OR (FUNCALL FUNCTION POINT ARG) (BARF))
		     ARG)
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (MOVE-BP (MARK) POINT)
  DIS-TEXT)

(DEFCOM COM-FORWARD-WORD "Move one or more words forward." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-WORD (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-WORD "Move one or more words backward." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-WORD (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-KILL-WORD "Kill one or more words forward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-WORD *NUMERIC-ARG*))

(DEFCOM COM-BACKWARD-KILL-WORD "Kill one or more words backward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-WORD (- *NUMERIC-ARG*)))

(DEFCOM COM-MARK-WORD "Set mark one or more words from point." (SM)
  (MOVE-BP (MARK) (OR (FORWARD-WORD (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-SEXP "Move one or more s-expressions forward." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-SEXP (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-SEXP-NO-UP "Move forward one or more s-expressions,
but never over an unbalanced ).  Useful in keyboard macros, e.g." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-SEXP (POINT) *NUMERIC-ARG* NIL 0 NIL T T) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-SEXP-NO-UP "Move backward one or more s-expressions,
but never over an unbalanced (.  Useful in keyboard macros, e.g." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-SEXP (POINT) (- *NUMERIC-ARG*) NIL 0 NIL T T) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-LIST "Move one or more lists forward." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-LIST (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-SEXP "Move one or more s-expressions backward." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-SEXP (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-LIST "Move one or more lists backwards." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-LIST (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-KILL-SEXP "Kill one or more s-expressions forward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP *NUMERIC-ARG*))

(DEFCOM COM-KILL-SEXP-NO-UP "Kill one or more s-expressions forward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP-NO-UP *NUMERIC-ARG*))

(DEFCOM COM-BACKWARD-KILL-SEXP "Kill one or more s-expressions backward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP (- *NUMERIC-ARG*)))

(DEFCOM COM-BACKWARD-KILL-SEXP-NO-UP "Kill one or more s-expressions backward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP-NO-UP (- *NUMERIC-ARG*)))

(DEFCOM COM-MARK-SEXP "Set mark one or more s-expressions from point." (SM)
  (MOVE-BP (MARK) (OR (FORWARD-SEXP (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-UP-LIST "Move up one level of list structure, forward.
Also, if called inside of a string, moves up out of that string." (KM)
  (LET ((BP (IF (LISP-BP-SYNTACTIC-CONTEXT (POINT))
		(FORWARD-UP-STRING (POINT) (MINUSP *NUMERIC-ARG*))
		(FORWARD-SEXP (POINT) *NUMERIC-ARG* NIL 1))))
    (OR BP (BARF))
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-BACKWARD-UP-LIST "Move up one level of list structure, backward.
Also, if called inside of a string, moves back up out of that string." (KM)
  (LET ((BP (IF (LISP-BP-SYNTACTIC-CONTEXT (POINT))
		(FORWARD-UP-STRING (POINT) (NOT (MINUSP *NUMERIC-ARG*)))
		(FORWARD-SEXP (POINT) (- *NUMERIC-ARG*) NIL 1))))
    (OR BP (BARF))
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-BEGINNING-OF-DEFUN "Go to the beginning of the current defun." (KM)
  (LET ((BP (OR (FORWARD-DEFUN (POINT) (- *NUMERIC-ARG*)) (BARF))))
    (POINT-PDL-PUSH (POINT) *WINDOW*)
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-END-OF-DEFUN "Go to the end of the current defun." (KM)
  (LET ((BP (FORWARD-DEFUN (POINT) -1 T)))		;Go to front of defun.
    (OR (SETQ BP (FORWARD-LIST BP)) (BARF))		; and forward over it.
    (SETQ BP (BEG-LINE BP 1 T))
    (COND ((OR (BP-< BP (POINT))                      ;If we were between defuns,
	       (AND (PLUSP *NUMERIC-ARG*) (BP-= BP (POINT))))
	   (SETQ BP (END-LINE BP -1 T))
	   (OR (SETQ BP (FORWARD-LIST (FORWARD-DEFUN BP 1 T)))
	       (BARF))
	   (SETQ BP (BEG-LINE BP 1 T))))              ; then move ahead another.
    (POINT-PDL-PUSH (POINT) *WINDOW*)
    (OR (= *NUMERIC-ARG* 1)
	(SETQ BP (BEG-LINE (FORWARD-LIST (FORWARD-DEFUN BP (1- *NUMERIC-ARG*) T) 1 T) 1 T)))
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-DOWN-LIST "Move down one or more levels of list structure." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-LIST (POINT) 1 NIL (- *NUMERIC-ARG*) T) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-DOWN-LIST
	"Move down one or more levels of list structure, backward." (KM)
  (MOVE-BP (POINT)
	   (OR (FORWARD-LIST (POINT) -1 NIL (- *NUMERIC-ARG*) T T) (BARF)))
  DIS-BPS)
