;;; Zwei commands, see ZWEI;COMA for comments -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCOM COM-MARK-PAGE "Put point at top of page, mark at end.
A numeric arg specifies the page: 0 for current, 1 for next,
-1 for previous, larger numbers to move many pages." (SM)
  (MULTIPLE-VALUE-BIND (BP1 BP2)
      (MARK-PAGE-INTERNAL (POINT) (IF *NUMERIC-ARG-P* *NUMERIC-ARG* 0))
    (MOVE-BP (POINT) BP1)
    (MOVE-BP (MARK) BP2))
  DIS-BPS)

;;; For COM-MARK-PAGE and COM-SET-BOUNDS-PAGE (latter not yet written).
(DEFUN MARK-PAGE-INTERNAL (BP ARG)
  (COND ((AND (ZEROP ARG) (LOOKING-AT-BACKWARD BP #\FF)))
	(( ARG 0)
	 (DOTIMES (I (1+ (MINUS ARG)))
	   (SETQ BP (FORWARD-PAGE BP -1 T))))
	(T
	 (DOTIMES (I ARG)
	   (SETQ BP (FORWARD-PAGE BP 1 T)))))
  (MVRETURN BP (FORWARD-PAGE BP)))

;;; Make this a variable just in case someone wants to modify it
(DEFVAR *MATCHING-DELIMITER-LIST*
	'((#/( #/) FORWARD-SEXP) (#/" #/" FORWARD-WORD) (#/[ #/] FORWARD-SEXP)
	  (#/{ #/} FORWARD-SEXP) (#/< #/> FORWARD-WORD) (#/* #/* FORWARD-WORD)
	  (#/ #/ FORWARD-WORD)))

(DEFCOM COM-MAKE-/(/) "Insert matching delimiters, putting point between them.
With an argument, puts that many s-exprs within the new ()." ()
  (LET ((OPEN #/() (CLOSE #/))
	(MOVER 'FORWARD-SEXP) (POINT (POINT)))
    (DO ((CH (LDB %%CH-CHAR *LAST-COMMAND-CHAR*))
	 (L *MATCHING-DELIMITER-LIST* (CDR L)))
	((NULL L))
      (COND ((OR (= CH (CAAR L)) (= CH (CADAR L)))
	     (SETQ OPEN (CAAR L) CLOSE (CADAR L) MOVER (CADDAR L))
	     (RETURN T))))
    (LET ((BP (IF *NUMERIC-ARG-P*
		  (OR (IF (EQ MOVER 'FORWARD-SEXP)
			  (FORWARD-SEXP POINT *NUMERIC-ARG* NIL 0 NIL T T)	;No UP
			  (FUNCALL MOVER POINT *NUMERIC-ARG*))
		      (BARF))
		  POINT)))
      (AND (MINUSP *NUMERIC-ARG*) (PSETQ BP POINT POINT BP))
      (INSERT BP (IN-CURRENT-FONT CLOSE))
      (INSERT-MOVING POINT (IN-CURRENT-FONT OPEN))
      DIS-TEXT)))

(DEFCOM COM-MAKE-/(/)-BACKWARD "Insert matching delimiters backwards." ()
  (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*) *NUMERIC-ARG-P* T)
  (COM-MAKE-/(/)))

(DEFCOM COM-DELETE-/(/) "Delete both of the nth innermost pair of parens enclosing point." ()
  (LET ((POINT (POINT)))
    (LET ((BP1 (OR (FORWARD-LIST POINT *NUMERIC-ARG* NIL 1) (BARF)))
	  (BP2 (OR (FORWARD-LIST POINT (- *NUMERIC-ARG*) NIL 1) (BARF))))
      (DELETE-INTERVAL (FORWARD-CHAR BP1 -1) BP1)
      (DELETE-INTERVAL BP2 (FORWARD-CHAR BP2 1))
      DIS-TEXT)))

(DEFCOM COM-MOVE-OVER-/) "Moves over the next ), updating indentation.
Any indentation before the ) is deleted.
LISP-style indentation is inserted after the )." ()
  (LET ((POINT (POINT)) (CHAR NIL))
    (DO ((CH (LDB %%CH-CHAR *LAST-COMMAND-CHAR*))
	 (L *MATCHING-DELIMITER-LIST* (CDR L)))
	((NULL L))
      (COND ((= CH (CADAR L))
	     (OR (= (LIST-SYNTAX CH) LIST-CLOSE)
		 (SETQ CHAR CH))
	     (RETURN T))))
    (LET ((BP (OR (IF CHAR (SEARCH (POINT) CHAR) (FORWARD-LIST POINT 1 NIL 1)) (BARF))))
      (MOVE-BP (POINT) BP)
      (DELETE-BACKWARD-OVER *WHITESPACE-CHARS* (FORWARD-CHAR BP -1))
      (LET ((ARG (1- *NUMERIC-ARG*)))
	(AND (> ARG 0)
	     (MOVE-BP (POINT) (OR (IF CHAR (SEARCH (POINT) CHAR ARG)
				      (FORWARD-LIST POINT ARG NIL 1))
				  (BARF)))))))
  (COM-INSERT-CRS)
  (COM-INDENT-FOR-LISP)
  DIS-TEXT)

(DEFCOM COM-GROW-LIST-FORWARD
"Move the closing delimiter of the current list forward over one or more sexps.
With negative arg, shrink list by moving closing delimiter backwards.
Marks the end of the resulting list for visibility.
Always leaves point where the same command with a negative arg will undo it." (RM)
  (LET ((OLD-END (OR (FORWARD-LIST (POINT) 1 NIL 1) (BARF)))
	(POINT (POINT)) OLD-END-1)
    (SETQ OLD-END-1 (FORWARD-CHAR OLD-END -1))
    (LET ((NEW-END (OR (FORWARD-SEXP (IF (MINUSP *NUMERIC-ARG*) OLD-END-1 OLD-END)
				     *NUMERIC-ARG* NIL 0 NIL T T)
		       (BARF))))
      (AND (MINUSP *NUMERIC-ARG*)
	   (SETQ NEW-END (BACKWARD-OVER *WHITESPACE-CHARS* NEW-END)))
      (LET ((CHAR (BP-CHAR-BEFORE OLD-END)))
	(WITH-BP (BP NEW-END ':NORMAL)
	  (DELETE-INTERVAL OLD-END-1 OLD-END T)
	  (INSERT BP CHAR)
	  (COND ((BP-< NEW-END POINT)
		 (MOVE-BP POINT NEW-END))
		(T
		 (MOVE-BP (MARK) (FORWARD-CHAR BP 1 T))
		 (SETF (WINDOW-MARK-P *WINDOW*) T)
		 (SETQ *MARK-STAYS* T)))))))
  DIS-TEXT)

(DEFCOM COM-GROW-LIST-BACKWARD
"Move the opening delimiter of the current list backward over one or more sexps.
With negative arg, shrink list by moving opening delimiter forwards.
Marks the beginning of the resulting list for visibility.
Always leaves point where the same command with a negative arg will undo it." (RM)
  (LET ((OLD-BEGIN (OR (FORWARD-LIST (POINT) -1 NIL 1) (BARF)))
	(POINT (POINT)) OLD-BEGIN+1)
    (SETQ OLD-BEGIN+1 (FORWARD-CHAR OLD-BEGIN 1))
    (LET ((NEW-BEGIN (OR (FORWARD-SEXP (IF (MINUSP *NUMERIC-ARG*) OLD-BEGIN+1 OLD-BEGIN)
				       (- *NUMERIC-ARG*) NIL 0 NIL NIL T)
			 (BARF))))
      (AND (MINUSP *NUMERIC-ARG*)
	   (SETQ NEW-BEGIN (FORWARD-OVER *WHITESPACE-CHARS* NEW-BEGIN)))
      (LET ((CHAR (BP-CHAR OLD-BEGIN)))
	(WITH-BP (BP NEW-BEGIN ':MOVES)
	  (DELETE-INTERVAL OLD-BEGIN OLD-BEGIN+1 T)
	  (INSERT BP CHAR)
	  (COND ((BP-< POINT NEW-BEGIN)
		 (MOVE-BP POINT BP))
		(T
		 (MOVE-BP (MARK) (FORWARD-CHAR BP -1 T))
		 (SETF (WINDOW-MARK-P *WINDOW*) T)
		 (SETQ *MARK-STAYS* T)))))))
  DIS-TEXT)


(DEFCOM COM-KILL-BACKWARD-UP-LIST "Delete the list that contains the sexp after point,
but leave that sexp itself." ()
  (LET ((POINT (POINT))
	BP1 BP2 BP3)
    (OR (AND (SETQ BP1 (FORWARD-SEXP POINT -1 NIL 1 NIL NIL))
	     (SETQ BP2 (FORWARD-SEXP POINT *NUMERIC-ARG* NIL 0 NIL NIL T))
	     (SETQ BP3 (FORWARD-SEXP BP1 1)))
	(BARF))
    (UNDO-SAVE BP1 BP3 T "Kill up")
    (DELETE-INTERVAL BP2 BP3 T)
    (DELETE-INTERVAL BP1 POINT T))
  DIS-TEXT)

(DEFCOM COM-FORMAT-CODE "Grind the sexp after the pointer.
WARNING: This calls the Lisp grinder, and will delete comments!
A copy of the sexp is first saved on the kill ring." ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT)))
	(EOF '())
	(POINT (POINT))
	)
    (LET ((SEXP (READ STREAM EOF)))
      (AND (EQ SEXP EOF) (BARF "Missing close parentheses"))
      (UNDO-SAVE POINT (FUNCALL STREAM ':READ-BP) T "Grind")
      (GRIND-INTO-BP (DELETE-INTERVAL POINT (FUNCALL STREAM ':READ-BP)) SEXP)))
  DIS-TEXT)

(DEFCOM COM-FORWARD-PARAGRAPH "Move to start of next paragraph.
Paragraphs are delimited by blank lines or by lines which start with
a delimiter in *PARAGRAPH-DELIMITER-LIST* or in *PAGE-DELIMITER-LIST*.
If there is a fill prefix, any line that does not start with it starts
a paragraph.
Lines which start with a character in *TEXT-JUSTIFIER-ESCAPE-LIST*, if that
character is also in *PARAGRAPH-DELIMITER-LIST*, count as blank lines in
that they separate paragraphs and are not part of them." (KM)
  (MOVE-BP (POINT) (FORWARD-PARAGRAPH (POINT) *NUMERIC-ARG* T))
  DIS-BPS)

(DEFCOM COM-BACKWARD-PARAGRAPH "Move to start of this (or last) paragraph.
See Forward Paragraph for the definition of a paragraph." (KM)
  (MOVE-BP (POINT) (FORWARD-PARAGRAPH (POINT) (- *NUMERIC-ARG*) T))
  DIS-BPS)

(DEFCOM COM-MARK-PARAGRAPH "Set point and mark around current paragraph.
See Forward Paragraph for the definition of a paragraph." (SM)
  (LET ((INT (PARAGRAPH-INTERVAL (POINT) *NUMERIC-ARG*)))
    (MOVE-BP (POINT) (INTERVAL-FIRST-BP INT))
    (MOVE-BP (MARK) (INTERVAL-LAST-BP INT)))
  DIS-BPS)

(DEFCOM COM-FORWARD-SENTENCE "Move to end of this sentence.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." (KM)
  (MOVE-BP (POINT) (FORWARD-SENTENCE (POINT) *NUMERIC-ARG* T))
  DIS-BPS)

(DEFCOM COM-BACKWARD-SENTENCE "Move to beginning of sentence.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." (KM)
  (MOVE-BP (POINT) (FORWARD-SENTENCE (POINT) (- *NUMERIC-ARG*) T))
  DIS-BPS)

(DEFCOM COM-KILL-SENTENCE "Kill one or more sentences forward.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." ()
  (KILL-INTERVAL-ARG (POINT)
		     (FORWARD-SENTENCE (POINT) *NUMERIC-ARG* T)
		     *NUMERIC-ARG*)
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-TEXT)

(DEFCOM COM-BACKWARD-KILL-SENTENCE "Kill one or more sentences backward.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." ()
  (KILL-INTERVAL-ARG (POINT)
		     (FORWARD-SENTENCE (POINT) (- *NUMERIC-ARG*) T)
		     (- *NUMERIC-ARG*))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-TEXT)

;;; The standard c-G command.
(DEFCOM COM-BEEP "Beep, and if not given a numeric arg turn off the region." ()
  (AND (MEMQ ':MACRO-ERROR (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
       (FUNCALL STANDARD-INPUT ':MACRO-ERROR))
  (BEEP)
  (AND *NUMERIC-ARG-P*
       (SETQ *MARK-STAYS* T))
  DIS-NONE)

;;; The standard c-X c-G command.
(DEFCOM COM-PREFIX-BEEP "Beep and don't do anything else." (KM)
  (AND (MEMQ ':MACRO-ERROR (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
       (FUNCALL STANDARD-INPUT ':MACRO-ERROR))
  (BEEP)
  DIS-NONE)

(DEFCOM COM-INDENT-FOR-COMMENT "Move to or create comment.
Finds start of existing comments or creates one at end of current line.
With numeric argument, re-aligns existing comments for n lines, but does
not create any.
Note that unlike EMACS, all units are raster pixels, not character counts!
*COMMENT-COLUMN* is the minimum column for aligning comments.
*COMMENT-START* is the string used to recognize existing comments.
*COMMENT-BEGIN* is the string used to start new comments.
*COMMENT-ROUND-FUNCTION* is the function used to compute the column for comments past the
comment column." ()
  (MOVE-BP (POINT)
	   (INDENT-FOR-COMMENT (POINT) *NUMERIC-ARG* (NOT *NUMERIC-ARG-P*) *NUMERIC-ARG-P*))
  DIS-TEXT)

(DEFUN INDENT-FOR-COMMENT (BP &OPTIONAL (TIMES 1) CREATE-P MOVE-TO-NEXT-P &AUX (UP-P 1))
  (SETQ BP (COPY-BP BP ':MOVES))
  (AND (MINUSP TIMES)
       (SETQ UP-P -1 TIMES (MINUS TIMES)))
  (DO ((I 0 (1+ I))
       (LINE)
       (LEN)
       (CH)
       (START-START-INDEX)		;Index in line of start of existing comment starter.
       (START-END-INDEX))		;Index in line of end of ...
      (( I TIMES))
    (SETQ LINE (BP-LINE BP)
	  LEN (LINE-LENGTH LINE))
    (MULTIPLE-VALUE (START-START-INDEX START-END-INDEX)
      (FIND-COMMENT-START LINE T))
    (COND (START-START-INDEX
	   ;; A comment already exists.  Move BP to it.
	   (MOVE-BP BP LINE START-START-INDEX)
	   ;; Distinguish between ";", ";;" and ";;;" type comments.
	   (COND ((AND (> LEN (1+ START-START-INDEX))
		       (CHAR-EQUAL (AREF LINE (1+ START-START-INDEX))
				   (SETQ CH (AREF LINE START-START-INDEX))))
		  (COND ((OR ( LEN (+ START-START-INDEX 2))	; ";;;" doesnt move
			     (NOT (CHAR-EQUAL CH (AREF LINE (+ START-START-INDEX 2)))))
			 ;; It is a double semicolon, indent as code.
			 (INDENT-LINE BP (INDENT-FOR-LISP BP)))))
		 (T
		  (DELETE-BACKWARD-OVER *BLANKS* BP)
		  (INDENT-TO-COMMENT-COLUMN BP)))
	   ;; Now that indentation is adjusted, move over the comment starter.
	   (MOVE-BP BP (FORWARD-CHAR BP (- START-END-INDEX START-START-INDEX))))
	  (CREATE-P
	   ;; No existing comment, and no numeric arg, means make a comment.
	   (MOVE-BP BP LINE LEN)		; Move to end of line
	   (DELETE-BACKWARD-OVER *BLANKS* BP)
	   (INDENT-TO-COMMENT-COLUMN BP)
	   (INSERT BP *COMMENT-BEGIN*)))
    (AND MOVE-TO-NEXT-P				; Move to next line
	 (MOVE-BP BP (OR (BEG-LINE BP UP-P) (RETURN NIL)))))
  BP)

;; Internal function of above.
(DEFUN INDENT-TO-COMMENT-COLUMN (BP)
  (LET ((HERE (BP-INDENTATION BP))
	(GOAL *COMMENT-COLUMN*))
    (COND (( HERE GOAL)
	   (SETQ GOAL (FUNCALL *COMMENT-ROUND-FUNCTION* HERE))))
    (INDENT-TO BP GOAL)))

;;; This is the default value of *COMMENT-ROUND-FUNCTION*, the function used
;;; to figure out how to round up the position of comments in an attempt to
;;; make the comments line up nicely.
(DEFUN ROUND-FOR-COMMENT (IND)
  (LET ((SPACE-WIDTH (FONT-SPACE-WIDTH)))
    (LET ((X (* 8 SPACE-WIDTH)))
      (+ *COMMENT-COLUMN* (* (+ (// (- IND *COMMENT-COLUMN*) X) 1) X)))))

;; Return the index in LINE of the start of the comment, or NIL if no comment.
;; The second value is the index of the end of the comment-starter.
;; BEGIN-ALSO means if there is no *COMMENT-START*, find *COMMENT-BEGIN*;
;; this is for C-M-; to work right in text mode.
(DEFUN FIND-COMMENT-START (LINE &OPTIONAL BEGIN-ALSO)
  (PROG (START-START-INDEX START-END-INDEX)
    (IF (AND *COMMENT-START* (SYMBOLP *COMMENT-START*))
	(MULTIPLE-VALUE (START-START-INDEX START-END-INDEX)
	  (FUNCALL *COMMENT-START* LINE))
	(AND (OR *COMMENT-START* BEGIN-ALSO)
	     (SETQ START-START-INDEX (STRING-SEARCH (OR *COMMENT-START* *COMMENT-BEGIN*)
						    LINE))
	     (SETQ START-END-INDEX (+ START-START-INDEX
				      (STRING-LENGTH *COMMENT-START*)))))
    (RETURN START-START-INDEX START-END-INDEX)))

(DEFCOM COM-KILL-COMMENT "Delete any comment on the current line." ()
  (LET ((LEN (LINE-LENGTH (BP-LINE (POINT)))))
    (KILL-COMMENT (BP-LINE (POINT)) NIL)
    (OR (= LEN (LINE-LENGTH (BP-LINE (POINT))))
	(MOVE-BP (POINT) (END-LINE (POINT)))))
  DIS-TEXT)

(DEFCOM COM-UNCOMMENT-REGION "Delete any comments within the region." ()
  (REGION-LINES (START-LINE STOP-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE)))
	((EQ LINE STOP-LINE))
      (KILL-COMMENT LINE T)
      (SETQ *LAST-COMMAND-TYPE* 'KILL)))
  DIS-TEXT)

;; Kill the comment on the line with BP.  APPEND-P = NIL means don't append to
;; a previous kill.  T means do append, and append an additional CR.
(DEFUN KILL-COMMENT (LINE APPEND-P &AUX START-INDEX)
  (OR APPEND-P (SETQ *LAST-COMMAND-TYPE* NIL))
  (AND (SETQ START-INDEX (FIND-COMMENT-START LINE T))
       (LET ((BP (CREATE-BP LINE START-INDEX)))
	 (KILL-INTERVAL (BACKWARD-OVER *BLANKS* BP) (END-LINE BP) T)
	 (AND APPEND-P (INSERT (INTERVAL-LAST-BP (CAR *KILL-RING*)) #\CR))))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL))

(DEFCOM COM-DOWN-COMMENT-LINE "Move to the comment position in the next line.
Equivalent to COM-DOWN-REAL-LINE followed by COM-INDENT-FOR-COMMENT, except
that any blank comment on the current line is deleted first." ()
  (LET ((LINE (BP-LINE (POINT)))
	(LEN (ARRAY-ACTIVE-LENGTH *COMMENT-BEGIN*)))
    (AND ( (LINE-LENGTH LINE) LEN)
	 (STRING-EQUAL *COMMENT-BEGIN* LINE	;Delete any empty comment on this line
		       0 (- (LINE-LENGTH LINE) LEN))
	 (LET ((BP1 (END-LINE (POINT))))
	   (LET ((BP2 (BACKWARD-OVER *BLANKS* (FORWARD-CHAR BP1 (MINUS LEN)))))
	     (DELETE-INTERVAL BP2 BP1 T)))))
  (COM-DOWN-REAL-LINE)
  (LET ((*NUMERIC-ARG-P* NIL) (*NUMERIC-ARG* 1))
    (COM-INDENT-FOR-COMMENT)))

(DEFCOM COM-UP-COMMENT-LINE "Move to comment position in the previous line.
Equivalent to COM-UP-REAL-LINE followed by COM-INDENT-FOR-COMMENT, except
that any blank comment on the current line is deleted first." ()
  (LET ((*NUMERIC-ARG* (MINUS *NUMERIC-ARG*)))
    (COM-DOWN-COMMENT-LINE)))

(DEFCOM COM-INDENT-COMMENT-RELATIVE "Align new comment with previous one.
Sets *COMMENT-COLUMN* to position of previous comment then does COM-INDENT-FOR-COMMENT." ()
  (LET (START-INDEX BP)
    ;; Find a line, before our starting one, which has a comment on it.
    (DO ((LINE (LINE-PREVIOUS (BP-LINE (POINT))) (LINE-PREVIOUS LINE)))
	((NULL LINE) (BARF))
      (SETQ START-INDEX (FIND-COMMENT-START LINE))
      (AND START-INDEX (RETURN (SETQ BP (CREATE-BP LINE START-INDEX)))))
    (SETQ *COMMENT-COLUMN* (BP-INDENTATION BP))
    (COM-INDENT-FOR-COMMENT)))

(DEFCOM COM-SET-COMMENT-COL "Set *COMMENT-COLUMN* to the current horizontal position.
With an argument, sets it to position of previous comment then aligns or creates a comment
on the current line." ()
  (COND (*NUMERIC-ARG-P*
	 (LET ((*NUMERIC-ARG-P* NIL)
	       (*NUMERIC-ARG* 1))
	   (COM-INDENT-COMMENT-RELATIVE)))
	(T
	 (TYPEIN-LINE "Comment column = ~D"
		      (SETQ *COMMENT-COLUMN* (BP-INDENTATION (POINT))))
	 DIS-NONE)))

(DEFCOM COM-INDENT-NEW-COMMENT-LINE "Insert newline, then start new comment.
If done when not in a comment, acts like COM-INDENT-NEW-LINE.  Otherwise,
the comment is ended." ()
  (LET ((PT (POINT))
	START END)
    (DELETE-BACKWARD-OVER *BLANKS* PT)
    (MULTIPLE-VALUE (START END)
      (FIND-COMMENT-START (BP-LINE PT)))
    (COND ((OR (NOT START) (< (BP-INDEX PT) START))
	   (MUST-REDISPLAY *WINDOW* (KEY-EXECUTE #\CR))
	   (IF *SPACE-INDENT-FLAG* (KEY-EXECUTE #\TAB) DIS-NONE))
	  (T
	   (INSERT-MOVING PT *COMMENT-END*)
	   (INSERT PT (SUBSTRING (BP-LINE PT) START END))
	   (MUST-REDISPLAY *WINDOW* (KEY-EXECUTE #\CR))
	   (COM-INDENT-FOR-COMMENT)))))

(DEFCOM COM-END-COMMENT "Terminate comment on this line and move to the next.
Terminates the comment if there is one on this line and moves to the next line
down.  Primarily useful when a comment terminator exists (TECO or MACSYMA mode)." ()
  (LET ((PT (POINT)))
    (COND ((FIND-COMMENT-START (BP-LINE PT))
	   ;; This line has a comment on it.
	   (INSERT (END-LINE PT) *COMMENT-END*)
	   ;; Make sure interval ends in a newline.
	   (COND ((NOT (= (BP-CH-CHAR (INTERVAL-LAST-BP *INTERVAL*)) #\CR))
		  (INSERT (INTERVAL-LAST-BP *INTERVAL*) #\CR)))
	   (MOVE-BP (LINE-NEXT (BP-LINE PT)) 0)
	   DIS-TEXT)
	  (T DIS-NONE))))

(DEFCOM COM-SET-FILL-COLUMN "Set the fill column from point's current hpos.
With an argument, if it is less than 200., set fill column to that many characters;
otherwise set it to that many pixels." ()
  (LET ((COL (COND (*NUMERIC-ARG-P*
		    (COND ((< *NUMERIC-ARG* 200.)
			   (* *NUMERIC-ARG* (FONT-SPACE-WIDTH)))
			  (T *NUMERIC-ARG*)))
		   (T (BP-INDENTATION (POINT))))))
    (TYPEIN-LINE "Fill Column = ~D. pixels." COL)
    (SETQ *FILL-COLUMN* COL))
  DIS-NONE)

(DEFCOM COM-FILL-PARAGRAPH "Fill (or adjust) this (or next) paragraph.
Point stays the same.  A positive argument means to adjust rather than fill." ()
  (LET ((INT (PARAGRAPH-INTERVAL (POINT))))
    (FILL-INTERVAL INT NIL T (AND *NUMERIC-ARG-P* (PLUSP *NUMERIC-ARG*))))
  DIS-TEXT)

(DEFCOM COM-FILL-REGION "Fill (or adjust) the region." ()
  (REGION (BP1 BP2)
    (FILL-INTERVAL BP1 BP2 T (AND *NUMERIC-ARG-P* (PLUSP *NUMERIC-ARG*))))
  DIS-TEXT)

(DEFCOM COM-SET-FILL-PREFIX "Define Fill Prefix from the current line.
All of the current line up to point becomes the Fill Prefix.  Fill Region
assumes that each non-blank line starts with the prefix (which is
ignored for filling purposes).  To stop using a Fill Prefix, do
a Set Fill Prefix at the beginning of a line." () 
  (SETQ *FILL-PREFIX* (SUBSTRING (BP-LINE (POINT)) 0 (BP-INDEX (POINT))))
  DIS-NONE)

(DEFCOM COM-FILL-LONG-COMMENT "Fill this comment.
Comment must begin at the start of the line" (KM)
  (LET ((BP1 (BACKWARD-OVER-COMMENT-LINES (POINT)))
	BP2 LINE1 LINE2 (MINEND 177777) LINE3 NON-COMMENT-LINES)
    (SETQ BP2 (SKIP-OVER-BLANK-LINES-AND-COMMENTS BP1 T)
	  LINE1 (BP-LINE BP1) LINE2 (BP-LINE BP2))
    (AND (EQ LINE1 LINE2) (BARF "No comment starting at beginning of line"))
    (DO ((LINE LINE1 (LINE-NEXT LINE))
	 (START) (END))
	((EQ LINE LINE2))
      (MULTIPLE-VALUE (START END)
	(FIND-COMMENT-START LINE))
      (IF START
	  (SETQ LINE3 LINE			;Remember a non-blank line
		MINEND (MIN MINEND END))
	  (PUSH LINE NON-COMMENT-LINES)))
    (OR LINE3 (BARF "No comment starting at beginning of line"))
    (LET ((*FILL-PREFIX* (SUBSTRING LINE3 0 MINEND)))
      (FILL-INTERVAL BP1 (END-LINE BP2 -1) T)
      (DOLIST (LINE NON-COMMENT-LINES)		;Now remove excess comments
	(AND (STRING-EQUAL LINE *FILL-PREFIX*)
	     (SETF (LINE-LENGTH LINE) 0)))))
  DIS-TEXT)

(DEFCOM COM-DELETE-HORIZONTAL-SPACE "Delete any spaces or tabs around point.
If given a numeric argument, that many spaces are then inserted." ()
  (DELETE-AROUND *BLANKS* (POINT))
  (AND *NUMERIC-ARG-P* (MOVE-BP (POINT) (INSERT-CHARS (POINT) #\SP *NUMERIC-ARG*)))
  DIS-TEXT)

(DEFCOM COM-BACK-TO-INDENTATION "Move to start of current line and past any blanks." (KM)
  (MOVE-BP (POINT) (FORWARD-OVER *BLANKS* (BEG-LINE (POINT))))
  DIS-BPS)

(DEFCOM COM-UPPERCASE-REGION "Uppercase from point to the mark." ()
  (REGION (BP1 BP2)
    (UNDO-SAVE BP1 BP2 T "Upcase")
    (UPCASE-INTERVAL BP1 BP2 T))
  DIS-TEXT)

(DEFCOM COM-LOWERCASE-REGION "Lowercase from point to the mark." ()
  (REGION (BP1 BP2)
    (UNDO-SAVE BP1 BP2 T "Downcase")
    (DOWNCASE-INTERVAL BP1 BP2 T))
  DIS-TEXT)

(DEFCOM COM-UPPERCASE-WORD "Uppercase one or more words forward." ()
  (LET ((TEM (FORWARD-WORD (POINT) *NUMERIC-ARG*)))
    (OR TEM (BARF))
    (UPCASE-INTERVAL (POINT) TEM)
    (AND (PLUSP *NUMERIC-ARG*)
         (MOVE-BP (POINT) TEM)))
  DIS-TEXT)

(DEFCOM COM-LOWERCASE-WORD "Lowercase one or more words forward." ()
  (LET ((TEM (FORWARD-WORD (POINT) *NUMERIC-ARG*)))
    (OR TEM (BARF))
    (DOWNCASE-INTERVAL (POINT) TEM)
    (AND (PLUSP *NUMERIC-ARG*)
         (MOVE-BP (POINT) TEM)))
  DIS-TEXT)

(DEFCOM COM-UPPERCASE-INITIAL "Put next word in lowercase, but capitalize initial.
With an argument, captializes that many words." ()
  (LET ((BP1 (COPY-BP (POINT))) (ARG *NUMERIC-ARG*))
    (COND ((MINUSP ARG)
	   (OR (SETQ BP1 (FORWARD-WORD BP1 ARG)) (BARF))
	   (SETQ ARG (MINUS ARG))))
    (DO ((I 0 (1+ I))
	 (BP))
	(( I ARG))
      (OR (SETQ BP (FORWARD-TO-WORD BP1)) (BARF))
      (OR (SETQ BP1 (FORWARD-WORD BP)) (BARF))
      (DO ((CH)) (NIL)
	(SETQ CH (BP-CH-CHAR BP))
	(AND (OR (BP-= BP BP1)
		 (AND ( CH #/A) ( CH #/Z))
		 (AND ( CH #/a) ( CH #/z)))
	     (RETURN))
	(IBP BP))
      (DOWNCASE-INTERVAL BP BP1)
      (UPCASE-CHAR BP))    
    (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1)))
  DIS-TEXT)

(DEFCOM COM-DELETE-BLANK-LINES "Delete any blank lines around the end of the current line." ()
  (LET ((FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
	(LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	(LINE (BP-LINE (POINT)))
	(TEM))
    (COND ((LINE-BLANK-P LINE)
	   (SETQ TEM LINE)
	   (DO ((L TEM))			;Move backward over blank lines.
	       ((EQ L FIRST-LINE))
	     (SETQ L (LINE-PREVIOUS L))
	     (OR (LINE-BLANK-P L) (RETURN NIL))
	     (SETQ TEM L))
	   (MOVE-BP (POINT) TEM 0)
	   (DO ((L LINE))			;Move forward over more blank lines.
	       ((EQ L LAST-LINE))
	     (SETQ L (LINE-NEXT L))
	     (OR (LINE-BLANK-P L) (RETURN NIL))
	     (SETQ LINE L))
	   (AND (EQ LINE TEM) (NEQ LINE LAST-LINE) (SETQ LINE (LINE-NEXT LINE)))
	   (DELETE-INTERVAL (POINT) (BEG-OF-LINE LINE)))
	  (T
	   (SETQ TEM (BACKWARD-OVER *BLANKS* (END-OF-LINE LINE)))
	   (DO ((L LINE))
	       ((EQ L LAST-LINE))
	     (SETQ L (LINE-NEXT L))
	     (OR (LINE-BLANK-P L) (RETURN NIL))
	     (SETQ LINE L))
	   (DELETE-INTERVAL TEM (END-OF-LINE LINE)))))
  DIS-TEXT)

(DEFCOM COM-INDENT-RIGIDLY "Shift text in the region sideways as a unit.
All lines in the region have their indentation increased by the numeric
argument of this command (the argument may be negative).  The argument
is a number of SPACE characters in the default font." ()
  (AND (EQ *LAST-COMMAND-TYPE* 'REGION) (SETF (WINDOW-MARK-P *WINDOW*) T))
  (REGION-LINES (START-LINE STOP-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
	 (DELTA (* *NUMERIC-ARG* (FONT-SPACE-WIDTH))))
	((EQ LINE STOP-LINE))
      (INDENT-LINE (CREATE-BP LINE 0) (MAX 0 (+ DELTA (LINE-INDENTATION LINE))))))
  (SETQ *CURRENT-COMMAND-TYPE* 'REGION)
  DIS-TEXT)

(DEFCOM COM-INDENT-REGION "Indent each line in the region.
With no argument, it calls the current TAB command to indent.
With an argument, makes the indentation of each line be as wide as that
many SPACEs in the current font." ()
  (LET ((COMMAND (COMMAND-LOOKUP #\TAB *COMTAB*)))
    (IF (EQ COMMAND 'COM-INDENT-FOR-LISP)
	(REGION (BP1 BP2)
	  (INDENT-INTERVAL-FOR-LISP BP1 BP2 T))	;Efficiency hack
	(REGION-LINES (START-LINE STOP-LINE)
	  (LET ((WIDTH (* *NUMERIC-ARG*
			  (FONT-SPACE-WIDTH)))
		(POINT (POINT))
		(OLD-POINT (COPY-BP (POINT))))
	    (MOVE-BP POINT START-LINE 0)
	    (DO ()
		(NIL)
	      (IF *NUMERIC-ARG-P*
		  (INDENT-LINE POINT WIDTH)
		  (FUNCALL COMMAND))
	      (MOVE-BP POINT (BEG-LINE POINT 1))
	      (IF (EQ (BP-LINE POINT) STOP-LINE)
		  (RETURN NIL)))
	    (MOVE-BP POINT OLD-POINT)))))
   DIS-TEXT)

(DEFCOM COM-STUPID-TAB "Insert spaces to next even multiple of 8 in current font." ()
  (LET ((PT (POINT))
	(FONT (CURRENT-FONT *WINDOW*)))
    (LET ((FONT-SPACE-WIDTH (FONT-SPACE-WIDTH FONT)))
      (LET ((POS (BP-INDENTATION PT))
	    (X (* 10 FONT-SPACE-WIDTH))
	    (SPACE (DPB *FONT* %%CH-FONT #\SP)))
	(DO L (// (- (* X (1+ (// POS X))) POS) FONT-SPACE-WIDTH) (1- L) ( L 0)
	    (INSERT-MOVING PT SPACE)))))
    DIS-TEXT)

(DEFCOM COM-INSERT-TAB "Insert a Tab in the buffer at point." ()
  (DOTIMES (I *NUMERIC-ARG*) (INSERT-MOVING (POINT) #\TAB))
  DIS-TEXT)

(DEFCOM COM-INSERT-FF "Insert a Form-feed in the buffer at point." ()
  (DOTIMES (I *NUMERIC-ARG*) (INSERT-MOVING (POINT) #\FF))
  DIS-TEXT)

(DEFCOM COM-RIGHT-ADJUST-LINE "Adjust the current line to the right margin.
Non-zero argument means adjust from point to the end of the line." ()
  (COND ((NOT *NUMERIC-ARG-P*)
	 (MOVE-BP (POINT) (FORWARD-OVER *BLANKS* (BEG-LINE (POINT))))))
  (LET ((LINE (BP-LINE (POINT))))
    (LET ((SWID (STRING-WIDTH LINE
			      (BP-INDEX (POINT))
			      (BP-INDEX (BACKWARD-OVER *BLANKS* (END-LINE (POINT))))))
	  (RPOS (OR *FILL-COLUMN* (TV:SHEET-INSIDE-WIDTH (WINDOW-SHEET *WINDOW*)))))
      (MOVE-BP (POINT) (INDENT-TO (POINT) (- RPOS SWID)))))
  DIS-TEXT)

(DEFCOM COM-CENTER-LINE "Center this line's text within the line.
With argument, centers that many lines and moves past." ()
  (COND ((MINUSP *NUMERIC-ARG*)
	 (MOVE-BP (POINT) (OR (BEG-LINE (POINT) *NUMERIC-ARG*) (BARF)))
	 (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*))))
  (LET ((SHEET (WINDOW-SHEET *WINDOW*)))
    (DO ((I 0 (1+ I))
	 (LINE (BP-LINE (POINT)) (LINE-NEXT LINE))
	 (LIMIT-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	 (BP)
	 (TEM))
	(( I *NUMERIC-ARG*)
	 (AND *NUMERIC-ARG-P* (MOVE-BP (POINT) LINE 0)))
      (SETQ BP (FORWARD-OVER *BLANKS* (BEG-OF-LINE LINE)))
      (SETQ TEM (BP-INDEX BP))
      (SETQ BP (BACKWARD-OVER *BLANKS* (END-LINE BP)))
      (SETQ TEM (STRING-WIDTH LINE TEM (BP-INDEX BP) SHEET))
      (AND (> TEM *FILL-COLUMN*)
	   (BARF "The text of the line is too long."))
      (INDENT-LINE BP (// (- *FILL-COLUMN* TEM) 2))
      (COND ((EQ LINE LIMIT-LINE)
	     (AND *NUMERIC-ARG-P*
		  (MOVE-BP (POINT) (END-LINE BP)))
	     (RETURN NIL)))))
  DIS-TEXT)

(DEFCOM COM-INDENT-NESTED "Indent line for specified nesting level.
With no argument (or argument 1) indents the line at the same nesting
level as the last nonblank line (ie, directly under it).
A larger argument means that this line is that many levels
closer to the surface, and should indent under the last line
above it whose level is the same.  The previous lines are scanned
under the assumption that any line less indented than its successors
is one level higher than they.
However, unindented lines and comment lines are ignored.
If the cursor is not at the beginning of a line, the whole line
is indented, but the cursor stays fixed with respect to the text." ()
  (LET ((PT (POINT))
	(IND-SEEN 7777777))
    (DO-NAMED LUPO
	((J 0 (1+ J))
	 (LINE (BP-LINE PT))
	 (LIMIT-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
	(( J *NUMERIC-ARG*))
      (DO ((BP)
	   (IND))
	  ((EQ LINE LIMIT-LINE)
	   (SETQ IND-SEEN 0)
	   (RETURN-FROM LUPO))
	(SETQ LINE (LINE-PREVIOUS LINE))
	(COND ((NOT (LINE-BLANK-P LINE))
	       ;; We have found a non-blank line.
	       (SETQ BP (FORWARD-OVER *BLANKS* (BEG-OF-LINE LINE)))
	       ;; BP is now just past lines's indentation.
	       (COND ((NOT (OR (AND *COMMENT-START*
				    ;;Lines starting with a comment don't count.
				    (LOOKING-AT BP *COMMENT-START*))
			       ;; Line is unindented, doesn't count.
			       (ZEROP (SETQ IND (LINE-INDENTATION LINE)))
			       ;; Is this less indented than anything we have seen yet?
			       ( IND IND-SEEN)))
		      (SETQ IND-SEEN IND)
                      (RETURN NIL)))))))
    ;; Now IND-SEEN is the place to which to indent.
    (INDENT-LINE PT IND-SEEN)
    (INDENT-BP-ADJUSTMENT PT))
  DIS-TEXT)

(DEFVAR *STRING-UNDER*)
(DEFCOM COM-INDENT-UNDER "Indent to align under STRING (read from tty).
Searches back, line by line, forward in each line, for a string
that matches the one read and that is more to the right than the
caller's cursor already is.  Indents to align with string found,
removing any previous indentation around point first." ()
  (LET ((ORIGINAL-IND (BP-INDENTATION (POINT)))
	(STRING (TYPEIN-LINE-READLINE "String to align with:"))
	(PT (POINT))
	(STRING-LEN 0)
	(LINE NIL)				;The line we finally found.
	(INDENTATION NIL))			;Its indentation.
    (SETQ STRING-LEN (STRING-LENGTH STRING)
	  LINE (BP-LINE PT))
    (COND ((PLUSP STRING-LEN)
	   (SETQ *STRING-UNDER* STRING))
	  (T (SETQ STRING *STRING-UNDER*)))
    
    (DO-NAMED LUPO
	((LIMIT-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
	 (BP (COPY-BP PT)))
	((EQ LINE LIMIT-LINE)
	 (BARF "String not found."))
      (SETQ LINE (LINE-PREVIOUS LINE))
      (SETF (BP-LINE BP) LINE)
      (DO ((INDEX 0))
	  ((NULL (SETQ INDEX (STRING-SEARCH STRING LINE (+ STRING-LEN INDEX)))))
	(SETF (BP-INDEX BP) INDEX)
	(AND (> (SETQ INDENTATION (BP-INDENTATION BP))
		ORIGINAL-IND)
	     (RETURN-FROM LUPO))))
    (OR (FIND-BP-IN-WINDOW *WINDOW* LINE 0)
	(FUNCALL *TYPEIN-WINDOW* ':LINE-OUT LINE))
    (MOVE-BP PT (INDENT-LINE PT INDENTATION)))
  DIS-TEXT)

(DEFCOM COM-INDENT-RELATIVE "Indent Relative to the previous line.
With non-null argument, does Tab-to-Tab-Stop.  Otherwise,
Add whitespace characters until underneath an indentation point
in the previous non-null line.  Successive calls find successive
indentation points.  An indentation point is the end
of a sequence of spaces and tabs.  The end of the line counts;
after that, we cycle back to the first suitable indentation.
If there is no suitable indentation point, Tab-to-Tab-Stop
is done." ()
  (LET ((PT (POINT)) IND)
    (IF (OR *NUMERIC-ARG-P*
	     (NULL (SETQ IND (INDENT-RELATIVE PT))))
	 (COM-TAB-TO-TAB-STOP)
	 (DELETE-BACKWARD-OVER *BLANKS* PT)
	 (MOVE-BP PT (INDENT-TO PT IND))
	 DIS-TEXT)))

(DEFUN INDENT-RELATIVE (BP &OPTIONAL (RESTART-OK T) INDENT-TO-WORDS &AUX START DEST BP1 L)
  (SETQ BP1 (BACKWARD-OVER *BLANKS* BP)
	L (DO ((L (BP-LINE BP))
	       (FIRST (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
	      ((EQ L FIRST) NIL)
	    (SETQ L (LINE-PREVIOUS L))
	    (OR (ZEROP (LINE-LENGTH L))
		(RETURN L))))
  (COND ((NULL L) NIL)
	;; L is the previous non-blank line.
	;; BP1 is at the beginning of the current line whitespace.
	((OR (AND (SETQ START (INDENTATION-INDEX L (BP-INDENTATION BP)))
		  (< START (LINE-LENGTH L)))
	     (AND RESTART-OK
		  (SETQ START (INDENTATION-INDEX L (BP-INDENTATION BP1)))))
	 (SETQ DEST (IF (AND INDENT-TO-WORDS (ZEROP START)) START
			(STRING-SEARCH-SET *BLANKS* L START)))
	 (MOVE-BP BP1 L (OR DEST (LINE-LENGTH L)))
	 (SETQ BP1 (IF INDENT-TO-WORDS
		       (LET ((BP2 (FORWARD-TO-WORD BP1))
			     (BP3 (END-LINE BP1)))
			 (IF (OR (NULL BP2) (BP-< BP3 BP2))
			     BP3 BP2))
		       (FORWARD-OVER *BLANKS* BP1)))
	 (BP-INDENTATION BP1))))

(DEFCOM COM-STACK-LIST-VERTICALLY "Indent the list after point, first insertings crlfs" ()
  (LET ((PT (POINT)))
    (WITH-BP (END (BACKWARD-OVER '(#/) #\SP #\TAB #\CR) (OR (FORWARD-SEXP PT) (BARF)))
		  ':MOVES)
      (DO ((BP (FORWARD-SEXP (FORWARD-LIST PT 1 NIL -1 T)
			     (IF *NUMERIC-ARG-P* 1 2))
	       (FORWARD-SEXP BP)))
	  ((NOT (BP-< BP END)))
	(INSERT-MOVING BP #\CR))
      (INDENT-INTERVAL-FOR-LISP PT END T)))
  DIS-TEXT)

(DEFCOM COM-MULTIPLE-TRY-LISP-TAB "Indent line differently if called more than once" ()
  (IF *NUMERIC-ARG-P* (COM-INDENT-FOR-LISP) (COM-INDENT-DIFFERENTLY)))

(DEFVAR *INDENT-DIFFERENTLY-REPETITION-LEVEL*)
(DEFVAR *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*)

(DEFCOM COM-INDENT-DIFFERENTLY "Try to indent this line differently
If called repeatedly, makes multiple attempts." ()
  (LET ((POINT (POINT)) IND)
    (OR (EQ *LAST-COMMAND-TYPE* 'INDENT-DIFFERENTLY)
	(SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 0
	      *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS* (LIST (BP-INDENTATION POINT))))
    (SETQ *CURRENT-COMMAND-TYPE* 'INDENT-DIFFERENTLY)
    (DO ((BP (BEG-LINE POINT))
	 (TIMES *NUMERIC-ARG*))
	(NIL)
      (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* (1+ *INDENT-DIFFERENTLY-REPETITION-LEVEL*))
      (SETQ IND (COND ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1000)
		       (NTH (- *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1001)
			    *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))
		      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 4)
		       (SETQ IND NIL)
		       (IF (> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 400)
			   (LET ((OIND (CAR *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))
				 (LINE (BP-LINE BP)))
			     (INDENT-LINE BP OIND)
			     (MOVE-BP BP LINE (INDENTATION-INDEX LINE OIND)))
			   (OR (> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 100)
			       (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 101))
			   (LET ((BP1 (FORWARD-SEXP POINT
						    (- 100
						       *INDENT-DIFFERENTLY-REPETITION-LEVEL*)
						    NIL 0 NIL T T)))
			     (IF BP1
				 (SETQ IND (BP-INDENTATION
					     (IF (EQ (BP-LINE BP1) (BP-LINE POINT))
						 POINT BP1)))
				 (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 400))))
		       (OR IND
			   (ATOM-WORD-SYNTAX-BIND
			     (INDENT-RELATIVE BP NIL T))))
		      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 3)
		       (LET ((*LISP-INDENT-OFFSET* 1))
			 (INDENT-FOR-LISP BP)))
		      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 2)
		       (LET ((*LISP-INDENT-OFFSET* 0))
			 (INDENT-FOR-LISP BP)))
		      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1)
		       (LET ((*LISP-INDENT-OFFSET-ALIST* NIL)
			     (*LISP-DEFUN-INDENTATION* NIL))
			 (INDENT-FOR-LISP BP)))
		      (T
		       (INDENT-FOR-LISP BP))))
      (COND ((NULL IND)
	     (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1000)
	     (SETQ *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*
		   (SI:ELIMINATE-DUPLICATES *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))
	     (SETQ *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*
		   (SORT *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS* #'LESSP)))
	    ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1000)
	     (RETURN T))
	    ((NOT (MEMQ IND (PROG1 *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*
				   (PUSH IND *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))))
	     (OR (PLUSP (SETQ TIMES (1- TIMES))) (RETURN T)))))
    (INDENT-LINE POINT IND)
    (INDENT-BP-ADJUSTMENT POINT))
  DIS-TEXT)
