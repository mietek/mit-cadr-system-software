;;; Functions that deal with indentation -*-Mode:LISP;Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Returns the number of PIXELs needed to produce the amount of indentation
;;;    this line has.
(DEFUN LINE-INDENTATION (LINE &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)))
  (STRING-WIDTH LINE
		0
		(BP-INDEX (FORWARD-OVER *BLANKS* (CREATE-BP LINE 0)))
		SHEET))

;;; Returns the number of PIXELs needed to produce the amount of indentation
;;;   to reach this BP, starting at the beginning of its line.
(DEFUN BP-INDENTATION (BP &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)))
  (TV:SHEET-COMPUTE-MOTION SHEET 0 0 (BP-LINE BP) 0 (BP-INDEX BP)))

;;; Like above, but ignores continuation lines
(DEFUN BP-VIRTUAL-INDENTATION (BP &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)))
  (TV:SHEET-STRING-LENGTH SHEET (BP-LINE BP) 0 (BP-INDEX BP)))

;;; Adds whitespace characters at BP until BP is at INDENTATION.
;;; Returns a BP to the end of what it inserted.
;;; This is the only function which knows to use spaces to perform
;;; indentation!!!  Nobody else should know that.
(DEFUN INDENT-TO (BP GOAL &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)) &AUX SPACES)
  (LET (N M)
    (LET ((BPI (BP-VIRTUAL-INDENTATION BP SHEET))
	  (SW (FONT-SPACE-WIDTH))
	  TW)
      (SETQ M (// GOAL (SETQ TW (* SW 10)))	;Number of tabs to get to goal
	    N (* TW M))				;Position of rightmost tab
      (IF (> BPI N)				;Past there, no tabs can be used,
	  (SETQ N (// (- GOAL BPI) SW) M 0)	;use all spaces
	  (SETQ M (- M (// BPI TW)) N (// (- GOAL N) SW))))	;else tabs and spaces
    (SETQ SPACES (MAKE-ARRAY NIL (IF (ZEROP *FONT*) ART-STRING ART-16B) (+ M N)))
    (LET ((TAB (DPB *FONT* %%CH-FONT #\TAB))
	  (SPACE (DPB *FONT* %%CH-FONT #\SP)))
      (DO I (1- M) (1- I) (MINUSP I)
	  (ASET TAB SPACES I))
      (DO ((I 1 (1+ I))
	   (J M (1+ J)))
	  ((> I N))
	(ASET SPACE SPACES J))))
  (PROG1 (INSERT-MOVING BP SPACES)
	 (RETURN-ARRAY SPACES)))

;;; Given a font, return the width of SPACE in that font.
(DEFUN FONT-SPACE-WIDTH (&OPTIONAL (FONT (CURRENT-FONT *WINDOW*)))
  (LET ((CHAR-WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT)))
    (IF CHAR-WIDTH-TABLE
	(AREF CHAR-WIDTH-TABLE #\SP)
	(FONT-CHAR-WIDTH FONT))))

;;; Causes the indentation at the front of BP's line to be INDENTATION pixels wide.
;;; Returns a BP to the end of what it inserted.
;;; Preserves the indentations of bps pointing within the indentation, if possible.
;;; If the indentation is already as desired, the line is not changed.
;;; By specifying BP1, the indentation after a certain point can be acted on.
;;; In that case, BP is ignored.
(DEFUN INDENT-LINE (BP INDENTATION &OPTIONAL
		       (SHEET (WINDOW-SHEET *WINDOW*))
		       (BP1 (CREATE-BP (BP-LINE BP) 0)))
  (LET (BP-AFTER BP-LIST NONBLANK-INDEX)
    (SETQ BP-AFTER (FORWARD-OVER *BLANKS* BP1))
    (SETQ NONBLANK-INDEX (BP-INDEX BP-AFTER))
    (IF (= INDENTATION (STRING-WIDTH (BP-LINE BP1) 0 NONBLANK-INDEX SHEET)) BP-AFTER
	(PROGN
	  (DOLIST (BP2 (LINE-BP-LIST (BP-LINE BP1)))
	    (COND ((OR (< (BP-INDEX BP2) NONBLANK-INDEX)
		       (AND (= (BP-INDEX BP2) NONBLANK-INDEX)
			    (EQ (BP-STATUS BP2) ':NORMAL)))
		   (PUSH (CONS BP2 (BP-INDENTATION BP2 SHEET)) BP-LIST))))
	  (DELETE-INTERVAL BP1 BP-AFTER T)
	  (PROG1 (INDENT-TO BP1 INDENTATION SHEET)
		 (LET ((NONBLANK-INDEX (BP-INDEX (FORWARD-OVER *BLANKS*
							       (CREATE-BP (BP-LINE BP) 0)))))
		   (DOLIST (BP-AND-INDENTATION BP-LIST)
		     (LET ((INDEX (INDENTATION-INDEX (BP-LINE (CAR BP-AND-INDENTATION))
						     (CDR BP-AND-INDENTATION) SHEET)))
		       (AND INDEX (SETF (BP-INDEX (CAR BP-AND-INDENTATION))
					(MIN NONBLANK-INDEX INDEX)))))))))))

;;; If the point is within this line's indentation, move it past that
;;; indentation.
(DEFUN INDENT-BP-ADJUSTMENT (BP)
    (LET ((BP1 (FORWARD-OVER *BLANKS* (CREATE-BP (BP-LINE BP) 0))))
      (COND ((AND (< (BP-INDEX (POINT))
		     (BP-INDEX BP1)))
	      (MOVE-BP BP BP1)))))

;;; Returns the char pos in LINE which would be at position XPOS.
;;; If XPOS is greater than the length of the string, return NIL.
;;; It the answer is between N and N+1, returns N+1.
(DEFUN INDENTATION-INDEX (LINE XPOS &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)))
      (TV:SHEET-SET-FONT SHEET (AREF (TV:SHEET-FONT-MAP SHEET) *FONT*))
      (MULTIPLE-VALUE-BIND (NIL NIL INDEX)
           (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE 0 NIL NIL XPOS 0)
	   INDEX))

(DEFCOM COM-TAB-HACKING-DELETE-FORWARD "Delete characters forward, changing tabs into spaces.
Argument is repeat count." ()
    (DELETE-CHARS-CONVERTING-TABS (POINT) *NUMERIC-ARG*))

(DEFCOM COM-TAB-HACKING-RUBOUT "Rub out a character, changing tabs to spaces.
So tabs rub out as if they had been spaces all along.
A numeric argument is a repeat count." ()
    (DELETE-CHARS-CONVERTING-TABS (POINT) (- *NUMERIC-ARG*)))

(DEFUN DELETE-CHARS-CONVERTING-TABS (POINT COUNT &AUX (BP (COPY-BP POINT)))
    ;; Scan across what we will delete, converting tabs to spaces.
    ;; BP gets set to the other end of the range to be deleted.
    (COND ((> COUNT 0)
	   (DOTIMES (I COUNT)
	     (AND (BP-= BP (INTERVAL-LAST-BP *INTERVAL*))
		  (RETURN (BEEP)))
             ;; When moving forward, whenever we find a blank we must
             ;; convert all tabs within the blanks that follow.
             (AND (MEMQ (BP-CH-CHAR BP) *BLANKS*)
                  (LET ((BP1 (COPY-BP BP)))
                    (DO ()
                        ((OR (BP-= BP1 (INTERVAL-LAST-BP *INTERVAL*))
			     (NOT (MEMQ (BP-CH-CHAR BP1) *BLANKS*))))
                      (COND ((= (BP-CH-CHAR BP1) #\TAB)
			     (TAB-CONVERT BP1 (FORWARD-CHAR BP1 1)))
                            (T (IBP BP1))))))
	     (IBP BP)))
	  (T
	   (DOTIMES (I (- COUNT))
	     (AND (BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))
		  (RETURN (BEEP)))
	     (AND (= (LDB %%CH-CHAR (BP-CHAR-BEFORE BP)) #\TAB)
                  (TAB-CONVERT (FORWARD-CHAR BP -1) BP))
	     (DBP BP))))
    (COND ((EQ (BP-LINE POINT) (BP-LINE BP))
	   (MUST-REDISPLAY *WINDOW* DIS-LINE (BP-LINE POINT)
			   (MIN (BP-INDEX POINT) (BP-INDEX BP))))
	  (T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
    (DELETE-INTERVAL POINT BP)
    DIS-NONE)

;; Convert a tab to spaces preserving the font.
;; We take a bp to before the tab and a bp to after, since that is easiest.
;; If they are not temporary bps, you can lose!
(DEFUN TAB-CONVERT (BP-BEFORE BP-AFTER)
  (LET ((INDENT-BEFORE (BP-VIRTUAL-INDENTATION BP-BEFORE))
	(INDENT-AFTER (BP-VIRTUAL-INDENTATION BP-AFTER))
	(*FONT* (LDB %%CH-FONT (BP-CHAR BP-BEFORE)))
	SPACE NSPACES)
    (SETQ SPACE (DPB *FONT* %%CH-FONT #\SP))
    (SETQ NSPACES (// (- INDENT-AFTER INDENT-BEFORE) (FONT-SPACE-WIDTH)))
    (MUNG-BP-LINE-AND-INTERVAL BP-BEFORE)
    (ASET SPACE (BP-LINE BP-BEFORE) (BP-INDEX BP-BEFORE))
    (INSERT-CHARS BP-BEFORE SPACE (1- NSPACES))
    (MOVE-BP BP-AFTER (BP-LINE BP-AFTER) (+ (BP-INDEX BP-AFTER) NSPACES -1))
    BP-AFTER))

(DEFCOM COM-INDENT-FOR-LISP-COMMENTS-SPECIAL 
	"Like LISP tab, except in comments which start at the beginning of the line,
where is it self inserting." ()
  (LET ((POINT (POINT))
	IN-COMMENT)
    (MULTIPLE-VALUE (NIL NIL IN-COMMENT)
      (LISP-BP-SYNTACTIC-CONTEXT POINT))
    (IF (AND IN-COMMENT (ZEROP (FIND-COMMENT-START (BP-LINE POINT))))
	(COM-INSERT-TAB) (COM-INDENT-FOR-LISP))))

(DEFCOM COM-INDENT-FOR-LISP "Indent this line to make ground LISP code.
Numeric argument is number of lines to indent." ()
  (LET ((PT (POINT)) END FLAG)
    (SETQ END (OR (BEG-LINE PT *NUMERIC-ARG*)
		  (INSERT (SETQ FLAG (INTERVAL-LAST-BP *INTERVAL*)) #\CR)))
    (SETQ END (INDENT-INTERVAL-FOR-LISP (BEG-LINE PT) END NIL NIL *NUMERIC-ARG-P*))
    (IF (= *NUMERIC-ARG* 1)
        (INDENT-BP-ADJUSTMENT PT)
        (MOVE-BP PT END))
    (AND FLAG
	 (DELETE-INTERVAL (FORWARD-CHAR FLAG -1) FLAG T)))
  DIS-TEXT)

(DEFCOM COM-INDENT-NEW-LINE "Insert a CRLF and the proper indentation on the new line." ()
  (MOVE-BP (POINT) (DELETE-BACKWARD-OVER *BLANKS* (POINT)))
  (LET (*CURRENT-COMMAND-TYPE*)			;Don't be fooled
    (KEY-EXECUTE #\CR *NUMERIC-ARG-P* *NUMERIC-ARG*)
    (KEY-EXECUTE #\TAB)))

(DEFCOM COM-INDENT-SEXP "Indent the following s-expression." ()
  (LET ((BP1 (OR (BEG-LINE (POINT) 1) (BARF)))
	(BP2 (OR (FORWARD-SEXP (POINT)) (BARF))))
    (AND (BP-< BP1 BP2)
	 (INDENT-INTERVAL-FOR-LISP BP1 BP2 NIL (BEG-LINE (POINT)))))
  DIS-TEXT)

(DEFCOM COM-INDENT-NEW-LINE-AT-PREVIOUS-SEXP
	"Insert a CRLF and the proper indentation at the s-expression before point." ()
  (LET* ((POINT (POINT))
	 (BP (OR (FORWARD-SEXP POINT (- *NUMERIC-ARG*)) (BARF))))
    (WITH-BP (OLD-POINT POINT ':NORMAL)
      (MOVE-BP POINT BP)
      (UNWIND-PROTECT
	(COM-INDENT-NEW-LINE)
	(MOVE-BP POINT OLD-POINT)))))

;;; Text grinding functions

(DEFUN FILL-INTERVAL (START-BP END-BP &OPTIONAL IN-ORDER-P ADJUST &AUX (FILLCOL *FILL-COLUMN*)
								       LINE1 LINE2 TEM)
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (UNDO-SAVE START-BP END-BP T "Fill")
  (SETQ LINE1 (BP-LINE START-BP)
	LINE2 (LET ((LINE (BP-LINE END-BP)))
		(IF (OR (LINE-BLANK-P LINE) (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
		    (LINE-NEXT LINE) LINE)))
  ;; Remove any fill prefixes that are there already
  (COND ((PLUSP (SETQ TEM (STRING-LENGTH *FILL-PREFIX*)))
	 (SETQ FILLCOL (- FILLCOL (STRING-WIDTH *FILL-PREFIX*)))
	 (DO ((LINE LINE1 (LINE-NEXT LINE)))
	     ((EQ LINE LINE2))
	   (AND ( (LINE-LENGTH LINE) TEM)
		(STRING-EQUAL LINE *FILL-PREFIX* 0 0 TEM TEM)
		(DELETE-INTERVAL (CREATE-BP LINE 0) (CREATE-BP LINE TEM) T)))))
  ;; Make sentences ending at eol have extra space
  (DO ((LINE LINE1 (LINE-NEXT LINE)))
      ((EQ LINE LINE2))
    (AND ( (SETQ TEM (1- (LINE-LENGTH LINE))) 0)
	 (MEMQ (AREF LINE TEM) *FILL-EXTRA-SPACE-LIST*)
	 (NEQ LINE2 (SETQ TEM (LINE-NEXT LINE)))
	 (NOT (ZEROP (LINE-LENGTH TEM)))
	 (INSERT (CREATE-BP LINE (LINE-LENGTH LINE)) #\SP)))
  ;; If adjusting, remove excess spaces
  (AND ADJUST (DO ((BP (COPY-BP START-BP)))
		  (())
		(OR (AND (SETQ BP (SEARCH BP #\SP))
			 (BP-< BP END-BP))
		    (RETURN NIL))
		(DELETE-OVER *BLANKS* (FORWARD-CHAR BP))
		(AND (MEMQ (BP-CHAR-BEFORE BP) *FILL-EXTRA-SPACE-LIST*)
		     (INSERT BP #\SP))))
  ;; And now start filling
  (DO ((LINE LINE1 (LINE-NEXT LINE))
       (TEM)
       (BREAK-NEXT)
       (SHEET (WINDOW-SHEET *WINDOW*))
       (FONT (CURRENT-FONT *WINDOW*)))
      ((EQ LINE LINE2))
    ;; Break generated by next line?
    (SETQ BREAK-NEXT (OR (EQ LINE2 (SETQ TEM (LINE-NEXT LINE)))
			 (ZEROP (LINE-LENGTH TEM))
			 (MEMQ (AREF TEM 0) *PARAGRAPH-DELIMITER-LIST*))
	  TEM (DO ((I 0 (1+ I))			;Initial blanks count in first word
		   (LEN (LINE-LENGTH LINE)))
		  ((OR (= I LEN)
		       (NOT (MEMQ (LDB %%CH-CHAR (AREF LINE I)) *PARAGRAPH-DELIMITER-LIST*)))
		   I)))
    ;; Handle this line
    (OR (ZEROP (LINE-LENGTH LINE))
	(DO ((POS 0)
	     (CHAR-POS 0)
	     (CP TEM)
	     (BP1 (COPY-BP START-BP))
	     (BP2 (COPY-BP START-BP))
	     (NBLANKS 0))
	    ((EQ LINE LINE2))
	  (SETQ POS (TV:SHEET-STRING-LENGTH SHEET LINE CHAR-POS CP NIL FONT POS))
	  (COND ((> POS FILLCOL)		;Line overflew
		 (AND ( NBLANKS 1) (RETURN NIL))
		 (MOVE-BP BP1 LINE CHAR-POS)
		 (INSERT-MOVING BP1 #\CR)
		 (DELETE-OVER *BLANKS* BP1)
		 (MOVE-BP BP2 LINE (LINE-LENGTH LINE))
		 (DELETE-BACKWARD-OVER *BLANKS* BP2)
		 (COND ((NOT BREAK-NEXT)
			(SETQ BP1 (INSERT-MOVING (END-LINE BP1) #\SP))
			(MOVE-BP BP2 (LINE-NEXT (BP-LINE BP1)) 0)
			(DELETE-INTERVAL BP1 BP2 T)))
		 (AND ADJUST (ADJUST-LINE LINE NBLANKS FILLCOL))
		 (RETURN NIL)))
	  (SETQ CHAR-POS CP)
	  (COND ((= CHAR-POS (LINE-LENGTH LINE))
		 (AND BREAK-NEXT (RETURN NIL))
		 (MOVE-BP BP1 LINE CHAR-POS)
		 (INSERT-MOVING BP1 #\SP)
		 (MOVE-BP BP2 (LINE-NEXT (BP-LINE BP1)) 0)
		 (DELETE-INTERVAL BP1 BP2 T)
		 (SETQ BREAK-NEXT (OR (EQ LINE2 (SETQ TEM (LINE-NEXT LINE)))
				      (ZEROP (LINE-LENGTH TEM))
				      (MEMQ (AREF TEM 0) *PARAGRAPH-DELIMITER-LIST*)))
		 (SETQ NBLANKS (1+ NBLANKS)))
		(( (SETQ CP (OR (STRING-SEARCH-CHAR #\SP LINE (1+ CHAR-POS))
				 (LINE-LENGTH LINE)))
		    (1+ CHAR-POS))
		 (SETQ NBLANKS  (1+ NBLANKS)))))))
  (AND (PLUSP (STRING-LENGTH *FILL-PREFIX*))
       (DO ((LINE LINE1 (LINE-NEXT LINE)))
	   ((EQ LINE LINE2))
	 (INSERT (CREATE-BP LINE 0) *FILL-PREFIX*))))

(DEFUN ADJUST-LINE (LINE NBLANKS FILL-COLUMN &AUX NEEDED AVG EXTRA EXPER)
  (SETQ NEEDED (// (- FILL-COLUMN (STRING-WIDTH LINE)) (FONT-SPACE-WIDTH))
	AVG (// NEEDED NBLANKS)
	EXTRA (\ NEEDED NBLANKS)
	EXPER (COND ((ZEROP EXTRA) 0) (T (// (+ NBLANKS (1- EXTRA)) EXTRA))))
  (DO ((N NBLANKS (1- N))
       (BP (CREATE-BP LINE 0))
       (EXP EXPER (1- EXP))
       (I AVG AVG))
      ((= N 0))
    (OR (SETQ BP (FORWARD-OVER *BLANKS* (SEARCH BP #\SP)))
	(FERROR NIL "Not enough spaces to adjust with in ~S" LINE))
    (AND (> EXTRA 0) (= EXP 1)
	 (SETQ I (1+ I)
	       EXTRA (1- EXTRA)
	       EXP EXPER))
    (DO I I (1- I) (= I 0)
	(INSERT-MOVING BP #\SP))))

;;;Common indenter for Tab, C-M-Q, and friends
(DEFUN INDENT-INTERVAL-FOR-LISP (BP1 &OPTIONAL BP2 IN-ORDER-P START-BP (COMMENTS-P T))
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (OR START-BP (SETQ START-BP (FORWARD-DEFUN BP1 -1 T)))
  (LISP-PARSE-FROM-DEFUN (BP-LINE BP2) START-BP)
  (INTERVAL-LINES (BP1 BP2) (START-LINE STOP-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
	 (*LISP-PARSE-PREPARSED-FLAG* T)
	 (BP)
	 (INDENTATION)
	 (IN-STRING))
	((EQ LINE STOP-LINE) BP2)
      (SETQ BP (CREATE-BP LINE 0))
      (MULTIPLE-VALUE (INDENTATION IN-STRING)
	(INDENT-FOR-LISP BP START-BP))
      (COND ((NOT IN-STRING)			;Dont touch lines inside a string
	     (INDENT-LINE BP INDENTATION)
	     (AND COMMENTS-P (INDENT-FOR-COMMENT BP))))
      (OR (EQ (LINE-NEXT LINE) STOP-LINE)
	  (LISP-PARSE-LINE-MEMOIZED LINE IN-STRING)))))

;;;This returns the amount of indentation that you want.
;;;*LISP-INDENT-OFFSET* is the amount to offset if there isnt a complete sexp on another line
;;;*LISP-DEFUN-INDENTION* is the amount to indent for top-level forms
;;;*LISP-INDENT-OFFSET-ALIST* is an alist of the form ((FUNCTION . OFFSET-LIST) ...)
;;;OFFSET-LIST is a list specifying (number-of-sexps-to-skip amount-to-change-indentation ...)
;;;or if OFFSET-LIST is a symbol or function, it is funcall'ed and can return
;;;the indentation, an offset, or a bp whose indentation to use
;;;The default OFFSET-ALIST is *DEFAULT-INDENT-ALIST* in ZWEI;MACROS >.
(DEFUN INDENT-FOR-LISP (BP &OPTIONAL START-DEFUN-BP
			   &AUX BP1 BP2 INDENTATION OFFSET SYM
			   TEM SPACE-WIDTH NSEXPS LASTPAREN LASTSEXP IN-STRING)
  (PROG ()
    (SETQ BP (CREATE-BP (BP-LINE BP) 0)
	  BP1 (OR START-DEFUN-BP (FORWARD-DEFUN BP -1 T)))
    (SETQ IN-STRING (LISP-PARSE-FROM-DEFUN (BP-LINE BP) BP1))
    (AND IN-STRING (RETURN 0 IN-STRING))
    ;; Get BP to last unterminated paren (up one level).  Sixth argument of NIL makes
    ;; sure we get an open paren and not a single-quote (forward or backward).
    (SETQ LASTPAREN (FORWARD-SEXP BP -1 NIL 1 BP1 NIL))
    ;; Get BP to start of last complete sexp, or NIL if none at this level.
    (SETQ LASTSEXP (FORWARD-SEXP BP -1 NIL 0 BP1))
    (AND LASTPAREN LASTSEXP (BP-= LASTSEXP LASTPAREN) (SETQ LASTSEXP NIL))
    (SETQ OFFSET 0
	  SPACE-WIDTH (FONT-SPACE-WIDTH))
    (AND LASTPAREN                  ;Try to find the indentation for the current function
	 (LET ((BP2 (FORWARD-CHAR LASTPAREN)))
	   (LET ((I (BP-INDEX BP2)))
	     (SETQ SYM (DO ((J I (1+ J))
			    (LINE (BP-LINE BP2))
			    (LENGTH (LINE-LENGTH (BP-LINE BP2))))
			   ((OR ( J LENGTH)
				( (LIST-SYNTAX (AREF LINE J)) LIST-ALPHABETIC))
			    (AND ( I J) (READ-FROM-STRING (NSUBSTRING LINE I J) '*EOF*)))))))
	 (SETQ TEM (OR (CDR (ASSQ SYM *LISP-INDENT-OFFSET-ALIST*))
		       (AND ( (STRING-LENGTH SYM) 3)
			    (STRING-EQUAL SYM "DEF" 0 0 3 3)
			    *LISP-DEFUN-INDENTATION*)))
	 ;; This function on the alist => value is either
	 ;; an indentation list or a function to call.
	 (COND ((LISTP TEM)         ;Indentation list, see how do handle this depth
		;; How many sexps at this level precede point?  Set NSEXPS.
		;; But, first, let's see how many are interesting (that's (1- MAX-I) ).
		;; Don't keep counting NSEXPS when it's already larger than is interesting.
		(DO ((BP3 (FORWARD-CHAR LASTPAREN 1) (FORWARD-SEXP BP3 1 NIL 0 BP))
		     (MAX-I (1+ (CAR (NLEFT 2 TEM))))
		     (I 0 (1+ I)))
		    ((NULL BP3) (SETQ NSEXPS (- I 2)))
		  (AND (> I MAX-I) (RETURN NIL)))
		;; Now see what the indentation lists says about that many sexps.
		(AND NSEXPS
		     (DO ((L TEM (CDDR L))
			  (I 0))
			 ((OR (NULL L) (> I NSEXPS)))
		       (AND (= (SETQ I (CAR L)) NSEXPS)
			    (SETQ OFFSET (CADR L) LASTSEXP NIL)))))
	       (T
		(MULTIPLE-VALUE (BP2 INDENTATION OFFSET)
		  (FUNCALL TEM BP1 BP LASTPAREN LASTSEXP SPACE-WIDTH SYM)))))
    (SETQ BP1 (DO () (NIL)
		(COND ((NULL LASTPAREN)		;If already balanced, nothing to do
		       (RETURN BP))
		      (BP2			;Specified what to indent to
		       (RETURN BP2))
		      (INDENTATION)		;Specified how far to indent
		      ;;If there is no complete sexp at this paren depth, line up just after
		      ;;the leftparen.
		      ((OR (NULL LASTSEXP) (BP-< LASTSEXP LASTPAREN))
		       (RETURN (FORWARD-CHAR LASTPAREN)))
		      (T
		       (SETQ BP1 (CREATE-BP (BP-LINE LASTSEXP) 0))
		       ;;If complete sexp is on different line than the unmatched leftparen,
		       ;;line up with start of sexp's line.
		       (COND ((OR (NULL LASTPAREN) (BP-< LASTPAREN BP1))
			      (SETQ BP1 (FORWARD-OVER *BLANKS* BP1))
			      ;;OK only if the first on the line or at that level.
			      (AND (OR (BP-= BP1 LASTSEXP)
				       (FORWARD-SEXP LASTSEXP -1 NIL 0 BP1))
				   (RETURN BP1))
			      (SETQ BP1 LASTPAREN
				    LASTPAREN (FORWARD-SEXP LASTSEXP -1 NIL 1 LASTPAREN NIL)
				    LASTSEXP (FORWARD-SEXP LASTSEXP -1 NIL 0 LASTPAREN)))
			     ;;Otherwise, maybe user specified how to handle this case
			     (*LISP-INDENT-OFFSET*
			      (SETQ OFFSET (+ *LISP-INDENT-OFFSET* OFFSET))
			      (RETURN (FORWARD-CHAR LASTPAREN)))
			     ;;If only one element in list so far, line up under left-paren
			     ;;also if the CAR doesnt look like the name of a function
			     ((INDENT-NOT-FUNCTION-P LASTPAREN
						     (SETQ BP2 (FORWARD-CHAR LASTPAREN)))
			      (RETURN BP2))
			     ((BP-< LASTSEXP (SETQ BP1 (FORWARD-SEXP BP2)))
			      (SETQ OFFSET (LONE-FUNCTION-OFFSET LASTPAREN))
			      (RETURN BP2))
			     ;;Otherwise line up with start of the second element of that list
			     (T
			      (RETURN (SKIP-OVER-BLANK-LINES-AND-COMMENTS
					(SKIP-OVER-BLANK-LINES-AND-COMMENTS BP1)))))))))
    (OR INDENTATION (SETQ INDENTATION (+ (* OFFSET SPACE-WIDTH) (BP-INDENTATION BP1))))
    (RETURN INDENTATION IN-STRING)))

(DEFVAR *NOT-LONE-FUNCTION-SUPERIORS* '(COND SELECT SELECTQ))
(DEFUN LONE-FUNCTION-OFFSET (BP &AUX SUPBP LINE IDX)
  (IF (AND (NOT (BEG-LINE-P BP))
	   (SETQ SUPBP (FORWARD-SEXP BP -1 NIL 1 NIL NIL))
	   (EQ (SETQ IDX (BP-INDEX (IBP SUPBP)) LINE (BP-LINE SUPBP))
	       (BP-LINE (SETQ SUPBP (FORWARD-SEXP SUPBP))))
	   (MEMQ (INTERN-SOFT (STRING-UPCASE (NSUBSTRING LINE IDX (BP-INDEX SUPBP))))
		 *NOT-LONE-FUNCTION-SUPERIORS*))
      0
      *LISP-INDENT-LONE-FUNCTION-OFFSET*))

(DEFVAR *INDENT-NOT-FUNCTION-SUPERIORS* '(LET LET* LET-GLOBALLY LAMBDA PROG PROG*
					  MULTIPLE-VALUE MULTIPLE-VALUE-BIND
					  (DEFUN 2) (DEFFLAVOR 3)))
(DEFUN INDENT-NOT-FUNCTION-P (BP BP2 &AUX SUPBP LINE IDX TEM)
  (OR (AND ( (LIST-SYNTAX (SETQ TEM (BP-CH-CHAR BP2))) LIST-ALPHABETIC)
	   (NOT (MEMQ TEM '(#// #/|))))		;These are really atoms
      (AND (NOT (BEG-LINE-P BP))
	   (SETQ SUPBP (FORWARD-SEXP BP -1 NIL 1 NIL NIL))
	   (EQ (SETQ IDX (BP-INDEX (IBP SUPBP)) LINE (BP-LINE SUPBP))
	       (BP-LINE (SETQ SUPBP (FORWARD-SEXP SUPBP))))
	   (SETQ TEM (DO ((SYM (INTERN-SOFT (STRING-UPCASE (NSUBSTRING LINE IDX
								       (BP-INDEX SUPBP)))))
			  (L *INDENT-NOT-FUNCTION-SUPERIORS* (CDR L)))
			 ((NULL L) NIL)
		       (AND (EQ SYM (IF (ATOM (CAR L)) (CAR L) (CAAR L)))
			    (RETURN (CAR L)))))
	   (BP-= BP (FORWARD-OVER *WHITESPACE-CHARS* (IF (ATOM TEM) SUPBP
							 (FORWARD-SEXP SUPBP
								       (1- (CADR TEM))
								       T)))))))

;;;This is the default indenter for PROGs; tags and forms must be handled separately.
(DEFVAR *PROG-TAG-INDENT-OFFSET* -3)
(DEFVAR *PROG-FORM-INDENT-OFFSET* 0)

(DEFUN INDENT-PROG (IGNORE BP LASTPAREN &REST IGNORE &AUX BEG BP1 (OFFSET 0))
    (SETQ BEG (BP-LINE BP))
    (LET ((TYPE (IF (EQ (LINE-TYPE BEG) ':ATOM) ':ATOM ':NORMAL))
	  (BP2 (FORWARD-SEXP (FORWARD-CHAR LASTPAREN) 2 T 0 BP))
	  (FLAG))
      (AND BP2
	   (DO ((LINE))
	       (NIL)
	     (SETQ BP2 (FORWARD-OVER *WHITESPACE-CHARS* BP2))
	     (OR (BP-< BP2 BP) (RETURN NIL))
	     (AND (BEG-LINE-P (BACKWARD-OVER *BLANKS* BP2))
		  (NEQ (SETQ LINE (BP-LINE BP2)) BEG)
		  (IF (EQ (LINE-TYPE LINE) TYPE)
		      (SETQ BP1 BP2)
		      (SETQ FLAG T)))		;Remember if must reverse offset
	     (OR (SETQ BP2 (FORWARD-SEXP BP2 1 NIL 0 BP))
		 (RETURN NIL))))
      (COND ((NULL BP1)
	     (SETQ OFFSET (IF (EQ TYPE ':ATOM) *PROG-TAG-INDENT-OFFSET*
			                       *PROG-FORM-INDENT-OFFSET*))
	     (AND FLAG
		  (SETQ OFFSET (- OFFSET (IF (EQ TYPE ':ATOM) *PROG-FORM-INDENT-OFFSET*
					                      *PROG-TAG-INDENT-OFFSET*)))))))
    (MVRETURN BP1 NIL OFFSET))
