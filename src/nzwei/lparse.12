;; -*-Mode:Lisp; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;; The purpose of this file is parsing and moving over units of Lisp code.
;; We avoid having to parse all the text that we move over each time we move
;; by remembering information about each line that we parse and using
;; that information the next time we parse across that line.
;; This information is stored on the LINE-CONTENTS-PLIST of the line
;; as the LISP-PARSE-LINE property, and its form is the same as the value
;; returned by LISP-PARSE-LINE (q.v.).  LINE-CONTENTS-PLIST is cleared
;; out whenever the contents of the line are changed.  Thus, if there is
;; any remembered information available, it has to be valid.

;; Useful functions include:
;; FORWARD-SEXP which is the normal way to move across or up list structure;
;; LISP-PARSE-FROM-DEFUN which can tell you whether a given line
;;   starts inside a string or not;
;; LISP-BP-SYNTACTIC-CONTEXT which tells you whether a given char is
;;   inside a string, slashified, or in a comment;
;; LISP-FIND-COMMENT-START which can tell you whether a line contains
;;   a comment, and, if so, where in the line it starts;
;; LISP-FIND-COMMENT-START-AND-END which does that
;;   and also says where the comment starter ends.

;; If this variable is T, LISP-PARSE-FROM-DEFUN assumes that the
;; lines all already have memoized parsings which are consistent and accurate.
(DEFVAR *LISP-PARSE-PREPARSED-FLAG*)

;; LISP-PARSE-LINE.
;; Scan a line, parsing as Lisp code to learn the information
;; useful for scanning quickly over the line as part of parsing a
;; multi-line s-expression.
;; If START-IN-STRING is non-nil, it should be #/" or #/|,
;; and signifies that the start of the line is within such a grouping.
;; START-INDEX and END-INDEX delimit the part of the line to be scanned.
;; The value is a number or a list.  If it is a number, it is the
;; total change in paren depth across the line (positive means open-parens).
;; If it is a list, the first element is the total change in depth,
;; the second is the smallest depth encountered in the line
;; (for the obscure case of lines looking like " ...) (... "),
;; the third is copied from START-IN-STRING, and the fourth
;; is like START-IN-STRING but applies to the end of the line instead of the start.
;; Elements of the list which are nil can be left out entirely.
;; If the value is just a number, you can assume that the minimum depth
;; was equal to (MIN 0 total-change-in-depth).
(DEFUN LISP-PARSE-LINE (LINE START-IN-STRING 
			&OPTIONAL (START-INDEX 0) (END-INDEX (LINE-LENGTH LINE)))
  (DO ((DEPTH 0) (MINDEPTH 0) (IN-STRING START-IN-STRING)
       (INDEX START-INDEX (1+ INDEX))
       (CH) (SYNTAX))
      (( INDEX END-INDEX)
       (COND ((OR IN-STRING START-IN-STRING)
	      (LIST DEPTH MINDEPTH START-IN-STRING IN-STRING))
	     (( MINDEPTH (MIN 0 DEPTH)) DEPTH)
	     (T (LIST DEPTH MINDEPTH))))
      (SETQ CH (LDB %%CH-CHAR (AREF LINE INDEX)))
      (COND ((= (SETQ SYNTAX (LIST-SYNTAX CH)) LIST-SLASH)
	     (SETQ INDEX (1+ INDEX)))		;This one a / => skip over next char.
	    (IN-STRING				;In a string, the opening char can close it.
	     (AND (= CH IN-STRING)		;Aside from that and slashes, nothing matters.
		  (SETQ IN-STRING NIL)))
	    ((= SYNTAX LIST-DOUBLE-QUOTE)	;String-starting chars.
	     (SETQ IN-STRING CH))
	    ((= SYNTAX LIST-OPEN)		;Open
	     (SETQ DEPTH (1+ DEPTH)))
	    ((= SYNTAX LIST-CLOSE)		;Close
	     (SETQ DEPTH (1- DEPTH))
	     (SETQ MINDEPTH (MIN DEPTH MINDEPTH)))
	    ((= SYNTAX LIST-COMMENT)		; ; starts comment
	     (SETQ INDEX END-INDEX)))))		; No need to scan through it!

;; This is just like LISP-PARSE-LINE except that it is memoized.
;; It remembers its parsing of a line as the LISP-PARSE-LINE propety
;; in the LINE-CONTENTS-PLIST of the line.
;; Memoizing is only done when the entire line is being parsed.
;; Because START-IN-STRING is remembered as part of the value,
;; we can tell whether the remembered value is for the same
;; setting of START-IN-STRING as we are now using.
;; The LINE-CONTENTS-PLIST of a line is cleared when the line is munged.
;; Thus, if we have a remembered value, we know it matches the current contents.
(DEFUN LISP-PARSE-LINE-MEMOIZED (LINE START-IN-STRING
				 &OPTIONAL (START-INDEX 0) (END-INDEX (LINE-LENGTH LINE))
				 &AUX TEM)
  (COND ((AND (ZEROP START-INDEX)
	      (= END-INDEX (LINE-LENGTH LINE))
	      ;; If we are parsing the whole string,
	      ;; and there is a remembered value for this string,
	      (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'LISP-PARSE-LINE))
	      ;; and it used the same START-IN-STRING as we are using now,
	      ;; then just return it.
	      (EQUAL START-IN-STRING
		     (AND (LISTP TEM) (CADDR TEM)))))
	;; Otherwise, reparse the line
	(T (SETQ TEM (LISP-PARSE-LINE LINE START-IN-STRING START-INDEX END-INDEX))
	   ;; and, if we are parsing the entire line, remember the result.
	   (AND (ZEROP START-INDEX)
		(= END-INDEX (LINE-LENGTH LINE))
		(PUTPROP (LOCF (LINE-CONTENTS-PLIST LINE)) TEM 'LISP-PARSE-LINE))))
  TEM)

;; Parse and remember down to LINE from the start of the DEFUN containing LINE.
;; Returns the proper START-IN-STRING for parsing LINE.
(DEFUN LISP-PARSE-FROM-DEFUN (LINE &OPTIONAL DEFUN-BEG &AUX TEM)
  (COND (*LISP-PARSE-PREPARSED-FLAG*
	  (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'LISP-PARSE-LINE))
	  (AND (LISTP TEM) (CADDR TEM)))
	(T
	  (OR DEFUN-BEG (SETQ DEFUN-BEG (FORWARD-DEFUN (BEG-OF-LINE LINE) -1 T)))
	  (DO ((LINE1 (BP-LINE DEFUN-BEG) (LINE-NEXT LINE1))
	       (START-INDEX (BP-INDEX DEFUN-BEG) 0)
	       (IN-STRING))
	      ((EQ LINE LINE1) IN-STRING)
	    (SETQ TEM (LISP-PARSE-LINE-MEMOIZED LINE1 IN-STRING START-INDEX))
	    (SETQ IN-STRING
		  (AND (LISTP TEM) (CADDDR TEM)))))))

;; Describe the syntactic context of a spot identified by BP.
;; The first value is non-NIL if that spot is in a string.
;; The second is non-NIL if that spot is slashified.
;; The third is non-NIL if that spot is in a comment.
(DEFUN LISP-BP-SYNTACTIC-CONTEXT (BP &OPTIONAL (START-BP (FORWARD-DEFUN BP -1 T)))
  (DECLARE (RETURN-LIST IN-STRING SLASHIFIED IN-COMMENT))
  (DO ((I (COND ((EQ (BP-LINE BP) (BP-LINE START-BP)) (BP-INDEX START-BP)) (T 0))
	  (1+ I))
       (LINE (BP-LINE BP))
       (END-IDX (BP-INDEX BP))
       ;; Start of line is not slashified, and may be in a string.
       (SLASH NIL)
       (IN-STRING (LISP-PARSE-FROM-DEFUN (BP-LINE BP) START-BP))
       (CH))
      (( I END-IDX)
       (RETURN IN-STRING SLASH NIL))
    ;; Now scan through the line parsing till we reach the spot.
    (SETQ CH (LDB %%CH-CHAR (AREF LINE I)))
    (COND (SLASH
	    (SETQ SLASH NIL))
	  (T
	    (SELECT (LIST-SYNTAX CH)
	      (LIST-SLASH
		(SETQ SLASH T))
	      ;; Once we reach the start of a comment, we know the answer, so exit.
	      (LIST-COMMENT
		(OR IN-STRING (RETURN NIL NIL T)))
	      (LIST-DOUBLE-QUOTE
		(COND ((NOT IN-STRING)
		       (SETQ IN-STRING CH))
		      ((= CH IN-STRING)
		       (SETQ IN-STRING NIL)))))))))

;; Help parse lists backwards.
;; Given a line, and the status at the end of that line (IN-STRING and DEPTH),
;; look backward for the line following that containing the beginning of the list.
;; Return that line, and the in-string and depth for the end of that line.
(DEFUN LISP-BACKWARD-LIST-AUX (LINE IN-STRING DEPTH &OPTIONAL STOP-LINE &AUX DEFUN-BEG)
  ;; First, make sure everything from the start of this defun is all parsed.
  ;; That way, each line's LISP-PARSE-LINE property is set up for
  ;; the correct setting of START-IN-STRING - which is a setting we
  ;; would have no simple way of computing on our backwards scan,
  ;; due to the losing interaction of comments with strings.
  (LISP-PARSE-FROM-DEFUN (LINE-NEXT LINE)
			 (SETQ DEFUN-BEG (FORWARD-DEFUN (BEG-OF-LINE LINE) -1 T)))
  (DO ((LINE LINE (LINE-PREVIOUS LINE)) (TEM)
       ;; Paren depth of end of previous line.
       (PREVIOUS-DEPTH)
       ;; Minimum paren depth reached moving back over this line.
       (MIN-DEPTH))
      ((OR (EQ LINE STOP-LINE) (EQ LINE (BP-LINE DEFUN-BEG)) (NULL (LINE-PREVIOUS LINE)))
       (RETURN LINE IN-STRING DEPTH))
    ;; To move back to end of previous line, get parsing info on this line.
    (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'LISP-PARSE-LINE))
    ;; Depth changes count negatively when scanned backwards.
    (COND ((NUMBERP TEM) (SETQ PREVIOUS-DEPTH (- DEPTH TEM) MIN-DEPTH PREVIOUS-DEPTH))
	  (T (SETQ PREVIOUS-DEPTH (- DEPTH (CAR TEM))
		   ;; An explicit minimum depth is relative to the depth at start of line.
		   MIN-DEPTH (+ PREVIOUS-DEPTH (CADR TEM)))))
    (AND ( MIN-DEPTH 0) (RETURN LINE IN-STRING DEPTH))
    (SETQ DEPTH PREVIOUS-DEPTH
	  IN-STRING (AND (LISTP TEM) (CADDR TEM)))))

;; Return the index in LINE at which the comment starts, or NIL if no comment.
;; Knows how to check for strings, even strings which started on lines above this one.
(DEFUN LISP-FIND-COMMENT-START (LINE &OPTIONAL BEG-INDEX END-INDEX)
  (OR BEG-INDEX (SETQ BEG-INDEX 0))
  (OR END-INDEX (SETQ END-INDEX (LINE-LENGTH LINE)))
  (DO ((INDEX 0 (1+ INDEX)) (CH) (SYNTAX)
       (IN-STRING (LISP-PARSE-FROM-DEFUN LINE)))
      (( INDEX END-INDEX) NIL)
    (SETQ CH (LDB %%CH-CHAR (AREF LINE INDEX)))
    (SETQ SYNTAX (LIST-SYNTAX CH))
    (COND ((= SYNTAX LIST-SLASH) (SETQ INDEX (1+ INDEX)))
	  (IN-STRING (AND (= CH IN-STRING) (SETQ IN-STRING NIL)))
	  ((= SYNTAX LIST-DOUBLE-QUOTE) (SETQ IN-STRING CH))
	  ((= SYNTAX LIST-COMMENT) (RETURN INDEX)))))

;; Find the start and end indices of the comment starter on LINE.
;; This is useful as the value of *COMMENT-START*.
(DEFUN LISP-FIND-COMMENT-START-AND-END (LINE)
  (PROG (INDEX I2
	 (BEG-INDEX (COND ((EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
			   (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*)))
			  (T 0)))
	 (END-INDEX (COND ((EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
			   (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*)))
			  (T (LINE-LENGTH LINE)))))
	;; Find start of comment.  Return NIL if none.
	(SETQ INDEX (LISP-FIND-COMMENT-START LINE BEG-INDEX END-INDEX))
	(OR INDEX (RETURN NIL))
	;; Now find the end.  Skip over the semicolons and the spaces after them.
	(DO ((I1 INDEX (1+ I1)))
	    ((OR (= I1 END-INDEX)
		 ( (LIST-SYNTAX (AREF LINE I1)) LIST-COMMENT))
	     (SETQ I2 I1)))
	(RETURN INDEX (BP-INDEX (FORWARD-OVER *BLANKS* (CREATE-BP LINE I2))))))

;; Starting from the start of LINE at depth DEPTH inside a list,
;; find the line at which that list ends,
;; and return it and the in-string and depth at the start of that line.
(DEFUN LISP-FORWARD-LIST-AUX (LINE IN-STRING DEPTH &OPTIONAL STOP-LINE)
  (DO ((LINE LINE (LINE-NEXT LINE)) (TEM)
       ;; Holds paren depth of start of next line.
       (NEXT-DEPTH)
       ;; Minimum paren depth reached during this line.
       (MIN-DEPTH))
      ((OR (EQ LINE STOP-LINE) (NULL (LINE-NEXT LINE)))
       (RETURN LINE IN-STRING DEPTH))
    (SETQ TEM (LISP-PARSE-LINE-MEMOIZED LINE IN-STRING))
    (COND ((NUMBERP TEM) (SETQ NEXT-DEPTH (+ TEM DEPTH) MIN-DEPTH NEXT-DEPTH))
	  (T (SETQ NEXT-DEPTH (+ (CAR TEM) DEPTH)
		   MIN-DEPTH (+ (CADR TEM) DEPTH))))
    (AND ( MIN-DEPTH 0) (RETURN LINE IN-STRING DEPTH))
    (SETQ DEPTH NEXT-DEPTH
	  IN-STRING (AND (LISTP TEM) (CADDDR TEM)))))

;;; STOP-BP is place to give up, for preventing long futile searches
;;; BACK-OVER-SINGLEQUOTES-P if T means that a backward motion should
;;; move back over any singlequote-like characters before the open-paren.
;;; NO-UP-P means do not move over a ) to a higher level, note that NO-UP-P
;;; implies not FIXUP-P, since the fixup case of NO-UP-P would be the normal
;;; behaviour.
(DEFUN FORWARD-SEXP (BP &OPTIONAL (TIMES 1) FIXUP-P
				  (LEVEL 0) STOP-BP (BACK-OVER-SINGLEQUOTES-P T) NO-UP-P
			&AUX CH STRCH)
   (COND ((ZEROP TIMES) (COPY-BP BP))
	 ((PLUSP TIMES)
	  (LET ((STATE 'NORMAL)  ;STATE is NORMAL, STRING or ALPHABETIC.
		(TIME 0)
		(LAST-BP (OR STOP-BP (INTERVAL-LAST-BP *INTERVAL*))))
	    (CHARMAP-PER-LINE (BP LAST-BP (IF (OR FIXUP-P
						  (AND (EQ STATE 'ALPHABETIC)
						       ( LEVEL 0)
						       (= (1+ TIME) TIMES)))
					      (COPY-BP LAST-BP)
					      NIL))
		     ;; Per-line forms
		     ;; If at start of line and inside some parens,
		     ;; skip over some lines using memoized LISP-PARSE-LINE info.
		     ;; This is an invisible speed-up for the rest of this loop.
		     ((COND ((AND (ZEROP *FIRST-INDEX*) (> LEVEL 0))
			     (MULTIPLE-VALUE (LINE STRCH LEVEL)
			       (LISP-FORWARD-LIST-AUX LINE
						      (AND (EQ STATE 'STRING) STRCH)
						      LEVEL *LAST-LINE*))
			     (SETQ STATE (COND (STRCH 'STRING) (T 'NORMAL)))
			     (SETQ *THIS-IS-THE-LAST-LINE*
				   (EQ LINE *LAST-LINE*)))))
              RESTART
	      (LET ((SYNTAX (LIST-SYNTAX (SETQ CH (CHARMAP-CH-CHAR)))))
		(SELECTQ STATE
		  (ALPHABETIC
		   (SELECT SYNTAX
		     (LIST-ALPHABETIC)
		     (LIST-SLASH
		      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))
		     (OTHERWISE
                      (IF ( LEVEL 0)
                          (IF ( (SETQ TIME (1+ TIME)) TIMES)
                              (CHARMAP-RETURN (CHARMAP-BP-BEFORE))))
                      (SETQ STATE 'NORMAL)
		      (GO RESTART))))
		  (STRING
		   (SELECT SYNTAX
		    (LIST-DOUBLE-QUOTE
		      (COND ((= CH STRCH)
			     (IF ( LEVEL 0)
				 (IF ( (SETQ TIME (1+ TIME)) TIMES)
				     (CHARMAP-RETURN (CHARMAP-BP-AFTER))))
			     (SETQ STATE 'NORMAL))))
		     (LIST-SLASH
		      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))))
		  (NORMAL
		   (SELECT SYNTAX
		     (LIST-ALPHABETIC
		      (SETQ STATE 'ALPHABETIC))
		     (LIST-DELIMITER)
		     (LIST-SLASH
		      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL))
		      (SETQ STATE 'ALPHABETIC))
		     (LIST-COMMENT
		      (SETQ INDEX *LAST-INDEX*))
		     (LIST-DOUBLE-QUOTE
		      (SETQ STATE 'STRING STRCH CH))
		     (LIST-SINGLE-QUOTE)
		     (LIST-CLOSE
		      (SETQ LEVEL (1- LEVEL))
		      (COND ((AND NO-UP-P (< LEVEL 0))
			     (CHARMAP-RETURN NIL))
			    (( LEVEL 0)
                             (IF ( (SETQ TIME (1+ TIME)) TIMES)
                                 (CHARMAP-RETURN (CHARMAP-BP-AFTER)))))
                      (SETQ STATE 'NORMAL))
		     (LIST-OPEN
		      (SETQ LEVEL (1+ LEVEL))))))))))
         (T
	  (LET ((STATE 'NORMAL)
                (TIME 0)
		NEW-LINE-FLAG
		(FIRST-BP (OR STOP-BP (INTERVAL-FIRST-BP *INTERVAL*))))
            (RCHARMAP-PER-LINE (BP FIRST-BP (IF (OR FIXUP-P
						    (AND (OR (AND (EQ STATE 'ALPHABETIC) ( LEVEL 0))
							     (AND (EQ STATE 'SKIP-LEADING-SINGLE-QUOTES)
								  ( LEVEL 1)))
							 (= (1- TIME) TIMES)))
						(COPY-BP FIRST-BP)
						NIL))
		     ;; Per-line forms.  Work like those for forward case.
		     ((COND ((AND (NOT *FIRST-LINE-P*) (> LEVEL 0))
			     (MULTIPLE-VALUE (LINE STRCH LEVEL)
			       (LISP-BACKWARD-LIST-AUX LINE
						       (AND (EQ STATE 'STRING) STRCH)
						       LEVEL *LAST-LINE*))
			     (SETQ STATE (COND (STRCH 'STRING) (T 'NORMAL)))
			     (SETQ *THIS-IS-THE-LAST-LINE*
				   (EQ LINE *LAST-LINE*))))
		      (SETQ NEW-LINE-FLAG T))
	      ;; When we arrive on a line, skip back to start of any comment on that line.
	      (AND (PROG1 NEW-LINE-FLAG (SETQ NEW-LINE-FLAG NIL))
		   (NOT *FIRST-LINE-P*)
		   (SETQ INDEX (OR (LISP-FIND-COMMENT-START LINE *LAST-INDEX*) INDEX)))
              RESTART
              (COND ((AND (= (LIST-SYNTAX (RCHARMAP-CH-CHAR-BEFORE)) LIST-SLASH)
			  (DO ((SL NIL (NOT SL))
			       (BP (FORWARD-CHAR (RCHARMAP-BP-BEFORE) -1)
				   (FORWARD-CHAR BP -1)))
			      ((OR (NULL BP)
				   ( (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-SLASH))
			       SL)))
		     ;; Odd number of preceding slashes means non-special character
		     (RCHARMAP-DECREMENT)
		     (AND (EQ STATE 'NORMAL)
			  (SETQ STATE 'ALPHABETIC)))
                    (T
                     (LET ((SYNTAX (LIST-SYNTAX (SETQ CH (RCHARMAP-CH-CHAR)))))
                       (SELECTQ STATE
                         (ALPHABETIC
                          (SELECT SYNTAX
                            (LIST-ALPHABETIC)
                            (LIST-SINGLE-QUOTE)
                            (OTHERWISE
                             (IF ( LEVEL 0)
                                 (IF ( (SETQ TIME (1- TIME)) TIMES)
                                     (RCHARMAP-RETURN (RCHARMAP-BP-AFTER))))
                             (SETQ STATE 'NORMAL)
                             (GO RESTART))))
                         (STRING
                          (SELECT SYNTAX
                            (LIST-DOUBLE-QUOTE
                             (COND ((= CH STRCH)
                                    (IF ( LEVEL 0)
                                        (IF ( (SETQ TIME (1- TIME)) TIMES)
                                            (RCHARMAP-RETURN (RCHARMAP-BP-BEFORE))))
                                    (SETQ STATE 'NORMAL))))))
                         (NORMAL
                          (SELECT SYNTAX
                            (LIST-ALPHABETIC
                             (SETQ STATE 'ALPHABETIC))
                            (LIST-SLASH
                             ;; Crock.
                             (SETQ STATE 'ALPHABETIC))
                            (LIST-DOUBLE-QUOTE
                             (SETQ STATE 'STRING STRCH CH))
                            (LIST-CLOSE
                             (SETQ LEVEL (1+ LEVEL)))
                            (LIST-OPEN
			     (SETQ LEVEL (1- LEVEL))
			     (COND ((AND NO-UP-P (< LEVEL 0) (NOT FIXUP-P))
				    (RCHARMAP-RETURN NIL))
				   (( LEVEL 0)
				    (IF ( (SETQ TIME (1- TIME)) TIMES)
					(RCHARMAP-RETURN
					  (COND (BACK-OVER-SINGLEQUOTES-P
						 (BACKWARD-LEADING-SINGLE-QUOTES
						   (RCHARMAP-BP-BEFORE) FIRST-BP))
						(T (RCHARMAP-BP-BEFORE)))))))))))))))))))

;; After moving back past a sexp, move back past any singlequote-syntax chars
;; preceding the sexp.  We accept a BP to the beginning of the sexp,
;; and return a bp to the first singlequote of those that precede it.
;; We also accept a BP not to move back past.
;; We check for slashification of the singlequotes.
(DEFUN BACKWARD-LEADING-SINGLE-QUOTES (BP FIRST-BP)
  (CREATE-BP (BP-LINE BP)
	     (DO ((INDEX (BP-INDEX BP) (1- INDEX))
		  (STOP-INDEX (COND ((EQ (BP-LINE BP) (BP-LINE FIRST-BP)) (BP-INDEX FIRST-BP))
				    (T 0)))
		  (LINE (BP-LINE BP))
		  (SYNTAX) (SLASH-PARITY-ODD))
		 ((= INDEX STOP-INDEX) STOP-INDEX)
	       (SETQ SYNTAX (LIST-SYNTAX (AREF LINE (1- INDEX))))
	       (COND ((= SYNTAX LIST-SINGLE-QUOTE))
		     ;; We have found all the singlequotes.
		     ;; INDEX is the index of the first singlequote.
		     ((= SYNTAX LIST-SLASH)
		      ;; Don't worry about slashes if there weren't any singlequotes.
		      (AND (= INDEX (BP-INDEX BP))
			   (RETURN INDEX))
		      ;; Count the parity of slashes here.
		      (DO ((INDEX1 INDEX (1- INDEX1)))
			  ((= INDEX1 STOP-INDEX))
			(COND ((= (LIST-SYNTAX (AREF LINE (1- INDEX1))) LIST-SLASH)
			       (SETQ SLASH-PARITY-ODD (NOT SLASH-PARITY-ODD)))
			      (T (RETURN NIL))))
		      ;; If odd # of slashes, the first singlequote doesn't count as one.
		      (RETURN (COND (SLASH-PARITY-ODD (+ 1 INDEX))
				    ;; If even # of slashes, the first singlequote counts.
				    (T INDEX))))
		     (T (RETURN INDEX))))))

(DEFUN FORWARD-UP-STRING (BP ARG)
  (DO ((BP BP (SEARCH BP #/" ARG)))
      ((OR (NULL BP)
	   (NOT (LISP-BP-SYNTACTIC-CONTEXT BP)))
       BP)))

;;; For things with a standard interface, like KILL-COMMAND-INTERNAL
(DEFUN FORWARD-SEXP-NO-UP (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (FORWARD-SEXP BP TIMES FIXUP-P 0 NIL T T))
