;;;-*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Functions in this file know about bps, lines, and intervals.
;;; They use *INTERVAL* for their limit-checking.

;;; Standard motion functions.

(DEFUN FORWARD-CHAR (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES)
	 (COPY-BP BP))
	((> TIMES 0)
	 (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
	      (INDEX (BP-INDEX BP) 0)
	      (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	      (LAST-INDEX (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))))
	     (NIL)
	   (LET ((LL (LINE-LENGTH LINE))
		 (I (+ INDEX TIMES)))
	     (COND ((AND (EQ LINE LAST-LINE)
			 (> I LAST-INDEX))
		    (RETURN (IF FIXUP-P (CREATE-BP LINE LAST-INDEX) NIL)))
		   (( I LL)
		    (RETURN (CREATE-BP LINE I))))
	     (SETQ TIMES (- TIMES (- LL INDEX) 1)))))
	(T
	 (SETQ TIMES (- TIMES))
	 (DO ((LINE (BP-LINE BP))
	      (INDEX (- (BP-INDEX BP) TIMES))
	      (LINE-LENGTH (BP-INDEX BP))
	      (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
	      (FIRST-INDEX (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))))
	     (NIL)
	   (COND ((AND (EQ LINE FIRST-LINE) (< INDEX FIRST-INDEX))
		  (RETURN (IF FIXUP-P (CREATE-BP FIRST-LINE FIRST-INDEX) NIL)))
		 (( INDEX 0)
		  (RETURN (CREATE-BP LINE INDEX))))
	   (SETQ TIMES (- TIMES LINE-LENGTH 1)
		 LINE (LINE-PREVIOUS LINE)
		 LINE-LENGTH (LINE-LENGTH LINE)
		 INDEX (- LINE-LENGTH TIMES))))))

;;; Move forward TIMES characters, the way a program on ITS would count
;;; characters.  This means that CRs and LFs get counted separately; that
;;; is, a newline counts as two characters.  If the TIMES given would give
;;; a BP between a CR and an LF, which we do not represent, we return the
;;; position just after the newline.
(DEFUN FORWARD-ITS-CHAR (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES)
	 (COPY-BP BP))
	((> TIMES 0)
	 (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
	      (INDEX (BP-INDEX BP) 0)
	      (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	      (LAST-INDEX (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))))
	     (NIL)
	   (LET ((LL (LINE-LENGTH LINE))
		 (I (+ INDEX TIMES)))
	     (COND ((AND (EQ LINE LAST-LINE)
			 (> I LAST-INDEX))
		    (RETURN (IF FIXUP-P (CREATE-BP LINE LAST-INDEX) NIL)))
		   (( I LL)
		    (RETURN (CREATE-BP LINE I))))
	     (SETQ TIMES (MAX 0 (- TIMES (- LL INDEX) 2))))))
	(T
	 (SETQ TIMES (- TIMES))
	 (DO ((LINE (BP-LINE BP))
	      (INDEX (- (BP-INDEX BP) TIMES))
	      (LINE-LENGTH (BP-INDEX BP))
	      (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
	      (FIRST-INDEX (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))))
	     (NIL)
	   (COND ((AND (EQ LINE FIRST-LINE) (< INDEX FIRST-INDEX))
		  (RETURN (IF FIXUP-P (CREATE-BP FIRST-LINE FIRST-INDEX) NIL)))
		 (( INDEX 0)
		  (RETURN (CREATE-BP LINE INDEX))))
	   (SETQ TIMES (MAX 0 (- TIMES LINE-LENGTH 2))
		 LINE (LINE-PREVIOUS LINE)
		 LINE-LENGTH (LINE-LENGTH LINE)
		 INDEX (- LINE-LENGTH TIMES))))))

(DEFUN FORWARD-LINE (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES) (COPY-BP BP))
	((PLUSP TIMES)
	 (DO ((LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	      (LINE (BP-LINE BP) (LINE-NEXT LINE))
	      (I 0 (1+ I)))
	     (( I TIMES)
	      (CREATE-BP LINE 0))
	   (COND ((EQ LINE LAST-LINE)
		  (RETURN (IF FIXUP-P
			      (CREATE-BP LINE 0)
			      NIL))))))
	(T
	 (DO ((FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
	      (LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
	      (I 0 (1- I)))
	     (( I TIMES)
	      (CREATE-BP LINE (IF (EQ LINE FIRST-LINE)
				  (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
				  0)))
	   (COND ((EQ LINE FIRST-LINE)
		  (RETURN (IF FIXUP-P
			      (CREATE-BP LINE (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*)))
			      NIL))))))))

;; This is the function for moving from BP forward or backward over lists
;; as opposed to sexps.  That is, atoms are ignored (treated like spaces).
;; LEVEL can be positive to move up in the list structure.
;; To move down, supply DOWNP as T and make LEVEL minus the number of levels to move.
;; NO-UP-P means it is an error to move past an ) to a higher level
(DEFUN FORWARD-LIST (BP &OPTIONAL (TIMES 1) FIXUP-P (LEVEL 0) DOWNP NO-UP-P
                        &AUX (ORIGINAL-LEVEL LEVEL))
  (COND ((ZEROP TIMES) (COPY-BP BP))
	((PLUSP TIMES)
	 (LET ((STATE 'NORMAL)
	       (TIME 0)
	       (LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
	   (CHARMAP (BP LAST-BP (IF FIXUP-P LAST-BP NIL))
	    RESTART
	     (LET ((SYNTAX (LIST-SYNTAX (CHARMAP-CHAR))))
	       (SELECTQ STATE
		 (STRING
		  (SELECT SYNTAX
		    (LIST-DOUBLE-QUOTE
		     (SETQ STATE 'NORMAL))
		    (LIST-SLASH
		     (CHARMAP-INCREMENT (IF FIXUP-P LAST-BP NIL)))))
		 (NORMAL
		  (SELECT SYNTAX
		    (LIST-SLASH
		     (CHARMAP-INCREMENT (IF FIXUP-P LAST-BP NIL)))
		    (LIST-DOUBLE-QUOTE
		     (SETQ STATE 'STRING))
		    (LIST-CLOSE
		     (SETQ LEVEL (1- LEVEL))
		     (COND (DOWNP
			    (COND ((< LEVEL ORIGINAL-LEVEL)
				   (CHARMAP-RETURN (IF FIXUP-P LAST-BP NIL)))))
			   ((AND NO-UP-P (< LEVEL 0))
			    (CHARMAP-RETURN NIL))
			   (( LEVEL 0)
			    (IF ( (SETQ TIME (1+ TIME)) TIMES)
				(CHARMAP-RETURN (CHARMAP-BP-AFTER))))))
		    (LIST-OPEN
		     (COND ((AND ( (SETQ LEVEL (1+ LEVEL)) 0) DOWNP)
			    (IF ( (SETQ TIME (1+ TIME)) TIMES)
				(CHARMAP-RETURN (CHARMAP-BP-AFTER)))))))))))))
	(T
	 (LET ((STATE 'NORMAL)
	       (TIME 0)
	       (FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
	   (RCHARMAP (BP FIRST-BP (IF FIXUP-P FIRST-BP NIL))
	    RESTART
	     (LET ((SYNTAX (LIST-SYNTAX (RCHARMAP-CHAR))))
	       (SELECTQ STATE
		 (STRING
		  (SELECT SYNTAX
		    (LIST-DOUBLE-QUOTE
		     (SETQ STATE 'NORMAL))))
		 (NORMAL
		  (SELECT SYNTAX
		    (LIST-DOUBLE-QUOTE
		     (SETQ STATE 'STRING))
		    (LIST-CLOSE
		     (AND ( (SETQ LEVEL (1+ LEVEL)) 0) DOWNP
			  (IF ( (SETQ TIME (1- TIME)) TIMES)
			      (RCHARMAP-RETURN (RCHARMAP-BP-BEFORE)))))
		    (LIST-OPEN
		     (SETQ LEVEL (1- LEVEL))
		     (AND NO-UP-P (< LEVEL 0) (RCHARMAP-RETURN NIL))
		     (AND ( LEVEL 0) (NOT DOWNP)
			  (IF ( (SETQ TIME (1- TIME)) TIMES)
			      (RCHARMAP-RETURN (RCHARMAP-BP-BEFORE))))))))))))))

;Return true if the line starts a list which it doesn't end, i.e.
;contains an unmatched open paren
(DEFUN LINE-OPENS-PARENS (LINE)
  (DO ((I (IF (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
	      (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
	      0)
	  (1+ I))
       (LIM (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
		(BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
		(LINE-LENGTH LINE)))
       (STATE 'NORMAL)
       (LEVEL 0))
      (( I LIM) (> LEVEL 0))
    (LET* ((CH (LDB %%CH-CHAR (AREF LINE I)))
	   (SYNTAX (LIST-SYNTAX CH)))
      (SELECTQ STATE
	(STRING (SELECT SYNTAX
		  (LIST-DOUBLE-QUOTE (SETQ STATE 'NORMAL))
		  (LIST-SLASH (SETQ I (1+ I)))))
	(NORMAL (SELECT SYNTAX
		  (LIST-SLASH (SETQ I (1+ I)))
		  (LIST-DOUBLE-QUOTE (SETQ STATE 'STRING))
		  (LIST-CLOSE (SETQ LEVEL (MAX (1- LEVEL) 0)))
		  (LIST-OPEN (SETQ LEVEL (1+ LEVEL)))))))))

(DEFUN FORWARD-WORD (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES) (COPY-BP BP))
	((PLUSP TIMES)
	 (LET ((STATE NIL)
	       (TIME 0)
	       (LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
	   (CHARMAP (BP LAST-BP (IF (OR FIXUP-P
					(AND STATE (= (1+ TIME) TIMES)))
				    LAST-BP
				    NIL))
	     (LET ((SYNTAX (WORD-SYNTAX (CHARMAP-CHAR))))
	       (SELECTQ STATE
		 (NIL
		  (SELECT SYNTAX
		    (WORD-ALPHABETIC
		     (SETQ STATE T))))
		 (T
		  (SELECT SYNTAX
		    (WORD-DELIMITER
		     (SETQ TIME (1+ TIME))
		     (IF ( TIME TIMES)
			 (CHARMAP-RETURN (CHARMAP-BP-BEFORE))
			 (SETQ STATE NIL))))))))))
	(T
	 (LET ((STATE NIL)
	       (TIME 0)
	       (FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
	   (RCHARMAP (BP FIRST-BP (IF (OR FIXUP-P
					  (AND STATE (= (1- TIME) TIMES)))
				      FIRST-BP
				      NIL))
	     (LET ((SYNTAX (WORD-SYNTAX (RCHARMAP-CHAR))))
	       (SELECTQ STATE
		 (NIL
		  (SELECT SYNTAX
		    (WORD-ALPHABETIC
		     (SETQ STATE T))))
		 (T
		  (SELECT SYNTAX
		    (WORD-DELIMITER
		     (SETQ TIME (1- TIME))
		     (IF ( TIME TIMES)
			 (RCHARMAP-RETURN (RCHARMAP-BP-AFTER))
			 (SETQ STATE NIL))))))))))))

(DEFUN FORWARD-TO-WORD (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (*CATCH 'LOSSAGE
    (COND ((ZEROP TIMES) (COPY-BP BP))
	  ((PLUSP TIMES)
           (LET ((LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
             (COND ((> TIMES 1)
                    (SETQ BP (FORWARD-WORD BP (1- TIMES)))
                    (COND ((NULL BP)
                           (*THROW 'LOSSAGE (IF FIXUP-P LAST-BP NIL))))))
             (CHARMAP (BP LAST-BP (IF FIXUP-P LAST-BP NIL))
	       (LET ((SYNTAX (WORD-SYNTAX (CHARMAP-CHAR))))
                 (SELECT SYNTAX
                    (WORD-ALPHABETIC
                     (CHARMAP-RETURN (CHARMAP-BP-BEFORE))))))))
          (T
           (LET ((FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
             (COND ((< TIMES -1)
                    (SETQ BP (FORWARD-WORD BP (1+ TIMES)))
                    (COND ((NULL BP)
                           (*THROW 'LOSSAGE (IF FIXUP-P FIRST-BP NIL))))))
	     (RCHARMAP (BP FIRST-BP (IF FIXUP-P FIRST-BP NIL))
	       (LET ((SYNTAX (WORD-SYNTAX (RCHARMAP-CHAR))))
                 (SELECT SYNTAX
                    (WORD-ALPHABETIC
                     (RCHARMAP-RETURN (RCHARMAP-BP-AFTER)))))))))))

(DEFUN FORWARD-DEFUN (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES) (COPY-BP BP))
	((PLUSP TIMES)
	 (DO-NAMED LUPO
	     ((I 0 (1+ I)))
	     (( I TIMES)
	      BP)
	   (DO () (NIL)
	     (SETQ BP (BEG-LINE BP 1))
	     (COND ((NULL BP)
		    (RETURN-FROM LUPO (IF FIXUP-P
					  (COPY-BP (INTERVAL-LAST-BP *INTERVAL*))
					  NIL)))
		   ((= (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-OPEN)
		    (RETURN NIL))))))
	(T
	 (DO-NAMED LUPO
	     ((I 0 (1- I)))
	     (( I TIMES)
	      BP)
	   (DO ((FIRSTP T NIL)) (NIL)
	     (SETQ BP (BEG-LINE BP (IF (AND FIRSTP (NOT (BEG-LINE-P BP)))
				       0
				       -1)))
	     (COND ((NULL BP)
		    (RETURN-FROM LUPO (IF FIXUP-P
					  (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))
					  NIL)))
		   ((= (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-OPEN)
		    (RETURN NIL))))))))

(DEFUN FORWARD-PAGE (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES) (COPY-BP BP))
	((PLUSP TIMES)
	 (LET ((STOP-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
	       (FIRST-LINE (BP-LINE BP)))
	   (COND ((EQ FIRST-LINE STOP-LINE)
		  (AND FIXUP-P (COPY-BP (INTERVAL-LAST-BP *INTERVAL*))))
		 (T (DO ((LINE (LINE-NEXT FIRST-LINE) (LINE-NEXT LINE)))
			((EQ LINE STOP-LINE)
			 (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))
		      (COND ((AND ( (LINE-LENGTH LINE) 1)
				  (= #\FF (LDB %%CH-CHAR (AREF LINE 0))))
			     (AND ( (SETQ TIMES (1- TIMES)) 0)
				  (RETURN (CREATE-BP LINE 1))))))))))
	(T
	 (LET ((STOP-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
	       (FIRST-LINE (BP-LINE BP)))
	   (COND ((EQ FIRST-LINE STOP-LINE)
		  (AND FIXUP-P (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))))
		 (T (DO ((LINE (LINE-PREVIOUS FIRST-LINE) (LINE-PREVIOUS LINE)))
			((EQ LINE STOP-LINE)
			 (AND FIXUP-P (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))))
		      (COND ((AND ( (LINE-LENGTH LINE) 1)
				  (= #\FF (LDB %%CH-CHAR (AREF LINE 0))))
			     (AND ( (SETQ TIMES (1+ TIMES)) 0)
				  (RETURN (CREATE-BP LINE 1))))))))))))

(DEFUN FORWARD-INTERVAL (BP &OPTIONAL (TIMES 1) FIXUP-P)
  BP FIXUP-P					;Never out of range
  (COPY-BP (IF (MINUSP TIMES)
	       (INTERVAL-FIRST-BP *INTERVAL*)
	       (INTERVAL-LAST-BP *INTERVAL*))))

(DEFUN FORWARD-PARAGRAPH (BP &OPTIONAL (TIMES 1) FIXUP-P
			     &AUX BACKWARD-P
				  (FILL-PREFIX-P (PLUSP (STRING-LENGTH *FILL-PREFIX*)))
				  BLANK-P PREV-BLANK-P)
  (AND (MINUSP TIMES) (SETQ TIMES (- TIMES) BACKWARD-P T))
  (COND ((NOT BACKWARD-P)			;Move to the beginning of a line
	 (SETQ BP (BEG-LINE BP)))
	((NOT (BEG-LINE-P BP))
	 (SETQ BP (BEG-LINE BP 1 T))))
  (DO ((I 0 (1+ I)))
      ((OR (NULL BP) ( I TIMES)) BP)
    (SETQ BLANK-P T)
    (DO ((FIRST-P (IF (AND BACKWARD-P (BP-= BP (INTERVAL-LAST-BP *INTERVAL*))) 1 0)
		  (1+ FIRST-P)))
	(NIL)
      (SETQ PREV-BLANK-P BLANK-P BLANK-P NIL)
      (AND (SETQ BLANK-P (OR (LINE-BLANK-P (BP-LINE BP))
			     (AND (NOT FILL-PREFIX-P)	;If no fill prefix
				  (BP-LOOKING-AT-LIST BP *TEXT-JUSTIFIER-ESCAPE-LIST*)
				  (OR (BP-LOOKING-AT-LIST BP *PARAGRAPH-DELIMITER-LIST*)
				      (BP-LOOKING-AT-LIST BP *PAGE-DELIMITER-LIST*)))))
	   (NOT PREV-BLANK-P)
	   (OR (> FIRST-P 1)
	       (NOT BACKWARD-P))
	   (RETURN))
      (COND ((NOT (IF BACKWARD-P
		      (OR (SETQ BP (BEG-LINE BP -1)) (RETURN))
		      (OR (SETQ BP (BEG-LINE BP 1)) (RETURN))
		      (NOT BLANK-P))))
	    (FILL-PREFIX-P
	     (OR (LOOKING-AT BP *FILL-PREFIX*) (RETURN)))
	    (T
	     (AND (OR (BP-LOOKING-AT-LIST BP *PARAGRAPH-DELIMITER-LIST*)
		      (BP-LOOKING-AT-LIST BP *PAGE-DELIMITER-LIST*))
		  (NOT (BP-LOOKING-AT-LIST BP *TEXT-JUSTIFIER-ESCAPE-LIST*))
		  (RETURN))))))
  (COND (BP
	 (AND BACKWARD-P BLANK-P (NOT PREV-BLANK-P)
	      (SETQ BP (BEG-LINE BP 1 T)))
	 (LET ((BP1 (BEG-LINE BP -1)))
	   (AND BP1 (LINE-BLANK-P (BP-LINE BP1))
		(SETQ BP BP1)))))
  (OR BP
      (COND ((NOT FIXUP-P) NIL)
	    (BACKWARD-P (INTERVAL-FIRST-BP *INTERVAL*))
	    (T (INTERVAL-LAST-BP *INTERVAL*)))))

(DEFUN FORWARD-OVER-BLANK-OR-TEXT-JUSTIFIER-LINES (BP)
  (DO ((BP BP (BEG-LINE BP 1)))
      ((OR (NULL BP)
	   (NOT (OR (LINE-BLANK-P (BP-LINE BP))
		    (AND (BP-LOOKING-AT-LIST BP *TEXT-JUSTIFIER-ESCAPE-LIST*)
			 (OR (BP-LOOKING-AT-LIST BP *PARAGRAPH-DELIMITER-LIST*)
			     (BP-LOOKING-AT-LIST BP *PAGE-DELIMITER-LIST*))))))
       (OR BP (INTERVAL-LAST-BP *INTERVAL*)))))

(DEFUN FORWARD-ATOM (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (ATOM-WORD-SYNTAX-BIND
    (FORWARD-WORD BP TIMES FIXUP-P)))

(DEFUN FORWARD-SENTENCE (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES) (COPY-BP BP))
	((PLUSP TIMES)
	 (DO ((LAST-BP (INTERVAL-LAST-BP *INTERVAL*))
	      (TIME 0 (1+ TIME))
	      (STATE NIL)
	      (CH))
	     (( TIME TIMES) BP)
	   (SETQ BP (FORWARD-OVER '(#\CR) BP))	;Skip initial blank lines
	   (SETQ BP (CHARMAP (BP LAST-BP (AND (OR STATE FIXUP-P) LAST-BP))
		      (SETQ CH (CHARMAP-CH-CHAR))
		      (AND STATE		;If special character last time...
			   (COND ((OR (= CH #\CR)	;"<cr><cr>" ".<cr>" or ". <cr>" win
				      (AND (EQ STATE 'SP) (= CH #\SP)))	;".  " wins
				  (CHARMAP-RETURN (COND ((EQ STATE 'DOT)	;".<cr>"
							 (CHARMAP-BP-BEFORE))
							(T (FORWARD-CHAR (CHARMAP-BP-BEFORE)
									 -1)))))
				 ((AND (EQ STATE 'DOT) (= CH #\SP))	;". "
				  (SETQ STATE 'SP))
				 (T (SETQ STATE NIL))))
		      (COND ((= CH #\CR)	;If at end of line, check for another
			     (SETQ STATE 'CR))	;<cr> next time
			    ((MEMQ CH '(#/. #/! #/?))
			     ;;Skip over closing frobs that might contain the sentence
			     (DO NIL
				 ((NOT (MEMQ CH '(#/" #/' #/) #/]))))
			       (CHARMAP-INCREMENT (AND FIXUP-P LAST-BP))
			       (SETQ CH (CHARMAP-CH-CHAR)))
			     (SETQ STATE 'DOT)))))))
	(T
	 (DO ((START-BP (INTERVAL-FIRST-BP *INTERVAL*))
	      (TIME 0 (1- TIME))
	      (STATE NIL)
	      (NFROBS)
	      (CH))
	     (( TIME TIMES) (FORWARD-OVER *WHITESPACE-CHARS* (FORWARD-CHAR BP NFROBS)))
	   (SETQ BP (BACKWARD-OVER '(#\CR #\SP #/" #/' #/) #/]) BP)
		 NFROBS 0)
	   (SETQ BP (RCHARMAP (BP START-BP (AND FIXUP-P START-BP))
		      (SETQ CH (RCHARMAP-CH-CHAR))
		      (COND ((MEMQ STATE '(CR SPSP))
			     (DO NIL
				 ((NOT (MEMQ CH '(#/" #/' #/) #/]))))
			       (RCHARMAP-DECREMENT (AND FIXUP-P START-BP))
			       (SETQ CH (RCHARMAP-CH-CHAR)
				     NFROBS (1+ NFROBS)))
			     (AND (OR (MEMQ CH '(#/. #/! #/?))
				      (AND (= CH #\CR) (EQ STATE 'CR) (ZEROP NFROBS)))
				  (RCHARMAP-RETURN (RCHARMAP-BP-AFTER)))
			     (SETQ STATE NIL
				   NFROBS 0)))
		      (COND ((EQ STATE 'SP)
			     (SETQ STATE (AND (= CH #\SP) 'SPSP)))
			    ((= CH #\SP)
			     (SETQ STATE 'SP))
			    ((= CH #\CR)
			     (SETQ STATE 'CR)))))
	   (OR BP (RETURN NIL))))))

;;; Not-so-standard motion functions.

;;; Return an interval surrounding the DEFUN that BP is in, or NIL if it fails.

(DEFUN DEFUN-INTERVAL (BP &OPTIONAL (TIMES 1) FIXUP-P (COMMENTS-P T))
  (PROG (BP1 BP2 BP3 BP4 SBP)
	(COND ((NULL (SETQ BP1 (FORWARD-DEFUN BP -1)))
	       (SETQ BP1 (BEG-LINE BP 0))
	       (COND ((= (LIST-SYNTAX (BP-CHAR BP1)) LIST-OPEN)
		      (GO BUFBEG1))
		     (T (GO BUFBEG)))))
	(OR (SETQ BP2 (FORWARD-SEXP BP1 TIMES))
	    (IF (NOT FIXUP-P) (RETURN NIL)
		(SETQ BP2 (BEG-LINE (BACKWARD-OVER-COMMENT-LINES (FORWARD-DEFUN BP1 1 T))
				    -1))))
	(OR (BP-< (END-LINE BP2) BP)
	    ;; We were in the middle of the defun.
	    (GO FOUND))
	(SETQ BP BP1)
     BUFBEG
	(COND ((NULL (SETQ BP1 (FORWARD-DEFUN BP)))
	       (AND BP2 (SETQ BP1 (FORWARD-DEFUN BP2 -1))
		    (GO FOUND))              ;At end of buffer, take previous
	       (RETURN NIL)))
     BUFBEG1
	(OR (SETQ BP2 (FORWARD-SEXP BP1 TIMES)) (RETURN NIL))
     FOUND
	;; At this point, BP1 and BP2 surround a "defun".  Now we should grab any
	;; comment lines and intervening blank lines before the beginning, and the
	;; rest of the last line.
	(SETQ SBP BP1)			;Save real starting line
     CONTIN
	(AND COMMENTS-P (SETQ BP1 (BACKWARD-OVER-COMMENT-LINES BP1)))
	(SETQ BP3 (FORWARD-OVER *BLANKS* BP2))
	(AND BP3 (OR (= (LIST-SYNTAX (BP-CHAR BP3)) LIST-COMMENT)
		     (= (BP-CH-CHAR BP3) #\CR))
	     (SETQ BP2 (BEG-LINE BP2 1 T)))
	;; Now try to find any extra close-parens because of a LOCAL-DECLARE
	(SETQ BP3 (FORWARD-OVER '(#/)) BP2))
	(AND (NOT (BP-= BP2 BP3))
	     (SETQ BP4 (FORWARD-SEXP BP3 (- TIMES)))
	     (BP-< BP4 BP1)
	     (SETQ BP1 BP4 BP2 BP3)
	     (GO CONTIN))
	;; Now try to find a package prefix
	(SETQ BP3 (BACKWARD-OVER *WHITESPACE-CHARS* BP1)
	      BP4 (FORWARD-WORD BP3 -1 T))
	(COND ((AND (CHAR-EQUAL (BP-CHAR-BEFORE BP3) #/:) (BEG-LINE-P BP4))
	       (SETQ BP1 BP4)
	       (GO CONTIN)))
	(RETURN (CREATE-INTERVAL BP1 BP2) SBP)))

;; Decide how much text before a defun to include with the defun when marking the defun.
;; It moves over all comment lines and intervening blank lines,
;; and also over one blank line before them (unless TOP-BLANK-P is NIL).
;; It includes all blank lines at the beginning of the interval so
;; as not to leave them orphaned.
;; If there is List structure before the defun that encloses it, e.g.
;; a LOCAL-DECLARE, it gets included.

(DEFUN BACKWARD-OVER-COMMENT-LINES (BP &OPTIONAL (TOP-BLANK-P T)
				       &AUX (LAST-GOOD-LINE (BP-LINE BP)))
  (DO ((LINE (LINE-PREVIOUS (BP-LINE BP)) (LINE-PREVIOUS LINE)))
      ((NULL LINE)
       (SETQ LAST-GOOD-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
    (SELECTQ (LINE-TYPE LINE)
	(:BLANK)
	(:COMMENT (SETQ LAST-GOOD-LINE LINE))
	(:NORMAL (IF (LINE-OPENS-PARENS LINE) (SETQ LAST-GOOD-LINE LINE)
		     (RETURN)))
        (OTHERWISE (RETURN))))
  (COND ((EQ LAST-GOOD-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
	((NOT TOP-BLANK-P))
	((MEMQ (LINE-TYPE (LINE-PREVIOUS LAST-GOOD-LINE)) ':(BLANK FORM))
	 (SETQ LAST-GOOD-LINE (LINE-PREVIOUS LAST-GOOD-LINE))))
  (IF (EQ (LINE-TYPE LAST-GOOD-LINE) ':FORM) (END-OF-LINE LAST-GOOD-LINE)
      (BEG-OF-LINE LAST-GOOD-LINE)))

;; Return a bp to the front of the first non-blank non-comment line after BP.
;; If there is non-blank non-comment data following BP on the same line
;; we return a pointer to that.
;; This is good for finding the next interesting piece of lisp code after a point.
(DEFUN SKIP-OVER-BLANK-LINES-AND-COMMENTS (BP &OPTIONAL FIXUP-P)
  (SETQ BP (FORWARD-OVER *BLANKS* BP))
  (AND BP (OR (= (BP-CH-CHAR BP) #/;)
	      (= (BP-CH-CHAR BP) #\CR))
       (DO () (NIL)
	 (SETQ BP (BEG-LINE BP 1))
	 (OR BP (RETURN NIL))
	 (SELECTQ (LINE-TYPE (BP-LINE BP))
	   (:BLANK)
	   (:COMMENT)
	   (OTHERWISE (RETURN BP)))))
  (OR BP (AND FIXUP-P (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))))

(DEFUN BEG-LINE (BP &OPTIONAL (TIMES 0) FIXUP-P)
  (COND (( TIMES 0)
	 (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
	      (I TIMES (1- I))
	      (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
	     (NIL) 
	   (COND ((EQ LINE LAST-LINE)
		  (RETURN (IF (OR ( I 0) FIXUP-P)
			      (CREATE-BP LINE (IF ( I 0) 0 (LINE-LENGTH LINE)))
			      NIL)))
		 (( I 0)
		  (RETURN (CREATE-BP LINE 0))))))
	(T
	 (DO ((LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
	      (I TIMES (1+ I))
	      (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
	     (NIL)
	   (COND ((EQ LINE FIRST-LINE)
		  (RETURN (IF (OR ( I 0) FIXUP-P)
			      (CREATE-BP LINE (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*)))
			      NIL)))
		 (( I 0)
		  (RETURN (CREATE-BP LINE 0))))))))

(DEFUN END-LINE (BP &OPTIONAL (TIMES 0) FIXUP-P)
  (COND (( TIMES 0)
	 (DO ((LINE (BP-LINE BP) (LINE-NEXT LINE))
	      (I TIMES (1- I))
	      (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
	     (NIL)
	   (COND ((EQ LINE LAST-LINE)
		  (RETURN (IF (OR ( I 0) FIXUP-P)
			      (CREATE-BP LINE (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*)))
			      NIL)))
		 (( I 0)
		  (RETURN (CREATE-BP LINE (LINE-LENGTH LINE)))))))
	(T
	 (DO ((LINE (BP-LINE BP) (LINE-PREVIOUS LINE))
	      (I TIMES (1+ I))
	      (FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
	     (NIL)
	   (COND ((EQ LINE FIRST-LINE)
		  (RETURN (IF (OR ( I 0) FIXUP-P)
			      (CREATE-BP LINE (LINE-LENGTH LINE))
			      NIL)))
		 (( I 0)
		  (RETURN (CREATE-BP LINE (LINE-LENGTH LINE)))))))))

(DEFUN FORWARD-OVER (LIST BP)
  (CHARMAP (BP (INTERVAL-LAST-BP *INTERVAL*) (CHARMAP-BP-BEFORE))
    (IF (NOT (MEMQ (CHARMAP-CH-CHAR) LIST))
	(CHARMAP-RETURN (CHARMAP-BP-BEFORE)))))

(DEFUN BACKWARD-OVER (LIST BP)
  (RCHARMAP (BP (INTERVAL-FIRST-BP *INTERVAL*) (RCHARMAP-BP-AFTER))
    (IF (NOT (MEMQ (RCHARMAP-CH-CHAR) LIST))
	(RCHARMAP-RETURN (RCHARMAP-BP-AFTER)))))

(DEFUN DELETE-OVER (LIST BP)
  (DELETE-INTERVAL BP (FORWARD-OVER LIST BP) T))

(DEFUN DELETE-BACKWARD-OVER (LIST BP)
  (DELETE-INTERVAL (BACKWARD-OVER LIST BP) BP T))

(DEFUN DELETE-AROUND (LIST BP)
  (DELETE-INTERVAL (BACKWARD-OVER LIST BP) (FORWARD-OVER LIST BP) T))
