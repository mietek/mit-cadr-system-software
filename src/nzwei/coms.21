;;; Zwei searching and replacing commands -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; see ZWEI;COMA for comments.

;;; Character search

(DEFCOM COM-CHAR-SEARCH "Search for a single character.
Special characters:
C-A	Do string search
C-B	Go to beginning first
C-E	Go to end first
C-F	Put the line containing the search object at the top of the screen
C-R	Search backwards
C-S	Repeat the last search." (KM)
   (CHAR-SEARCH-INTERNAL NIL))

(DEFCOM COM-REVERSE-CHAR-SEARCH "Search backward for a single character.
Special characters:
C-A	Do string search
C-B	Go to beginning first
C-E	Go to end first
C-F	Put the line containing the search object at the top of the screen
C-R	Repeat the last search
C-S	Ditto." (KM)
   (CHAR-SEARCH-INTERNAL T))

(DEFUN CHAR-SEARCH-INTERNAL (REVERSEP)
  (UNWIND-PROTECT
    (PROG (XCHAR CHAR UCHAR BJP ZJP TOP-P STRING BP FAILED-P
	   (ORIG-PT (COPY-BP (POINT))) (ARG *NUMERIC-ARG*))
	(AND (MINUSP ARG) (SETQ REVERSEP (NOT REVERSEP) ARG (- ARG)))
     LOOP (COND ((OR FAILED-P			;Force redisplay on failing search
		     (NULL (SETQ XCHAR (FUNCALL STANDARD-INPUT ':TYI-NO-HANG))))
		 (TYPEIN-LINE-WITH-REDISPLAY "")
		 (AND BJP (TYPEIN-LINE-MORE "Begin "))
		 (AND ZJP (TYPEIN-LINE-MORE "End "))
		 (AND TOP-P
		      (TYPEIN-LINE-MORE "Top Line "))
		 (AND REVERSEP (TYPEIN-LINE-MORE "Reverse "))
		 (TYPEIN-LINE-MORE "Search: ")))
	  (COND ((NOT FAILED-P)
		 (SETQ CHAR (OR XCHAR
				(TYPEIN-LINE-ACTIVATE
				  (FUNCALL STANDARD-INPUT ':TYI))))
		 (SETQ UCHAR (CHAR-UPCASE CHAR))
		 (COND ((= UCHAR #/A)
			(RETURN (COM-STRING-SEARCH-INTERNAL REVERSEP BJP ZJP TOP-P)))
		       ((AND (= UCHAR #/R) (NOT REVERSEP))
			(SETQ REVERSEP (NOT REVERSEP))
			(GO LOOP))
		       ((= UCHAR #/B)
			(SETQ BJP T ZJP NIL REVERSEP NIL)
			(GO LOOP))
		       ((= UCHAR #/E)
			(SETQ ZJP T BJP NIL REVERSEP T)
			(GO LOOP))
		       ((= UCHAR #/F)
			(SETQ *CENTERING-FRACTION* 0.0s0 TOP-P T)
			(GO LOOP))
		       ((= UCHAR #/G)
			(BEEP)
			(GO QUIT))
		       ((OR (= UCHAR #/S)
			    (AND REVERSEP (= UCHAR #/R)))
			(OR *SEARCH-RING* (BARF))
			(SETQ STRING (CAR *SEARCH-RING*)))
		       ((> CHAR 220)		;Random control character
			(BEEP)
			(GO LOOP))
		       (T
			(SETQ STRING CHAR)
			(SEARCH-RING-PUSH CHAR)))))
	  (AND (OR (NULL XCHAR) FAILED-P)
	       (IF (NUMBERP STRING)
		   (TYPEIN-LINE-MORE "~C" STRING)
		   (TYPEIN-LINE-MORE "~A" STRING)))
	  (SETQ BP (AND (NOT FAILED-P)
			(DO ((I 0 (1+ I))
			     (BP (COND (BJP (INTERVAL-FIRST-BP *INTERVAL*))
				       (ZJP (INTERVAL-LAST-BP *INTERVAL*))
				       (T (POINT)))
				 (SEARCH BP STRING REVERSEP)))
			    ((OR ( I ARG) (NULL BP))
			     BP))))
	  (COND (BP
		 (MOVE-BP (POINT) BP))
		((OR FAILED-P (NULL XCHAR))
		 (TYPEIN-LINE-MORE " Search failed.")
		 (BARF))
		(T
		 (SETQ FAILED-P T)
		 (GO LOOP)))			;Failed search typed ahead
    QUIT (MAYBE-PUSH-POINT ORIG-PT)
	 (RETURN DIS-BPS))
    (FUNCALL *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW)))

(DEFCOM COM-STRING-SEARCH "Search for a specified string." (KM)
    (COM-STRING-SEARCH-INTERNAL NIL NIL NIL NIL))

(DEFCOM COM-REVERSE-STRING-SEARCH "Search backward for a specified string." (KM)
    (COM-STRING-SEARCH-INTERNAL T NIL NIL NIL))

;; A special hack is needed to stop an altmode that follows a S from searching.
;; That is what HACK1 and HACK2 are for.
(DEFUN COM-STRING-SEARCH-INTERNAL (REVERSEP BJP ZJP TOP-P &AUX TEM)
  (UNWIND-PROTECT
    (PROG ((STRING (MAKE-ARRAY NIL ART-STRING 200 NIL '(0)))
	   (ORIG-PT (COPY-BP (POINT)))
	   XCHAR CHAR WORD-P HACK1 HACK2 ECHOED-P FAILED-P)
       REDIS (COND ((NULL (SETQ XCHAR (AND (NOT ECHOED-P)
					   (FUNCALL STANDARD-INPUT ':TYI-NO-HANG))))
		    (SETQ ECHOED-P T)			;Started to echo now
		    (TYPEIN-LINE-WITH-REDISPLAY "")
		    (AND BJP (TYPEIN-LINE-MORE "Begin "))
		    (AND ZJP (TYPEIN-LINE-MORE "End "))
		    (AND TOP-P (TYPEIN-LINE-MORE "Top Line "))
		    (AND REVERSEP (TYPEIN-LINE-MORE "Reverse "))
		    (TYPEIN-LINE-MORE (IF WORD-P "Word search: " "String search: "))
		    (TYPEIN-LINE-MORE "~A" STRING)))
	     (AND FAILED-P (GO FAILED))
	     (GO LOP1)
        LOOP (SETQ XCHAR (AND (NOT ECHOED-P)
			      (FUNCALL STANDARD-INPUT ':TYI-NO-HANG)))
	LOP1 (SETQ CHAR (OR XCHAR
			    (TYPEIN-LINE-ACTIVATE
			      (FUNCALL STANDARD-INPUT ':TYI))))
             (SETQ HACK2 HACK1 HACK1 NIL)
	     (COND ((BIT-TEST 400 CHAR)
		    (SETQ CHAR (CHAR-UPCASE (LOGAND 377 CHAR)))
		    (SELECT CHAR
                         (#/B (SETQ BJP T ZJP NIL REVERSEP NIL)
			      (GO REDIS))
                         (#/E (SETQ BJP NIL ZJP T REVERSEP T)
			      (GO REDIS))
			 (#/F (SETQ *CENTERING-FRACTION* 0.0s0 TOP-P T)
			      (GO REDIS))
                         (#/G (TYPEIN-LINE "")
                              (BARF))
                         (#/D (SETQ TEM (SEARCH-RING-POP))
			      (COND ((NUMBERP TEM)
				     (SETQ STRING (MAKE-ARRAY NIL ART-STRING
							      200 NIL '(1)))
				     (ASET TEM STRING 0))
				    (T (SETQ STRING TEM)))
			      (GO REDIS))
                         (#/L (GO REDIS))
                         (#/Q (TYPEIN-LINE-ACTIVATE
			       (SETQ CHAR (LOGAND 377 (FUNCALL STANDARD-INPUT ':TYI))))
			      (GO NORMAL))
                         (#/R (SETQ REVERSEP (NOT REVERSEP))
			      (GO REDIS))
                         (#/S (LET ((TEM (FUNCALL (IF WORD-P #'WORD-SEARCH #'SEARCH )
                                                  (COND (ZJP (INTERVAL-LAST-BP *INTERVAL*))
                                                        (BJP (INTERVAL-FIRST-BP *INTERVAL*))
                                                        (T (POINT)))
                                                  (COND ((AND (EQUAL "" STRING)
                                                              *SEARCH-RING*)
                                                         (CAR *SEARCH-RING*))
                                                        (T STRING))
                                                  REVERSEP)))
                                (COND ((NULL TEM)
                                       ;; Next line commented out for Emacs compatibility
                                       ;(BEEP)
				       ;; Comment this BARF instead to stay in search if fail
				       ;; But don't forget to update search default ring
				       (OR (EQUAL "" STRING)
					   (SEARCH-RING-PUSH STRING))
				       (GO FAILED)
                                       )
				      (T (MOVE-BP (POINT) TEM)
					 (MUST-REDISPLAY *WINDOW* DIS-BPS)
					 (AND (WINDOW-READY-P *WINDOW*)	;Minibuffer
					      (REDISPLAY *WINDOW* ':POINT))
					 (SETQ BJP NIL ZJP NIL)
					 (AND TOP-P
					      (SETQ *CENTERING-FRACTION* 0.0s0))
					 (SETQ HACK1 T))))
			      (IF (NULL XCHAR)
				  (GO LOOP)
				  (SETQ ECHOED-P T)
				  (GO REDIS)))
                         (#/U (STORE-ARRAY-LEADER 0 STRING 0)
                              (GO REDIS))
                         (#/W (SETQ WORD-P T)
                              (GO REDIS))
                         (#/Y (SETQ TEM (CAR *SEARCH-RING*))
			      (IF (NUMBERP TEM)
				  (ARRAY-PUSH STRING CHAR)
				  (DOTIMES (I (STRING-LENGTH TEM))
				    (ARRAY-PUSH STRING (AREF TEM I))))
			      (GO REDIS))
			 (OTHERWISE (BEEP)
                                    (GO REDIS))))
		   ((= CHAR #\RUBOUT)
		    (OR (ZEROP (ARRAY-LEADER STRING 0))
			(ARRAY-POP STRING))
		    (GO REDIS))
		   ((= CHAR #/)
		    (OR XCHAR
			(TYPEIN-LINE-MORE "~C" CHAR))
		    (OR (EQUAL "" STRING)
			(SEARCH-RING-PUSH STRING))
		    (OR HACK2
		        (DO ((FCN (IF WORD-P #'WORD-SEARCH #'SEARCH))
			     (ARG (ABS *NUMERIC-ARG*) (1- ARG))
			     (KEY (COND ((AND (EQUAL "" STRING)
					      *SEARCH-RING*)
					 (CAR *SEARCH-RING*))
					(T STRING)))
			     (BP (COND (ZJP (INTERVAL-LAST-BP *INTERVAL*))
				       (BJP (INTERVAL-FIRST-BP *INTERVAL*))
				       (T (POINT)))))
			    (( ARG 0) (MOVE-BP (POINT) BP))
			  (OR (SETQ BP (FUNCALL FCN BP KEY REVERSEP))
			      (GO FAILED))))
		    (MAYBE-PUSH-POINT ORIG-PT)
		    (RETURN DIS-BPS)))
	     (SETQ CHAR (LOGAND 377 CHAR))
      NORMAL (ARRAY-PUSH STRING CHAR)
             (IF XCHAR
		 (GO REDIS)
		 (SETQ ECHOED-P T)			;Started to echo
		 (TYPEIN-LINE-MORE "~C" CHAR)
		 (GO LOOP))
      FAILED (COND (XCHAR			;Typed ahead failing search, force redisplay
		    (SETQ FAILED-P T ECHOED-P T)
		    (GO REDIS))
		   (FAILED-P			;Typed ahead last time
		    (TYPEIN-LINE-MORE "")))
	     (TYPEIN-LINE-MORE " Search failed.")
	     (BARF))
    (FUNCALL *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW)))

;;; Incremental search.

(DEFCOM COM-INCREMENTAL-SEARCH "Search for character string.
As characters are typed in the accumulated string is displayed and searched for.
You can rubout characters.  Use Q to quote, S to repeat the search with the same
string, R to search backwards.  If S or R is the first character typed, the
previous search string is used again." (KM)
   (INCREMENTAL-SEARCH (< *NUMERIC-ARG* 0)))

(DEFCOM COM-REVERSE-INCREMENTAL-SEARCH "Reverse search for character string.
As characters are typed in the accumulated string is displayed and searched for.
You can rubout characters.  Use Q to quote, S to repeat the search with the same
string, R to search backwards.  If S or R is the first character typed, the
previous search string is used again." (KM)
   (INCREMENTAL-SEARCH (> *NUMERIC-ARG* 0)))

;;; Kludgey incremental search fixed arrays.
(DEFVAR *IS-STRING*)
(DEFVAR *IS-BP*)
(DEFVAR *IS-STATUS*)
(DEFVAR *IS-REVERSE-P*)
(DEFVAR *IS-POINTER*)
(DEFVAR *IS-OPERATION*)

(DEFUN INITIALIZE-INCREMENTAL-SEARCH-GLOBALS ()
  (SETQ *IS-STRING* (MAKE-ARRAY NIL ART-STRING 200 NIL '(0))
	;; All of the arrays below constitute a push-down stack.
	*IS-BP* (MAKE-ARRAY NIL ART-Q 200 NIL '(0))
	;; STATUS is NIL for a failing search, T for a successful one,
	;; and :GO for one that is still looking.
	*IS-STATUS* (MAKE-ARRAY NIL ART-Q 200 NIL '(0))
	;; T if the search is reverse at this level.
	*IS-REVERSE-P* (MAKE-ARRAY NIL ART-Q 200 NIL '(0))
	;; This points to the end of the part of *IS-STRING* active at this level.
	*IS-POINTER* (MAKE-ARRAY NIL ART-Q 200 NIL '(0))
	;; This is what sort of thing the char at this level is:
	;; :NORMAL, :REVERSE or :REPEAT.
	*IS-OPERATION* (MAKE-ARRAY NIL ART-Q 200 NIL '(0))
	))

(DEFMACRO PUSH-ISEARCH-STATUS ()
    '(PUSH-ISEARCH-STATUS-1 (SETQ P (1+ P))))

(DEFUN PUSH-ISEARCH-STATUS-1 (P)
    (COND ((= P (ARRAY-LENGTH *IS-REVERSE-P*))
           (ADJUST-ARRAY-SIZE *IS-REVERSE-P* (+ P 100))
           (ADJUST-ARRAY-SIZE *IS-STATUS* (+ P 100))
           (ADJUST-ARRAY-SIZE *IS-OPERATION* (+ P 100))
           (ADJUST-ARRAY-SIZE *IS-BP* (+ P 100))
           (ADJUST-ARRAY-SIZE *IS-POINTER* (+ P 100))))
    (ASET (AREF *IS-REVERSE-P* (1- P)) *IS-REVERSE-P* P)
    (ASET (AREF *IS-POINTER* (1- P)) *IS-POINTER* P)
    (ASET ':GO *IS-STATUS* P))

;;; This is how incremental search manages to allow both type-ahead and rubout-ahead:
;;; What to do is kept in five stacks, arrays in the *IS-...* variables.
;;; Input of normal characters pushes onto the end using index P,
;;; and rubout pops off at the same index.  *IS-REVERSE-P* remembers the
;;; search direction at each level, *IS-OPERATION* remembers the type of search
;;; (:NORMAL for a normal character, :REVERSE for a R or S that reverses,
;;; or :REPEAT for a R or S that repeats), *IS-POINTER* is the length of
;;; the search string at that level.

;;; In parallel, with lower priority, the entries thus pushed are processed
;;; by searching according to them.  P1 is the index of the entry or "level"
;;; which is currently being worked on.  P1 advances only when the level is
;;; determined to be successful or failing.  Advancing involves examining the three
;;; *IS-...* entries of the next level to see what to do.  If P1 meets P, then there is no
;;; work to do for the moment.  The state of this process is kept in *IS-STATUS*
;;; and *IS-BP*.  *IS-BP* is the bp of the place found at a given level or the 
;;; place at which searching is going on.  *IS-STATUS* is T for a successful search,
;;; NIL for a failing one, and :GO if it isn't known yet.  New levels are pushed
;;; (via P) in the :GO state.

;;; Rubbing out decrements P1 if necessary to keep it no greater than P.
;;; The searching process is not confused because it keeps all its state
;;; in *IS-STATUS* and *IS-BP* and all that is needed is to change P1.

;;; Updating the echo area is under input in priority, but above actual searching.
;;; Thus, as soon as there is no type-ahead everything will be correct.
;;; This is because the echo area is presumed to be fast to update.
;;; Buffer redisplay is lower than searching, of course.

(DEFUN INCREMENTAL-SEARCH (REVERSE-P)
  (SELECT-WINDOW *WINDOW*)
  (TYPEIN-LINE "")		;Necessary if in the mini-buffer
  (UNWIND-PROTECT
   (TYPEIN-LINE-ACTIVATE
    (PROG (CHAR			   ; The current command.
	   REAL-CHAR		   ; The one to :UNTYI if need be
	   XCHAR		   ; Upcase version of character
	   MUST-REDIS		   ; T => The echo-area must be completely redisplayed.
	   (P 0)		   ; The stack pointer into *IS-BP*, etc. for input and rubout
           (P1 0)                  ; The pointer for which search we are doing.
                                   ; Can never exceed P.
           SUPPRESSED-REDISPLAY    ; T if the last input char was read before
                                   ; redisplay had a chance to finish.
                                   ;  A G read that way acts like a failing search quit.
	   (BP (POINT))		   ; The POINT.
	   BP1			   ; Aux BP used for actual searching.
           NEW-BP
           TIME-OUT                ; Set by SEARCH when it times out so we can check input.
           INPUT-DONE              ; An altmode or control char has been seen.
                                   ; Do not look for input any more; just search, then exit.
           (ORIG-PT)               ; Original position of POINT.
           )
          (SETQ ORIG-PT (COPY-BP BP))
	  (SETQ BP1 (COPY-BP BP))	   ; This is reused to save consing.
	  (STORE-ARRAY-LEADER 0 *IS-STRING* 0)	   ; Clear out the search string.
          (ASET T *IS-STATUS* 0)   ; Initialize the stacks.
          (ASET REVERSE-P *IS-REVERSE-P* 0)
          (ASET ':NORMAL *IS-OPERATION* 0)
          (ASET 0 *IS-POINTER* 0)
          (ASET (COPY-BP BP) *IS-BP* 0)
	  (SETQ MUST-REDIS T)	   ; Initially we must redisplay.
	  (GO CHECK-FOR-INPUT)

          ;; Come here if there is input, or nothing to do until there is input.
    INPUT (SETQ SUPPRESSED-REDISPLAY NIL)
	  (AND (WINDOW-READY-P *WINDOW*) 	;In case of minibuffer
	       (REDISPLAY *WINDOW* ':POINT))	; Redisplay point position while waiting.
          (OR (= (WINDOW-REDISPLAY-DEGREE *WINDOW*) DIS-NONE)
              (SETQ SUPPRESSED-REDISPLAY T))
	  (MULTIPLE-VALUE (CHAR REAL-CHAR)
	    (FUNCALL STANDARD-INPUT ':MOUSE-OR-KBD-TYI))
	  (SETQ XCHAR (CHAR-UPCASE CHAR))
	  (COND ((NOT (OR (LDB-TEST %%KBD-CONTROL-META CHAR) (LDB-TEST %%KBD-MOUSE CHAR)
			  (= CHAR #/) (= CHAR #\RUBOUT)))
		 (GO NORMAL))
		((MEMQ XCHAR '(#/S #/R))
		 (PUSH-ISEARCH-STATUS)
		 (ASET ':REPEAT *IS-OPERATION* P)
		 (LET ((NEW-REVERSE-P (= XCHAR #/R)))
		   (COND   ;; In reverse mode, just go to forward.
		     ((NEQ (AREF *IS-REVERSE-P* P) NEW-REVERSE-P)
		      (ASET NEW-REVERSE-P *IS-REVERSE-P* P)
		      (SETQ MUST-REDIS T)
		      (ASET ':REVERSE *IS-OPERATION* P))
		     ((ZEROP (AREF *IS-POINTER* P))
		      (LET ((STRING (STRING (SEARCH-RING-POP))))
			(COPY-ARRAY-CONTENTS STRING *IS-STRING*)
			(ASET (ARRAY-ACTIVE-LENGTH STRING) *IS-POINTER* P))
		      (SETQ MUST-REDIS T))))
		 (GO CHECK-FOR-INPUT))
		((= XCHAR #/Q)
		 (SETQ CHAR (LOGAND 377 (FUNCALL STANDARD-INPUT ':TYI)))
		 (GO NORMAL))
		((= XCHAR #/G)
		 (BEEP)
		 (COND ((OR SUPPRESSED-REDISPLAY (NEQ (AREF *IS-STATUS* P) T))
			;; G in other than a successful search
			;; rubs out until it becomes successful.
			(SETQ P (DO ((P (1- P) (1- P)))
				    ((EQ (AREF *IS-STATUS* P) T) P)))
			(SETQ P1 (MIN P P1) MUST-REDIS T)
			(GO CHECK-FOR-INPUT))
		       (T
			(MOVE-BP BP (AREF *IS-BP* 0))
			(TYPEIN-LINE "")
			(RETURN))))
		((= CHAR #/)
		 (AND (ZEROP P)
		      (RETURN (COM-STRING-SEARCH-INTERNAL REVERSE-P NIL NIL NIL)))
		 (SETQ INPUT-DONE T)
		 (GO CHECK-FOR-INPUT))
		((= CHAR #\RUBOUT)
		 (COND (( P 0)	   ; If he over-rubbed out,
			(BEEP)	   ;   that is an error.
			(GO CHECK-FOR-INPUT))
		       (T
			;; Rubout pops all of these PDLs.
			(SETQ P (1- P))
			(SETQ P1 (MIN P P1))
			(SETQ MUST-REDIS T)
			(GO CHECK-FOR-INPUT))))
		(T
		 (FUNCALL STANDARD-INPUT ':UNTYI REAL-CHAR)
		 (SETQ INPUT-DONE T)
		 (GO CHECK-FOR-INPUT)))
	  (FERROR NIL "A clause fell through.")

          ;; Normal chars to be searched for come here.
   NORMAL (OR MUST-REDIS (TYPEIN-LINE-MORE "~C" CHAR))
          (PUSH-ISEARCH-STATUS)
          (AND (= (AREF *IS-POINTER* P) (STRING-LENGTH *IS-STRING*))
               (ADJUST-ARRAY-SIZE *IS-STRING* (+ 100 (STRING-LENGTH *IS-STRING*))))
          (ASET CHAR *IS-STRING* (AREF *IS-POINTER* P))
          (ASET (1+ (AREF *IS-POINTER* P)) *IS-POINTER* P)
          (ASET ':NORMAL *IS-OPERATION* P)
          ;; Come here after possibly processing input to update the search tables
          ;; to search for a while.  First, if necessary and not suppressed
          ;; update the search string displayed in the echo area.
   CHECK-FOR-INPUT
          ;; If there is input available, go read it.
          ;; Otherwise, do work if there is work to be done.
          (AND (NOT INPUT-DONE)
               (FUNCALL STANDARD-INPUT ':LISTEN)
               (GO INPUT))
          ;; Now do some work for a while, then go back to CHECK-FOR-INPUT.
          (COND (MUST-REDIS
                 (SETQ MUST-REDIS NIL)
		 (TYPEIN-LINE "")
                 (OR (AREF *IS-STATUS* P1) (TYPEIN-LINE-MORE "Failing "))
                 (AND (AREF *IS-REVERSE-P* P) (TYPEIN-LINE-MORE "Reverse "))
                 (TYPEIN-LINE-MORE "I-Search: ")
                 (STORE-ARRAY-LEADER (AREF *IS-POINTER* P) *IS-STRING* 0)
                 (TYPEIN-LINE-MORE "~A" *IS-STRING*)))
          ;; Now see what sort of state the actual search is in, and what work there is to do.
          ;; P1 points at the level of the table on which we are actually working.
          (MOVE-BP BP1 (AREF *IS-BP* P1))
          ;; Display point at the end of the last search level which has succeeded.
          (DO ((P0 P1 (1- P0)))
              ((EQ (AREF *IS-STATUS* P0) T)
               (MOVE-BP BP (AREF *IS-BP* P0))))
          (MUST-REDISPLAY *WINDOW* DIS-BPS)
          (COND ((EQ (AREF *IS-STATUS* P1) ':GO)
                 ;; If the level we were working on is still not finished,
                 ;; search at most 100 more lines.  If we find it or the end of the buffer
                 ;; before then, this level is determined and we can work on the next.
                 ;; Otherwise, we remain in the :GO state and do 100 more lines next time.
                 (MULTIPLE-VALUE (NEW-BP TIME-OUT)
                     (SEARCH BP1 *IS-STRING*
                             (AREF *IS-REVERSE-P* P1) NIL 100))
                 ;; What happened?
                 (COND (TIME-OUT
                        ;; Nothing determined.  NEW-BP has where we stopped.
                        (MOVE-BP BP1 NEW-BP))
                       ((NULL NEW-BP)
                        ;; This search was determined to be a failure.
			(OR (AND (MEMQ ':MACRO-ERROR
				       (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
				 (FUNCALL STANDARD-INPUT ':MACRO-ERROR))
			    (BEEP))
                        (ASET NIL *IS-STATUS* P1)
                        (MOVE-BP BP1 (AREF *IS-BP* (1- P1)))
                        (MOVE-BP BP BP1)
                        (SETQ MUST-REDIS T))
                       (T ;; This search level has succeeded.
                        (ASET T *IS-STATUS* P1)
                        (MOVE-BP BP NEW-BP)
                        (MOVE-BP BP1 NEW-BP))))
                (( P P1)
                 ;; This level is finished, but there are more pending levels typed ahead.
                 (SETQ P1 (1+ P1))
                 (ASET (SETQ BP1 (COPY-BP BP1)) *IS-BP* P1)
                 (STORE-ARRAY-LEADER (AREF *IS-POINTER* P1) *IS-STRING* 0)
                 (COND ((NULL (AREF *IS-STATUS* (1- P1)))
                        (COND ((NEQ (AREF *IS-OPERATION* P1) ':REVERSE)
                               ;; A failing search remains so unless we reverse direction.
                               (ASET NIL *IS-STATUS* P1))
                              (T ;; If we reverse direction, change prompt line.
                               (SETQ MUST-REDIS T))))
                       ((EQ (AREF *IS-OPERATION* P1) ':NORMAL)
                        ;; Normal char to be searched for comes next.
                        ;; We must adjust the bp at which we start to search
                        ;; so as to allow the user to extend the string already found.
                        (MOVE-BP BP1
                                 (FORWARD-CHAR BP1
                                      (COND ((AREF *IS-REVERSE-P* P1)
                                             (COND ((= (ARRAY-ACTIVE-LENGTH *IS-STRING*) 1)
                                                    0)
                                                   (T (ARRAY-ACTIVE-LENGTH *IS-STRING*))))
                                            (T (- 1 (ARRAY-ACTIVE-LENGTH *IS-STRING*))))
                                      T)))))
                ;; If there is nothing left to do, and terminator seen, exit.
                (INPUT-DONE
                 (SEARCH-RING-PUSH
		   (SUBSTRING *IS-STRING* 0 (ARRAY-ACTIVE-LENGTH *IS-STRING*)))
                 (TYPEIN-LINE-MORE "")
		 (MAYBE-PUSH-POINT ORIG-PT)
		 (SELECT-WINDOW *WINDOW*)
                 (RETURN))
                ;; Nothing to do and no terminator, wait for input.
                (T (GO INPUT)))
          (GO CHECK-FOR-INPUT)

	  ))
   (FUNCALL *MODE-LINE-WINDOW* ':DONE-WITH-MODE-LINE-WINDOW))
  DIS-BPS)

(DEFCOM COM-REPLACE-STRING "Replace all occurrences of a given string with another.
Prompts for two string: to replace all FOO's with BAR's, type FOO and BAR.
With no numeric arg, all occurrences after point are replaced.
With numeric arg, that many occurrences are replaced.
If *CASE-REPLACE-P* is nonnull, BAR's initial will be capitalized
if FOO's initial had been (supply it in lower case)." ()
  (LET ((FROM (TYPEIN-LINE-READLINE "Replace all occurrences of:")))
    (AND (ZEROP (STRING-LENGTH FROM))
	 (BARF "The string may not be null."))
    (LET ((TO (TYPEIN-LINE-READLINE "Replace all occurrences of /"~A/" with:" FROM)))
      (REPLACE-STRING (POINT) FROM TO (AND *NUMERIC-ARG-P*
					   *NUMERIC-ARG*))))
  DIS-TEXT)

(DEFCOM COM-QUERY-REPLACE "Replace string, asking about each occurrence.
Prompts for each string.  If you first give it FOO, then BAR, it
finds the first FOO, displays, and
reads a character.  Space => replace it with BAR and show next FOO.
Rubout => don't replace, but show next FOO.
Comma => replace this FOO and show result, waiting for a
space, R or Altmode.
Period => replace this FOO and exit.  Altmode => just exit.
^ => return to site of previous FOO (actually, pop the point pdl).
W => kill this FOO and enter recursive edit.
R => enter editing mode recursively.  L => redisplay screen.
Exclamation mark => replace all remaining FOOs without asking.
Any other character exits and (except altmode) is read again.
If *CASE-REPLACE-P* is nonnull, BAR's initial will be capitalized
if FOO's initial had been.
If you give a numeric argument, it will not consider FOOs that are not
bounded on both sides by delimiter characters." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS)
    (QUERY-REPLACE (POINT) FROM TO *NUMERIC-ARG-P*))
  DIS-TEXT)


(DEFCOM COM-ATOM-QUERY-REPLACE "Query replaces delimited atoms.
See Query Replace for documentation of the various options." ()
  (ATOM-WORD-SYNTAX-BIND
    (LET ((*NUMERIC-ARG-P* T))
      (COM-QUERY-REPLACE))))

(DEFUN QUERY-REPLACE-STRINGS (&OPTIONAL (TYPE "replace") RETURN-EMPTY &AUX FROM TO)
  (SETQ FROM (TYPEIN-LINE-READLINE "Query-~A some occurrences of:" TYPE))
  (COND ((NOT (ZEROP (STRING-LENGTH FROM)))
	 (TEMP-KILL-RING FROM
	   (SETQ TO (TYPEIN-LINE-READLINE "Query-~A some occurrences of /"~A/" with:"
					  TYPE FROM)))
	 (MVRETURN FROM TO))
	((NOT RETURN-EMPTY)
	 (BARF "The string may not be null."))
	(T NIL)))

(DEFVAR *QUERY-FROM*)				;These are for the mode line
(DEFVAR *QUERY-TO*)

;;; This is the normal form of query replace
(DEFUN QUERY-REPLACE (BP *QUERY-FROM* *QUERY-TO* &OPTIONAL BREAKS)
  (QUERY-REPLACE-INTERNAL BP *QUERY-FROM* *QUERY-TO* #'QUERY-REPLACE-SEARCH BREAKS))

(DEFUN QUERY-REPLACE-SEARCH (BP QUERY-FROM IGNORE &AUX BP1)
  (AND (SETQ BP1 (SEARCH BP QUERY-FROM))
       (MVRETURN BP1 (FORWARD-CHAR BP1 (- (STRING-LENGTH QUERY-FROM))))))

(DEFMACRO QREP ()
  `(COND ((NOT FLAG-2)
	  (MOVE-BP BP (CASE-REPLACE BP1 BP *QUERY-TO*))
	  (MUST-REDISPLAY *WINDOW* DIS-TEXT))))

;;; General query replace.  Note: BP itself is moved around.  It is usually POINT.
;;; BREAKS means only consider things surrounded by delimiters.
;;; Function is called on with BP and QUERY-FROM and QUERY-to, it should return two bps to
;;; the area of the thing found or NIL.
;;; FLAG-1 and FLAG-2 implement the hairy COMMA command.
(DEFUN QUERY-REPLACE-INTERNAL (BP QUERY-FROM QUERY-TO FUNCTION BREAKS
			       &AUX TEM BP1 DO-THE-REST CHAR UCHAR FLAG-1 FLAG-2)
  (BIND-MODE-LINE ("Query Replacing " *QUERY-FROM* " => " *QUERY-TO*)
    (SETQ BP1 (COPY-BP BP))
    (DO () (NIL)
      (SETQ FLAG-2 FLAG-1 FLAG-1 NIL)
      (COND ((NOT FLAG-2)
	     (MULTIPLE-VALUE (TEM BP1)
	       (FUNCALL FUNCTION BP QUERY-FROM QUERY-TO))
	     (OR TEM (RETURN NIL))
	     (MOVE-BP BP TEM)
	     (MUST-REDISPLAY *WINDOW* DIS-BPS)))
      (COND ((OR FLAG-2
		 (NOT BREAKS)			; If we don't care about breaks, go ahead.
		 (AND				; Both beginning and end must be breaks.
		   (OR (EQ BP (INTERVAL-LAST-BP *INTERVAL*))	; EOB counts as a break.
		       (= (WORD-SYNTAX (BP-CHAR BP)) WORD-DELIMITER))
		   (OR (EQ BP1 (INTERVAL-FIRST-BP *INTERVAL*))
		       (= (WORD-SYNTAX (BP-CHAR-BEFORE BP1)) WORD-DELIMITER))))
	     ;; We want to offer this string for replacement.
	     (COND (DO-THE-REST (QREP))
		   (T
		    (REDISPLAY *WINDOW* ':POINT)
		    (REDISPLAY-MODE-LINE)
		    (POINT-PDL-PUSH BP *WINDOW*)
		    (PROG ()
		       GETCHAR
			  (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI))
			  (OR (NUMBERP CHAR) (GO GETCHAR))	;Ignore special request
			  (SETQ UCHAR (CHAR-UPCASE CHAR))
			  (COND ((= UCHAR #/^)
				 (MULTIPLE-VALUE-BIND (BP1 PLINE)
				     (POINT-PDL-POP *WINDOW*)
				   (MOVE-BP BP BP1)
				   (REDISPLAY-POINT-ON-PLINE BP *WINDOW* PLINE))
				 (MUST-REDISPLAY *WINDOW* DIS-BPS)
				 (REDISPLAY *WINDOW* ':POINT)
				 (GO GETCHAR))
				((MEMQ UCHAR '(#\FF #/L))
				 (MUST-REDISPLAY *WINDOW*
						 (IF (= UCHAR #\FF) DIS-ALL
						     (COM-RECENTER-WINDOW)))
				 (REDISPLAY *WINDOW* ':POINT)
				 (GO GETCHAR))))
		    (SELECTQ UCHAR
		      (#\SP (QREP))		;Space: Replace and continue.
		      (#\RUBOUT NIL)		;Rubout: Continue.
		      (#/,			;Comma:
		       (QREP)
		       (SETQ FLAG-1 T))
		      (#/ (RETURN NIL))	;Altmode: Quit.
		      (#/. (QREP)		;Point: Replace and quit.
		       (RETURN NIL))
		      (#/R (CONTROL-R))	;C-R: Recurse.
		      (#/W			;C-W: Delete, and recurse.
		       (DELETE-INTERVAL BP BP1)
		       (MUST-REDISPLAY *WINDOW* DIS-TEXT)
		       (CONTROL-R))
		      (#/! (QREP)		;!: Do this and rest.
		       (SETQ DO-THE-REST T))
		      (OTHERWISE
		       (FUNCALL STANDARD-INPUT ':UNTYI CHAR)
		       (RETURN 'ABORTED))))))))))

(DEFCOM COM-QUERY-EXCHANGE "Query replace two strings with one another at the same time.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (QUERY-REPLACE-STRINGS "exchange")
    (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
				   *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*)))
      (QUERY-REPLACE-LIST (POINT) (LIST FROM TO) (LIST TO FROM)
			  *NUMERIC-ARG-P*)))
  DIS-TEXT)

(DEFCOM COM-MULTIPLE-QUERY-REPLACE "Query replace two sets of strings at the same time.
Strings are read in alternate mini-buffers, ended by a null string.
Argument means things must be surrounded by breaks.
Negative argument means delimited atoms, rather than words." ()
  (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
				 *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*))
	FROM-LIST TO-LIST)
    (MULTIPLE-VALUE (FROM-LIST TO-LIST)
      (MULTIPLE-QUERY-REPLACE-STRINGS))
    (QUERY-REPLACE-LIST (POINT) FROM-LIST TO-LIST *NUMERIC-ARG-P*))
  DIS-TEXT)

(LOCAL-DECLARE ((SPECIAL *BP* *STATE*))
(DEFUN QUERY-REPLACE-LIST (*BP* FROM-LIST TO-LIST &OPTIONAL BREAKS
						  &AUX *QUERY-FROM* *QUERY-TO* (*STATE* 0))
  (QUERY-REPLACE-INTERNAL *BP* FROM-LIST TO-LIST #'QUERY-REPLACE-SEARCH-LIST BREAKS))

(DEFUN QUERY-REPLACE-SEARCH-LIST (BP FROM-LIST TO-LIST &AUX TEM)
  (OR (BP-= BP *BP*) (SETQ *STATE* 0))		;If bp has moved, reset state
  (MULTIPLE-VALUE (*BP* TEM *STATE*)
    (FSM-SEARCH BP FROM-LIST NIL NIL NIL *STATE*))
  (COND (*BP*
	 (SETQ *QUERY-FROM* TEM
	       *QUERY-TO* (NTH (FIND-POSITION-IN-LIST TEM FROM-LIST) TO-LIST))
	 (MVRETURN *BP* (FORWARD-CHAR *BP* (- (STRING-LENGTH TEM)))))))
)

(DEFUN MULTIPLE-QUERY-REPLACE-STRINGS (&AUX FROM-LIST TO-LIST)
  (DO ((FROM) (TO)) (NIL)
    (MULTIPLE-VALUE (FROM TO)
      (QUERY-REPLACE-STRINGS "replace" T))
    (OR FROM (RETURN (NREVERSE FROM-LIST) (NREVERSE TO-LIST)))
    (PUSH FROM FROM-LIST)
    (PUSH TO TO-LIST)))

;;; Miscellaneous searching commands

(DEFCOM COM-OCCUR "Display text lines that contain a given string.
With an argument, show the next n lines containing the string.  If
no argument is given, all lines are shown." ()
  (LET ((CNT (IF *NUMERIC-ARG-P* *NUMERIC-ARG* 7777777))
	KEY FUNCTION REVERSE-P BJ-P)
    (MULTIPLE-VALUE (FUNCTION KEY REVERSE-P BJ-P)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Show lines containing:"
					  *STRING-SEARCH-SINGLE-LINE-COMTAB*))
    (DO ((BP (COND ((NOT BJ-P) (POINT))
		   ((NOT REVERSE-P) (INTERVAL-FIRST-BP *INTERVAL*))
		   (T (INTERVAL-LAST-BP *INTERVAL*))))
	 (I 0 (1+ I)))
	(( I CNT) NIL)
      (OR (SETQ BP (FUNCALL FUNCTION BP KEY REVERSE-P)) (RETURN NIL))
      (LET ((LINE (BP-LINE BP))
	    (INDEX (BP-INDEX BP)))
	(FUNCALL *TYPEOUT-WINDOW* ':ITEM 'BP (CREATE-BP LINE INDEX NIL *INTERVAL*) LINE))
      (FUNCALL *TYPEOUT-WINDOW* ':TYO #\CR)
      (OR (SETQ BP (BEG-LINE BP 1)) (RETURN NIL)))
    (FUNCALL *TYPEOUT-WINDOW* ':LINE-OUT "Done."))
  DIS-NONE)

(DEFCOM COM-KEEP-LINES "Delete all lines not containing the specified string.
Covers from point to the end of the buffer" ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Keep lines containing:"
					  *SEARCH-MINI-BUFFER-COMTAB*)
    (LET ((BP (INTERVAL-FIRST-BP *INTERVAL*))
	  (NEW-BP))
      (DO () (())
	(SETQ NEW-BP (FUNCALL FUNCTION BP KEY NIL T))
	(DELETE-INTERVAL BP (BEG-LINE NEW-BP 0) T)
	(OR (SETQ BP (BEG-LINE BP 1)) (RETURN NIL)))))
  DIS-TEXT)

(DEFCOM COM-FLUSH-LINES "Delete all lines containing the specified string.
Covers from point to the end of the buffer" ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Flush lines containing:"
					  *SEARCH-MINI-BUFFER-COMTAB*)
    (LET ((BP (INTERVAL-FIRST-BP *INTERVAL*)))
      (DO () (())
	(OR (SETQ BP (FUNCALL FUNCTION BP KEY)) (RETURN NIL))
	(DELETE-INTERVAL (BEG-LINE BP 0) (BEG-LINE BP 1)))))
  DIS-TEXT)

(DEFCOM COM-HOW-MANY "Counts occurences of a substring, after point." ()
  (MULTIPLE-VALUE-BIND (FUNCTION KEY REVERSE-P BJ-P)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "How many occurences of:"
					  *STRING-SEARCH-SINGLE-LINE-COMTAB*)
    (DO ((BP (COND ((NOT BJ-P) (POINT))
		   ((NOT REVERSE-P) (INTERVAL-FIRST-BP *INTERVAL*))
		   (T (INTERVAL-LAST-BP *INTERVAL*)))
	     (FUNCALL FUNCTION BP KEY REVERSE-P))
	 (N 0 (1+ N)))
	((NULL BP)
	 (TYPEIN-LINE "~D. occurence~:P.~%" (1- N)))))
  DIS-NONE)

(DEFCOM COM-COUNT-LINES "Counts the number of lines in the buffer." ()
  (TYPEIN-LINE "There are ~D. lines in the buffer.~%"
	       (1- (COUNT-LINES *INTERVAL*)))
  DIS-NONE)
