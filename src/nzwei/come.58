;;; Zwei commands, see ZWEI;COMA for comments -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Various Quantities.

(DEFCOM COM-VARIOUS-QUANTITIES "Given characters with control/meta bits or non-letters, inserts them.
Otherwise hacks various quantities.
Note that @ and ? are letters.  If followed by a number, inserts that
octal character <arg> number of times.
First character following is operation:
  F forward, B backward, D delete, R rubout, T twiddle, @ mark region, U uppercase,
  L lowercase, S save, C copy or M to change the Mode of F, B, D, rubout, and T but not @
Second character following is quantity type:
  C character, W word, S sentence, P paragraph, L line, A atom, - S-expression,
  ( or ) list, D defun, L page separated by Ls, H buffer.
Numeric arguments are obeyed.  ? for help." ()
    (SELECT-WINDOW *WINDOW*)
    (LET (CH MODE-NAME MODE QUANTITY)
      (TYPEIN-LINE-ACTIVATE
	(COND ((NOT NIL
;		    (SUPPRESS-REDISPLAY)
		    )
	       (TYPEIN-LINE "~:[~*~;~D ~]~:C: "
			    *NUMERIC-ARG-P* *NUMERIC-ARG* *LAST-COMMAND-CHAR*)))
	(SETQ CH (FUNCALL STANDARD-INPUT ':TYI))
	(COND ((OR (LDB-TEST %%KBD-CONTROL-META CH)
		   (MEMQ CH '(#/ #/ #/ #/ #\CR)))
	       ;; If char has control/meta, or is alpha, beta, epsilon, or equiv, then
	       ;; insert into buffer as a two character sequence in the standard way.
	       (INSERT-MOVING (POINT) (FORMAT NIL "~C" CH))
	       DIS-TEXT)
	      ((OR (< (SETQ CH (CHAR-UPCASE CH)) #/?) (> CH #/Z))
	       (COND ((AND ( CH #/0) ( CH #/7))
		      (FUNCALL *TYPEIN-WINDOW* ':TYO CH)
		      (SETQ CH (- CH #/0))
		      (DO ((I 2 (1- I))
			   (CH1))
			  (( I 0))
			  (SETQ CH1 (FUNCALL STANDARD-INPUT ':TYI))
			  (COND ((AND ( CH1 #/0) ( CH1 #/7))
				 (FUNCALL *TYPEIN-WINDOW* ':TYO CH1)
				 (SETQ CH (+ (* CH 8) (- CH1 #/0))))
				(T (OR (= CH1 #\SP)
				       (FUNCALL STANDARD-INPUT ':UNTYI CH1))
				   (RETURN NIL))))))
	       (LET ((*LAST-COMMAND-CHAR* CH))
		 (MULTIPLE-VALUE-CALL (COM-SELF-INSERT))))
	      (T
	       (PROG ()
	        GET-A-MODE
		  (SELECTQ CH
		   (#/?
		    (TYPEIN-LINE "~%Type strange character or rubout to be inserted, or octal escape, or
F forward, B backward, D delete, R rubout, T twiddle, M mode, @ Mark, U uppercase, L lowercase,
S save, C copy, Z reverse  ")
		    (TYPEIN-LINE-MORE "~:[~*~;~D ~]~:C: "
				 *NUMERIC-ARG-P* *NUMERIC-ARG* *LAST-COMMAND-CHAR*)
		    (SETQ CH (CHAR-UPCASE (FUNCALL STANDARD-INPUT ':TYI)))
		    (GO GET-A-MODE))
		   (#/F
		    (SETQ MODE-NAME "Forward"
			  MODE 'COM-QUANTITY-FORWARD))
		   (#/B
		    (SETQ MODE-NAME "Backward"
			  MODE 'COM-QUANTITY-BACKWARD))
		   (#/D
		    (SETQ MODE-NAME "Delete"
			  MODE 'COM-QUANTITY-DELETE))
		   (#/R
		    (SETQ MODE-NAME "Rubout"
			  MODE 'COM-QUANTITY-RUBOUT))
		   (#/T
		    (SETQ MODE-NAME "Twiddle"
			  MODE 'COM-QUANTITY-TWIDDLE))
		   (#/@
		    (SETQ MODE-NAME "Mark"
			  MODE 'COM-QUANTITY-MARK))
		   (#/M
		    (SETQ MODE-NAME "Mode"
			  MODE 'QUANTITY-MODE-SET))
		   (#/U
		    (SETQ MODE-NAME "Uppercase"
			  MODE 'COM-QUANTITY-UPPERCASE))
		   (#/L
		    (SETQ MODE-NAME "Lowercase"
			  MODE 'COM-QUANTITY-LOWERCASE))
		   (#/S
		    (SETQ MODE-NAME "Save"
			  MODE 'COM-QUANTITY-SAVE))
		   (#/C
		    (SETQ MODE-NAME "Copy"
			  MODE 'COM-QUANTITY-COPY))
		   (#/Z
		    (SETQ MODE-NAME "Reverse"
			  MODE 'COM-QUANTITY-REVERSE))
		   (OTHERWISE
		    (BARF "Invalid quantity operation")))
		  (TYPEIN-LINE "")
	       GET-A-QUANTITY
		  (TYPEIN-LINE "~A~:[~*~; ~R~] "
			       MODE-NAME *NUMERIC-ARG-P* *NUMERIC-ARG*)
		  (SETQ CH (CHAR-UPCASE (FUNCALL STANDARD-INPUT ':TYI)))
		  (SELECTQ CH
		   (#/?
		    (TYPEIN-LINE "Type quantity name: C character, W word, S sentence, P paragraph, A atom, L line, -
S-expression, ( or ) list, D defun, Form page, H buffer~%")
		    (GO GET-A-QUANTITY))
		   (#/C
		    (SETQ MODE-NAME "Character"
			  QUANTITY 'FORWARD-CHAR))
		   (#/W
		    (SETQ MODE-NAME "Word"
			  QUANTITY 'FORWARD-WORD))
		   (#/A
		    (SETQ MODE-NAME "Atom"
			  QUANTITY 'FORWARD-ATOM))
		   (#/S
		    (SETQ MODE-NAME "Sentence"
			  QUANTITY 'FORWARD-SENTENCE))
		   (#/P
		    (SETQ MODE-NAME "Paragraph"
			  QUANTITY 'FORWARD-PARAGRAPH))
		   (#/L
		    (SETQ MODE-NAME "Line"
			  QUANTITY 'FORWARD-LINE))
		   (#/-
		    (SETQ MODE-NAME "S-Expression"
			  QUANTITY 'FORWARD-SEXP))
		   ((#/( #/))
		    (SETQ MODE-NAME "List"
			  QUANTITY 'FORWARD-LIST))
		   (#/D
		    (SETQ MODE-NAME "Defun"
			  QUANTITY 'FORWARD-DEFUN))
		   (#\FF
		    (SETQ MODE-NAME "Page"
			  QUANTITY 'FORWARD-PAGE))
		   (#/H
		    (SETQ MODE-NAME "Buffer"
			  QUANTITY 'FORWARD-BUFFER))
		   (OTHERWISE
		    (BARF "Invalid quantity type")))
		  (TYPEIN-LINE-MORE "~A~P" MODE-NAME *NUMERIC-ARG*)
		  )
	       (COND ((EQ MODE 'QUANTITY-MODE-SET)
		      (QUANTITY-MODE-SET QUANTITY MODE-NAME)
		      DIS-NONE)
		     (T
		      (LET ((*QUANTITY-MODE* QUANTITY))
			   (FUNCALL MODE)))))))))

(DEFCOM COM-QUANTITY-FORWARD "Move forward according to the current quantity mode." (KM)
    (MOVE-BP (POINT) (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF)))
    DIS-BPS)

(DEFCOM COM-QUANTITY-BACKWARD "Move backward according to the current quantity mode." (KM)
    (MOVE-BP (POINT) (OR (FUNCALL *QUANTITY-MODE* (POINT) (- *NUMERIC-ARG*)) (BARF)))
    DIS-BPS)

(DEFCOM COM-QUANTITY-DELETE "Kill forward according to the current quantity mode." ()
    (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
    (KILL-INTERVAL (POINT)
		   (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*)
		       (BARF))
		   NIL
		   T)
    DIS-TEXT)

(DEFCOM COM-QUANTITY-RUBOUT "Kill backward according to the current quantity mode." ()
    (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
    (KILL-INTERVAL (POINT)
		   (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*)
		       (BARF))
		   NIL
		   NIL)
    DIS-TEXT)

(DEFCOM COM-QUANTITY-TWIDDLE "Exchange things according to the current quantity mode." ()
  (EXCHANGE-SUBR *QUANTITY-MODE* *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-QUANTITY-REVERSE "Reverse things according to the current quantity mode." ()
  (REVERSE-SUBR *QUANTITY-MODE* *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-QUANTITY-MARK "Mark according to the current quantity mode." (SM)
  (LET (BP1 BP2)
    (OR (SETQ BP1 (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*))
	(BARF))
    (OR (SETQ BP2 (FUNCALL *QUANTITY-MODE* BP1 (MINUS *NUMERIC-ARG*)))
	(BARF))
    (AND (MINUSP *NUMERIC-ARG*)
	 (SETQ BP2 (PROG1 BP1 (SETQ BP1 BP2))))
    (MOVE-BP (POINT) BP1)
    (MOVE-BP (MARK) BP2))
  DIS-BPS)

(DEFCOM COM-QUANTITY-UPPERCASE "Uppercase according to the current quantity mode." ()
   (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
     (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
       (UNDO-SAVE BP1 BP2 NIL "Upcase")
       (UPCASE-INTERVAL BP1 BP2)
       (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1))))
   DIS-TEXT)

(DEFCOM COM-QUANTITY-LOWERCASE "Lowercase according to the current quantity mode." ()
   (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
     (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
       (UNDO-SAVE BP1 BP2 NIL "Downcase")
       (DOWNCASE-INTERVAL BP1 BP2)
       (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1))))
   DIS-TEXT)

(DEFCOM COM-QUANTITY-SAVE "Save on kill ring according to the current quantity mode." ()
   (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
     (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
       (KILL-RING-PUSH (COPY-INTERVAL BP1 BP2))
       (MOVE-BP (POINT) BP1)))
   DIS-TEXT)

(DEFCOM COM-QUANTITY-COPY "Insert a copy according to the current quantity mode." ()
   (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
     (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
       (MOVE-BP (POINT)
		(INSERT-INTERVAL BP2 (COPY-INTERVAL BP1 BP2)))))
   DIS-TEXT)

(DEFUN QUANTITY-MODE-SET (QUANTITY MODE-NAME)
    (SETQ *QUANTITY-MODE* QUANTITY)
    (COND ((NULL *QUANTITY-MODE-SAVE*)
	   (SETQ *QUANTITY-MODE-SAVE* (MAKE-ARRAY NIL 'ART-Q 5))
	   (ASET (COMMAND-LOOKUP 506 *COMTAB*) *QUANTITY-MODE-SAVE* 0)
	   (ASET (COMMAND-LOOKUP 502 *COMTAB*) *QUANTITY-MODE-SAVE* 1)
	   (ASET (COMMAND-LOOKUP 504 *COMTAB*) *QUANTITY-MODE-SAVE* 2)
	   (ASET (COMMAND-LOOKUP 207 *COMTAB*) *QUANTITY-MODE-SAVE* 3)
	   (ASET (COMMAND-LOOKUP 524 *COMTAB*) *QUANTITY-MODE-SAVE* 4)))
    (COND ((EQ QUANTITY 'FORWARD-CHAR)
	   (SETQ *MODE-QUANTITY-NAME* NIL)
	   (COMMAND-STORE (AREF *QUANTITY-MODE-SAVE* 0) 506 *COMTAB*)
	   (COMMAND-STORE (AREF *QUANTITY-MODE-SAVE* 1) 502 *COMTAB*)
	   (COMMAND-STORE (AREF *QUANTITY-MODE-SAVE* 2) 504 *COMTAB*)
	   (COMMAND-STORE (AREF *QUANTITY-MODE-SAVE* 3) 207 *COMTAB*)
	   (COMMAND-STORE (AREF *QUANTITY-MODE-SAVE* 4) 524 *COMTAB*))
	  (T
	   (SETQ *MODE-QUANTITY-NAME* MODE-NAME)
	   (COMMAND-STORE 'COM-QUANTITY-FORWARD 506 *COMTAB*)
	   (COMMAND-STORE 'COM-QUANTITY-BACKWARD 502 *COMTAB*)
	   (COMMAND-STORE 'COM-QUANTITY-DELETE 504 *COMTAB*)
	   (COMMAND-STORE 'COM-QUANTITY-RUBOUT 207 *COMTAB*)
	   (COMMAND-STORE 'COM-QUANTITY-TWIDDLE 524 *COMTAB*))))

(DEFCOM COM-PREVIOUS-PAGE "Move to the previous page" (KM)
    (MOVE-BP (POINT) (FORWARD-PAGE (POINT) (MINUS *NUMERIC-ARG*) T))
    DIS-BPS)

(DEFCOM COM-NEXT-PAGE "Move to the next page" (KM)
    (MOVE-BP (POINT) (FORWARD-PAGE (POINT) *NUMERIC-ARG* T))
    DIS-BPS)

(DEFCOM COM-MARK-WHOLE "Put mark at beginning of buffer and point end,
or with a numeric argument, vice versa" (SM)
    (LET ((BP1 (POINT)) (BP2 (MARK)))
     (AND *NUMERIC-ARG-P* (PSETQ BP1 BP2 BP2 BP1))
     (MOVE-BP BP1 (INTERVAL-LAST-BP *INTERVAL*))
     (MOVE-BP BP2 (INTERVAL-FIRST-BP *INTERVAL*)))
    DIS-BPS)

(DEFCOM COM-MARK-DEFUN "Put point and mark around current defun." ()
  (LET ((INT (DEFUN-INTERVAL (POINT) *NUMERIC-ARG* NIL T T))) ;including previous blank line
    (OR INT (BARF))
    (SETF (WINDOW-MARK-P *WINDOW*) T)
    (SETQ *MARK-STAYS* T)
    (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
    (MOVE-BP (POINT) (INTERVAL-FIRST-BP INT))
    (MOVE-BP (MARK) (INTERVAL-LAST-BP INT)))
  DIS-BPS)

(DEFCOM COM-REPOSITION-WINDOW "Try to get all of current defun in the window.
Wins if the beginning of the current defun can be at the top of the window with
the current position still visible." (KM)
  (LET ((POINT (POINT))
        (SHEET (WINDOW-SHEET *WINDOW*))
        (N-PLINES (WINDOW-N-PLINES *WINDOW*))
        (INT (DEFUN-INTERVAL (POINT) 1 T T))
        START-BP END-BP TOP-BP)
    (COND ((NOT (NULL INT))
	   (SETQ START-BP (INTERVAL-FIRST-BP INT)
		 END-BP (INTERVAL-LAST-BP INT))
	   ;; Don't include the blank line after the defun
	   (AND (ZEROP (BP-INDEX END-BP)) (SETQ END-BP (END-LINE END-BP -1 T)))
	   (COND ((AND (PLINE-OF-POINT T *WINDOW* START-BP) ;If start of defun on the screen
		       (NULL (PLINE-OF-POINT T *WINDOW* END-BP))	;and not bottom
		       (MULTIPLE-VALUE-BIND (LINE INDEX)
			   (PUT-POINT-AT-PLINE SHEET (BP-LINE END-BP) (BP-INDEX END-BP)
					       (1- N-PLINES) (INTERVAL-FIRST-BP *INTERVAL*)
					       (INTERVAL-LAST-BP *INTERVAL*))
			 (SETQ TOP-BP (CREATE-BP LINE INDEX))
			 ;; And can fit bottom of the defun on as well
			 (NOT (BP-< START-BP TOP-BP)))))
		 ((BP-< START-BP (SETQ TOP-BP (MULTIPLE-VALUE-BIND (LINE INDEX)
						  (PUT-POINT-AT-PLINE SHEET (BP-LINE POINT)
						     (BP-INDEX POINT) (1- N-PLINES)
						     START-BP
						     (INTERVAL-LAST-BP *INTERVAL*))
						(CREATE-BP LINE INDEX))))
		  ;; If displaying from the start of the defun would push point off
		  ;; the bottom, complain, and bring in as much as possible anyway.
		  (BEEP))
		 (T
		  (SETQ TOP-BP START-BP)))
	   (RECENTER-WINDOW *WINDOW* ':START TOP-BP))
	  (T (BARF "no defun here")))
    DIS-NONE))

(DEFCOM COM-UPCASE-DIGIT "Up-shift the previous digit on this or the previous line." ()
    (LET ((BP (COPY-BP (POINT))))
     (RCHARMAP (BP (BEG-LINE (POINT) -1 T) NIL)
      (COND ((MEMQ (RCHARMAP-CH-CHAR) '(#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9))
	     (RCHARMAP-SET-CHAR (LOGXOR (RCHARMAP-CHAR) 20))
	     (RCHARMAP-RETURN NIL)))))
    DIS-TEXT)

(LOCAL-DECLARE ((SPECIAL STREAM ARRAY UNRCHF OPS))
(DEFUN MAKE-RECORDING-STREAM (STREAM LENGTH &AUX ARRAY UNRCHF OPS)
    (SETQ ARRAY (MAKE-ARRAY NIL 'ART-Q LENGTH NIL '(0 0)))
    (STORE-ARRAY-LEADER LENGTH ARRAY 0)
    (SETQ OPS (APPEND (FUNCALL STREAM ':WHICH-OPERATIONS) NIL))
    (MAPC (FUNCTION (LAMBDA (X) (SETQ OPS (DELQ X OPS))))
	  '(:TYI :UNTYI :PLAYBACK :RECORD       ; Operations I want.
	    :LINE-IN))                          ; Operations for default handler.
    (SETQ OPS `(:TYI :UNTYI :PLAYBACK :RECORD . ,OPS))
    (CLOSURE '(STREAM ARRAY UNRCHF OPS)
	     #'RECORDING-STREAM))

(DEFSELECT (RECORDING-STREAM RECORDING-STREAM-DEFAULT-HANDLER T)
  ((:TYI :ANY-TYI :MOUSE-OR-KBD-TYI
    :TYI-NO-HANG :ANY-TYI-NO-HANG :MOUSE-OR-KBD-TYI-NO-HANG) ()
   (COND (UNRCHF
	  (PROG1 UNRCHF (SETQ UNRCHF NIL)))
	 (T (MULTIPLE-VALUE-BIND (CHAR TEM)
		(FUNCALL STREAM SI:**DEFSELECT-OP**)
	      (RECORDING-STREAM ':RECORD CHAR)
	      (MVRETURN CHAR TEM)))))
  (:LISTEN ()
   (OR UNRCHF (FUNCALL STREAM ':LISTEN)))
  (:WHICH-OPERATIONS ()
   OPS)
  (:UNTYI (CH)
   (SETQ UNRCHF CH))
  (:RECORD (CH)
   (LET ((PTR (ARRAY-LEADER ARRAY 1)))
	(COND ((NUMBERP CH)
	       (SETQ PTR (\ (1+ PTR) (ARRAY-LEADER ARRAY 0)))
	       (STORE-ARRAY-LEADER PTR ARRAY 1)
	       (ASET CH ARRAY PTR)))))
  (:PLAYBACK ()
   ARRAY))

(DEFUN RECORDING-STREAM-DEFAULT-HANDLER (OP &REST REST)
  (IF (MEMQ OP OPS)
      (LEXPR-FUNCALL STREAM OP REST)
      (STREAM-DEFAULT-HANDLER 'RECORDING-STREAM OP (CAR REST) (CDR REST))))
);LOCAL-DECLARE

(DEFCOM COM-WHAT-LOSSAGE "What commands did I type to cause this lossage?
Prints out descriptions of the last sixty characters typed on the keyboard." (KM)
  (COND ((NOT (MEMQ ':PLAYBACK (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS)))
	 (BARF "Your input was not being recorded; sorry."))
	(T (LET ((A (FUNCALL STANDARD-INPUT ':PLAYBACK)))
		(LET ((P (ARRAY-LEADER A 1))
		    (L (ARRAY-LEADER A 0)))
		(DO ((I (\ (1+ P) L) (\ (1+ I) L))
		     (J 0 (1+ J)))
		    (( J L))
		  (LET ((CH (AREF A I)))
		     (COND ((NOT (NULL CH))
			    (FORMAT T "~:C " CH)))))))))
  DIS-NONE)

(DEFCOM COM-EXIT-CONTROL-R "Exits from a recursive edit" ()
  (*THROW 'EXIT-CONTROL-R NIL))

(DEFCOM COM-QUIT "Return from the top-level edit" ()
  (*THROW 'EXIT-TOP-LEVEL NIL))

(DEFVAR *INSIDE-BREAK* NIL)
(DEFCOM COM-BREAK "Enter a lisp break loop" ()
  (UNWIND-PROTECT
    (LET ((*INSIDE-BREAK* T))
      (BREAK ZMACS))
    (FUNCALL-SELF ':EXPOSE-MODE-LINE-WINDOW))
  (FUNCALL *TYPEOUT-WINDOW* ':MAKE-COMPLETE)
  DIS-NONE)

; TAB TO TAB STOP stuff.
;This should really be something hairy with the mouse...
; A "tab stop buffer" has two lines: one to indicate characters to fill
;with, and the second to indicate where the tab stops are.  In the second line
;colons and periods are the only significant characters.  Everything else
;is ignored.  If there is a :, fill with spaces, else with contents of 
;the first line.  I dont think this can work reasonably with variable
;width fonts very well, so the initial version, at least, will assume
;that you are using only one fixed width font.

(DEFUN INITIALIZE-TAB-STOP-BUFFER ()
    (SETQ *TAB-STOP-BUFFER* (CREATE-INTERVAL NIL NIL T))
    (INSERT (INTERVAL-FIRST-BP *TAB-STOP-BUFFER*)
	      "                                                                                          
        :       :       :       :       :       :       :       :       :       :       :       :")
    NIL)

(DEFCOM COM-EDIT-TAB-STOPS "Edit the tab-stop buffer." ()
    (RECURSIVE-EDIT *TAB-STOP-BUFFER* "Edit tab stops")
    DIS-ALL)

(DEFCOM COM-TAB-TO-TAB-STOP "Tab to fixed column as specified by the tab-stop buffer." ()
  (LET ((GOAL (BP-VIRTUAL-INDENTATION (POINT)))
	(L2 (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *TAB-STOP-BUFFER*))))
	(CHAR-POS))
    (MULTIPLE-VALUE (NIL CHAR-POS)
      (TV:SHEET-STRING-LENGTH (WINDOW-SHEET *WINDOW*) L2 0 NIL GOAL))
    (AND CHAR-POS
	 (SETQ GOAL (DO ((I 0 (1+ I))
			 (CP CHAR-POS))
			(( I *NUMERIC-ARG*) CP)
		      (SETQ CP (OR (STRING-SEARCH-SET '(#/: #/.) L2 (1+ CP))
				   (LET ((BP (END-OF-LINE L2)))
				     (INSERT BP "       :")
				     (INSERT (END-LINE BP -1) "        ")
				     (SETQ I (1- I))
				     CP)))))
	 (IF (CHAR-EQUAL (AREF L2 GOAL) #/:)
	     (INDENT-TO (POINT) (BP-VIRTUAL-INDENTATION (CREATE-BP L2 GOAL)))
	     (INSERT-MOVING (POINT) (NSUBSTRING (LINE-PREVIOUS L2) CHAR-POS GOAL)))))
  DIS-TEXT)

(DEFCOM COM-COMPILE-AND-EXIT "Compile the buffer and return from top-level" ()
  (FUNCALL *TYPEOUT-WINDOW* ':MAKE-COMPLETE)
  (COM-COMPILE-BUFFER)
  (OR (AND (FUNCALL *TYPEOUT-WINDOW* ':INCOMPLETE-P)	;If any compiler messages
	   (NOT (Y-OR-N-P "Exit anyway? " *TYPEOUT-WINDOW*)))
      (*THROW 'EXIT-TOP-LEVEL NIL))
  DIS-NONE)

(DEFCOM COM-EVALUATE-AND-EXIT "Evaluate the buffer and return from top-level" ()
  (COM-EVALUATE-BUFFER)
  (*THROW 'EXIT-TOP-LEVEL NIL))

(DEFCOM COM-GRIND-DEFINITION "Grind the definition of a function into the buffer.
Reads the name of the function from the mini-buffer and inserts its ground definition
at point." ()
    (LET ((SYMBOL (TYPEIN-LINE-READ "Name of function:")))
      (SI:GRIND-1 SYMBOL 90. (INTERVAL-STREAM (POINT) (POINT) T) T))
    DIS-TEXT)

(DEFCOM COM-GRIND-S-EXPRESSION "Grind the evaluation of a form into the buffer.
Reads a form from the mini-buffer, evals it and inserts the result, ground, at
point." ()
    (LET ((TEM (EVAL (TYPEIN-LINE-READ "Lisp form:"))))
      (GRIND-INTO-BP (POINT) TEM))
    DIS-TEXT)

(DEFCOM COM-DOWN-INDENTED-LINE "Move to the next line and past any indentation." (KM)
    (LET ((POINT (POINT)) (EOL))
      (COND ((AND (NOT *NUMERIC-ARG-P*)
		  (BP-= (SETQ EOL (END-LINE POINT))
			(INTERVAL-LAST-BP *INTERVAL*)))
	     (MOVE-BP POINT (INSERT-MOVING EOL #\CR))
	     DIS-TEXT)
	    (T
	     (MOVE-BP POINT (FORWARD-OVER *BLANKS* (FORWARD-LINE POINT *NUMERIC-ARG* T)))
	     DIS-BPS))))

(DEFCOM COM-UP-INDENTED-LINE "Move to previous line and after any indentation." (KM)
    (MOVE-BP (POINT) (FORWARD-OVER *BLANKS* (FORWARD-LINE (POINT) (- *NUMERIC-ARG*) T)))
    DIS-BPS)

(DEFCOM COM-TEXT-JUSTIFIER-CHANGE-FONT-WORD "Puts the previous word in a different font (R).
The font to change to is specified with a numeric argument.
No arg means move last font change forward past next word.
A negative arg means move last font change back one word." ()
  (IF (AND *NUMERIC-ARG-P* (PLUSP *NUMERIC-ARG*))
      (LET ((BP1 (OR (FORWARD-WORD (POINT) -1) (BARF)))	;Positive explicit arg,
	    BP2)
	(SETQ BP2 (FORWARD-WORD BP1 1 T))		;Surround previous word
	(INSERT BP2 "*")
	(INSERT-MOVING BP1 #/)
	(INSERT BP1 (+ *NUMERIC-ARG* #/0)))		;With indicated font change
      (MULTIPLE-VALUE-BIND (BP1 BP2 TYPE)
	  (FIND-FONT-CHANGE (POINT) (INTERVAL-FIRST-BP *INTERVAL*) T)
	(OR BP1 (BARF))					;Find previous font change
	(DELETE-INTERVAL BP1 BP2 T)			;Flush it
	(LET ((BP3 (FORWARD-WORD BP1 (IF (MINUSP *NUMERIC-ARG*) -2 1) T))	;Where it goes
	      BP4 BP5 NTYPE)
	  (MULTIPLE-VALUE (BP4 BP5 NTYPE)
	    (FIND-FONT-CHANGE BP3 BP1 NIL))		;If moving over another one
	  (OR (MINUSP *NUMERIC-ARG*)
	      (SETQ TYPE NTYPE))
	  (OR (COND (BP4
		     (DELETE-INTERVAL BP4 BP5 T)	;flush it
		     (CHAR-EQUAL (AREF TYPE 1) #/*)))
	      (INSERT BP3 TYPE)))))			;Put in one moved unless was *
  DIS-TEXT)

(DEFCOM COM-TEXT-JUSTIFIER-CHANGE-FONT-REGION "Puts the region in a different font (R).
The font to change to is specified with a numeric argument.
Inserts ^F<n> before and ^F* after.
A negative arg removes font changes in or next to region." ()
  (REGION (BP1 BP2)
    (COND ((NOT (MINUSP *NUMERIC-ARG*))
	   (INSERT BP2 "*")
	   (INSERT-MOVING BP1 #/)
	   (INSERT BP1 (+ #/0 *NUMERIC-ARG*)))
	  (T
	   (AND (LOOKING-AT BP2 #/)
		(DELETE-INTERVAL BP2 (FORWARD-CHAR BP2 2) T))
	   (OR (LOOKING-AT-BACKWARD BP1 #/)
	       (SETQ BP1 (FORWARD-CHAR BP1 -1)))
	   (AND (LOOKING-AT-BACKWARD BP1 #/)
		(DELETE-INTERVAL (FORWARD-CHAR BP1 -2) BP2 T))
	   (DO ((BP3))
	       (NIL)
	     (MULTIPLE-VALUE (BP1 BP3)
	       (FIND-FONT-CHANGE BP1 BP2 NIL))
	     (OR BP1 (RETURN NIL))
	     (DELETE-INTERVAL BP1 BP3 T)))))
  DIS-TEXT)

(DEFUN FIND-FONT-CHANGE (BP LIMIT-BP REVERSE-P &AUX BP1 BP2)
  (COND ((SETQ BP1 (SEARCH BP #/ REVERSE-P NIL NIL LIMIT-BP))
	 (IF (NOT REVERSE-P)
	     (SETQ BP2 BP1
		   BP1 (FORWARD-CHAR BP2 -2))
	     (SETQ BP2 (FORWARD-CHAR BP1 2)))
	 (MVRETURN BP1 BP2 (STRING-INTERVAL BP1 BP2 T)))))

(DEFCOM COM-TEXT-JUSTIFIER-UNDERLINE-WORD " Puts underlines around the previous word (R).
If there is an underline begin or end near that word, it is moved forward one word.
An argument specifies the number of words, and the direction: positive means forward.
*TEXT-JUSTIFIER-UNDERLINE-BEGIN* is the character that begins underlines and
*TEXT-JUSTIFIER-UNDERLINE-END* is the character that ends it." ()
  (LET ((LIST (LIST *TEXT-JUSTIFIER-UNDERLINE-BEGIN* *TEXT-JUSTIFIER-UNDERLINE-END*))
	(BP (FORWARD-TO-WORD (POINT)))
	BP1 TYPE)
    (SETQ BP1 (FORWARD-WORD (FORWARD-WORD BP1 -2 T)))
    (MULTIPLE-VALUE (BP TYPE)
      (SEARCH-SET BP1 LIST T NIL BP))
    (IF (NULL BP)
	(LET ((ARG (IF *NUMERIC-ARG-P* *NUMERIC-ARG* -1)))
	     (LET ((BP2 (OR (FORWARD-WORD BP1 ARG) (BARF))))
	       (COND ((MINUSP ARG)
		      (SETQ BP1 (FORWARD-WORD BP2 (- ARG)))
		      (INSERT BP1 *TEXT-JUSTIFIER-UNDERLINE-END*)
		      (INSERT BP2 *TEXT-JUSTIFIER-UNDERLINE-BEGIN*))
		     (T
		      (INSERT BP2 *TEXT-JUSTIFIER-UNDERLINE-END*)
		      (INSERT BP1 *TEXT-JUSTIFIER-UNDERLINE-BEGIN*)))))
	(DELETE-INTERVAL BP (FORWARD-CHAR BP) T)
	(SETQ BP1 (IF (MINUSP *NUMERIC-ARG*)
		      (FORWARD-WORD (FORWARD-WORD BP (1- *NUMERIC-ARG*) T))
		      (FORWARD-TO-WORD BP (1+ *NUMERIC-ARG*) T)))
	(MULTIPLE-VALUE-BIND (BP2 NTYPE)
	    (SEARCH-SET BP LIST (MINUSP *NUMERIC-ARG*) NIL BP1 )
	  (OR (COND (BP2
		     (DELETE-INTERVAL BP2 (FORWARD-CHAR BP2 (IF (MINUSP *NUMERIC-ARG*) 1 -1)))
		     ( TYPE NTYPE)))
	      (LET ((BP3 (IF (MINUSP *NUMERIC-ARG*)
			     (FORWARD-WORD (FORWARD-WORD BP (1- *NUMERIC-ARG*)))
			     (FORWARD-WORD BP *NUMERIC-ARG*))))
		(INSERT BP3 TYPE))))))
  DIS-TEXT)

(DEFCOM COM-TEXT-JUSTIFIER-UNDERLINE-REGION "Puts underlines a la R around the region.
A negative argument removes underlines in or next to region.
*TEXT-JUSTIFIER-UNDERLINE-BEGIN* is the character that begins underlines and
*TEXT-JUSTIFIER-UNDERLINE-END* is the character that ends it." ()
  (REGION (BP1 BP2)
    (LET ((LIST (LIST *TEXT-JUSTIFIER-UNDERLINE-BEGIN* *TEXT-JUSTIFIER-UNDERLINE-END*)))
      (IF (MINUSP *NUMERIC-ARG*)
	  (DO ((BP (FORWARD-WORD (FORWARD-WORD BP1 -1 T)))
	       (LIM-BP (FORWARD-WORD BP2 1 T)))
	      (NIL)
	    (OR (SETQ BP (SEARCH-SET BP LIST NIL NIL LIM-BP))
		(RETURN NIL))
	    (DELETE-INTERVAL (FORWARD-CHAR BP -1) BP T))
	  (INSERT BP2 *TEXT-JUSTIFIER-UNDERLINE-END*)
	  (INSERT BP1 *TEXT-JUSTIFIER-UNDERLINE-BEGIN*))))
  DIS-TEXT)
