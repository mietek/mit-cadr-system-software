;;; Zwei commands, see ZWEI;COMA for comments -*-Mode:LISP; Package:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Q-Register Commands.

;;; Puts the PROMPT in the mode line, and gets a qreg name in the echo area.
;;; Returns a symbol in the utility-package.
(DEFUN GET-Q-REG-NAME (PROMPT &AUX CHAR STR OLDP SYM XCHAR)
  (SETQ XCHAR (FUNCALL STANDARD-INPUT ':TYI-NO-HANG))
  (COND ((NULL XCHAR)
	 (PROMPT-LINE "~A" PROMPT)
	 (TYPEIN-LINE "Q-Reg: ")
	 (TYPEIN-LINE-ACTIVATE
	   (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI)))))
  (SETQ CHAR (CHAR-UPCASE (LDB %%CH-CHAR (OR XCHAR CHAR))))
  (OR XCHAR (TYPEIN-LINE-MORE "~C" CHAR))
  (SETQ STR (MAKE-ARRAY NIL 'ART-STRING 1))
  (ASET CHAR STR 0)
  (MULTIPLE-VALUE (SYM OLDP)
    (INTERN-LOCAL STR *UTILITY-PACKAGE*))
  (COND (OLDP
	 (RETURN-ARRAY STR))
	(T (PUSH SYM *Q-REG-LIST*)))
  SYM)

(DEFCOM COM-OPEN-GET-Q-REG "Insert text in a specified Q-reg, overwriting
blank lines the way Return does (calling the definition of Return).
Leaves the point after, and the mark before, the text.
With an argument, puts point before and mark after." ()
  (LET ((QREG (GET-Q-REG-NAME "Get text from Q-Register.")))
    (LET ((POINT (POINT))
	  (MARK (MARK))
	  (THING (GET QREG 'TEXT)))
      (OR THING (BARF "The q-register ~A does not contain any text." QREG))
      (MOVE-BP MARK (INSERT-INTERVAL POINT THING))
      (SETQ *CURRENT-COMMAND-TYPE* 'YANK)
      (LET ((SAVE-PT (COPY-BP POINT))
	    (NL (1- (COUNT-LINES POINT MARK))))
	(AND (BEG-LINE-P (MARK))
	     (MOVE-BP MARK (FORWARD-CHAR MARK -1)))
	(MOVE-BP POINT MARK)
	(DOTIMES (I NL)
	  (KEY-EXECUTE #\CR))
	(DELETE-INTERVAL POINT MARK)
	(MOVE-BP (POINT) SAVE-PT))
      (OR *NUMERIC-ARG-P*
	  (SWAP-BPS POINT MARK))))
  DIS-TEXT)

(DEFCOM COM-GET-Q-REG "Get contents of Q-reg (reads name from kbd).
Leaves the pointer before, and the mark after, the text.
With argument, puts point after and mark before." ()
  (LET ((QREG (GET-Q-REG-NAME "Get text from Q-Register.")))
    (LET ((THING (GET QREG 'TEXT)))
      (OR THING (BARF "The q-register ~A does not contain any text." QREG))
      (MOVE-BP (MARK) (INSERT-INTERVAL (POINT) THING))
      (SETQ *CURRENT-COMMAND-TYPE* 'YANK)
      (AND *NUMERIC-ARG-P*
	   (SWAP-BPS (POINT) (MARK)))))
  DIS-TEXT)

(DEFCOM COM-PUT-Q-REG "Put point to mark into q-reg (reads name from kbd).
With an argument, the text is also deleted." ()
  (REGION (BP1 BP2)
    (LET ((QREG (GET-Q-REG-NAME "Put text into Q-Register.")))
      (PUTPROP QREG (COPY-INTERVAL BP1 BP2 T) 'TEXT)
      (COND (*NUMERIC-ARG-P*
	     (DELETE-INTERVAL (POINT) (MARK))
	     DIS-TEXT)
	    (T DIS-NONE)))))

(DEFCOM COM-VIEW-Q-REGISTER "Display the contents of a q-reg (reads name from kbd)." (KM)
  (VIEW-Q-REG (GET-Q-REG-NAME "View Q-Register."))
  DIS-NONE)

(DEFUN VIEW-Q-REG (SYM)
  (LET ((TEXT (GET SYM 'TEXT)))
    (FORMAT T "~&~10,5,2A~A~%" SYM
	    (COND ((NULL TEXT) "[EMPTY]")
		  ((< (STRING-LENGTH (SETQ TEXT (BP-LINE (INTERVAL-FIRST-BP TEXT))))
		      50.) TEXT)
		  (T (NSUBSTRING TEXT 0 50.))))))

(DEFCOM COM-LIST-Q-REGISTERS "List and display the contents of all defined q-regs." ()
  (FORMAT T "List of all Q-registers:")
  (DO L *Q-REG-LIST* (CDR L) (NULL L)
      (VIEW-Q-REG (CAR L)))
  (FORMAT T "Done.")
  DIS-NONE)

(DEFCOM COM-KILL-Q-REGISTER "Kill a q-reg." ()
  (LET ((Q-REG (GET-Q-REG-NAME "Kill Q-Register.")))
    (COND ((GET Q-REG 'TEXT)
	   (SETQ *Q-REG-LIST* (DELQ Q-REG *Q-REG-LIST*))
	   (REMPROP Q-REG 'TEXT))
	  (T (BARF "The q-register ~S is not defined." Q-REG))))
  DIS-NONE)

(DEFCOM COM-POINT-TO-Q-REG "Save the current location in a q-reg." ()
  (LET ((Q-REG (GET-Q-REG-NAME "Point to Q-Register")))
    (LET ((PT (GET Q-REG 'POINT)))
      (COND (PT
	     (MOVE-BP (CAR PT) (POINT))
	     (RPLACD PT *INTERVAL*))
	    (T
	     (SETQ PT (CONS (COPY-BP (POINT) ':NORMAL) *INTERVAL*))))
      (PUTPROP Q-REG PT 'POINT)))
  DIS-NONE)

(DEFCOM COM-Q-REG-TO-POINT "Restore a saved point from a q-reg." (KM)
  (LET ((Q-REG (GET-Q-REG-NAME "Q-Register to point")))
    (LET ((PT (GET Q-REG 'POINT)))
      (COND ((NULL PT)
	     (BARF "The q-register ~A doesnt point anywhere." Q-REG))
	    ((NEQ (CDR PT) *INTERVAL*)
	     (BARF "That q-register ~A doesnt point to this buffer." Q-REG)))
      (MOVE-BP (POINT) (CAR PT))))
  DIS-BPS)

;;; Completing-reader and other mini-buffer stuff

(DEFCOM COM-END-OF-MINI-BUFFER "Terminate input from the typein line." ()
  (*THROW 'RETURN-FROM-COMMAND-LOOP NIL))

;; The c-G command in the minibuffer.
(DEFCOM COM-MINI-BUFFER-BEEP "Quit out of the mini buffer.
If there is text in the mini buffer, delete it all.
If the mini buffer is empty, quit out of it." ()
  (BEEP)
  (COND (*NUMERIC-ARG-P* DIS-NONE)
	((BP-= (INTERVAL-FIRST-BP *INTERVAL*) (INTERVAL-LAST-BP *INTERVAL*))
	 (*THROW 'TOP-LEVEL T))
	(T
	 (DELETE-INTERVAL *INTERVAL*)
	 DIS-TEXT)))

(DEFVAR *MINI-BUFFER-COMMAND-IN-PROGRESS* NIL)

(DEFUN EDIT-IN-MINI-BUFFER (&OPTIONAL (COMTAB *MINI-BUFFER-COMTAB*)
				      INITIAL-CONTENTS INITIAL-CHAR-POS MODE-LINE-LIST)
  (AND *MINI-BUFFER-REPEATED-COMMAND*
       (POP *MINI-BUFFER-REPEATED-COMMAND* INITIAL-CONTENTS)
       (SETQ INITIAL-CHAR-POS (STRING-LENGTH INITIAL-CONTENTS)))
  (DELETE-INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
  (MUST-REDISPLAY *MINI-BUFFER-WINDOW* DIS-TEXT)
  (LET ((*INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
	(BP (WINDOW-POINT *MINI-BUFFER-WINDOW*)))
    (AND INITIAL-CONTENTS
	 (INSERT BP INITIAL-CONTENTS))
    (AND INITIAL-CHAR-POS
	 (MOVE-BP BP (FORWARD-CHAR BP INITIAL-CHAR-POS))))
  (OR *MINI-BUFFER-COMMAND* 
      (MINI-BUFFER-RING-PUSH (SETQ *MINI-BUFFER-COMMAND*
				   `((,*CURRENT-COMMAND* ,*NUMERIC-ARG-P* ,*NUMERIC-ARG*)))))
  (PROG KLUDGE (VAL)
    (UNWIND-PROTECT
      (LET ((*MINI-BUFFER-COMMAND-IN-PROGRESS* *CURRENT-COMMAND*)
	    (PACKAGE PACKAGE)
	    (*MODE-LINE-LIST* MODE-LINE-LIST)
	    (INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
	(SETQ VAL (COMMAND-LOOP COMTAB *MINI-BUFFER-WINDOW*))
	(RPLACD (LAST *MINI-BUFFER-COMMAND*) (NCONS (STRING-INTERVAL INTERVAL)))
	(RETURN-FROM KLUDGE VAL *MINI-BUFFER-WINDOW* INTERVAL))
      (DISAPPEAR-MINI-BUFFER-WINDOW)
      (AND (WINDOW-EXPOSED-P *WINDOW*)
	   (LET ((TYPEOUT-WINDOW (TV:ESSENTIAL-WINDOW-WITH-TYPEOUT-MIXIN-TYPEOUT-WINDOW
				   (WINDOW-SHEET *WINDOW*))))
	     (IF (TV:BASIC-TYPEOUT-WINDOW-BOTTOM-REACHED TYPEOUT-WINDOW)
		 (FUNCALL TYPEOUT-WINDOW ':SELECT)
		 (SELECT-WINDOW *WINDOW*)))))))

(DEFCOM COM-REPEAT-LAST-MINI-BUFFER-COMMAND "Repeat a recent mini-buffer command" ()
  (IF (NOT (ZEROP *NUMERIC-ARG*))
      (RE-EXECUTE-MINI-BUFFER-COMMAND (NTH (1- *NUMERIC-ARG*) *MINI-BUFFER-RING*))
      (FUNCALL *TYPEOUT-WINDOW* ':LINE-OUT "Recent mini-buffer commands:")
      (DO ((RING *MINI-BUFFER-RING* (CDR RING))
	   (COMMAND) (ARG-P) (ARG) (STR))
	  ((NULL RING))
	(SETQ COMMAND (CAAR RING))
	(SETQ ARG-P (CADR COMMAND)
	      ARG (CADDR COMMAND))
	(SETQ COMMAND (CAR COMMAND)
	      STR (OR (KEY-FOR-COMMAND COMMAND)
		      (GET COMMAND 'COMMAND-NAME)))
	(AND ARG-P
	     (SETQ STR (STRING-APPEND (FORMAT-ARGUMENT ARG-P ARG) #\SP STR)))
	(DOLIST (CONTENTS (CDAR RING))
	  (SETQ STR (STRING-APPEND STR #\SP CONTENTS)))
	(FUNCALL *TYPEOUT-WINDOW* ':ITEM ':MINI-BUFFER-COMMAND (CAR RING) STR)
	(FUNCALL *TYPEOUT-WINDOW* ':TYO #\CR))
      DIS-NONE))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* :MINI-BUFFER-COMMAND "Re-execute"
			  RE-EXECUTE-MINI-BUFFER-COMMAND T)

(DEFUN RE-EXECUTE-MINI-BUFFER-COMMAND (*MINI-BUFFER-REPEATED-COMMAND*)
  (OR *MINI-BUFFER-REPEATED-COMMAND* (BARF "No previous command"))
  (POP *MINI-BUFFER-REPEATED-COMMAND* `(,*CURRENT-COMMAND* ,*NUMERIC-ARG-P* ,*NUMERIC-ARG*))
  (FUNCALL *CURRENT-COMMAND*))

(DEFCOM COM-POP-MINI-BUFFER-RING "Abort this mini-buffer command and redo the last one" ()
  (LET ((COMMAND (CAR *MINI-BUFFER-RING*)))
    ;; Setup to repeat the one before this
    (FUNCALL STANDARD-INPUT ':UNTYI `(:EXECUTE RE-EXECUTE-MINI-BUFFER-COMMAND ,COMMAND))
    ;; Flush this one and move that to the end
    (SETQ *MINI-BUFFER-RING* (NCONC (CDR *MINI-BUFFER-RING*) (NCONS COMMAND))))
  (*THROW 'TOP-LEVEL T))

(DEFVAR *COMPLETING-ALIST*)
(DEFVAR *COMPLETING-IMPOSSIBLE-IS-OK-P*)
(DEFVAR *COMPLETING-HELP-MESSAGE*)
(DEFVAR *COMPLETING-DOCUMENTER*)
(DEFVAR *COMPLETING-DELIMS* '(#\SP #/-))

(DEFUN COMPLETING-READ-FROM-MINI-BUFFER (PROMPT *COMPLETING-ALIST*
						&OPTIONAL *COMPLETING-IMPOSSIBLE-IS-OK-P*
						INITIAL-COMPLETE
						*COMPLETING-HELP-MESSAGE*
						*COMPLETING-DOCUMENTER*
						&AUX CONTENTS CHAR-POS)
  (AND INITIAL-COMPLETE
       (MULTIPLE-VALUE (CONTENTS NIL NIL NIL CHAR-POS)
	 (COMPLETE-STRING "" *COMPLETING-ALIST* *COMPLETING-DELIMS* T 0)))
  (EDIT-IN-MINI-BUFFER *COMPLETING-READER-COMTAB* CONTENTS CHAR-POS
		       (AND PROMPT (NCONS PROMPT))))

;; Note that WINDOW is a window system type window, not a ZWEI-WINDOW
(DEFUN COMPLETING-READ (WINDOW *COMPLETING-ALIST*
			&OPTIONAL PROMPT *COMPLETING-IMPOSSIBLE-IS-OK-P*
			INITIAL-COMPLETE *COMPLETING-HELP-MESSAGE*
			*COMPLETING-DOCUMENTER*
			&AUX ZWEI-WINDOW CONTENTS CHAR-POS)
  (AND INITIAL-COMPLETE
       (MULTIPLE-VALUE (CONTENTS NIL NIL NIL CHAR-POS)
	 (COMPLETE-STRING "" *COMPLETING-ALIST* *COMPLETING-DELIMS* T 0)))
  (AND PROMPT (FUNCALL WINDOW ':SET-LABEL PROMPT))
  (SETQ ZWEI-WINDOW (FUNCALL WINDOW ':ZWEI-WINDOW))
  (LET ((INTERVAL (WINDOW-INTERVAL ZWEI-WINDOW)))
    (IF INTERVAL (DELETE-INTERVAL INTERVAL)
	(SET-WINDOW-INTERVAL ZWEI-WINDOW (CREATE-INTERVAL NIL NIL T))))
  (SETF (WINDOW-REDISPLAY-DEGREE ZWEI-WINDOW) DIS-ALL)
  (AND CONTENTS (NOT (EQUAL CONTENTS ""))
       (LET ((*INTERVAL* (WINDOW-INTERVAL ZWEI-WINDOW))
	     (BP (WINDOW-POINT ZWEI-WINDOW)))
	 (INSERT BP CONTENTS)
	 (AND CHAR-POS (MOVE-BP BP (FORWARD-CHAR BP CHAR-POS)))))
  (LET ((OLD-STATUS (FUNCALL WINDOW ':STATUS)))
    (UNWIND-PROTECT
      (TV:WINDOW-CALL (WINDOW)
	(COMMAND-LOOP *COMPLETING-READER-COMTAB* ZWEI-WINDOW 'TOP-LEVEL-EDITOR))
      (FUNCALL WINDOW ':SET-STATUS OLD-STATUS))))

(DEFCOM COM-COMPLETE "Attempt to complete the current line." ()
  (COMPLETE-LINE T T)
  DIS-TEXT)

(DEFCOM COM-SELF-INSERT-AND-COMPLETE "Attempt to complete after inserting break character." ()
  (OR (END-LINE-P (POINT)) (INSERT-MOVING (POINT) *LAST-COMMAND-CHAR*))
  (COMPLETE-LINE NIL NIL *LAST-COMMAND-CHAR*)
  DIS-TEXT)

(DEFCOM COM-COMPLETE-AND-EXIT "Attempt to complete and return if unique." ()
  (PROG ((LINE (BP-LINE (WINDOW-START-BP *WINDOW*)))
	 COMPLETION VAL)
    (SETQ VAL (COND ((ZEROP (LINE-LENGTH LINE))	;Allow typing just CR
		     "")
		    ((NOT *COMPLETING-IMPOSSIBLE-IS-OK-P*) ;Not allowed to type new things,
		     (SETQ COMPLETION (COMPLETE-LINE T NIL))
		     (COND ((NULL (CDR COMPLETION))
			    (SETQ VAL (CAR COMPLETION)))
			   ((NULL (SETQ VAL (ASSOC LINE COMPLETION))) ;Something ambiguous,
			    (RETURN NIL)))	;return for something good
		     (MUST-REDISPLAY *WINDOW* DIS-TEXT)	;Typed something good
		     (AND (WINDOW-READY-P *WINDOW*) (REDISPLAY *WINDOW* ':NONE))
		     VAL)
		    ((AND (EQ *COMPLETING-IMPOSSIBLE-IS-OK-P* 'MAYBE)
			  ;; If allowed one failure
			  (NEQ *LAST-COMMAND-TYPE* 'FAILING-COMPLETION)
			  (NOT (BIT-TEST 400 *LAST-COMMAND-CHAR*)))
		     (SETQ COMPLETION (COMPLETE-LINE T NIL))
		     (SETQ COMPLETION (IF (= (LENGTH COMPLETION) 1) (CAR COMPLETION)
					  (ASSOC LINE COMPLETION)))
		     (COND ((NULL COMPLETION)	;This is no good
			    (SETQ *CURRENT-COMMAND-TYPE* 'FAILING-COMPLETION)
			    (BEEP)
			    (RETURN NIL))
			   (T
			    (MUST-REDISPLAY *WINDOW* DIS-TEXT)
			    (AND (WINDOW-READY-P *WINDOW*) (REDISPLAY *WINDOW* ':NONE))
			    COMPLETION)))
		    ((AND (NEQ *COMPLETING-IMPOSSIBLE-IS-OK-P* 'ALWAYS-STRING)
			  (SETQ COMPLETION (ASS 'STRING-EQUAL LINE
						(IF (ARRAYP *COMPLETING-ALIST*)
						    (G-L-P *COMPLETING-ALIST*)
						    *COMPLETING-ALIST*))))
		     COMPLETION)
		    (T
		     (STRING-APPEND LINE))))
    (*THROW 'RETURN-FROM-COMMAND-LOOP VAL))
  DIS-TEXT)

(DEFCOM COM-LIST-COMPLETIONS "Give a menu of possible completions for string so far." ()
  (LET (POSS)
    (MULTIPLE-VALUE (NIL POSS)
         (COMPLETE-STRING (BP-LINE (POINT)) *COMPLETING-ALIST* *COMPLETING-DELIMS*))
   (OR POSS (BARF))
   (AND *COMPLETING-HELP-MESSAGE* (FORMAT *TYPEOUT-WINDOW* "~&~A" *COMPLETING-HELP-MESSAGE*))
   (FORMAT *TYPEOUT-WINDOW*
	   "~&These are the possible completions of the text you have typed:~2%")
   (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'COMPLETION
	    (SORT (MAPCAR #'CAR POSS) #'STRING-LESSP))
   (TERPRI *TYPEOUT-WINDOW*)
   DIS-NONE))

(DEFCOM COM-COMPLETION-APROPOS "Do apropos within the completions of what has been typed." ()
  (LET ((LINE (BP-LINE (POINT)))
	FUNCTION)
    (LET (IDX)
      (IF (SETQ IDX (STRING-SEARCH-SET *COMPLETING-DELIMS* LINE))
	  (SETQ LINE (DO ((I 0)
			  (J IDX)
			  (LIST))
			 (NIL)
		       (PUSH (SUBSTRING LINE I J) LIST)
		       (OR J
			   (RETURN (NREVERSE LIST)))
		       (SETQ I (1+ J)
			     J (STRING-SEARCH-SET *COMPLETING-DELIMS* LINE I)))
		FUNCTION 'FSM-STRING-SEARCH)
	  (SETQ FUNCTION 'STRING-SEARCH)))
    (AND *COMPLETING-HELP-MESSAGE*
	 (FORMAT *TYPEOUT-WINDOW* "~&~A" *COMPLETING-HELP-MESSAGE*))
    (FORMAT *TYPEOUT-WINDOW*
	    "~&These are the completions matching~:[ /"~A/"~;~{ /"~A/"~^ or~}~]:"
	    (LISTP LINE) LINE)
    (AND (LISTP LINE)
	 (SETQ LINE (LIST LINE NIL NIL)))
    (DO ((ALIST (IF (ARRAYP *COMPLETING-ALIST*) (G-L-P *COMPLETING-ALIST*)
		    *COMPLETING-ALIST*)
		(CDR ALIST))
	 (POSS NIL))
	((NULL ALIST)
	 (FUNCALL *TYPEOUT-WINDOW* ':ITEM-LIST 'COMPLETION
		  (SORT (MAPCAR #'CAR POSS) #'STRING-LESSP)))
      (DO NIL ((LISTP ALIST)) (SETQ ALIST (CAR ALIST)))	;Indirect through multiple alists
      (AND (FUNCALL FUNCTION LINE (CAAR ALIST))
	   (PUSH (CAR ALIST) POSS))))
  (TERPRI *TYPEOUT-WINDOW*)
  DIS-NONE)
 
(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* COMPLETION "Select" SELECT-COMPLETION T)

;Called if the user mouses one of the completions
(DEFUN SELECT-COMPLETION (STRING)
  (OR (EQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)) (BARF))
  (FUNCALL *TYPEOUT-WINDOW* ':MAKE-COMPLETE)	;Only one completion can be meaningful
  (DELETE-INTERVAL *INTERVAL*)
  (INSERT-MOVING (POINT) STRING)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COM-COMPLETE-AND-EXIT))

;;; This command is on the HELP key when the user is in the completing reader.
;;; The caller of the completing reader can pass this two implicit arguments
;;; through the specal variables *COMPLETING-HELP-MESSAGE* and *COMPLETING-DOCUMENTER*.
;;; The command first prints the value of *COMPLETING-HELP-MESSAGE*, if non-NIL;
;;; otherwise it prints "You are in the completing reader."  The top-level value
;;; of this variable is NIL.  Then it explains how completion works, and tells
;;; the user what options he can complete to.  If there is only one option,
;;; and *COMPLETING-DOCUMENTER* is non-NIL, then *COMPLETING-DOCUMENTER* is
;;; applied to the one element of the ALIST that the user is indicating;
;;; the function should output helpful cruft to *TYPEOUT-WINDOW*.
(DEFCOM COM-DOCUMENT-COMPLETING-READ "Explain how the completing reader works.
Also tell you what you are currently doing." ()
  (LET (POSS)
   (FORMAT T "~&~A~2%"
	   (OR *COMPLETING-HELP-MESSAGE* "You are in the completing reader."))
   (FORMAT T
"You are typing to a mini-buffer, with the following commands redefined:
Altmode causes as much of the string as can be determined to be inserted
into the mini-buffer (this is called command completion).  Space and -
are similar; they complete up to the next Space and - respectively.
? lists all the strings that match what you have typed so far.
Return will complete as much as possible, and ")
   (FORMAT T
	   (IF *COMPLETING-IMPOSSIBLE-IS-OK-P*
	       "return the result."
	       "if that is a valid string it
will return it."))
   (FORMAT T "~2%")
   (MULTIPLE-VALUE (NIL POSS)
	 (COMPLETE-STRING (BP-LINE (POINT)) *COMPLETING-ALIST* *COMPLETING-DELIMS*))
   (SELECTQ (LENGTH POSS)
     (0 (FORMAT T "There are no possible completions of the text you have typed.~%"))
     (1 (FORMAT T "The only possible completion of the text you have typed
is ~A.~%" (CAAR POSS))
	(COND (*COMPLETING-DOCUMENTER*
	       (TERPRI T)
	       (FUNCALL *COMPLETING-DOCUMENTER* (CAR POSS)))))
     (OTHERWISE
      (FORMAT T "These are the possible completions of the text you have typed:~2%")
      (DO ((L POSS (CDR L))
	   (FLAG 1 0))
	  ((NULL L))
	(FORMAT T "~[, ~]~A" FLAG (CAAR L)))
      (TERPRI))))
   DIS-NONE)

(DEFUN COMPLETE-LINE (FORWARD-OK MUST-COMPLETE &OPTIONAL INSERT &AUX NSTR POSS WINP LINE POINT
								     CHAR-POS EOLP MAGIC-POS)
  (SETQ POINT (POINT))
  (SETQ LINE (BP-LINE POINT)
	CHAR-POS (BP-INDEX POINT))
  (SETQ EOLP (= CHAR-POS (LINE-LENGTH LINE)))
  (MULTIPLE-VALUE (NSTR POSS WINP CHAR-POS MAGIC-POS)
    (COMPLETE-STRING LINE *COMPLETING-ALIST* *COMPLETING-DELIMS* T CHAR-POS
		     (NOT FORWARD-OK)))
  (AND MAGIC-POS FORWARD-OK
       (SETQ CHAR-POS MAGIC-POS))
  (COND (POSS
	 (DELETE-INTERVAL (BEG-LINE POINT) (END-LINE POINT))
	 (INSERT-MOVING POINT NSTR)))
  ;; Insert the given character, unless we have fully completed only one completion.
  (AND INSERT EOLP (OR (NEQ WINP 'NOSPACE)
		       (AND (ASSOC LINE POSS) (NOT (NULL (CDR POSS)))))
       (INSERT-MOVING POINT INSERT))
  (COND (WINP)
	((AND (NOT *COMPLETING-IMPOSSIBLE-IS-OK-P*) (NULL POSS))
	 (BARF))
	(FORWARD-OK
	 (COND (MAGIC-POS
		(MOVE-BP POINT LINE MAGIC-POS))
	       ((AND MUST-COMPLETE (NULL POSS))
		(BARF)))))
  POSS)

;;;Complete a given STRING from an ALIST of strings.  CHAR-POS is position in string to be
;;;relocated with new things inserted.  TRUNC says dont complete more than one chunk at end
;;;Returns new STRING, matching subset of ALIST, COMPLETED-P if some completion was done,
;;;new CHAR-POS, and MAGIC-POS location of first point of ambiguity.  COMPLETED-P is 'NOSPACE
;;;if proper delimiter is already at end of string.
;;;For efficiency, if ALIST is an ART-Q-LIST array, it is assumed to be alphabetically
;;;sorted.  DONT-NEED-LIST says we really dont want all the possibilities, just not NIL.
(DEFUN COMPLETE-STRING (STRING ALIST DELIMS &OPTIONAL DONT-NEED-LIST CHAR-POS TRUNC
			       &AUX NCHUNKS CHUNKS CHUNK-DELIMS FILLS CHAMB TEMS RETS
			       RCHUNKS TEM LEN COMPLETED-P CHAR-CHUNK CHAR-OFFSET MAGIC-POS
			       TAIL)
  (SETQ CHUNKS (MAKE-ARRAY NIL 'ART-Q 20. NIL '(0))
	CHUNK-DELIMS (MAKE-ARRAY NIL 'ART-32B 20. NIL '(0)))
  (SETQ LEN (STRING-LENGTH STRING))
  (DO ((I 0 (1+ I))
       (J 0))
      ((> I LEN))
    (COND ((COND ((= I LEN)
		  (SETQ TEM -1))	;Last character delimits a chunk
		 (T
		  (MEMQ (SETQ TEM (AREF STRING I)) DELIMS)))
	   (AND CHAR-POS (> CHAR-POS J)	;Keep track of relative position
		(SETQ CHAR-CHUNK (ARRAY-LEADER CHUNKS 0)
		      CHAR-OFFSET (- CHAR-POS J)))
	   (ARRAY-PUSH-EXTEND CHUNKS (NSUBSTRING STRING J I))
	   (ARRAY-PUSH-EXTEND CHUNK-DELIMS TEM)
	   (SETQ J I))))
  (SETQ NCHUNKS (ARRAY-ACTIVE-LENGTH CHUNKS)
	FILLS (MAKE-ARRAY NIL 'ART-Q NCHUNKS)
	TEMS (MAKE-ARRAY NIL 'ART-Q NCHUNKS)
	RCHUNKS (MAKE-ARRAY NIL 'ART-Q NCHUNKS)
	CHAMB (MAKE-ARRAY NIL 'ART-1B NCHUNKS))
  (AND (ARRAYP ALIST)
       (MULTIPLE-VALUE (ALIST TAIL)
	 (COMPLETE-STRING-BOUNDS ALIST NCHUNKS CHUNKS CHUNK-DELIMS)))
  (DO ((L ALIST (CDR L))
       (ALL-AMBIG))
      ((EQ L TAIL))
    (DO NIL ((LISTP L)) (SETQ L (CAR L)))	;Indirect through multiple alists
    (COND ((NULL (COMPLETE-CHUNK-COMPARE (CAAR L) NCHUNKS CHUNKS CHUNK-DELIMS TEMS
					 (AND (NULL RETS) RCHUNKS)))
	   (OR RETS (SETQ CHUNKS RCHUNKS))	;First winner determines case of result
	   (PUSH (CAR L) RETS)		;add to list of partial matches
	   (SETQ ALL-AMBIG DONT-NEED-LIST)
	   (DO ((I 0 (1+ I))
		(FILL))
	       (( I NCHUNKS))
	     (SETQ TEM (AREF TEMS I)
		   FILL (AREF FILLS I))
	     (COND ((NULL FILL)		;First one to complete a chunk
		    (ASET TEM FILLS I)	;save for later ones
		    (AND (PLUSP (STRING-LENGTH TEM))
			 (SETQ ALL-AMBIG NIL)))	;This chunk not ambiguous yet
		   (T
		    (SETQ LEN (STRING-LENGTH FILL))
		    (DO ((J 0 (1+ J))
			 (LEN1 (STRING-LENGTH TEM)))
			(( J LEN)
			 (OR (ZEROP LEN)
			     (AND (= I (1- NCHUNKS))
				  (= LEN 1)
				  (MEMQ (AREF FILL 0) DELIMS))
			     (SETQ ALL-AMBIG NIL)))
		      (COND ((OR ( J LEN1)
				 (NOT (CHAR-EQUAL (AREF FILL J) (AREF TEM J))))
			     ;;Not the same completion, shorten final version
			     (ASET (NSUBSTRING FILL 0 J) FILLS I)
			     (ASET 1 CHAMB I)	;Remember this was ambiguous
			     (OR (ZEROP J) (SETQ ALL-AMBIG NIL))
			     (RETURN NIL)))))))
	   ;;If not going to complete and dont need actual list, finish up now.
	   (AND ALL-AMBIG (NULL (AREF FILLS (1- NCHUNKS))) (RETURN NIL)))))
  (COND ((AND TRUNC (SETQ TEMS (AREF FILLS (1- NCHUNKS))))
	 (SETQ LEN (STRING-LENGTH TEMS))
	 (AND (ZEROP (AREF CHAMB (1- NCHUNKS)))	;If last chunk wasnt ambigous,
	      (SETQ TRUNC 'NOSPACE))	;shouldnt have delimiter there
	 (DO I 0 (1+ I) ( I LEN)
	     (COND ((MEMQ (AREF TEMS I) DELIMS)
		    (ASET (NSUBSTRING TEMS 0 (1+ I)) FILLS (1- NCHUNKS))
		    (SETQ TRUNC 'NOSPACE)	;Already gave a delimiter
		    (RETURN NIL))))))
  (SETQ TEMS "")
  (DO I 0 (1+ I) ( I NCHUNKS)
      (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK)	;In case inside chunk not completed,
	   (SETQ CHAR-POS (+ (STRING-LENGTH TEMS) CHAR-OFFSET)))	;relocate
      (SETQ TEMS (STRING-APPEND TEMS (AREF CHUNKS I)))
      (COND ((AND (SETQ TEM (AREF FILLS I)) (> (STRING-LENGTH TEM) 0))
	     (SETQ TEMS (STRING-APPEND TEMS TEM)
		   COMPLETED-P T)
	     (AND CHAR-POS CHAR-CHUNK (= I CHAR-CHUNK)	;If inside completed chunk,
		  (SETQ CHAR-POS (STRING-LENGTH TEMS)))))	;move to end of it
      (OR MAGIC-POS (ZEROP (AREF CHAMB I))	;Remember end of leftmost ambigous chunk
	  (SETQ MAGIC-POS (STRING-LENGTH TEMS))))
  (AND COMPLETED-P (EQ TRUNC 'NOSPACE)
       (SETQ COMPLETED-P 'NOSPACE))
  (MVRETURN TEMS (NREVERSE RETS) COMPLETED-P CHAR-POS MAGIC-POS))

;;;Compare a STR with the given chunks and return NIL if it is a possible completion,
;;;else LESS or GREATER according as it is less or greater than the CHUNKS.
;;;T is returned for the indeterminate case, for the sake of the binary search in the
;;;array case.  The actual completer only checks NULL.
(DEFUN COMPLETE-CHUNK-COMPARE (STR NCHUNKS CHUNKS CHUNK-DELIMS &OPTIONAL TEMS RCHUNKS
				   &AUX LEN2)
  (SETQ LEN2 (STRING-LENGTH STR))
  (DO ((I 0 (1+ I))
       (J 0)
       (K)
       (LEN1)
       (CHUNK)
       (DELIM)
       (FLAG))
      (( I NCHUNKS) NIL)		;Aligns with each chunk, a winner
    (SETQ CHUNK (AREF CHUNKS I)
	  LEN1 (STRING-LENGTH CHUNK))
    (SETQ K (DO ((J1 0 (1+ J1))
		 (K1 J (1+ K1))
		 (CH1)
		 (CH2))
		(( J1 LEN1) K1)
	      (AND ( K1 LEN2) (RETURN (OR FLAG 'LESS)))
	      (SETQ CH1 (LDB %%CH-CHAR (AREF CHUNK J1))
		    CH2 (LDB %%CH-CHAR (AREF STR K1)))
	      (AND ( CH1 #/a) ( CH1 #/z) (SETQ CH1 (LOGXOR CH1 40)))
	      (AND ( CH2 #/a) ( CH2 #/z) (SETQ CH2 (LOGXOR CH2 40)))
	      (COND ((= CH1 CH2))
		    (FLAG (RETURN T))
		    ((< CH1 CH2) (RETURN 'GREATER))
		    (T (RETURN 'LESS)))))
    (OR (NUMBERP K) (RETURN K))
    (AND RCHUNKS (ASET (NSUBSTRING STR J K) RCHUNKS I))
    (COND ((MINUSP (SETQ DELIM (AREF CHUNK-DELIMS I)))
	   (SETQ J NIL))		;For the last chunk, use rest of string
	  ((SETQ J (%STRING-SEARCH-CHAR DELIM STR K LEN2))
	   (OR FLAG (= J K) (SETQ FLAG T)))
	  ((OR FLAG ( (1+ I) NCHUNKS)) (RETURN T))	;If more could follow or ambig
	  ((= K LEN2) (RETURN 'LESS))
	  (T (RETURN 'GREATER)))	;Delim not found, if at end of STR, it's less
    (AND TEMS (ASET (NSUBSTRING STR K J) TEMS I))))

;;;Given an ART-Q-LIST array and the chunks to match, compute the subset of that array
;;;that could possibly be a completion of the string, and return an NTHCDR of the G-L-P
;;;and the appropriate tail to stop with.
(DEFUN COMPLETE-STRING-BOUNDS (ALIST NCHUNKS CHUNKS CHUNK-DELIMS &AUX LO HI HIHI)
  (SETQ LO 0 HI 0
	HIHI (ARRAY-ACTIVE-LENGTH ALIST))
  (DO ((HILO HIHI)
       (IDX)
       (VAL T))
      (NIL)
    (AND (ZEROP (SETQ IDX (// (- HILO LO) 2)))	;binary search
	 (RETURN NIL))
    (SETQ IDX (+ LO IDX))
    (SETQ VAL (COMPLETE-CHUNK-COMPARE (CAR (AREF ALIST IDX))
				      NCHUNKS CHUNKS CHUNK-DELIMS))
    (COND ((EQ VAL 'LESS)
	   (SETQ LO IDX)
	   (SETQ HI IDX))
	  (T
	   (SETQ HILO IDX)
	   (COND ((NEQ VAL 'GREATER)
		  (SETQ HI IDX))
		 (T (SETQ HIHI IDX))))))
  (DO ((IDX)
       (VAL))
      (NIL)
    (AND (ZEROP (SETQ IDX (// (- HIHI HI) 2)))
	 (RETURN NIL))
    (SETQ IDX (+ HI IDX))
    (SETQ VAL (COMPLETE-CHUNK-COMPARE (CAR (AREF ALIST IDX))
				      NCHUNKS CHUNKS CHUNK-DELIMS))
    (COND ((NEQ VAL 'GREATER)
	   (SETQ HI IDX))
	  (T (SETQ HIHI IDX))))
  (SETQ ALIST (G-L-P ALIST))
  (MVRETURN (NTHCDR LO ALIST) (NTHCDR (1+ HI) ALIST)))

;;; Sort an art-q array, such as can be passed to the completing reader
;;; The second (1) element of the array leader is non-NIL if sorting is
;;; required.
(DEFUN SORT-COMPLETION-AARRAY (AARRAY)
  (COND ((NOT (ARRAY-LEADER AARRAY 1))	;If not sorted right now
	 (SORT AARRAY (FUNCTION (LAMBDA (X Y)
				  (STRING-LESSP (CAR X) (CAR Y)))))
	 (STORE-ARRAY-LEADER T AARRAY 1))))

;; Merge a sorted array ADDITIONAL-AARRAY of additional pairs into AARRAY.
;; Assuming that AARRAY was also sorted, the result is sorted.
(DEFUN MERGE-COMPLETION-AARRAY (AARRAY ADDITIONAL-AARRAY &AUX NEW-AARRAY TEM TEM1)
  ;; Make a new AARRAY big enough to hold both.
  (SETQ NEW-AARRAY (MAKE-ARRAY NIL ART-Q-LIST
			       (+ (ARRAY-ACTIVE-LENGTH AARRAY)
				  (ARRAY-ACTIVE-LENGTH ADDITIONAL-AARRAY))
			       NIL 2))
  ;; Mark it empty.
  (STORE-ARRAY-LEADER 0 NEW-AARRAY 0)
  ;; Now merge the two inputs into it.
  (DO ((OLD 0) (ADDED 0)
       (OLD-MAX (ARRAY-ACTIVE-LENGTH AARRAY))
       (ADDED-MAX (ARRAY-ACTIVE-LENGTH ADDITIONAL-AARRAY)))
      ;; Done when both inputs are empty.
      ((AND (= OLD OLD-MAX) (= ADDED ADDED-MAX)))
    ;; Find which input aarray's next element is least.  Remove it
    (COND ((OR (= ADDED ADDED-MAX)
	       (AND (NOT (= OLD OLD-MAX))
		    (STRING-LESSP (CAR (AREF AARRAY OLD))
				  (CAR (AREF ADDITIONAL-AARRAY ADDED)))))
	   (SETQ TEM (AREF AARRAY OLD))
	   (SETQ OLD (1+ OLD)))
	  (T (SETQ TEM (AREF ADDITIONAL-AARRAY ADDED))
	     (SETQ ADDED (1+ ADDED))))
    ;; and insert it into the new aarray.  But flush duplicate strings.
    (IF (AND (NOT (ZEROP (ARRAY-ACTIVE-LENGTH NEW-AARRAY)))
	     (STRING-EQUAL (CAR TEM)
			   (CAR (SETQ TEM1 (AREF NEW-AARRAY
						 (1- (ARRAY-ACTIVE-LENGTH NEW-AARRAY)))))))
	(LET ((LIST (SI:ELIMINATE-DUPLICATES (NCONC (IF (LISTP (CDR TEM)) (CDR TEM)
							(NCONS (CDR TEM)))
						    (IF (LISTP (CDR TEM1)) (CDR TEM1)
							(NCONS (CDR TEM1)))))))
	  (SETF (CDR TEM1) (IF (CDR LIST) LIST (CAR LIST))))
	(ARRAY-PUSH-EXTEND NEW-AARRAY TEM)))
  (STORE-ARRAY-LEADER T NEW-AARRAY 1)
  (STRUCTURE-FORWARD AARRAY NEW-AARRAY))

;;;Is this string in the completion list?
(DEFUN STRING-IN-AARRAY-P (STRING AARRAY)
  (SETQ STRING (STRING STRING))
  (DO ((LO 0)
       (HI (ARRAY-ACTIVE-LENGTH AARRAY))
       (IDX)
       (INC))
      (NIL)
    (AND (ZEROP (SETQ INC (// (- HI LO) 2)))
	 (RETURN NIL))
    (SETQ IDX (+ LO INC))
    (SELECTQ (STRING-COMPARE STRING (CAR (AREF AARRAY IDX)))
      (:EQUAL
       (RETURN T))
      (:GREATER
       (SETQ LO IDX))
      (OTHERWISE
       (SETQ HI IDX)))))

(DEFUN STRING-COMPARE (STR1 STR2)
  (DO ((I 0 (1+ I))
       (LEN1 (STRING-LENGTH STR1))
       (LEN2 (STRING-LENGTH STR2))
       (CH1)
       (CH2))
      (NIL)
    (AND ( I LEN1)
	 (RETURN (OR (IF ( I LEN2) ':EQUAL ':LESS))))
    (AND ( I LEN2)
	 (RETURN ':GREATER))
    (SETQ CH1 (AREF STR1 I)
	  CH2 (AREF STR2 I))
    (AND (CHAR-LESSP CH1 CH2)
	 (RETURN ':LESS))
    (AND (CHAR-LESSP CH2 CH1)
	 (RETURN ':GREATER))))

;;; Variables.

;;; Given a variable and a stream, prints the variable's name and value to that stream.
(DEFUN PRINT-VARIABLE (VAR &OPTIONAL (STREAM STANDARD-OUTPUT))
  (LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI"))
	(BASE 10.)
	(*NOPOINT NIL)
	(VAL (SYMEVAL VAR))
	(TYPE (GET VAR 'VARIABLE-TYPE)))
    (FORMAT STREAM "~25,4,2A " (STRING-APPEND (GET VAR 'VARIABLE-NAME) ":"))
    (SELECTQ TYPE
      ((:BOOLEAN :KEYWORD :STRING :FIXNUM-OR-NIL :FIXNUM :ANYTHING)
       (PRIN1 VAL STREAM))
      (:CHAR (TYO VAL STREAM))
      (:CHAR-LIST
       (TYO #/" STREAM)
       (DO L VAL (CDR L) (NULL L) (TYO (CAR L) STREAM))
       (TYO #/" STREAM)))
    (TERPRI STREAM)))

;;; Given a variable and a stream, print the short documentation on that
;;; variable, with some leading spaces and a trailing newline.
(DEFUN PRINT-VARIABLE-DOC (VAR &OPTIONAL (STREAM STANDARD-OUTPUT))
  (LET ((DOC (GET VAR 'VARIABLE-DOCUMENTATION)))
    (LET ((FIRST-CR (STRING-SEARCH-CHAR #\CR DOC)))
      (FORMAT STREAM "    ~A~&" (IF FIRST-CR
				    (NSUBSTRING DOC 0 FIRST-CR)
				    DOC)))))

(DEFCOM COM-LIST-VARIABLES "List all ZWEI variables and their values.
With an argument, print out documentation as well." ()
  (FORMAT T "~%ZWEI variables:~2%")
  (SETQ *VARIABLE-ALIST* (SORTCAR *VARIABLE-ALIST* #'STRING-LESSP))
  (DO L *VARIABLE-ALIST* (CDR L) (NULL L)
      (PRINT-VARIABLE (CDAR L))
      (AND *NUMERIC-ARG-P*
	   (PRINT-VARIABLE-DOC (CDAR L))))
  (FORMAT T "~%Done.~%")
  DIS-NONE)

(DEFCOM COM-VARIABLE-APROPOS "List all variables whose names contain a given substring.
With an argument, print documentation as well." ()
  (MULTIPLE-VALUE-BIND (FUNCTION STR)
      (GET-EXTENDED-SEARCH-STRINGS "Variable Apropos (substring):")
    (FORMAT T "~%ZWEI variables containing /"~A/":~2%" STR)
    (DO L *VARIABLE-ALIST* (CDR L) (NULL L)
	(COND ((FUNCALL FUNCTION STR (CAAR L))
	       (PRINT-VARIABLE (CDAR L))
	       (AND *NUMERIC-ARG-P*
		    (PRINT-VARIABLE-DOC (CDAR L))))))
    (FORMAT T "~%Done.~%"))
  DIS-NONE)

(DEFCOM COM-VARIABLE-DOCUMENT "Reads the name of a variable (using completion),
and print documentation on it." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
	     "Variable name:" *VARIABLE-ALIST* NIL NIL
	     "You are typing the name of a variable to document.")))
    (COND ((EQUAL X "") (BARF))
	  (T (PRINT-VARIABLE (CDR X))
	     (FORMAT T "~A~&"
		     (GET (CDR X) 'VARIABLE-DOCUMENTATION)))))
  DIS-NONE)

(DEFCOM COM-VARIABLE-SET "Set a variable, checking type.
Read the name of a variable (with completion), display current value
and documentation, and read a new variable.  Some checking is done
that the variable is the right type." ()
  (LET ((X (COMPLETING-READ-FROM-MINI-BUFFER
	    "Variable name:" *VARIABLE-ALIST* NIL NIL
	    "You are typing the name of a variable to be documented."
	    #'(LAMBDA (X)
		   (PRINT-VARIABLE (CDR X))
		   (FORMAT T "~A~&" (GET (CDR X) 'VARIABLE-DOCUMENTATION))))))
     (AND (EQUAL X "") (BARF))
     (PRINT-VARIABLE (CDR X))
     (FORMAT T "~A~&" (GET (CDR X) 'VARIABLE-DOCUMENTATION))
     (TEMP-KILL-RING (VARIABLE-STRING (CDR X))
       (LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI"))
	     (TYPE (GET (CDR X) 'VARIABLE-TYPE)))
	 (SET (CDR X)
	      (SELECTQ TYPE
		(:CHAR
		 (LET ((V (TYPEIN-LINE-READLINE "New value (one character)")))
		   (OR (= (STRING-LENGTH V) 1) (BARF "~A is not one character." V))
		   (LDB %%CH-CHAR (AREF V 0))))
		(:CHAR-LIST
		 (LET ((V (TYPEIN-LINE-READLINE "New value (a string)")))
		   (DO ((I 0 (1+ I))
			(RET)
			(LIM (STRING-LENGTH V)))
		       (( I LIM) (NREVERSE RET))
		     (PUSH (LDB %%CH-CHAR (AREF V I)) RET))))
		(:STRING
		 (TYPEIN-LINE-READLINE "New value (a string)"))
		(:FIXNUM
		 (LET ((V (TYPEIN-LINE-READ "New value (a fixnum)")))
		   (OR (FIXP V) (BARF "~S is not a fixnum." V))
		   V))
		(:FIXNUM-OR-NIL
		 (LET ((V (TYPEIN-LINE-READ "New value (NIL or a fixnum)")))
		   (OR (FIXP V) (NULL V) (BARF "~S is neither a fixnum not NIL." V))
		   V))
		(:SMALL-FRACTION
		 (LET ((V (TYPEIN-LINE-READ "New value (a flonum between 0.0 and 1.0")))
		   (OR (FLOATP V) (BARF "~S is not a floating-point number." V))
		   (OR (AND ( V 0.0s0) ( V 1.0s0))
		       (BARF "~S is not between 0.0 and 1.0" V))
		   (SMALL-FLOAT V)))
		(:BOOLEAN
		 (LET ((V (TYPEIN-LINE-READ "New value (T or NIL)")))
		   (OR (EQ T V) (BARF "~S is neither T nor NIL." V))
		   V))
		(:KEYWORD
		 (LET ((V (TYPEIN-LINE-READ "New value (a symbol)")))
		   (OR (SYMBOLP V) (BARF "~S is not a symbol." V))
		   V))
		(:ANYTHING
		 (TYPEIN-LINE-READ "New value")))))))
  DIS-NONE)

(DEFUN VARIABLE-STRING (VAR)
  (LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI"))
	(BASE 10.)
	(*NOPOINT NIL)
	(VAL (SYMEVAL VAR))
	(TYPE (GET VAR 'VARIABLE-TYPE)))
    (SELECTQ TYPE
      ((:BOOLEAN :KEYWORD :STRING :FIXNUM-OR-NIL :FIXNUM :ANYTHING)
       (FORMAT NIL "~S" VAL))
      (:CHAR (FORMAT NIL "~C" VAL))
      (:CHAR-LIST
       (DO ((VAL VAL (CDR VAL))
	    (STRING (MAKE-ARRAY NIL 'ART-STRING 10. NIL '(0))))
	   ((NULL VAL) STRING)
	 (ARRAY-PUSH-EXTEND STRING (CAR VAL)))))))
