;;; -*-Mode:LISP; Package:SYSTEM-INTERNALS -*-

;;; Functions to hang things off the <esc> keys.
;;; See the documentaion for the function KBD-ESC-INSTALL-FUNCTION for
;;; instruction on how to use it to install things on the <esc> keys.
;;; Note that the two <esc> keys ˆ and  are helpful in defining or
;;; undefining keys in realtime.  

;;; Note that the the items hung on <esc> keys are run in their own process
;;; to protect the kbd process.

;;; The end of this file contains various key definitions.


;;; Various things hang on this variable.  Its value is an alist of 
;;; (character form documentation).  Form is either evaled of funcalled.
;;; If it is a list that is a lambda expression that takes arguments or
;;; a symbol it is funcalled with the <esc> arg as its arguments.  Other-
;;; wise it is just evaled.
;;; Various properties hold information pertaining to the window used
;;; by some of the <esc> function.
;(DEFVAR KBD-ESC-REPOSITORY NIL)

;;; The KBD-ESC-UTILITY-WINDOW property hold the window.
(PUTPROP 'KBD-ESC-REPOSITORY (<- POP-UP-TEXT-WINDOW-CLASS ':NEW)
	 'KBD-ESC-UTILITY-WINDOW)

;;; Properties holding information pertaining to window size and position.
(LET ((WID)(HGT)(XPOS)(YPOS)(W)(SCR)(HGT2)(YPOS2))
  (SETQ W (GET 'KBD-ESC-REPOSITORY 'KBD-ESC-UTILITY-WINDOW)
	SCR (<- W ':SCREEN)
	STR (<- W ':STREAM)
	WID (// (* 3 (- (SCREEN-X2 SCR) (SCREEN-X1 SCR))) 4)
	HGT (// (- (SCREEN-Y2 SCR) (SCREEN-Y1 SCR)) 4)
	HGT2 (// (- (SCREEN-Y2 SCR) (SCREEN-Y1 SCR)) 2)
	XPOS (// (- (SCREEN-X2 SCR) (SCREEN-X1 SCR)) 2)
	YPOS  (// (- (SCREEN-Y2 SCR) (SCREEN-Y1 SCR)) 3)
	YPOS2 (- (SCREEN-Y2 SCR) YPOS))
  (PUTPROP 'KBD-ESC-REPOSITORY (LIST WID HGT) 'SMALL-SIZE)
  (PUTPROP 'KBD-ESC-REPOSITORY (LIST WID HGT2) 'MEDIUM-SIZE)
  (PUTPROP 'KBD-ESC-REPOSITORY (LIST XPOS YPOS) 'UPPER-PORTION-CENTERED-POSITION)
  (PUTPROP 'KBD-ESC-REPOSITORY (LIST XPOS YPOS2) 'LOWER-PORTION-CENTERED-POSITION))

(DEFUN KBD-ESC-PREPARE-WINDOW (LBL &REST OPT &AUX W STR)
  "Prepare the kbde-esc window for use.
Gets the window, sets it size, label and location as requested
and pops it up."
  (OR OPT (SETQ OPT '(FULL-SCREEN)))
  (SETQ W (GET 'KBD-ESC-REPOSITORY 'KBD-ESC-UTILITY-WINDOW))
  (AND (<- W ':STATUS)(<- W ':DEACTIVATE))
  (<- W ':LABEL<- LBL)
  (COND
    ((MEMQ 'FULL-SCREEN OPT)
     (<- W ':FULL-SCREEN))
    (T
      (COND ((MEMQ 'SMALL-SIZE OPT)
	     (LEXPR-FUNCALL W ':SIZE<- (GET 'KBD-ESC-REPOSITORY 'SMALL-SIZE)))
	    ((MEMQ 'MEDIUM-SIZE OPT)
	     (LEXPR-FUNCALL W ':SIZE<- (GET 'KBD-ESC-REPOSITORY 'MEDIUM-SIZE))))
      (COND ((MEMQ 'UPPER-PORTION-CENTERED-POSITION OPT)
	     (LEXPR-FUNCALL W ':MOVE-NEAR
			    (GET 'KBD-ESC-REPOSITORY 'UPPER-PORTION-CENTERED-POSITION)))
	    ((MEMQ 'LOWER-PORTION-CENTERED-POSITION OPT)
	     (LEXPR-FUNCALL W ':MOVE-NEAR
			    (GET 'KBD-ESC-REPOSITORY 'LOWER-PORTION-CENTERED-POSITION))))))
  (SETQ STR (<- W ':STREAM))
  (<- W ':POP-UP)
  (PROG () (RETURN W STR)))

(DEFUN KBD-ESC-INSTALL-FUNCTION (FCTN CHAR-VALUE &OPTIONAL DOC)
  "This is used to install an item on an <esc> key.
The second arg is the key in question or a list of such keys for
multiple installations.  How the first item is treated depends on the options:
    If it is a list and not a lambda expression of at least one
        arg it is evaled when selected.
    If it a lambda escpresion that takes at least arg it is funcalled
        with the <esc> arg as argument.
    If it is a symbol it is funcalled with the <esc> arg.
If no documentation is supplied then an attempt is made to find some using the
FUNCTION-DOCUMENTATION function.  If all attempts fail to find some documentation
then the item itself is used."

  (OR (LISTP CHAR-VALUE) (SETQ CHAR-VALUE (LIST CHAR-VALUE)))
  ;; We eval the documentation when asked to print it so quote correctly.
  (COND ((LISTP DOC)
	 (SETQ DOC `',DOC))
	((NULL DOC)
	 (SETQ DOC (IF (LISTP FCTN)
		       ; If we are given a list of one item then get doc from it.
		       (IF (= (LENGTH FCTN) 1)
			   `(FUNCTION-DOCUMENTATION ',(CAR FCTN))
			   (FORMAT NIL "~S" FCTN))
		       `(FUNCTION-DOCUMENTATION ',FCTN)))))
  (DOLIST (CHAR CHAR-VALUE)
    (KBD-ESC-REMOVE-FUNCTION CHAR)
    (PUSH (LIST (CHAR-UPCASE CHAR) FCTN DOC) KBD-ESC-REPOSITORY)))

(DEFUN KBD-ESC-REMOVE-FUNCTION (CHAR)
  "Given a character removes its associated form and doc from the <esc> keys."
  (SETQ KBD-ESC-REPOSITORY (DELQ (ASSQ CHAR KBD-ESC-REPOSITORY) KBD-ESC-REPOSITORY)))
	    

;;; What follows are various functions that are hung on the <esc> keys.

(DEFUN KBD-ESC-FINGER (ARG)
  "Finger the local machines.
 No arg => Who's on AI
 0 => Finger a user
 1 => Who's on Lisp Machines
 2 => Who's on MC
 3 => Who's on AI and MC"
  (OR ARG (SETQ ARG -1))			;Distinguish ESC F from ESC 0 F
  (LET ((WINDOW)(STREAM))
    (MULTIPLE-VALUE (WINDOW STREAM)
      (KBD-ESC-PREPARE-WINDOW (COND ((= ARG 0) "Finger")
				    ((= ARG 1) "Who's on Lisp Machines")
				    ((= ARG 2) "Who's on MC")
				    ((= ARG 3) "Who's on AI and MC")
				    (T "Who's on AI"))
			      'FULL-SCREEN))
    (SELECTQ ARG
      (0 (FORMAT STREAM "~&Finger:~%")
	 (FUNCALL #'CHAOS:FINGER (READLINE STREAM) STREAM))
      (1 (CHAOS:FINGER-ALL-LMS STREAM))
      (2 (CHAOS:FINGER "//L@MC" STREAM))
      (3 (CHAOS:FINGER "@AI" STREAM)
	 (TERPRI STREAM)
	 (CHAOS:FINGER "@MC" STREAM))
      (:OTHERWISE (CHAOS:FINGER "@AI" STREAM)))
    (FORMAT STREAM "~&~%Type a space to flush: ")
    (TYI STREAM)
    (<- WINDOW ':POP-DOWN)))

(DEFUN KBD-ESC-DOCUMENT-ALL-KEYS (IGNORE &AUX STREAM WINDOW)
  "Document all the Escape keys."
  (MULTIPLE-VALUE (WINDOW STREAM)
    (KBD-ESC-PREPARE-WINDOW "Documenting all the <esc> keys, <esc>? documents single keys."
			    'FULL-SCREEN))
  (FORMAT STREAM "Documentation of ESC keys:~%")
  (DOLIST (ITEM (REVERSE KBD-ESC-REPOSITORY))
    (KBD-ESC-PRINT-DOCUMENTATION STREAM ITEM))
  (FORMAT STREAM "~2%Type a space to flush:")
  (TYI STREAM)
  (<- WINDOW ':POP-DOWN))

;;; Used to rebind the kbd-tyi-hook so we quit on Z.
;;; Throw to first tag when all done, throw to second when want to retry.
(DEFMACRO KBD-ESC-TYI-HOOK-BIND (QUIT-TAG RETRY-TAG &REST FORMS)
  `(LET ((KBD-TYI-HOOK
	   #'(LAMBDA (C) (IF (= (CHAR-UPCASE C) #/Z) (*THROW ,QUIT-TAG NIL) C))))
     (*CATCH ,QUIT-TAG
	     (DO ()
		 (NIL)
	       (*CATCH ,RETRY-TAG
		       (PROGN ,@FORMS))))))

(DEFUN KBD-ESC-DOCUMENT-A-KEY (IGNORE &AUX W STR C)
  "Document an <esc> key."
  (MULTIPLE-VALUE (W STR)
    (KBD-ESC-PREPARE-WINDOW "Document an <esc> key, Z quits, <esc><help> documents all keys."
			    'SMALL-SIZE 
			    'UPPER-PORTION-CENTERED-POSITION))
  (KBD-ESC-TYI-HOOK-BIND
    'ALL-DONE-SINGLE-KEY-DOCUMENTATION
    'TRY-AGAIN
    (FORMAT STR "~%What is key? ")
    (SETQ C (CHAR-UPCASE (TYI STR)))
    (KBD-ESC-PRINT-DOCUMENTATION STR C))
  (<- W ':POP-DOWN))

(DEFUN KBD-ESC-PRINT-DOCUMENTATION (STREAM KEY &AUX SAVE-KEY (INDENT 10.))
  "Given a key this function finds its documentation and outputs it to STREAM.
The key may also be an alist elemnt."
  (IF (NUMBERP KEY)
      (SETQ SAVE-KEY KEY KEY (ASSQ KEY KBD-ESC-REPOSITORY)))
  (COND ((NULL KEY)
	 (FORMAT STREAM "~%The key ~C is not defined" SAVE-KEY))
	(T						;Print for all other cases.
	  (LET ((DOC (OR (EVAL (CADDR KEY))
			 (FORMAT NIL "~S" (CADR KEY)))))
	    (IF (NOT (LISTP DOC))
		(DO ((D DOC (SUBSTRING D (1+ (STRING-SEARCH-CHAR #\CR D))))
		     (L NIL (CONS (SUBSTRING D 0 (STRING-SEARCH-CHAR #\CR D)) L)))
		    ((NOT (STRING-SEARCH-CHAR #\CR D)) (SETQ DOC (NREVERSE (CONS D L))))))
	    (FORMAT STREAM "~%~C~VT" (CAR KEY) INDENT)
	    (FORMAT STREAM "~A" (IF (LISTP DOC) (CAR DOC) DOC))
	    (IF (LISTP DOC)
		(DO ((D (CDR DOC) (CDR D)))
		    ((NULL D))
		  (FORMAT STREAM "~%~VT~A" INDENT (CAR D))))
	    (TERPRI STREAM)))))

(DEFUN FIND-A-WINDOW-OF-CLASS (CLASS &OPTIONAL (NTH 1))
"Given a class and an optional n, find nth window of that class on ACTIVE-WINDOWS-LIST."
  (AND (SYMBOLP CLASS) (SETQ CLASS (SYMEVAL CLASS)))
  (DO ((W ACTIVE-WINDOWS (CDR W))
       (CNT 1))
      ((NULL W) NIL)
    (IF (OR (EQ (CLASS (CAR W)) CLASS)
	    (AND (EQ (CLASS (CAR W)) SI:WINDOW-SINGLE-FRAME-CLASS)
		 (EQ (CLASS (<- (CAR W) ':PANE)) CLASS)))
	(IF (= CNT NTH)
	    (RETURN (CAR W))
	    (SETQ CNT (1+ CNT))))))

(DEFUN KBD-ESC-CREATE-WINDOW-WITH-FRAME (CLASS &AUX P W)
  (SETQ P (<- WINDOW-SINGLE-FRAME-CLASS ':NEW))
  (SETQ W (<- CLASS ':NEW))
  (<- P ':FULL-SCREEN)
  (<- P ':PANE<- W)
  P)
  

(DEFUN KBD-ESC-FIND-OR-MAKE-SUPDUP-OR-TELNET (ARG &AUX W (NTH 1))
  "Network:  Get or make a SUPDUP or TELNET
 0 or no arg => find a SUPDUP, make one if none around
 1 => find a TELNET, make one if none around
 2 => make a new SUPDUP
 3 => make a new TELNET
 precomma arg is nth one to find."
  (IF (LISTP ARG)
      (SETQ NTH (CAR ARG) ARG (CADR ARG)))
  (SELECTQ ARG
    ((0 NIL)
     (IF (NOT (SETQ W (FIND-A-WINDOW-OF-CLASS SUPDUP:SUPDUP-CLASS NTH)))
	 (SETQ W (KBD-ESC-CREATE-WINDOW-WITH-FRAME SUPDUP:SUPDUP-CLASS)))
     (WINDOW-SELECT W))
    (1
      (IF (NOT (SETQ W (FIND-A-WINDOW-OF-CLASS SUPDUP:TELNET-CLASS NTH)))
	  (SETQ W (KBD-ESC-CREATE-WINDOW-WITH-FRAME SUPDUP:TELNET-CLASS)))
      (WINDOW-SELECT W))
    (2 (WINDOW-SELECT (KBD-ESC-CREATE-WINDOW-WITH-FRAME SUPDUP:SUPDUP-CLASS)))
    (3 (WINDOW-SELECT (KBD-ESC-CREATE-WINDOW-WITH-FRAME SUPDUP:TELNET-CLASS)))))

(DEFUN KBD-ESC-ASK-AND-INSTALL-FUNCTION-REALTIME (IGNORE &AUX W STR FCTN CHAR DOC)
  "Install a function on an <esc> key.
 Z at anytime aborts the operation."
  (MULTIPLE-VALUE (W STR)
    (KBD-ESC-PREPARE-WINDOW "Installing a new <esc> function.    (Z aborts)"
			    'SMALL-SIZE
			    'UPPER-PORTION-CENTERED-POSITION))
  (KBD-ESC-TYI-HOOK-BIND
    'GIVE-IT-UP-BOYS
    'TRY-AGAIN
    (FORMAT STR "~%What is form to eval and store on character? ")
    (SETQ
      FCTN
      (LET ((ITEM (EVAL (READ STR))))
	(IF (AND (SYMBOLP ITEM) (NOT (FBOUNDP ITEM)))	;If we can, check if its defined.
	    (PROGN
	      (FORMAT STR "~%I can't find a definition for ~S" ITEM)
	      (*THROW 'TRY-AGAIN NIL)))
	(FUNCALL STR ':CLEAR-INPUT)
	(FORMAT STR "~%Object is ~S, confirm: " ITEM)
	(IF (Y-OR-N-P NIL STR)
	    ITEM
	    (*THROW 'TRY-AGAIN NIL))))
    (FORMAT STR "~%What is character? ")
    ;; If it is alread defined make sure he knows it.
    (SETQ CHAR (DO ((TRY (CHAR-UPCASE (TYI STR))(CHAR-UPCASE (TYI STR))))
		   ((NOT (ASSQ TRY KBD-ESC-REPOSITORY)) TRY)
		 (TERPRI STR)
		 (FORMAT
		   STR
		   "~%This character is already defined as: ~% ~S, go on? "
		   (CADR (ASSQ TRY KBD-ESC-REPOSITORY)))		      
		 (IF (Y-OR-N-P NIL STR)
		     (RETURN TRY))
		 (FORMAT STR "~%Another character please: ")))
    (FORMAT STR "~%What is form to eval for documentation? ")
    (SETQ DOC (READ STR))
    (FUNCALL STR ':CLEAR-INPUT)
    (KBD-ESC-INSTALL-FUNCTION FCTN CHAR DOC)
    (*THROW 'GIVE-IT-UP-BOYS NIL))
  (<- W ':POP-DOWN))

(DEFUN KBD-ESC-DEINSTALL-FUNCTION-REALTIME (IGNORE &AUX W STR CHAR)
  "Remove the function bound to a key, Z at anytime aborts the operation."
  (MULTIPLE-VALUE (W STR)
    (KBD-ESC-PREPARE-WINDOW "Deleting an <esc> character definition.    (Z aborts)"
			    'SMALL-SIZE
			    'UPPER-PORTION-CENTERED-POSITION))
  (KBD-ESC-TYI-HOOK-BIND
    'ALL-DONE
    'LOSE-LOSE
    (FORMAT STR "~% What is Character? ")
    (SETQ CHAR (CHAR-UPCASE (TYI STR)))
    (IF (NULL (ASSQ CHAR KBD-ESC-REPOSITORY))
	(PROGN
	  (FORMAT STR "~%There is nothing defined for this character.~%")
	  (*THROW 'LOSE-LOSE NIL)))
    (FORMAT STR "~% Clobber ~%~S~% on character: ~:C, (confirm)? "
	    (CADR (ASSQ CHAR KBD-ESC-REPOSITORY))
	    CHAR)
    (IF (Y-OR-N-P NIL STR)
	(PROGN
	  (KBD-ESC-REMOVE-FUNCTION CHAR)
	  (*THROW 'ALL-DONE NIL))
	(*THROW 'LOSE-LOSE NIL)))
  (<- W ':POP-DOWN))

(DEFUN KBD-ESC-WINDOW-OPERATION (ARG &AUX W STR KILLEE)
  "Perform a window operation depending on ARG:
 -1 => unbury a window, ie, select last buried window.
 0 => bury SELECTED-WINDOW
 1 => Kill SELECTED-WINDOW, with confirmation
 2 => invoke the window selection menu."
  (OR ARG (SETQ ARG 0))
  (COND ((= ARG -1)				;Unbury a window.
	 (WINDOW-SELECT (CAR (LAST ACTIVE-WINDOWS))))
	((AND SELECTED-WINDOW (EQ ARG 0))	; Just bury the window.
	 (<- SELECTED-WINDOW ':BURY))
	((AND (SETQ KILLEE SELECTED-WINDOW) (EQ ARG 1))	; Kill with confirmation.
	 (MULTIPLE-VALUE (W STR)
	   (KBD-ESC-PREPARE-WINDOW "Killing a window"
				   'SMALL-SIZE
				   'UPPER-PORTION-CENTERED-POSITION))
	 (FORMAT STR "~2% Killing window:~2%~A,~2%confirm: " KILLEE)
	 (FUNCALL STR ':CLEAR-INPUT)
	 (AND (PROG1
		(Y-OR-N-P NIL STR)
		(<- W ':POP-DOWN))
	      (<- KILLEE ':KILL)))
	((= ARG 2)				; Get the window selection menu.
	 (LEXPR-FUNCALL #'MOUSE-WARP
			(GET 'KBD-ESC-REPOSITORY 'UPPER-PORTION-CENTERED-POSITION))
	 (REDEFINE-ACTIVE-WINDOWS-MENU))))

(DEFUN KBD-ESC-FIND-OR-CREATE-PEEK-WINDOW (ARG &AUX PW PANE)
"Find or create a peek window and select it.
Arg is nth window to choose. (Default is first.)"
  (OR ARG (SETQ ARG 1))
  (COND ((SETQ PW (FIND-A-WINDOW-OF-CLASS PEEK-WINDOW-CLASS ARG)))
	(T
	  (SETQ PANE (<- PEEK-WINDOW-CLASS ':NEW))
	  (SETQ PW (<- WINDOW-SINGLE-FRAME-CLASS ':NEW))
	  (LEXPR-FUNCALL PW ':SIZE<- (GET 'KBD-ESC-REPOSITORY 'MEDIUM-SIZE))
	  (LEXPR-FUNCALL PW ':MOVE-NEAR
			 (GET 'KBD-ESC-REPOSITORY 'LOWER-PORTION-CENTERED-POSITION))
	  (<- PW ':PANE<- PANE)))
  (WINDOW-SELECT PW))

(DEFUN KBD-ESC-SELECT-A-WINDOW (ARG &AUX (OLD-W SELECTED-WINDOW))
"Select a window:
 -1 or - => least recent selected window
 1 (default) => last selected window
 n => nth most recent selected window.
The nth most recent selected window is interpreted to be the n+1th window on the
active windows list."
  (OR ARG (SETQ ARG 1))
  (COND ((< ARG 0)
	 (SETQ ARG (1- (LENGTH ACTIVE-WINDOWS))))
	((= ARG 0))
	(T
	  (IF (NOT SELECTED-WINDOW)
	      (SETQ ARG (1- ARG)))
	  (IF (< ARG (LENGTH ACTIVE-WINDOWS))
	      NIL
	      (SETQ ARG (1- (LENGTH ACTIVE-WINDOWS))))))
  ;; Try to select a window until we get one that is not what we had
  ;; or we get to the end of the list.
  (WINDOW-SELECT (NTH ARG ACTIVE-WINDOWS))
  (DO ((ARG ARG (1+ ARG)))
      ((OR (AND SELECTED-WINDOW (NEQ SELECTED-WINDOW OLD-W))
	   (= ARG (1- (LENGTH ACTIVE-WINDOWS)))))
    (WINDOW-SELECT (NTH ARG ACTIVE-WINDOWS))))

(DEFUN KBD-ESC-DESCRIBE-OR-DOCUMENT (ARG &AUX W STR THING LBL)
  "Describe an object or document a function accorging to args.
 0 => describe an object (default)
 1 => document a function."
  (IF (OR (NOT ARG) (= ARG 0))
      (SETQ LBL "Describe an object  (Z terminates)")
      (SETQ LBL "Document a function  (Z terminates)"))
  (MULTIPLE-VALUE (W STR)
    (KBD-ESC-PREPARE-WINDOW LBL
			    'MEDIUM-SIZE 'UPPER-PORTION-CENTERED-POSITION))  
  (KBD-ESC-TYI-HOOK-BIND
    'ALL-DONE
    'LOSE-LOSE
    (SELECTQ ARG
      ((NIL 0)
       (FORMAT STR "~2& What is form to eval and describe? ")
       (SETQ THING (EVAL (READ STR)))
       (FUNCALL STR ':FRESH-LINE)
       (LET ((STANDARD-OUTPUT STR))
	 (DESCRIBE THING)))
      (1
	(FORMAT STR "~2& What is form to eval and document? ")
	(SETQ THING (EVAL (READ STR)))
	(FORMAT STR "~&~A" (FUNCTION-DOCUMENTATION THING))))
    (FUNCALL STR ':CLEAR-INPUT)
    (*THROW 'LOSE-LOSE NIL))
  (<- W ':POP-DOWN))



;;; The binding of various keys follows.

;; <esc>  - Unbind the function from a key.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-DEINSTALL-FUNCTION-REALTIME #/)

;; <esc> ˆ - Bind a function to a key.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-ASK-AND-INSTALL-FUNCTION-REALTIME #/ˆ)

;; <esc> BREAK or B - Send a supervisory signal to the selected window.
(KBD-ESC-INSTALL-FUNCTION '(AND SELECTED-WINDOW
				(<- SELECTED-WINDOW ':SUPERVISORY-SIGNAL ':BREAK))
			  '(#\BREAK #/B)   ;also B for compatibility.
			  "Send a SUPERVISORY-SIGNAL BREAK to selected-window.")

;; <esc> C - Complement black on white mode.
(KBD-ESC-INSTALL-FUNCTION '(TV-COMPLEMENT-BOW-MODE)
			  #/C
			  "Complement TV's black on white mode.")

;; <esc> D - Describe or document a thing.
;(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-DESCRIBE-OR-DOCUMENT #/D)

;; <esc> D - Buzz the 9th floor door.
(KBD-ESC-INSTALL-FUNCTION '(PROGN (CHAOS:BUZZ-DOOR)(%BEEP 34000 4000000))
			  #/D
			  "Buzz the 9th floor door.")

;; <esc> E - Call the elevator.
(KBD-ESC-INSTALL-FUNCTION '(PROGN (CHAOS:CALL-ELEVATOR) (%BEEP 1000 140000))
			  #/E
			  "Call the elevator.")

;; <esc> F - Finger local machines.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-FINGER #/F)

;; <esc> L - select the first lisp listener that comes to mind.
(KBD-ESC-INSTALL-FUNCTION '(LAMBDA (ARGS &AUX W)
			     (OR ARGS (SETQ ARGS 1))
			     (AND
			       (SETQ W (FIND-A-WINDOW-OF-CLASS SI:LISP-LISTENER-CLASS ARGS))
			       (WINDOW-SELECT W)))
			  #/L
			  (LIST "Find and select a LISP-LISTENER"
				"Arg is nth window to select"))

;; <esc> M - Control more processing.
(KBD-ESC-INSTALL-FUNCTION
  '(LAMBDA (ARG)
      (SETQ TV-MORE-PROCESSING-GLOBAL-ENABLE
	    (COND ((NOT ARG) (NOT TV-MORE-PROCESSING-GLOBAL-ENABLE))
		  ((= ARG 0) NIL)			;ESC 0 M MORE PROC OFF
		  (T T))))
  #/M
  "More processing, no arg => complement, 0 => off, 1 => on.")

;; <esc> N - Network, get a supdup or telnet.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-FIND-OR-MAKE-SUPDUP-OR-TELNET #/N)

;; <esc> Q - Hardcopy the screen.
(KBD-ESC-INSTALL-FUNCTION '(SCREEN-XGP-HARDCOPY-BACKGROUND)
			  #/Q
			  "Hardcopy of the screen.")

;; <esc> P - Select or create a Peek window.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-FIND-OR-CREATE-PEEK-WINDOW #/P)

;; <esc> S - Select windows
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-SELECT-A-WINDOW #/S)

;; <esc> W - Bury or kill the selected window.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-WINDOW-OPERATION #/W)

;; <esc> Z - Find an editor window.
(KBD-ESC-INSTALL-FUNCTION '(LAMBDA (ARGS &AUX W)
			     (OR ARGS (SETQ ARGS 1))
			     (IF 
			       (SETQ W (FIND-A-WINDOW-OF-CLASS ZWEI:ZWEI-WINDOW-CLASS ARGS))
			       (WINDOW-SELECT W)
			       (ED)))
			  #/Z
			  (LIST "Find and select a Zwei window."
				"Arg is nth window to choose."))

;; <esc> ? - document a key.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-DOCUMENT-A-KEY #/?)

;; <esc> <help> - document the escape keys.
(KBD-ESC-INSTALL-FUNCTION 'KBD-ESC-DOCUMENT-ALL-KEYS #\HELP)
