;; -*- Mode: LISP; Package: SUPDUP -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;; Lisp Machine User SUPDUP windows.   RMS 12/24/78

(DECLARE
 (SPECIAL SUPDUP-KEYS		      ; Used by :KBD-TYI method.
	  SUPDUP-HELP-MESSAGE	      ; List of FORMAR-STRING strings.
	  SUPDUP-TYO-DISPATCH	      ; Dispatch for SUPDUP-TYPE-OUT-LOOP
	  ;; The following are variables of SUPDUP instances.
	  SUPDUP-LINE-HEIGHT	      ; Constants of the PC PPR, kept in specials
	  SUPDUP-CHAR-WIDTH	      ;   for speed.
	  SUPDUP-OUTPUT-LOCK	      ; The value cell of this symbol is the lock.
	  SUPDUP-COMMUNICATE	      ; Set to error string when lossage is detected
				      ; in the type out process.
	  CHAOS:FINGER-ALIST	      ; Associates host number with physical locn. In CHAOS.
	  ))

(DECLARE (SPECIAL CURRENT-PROCESS))

(ENDF HEAD)

(DEFMACRO SUPDUP-OLOCK BODY
   `(UNWIND-PROTECT
     (PROGN
      (PROCESS-LOCK (VALUE-CELL-LOCATION 'SUPDUP-OUTPUT-LOCK))
      . ,BODY)
     (COND ((EQ CURRENT-PROCESS SUPDUP-OUTPUT-LOCK)
	    (PROCESS-UNLOCK (VALUE-CELL-LOCATION 'SUPDUP-OUTPUT-LOCK))))))


;; Creation of supdup windows.

(DEFCLASS SUPDUP-CLASS WINDOW-WITH-PC-PPR-CLASS
        ( SUPDUP-ESCAPE		      ; Character to escape to supdup, in ITS char set.
	  SUPDUP-ESCAPE-1	      ; Same character in Lisp machine char set.
	  SUPDUP-LINE-HEIGHT	      ; Constants of the PC PPR, kept in specials
	  SUPDUP-CHAR-WIDTH	      ;   for speed.
	  SUPDUP-CONNECTION	      ; The net connection being used by SUPDUP, or NIL.
	  SUPDUP-STREAM		      ; Stream for the net connection.
	  SUPDUP-TERMINAL-STREAM      ; Stream for the terminal.
	  SUPDUP-COMMUNICATE	      ; InterProcess Communication.
				      ; The type out process sets this to an error string
				      ; to tell the type in process that it got an error.
	  SUPDUP-TYPE-OUT-PROCESS      ; The process which performs type-out.
	  SUPDUP-TYPE-IN-PROCESS      ; The process which reads type in.
	  SUPDUP-OUTPUT-LOCK	      ; The value cell of this symbol is the lock.
	  OUTPUT-BUFFER		      ; String holding chars to be output.
	  LABEL-STATUS		      ; String to display in label line
				      ; containing host name or "not connected".
	  ))

(DEFMETHOD (SUPDUP-CLASS :BORN) ()
    (OR NAME (SETQ NAME (STRING-APPEND "Supdup-" (GENSYM))))
    (OR SI:SCREEN (SETQ SI:SCREEN SI:TV-DEFAULT-SCREEN))
    ;; Make a PC PPR, without more processing or any of that, since ITS does it all.
    (OR SI:PC-PPR
      (SETQ SI:PC-PPR
	  (TV-DEFINE-PC-PPR NAME NIL ':MORE-P NIL ':SCREEN SI:SCREEN
			    ;':ACTIVATE-P NIL  ;option no longer exists, apparently.
			    )))
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':BORN)
    (SETQ SUPDUP-TYPE-OUT-PROCESS (PROCESS-CREATE (STRING-APPEND NAME "-Type-Out")))
    (SETQ SUPDUP-TYPE-IN-PROCESS (PROCESS-CREATE (STRING-APPEND NAME "-Type-In")))
    (SETQ SI:PROCESS (<- PARALLELISM-CLASS ':NEW ':NAME (STRING-APPEND NAME "-Parallelism")))
    (<- SI:PROCESS ':PROCESSES<-
	(LIST SUPDUP-TYPE-OUT-PROCESS SUPDUP-TYPE-IN-PROCESS))
    (<- SI:PROCESS ':ENABLED-PROCESSES<-
	(LIST SUPDUP-TYPE-OUT-PROCESS SUPDUP-TYPE-IN-PROCESS))
    (<- SI:PROCESS ':INPUT-PROCESS<- SUPDUP-TYPE-IN-PROCESS)
    (PROCESS-PRESET SUPDUP-TYPE-OUT-PROCESS (FUNCTION SUPDUP-TYPE-OUT-TOP-LEVEL) SELF)
    (PROCESS-PRESET SUPDUP-TYPE-IN-PROCESS (FUNCTION SUPDUP-TYPE-IN-TOP-LEVEL) SELF)
    (SETQ LABEL-STATUS "not connected")
    (SETQ SUPDUP-TERMINAL-STREAM (TV-MAKE-STREAM SI:PC-PPR))
    ;; Extract PC PPR parameters only once, for speed.
    (SETQ SUPDUP-LINE-HEIGHT (PC-PPR-LINE-HEIGHT SI:PC-PPR)
	  SUPDUP-CHAR-WIDTH (PC-PPR-CHAR-WIDTH SI:PC-PPR))
    (SETQ SUPDUP-ESCAPE 4102)
    (SETQ SUPDUP-ESCAPE-1 201)
    (SETQ OUTPUT-BUFFER (MAKE-ARRAY NIL 'ART-STRING 200 NIL '(0))))

(DEFMETHOD (SUPDUP-CLASS :KILL) ()
  (FUNCALL SELF ':DISCONNECT))

(DEFMETHOD (SUPDUP-CLASS :EDGES<-) (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
    (AND SUPDUP-CONNECTION
         (OR ( (- NEW-RIGHT NEW-LEFT) (- SI:RIGHT SI:LEFT))
             ( (- NEW-BOTTOM NEW-TOP) (- SI:BOTTOM SI:TOP)))
         (FERROR NIL "Size of ~S is fixed while connection is open" SELF))
    (<-AS WINDOW-WITH-PC-PPR-CLASS ':EDGES<-
          NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))

(DECLARE (SPECIAL MAIN-SUPDUP-WINDOW))

(DEFUN SUPDUP (&OPTIONAL PATH (WINDOW 3) &AUX MSW)
    (COND ((BOUNDP 'MAIN-SUPDUP-WINDOW))
	  (T (SETQ MSW (<- SUPDUP-CLASS ':NEW ':NAME "Supdup"))
	     (<- (<- SI:WINDOW-SINGLE-FRAME-CLASS ':NEW)
		 ':PANE<-  MSW)
	     (SETQ MAIN-SUPDUP-WINDOW MSW)))
    (COND ((OR PATH (NOT (<- MAIN-SUPDUP-WINDOW ':CONNECTED-P)))
	   (<- MAIN-SUPDUP-WINDOW ':CONNECT PATH WINDOW)))
    (LET ((SW SELECTED-WINDOW))
      (WINDOW-SELECT MAIN-SUPDUP-WINDOW)
      (<- MAIN-SUPDUP-WINDOW ':UPDATE-LABEL-IF-NECESSARY)
      ;; Don't return until that window de-selects itself.  The reason for this
      ;; is only that output-hold is suppressed for the main lisp listener window,
      ;; hence without this kludge it would type T over the supdup window.
      (PROCESS-WAIT "SUPDUP" #'(LAMBDA (OUR-STATUS) (CAR OUR-STATUS))
		    (LOCATE-IN-CLOSURE SW ':STATUS))))

;; Making and breaking connections.

;IF ARG A STRING, STUFF PRECEEDING  SPECIFIES HOST TO USE AS ARPANET BRIDGE.  IF NO 
; BRIDGE SPEC'D AND HOST IS NOT KNOWN, WILL TRY TO USE AI TO BRIDGE.
;IF ARG A SYMBOL, IT WILL ALWAYS BE USED DIRECTLY AS CHAOSNET CONTACT NAME.
;RETURNS T ON SUCCESSFUL CONNECTION, RETURNS ERROR STRING ON FAILURE.
(DEFMETHOD (SUPDUP-CLASS :CONNECT) (&OPTIONAL PATH (NET-WINDOW 3) &AUX BRIDGE TEM CONNECTION)
    (<- SELF ':DISCONNECT)
    (SETQ PATH (OR PATH "AI"))
    (AND (SYMBOLP PATH) (SETQ PATH (GET-PNAME PATH)))
    (COND ((STRINGP PATH) ;as opposed to a number
	   (COND ((SETQ TEM (STRING-SEARCH-CHAR #/ PATH))
		  (SETQ BRIDGE (NSUBSTRING PATH 0 TEM))
		  (SETQ PATH (NSUBSTRING PATH (1+ TEM)))))
	   (COND ((AND (NULL BRIDGE)
		       (NOT (ASSOC PATH CHAOS:HOST-ALIST)))
		  (SETQ BRIDGE "AI")))))
    (SETQ CONNECTION
	  (COND (BRIDGE (CHAOS:CONNECT BRIDGE
				       (STRING-APPEND "ARPA " PATH " 137") NET-WINDOW))
		(T (CHAOS:CONNECT PATH "SUPDUP" NET-WINDOW))))
    (COND ((STRINGP CONNECTION) CONNECTION)
          (T (<- SUPDUP-TYPE-IN-PROCESS ':FLUSH)
	     (<- SUPDUP-TYPE-OUT-PROCESS ':FLUSH)
	     (SETQ SUPDUP-CONNECTION CONNECTION)
	     (SETQ SUPDUP-STREAM (CHAOS:STREAM SUPDUP-CONNECTION))
             (LET ((SUPDUP-%TOCID (IF (AND BRIDGE (STRING-EQUAL PATH "SAIL")) T
                                      SUPDUP-%TOCID)))
               (SUPDUP-SEND-TTY-VARIABLES SI:PC-PPR SUPDUP-STREAM))
	     (SUPDUP-SEND-FINGER-STRING SUPDUP-STREAM)
	     (SETQ LABEL-STATUS
		   (STRING-APPEND
                       (CHAOS:HOST-DATA (CHAOS:FOREIGN-ADDRESS SUPDUP-CONNECTION))
		       (IF BRIDGE "  " "")
		       (IF BRIDGE PATH "")))
             (IF SI:STATUS (<- SELF ':UPDATE-LABEL) (<- SELF ':CLOBBER-LABEL))
	     (<- SELF ':GOBBLE-GREETING)
	     (<- SUPDUP-TYPE-IN-PROCESS ':RESET)
	     (<- SUPDUP-TYPE-OUT-PROCESS ':RESET))))

(DEFMETHOD (SUPDUP-CLASS :CONNECTED-P) ()
    (AND SUPDUP-CONNECTION (EQ (CHAOS:STATE SUPDUP-CONNECTION) 'CHAOS:OPEN-STATE)))

(DEFMETHOD (SUPDUP-CLASS :DISCONNECT) ()
    (<- SUPDUP-TYPE-IN-PROCESS ':FLUSH)
    (<- SUPDUP-TYPE-OUT-PROCESS ':FLUSH)
    (AND SUPDUP-CONNECTION (CHAOS:CLOSE SUPDUP-CONNECTION))
    (SETQ SUPDUP-CONNECTION NIL)
    (SETQ SUPDUP-COMMUNICATE NIL)
    (SETQ LABEL-STATUS "not connected")
    (COND (SI:STATUS
	   (<- SELF ':UPDATE-LABEL)
	   (<- SELF ':CLEAN)))
    (<- SUPDUP-TYPE-IN-PROCESS ':RESET)
    (<- SUPDUP-TYPE-OUT-PROCESS ':RESET))


;; Display updating of supdup windows (aside from type out).
(DEFMETHOD (SUPDUP-CLASS :PRINT-LABEL) (PCPPR)
    (TV-STRING-OUT PCPPR NAME)
    (TV-STRING-OUT PCPPR " -- ")
    (TV-STRING-OUT PCPPR LABEL-STATUS))

;; If asked for the label return what is printed.
(DEFMETHOD (SUPDUP-CLASS :LABEL) ()
    (STRING-APPEND NAME " -- " LABEL-STATUS))

;; Input side.  These are the functions executed in the supdup input process,
;; which transfers data to the network connection.
(DEFUN SUPDUP-TYPE-IN-TOP-LEVEL (SUPDUP-WINDOW)
    (DO ((STR)) (())
        (SETQ STR NIL)
        (<- SUPDUP-WINDOW ':UPDATE)
        (*CATCH 'SI:TOP-LEVEL
                (SETQ STR (<- SUPDUP-WINDOW ':INPUT-TOP-LEVEL)))
	(COND ((STRINGP STR)
               (<- SUPDUP-WINDOW ':DISCONNECT)
               (FORMAT (<- SUPDUP-WINDOW ':SUPDUP-TERMINAL-STREAM) "~%~A~%" STR))))) 

;If this returns a string, it will be printed as an error message.
(DEFMETHOD (SUPDUP-CLASS :INPUT-TOP-LEVEL) ()
  (CONDITION-BIND ((CHAOS:READ-ON-LOS-CONNECTION NET-ERROR)
		   (CHAOS:HOST-DOWN NET-ERROR))
    (COND (SUPDUP-CONNECTION
           (*CATCH 'SUPDUP-DONE
	     (PROG READ-INPUT (CH)
		(PUTPROP (LOCF (PROCESS-PLIST SUPDUP-TYPE-IN-PROCESS)) T ':KBD-SUPER-IMAGE-P)
		(DO () (NIL)
		    (<- SELF ':WAIT-FOR-KBD-INPUT)
		    (AND SUPDUP-COMMUNICATE
			 (RETURN-FROM READ-INPUT
                                      (PROG1 SUPDUP-COMMUNICATE (SETQ SUPDUP-COMMUNICATE NIL))))
		    (SELECTQ (CHAOS:STATE SUPDUP-CONNECTION)
			     (CHAOS:OPEN-STATE
			      (SETQ CH (<- SELF ':KBD-TYI))      ;Get an ITS character from the kbd.
			      (COND ((= (CHAR-UPCASE CH) SUPDUP-ESCAPE)
				     ;;Handle the escape character,
				     (SETQ CH (<- SELF ':HANDLE-ESCAPE)))
				    ;; otherwise just send through what he typed.
				    (T (<- SELF ':NET-OUTPUT CH))))
			     (CHAOS:HOST-DOWN-STATE
			      (RETURN-FROM READ-INPUT "Foreign Host died"))
			     (CHAOS:CLS-RECEIVED-STATE
			      (RETURN-FROM READ-INPUT "Closed by foreign host"))
			     (CHAOS:LOS-RECEIVED-STATE
			      (RETURN-FROM READ-INPUT "Connection closed due to random lossage:"))
			     (OTHERWISE
			      (RETURN-FROM READ-INPUT
					   (FORMAT NIL "Connection in unknown state:~S"
						   (CHAOS:STATE SUPDUP-CONNECTION)))))))))
	  (T (LET ((TERMINAL-IO SUPDUP-TERMINAL-STREAM))
		  (PUTPROP (LOCF (PROCESS-PLIST SUPDUP-TYPE-IN-PROCESS))
			   NIL ':KBD-SUPER-IMAGE-P)
		  (FORMAT T "~%Connect to host: ")
		  (<- SELF ':CONNECT (READLINE)))))))

(DEFMETHOD (SUPDUP-CLASS :WAIT-FOR-KBD-INPUT) ()
    (OR (KBD-CHAR-AVAILABLE)
	(FUNCALL SUPDUP-STREAM ':FORCE-OUTPUT))
    (PROCESS-WAIT "SUPDUP TYI"
		  (FUNCTION (LAMBDA (CONN-LOC COMM-LOC)
		      (OR (KBD-CHAR-AVAILABLE)
			  (NOT (EQ (CHAOS:STATE (CDR CONN-LOC))
				   'CHAOS:OPEN-STATE))
			  (CDR COMM-LOC))))
		  (LOCATE-IN-CLOSURE SELF 'SUPDUP-CONNECTION)
		  (LOCATE-IN-CLOSURE SELF 'SUPDUP-COMMUNICATE)))

(SETQ SUPDUP-KEYS (MAKE-ARRAY NIL 'ART-Q 20))
(FILLARRAY SUPDUP-KEYS '(0 4102 4103 32 0 37 4110 177 10 11 12 13 14 15 4102))

;; This is used like KBD-TYI, but returns a character in the ITS 12-bit
;; character set.  Althought this function hacks ESCAPE itself it does
;; NOT hack CALL; it just returns a 32.  So be careful.
;; The second value of this function is the ordinary Lisp machine form of the character.
;; However, right now <- won't let the second value out!
;; So you must do (FUNCALL (<- SELF ':HANDLER-FOR ':KBD-TYI)) to get it.
(DEFMETHOD (SUPDUP-CLASS :KBD-TYI) ()
    (PROG (CH)
      TOP (SETQ CH (KBD-TYI-1))
          (COND ((= CH 204)         ;Escape.
		 (KBD-ESC)
		 (GO TOP))
                ((BIT-TEST CH 2000) (GO TOP)))  ;IGNORE MOUSE CHARACTERS.
	  (RETURN 
	   (LET ((CHAR (LDB %%KBD-CHAR CH)))
	     (DPB (LDB %%KBD-CONTROL-META CH)
		  0702
		  (COND ((= CHAR 33) CHAR)    ;(Special case)
			((< CHAR 40) (LOGIOR CHAR 4000))
			((< CHAR 200) CHAR)
			(T (AR-1 SUPDUP-KEYS (- CHAR 200))))))
	   CH)))

;; This sends a character of the ITS 12-bit character set to the network,
;; using the ITS Intelligent Terminal Protocol to get the extra bits through.
(DEFMETHOD (SUPDUP-CLASS :NET-OUTPUT) (CH &AUX BITS)
    (SETQ BITS (LDB 0705 CH))
    (COND ((NOT (ZEROP BITS))
	   (SUPDUP-OLOCK
	    (FUNCALL SUPDUP-STREAM ':TYO 34)
	    (FUNCALL SUPDUP-STREAM ':TYO (LOGIOR 100 BITS))
	    (FUNCALL SUPDUP-STREAM ':TYO (LOGAND 177 CH))))
	  ((= CH 34)
	   (SUPDUP-OLOCK
	    (FUNCALL SUPDUP-STREAM ':TYO 34)
	    (FUNCALL SUPDUP-STREAM ':TYO CH)))
	  (T (FUNCALL SUPDUP-STREAM ':TYO CH))))

;; Handle a command to the SUPDUP program itself.
(DEFMETHOD (SUPDUP-CLASS :HANDLE-ESCAPE) (&AUX CH XPOS YPOS)
    (UNWIND-PROTECT
     (PROGN
      (MULTIPLE-VALUE (XPOS YPOS) (TV-READ-CURSORPOS SI:PC-PPR))
      (SUPDUP-PUT-DOWN-STRING SI:PC-PPR "CMND-->")
      (SETQ CH (CHAR-UPCASE (<- SELF ':KBD-TYI)))
      (SELECTQ CH
         ((32 #/P)
	  (SI:TOP-WINDOW)
          NIL)
	 ((#/B)
	  (PUTPROP (LOCF (PROCESS-PLIST SUPDUP-TYPE-IN-PROCESS)) NIL ':KBD-SUPER-IMAGE-P)
	  (BREAK SUPDUP-BREAK T)
	  (PUTPROP (LOCF (PROCESS-PLIST SUPDUP-TYPE-IN-PROCESS)) T ':KBD-SUPER-IMAGE-P)
          NIL)
         (#/C			      ;C = Change escape character.
          (SUPDUP-PUT-DOWN-STRING SI:PC-PPR "CHANGE ESCAPE CHARACTER TO -->")
          (MULTIPLE-VALUE (SUPDUP-ESCAPE SUPDUP-ESCAPE-1)
                          (FUNCALL (<- SELF ':HANDLER-FOR ':KBD-TYI) NIL))
          (SETQ SUPDUP-ESCAPE (CHAR-UPCASE SUPDUP-ESCAPE))
          NIL)
         (#/D                         ;D = Disconnect, ask for new host to connect to.
          (<- SELF ':DISCONNECT)
          (*THROW 'SUPDUP-DONE "Disconnected"))
	 (#/L			      ;L = Logout.
	  (SUPDUP-OLOCK
	   (FUNCALL SUPDUP-STREAM 'TYO 300)
	   (FUNCALL SUPDUP-STREAM 'TYO 301)
	   (FUNCALL SUPDUP-STREAM 'FORCE-OUTPUT))
	  (SUPDUP-QUIT "Logout"))
         (#/Q			      ;Q = Quit.
	  (SUPDUP-QUIT))
	 ((4110 #/?)		      ;<HELP> or ? = Help
	  (TV-HOME SI:PC-PPR)
          (TV-CLEAR-EOL SI:PC-PPR)
          (TV-STRING-OUT SI:PC-PPR
                         (FORMAT NIL "After typing the Escape character, which is ~:C,
you can type these commands:~%" SUPDUP-ESCAPE-1))
	  (MAPC (FUNCTION (LAMBDA (X) (TV-STRING-OUT SI:PC-PPR X) (TV-CRLF SI:PC-PPR)))
                SUPDUP-HELP-MESSAGE)
          (TV-STRING-OUT SI:PC-PPR
                         (FORMAT NIL "~4A -- Send ~:C through"
                                 (FORMAT NIL "~:C" SUPDUP-ESCAPE-1)
                                 SUPDUP-ESCAPE-1))
          (TV-CRLF SI:PC-PPR)
          NIL)
	 (177			      ;<RUBOUT> = Do nothing.
	  NIL)
	 (OTHERWISE
	  (COND ((= CH SUPDUP-ESCAPE)
		 (<- SELF ':NET-OUTPUT CH)
		 (FUNCALL SUPDUP-STREAM ':FORCE-OUTPUT))
		(T (TV-BEEP)))
          NIL)))
     (COND (STATUS
	    (SUPDUP-PUT-DOWN-STRING SI:PC-PPR "")      ;Clear the bottom line.
	    (TV-SET-CURSORPOS SI:PC-PPR XPOS YPOS))
	   (T					;Even if not exposed, reposition cursor.
	    (SETF (PC-PPR-CURRENT-X SI:PC-PPR)
		  (MIN (+ (PC-PPR-LEFT-MARGIN SI:PC-PPR) XPOS)
		       (PC-PPR-RIGHT-LIMIT SI:PC-PPR)))
	    (SETF (PC-PPR-CURRENT-Y SI:PC-PPR)
		  (MIN (+ (PC-PPR-TOP-MARGIN SI:PC-PPR) YPOS)
		       (PC-PPR-BOTTOM-LIMIT SI:PC-PPR)))))
))

(DEFUN SUPDUP-QUIT (&OPTIONAL (STRING "Quit"))
    (<- SELF ':DISCONNECT)
    (SI:TOP-WINDOW T)
    (*THROW 'SUPDUP-DONE STRING))

;; Support function to set the lowest line on the pc ppr to STRING.
(DEFUN SUPDUP-PUT-DOWN-STRING (PC-PPR STRING)
	  (TV-HOME-DOWN PC-PPR)
	  (TV-CLEAR-EOL PC-PPR)
	  (TV-STRING-OUT PC-PPR STRING))

;; The help message.

(SETQ SUPDUP-HELP-MESSAGE
 '(""
   "CALL -- Do a local CALL (return to top window)."
   "B    -- Enter a breakpoint."
   "C    -- Change the SUPDUP escape character."
   "D    -- Disconnect and connect to new host."
   "L    -- Log out of remote host, and break the connection."
   "P    -- Return to top window, but don't break connection."
   "Q    -- Disconnect and return to top window."
   "?    -- Type this cruft."))

;; Send the initial information describing the Lisp Machine as an
;; intelligent terminal.  The TTYOPT word contains the following:
;; %TOERS+%TOMVB+%TOSAI+%TOOVR+%TOMVU+%TOLWR+%TOFCI+%TOMOR+%TOLID,,%TPCBS+%TPORS
;; Note that %TOCID is off; someday this will be changed.
(DECLARE (SPECIAL SUPDUP-%TOCID))
(OR (BOUNDP 'SUPDUP-%TOCID)
    (SETQ SUPDUP-%TOCID NIL))

(DEFUN SUPDUP-SEND-TTY-VARIABLES (PC-PPR STREAM)
    (SUPDUP-18BIT-OUT STREAM -5)	      ;First word is -5,,0
    (SUPDUP-18BIT-OUT STREAM 0)
    (SUPDUP-18BIT-OUT STREAM 0)	      ;TCTYP word must be %TNSFW: 0,,7
    (SUPDUP-18BIT-OUT STREAM 7)
    (SUPDUP-18BIT-OUT STREAM (COND ((NOT SUPDUP-%TOCID) 55632) ;TTYOPT word explained above.
                                   (T 55633)))
    (SUPDUP-18BIT-OUT STREAM 50)
    (SUPDUP-18BIT-OUT STREAM 0)	      ;TCMXV
    (SUPDUP-18BIT-OUT STREAM (- (// (- (PC-PPR-BOTTOM-MARGIN PC-PPR)
				(PC-PPR-TOP-MARGIN PC-PPR))
			     (PC-PPR-LINE-HEIGHT PC-PPR))
			 1))
    (SUPDUP-18BIT-OUT STREAM 0)	      ;TCMXH
    (SUPDUP-18BIT-OUT STREAM (- (// (- (PC-PPR-RIGHT-MARGIN PC-PPR)
				(PC-PPR-LEFT-MARGIN PC-PPR))
			     (PC-PPR-CHAR-WIDTH PC-PPR))
			 1))
    (SUPDUP-18BIT-OUT STREAM 0)	      ;TTYROL
    (SUPDUP-18BIT-OUT STREAM 0)	      ;This is CHAOS, we don't "SCROLL" here!!!!!
    (FUNCALL STREAM 'FORCE-OUTPUT))

(DEFUN SUPDUP-18BIT-OUT (STREAM N)
    (FUNCALL STREAM 'TYO (LDB 1406 N))
    (FUNCALL STREAM 'TYO (LDB 0606 N))
    (FUNCALL STREAM 'TYO (LDB 0006 N)))

;; Send the string to TELSER saying where we are, so that NAME can find it inside
;; the TELSER and print it.  Boy, what a kludge.
(DEFUN SUPDUP-SEND-FINGER-STRING (STREAM &AUX ID)
    (SETQ ID (OR (CDR (ASSQ CHAOS:MY-ADDRESS CHAOS:FINGER-ALIST))
                 (CHAOS:HOST-DATA CHAOS:MY-ADDRESS)))
    (FUNCALL STREAM 'TYO 300)  ;SUPDUP escape string meaning that the FINGER
    (FUNCALL STREAM 'TYO 302)  ;  identification string follows.
    (DO ((I 0 (1+ I))
	 (LEN (STRING-LENGTH ID)))
	((>= I LEN))
      (FUNCALL STREAM 'TYO (AR-1 ID I)))
    (FUNCALL STREAM 'TYO 0)    ; End with a 0.
    (FUNCALL STREAM 'FORCE-OUTPUT))

;; Condition handler for typein side.
(DEFUN NET-ERROR (ARGS)
   (*THROW 'SUPDUP-DONE (LEXPR-FUNCALL (FUNCTION FORMAT) NIL ARGS)))

;; Output side.  These functions are executed in the supdup output process
;; which transfers things from the remote host to the local display.

;; Read in a character from SUPDUP-STREAM.  If end-of-file, suicide.
;; Used only in the type out process.
(DEFUN SUPDUP-NETI (&OPTIONAL (RETURN-EOF-P NIL))
    (LOCAL-DECLARE ((SPECIAL SUPDUP-STREAM))
       (COND ((FUNCALL SUPDUP-STREAM ':TYI))
	     ((NOT RETURN-EOF-P)
	      (SETQ SUPDUP-COMMUNICATE "Closed by foreign host.")
	      (PROCESS-WAIT "Connection closed" (FUNCTION FALSE))))))

;; Condition handler for type-out side.
(DEFUN NET-ERROR-TYPE-OUT-SIDE (ARGS)
   (SETQ SUPDUP-COMMUNICATE (LEXPR-FUNCALL (FUNCTION FORMAT) NIL ARGS))
   (PROCESS-WAIT "Connection closed" (FUNCTION FALSE)))

;; Read in a character from SUPDUP-STREAM.  If there is none currently, return NIL.
(DEFUN SUPDUP-NETI-NO-HANG ()
    (LOCAL-DECLARE ((SPECIAL SUPDUP-STREAM))
        (FUNCALL SUPDUP-STREAM ':TYI-NO-HANG)))

;; This is the top level function of the supdup output process.

(DEFUN SUPDUP-TYPE-OUT-TOP-LEVEL (SUPDUP-WINDOW)
    (*CATCH 'SI:TOP-LEVEL
            (<- SUPDUP-WINDOW ':OUTPUT-TOP-LEVEL))
    (PROCESS-WAIT "Error-return" (FUNCTION FALSE)))

(DEFMETHOD (SUPDUP-CLASS :OUTPUT-TOP-LEVEL) (&AUX (TERMINAL-IO SUPDUP-TERMINAL-STREAM))
    (PROCESS-WAIT "Never-open" (FUNCTION (LAMBDA (X) X)) SUPDUP-CONNECTION)
    (CONDITION-BIND ((CHAOS:READ-ON-LOS-CONNECTION NET-ERROR-TYPE-OUT-SIDE)
		     (CHAOS:HOST-DOWN NET-ERROR-TYPE-OUT-SIDE))
	;; Read in characters, and print them.
	;; This never returns, but upon EOF this process hangs until reset.
	(COMMENT
	 ;; This is basically what we do, but we read many characters at a time
	 ;; storing them in OUTPUT-BUFFER, and print them all at once, for efficiency.
	 (DO ((CH)) (NIL)
	     (SETQ CH (SUPDUP-NETI))
	     ... ;Process this one character.
	     ))
	;; The way we do it is, empty the buffer and then read characters until
	;; there are no more characters or the buffer is full, then print the buffer.
	;; CHAR-PRE-PROCESS is called on each character and can return an altered character
	;; or return T to say that it has processed the character.
	(DO ((UNTYI)
	     (CHAR-PRE-PROCESS (<- SELF ':HANDLER-FOR ':CHAR-PRE-PROCESS)))
	    (())
	   (STORE-ARRAY-LEADER 0 OUTPUT-BUFFER 0)
	   (DO ((CH (OR UNTYI (SUPDUP-NETI))
                    (SUPDUP-NETI-NO-HANG)))
	       ((NULL CH))
	       (COND (UNTYI (SETQ UNTYI NIL))
		     (T (SETQ CH (FUNCALL CHAR-PRE-PROCESS NIL CH))))
	       (COND ((EQ CH T))
		     (T
		      (COND ((NOT (ARRAY-PUSH OUTPUT-BUFFER CH))
			     (SETQ UNTYI CH)
                             (RETURN NIL))))))
;	   (TV-STRING-OUT SI:PC-PPR OUTPUT-BUFFER)  ;Only slightly faster and loses
						    ; if SUPDUP-TERMINAL-STREAM<- done.
	   (FUNCALL SUPDUP-TERMINAL-STREAM ':STRING-OUT OUTPUT-BUFFER))))

(DEFMETHOD (SUPDUP-CLASS :GOBBLE-GREETING) ()
    (TERPRI)		      ;Print out the greeting message ITS sends in ASCII.
    (DO ((CH (SUPDUP-NETI T) (SUPDUP-NETI T)))
	((OR (NULL CH) (= CH 210)))		;The end is marked with a %TDNOP, NIL is eof
	(AND (< CH 40) (SETQ CH (+ 200 CH)))
	(OR (= CH 212)	      ;Don't type linefeeds (ITS sends CRLFs).
	    (TYO CH))))

(DEFMETHOD (SUPDUP-CLASS :CHAR-PRE-PROCESS) (CH)
    (COND ((< CH 200) CH)
	  (T
;	   (TV-STRING-OUT SI:PC-PPR OUTPUT-BUFFER)
	   (FUNCALL SUPDUP-TERMINAL-STREAM ':STRING-OUT OUTPUT-BUFFER)
	   (STORE-ARRAY-LEADER 0 OUTPUT-BUFFER 0)
	   (OR (>= CH 240)
	       (FUNCALL (AREF SUPDUP-TYO-DISPATCH (- CH 200)) SI:PC-PPR))
	   T)))

;; Dispatch table for the %TD codes.
(SETQ SUPDUP-TYO-DISPATCH (MAKE-ARRAY NIL 'ART-Q 40))
(FILLARRAY SUPDUP-TYO-DISPATCH
   '(SUPDUP-TDMOV SUPDUP-TDMV0 TV-CLEAR-EOF TV-CLEAR-EOL TV-CLEAR-CHAR SUPDUP-NOTHING
;;;  %TDMOV       %TDMV0       %TDEOF       %TDEOL       %TDDLF        %TDMTF

     SUPDUP-GT40  TV-CRLF SUPDUP-NOTHING SUPDUP-NOTHING SUPDUP-NOTHING SUPDUP-NOTHING
;;;  %TDMTN       %TDCRL  %TDNOP         %TDBS          %TDLF          %TDCR

     SUPDUP-TDORS SUPDUP-TDQOT TV-SPACE SUPDUP-TDMV0 SUPDUP-CLEAR    SUPDUP-BEEP
;;;  %TDORS       %TDQOT       %TDFS    %TDMV0       %TDCLR          %TDBEL

     SUPDUP-NOTHING SUPDUP-INSERT-LINE SUPDUP-DELETE-LINE
;;;  %TDINI	    %TDILP	       %TDDLP

     SUPDUP-INSERT-CHAR SUPDUP-DELETE-CHAR SUPDUP-NOTHING SUPDUP-NOTHING SUPDUP-NOTHING 
;;;  %TDICP	    	%TDDCP		   %TDBOW	  %TDRST	 %TDGRF

;;; PTV compatibility hacks (ARDS, etc.)
     SUPDUP-NOTHING     SUPDUP-NOTHING     SUPDUP-NOTHING SUPDUP-ARDS-SET
;;;  undefined          %TDBIT             %TDGXT         %TDLNG

     SUPDUP-ARDS-LONG   SUPDUP-ARDS-SHORT
;;;  %TDLV              %TDSV
     ))

;; Handle %TDMOV by ignoring two characters and then acting as if it were a %TDMV0.
(DEFUN SUPDUP-TDMOV (PC-PPR)
   (SUPDUP-NETI)
   (SUPDUP-NETI)
   (SUPDUP-TDMV0 PC-PPR))

;; Handle %TDMV0 or %TDMV1 by moving the cursor.  This is kludgey because
;; ITS sends out positions as VPOS followed by HPOS.
(DEFUN SUPDUP-TDMV0 (PC-PPR &AUX YPOS)
   (SETQ YPOS (* (SUPDUP-NETI) SUPDUP-LINE-HEIGHT))
   (TV-SET-CURSORPOS PC-PPR
		     (* (SUPDUP-NETI) SUPDUP-CHAR-WIDTH)
		     YPOS))

;; This "null function" is used for codes which we should ignore.
(DEFUN SUPDUP-NOTHING (IGNORE) NIL)

;; Handle %TDORS.  Just tell ITS where the cursor position is, using the
;; Intelligent Terminal Protocol's ^\ ^P command.
(DEFUN SUPDUP-TDORS (PC-PPR &AUX VPOS HPOS)
  (LOCAL-DECLARE ((SPECIAL SUPDUP-STREAM))
    (MULTIPLE-VALUE (HPOS VPOS)
	(TV-READ-CURSORPOS PC-PPR))
    (SUPDUP-OLOCK
     (FUNCALL SUPDUP-STREAM 'TYO 34) ;^\
     (FUNCALL SUPDUP-STREAM 'TYO 20) ;^P
     (FUNCALL SUPDUP-STREAM 'TYO (// VPOS SUPDUP-LINE-HEIGHT))
     (FUNCALL SUPDUP-STREAM 'TYO (// HPOS SUPDUP-CHAR-WIDTH))
     (FUNCALL SUPDUP-STREAM 'FORCE-OUTPUT))
    ))

;; %TDQOT means the next character should be quoted.
(DEFUN SUPDUP-TDQOT (PC-PPR)
    (TV-TYO PC-PPR (SUPDUP-NETI)))

;; %TDBEL means to ring the "bell".
;; To avoid gross obnoxosity, we merge multiple consecutive beeps into one
(LOCAL-DECLARE ((SPECIAL SUPDUP-STREAM))
(DEFUN SUPDUP-BEEP (IGNORE)
    (TV-BEEP)
    (DO ((CH (SUPDUP-NETI-NO-HANG) (SUPDUP-NETI-NO-HANG)))
        ((OR (NULL CH) ( CH 221))
         (AND CH (FUNCALL SUPDUP-STREAM ':UNTYI CH))
         NIL)))
)

(DECLARE (SPECIAL GT40-DISPLAY-LIST))	      ; Used by the GT40 simulator

;;; %TDCLR
(DEFUN SUPDUP-CLEAR (PC-PPR)
    (TV-CLEAR-PC-PPR PC-PPR)
    (FILLARRAY GT40-DISPLAY-LIST '(NIL)))	;kludges, kludges everywhere...
    
;; %TDILP means to insert lines, takes one arg from stream which is number of lines to insert
;; Lines are inserted at current VPOS.  The current line is affected.
(DEFUN SUPDUP-INSERT-LINE (PC-PPR)
  (TV-INSERT-LINE PC-PPR (SUPDUP-NETI)))

;; %TDDLP means to delete lines, takes one arg from stream which is the number of lines.
;; Affects the current line.
(DEFUN SUPDUP-DELETE-LINE (PC-PPR)
  (TV-DELETE-LINE PC-PPR (SUPDUP-NETI)))

;;; %TDICP insert character positions, takes an arg.
(DEFUN SUPDUP-INSERT-CHAR (PC-PPR)
  (TV-INSERT-CHAR PC-PPR (SUPDUP-NETI)))

;;; %TDDCP delete character positions, takes an arg.
(DEFUN SUPDUP-DELETE-CHAR (PC-PPR)
  (TV-DELETE-CHAR PC-PPR (SUPDUP-NETI)))

;;; GT40 Simulator (used with the DEC simulator on I.T.S. for running SUDS)

;;; This crock maintains a display list for writing, erasing, and moving display objects
;;; consisting of characters, vectors, and points.  This protocol is not documented
;;; anywhere except in the code for DECUUO.

(DECLARE (SPECIAL GT40-DISPATCH GT40-CURRENT-ITEM-NUMBER GT40-BLINKER))

;; %TDMTN is a crock for simulating GT-40's, used by DECUUO on ITS for Imlacs...

(DEFUN SUPDUP-GT40 (PC-PPR &AUX (BYTE (- (SUPDUP-NETI) 100)))
    (OR (< BYTE 0)
	(FUNCALL (AREF GT40-DISPATCH (LOGAND 17 BYTE)) PC-PPR)))

;; Dispatch table for the GT40 simulator.  These functions take one argument, the pc-ppr.
(SETQ GT40-DISPATCH (MAKE-ARRAY NIL 'ART-Q 17))
(FILLARRAY GT40-DISPATCH
	   '(GT40-INSERT-OR-DELETE
	     GT40-INSERT
	     GT40-DELETE
;	     GT40-RESET
;	     GT40-TURN-ON
;	     GT40-TURN-OFF
;	     GT40-COPY
;	     GT40-MOVE
;	     GT40-MODE
;	     GT40-APPEND
;	     GT40-SUBROUTINIZE
;	     GT40-UNSUBROUTINIZE
	     SUPDUP-NOTHING))			;most are not used by DECUUO

;;; Macros used below to pack characters into words, decode vector formats, etc.

;;; Make a 16-bit "word" from 3 chars in 6-4-6 format
(DEFMACRO GT40-WORD ()
   '(DPB (SUPDUP-NETI) 0006
	 (DPB (SUPDUP-NETI) 0604
	      (DPB (SUPDUP-NETI) 1206 0))))

;;; Get a word count
(DEFMACRO GT40-COUNT () '(LSH (- (GT40-WORD) 5) -1))

;;; Used in constructing display objects - used only in GT40-INSERT.
(DEFMACRO APUSH (DOB ITEM) `(ARRAY-PUSH-EXTEND ,DOB ,ITEM 500.))

;;; Compute the index of the last thing pushed
(DEFMACRO GT40-LAST-INDEX (DOB) `(1- (ARRAY-ACTIVE-LENGTH ,DOB)))

;;; Get the last item pushed onto a display object
(DEFMACRO GT40-LAST-ITEM (DOB) `(AREF ,DOB (GT40-LAST-INDEX ,DOB)))

;;; Short vector format
(DEFMACRO GT40-SHORT (DOB WORD)
   `(PROGN
      (APUSH ,DOB (* (LDB 0706 ,WORD) (IF (BIT-TEST 20000 ,WORD) -1 1)))
      (APUSH ,DOB (* (LDB 0006 ,WORD) (IF (BIT-TEST 100 ,WORD) -1 1)))
      (APUSH ,DOB (BIT-TEST 40000 ,WORD))))

;;; Long vector format
(DEFMACRO GT40-LONG (DOB WORD1 WORD2)
    `(LET ((WORD2 ,WORD2))
       (APUSH ,DOB (* (LOGAND 1777 ,WORD1) (IF (BIT-TEST 20000 ,WORD1) -1 1)))
       (APUSH ,DOB (* (LOGAND 1777 WORD2) (IF (BIT-TEST 20000 WORD2) -1 1)))
       (APUSH ,DOB (BIT-TEST 40000 ,WORD1))))

;;; Coordinate scaling macro
(DEFMACRO GT40-COORD (X) `(MAX 0 (// (* 7 ,X) 10.)))

;;; Save TTY parameters while drawing.  Note end of line hackery.
(DEFMACRO GT40-BIND-TTY-PARAMS (PC-PPR &REST FORMS)
  `(LET ((CUR-X (PC-PPR-CURRENT-X ,PC-PPR))
	 (CUR-Y (PC-PPR-CURRENT-Y ,PC-PPR))
	 (CUR-ALU (PC-PPR-CHAR-ALUF ,PC-PPR))
	 (CUR-EOL-FCN (PC-PPR-END-LINE-FCN ,PC-PPR)))
     (UNWIND-PROTECT
      (PROGN
       (SETF (PC-PPR-CHAR-ALUF ,PC-PPR) TV-ALU-XOR)
       (SETF (PC-PPR-END-LINE-FCN ,PC-PPR)
	     #'(LAMBDA (IGNORE) (*THROW 'EOL-CATCH NIL)))
       . ,FORMS)
      (SETF (PC-PPR-CHAR-ALUF ,PC-PPR) CUR-ALU)
      (SETF (PC-PPR-END-LINE-FCN ,PC-PPR) CUR-EOL-FCN)
      (TV-SET-CURSORPOS ,PC-PPR CUR-X CUR-Y))))


;;; Draw a string.  Note special end of line hackery.  XPOS and YPOS must be symbols.
(DEFMACRO GT40-DRAW-STRING (STRING XPOS YPOS PC-PPR)
    `(LET ((MAX-Y 750.))
      (TV-SET-CURSORPOS ,PC-PPR
			 (GT40-COORD ,XPOS)
			 (- MAX-Y (GT40-COORD ,YPOS) 11.))
      (*CATCH 'EOL-CATCH (TV-STRING-OUT ,PC-PPR ,STRING))))

;;; Draw a vector.  XPOS and YPOS must be symbols
(DEFMACRO GT40-DRAW-VECTOR (XPOS YPOS X Y FLAG PC-PPR)
    `(LET ((MAX-Y 750.) (OXPOS ,XPOS) (OYPOS ,YPOS))
	  (SETQ ,XPOS (+ ,XPOS ,X) ,YPOS (+ ,YPOS ,Y))
	  (IF ,FLAG
	       (TV-DRAW-LINE (GT40-COORD OXPOS)
			     (- MAX-Y (GT40-COORD OYPOS))
			     (GT40-COORD ,XPOS)
			     (- MAX-Y (GT40-COORD ,YPOS))
			     TV-ALU-XOR
			     (PC-PPR-SCREEN ,PC-PPR)))))

;;; Read a vector out of the display list and draw it
(DEFMACRO GT40-VECTOR (DOB XPOS YPOS PC-PPR)
    `(LET ((I (GT40-LAST-INDEX ,DOB)))
	  (GT40-DRAW-VECTOR
	   ,XPOS ,YPOS
	   (AREF ,DOB (- I 2)) (AREF ,DOB (- I 1))	;new x y
	   (AREF ,DOB I) ,PC-PPR)))			;visibility flag

;;; Display list format:  The display list is an ART-Q array of display objects, each of
;;; which is, in turn, an ART-Q array.  The format of display objects is a sequence of
;;; display items.  A display item is either a single string of characters or an in-line
;;; subsequence consisting of a symbol describing the item-type followed by 2 numbers (x,y)
;;; and a visibility flag.  Numbers and flags are repeated until a new symbol is encountered
;;; indicating a type change.

;;; Display list array.
(SETQ GT40-DISPLAY-LIST (MAKE-ARRAY NIL 'ART-Q-LIST 10.) GT40-BLINKER NIL)

;;; GT40 Command 0 - Insert or delete display items
(DEFUN GT40-INSERT-OR-DELETE (PC-PPR)
    (SELECTQ (LOGAND 3 (GT40-WORD))		;only 1 and 2 are recognized for now
             (1 (GT40-INSERT PC-PPR))	;insert a new display item
	     (2 (GT40-DELETE PC-PPR (1+ (GT40-COUNT))))))	;delete n items

;;; GT40 Command 1 - Insert a display item into the display list.
(DEFUN GT40-INSERT (PC-PPR &AUX (WORD-COUNT (GT40-COUNT)))
    (GT40-BIND-TTY-PARAMS PC-PPR
      (GT40-DELETE PC-PPR 1 NIL)		;Delete the item we are about to insert
      (DO ((I 0 (1+ I))			;Loop over words, contructing a display list
	   (WORD)(MODE -1)		;Mode is initially undefined.
	   (XPOS 0) (YPOS 0) (BLINK-THIS)
	   (DOB				;Display OBject
	    (OR (AREF GT40-DISPLAY-LIST GT40-CURRENT-ITEM-NUMBER)	;Already an array or
		(ASET (MAKE-ARRAY NIL ART-Q 200. NIL '(NIL 0))	;cons an array with leader
		      GT40-DISPLAY-LIST GT40-CURRENT-ITEM-NUMBER))))	;and install it
	  (( I WORD-COUNT)
	   (IF (= 0 MODE)		; was char mode, display the string
	       (GT40-DRAW-STRING (GT40-LAST-ITEM DOB) XPOS YPOS PC-PPR))
	   (IF BLINK-THIS (STORE-ARRAY-LEADER 'ON DOB 1)))
	(SETQ WORD (GT40-WORD))
	(COND ((BIT-TEST 100000 WORD)		;If command, only look at blink bit and mode
	       (IF (NOT (BIT-TEST 40000 WORD))	;ignore words with the 40000 bit on
		   (LET ((NMODE (LDB 1303 WORD))
			 (BLINK-FLAG (AND (BIT-TEST 20 WORD) (BIT-TEST 10 WORD))))
			(COND ((NOT (= MODE NMODE))		;get the new datatype mode
			       (IF (= 0 MODE)		; was char mode, display the string
				   (GT40-DRAW-STRING (GT40-LAST-ITEM DOB) XPOS YPOS PC-PPR))
			       (SETQ MODE NMODE)
			       (APUSH DOB
				      (SELECTQ
				       MODE	;initializings
					(0 (MAKE-ARRAY NIL 'ART-STRING 10. NIL '(0)))
					(1 'VECTOR)
					(2 'VECTOR)
					(3 'POINT)
					(6 'RPOINT)
					((4 5 7) 'UNKNOWN)))))
			(COND (BLINK-FLAG
				(OR (MEMQ GT40-BLINKER (PC-PPR-BLINKER-LIST PC-PPR))
				    (SETQ GT40-BLINKER
					  (TV-DEFINE-BLINKER PC-PPR 'FUNCTION
							     'GT40-BLINKER-FUNCTION)))
				(SETQ BLINK-THIS T))))))
	      (T (SELECTQ MODE
		   (0 (DO ((CHAR (LDB 0007 WORD) (LDB 1007 WORD))	;character mode
			   (STRING (GT40-LAST-ITEM DOB))
			   (I 0 (1+ I)))
			  ((= I 2))
			(OR (= 0 CHAR) (= 17 CHAR) (ARRAY-PUSH-EXTEND STRING CHAR))))
		   (1 (GT40-SHORT DOB WORD)	;short vector
		      (GT40-VECTOR DOB XPOS YPOS PC-PPR))
		   (2 (SETQ I (1+ I))	;long vector
		      (GT40-LONG DOB WORD (GT40-WORD))
		      (GT40-VECTOR DOB XPOS YPOS PC-PPR))
		   (3 (SETQ I (1+ I))	;point data
		      (GT40-LONG DOB WORD (GT40-WORD))
		      (LET ((I (GT40-LAST-INDEX DOB)))
			(SETQ XPOS (AREF DOB (- I 2))
			      YPOS (AREF DOB (- I 1)))
			(GT40-DRAW-VECTOR XPOS YPOS 0 0 (AREF DOB I) PC-PPR)))
		   (4)			;graphplot x data (not used)
		   (5)			;graphplot y data (not used)
		   (6 (GT40-SHORT DOB WORD)	;relative point data
		      (LET ((I (GT40-LAST-INDEX DOB)))
			(SETQ XPOS (+ XPOS (AREF DOB (- I 2)))
			      YPOS (+ YPOS (AREF DOB (- I 1))))
			(GT40-DRAW-VECTOR XPOS YPOS 0 0 (AREF DOB I) PC-PPR)))
		   (7)))))			;not used
    (GT40-WORD)))				;gobble the checksum

;;; GT40 Command 2 - Delete a display item from the display list
(DEFUN GT40-DELETE (PC-PPR &OPTIONAL (NITEMS 1) (CHECKSUM-FLAG T))
    (DO ((I 0 (1+ I)) (DOB) (ITEM-NUMBER))
	(( I NITEMS))
      (SETQ ITEM-NUMBER (GT40-WORD)
	    GT40-CURRENT-ITEM-NUMBER ITEM-NUMBER	;record item # being hacked
	    DOB (AREF GT40-DISPLAY-LIST ITEM-NUMBER))
      (IF DOB (PROGN (OR (EQ 'OFF (ARRAY-LEADER DOB 1))	;don't erase if its already off
			 (GT40-DISPLAY-ITEM DOB PC-PPR))
		     (FILLARRAY DOB '(NIL))
		     (STORE-ARRAY-LEADER 0 DOB 0)	;zero the fill pointer
		     (STORE-ARRAY-LEADER NIL DOB 1))))	;blinking is off
    (IF CHECKSUM-FLAG (GT40-WORD)))		;gobble the checksum

;;; Display a display item.
(DEFUN GT40-DISPLAY-ITEM (DOB PC-PPR)
    (GT40-BIND-TTY-PARAMS PC-PPR
      (DO ((I 0 (1+ I)) (END (ARRAY-ACTIVE-LENGTH DOB))
	   (ITEM) (X) (Y) (FLAG) (XPOS 0) (YPOS 0))
	  ((>= I END))
	(SETQ ITEM (AREF DOB I))
	(COND ((STRINGP ITEM) (GT40-DRAW-STRING ITEM XPOS YPOS PC-PPR))
	      ((EQ 'UNKNOWN ITEM))		;ignore
	      (T (DO NIL
		     ((OR (<= (- END I) 3)
			  (SYMBOLP (AREF DOB (1+ I)))
			  (STRINGP (AREF DOB (1+ I)))))
		   (SETQ I (+ 3 I)
			 X    (AREF DOB (- I 2))
			 Y    (AREF DOB (- I 1))
			 FLAG (AREF DOB I))
		   (SELECTQ ITEM
		     (VECTOR (GT40-DRAW-VECTOR XPOS YPOS X Y FLAG PC-PPR))
		     (POINT (SETQ XPOS X YPOS Y)
			    (GT40-DRAW-VECTOR XPOS YPOS 0 0 FLAG PC-PPR))
		     (RPOINT (SETQ XPOS (+ XPOS X) YPOS (+ YPOS Y))
			     (GT40-DRAW-VECTOR XPOS YPOS 0 0 FLAG PC-PPR)))))))))

;;; Blink a display item
(DEFUN GT40-BLINKER-FUNCTION (BLINKER OPCODE &REST IGNORE)
    (DO ((ITEM (G-L-P GT40-DISPLAY-LIST) (CDR ITEM))
	 (PC-PPR (TV-BLINKER-PC-PPR BLINKER))
	 (BLINK-FLAG NIL NIL) (DITEM))
	((NULL ITEM))
      (SETQ DITEM (CAR ITEM))
      (IF DITEM (SETQ BLINK-FLAG (ARRAY-LEADER DITEM 1)))
      (IF (AND (MEMQ BLINK-FLAG '(ON OFF))
	       (OR (EQ ':BLINK OPCODE) (AND (EQ T OPCODE) (EQ 'OFF BLINK-FLAG))
		   (AND (NULL OPCODE) (EQ 'ON BLINK-FLAG))))
	  (PROGN (GT40-DISPLAY-ITEM DITEM PC-PPR)
		 (STORE-ARRAY-LEADER (SELECTQ BLINK-FLAG (ON 'OFF) (OFF 'ON)) DITEM 1)))))

;;; ARDS simulator (for compatibility with PTV's)

(DEFVAR ARDS-XPOS 0)				;current pos in ARDS coordinates
(DEFVAR ARDS-YPOS 0)
(DEFVAR ARDS-SCALE 1.0)
(DEFVAR ARDS-SCR-XPOS 0)			;current pos in screen coordinates
(DEFVAR ARDS-SCR-YPOS 0)

;;; Setup scaling and offsets, then loop until exit condition
(DEFMACRO ARDS-LOOP (&REST BODY)
  `(LET* ((ARDS-MAX-X (<- SELF ':RIGHT))
	  (ARDS-MAX-Y (<- SELF ':BOTTOM))
	  (ARDS-X-OFFSET (<- SELF ':LEFT))
	  (ARDS-Y-OFFSET (<- SELF ':TOP))
	  (ARDS-X-WIDTH (- ARDS-MAX-X ARDS-X-OFFSET))
	  (ARDS-Y-WIDTH (- ARDS-MAX-Y ARDS-Y-OFFSET))
	  (ARDS-SCR-SCALE (* ARDS-SCALE (// (MIN ARDS-X-WIDTH ARDS-Y-WIDTH) 1023.0)))
	  (ARDS-CENTER-OFFSET (// (1+ (- (MAX ARDS-X-WIDTH ARDS-Y-WIDTH)
					 (MIN ARDS-X-WIDTH ARDS-Y-WIDTH)))
				  2)))
     (IF (< ARDS-X-WIDTH ARDS-Y-WIDTH) (SETQ ARDS-MAX-Y (- ARDS-MAX-Y ARDS-CENTER-OFFSET))
	 (SETQ ARDS-X-OFFSET (+ ARDS-X-OFFSET ARDS-CENTER-OFFSET)))
     (*CATCH 'ARDS-RETURN
	     (DO NIL (NIL) ,@BODY))))

;;; Convert -512./511. to 0/1023. and scale if the user wants it.
(DEFMACRO ARDS-COORD (X)
  `(MAX 1 (FIX (+ .5 (* ARDS-SCR-SCALE (+ 512. ,X))))))

(DECLARE (SPECIAL SUPDUP-STREAM))

;;; Get a character and punt out of graphics mode if it is a control char or %TD code
(DEFMACRO ARDS-GET ()
  '(LET ((X (SUPDUP-NETI)))
     (IF (OR (< X 100) (> X 177))
	 (*THROW 'ARDS-RETURN
		 (PROGN (FUNCALL SUPDUP-STREAM ':UNTYI X)
			(TV-SET-CURSORPOS PC-PPR
					  ARDS-SCR-XPOS
					  (- ARDS-SCR-YPOS 11.)))))
     X))

;;; Unpack long and short format coordinates
(DEFMACRO ARDS-LONG (F)
  `(LET ((A (ARDS-GET)) (B (ARDS-GET)))
     ,(IF F '(SETQ ARDS-FLAG (NOT (BIT-TEST B 40))))
     (* (IF (BIT-TEST A 1) -1 1)
	(LOGIOR (LSH (LOGAND 77 A) -1) (LSH (LOGAND 37 B) 5)))))

(DEFMACRO ARDS-SHORT ()
  `(LET ((A (ARDS-GET)))
     (SETQ ARDS-FLAG T)
     (* (IF (BIT-TEST A 1) -1 1)
	(LSH (LOGAND 77 A) -1))))

;;; Draw a vector
(DEFMACRO ARDS-VECTOR (DX DY)
  `(LET ((X0 ARDS-XPOS) (Y0 ARDS-YPOS))
     (SETQ ARDS-XPOS (+ ARDS-XPOS ,DX)
	   ARDS-YPOS (+ ARDS-YPOS ,DY)
	   ARDS-SCR-XPOS (MIN ARDS-MAX-X (+ ARDS-X-OFFSET (ARDS-COORD ARDS-XPOS)))
	   ARDS-SCR-YPOS (MAX ARDS-Y-OFFSET (- ARDS-MAX-Y (ARDS-COORD ARDS-YPOS))))
     (IF ARDS-FLAG
	 (TV-DRAW-LINE (MIN ARDS-MAX-X (+ ARDS-X-OFFSET (ARDS-COORD X0)))
		       (MAX ARDS-Y-OFFSET (- ARDS-MAX-Y (ARDS-COORD Y0)))
		       ARDS-SCR-XPOS
		       ARDS-SCR-YPOS
		       TV-ALU-IOR
		       (PC-PPR-SCREEN PC-PPR)))))

(DEFUN SUPDUP-ARDS-SET (PC-PPR)
  (ARDS-LOOP
    (SETQ ARDS-XPOS (ARDS-LONG T) ARDS-YPOS (ARDS-LONG NIL))
    (ARDS-VECTOR 0 0)))		;for plotting points

(DEFUN SUPDUP-ARDS-LONG (PC-PPR)
  (ARDS-LOOP (ARDS-VECTOR (ARDS-LONG T) (ARDS-LONG NIL))))

(DEFUN SUPDUP-ARDS-SHORT (PC-PPR)
  (ARDS-LOOP (ARDS-VECTOR (ARDS-SHORT) (ARDS-SHORT))))

;-- TELNET USER PROGRAM 

(DECLARE
 (SPECIAL TELNET-KEYS
	   NVT-IAC NVT-DONT NVT-DO NVT-WONT NVT-WILL NVT-TIMING-MARK
	  NVT-SUPRESS-GO-AHEAD NVT-ECHO NVT-TRANSMIT-BINARY))

(SETQ NVT-TRANSMIT-BINARY 0 NVT-ECHO 1 NVT-SUPRESS-GO-AHEAD 3 NVT-TIMING-MARK 6
      NVT-WILL 373 NVT-WONT 374 NVT-DO 375 NVT-DONT 376 NVT-IAC 377)

(DEFCLASS TELNET-CLASS SUPDUP-CLASS
	  (TELNET-NEW-TELNET-P
	   TELNET-MORE-FLAG
           TELNET-ECHO-FLAG))

(DEFMETHOD (TELNET-CLASS :BORN) ()
    (OR NAME (SETQ NAME (STRING-APPEND "TELNET-" (GENSYM))))
    (<-AS SUPDUP-CLASS ':BORN)
    (SETF (PC-PPR-MORE-FCN SI:PC-PPR) 'TELNET-MORE)
    (SETQ SUPDUP-ESCAPE 201))

(DECLARE (SPECIAL MAIN-TELNET-WINDOW))

(DEFUN TELNET (&OPTIONAL PATH WINDOW CONTACT-NAME &AUX MTW)
    (COND ((BOUNDP 'MAIN-TELNET-WINDOW))
	  (T (SETQ MTW (<- TELNET-CLASS ':NEW ':NAME "Telnet"))
	     (<- (<- SI:WINDOW-SINGLE-FRAME-CLASS ':NEW)
		 ':PANE<-  MTW)
	     (SETQ MAIN-TELNET-WINDOW MTW)))
    (COND ((OR PATH (NOT (<- MAIN-TELNET-WINDOW ':CONNECTED-P)))
	   (<- MAIN-TELNET-WINDOW ':CONNECT (OR PATH "AI") WINDOW CONTACT-NAME)))
    (LET ((SW SELECTED-WINDOW))
      (WINDOW-SELECT MAIN-TELNET-WINDOW)
      (PROCESS-WAIT "TELNET" #'(LAMBDA (OUR-STATUS) (CAR OUR-STATUS))
		    (LOCATE-IN-CLOSURE SW ':STATUS))))

(DEFMETHOD (TELNET-CLASS :CONNECT) (&OPTIONAL (PATH "AI") NET-WINDOW CONTACT-NAME
				    &AUX BRIDGE TEM CONNECTION)
    (<- SELF ':DISCONNECT)
    (IF (NULL NET-WINDOW)(SETQ NET-WINDOW 3))
    (SETQ PATH (STRING PATH))
    (COND ((SETQ TEM (STRING-SEARCH-CHAR #/ PATH))
	   (SETQ BRIDGE (NSUBSTRING PATH 0 TEM))
	   (SETQ PATH (NSUBSTRING PATH (1+ TEM)))))
    (AND (NULL BRIDGE)
         (NOT (ASSOC PATH CHAOS:HOST-ALIST))
         (SETQ BRIDGE "AI"))
    (COND (BRIDGE
           (SETQ CONTACT-NAME (COND ((STRINGP CONTACT-NAME) CONTACT-NAME)
                                    ((NULL CONTACT-NAME) " 27")
                                    (T (FORMAT NIL " ~O" CONTACT-NAME))))
	   (SETQ CONTACT-NAME (STRING-APPEND "ARPA " PATH CONTACT-NAME))
	   (SETQ PATH BRIDGE)))
    (IF (NULL CONTACT-NAME)(SETQ CONTACT-NAME "TELNET"))
    (SETQ TELNET-MORE-FLAG NIL
	  TELNET-NEW-TELNET-P NIL
          TELNET-ECHO-FLAG NIL
	  CONNECTION (CHAOS:CONNECT PATH CONTACT-NAME NET-WINDOW))
    (COND ((STRINGP CONNECTION) CONNECTION)
          (T (<- SUPDUP-TYPE-IN-PROCESS ':FLUSH)
             (<- SUPDUP-TYPE-OUT-PROCESS ':FLUSH)
             (SETQ SUPDUP-CONNECTION CONNECTION)
	     (SETQ SUPDUP-STREAM (CHAOS:STREAM SUPDUP-CONNECTION))
	     (SETQ LABEL-STATUS
		   (STRING-APPEND
                       (CHAOS:HOST-DATA (CHAOS:FOREIGN-ADDRESS SUPDUP-CONNECTION))))
             (AND SI:STATUS (<- SELF ':UPDATE-LABEL))
	     (<- SUPDUP-TYPE-IN-PROCESS ':RESET)
	     (<- SUPDUP-TYPE-OUT-PROCESS ':RESET))))

;; Redefine some subroutines of the SUPDUP input side.

;; Make the wait function detect and handle more's.
(DEFMETHOD (TELNET-CLASS :WAIT-FOR-KBD-INPUT) ()
    (OR (KBD-CHAR-AVAILABLE)
	(FUNCALL SUPDUP-STREAM ':FORCE-OUTPUT))
    (DO () (())
	(PROCESS-WAIT "TELNET TYI"
		      (FUNCTION (LAMBDA (CONN-LOC COMM-LOC MORE-FLAG-LOC)
			  (OR (KBD-CHAR-AVAILABLE)
			      (NOT (EQ (CHAOS:STATE (CDR CONN-LOC))
				       'CHAOS:OPEN-STATE))
			      (CDR COMM-LOC)
			      (CDR MORE-FLAG-LOC))))
		      (LOCATE-IN-CLOSURE SELF 'SUPDUP-CONNECTION)
		      (LOCATE-IN-CLOSURE SELF 'SUPDUP-COMMUNICATE)
		      (LOCATE-IN-CLOSURE SELF 'TELNET-MORE-FLAG))
	(COND (TELNET-MORE-FLAG
	       (SI:TV-MORE-DEFAULT TELNET-MORE-FLAG)
	       (SETQ TELNET-MORE-FLAG NIL))
	      (T (RETURN NIL)))))

(SETQ TELNET-KEYS (MAKE-ARRAY NIL 'ART-Q 20))
(FILLARRAY TELNET-KEYS '(0 201 0 32 0 37 0 177 10 11 12 13 14 15 201))

;; Read character and convert to NVT ASCII (except don't convert CR to two characters).
;; Same calling and returning conventions as the one in SUPDUP-CLASS.
(DEFMETHOD (TELNET-CLASS :KBD-TYI) ()
    (PROG (CH)
      TOP (SETQ CH (SI:KBD-TYI-1))
          (COND ((= CH #\ESC)
		 (SI:KBD-ESC)
		 (GO TOP)))
	  (RETURN 
	   (LET ((CHAR (LDB %%KBD-CHAR CH)))
	     (AND TELNET-ECHO-FLAG		;Would be nice to do rubout, but...
		  (FUNCALL SUPDUP-TERMINAL-STREAM ':TYO CHAR))
	     (OR (ZEROP (LDB %%KBD-CONTROL CH)) (SETQ CHAR (LDB 0005 CH)))	;controlify
	     (COND ((>= CHAR 200) (AR-1 TELNET-KEYS (- CHAR 200)))
		   (T CHAR)))
	   CH)))

(DEFMETHOD (TELNET-CLASS :NET-OUTPUT) (CH)
  (SUPDUP-OLOCK
    (FUNCALL SUPDUP-STREAM ':TYO CH)
    (AND (= CH 15) (FUNCALL SUPDUP-STREAM ':TYO 12))))	;CR is two chars

;; Redefine a few subroutines of the SUPDUP output side.

;; There's no special greeting sent to us at the start.
(DEFMETHOD (TELNET-CLASS :GOBBLE-GREETING) ()
    (TV-CRLF SI:PC-PPR))

;; This method looks at each input char from the net
;; and returns a character to print or T to do nothing.
(DEFMETHOD (TELNET-CLASS :CHAR-PRE-PROCESS) (CH)
    (COND ((= CH NVT-IAC)
	   (<- SELF ':HANDLE-IAC)			;Perform new telnet negotiations.
	   T)
	  ((>= CH 200) T)				;Ignore otelnet negotiations
	  ((= CH 7) (TV-BEEP) T)			;^G rings the bell.
	  ((= CH 12) T)					;Ignore linefeeds
	  ((AND (>= CH 10) (<= CH 15))			;Convert formatting controls
	   (+ CH 200))					;to Lisp machine char set.
	  (T CH)))

;; New telnet protocol IAC handler
(DEFMETHOD (TELNET-CLASS :HANDLE-IAC) (&AUX COMMAND OPTION)
    (COND ((NULL TELNET-NEW-TELNET-P)
	   (TELNET-SEND-OPTION NVT-DO NVT-ECHO)
	   (TELNET-SEND-OPTION NVT-DO NVT-SUPRESS-GO-AHEAD)
	   (SETQ TELNET-NEW-TELNET-P T)))
    (SETQ COMMAND (SUPDUP-NETI))
    (AND ( COMMAND NVT-WILL) ( COMMAND NVT-DONT)
	 (SETQ OPTION (SUPDUP-NETI)))
    (SELECT COMMAND
     (NVT-WILL
      (SELECT OPTION
       (NVT-ECHO
        (TELNET-ECHO NIL))
       (NVT-SUPRESS-GO-AHEAD)		;ignore things we requested
       (NVT-TRANSMIT-BINARY
	(TELNET-SEND-OPTION NVT-DO OPTION))
       (OTHERWISE
	(TELNET-SEND-OPTION NVT-DONT OPTION))))
     (NVT-DO
      (COND ((= OPTION NVT-ECHO) (TELNET-ECHO T))
            ((OR (= OPTION NVT-SUPRESS-GO-AHEAD) (= OPTION NVT-TIMING-MARK))
	     (TELNET-SEND-OPTION NVT-WILL OPTION))
	    (T (TELNET-SEND-OPTION NVT-WONT OPTION))))
     (NVT-DONT
      (COND ((= OPTION NVT-ECHO) (TELNET-ECHO NIL))
            ((= OPTION NVT-TRANSMIT-BINARY)
             (TELNET-SEND-OPTION NVT-WONT OPTION))))
     (NVT-WONT
      (COND ((= OPTION NVT-ECHO) (TELNET-ECHO T))
            ((= OPTION NVT-TRANSMIT-BINARY)
             (TELNET-SEND-OPTION NVT-DONT OPTION))))))

(DEFUN TELNET-ECHO (ON-P)
  (LOCAL-DECLARE ((SPECIAL TELNET-ECHO-FLAG))
    (COND ((NEQ TELNET-ECHO-FLAG ON-P) ;If not the right way already
           (SETQ TELNET-ECHO-FLAG ON-P)
           (TELNET-SEND-OPTION (IF ON-P NVT-DO NVT-WILL) NVT-ECHO)))))

(DEFUN TELNET-SEND-OPTION (COMMAND OPTION)
  (LOCAL-DECLARE ((SPECIAL SUPDUP-STREAM))
    (SUPDUP-OLOCK
      (FUNCALL SUPDUP-STREAM ':TYO NVT-IAC)
      (FUNCALL SUPDUP-STREAM ':TYO COMMAND)
      (FUNCALL SUPDUP-STREAM ':TYO OPTION)
      (FUNCALL SUPDUP-STREAM ':FORCE-OUTPUT))))

;; More function while in telnet-typeout-process
(DEFUN TELNET-MORE (PC-PPR)
  (LOCAL-DECLARE ((SPECIAL TELNET-MORE-FLAG))
    (SETQ TELNET-MORE-FLAG PC-PPR)
    (PROCESS-WAIT "MORE" (FUNCTION (LAMBDA (MORE-FLAG-LOCATION) (NOT (CDR MORE-FLAG-LOCATION))))
		  (LOCATE-IN-CLOSURE SELF 'TELNET-MORE-FLAG))))
