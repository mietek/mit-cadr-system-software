;;; -*- Mode: LISP; Package: SI -*-

;; Com-link facility for Lisp Machines.

;; list who is on.
;; find free cadrs
;; eval here and there.

(DECLARE (SPECIAL CADR-MENU))

(DEFVAR COM-LINK-POP-UP-WINDOW
	(<- POP-UP-TEXT-WINDOW-CLASS ':NEW ':LEFT 20. ':TOP 322. ':RIGHT 355. ':BOTTOM 465.))

(DEFCLASS COM-LINK-FRAME-CLASS WINDOW-FRAME-CLASS
  (HOST						;The host we are communicating with.
    CONN					;The Chaos net connction to that host.
    REMOTE-HOST-STREAM				;The remote host's stream.
    REMOTE-ECHO-STREAM				;The stream on which to echo remote input.
    LOCAL-STREAM				;The local stream.
    REMOTE-LISTEN-PROCESS			;The process that listens to the other host.
    LOCAL-LISTEN-PROCESS			;The process that listens to kbd.
    BUFFER))					;The buffer to read into from the other host.


;; We need a new class of windows since we want a specific pane to be selected 
;; when either pane is moused.
(DEFCLASS COM-LINK-PANE-CLASS WINDOW-WITH-PC-PPR-AND-BOX-CLASS ())

;; The :BORN method sets things up.
(DEFMETHOD (COM-LINK-FRAME-CLASS :BORN) (&AUX REMOTE-PANE LOCAL-PANE)
  ;; Set up basic parameters.
  (OR SCREEN (SETQ SCREEN SI:TV-DEFAULT-SCREEN))
  (OR NAME (SETQ NAME  (STRING-APPEND "Com-link " (GENSYM '-))))
  ;; Set to a default size.
  (COND ((NULL LEFT)
	 (SETQ LEFT 0 TOP 0 RIGHT 0 BOTTOM 0)	;So window code doesn't blow up.
 	 (<- SELF ':EDGES<-  50. 50.
			    (FIX (* (SCREEN-X2 TV-DEFAULT-SCREEN) .9))
			    (FIX (* (SCREEN-Y2 TV-DEFAULT-SCREEN) .5)))))
  (OR LABEL
      (SETQ SI:LABEL (STRING-APPEND "Com-link to " HOST "   (<break>? or <help> for help.)")))
  (SETQ REMOTE-PANE (<- COM-LINK-PANE-CLASS
			':NEW ':NAME "Com-link Remote Pane"
			      ':PC-PPR `(NIL :MORE-P NIL :BLINKER-P NIL
					     :OUTPUT-HOLD-FCN ,#'COM-LINK-OUTPUT-HOLD-FCN))
	LOCAL-PANE (<- COM-LINK-PANE-CLASS
		       ':NEW ':NAME "Com-link Local Pane"
                	     ':PC-PPR `(NIL :MORE-P NIL
					    :OUTPUT-HOLD-FCN ,#'COM-LINK-OUTPUT-HOLD-FCN))
	PANES (LIST REMOTE-PANE LOCAL-PANE)
	RESET-PANES (LIST REMOTE-PANE LOCAL-PANE)
	RESET-SELECTED-PANE LOCAL-PANE)
  (<- SELF ':PANE-LAYOUT<- (LIST ':VERTICAL REMOTE-PANE .5 LOCAL-PANE))
  (SETQ SELECTED-PANE LOCAL-PANE)		;Must be done after the panes are set.
  (SETQ LOCAL-STREAM (TV-MAKE-STREAM (<- LOCAL-PANE ':PC-PPR))
	REMOTE-ECHO-STREAM (TV-MAKE-STREAM (<- REMOTE-PANE ':PC-PPR)))
  ;; Parallelism hackery, one process to listen to remote, one to listen local.
  (SETQ PROCESS (<- PARALLELISM-CLASS ':NEW ':NAME (STRING-APPEND NAME " parallelism")))
  (SETQ LOCAL-LISTEN-PROCESS (PROCESS-CREATE (STRING-APPEND NAME " local")))
  (SETQ REMOTE-LISTEN-PROCESS (PROCESS-CREATE (STRING-APPEND NAME " remote")))
  (<- PROCESS ':PROCESSES<-
      (LIST LOCAL-LISTEN-PROCESS REMOTE-LISTEN-PROCESS))
  (<- PROCESS ':ENABLED-PROCESSES<-
      (LIST LOCAL-LISTEN-PROCESS REMOTE-LISTEN-PROCESS))
  (<- PROCESS ':INPUT-PROCESS<- LOCAL-LISTEN-PROCESS)
  ;; Set their top level functions.
  (PROCESS-PRESET LOCAL-LISTEN-PROCESS
		  (FUNCTION <-) SELF ':LOCAL-LISTEN-TOP-LEVEL)
  (PROCESS-PRESET REMOTE-LISTEN-PROCESS
		  (FUNCTION <-) SELF ':REMOTE-LISTEN-TOP-LEVEL)
  ;; Input buffer from remote host.
  (SETQ BUFFER (MAKE-ARRAY NIL 'ART-STRING 50.)))


;; New output hold function, buzzes and exposes frame.
(DEFUN COM-LINK-OUTPUT-HOLD-FCN (IGNORE)
  (MAPCAR #'SI:%BEEP '(1000 5000)'(500000 600000))  
  (COND ((NEQ (CLASS (WINDOW-OWNING-MOUSE))
	      MOMENTARY-MENU-CLASS)
	 (LOCK-SCREEN-LAYOUT (<- SELF ':EXPOSE)))
	(T (PROCESS-WAIT "go awawy menu!"
			 #'(LAMBDA () (NEQ (CLASS (WINDOW-OWNING-MOUSE))
					   MOMENTARY-MENU-CLASS)))
	   (LOCK-SCREEN-LAYOUT (<- SELF ':EXPOSE)))))   

;; Make a connection to the HOST.  Return result of trying, string, if failure.
(DEFMETHOD (COM-LINK-FRAME-CLASS :MAKE-CONNECTION) (&OPTIONAL H)
  (AND H (SETQ HOST H))
  (FORMAT LOCAL-STREAM "Attempting to establish connection to ~A" HOST)
  (AND						;In case we are already connected.
    (ARRAYP CONN)
    (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN)
    (CHAOS:CLOSE CONN))
  (SETQ CONN (CHAOS:CONNECT HOST "Com-Link"))
  (COND ((NOT (STRINGP CONN))
	 (SETQ REMOTE-HOST-STREAM (CHAOS:STREAM CONN))
	 ;; First thing I have to tell him who I am, delimit with a space.
	 (PRINC (STRING-APPEND USER-ID " ") REMOTE-HOST-STREAM)
	 (FUNCALL REMOTE-HOST-STREAM ':FORCE-OUTPUT)
	 (SETQ LABEL (STRING-APPEND "Com-link with " HOST "   (<break>? or <help> for help.)"))
	 (FORMAT LOCAL-STREAM "~%Connection established...~%")
	 (<- SELF ':UPDATE-LABEL)))
  CONN)


;; The following 3 method definitions are needed just to make sure that 
;; one specific pane is always selected when any in the frame are moused.

;; Want the local pane to always be selected when the frame is selected.
(DEFMETHOD (COM-LINK-FRAME-CLASS :SELECT) ()
    (<-AS WINDOW-CLASS ':SELECT)
    (<- RESET-SELECTED-PANE ':SELECT))		;Where the local pane lives.

;; Make sure we always select the reset-selected-pane of our frame.
(DEFMETHOD (COM-LINK-PANE-CLASS :SELECT) ()
  (LET ((PANE (<- FRAME ':RESET-SELECTED-PANE)))
    (IF (EQ SELF PANE)
	(<-AS WINDOW-WITH-PC-PPR-AND-BOX-CLASS ':SELECT)
	(<- PANE ':SELECT))))

;; When a pane gets a :select message, it sends us this.
;; We make sure we always select the reset selected-pane.
(DEFMETHOD (COM-LINK-FRAME-CLASS :SELECT-PANE) (IGNORE)
  (OR (EQ SELECTED-PANE RESET-SELECTED-PANE)
      (FUNCALL SELF ':SELECTED-PANE<- RESET-SELECTED-PANE))
  (OR (EQ STATUS ':SELECTED)
      (<-AS WINDOW-CLASS ':SELECT)))

(DEFMETHOD (COM-LINK-FRAME-CLASS :EXPOSE) ()
  (<- PROCESS ':RUN-REASON PROCESS)
  (<-AS WINDOW-FRAME-CLASS ':EXPOSE))

;; When we go away make sure Chaos connection is closed.
(DEFMETHOD (COM-LINK-FRAME-CLASS :DEACTIVATE) ()
 (AND CONN (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)
      (CHAOS:CLOSE CONN))
  (<- PROCESS ':REVOKE-RUN-REASON PROCESS)
  (<-AS WINDOW-FRAME-CLASS ':DEACTIVATE))

(DEFMETHOD (COM-LINK-FRAME-CLASS :MOUSE-RESHAPE) (&AUX INHIBIT-ALL-SCREEN-BLTING-FLAG)
  (LOCK-SCREEN-LAYOUT
    (LET ((INHIBIT-AUTOEXPOSE-FLAG T))
      (<- SELF ':EDGES<- (MOUSE-SPECIFY-RECTANGLE LEFT TOP RIGHT BOTTOM))
      (LET ((INHIBIT-SCREEN-RESTORATION-FLAG T))
	(<- SELF ':SELECT)))
    (WINDOWS-AUTOEXPOSE)))

;; This method is run inside its own process to handle input and send it over the net.
(DEFMETHOD (COM-LINK-FRAME-CLASS :LOCAL-LISTEN-TOP-LEVEL) ()
  (*CATCH 'NUKE-THE-WORLD
	  (DO () (NIL)
	    ;; If no connetion yet then wait.
	    (OR CONN (PROCESS-WAIT "Chaos Conn Wait" #'CDR
				   (LOCATE-IN-CLOSURE SELF 'CONN)))
	    (COND ((NOT (STRINGP CONN))		;Connection Succeeded.
		   ;The main loop, read, echo, and send characters.
		   (DO ((CHAR 0.)(QUITTING-A-CONNECTION NIL))
		       (QUITTING-A-CONNECTION)
		     (SETQ CHAR (COM-LINK-READ LOCAL-STREAM 'LOCAL-STREAM SELF CONN))
		     (COND ((EQ CHAR #\BREAK)
			    (SETQ QUITTING-A-CONNECTION
				  (COM-LINK-HANDLE-BREAK-OR-HELP
				   'BREAK LOCAL-STREAM REMOTE-HOST-STREAM SELF CONN)))
			   ((EQ CHAR #\HELP)
			    (SETQ QUITTING-A-CONNECTION
				  (COM-LINK-HANDLE-BREAK-OR-HELP
				   'HELP LOCAL-STREAM REMOTE-HOST-STREAM SELF CONN)))
			   (T (COM-LINK-PROCESS-CHAR LOCAL-STREAM CHAR)
			      (FUNCALL REMOTE-HOST-STREAM ':TYO CHAR)
			      (FUNCALL REMOTE-HOST-STREAM ':FORCE-OUTPUT)))))
		  ;Openning of connection failed...
		  (T
		    (FORMAT LOCAL-STREAM "~%Can't open Chaos connection:~%~A~%" CONN)
		    (COND ((Y-OR-N-P
			     "Try harder? (ie load LMIO;COMLNK through his EVAL server) "
			     LOCAL-STREAM)
			   (COND ((COM-LINK-TRY-HARDER HOST LOCAL-STREAM)
				  (<- SELF ':MAKE-CONNECTION))
				 (T (*THROW 'NUKE-THE-WORLD NIL))))
			  (T (*THROW 'NUKE-THE-WORLD NIL)))))))
  (<- SELF ':DEACTIVATE))			;If you gotta go, you gotta go...

;; Handle a break or help request depending on first arg.  We return T if
;; all is ok, NIL if we are abandoning current chaos connection for another
;; one and if we are quitting altogether we throw to the 'NUKE-THE-WORLD tag.
(DEFUN COM-LINK-HANDLE-BREAK-OR-HELP
       (BREAK-OR-HELP LOCAL-STREAM REMOTE-HOST-STREAM WINDOW CONN
		      &AUX CHAR P-STREAM POP-UP-POSITION)
  (SETQ POP-UP-POSITION (IF (< (<- WINDOW ':TOP)
			       (// (- (SCREEN-Y2 (<- WINDOW ':SCREEN))
				      (SCREEN-Y1 (<- WINDOW ':SCREEN)))
				   2))
			    ':BOTTOM ':TOP))
  (COND ((EQ BREAK-OR-HELP 'BREAK)
       (SETQ P-STREAM (COM-LINK-POP-UP 229. 64.
					  (<- WINDOW ':LEFT) (<- WINDOW POP-UP-POSITION)
					  "Com Link Command:"))
       (LET ((SI:KBD-TYI-HOOK #'(LAMBDA (C) C)))	;So <break> doesn't cause a break.
	    (SETQ CHAR (TYI P-STREAM)))
       (<- COM-LINK-POP-UP-WINDOW ':POP-DOWN))
      ((EQ BREAK-OR-HELP 'HELP)
       (SETQ CHAR #/?)))
  (PROG NIL
    TRY-AGAIN
    (SELECTQ (CHAR-UPCASE CHAR)
      (#\BREAK					;send a break on thru.
       (FUNCALL LOCAL-STREAM ':TYO CHAR)
       (FUNCALL REMOTE-HOST-STREAM ':TYO CHAR)
       (FUNCALL REMOTE-HOST-STREAM ':FORCE-OUTPUT)
       (RETURN T))
      (#/Q
       (*THROW 'NUKE-THE-WORLD NIL))		;Quit.
      (#/P
       (<- WINDOW ':BURY)				;Bury the window for now.
       (RETURN T))
      ((#/? #\HELP)
       (SETQ P-STREAM
	     (COM-LINK-POP-UP 470. 160.
			      (<- WINDOW ':LEFT) (<- WINDOW POP-UP-POSITION)
			      "The Com Link <break> Commands Are:"))
       (FORMAT P-STREAM				;Print documentation.
	       (LIST "~%    <break>  -  Send a break on through."
		     "~%    D  -  Disconnect and connect to new CADR."
		     "~%    Q  -  Quit."
		     "~%    P  -  Bury this Com Link window."
		     "~%    ? or <help>  -  Print this."
		     "~%    Anything else is ignored.~%"
		     "~%Com Link Command: "))
       (LET ((SI:KBD-TYI-HOOK #'(LAMBDA (C) C)))	;So <break> doesn't cause a break.
	 (SETQ CHAR (FUNCALL P-STREAM ':TYI)))
       (TYO CHAR P-STREAM)
       (<- COM-LINK-POP-UP-WINDOW ':POP-DOWN)
       (GO TRY-AGAIN))
      (#/D					;Disconnect and connect to new host.
	(FORMAT LOCAL-STREAM "~%Disconnecting from ~A~%" (<- WINDOW ':HOST))
	(CHAOS:CLOSE CONN)
	(LET ((NEW-CADR (<- CADR-MENU ':CHOOSE)))
	  (COND (NEW-CADR	   
		  (AND (NOT (STRINGP (<- WINDOW ':MAKE-CONNECTION NEW-CADR)))
		       (<- (<- WINDOW ':REMOTE-LISTEN-PROCESS) ':RESET))
		  (RETURN NIL))
		((*THROW 'NUKE-THE-WORLD NIL)))))
       (:OTHERWISE (RETURN T)))))				;Ignore otherwise.

(DEFUN COM-LINK-POP-UP (WIDTH HEIGHT X-LOC Y-LOC LABEL)
  (LET ((W COM-LINK-POP-UP-WINDOW))
    (<- W ':SIZE<- WIDTH HEIGHT)
    (<- W ':MOVE-NEAR X-LOC Y-LOC)
    (<- W ':LABEL<- LABEL)
    (<- W ':POP-UP)
    (<- W ':STREAM)))

;; Read a character from a stream, can't use TYI becase we want to know if the connection
;; closes on us.
(DEFUN COM-LINK-READ (STREAM STREAM-INSTANCE-VARIABLE-NAME WINDOW CONN )
  (PROCESS-WAIT "TYI"
		#'COM-LINK-TYI-WAIT-FCN
		;; Must send location of value in case it changes
		;; while we are waiting.
		(LOCATE-IN-CLOSURE WINDOW STREAM-INSTANCE-VARIABLE-NAME)
		(LOCATE-IN-CLOSURE WINDOW 'CONN))
  (AND (NEQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)
       (*THROW 'NUKE-THE-WORLD NIL))
  (FUNCALL STREAM ':TYI))

;; Contact his EVAL server and try to load the Com-link file.
;; Returns T on success, else nil.
(DEFUN COM-LINK-TRY-HARDER (HOST LOCAL-STREAM &AUX C S REPLY)
    (SETQ C (CHAOS:CONNECT HOST "EVAL"))
    (COND ((STRINGP C)
           (FORMAT LOCAL-STREAM "~%Attempt to connect to EVAL server failed:~%~A" C)
           (FORMAT LOCAL-STREAM "~%Type a character to flush:")
           (TYI LOCAL-STREAM)
           NIL)
          (T
           (FORMAT LOCAL-STREAM "~%Connection to EVAL server succeeded.~%")
           (FORMAT LOCAL-STREAM "Loading the Com-link file.~%")
           (SETQ S (CHAOS:STREAM C))
           (PRINC '(LOAD "lmio;comlnk") S)
           (FUNCALL S 'FORCE-OUTPUT)
           (SETQ REPLY (READ S))
           (FUNCALL S ':EOF)
           (CHAOS:CLOSE C)
           T)))

;; The function used by the two processes to wait for input.
(DEFUN COM-LINK-TYI-WAIT-FCN (STREAM CONN &AUX TEMP)
  (SETQ STREAM (CDR STREAM))
  (IF (EQ (CHAOS:STATE (CDR CONN)) 'CHAOS:OPEN-STATE)
      (PROG1
	(SETQ TEMP (FUNCALL STREAM ':TYI-NO-HANG))
	(COND (TEMP (FUNCALL STREAM ':UNTYI TEMP))))
      T))

;; This method is run inside its own process to handle remote input.
(DEFMETHOD (COM-LINK-FRAME-CLASS :REMOTE-LISTEN-TOP-LEVEL)()
  (*CATCH 'CHAOS-CONNECTION-CLOSED
	  (PROGN
	    (OR CONN (PROCESS-WAIT "Chaos Conn Wait"	;Wait for connection.
				   #'CDR (LOCATE-IN-CLOSURE SELF 'CONN)))
	    (DO ((LEN 0 0))
		((NOT (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)))
	      ;; Read characters until there are no more or buffer is full.
	      (DO ((CHAR))
		  ((COND ((NOT (SETQ CHAR (FUNCALL REMOTE-HOST-STREAM ':TYI-NO-HANG))))
			 (( LEN 50.))
			 ((NOT (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE))
			  ;; Connection closed, clean up and terminate.
			  (DOTIMES (I LEN)
			    (COM-LINK-PROCESS-CHAR
			      REMOTE-ECHO-STREAM (AR-1 BUFFER I)))
			  (*THROW 'CHAOS-CONNECTION-CLOSED NIL))))
		(AS-1 CHAR BUFFER LEN)
		(SETQ LEN (1+ LEN)))
	      ;; Output what we have read.
	      (DOTIMES (I LEN)
		(COM-LINK-PROCESS-CHAR REMOTE-ECHO-STREAM (AR-1 BUFFER I)))
	      ;; If we stopped reading becuase there were no more chacacters wait for some.
	      (COND ((< LEN 50.)
		     (PROCESS-WAIT "Chaos TYI" #'COM-LINK-TYI-WAIT-FCN
				   ;; Must send location of value in case it changes
				   ;; while we are waiting.
				   (LOCATE-IN-CLOSURE SELF 'REMOTE-HOST-STREAM)
				   (LOCATE-IN-CLOSURE SELF 'CONN))))))))

(DEFUN COM-LINK-PROCESS-CHAR (STREAM CHAR)
  (SELECTQ CHAR
      (#\RUBOUT
       (LET ((PC-PPR (SYMEVAL-IN-CLOSURE STREAM 'SI:TV-MAKE-STREAM-PC-PPR)))
	 (TV-BACKSPACE PC-PPR)
	 (TV-CLEAR-CHAR PC-PPR)))
      (#\FORM
       (FUNCALL STREAM ':CLEAR-SCREEN))
      (:OTHERWISE
	(FUNCALL STREAM ':TYO CHAR))))

;; This little gem is needed because we want the pop-up window to have the label
;; at the top instead of the bottom.  sigh...
(DEFMETHOD-INSTANCE (COM-LINK-POP-UP-WINDOW :EDGES<-)
	   (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM &AUX NEW-MARGINS)
    (MULTIPLE-VALUE (NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM)
		    (WINDOW-CHECK-EDGES SCREEN NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))
    (COND ((AND (EQ NEW-LEFT (SCREEN-X1 SCREEN))
		(EQ NEW-TOP (SCREEN-Y1 SCREEN))
		(EQ NEW-RIGHT (SCREEN-X2 SCREEN))
		(EQ NEW-BOTTOM (SCREEN-Y2 SCREEN)))
	   (SETQ NEW-MARGINS (LIST 0 (<- SELF ':LABEL-HEIGHT) 0 0)))
	  (T
	   (SETQ NEW-MARGINS (LIST 2 (+ 2 (<- SELF ':LABEL-HEIGHT)) 2 2))))
    (<- SELF ':EDGES-INSIDE-BOX<-
	(+ NEW-LEFT (CAR NEW-MARGINS))
	(+ NEW-TOP (CADR NEW-MARGINS))
	(- NEW-RIGHT (CADDR NEW-MARGINS))
	(- NEW-BOTTOM (CADDDR NEW-MARGINS)))
    (SETQ LABEL-POSITION
	  (LIST (+ NEW-LEFT (CAR NEW-MARGINS))
		(+ NEW-TOP (CADDDR NEW-MARGINS))
		(- NEW-RIGHT (CADDR NEW-MARGINS))
		(+ NEW-TOP (CADR NEW-MARGINS))))
    (SETQ MARGINS NEW-MARGINS)
    (<-AS WINDOW-CLASS ':EDGES<- NEW-LEFT NEW-TOP NEW-RIGHT NEW-BOTTOM))

;; Run this when we get a RFC from somebody.
(DEFUN RECEIVE-COM-LINK (&AUX CONN FRAME HOST REMOTE-HOST-STREAM)
  (SETQ CONN (CHAOS:LISTEN "Com-Link"))
  (CHAOS:ACCEPT CONN)
  (SETQ REMOTE-HOST-STREAM (CHAOS:STREAM CONN))
  ;; First read who he is.
  (SETQ HOST (DO ((CHAR (READCH REMOTE-HOST-STREAM)(READCH REMOTE-HOST-STREAM))
		  (HOST "" (STRING-APPEND HOST CHAR)))
		 ((STRING-EQUAL " " CHAR) HOST)))
  (SETQ FRAME (<- COM-LINK-FRAME-CLASS ':NEW
		  ':HOST HOST ':CONN CONN ':REMOTE-HOST-STREAM REMOTE-HOST-STREAM))
  (MAPC #'SI:%BEEP '(1000 5000)'(500000 600000))
  (WINDOW-SELECT FRAME))

;; For use in the window creation menu.
(DEFUN CREATE-COM-LINK-WINDOW-WITH-RESHAPING (&AUX HOST WINDOW)
  (COND ((SETQ HOST (<- CADR-MENU ':CHOOSE))
	 (SETQ WINDOW (<- COM-LINK-FRAME-CLASS ':NEW ':SCREEN MOUSE-SCREEN ':HOST HOST))
	 (LEXPR-FUNCALL WINDOW ':EDGES<- (MOUSE-SPECIFY-RECTANGLE))
	 (WINDOW-SELECT WINDOW)
	 (<- WINDOW ':MAKE-CONNECTION HOST)
  	 WINDOW)))
  
;; Now update the window creation menu
(<- WINDOW-CREATION-MENU ':ADD-AN-ITEM
    '("Com-link" :FUNCALL CREATE-COM-LINK-WINDOW-WITH-RESHAPING))

;; Need a better way to build this menu.
(COND ((NOT (BOUNDP 'CADR-MENU))
       (SETQ CADR-MENU (<- SI:MOMENTARY-MENU-CLASS ':NEW ':ITEM-LIST
                                                    '("Cadr1" "Cadr2" "Cadr3" "Cadr4"
						      "Cadr5" "Cadr6" "Cadr7" "Cadr8"
						      "Cadr9" "Cadr10")
						    ':LABEL " Choose any CADR "))
       (<- CADR-MENU ':COMPUTE-GEOMETRY)))

(DEFUN :COM-LINK (IGNORE)
  (PRINT "Look on your window creation menu... "))

;; Add the server to the Chaos server alist.
(ADD-INITIALIZATION "Com-Link"
		    '(PROCESS-RUN-FUNCTION "Com-Link" (FUNCTION RECEIVE-COM-LINK))
		    NIL
		    'CHAOS:SERVER-ALIST)

;;Stick this in untill the one in the system is updated (26.x)

;Assign the frame a new list of panes.
;At the moment, nothing checks that they don't overlap.
;For that matter, when you change their edges, nothing checks that
;or checks that you haven't moved them outside the frame!
;Something will be done about that.
(defmethod (window-frame-class :panes<-) (new-panes &aux pane-edges)
  (maybe-lock-screen-layout status
    (or (and left top right bottom)
	(ferror nil "Attempt to assign panes to ~S before edges" self))
    (dolist (pane new-panes)
      (setq pane-edges (<- pane ':edges))
      (or (and ( (+ left (first margins)) (first pane-edges))
	       ( (+ top (second margins)) (second pane-edges))
	       ( (third pane-edges) (- right (third margins)))
	       ( (fourth pane-edges) (- bottom (fourth margins))))
	  (ferror nil "~S doesn't fit within ~S" pane self)))
    (and selected-pane (eq status ':selected)
	 (<- selected-pane ':deselect))
    (and active-flag (funcall self ':revoke-run-reason))
    (dolist (pane panes)
      (<- pane ':frame<- nil)
      ;Don't deexpose unless necessary becuase screen layout  may not be locked.
      ;Assumption:  if the frame is not exposed, niether are the panes.
      ;Otherwise we get an extraneous "screen layout not locked" message.
      (and (<- pane ':status) (<- pane ':deexpose)))
    (setq panes new-panes)
    (and status (funcall self ':clean))
    (dolist (pane panes)
      (<- pane ':deactivate)
      (<- pane ':frame<- self)
      (and status (<- pane ':expose)))
    (and active-flag (funcall self ':run-reason))
    (setq selected-pane nil)))
