;;; -*- Mode: LISP;  Package: TV;  Base: 8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; PEEK -- displays status information about the Lisp Machine

(DEFFLAVOR BASIC-PEEK ((NEEDS-REDISPLAY NIL) (MODE-ALIST))
   (SCROLL-MOUSE-MIXIN SCROLL-WINDOW-WITH-TYPEOUT)
  :SETTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :SAVE-BITS T
    		       :LABEL "Peek"
		       :TRUNCATION T)
  (:DOCUMENTATION :SPECIAL-PURPOSE "The actual peek window"))

(DEFMETHOD (BASIC-PEEK :NAME-FOR-SELECTION) ()
  (STRING-APPEND "Peek: " (LABEL-STRING LABEL)))

(DEFFLAVOR PEEK () (PROCESS-MIXIN BASIC-PEEK)
  (:DOCUMENTATION :COMBINATION "Peek window with a process"))

(DEFMETHOD (PEEK :BEFORE :INIT) (IGNORE)
  (OR PROCESS (SETQ PROCESS '(PEEK-STANDALONE-TOP-LEVEL :SPECIAL-PDL-SIZE 4000
							:REGULAR-PDL-SIZE 10000))))

(COMPILE-FLAVOR-METHODS BASIC-PEEK PEEK)

(DECLARE (SPECIAL VALUE-ARRAY))

(DEFVAR PEEK-DEFAULT-MODE-ALIST '(
        (#/P PEEK-PROCESSES "Active Processes" NIL)
	(#/M PEEK-MEMORY-USAGE "Memory usage by area" NIL)
	(#/C PEEK-CHAOS "Chaosnet Connections" NIL)
	(#/A PEEK-AREAS "Areas" NIL)
	(#/H PEEK-HOSTAT "Hostat" T)
	(#/% PEEK-COUNTERS "Statistics Counters" NIL)
	(#/F PEEK-FILE-SYSTEM "File System Status" NIL)
	(#/W PEEK-WINDOW-HIERARCHY "Window hierarchy" NIL)))
(DEFVAR PEEK-SLEEP-TIME 120.)

(DEFUN PEEK-SET-MODE (WINDOW MODE &REST ARGS)
  (COND ((SETQ MODE (ASSOC MODE (FUNCALL WINDOW ':MODE-ALIST)))
	 (IF (FOURTH MODE)
	     (LEXPR-FUNCALL (SECOND MODE) WINDOW ARGS)
	     (PEEK-ASSURE-NO-TYPEOUT WINDOW)
	     (FUNCALL WINDOW ':SET-LABEL (THIRD MODE))
	     (FUNCALL WINDOW ':SET-DISPLAY-ITEM (LEXPR-FUNCALL (SECOND MODE) ARGS)))
	 T)
	(T NIL)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-PEEK)
(DEFUN PEEK-MOUSE-CLICK (ITEM LEADER-TO-COMPLEMENT)
  (SETQ NEEDS-REDISPLAY T)
  (SETF (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET LEADER-TO-COMPLEMENT))
	(NOT (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET LEADER-TO-COMPLEMENT))))))

(DEFUN PEEK-STANDALONE-TOP-LEVEL (WINDOW)
  (*CATCH 'SI:TOP-LEVEL
    (PROGN (FUNCALL WINDOW ':SET-MODE-ALIST PEEK-DEFAULT-MODE-ALIST)
	   (PEEK-TOP-LEVEL WINDOW "?")
	   (DO () (())
	     (FUNCALL WINDOW ':DESELECT T)
	     (PEEK-TOP-LEVEL WINDOW NIL))))
  (FUNCALL WINDOW ':BURY))

(DEFUN PEEK (&OPTIONAL (INITIAL-MODE "P") (WINDOW TERMINAL-IO))
  "The peek function itself -- window pushes terminal-io, and starts displaying
status information."
  (WITH-RESOURCE (BIT-ARRAYS BIT-ARRAY)
    (WINDOW-BIND (WINDOW 'BASIC-PEEK ':BIT-ARRAY BIT-ARRAY)
      (FUNCALL WINDOW ':SET-MODE-ALIST PEEK-DEFAULT-MODE-ALIST)
      (*CATCH 'SI:TOP-LEVEL (PEEK-TOP-LEVEL WINDOW INITIAL-MODE)))))

(DEFUN PEEK-TOP-LEVEL (WINDOW MODE)
  (COND-EVERY
    ((AND MODE (SYMBOLP MODE)) (SETQ MODE (GET-PNAME MODE)))
    ((STRINGP MODE) (SETQ MODE (AREF MODE 0)))
    ((NUMBERP MODE) (FUNCALL WINDOW ':FORCE-KBD-INPUT MODE)))
  (*CATCH 'SI:TOP-LEVEL
    (DO ((SLEEP-TIME PEEK-SLEEP-TIME)
	 (WAKEUP-TIME (TIME-DIFFERENCE (TIME) (- PEEK-SLEEP-TIME)))
	 (TERMINAL-IO (FUNCALL WINDOW ':TYPEOUT-WINDOW))
	 (ARG)
	 (CHAR))
	(())
      (AND (TIME-LESSP WAKEUP-TIME (TIME))
	   (SETQ WAKEUP-TIME (TIME-DIFFERENCE (TIME) (- SLEEP-TIME))))
      (OR (= SLEEP-TIME 0)
	  (PROCESS-WAIT "Peek Timeout or TYI"
			#'(LAMBDA (TIME FLAG-LOC STREAM)
			    (OR (TIME-LESSP TIME (TIME))
				(CAR FLAG-LOC)
				(FUNCALL STREAM ':LISTEN)))
			WAKEUP-TIME
			(LOCATE-IN-INSTANCE WINDOW 'NEEDS-REDISPLAY)
			TERMINAL-IO))
      (DO ()
	  ((PROGN (PEEK-ASSURE-NO-TYPEOUT WINDOW)
		  (NULL (SETQ CHAR (FUNCALL TERMINAL-IO ':TYI-NO-HANG)))))
	(COND ((NUMBERP CHAR)
	       ;; Standard character, either accumulate arg or select new mode
	       (SETQ CHAR (CHAR-UPCASE CHAR))
	       (IF (OR (< CHAR #/0) (> CHAR #/9))
		   (COND ((PEEK-SET-MODE WINDOW CHAR ARG)
			  (SETQ ARG NIL))
			 (T
			  ;; Check for standard character assignments
			  (SELECTQ CHAR
			    ((#\HELP #/?)
			     (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
			     (FORMAT T "Peek modes:~%~%")
			     (DOLIST (E (FUNCALL WINDOW ':MODE-ALIST))
			       (FORMAT T "~:@C~20T~A~%" (FIRST E) (THIRD E)))
			     (FORMAT T "Q~20TQuit~%")
			     (FORMAT T "nZ~20TSets sleep time between updates~%")
			     (FORMAT T "?~20TPrints this message~%")
			     (SETQ ARG NIL))
			    (#/Q
			     (*THROW 'SI:TOP-LEVEL NIL))
			    (#/Z
			     (AND ARG (SETQ SLEEP-TIME ARG))
			     (SETQ ARG NIL))
			    (OTHERWISE (BEEP)))))
		   (OR ARG (SETQ ARG 0))
		   (SETQ ARG (+ (* 10. ARG) (- CHAR #/0)))))
	      ((LISTP CHAR)
	       ;; A special command (forced input, no doubt)
	       (SELECTQ (CAR CHAR)
		 (SUPDUP (SUPDUP (CADR CHAR)))
		 (TELNET (TELNET (CADR CHAR)))
		 (QSEND
		  (CHAOS:SEND-MSG (CADR CHAR))
		  (FUNCALL WINDOW ':SET-NEEDS-REDISPLAY T)
		  (FUNCALL TERMINAL-IO ':MAKE-COMPLETE))
		 (EH (EH (CADR CHAR)))
		 (OTHERWISE (BEEP)))
	       (SETQ ARG NIL))))
      (COND ((OR (FUNCALL WINDOW ':NEEDS-REDISPLAY) (TIME-LESSP WAKEUP-TIME (TIME)))
	     ;; We want to redisplay.  If have typeout, hang until user confirms.
	     (FUNCALL WINDOW ':SET-NEEDS-REDISPLAY NIL)
	     (FUNCALL WINDOW ':REDISPLAY))))))

(DEFUN PEEK-ASSURE-NO-TYPEOUT (WINDOW)
  (COND ((FUNCALL (SETQ WINDOW (FUNCALL WINDOW ':TYPEOUT-WINDOW)) ':INCOMPLETE-P)
	 (FORMAT T "~&Type any character to flush:")
	 (LET ((CHAR (FUNCALL TERMINAL-IO ':TYI)))
	   (FUNCALL WINDOW ':MAKE-COMPLETE)
	   (OR (= CHAR #\SPACE) (FUNCALL TERMINAL-IO ':UNTYI CHAR))))))

(DEFUN PEEK-HOSTAT (&REST IGNORE)
  (CHAOS:HOSTAT))

;;; Processes, meters
(DEFUN PEEK-PROCESSES (IGNORE)
  "Shows state of all active processes."
  (LIST ()
	(SCROLL-PARSE-ITEM (FORMAT NIL "~30A~A" "Process Name" "State"))
	(SCROLL-PARSE-ITEM "")
	(SCROLL-MAINTAIN-LIST #'(LAMBDA () ACTIVE-PROCESSES)
			      #'(LAMBDA (PROCESS)
				  (AND PROCESS
				       (SCROLL-PARSE-ITEM
					 `(:MOUSE-ITEM
					    (NIL :EVAL (PEEK-PROCESS-MENU ',PROCESS 'ITEM 0))
					    :STRING ,(PROCESS-NAME PROCESS) 30.)
					 `(:FUNCTION ,#'PROCESS-WHOSTATE
						     ,(NCONS PROCESS)))))
			      NIL
			      #'(LAMBDA (STATE)
				  (PROG ()
				    (RETURN (CAAR STATE) (CDR STATE) (NULL (CDR STATE))))))
	(SCROLL-PARSE-ITEM "")
	(SCROLL-PARSE-ITEM "Clock Function List")
	(SCROLL-MAINTAIN-LIST #'(LAMBDA () SI:CLOCK-FUNCTION-LIST)
			      #'(LAMBDA (FUNC)
				  (SCROLL-PARSE-ITEM `(:STRING ,(GET-PNAME FUNC)))))))

(DEFUN PEEK-COUNTERS (IGNORE)
  "Statistics counters"
  (LIST ()
    (SCROLL-MAINTAIN-LIST #'(LAMBDA () SYS:A-MEMORY-COUNTER-BLOCK-NAMES)
			  #'(LAMBDA (COUNTER)
			      (SCROLL-PARSE-ITEM
				`(:STRING ,(STRING COUNTER) 35.)
				`(:FUNCTION READ-METER (,COUNTER) NIL ("~@15A" 10. T)))))))

;;; Memory

(DEFUN PEEK-MEMORY-HEADER ()
  (SCROLL-PARSE-ITEM
      "Physical memory: "
      `(:FUNCTION ,#'(LAMBDA (&AUX (VAL (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)))
		       (SETF (VALUE 0) (// VAL 2000))
		       VAL)
		  NIL NIL (NIL 8.))
      `(:VALUE 0 NIL (" (~DK), "))
      "Free space: "
      `(:FUNCTION ,#'(LAMBDA (&AUX (VAL (SI:ROOM-GET-AREA-LENGTH-USED FREE-AREA)))
		       (SETF (VALUE 0) (// VAL 2000))
		       VAL)
		  NIL NIL (NIL 8.))
      `(:VALUE 0 NIL (" (~DK)"))
      ", Wired pages "
      `(:FUNCTION ,#'(LAMBDA ()
		       (MULTIPLE-VALUE-BIND (N-WIRED-PAGES N-FIXED-WIRED-PAGES)
			   (SI:COUNT-WIRED-PAGES)
			 (SETF (VALUE 0) (- N-WIRED-PAGES N-FIXED-WIRED-PAGES))
			 (SETF (VALUE 1) (// N-WIRED-PAGES (// 2000 PAGE-SIZE)))
			 (SETF (VALUE 2) (\ N-WIRED-PAGES (// 2000 PAGE-SIZE)))
			 N-FIXED-WIRED-PAGES))
		  NIL NIL ("~D"))
      `(:VALUE 0 NIL ("+~D "))
      `(:VALUE 1 NIL ("(~D"))
      `(:VALUE 2 NIL ("~[~;.25~;.5~;.75~]K)"))))

(DEFUN PEEK-MEMORY-USAGE (IGNORE)
  "Memory usage by area."
  (LIST ()
    (PEEK-MEMORY-HEADER)
    (SCROLL-PARSE-ITEM "")
    (SCROLL-MAINTAIN-LIST #'(LAMBDA () 0)
			  #'(LAMBDA (IDX)
			      (SCROLL-PARSE-ITEM
				`(:STRING ,(STRING (AREA-NAME IDX)) 40.)
				`(:FUNCTION SI:ROOM-GET-AREA-LENGTH-USED (,IDX)
					    NIL ("~@15A" 10. T))))
			  NIL
			  #'(LAMBDA (STATE)
			      (PROG (NEXT-ONE THIS-ONE
				     (LEN (ARRAY-LENGTH #'AREA-NAME)))
				(DO ((I STATE (1+ I)))
				    (( I LEN) NIL)
				  (COND ((AND (NULL THIS-ONE) (AREF #'AREA-NAME I))
					 (SETQ THIS-ONE I))
					((AND THIS-ONE (AREF #'AREA-NAME I))
					 (SETQ NEXT-ONE I)
					 (RETURN T))))
				(RETURN THIS-ONE NEXT-ONE (NULL NEXT-ONE)))))))

(DEFUN PEEK-AREAS (IGNORE)
  "Areas"
  (LIST ()
    (PEEK-MEMORY-HEADER)
    (SCROLL-PARSE-ITEM "")
    (SCROLL-MAINTAIN-LIST
      #'(LAMBDA () 0)
      #'(LAMBDA (AREA)
	  (LIST '(:PRE-PROCESS-FUNCTION PEEK-AREAS-REGION-DISPLAY)
	    (SCROLL-PARSE-ITEM
	      ':MOUSE-SELF '(NIL :EVAL (PEEK-MOUSE-CLICK 'SELF 0))
	      ':LEADER `(NIL ,AREA)
	      `(:STRING ,(STRING (AREA-NAME AREA)) 40.)
	      `(:FUNCTION ,#'(LAMBDA (AREA)
			       (MULTIPLE-VALUE-BIND (LENGTH USED N-REGIONS)
				   (SI:ROOM-GET-AREA-LENGTH-USED AREA)
				 (SETF (VALUE 0) USED)
				 (SETF (VALUE 1) LENGTH)
				 (SETF (VALUE 2)
				       (COND ((ZEROP LENGTH) 0)
					     ((< LENGTH 40000)
					      (// (* 100. (- LENGTH USED)) LENGTH))
					     (T
					      (// (- LENGTH USED) (// LENGTH 100.)))))
				 N-REGIONS))
			  (,AREA) 15. ("(~D region~0G~P)"))
	      `(:VALUE 2 NIL ("~@3A% free, " 10. T))
	      `(:VALUE 0 NIL ("~O"))
	      `(:VALUE 1 NIL ("//~O used")))))
      NIL
      #'(LAMBDA (STATE)
	  (PROG (NEXT-ONE THIS-ONE
		  (LEN (ARRAY-LENGTH #'AREA-NAME)))
	    (DO ((I STATE (1+ I)))
		(( I LEN) NIL)
	      (COND ((AND (NULL THIS-ONE) (AREF #'AREA-NAME I))
		     (SETQ THIS-ONE I))
		    ((AND THIS-ONE (AREF #'AREA-NAME I))
		     (SETQ NEXT-ONE I)
		     (RETURN T))))
	    (RETURN THIS-ONE NEXT-ONE (NULL NEXT-ONE)))))))

(DEFUN PEEK-AREAS-REGION-DISPLAY (ITEM)
  "Handles adding/deleting of the region display when a mouse button is clicked."
  (COND ((NULL (ARRAY-LEADER (CADR ITEM) SCROLL-ITEM-LEADER-OFFSET)))
	 ;; Clicked on this item, need to complement state
	((= (LENGTH ITEM) 2)
	 ;; If aren't displaying regions now, display them
	 (RPLACD (CDR ITEM)
		 (NCONS
		   (SCROLL-MAINTAIN-LIST
		   `(LAMBDA ()
		      (AREA-REGION-LIST (ARRAY-LEADER ',(FIRST (SCROLL-ITEMS ITEM))
						      (1+ SCROLL-ITEM-LEADER-OFFSET))))
		   #'(LAMBDA (REGION)
		       (SCROLL-PARSE-ITEM
			`(:STRING
			  ,(FORMAT NIL "  #~O: Origin ~O, Length ~O, "
				       REGION (REGION-ORIGIN REGION) (REGION-LENGTH REGION)))
			`(:FUNCTION ,#'REGION-FREE-POINTER (,REGION) NIL ("Used ~O, "))
			`(:FUNCTION ,#'REGION-GC-POINTER (,REGION) NIL ("GC ~O, "))
			`(:FUNCTION ,#'(LAMBDA (REGION &AUX BITS)
					(SETQ BITS (REGION-BITS REGION))
					(SETF (VALUE 0)
					      (NTH (LDB %%REGION-SPACE-TYPE BITS)
						   '(FREE OLD NEW STATIC FIXED EXITED
						     EXIT EXTRA-PDL WIRED MAPPED COPY
						     "TYPE=13" "TYPE=14" "TYPE=15"
						     "TYPE=16" "TYPE=17")))
					(SETF (VALUE 1) (LDB %%REGION-MAP-BITS BITS))
					(SETF (VALUE 2) (LDB %%REGION-SCAVENGE-ENABLE BITS))
					(NTH (LDB %%REGION-REPRESENTATION-TYPE BITS)
					     '(LIST STRUC "REP=2" "REP=3")))
				    (,REGION) NIL ("Type ~A "))
			`(:VALUE 0 NIL ("~A, "))
			`(:VALUE 1 NIL ("Map ~O, "))
			`(:VALUE 2 NIL ("~[NoScav~;Scav~]"))))
		   NIL
		   #'(LAMBDA (STATE)
		       (PROG ()
			 (RETURN STATE (REGION-LIST-THREAD STATE)
				 (MINUSP (REGION-LIST-THREAD STATE)))))))))
	(T (RPLACD (CDR ITEM) NIL)))
  (SETF (ARRAY-LEADER (CADR ITEM) SCROLL-ITEM-LEADER-OFFSET) NIL))

;;; Chaos stuff

(DEFUN PEEK-CHAOS-PACKET-ITEM (PKT &OPTIONAL (INDENT 0))
  "Returns an item that describes a chaosnet packet.  Mouseable subfields are:
   The host:  Left: Causes info about the host to displayed inferior to the packet.
	      Middle: Causes a static hostat to be displayed inferior to the packet.
  	      Right (menu): Typeout Hostat, Supdup, Telnet, Qsend

Sample output:
Pkt [to ! from] <name> (number){, transmitted <n> times (at <time>)}{, being retransmitted}{, released}{, fowarded <n> times}
    <op> (<number>), <n> bytes, number <n>, acking <n>, source idx <n>, dest idx <n>
    Words from <n>: <wordn> ... <wordn+m>
    String: <string>

Packet: to AI (2026), transmitted 27 times (at 1231232), being retransmitted
 CLS (11), 432 bytes, number 3422, acking 3221, source idx 177777, dest idx 177777
 Words from 0: 123123 12371 1227 272727 272626
 String: 'Now is the time for all good men'

Packet: from MC (1440), released, forwarded 17 times
 DAT (201), 100 bytes, number 432, acking 102, source idx 123451, dest idx 123441
 Words from 0: 123123 64532
 String: 'FUKT!'

"
  (LET ((TO-US (AND (ZEROP (CHAOS:PKT-TIMES-TRANSMITTED PKT))
		    (= (CHAOS:PKT-DEST-ADDRESS PKT) CHAOS:MY-ADDRESS)))
	(OTHER-HOST))
    (SETQ OTHER-HOST (IF TO-US
			 (CHAOS:PKT-SOURCE-ADDRESS PKT)
			 (CHAOS:PKT-DEST-ADDRESS PKT)))
    (LIST ()
      (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-PACKET-INSERT-HOSTAT)
	(SCROLL-PARSE-ITEM
	  ':LEADER 4
	  `(:MOUSE-ITEM (NIL :EVAL (PEEK-CHAOS-HOST-MENU ',OTHER-HOST 'ITEM 0 ,INDENT))
	    :STRING ,(FORMAT NIL "~VXPacket ~:[to~;from~] ~A (~O)"
			     INDENT TO-US (CHAOS:HOST-DATA OTHER-HOST) OTHER-HOST))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'CHAOS:PKT-TIMES-TRANSMITTED (,PKT)
			   NIL (", transmitted ~D times")))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'CHAOS:PKT-TIME-TRANSMITTED (,PKT) NIL (" (at ~O)")))
	  (AND (NOT TO-US)
	       `(:FUNCTION ,#'CHAOS:PKT-BEING-RETRANSMITTED (,PKT)
			   NIL ("~:[, being retransmitted~;~]")))
	  `(:FUNCTION ,#'CHAOS:PKT-STATUS (,PKT) NIL ("~:[~;, Status: ~0G~A~]"))
	  (AND TO-US
	       (FORMAT NIL ", fowarded ~D times" (CHAOS:PKT-FWD-COUNT PKT)))))

      ;; Second line
      (LET ((OP (CHAOS:PKT-OPCODE PKT)))
       (SCROLL-PARSE-ITEM
	(FORMAT NIL
		"~VX~A (~O), ~O bytes, number ~O, acking ~O, source idx ~O, dest idx ~O"
		INDENT
		(IF ( OP CHAOS:DAT-OP)
		    "Data"
		    (NTH OP CHAOS:OPCODE-LIST))
		OP
		(CHAOS:PKT-NBYTES PKT)
		(CHAOS:PKT-NUM PKT) (CHAOS:PKT-ACK-NUM PKT)
		(CHAOS:PKT-SOURCE-INDEX-NUM PKT) (CHAOS:PKT-DEST-INDEX-NUM PKT))))
      (SCROLL-PARSE-ITEM (FORMAT NIL "~VX" INDENT) (PEEK-CHAOS-PKT-WORDS PKT 0 6))
      (SCROLL-PARSE-ITEM (FORMAT NIL "~VXString: " INDENT) (PEEK-CHAOS-PKT-STRING PKT)))))

(DEFUN PEEK-CHAOS-PKT-WORDS (PKT START NUMBER &AUX STRING)
  "Returns a string consisting of words from the packet."
  (SETQ STRING (FORMAT NIL "Words from ~O: " START))
  (DO ((I START (1+ I))
       (LEN (ARRAY-DIMENSION-N 1 PKT)))
      ((OR ( I (+ START NUMBER)) ( I LEN))
       STRING)
    (SETQ STRING
	  (STRING-APPEND STRING
			 (FORMAT NIL "~6O" (AREF PKT (+ CHAOS:FIRST-DATA-WORD-IN-PKT I)))
			 " "))))

;;; Boy, is this piece of shit ad hoc!!
(DEFUN PEEK-CHAOS-PKT-STRING (PKT &OPTIONAL COUNT)
  "Returns a 'safe' string as far as the scrolling stuff is concerned"
  (DO ((STRING (MAKE-ARRAY NIL 'ART-STRING 100 NIL '(0)))
       (PKT-STRING (CHAOS:PKT-STRING PKT))
       (CHAR)
       (I 0 (1+ I))
       (LEN (STRING-LENGTH (CHAOS:PKT-STRING PKT))))
      ((OR ( I LEN) (AND COUNT ( I COUNT)))
       STRING)
      (SETQ CHAR (AREF PKT-STRING I))
      (IF (AND (< CHAR 200) ( CHAR #/))
	  (ARRAY-PUSH-EXTEND STRING CHAR)
	  (ARRAY-PUSH-EXTEND STRING #/)
	  (IF ( CHAR #/)
	      (ARRAY-PUSH-EXTEND STRING (LOGIOR 100 (LOGAND CHAR 77)))
	      (ARRAY-PUSH-EXTEND STRING #/)))))

(DEFUN PEEK-CHAOS-CONN (CONN)
  "Format is:

Host <name> (<number>), <state>, local idx <n>, foreign idx <n>
Windows: local <n>, foreign <n> (<n> available)
Received: pkt <n> (time <n>), read pkt <n>, ack pkt <n>, <n> queued
Sent: pkt <n>, ack for pkt <n>, <n> queued
"
  (LIST ()
    (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-INSERT-HOSTAT)
	  (SCROLL-PARSE-ITEM
	    ':LEADER 3
	    (LOCAL-DECLARE ((SPECIAL PEEK-CHAOS-HOST))
	      (LET ((PEEK-CHAOS-HOST (CONS -1 NIL)))
		`(:MOUSE-ITEM
		   (NIL :EVAL (PEEK-CHAOS-HOST-MENU ',(LOCF (CAR PEEK-CHAOS-HOST)) 'ITEM 0))
		  :FUNCTION ,(CLOSURE '(PEEK-CHAOS-HOST)
			       #'(LAMBDA (CONN)
				   (AND ( (CAR PEEK-CHAOS-HOST)
					   (PROG2 (RPLACA PEEK-CHAOS-HOST
							  (CHAOS:FOREIGN-ADDRESS CONN))
						  (CAR PEEK-CHAOS-HOST)))
					(RPLACD PEEK-CHAOS-HOST
						(FORMAT NIL "Host ~A (~O), "
						   (CHAOS:HOST-DATA (CAR PEEK-CHAOS-HOST))
						   (CAR PEEK-CHAOS-HOST))))
				   (CDR PEEK-CHAOS-HOST)))
		  (,CONN) NIL)))
	    `(:FUNCTION ,#'CHAOS:STATE (,CONN) NIL)
	    `(:FUNCTION ,#'CHAOS:LOCAL-INDEX-NUM (,CONN) NIL (", local idx ~O, "))
	    `(:FUNCTION ,#'CHAOS:FOREIGN-INDEX-NUM (,CONN) NIL ("foreign idx ~O"))))
    (SCROLL-PARSE-ITEM
      `(:FUNCTION ,#'CHAOS:LOCAL-WINDOW-SIZE (,CONN) NIL ("Windows: local ~D, "))
      `(:FUNCTION ,#'CHAOS:FOREIGN-WINDOW-SIZE (,CONN) NIL ("foreign ~D, "))
      `(:FUNCTION ,#'CHAOS:WINDOW-AVAILABLE (,CONN) NIL ("(~D available)")))
    (LIST `(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-RECEIVED-PKTS :CONNECTION ,CONN)
      (SCROLL-PARSE-ITEM
	':LEADER 1
	':MOUSE-SELF '(NIL :EVAL (PEEK-MOUSE-CLICK 'SELF 0))
	`(:FUNCTION ,#'CHAOS:PKT-NUM-RECEIVED (,CONN) NIL ("Received: pkt ~O"))
	`(:FUNCTION ,#'CHAOS:TIME-LAST-RECEIVED (,CONN) NIL (" (time ~O), "))
	`(:FUNCTION ,#'CHAOS:PKT-NUM-READ (,CONN) NIL ("read pkt ~O, "))
	`(:FUNCTION ,#'CHAOS:PKT-NUM-ACKED (,CONN) NIL ("ack pkt ~O, "))
	`(:FUNCTION ,#'(LAMBDA (CONN)
			 (- (CHAOS:PKT-NUM-RECEIVED CONN) (CHAOS:PKT-NUM-READ CONN)))
		    (,CONN) NIL ("~D queued"))))
    (LIST `(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-SEND-PKTS :CONNECTION ,CONN)
      (SCROLL-PARSE-ITEM
	':LEADER 1
	':MOUSE-SELF '(NIL :EVAL (PEEK-MOUSE-CLICK 'SELF 0))
	`(:FUNCTION ,#'CHAOS:PKT-NUM-SENT (,CONN) NIL ("Sent: pkt ~O, "))
	`(:FUNCTION ,#'CHAOS:SEND-PKT-ACKED (,CONN) NIL ("ack for pkt ~O, "))
	`(:FUNCTION ,#'CHAOS:SEND-PKTS-LENGTH (,CONN) NIL ("~D queued"))))
    (SCROLL-PARSE-ITEM "")))

(DEFUN PEEK-CHAOS (IGNORE)
  "Displays state of all chaos net connections"
  (LIST NIL
	(SCROLL-PARSE-ITEM
	  "Chaos connections at "
	  `(:FUNCTION ,#'TIME () NIL ("~O")))
	(SCROLL-PARSE-ITEM "")
	(SCROLL-MAINTAIN-LIST #'(LAMBDA () CHAOS:CONN-LIST)
			      #'PEEK-CHAOS-CONN)
	(SCROLL-PARSE-ITEM "Interesting meters")
	(SCROLL-MAINTAIN-LIST #'(LAMBDA () CHAOS:PEEK-A-BOO-LIST)
			      #'(LAMBDA (COUNTER)
				  (SCROLL-PARSE-ITEM
				    `(:STRING ,(STRING COUNTER) 35.)
				    `(:FUNCTION SYMEVAL (,COUNTER) NIL ("~@15A" 10. T)))))
	(SCROLL-PARSE-ITEM '(:STRING "%COUNT-CHAOS-TRANSMIT-ABORTS" 35.)
			   '(:FUNCTION READ-METER (SYS:%COUNT-CHAOS-TRANSMIT-ABORTS) NIL
				       ("~@15A" 10. T)))))

(DEFUN PEEK-CHAOS-HOST-MENU (&REST ARGS)
  (LEXPR-FUNCALL #'PROCESS-RUN-FUNCTION "Peek Chaos Menu"
		 SELF ':FUNCALL-INSIDE-YOURSELF #'PEEK-CHAOS-HOST-MENU-INTERNAL ARGS))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-PEEK)
(DEFUN PEEK-CHAOS-HOST-MENU-INTERNAL (HOST ITEM &OPTIONAL (OFFSET 0) &REST ADDITIONAL-STUFF)
  "Menu for interesting operations on hosts in a peek chaos display"
  (OR (NUMBERP HOST) (SETQ HOST (CAR HOST)))
  (LET ((CHOICE (MENU-CHOOSE '(("Hostat One" . HOSTAT-ONE)
			       ("Hostat All" . HOSTAT-ALL)
			       ("Insert Hostat" . HOSTAT-INSERT)
			       ("Remove Hostat" . HOSTAT-REMOVE)
			       ("Supdup" . HOSTAT-SUPDUP)
			       ("Telnet" . HOSTAT-TELNET)
			       ("Qsend" . HOSTAT-QSEND))))
	(TERMINAL-IO TYPEOUT-WINDOW))
    (SELECTQ CHOICE
      (HOSTAT-ONE (HOSTAT HOST))
      (HOSTAT-ALL (HOSTAT))
      ((HOSTAT-INSERT HOSTAT-REMOVE)
       (SETF (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET OFFSET))
	     (EQ CHOICE 'HOSTAT-INSERT))
       (SETF (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET OFFSET 1)) HOST)
       (DOTIMES (I (LENGTH ADDITIONAL-STUFF))
	 (SETF (ARRAY-LEADER ITEM (+ SCROLL-ITEM-LEADER-OFFSET OFFSET I 2))
	       (NTH I ADDITIONAL-STUFF)))
       (SETQ NEEDS-REDISPLAY T))
      (HOSTAT-SUPDUP (FUNCALL-SELF ':FORCE-KBD-INPUT `(SUPDUP ,HOST)))
      (HOSTAT-TELNET (FUNCALL-SELF ':FORCE-KBD-INPUT `(TELNET ,HOST)))
      (HOSTAT-QSEND (FUNCALL-SELF ':FORCE-KBD-INPUT `(QSEND ,HOST)))
      (NIL)
      (OTHERWISE (BEEP))))))

(DEFUN PEEK-CHAOS-CONN-INSERT-HOSTAT (ITEM &AUX HOST)
  "A pre-process function to insert/remove a hostat from the display."
  (COND ((ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM)) SCROLL-ITEM-LEADER-OFFSET)
	 ;; Want a hostat, make sure it's there and for the right host
	 (IF (AND (EQ (SETQ HOST (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM))
					       (1+ SCROLL-ITEM-LEADER-OFFSET)))
		      (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM))
				    (+ SCROLL-ITEM-LEADER-OFFSET 2)))
		  (CDDR ITEM))
	     NIL
	     (RPLACD (CDR ITEM)
		     (PEEK-CHAOS-HOSTAT HOST 1))
	     (SETF (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM))
				 (+ SCROLL-ITEM-LEADER-OFFSET 2)) HOST)))
	(T (RPLACD (CDR ITEM) NIL)
	   (SETF (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM))
			       (+ SCROLL-ITEM-LEADER-OFFSET 2)) NIL))))

(DEFUN PEEK-CHAOS-PACKET-INSERT-HOSTAT (ITEM &AUX HOST SI)
  "A pre-process function to insert/remove a hostat from the display."
  (COND ((ARRAY-LEADER (SETQ SI (FIRST (SCROLL-ITEMS ITEM))) SCROLL-ITEM-LEADER-OFFSET)
	 ;; Want a hostat, make sure it's there and for the right host
	 (IF (AND (EQ (SETQ HOST (ARRAY-LEADER SI (+ 2 SCROLL-ITEM-LEADER-OFFSET)))
		      (ARRAY-LEADER SI (+ SCROLL-ITEM-LEADER-OFFSET 3)))
		  (CDDR ITEM))
	     NIL
	     (RPLACD (CDR ITEM)
		     (PEEK-CHAOS-HOSTAT HOST
					(1+ (ARRAY-LEADER SI
							  (1+ SCROLL-ITEM-LEADER-OFFSET)))))
	     (SETF (ARRAY-LEADER SI (+ SCROLL-ITEM-LEADER-OFFSET 3)) HOST)))
	(T (RPLACD (CDR ITEM) NIL)
	   (SETF (ARRAY-LEADER SI (+ SCROLL-ITEM-LEADER-OFFSET 3)) NIL))))

(DEFUN PEEK-CHAOS-HOSTAT (HOST INDENT &OPTIONAL PKT)
  (COND ((OR PKT (SETQ PKT (CHAOS:GET-HOST-STATUS-PACKET HOST)))
	 (LET ((NBYTES (CHAOS:PKT-NBYTES PKT))
	       (ITEM (LIST (FORMAT NIL
   "Site   Name//Status~40TSubnet #-in #-out abort  lost  crc ramerr bitc  other"))))
	   (PUSH (FORMAT NIL "~VX~O~7T~A" 
			 INDENT HOST
			 (NSUBSTRING (CHAOS:PKT-STRING PKT) 0
				     (MIN NBYTES 32.
					  (OR (STRING-SEARCH-CHAR 200 (CHAOS:PKT-STRING PKT))
					      32.))))
		 ITEM)
	   (DO ((I 24. (+ I 2 CT))			;Now display subnet meters
		(ID) (CT)
		(STRING "" "")
		(MAXI (+ 8 (// NBYTES 2))))
	       ((>= I MAXI))
	     (SETQ ID (AREF PKT I) CT (AREF PKT (1+ I)))	;Block header
	     (COND ((< ID 400)					;Subnet info
		    (SETQ STRING (FORMAT NIL "~VX~4O" (+ 40 INDENT) ID))
		    (DO ((J (+ I 2) (1+ J))	;Now print those meters that are present
			 (N (MIN CT 8) (1- N)))
			((ZEROP N))
		      (SETQ STRING (STRING-APPEND STRING (FORMAT NIL " ~5D" (AR-1 PKT J)))))
		    (PUSH STRING ITEM))
		   (T				;I don't know about this
		    (PUSH (FORMAT NIL "~40T~O unknown info block ID" ID) ITEM))))
	   (DO ((L (SETQ ITEM (NREVERSE ITEM)) (CDR L)))
	       ((NULL L) (LIST* () ITEM))
	     (RPLACA L (SCROLL-PARSE-ITEM (CAR L))))))
	(T (NCONS (SCROLL-PARSE-ITEM "Host data unavailable")))))

(DEFUN PEEK-CHAOS-CONN-RECEIVED-PKTS (ITEM &OPTIONAL (INDENT 0) &AUX CONN)
  "Show/unshow the received pkts of the connection"
  (OR (SETQ CONN (GET (LOCF (SCROLL-FLAGS ITEM)) ':CONNECTION))
      (FERROR NIL "~S has no associated connection, can't display packets." ITEM))
  (COND ((NOT (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM)) SCROLL-ITEM-LEADER-OFFSET))
	 ;; Want to leave state alone
	 )
	((CDR (SCROLL-ITEMS ITEM))
	 ;; Remove display
	 (RPLACD (SCROLL-ITEMS ITEM) NIL))
	(T
	 ;; Add display
	 (RPLACD (SCROLL-ITEMS ITEM)
		 (NCONS
		   (SCROLL-MAINTAIN-LIST `(LAMBDA () ',(CHAOS:READ-PKTS CONN))
					 `(LAMBDA (X)
					    (PEEK-CHAOS-PACKET-ITEM X ,(+ INDENT 2)))
					 NIL
					 #'(LAMBDA (STATE)
					     (PROG ()
					       (RETURN STATE (CHAOS:PKT-LINK STATE)
						       (NULL (CHAOS:PKT-LINK STATE))))))))))
  (SETF (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM)) SCROLL-ITEM-LEADER-OFFSET) NIL))

(DEFUN PEEK-CHAOS-CONN-SEND-PKTS (ITEM &OPTIONAL (INDENT 0) &AUX CONN)
  "Show/unshow the send pkts of the connection"
  (OR (SETQ CONN (GET (LOCF (SCROLL-FLAGS ITEM)) ':CONNECTION))
      (FERROR NIL "~S has no associated connection, can't display packets." ITEM))
  (COND ((NOT (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM)) SCROLL-ITEM-LEADER-OFFSET))
	 ;; Want to leave state alone
	 )
	((CDR (SCROLL-ITEMS ITEM))
	 ;; Remove display
	 (RPLACD (SCROLL-ITEMS ITEM) NIL))
	(T
	 ;; Add display
	 (RPLACD (SCROLL-ITEMS ITEM)
		 (NCONS
		   (SCROLL-MAINTAIN-LIST `(LAMBDA () (CHAOS:SEND-PKTS ',CONN))
					 `(LAMBDA (X)
					    (PEEK-CHAOS-PACKET-ITEM X ,(+ INDENT 2)))
					 NIL
					 #'(LAMBDA (STATE)
					     (PROG ()
					       (RETURN STATE (CHAOS:PKT-LINK STATE)
						       (NULL (CHAOS:PKT-LINK STATE))))))))))
    (SETF (ARRAY-LEADER (FIRST (SCROLL-ITEMS ITEM)) SCROLL-ITEM-LEADER-OFFSET) NIL))

;;; File system status
(DEFUN PEEK-FILE-SYSTEM (IGNORE)
  "Display status of file system"
  (SCROLL-MAINTAIN-LIST 
    #'(LAMBDA () FS:FILE-HOST-ALIST)
    #'(LAMBDA (HOST)
	(LIST ()
	  (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-INSERT-HOSTAT)
	    (SCROLL-PARSE-ITEM
	      ':LEADER 3
	      `(:MOUSE-ITEM
		(NIL :EVAL (PEEK-CHAOS-HOST-MENU ,(CHAOS:ADDRESS-PARSE (CAR HOST)) 'ITEM 0))
		:STRING ,(FORMAT NIL "Host ~A" (CAR HOST)))))
	  (SCROLL-MAINTAIN-LIST
	    `(LAMBDA () ',(SYMEVAL-IN-CLOSURE (CADR HOST) 'FS:FILE-HOST-FIRST-UNIT))
	    #'PEEK-FILE-SYSTEM-HOST-UNIT
	    NIL
	    #'(LAMBDA (STATE)
		(PROG (NHU)
		      (RETURN STATE
			      (SETQ NHU (FS:HOST-UNIT-LINK STATE))
			      (NULL NHU)))))))))

(DEFUNP PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-CHANNEL (STATE &OPTIONAL DONT-STEP &AUX CHAN FLAG NS)
  "Returns new state and next channel.  If DONT-STEP is specified, returns the current
state if there is a channel available, else NIL"
  (SETQ FLAG (CDR STATE))
  (DO ((S (CAR STATE) (CDR S)))
      ((NULL S) (SETQ NS NIL))
    (SETQ NS S)
    (AND (NULL FLAG) (SETQ CHAN (FS:DATA-CHANNEL (CAR S) ':INPUT)) (RETURN (SETQ FLAG T)))
    (SETQ FLAG NIL)
    (AND (SETQ CHAN (FS:DATA-CHANNEL (CAR S) ':OUTPUT)) (RETURN (SETQ NS (CDR NS)))))
  (AND CHAN
       (RETURN (IF DONT-STEP
		   STATE
		   (RPLACA STATE NS)
		   (RPLACD STATE FLAG))
	       CHAN)))

(DEFUN PEEK-FILE-SYSTEM-HOST-UNIT (UNIT &OPTIONAL (INDENT 2))
  "Generate a scroll item describing a host unit"
  (LIST ()
    (SCROLL-PARSE-ITEM (FORMAT NIL "~VXHost unit ~A, control connection in " INDENT UNIT)
		       `(:FUNCTION ,#'(LAMBDA (UNIT)
					(LET ((CONN (FS:HOST-UNIT-CONTROL-CONNECTION UNIT)))
					  (COND (CONN (GET-PNAME (CHAOS:STATE CONN)))
						(T "NONEXISTANT-STATE"))))
				   (,UNIT)))
    (SCROLL-MAINTAIN-LIST `(LAMBDA () (PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-CHANNEL
					(NCONS (FS:HOST-UNIT-DATA-CONNECTIONS ',UNIT)) T))
			  `(LAMBDA (CHAN)
			     (PEEK-FILE-SYSTEM-CHANNEL CHAN (+ 2 ,INDENT)))
			  NIL
			  #'(LAMBDA (STATE)
			      (PROG (CHAN NS)
				(MULTIPLE-VALUE (NS CHAN)
				  (PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-CHANNEL STATE))
				(RETURN CHAN NS
					(NULL (PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-CHANNEL
						NS T))))))))

(DEFUN PEEK-FILE-SYSTEM-CHANNEL (CHAN &OPTIONAL (INDENT 0))
  "Returns a scroll item describing a channel"
  (SCROLL-PARSE-ITEM
    ':MOUSE `(NIL :EVAL (PEEK-FILE-SYSTEM-CHANNEL-MENU ',CHAN))
    (OR (= INDENT 0) (FORMAT NIL "~VX" INDENT))
    (SELECTQ (FS:CHANNEL-DIRECTION CHAN)
      (:INPUT "Input ")
      (:OUTPUT "Output ")
      (OTHERWISE "Direction? "))
    (FUNCALL (FS:CHANNEL-FILE-NAME CHAN) ':STRING-FOR-PRINTING)
    (SELECTQ (FS:CHANNEL-MODE CHAN)
      (:BINARY ", Binary, ")
      (:CHARACTER ", Character, ")
      (OTHERWISE ", unknown mode, "))
    `(:FUNCTION ,#'(LAMBDA (CHAN)
		     (SETF (VALUE 0)
			   (+ (FS:CHANNEL-FIRST-FILEPOS CHAN)
			      (- (FS:CHANNEL-FIRST-COUNT CHAN)
				 (FS:CHANNEL-DATA-COUNT CHAN))))
		     (VALUE 0))
		(,CHAN) NIL ("~D"))
    (AND (EQ (FS:CHANNEL-DIRECTION CHAN) ':INPUT)
	 `(:FUNCTION ,#'(LAMBDA (CHAN)
			  (LET ((LENGTH (FS:CHANNEL-PROPERTY-GET CHAN ':LENGTH)))
			    (AND LENGTH (NOT (ZEROP LENGTH))
				 (// (* 100. (VALUE 0)) LENGTH))))
		     (,CHAN) NIL ("~:[~;~0G (~D%)~]")))
    " bytes"))

(DEFUN PEEK-FILE-SYSTEM-CHANNEL-MENU (CHAN)
  (PROCESS-RUN-FUNCTION "Peek File System Menu"
			#'(LAMBDA (FS:FILE-CHANNEL)
			    (MENU-CHOOSE '(("Close" :EVAL FS:(FILE-CLOSE FILE-CHANNEL))
					   ("Delete" :EVAL
					    FS:(FILE-CHANNEL-OPERATIONS ':DELETE))
					   ("Flush" :EVAL
					    (PROGN FS:(FILE-CHANNEL-OPERATIONS ':DELETE)
						   FS:(FILE-CLOSE FILE-CHANNEL))))))
			CHAN))

(DEFUN PEEK-PROCESS-MENU (&REST ARGS)
  (LEXPR-FUNCALL #'PROCESS-RUN-FUNCTION "Peek Process Menu"
		 SELF ':FUNCALL-INSIDE-YOURSELF #'PEEK-PROCESS-MENU-INTERNAL ARGS))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BASIC-PEEK)
(DEFUN PEEK-PROCESS-MENU-INTERNAL (PROCESS &REST IGNORE &AUX CHOICE)
  "Menu for interesting operations on processes in a peek display"
  (LET ((TERMINAL-IO TYPEOUT-WINDOW))
    (SETQ CHOICE (MENU-CHOOSE '(("Arrest" . PROCESS-ARREST)
				("Un-Arrest" . PROCESS-UN-ARREST)
				("Flush" . PROCESS-FLUSH)
				("EH" . PROCESS-EH))))
    (SELECTQ CHOICE
      (PROCESS-ARREST (FUNCALL PROCESS ':ARREST-REASON))
      (PROCESS-UN-ARREST (FUNCALL PROCESS ':REVOKE-ARREST-REASON))
      (PROCESS-FLUSH (FUNCALL PROCESS ':FLUSH))
      (PROCESS-EH (FUNCALL-SELF ':FORCE-KBD-INPUT `(EH ,PROCESS)))
      (NIL)
      (OTHERWISE (BEEP))))))

(DEFUN PEEK-WINDOW-HIERARCHY (IGNORE)
  (SCROLL-MAINTAIN-LIST #'(LAMBDA () ALL-THE-SCREENS)
			#'(LAMBDA (SCREEN)
			    (LIST ()
			      (SCROLL-PARSE-ITEM (FORMAT NIL "Screen ~A" SCREEN))
			      (PEEK-WINDOW-INFERIORS SCREEN 2)
			      (SCROLL-PARSE-ITEM "")))))

(DEFUN PEEK-WINDOW-INFERIORS (WINDOW INDENT)
  (SCROLL-MAINTAIN-LIST `(LAMBDA () (SHEET-INFERIORS ',WINDOW))
			`(LAMBDA (SHEET)
			   (LIST ()
			     (SCROLL-PARSE-ITEM 
			       ':MOUSE `(NIL :EVAL (PEEK-WINDOW-MENU ',SHEET))
			       (FORMAT NIL "~VX~A" ,INDENT SHEET))
			     (PEEK-WINDOW-INFERIORS SHEET (+ ,INDENT 4))))))

(DEFUN PEEK-WINDOW-MENU (&REST ARGS)
  (LEXPR-FUNCALL #'PROCESS-RUN-FUNCTION "Peek Window Menu"
		 #'PEEK-WINDOW-MENU-INTERNAL ARGS))

(DEFUN PEEK-WINDOW-MENU-INTERNAL (SHEET &REST IGNORE &AUX CHOICE)
  "Menu for interesting operations on sheets in a peek display"
  (SETQ CHOICE (MENU-CHOOSE '(("Deexpose" . :DEEXPOSE)
			      ("Expose" . :EXPOSE)
			      ("Select" . :SELECT)
			      ("Deselect" . :DESELECT)
			      ("Deactivate" . :DEACTIVATE)
			      ("Kill" . :KILL)
			      ("Bury" . :BURY))))
  (AND CHOICE (FUNCALL SHEET CHOICE)))

(WINDOW-CREATE 'PEEK ':ACTIVATE-P T)  ;Pre-create one for the system key
