;;; -*- MODE: LISP; PACKAGE: SYSTEM-INTERNALS -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCLASS PEEK-WINDOW-CLASS SCROLL-TEXT-WINDOW-CLASS
	  (PEEK-MODE PEEK-MODE-CONTINUE-FUNCTION PEEK-MODE-DATA
		     PEEK-SLEEP-TIME PEEK-LAST-INPUT-TIME))

(DECLARE (SPECIAL PEEK-MODE-DATA PEEK-MODE-CONTINUE-FUNCTION PEEK-MODE-ALIST))
(SETQ PEEK-MODE-ALIST '((#/A PEEK-PROCESSES "Status of active processes")
			(#/H PEEK-HOSTAT "Chaosnet host status")
			(#/K PEEK-CHAOS "Chaosnet connection status
           nK shows all packets of connection n")
			(#/M PEEK-AREAS "Memory usage by area")
			(#/N PEEK-PROCESSES "Status of active processes")
			(#/F PEEK-FILE-SYSTEM-STATUS "Status of file system")
			(#/% PEEK-COUNTERS "Microcode event counters")
			(#/? PEEK-HELP "List of Peek commands")))
(ENDF HEAD)

(DECLARE (SPECIAL MAIN-PEEK-WINDOW))
(DEFUN PEEK (&OPTIONAL MODE)
    (OR (BOUNDP 'MAIN-PEEK-WINDOW)
	(<- (<- WINDOW-SINGLE-FRAME-CLASS ':NEW) ':PANE<-
	    (SETQ MAIN-PEEK-WINDOW (<- PEEK-WINDOW-CLASS ':NEW))))
    (AND MODE
	 (<- MAIN-PEEK-WINDOW ':FORCE-KBD-INPUT
	     (COND ((NUMBERP MODE) MODE)
		   (T (AR-1 (STRING MODE) 0)))))
    (WINDOW-SELECT MAIN-PEEK-WINDOW))

(DEFMETHOD (PEEK-WINDOW-CLASS :BORN) ()
    (SETQ SI:PROCESS '(:NEW PEEK-TOP-LEVEL :SPECIAL-PDL-SIZE 1000))
    (OR PEEK-MODE (SETQ PEEK-MODE #/?))
    (OR PEEK-SLEEP-TIME (SETQ PEEK-SLEEP-TIME 5))
    (SETQ PEEK-LAST-INPUT-TIME 0)
    (<-AS SCROLL-TEXT-WINDOW-CLASS ':BORN))

(DEFUN PEEK-TOP-LEVEL (WINDOW)
    (<- WINDOW ':TOP-LEVEL))

(DEFMETHOD (PEEK-WINDOW-CLASS :TOP-LEVEL) (&AUX CH M NEW-MODE ARG)
    (SETQ PEEK-MODE-CONTINUE-FUNCTION NIL PEEK-MODE-DATA NIL)
    (SETQ NEW-MODE PEEK-MODE)
    (SETQ ARG NIL)
    (DO () (())
	(COND ((KBD-CHAR-AVAILABLE)
	       (SETQ CH (CHAR-UPCASE (KBD-TYI)))
	       (SETQ PEEK-LAST-INPUT-TIME (TIME))
	       (SELECTQ CH
		 (#/V (<- SELF ':SCROLL-POSITION<-
			  (+ TOP-SCREEN-LINE (OR ARG (MAX 1 (1- SCREEN-LINES))))))
		 ((#/V #\BS)
                   (<- SELF ':SCROLL-POSITION<-
                       (- TOP-SCREEN-LINE (OR ARG (MAX 1 (1- SCREEN-LINES))))))
                 (#/> (<- SELF ':SCROLL-POSITION<-
                           (SCROLL-LINE-SCREEN-LINE (CAR (LAST LINE-TABLE)))))
		 ((#\BREAK #/P #/Q) (TOP-WINDOW))
		 (#\FF (SETQ REDISPLAY-NEEDED T))
		 (#/< (<- SELF ':SCROLL-POSITION<- 0))
		 (#\SP (COND ((= (+ TOP-SCREEN-LINE SCREEN-LINES)
				 (SCROLL-LINE-SCREEN-LINE (CAR (LAST LINE-TABLE))))
			      (SETQ NEW-MODE PEEK-MODE))
			     (T 
			      (<- SELF ':SCROLL-POSITION<-
				  (+ TOP-SCREEN-LINE (MAX 1 (1- SCREEN-LINES)))))))
		 ((#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
		  (SETQ ARG (+ (- CH #/0) (* (OR ARG 0) 8))))
		 (OTHERWISE (SETQ NEW-MODE CH)))
	       (AND (OR (< CH #/0) (> CH #/9))
                    (NULL NEW-MODE)
		    (SETQ ARG NIL))))
	(COND (NEW-MODE
	       (COND ((SETQ M (ASSQ NEW-MODE PEEK-MODE-ALIST))
		      (AND PEEK-MODE-CONTINUE-FUNCTION
			   (FUNCALL PEEK-MODE-CONTINUE-FUNCTION T))
		      (<- SELF ':RESET-LINES 0)
		      (SETQ PEEK-MODE NEW-MODE PEEK-MODE-DATA NIL
			    PEEK-MODE-CONTINUE-FUNCTION NIL)
		      (<- SELF ':UPDATE-LABEL)
		      (FUNCALL (CADR M) ARG)
		      (SETQ ARG NIL))
		     (T (TV-BEEP)))
	       (SETQ NEW-MODE NIL))
	      (PEEK-MODE-CONTINUE-FUNCTION
	        (FUNCALL PEEK-MODE-CONTINUE-FUNCTION NIL))
              (T (PROCESS-SLEEP 10)))
        (<- SELF ':UPDATE)))

(DEFMETHOD (PEEK-WINDOW-CLASS :PRINT-LABEL) (PCPPR)
    (TV-STRING-OUT PCPPR SI:NAME)
    (TV-STRING-OUT PCPPR "  Mode ")
    (TV-STRING-OUT PCPPR (FORMAT NIL "~:C" PEEK-MODE)))

;; Print the status of all the hosts and gateways, or specified ones
(DEFUN PEEK-HOSTAT (&OPTIONAL IGNORE &AUX CONNECTIONS ADR (I 1))
  (CHAOS:ASSURE-ENABLED)
  (DO ((L (SORT (APPEND CHAOS:HOST-ALIST NIL) #'(LAMBDA (X Y) (< (CDR X) (CDR Y))))
	  (CDR L)))
      ((NULL L))
    (SETQ ADR (CDAR L))
    (COND ((NOT (ASSQ ADR CONNECTIONS))
           (PUSH (LIST ADR (CHAOS:OPEN-CONNECTION ADR "STATUS" 1) I)
                 CONNECTIONS)
           (SETQ I (1+ I)))))                   ;Set up all connections in parallel
  ;; CONNECTIONS has elements that look like
  ;; (host-address connection virtual-line-number)
  (<- SELF ':RESET-LINES I)
  ;; Columns are:
  ;;7 name, 40 subnet, 45 #in, 51 #out, 57 tabort, 63 lost, 69 crc, 75 crc2, 81 bitc, 87 other
  (<- SELF ':CHANGE-LINE 0
      (FORMAT NIL "Site   Name//Status~22XSubnet #-in #-out abort  lost  crc ramerr bitc  other"))
  (SETQ PEEK-MODE-CONTINUE-FUNCTION 'PEEK-HOSTAT-1)
  (SETQ PEEK-MODE-DATA CONNECTIONS))

(DEFUN PEEK-HOSTAT-1 (EXIT-FLAG &AUX PKT)
  (COND (EXIT-FLAG
	 (DO L PEEK-MODE-DATA (CDR L) (NULL L)	;Flush any connections that remain
	     (CHAOS:REMOVE-CONN (CADAR L))))
	(T
	 (PROCESS-SLEEP 10.)			;Take a few chaos net interrupts
	 (DO ((LIST PEEK-MODE-DATA (CDR LIST)))	;Check on each connection
	     ((NULL LIST))
	   (SELECTQ (CHAOS:STATE (CADAR LIST))
	      (CHAOS:RFC-SENT-STATE
	       (COND ((NOT (BIT-TEST 1000000	;5-second timeout
				     (%24-BIT-DIFFERENCE
				      (%24-BIT-DIFFERENCE (TIME)
							  (CHAOS:TIME-LAST-RECEIVED (CADAR LIST)))
				      300.)))
		      (<- SELF ':CHANGE-LINE (CADDAR LIST)
			  (FORMAT NIL "~7A~:[~;~1G~A   ~]host not responding"
				  (FORMAT NIL "~O" (CAAR LIST))
                                  (CAR (RASSOC (CAAR LIST) CHAOS:HOST-ALIST))))
		      (CHAOS:REMOVE-CONN (CADAR LIST))
		      (SETQ PEEK-MODE-DATA (DELQ (CAR LIST) PEEK-MODE-DATA)))))
	      (CHAOS:ANSWERED-STATE			;This is what we want
	        (SETQ PKT (CHAOS:GET-NEXT-PKT (CADAR LIST)))
		(PEEK-HOSTAT-FORMAT-ANS (CAR LIST) PKT)
		(CHAOS:RETURN-PKT PKT)
		(CHAOS:CLOSE (CADAR LIST))
		(SETQ PEEK-MODE-DATA (DELQ (CAR LIST) PEEK-MODE-DATA)))
	      (CHAOS:CLS-RECEIVED-STATE
	        (SETQ PKT (CHAOS:GET-NEXT-PKT (CADAR LIST)))
		(<- SELF ':CHANGE-LINE (CADDAR LIST)
		    (FORMAT NIL "~7A~:[~;~1G~A   ~]returned a CLS:~A"
                            (FORMAT NIL "~O" (CAAR LIST))
                            (CAR (RASSOC (CAAR LIST) CHAOS:HOST-ALIST))
			    (CHAOS:PKT-STRING PKT)))
		(CHAOS:RETURN-PKT PKT)
		(CHAOS:CLOSE (CADAR LIST))
		(SETQ PEEK-MODE-DATA (DELQ (CAR LIST) PEEK-MODE-DATA)))
	      (CHAOS:OPEN-STATE
	        (<- SELF ':CHANGE-LINE (CADDAR LIST)
		    (FORMAT NIL "~7A~:[~;~1G~A   ~]returned an OPN"
                            (FORMAT NIL "~O" (CAAR LIST))
                            (CAR (RASSOC (CAAR LIST) CHAOS:HOST-ALIST))))
		(CHAOS:CLOSE (CADAR LIST) "I expected an ANS, not an OPN.")
		(SETQ PEEK-MODE-DATA (DELQ (CAR LIST) PEEK-MODE-DATA)))
	      (CHAOS:LOS-RECEIVED-STATE
	        (SETQ PKT (CHAOS:READ-PKTS-LAST (CADAR LIST)))
		(<- SELF ':CHANGE-LINE (CADDAR LIST)
		    (FORMAT NIL "~7A~:[~;~1G~A   ~]returned a LOS:~A"
                            (FORMAT NIL "~O" (CAAR LIST))
                            (CAR (RASSOC (CAAR LIST) CHAOS:HOST-ALIST))
			    (CHAOS:PKT-STRING PKT)))
		(CHAOS:CLOSE (CADAR LIST))
		(SETQ PEEK-MODE-DATA (DELQ (CAR LIST) PEEK-MODE-DATA)))
	      (OTHERWISE
	        (<- SELF ':CHANGE-LINE (CADDAR LIST)
		    (FORMAT NIL "~7A~:[~;~1G~A   ~]connection entered bad state:~A"
                            (FORMAT NIL "~O" (CAAR LIST))
                            (CAR (RASSOC (CAAR LIST) CHAOS:HOST-ALIST))
			    (CHAOS:STATE (CADAR LIST))))
		(CHAOS:CLOSE (CADAR LIST))
		(SETQ PEEK-MODE-DATA (DELQ (CAR LIST) PEEK-MODE-DATA))))))))

 ;; 7 name, 40 subnet, 45 #in, 51 #out, 57 tabort, 63 lost, 69 crc, 75 crc2, 81 bitc, 87 other
(DEFUN PEEK-HOSTAT-FORMAT-ANS (HOST-CONN PKT &AUX STR STR1 (NBYTES (CHAOS:PKT-NBYTES PKT)))
  (SETQ STR (FORMAT NIL "~7A~A"		;Print host number and name as returned
		    (FORMAT NIL "~O" (CAR HOST-CONN))
		    (NSUBSTRING (CHAOS:PKT-STRING PKT) 0
				(MIN NBYTES 32.
				     (OR (STRING-SEARCH-CHAR 200
							     (CHAOS:PKT-STRING PKT)) 32.)))))
  (SETQ STR1 STR)
  (DO ((I 24. (+ I 2 CT))			;Now display subnet meters
       (ID) (CT) (FIRST-TIME T NIL)
       (MAXI (+ 8 (// NBYTES 2))))
      ((>= I MAXI))
    (OR FIRST-TIME (SETQ STR ""))
    (SETQ ID (AREF PKT I) CT (AREF PKT (1+ I)))	;Block header
    (COND ((< ID 400)				;Subnet info
	   (SETQ STR (FORMAT NIL "~40,5,1A~4O" STR ID))
	   (DO ((J (+ I 2) (1+ J))		;Now print those meters that are present
		(N (MIN CT 8) (1- N)))
	       ((ZEROP N))
	      (SETQ STR (FORMAT NIL "~A ~5D" STR (AR-1 PKT J)))))
	  (T					;I don't know about this
	   (SETQ STR (FORMAT NIL "~40,5,1A~O unknown info block ID" STR ID))))
    (COND (FIRST-TIME (SETQ STR1 STR))
          (T (SETQ STR1 (FORMAT NIL "~A~%~A" STR1 STR)))))
  (<- SELF ':CHANGE-LINE (CADDR HOST-CONN) STR1))

;; Handler for Peek "A" mode.  Print names and states of active processes.
(DEFUN PEEK-PROCESSES (&OPTIONAL IGNORE)
    (<- SELF ':RESET-LINES 6)
    (<- SELF ':CHANGE-LINE 0 "  Process           State")
    (<- SELF ':CHANGE-LINE 1 "" 'PROCESSES-START)
    (<- SELF ':CHANGE-LINE 2 "" 'PROCESSES-END)
    (<- SELF ':CHANGE-LINE 3 "Clock functions enabled:")
    (<- SELF ':CHANGE-LINE 4 "" 'CLOCK-FCNS-START)
    (<- SELF ':CHANGE-LINE 5 "" 'CLOCK-FCNS-END)
    (SETQ PEEK-MODE-CONTINUE-FUNCTION 'PEEK-PROCESSES-UPDATE)
    (PEEK-PROCESSES-UPDATE 'NOWAIT))

(DEFUN PEEK-PROCESSES-UPDATE (FLAG)
    (OR (EQ FLAG 'NOWAIT) (PROCESS-SLEEP 10))
    (PEEK-UPDATE-SUBSECTION 'PROCESSES-START 'PROCESSES-END
	  'CAR 'CDR ACTIVE-PROCESSES	;Note process in CAR of ACTIVE-PROCESS entry now.
	  (FUNCTION (LAMBDA (P)
	      (LET ((PN (FUNCTION (LAMBDA (PROC) (AND (CAR PROC)
						      (PROCESS-NAME (CAR PROC))))))
		    (PWS (FUNCTION (LAMBDA (PROC)
				     (AND (CAR PROC)
					  (OR (PROCESS-WHOSTATE (CAR PROC)) "RUN"))))))
	       ;; For each process, make a line containing automatically updating items
	       ;; that give the process name, the stack group name, and the whostate.
               ;; ,@NIL copies what precedes it.
		   `(((SCROLL-ITEM-COMPARE-STRING-EQ ,PN ,P) 160. ,@(LIST NIL NIL NIL))
                     ((SCROLL-ITEM-COMPARE-STRING-EQ ,PWS ,P) 160. ,@(LIST NIL NIL NIL)))))))
    (PEEK-UPDATE-SUBSECTION 'CLOCK-FCNS-START 'CLOCK-FCNS-END
	  'CAR 'CDR KBD-SIMULATED-CLOCK-FCN-LIST
	  'STRING))

;; Handler for "?" mode, driven off the same alist as mode lookup.
(DEFUN PEEK-HELP (&OPTIONAL IGNORE)
    (<- SELF ':RESET-LINES (+ 4 (LENGTH PEEK-MODE-ALIST)))
    (<- SELF ':CHANGE-LINE 0 "Table of PEEK modes:")
    (DO ((I 2 (1+ I)) (MS PEEK-MODE-ALIST (CDR MS)))
	((NULL MS))
       (<- SELF ':CHANGE-LINE I
	   (FORMAT NIL "~8A  ~A"
		   (FORMAT NIL "~:C" (CAAR MS))
		   (OR (CADDAR MS) (CADAR MS)))))
    (<- SELF ':CHANGE-LINE (+ 3 (LENGTH PEEK-MODE-ALIST))
        "C-V       scrolls down a full window
Space     scrolls down a window or repeats mode
M-V or BS scrolls up a full window
M-<       scrolls to the top
M->       scrolls to the bottom
Form      repeats the current mode
P or Q    goes back to top window"))

;; Peek "M" mode - describe all areas.
(DEFUN PEEK-AREAS (&OPTIONAL IGNORE)
    (<- SELF ':RESET-LINES 5)
    (<- SELF ':CHANGE-LINE 0
        `("Physical memory size: "
          ((SCROLL-ITEM-COMPARE-NUMBER SYSTEM-COMMUNICATION-AREA ,%SYS-COM-MEMORY-SIZE)
           80. ,@(LIST NIL NIL NIL))))
    (<- SELF ':CHANGE-LINE 1
        `("Virtual memory free: "
          ((SCROLL-ITEM-COMPARE-NUMBER ROOM-GET-AREA-LENGTH-USED ,FREE-AREA)
           80. ,@(LIST NIL NIL NIL))))
    (<- SELF ':CHANGE-LINE 3
        "Area                            Used")
    (LET ((SIZE (FUNCTION (LAMBDA (AREA &AUX USED)
                     (MULTIPLE-VALUE (NIL USED NIL) (ROOM-GET-AREA-LENGTH-USED AREA))
                     USED))))
        (DO ((I 0 (1+ I)) (J 5)) ((= I (ARRAY-LENGTH (FUNCTION AREA-NAME))))
           (COND ((AREA-NAME I)
                  (<- SELF ':INSERT-LINES J 1)
                  (<- SELF ':CHANGE-LINE J
                      `(((SCROLL-ITEM-COMPARE-STRING-EQ EVAL ,(STRING (AREA-NAME I)))
                         260. ,@(LIST NIL NIL NIL))
                        ((SCROLL-ITEM-COMPARE-NUMBER ,SIZE ,I)
                         160. ,@(LIST NIL NIL NIL))))
                  (SETQ J (1+ J)))))))

;% mode
(DEFUN PEEK-COUNTERS (&OPTIONAL IGNORE)
  (<- SELF ':RESET-LINES (LENGTH SYS:A-MEMORY-COUNTER-BLOCK-NAMES))
  (DO ((I 0 (1+ I))
       (L SYS:A-MEMORY-COUNTER-BLOCK-NAMES (CDR L)))
      ((NULL L))
    (<- SELF ':CHANGE-LINE I
	(LIST (LIST `(SCROLL-ITEM-FORMAT "~35A" ,(STRING (CAR L)))
		     280. NIL NIL NIL)
	      (LIST `(SCROLL-ITEM-FORMAT " ~D."
					 (READ-METER ',(CAR L)))
		     80. NIL NIL NIL)))))

(DEFUN PEEK-CHAOS (&OPTIONAL INDEX)
    (<- SELF ':RESET-LINES 14.)
    (<- SELF ':CHANGE-LINE 0
	`("Chaosnet status, time "
	  ((SCROLL-ITEM-COMPARE-NUMBER TIME) 100. ,@(LIST NIL NIL NIL))
	  ((SCROLL-ITEM-COMPARE-STRING-EQ
	    ,(FUNCTION (LAMBDA ()
	         (COND (CHAOS:ENABLE "Active")
		       (T "Deactivated.")))))
	   160. ,@(LIST NIL NIL NIL))))
    (<- SELF ':CHANGE-LINE 1
	`("Fwd " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-FORWARDED)
		  60. ,@(LIST NIL NIL NIL))
	  "Overfwd " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-OVER-FORWARDED)
		      60. ,@(LIST NIL NIL NIL))
	  "Lost " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-LOST)
		   60. ,@(LIST NIL NIL NIL))))
    (<- SELF ':CHANGE-LINE 2
	`("Made " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-MADE)
		   40. ,@(LIST NIL NIL NIL))
	  "Free " ((SCROLL-ITEM-COMPARE-NUMBER
		    ,(FUNCTION (LAMBDA ()
		         (DO ((I 0 (1+ I))
			      (FP CHAOS:FREE-PKTS (CHAOS:PKT-LINK FP)))
			     ((SYMBOLP FP) I)))))
		   40. ,@(LIST NIL NIL NIL))
	  "Pkts Sent " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-TRANSMITTED)
			60. ,@(LIST NIL NIL NIL))
	  "Pkts Rcvd " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-RECEIVED)
			60. ,@(LIST NIL NIL NIL))
	  "LOS " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:CURRENT-LOS-PKT-COUNT)
		  40. ,@(LIST NIL NIL NIL))))
    (<- SELF ':CHANGE-LINE 3
	`("Bad:  Dest " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-BAD-DEST)
			 40. ,@(LIST NIL NIL NIL))
	  "Bit count " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-BAD-BIT-COUNT)
			40. ,@(LIST NIL NIL NIL))
	  "CRC-1 " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-BAD-CRC-1)
		    40. ,@(LIST NIL NIL NIL))
	  "CRC-2 " ((SCROLL-ITEM-COMPARE-NUMBER SYMEVAL CHAOS:PKTS-BAD-CRC-2)
		    40. ,@(LIST NIL NIL NIL))
	  "Transmit-aborts " ((SCROLL-ITEM-COMPARE-NUMBER READ-METER
							  SI:%COUNT-CHAOS-TRANSMIT-ABORTS)
			      40. ,@(LIST NIL NIL NIL))))
    (COND (INDEX (PEEK-CHAOS-PKTS INDEX))
	  (T
	   (<- SELF ':CHANGE-LINE 5 "Connections:")
	   (<- SELF ':CHANGE-LINE 6 "" 'CONNECTIONS-START)
	   (<- SELF ':CHANGE-LINE 7 "" 'CONNECTIONS-END)
	   (<- SELF ':CHANGE-LINE 8 "Pending LISTENs:")
	   (<- SELF ':CHANGE-LINE 9 "" 'LISTENS-START)
	   (<- SELF ':CHANGE-LINE 10. "" 'LISTENS-END)
	   (<- SELF ':CHANGE-LINE 11. "Pending RFCs:")
	   (<- SELF ':CHANGE-LINE 12. "" 'RFCS-START)
	   (<- SELF ':CHANGE-LINE 13. "" 'RFCS-END)
	   (SETQ PEEK-MODE-CONTINUE-FUNCTION 'PEEK-CHAOS-UPDATE)
	   (PEEK-CHAOS-UPDATE 'NOWAIT))))

(DEFUN PEEK-CHAOS-UPDATE (FLAG)
    (OR (EQ FLAG 'NOWAIT) (PROCESS-SLEEP 10))
    (PEEK-UPDATE-SUBSECTION 'LISTENS-START 'LISTENS-END 
	  'PROG1 'CDR CHAOS:PENDING-LISTENS   ;Element is CONS of CONTACT-NAME and CONN 
	  'CAAR)
    (PEEK-UPDATE-SUBSECTION 'RFCS-START 'RFCS-END
	  'PROG1 (FUNCTION (LAMBDA (PKT) (CHAOS:PKT-LINK PKT))) CHAOS:PENDING-RFC-PKTS
	  'CHAOS:PKT-STRING)
    (PEEK-UPDATE-SUBSECTION 'CONNECTIONS-START 'CONNECTIONS-END
	  (FUNCTION (LAMBDA (I) (AR-1 CHAOS:INDEX-CONN I)))
	  (FUNCTION (LAMBDA (I)
		    (AND (< (1+ I) (ARRAY-LENGTH CHAOS:INDEX-CONN))
			 (1+ I))))
	  0
	  'PEEK-CHAOS-CONN-LINE))

(DEFUN PEEK-CHAOS-CONN-LINE (CONN)
    `(,(FORMAT NIL "Index ~O (~O) "
	       (LDB CHAOS:MAXIMUM-INDEX-LOG-2-MINUS-1
		    (CHAOS:LOCAL-INDEX-NUM CONN))
	       (%POINTER CONN))
      "State: " ((SCROLL-ITEM-COMPARE-STRING-EQ
		  ,(FUNCTION (LAMBDA (CONN) (CHAOS:STATE CONN)))
		  ,CONN)
		 140. ,@(LIST NIL NIL NIL))
      ,(FORMAT NIL "From ~O-~O to ~O-~O~%"
               CHAOS:MY-ADDRESS (CHAOS:LOCAL-INDEX-NUM CONN)
               (CHAOS:FOREIGN-ADDRESS CONN) (CHAOS:FOREIGN-INDEX-NUM CONN))
      "Rcvd #" ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:PKT-NUM-RECEIVED CONN)))
		 ,CONN)
		50. ,@(LIST NIL NIL NIL))
      "Read #" ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:PKT-NUM-READ CONN)))
		 ,CONN)
		50. ,@(LIST NIL NIL NIL))
      "Acked #" ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:PKT-NUM-ACKED CONN)))
		 ,CONN)
		50. ,@(LIST NIL NIL NIL))
      "Sent #" ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:PKT-NUM-SENT CONN)))
		 ,CONN)
		50. ,@(LIST NIL NIL NIL))
      "Acked #" ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:SEND-PKT-ACKED CONN)))
		 ,CONN)
		50. ,@(LIST NIL NIL NIL))
      "
Windows: "
      "Local " ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:LOCAL-WINDOW-SIZE CONN)))
		 ,CONN)
		40. ,@(LIST NIL NIL NIL))
      "Foreign " ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:FOREIGN-WINDOW-SIZE CONN)))
		 ,CONN)
		40. ,@(LIST NIL NIL NIL))
      "Available " ((SCROLL-ITEM-COMPARE-NUMBER
		 ,(FUNCTION (LAMBDA (CONN) (CHAOS:WINDOW-AVAILABLE CONN)))
		 ,CONN)
		40. ,@(LIST NIL NIL NIL))))

(DEFUN PEEK-CHAOS-PKTS (INDEX &AUX CONN)
    (<- SELF ':DELETE-LINES 5 (- (LENGTH (<- SELF ':LINE-TABLE)) 6))
    (COND ((AND (>= INDEX 0)
		(< INDEX (ARRAY-ACTIVE-LENGTH CHAOS:INDEX-CONN))
		(SETQ CONN (AR-1 CHAOS:INDEX-CONN INDEX)))
	   (<- SELF ':INSERT-LINES 5 11.)
	   (<- SELF ':CHANGE-LINE 5 (PEEK-CHAOS-CONN-LINE CONN))
	   (<- SELF ':CHANGE-LINE 7 "Send Packets:")
	   (<- SELF ':CHANGE-LINE 8 "" 'SEND-PKTS-START)
	   (<- SELF ':CHANGE-LINE 9 "" 'SEND-PKTS-END)
	   (<- SELF ':CHANGE-LINE 10. "Read Packets:")
	   (<- SELF ':CHANGE-LINE 11. "" 'READ-PKTS-START)
	   (<- SELF ':CHANGE-LINE 12. "" 'READ-PKTS-END)
	   (<- SELF ':CHANGE-LINE 13. "Received Packets:")
	   (<- SELF ':CHANGE-LINE 14. "" 'RECEIVED-PKTS-START)
	   (<- SELF ':CHANGE-LINE 15. "" 'RECEIVED-PKTS-END)
	   (SETQ PEEK-MODE-DATA CONN))
	  (T (<- SELF ':INSERT-LINES 5 1)
	     (<- SELF ':CHANGE-LINE 5
		 (FORMAT NIL "No such connection (index ~O)" INDEX))
	     (SETQ PEEK-MODE-DATA NIL)))
    (SETQ PEEK-MODE-CONTINUE-FUNCTION 'PEEK-CHAOS-PKTS-UPDATE)
    (PEEK-CHAOS-PKTS-UPDATE 'NOWAIT))

(DEFUN PEEK-CHAOS-PKTS-UPDATE (FLAG)
    (OR (EQ FLAG 'NOWAIT) (PROCESS-SLEEP 10))
    (AND PEEK-MODE-DATA
	 (NULL (AR-1 CHAOS:INDEX-CONN
		     (LDB CHAOS:MAXIMUM-INDEX-LOG-2-MINUS-1
			  (CHAOS:LOCAL-INDEX-NUM PEEK-MODE-DATA))))
	 (PEEK-CHAOS-PKTS (LDB CHAOS:MAXIMUM-INDEX-LOG-2-MINUS-1
			       (CHAOS:LOCAL-INDEX-NUM PEEK-MODE-DATA))))
    (COND (PEEK-MODE-DATA
	   (PEEK-UPDATE-SUBSECTION 'SEND-PKTS-START 'SEND-PKTS-END
				   'PROG1 (FUNCTION (LAMBDA (PKT) (CHAOS:PKT-LINK PKT)))
				   (CHAOS:SEND-PKTS PEEK-MODE-DATA)
				   'PEEK-CHAOS-PKT-LINE)
	   (PEEK-UPDATE-SUBSECTION 'READ-PKTS-START 'READ-PKTS-END
				   'PROG1 (FUNCTION (LAMBDA (PKT) (CHAOS:PKT-LINK PKT)))
				   (CHAOS:READ-PKTS PEEK-MODE-DATA)
				   'PEEK-CHAOS-PKT-LINE)
	   (PEEK-UPDATE-SUBSECTION 'RECEIVED-PKTS-START 'RECEIVED-PKTS-END
				   'PROG1 (FUNCTION (LAMBDA (PKT) (CHAOS:PKT-LINK PKT)))
				   (CHAOS:RECEIVED-PKTS PEEK-MODE-DATA)
				   'PEEK-CHAOS-PKT-LINE))))

(DEFUN PEEK-CHAOS-PKT-LINE (PKT)
    (FORMAT NIL "Number: ~O (~O)  Opcode: ~O (~A).  Number of bytes = ~O ."
	       (CHAOS:PKT-NUM PKT)
	       (%POINTER PKT)
	       (CHAOS:PKT-OPCODE PKT)
	       (COND ((< (CHAOS:PKT-OPCODE PKT) (LENGTH CHAOS:OPCODE-LIST))
		      (NTH (CHAOS:PKT-OPCODE PKT) CHAOS:OPCODE-LIST))
		     (( (CHAOS:PKT-OPCODE PKT) CHAOS:DAT-OP) 'DAT)
		     (T (FORMAT NIL "==> ~O <==" (CHAOS:PKT-OPCODE PKT))))
	       (CHAOS:PKT-NBYTES PKT)))


;Perform a major update on a part of a PEEK display that shows a list of things,
;taking care of new elements and elements that disappear
;(assuming that changing facts about items already displayed
; are handled by automatically updating scroll items).
;The part of the display we are dealing with is found by two "tags"
;which are just the topics of the lines before and after those we update.
;Since the "list" of things may not actually be represented as a list,
;the caller supplies an initial link, a CAR-function, and a CDR-function.
;If an actual list is used, they can be the list, CAR, and CDR.
;When a new element appears, the element is passed to the caller-supplied
;DPY-FUNCTION which should return a suitable line for :CHANGE-LINE.
;The COMPARE-FUNCTION should be the function used to compare two of the things.
; If not supplied, this function defaults to EQ
(DEFUN PEEK-UPDATE-SUBSECTION (START-TAG END-TAG CAR-FUNCTION
			       CDR-FUNCTION INITIAL-LINK DPY-FUNCTION
			       &OPTIONAL (COMPARE-FUNCTION #'EQ))
    (LET ((LT (<- SELF ':LINE-TABLE)))
	(LET ((START (PEEK-FIND-TOPIC LT START-TAG))
	      (END (PEEK-FIND-TOPIC LT END-TAG))
	      NEXT-ITEM NEXT-POS START-POS)
      (COND ((NUMBERP (SETQ START-POS (FIND-POSITION-IN-LIST (CAR START) LT))) ;MIGHT HAVE
	     (SETQ NEXT-POS (1+ START-POS))    ;GONE AWAY IF WINDOW SHRUNK.
	     (DO () ((NULL INITIAL-LINK))
		(SETQ NEXT-ITEM (FUNCALL CAR-FUNCTION INITIAL-LINK))
		(COND (NEXT-ITEM
		       (DO ((L (NTHCDR (- NEXT-POS START-POS) START) (CDR L)) (I 0 (1+ I)))
			   ((EQ L END)
			    (<- SELF ':INSERT-LINES NEXT-POS 1)
			    (<- SELF ':CHANGE-LINE NEXT-POS
				(FUNCALL DPY-FUNCTION NEXT-ITEM)
				NEXT-ITEM))
			   (OR L (BREAK T))
			  (AND (FUNCALL COMPARE-FUNCTION (SCROLL-LINE-TOPIC (CAR L))
					                 NEXT-ITEM)
			       (RETURN (AND (NOT (ZEROP I))
                                            (<- SELF ':DELETE-LINES NEXT-POS I)))))
		       (SETQ NEXT-POS (1+ NEXT-POS))))
		(SETQ INITIAL-LINK (FUNCALL CDR-FUNCTION INITIAL-LINK)))
	     (DO () ((EQ (NTHCDR NEXT-POS LT) END))
		(<- SELF ':DELETE-LINES NEXT-POS 1)))))))

(DEFUN PEEK-FIND-TOPIC (LINE-TABLE TOPIC)
    (DO LT LINE-TABLE (CDR LT) (NULL LT)
       (AND (EQ (SCROLL-LINE-TOPIC (CAR LT)) TOPIC)
	    (RETURN LT))))

;;; Display file system status
(DEFUN PEEK-FILE-SYSTEM-STATUS (&OPTIONAL IGNORE)
  (<- SELF ':RESET-LINES (1+ (* 2 (LENGTH FILE-HOST-ALIST))))
  (<- SELF ':CHANGE-LINE 0
      `("File system status as of " 
	  ((SCROLL-ITEM-COMPARE-NUMBER TIME) 100. ,@(LIST NIL NIL NIL))))
  ;; Use host device name and first host unit as identifiers of the subsection
  (DO ((L FILE-HOST-ALIST (CDR L))
       (LINE 1 (+ LINE 2)))
      ((NULL L))
    (<- SELF ':CHANGE-LINE LINE "" (CAAR L))
    (<- SELF ':CHANGE-LINE (1+ LINE) "" (CADAR L)))
  (SETQ PEEK-MODE-CONTINUE-FUNCTION 'PEEK-FILE-SYSTEM-UPDATE)
  (PEEK-FILE-SYSTEM-UPDATE 'NOWAIT))

(DEFUN PEEK-FILE-SYSTEM-UPDATE (FLAG)
  (OR (EQ FLAG 'NOWAIT)
      (PROCESS-SLEEP 10.))
  (DOLIST (L FILE-HOST-ALIST)
    (PEEK-UPDATE-SUBSECTION (CAR L) (CADR L)
			    #'PROG1  #'(LAMBDA (X) (HOST-UNIT-LINK X))
			    (SYMEVAL-IN-CLOSURE (CADR L) 'SI:FILE-HOST-FIRST-UNIT)
			    #'PEEK-FILE-DPY-HOST-UNIT)))

(DEFUN PEEK-FILE-DPY-HOST-UNIT (UNIT &AUX STRING CHANNEL)
  (SETQ STRING (FORMAT NIL "Host ~A, control connection in ~A"
		       (HOST-UNIT-HOST UNIT)
		       (LET ((CONN (HOST-UNIT-CONTROL-CONNECTION UNIT)))
			 (COND (CONN (CHAOS:STATE CONN))
			       (T "NONEXISTANT-STATE")))))
  (DOLIST (DATA-CONN (HOST-UNIT-DATA-CONNECTIONS UNIT))
    (COND ((SETQ CHANNEL (DATA-CHANNEL DATA-CONN ':INPUT))
	   (SETQ STRING
		 (STRING-APPEND
		  STRING
		  (FORMAT NIL "~%Input file:  ~29A  ~10A ~D bytes read"
			  (CHANNEL-FILE-NAME CHANNEL) (CHANNEL-MODE CHANNEL)
			  (+ (CHANNEL-FIRST-FILEPOS CHANNEL)
			     (- (CHANNEL-FIRST-COUNT CHANNEL)
				(CHANNEL-DATA-COUNT CHANNEL))))))))
    (COND ((SETQ CHANNEL (DATA-CHANNEL DATA-CONN ':OUTPUT))
	   (SETQ STRING
		 (STRING-APPEND
		  STRING
		  (FORMAT NIL "~%Output file: ~29A  ~10A ~D bytes written"
			  (CHANNEL-FILE-NAME CHANNEL) (CHANNEL-MODE CHANNEL)
			  (+ (CHANNEL-FIRST-FILEPOS CHANNEL)
			     (- (CHANNEL-FIRST-COUNT CHANNEL)
				(CHANNEL-DATA-COUNT CHANNEL)))))))))
  STRING)
