;;; -*- Mode: LISP; Package: CHAOS; BASE: 8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;; Very high-level CHAOSnet functions.
;; The NCP and low level functions in LMIO;CHAOS

(DECLARE (SPECIAL
	HOST-ALIST		;ALIST of host names and numbers for symbolic addressing
	KNOWN-NAME-ALIST        ;ALIST of host numbers and names for sepcially known machines
        FINGER-ALIST		;ALIST of host numbers and finger strings (for Supdup)
	CONSOLE-LOCATION-ALIST	;ALIST of host numbers and console location data
))

;; This does a full "ICP": it sends an RFC, waits for a reply or timeout,
;; and returns a string to get an error, or else the CONN to indicate that
;; the foreign host sent an OPN and we are connected.
;; The first argument gets parsed as an address.
(DEFUN CONNECT (ADDRESS CONTACT-NAME &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE)
					       (TIMEOUT (* 10. 60.))
			      &AUX CONN REAL-ADDRESS)
    (COND ((NULL (SETQ REAL-ADDRESS (ADDRESS-PARSE ADDRESS)))
	   (FORMAT NIL "~S is not a known address." ADDRESS))
	  (T (ASSURE-ENABLED)
	     (SETQ CONN (OPEN-CONNECTION REAL-ADDRESS CONTACT-NAME WINDOW-SIZE))
	     (WAIT CONN 'RFC-SENT-STATE TIMEOUT)
	     (SELECTQ (STATE CONN)
	       (OPEN-STATE CONN)
	       (RFC-SENT-STATE (CLOSE CONN)
			       "Host not responding.")
	       (ANSWERED-STATE (CLOSE CONN)
			       "Received an ANS instead of an OPN.")
	       (CLS-RECEIVED-STATE (PROG1 (LET ((PKT (GET-NEXT-PKT CONN)))
					    (PROG1 (STRING-APPEND (PKT-STRING PKT))
						   (RETURN-PKT PKT)))
					  (CLOSE CONN)))
	       (OTHERWISE (PROG1 (FORMAT NIL "Bad state in CHAOS:CONNECT: ~A~@[, ~A~]"
					 (STATE CONN) 
					 (AND (READ-PKTS CONN) (PKT-STRING (READ-PKTS CONN))))
				 (REMOVE-CONN CONN)))))))

;; Takes anything anyone might use as a ChaosNet address, and tries to return
;; the corresponding host number.  If it fails, returns NIL.
(DEFUN ADDRESS-PARSE (ADDRESS)
    (COND ((FIXP ADDRESS) ADDRESS)
	  ((SYMBOLP ADDRESS)
	   (ADDRESS-PARSE (GET-PNAME ADDRESS)))
	  ((STRING ADDRESS)
	   (CDR (ASSOC (STRING-UPCASE (STRING-TRIM '(#\SP #/: #\TAB) ADDRESS))
		       HOST-ALIST)))
	  (T NIL)))

;; This is used to perform a "simple connection".  An RFC is sent to the
;; specified address, expecting an ANS.  Returns a string if there was an
;; error, in which case the string is an ASCII explanation.  Otherwise
;; returns the ANS.  When you are done perusing the ANS, RETURN-PKT the PKT.
(DEFUN SIMPLE (ADDRESS CONTACT-NAME &OPTIONAL (TIMEOUT (* 10. 60.))
			     &AUX CONN REAL-ADDRESS)
    (COND ((NULL (SETQ REAL-ADDRESS (ADDRESS-PARSE ADDRESS)))
	   (FORMAT NIL "~S is not a known address." ADDRESS))
	  (T (ASSURE-ENABLED)
	     (SETQ CONN (OPEN-CONNECTION REAL-ADDRESS CONTACT-NAME 5))
	     (WAIT CONN 'RFC-SENT-STATE TIMEOUT)
	     (SELECTQ (STATE CONN)
		      (RFC-SENT-STATE
		       (REMOVE-CONN CONN)
		       "Host not responding.")
		      (CLS-RECEIVED-STATE
		       (LET ((PKT (GET-NEXT-PKT CONN)))
			    (PROG1 (STRING-APPEND (PKT-STRING PKT))
				   (RETURN-PKT PKT))))
		      (OPEN-STATE
		       (CLOSE CONN "I expected an ANS, not an OPN.")
		       "Received an OPN instead of an ANS.")
		      (ANSWERED-STATE (PROG1 (GET-NEXT-PKT CONN)
                                             (CLOSE CONN)))
		      (OTHERWISE (PROG1 (FORMAT NIL "Bad state: ~A" (STATE CONN))
					(REMOVE-CONN CONN)))))))

;;; USER FUNCTIONS: Functions for the user side of a connection.

;; This is called as the first step in opening a connection.  Note the
;; CONNECT function, which is a higher-level frob (like NETWRK's ICP routine)
;; which you may want to use instead.
;;   The first arg is the address of the foreign host.  Next is the contact name.
;; Optionally following are the one-way flag and window size.
(DEFUN OPEN-CONNECTION (ADDRESS CONTACT-NAME &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE)
		      &AUX PKT CONN)
    (CHECK-ARG ADDRESS (AND (NUMBERP ADDRESS) (>= ADDRESS 0) (<= ADDRESS 177777))
	       "an address")
    (CHECK-ARG CONTACT-NAME
               (AND (STRINGP CONTACT-NAME)
                    (<= (ARRAY-ACTIVE-LENGTH CONTACT-NAME) MAX-DATA-BYTES-PER-PKT))
               "a string")
    (CHECK-ARG WINDOW-SIZE NUMBERP "a number")
    (SETQ CONN (MAKE-CONNECTION))
    (SETF (LOCAL-WINDOW-SIZE CONN) (MAX 1 (MIN WINDOW-SIZE MAXIMUM-WINDOW-SIZE)))
    (SETF (FOREIGN-ADDRESS CONN) ADDRESS)

    (SETQ PKT (ALLOCATE-PKT))
    (SETF (PKT-OPCODE PKT) RFC-OP)
    (SET-PKT-STRING PKT CONTACT-NAME)
    (SETF (PKT-LINK PKT) NIL)
    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
    (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
    (SETF (PKT-DEST-ADDRESS PKT) (FOREIGN-ADDRESS CONN))
    (SETF (PKT-DEST-INDEX-NUM PKT) (FOREIGN-INDEX-NUM CONN))
    (WITHOUT-INTERRUPTS
      (SETF (SEND-PKTS CONN) PKT)
      (SETF (SEND-PKTS-LAST CONN) PKT)
      (SETF (SEND-PKTS-LENGTH CONN) 1)
      (SETF (WINDOW-AVAILABLE CONN) 1)
      (SETF (TIME-LAST-RECEIVED CONN) (TIME))
      (SETF (STATE CONN) 'RFC-SENT-STATE)
      (SETQ RETRANSMISSION-NEEDED T))
    (TRANSMIT-NORMAL-PKT CONN PKT (PKT-NUM-SENT CONN))
 
    CONN)


;;; SERVER FUNCTIONS: Functions used by the server side of a connection only.

(DEFUN LISTEN (CONTACT-NAME &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE)
		     &AUX CONN PREV RFC LENGTH)
    (CHECK-ARG CONTACT-NAME STRINGP "a string")
    (CHECK-ARG WINDOW-SIZE NUMBERP "a number")
    (SETQ CONN (MAKE-CONNECTION))
    (SETF (LOCAL-WINDOW-SIZE CONN) (MAX 1 (MIN WINDOW-SIZE MAXIMUM-WINDOW-SIZE)))

    (SETQ LENGTH (ARRAY-ACTIVE-LENGTH CONTACT-NAME))
    (SETQ PREV NIL)
    (WITHOUT-INTERRUPTS
      (SETQ RFC (DO PKT PENDING-RFC-PKTS (PKT-LINK PKT) (NULL PKT)
		    (AND (STRING-EQUAL (CONTACT-NAME-FROM-RFC PKT) CONTACT-NAME 0 0 LENGTH)
			 (RETURN PKT))
		    (SETQ PREV PKT)))
      (COND (RFC				;There is a pending RFC: you win!
	      (COND ((NULL PREV) (SETQ PENDING-RFC-PKTS (PKT-LINK RFC)))
		    (T (SETF (PKT-LINK PREV) (PKT-LINK RFC))))
	      (RFC-MEETS-LSN CONN RFC)
	      T)
	    (T				;Nope, let it pend.
	      (SETF (STATE CONN) 'LISTENING-STATE)
	      (PUSH (CONS CONTACT-NAME CONN) PENDING-LISTENS)
	      NIL))
      CONN))

;; If you have done a LISTEN and the state has changed to RFC-RECEIVED, you
;; call one of the following four functions.

;; Send an OPN, and leave conn in OPEN-STATE.
(DEFUN ACCEPT (CONN &AUX PKT)
    (OR (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
        (FERROR NIL "Attempt to accept ~S, which was in ~A, not RFC-RECEIVED-STATE"
		CONN (STATE CONN)))
    (SETQ PKT (READ-PKTS CONN))
    (COND (PKT					;In case the user has not read the RFC
	   (SETF (PKT-NUM-RECEIVED CONN) (PKT-NUM PKT))
	   (FREE-PKT PKT)))
    (SETF (READ-PKTS CONN) NIL)
    (SETQ PKT (ALLOCATE-PKT))
    (SETF (PKT-OPCODE PKT) OPN-OP)
    (SETF (PKT-NBYTES PKT) 4)
    (SETF (PKT-SECOND-DATA-WORD PKT) (LOCAL-WINDOW-SIZE CONN))
    (SETF (PKT-FIRST-DATA-WORD PKT) (PKT-NUM-READ CONN))
    (WITHOUT-INTERRUPTS
      (SETF (SEND-PKTS CONN) PKT)
      (SETF (PKT-LINK PKT) NIL)
      (SETF (SEND-PKTS-LAST CONN) PKT)
      (SETF (SEND-PKTS-LENGTH CONN) 1)
      (SETF (WINDOW-AVAILABLE CONN) 0)
      (SETQ RETRANSMISSION-NEEDED T)
      (SETF (TIME-LAST-RECEIVED CONN) (TIME))
      (SETF (STATE CONN) 'OPEN-STATE))  ;Set this -before- telling other end it's open!
    (TRANSMIT-NORMAL-PKT CONN PKT T)
    T)

;; Send a CLS and leave conn INACTIVE.
(DEFUN REJECT (CONN REASON &AUX PKT)
    (OR (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
        (FERROR NIL "Attempt to reject ~S, which was in ~A, not RFC-RECEIVED-STATE"
		CONN (STATE CONN)))
    (SETQ PKT (ALLOCATE-PKT))
    (SETF (PKT-OPCODE PKT) CLS-OP)
    (SET-PKT-STRING PKT REASON)
    (TRANSMIT-NORMAL-PKT CONN PKT)
    (FREE-PKT PKT)
    (REMOVE-CONN CONN)
    T)

;; Send an ANS, and leave conn INACTIVE.
;; The caller passes in a PKT with data and NBYTES set up.
(DEFUN ANSWER (CONN PKT)
    (COND ((EQ (STATE CONN) 'RFC-RECEIVED-STATE)
	   (SETF (PKT-OPCODE PKT) ANS-OP)
	   (TRANSMIT-NORMAL-PKT CONN PKT)))
    (RETURN-PKT PKT)
    (REMOVE-CONN CONN)
    T)

(DEFUN ANSWER-STRING (CONN STRING)
  (LET ((PKT (GET-PKT)))
    (SETF (PKT-NBYTES PKT) (MIN (STRING-LENGTH STRING) MAX-DATA-BYTES-PER-PKT))
    (COPY-ARRAY-CONTENTS STRING (PKT-STRING PKT))
    (ANSWER CONN PKT)))

;; Minimal-consing simple-transaction answerer.
;; Returns T if succeeds, NIL if fails, although you probably don't care, since
;; a value of T does not assure that the ANS really reached the requestor.
(DEFUN FAST-ANSWER-STRING (CONTACT-NAME STRING)
  (PROG ((PREV NIL) RFC PKT PSTR)
    (WITHOUT-INTERRUPTS
      (SETQ RFC (DO PKT PENDING-RFC-PKTS (PKT-LINK PKT) (NULL PKT)
		    (AND (STRING-EQUAL (CONTACT-NAME-FROM-RFC PKT) CONTACT-NAME)
			 (RETURN PKT))
		    (SETQ PREV PKT)))
      (IF (NULL RFC) (RETURN NIL)
	  (IF (NULL PREV) (SETQ PENDING-RFC-PKTS (PKT-LINK RFC))
	      (SETF (PKT-LINK PREV) (PKT-LINK RFC)))))
    (SETQ PKT (ALLOCATE-INT-PKT))
    (SETF (PKT-NBYTES PKT) (MIN (STRING-LENGTH STRING) MAX-DATA-BYTES-PER-PKT))
    (SETQ PSTR	     ;Create indirect array to reference as a string
	  (MAKE-ARRAY NIL 'ART-STRING MAX-DATA-BYTES-PER-PKT PKT '(0) 16.))
    (COPY-ARRAY-CONTENTS STRING PSTR)
    (RETURN-ARRAY (PROG1 PSTR (SETQ PSTR NIL)))
    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
    (SETF (PKT-SOURCE-INDEX-NUM PKT) 0)
    (SETF (PKT-DEST-ADDRESS PKT) (PKT-SOURCE-ADDRESS RFC))
    (SETF (PKT-DEST-INDEX-NUM PKT) (PKT-SOURCE-INDEX-NUM RFC))
    (SETF (PKT-OPCODE PKT) ANS-OP)
    (SETF (PKT-NUM PKT) 0)
    (SETF (PKT-ACK-NUM PKT) 0)
    (TRANSMIT-INT-PKT PKT)
    (SETF (PKT-STATUS RFC) NIL)
    (FREE-PKT RFC)
    (RETURN T)))

;; Send a FWD, and leave conn INACTIVE.
;; The caller passes in a PKT with data and NBYTES set up, and the address of
;;  the HOST to whom the RFCer should forward.
(DEFUN FORWARD (CONN PKT HOST)
    (OR (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
        (FERROR NIL "Attempt to forward ~S, which was in ~A, not RFC-RECEIVED-STATE"
		CONN (STATE CONN)))
    (SETF (PKT-OPCODE PKT) FWD-OP)
    (TRANSMIT-NORMAL-PKT CONN PKT 0 HOST)
    (RETURN-PKT PKT)
    (REMOVE-CONN CONN)
    T)

;; CONTROL OPERATIONS USED BY BOTH USERS AND SERVERS.

;; If CONN has received a close, free it up.
;; If CONN is inactive, do nothing.
;; If CONN is open, send a CLS containing the reason, leaving CONN inactive.
(DEFUN CLOSE (CONN &OPTIONAL (REASON "") &AUX PKT)
    (SELECTQ (STATE CONN)
       ((CLS-RECEIVED-STATE ANSWERED-STATE)
        (REMOVE-CONN CONN)
	NIL)
       (INACTIVE-STATE
	(SETQ CONN-LIST (DELQ CONN CONN-LIST))	;Just in case
	NIL)
       (OPEN-STATE
	(SETQ PKT (ALLOCATE-PKT))
	(SETF (PKT-OPCODE PKT) CLS-OP)
	(SET-PKT-STRING PKT REASON)
	(TRANSMIT-NORMAL-PKT CONN PKT)
	(FREE-PKT PKT)
	(REMOVE-CONN CONN)
	NIL)
       ((LOS-RECEIVED-STATE HOST-DOWN-STATE LISTENING-STATE RFC-SENT-STATE)
	(REMOVE-CONN CONN)
	NIL)
       (OTHERWISE
	(FERROR NIL "Attempt to close ~S, which was in ~S, not an acceptable state"
		CONN (STATE CONN)))))

(FSET 'CHAOS-CLOSE (FUNCTION CLOSE))

;; Wait until either:
;;  the state of CONN is not STATE  (return T), or
;;  over TIMEOUT 60ths of a second happen (return NIL).
(DEFUN WAIT (CONN STATE TIMEOUT &OPTIONAL (WHOSTATE "NET WAIT") &AUX START-TIME)
   (SETQ START-TIME (TIME))
   (DO () (NIL)
     (OR (EQ STATE (STATE CONN))
	 (RETURN T))
     (OR (< (TIME-DIFFERENCE (TIME) START-TIME) TIMEOUT)
	 (RETURN NIL))
     (PROCESS-WAIT WHOSTATE
		   (FUNCTION (LAMBDA (CONN STATE START-TIME TIMEOUT)
				     (OR (NEQ (STATE CONN) STATE)
					 ( (TIME-DIFFERENCE (TIME) START-TIME) TIMEOUT))))
		   CONN
		   STATE
		   START-TIME
		   TIMEOUT)))

;;;The following functions allow a CONN to be used as a full-duplex stream
(DECLARE (SPECIAL CONN IN-PKT IN-BYTES IN-I OUT-PKT OUT-I UNRCHF))

(BEGF STREAM-HANDLER)

(DEFUN STREAM (CONN &AUX IN-PKT (IN-BYTES 0) IN-I OUT-PKT OUT-I UNRCHF) 
  (CLOSURE '(CONN IN-PKT IN-BYTES IN-I OUT-PKT OUT-I UNRCHF) (FUNCTION STREAM-INTERNAL)))

;NOTE: READ THIS BEFORE MODIFYING FUNCTION BELOW
; Must setq in-pkt to NIL because it has a pointer into the free list,
; and since GET-NEXT-PKT goes blocked, it can be quit out of!!  This caused
; a re-entrant free list bug; do not remove the setq!
;FURTHER: since SEND-PKT can go blocked the out-pkt must likewise be set to
; NIL before calling it otherwise another process trying to output to the
; same stream while the previous is still blocked will try and use the old pkt.
;Actually, even that isn't enuf now that we have interrupts.  Scheduling must be
; inhibited while stuff is in OUT-PKT, etc.  It's ok to reschedule while in GET-PKT
; or SEND-PKT though.

(DEFUN STREAM-INTERNAL (OP &OPTIONAL ARG1 &REST REST)
   (SELECTQ OP
;      (:LINE-IN
;       (LET (CR-IDX)
;	 (IF (AND (PLUSP IN-BYTES)		;Optimize the fast case
;		  (SETQ CR-IDX (STRING-SEARCH-CHAR #\CR (PKT-STRING IN-PKT) IN-I)))
;	     (LET ((LEN (- CR-IDX IN-I))
;		   LINE)
;	       (SETQ LINE (MAKE-ARRAY NIL ART-STRING LEN NIL (AND (NUMBERP ARG1) ARG1)))
;	       (COPY-ARRAY-PORTION (PKT-STRING IN-PKT) IN-I CR-IDX LINE 0 LEN)
;	       (SETQ IN-I (1+ CR-IDX)
;		     IN-BYTES (- IN-BYTES (1+ LEN)))
;	       (AND (NUMBERP ARG1) (STORE-ARRAY-LEADER LEN LINE 0))
;	       LINE)
;	     (STREAM-DEFAULT-HANDLER 'STREAM-INTERNAL OP ARG1 REST))))
      ((:TYI :TYI-NO-HANG)
       (STREAM-INTERNAL ':FORCE-OUTPUT)
       (COND (UNRCHF (PROG1 UNRCHF (SETQ UNRCHF NIL)))
             (T (COND ((EQ IN-PKT 'EOF) (CAR REST))	;Hit EOF
                      (T (DO () ((> IN-BYTES 0)
                                 (SETQ IN-BYTES (1- IN-BYTES))
                                 (PROG1 (AR-1 (PKT-STRING IN-PKT) IN-I)
                                        (SETQ IN-I (1+ IN-I))))
                             (AND IN-PKT (RETURN-PKT (PROG1 IN-PKT (SETQ IN-PKT NIL))))
                             (SETQ IN-PKT (GET-NEXT-PKT CONN (EQ OP ':TYI-NO-HANG)))
                             (COND ((NULL IN-PKT) (RETURN NIL)) ;For the case of no hang
                                   ((OR (= (PKT-OPCODE IN-PKT) EOF-OP)
					(= (PKT-OPCODE IN-PKT) CLS-OP))
                                    (RETURN-PKT IN-PKT)    ;HIT EOF ON STREAM
                                    (SETQ IN-PKT 'EOF)     ;FLAG IT
                                    (RETURN (CAR REST)))   ;AND RETURN THE EOF VALUE
                                   ((>= (PKT-OPCODE IN-PKT) DAT-OP)
                                    (SETQ IN-BYTES (PKT-NBYTES IN-PKT)
                                          IN-I 0))
                                   (T (UNWIND-PROTECT
				        (FERROR NIL
                                    "~S is illegal packet opcode, received for connection ~S"
                                              (PKT-OPCODE IN-PKT) CONN)
				        (RETURN-PKT (PROG1 IN-PKT (SETQ IN-PKT NIL))))))))))))
      (:TYO (WITHOUT-INTERRUPTS
	      (COND ((NULL OUT-PKT)
		     (SETQ OUT-PKT (GET-PKT))
		     (SETF (PKT-OPCODE OUT-PKT) DAT-OP)
		     (SETQ OUT-I 0)))
	      (AS-1 ARG1 (PKT-STRING OUT-PKT) OUT-I)
	      (SETQ OUT-I (1+ OUT-I))
	      (COND ((>= OUT-I MAX-DATA-BYTES-PER-PKT)
		     (SETF (PKT-NBYTES OUT-PKT) OUT-I)
		     (SEND-PKT CONN (PROG1 OUT-PKT (SETQ OUT-PKT NIL)))))))
      (:STRING-OUT
	(WITHOUT-INTERRUPTS ;apparently a feeble attempt to make this stream work
			    ;for multiple processes sending output at the same time?
	  (DO ((START (OR (CAR REST) 0))
	       (END (OR (CADR REST) (ARRAY-ACTIVE-LENGTH ARG1)))
	       (AMT))
	      (( START END))
	    (COND ((NULL OUT-PKT)
		   (SETQ OUT-PKT (GET-PKT))
		   (SETF (PKT-OPCODE OUT-PKT) DAT-OP)
		   (SETQ OUT-I 0)))
	    (SETQ AMT (MIN (- END START) (- MAX-DATA-BYTES-PER-PKT OUT-I)))
	    (COPY-ARRAY-PORTION ARG1 START (SETQ START (+ START AMT))
				(PKT-STRING OUT-PKT) OUT-I (SETQ OUT-I (+ OUT-I AMT)))
	    (COND ((>= OUT-I MAX-DATA-BYTES-PER-PKT)
		   (SETF (PKT-NBYTES OUT-PKT) OUT-I)
		   (SEND-PKT CONN (PROG1 OUT-PKT (SETQ OUT-PKT NIL))))))))
      (:FORCE-OUTPUT
	(WITHOUT-INTERRUPTS
	  (COND (OUT-PKT
		  (SETF (PKT-NBYTES OUT-PKT) OUT-I)
		  (SEND-PKT CONN (PROG1 OUT-PKT (SETQ OUT-PKT NIL)))))))
      (:UNTYI (SETQ UNRCHF ARG1))
      (:EOF (STREAM-INTERNAL ':FORCE-OUTPUT)
            (SEND-PKT CONN (GET-PKT) EOF-OP)
            (FINISH CONN))
      (:FINISH (FINISH CONN))
      (:CLOSE (CLOSE CONN))
      (:CLEAR-EOF (AND (EQ IN-PKT 'EOF) (SETQ IN-PKT NIL)))
      (:WHICH-OPERATIONS
       '(:TYI :TYI-NO-HANG :UNTYI :TYO :STRING-OUT
	 :FORCE-OUTPUT :EOF :CLEAR-EOF :CLOSE :FINISH))
      (OTHERWISE
       (MULTIPLE-VALUE-CALL (STREAM-DEFAULT-HANDLER 'STREAM-INTERNAL OP ARG1 REST))))
   )

(DECLARE (UNSPECIAL CONN IN-PKT IN-BYTES IN-I OUT-PKT OUT-I))

(ENDF STREAM-HANDLER)

;; The HOSTAT function

;; Print the status of all the hosts and gateways, or specified ones
(DEFUN HOSTAT (&REST HOSTS &AUX CONNECTIONS PKT ADR)
  (UNWIND-PROTECT (PROGN
     (ASSURE-ENABLED)
     (DO L (OR HOSTS HOST-ALIST) (CDR L) (NULL L)	;Do all hosts, or specified hosts
       (SETQ ADR (COND ((NULL HOSTS) (CDAR L))	;Numeric address of this host
		       (T (OR (ADDRESS-PARSE (CAR L))
			      (FERROR NIL "Not a known Chaos address: ~S" (CAR L))))))
       (OR (ASSQ ADR CONNECTIONS)
	   (PUSH (CONS ADR (OPEN-CONNECTION ADR "STATUS" 1))
		 CONNECTIONS)))        		;Set up all connections in parallel
     ;; Now print heading
     (FORMAT T "~%~7A~25A" "Site" "Name//Status")
     (DO ((HEADS '("Subnet" "#-in" "#-out" "abort" "lost" "crc" "ram" "bitc" "other")
		 (CDR HEADS))
	  (WIDTHS '(6 9 9 8 8 8 4 5 6) (CDR WIDTHS)))
	 ((NULL HEADS) (TERPRI))
       (FORMAT T "~V@A" (CAR WIDTHS) (CAR HEADS)))
     ;; Now wait until connections come up with an answer, when they do print it out
     (DO () ((NULL CONNECTIONS))
       (COND ((FUNCALL TERMINAL-IO ':TYI-NO-HANG)
	      (RETURN "QUIT")))
       (PROCESS-ALLOW-SCHEDULE)				;Take a few chaos net interrupts
       (DO ((LIST CONNECTIONS (CDR LIST)))		;Check on each connection
	   ((NULL LIST))
	 (SELECTQ (STATE (CDAR LIST))
	   (RFC-SENT-STATE
	     (COND (( (TIME-DIFFERENCE (TIME) (TIME-LAST-RECEIVED (CDAR LIST)))
		     300.)		;5-second timeout
		    (FORMAT T "~O~7T~:[~;~1G~A   ~]host not responding~%"
			      (CAAR LIST) (CAR (RASSOC (CAAR LIST) HOST-ALIST)))
		    (REMOVE-CONN (CDAR LIST))
		    (SETQ CONNECTIONS (DELQ (CAR LIST) CONNECTIONS)))))
	   (ANSWERED-STATE				;This is what we want
	     (SETQ PKT (GET-NEXT-PKT (CDAR LIST)))
	     (HOSTAT-FORMAT-ANS (CAR LIST) PKT)
	     (RETURN-PKT PKT)
	     (CLOSE (CDAR LIST))
	     (SETQ CONNECTIONS (DELQ (CAR LIST) CONNECTIONS)))
	   (CLS-RECEIVED-STATE
	     (SETQ PKT (GET-NEXT-PKT (CDAR LIST)))
	     (FORMAT T "~O~7T~:[~;~1G~A   ~]returned a CLS:~A~%"
		       (CAAR LIST) (CAR (RASSOC (CAAR LIST) HOST-ALIST)) (PKT-STRING PKT))
	     (RETURN-PKT PKT)
	     (CLOSE (CDAR LIST))
	     (SETQ CONNECTIONS (DELQ (CAR LIST) CONNECTIONS)))
	   (OPEN-STATE
	     (FORMAT T "~O~7T~:[~;~1G~A   ~]returned an OPN~%"
		       (CAAR LIST) (CAR (RASSOC (CAAR LIST) HOST-ALIST)))
	     (CLOSE (CDAR LIST) "I expected an ANS, not an OPN.")
	     (SETQ CONNECTIONS (DELQ (CAR LIST) CONNECTIONS)))
	   (LOS-RECEIVED-STATE
	     (SETQ PKT (READ-PKTS-LAST (CDAR LIST)))
	     (FORMAT T "~O~7T~:[~;~1G~A   ~]returned a LOS:~A~%"
		       (CAAR LIST) (CAR (RASSOC (CAAR LIST) HOST-ALIST)) (PKT-STRING PKT))
	     (CLOSE (CDAR LIST))
	     (SETQ CONNECTIONS (DELQ (CAR LIST) CONNECTIONS)))
	   (OTHERWISE
	     (FORMAT T "~O~7T~:[~;~1G~A   ~]connection entered bad state:~A~%"
		       (CAAR LIST) (CAR (RASSOC (CAAR LIST) HOST-ALIST)) (STATE (CDAR LIST)))
	     (CLOSE (CDAR LIST))
	     (SETQ CONNECTIONS (DELQ (CAR LIST) CONNECTIONS)))))))
   ;; Unwind-protect cleanup
   (DO L CONNECTIONS (CDR L) (NULL L)	;Flush any connections that remain
       (REMOVE-CONN (CDAR L)))))

;Note host-name truncated to 27. characters to make more room for statistics
(DEFUN HOSTAT-FORMAT-ANS (HOST-CONN PKT
			  &AUX (NBYTES (PKT-NBYTES PKT)))
  (FORMAT T "~7@<~O ~>~27A"		;Print host number and name as returned
	    (CAR HOST-CONN)
	    (NSUBSTRING (PKT-STRING PKT) 0
			(MIN NBYTES 27. (OR (STRING-SEARCH-CHAR 0 (PKT-STRING PKT) 0 32.)
					    ;; This line is temporary! *******
					    (STRING-SEARCH-CHAR 200 (PKT-STRING PKT) 0 32.)
					    32.))))
  (HOSTAT-FORMAT-ANS-1 PKT 34. '(4 9 9 8 8 8 4 5 6)))

(DEFUN HOSTAT-FORMAT-ANS-1 (PKT START-COLUMN COLUMN-WIDTHS &AUX (NBYTES (PKT-NBYTES PKT)))
  (DO ((I 24. (+ I 2 CT))		;Now display subnet meters
       (FIRST-LINE T NIL)
       (ID) (CT)
       (MAXI (+ 8 (// NBYTES 2))))
      ((>= I MAXI) (AND FIRST-LINE (TERPRI)))
    (SETQ ID (AREF PKT I) CT (AREF PKT (1+ I)))	;Block header
    (OR FIRST-LINE (FORMAT T "~VA" START-COLUMN ""))
    (COND ((< ID 400)				;Subnet info (old 16-bit format)
	   (FORMAT T "~VO" (CAR COLUMN-WIDTHS) ID)
	   (DO ((J (+ I 2) (1+ J))		;Now print those meters that are present
		(L (CDR COLUMN-WIDTHS) (CDR L))
		(N (MIN CT 8) (1- N)))
	       ((ZEROP N))
	     (FORMAT T "~VD" (CAR L) (AREF PKT J))))
	  ((< ID 1000)				;Subnet info
	   (FORMAT T "~VO" (CAR COLUMN-WIDTHS) (- ID 400))
	   (DO ((J (+ I 2) (+ J 2))		;Now print those meters that are present
		(L (CDR COLUMN-WIDTHS) (CDR L))
		(N (MIN (// CT 2) 8) (1- N)))
	       ((ZEROP N))
	     (FORMAT T "~VD" (CAR L) (DPB (AREF PKT (1+ J)) 2020 (AREF PKT J)))))
	  (T					;I don't know about this
	   (FORMAT T "~O unknown info block ID" ID)))
    (TERPRI))) 

;; Random server and user routines

;; The infamous LIMERICK getter
(DEFUN LIMERICK ( &OPTIONAL (ARGS "") &AUX STREAM CONN)
    (AND (NUMBERP ARGS) (SETQ ARGS (FORMAT NIL "~D" ARGS)))
    (TERPRI) (TERPRI) 
    (SETQ CONN (CONNECT "MC" (STRING-APPEND "LIMERICK " ARGS)))
    (COND ((STRINGP CONN) CONN)
	  (T (SETQ STREAM (STREAM CONN))
	     (DO CH (FUNCALL STREAM 'TYI) (FUNCALL STREAM 'TYI) (NULL CH)
		 (TYO CH))
             (CLOSE CONN "")
	     NIL)))


;;; This function sets up so that all requests for the service indicated
;;; by the contact name given will be forwarded to the indicated host
(DEFUN FORWARD-ALL (CONTACT-NAME HOST)
    (SETQ HOST (ADDRESS-PARSE HOST))
    (PUSH (CONS CONTACT-NAME
	    `(PROG (CONN)
		(SETQ CONN (LISTEN ,CONTACT-NAME))
		(COND ((EQ (STATE CONN) 'RFC-RECEIVED-STATE)
		       (FORWARD CONN (GET-NEXT-PKT CONN) ,HOST))
		      (T ;; Lost in some way
		         (REMOVE-CONN CONN)))))
	  SERVER-ALIST)
    NIL)

;;; Network message facility

(DEFVAR SEND-BELLCOUNT 2)	;How many times to ring the bell when a message arrives
(DEFVAR SAVED-SENDS		;Extensible string containing all messages.
	(MAKE-ARRAY NIL 'ART-STRING 100 NIL '(0)))

;USER
(DEFMACRO QSEND (DESTINATION &OPTIONAL MESSAGE)
  `(SEND-MSG ',DESTINATION ',MESSAGE))

(DEFUN PRINT-SENDS (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (PRINC SAVED-SENDS STREAM)
  T)

(DEFVAR POP-UP-QSEND-WINDOW)
(DEFVAR POP-UP-QSEND-LOCK NIL)	;One guy typing at a time
(DEFUN POP-UP-RECEIVE-SEND-MSG (&AUX CONN STREAM RECIPIENT RFC SENDER VISP TEM
				     (START (ARRAY-ACTIVE-LENGTH SAVED-SENDS))
				     (FORMAT:FORMAT-STRING SAVED-SENDS))
  (UNWIND-PROTECT
    (PROGN
      (SETQ CONN (LISTEN "SEND"))
      (COND ((EQ (STATE CONN) 'RFC-RECEIVED-STATE)
	     (SETQ RFC (PKT-STRING (READ-PKTS CONN)))
	     (SETQ RECIPIENT
		   (COND ((SETQ TEM (STRING-SEARCH " " RFC))
			  (NSUBSTRING RFC (1+ TEM)))
			 (T "anyone")))
	     (FORMAT 'FORMAT:FORMAT-STRING-STREAM "~%[Message from ~A for ~A]~%"
		     (HOST-DATA (FOREIGN-ADDRESS CONN))
		     RECIPIENT)
	     (ACCEPT CONN)
	     (SETQ STREAM (STREAM CONN))
	     (SETQ SENDER (FUNCALL STREAM ':LINE-IN))
	     (FORMAT:FORMAT-STRING-STREAM ':LINE-OUT SENDER)
	     (COND ((SETQ TEM (STRING-SEARCH-CHAR #/@ SENDER))
		    (SETQ SENDER (NSUBSTRING SENDER 0 TEM)))
		   ((SETQ TEM (STRING-SEARCH "from " SENDER))
		    (SETQ SENDER (NSUBSTRING SENDER (+ TEM 5)
				     (STRING-SEARCH-SET '(#/] #\SP) SENDER (+ TEM 5)))))
		   (T
		    (SETQ SENDER "")))
	     (SETQ SENDER (STRING-APPEND SENDER #/@
					 (CAR (RASSOC (FOREIGN-ADDRESS CONN) HOST-ALIST))))
	     (STREAM-COPY-UNTIL-EOF STREAM 'FORMAT:FORMAT-STRING-STREAM)
	     (CLOSE CONN)
	     (ARRAY-PUSH-EXTEND SAVED-SENDS #\CR)
	     (ARRAY-PUSH-EXTEND SAVED-SENDS #\CR)
	     (OR (BOUNDP 'POP-UP-QSEND-WINDOW)
		 (SETQ POP-UP-QSEND-WINDOW TV:(WINDOW-CREATE 'POP-UP-TEXT-WINDOW
							     ':NAME "QSend"
							     ':HEIGHT 400 ;About 17 lines
							     ':SAVE-BITS T)))
	     ;; Ring the bell before the real-time delay of popping it up.
	     ;; Also ring it before locking so he knows another message came in.
	     (DOTIMES (I SEND-BELLCOUNT) (FUNCALL POP-UP-QSEND-WINDOW ':BEEP))
	     ;; This waits until any other message is done
	     ;; then seizes the lock.  The effect is to handle only one message at a time.
	     (DO ((FIRST-TIME T NIL))
		 ((%STORE-CONDITIONAL (VALUE-CELL-LOCATION 'POP-UP-QSEND-LOCK)
					 NIL CURRENT-PROCESS))
	       ;; If messages coming in with thing hung up, let user know
	       (AND FIRST-TIME (NEQ (FUNCALL POP-UP-QSEND-WINDOW ':STATUS) ':SELECTED)
		    (FORMAT (TV:GET-NOTIFICATION-STREAM)
			    "~&[Message from ~A waiting for QSend window]~%" SENDER))
	       (PROCESS-WAIT "Lock" #'(LAMBDA (X) (NULL (CDR X)))
			     (VALUE-CELL-LOCATION 'POP-UP-QSEND-LOCK)))
	     (SETQ VISP (TV:SHEET-EXPOSED-P POP-UP-QSEND-WINDOW))
	     (FUNCALL POP-UP-QSEND-WINDOW ':MOUSE-SELECT)
	     ;; If the window was not already visible, erase it.
	     (COND ((NOT VISP)
		    (SETQ START (1+ START))	;Skip the first CR
		    (FUNCALL POP-UP-QSEND-WINDOW ':CLEAR-SCREEN)))
	     (FUNCALL POP-UP-QSEND-WINDOW ':STRING-OUT SAVED-SENDS START)))
      (REMOVE-CONN CONN)
      (COND (SENDER
	     (LET ((TERMINAL-IO POP-UP-QSEND-WINDOW) ERR)
	       (FUNCALL POP-UP-QSEND-WINDOW ':CLEAR-INPUT)
	       (COND ((Y-OR-N-P "Reply? " POP-UP-QSEND-WINDOW)
		      (FORMAT POP-UP-QSEND-WINDOW "~&To: ~A" SENDER)
		      (COND ((SETQ ERR (SEND-MSG SENDER))
			     (FORMAT POP-UP-QSEND-WINDOW
				     "~&~A -- type space to continue" ERR)
			     (FUNCALL POP-UP-QSEND-WINDOW ':CLEAR-INPUT)
			     (FUNCALL POP-UP-QSEND-WINDOW ':TYI))))))
	     ;; We are done with the window
	     (SETQ POP-UP-QSEND-LOCK NIL)
	     (PROCESS-ALLOW-SCHEDULE)
	     (COND ((NULL POP-UP-QSEND-LOCK)
		    (FUNCALL POP-UP-QSEND-WINDOW ':DESELECT T)
		    (FUNCALL POP-UP-QSEND-WINDOW ':DEACTIVATE))))))
    (AND (EQ POP-UP-QSEND-LOCK CURRENT-PROCESS) (SETQ POP-UP-QSEND-LOCK NIL))))

(ADD-INITIALIZATION "SEND"
                    '(PROCESS-RUN-FUNCTION "SEND" (FUNCTION POP-UP-RECEIVE-SEND-MSG))
                    NIL
                    'SERVER-ALIST)

(DEFUN SEND-MSG (DESTINATION &OPTIONAL MSG &AUX HOST PERSON CONN STREAM)
  (COND ((AND (NOT (NUMBERP DESTINATION))
	      (SETQ HOST (DO ((@-POS (STRING-SEARCH "@" DESTINATION)
				     (STRING-SEARCH "@" DESTINATION (1+ @-POS)))
			      (LAST-@-POS NIL @-POS))
			     ((NULL @-POS) LAST-@-POS))))
         (SETQ PERSON (STRING-UPCASE (NSUBSTRING DESTINATION 0 HOST))
               HOST (NSUBSTRING DESTINATION (1+ HOST) (STRING-LENGTH DESTINATION))))
        (T (SETQ PERSON "anyone"  HOST DESTINATION)))
  (FS:FORCE-USER-TO-LOGIN)
  (COND ((NULL MSG)
	 (FORMAT T "~%Message: (terminate with End)~%")
	 (SETQ MSG (SEND-MSG-GET-MESSAGE))))
  (SETQ CONN (CONNECT HOST (STRING-APPEND "SEND " PERSON)))
  (COND ((STRINGP CONN) CONN) ;err msg
	(T (SETQ STREAM (STREAM CONN))
	   (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS)
	       (TIME:GET-TIME)
	     (FORMAT STREAM "[Message from ~A~:[~; at ~D:~2,'0D:~2,'0D~]]~%"
			    USER-ID SECONDS HOURS MINUTES SECONDS))
           (PRINC MSG STREAM)
           (FUNCALL STREAM ':EOF)   ;DOES A :FORCE-OUTPUT, SENDS AN EOF-OP
	   (CLOSE CONN "")
	   NIL)))

(DEFUN SEND-MSG-GET-MESSAGE (&OPTIONAL (STREAM STANDARD-INPUT))
  (IF (AND (NOT RUBOUT-HANDLER)
	   (MEMQ ':RUBOUT-HANDLER (FUNCALL STREAM ':WHICH-OPERATIONS)))
      (FUNCALL STREAM ':RUBOUT-HANDLER
	       '((:PASS-THROUGH #\END #/c #/C)) #'SEND-MSG-GET-MESSAGE STREAM)
      (DO ((MSG (MAKE-ARRAY NIL 'ART-STRING 100 NIL '(0)))
	   (CH))
	  (NIL)
	(SETQ CH (FUNCALL STREAM ':TYI))
	(AND (MEMQ CH '(#\END #/c #/C NIL))
	     (RETURN MSG))
	(ARRAY-PUSH-EXTEND MSG CH))))

;;; Finger server and NAME user end

(ADD-INITIALIZATION "FINGER" '(GIVE-FINGER) NIL 'SERVER-ALIST)

(DEFVAR GIVE-FINGER-SAVED-STRING NIL)
(DEFVAR GIVE-FINGER-SAVED-IDLE NIL)
(DEFVAR GIVE-FINGER-SAVED-USER-ID NIL)

;This runs in the background task now.
(DEFUN GIVE-FINGER (&AUX IDLE)
  (SETQ IDLE (// (TIME-DIFFERENCE (TIME) TV:KBD-LAST-ACTIVITY-TIME) 3600.)) ;Minutes
  ;; Making the string is expensive in terms of paging, and it is almost
  ;; always the same as last time.  So try to use a saved string.
  (COND ((OR (NEQ GIVE-FINGER-SAVED-IDLE IDLE)
	     (NEQ GIVE-FINGER-SAVED-USER-ID USER-ID))
	 (SETQ GIVE-FINGER-SAVED-IDLE IDLE
	       GIVE-FINGER-SAVED-USER-ID USER-ID
	       GIVE-FINGER-SAVED-STRING
	          (FORMAT NIL "~A~%~A~%~:[~3*~;~:[~D:~2,48D~;~*~D~]~]~%~A~%~C~%"
                          USER-ID
                          MY-FINGER-LOCATION-STRING
                          (NOT (ZEROP IDLE))
                          (ZEROP (// IDLE 60.))
                          (// IDLE 60.)
                          (\ IDLE 60.)
			  FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST
			  FS:USER-GROUP-AFFILIATION))))
  (FAST-ANSWER-STRING "FINGER" GIVE-FINGER-SAVED-STRING))

;; This can't run in the background process, since it uses a full byte-stream
;; connection, which requires retransmission, which is done by the background process.
(ADD-INITIALIZATION "NAME" '(PROCESS-RUN-FUNCTION "Name" 'GIVE-NAME) NIL 'SERVER-ALIST)

(DEFUN GIVE-NAME (&AUX CONN IDLE)
  (SETQ CONN (LISTEN "NAME"))
  (COND ((EQ (STATE CONN) 'RFC-RECEIVED-STATE)
	 (SETQ IDLE (// (TIME-DIFFERENCE (TIME) TV:KBD-LAST-ACTIVITY-TIME) 3600.)) ;Minutes
	 (FORMAT-AND-EOF
	   CONN
	   "~6A ~C ~22A ~6A ~:[    ~3*~;~:[~D:~2,48D~;  ~*~D~]~]     ~A"
	   USER-ID
	   FS:USER-GROUP-AFFILIATION
	   FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST
	   MY-NAME-STRING
	   (NOT (ZEROP IDLE))
	   (ZEROP (// IDLE 60.))
	   (// IDLE 60.)
	   (\ IDLE 60.)
	   MY-FINGER-LOCATION-STRING))
	(T (REMOVE-CONN CONN))))

;; Send the specied format string, and eof and close
(DEFUN FORMAT-AND-EOF (CONN &REST FORMAT-ARGS)
  (ACCEPT CONN)
  (LET ((STREAM (STREAM CONN)))
    (LEXPR-FUNCALL #'FORMAT STREAM FORMAT-ARGS)
    (FUNCALL STREAM ':EOF)
    (FUNCALL STREAM ':CLOSE))
  (REMOVE-CONN CONN))

;;; This is needed so that calls to FINGER can be compiled
(DEFPROP FINGER
	 COMPILER:((1 (FEF-ARG-OPT FEF-QT-QT))
		   (1 (FEF-ARG-OPT FEF-QT-EVAL)))
	 COMPILER:ARGDESC)

(DEFUN FINGER (&OPTIONAL &QUOTE (SPEC "@AI") &EVAL (STREAM STANDARD-OUTPUT)
	       &AUX HOST GATEWAY-P)
  (SETQ SPEC (STRING SPEC))
  (COND ((SETQ HOST (STRING-SEARCH-CHAR #/@ SPEC))
	 (SETQ HOST (SUBSTRING SPEC (1+ HOST)))
	 (OR (ASSOC HOST HOST-ALIST)		;Go directly if host is on ChaosNet
	     (SETQ HOST NIL GATEWAY-P T)))	;Else use default host
	(T (SETQ HOST "AI")))			;No explicit host, use AI
  (AND HOST (SETQ HOST (ADDRESS-PARSE HOST)))
  (SETQ SPEC (STRING-APPEND "NAME " SPEC))
  (DO ((DEFAULTS '("MC" "AI") (CDR DEFAULTS))
       (CONN))
      ((NULL DEFAULTS)
       "No host available")
    (SETQ CONN (CONNECT (OR HOST (CAR DEFAULTS)) SPEC))
    (COND ((STRINGP CONN)			;If attempt to connect failed
	   (AND HOST				;If explicit host, return reason for failure
		(RETURN CONN)))			; else try next default
	  (T					;Have connection, just transfer the info
	    (FORMAT STREAM "~&")
	    (COND ((NOT GATEWAY-P)
		   (STREAM-COPY-UNTIL-EOF (STREAM CONN) STREAM))
		  (T	;If going through a gateway, character set is ascii and has
		        ;to be translated.
		    (DO ((ISTREAM (STREAM CONN))
			 (CH))
			(NIL)
		     IGNORE
		      (SETQ CH (FUNCALL ISTREAM ':TYI))
		      (OR CH (RETURN NIL))
		      (SELECTQ CH
			(11 (SETQ CH #\TAB))
			(12 (GO IGNORE))
			(15 (SETQ CH #\CR)))
		      (FUNCALL STREAM ':TYO CH))))
	    (CLOSE CONN)
	    (REMOVE-CONN CONN)
	    (RETURN NIL)))))

(DEFUN FINGER-ALL-LMS (STREAM &OPTIONAL PRINT-FREE &AUX CONNS FREE)
  (DOLIST (HOST HOST-ALIST)
    (AND (STRING-EQUAL "CADR" (CAR HOST) 0 0 4 4)
	 (PUSH (OPEN-CONNECTION (CDR HOST) "FINGER") CONNS)))
  (DO ((OLD-TIME (TIME)))
      (NIL)
    (DOLIST (CONN CONNS)
      (LET ((STATE (STATE CONN)))
	(COND ((NEQ STATE 'RFC-SENT-STATE)	;Still waiting
	       (AND (EQ STATE 'ANSWERED-STATE)	;Got something meaningful
		    (LET ((PKT (GET-NEXT-PKT CONN)))
		      (LET ((STR (PKT-STRING PKT))
			    (HOST-NAME (CAR (RASSOC (FOREIGN-ADDRESS CONN) HOST-ALIST)))
			    IDX)
			(COND (( (AREF STR 0) #\CR)	;Logged in
			       (FORMAT STREAM
				 "~&~6A ~4G~C ~3G~22A CADR-~5G~2A ~2G~4@A    ~1G~A~%"
				 (NSUBSTRING STR 0
					     (SETQ IDX (STRING-SEARCH-CHAR #\CR STR)))
				 (NSUBSTRING STR (SETQ IDX (1+ IDX))
					     (SETQ IDX (STRING-SEARCH-CHAR #\CR STR IDX)))
				 (NSUBSTRING STR (SETQ IDX (1+ IDX))
					     (SETQ IDX (STRING-SEARCH-CHAR #\CR STR IDX)))
				 (NSUBSTRING STR (SETQ IDX (1+ IDX))
					     (SETQ IDX (STRING-SEARCH-CHAR #\CR STR IDX)))
				 (AREF STR (1+ IDX))
				 (NSUBSTRING HOST-NAME 4)))
			      (PRINT-FREE
			       (PUSH (SUBSTRING STR 1 (STRING-SEARCH-CHAR #\SP STR 1)) FREE)
			       (PUSH HOST-NAME FREE))))
		      (RETURN-PKT PKT)))
	       (SETQ CONNS (DELQ CONN CONNS))
	       (CLOSE CONN)
	       (REMOVE-CONN CONN)))))
    (OR CONNS (RETURN NIL))			;Done with all of them
    (AND (> (TIME-DIFFERENCE (TIME) OLD-TIME) 240.)	;Allow 5 secs for this all
	 (RETURN NIL))
    (PROCESS-WAIT "Finger"
		  #'(LAMBDA (OLD-TIME CONNS)
		      (OR (> (TIME-DIFFERENCE (TIME) OLD-TIME) 240.)
			  (DO ((CONNS CONNS (CDR CONNS)))
			      ((NULL CONNS) NIL)
			    (OR (EQ (STATE (CAR CONNS)) 'RFC-SENT-STATE)
				(RETURN T)))))
		  OLD-TIME CONNS))
  ;; Flush all outstanding connections
  (DOLIST (CONN CONNS)
    (REMOVE-CONN CONN))
  (AND PRINT-FREE
       (FORMAT STREAM (IF (NULL FREE) "~&No Free Lisp machines~%"
			  "~&Free Lisp machines: ~{~<~%~5X~2:;~A ~A~>~^, ~}")
	       FREE)))

;;; Dummy mail server, rejects all incoming mail
(DEFUN DUMMY-MAIL-SERVER (&AUX CONN STREAM RCPT)
  (SETQ CONN (LISTEN "MAIL"))
  (COND ((EQ (STATE CONN) 'RFC-RECEIVED-STATE)
	 (ACCEPT CONN)
	 (SETQ STREAM (STREAM CONN))
	 (*CATCH 'DONE
	   (CONDITION-BIND (((READ-ON-CLOSED-CONNECTION LOS-RECEIVED-STATE HOST-DOWN)
			     #'(LAMBDA (&REST IGNORE) (*THROW 'DONE NIL))))
	     (DO () (NIL)			;Read the rcpts
	       (SETQ RCPT (FUNCALL STREAM ':LINE-IN NIL))
	       (AND (ZEROP (STRING-LENGTH RCPT))	;Blank line = start text
		    (RETURN))
	       (FUNCALL STREAM ':LINE-OUT
		 "-Lisp Machines do not accept mail, maybe you want the :LMSEND command."))))
	 (CLOSE CONN "all rcpts read")))
  (REMOVE-CONN CONN))

(ADD-INITIALIZATION "MAIL" '(PROCESS-RUN-FUNCTION "MAIL-SERVER" 'DUMMY-MAIL-SERVER)
		    NIL 'SERVER-ALIST)

;;; Remote disk facilities.

(ADD-INITIALIZATION "REMOTE-DISK"
		    '(PROCESS-RUN-FUNCTION "Remote-Disk" #'REMOTE-DISK-SERVER)
		    NIL 'SERVER-ALIST)

(DEFUN REMOTE-DISK-SERVER (&AUX CONN STREAM LINE CMD CMDX UNIT BLOCK N-BLOCKS RQB
				BLOCK-PKT-1 BLOCK-PKT-2 BLOCK-PKT-3)
  (ERRSET (PROGN  ;If an error happens, don't ruin the machine.  (SETQ ERRSET T) to debug
    (SETQ CONN (LISTEN "REMOTE-DISK" 25.))
    (ACCEPT CONN)
    (SETQ STREAM (STREAM CONN))
    (DO () (NIL)
      (PROCESS-WAIT "NETI" #'(LAMBDA (CONN) (OR (READ-PKTS CONN)
						(NEQ (STATE CONN) 'OPEN-STATE)))
			   CONN)
      (AND (NEQ (STATE CONN) 'OPEN-STATE) (RETURN NIL))
      (SETQ LINE (READLINE STREAM)		;Get a command line
	    CMDX (STRING-SEARCH-CHAR #\SP LINE)
	    CMD (SUBSTRING LINE 0 CMDX))
      (COND ((OR (STRING-EQUAL CMD "READ") (STRING-EQUAL CMD "WRITE"))
	     (LET ((IBASE 10.)
		   (SI:*IOLST LINE)
		   (SI:*IOCH CMDX))
	       (SETQ UNIT (READ #'SI:READ-FROM-STRING-STREAM)
		     BLOCK (READ #'SI:READ-FROM-STRING-STREAM)
		     N-BLOCKS (READ #'SI:READ-FROM-STRING-STREAM)
		     RQB NIL))
	     (UNWIND-PROTECT
	       (PROGN (SETQ RQB (GET-DISK-RQB N-BLOCKS)
			    BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T)
			    BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T)
			    BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T))
		      (COND ((STRING-EQUAL CMD "READ")
			     (DISK-READ RQB UNIT BLOCK)
			     ;; Give to net
			     (DO ((BLOCK BLOCK (1+ BLOCK))
				  (N-BLOCKS N-BLOCKS (1- N-BLOCKS)))
				 ((ZEROP N-BLOCKS))
			       ;; Transmit three packets from block in buffer
			       (SI:TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-1)
			       (SI:TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-2)
			       (SI:TRANSMIT-PARTITION-PACKET CONN BLOCK-PKT-3)
			       ;; Advance magic strings to next block
			       (%P-STORE-CONTENTS-OFFSET
				 (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3) (* 4 PAGE-SIZE))
				 BLOCK-PKT-1 3)
			       (%P-STORE-CONTENTS-OFFSET
				 (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3) (* 4 PAGE-SIZE))
				 BLOCK-PKT-2 3)
			       (%P-STORE-CONTENTS-OFFSET
				 (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3) (* 4 PAGE-SIZE))
				 BLOCK-PKT-3 3)))
			    (T
			     ;; Get from net
			     (DO ((BLOCK BLOCK (1+ BLOCK))
				  (N-BLOCKS N-BLOCKS (1- N-BLOCKS)))
				 ((ZEROP N-BLOCKS))
			       ;; Get 3 packets and form a block in the buffer
			       ;; RECEIVE-PARTITION-PACKET will throw if it gets to eof.
			       (SI:RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-1)
			       (SI:RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-2)
			       (SI:RECEIVE-PARTITION-PACKET CONN BLOCK-PKT-3)
			       ;; Advance magic strings to next block
			       (%P-STORE-CONTENTS-OFFSET
				 (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3) (* 4 PAGE-SIZE))
				 BLOCK-PKT-1 3)
			       (%P-STORE-CONTENTS-OFFSET
				 (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3) (* 4 PAGE-SIZE))
				 BLOCK-PKT-2 3)
			       (%P-STORE-CONTENTS-OFFSET
				 (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3) (* 4 PAGE-SIZE))
				 BLOCK-PKT-3 3))
			     (DISK-WRITE RQB UNIT BLOCK))))
	       (AND BLOCK-PKT-3 (RETURN-ARRAY BLOCK-PKT-3))
	       (AND BLOCK-PKT-2 (RETURN-ARRAY BLOCK-PKT-2))
	       (AND BLOCK-PKT-1 (RETURN-ARRAY BLOCK-PKT-1))
	       (RETURN-DISK-RQB RQB)))
	    ((STRING-EQUAL CMD "SAY")
	     (FORMAT (TV:GET-NOTIFICATION-STREAM)
		     "~&[REMOTE-DISK-SERVER:~A]~%" (SUBSTRING LINE CMDX))
	     (TV:BEEP))))))
  (AND CONN (REMOVE-CONN CONN)))

;; Useful information gatherers

;HOST-DATA: returns information about a specified host.  Currently,
; returns name of machine as primary value and host number as second value
(DEFUN HOST-DATA (&OPTIONAL (HOST MY-ADDRESS))
  (PROG (HOST-ADDRESS HOST-DATA)    ;PROG NEEDED TO DO MULTIPLE VALUE RETURN
	(OR (SETQ HOST-ADDRESS (ADDRESS-PARSE HOST))
	    (FERROR NIL "~S is an illegal host specification" HOST))
	(AND (SETQ HOST-DATA (ASSQ HOST-ADDRESS KNOWN-NAME-ALIST))
	     (RETURN (CDR HOST-DATA) (CAR HOST-DATA)))
	(AND (SETQ HOST-DATA (RASSOC HOST-ADDRESS HOST-ALIST))
	     (RETURN (CAR HOST-DATA) (CDR HOST-DATA)))
	(OR (SETQ HOST-DATA (GET-HOST-STATUS-PACKET HOST-ADDRESS))
	    (RETURN "Unknown" HOST-ADDRESS))
	(RETURN (PROG1 (NSUBSTRING (PKT-STRING HOST-DATA) 0
			(MIN (PKT-NBYTES HOST-DATA)
                             32.
                             (OR (STRING-SEARCH-CHAR 0 (PKT-STRING HOST-DATA)) 32.)))
		       (RETURN-PKT HOST-DATA))
		HOST-ADDRESS)))

;Returns a STATUS packet from the specified host or NIL if couldn't get the packet
(DEFUN GET-HOST-STATUS-PACKET (HOST &AUX CONNECTION PKT ADR)
  (ASSURE-ENABLED)
  (SETQ ADR (OR (ADDRESS-PARSE HOST)
		(FERROR NIL "Not a known Chaos address: ~S" HOST)))
  (SETQ CONNECTION (OPEN-CONNECTION ADR "STATUS" 1))
  (DO () ((NULL CONNECTION))
      (PROCESS-SLEEP 10.)				;Take a few chaos net interrupts
      (SELECTQ (STATE CONNECTION)
	(RFC-SENT-STATE
	  (COND (( (TIME-DIFFERENCE (TIME) (TIME-LAST-RECEIVED CONNECTION))
		    300.)		;5-second timeout
		 (REMOVE-CONN CONNECTION)
		 (RETURN NIL))))
	(ANSWERED-STATE				;This is what we want
	  (SETQ PKT (GET-NEXT-PKT CONNECTION))
	  (CLOSE CONNECTION)
	  (RETURN PKT))
	(CLS-RECEIVED-STATE (CLOSE CONNECTION) (RETURN NIL))
	(OPEN-STATE
	  (CLOSE CONNECTION "I expected an ANS, not an OPN.")
	  (RETURN NIL))
	(LOS-RECEIVED-STATE
	  (CLOSE CONNECTION)
	  (RETURN NIL))
	(OTHERWISE
	  (CLOSE CONNECTION)
	  (RETURN NIL)))))

(DEFVAR EVAL-SERVER-ON NIL)

;Call this if you want to enable the eval server on your machine
(DEFUN EVAL-SERVER-ON ()
  (SETQ EVAL-SERVER-ON T))

(DEFUN EVAL-SERVER-FUNCTION (&AUX CONN STREAM)
  (SETQ CONN (LISTEN "EVAL"))
  (COND ((OR EVAL-SERVER-ON (EQUAL USER-ID "")(NULL USER-ID))
	 (ACCEPT CONN)
	 (SETQ STREAM (STREAM CONN))
	 (ERRSET (DO ((TERMINAL-IO STREAM)	;Don't blow away machine on lossage
		      (INPUT))
		     (NIL)
		   (AND (EQ (SETQ INPUT (READ STREAM 'QUIT)) 'QUIT)
			(RETURN NIL))
		   (PRINT (ERRSET (EVAL INPUT) NIL) STREAM)
		   (TERPRI STREAM)
		   (FUNCALL STREAM ':FORCE-OUTPUT))
		 NIL)
	 (REMOVE-CONN CONN))
	(T (REJECT CONN (FORMAT NIL "This machine is in use by ~A" USER-ID)))))

(ADD-INITIALIZATION "EVAL"
                    '(PROCESS-RUN-FUNCTION "EVAL" (FUNCTION EVAL-SERVER-FUNCTION))
                    NIL
                    'SERVER-ALIST)


(DEFVAR PACKET-HEADER-ARRAY (MAKE-ARRAY NIL 'ART-16B 8.))
(DEFVAR NUMBER-OF-16B-WORDS (1- MAX-DATA-WORDS-PER-PKT))
(DEFVAR MAX-SCREEN-IDX (// (+ (// (* 1400 1600) 20) (1- NUMBER-OF-16B-WORDS))
			   NUMBER-OF-16B-WORDS))
(DEFVAR SCREEN-ARRAY (MAKE-ARRAY NIL 'ART-16B
				 (* MAX-SCREEN-IDX NUMBER-OF-16B-WORDS)
				 (SCREEN-BUFFER MAIN-SCREEN)))
(DEFVAR CURRENT-POINT 0)

(DEFUN SPY-AT-CLOCK-LEVEL (IGNORE)
  (LET ((IP (LET ((FREE-LIST (INT-FREE-LIST)))
	      (COND ((NULL FREE-LIST) NIL)
		    ((%STORE-CONDITIONAL INT-FREE-LIST-POINTER
					 FREE-LIST (INT-PKT-THREAD FREE-LIST))
		     FREE-LIST))))
;	(RN (RANDOM MAX-SCREEN-IDX))
	(RN (SETQ CURRENT-POINT (\ (1+ CURRENT-POINT) MAX-SCREEN-IDX))))
    (COND (IP (SETF (INT-PKT-THREAD IP) NIL)
	      (COPY-ARRAY-PORTION PACKET-HEADER-ARRAY 0 8 IP 0 8)
	      (ASET RN IP 8)
	      (COPY-ARRAY-PORTION SCREEN-ARRAY (* RN NUMBER-OF-16B-WORDS)
				  (* (1+ RN) NUMBER-OF-16B-WORDS)
				  IP 9. (+ 9. NUMBER-OF-16B-WORDS))
	      (TRANSMIT-INT-PKT IP)))))

(DEFUN SPY-START ()
  (LET ((CONN (LISTEN "SPY")))
    (ACCEPT CONN)
    (ASET (LSH UNC-OP 8) PACKET-HEADER-ARRAY 0)
    (ASET MAX-DATA-BYTES-PER-PKT PACKET-HEADER-ARRAY 1)
    (ASET (FOREIGN-ADDRESS CONN) PACKET-HEADER-ARRAY 2)
    (ASET (FOREIGN-INDEX-NUM CONN) PACKET-HEADER-ARRAY 3)
    (ASET MY-ADDRESS PACKET-HEADER-ARRAY 4)
    (ASET (LOCAL-INDEX-NUM CONN) PACKET-HEADER-ARRAY 5)
    (ASET 0 PACKET-HEADER-ARRAY 6)
    (ASET 0 PACKET-HEADER-ARRAY 7)
    (PUSH 'SPY-AT-CLOCK-LEVEL SI:CLOCK-FUNCTION-LIST)
    (WAIT CONN 'OPEN-STATE 100000000)
    (SETQ SI:CLOCK-FUNCTION-LIST (DELQ 'SPY-AT-CLOCK-LEVEL SI:CLOCK-FUNCTION-LIST))
    (REMOVE-CONN CONN)))

(ADD-INITIALIZATION "SPY"
                    '(PROCESS-RUN-FUNCTION "PEEK-A-BOO" 'SPY-START)
                    NIL
                    'SERVER-ALIST)

(COMMENT Debugging Stuff)

(DEFUN CD-SEND ( &AUX PKT)
    (DISABLE)
    (SETQ PKT (ALLOCATE-PKT))
    (TERPRI) (TERPRI)
    (SETF (PKT-OPCODE PKT) (CD-GET "Opcode: "))
    (SETF (PKT-DEST-ADDRESS PKT) (CD-GET "Destination: "))
    (SETF (PKT-DEST-INDEX-NUM PKT) (CD-GET "Destination index: "))
    (SETF (PKT-SOURCE-ADDRESS PKT) (CD-GET "Source address: "))
    (SETF (PKT-SOURCE-INDEX-NUM PKT) (CD-GET "Source index: "))
    (SETF (PKT-NUM PKT) (CD-GET  "Packet number: "))
    (SETF (PKT-ACK-NUM PKT) (CD-GET "Ack packet number: "))
    (SETF (PKT-FWD-COUNT PKT) (CD-GET "Forwarding count: "))
    (SET-PKT-STRING PKT (CD-GET "Data (a string):"))
    (TRANSMIT-INT-PKT (CONVERT-TO-INT-PKT PKT))
    (FREE-PKT PKT))

(DEFUN CD-GET (PROMPT &AUX X)
    (PRINC PROMPT)
    (SETQ X (EVAL (READ)))
    (TERPRI)
    X)

(DEFUN CD-RECEIVE ( &AUX PKT)
    (DISABLE)
    (SETQ PKT (CONVERT-TO-PKT (RECEIVE-PROCESS-NEXT-INT-PKT)))
    (COND (PKT (PRINT-PKT PKT))
	  (T NIL)))

(DEFUN SOAK (CONN &AUX PKT)
    (AND (NUMBERP CONN) (SETQ CONN (AR-1 INDEX-CONN CONN)))
    (SETQ PKT (GET-NEXT-PKT CONN))
    (COND ((= (PKT-OPCODE PKT) CLS-OP)
	   (FORMAT T "==> CLOSED!!! <===  ~S" (PKT-STRING PKT)))
	  (PKT
	   (PRINT-PKT PKT)))
    (AND PKT (FREE-PKT PKT)))

(SPECIAL C L)
(DEFUN SETUP (&OPTIONAL (CNAME "FOO"))
   (ENABLE)
   (SETQ C (OPEN-CONNECTION MY-ADDRESS CNAME))
   (SETQ L (LISTEN CNAME))
   (WAIT L 'LISTENING-STATE (* 10. 60.))
   (IF (EQ (STATE L) 'LISTENING-STATE)
       (FORMAT T "Lost")
       (ACCEPT L))
   (PEEK 'K))

;;; Called By PEEK.

(DECLARE (SPECIAL PEEK-SHORT-PKT-DISPLAY))
(SETQ PEEK-SHORT-PKT-DISPLAY T)	;Display packets in short form in peek

(DEFUN PEEK-DISPLAY ( &AUX CONN)
    (FORMAT T "~&ChaosNet Status: ~O" (TIME))
    (FORMAT T (COND (ENABLE "   Active!~%")
		    (T "   Deactivated.~%")))
    (DO I 0 (1+ I) (>= I (ARRAY-LENGTH INDEX-CONN))
      (COND ((ARRAYP (SETQ CONN (AR-1 INDEX-CONN I)))
             (PRINT-CONN CONN PEEK-SHORT-PKT-DISPLAY)
	     (TERPRI))))
    (FORMAT T "~2%Forwarded: ~O  Overforwarded: ~O  Lost: ~O   Made: ~O  Free: ~O (+~O Recorded LOS packets)~%"
	    PKTS-FORWARDED PKTS-OVER-FORWARDED PKTS-LOST PKTS-MADE
	    (DO ((I 0 (1+ I))
		 (FP FREE-PKTS (PKT-LINK FP)))
		((SYMBOLP FP) I))
	    CURRENT-LOS-PKT-COUNT)
    (FORMAT T "Bad Destination: ~O   Bad Bit Count: ~O   Bad CRC-1: ~O  Bad CRC-2: ~O~%"
	    PKTS-BAD-DEST PKTS-BAD-BIT-COUNT PKTS-BAD-CRC-1 PKTS-BAD-CRC-2)
    (COND (PENDING-LISTENS
           (FORMAT T "~%Pending LISTENs:~%")
           (DO L PENDING-LISTENS (CDR L) (NULL L)
             (FORMAT T "   Contact name: ~S~%" (CAR L)))))
    (COND (PENDING-RFC-PKTS
           (FORMAT T "~%Pending RFCs:~%")
           (DO PKT PENDING-RFC-PKTS (PKT-LINK PKT) (NULL PKT)
             (FORMAT T "   Contact name: ~S~%" (PKT-STRING PKT))))))

(DEFUN PRINT-BAD-PKTS ()
   (DO ((LIST BAD-PKT-LIST (CDR LIST)))
       ((NULL LIST))
     (FORMAT T "~&~A" (CAAR LIST))
     (PRINT-PKT (CADAR LIST))
     (TERPRI)))

(DEFUN PRINT-RECENT-HEADERS ( &OPTIONAL (NBR 200))
   (DO ((I (\ (+ 177 RECENT-HEADERS-POINTER) 200) (COND ((ZEROP I) 177) (T (1- I))))
	(COUNT NBR (1- COUNT)))
       ((ZEROP COUNT))
     (FORMAT T "~%Nbr:~O Opcd:~O(~A). Len:~O bytes. "
	        (RCNT-PKT-NUM I)
		(RCNT-OPCODE I)
		(COND ((< (RCNT-OPCODE I) (LENGTH OPCODE-LIST))
		       (NTH (RCNT-OPCODE I) OPCODE-LIST))
		      (( (RCNT-OPCODE I) DAT-OP) 'DAT)
		      (T (FORMAT NIL "==> ~O <==" (RCNT-OPCODE I))))
		(RCNT-NBYTES I))
     (FORMAT T "From ~O-~O to ~O-~O, Fwded ~O Times, Rcrded:~O"
	        (RCNT-SOURCE-ADDRESS I) (RCNT-SOURCE-INDEX I)
		(RCNT-DEST-ADDRESS I) (RCNT-DEST-INDEX I)
		(RCNT-FWD-COUNT I) (RCNT-TIME-RECORDED I))))

;;; Prints as much info as possible
(DEFUN DUMP-GUTS ( &AUX (PEEK-SHORT-PKT-DISPLAY NIL))
       (PEEK-DISPLAY)
       (FORMAT T "~2%Recent headers:")
       (PRINT-RECENT-HEADERS))

;;; "Physical Support Facilities"

(DEFUN CALL-ELEVATOR ()
  (COND ((TECH-SQUARE-FLOOR-P 8)
	 (HACK-DOOR "8"))
	((TECH-SQUARE-FLOOR-P 9)
	 (HACK-DOOR "9"))
	((FERROR NIL "I don't know how to get an elevator to your location."))))

(DEFUN BUZZ-DOOR ()
  (COND ((TECH-SQUARE-FLOOR-P 9) (HACK-DOOR "D"))
	((FERROR NIL "I can only open the 9th floor door at Tech square"))))

(DEFUN HACK-DOOR (COMMAND)
  (LET ((RESULT (SIMPLE "AI" (STRING-APPEND "DOOR " COMMAND))))
    (COND ((STRINGP RESULT) (FERROR NIL "Failed trying to hack the door: ~A" RESULT))
	  (T (RETURN-PKT RESULT) T))))

(DEFUN TECH-SQUARE-FLOOR-P (FLOOR)
  (LET ((DATA (ASSQ MY-ADDRESS CONSOLE-LOCATION-ALIST)))
    (AND DATA
	 (EQ (CADR DATA) 'MIT-NE43)
	 (= (CADDR DATA) FLOOR))))
