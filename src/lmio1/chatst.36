;; -*- Mode: LISP; Package: CHAOS -*-

;;; TESTING FUNCTIONS:

;;;   SET-BASE-ADDRESS - sets the base unibus address for the network interface
;;;                      (defaults to 764140)
;;;   CHATST         - send to self with and without loopback printing the results,
;;;                    using pattern set by SET-PATTERN
;;;   CHATST-LOOP    - send packets to another host (defaults to MC) which will echo it back
;;;                    (useful for scope loops)
;;;   CHATST-MONITOR - looks at everying flowing on the network
;;;   CHATST-STATUS  - prints the status of the network interface, interpreting the CSR
;;;   CHATST-RESET   - resets the network interface
;;;   CHATST-ECHO    - 
;;;   CHATST-ECHO-ONCE
;;;   CHATST-SOAK
;;;   SET-NCP-BASE-ADDRESS - Sets the device address used by the NCP so that the
;;;                          interface to be tested can be tried in full service.
;;;                          NOTE!!!! A bus grant jumper must be run to the board you are
;;;                          debugging in order for interrupts to work!

;;;  **** NOTE *****
;;;  Here are some typical screws encountered in testing chaos boards:
;;;  If you get a CRC error, but the contents of the packet is NOT printed out,
;;;  this means the data came back correctly, but a CRC error was indicated
;;;  (often implying a bad CRC generator chip).
;;;  MAKE SURE AND TEST WITH SEVERAL PATTERNS!!  Certain patterns (e.g all zeros)
;;;  will not show certain errors.

(DECLARE (SPECIAL
   ;;; hardware related specials
	CONTROL-STATUS-REGISTER-TEST	;the control-status register
	MY-NUMBER-REGISTER-TEST	;the cable address register
	WRITE-BUFFER-REGISTER-TEST	;the write-data register
	READ-BUFFER-REGISTER-TEST	;the read-data register
	BIT-COUNT-REGISTER-TEST	;the bit count register
	INITIATE-TRANSFER-REGISTER-TEST	;the start transfer register
	INTERVAL-TIMER-REGISTER-TEST	;start the interval timer
	CHATST-PATTERN
	CHATST-ADDRESS			;Host address of interface we're testing
))


;;;Format of control register
;;; 1		;XMT BUSY
;;; 2		;LOOP BACK
;;; 4		;RECEIVE-ALL
;;; 10		;RESET-RECEIVE
;;; 20		;RCV INTERRUPT ENABLE
;;; 40		;TRANSMIT INTERRUPT ENABLE
;;; 100		;TRANSMIT ABORT
;;; 200		;TRANSMIT DONE
;;; 400		;TRANSMIT RESET
;;; 17000	;LOST COUNT
;;; 20000	;IO RESET
;;; 40000	;CRC ERROR
;;; 10000	;RCV DONE


(DEFUN SET-BASE-ADDRESS (&OPTIONAL (BASE-ADDRESS 764140))
    "Set the base UNIBUS address for the Chaos net device.
Argument is optional and defaults to 764140.  Defines various
special variables and read and prints the host address of
the device at the specified address."

    (SETQ CONTROL-STATUS-REGISTER-TEST BASE-ADDRESS
	  MY-NUMBER-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-MY-NUMBER-OFFSET 1))
	  WRITE-BUFFER-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-WRITE-BUFFER-OFFSET 1))
	  READ-BUFFER-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-READ-BUFFER-OFFSET 1))
	  BIT-COUNT-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-BIT-COUNT-OFFSET 1))
	  INITIATE-TRANSFER-REGISTER-TEST
	  (+ BASE-ADDRESS (LSH %CHAOS-START-TRANSMIT-OFFSET 1))
	  INTERVAL-TIMER-REGISTER-TEST
	  (+ BASE-ADDRESS 20))
    (FORMAT T "~%My number: ~O" (setq chatst-address (%unibus-read MY-NUMBER-REGISTER-TEST))))

(SET-BASE-ADDRESS)

(DEFVAR CHATST-PATTERN (MAKE-ARRAY NIL 'ART-16B 256.))

(DEFVAR CHATST-PATTERN-TYPE 0)

(DEFUN SET-PATTERN (PAT)
  (SETQ CHATST-PATTERN-TYPE PAT)
  (DO I 0 (1+ I) (= I 20)
    (AS-1 (COND ((EQ PAT 'FLOATING-ONE) (LSH 1 I))
                ((EQ PAT 'FLOATING-ZERO) (LOGXOR (LSH 1 I) -1))
                ((EQ PAT 'ADDRESS) I)
                ((NUMBERP PAT) PAT)
                ((ERROR "BAD PATTERN" I)))
          CHATST-PATTERN
          I)))

(SET-PATTERN 'FLOATING-ONE)			;REASONABLE DEFAULT

(DEFVAR CHATST-USE-RECEIVE-ALL T)		;reasonable???

(DEFUN CHATST ()
    "Standard test function for the chaos network interface.
If it passes this test, sending and receiving packets from the network
probably works.  Use SET-NCP-BASE-ADDRESS to give it a full test.
Things not tested by this function include UNIBUS interrupts, bus grant
logic, etc.  This function cycles through several bit patterns, sending
4 packets with each pattern, both in loopback and out on the cable.
It does not send a properly formated packet with a header, but just
a packet of raw bits."
    (CHATST-RESET)
    (DOLIST (PAT '(FLOATING-ONE FLOATING-ZERO ADDRESS 52525 0 177777))
      (FORMAT T "~%Pattern:  ~A ~%Using Loopback ~%" PAT)
      (SET-PATTERN PAT)
      (LET ((CHATST-USE-RECEIVE-ALL T))
	(DO I 0 (1+ I) (= I 4) (CHATST-PREP T) (CHATST-XMT) (CHATST-RCV)))
      (FORMAT T "~%Using the cable ~%")
      (LET ((CHATST-USE-RECEIVE-ALL NIL))
	(DO I 0 (1+ I) (= I 4) (CHATST-PREP NIL) (CHATST-XMT) (CHATST-RCV T)))))

(DEFUN CHATST-ONCE (&OPTIONAL (LOOPBACK NIL) (CHATST-USE-RECEIVE-ALL LOOPBACK))
  "Like CHATST, but only tries the currently defined pattern.  Call SET-PATTERN
to change the pattern."
  (CHATST-RESET)
  (FORMAT T "~%Loopback: ~A,  Pattern:  ~A" LOOPBACK CHATST-PATTERN-TYPE)
  (DO I 0 (1+ I) (= I 4) (CHATST-PREP NIL) (CHATST-XMT) (CHATST-RCV T)))

(DEFUN CHATST-TR-LOOP (&OPTIONAL LOOPBACK &AUX (CHATST-USE-RECEIVE-ALL LOOPBACK))
  (CHATST-RESET)
  (DO () ((KBD-TYI-NO-HANG)) (CHATST-PREP LOOPBACK) (CHATST-XMT) (CHATST-RCV T)))

(DEFUN CHATST-XMT ()
    "Send a packet consisting of 16 rotating 1's and my address."
    (DO I 0 (1+ I) (= I 20)
	(%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
    (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (%UNIBUS-READ MY-NUMBER-REGISTER-TEST))
    (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST	;improve chances of avoiding an abort
		   (LOGIOR 10 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST)))
    (%UNIBUS-READ INITIATE-TRANSFER-REGISTER-TEST))

(DEFUN CHATST-PACKET (&OPTIONAL (CABLE-DEST 440))	;MC-11
    "Send a packet to some host (defaults to MC) which it will echo back."
  (DO () ((bit-test 200 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST)))) ;AWAIT TDONE
  (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST 100000)  ;DATA
  (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST 40)	;NBYTES
  (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST 1440)	;MC
  (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST 0)
  (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST chatst-address)	;LISPM
  (DO I 0 (1+ I) (= I 3)			;SEND THE PATTERN AS IDX, PKT, ACK
    (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
  (DO I 0 (1+ I) (= I 20)			;SEND THE PATTERN AS 40 BYTES OF DATA
    (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
  (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST CABLE-DEST)
  (%UNIBUS-READ INITIATE-TRANSFER-REGISTER-TEST))


(DEFUN CHATST-LOOP (&OPTIONAL (CABLE-DEST 440) (LOOP-BACK-P NIL))	;MC-11, NO LOOPBACK
    "Scope loop, ignore what is received (defaults to mc)"
    (DO () ((KBD-TYI-NO-HANG))
      (CHATST-PREP LOOP-BACK-P)
      (CHATST-PACKET CABLE-DEST)))

;;; Prepare the interface to receive.
(DEFUN CHATST-PREP (LOOPBACK-P)
  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST
                 (+ (COND ((NOT LOOPBACK-P) 10) (T 12))
                    (COND ((NOT CHATST-USE-RECEIVE-ALL) 0) (T 4)))))

(DEFUN CHATST-RESET ()
    (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 20000))

(SETQ INBUF (MAKE-ARRAY NIL 'ART-16B 256.))
(DECLARE (SPECIAL INBUF))

;;; Look for a received packet, and complain in various ways.
(DEFUN CHATST-RCV ( &OPTIONAL BUSY-WAIT (CNT 16.) &AUX CSR TEM ME LOSE)
  (IF BUSY-WAIT
      (DO () ((LDB-TEST %%CHAOS-CSR-RECEIVE-DONE
			(%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))))
      (PROCESS-SLEEP 10.))  ;Give it time to arrive
  (SETQ CSR (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
  (SETQ ME (%UNIBUS-READ MY-NUMBER-REGISTER-TEST))
  (IF (NOT (ZEROP (LDB-TEST %%CHAOS-CSR-TRANSMIT-ABORT CSR)))
      (FORMAT t "~%Transmit aborted, then~%"))
  (COND ((NOT (LDB-TEST %%CHAOS-CSR-RECEIVE-DONE CSR))
         (SETQ LOSE T) (PRINT 'NO-RECEIVE))
        (T (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR CSR)
                (PROGN (SETQ LOSE T)
                       (PRINT '"CRC Error indicated (check the data)")))
           (OR (= (%UNIBUS-READ BIT-COUNT-REGISTER-TEST) (1- (* 16. (+ 3 CNT))))
               (PROGN (SETQ LOSE T)
                      (PRINT (LIST (%UNIBUS-READ BIT-COUNT-REGISTER-TEST) 'BAD-BIT-COUNT))))
           (DO I 0 (1+ I) (= I CNT)
             (AS-1 (%UNIBUS-READ READ-BUFFER-REGISTER-TEST) INBUF I))
           (OR (= (SETQ TEM (%UNIBUS-READ READ-BUFFER-REGISTER-TEST)) ME)
               (PROGN (SETQ LOSE T)
                      (FORMAT T "~% DEST=~O SHOULD=~O" TEM ME)))
           (OR (= (SETQ TEM (%UNIBUS-READ READ-BUFFER-REGISTER-TEST)) ME)
               (PROGN (SETQ LOSE T)
                      (FORMAT T "~% SOURCE=~O SHOULD=~O" TEM ME)))
           (DO ((I 0 (1+ I))
                (K))
               ((= I CNT) (IF LOSE (PRINT "Data returned was correct")))
             (SETQ K (AR-1 CHATST-PATTERN I))
             (COND (( K (AR-1 INBUF I))
                    (SETQ LOSE T)
                    (TERPRI) (PRINC "LOC    GOOD   BAD")
                    (DO I 0 (1+ I) (= I CNT)
                      (FORMAT T "~%~2O  ~6O ~6O" I (AR-1 CHATST-PATTERN I) (AR-1 INBUF I)))
                    (RETURN NIL))))))
  (OR LOSE (FORMAT T "~&WIN")))

;;; Monitor the Net for traffic

(DEFUN CHATST-MONITOR (&OPTIONAL (SHORT-P T) &AUX BITS cnt)
 "Monitor all network traffic.  This will often tell you if your interface or
  transceiver has trouble receiving packets from a particular host.  It all
  may tell you if something strange is happening on the network, such as
  a random host sending garbage packets, etc."
  (CHATST-RESET)
      (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 14)        ;reset rcvr, RCV ALL
  (DO () ((KBD-CHAR-AVAILABLE) (KBD-TYI-NO-HANG))
    (DO ((i 0 (1+ i)))
	((> I 50.) (FORMAT T "."))
      (COND ((bit-test 100000 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
	     (FORMAT T "~%---------------------~%")
	     (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
		  (FORMAT T "CRC-Error "))
	     (SETQ BITS (1+ (%UNIBUS-READ BIT-COUNT-REGISTER-TEST))
		   CNT (// BITS 16.))
	     (OR (ZEROP (\  BITS 16.))
		 (FORMAT T "Bad bit count, is ~O" BITS))
	     (COND ((AND SHORT-P (> CNT 8))
		    (DO I 0 (1+ I) (= I 5)
			(FORMAT T "~&~O   ~O" I (%UNIBUS-READ READ-BUFFER-REGISTER-TEST)))
		    (FORMAT T "~%     ...")
		    (DO I 0 (1+ I) ( I (- CNT 8))(%UNIBUS-READ READ-BUFFER-REGISTER-TEST))
		    (DO I (- CNT 3) (1+ I) (= I CNT)
			(FORMAT T "~%~O   ~O" I (%UNIBUS-READ READ-BUFFER-REGISTER-TEST))))
		   (T (DO I 0 (1+ I) (= I CNT)
			  (FORMAT T "~&~O   ~O" I (%UNIBUS-READ READ-BUFFER-REGISTER-TEST)))))
	     (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 14)        ;reset rcvr, RCV ALL
	     (RETURN NIL)))))
  (CHATST-RESET))


(DECLARE (SPECIAL CHATST-HEADER))
(SETQ CHATST-HEADER (MAKE-ARRAY NIL 'ART-16B 8))

(DEFUN CHATST-SET-HEADER NIL
   (AS-1 100000 CHATST-HEADER 0)                   ;OPCODE (DATA)
   (AS-1 0 CHATST-HEADER 1)                        ;LENGTH IN BYTES
   (AS-1 chatst-address CHATST-HEADER 2)               ;DESTINATION (CAUSE FORWARDING)
   (AS-1 0 CHATST-HEADER 3)
   (AS-1 chatst-address CHATST-HEADER 4)               ;SOURCE
   (DO I 0 (1+ I) (= I 3)                          ;SRC-IDX, PK#, ACK#
       (AS-1 (AR-1 CHATST-PATTERN I) CHATST-HEADER (+ I 5))))

(CHATST-SET-HEADER)                                ;Setup an echo header

(DEFUN CHATST-ECHO (&OPTIONAL (DEST 440)  (LEN 20))
  (CHATST-RESET)
  (SETQ LEN (MIN LEN 248.))         ;4096.-header
  (AS-1 (* LEN 2) CHATST-HEADER 1)
  (DO ((pat1 0 (1+ pat1))
       (pat2 (random) (random)))
      ((KBD-TYI-NO-HANG))
    (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 10)        ;reset rcvr
      (do i 0 (+ i 2) ( i len)
          (as-1 pat1 chatst-pattern i)
          (as-1 pat2 chatst-pattern (1+ i)))
      (format t "~%Patterns ~O, ~O" pat1 pat2)
      ;;Try this pattern 10. times
      (do ((j 0 (1+ j))) ((= j 10.))
          (DO ((i 0 (1+ i)))
              ((bit-test 200 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))) ;AWAIT TDONE
              (COND ((> i 50.)
                     (FORMAT T "~% TDONE timeout")
                     (RETURN NIL))))
          (DO I 0 (1+ I) (= I 8)    ;Fill in IDX, PKT, ACK with pattern 
              (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-HEADER I)))
          (DO I 0 (1+ I) (= I LEN)
              (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
          (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 10)    ;reset rcvr
          (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST DEST)
          (%UNIBUS-READ INITIATE-TRANSFER-REGISTER-TEST)     ;start xmission
          (DO ((i 0 (1+ i)))
              ((> I 1000.) (FORMAT T "~% Rcv-done timeout"))
              (COND ((BIT-TEST 100000 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
                     (CHATST-CHECK-PK DEST LEN)
                     (RETURN NIL)))) )))

;;Scope trace - echo from some host

(DEFUN CHATST-BUZZ (&OPTIONAL (DEST 440) (LEN 20))
  (CHATST-RESET)
  (SETQ LEN (MIN LEN 248.))         ;4096.-header
  (AS-1 (* LEN 2) CHATST-HEADER 1)
  (DO () ((KBD-TYI-NO-HANG)(CHATST-PRINT-STATUS DEST LEN))
      (as-1 (1+ (ar-1 chatst-pattern 0)) chatst-pattern 0)
      ;;Try this pattern 10. times
      (do ((j 0 (1+ j))) ((= j 10.))
	  ;;Wait for Transmit side idle
          (DO ((i 0 (1+ i)))
              ((bit-test 200 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST)))
              (COND ((> i 50.)
                     (FORMAT T "~% TDONE timeout")
                     (RETURN NIL))))
	  ;;Fill in header, data with pattern
          (DO I 0 (1+ I) (= I 8)
              (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-HEADER I)))
          (DO I 0 (1+ I) (= I LEN)
              (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
          (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST DEST)
	  ;;Now wait for echoed packet
          (DO ((i 0 (1+ i)))
              ((> I 50.))
              (COND ((bit-test 100000 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
                     (RETURN NIL))))
	  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST (IF (= DEST 0) 12 10))
          (%UNIBUS-READ INITIATE-TRANSFER-REGISTER-TEST))))

(DEFUN CHATST-PRINT-STATUS ( &OPTIONAL (DEST 100) (LEN 16.))
  (TERPRI)
  (PROCESS-SLEEP 30.)  ;Give it time to arrive
  (CHATST-STATUS)                                           ;Decode status
  (CHATST-CHECK-PK DEST LEN)                             ;Check if any errors in PK
)


(DEFUN CHATST-CHECK-PK (&OPTIONAL (DEST-HOST 100) (CNT 16.) &AUX DEST CSR TEM ME BITS)
  (SETQ CSR (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST)
        ME (%UNIBUS-READ MY-NUMBER-REGISTER-TEST)
        BITS (1- (* 16. (+ 11. CNT)))
        DEST DEST-HOST)
  (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR CSR)
       (PRINT 'CRC-ERROR))
  (OR (= (SETQ TEM (%UNIBUS-READ BIT-COUNT-REGISTER-TEST)) BITS)
      (FORMAT T "~%Bad bit count, is ~O, should be ~O" TEM BITS))
  (DO I 0 (1+ I) (= I (+ 8 CNT))
      (AS-1 (%UNIBUS-READ READ-BUFFER-REGISTER-TEST) INBUF I))
  (OR (= (SETQ TEM (%UNIBUS-READ READ-BUFFER-REGISTER-TEST)) ME)
         (FORMAT T "~% DEST=~O, should be ~O"  TEM ME))
  (OR (= (SETQ TEM (%UNIBUS-READ READ-BUFFER-REGISTER-TEST)) DEST)
         (FORMAT T "~% SOURCE=~O, should be ~O"  TEM DEST))
  (AS-1 (LOGAND (AR-1 INBUF 1) 7777) INBUF 1)           ;FLUSH FORWARDING COUNT
  (DO I 0 (1+ I) (= I 8)
      (COND (( (AR-1 CHATST-HEADER I) (AR-1 INBUF I))
             (TERPRI) (PRINC "HEADER  SENT    RCVD")
             (DO I 0 (1+ I) (= I 8)
                 (FORMAT T "~%~2O  ~6O ~6O" I (AR-1 CHATST-HEADER I) (AR-1 INBUF I)))
             (RETURN NIL))))
  (DO ((I 0 (1+ I)) (J 8 (1+ J))) ((= I CNT))
      (COND (( (AR-1 CHATST-PATTERN I) (AR-1 INBUF J))
             (TERPRI) (PRINC "LOC    SENT    RCVD")
             (DO ((I 0 (1+ I))(J 8 (1+ J))) ((= I CNT))
                 (FORMAT T "~%~2O  ~6O ~6O" I (AR-1 CHATST-PATTERN I) (AR-1 INBUF J)))
             (RETURN NIL))))
  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 10)    ;reset rcvr
)

(DEFUN CHATST-ECHO-ONCE (&OPTIONAL (DEST 500) (LEN 20))
       (DO ()((bit-test 200 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))))
       (DO I 0 (1+ I) (= I LEN)			;SEND THE PATTERN AS 40 BYTES OF DATA
           (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
      (%UNIBUS-WRITE WRITE-BUFFER-REGISTER-TEST DEST)
      (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 10)    ;reset rcvr
      (%UNIBUS-READ INITIATE-TRANSFER-REGISTER-TEST)
      (DO ((i 0 (1+ i)))
          ((or (bit-test 200 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
               (> i 50.)))) ;AWAIT TDONE
      (%UNIBUS-WRITE CONTROL-STATUS-REGISTER-TEST 14)        ;RCV ALL
      (CHATST-PRINT-STATUS DEST LEN))

(DEFUN CHATST-STATUS ( &AUX CSR LC)
    "Describes the bits currently on in the control status register for the
board being tested."
    (SETQ CSR (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
    (FORMAT T "~2%CSR = ~O~%" CSR)
    (AND (LDB-TEST %%CHAOS-CSR-TIMER-INTERRUPT-ENABLE CSR)
	 (FORMAT T "Timer interrupt enable. ?? ~%"))  ;This bit doesnt seem to do anything.
;    (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-BUSY CSR)
;	 (FORMAT T "Transmit busy.~%"))
    (AND (LDB-TEST %%CHAOS-CSR-LOOP-BACK CSR)
	 (FORMAT T "Loopback.~%"))
    (AND (LDB-TEST %%CHAOS-CSR-RECEIVE-ALL CSR)
	 (FORMAT T "Receive all messages mode is on.~%"))
    (AND (LDB-TEST %%CHAOS-CSR-RECEIVE-ENABLE CSR)
	 (FORMAT T "Receiver interrupt enabled.~%"))
    (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-ENABLE CSR)
	 (FORMAT T "Transmit interrupt enabled.~%"))
    (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-ABORT CSR)
	 (FORMAT T "Transmit aborted by collision.~%"))
    (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-DONE CSR)
	 (FORMAT T "Transmit done.~%"))
    (OR  (ZEROP (SETQ LC (LDB %%CHAOS-CSR-LOST-COUNT CSR)))
	 (FORMAT T "Lost count = ~O~%" LC))
    (AND (LDB-TEST %%CHAOS-CSR-RESET CSR)
	 (FORMAT T "I//O reset.~%"))
    (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR CSR)
	 (FORMAT T "==> CRC ERROR!!! <==~%"))
    (AND (LDB-TEST %%CHAOS-CSR-RECEIVE-DONE CSR)
	 (FORMAT T "Receive done.~%"))
    (FORMAT T "Bit count: ~O~%" (%UNIBUS-READ BIT-COUNT-REGISTER-TEST))
    NIL)

(DEFUN CHATST-SOAK (&AUX (M-ONES 0) (OTHERS 0))
  (%unibus-write control-status-register-test 14)
  (DO () ((KBD-TYI-NO-HANG) (FORMAT T "~%-1 length packets ~O, others ~O" m-ones others))
    (COND ((bit-test 100000 (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST))
;	   (DO ((I 0 (1+ I))) ((> I 10.))
;	     (FORMAT T "~%~O" (%UNIBUS-READ CONTROL-STATUS-REGISTER-TEST)))
	   (let ((tem (%unibus-read bit-count-register-test)))
	     (if (= tem 7777)			;Null packet "received"
		 (setq m-ones (1+ m-ones))
		 (setq others (1+ others))))
	       (%unibus-write control-status-register-test 14)))))


(DEFUN SET-NCP-BASE-ADDRESS (ADDR &AUX (OLD-CSR CONTROL-STATUS-REGISTER))
 "Set the base address that the NCP uses for all Chaos net functions.
NOTE!!!! A bus grant jumper must be run to the board you are debugging in
order for interrupts to work!  This function makes the board you are debugging
used for everything, rather than the default."
  (SET-BASE-ADDRESS ADDR)
  (SETQ BASE-ADDRESS ADDR
	CONTROL-STATUS-REGISTER BASE-ADDRESS
	MY-NUMBER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-MY-NUMBER-OFFSET 1))
	WRITE-BUFFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-WRITE-BUFFER-OFFSET 1))
	READ-BUFFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-READ-BUFFER-OFFSET 1))
	BIT-COUNT-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-BIT-COUNT-OFFSET 1))
	INITIATE-TRANSFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-START-TRANSMIT-OFFSET 1)))

  (SETQ SI:%CHAOS-CSR-ADDRESS
	(SI:MAKE-24-BIT-UNSIGNED (+ 77400000 (LSH ADDR -1))))  ; SET THE A MEMORY LOCATION
  (INITIALIZE-NCP-SYSTEM)
  (%UNIBUS-WRITE OLD-CSR 20010)			;avoid interrupt hang screw
  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER 20010)
  (FORMAT NIL "NCP now using ~6O as the network interface base address." ADDR))


(DEFUN TIMER-LOOP (&OPTIONAL (COUNT 511.) (SLEEP-TIME 1))
  "Scope loop for looking at the interval timer."
  (DO NIL ((KBD-TYI-NO-HANG))
    (%UNIBUS-WRITE INTERVAL-TIMER-REGISTER-TEST COUNT)
    (PROCESS-SLEEP SLEEP-TIME)))
