;;; -*- Mode: LISP; Package: CHAOS; BASE: 8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; Lisp Machine package for using the ChaosNet.
;;; New Protocol of May, 1978

;;; This file is AI: LMIO; CHAOS >
;;; Real ChaosNet documentation is on AI: MOON; CHAORD >

;;; TO BE FIXED!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;****Hack in new routing table stuff****
;****Fix the packet recording stuff****
;****All these hundreds of routines each with its own idiosyncratic idea
;****of how to signal errors should be replaced with one routine which
;****takes a conn and signals the appropriate error.  QFILE should call it, too.

;This file contains the CHAOS net software from the packet level down

;Some standard abbreviations and mnemonic indicators:
;Items in this list with no "-" at either end signify abbreviations
;	which may occur in any context, those with one or more "-"'s
;	only occur in a context in which there are "-"'s in exactly
;	those places.
; PKT		Abbreviation for PACKET.  see the DEFSTRUCT
; CONN		Abbreviation for CONNECTION.  see the DEFSTRUCT
; SEND-		Cause a packet to be "sent" with retry etc. if applicable.
; TRANSMIT-	Cause a packet to be "transmitted" (i.e. placed on the net).
;			This is done by all the SEND- routines by calling
;			an appropriate TRANSMIT- routine to put the packet
;			on a list of packets to be put on the net.
; -STATE	A symbol representing a state that a connection might be in.
; -OP		A SPECIAL whose value represents that of a packet op code.

;The file is divided into more-or-less localized units.  These units
;	and which of the contained functions are intended for outside
;	use are listed here in the order in which they appear in the file.

;Definitions for high-level structures (with functions to print them textually)
;	and various macros for the specialized formulas of the protocol
;   (Everything here is used everywhere)

;Low-level PKT management.
;functions referenced elsewhere:
;   ALLOCATE-PKT, FREE-PKT, SET-PKT-STRING

;Low-level CONN management.
;functions referenced elsewhere:
;   MAKE-CONNECTION, REMOVE-CONN

;High level Transmission routines
;functions referenced elsewhere:
;   TRANSMIT-LOS-INT-PKT, TRANSMIT-STS, TRANSMIT-NORMAL-PKT

(DECLARE (SPECIAL	;Definitions of all specials

;Opcodes of packets
	RFC-OP OPN-OP
	CLS-OP FWD-OP
	ANS-OP SNS-OP
	STS-OP RUT-OP
	LOS-OP LSN-OP
	MNT-OP EOF-OP
        UNC-OP
	DAT-OP

;This is for printing out packets nicely.
	OPCODE-LIST

;Size-of-packet symbols.  These symbols define the protocol specified sizes.
        MAX-WORDS-PER-PKT	;Largest packet possible in words
	MAX-DATA-WORDS-PER-PKT	;Number of data words in largest packet
        MAX-DATA-BYTES-PER-PKT	;Number of data bytes in largest packet
	FIRST-DATA-WORD-IN-PKT	;Offset to first data word in packet

	ENABLE	    ;Non NIL if this thing trying to work.

;; This process runs all Time response actions such as PROBEs and Retransmission
;; as well as any background requests
        BACKGROUND

;; This is a simple process, that handles reading packets from the wired buffers
	RECEIVER

;The following are used for negotiating the initial connection between
;	a host and a server.
	PENDING-RFC-PKTS	;Linked through the PKT-LINK
	PENDING-LISTENS		;List of (CONTACT-NAME . CONN) for pending listens.
	SERVER-ALIST		;Initialization-list where init name is the contact name
                                ;and form is to be evaluated in the background task
				;whenever an RFC appears for the contact name

;;; Packet lists and other pointers
	FREE-PKTS		;First pkt on the free list. Linked through the PKT-LINK.
	MADE-PKTS		;First pkt on the made list. Linked through the PKT-MADE-LINK.

;;; Connection list
        CONN-LIST		;List of existing connections
	FREE-CONN-LIST		;List of free CONN structures (save consing)
	PROTOTYPE-CONN		;Kludgey way of getting a conn initialized

;;; Meters			Bumped when:
	PKTS-FORWARDED		;We forward a PKT to someone
	PKTS-OVER-FORWARDED	;We increment a FWD-COUNT too high.
	PKTS-BAD-BIT-COUNT	;Bit count less than Dest, Source, and CRC words,
				; or not mod 16., or doesn't agree with software
				; packet length.
	PKTS-BAD-DEST		;Hardware dest wasn't MY-ADDRESS
	PKTS-BAD-CRC-1		;CRC was bad on receive
	PKTS-BAD-CRC-2		;CRC was bad after readout
	PKTS-LOST		;Sum of LOST COUNT field of the hardware interface.
	PKTS-MADE		;Number of PKTS ever created.
	PKTS-RECEIVED		;Number of packets received
	PKTS-TRANSMITTED	;Number of packets transmitted
	PKTS-OTHER-DISCARDED	;Number of packets discarded for other reasons
				;  (too small to contain a protocol packet)
	LOS-PKT-COUNT		;Count of all LOS packets ever received
	CURRENT-LOS-PKT-COUNT 	;Number of packets currently on LOS-PKTS
	PKTS-RETRANSMITTED	;Number of packets retransmitted
	PKTS-DUPLICATED		;On receipt
	DATA-PKTS-IN
	DATA-PKTS-OUT
  ;also SI:%COUNT-CHAOS-TRANSMIT-ABORTS which is maintained by the microcode
  ;Reference this with READ-METER, WRITE-METER

;;; Debugging aids which keep records into the past (a short way).
	BAD-PKT-LIST		;List of strings describing packets received in error
        PKTS-BAD-CRC-SAVE-P	;Non-NIL means save all packets with bad crc
	LOS-PKTS		;LOS PKTs received from the network linked by PKT-LINK.
	MAX-LOS-PKTS-TO-KEEP	;Maximum number of LOS packets to keep on LOS-PKTS
				;There may actually be more but they will be
				;used by allocator.
        RECENT-HEADERS		;Array of 200 most recent packet transactions each row
				;containing the eight header words of the packet and
				;the time at which the record was made.
	RECENT-HEADERS-POINTER

;Clock constants (should be fixed with new clock stuff)
	RETRANSMISSION-INTERVAL	;1 SECOND
	PROBE-INTERVAL		;10 SECONDS
        LONG-PROBE-INTERVAL	;1 MINUTE
	HOST-DOWN-INTERVAL	;1.5 MINUTES
        BACKGROUND-REQUESTS     ;Requests to the background ChaosNet process
	RETRANSMISSION-NEEDED	;T if retransmission may be needed, enables
				; background process to wake up on clock
				;Set this whenever you put something on SEND-PKTS of a CONN
	MORE-RETRANSMISSION-NEEDED

	DEFAULT-WINDOW-SIZE	;This is the default size of the window for a CONN
	MAXIMUM-WINDOW-SIZE	;This is the maximum size of the window for a CONN
	MY-ADDRESS		;Full address of this host.
	MY-FINGER-LOCATION-STRING  ;Compute from MY-ADDRESS and FINGER-ALIST
	MY-NAME-STRING		   ;Compute from MY-ADDRESS and HOST-ALIST
	MY-SUBNET		;Subnet of this host.
	MAXIMUM-INDEX		;Number of INDEX-CONN slots, must be a power of 2
				;(LOG base 2 of MAXIMUM-INDEX)-1
	MAXIMUM-INDEX-LOG-2-MINUS-1
;; This array holds the CONN for the given index number.
	INDEX-CONN
	INDEX-CONN-FREE-POINTER

;; This array holds the uniquizer for the current (or last) connection for a given index
	UNIQUIZER-TABLE

	PEEK-SHORT-PKT-DISPLAY		;Display packets in short form in peek if T
	PEEK-A-BOO-LIST		;List of interesting meters to type out

;;; This array is the routing table:  if we want to send a message to a given
;;; subnet, to where should I forward it?  If the subnet # is greater than the
;;; length of the array, use the contents of array element zero.
;;; The contents of the array are the host number on our subnet who knows
;;; how to handle this packet.  NOTE that for now we can only be on one subnet.
;;; Don't use this table unless you are sure that the packet is not going to
;;; a host on THIS subnet!
	ROUTING-TABLE
	ROUTING-TABLE-COST

;;; hardware related specials
	BASE-ADDRESS		;the base address of the hardware register
	CONTROL-STATUS-REGISTER	;the control-status register
	MY-NUMBER-REGISTER	;the cable address register
	WRITE-BUFFER-REGISTER	;the write-data register
	READ-BUFFER-REGISTER	;the read-data register
	BIT-COUNT-REGISTER	;the bit count register
	INITIATE-TRANSFER-REGISTER	;the start transfer register

;Interrupt (microcode) related specials
	INT-FREE-LIST-POINTER   ;Freelist used by microcode
	INT-RECEIVE-LIST-POINTER  ;Packets received at interrrupt level
	INT-TRANSMIT-LIST-POINTER ;Packets to be transmitted at interrupt level

	CHAOS-BUFFER-AREA	;Area in which chaosnet INT-PKT's reside

	RESERVED-INT-PKT	;If non-NIL, the INT-PKT to use.  This permits the
				; receiver level stuff to reserve a packet and thus
				; avoid the possibility of blocking.
	)) ;;; End of specials

;;; These are interesting meters
(SETQ PEEK-A-BOO-LIST '(PKTS-FORWARDED PKTS-OVER-FORWARDED PKTS-BAD-BIT-COUNT PKTS-BAD-DEST
			PKTS-BAD-CRC-1 PKTS-BAD-CRC-2 PKTS-LOST PKTS-MADE PKTS-RECEIVED
			PKTS-TRANSMITTED PKTS-OTHER-DISCARDED LOS-PKT-COUNT
			CURRENT-LOS-PKT-COUNT PKTS-RETRANSMITTED PKTS-DUPLICATED DATA-PKTS-IN
			DATA-PKTS-OUT))

(DEFUN RESET-METERS ()
  (DOLIST (METER PEEK-A-BOO-LIST)
    (SET METER 0))
  (WRITE-METER 'SYS:%COUNT-CHAOS-TRANSMIT-ABORTS 0))

(RESET-METERS)	;avoid unbound-symbol errors if new things have been added to peek-a-boo-list.
		; These can be very embarressing.

;; High Level Structures

;;; Definitions for high level structures and various macros for the
;;;     specialized formulas of the protocol

;;; STRUCTURE DEFINITIONS: Connections (CONNs) and Packets (PKTs)

;;; This structure is a connection, abbreviated CONN.
(DEFSTRUCT (CONN :ARRAY :NAMED)
	LOCAL-WINDOW-SIZE	;Window size for receiving.
	FOREIGN-WINDOW-SIZE	;Window size for transmitting.
	(STATE 'INACTIVE-STATE)	;State of this connection.
;States in which a connection may be.

;	INACTIVE-STATE
;This state indicates the CONN is not currently associated with any CHAOS channel.

;	ANSWERED-STATE
;This state indicates the CONN has received an ANS from the other end of the channel;
;       it is waiting to be read.
;   No I/O is allowed except reading the ANS packet already on the READ-PKTS chain.

;	CLS-RECEIVED-STATE
;This state indicates the CONN has received a CLS from the other end of the channel;
;       any data packets that were received in order before the CLS are waiting
;       to be read, followed by the CLS packet.
;   No I/O is allowed except reading the packets already on th READ-PKTS chain.

;	LISTENING-STATE
;This state is given to a CONN on which a LSN has been sent while it is awaiting the RFC.

;	RFC-RECEIVED-STATE
;This state indicates that an RFC has been received on this CONN but that no response
;	has yet been given (such as done by ACCEPT and REJECT).

;	RFC-SENT-STATE
;This state indicates that there has been an RFC sent on this CONN but that no response
;	has yet been received from the foreign host.

;	OPEN-STATE
;This state is the normal state for an open connection

;	LOS-RECEIVED-STATE
;This state indicates the CONN has received a LOS from the other end of the channel;
;	the LOS packet will be the data packet waiting to be read; any READ-PKTS
;	or SEND-PKTS are discarded
;   No I/O is allowed except reading the packets already on th READ-PKTS chain.

;	HOST-DOWN-STATE
;This state is entered when it is determined that the foreign
;	host is down (or something). This is done in PROBE-CONN.
;   No I/O is allowed except reading the packets already on th READ-PKTS chain.

	(FOREIGN-ADDRESS 0)	;Address <his> for the other end of this CONN
	(FOREIGN-INDEX-NUM 0)	;Index number <his> for the other end of this CONN
	;; LOCAL-ADDRESS is a constant and therefore not needed or included
	(LOCAL-INDEX-NUM 0)	;Index number <mine> for this end of the CONN

	(READ-PKTS NIL)		;Packets which have been read from the net and are in order.
	(READ-PKTS-LAST NIL)	;Last packet on above list.
	(RECEIVED-PKTS NIL)	;Packets which have been received but are not in order.

	(PKT-NUM-READ -1)	;The <his> highest packet number given to user.
	(PKT-NUM-RECEIVED -1)	;The <his> highest packet number in ordered list (READ-PKTS).
	(PKT-NUM-ACKED -1)	;The level of acknowledgement we have sent out to date.
	(TIME-LAST-RECEIVED)    ;Time of last input from net.

	(SEND-PKTS NIL)		;List of packets which we must send.
	(SEND-PKTS-LAST NIL)	; (Last PKT on above)
	(SEND-PKTS-LENGTH 0)	; (Length of SEND-PKTS chain)
	(PKT-NUM-SENT 0)	;Highest <our> packet number assigned.
	(SEND-PKT-ACKED 0)	;The last packet number for which we received acknowledgement
	(WINDOW-AVAILABLE 0)	;Space in window not occupied by unacknowledged packets

        (INTERRUPT-FUNCTION NIL);Function to be called in when a new packet arrives at the
                                ; head of READ-PKTS
	)

(DEFUN CONN (OP &OPTIONAL X &REST ARGS)
    (SELECTQ OP
	     (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
	     ((:PRINT :PRINT-SELF)
	      (FORMAT (CAR ARGS) "#<CHAOS Connection ~O>" (%POINTER X)))
	     (OTHERWISE (ERROR OP "I have never heard of "))))

;;; Packets.  The following structure is a packet, abbreviated PKT.  The
;;; elements of the array are the actual bits of the packet, whereas the 
;;; elements of the leader are internal information not transmitted.
(BEGF PKT-STRUCTURE)

;***THESE DEFSTRUCTS USED BY QFILE!  RECOMPILE IT IF THEY CHANGE!***
(DEFSTRUCT (PKT-LEADER :ARRAY-LEADER (:CONSTRUCTOR NIL))
	PKT-ACTIVE-LENGTH	;Not used
	(PKT-NAMED-STRUCTURE-SYMBOL PKT) ;Note PKT not PKT-LEADER; 2 defstructs for 1 object!
	PKT-TIME-TRANSMITTED	;Time this PKT last transmitted
	PKT-TIMES-TRANSMITTED	;Number of times this PKT has been transmitted
	PKT-STRING		;A string which is the bytes of the PKT
	PKT-LINK		;Links PKTs in the chain that describes them
	PKT-MADE-LINK		;Links all packets ever made
;for all three -LINKs NIL = Last in chain, T = Not on chain at all.
;PKT-LINK is T only if the PKT was freed but was on transmit list and so is temporarily kept.
        PKT-BEING-RETRANSMITTED ;T if the packet is being retransmitted and so cannot be
                                ; really freed.  If this is the case, it is bashed to be
                                ; FREE so that the retransmitter will know to free it up
        PKT-STATUS              ;Status of the packet
;The status slot is used by the NCP to remember a small amount of info about the packet:
;	NIL		Normal packet, in use by the NCP
;	RELEASED	Packet has been given to the user
)
;For a description of the fields in a PKT see the documentation on the CHAOS Net

(DEFSTRUCT (PKT :ARRAY (:CONSTRUCTOR NIL))
      ((PKT-OPCODE-LEFT-JUSTIFIED NIL) (PKT-OPCODE 1010))
      ((PKT-NBYTES 0014) (PKT-FWD-COUNT 1404))
      ((PKT-DEST-ADDRESS NIL) (PKT-DEST-HOST-NUM 0010) (PKT-DEST-SUBNET 1010))
	PKT-DEST-INDEX-NUM
      ((PKT-SOURCE-ADDRESS NIL) (PKT-SOURCE-HOST-NUM 0010) (PKT-SOURCE-SUBNET 1010))
	PKT-SOURCE-INDEX-NUM
	PKT-NUM
	PKT-ACK-NUM
	PKT-FIRST-DATA-WORD
	PKT-SECOND-DATA-WORD
	)

(DEFMACRO PKT-NWORDS (PKT)
   `(+ FIRST-DATA-WORD-IN-PKT (LSH (1+ (PKT-NBYTES ,PKT)) -1)))

(DEFMACRO PKT-DEST-CONN (PKT)
    `(AR-1 INDEX-CONN (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (PKT-DEST-INDEX-NUM ,PKT))))

(DEFMACRO PKT-SOURCE-CONN (PKT)
    `(AR-1 INDEX-CONN (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (PKT-SOURCE-INDEX-NUM ,PKT))))

;; The following macros are for accessing the elements of RECENT-HEADERS

(DEFMACRO RCNT-OPCODE (INDEX)
	  `(LDB 1010 (AR-2 RECENT-HEADERS ,INDEX 0)))

(DEFMACRO RCNT-NBYTES (INDEX)
	  `(LDB 0014 (AR-2 RECENT-HEADERS ,INDEX 1)))

(DEFMACRO RCNT-FWD-COUNT (INDEX)
	  `(LDB 1404 (AR-2 RECENT-HEADERS ,INDEX 1)))

(DEFMACRO RCNT-DEST-ADDRESS (INDEX)
	  `(AR-2 RECENT-HEADERS ,INDEX 2))

(DEFMACRO RCNT-DEST-INDEX (INDEX)
	  `(AR-2 RECENT-HEADERS ,INDEX 3))

(DEFMACRO RCNT-SOURCE-ADDRESS (INDEX)
	  `(AR-2 RECENT-HEADERS ,INDEX 4))

(DEFMACRO RCNT-SOURCE-INDEX (INDEX)
	  `(AR-2 RECENT-HEADERS ,INDEX 5))

(DEFMACRO RCNT-PKT-NUM (INDEX)
	  `(AR-2 RECENT-HEADERS ,INDEX 6))

(DEFMACRO RCNT-ACK-NUM (INDEX)
	  `(AR-2 RECENT-HEADERS ,INDEX 7))

(DEFMACRO RCNT-TIME-RECORDED (INDEX)
	  `(AR-2 RECENT-HEADERS ,INDEX 8))

(ENDF PKT-STRUCTURE)

;;; These are routines to print out the preceding structures in a readable form

(DEFUN PRINT-CONN (CONN &OPTIONAL (SHORT-PKT-DISPLAY T) &AUX (LAST NIL))
	     (FORMAT T
		     "~%Chn: ~O (~O): State: ~S From: ~O-~O to ~O-~O .~%"
		     (LOCAL-INDEX-NUM CONN) (%POINTER CONN) (STATE CONN)
		     MY-ADDRESS (LOCAL-INDEX-NUM CONN)
		     (FOREIGN-ADDRESS CONN) (FOREIGN-INDEX-NUM CONN))
	     (FORMAT T
   " Rcvd #~O, Read #~O, Acked #~O; Sent #~O, Acked #~O.  Windows: ~O, ~O (~O available).~%"
		(PKT-NUM-RECEIVED CONN) (PKT-NUM-READ CONN) (PKT-NUM-ACKED CONN)
		(PKT-NUM-SENT CONN) (SEND-PKT-ACKED CONN)
		(LOCAL-WINDOW-SIZE CONN) (FOREIGN-WINDOW-SIZE CONN) (WINDOW-AVAILABLE CONN))

             (COND ((SEND-PKTS CONN)
                    (FORMAT T " Send pkts:")
                    (DO ((PKT (SEND-PKTS CONN) (PKT-LINK PKT))
			 (LAST NIL PKT)
			 (LEN 0 (1+ LEN)))
			((NULL PKT)
			 (OR (EQ LAST (SEND-PKTS-LAST CONN))
			     (FORMAT T
				     "==> SEND-PKTS-LAST IS SCREWED! <==~%"))
			 (OR (= LEN (SEND-PKTS-LENGTH CONN))
			     (FORMAT T "==> SEND-PKTS-LENGTH IS SCREWED! <==~%")))
		      (COND ((NOT (EQ CONN (PKT-SOURCE-CONN PKT)))
			     (FORMAT T "~Following PKT has bad PKT-SOURCE-CONN PKT = ~S"
				     (PKT-SOURCE-CONN PKT))))
		      (PRINT-PKT PKT SHORT-PKT-DISPLAY))))
             (SETQ LAST NIL)
	     (COND ((READ-PKTS CONN)
                    (FORMAT T " Read pkts:")
                    (DO ((PKT (READ-PKTS CONN) (PKT-LINK PKT))
			 (LAST NIL PKT)
			 (LEN 0 (1+ LEN)))
			((NULL PKT)
			 (OR (EQ LAST (READ-PKTS-LAST CONN))
			     (FORMAT T
				     "==> READ-PKTS-LAST IS SCREWED! <==~%")))
			(PRINT-PKT PKT SHORT-PKT-DISPLAY))))

             (COND ((RECEIVED-PKTS CONN)
                    (FORMAT T " Received pkts:")
                    (DO PKT (RECEIVED-PKTS CONN) (PKT-LINK PKT) (NULL PKT)
                      (SETQ LAST PKT)
		      (PRINT-PKT PKT SHORT-PKT-DISPLAY)))))

;;; Print out a packet, if SHORT-DISPLAY is T only 1 line is printed.
(DEFUN PRINT-PKT (PKT &OPTIONAL (SHORT-DISPLAY NIL))
    (TERPRI)
    (AND SHORT-DISPLAY (FORMAT T "   "))
    (FORMAT T "Number: ~O (~O)  Opcode: ~O (~A).  Number of bytes = ~O ."
	       (PKT-NUM PKT)
	       (%POINTER PKT)
	       (PKT-OPCODE PKT)
	       (COND ((< (PKT-OPCODE PKT) (LENGTH OPCODE-LIST))
		      (NTH (PKT-OPCODE PKT) OPCODE-LIST))
		     (( (PKT-OPCODE PKT) DAT-OP) 'DAT)
		     (T (FORMAT NIL "==> ~O <==" (PKT-OPCODE PKT))))
	       (PKT-NBYTES PKT))
    (COND ((NOT SHORT-DISPLAY)
	   (FORMAT T "~%From ~O-~O to ~O-~O .~%"
		   (PKT-SOURCE-ADDRESS PKT) (PKT-SOURCE-INDEX-NUM PKT)
		   (PKT-DEST-ADDRESS PKT) (PKT-DEST-INDEX-NUM PKT))
;	   (FORMAT T "Contents:~S~%   " (PKT-STRING PKT))
           (LET ((MIN-WORDS (MIN 8. (PKT-NWORDS PKT))))
                (DO ((I 0 (1+ I))) (( I min-words))
                    (FORMAT T "~6,48O~:[,~;~%~]" (AR-1 PKT I) (= (1+ I) MIN-WORDS))))
	   (FORMAT T "Pkt number = ~O, Ack number = ~O, Forwarded ~O times.~%"
		   (PKT-NUM PKT) (PKT-ACK-NUM PKT) (PKT-FWD-COUNT PKT))
	   (FORMAT T "Retransmitted ~O times, last at ~S.~%Link = ~S~%"
		   (PKT-TIMES-TRANSMITTED PKT) (PKT-TIME-TRANSMITTED PKT)
		   (PKT-LINK PKT))))
    NIL)

(DEFUN PRINT-ALL-PKTS (CHAIN &OPTIONAL (SHORT-DISPLAY T))
       (DO ((PKT CHAIN (PKT-LINK PKT))) ((NULL PKT))
	   (PRINT-PKT PKT SHORT-DISPLAY)))

(DEFUN PKT (OP &OPTIONAL X &REST ARGS)
    (SELECTQ OP
	     (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
	     ((:PRINT :PRINT-SELF)
	      (FORMAT (CAR ARGS) "#<CHAOS Packet ~O>" (%POINTER X)))
	     (OTHERWISE (ERROR OP "I have never heard of "))))


;;; Definitions for interrupt hacking

; To access the data base
(DEFMACRO INT-FREE-LIST () `(%P-CONTENTS-OFFSET INT-FREE-LIST-POINTER 0))

(DEFMACRO INT-RECEIVE-LIST () `(%P-CONTENTS-OFFSET INT-RECEIVE-LIST-POINTER 0))

(DEFMACRO INT-TRANSMIT-LIST () `(%P-CONTENTS-OFFSET INT-TRANSMIT-LIST-POINTER 0))


; The array leader offsets are defined in the SYSTEM package
(DEFMACRO INT-PKT-WORD-COUNT (INT-PKT)
  `(ARRAY-LEADER ,INT-PKT %CHAOS-LEADER-WORD-COUNT))

(DEFMACRO INT-PKT-THREAD (INT-PKT)
  `(ARRAY-LEADER ,INT-PKT %CHAOS-LEADER-THREAD))

(DEFMACRO INT-PKT-CSR-1 (INT-PKT)
  `(ARRAY-LEADER ,INT-PKT %CHAOS-LEADER-CSR-1))

(DEFMACRO INT-PKT-CSR-2 (INT-PKT)
  `(ARRAY-LEADER ,INT-PKT %CHAOS-LEADER-CSR-2))

(DEFMACRO INT-PKT-BIT-COUNT (INT-PKT)
  `(ARRAY-LEADER ,INT-PKT %CHAOS-LEADER-BIT-COUNT))

;This DEFSTRUCT applies to INT-PKT's also
;(DEFSTRUCT (PKT :ARRAY (:CONSTRUCTOR NIL))
;      ((PKT-OPCODE-LEFT-JUSTIFIED NIL) (PKT-OPCODE 1010))
;      ((PKT-NBYTES 0014) (PKT-FWD-COUNT 1404))
;      ((PKT-DEST-ADDRESS NIL) (PKT-DEST-HOST-NUM 0010) (PKT-DEST-SUBNET 1010))
;	PKT-DEST-INDEX-NUM
;      ((PKT-SOURCE-ADDRESS NIL) (PKT-SOURCE-HOST-NUM 0010) (PKT-SOURCE-SUBNET 1010))
;	PKT-SOURCE-INDEX-NUM
;	PKT-NUM
;	PKT-ACK-NUM
;	PKT-FIRST-DATA-WORD
;	PKT-SECOND-DATA-WORD
;	)

;;; Also, at the end of an INT-PKT are the source address, destination address, and CRC
(DEFMACRO INT-PKT-HARDWARE-DEST (INT-PKT)
  `(AREF ,INT-PKT (- (INT-PKT-WORD-COUNT ,INT-PKT) 3)))

(DEFMACRO INT-PKT-HARDWARE-SOURCE (INT-PKT)
  `(AREF ,INT-PKT (- (INT-PKT-WORD-COUNT ,INT-PKT) 2)))

(DEFMACRO INT-PKT-CRC (INT-PKT)
  `(AREF ,INT-PKT (1- (INT-PKT-WORD-COUNT INT-PKT))))

(BEGF MACROS)
;;; MACROS: for various random things

(DEFMACRO PKTNUM-< (A B)
   `(BIT-TEST 100000 (- ,A ,B)))

(DEFMACRO PKTNUM-1+ (A)
   `(LOGAND 177777 (1+ ,A)))

(DEFUN PKTNUM-- (A B &AUX TEM)
    (SETQ TEM (- A B))
    (COND ((< TEM 0) (+ TEM 200000))
	  (T TEM)))

;;; Adds a new background task to the queue:  these tasks are ORDERED on a fifo bases
(DEFMACRO BACKGROUND-TASK (TASK)
  `(WITHOUT-INTERRUPTS
     (PUSH ,TASK BACKGROUND-REQUESTS)))

(ENDF MACROS)

;;; Initialize all of the data of the NCP routines

;;; Once-only initialization stuff
(DEFUN INITIALIZE-NCP-ONCE ()

;;; hardware register address definitions
  (SETQ BASE-ADDRESS 764140
	CONTROL-STATUS-REGISTER BASE-ADDRESS
	MY-NUMBER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-MY-NUMBER-OFFSET 1))
	WRITE-BUFFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-WRITE-BUFFER-OFFSET 1))
	READ-BUFFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-READ-BUFFER-OFFSET 1))
	BIT-COUNT-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-BIT-COUNT-OFFSET 1))
	INITIATE-TRANSFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-START-TRANSMIT-OFFSET 1)))

  (SETQ
;Opcodes of packets
	RFC-OP 1 OPN-OP 2
	CLS-OP 3 FWD-OP 4
	ANS-OP 5 SNS-OP 6
	STS-OP 7 RUT-OP 10
	LOS-OP 11 LSN-OP 12
	MNT-OP 13 EOF-OP 14
        UNC-OP 15
	DAT-OP 200

;This is for printing out packets nicely.
	OPCODE-LIST '(ZERO? RFC OPN CLS FWD ANS SNS STS RUT LOS LSN MNT EOF UNC)

;Size-of-packet symbols.  These symbols define the protocol specified sizes.
        MAX-WORDS-PER-PKT 252.		;Largest packet possible in words
	MAX-DATA-WORDS-PER-PKT 244.	;Number of data words in largest packet
        MAX-DATA-BYTES-PER-PKT 488.	;Number of data bytes in largest packet
	FIRST-DATA-WORD-IN-PKT 8.	;Offset to first data word in packet

;Clock constants
	RETRANSMISSION-INTERVAL 60.	;1 SECOND
	PROBE-INTERVAL (* 60. 10.)	;10 SECONDS
	LONG-PROBE-INTERVAL (* 60. 60.) ;1 MINUTE
	HOST-DOWN-INTERVAL (* 60. 90.)	;1.5 MINUTES
        BACKGROUND-REQUESTS NIL         ;List of things for the background process to do
	RETRANSMISSION-NEEDED T

	DEFAULT-WINDOW-SIZE 15		;This is the default size of the window for a CONN
	MAXIMUM-WINDOW-SIZE 200		;This is the maximum size of the window for a CONN

;; NOTE WELL!  The number of index-conn slots must be more than the number
;; of hosts on the network for HOSTAT to work.
	MAXIMUM-INDEX 200		;Number of INDEX-CONN slots.
	MAXIMUM-INDEX-LOG-2-MINUS-1 (1- (HAULONG MAXIMUM-INDEX))

;; This array holds the CONN for the given index number.
;; It is big enough that no uniquizing is needed, since it is
;; used in circular fashion.
	INDEX-CONN (MAKE-ARRAY NIL ART-Q MAXIMUM-INDEX)
	INDEX-CONN-FREE-POINTER 1

;;; Connection list
        CONN-LIST NIL                   ;List of existing connections
	FREE-CONN-LIST NIL
	PROTOTYPE-CONN (MAKE-CONN)

;;; Recent headers
	RECENT-HEADERS (MAKE-ARRAY NIL ART-16B '(200 9.))
				;Array of 200 most recent packet transactions each row
				;containing the eight header words of the packet and the
				;time at which the record was made.

;The following are used for negotiating the initial connection between
;	a host and a server.
	PENDING-RFC-PKTS NIL	;Linked through the PKT-LINK
	PENDING-LISTENS NIL	;List of (CONTACT-NAME . CONN) for pending listens.

;;; Packet lists and other pointers
	FREE-PKTS NIL		;First pkt on the free list.
	MADE-PKTS NIL		;First pkt on the made list.

;; This array holds the uniquizer for the current (or last) connection for a given index
	UNIQUIZER-TABLE (MAKE-ARRAY NIL 'ART-16B MAXIMUM-INDEX)

;;; This array is the routing table:  if we want to send a message to a given
;;; subnet, to where should I forward it?  If the subnet # is greater than the
;;; length of the array, use the contents of array element zero.
;;; The contents of the array are the host number on our subnet who knows
;;; how to handle this packet.  NOTE that for now we can only be on one subnet.
;;; Don't use this table unless you are sure that the packet is not going to
;;; a host on THIS subnet!
;;; These tables are filled in by code below
	ROUTING-TABLE (MAKE-ARRAY PERMANENT-STORAGE-AREA ART-16B 32.)
	ROUTING-TABLE-COST (MAKE-ARRAY PERMANENT-STORAGE-AREA ART-16B 32.)

;;; Microcode and interrupt stuff
	INT-FREE-LIST-POINTER (ALOC (FUNCTION SYSTEM-COMMUNICATION-AREA)
                                    %SYS-COM-CHAOS-FREE-LIST)
	INT-RECEIVE-LIST-POINTER (ALOC (FUNCTION SYSTEM-COMMUNICATION-AREA)
                                       %SYS-COM-CHAOS-RECEIVE-LIST)
	INT-TRANSMIT-LIST-POINTER (ALOC (FUNCTION SYSTEM-COMMUNICATION-AREA)
                                        %SYS-COM-CHAOS-TRANSMIT-LIST)
	RESERVED-INT-PKT NIL        ;No top-level reserved packet

	ENABLE NIL		;set by (ENABLE)

  ;; This process runs all Time response actions such as PROBEs and Retransmission.
	BACKGROUND  (PROCESS-CREATE "Chaos Background")
	RECEIVER (PROCESS-CREATE "Chaos Receiver" ':SIMPLE-P T)

        )
  ;; Make 10 connections now so they're all on the same page.
  (DOTIMES (I 10)
    (PUSH (MAKE-CONN) FREE-CONN-LIST))

  ;; Initialize the routing table
  (FILLARRAY ROUTING-TABLE '(440))	;When in doubt, send to MC
  (FILLARRAY ROUTING-TABLE-COST '(1000))
  (ASET 426 ROUTING-TABLE 4)		;To AI via AI-CHAOS-11 (in case routing broken?)

) ;;; End of definition of INITIALIZE-NCP-ONCE


;;; Cold-boot initialization stuff
(DEFUN INITIALIZE-NCP-COLD ()
  ;; Debugging aids which keep records into the past (a short way).
  (SETQ BAD-PKT-LIST NIL	;List of strings describing packets received in error
        PKTS-BAD-CRC-SAVE-P NIL ;Don't defaultly save packets with bad CRC
	LOS-PKTS NIL		;LOS PKTs received from the network linked through PKT-LINK.
	MAX-LOS-PKTS-TO-KEEP 20	;Maximum number of LOS packets to keep on LOS-PKTS
				;There may actually be more but they will be used by allocator
	RECENT-HEADERS-POINTER 0
)
  (RESET-METERS)
  (SETUP-MY-ADDRESS))

(DEFUN SETUP-MY-ADDRESS NIL
  (SETQ MY-ADDRESS (%UNIBUS-READ MY-NUMBER-REGISTER)
					;Full address of this host.
	MY-SUBNET (LDB 0808 MY-ADDRESS)	;Subnet of this host.
	MY-FINGER-LOCATION-STRING
		(OR (CDR (ASSQ MY-ADDRESS FINGER-ALIST)) "(unknown location)")
	MY-NAME-STRING (OR (CAR (RASSOC MY-ADDRESS HOST-ALIST)) "CADR-?")))

;;; Initializations needed on every warm boot
(DEFUN INITIALIZE-NCP-SYSTEM ()
  (RESET)
  ;; This will cause the initialization to happen if it hasn't already
  (ADD-INITIALIZATION "CHAOS-NCP" '(INITIALIZE-NCP-COLD) '(:COLD :FIRST))
  (SETUP-MY-ADDRESS)
  (ENABLE))

;;; Low Level PKT Management

;;; PKT MANAGEMENT.

;;; Creates a new pkt.  Only allocates the storage, doesn't initialize anything.
;;; This should only be called by allocate and with interrupts inhibited
(DEFUN MAKE-PKT (&AUX PKT)
    (SETQ PKT (MAKE-ARRAY NIL 'ART-16B MAX-WORDS-PER-PKT NIL
			  (GET 'PKT-LEADER 'SI:DEFSTRUCT-SIZE)
			  NIL 'PKT))
    (SETF (PKT-STRING PKT)	     ;Create indirect array to reference as a string
	  (MAKE-ARRAY NIL 'ART-STRING MAX-DATA-BYTES-PER-PKT PKT '(0) 16.))
    (SETF (PKT-MADE-LINK PKT) MADE-PKTS)
    (SETQ MADE-PKTS PKT)
    PKT)

;;; Allocate a pkt off the free list. If none there, try old LOS pkts, else make one.
(DEFUN ALLOCATE-PKT (&AUX PKT)
  (WITHOUT-INTERRUPTS
    (SETQ PKT (COND (FREE-PKTS
		      (PROG1 FREE-PKTS
			     (SETQ FREE-PKTS (PKT-LINK FREE-PKTS))))
		    ((> CURRENT-LOS-PKT-COUNT MAX-LOS-PKTS-TO-KEEP)
		     (PROG1 LOS-PKTS
			    (SETQ LOS-PKTS (PKT-LINK LOS-PKTS))
			    (SETQ CURRENT-LOS-PKT-COUNT (1- CURRENT-LOS-PKT-COUNT))))
		    (T (SETQ PKTS-MADE (1+ PKTS-MADE))
		       (MAKE-PKT))))
    (SETF (PKT-TIME-TRANSMITTED PKT) 0)
    (SETF (PKT-TIMES-TRANSMITTED PKT) 0)
    (STORE-ARRAY-LEADER 0 (PKT-STRING PKT) 0)
    (SETF (PKT-LINK PKT) T)
    (SETF (PKT-OPCODE PKT) 0)
    (SETF (PKT-NBYTES PKT) 0)
    (SETF (PKT-FWD-COUNT PKT) 0)
    PKT))

;; Call this when the PKT is freeable as far as the MP level is concerned.
(DEFUN FREE-PKT (PKT)
  (WITHOUT-INTERRUPTS
   (COND ((NULL (PKT-BEING-RETRANSMITTED PKT))
          (SETF (PKT-LINK PKT) FREE-PKTS)
          (SETQ FREE-PKTS PKT))
         (T (SETF (PKT-BEING-RETRANSMITTED PKT) 'FREE)))))

;; Copies the contents of STRING into the PKT.  Adjusts the NBYTES and internal
;; string's fill pointer accordingly.
(DEFUN SET-PKT-STRING (PKT STRING &REST OTHER-STRINGS &AUX LEN)
    (COPY-ARRAY-PORTION (SETQ STRING (STRING STRING)) 0 (ARRAY-ACTIVE-LENGTH STRING)
			(PKT-STRING PKT) 0 (ARRAY-ACTIVE-LENGTH STRING))
    (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING))
    (COND (OTHER-STRINGS
	   (DO ((STRINGS OTHER-STRINGS (CDR STRINGS))
		(PKT-STRING (PKT-STRING PKT)))
	       ((OR (NULL STRINGS) (>= LEN MAX-DATA-BYTES-PER-PKT)))
	       (DO ((IDX 0 (1+ IDX))
		    (STR (STRING (CAR STRINGS)))
		    (STR-LEN (STRING-LENGTH (STRING (CAR STRINGS)))))
		   ((OR (>= IDX STR-LEN) (>= LEN MAX-DATA-BYTES-PER-PKT)))
		   (ASET (AREF STR IDX) PKT-STRING LEN)
		   (SETQ LEN (1+ LEN))))))
    (SETQ LEN (MIN MAX-DATA-BYTES-PER-PKT LEN))
    (SETF (PKT-NBYTES PKT) LEN)
    (STORE-ARRAY-LEADER LEN (PKT-STRING PKT) 0))

;;; INT-PKT management routines

;FREE-INT-PKT: returns an INT-PKT to the free list
(DEFUN FREE-INT-PKT (INT-PKT)
  (OR (= (%AREA-NUMBER INT-PKT) CHAOS-BUFFER-AREA)
      (FERROR NIL "Attempt to free non-interrupt packet ~A" INT-PKT))
  (PROG (OLD-FREE-LIST)
 LOOP (SETQ OLD-FREE-LIST (INT-FREE-LIST))
      (SETF (INT-PKT-THREAD INT-PKT) OLD-FREE-LIST)
      (OR (%STORE-CONDITIONAL INT-FREE-LIST-POINTER OLD-FREE-LIST INT-PKT)
          (GO LOOP))
      (%CHAOS-WAKEUP) ))

;CONVERT-TO-PKT: allocates a new packet, copies the INT-PKT to it, and then
;  deallocates the INT-PKT
(DEFMACRO CONVERT-TO-PKT (INT-PKT &OPTIONAL (FREE-PKT-FLAG T))
  `(LET ((PKT (ALLOCATE-PKT))
         (INT-PKT-INTERNAL ,INT-PKT) NW)
     (SETQ NW (PKT-NWORDS INT-PKT-INTERNAL))
     (COPY-ARRAY-PORTION INT-PKT-INTERNAL 0 NW PKT 0 NW)
     (STORE-ARRAY-LEADER (PKT-NBYTES INT-PKT-INTERNAL) (PKT-STRING PKT) 0)
     (AND ,FREE-PKT-FLAG (FREE-INT-PKT INT-PKT-INTERNAL))
     PKT))

;ALLOCATE-INT-PKT: allocates a new INT-PKT may have to wait for one, so be careful
; that it is ok that the process this gets called from can be safely suspended
; or that a packet is reserved  (in other words, freeing up INT-PKTS better
; not rely on the caller!).
(DEFUN ALLOCATE-INT-PKT (&AUX INT-PKT FREE-LIST)
  (COND ((NULL RESERVED-INT-PKT)
	 (DO () (NIL)
	   (SETQ FREE-LIST (INT-FREE-LIST))
	   (COND ((NULL FREE-LIST)
		  (PROCESS-WAIT "CHAOS buffer" #'(LAMBDA () (INT-FREE-LIST))))
		 ((%STORE-CONDITIONAL INT-FREE-LIST-POINTER
				      FREE-LIST (INT-PKT-THREAD FREE-LIST))
		  (RETURN (SETQ INT-PKT FREE-LIST))))))
	;; No WITHOUT-INTERRUPTS needed here since RESERVED-INT-PKT never non-null
	;; in a process, only inside the scheduler
	(T (SETQ INT-PKT RESERVED-INT-PKT
		 RESERVED-INT-PKT NIL)))
  (SETF (INT-PKT-THREAD INT-PKT) NIL)
  INT-PKT)

(DEFUN CONVERT-TO-INT-PKT (PKT &AUX INT-PKT NW)
  (SETQ INT-PKT (ALLOCATE-INT-PKT))
  (SETQ NW (PKT-NWORDS PKT))
  (COPY-ARRAY-PORTION PKT 0 NW INT-PKT 0 NW)
  (SETF (INT-PKT-WORD-COUNT INT-PKT) NW)		;This is probably superfluous
  INT-PKT)

;;; Low Level CONN Management

;;; CONN MANAGEMENT.

;;; Create a connection.  Returns the connection.
(DEFUN MAKE-CONNECTION ( &OPTIONAL CONN &AUX CONS)
    (WITHOUT-INTERRUPTS
      (COND (CONN )			;Caller supplying CONN to be recycled
	    ((SETQ CONS FREE-CONN-LIST)	;Recycle one
	     (SETQ FREE-CONN-LIST (CDR CONS)
		   CONN (CAR CONS))
	     (COPY-ARRAY-CONTENTS PROTOTYPE-CONN CONN))
	    ((SETQ CONN (MAKE-CONN)))))
    (OR (EQ (STATE CONN) 'INACTIVE-STATE)
	(FERROR NIL "Attempt to reuse ~S, which is in the ~A, not INACTIVE-STATE"
		CONN (STATE CONN)))
    (AND (MEMQ CONN FREE-CONN-LIST)
	 (FERROR NIL "You can't reuse this connection, it's been freed"))
    (AND (MEMQ CONN CONN-LIST)
	 (FERROR NIL "You can't reuse this connection, it's already in use"))
    (DO ((FP (\ (1+ INDEX-CONN-FREE-POINTER) MAXIMUM-INDEX) (\ (1+ FP) MAXIMUM-INDEX))
	 (COUNTER MAXIMUM-INDEX (1- COUNTER)))
	((%STORE-CONDITIONAL (AP-1 INDEX-CONN (COND ((= FP 0) (SETQ FP 1))
						    (T FP)))
			     NIL
			     CONN)
	 (SETQ INDEX-CONN-FREE-POINTER FP)
	 (SETF (LOCAL-INDEX-NUM CONN) (DPB (AS-1 (1+ (AR-1 UNIQUIZER-TABLE FP))
						 UNIQUIZER-TABLE FP)
					   (DPB MAXIMUM-INDEX-LOG-2-MINUS-1
						0606
						(- 20 MAXIMUM-INDEX-LOG-2-MINUS-1))
					   FP))
	 (WITHOUT-INTERRUPTS
	   (SETQ CONN-LIST (RPLACD (OR CONS (NCONS CONN)) CONN-LIST)))
	 CONN)
      (AND (MINUSP COUNTER)
	   (FERROR NIL "Connection table full"))))

;;; Given a connection, makes it null and void.
(DEFUN REMOVE-CONN (CONN)
  (WITHOUT-INTERRUPTS
    (FREE-ALL-READ-PKTS CONN)
    (FREE-ALL-RECEIVED-PKTS CONN)
    (FREE-ALL-SEND-PKTS CONN)
    (SETF (STATE CONN) 'INACTIVE-STATE)
    (AS-1 NIL INDEX-CONN (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (LOCAL-INDEX-NUM CONN)))
    (LET ((CONS (MEMQ CONN CONN-LIST)))
      (SETQ CONN-LIST (DELQ CONN CONN-LIST))
      (OR (MEMQ CONN FREE-CONN-LIST)
	  (SETQ FREE-CONN-LIST (RPLACD (OR CONS (NCONS CONN)) FREE-CONN-LIST))))
    (DOLIST (X PENDING-LISTENS)
      (AND (EQ (CDR X) CONN) (SETQ PENDING-LISTENS (DELQ X PENDING-LISTENS))))
    NIL))

;Must be called with interrupts off.
(DEFUN FREE-ALL-READ-PKTS (CONN)
    (DO ((PKT (READ-PKTS CONN) (PKT-LINK PKT))
	 (PREV NIL PKT))
	(NIL)
      (AND PREV (FREE-PKT PREV))
      (OR PKT (RETURN NIL)))
    (SETF (READ-PKTS CONN) NIL)
    (SETF (READ-PKTS-LAST CONN) NIL))

;Must be called with interrupts off.
(DEFUN FREE-ALL-RECEIVED-PKTS (CONN)
    (DO ((PKT (RECEIVED-PKTS CONN) (PKT-LINK PKT))
	 (PREV NIL PKT))
	(NIL)
      (AND PREV (FREE-PKT PREV))
      (OR PKT (RETURN NIL)))
    (SETF (RECEIVED-PKTS CONN) NIL))

;Must be called with interrupts off.
(DEFUN FREE-ALL-SEND-PKTS (CONN)
    (DO ((PKT (SEND-PKTS CONN) (PKT-LINK PKT))
	 (PREV NIL PKT))
	(NIL)
      (AND PREV (FREE-PKT PREV))	;This offseting so it doesnt rely on PKT-LINK of
      (OR PKT (RETURN NIL)))		; a PKT it has freed.
    (SETF (SEND-PKTS CONN) NIL)
    (SETF (SEND-PKTS-LAST CONN) NIL)
    (SETF (SEND-PKTS-LENGTH CONN) 0))


;Causes the CONN's INTERRUPT-FUNCTION to be run in the background process with the
; specified reason and arguments
; Reasons are:
;	:INPUT			input has arrived
;	:OUTPUT			the window, which was full, now has room in it
;	:CHANGE-OF-STATE	the state of the connection has just changed
(DEFUN INTERRUPT-CONN (REASON CONN &REST ARGS &AUX (IFUN (INTERRUPT-FUNCTION CONN)))
  (AND IFUN
       (BACKGROUND-TASK `(INTERRUPT-CONN-INTERNAL ',IFUN ',REASON ',CONN
						  ',(APPEND ARGS NIL)))))

;If while the request was on the queue, the connection was flushed, get rid
;of the interrupt.  Because of connection reusing, this is somewhat heuristic.
(DEFUN INTERRUPT-CONN-INTERNAL (IFUN REASON CONN ARGS)
  (OR (EQ (STATE CONN) 'INACTIVE-STATE)
      (NEQ (INTERRUPT-FUNCTION CONN) IFUN)
      (LEXPR-FUNCALL IFUN REASON CONN ARGS)))

;;; High Level Transmission Routines

;;; These are the routines which cause a packet to be queued for transmission.

;;; Put the pkt on the transmit list, and create a phony transmitter interrupt
;;;   if needed so that the interrupt level will start sending.
;;; If the second arg is T, put an ACK aboard this PKT.
;;; This is a very low level function, called mainly by the following 2 functions.
;;; (Also called by the retransmitter and forwarder.)
(DEFUN TRANSMIT-PKT (PKT &OPTIONAL ACK-P)
    (AND (> (PKT-NBYTES PKT) MAX-DATA-BYTES-PER-PKT)
	 (FERROR NIL "Attempt to transmit an invalid packet (~S).~%The length ~O is greater than the maximum packet size, ~O"
		 PKT (PKT-NBYTES PKT) MAX-DATA-BYTES-PER-PKT))
    (COND (ACK-P
	    (WITHOUT-INTERRUPTS
	      (LET ((CONN (PKT-SOURCE-CONN PKT)))
		(OR CONN (FERROR NIL "~S has null connection" PKT))
		(LET ((ACKN (PKT-NUM-READ CONN)))
		  (SETF (PKT-ACK-NUM PKT) ACKN)
		  (SETF (PKT-NUM-ACKED CONN) ACKN))))))
    (SETF (PKT-TIME-TRANSMITTED PKT) (TIME))
    (SETF (PKT-TIMES-TRANSMITTED PKT) (1+ (PKT-TIMES-TRANSMITTED PKT)))
    (TRANSMIT-INT-PKT (CONVERT-TO-INT-PKT PKT)) )

(DEFUN TRANSMIT-INT-PKT-FOR-CONN (CONN PKT)
    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
    (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
    (SETF (PKT-DEST-ADDRESS PKT) (FOREIGN-ADDRESS CONN))
    (SETF (PKT-DEST-INDEX-NUM PKT) (FOREIGN-INDEX-NUM CONN))
    (WITHOUT-INTERRUPTS
      (LET ((ACKN (PKT-NUM-READ CONN)))
        (SETF (PKT-ACK-NUM PKT) ACKN)
	(SETF (PKT-NUM-ACKED CONN) ACKN)))
    (TRANSMIT-INT-PKT PKT))

;;; Given a losing pkt or an RFC we want to reject, shuffle the
;;; pkt and return it.  Caller must specify opcode, either LOS or CLS.
;;; If the OP is CLS, include a string which is the reason the RFC was
;;; rejected.  Note that the very same pkt is used, so when this is called
;;; the pkt had better not be on any lists or anything.
(DEFUN TRANSMIT-LOS-INT-PKT (INT-PKT OP &OPTIONAL REASON &AUX DH DI LEN)
  (SETF (PKT-OPCODE INT-PKT) OP)
  (COND (REASON
	  (SETF (PKT-NBYTES INT-PKT) (SETQ LEN (ARRAY-ACTIVE-LENGTH REASON)))
	  (DO ((SIDX 0 (+ SIDX 2))
	       (WIDX FIRST-DATA-WORD-IN-PKT (1+ WIDX)))
	      (( SIDX LEN))
	    (ASET (DPB (IF (ODDP LEN) 0 (AREF REASON (1+ SIDX))) 1010 (AREF REASON SIDX))
		  INT-PKT WIDX))))
  (SETQ DH (PKT-DEST-ADDRESS INT-PKT)
	DI (PKT-DEST-INDEX-NUM INT-PKT))
  (SETF (PKT-DEST-ADDRESS INT-PKT) (PKT-SOURCE-ADDRESS INT-PKT))
  (SETF (PKT-DEST-INDEX-NUM INT-PKT) (PKT-SOURCE-INDEX-NUM INT-PKT))
  (SETF (PKT-SOURCE-ADDRESS INT-PKT) DH)
  (SETF (PKT-SOURCE-INDEX-NUM INT-PKT) DI)
  (TRANSMIT-INT-PKT INT-PKT))

;;; Send a normal pkt (i.e., not LOS nor DATA)
;;; Caller must allocate the pkt, and fill in the opcode, nbytes, and data parts.
;;; The PKT and ACK-PKT numbers to place in the packet are optional arguments,
;;; they default to 0.  If T is provided for either, the usual thing happens.
(DEFUN TRANSMIT-NORMAL-PKT (CONN PKT &OPTIONAL (PKTN 0) (ACK-PKTN 0) &AUX ACK-P)
    (COND ((EQ PKTN T)
	   (SETQ PKTN (PKTNUM-1+ (PKT-NUM-SENT CONN)))
	   (SETF (PKT-NUM-SENT CONN) PKTN)))
    (SETF (PKT-NUM PKT) PKTN)
    (COND ((EQ ACK-PKTN T) (SETQ ACK-P T))
	  (T (SETF (PKT-ACK-NUM PKT) ACK-PKTN)))
    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
    (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
    (SETF (PKT-DEST-ADDRESS PKT) (FOREIGN-ADDRESS CONN))
    (SETF (PKT-DEST-INDEX-NUM PKT) (FOREIGN-INDEX-NUM CONN))
    (TRANSMIT-PKT PKT ACK-P))

(SPECIAL STS-WHY-ARRAY)
(SETQ STS-WHY-ARRAY (MAKE-ARRAY NIL 'ART-Q 100 NIL '(0 100)))

(DEFUN PRINT-STS-WHY ()
  (LET ((N (ARRAY-LEADER STS-WHY-ARRAY 1)))
    (DO I (\ (1+ N) 100) (\ (1+ I) 100) NIL
      (PRINT (AR-1 STS-WHY-ARRAY I))
      (AND (= I N) (RETURN NIL)))))

;;; Internal routine to send a status packet to a connection.
(DEFUN TRANSMIT-STS (CONN WHY &AUX PKT)
    (AS-1 WHY STS-WHY-ARRAY (ARRAY-LEADER STS-WHY-ARRAY 1))
    (STORE-ARRAY-LEADER (\ (1+ (ARRAY-LEADER STS-WHY-ARRAY 1)) 100)
			STS-WHY-ARRAY 1)
    (SETQ PKT (ALLOCATE-INT-PKT))
    (SETF (PKT-OPCODE PKT) STS-OP)
    (SETF (PKT-NBYTES PKT) 4)
    (SETF (PKT-FIRST-DATA-WORD PKT) (PKT-NUM-RECEIVED CONN))
    (SETF (PKT-SECOND-DATA-WORD PKT) (LOCAL-WINDOW-SIZE CONN))
    (TRANSMIT-INT-PKT-FOR-CONN CONN PKT))

;; Output-Main Program level

;;; Release a packet to a routine outside the NCP.
;;; This routine should be called whenever returning a packet as a value
;;;	to a caller which is outside the NCP
(DEFUN RELEASE-PKT (PKT)
  (COND ((NULL (PKT-STATUS PKT))
         (SETF (PKT-STATUS PKT) 'RELEASED)
         (SETF (PKT-LINK PKT) NIL))
        (T (FERROR NIL "Attempt to release ~S, which is already released" PKT))))

;;; To send a PKT, first call GET-PKT to give you a pkt, fill it full of
;;; cruft and set its NBYTES, and then call SEND-PKT on it.

(DEFUN GET-PKT ( &AUX PKT)
  (SETQ PKT (ALLOCATE-PKT))
  (RELEASE-PKT PKT)
  PKT)

;; CONN must be in OPEN-STATE, and the OPCODE must be a DAT opcode.
(DEFUN SEND-PKT (CONN PKT &OPTIONAL (OPCODE DAT-OP))
  (SELECTQ (STATE CONN)
    (OPEN-STATE
      (OR (BIT-TEST DAT-OP OPCODE) (= EOF-OP OPCODE)
	  (FERROR NIL "~O is not a legal opcode" OPCODE))
      (PROCESS-WAIT "NETO"
		    (FUNCTION (LAMBDA (X) (OR (MAY-TRANSMIT X)
					      (NEQ (STATE X) 'OPEN-STATE))))
		    CONN)
      (COND ((EQ (STATE CONN) 'OPEN-STATE)
	     (OR (EQ (PKT-STATUS PKT) 'RELEASED)
		 (FERROR NIL "Attempt to transmit ~S, which is not released" PKT))
	     (SETF (PKT-STATUS PKT) NIL)
	     (SETF (PKT-OPCODE PKT) OPCODE)
	     (SETF (WINDOW-AVAILABLE CONN) (1- (WINDOW-AVAILABLE CONN)))
	     (TRANSMIT-NORMAL-PKT CONN PKT T T)   ;And send it for the first time.
	     (WITHOUT-INTERRUPTS		  ;Must do the transmit before putting it
	       (LET ((LAST (SEND-PKTS-LAST CONN)))  ;in SEND-PKTS because TRANSMIT-NORMAL-PKT
		 (COND (LAST (SETF (PKT-LINK LAST) PKT)) ;fills in lots of fields.
		       (T (SETF (SEND-PKTS CONN) PKT)))
		 (SETF (SEND-PKTS-LAST CONN) PKT)
		 (SETF (PKT-LINK PKT) NIL)
		 (SETF (SEND-PKTS-LENGTH CONN) (1+ (SEND-PKTS-LENGTH CONN)))
		 (SETQ RETRANSMISSION-NEEDED T))))
	    (T (FERROR 'NOT-OPEN-STATE
		       "Attempt to send on ~S which went into ~S"
		       CONN (STATE CONN)))))
    (LOS-RECEIVED-STATE
      (FERROR 'LOS-RECEIVED-STATE
	      "Attempt to transmit on a connection which got a LOS:~A"
	      (PKT-STRING (READ-PKTS-LAST CONN))))
    (OTHERWISE
      (FERROR 'NOT-OPEN-STATE
	      "Attempt to send on ~S, which is in the ~A, not OPEN-STATE"
	      CONN (STATE CONN)))))

(DEFUN SEND-STRING (CONN &REST STRINGS &AUX PKT)
  (SETQ PKT (GET-PKT))
  (LEXPR-FUNCALL #'SET-PKT-STRING PKT STRINGS)
  (SEND-PKT CONN PKT))

;; User level routine to transmit an uncontrolled packet.
(DEFUN SEND-UNC-PKT (CONN PKT
                     &OPTIONAL (PKTN-FIELD (PKT-NUM PKT)) (ACK-FIELD (PKT-ACK-NUM PKT)))
  (SETF (PKT-OPCODE PKT) UNC-OP)
  (TRANSMIT-NORMAL-PKT CONN PKT PKTN-FIELD ACK-FIELD))

;; Predicate: may we send (at MP level)?
(DEFUN MAY-TRANSMIT (CONN)
  (> (WINDOW-AVAILABLE CONN) 0))

;; Predicate: anything to receive?
(DEFUN DATA-AVAILABLE (CONN)
  (NOT (NULL (READ-PKTS CONN))))

;; Wait for all packets to be sent and acknowledged, or for the conn to go into an
;; illegal state.  If successful, return T.  If conn goes into bad state, return NIL
;; No error, regardless of what the state is (it may well already be closed
;; by the time this is called from the :EOF stream operation, for instance,
;; if sequence-breaks are on.)
(DEFUN FINISH (CONN &OPTIONAL (WHOSTATE "Net Finish"))
    (PROCESS-WAIT WHOSTATE #'FINISHED-P CONN)
    (EQ (STATE CONN) 'OPEN-STATE))

;;; Predicate to determine if all packets have been sent and acknowledged
;;; Also returns T if connection is broken
(DEFUN FINISHED-P (CONN)
  (OR (>= (WINDOW-AVAILABLE CONN) (FOREIGN-WINDOW-SIZE CONN))
      (NEQ (STATE CONN) 'OPEN-STATE)))

;; Input-Main Program level

;;; If this returns NIL, it means there were no PKTs, or that we have PKTs
;;; but not the sequentially next one.
;;; Else it should return a PKT, which may be a CLS, ANS, or DAT (or UNC).
;;; Be sure to give the PKT back (by giving it to RETURN-PKT when you are done with it).
(DEFUN GET-NEXT-PKT (CONN &OPTIONAL (NO-HANG-P NIL) &AUX PKT)
  ;; Loop until we get a packet, decide not to hang, or error out
  (DO () (NIL)
    ;; Check for connection in an erroneous state
    (COND (NO-HANG-P)		;If not going blocked, then allow trying to read in any state
	  ((EQ (STATE CONN) 'HOST-DOWN-STATE)
	   (FERROR 'HOST-DOWN
		   "Attempt to get a packet from ~S, a connection whose foreign host died"
		   CONN))
	  ((EQ (STATE CONN) 'LOS-RECEIVED-STATE)
	   (FERROR 'LOS-RECEIVED-STATE
		   "Attempt to receive from a connection which got a LOS:~A"
		   (PKT-STRING (READ-PKTS-LAST CONN))))
	  ((AND (EQ (STATE CONN) 'CLS-RECEIVED-STATE) ;Don't complain until all pkts read
		(NULL (READ-PKTS CONN)))
	   (FERROR 'READ-ON-CLOSED-CONNECTION
	 "Attempt to get a packet from ~S, a connection which has been closed by foreign host"
		   CONN))
	  ((NOT (MEMQ (STATE CONN) '(OPEN-STATE RFC-RECEIVED-STATE CLS-RECEIVED-STATE
						ANSWERED-STATE)))
	   (FERROR NIL
		   "Attempt to get a packet from ~S, which is in ~S, not a valid state"
		   CONN (STATE CONN))))
    ;; Now see if there are any packets we can have
    (WITHOUT-INTERRUPTS
      (SETQ PKT (READ-PKTS CONN))
      (COND (PKT				;Got packet, take off of read list
	     (AND ( UNC-OP (PKT-OPCODE PKT))
		  (SETF (PKT-NUM-READ CONN) (PKT-NUM PKT)))
	     (SETF (READ-PKTS CONN) (PKT-LINK PKT))
	     (COND ((NULL (READ-PKTS CONN))
		    (SETF (READ-PKTS-LAST CONN) NIL))))))
    (AND (NOT (NULL PKT))			;Got packet, acknowledge if necessary
	 (EQ (STATE CONN) 'OPEN-STATE)
	 ( (* 3 (PKTNUM-- (PKT-NUM PKT) (PKT-NUM-ACKED CONN))) (LOCAL-WINDOW-SIZE CONN))
	 (TRANSMIT-STS CONN 'WINDOW-FULL))
    (AND PKT					;Got packet, release from NCP
	 (RELEASE-PKT PKT))
    (AND (OR PKT NO-HANG-P) (RETURN PKT))	;If satisfied, return
    ;; Not satisfied, wait for something interesting to happen
    (PROCESS-WAIT "NETI" (FUNCTION (LAMBDA (X) (OR (READ-PKTS X)
						   (NEQ (STATE X) 'OPEN-STATE))))
			 CONN)))

;;; RETURN a released PKT to the NCP
(DEFUN RETURN-PKT (PKT)
  (SELECTQ (PKT-STATUS PKT)
    (RELEASED
     (SETF (PKT-STATUS PKT) NIL)
     (FREE-PKT PKT))
    (:OTHERWISE
     (FERROR NIL "Attempt to return unreleased packet (~S) to the NCP" PKT))))

;; Receiver Interrupt level

;;; RECEIVER FUNCTIONS:  These run at receiver interrupt level.

;;; This function is the called on an INT-PKT which has just come in from the net.
;;; It is mostly a transfer vector to more specialized functions, but it also
;;; does error checking.
;;; Note: Functions reached from here must not, in any case, go blocked, particularily
;;;  now that the RECEIVER process is flushed.  A common case is to call functions 
;;;  which ordinarily could go blocked waiting for a INT-PKT, but this is prevented from
;;;  happening by setting up RESERVED-INT-PKT to the INT-PKT just received.
;;; PUP-INT-PKT and PUP-INT-PKT-PORT are used in connection with EFTP.
(DEFUN RECEIVE-INT-PKT (INT-PKT &AUX (OP (PKT-OPCODE INT-PKT)) CONN ACKN
				     (VERSION (LDB 0010 (AREF INT-PKT 0)))) ;Header version
  (COND ((ZEROP VERSION)
    (COND ((= OP RUT-OP)
	   (DO ((I FIRST-DATA-WORD-IN-PKT (+ I 2))
		(N (// (PKT-NBYTES INT-PKT) 4) (1- N))
		(GATEWAY (PKT-SOURCE-ADDRESS INT-PKT))
		(N-SUBNETS (ARRAY-LENGTH ROUTING-TABLE))
		(SUBNET) (COST))
	       ((ZEROP N) (FREE-INT-PKT INT-PKT))
	     (SETQ SUBNET (AREF INT-PKT I) COST (AREF INT-PKT (1+ I)))
	     (COND ((AND (< SUBNET N-SUBNETS)
			 ( COST (AREF ROUTING-TABLE-COST SUBNET)))
		    (ASET GATEWAY ROUTING-TABLE SUBNET)
		    (ASET COST ROUTING-TABLE-COST SUBNET)))))
	  ((ZEROP (INT-PKT-HARDWARE-DEST INT-PKT))
	   (RECEIVE-BROADCAST-INT-PKT INT-PKT))
	  (( (PKT-DEST-ADDRESS INT-PKT) MY-ADDRESS)	;Packet to be forwarded
	   (COND ((OR (= (PKT-FWD-COUNT INT-PKT) 17)
		      (> (PKT-NBYTES INT-PKT) MAX-DATA-BYTES-PER-PKT))
		  (FREE-INT-PKT INT-PKT)
		  (SETQ PKTS-OVER-FORWARDED (1+ PKTS-OVER-FORWARDED)))
		 (T (SETF (PKT-FWD-COUNT INT-PKT) (1+ (PKT-FWD-COUNT INT-PKT)))
		    (SETQ PKTS-FORWARDED (1+ PKTS-FORWARDED))
		    (TRANSMIT-INT-PKT INT-PKT))))
	  (T (RECORD-INT-PKT-HEADER INT-PKT)
	     (AND (BIT-TEST 200 OP) (SETQ DATA-PKTS-IN (1+ DATA-PKTS-IN)))
	     (COND
	      ((= OP RFC-OP)
	       (RECEIVE-RFC INT-PKT))
	      ((= OP LOS-OP)
	       (RECEIVE-LOS INT-PKT))
	      ((= OP CLS-OP)
	       (RECEIVE-CLS INT-PKT))
	      ((= OP MNT-OP)
	       (FREE-INT-PKT INT-PKT))
	      ((NULL (COND ((SETQ CONN (PKT-DEST-CONN INT-PKT))
			    (SETF (TIME-LAST-RECEIVED CONN) (TIME))
			    CONN)))
	       (COND ((NOT (= OP SNS-OP))
		      (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "No such index exists"))
		     (T (FREE-INT-PKT INT-PKT))))  ;Ignore SNS's to non-existent connections
	      ((NOT (= (PKT-SOURCE-ADDRESS INT-PKT) (FOREIGN-ADDRESS CONN)))
	       (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "You did not initiate this connection"))
	      ((= OP OPN-OP)
	       (RECEIVE-OPN CONN INT-PKT))
	      ((= OP FWD-OP)
	       (RECEIVE-FWD CONN INT-PKT))
	      ((= OP ANS-OP)
	       (RECEIVE-ANS CONN INT-PKT))
	      ((NOT (OR (= OP SNS-OP) (= OP STS-OP)
                        (= OP EOF-OP) (= OP UNC-OP) (>= OP DAT-OP)))
	       (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Illegal opcode"))
	      ((NOT (= (PKT-SOURCE-INDEX-NUM INT-PKT) (FOREIGN-INDEX-NUM CONN)))
	       (TRANSMIT-LOS-INT-PKT
		    INT-PKT LOS-OP "That is not your index number for this connection"))
	  ;;; Below here can be UNC, SNS, STS, EOF, or DAT.  All but UNC have ACK fields.
	      ((NOT (EQ (STATE CONN) 'OPEN-STATE))
	       (COND ((= OP SNS-OP) (FREE-INT-PKT INT-PKT))
		     (T (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Connection not open"))))
              ((= OP UNC-OP)
               (RECEIVE-EOF-UNC-OR-DAT CONN INT-PKT))
	      (T
	   ;;; Below here, this INT-PKT contains a normal acknowledgement field.
	       (SETQ ACKN (PKT-ACK-NUM INT-PKT))	;Acknowledgement field
	       (RECEIPT CONN ACKN)		;Clear receipted packets from send list
	       (AND (PKTNUM-< (SEND-PKT-ACKED CONN) ACKN)
		    (SETF (SEND-PKT-ACKED CONN) ACKN))
	       (UPDATE-WINDOW-AVAILABLE CONN)
	       (COND ((OR (>= OP DAT-OP) (= OP EOF-OP))
		      (RECEIVE-EOF-UNC-OR-DAT CONN INT-PKT))
		     ((= OP SNS-OP)
		      (RECEIVE-SNS CONN INT-PKT))
		     ((= OP STS-OP)
		      (RECEIVE-STS CONN INT-PKT))))))))
	((AND (= VERSION 1)			;Muppet
	      (= (LDB 1010 (AREF INT-PKT 0)) 3)	;containing PUP
	      (NULL PUP-INT-PKT)		;and buffer not full
	      (NOT (NULL PUP-INT-PKT-PORT))	;and directed to right port
	      (ZEROP (PUP-DEST-PORT-HIGH INT-PKT))
	      (= (PUP-DEST-PORT-LOW INT-PKT) PUP-INT-PKT-PORT))
	 (SETQ PUP-INT-PKT INT-PKT))		;Accept it.  Other process will handle.
	(T					;Ignore this packet
	 (FREE-INT-PKT INT-PKT))))

(DEFUN RECORD-INT-PKT-HEADER (INT-PKT)
    (DO I 0 (1+ I) (= I 8.)
      (AS-2 (AR-1 INT-PKT I) RECENT-HEADERS RECENT-HEADERS-POINTER I))
    (AS-2 (TIME) RECENT-HEADERS RECENT-HEADERS-POINTER 8.)
    (SETQ RECENT-HEADERS-POINTER (\ (1+ RECENT-HEADERS-POINTER) 200)))

;Discard packets from send-list which have been receipted by other end
(DEFUN RECEIPT (CONN ACK-LEV)
 (WITHOUT-INTERRUPTS
  (LET ((SENDS (SEND-PKTS CONN))	;(Save array references...)
	(NEXT NIL)			;Prevent weird screw.
	(LENGTH (SEND-PKTS-LENGTH CONN)))
    (DO ((PKT SENDS NEXT))		;For each PKT not yet ACKed which this ACKs,
	((OR (NULL PKT) (PKTNUM-< ACK-LEV (PKT-NUM PKT))))
;     (SETQ NEXT (PKT-LINK PKT))
      (SETQ NEXT (SETQ SENDS (PKT-LINK PKT)))  ;Two variables only for "clairity"
      (FREE-PKT PKT)
      (SETQ LENGTH (1- LENGTH)))
    
    (SETF (SEND-PKTS CONN) SENDS)
    (SETF (SEND-PKTS-LENGTH CONN) LENGTH)
    (COND ((NULL SENDS) (SETF (SEND-PKTS-LAST CONN) NIL))))))

;;; A new ack has come in, so adjust the amount left in the window.  If the window was
;;; full, and has now become "un-full", cause an output interrupt
(DEFUN UPDATE-WINDOW-AVAILABLE (CONN &AUX (AVAILABLE (WINDOW-AVAILABLE CONN)))
  (SETF (WINDOW-AVAILABLE CONN)
	(MAX AVAILABLE			;in case rcvd out of order
	     (- (FOREIGN-WINDOW-SIZE CONN)
		(PKTNUM-- (PKT-NUM-SENT CONN) (SEND-PKT-ACKED CONN)))))
  (AND (ZEROP AVAILABLE) (NOT (ZEROP (WINDOW-AVAILABLE CONN)))
       (INTERRUPT-CONN ':OUTPUT CONN)))

;Called when a broadcast packet is received from the net.  For now, ignore it.
(DEFUN RECEIVE-BROADCAST-INT-PKT (INT-PKT)
  (FREE-INT-PKT INT-PKT))

;The following functions are called to process the receipt of a particular kind of packet.

(DEFUN RECEIVE-STS (CONN INT-PKT)
  (RECEIPT CONN (PKT-FIRST-DATA-WORD INT-PKT))
  (SETF (FOREIGN-WINDOW-SIZE CONN) (PKT-SECOND-DATA-WORD INT-PKT))
  (UPDATE-WINDOW-AVAILABLE CONN)
  (FREE-INT-PKT INT-PKT)
;  (BACKGROUND-TASK `(RETRANSMISSION ',CONN))  ;Background retransmits on all every time
;					; around anyway, but this might speed it up a little
; Actually, above would only slow it down by consing and calling EVAL unnecessarily.
  )

;;;   When this is called, CONN is known to be in OPEN-STATE.
(DEFUN RECEIVE-SNS (CONN INT-PKT)
    (SETQ RESERVED-INT-PKT INT-PKT)
    (TRANSMIT-STS CONN 'SNS))

;;; This uses the PKT-NUM to correctly order the PKTs.
;;;	If the PKT-NUM is less than or equal to the highest we received
;;;	     ignore it and send a receipt
;;;	If one higher then this one is added to the end of the successfully recieved PKTs.
;;;	     Then the out of sequence list is appended to the insequence list and the
;;;		point of break in sequence is found whereupon the list is broken
;;;		and all the appropriate pointers are set up.
;;;	If more than one larger try locating it's position in the out of order list
;;; When this is called, CONN is known to be in OPEN-STATE.
(DEFUN RECEIVE-EOF-UNC-OR-DAT (CONN INT-PKT &AUX PKT PKT-NUM PKTL-NUM PREV)
    (SETQ PKT-NUM (PKT-NUM INT-PKT))
    (COND ((= UNC-OP (PKT-OPCODE INT-PKT))
           (SETQ PKT (CONVERT-TO-PKT INT-PKT))
	   (AND (NULL (READ-PKTS CONN))
		(INTERRUPT-CONN ':INPUT CONN))
	   (WITHOUT-INTERRUPTS
	     (SETF (PKT-LINK PKT) (READ-PKTS CONN))
	     (SETF (READ-PKTS CONN) PKT)
	     (AND (NULL (READ-PKTS-LAST CONN))
		  (SETF (READ-PKTS-LAST CONN) PKT))))
          ((NOT (PKTNUM-< (PKT-NUM-RECEIVED CONN) PKT-NUM))
           (SETQ RESERVED-INT-PKT INT-PKT)
	   (SETQ PKTS-DUPLICATED (1+ PKTS-DUPLICATED))
	   (TRANSMIT-STS CONN '<-NUM-RCVD))     ;This is a duplicate, receipt and ignore
	  ((= PKT-NUM (PKTNUM-1+ (PKT-NUM-RECEIVED CONN)))
	   ;;; This is the one we were waiting for add it to READ-PKTS
           (SETQ PKT (CONVERT-TO-PKT INT-PKT))
	   (AND (NULL (READ-PKTS CONN))
		(INTERRUPT-CONN ':INPUT CONN))
	   (WITHOUT-INTERRUPTS
	     (SETF (PKT-LINK PKT) (RECEIVED-PKTS CONN))	;Link the two lists together
	     (SETF (RECEIVED-PKTS CONN) NIL)
	     (COND ((NULL (READ-PKTS-LAST CONN))
		    (SETF (READ-PKTS CONN) PKT))
		   (T (SETF (PKT-LINK (READ-PKTS-LAST CONN)) PKT)))
	     (DO ((PKTL-NUM (PKT-NUM PKT)
			    (PKTNUM-1+ PKTL-NUM)))
		 ((OR (NULL PKT)
		      ( PKTL-NUM (PKT-NUM PKT)))
		  (SETF (PKT-NUM-RECEIVED CONN) (PKTNUM-- PKTL-NUM 1))
		  (SETF (RECEIVED-PKTS CONN) PKT)
		  (AND PREV (SETF (PKT-LINK PREV) NIL))
		  (SETF (READ-PKTS-LAST CONN) PREV))
	       (SETQ PREV PKT)
	       (SETQ PKT (PKT-LINK PKT)))))
          (T (WITHOUT-INTERRUPTS 
	       (DO ((PKTL (RECEIVED-PKTS CONN) (PKT-LINK PKTL))
		    (PREV NIL PKTL))
		   ((NULL PKTL)
		    (SETQ PKT (CONVERT-TO-PKT INT-PKT))
		    (COND ((NULL PREV) (SETF (RECEIVED-PKTS CONN) PKT))
			  (T (SETF (PKT-LINK PREV) PKT)))
		    (SETF (PKT-LINK PKT) NIL))
		 (SETQ PKTL-NUM (PKT-NUM PKTL))
		 (COND ((= PKT-NUM PKTL-NUM)    ;Same as existing one, forget about it.
			(SETQ RESERVED-INT-PKT INT-PKT)
			(TRANSMIT-STS CONN 'ALREADY-ON-RCVD-PKTS)       ;Send a receipt
			(RETURN NIL))
		       ((PKTNUM-< PKT-NUM PKTL-NUM)	;This is the place!
                        (SETQ PKT (CONVERT-TO-PKT INT-PKT))
			(COND ((NULL PREV)
			       (SETF (PKT-LINK PKT) (RECEIVED-PKTS CONN))
			       (SETF (RECEIVED-PKTS CONN) PKT))
			      (T
				(SETF (PKT-LINK PKT) (PKT-LINK PREV))
				(SETF (PKT-LINK PREV) PKT)))
			(RETURN NIL))))))))

;;; If RFC matches a pending LSN, call RFC-MEETS-LSN, else if there is a server,
;;; add to pending list and start up a server.
;;; (So far all we have done is verified PKT-DEST-ADDRESS.)
;;; Note that because of RFC-ANS stuff, the contact "name" is not the
;;; whole string, so we must do a simple parse.
(DEFUN RECEIVE-RFC (INT-PKT &AUX PKT LSN SERVER CONTACT-NAME CONN)
  (COND ((OR (DO ((TST-PKT PENDING-RFC-PKTS (PKT-LINK TST-PKT)))
                 ((NULL TST-PKT) NIL)
                 (AND (= (PKT-SOURCE-ADDRESS INT-PKT) (PKT-SOURCE-ADDRESS TST-PKT))
                      (= (PKT-SOURCE-INDEX-NUM INT-PKT) (PKT-SOURCE-INDEX-NUM TST-PKT))
                      (RETURN T)))
             (DO ((I 1 (1+ I)))
                 (( I MAXIMUM-INDEX) NIL)
                 (AND (SETQ CONN (AR-1 INDEX-CONN I))
                      (= (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS INT-PKT))
                      (= (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM INT-PKT))
                      (RETURN T))))
         ;;; Duplicate RFC, just through the packet away
         (FREE-INT-PKT INT-PKT))
        (T (SETQ PKT (CONVERT-TO-PKT INT-PKT))
           (SETQ CONTACT-NAME (CONTACT-NAME-FROM-RFC PKT))
           (COND ((SETQ LSN (ASSOC CONTACT-NAME PENDING-LISTENS))
                  (SETQ PENDING-LISTENS (DELQ LSN PENDING-LISTENS))
                  (RFC-MEETS-LSN (CDR LSN) PKT))
                 ((SETQ SERVER (ASSOC CONTACT-NAME SERVER-ALIST))
		  (WITHOUT-INTERRUPTS	;seems like a good idea, altho probably not necessary
		    (SETF (PKT-LINK PKT) PENDING-RFC-PKTS)
		    (SETQ PENDING-RFC-PKTS PKT))
		  ;; This assumes that the name is in the CAR of an init list entry
		  ;; was just EVAL
		  (BACKGROUND-TASK (SI:INIT-FORM SERVER)))
		 (T (FREE-PKT PKT))))))

(DEFUN CONTACT-NAME-FROM-RFC (PKT &AUX CONTACT-STRING TEM)
    (SETQ CONTACT-STRING (PKT-STRING PKT))
    (COND ((SETQ TEM (STRING-SEARCH-CHAR 40 CONTACT-STRING))
	   (NSUBSTRING CONTACT-STRING 0 TEM))
	  (T CONTACT-STRING)))

;;; This is called when we have a LSN matching an RFC.  It can be called when we do
;;; a LSN (m.p. level) or when an RFC gets here (p.i. level).
;;; Here LISTEN has filled in some of the fields of the CONN, we must
;;; fill in the rest.
(DEFUN RFC-MEETS-LSN (CONN PKT)
    (SETF (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS PKT))
    (SETF (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM PKT))
    (SETF (FOREIGN-WINDOW-SIZE CONN) (PKT-ACK-NUM PKT))
    (SETF (PKT-NUM-READ CONN) (PKT-NUM PKT))
    (SETF (PKT-NUM-ACKED CONN) (PKT-NUM PKT))
    (SETF (STATE CONN) 'RFC-RECEIVED-STATE)
    (SETF (READ-PKTS CONN) PKT)
    (SETF (PKT-LINK PKT) NIL)
    (INTERRUPT-CONN ':CHANGE-OF-STATE CONN 'RFC-RECEIVED-STATE)
    (INTERRUPT-CONN ':INPUT CONN))

;;; So far both host and both index numbers have been verified.
(DEFUN RECEIVE-OPN (CONN INT-PKT)
    (SELECTQ (STATE CONN)
	     (RFC-SENT-STATE (SETF (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM INT-PKT))
			     (SETF (FOREIGN-WINDOW-SIZE CONN) (PKT-SECOND-DATA-WORD INT-PKT))
			     (SETF (PKT-NUM-READ CONN) (PKT-NUM INT-PKT))
			     (SETF (PKT-NUM-RECEIVED CONN) (PKT-NUM INT-PKT))
			     (SETF (PKT-NUM-ACKED CONN) (PKT-NUM INT-PKT))
			     (SETF (TIME-LAST-RECEIVED CONN) (TIME))
			     (RECEIPT CONN (PKT-ACK-NUM INT-PKT))
			     (UPDATE-WINDOW-AVAILABLE CONN)
			     (SETF (STATE CONN) 'OPEN-STATE)
			     (SETQ RESERVED-INT-PKT INT-PKT)
			     (TRANSMIT-STS CONN 'OPN)
			     (INTERRUPT-CONN ':CHANGE-OF-STATE CONN 'OPEN-STATE))
	     (OPEN-STATE (COND ((AND (= (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS INT-PKT))
				     (= (FOREIGN-INDEX-NUM CONN)
                                        (PKT-SOURCE-INDEX-NUM INT-PKT))
				     (= MY-ADDRESS (PKT-DEST-ADDRESS INT-PKT))
				     (= (LOCAL-INDEX-NUM CONN) (PKT-DEST-INDEX-NUM INT-PKT)))
				(SETQ RESERVED-INT-PKT INT-PKT)
				(TRANSMIT-STS CONN 'OPN))
			       (T (TRANSMIT-LOS-INT-PKT INT-PKT
                                                        LOS-OP
                                                        "You didn't open this connection"))))
	     (OTHERWISE (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Bad state for OPN"))))

;; We have received a CLS.  If the connection which he is closing is still open,
;; put it in closed state, free up all pending SEND-PKTs, and put the CLS packet
;; on the READ-PKTS list so that MP level can see it.
;;      If the connection does not exist, this is NOT an error, because two
;; CLSs might have passed each other.  So just free the PKT.
(DEFUN RECEIVE-CLS (INT-PKT &AUX PKT (INT-FLAG NIL))
   (LET ((CONN (PKT-DEST-CONN INT-PKT)))
     (COND ((NULL CONN)
	    (FREE-INT-PKT INT-PKT))
	   ((MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
            (SETQ PKT (CONVERT-TO-PKT INT-PKT))
	    (WITHOUT-INTERRUPTS
	      (FREE-ALL-SEND-PKTS CONN)
	      (FREE-ALL-RECEIVED-PKTS CONN)
	      (SETF (STATE CONN) 'CLS-RECEIVED-STATE)
	      (COND ((NULL (READ-PKTS-LAST CONN))
		     (SETF (READ-PKTS CONN) PKT)
		     (SETQ INT-FLAG T))
		    (T (SETF (PKT-LINK (READ-PKTS-LAST CONN)) PKT)))
	      (SETF (READ-PKTS-LAST CONN) PKT)
	      (SETF (PKT-LINK PKT) NIL))
	    (INTERRUPT-CONN ':CHANGE-OF-STATE CONN 'CLS-RECEIVED-STATE)
	    (AND INT-FLAG (INTERRUPT-CONN ':INPUT CONN)))
	   (T (TRANSMIT-LOS-INT-PKT INT-PKT
                                    LOS-OP
                                    "You sent a CLS to the wrong kind of connection.")))))

(DEFUN RECEIVE-LOS (INT-PKT &AUX (PKT (CONVERT-TO-PKT INT-PKT)) MY-INDEX CONN (INT-FLAG NIL))
    (SETQ MY-INDEX (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (PKT-DEST-INDEX-NUM PKT)))
    (COND ((AND (< MY-INDEX MAXIMUM-INDEX)
                (SETQ CONN (AREF INDEX-CONN MY-INDEX))
                CONN
		(EQ (STATE CONN) 'OPEN-STATE))
	   (WITHOUT-INTERRUPTS
	     (FREE-ALL-SEND-PKTS CONN)
	     (FREE-ALL-RECEIVED-PKTS CONN)
	     (SETF (STATE CONN) 'LOS-RECEIVED-STATE)
	     (COND ((NULL (READ-PKTS-LAST CONN))
		    (SETF (READ-PKTS CONN) PKT)
		    (SETQ INT-FLAG T))
		   (T (SETF (PKT-LINK (READ-PKTS-LAST CONN)) PKT)))
	     (SETF (READ-PKTS-LAST CONN) PKT)
	     (SETF (PKT-LINK PKT) NIL))
	   (INTERRUPT-CONN ':CHANGE-OF-STATE CONN 'LOS-RECEIVED-STATE)
	   (AND INT-FLAG (INTERRUPT-CONN ':INPUT CONN)))
	  (T (SETF (PKT-LINK PKT) LOS-PKTS)
	     (SETQ LOS-PKTS PKT)
	     (SETQ CURRENT-LOS-PKT-COUNT (1+ CURRENT-LOS-PKT-COUNT)))))

(DEFUN RECEIVE-FWD (CONN INT-PKT &AUX PKT)
    (COND ((NEQ (STATE CONN) 'RFC-SENT-STATE)
	   (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "An FWD was sent to a non-RFC-SENT index."))
	  (T (SETQ PKT (CONVERT-TO-PKT INT-PKT))
             (SETF (FOREIGN-ADDRESS CONN) (PKT-ACK-NUM PKT))
	     (SETF (PKT-OPCODE PKT) RFC-OP)
	     (TRANSMIT-NORMAL-PKT CONN PKT (PKT-NUM-SENT CONN) (LOCAL-WINDOW-SIZE CONN)))))

(DEFUN RECEIVE-ANS (CONN INT-PKT &AUX PKT)
    (COND ((NEQ (STATE CONN) 'RFC-SENT-STATE)
	   (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "An ANS was sent to a non-RFC-SENT index."))
	  (T (SETQ PKT (CONVERT-TO-PKT INT-PKT))
             (SETF (STATE CONN) 'ANSWERED-STATE)
	     (SETF (READ-PKTS CONN) PKT)
	     (SETF (PKT-LINK PKT) NIL)
	     (INTERRUPT-CONN ':CHANGE-OF-STATE CONN 'ANSWERED-STATE)
	     (INTERRUPT-CONN ':INPUT CONN))))

;; Timed Responses and background tasks

(DEFUN BACKGROUND (&AUX LAST-WAKEUP-TIME (LAST-PROBE-TIME (TIME)) TASKS TIME)
  (DO () (NIL)
    (SETQ TIME (TIME))
    (SETQ LAST-WAKEUP-TIME TIME)
    (DO () ((OR ( (TIME-DIFFERENCE TIME LAST-WAKEUP-TIME) RETRANSMISSION-INTERVAL)
		( (TIME-DIFFERENCE TIME LAST-PROBE-TIME) PROBE-INTERVAL)))
      (PROCESS-WAIT "Background Task"
		    #'(LAMBDA (LAST-WAKEUP-TIME LAST-PROBE-TIME &AUX (TIME (TIME)))
			(OR ( (TIME-DIFFERENCE TIME LAST-PROBE-TIME) PROBE-INTERVAL)
			    (AND RETRANSMISSION-NEEDED
				 ( (TIME-DIFFERENCE TIME LAST-WAKEUP-TIME)
				    RETRANSMISSION-INTERVAL))
			    BACKGROUND-REQUESTS))
		    LAST-WAKEUP-TIME LAST-PROBE-TIME)
      (WITHOUT-INTERRUPTS
	(SETQ TASKS (NREVERSE BACKGROUND-REQUESTS))
	(SETQ BACKGROUND-REQUESTS NIL))
      (DO () ((NULL TASKS))
	(EVAL (CAR TASKS))
	(SETQ TASKS (CDR TASKS)))
      (SETQ TIME (TIME)))
    (COND (RETRANSMISSION-NEEDED
	   (SETQ RETRANSMISSION-NEEDED NIL
		 MORE-RETRANSMISSION-NEEDED NIL)
	   (MAPC 'RETRANSMISSION CONN-LIST)	;Retransmit on all connections
	   (WITHOUT-INTERRUPTS
	     (SETQ RETRANSMISSION-NEEDED
		   (OR RETRANSMISSION-NEEDED MORE-RETRANSMISSION-NEEDED)))))
    (COND (( (TIME-DIFFERENCE TIME LAST-PROBE-TIME) PROBE-INTERVAL)
	   (SETQ LAST-PROBE-TIME TIME)
	   (DOTIMES (I (ARRAY-LENGTH ROUTING-TABLE-COST))
	     (ASET (MIN (+ (AREF ROUTING-TABLE-COST I) 2) 1000)
		   ROUTING-TABLE-COST I))
	   (MAPC 'PROBE-CONN CONN-LIST)))))	;Do probes and timeouts

;;; Retransmit all unreceipted packets not recently sent
(DEFUN RETRANSMISSION (CONN &AUX TIME (INHIBIT-SCHEDULING-FLAG T))
  (COND ((MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
	 ;Only if it is open or awaiting a response from RFC
	 (SETQ TIME (TIME))
         (DO-NAMED CONN-DONE
           () (NIL)
	   (LET ((INHIBIT-SCHEDULING-FLAG T))
             (DO ((PKT (SEND-PKTS CONN) (PKT-LINK PKT)))
                 ((NULL PKT) (RETURN-FROM CONN-DONE NIL))
	       (COND ((NOT (EQ CONN (PKT-SOURCE-CONN PKT)))
		      (FERROR NIL "~S in SEND-PKTS list for incorrect CONN
CONN ~S, (PKT-SOURCE-CONN PKT) ~S" PKT CONN (PKT-SOURCE-CONN PKT))))
	       (SETQ MORE-RETRANSMISSION-NEEDED T)
	       (COND (( (TIME-DIFFERENCE (TIME) (PKT-TIME-TRANSMITTED PKT))
			 RETRANSMISSION-INTERVAL)
		      (SETF (PKT-BEING-RETRANSMITTED PKT) T)
		      (SETQ INHIBIT-SCHEDULING-FLAG NIL)
		      (TRANSMIT-PKT PKT T)
		      (SETQ PKTS-RETRANSMITTED (1+ PKTS-RETRANSMITTED))
		      (SETQ INHIBIT-SCHEDULING-FLAG T)
		      (COND ((EQ (PKT-BEING-RETRANSMITTED PKT) 'FREE)
			     (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)
			     (FREE-PKT PKT))
			    (T (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)))
		      (RETURN NIL)))))  ;Must always start from beginning of chain if
					; turned on scheduling, since chain could be invalid
           (PROCESS-ALLOW-SCHEDULE)))))

;;; Send a SNS on this conn if necessary.  Decide whether foreign host is down.
;;; This gets called every PROBE-INTERVAL.
(DEFUN PROBE-CONN (CONN &AUX DELTA-TIME)
  (COND ((MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
	 ;Only if it is open or awaiting a response from RFC
	 (SETQ DELTA-TIME (TIME-DIFFERENCE (TIME) (TIME-LAST-RECEIVED CONN)))
	 (COND ((> DELTA-TIME HOST-DOWN-INTERVAL)
		(WITHOUT-INTERRUPTS
		  (FREE-ALL-SEND-PKTS CONN)
		  (FREE-ALL-RECEIVED-PKTS CONN)
		  (SETF (STATE CONN) 'HOST-DOWN-STATE))
		(INTERRUPT-CONN ':CHANGE-OF-STATE CONN 'HOST-DOWN-STATE))
	       ((AND (EQ (STATE CONN) 'OPEN-STATE)	;Send SNS only on open connections
		     (OR (< (WINDOW-AVAILABLE CONN) (FOREIGN-WINDOW-SIZE CONN))
			 (> DELTA-TIME LONG-PROBE-INTERVAL)))
		(LET ((PKT (ALLOCATE-INT-PKT)))
		  (SETF (PKT-OPCODE PKT) SNS-OP)
		  (SETF (PKT-NBYTES PKT) 0)
		  (TRANSMIT-INT-PKT-FOR-CONN CONN PKT)))))))

;; Hardware Interface

(DEFUN INTERFACE-RESET-AND-ENABLE ()
  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER
                 (DPB -1 %%CHAOS-CSR-RESET 0))
  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER
                 (DPB -1 %%CHAOS-CSR-INTERRUPT-ENABLES 0)))

(DEFUN INTERFACE-RESET ()
  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER
		 (DPB -1 %%CHAOS-CSR-RESET 0)))

(DEFUN RECEIVER-RESET ()
  (%UNIBUS-WRITE CONTROL-STATUS-REGISTER
                 (DPB 1 %%CHAOS-CSR-RECEIVER-CLEAR (%UNIBUS-READ CONTROL-STATUS-REGISTER))))

;;; Top level function for receiver to be called directly from scheduler. (Instead of the
;;; old thing where something like this was the top level of the RECEIVER process.)
(DEFUN RECEIVE-ANY-FUNCTION (&AUX INT-PKT)
  (DO ((RESERVED-INT-PKT NIL))
      ((OR (NULL ENABLE) (NULL (INT-RECEIVE-LIST))))
    (COND ((SETQ INT-PKT (RECEIVE-PROCESS-NEXT-INT-PKT))
	   (RECEIVE-INT-PKT INT-PKT)))  ;WITHOUT-INTERRUPTS not necc since from scheduler.
    (COND (RESERVED-INT-PKT
	    (FERROR NIL "Int PKT about to be lost!"))) ;Hopefully this will get printed
    (SI:SET-PROCESS-WAIT CURRENT-PROCESS #'(LAMBDA () (NOT (OR (NULL ENABLE)
							       (NULL (INT-RECEIVE-LIST)))))
			 NIL)
    (SETF (SI:PROCESS-WHOSTATE CURRENT-PROCESS) "Chaos Packet")
    ))

;;; Returns NIL if there was a CRC error, INT-PKT if win.
(DEFUN RECEIVE-PROCESS-NEXT-INT-PKT ()
  (PROG (INT-PKT BITS DEST OLD-RECEIVE-LIST)
 LOOP (SETQ OLD-RECEIVE-LIST (INT-RECEIVE-LIST))
      (OR (%STORE-CONDITIONAL INT-RECEIVE-LIST-POINTER OLD-RECEIVE-LIST
                              (INT-PKT-THREAD OLD-RECEIVE-LIST))
          (GO LOOP))
      (SETQ INT-PKT OLD-RECEIVE-LIST)
      (SETF (INT-PKT-THREAD INT-PKT) NIL)
      (COND ((< (INT-PKT-WORD-COUNT INT-PKT) (+ FIRST-DATA-WORD-IN-PKT 3))
             ;; Less than the minimum size that can exist in the current protocol?
	     (SETQ PKTS-OTHER-DISCARDED (1+ PKTS-OTHER-DISCARDED))
             (FREE-INT-PKT INT-PKT)
             (RETURN NIL)))
      (SETQ PKTS-RECEIVED (1+ PKTS-RECEIVED))
      (SETQ DEST (INT-PKT-HARDWARE-DEST INT-PKT)
            BITS (INT-PKT-BIT-COUNT INT-PKT)
            PKTS-LOST (+ (LDB %%CHAOS-CSR-LOST-COUNT (INT-PKT-CSR-2 INT-PKT))
                         PKTS-LOST))
      (COND ((LDB-TEST %%CHAOS-CSR-CRC-ERROR (INT-PKT-CSR-1 INT-PKT))
             (SETQ PKTS-BAD-CRC-1 (1+ PKTS-BAD-CRC-1))
             (FREE-INT-PKT INT-PKT))
            ((OR (< BITS 48.)
		 (BIT-TEST 17 BITS)
		 (AND (ZEROP (LOGAND 377 (AREF INT-PKT 0)))	;Header version 0
		      ( (// BITS 20) (+ (PKT-NWORDS INT-PKT) 3))))
             (SETQ PKTS-BAD-BIT-COUNT (1+ PKTS-BAD-BIT-COUNT))
             (FREE-INT-PKT INT-PKT))
            ((LDB-TEST %%CHAOS-CSR-CRC-ERROR (INT-PKT-CSR-2 INT-PKT))
             (SETQ PKTS-BAD-CRC-2 (1+ PKTS-BAD-CRC-2))
             (FREE-INT-PKT INT-PKT))
            ((AND ( DEST 0) ( DEST MY-ADDRESS))
             (SETQ PKTS-BAD-DEST (1+ PKTS-BAD-DEST))
             (FREE-INT-PKT INT-PKT))
            (T (RETURN INT-PKT))) ))

;;; This is called by anyone with an INT-PKT.  It looks at the
;;; destination field in the INT-PKT and sends it somewhere.
;;; This does not mess with links in any way.
;;; The host and subnet can be passed as optional arguments, so that
;;; this function can be used for debugging purposes.
(DEFUN TRANSMIT-INT-PKT (INT-PKT &OPTIONAL (HOST (PKT-DEST-ADDRESS INT-PKT))
                                           (SUBNET (PKT-DEST-SUBNET INT-PKT)))
    ;;; Simple routing if he is not on my subnet.
    (COND ((NOT (= SUBNET MY-SUBNET))
	   (AND (> SUBNET (ARRAY-LENGTH ROUTING-TABLE))
		(SETQ SUBNET 0))
	   (SETQ HOST (AR-1 ROUTING-TABLE SUBNET))))
    (SETF (INT-PKT-WORD-COUNT INT-PKT) (1+ (PKT-NWORDS INT-PKT)))
    (ASET HOST INT-PKT (1- (INT-PKT-WORD-COUNT INT-PKT)))
    (OR (= (%AREA-NUMBER INT-PKT) CHAOS-BUFFER-AREA)
        (FERROR NIL "Attempt to transmit non-interrupt packet ~A" INT-PKT))
    (AND (BIT-TEST 200 (PKT-OPCODE INT-PKT)) (SETQ DATA-PKTS-OUT (1+ DATA-PKTS-OUT)))
    (WITHOUT-INTERRUPTS
     (PROG (OLD-TRANSMIT-LIST)
	   (SETQ PKTS-TRANSMITTED (1+ PKTS-TRANSMITTED))
	LOOP
	   (SETQ OLD-TRANSMIT-LIST (INT-TRANSMIT-LIST))
           (SETF (INT-PKT-THREAD INT-PKT) OLD-TRANSMIT-LIST)
           (OR (%STORE-CONDITIONAL INT-TRANSMIT-LIST-POINTER OLD-TRANSMIT-LIST INT-PKT)
               (GO LOOP))
           (%CHAOS-WAKEUP))))

;;; DEBUGGING: The following are functions for printing out information, 
;;; principally for debugging.

;Print out contents of hardware registers
(DEFUN STATUS ( &AUX CSR LC)
    (SETQ CSR (%UNIBUS-READ CONTROL-STATUS-REGISTER))
    (TERPRI) (TERPRI)
    (AND (LDB-TEST %%CHAOS-CSR-TIMER-INTERRUPT-ENABLE CSR)
	 (FORMAT T "Timer interrupt enable or maybe transmit busy.~%"))
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
    (FORMAT T "Bit count: ~O~%" (%UNIBUS-READ BIT-COUNT-REGISTER))
    NIL)

;;; KLUDGES and assorted random functions

(DEFUN ASSURE-ENABLED ()
    (OR ENABLE
	(ENABLE)))

(DEFUN ENABLE ()
  (FUNCALL BACKGROUND ':REVOKE-RUN-REASON)
  (FUNCALL RECEIVER ':REVOKE-RUN-REASON)
  (INTERFACE-RESET-AND-ENABLE)
  (FUNCALL BACKGROUND ':PRESET 'BACKGROUND)
  (FUNCALL BACKGROUND ':RUN-REASON)
  (FUNCALL RECEIVER ':PRESET 'RECEIVE-ANY-FUNCTION)
  (FUNCALL RECEIVER ':RUN-REASON)
  (SETQ ENABLE T))

(FSET 'STOP 'DISABLE)
(DEFUN DISABLE ()
  (SETQ ENABLE NIL)
  (FUNCALL RECEIVER ':REVOKE-RUN-REASON)
  (FUNCALL BACKGROUND ':REVOKE-RUN-REASON)
  (INTERFACE-RESET))

(DEFUN RESET ()
   (DISABLE)
   (WITHOUT-INTERRUPTS
     (SETQ BACKGROUND-REQUESTS NIL)		;Get rid of requests for connections flushing
     (DO CL CONN-LIST (CDR CL) (NULL CL)
       (FREE-ALL-READ-PKTS (CAR CL))
       (FREE-ALL-RECEIVED-PKTS (CAR CL))
       (FREE-ALL-SEND-PKTS (CAR CL))
       (SETF (STATE (CAR CL)) 'INACTIVE-STATE))
     (DO I 1 (1+ I) (= I MAXIMUM-INDEX)
       (AS-1 NIL INDEX-CONN I))
     (SETQ CONN-LIST NIL)

     ;; The initialization is needed because if the LISP Machine has an open connection,
     ;; it gets reloaded, and the connection is established on the same index before the
     ;; other end has gone into INCXMT state, then the RFC will look like a duplicate.
     ;; Though this may sound like a rare event, it is exactly what happens with the
     ;; file job connection!!
     (DO ((INDEX 0 (1+ INDEX)))
         ((>= INDEX MAXIMUM-INDEX))
         (AS-1 (+ (TIME) INDEX) UNIQUIZER-TABLE INDEX))

     ;; Should actually try and free up these
     (SETQ PENDING-LISTENS NIL)
     (SETQ PENDING-RFC-PKTS NIL)
     ;; This is a pretty arbitrary number, but it used to be MAXIMUM-INDEX which
     ;; grew like mad causing an absurd number of pages to get wired.  This is undoubtedly
     ;; enough for average use.
     (CREATE-CHAOSNET-BUFFERS 40) )
    "Reset and disabled")

(DEFUN PKT-ADD-32 (PKT COUNT)
  (LET ((IDX (+ FIRST-DATA-WORD-IN-PKT (// (PKT-NBYTES PKT) 2))))
    (ASET (LDB 0020 COUNT) PKT IDX)
    (ASET (LDB 2020 COUNT) PKT (1+ IDX))
    (SETF (PKT-NBYTES PKT) (+ (PKT-NBYTES PKT) 4))))

(DEFUN SEND-STATUS (&AUX CONN PKT STRING)
  (SETQ CONN (LISTEN "STATUS"))
  (COND ((EQ (STATE CONN) 'RFC-RECEIVED-STATE)
         (SETQ PKT (GET-PKT))
         (SET-PKT-STRING PKT (HOST-DATA MY-ADDRESS))
	 (DO I (ARRAY-ACTIVE-LENGTH (SETQ STRING (PKT-STRING PKT))) (1+ I) ( I 32.)
	   (ARRAY-PUSH STRING 0))
	 (SETF (PKT-NBYTES PKT) 32.)
	 (PKT-ADD-32 PKT (DPB 16. 2020 (+ (LDB 1010 MY-ADDRESS) 400)))
	 (PKT-ADD-32 PKT PKTS-RECEIVED)
	 (PKT-ADD-32 PKT PKTS-TRANSMITTED)
	 (PKT-ADD-32 PKT (READ-METER '%COUNT-CHAOS-TRANSMIT-ABORTS))
	 (PKT-ADD-32 PKT PKTS-LOST)
	 (PKT-ADD-32 PKT PKTS-BAD-CRC-1)
	 (PKT-ADD-32 PKT PKTS-BAD-CRC-2)
	 (PKT-ADD-32 PKT PKTS-BAD-BIT-COUNT)
	 (PKT-ADD-32 PKT PKTS-OTHER-DISCARDED)
	 (ANSWER CONN PKT))
        (T ;;; Lost somehow
         (REMOVE-CONN CONN))))

(ADD-INITIALIZATION "STATUS" '(SEND-STATUS) NIL 'SERVER-ALIST)

;;; Wiring stuff

;;; Takes the number of an area and wires down all the allocated
;;; pages of it, or un-wires, depending on the second argument.
;;; The area had better have only one region.
;;; Also doesn't work on downwards-consed list regions.

(DEFUN WIRE-AREA (AREA WIRE-P)
  (LET ((REGION (AREA-REGION-LIST AREA)))
    (OR (MINUSP (REGION-LIST-THREAD REGION)) ;last region in area
	(FERROR NIL "Area ~A has more than one region" (AREA-NAME AREA)))
    (DO ((LOC (REGION-ORIGIN REGION) (+ LOC PAGE-SIZE))
	 (COUNT (// (+ (REGION-FREE-POINTER REGION) (1- PAGE-SIZE)) PAGE-SIZE) (1- COUNT)))
	((ZEROP COUNT))
      (SI:WIRE-PAGE LOC WIRE-P))))

;For now, doesn't worry about changing number of buffers.  If called
;more than once, will discard all old buffers.  You better not try to
;increase the number of buffers, though.
(DEFUN CREATE-CHAOSNET-BUFFERS (N-BUFFERS)
  (COND ((NOT (BOUNDP 'CHAOS-BUFFER-AREA))
	 (MAKE-AREA ':NAME 'CHAOS-BUFFER-AREA
		':SIZE (* (// (+ (* N-BUFFERS (+ 3 ;Leader header, leader length, array header
						 128. ;Max 32-bit wds in pkt incl hardware wds
						 (LENGTH CHAOS-BUFFER-LEADER-QS)))
				 (1- PAGE-SIZE))
			      PAGE-SIZE)
			  PAGE-SIZE)
		':GC ':STATIC))
	(T (RESET-TEMPORARY-AREA CHAOS-BUFFER-AREA)))
  (DO ((PREV NIL BUF)
       (BUF)
       (COUNT N-BUFFERS (1- COUNT)))
      ((ZEROP COUNT)
       (WIRE-AREA CHAOS-BUFFER-AREA T)
       (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-FREE-LIST) BUF)
       (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-TRANSMIT-LIST) NIL)
       (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-RECEIVE-LIST) NIL))
    (SETQ BUF (MAKE-ARRAY CHAOS-BUFFER-AREA 'ART-16B 256. NIL (LENGTH CHAOS-BUFFER-LEADER-QS)))
    (STORE-ARRAY-LEADER 0 BUF %CHAOS-LEADER-WORD-COUNT)
    (STORE-ARRAY-LEADER PREV BUF %CHAOS-LEADER-THREAD)))

(DEFUN PRINT-INT-PKT (INT-PKT)
  (DO ((I 0 (1+ I)))
      (( I  (INT-PKT-WORD-COUNT INT-PKT)))
      (FORMAT T "~%Word ~O, data ~O" I (AREF INT-PKT I))))

(ADD-INITIALIZATION "CHAOS-NCP" '(INITIALIZE-NCP-ONCE) '(ONCE))
(ADD-INITIALIZATION "CHAOS-NCP" '(INITIALIZE-NCP-COLD) '(COLD))
(ADD-INITIALIZATION "CHAOS-NCP"
                    '(INITIALIZE-NCP-SYSTEM)
                    '(SYSTEM NORMAL))   ;NORMAL keyword to override FIRST default
