;;; Routines for hacking the pseudo-debugger          -*-LISP-*-

;;; The following are the active locations:
;;; 766100  Reads or writes the debuggee-Unibus location addressed by the registers below.
;;; 766114  (Write only) Contains bits 1-16 of the debuggee-Unibus address
;;;	    to be accessed.  Bit 0 of the address is always zero.
;;; 766110  (Write only) Contains additional modifier bits, as follows.
;;;	    These bits are reset to zero when the debuggee's Unibus is reset.
;;;	    1  Bit 17 of the debuggee-Unibus address.
;;;	    2  Resets the debuggee's Unibus and bus interface.  Write a 1 here then write a 0.
;;;	    4  Timeout inhibit.  This turns off the NXM timeout for all Xbus and Unibus cycles
;;;	       done by the debuggee's bus interface (not just those by the debugger).
;;; 766104  (Read only) These contain the status for bus cycles executed on the debuggee's
;;;	    busses.  These bits are cleared by writing into location 766044 (Error Status)
;;;	    on the debuggee's Unibus.  They are not cleared by power up.
;;;	    1  Xbus NXM Error.  Set when an Xbus cycle times out for lack of response.
;;;	    2  Xbus Parity Error.  Set when an Xbus read receives a word with bad parity,
;;;	       and the Xbus ignore-parity line was not asserted.  Parity Error is also set
;;;	       by Xbus NXM Error.
;;;	    4  CADR Address Parity Error.  Set when an address received from the processor
;;;	       has bad parity.
;;;	   10  Unibus NXM Error.  Set when a Unibus cycle times out for lack of response.
;;;	   20  CADR Parity Error.  Set when data received from the processor has bad parity.
;;;	   40  Unibus Map Error.  Set when an attempt to perform an Xbus cycle through the
;;;	       Unibus map is refused because the map specifies invalid or write-protected.
;;;	   The remaining bits are random (not necessarily zero).

(DECLARE (SPECIAL DBG-NXM-INHIBIT DBG-ACCESS-PATH DBG-SERIAL-HIGH-BIT DBG-HOST
		  DBG-CHAOS-STRING DBG-CHAOS-16 DBG-UNIQUE-ID))
(SETQ DBG-NXM-INHIBIT NIL
      DBG-ACCESS-PATH 'BUSINT			;Possible values: BUSINT, SERIAL, CHAOS
      DBG-SERIAL-HIGH-BIT -1
      DBG-HOST NIL
      DBG-CHAOS-STRING (MAKE-ARRAY NIL 'ART-STRING
				   (* 2 CHAOS:MAX-DATA-WORDS-PER-PKT) NIL '(2))
      DBG-CHAOS-16 (MAKE-ARRAY NIL 'ART-16B  CHAOS:MAX-DATA-WORDS-PER-PKT DBG-CHAOS-STRING)
      DBG-UNIQUE-ID NIL)

;;; Reset the state of the internal variables
(DEFUN DBG-CC-RESET ()
  (SETQ DBG-SERIAL-HIGH-BIT -1
	DBG-UNIQUE-ID NIL))

;;; Read a location on the debuggee's Unibus
(DEFUN DBG-READ (ADR &OPTIONAL (CHAOS-DBG-TYPE 'DATA))
  (SELECTQ DBG-ACCESS-PATH
    (SERIAL
      (DBG-UPDATE-HIGH-BIT ADR)
      (FORMAT 'SERIAL-STREAM "~O//" (LSH ADR -1))
      (READ 'SERIAL-STREAM))
    (BUSINT
      (%UNIBUS-WRITE 766110 (+ (LSH ADR -17.) (COND (DBG-NXM-INHIBIT 4) (T 0))))
      (%UNIBUS-WRITE 766114 (LSH ADR -1))
      (%UNIBUS-READ 766100))
    (CHAOS
      (LET ((PKT (DBG-CHAOS CHAOS-DBG-TYPE ADR)))
	(PROG1 (AREF PKT CHAOS:FIRST-DATA-WORD-IN-PKT)
	       (CHAOS:RETURN-PKT PKT))))
    (OTHERWISE (FERROR NIL "~A is illegal DBG-ACCESS-PATH" DBG-ACCESS-PATH))))

;;; Write a location on the debuggee's Unibus
(DEFUN DBG-WRITE (ADR VAL &OPTIONAL (CHAOS-DBG-TYPE 'DATA))
  (SELECTQ DBG-ACCESS-PATH
    (SERIAL
      (DBG-UPDATE-HIGH-BIT ADR)
      (FORMAT 'SERIAL-STREAM "~O:~O:" (LSH ADR -1) VAL))
    (BUSINT
      (%UNIBUS-WRITE 766110 (+ (LSH ADR -17.) (COND (DBG-NXM-INHIBIT 4) (T 0))))
      (%UNIBUS-WRITE 766114 (LSH ADR -1))
      (%UNIBUS-WRITE 766100 (LOGAND VAL 177777)))
    (CHAOS
     (DBG-CHAOS CHAOS-DBG-TYPE ADR VAL))
    (OTHERWISE (FERROR NIL "~A is illegal DBG-ACCESS-PATH" DBG-ACCESS-PATH)))    
  T)

;;; Reset the debuggee's Unibus
(DEFUN DBG-RESET ()
  (SETQ CC-UNIBUS-MAP-TO-MD-OK-FLAG NIL)
  (SELECTQ DBG-ACCESS-PATH
    (SERIAL
      (FORMAT 'SERIAL-STREAM "2S")
      (FORMAT 'SERIAL-STREAM "~OS" (+ (ABS DBG-SERIAL-HIGH-BIT)
				    (COND (DBG-NXM-INHIBIT 4) (T 0)))))
    (BUSINT (%UNIBUS-WRITE 766110 2)
	    (%UNIBUS-WRITE 766110 (COND (DBG-NXM-INHIBIT 4) (T 0))))
    (CHAOS
     (DBG-CHAOS 'RESET 0 0))
    (OTHERWISE (FERROR NIL "~A is illegal DBG-ACCESS-PATH" DBG-ACCESS-PATH)))
  T)

;;; Print the error status bits
(DEFVAR SERIAL-STREAM (SI:MAKE-SERIAL-STREAM
			':PARITY NIL ':NUMBER-OF-DATA-BITS 8 ':BAUD 300.))

(DEFUN DBG-PRINT-STATUS ()
   (CC-PRINT-SET-BITS (SELECTQ DBG-ACCESS-PATH
			(SERIAL (FUNCALL SERIAL-STREAM ':TYO #/R)
				(READ 'SERIAL-STREAM))
			(BUSINT (%UNIBUS-READ 766104))
			(CHAOS (LET ((PKT (DBG-CHAOS 'STATUS 0)))
				 (PROG1 (AREF PKT CHAOS:FIRST-DATA-WORD-IN-PKT)
					(CHAOS:RETURN-PKT PKT))))
			(OTHERWISE
			  (FERROR NIL "~A is illegal DBG-ACCESS-PATH" DBG-ACCESS-PATH)))
		      '(XBUS-NXM-ERR XBUS-PARITY-ERR CADR-ADDRESS-PARITY-ERR
			UNIBUS-NXM-ERR CADR-DATA-PARITY-ERR UNIBUS-MAP-ERR
			NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))

;;; Reset the error status
(DEFUN DBG-RESET-STATUS ()
  (SELECTQ DBG-ACCESS-PATH
    (SERIAL)
    (BUSINT (DBG-WRITE 766044 0))
    (CHAOS (DBG-CHAOS 'STATUS 0 0))))

;;; Setup the high bit of the SERIAL debugger correctly
(DEFUN DBG-UPDATE-HIGH-BIT (ADR &AUX (HIGH (LDB 2101 ADR)))
  (OR (= HIGH DBG-SERIAL-HIGH-BIT)
      (FORMAT 'SERIAL-STREAM "~OS" (+ (SETQ DBG-SERIAL-HIGH-BIT HIGH)
				    (COND (DBG-NXM-INHIBIT 4) (T 0))))))

;;; Dummy stream for SERIAL I/O
(DEFUN SERIAL-STREAM (OP &OPTIONAL ARG1)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(TYI TYO))
    (:STRING-OUT (DOTIMES (I (STRING-LENGTH ARG1))
		   (FUNCALL SERIAL-STREAM ':TYO (CHARACTER-ODD-PARITY (AREF ARG1 I)))))
    (:TYI (DO ((CHAR (FUNCALL SERIAL-STREAM ':TYI) (FUNCALL SERIAL-STREAM ':TYI)))
	      (NIL)
;	    (OR (ODDP (CHARACTER-PARITY CHAR))
;		(FERROR NIL "BAD PARITY RECEIVED - ~O" CHAR))
	    (SELECTQ (SETQ CHAR (LOGAND CHAR 177))
	      (7 (FERROR NIL "ERRONEOUS COMMAND RECEIVED BY DEBUGGER"))
	      (10 (FUNCALL SERIAL-STREAM ':TYO 33)
		  (FERROR NIL "DEBUGGER GOT PARITY ERROR, RESETTING DEBUGGER"))
	      (15)
	      (T (RETURN CHAR)))))
    (:TYO (FUNCALL SERIAL-STREAM ':TYO (CHARACTER-ODD-PARITY ARG1)))))

(DEFUN CHARACTER-PARITY (CHAR &AUX (PARITY 0))
  (DOTIMES (I 8.)
    (SETQ PARITY (LOGXOR CHAR PARITY))
    (SETQ CHAR (LSH CHAR -1)))
  (LOGAND PARITY 1))

(DEFUN CHARACTER-ODD-PARITY (CHAR)
  (DPB (LOGXOR 1 (CHARACTER-PARITY (LOGAND CHAR 177))) 0701 CHAR))

;;; DBG-CHAOS: Take a debug cycle over the Chaos net
;;; First arg is type of cycle (DATA, STATUS, RESET, ANALOG, INTERNAL-8748,EXTERNAL-8748)
;;;  second is address, third is value
(DEFUN DBG-CHAOS (TYPE ADR &OPTIONAL DATA
		  &AUX PKT (TIMEOUT (COND (DBG-NXM-INHIBIT 4) (T 0))))
  (SETQ TIMEOUT (LOGIOR TIMEOUT (LDB 2101 ADR))) 
  (COND ((NULL DBG-HOST)
	 (FORMAT QUERY-IO "~&Chaos host to debug? ")
	 (SETQ DBG-HOST (CHAOS:ADDRESS-PARSE (READLINE QUERY-IO)))))
  (COND ((= (ARRAY-LEADER DBG-CHAOS-STRING 0) 2)
	 (COND ((OR (NULL DBG-UNIQUE-ID) (> DBG-UNIQUE-ID 370))
		(ASET #/$ DBG-CHAOS-STRING 0)
		(ASET #\SPACE DBG-CHAOS-STRING 1)
		(ASET 1_8. DBG-CHAOS-16 1)		;0 Unique ID, Reset
		(STORE-ARRAY-LEADER 4 DBG-CHAOS-STRING 0)
		(CHAOS:RETURN-PKT (CHAOS:SIMPLE DBG-HOST DBG-CHAOS-STRING))
		(STORE-ARRAY-LEADER 2 DBG-CHAOS-STRING 0)
		(SETQ DBG-UNIQUE-ID 0)))
	 (SETQ DBG-UNIQUE-ID (1+ DBG-UNIQUE-ID))
	 (ASET (+ 1_8. DBG-UNIQUE-ID) DBG-CHAOS-16 1)
	 (ASET #/$ DBG-CHAOS-STRING 0)
	 (ASET #\SPACE DBG-CHAOS-STRING 1)
	 (STORE-ARRAY-LEADER 4 DBG-CHAOS-STRING 0)))
  (SELECTQ TYPE
    (RESET (SETQ TIMEOUT (LOGIOR TIMEOUT DATA))
	   (SETQ TYPE 120))
    (DATA (SETQ TYPE (IF DATA 200 000)))
    (ANALOG (SETQ TYPE 040)			; ADR specifies which channel
	    (SETQ ADR (LOGIOR 400 (LSH ADR 1))))
    (STATUS (SETQ TYPE (IF DATA 240 040)
		  ADR 2))
    (DEBUGGER-HIBERNATE (SETQ TYPE 040)
			(SETQ ADR 200))
    (INTERNAL-8748 (SETQ TYPE (IF DATA 300 100))  ; DATA specifies address
		   (SETQ ADR (LSH ADR 1)))
    (EXTERNAL-8748 (SETQ TYPE (IF DATA 340 140))
		   (SETQ ADR (LSH ADR 1)))
    (OTHERWISE (FERROR NIL "Unknown request type ~S" TYPE)))
  (LET ((WORD (AREF DBG-CHAOS-16 1))
	(PTR))
    (SETQ PTR (1- (LSH WORD -8.)))
    (ASET (+ (LSH TYPE 8.) TIMEOUT) DBG-CHAOS-16 (+ 2 (* PTR 3)))
    (ASET (LSH ADR -1) DBG-CHAOS-16 (+ 3 (* PTR 3)))
    (AND DATA (ASET (LOGAND DATA 177777) DBG-CHAOS-16 (+ 4 (* PTR 3))))
    (ASET (DPB (SETQ PTR (+ PTR 2)) 1010 WORD) DBG-CHAOS-16 1)
    (COND ((OR (> (+ PTR 3) (// CHAOS:MAX-DATA-WORDS-PER-PKT 3))
	       (NOT (BIT-TEST TYPE 200)))
	   ;; Conservative, or a read
	   (STORE-ARRAY-LEADER (+ 4 (* (1- PTR) 6)) DBG-CHAOS-STRING 0)
	   (SETQ PKT (CHAOS:SIMPLE DBG-HOST DBG-CHAOS-STRING))
	   (AND (BIT-TEST TYPE 200) (CHAOS:RETURN-PKT PKT))
	   (STORE-ARRAY-LEADER 2 DBG-CHAOS-STRING 0)
	   PKT))))

(DEFUN DBG-CHAOS-WRITE-FROB ()
  (ASET 340_8. DBG-CHAOS-16 2)
  (ASET 060 DBG-CHAOS-16 3)
  (ASET 525252 DBG-CHAOS-16 4)
  (DO () (())
    (ERRSET
      (PROGN
	(SETQ DBG-UNIQUE-ID (1+ DBG-UNIQUE-ID))
	(ASET (+ 2_8 DBG-UNIQUE-ID) DBG-CHAOS-16 1)
	(CHAOS:RETURN-PKT (CHAOS:SIMPLE DBG-HOST DBG-CHAOS-STRING)))
      NIL)))

(DEFUN DBG-ANALOG ()
   (DOLIST (X '(0 1 2 3 4 5 6 7))
     (DBG-WRITE (LOGIOR 20 X) 0 'EXTERNAL-8748)
     (DBG-WRITE 30 -1 'EXTERNAL-8748)
     (PRINT (LDB 0010 (DBG-READ 40 'EXTERNAL-8748)))))

;;; Higher-level operations

;;; The Unibus map is 16 words at 766140.  It consists of 14 address bits, write-ok, and valid
;;; It controls locations 140000-177777 (2000 byte locations per page).
(DEFUN DBG-READ-UNIBUS-MAP (LOC)
  (DBG-READ (+ 766140 (* 2 LOC))))

(DEFUN DBG-WRITE-UNIBUS-MAP (LOC VAL)
  (SETQ CC-UNIBUS-MAP-TO-MD-OK-FLAG NIL)	;Caprine necrophilia
  (DBG-WRITE (+ 766140 (* 2 LOC)) VAL))

;; Returns unibus location mapped into specified xbus location
(DEFUN DBG-SETUP-UNIBUS-MAP (LOC XBUS-LOC)
  (DBG-WRITE-UNIBUS-MAP LOC (+ 140000 (LDB 1016 XBUS-LOC)))
  (+ 140000 (* LOC 2000) (* 4 (LOGAND 377 XBUS-LOC))))

(DEFUN DBG-PRINT-UNIBUS-MAP ()
  (DO ((LOC 0 (1+ LOC))
       (CONTENTS))
      ((= LOC 20))
    (SETQ CONTENTS (DBG-READ-UNIBUS-MAP LOC))
    (PRINT LOC)
    (PRIN1-THEN-SPACE (COND ((ZEROP (LDB 1701 CONTENTS)) 'NOT-VALID) (T 'VALID)))
    (PRIN1-THEN-SPACE (COND ((ZEROP (LDB 1601 CONTENTS)) 'READ-ONLY) (T 'WRITE-OK)))
    (PRIN1 (ASH (LOGAND 37777 CONTENTS) 8))))

;;; Routines to read and write the Xbus using Unibus map location 17

(DEFVAR DBG-UNIBUS-MAP-NUMBER 17)	;This can be changed by diagnostics

(DEFUN DBG-READ-XBUS (XBUS-LOC)
  (LET ((UBUS-LOC (DBG-SETUP-UNIBUS-MAP DBG-UNIBUS-MAP-NUMBER XBUS-LOC))
	(RES NIL))
    (SETQ RES (DBG-READ UBUS-LOC))
    (LOGDPB (DBG-READ (+ UBUS-LOC 2)) 2020 RES)))

(DEFUN DBG-WRITE-XBUS (XBUS-LOC VAL)
  (LET ((UBUS-LOC (DBG-SETUP-UNIBUS-MAP DBG-UNIBUS-MAP-NUMBER XBUS-LOC)))
    (DBG-WRITE UBUS-LOC (LOGLDB 0020 VAL))
    (DBG-WRITE (+ UBUS-LOC 2) (LDB 2020 VAL))))

;;; Accessing the interrupt-control and so forth registers
;;; 766040  Interrupt control
;;;		1  Disable Interrupt Grant
;;;		2  Local Enable (read only)
;;;	     1774  Interrupt Vector of last interrupt
;;;	     2000  Enable Unibus Interrupts
;;;	     4000  Interrupt Stops Grants
;;;	    30000  Interrupt level (0,4,5,6)
;;;	    40000  Xbus interrupt (read only)
;;;	   100000  Unibus Interrupt
;;;		   Only bits masked by 36001 can be written at this address.
;;; 766042  Interrupt control 2 (write only)
;;;		   Writes bits masked by 101774 of the above register.
;;; 766044  Error register (writing clears)  (see 764542 above)

(DEFUN DBG-PRINT-INTERRUPT-STATUS ()
  ((LAMBDA (INTC)
      (TERPRI)
      (CC-PRINT-SET-BITS INTC
			 '( DISABLE-INT-GRANT LOCAL-ENABLE NIL NIL NIL NIL NIL NIL NIL NIL
			    ENABLE-UNIBUS-INT INT-STOPS-GRANTS NIL NIL XBUS-INT UNIBUS-INT ))
      (PRINC '| LEVEL=|)
      (PRIN1 (LOGLDB 1402 INTC))
      (PRINC '| VECTOR=|)
      (PRIN1 (LOGAND 1774 INTC)))
   (DBG-READ 766040)))
