;;; Routines for hacking the pseudo-debugger          -*-LISP-*-

;;; The following are the active locations:
;;; 764540  Reads or writes the debuggee-Unibus location addressed by the registers below.
;;; 764546  (Write only) Contains bits 1-16 of the debuggee-Unibus address
;;;	    to be accessed.  Bit 0 of the address is always zero.
;;; 764544  (Write only) Contains additional modifier bits, as follows.
;;;	    These bits are reset to zero when the debuggee's Unibus is reset.
;;;	    1  Bit 17 of the debuggee-Unibus address.
;;;	    2  Resets the debuggee's Unibus and bus interface.  Write a 1 here then write a 0.
;;;	    4  Timeout inhibit.  This turns off the NXM timeout for all Xbus and Unibus cycles 
;;;	       done by the debuggee's bus interface (not just those commanded by the debugger).
;;; 764542  (Read only) These contain the status for bus cycles executed on the debuggee's
;;;	    busses.  These bits are cleared by writing into location 766044 (Error Status)
;;;	    on the debuggee's Unibus.  They are not cleared by power up.
;;;	    [Presently, as of 6/23/78, this doesn't work, you must get these over the Unibus.]
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
;;; Beware double-word references by the 10-11 interface.  Amazingly enough, it seems to work!

(DECLARE (SPECIAL DBG-NXM-INHIBIT)
	 (FIXNUM (CNSUBR FIXNUM) (DBG-READ FIXNUM) (CNSUBW FIXNUM FIXNUM))
	 (NOTYPE (DBG-WRITE FIXNUM FIXNUM))
	 (*EXPR CC-PRINT-SET-BITS CNSUBR CNSUBW PRIN1-THEN-SPACE))

(SETQ DBG-NXM-INHIBIT NIL)

;;; Read a location on the debuggee's Unibus
(DEFUN DBG-READ (ADR)
  (CNSUBW 764544 (+ (LSH ADR -17.) (COND (DBG-NXM-INHIBIT 4) (T 0))))
  (CNSUBW 764546 (LSH ADR -1))
  (CNSUBR 764540))

;;; Write a location on the debuggee's Unibus

(DEFUN DBG-WRITE (ADR VAL)
  (CNSUBW 764544 (+ (LSH ADR -17.) (COND (DBG-NXM-INHIBIT 4) (T 0))))
  (CNSUBW 764546 (LSH ADR -1))
  (CNSUBW 764540 VAL)
  T)

;;; Reset the debuggee's Unibus
(DEFUN DBG-RESET ()
  (CNSUBW 764544 2)
  (CNSUBW 764544 (COND (DBG-NXM-INHIBIT 4) (T 0)))
  T)

;;; Print the error status bits
(DEFUN DBG-PRINT-STATUS ()
   (CC-PRINT-SET-BITS ;(CNSUBR 764542)
		      (DBG-READ 766044)
		      '(XBUS-NXM-ERR XBUS-PARITY-ERR CADR-ADDRESS-PARITY-ERR
			UNIBUS-NXM-ERR CADR-DATA-PARITY-ERR UNIBUS-MAP-ERR
			NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))

;;; Reset the error status
(DEFUN DBG-RESET-STATUS ()
  (DBG-WRITE 766044 0))

;;; Higher-level operations

(DECLARE (FIXNUM (DBG-READ-UNIBUS-MAP FIXNUM)
		 (DBG-SETUP-UNIBUS-MAP FIXNUM FIXNUM)
		 (DBG-READ-XBUS FIXNUM))
	 (NOTYPE (DBG-WRITE-UNIBUS-MAP FIXNUM FIXNUM)
		 (DBG-WRITE-XBUS FIXNUM FIXNUM))
	 )

;;; The Unibus map is 16 words at 766140.  It consists of 14 address bits, write-ok, and valid
;;; It controls locations 140000-177777 (2000 byte locations per page).
(DEFUN DBG-READ-UNIBUS-MAP (LOC)
  (DBG-READ (+ 766140 (* 2 LOC))))

(DEFUN DBG-WRITE-UNIBUS-MAP (LOC VAL)
  (DBG-WRITE (+ 766140 (* 2 LOC)) VAL))

;; Returns unibus location mapped into specified xbus location
(DEFUN DBG-SETUP-UNIBUS-MAP (LOC XBUS-LOC)
  (DBG-WRITE-UNIBUS-MAP LOC (+ 140000 (BOOLE 1 37777 (LSH XBUS-LOC -8))))
  (+ 140000 (* 4 (BOOLE 1 377 XBUS-LOC))))

(DEFUN DBG-PRINT-UNIBUS-MAP ()
  (DO ((LOC 0 (1+ LOC))
       (CONTENTS))
      ((= LOC 20))
    (DECLARE (FIXNUM LOC CONTENTS))
    (SETQ CONTENTS (DBG-READ-UNIBUS-MAP LOC))
    (PRINT LOC)
    (PRIN1-THEN-SPACE (COND ((ZEROP (BOOLE 1 100000 CONTENTS)) 'NOT-VALID) (T 'VALID)))
    (PRIN1-THEN-SPACE (COND ((ZEROP (BOOLE 1 40000 CONTENTS)) 'READ-ONLY) (T 'WRITE-OK)))
    (PRIN1 (LSH (BOOLE 1 37777 CONTENTS) 8))))

;;; Routines to read and write the Xbus using Unibus map location 0

(DEFUN DBG-READ-XBUS (XBUS-LOC)
  ((LAMBDA (UBUS-LOC)
      (DECLARE (FIXNUM UBUS-LOC))
      (+ (DBG-READ UBUS-LOC) (LSH (DBG-READ (+ UBUS-LOC 2)) 16.)))
   (DBG-SETUP-UNIBUS-MAP 0 XBUS-LOC)))

(DEFUN DBG-WRITE-XBUS (XBUS-LOC VAL)
  ((LAMBDA (UBUS-LOC)
      (DECLARE (FIXNUM UBUS-LOC))
      (DBG-WRITE UBUS-LOC VAL)
      (DBG-WRITE (+ UBUS-LOC 2) (LSH VAL -16.)))
   (DBG-SETUP-UNIBUS-MAP 0 XBUS-LOC)))

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
      (DECLARE (FIXNUM INTC))
      (TERPRI)
      (CC-PRINT-SET-BITS INTC
			 '( DISABLE-INT-GRANT LOCAL-ENABLE NIL NIL NIL NIL NIL NIL NIL NIL
			    ENABLE-UNIBUS-INT INT-STOPS-GRANTS NIL NIL XBUS-INT UNIBUS-INT ))
      (PRINC '| LEVEL=|)
      (PRIN1 (BOOLE 1 3 (LSH INTC -12.)))
      (PRINC '| VECTOR=|)
      (PRIN1 (BOOLE 1 1774 INTC)))
   (DBG-READ 766040)))
