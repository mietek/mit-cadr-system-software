;;; DCHECK - for checking out newly-constructed disk controls		-*-LISP-*-
;;; Goes in CADR package

(DECLARE (COND ((STATUS FEATURE LISPM))   ;DO NOTHING ON LISP MACHINE.
	       ((NULL (MEMQ 'NEWIO (STATUS FEATURES)))
		(BREAK 'YOU-HAVE-TO-COMPILE-THIS-WITH-QCOMPL T))
	       ((NULL (GET 'IF-FOR-MACLISP 'MACRO))
		(LOAD '(MACROS > DSK LISPM2))  ;Just #M and #Q really
		(MACROS T)))) ;SEND OVER THE REST OF THE MACROS IN THIS FILE

(DECLARE (SPECIAL BYPASS-SEEKS))
(SETQ BYPASS-SEEKS NIL) ;NON-NIL INHIBITS SEEKING IN DCHECK

(DEFVAR MARKSMAN-P NIL)	;You must set this to T if you are testing a marksman

(declare (special local-disk-p))  ;Local disk at normal address -4
(setq local-disk-p nil)

(DECLARE (SPECIAL SPY-ACCESS-PATH CC-SUSPECT-BIT-LIST CC-DIAG-TRACE
		  DCHECK-AND DCHECK-IOR DCHECK-ADR-AND DCHECK-ADR-IOR
		  DC-STS-ADR DC-MA-ADR DC-DA-ADR DC-ECC-ADR DC-CMD-ADR DC-CLP-ADR DC-START-ADR
		  DC-READ DC-WRITE DC-SEEK DC-RECAL DC-FAULT-CLEAR DC-READ-ALL DC-WRITE-ALL
		  DC-READ-COMPARE DC-AT-EASE DC-OFFSET-CLEAR DC-STOP
		  DC-SOME-ERROR-BITS DC-ALL-ERROR-BITS CCW-LOC NXM-LOC1 NXM-LOC2 NXM-LOC3))

(SETQ DC-STS-ADR 17377774
      DC-MA-ADR 17377775
      DC-DA-ADR 17377776
      DC-ECC-ADR 17377777
      DC-CMD-ADR 17377774
      DC-CLP-ADR 17377775
      DC-START-ADR 17377777)

(SETQ DC-READ 0 DC-READ-COMPARE 10 DC-WRITE 11 DC-READ-ALL 2 DC-WRITE-ALL 13
      DC-SEEK 4 DC-AT-EASE 5 DC-RECAL 10001005 DC-FAULT-CLEAR 10000405 DC-OFFSET-CLEAR 6
      DC-STOP 16)

(SETQ DC-SOME-ERROR-BITS 06077560  ;MUL-SEL, NO-SEL, FLT, OFF-CYL, OFF-LINE, SEEK-ERR, TIMEOUT,
				   ;START-BLOCK, TRANSFER-ABORTED, OVERRUN, PAR, NXM
      DC-ALL-ERROR-BITS ;47777560   ;ALSO ECC-SOFT, ECC-HARD, ECC-HDR, HCE, IPE
			(+ (LSH 1 23.) 7777560) ;AVOID MAKING BIGNUM
      CCW-LOC 777
      NXM-LOC1 16777777		 ;THESE 3 ATTEMPT TO GET 1 AND 0 IN ALL BITS
      NXM-LOC2 15000000		 ;ASSUMING MACHINE HAS LESS THAN 1792K CORE
      NXM-LOC3 07000000)

(PROGN 'COMPILE  ;Damned splicing macros don't work at top level
#M (PROGN 'COMPILE
(DECLARE (FIXNUM (PHYS-MEM-READ FIXNUM) (DC-READ-MA))
	 (NOTYPE (PHYS-MEM-WRITE FIXNUM FIXNUM)
		 (DCHECK-COMPARE FIXNUM FIXNUM) (DCHECK-PM NOTYPE FIXNUM)))

(DECLARE (*EXPR PHYS-MEM-READ PHYS-MEM-WRITE
		CC-FINGER-SUSPECT-BIT CC-PRINT-SET-BITS CC-PRINT-BIT-LIST))

(DECLARE (FIXNUM I J K M N BITNO MASK ADR VAL))

(DEFUN LOGAND MACRO (X) `(BOOLE 1 . ,(CDR X)))

(DEFUN LOGIOR MACRO (X) `(BOOLE 7 . ,(CDR X)))

(DEFUN LOGXOR MACRO (X) `(BOOLE 6 . ,(CDR X)))

(DEFUN LDB-TEST MACRO (X)
  (LET ((PP (LSH (CADR X) -6))
	(SS (LOGAND 77 (CADR X)))
	(W (CADDR X)))
    (LIST 'NOT (LIST 'ZEROP (LIST 'LOGAND (LSH (1- (LSH 1 SS)) PP) W)))))

(DEFUN PHYS-MEM-READ-24 MACRO (X)
  (RPLACA X 'PHYS-MEM-READ))

);#M

#Q
(DEFUN PHYS-MEM-READ-24 MACRO (X)
  `(LET ((VAL (PHYS-MEM-READ . , (CDR X))))
     (LOGIOR (LSH (LDB 2701 VAL) 27) (LDB 27 VAL)))) ;Ensure fixnum
);PROGN 'COMPILE

(defun use-local-disk ()
  (setq local-disk-p t
	DC-STS-ADR 377770
	DC-MA-ADR 377771
	DC-DA-ADR 377772
	DC-ECC-ADR 377773
	DC-CMD-ADR 377770
	DC-CLP-ADR 377771
	DC-START-ADR 377773)
  (fset'phys-mem-read 'xbus-read)
  (fset'phys-mem-write 'xbus-write))

(defun xbus-read (loc)
  (setq loc (+ loc (lsh 77 18.)))
  (dpb (%p-ldb 2020 loc) 2020 (%p-ldb 0020 loc)))

(defun xbus-write (loc val)
  (%p-store-tag-and-pointer (+ loc (lsh 77 18.))
			    (ldb 3010 val)
			    (%24-bit-plus (lsh (ldb 2701 val) 27)
					  (ldb 0027 val))))


;;; Basic disk manipulation

(DEFUN DC-READ-MA ()  ;High bits of this register are garbage, only 22 bits are really MA
  (LOGLDB 0026 (PHYS-MEM-READ DC-MA-ADR)))

(DEFUN DC-PRINT-STATUS ()
  (DC-PRINT-STATUS1 (PHYS-MEM-READ DC-STS-ADR)))

(DEFUN DC-PRINT-STATUS1 (STATUS)
     (TERPRI)
     (CC-PRINT-SET-BITS STATUS '( IDLE ANY-ATTN SEL-UNIT-ATTN INTR MULTIPLE-SELECT NO-SELECT
				  SEL-UNIT-FAULT SEL-UNIT-READ-ONLY SEL-UNIT-OFF-CYLINDER
				  SEL-UNIT-OFF-LINE SEL-UNIT-SEEK-ERROR TIMEOUT-ERROR
				  START-BLOCK-ERROR TRANSFER-ABORTED OVERRUN
				  ECC-SOFT ECC-HARD ECC-HEADER HEADER-COMPARE-ERROR
				  MEM-PARITY-ERROR NXM-ERROR CCW-CYCLE READ-COMPARE-DIFFERENCE
				  INTERNAL-PARITY-ERROR ))
     #M (PROGN (PRINC '| SEL-UNIT-BLOCK-CTR=|) (PRIN1 (LSH STATUS -24.))))

(defun dc-print-marksman-status ()
  (let ((sts))
    (dc-print-status)
    (terpri)
    (dc-exec-1 5)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (cc-print-set-bits sts '( track-zero landing-zone ill-cmd ready
			      spin-out-of-limit end-of-cyl diag-error track-zero-error ))
    (dc-exec-1 200005)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (princ '|sector-length=|)
    (prin1 (logldb 0002 sts))
    (tyo 40)
    (cc-print-set-bits sts '( nil nil illegal-set-sector sector-switches-overridden
			      illegal-rezero-or-illegal-seek illegal-cylinder illegal-command
			      write-protect-violation ))
    (terpri)
    (dc-exec-1 400005)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (princ '|last cmd byte 1=|)
    (prin1 sts)
    (dc-exec-1 600005)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (princ '|, byte 2=|)
    (prin1 sts)
    (dc-exec-1 1000005)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (princ '|, cur cyl=|)
    (prin1 sts)
    (dc-exec-1 1200005)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (princ '|, sec//trk-1=|)
    (prin1 sts)
    (dc-exec-1 1400005)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (princ '|, diag sts 1=|)
    (prin1 sts)
    (dc-exec-1 1600005)
    (setq sts (logldb 3010 (phys-mem-read dc-ma-adr)))
    (princ '|, diag sts 2=|)
    (prin1 sts)))

;;; Seek, print status if error
(DEFUN DC-SEEK (CYL)
  (PHYS-MEM-WRITE DC-CMD-ADR DC-AT-EASE)
  (PHYS-MEM-WRITE DC-START-ADR 0)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR)))) ;Await Idle
  (PHYS-MEM-WRITE DC-DA-ADR (LOGDPB CYL 2014 0))
  (PHYS-MEM-WRITE DC-CMD-ADR (logdpb cyl 3010 (logdpb 100 2010 DC-SEEK)))
  (PHYS-MEM-WRITE DC-START-ADR 0)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR)))) ;Await Idle
  (OR MARKSMAN-P
      (DO () ((LDB-TEST 0201 (PHYS-MEM-READ DC-STS-ADR))) ;Await attention
	#M (SLEEP 0.03)
	#Q (PROCESS-ALLOW-SCHEDULE)
	))
  (DC-CHECK-STATUS DC-SOME-ERROR-BITS))

(defun dc-recal-marksman ()
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR)))) ;Await Idle
  (dc-exec 10000005 0 0 0 0 nil 0)
  (dc-print-marksman-status))

;;; Run internal marksman diagnostics (this doesn't work, maybe we don't have the firmware)
(defun dc-diag-marksman (test-number)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR)))) ;Await Idle
  (PHYS-MEM-WRITE DC-CMD-ADR (logdpb (+ 200 test-number) 2010 5))
  (PHYS-MEM-WRITE DC-START-ADR 0)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR)))) ;Await Idle
  (dc-print-marksman-status))

;;; Perform a read or write, check specified status bits.
(DEFUN DC-EXEC (CMD CYL HEAD BLOCK CLP CCW ERR-BITS)
  (PHYS-MEM-WRITE DC-CMD-ADR DC-AT-EASE)
  (PHYS-MEM-WRITE DC-START-ADR 0)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR)))) ;Await Idle
  (PHYS-MEM-WRITE DC-DA-ADR (LOGDPB CYL 2014 (+ (LSH HEAD 8) BLOCK)))
  (PHYS-MEM-WRITE DC-CLP-ADR CLP)
  (AND CCW (PHYS-MEM-WRITE CLP CCW))
  (PHYS-MEM-WRITE DC-CMD-ADR CMD)
  (PHYS-MEM-WRITE DC-START-ADR 0)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR))) ;Await Idle
    #M (SLEEP 0.03)
    #Q (KBD-CHAR-AVAILABLE)
    )
  (DC-CHECK-STATUS ERR-BITS))

;;; Very simplified version used for reading back status.  Don't want to bash
;;; disk address register.
(DEFUN DC-EXEC-1 (CMD)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR)))) ;Await Idle
  (PHYS-MEM-WRITE DC-CMD-ADR CMD)
  (PHYS-MEM-WRITE DC-START-ADR 0)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR))) ;Await Idle
    #M (SLEEP 0.03)
    #Q (KBD-CHAR-AVAILABLE)
    ))

;;; Barf if any of specified bits on in status
(DEFUN DC-CHECK-STATUS (MASK)
  (LET ((VAL (PHYS-MEM-READ-24 DC-STS-ADR)))
    (COND ((NOT (ZEROP (LOGAND MASK VAL)))
	   (DC-PRINT-STATUS1 VAL)))))

;;; This function provides a scope loop for debugging problems starting up
;;; the microcode that would otherwise lead to hangs.
(DEFUN DC-RAPID-START ()
  (DO () ((KBD-TYI-NO-HANG))
    (PHYS-MEM-WRITE DC-CMD-ADR DC-AT-EASE)	;Do the command that loses (at ease
    (PHYS-MEM-WRITE DC-START-ADR 0)		; is the most null command)
    (DOTIMES (I 100) NIL)
    (PHYS-MEM-WRITE DC-CMD-ADR DC-STOP)		;Stop the disk control forcibly
    (PHYS-MEM-WRITE DC-CMD-ADR DC-AT-EASE)))	;Cease to stop

;;; Simpler version of DC-EXEC, for scoping
(DEFUN DC-EXEC-2 (CMD CYL HEAD BLOCK CLP CCW ERR-BITS)
  (PHYS-MEM-WRITE DC-DA-ADR (LOGDPB CYL 2014 (+ (LSH HEAD 8) BLOCK)))
  (PHYS-MEM-WRITE DC-CLP-ADR CLP)
  (AND CCW (PHYS-MEM-WRITE CLP CCW))
  (PHYS-MEM-WRITE DC-CMD-ADR CMD)
  (PHYS-MEM-WRITE DC-START-ADR 0)
  (DO () ((LDB-TEST 0001 (PHYS-MEM-READ DC-STS-ADR))) ;Await Idle
    #M (SLEEP 0.03)
    #Q (KBD-CHAR-AVAILABLE)
    )
  (DC-CHECK-STATUS ERR-BITS))

;;; Test function

(DEFUN DCHECK (&AUX CONTROLLER-TYPE)
  (SETQ CONTROLLER-TYPE (LDB (BITS 2 22.) (PHYS-MEM-READ DC-MA-ADR)))
  (FORMAT T
      "~&CONTROLLER TYPE IS ~D~0G (~[Trident~;Marksman~;unused?~;Unmodified Trident~])~%"
      CONTROLLER-TYPE)
  ;Don't do this, the hardware isn't necessarily right.  This IS a diagnostic, after all.
  ;(SETQ MARKSMAN-P (= CONTROLLER-TYPE 1))
  (FORMAT T "~&Operating as if ~:[Trident~;Marksman~]~%" MARKSMAN-P)
  ;; Part 1 - verify bus response to reading and writing disk-address register
  (PHYS-MEM-WRITE DC-DA-ADR 1777777777) ;28 bits
  (COND ((ZEROP (PHYS-MEM-READ DC-DA-ADR))
	 (DCHECK-ERR-LOOP '|No response on Unibus or Xbus, or failed to write or read DA|
			  DC-DA-ADR 1777777777 1777777777)))
  ;; Part 2 - write 0 in DA, check for bits stuck at 1
  (PHYS-MEM-WRITE DC-DA-ADR 0)
  (DO ((VAL (PHYS-MEM-READ DC-DA-ADR))
       (BITNO 0 (1+ BITNO))
       #M (MASK 1 (LSH MASK 1))
       (CC-SUSPECT-BIT-LIST NIL))
      ((= BITNO 28.)
       (COND ((NOT (NULL CC-SUSPECT-BIT-LIST))
	      (CC-PRINT-BIT-LIST '|Bits in DA register stuck at 1, may be/
  broken wire in XBI or XBO data paths: |
				 CC-SUSPECT-BIT-LIST)
	      (DCHECK-ERR-LOOP '|Some bits in DA register won't clear|
			       DC-DA-ADR 0 0))))
    (AND #M (NOT (ZEROP (LOGAND MASK VAL)))
	 #Q (LDB-TEST (1+ (LSH BITNO 6)) VAL)
	 (CC-FINGER-SUSPECT-BIT BITNO)))
  ;; Part 3 - write floating 1's in DA, check for bits stuck at 0 or spuriously 1
  (DO ((BITNO 0 (1+ BITNO))
       (MASK 1 (+ MASK MASK)) ;May be bignum on Lisp machine
       #Q (PPSS 0001 (+ PPSS 100))
       (VAL)
       (CC-SUSPECT-BIT-LIST NIL)
       (STUCK-0 NIL)
       (SPURIOUS-1 NIL))
      ((= BITNO 28.)
       (CC-PRINT-BIT-LIST '|Bits in DA register stuck at 0: | STUCK-0)
       (CC-PRINT-BIT-LIST '|Bits in DA register 1 when they shouldn't be: | SPURIOUS-1)
       (AND STUCK-0
	    (DCHECK-ERR-LOOP '|Testing first stuck-0 bit in DA register:|
			     DC-DA-ADR 0 (LOGDPB 1 (1+ (LSH (CAR STUCK-0) 6)) 0)))
       (AND SPURIOUS-1
	    (DCHECK-ERR-LOOP '|Testing first spurious-1 bit in DA register:|
			    DC-DA-ADR 0 (LOGDPB 0 (1+ (LSH (CAR SPURIOUS-1) 6)) 1777777777))))
    (PHYS-MEM-WRITE DC-DA-ADR MASK)
    (SETQ VAL (PHYS-MEM-READ DC-DA-ADR))
    (AND #M (ZEROP (LOGAND MASK VAL))
	 #Q (NOT (LDB-TEST PPSS VAL))
	 (SETQ STUCK-0 (CONS BITNO STUCK-0)))
    (DO I 0 (1+ I) (= I 28.)
      (AND (NOT (= I BITNO))
	   #M (NOT (ZEROP (LOGAND (LSH 1 I) VAL)))
	   #Q (LDB-TEST (1+ (LSH I 6)) VAL)
	   (SETQ SPURIOUS-1 (CONS I SPURIOUS-1)))))
  ;; Part 3.5 - check that the block counter is counting.  This checks
  ;; that the disk is rotating and that the index/sector pulse logic works.
  #Q (DCHECK-BLOCK-COUNTER)
  ;; Part 3.6 - recalibrate.  Marksman needs this if I/O reset has been done.
  (FORMAT T "~&Recalibrate...")
  (DC-EXEC DC-RECAL 0 0 0 0 NIL 0)
  (DO () ((NOT (BIT-TEST 1_8 (PHYS-MEM-READ DC-STS-ADR))))
    (PROCESS-ALLOW-SCHEDULE))
  ;; Part 4 - Test disk bus bits and basic command logic by seeking
  (COND ((NOT BYPASS-SEEKS)
	 (DCHECK-SEEK (if marksman-p 209. 814.))
	 (DO I (if marksman-p 128. 512.) (LSH I -1) (ZEROP I)
	   (DCHECK-SEEK I))))
  (and local-disk-p (break the-rest-of-this-aint-gonna-work))
  ;; Part 5 - Check address logic by reading with a CLP that points at NXM
  ;;	      and then a CCW that points at NXM, check error status and MA.
  ;;   Note that if the read fails to happen, e.g. due to header-compare-error, the
  ;;   MA is naturally going to be wrong also since no memory cycles at all will happen.
  (LET ((MASK	;Bits which are suspect (this stuff is only 22 bits, fits in fixnum)
	 (LOGIOR (DCHECK-CLP-ADR NXM-LOC1) (DCHECK-CLP-ADR NXM-LOC2) (DCHECK-CLP-ADR NXM-LOC3)
		 (DCHECK-CCW-ADR NXM-LOC3) (DCHECK-CCW-ADR NXM-LOC2) (DCHECK-CCW-ADR NXM-LOC1)
		 )))
    (COND ((NOT (ZEROP MASK))
	   (DCHECK-ERR-LOOP   ;Not the ultimate winning test loop, but maybe OK for now
	      '|Writing CLP, reading MA (should be 16777777), frobbing bits that failed|
	      DC-CLP-ADR 0 MASK))))
  ;; Part 6 - Write and read block 1 of the disk.  Use a floating 1's and 0's
  ;;          pattern, and then an address pattern, and check for Xbus data path
  ;;	      and addressing failures.
  ;; This doesn't check high-order address bits
  (DO I 0 (1+ I) (= I 40)	;Loc 0-37 get floating 1's
    (PHYS-MEM-WRITE I (#M LSH #Q ASH 1 I)))
  (DO I 0 (1+ I) (= I 40)	;Loc 40-77 get floating 0's
    (PHYS-MEM-WRITE (+ 40 I) (- (#M LSH #Q ASH 1 32.) (#M LSH #Q ASH 1 I))))
  (DO I 100 (1+ I) (= I 400)	;Loc 100-377 get address pattern
    (PHYS-MEM-WRITE I (+ (LSH (LOGXOR 377 I) 8) I)))
  (PRINT 'WRITE)
  (DC-EXEC DC-WRITE 0 0 1 CCW-LOC 0 DC-ALL-ERROR-BITS)
  (LET ((MA (DC-READ-MA)))
    #M (DECLARE (FIXNUM MA))
    (COND ((NOT (= MA 377))
	   (TERPRI) (PRINC '|MA wrong on write of pattern, correct=377, actual=|)
	   (PRIN1 MA))))
  (DO I 0 (1+ I) (= I 400)	;Clear buffer
    (PHYS-MEM-WRITE I 0))
  (PRINT 'READ)
  (DC-EXEC DC-READ 0 0 1 CCW-LOC 0 DC-ALL-ERROR-BITS)
  (LET ((MA (DC-READ-MA)))
    #M (DECLARE (FIXNUM MA))
    (COND ((NOT (= MA 377))
	   (TERPRI) (PRINC '|MA wrong on read of pattern, correct=377, actual=|)
	   (PRIN1 MA))))
  ;; Check pattern read back into core, see if it's correct
  (LET ((DCHECK-AND 37777777777) (DCHECK-IOR 0)  ;Accumulate error bits here
	(DCHECK-ADR-AND 377) (DCHECK-ADR-IOR 0))
    (DO I 0 (1+ I) (= I 40)	;Loc 0-37 get floating 1's
      (DCHECK-COMPARE I (#M LSH #Q ASH 1 I)))
    (DO I 0 (1+ I) (= I 40)	;Loc 40-77 get floating 0's
      (DCHECK-COMPARE (+ 40 I) (- (#M LSH #Q ASH 1 32.) (#M LSH #Q ASH 1 I))))
    (DO I 100 (1+ I) (= I 400)	;Loc 100-377 get address pattern
      (DCHECK-COMPARE I (+ (#M LSH #Q ASH (LOGXOR 377 I) 8) I)))
    (DCHECK-PM '|Data bits dropped during write to or read from disk: |
	       (LOGXOR 37777777777 DCHECK-IOR))
    (DCHECK-PM '|Data bits picked during write to or read from disk: |
	       DCHECK-AND)
    (DCHECK-PM '|Address bits 0 with bad data during write to or read from disk: |
	       (LOGXOR 377 DCHECK-ADR-AND))
    (DCHECK-PM '|Address bits 1 with bad data during write to or read from disk: |
	       DCHECK-ADR-IOR))
  ;; Maybe there should be a test-loop for the above?
  ;; part 7 - in case loser didn't look at the heads and see that they moved
  ;; correctly during part 4, which is hard to do on a T-300, we will here
  ;; assume we have a good pack and try reading from each power of 2 cylinder.
  ;; This will get a header-compare error if a disk bus bit doesn't work.
  (TERPRI)
  (PRINC "Trying reads of various blocks; will get HEADER-COMPARE if disk bus bits bad")
  (TERPRI)
  (DC-EXEC DC-READ 0 0 0 CCW-LOC 0 DC-ALL-ERROR-BITS)
  (PRINC " cyl 0 ")
  (DO CYL 1 (LSH CYL 1) (= CYL 2000)
    (DC-EXEC DC-READ CYL 0 0 CCW-LOC 0 DC-ALL-ERROR-BITS)
    (PRINC " cyl ")
    (PRIN1 CYL))
  ;; end
  (TERPRI)
  (PRINC "You might enjoy trying DC-WRITE-READ-TEST")
  (TERPRI)
  (PRINC '|End of DCHECK.  Now run the format program and the ECC test program.|))

;;; Compare pattern, set special variables if lose
;;; Also obeys CC-DIAG-TRACE
(DEFUN DCHECK-COMPARE (ADR VAL)
  (LET ((MASK (PHYS-MEM-READ ADR)))
    (SETQ DCHECK-AND (LOGAND DCHECK-AND MASK)
	  DCHECK-IOR (LOGIOR DCHECK-IOR MASK))
    (COND ((NOT (= MASK VAL))
	   (AND CC-DIAG-TRACE
		(FORMAT T "~&Address ~O Good ~O Bad ~O~%" ADR VAL MASK))
	   (SETQ DCHECK-ADR-AND (LOGAND DCHECK-ADR-AND ADR)
		 DCHECK-ADR-IOR (LOGIOR DCHECK-ADR-IOR ADR))))
    NIL))

;;; Print bit list given as mask
(DEFUN DCHECK-PM (MESSAGE MASK &AUX CC-SUSPECT-BIT-LIST) ;CC-PRINT-BIT-LIST looks at it
  (OR (ZEROP MASK)
      (CC-PRINT-BIT-LIST MESSAGE
			 (DO ((BITNO 0 (1+ BITNO))
			      (L NIL))
			     ((ZEROP MASK) L)
			   (AND (ODDP MASK) (SETQ L (CONS BITNO L)))
			   (SETQ MASK (#M LSH #Q ASH MASK -1))))))

;;; Check that the block counter is counting, and producing
;;; all the right values and only the right values.  This one
;;; will not run in Maclisp.
#Q
(DEFUN DCHECK-BLOCK-COUNTER ()
  (DO ((DESIRED-VALUES #10R (IF MARKSMAN-P
				'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
				'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
       (GOOD-VALUES NIL)
       (BAD-VALUES NIL)
       (MISSING-VALUES)
       (BCTR)
       (START-TIME (TIME)))
      ((> (TIME-DIFFERENCE (TIME) START-TIME) 30.)	;Run for 1/2 second
       (SETQ GOOD-VALUES (SORT GOOD-VALUES #'<))
       (SETQ BAD-VALUES (SORT BAD-VALUES #'<))
       (SETQ MISSING-VALUES (COPYLIST DESIRED-VALUES))
       (DOLIST (X GOOD-VALUES)
	 (SETQ MISSING-VALUES (DELQ X MISSING-VALUES)))
       (AND (OR BAD-VALUES MISSING-VALUES)
	    (FORMAT T "~&Problems with block counter.  May be disk not spinning, lack of
 index or sector pulse, or problem with block counter logic.~%"))
       (AND BAD-VALUES (FORMAT T "Erroneous values seen (octal): ~{~O~}~%" BAD-VALUES))
       (AND MISSING-VALUES (FORMAT T "Values not seen (octal): ~{~O~}
Good values that were seen: ~{~O~}~%" MISSING-VALUES GOOD-VALUES)))
    (SETQ BCTR (LDB 3010 (PHYS-MEM-READ DC-STS-ADR)))
    (IF (MEMQ BCTR DESIRED-VALUES)
	(OR (MEMQ BCTR GOOD-VALUES) (PUSH BCTR GOOD-VALUES))
	(OR (MEMQ BCTR BAD-VALUES) (PUSH BCTR BAD-VALUES)))))

;;; Check address logic, return bits which failed
(DEFUN DCHECK-CLP-ADR (ADR)
  (LET ((MA 0))
    (DECLARE (FIXNUM MA))
    (DC-EXEC DC-READ 0 0 0 ADR NIL 0)
    (COND ((NOT (= (LOGAND 14000000 (PHYS-MEM-READ-24 DC-STS-ADR))
		   14000000))  ;NXM and CCW CYCLE
	   (DC-PRINT-STATUS) (TERPRI)
	   (PRINC '|NXM or CCW-CYCLE failure with CLP pointing to non-existent memory loc |)
	   (PRIN1 ADR)))
    (SETQ MA (DC-READ-MA))
    (COND ((NOT (= MA ADR))
	   (TERPRI)
	   (PRINC '|MA wrong on CLP nxm, probably address data path failure, correct=|)
	   (PRIN1 ADR)
	   (PRINC '|, actual=|)
	   (PRIN1 MA)
	   (LOGXOR MA ADR))
	  (T 0))))

;Tight reading loop.
(DEFUN DCHECK-HEADER-COMPARE-LOOP NIL
 (DO () ((KBD-TYI-NO-HANG))
   (DC-EXEC DC-READ 0 0 0 CCW-LOC 1000 0)))

(DEFUN DCHECK-CCW-ADR (ADR)
  (LET ((MA 0))
    (DECLARE (FIXNUM MA))
    (DC-EXEC DC-READ 0 0 0 CCW-LOC (SETQ ADR (LOGAND 77777400 ADR)) 0)
    (COND ((NOT (= (LOGAND 14000000 (PHYS-MEM-READ-24 DC-STS-ADR))
		   04000000))  ;NXM and -CCW CYCLE
	   (DC-PRINT-STATUS) (TERPRI)
	   (PRINC '|NXM or CCW-CYCLE failure with CCW pointing to non-existent memory loc |)
	   (PRIN1 ADR)))
    (SETQ MA (DC-READ-MA))
    (COND ((NOT (= MA ADR))
	   (TERPRI)
	   (PRINC '|MA wrong on CCW nxm, probably address data path failure, correct=|)
	   (PRIN1 ADR)
	   (PRINC '|, actual=|)
	   (PRIN1 MA)
	   (LOGXOR MA ADR))
	  (T 0))))

;;; Alternating seek test
(DEFUN DCHECK-SEEK (CYL)
  (TERPRI)
  (PRINC '|Should be seeking between cylinders 0 and |)
  (LET ((BASE 10.) (*NOPOINT NIL))
    (PRIN1 CYL))
  (PRINC '| - type space when OK. |)
  (DO () (#M (NOT (ZEROP (LISTEN))) #Q (KBD-TYI-NO-HANG))
    (DC-SEEK 0)
    (DC-SEEK CYL))
  #M (TYI)
  (TERPRI))

;;; Basic XBUS errors call this guy, which prints a message and enters a test loop.
(DEFUN DCHECK-ERR-LOOP (MESSAGE ADR VAL1 VAL2)
  (TERPRI)
  (PRINC MESSAGE)
  (TERPRI)
  (PRINC '|Now entering scope loop, writing |)
  (PRIN1 VAL1)
  (COND ((NOT (= VAL1 VAL2))
	 (PRINC '| and |)
	 (PRIN1 VAL2)))
  (PRINC '| into |)
  (PRIN1 ADR)
  (PRINC '| and reading it back.|)
  (DO () (#Q (KBD-TYI-NO-HANG) #M (LISTEN))
    (PHYS-MEM-WRITE ADR VAL1)
    (PHYS-MEM-READ ADR)
    (PHYS-MEM-WRITE ADR VAL2)
    (PHYS-MEM-READ ADR)))

;;; ECC Test (in DCFU) error-message printer

;;; This frob goes with the ECC tester in DCFU
;;; He prints out the results of the log, which
;;; are in page 28.
;;; Each frob has status, ecc, bit mask, preceding word,
;;; error word, next word.
;;; The background is 0's.
;;; The special variable ecc-wd-no is the bit corresponding
;;; to a-ecc-wd in the ucode.

(declare (special ecc-wd-no))

(defun decode-ecc-logout ()
  (do ((bitno 0 (1+ bitno))
       (real-bitno (* 32. ecc-wd-no) (1+ real-bitno))
       (logout-pntr (* 28. 400) (+ logout-pntr 6)))
      ((= bitno 32.))
    (declare (fixnum bitno real-bitno logout-pntr
		     sts ecc msk prev-wd err-wd next-wd))
    (let ((sts (phys-mem-read-24 logout-pntr))
	  (ecc (phys-mem-read (+ logout-pntr 1)))
	  (msk (phys-mem-read (+ logout-pntr 2)))
	  (prev-wd (phys-mem-read (+ logout-pntr 3)))
	  (err-wd (phys-mem-read (+ logout-pntr 4)))
	  (next-wd (phys-mem-read (+ logout-pntr 5))))
      #M (or (= msk (lsh 1 bitno))
	     (break msk-bites-the-fucking-bag t))
      (cond ((bit-test (logxor dc-all-error-bits 1_17) sts) ;all errs except ecc soft
	     (dc-print-status1 sts)
	     (format t '| error for bit ~D.| real-bitno))
	    ((not (LDB-TEST 1701 sts)) ;Bit 15.
	     (format t '|~%missing ecc soft error bit ~D.| real-bitno))
	    (t  ;Soft error, check pattern
	     (do ((pat (logldb 2013 ecc) (lsh pat -1))
		  (pos (1- (logldb 0020 ecc)) (1+ pos)))
		 ((oddp pat)
		  (cond ((and (= pat 1) (= pos real-bitno)))
			(t (format t '|~%soft err wrong bit ~D., pos=~D.-1, pat=~O (i.e. ~D., ~O)|
				      real-bitno (logldb 0020 ecc)
				      (logldb 2013 ecc) pos pat))))
	       (declare (fixnum pos pat)))))
      ;; Also check out the data read in
      (and (> real-bitno 40)
	   (not (zerop prev-wd))
	   (format t '|~%For bit ~D., prev wd ~O should be 0|
		     real-bitno prev-wd))
      (and (not (= err-wd msk))
	   (format t '|~%For bit ~D., err wd ~O should be ~O|
		     real-bitno err-wd msk))
      (and (< real-bitno (- (* 256. 32.) 40))
	   (not (zerop next-wd))
	   (format t '|~%For bit ~D., next wd ~O should be 0|
		     real-bitno next-wd))
      )))

;;; Read/Write test

(declare (special dc-write-read-trace))
(setq dc-write-read-trace t)

;;; Low-level routine, does a write and a read and compares
;;; Intended to run on Lisp machine.
;;; Uses memory page 200 for buffer and loc 777 for CCW
(defun dc-write-read-test-0 (cyl head blk pattern-func &aux offset)
  (setq offset 100000)  ;use this page of main memory
  ;; Trace
  (and dc-write-read-trace
       (format t '|~%WRITE-READ-TEST: cyl=~O, head=~O, blk=~O, pattern=~A|
	         cyl head blk pattern-func))
  ;; Fill memory with pattern
  (do i 0 (1+ i) (= i 400)
    (phys-mem-write (+ offset i) (funcall pattern-func i)))
  ;; Write it out
  (dc-exec dc-write cyl head blk 777 (+ offset 0) dc-all-error-bits)
  (do i 0 (1+ i) (= i 400)
      (phys-mem-write (+ offset i) 0))
  ;; Read it back
  (dc-exec dc-read cyl head blk 777 (+ offset 0) dc-all-error-bits)  
  ;; Check pattern
  (do ((i 0 (1+ i))
       (good) (bad) (heading-printed nil))
      ((= i 400))
    (setq good (funcall pattern-func i)
	  bad (phys-mem-read (+ offset i)))
    (cond ((not (= good bad))
	   (cond ((not heading-printed)
		  (format t '|~% Compare error for ~A pattern, cyl ~O, head ~O, blk ~O:~%Loc    Good      Bad|
			    pattern-func cyl head blk)
		  (setq heading-printed t)))
	   (format t '|~%~3O  ~8O ~8O| i good bad)))))

;;; Patterns for above
(defun all-zero-pat (ignore) 0)
(defun all-one-pat (ignore) 37777777777)
(defun alt-bits-pat (ignore) 25252525252)
(defun addr-pat (loc) (+ (lsh (logxor 377 loc) 8) loc))
(defun floating-one-pat (loc) (logdpb 1 (1+ (lsh (\ loc 40) 6)) 0))
(defun floating-zero-pat (loc) (logdpb 0 (1+ (lsh (\ loc 40) 6)) 37777777777))
(declare (special gubbish))
(setq gubbish 7700770066)
(defun gubbish-pat (ignore) gubbish)

;;; Uses memory page 200 for buffer and loc 777 for CCW
(defun dc-read-test-0 (cyl head blk ignore &aux offset)
  (setq offset 100000)  ;use this page of main memory
  ;; Trace
  (and dc-write-read-trace
       (format t "~%READ-TEST: cyl=~O, head=~O, blk=~O"
	         cyl head blk))
  ;; Read it
  (dc-exec dc-read cyl head blk 777 (+ offset 0) dc-all-error-bits)  
)

;;; An address specifier is a single number, a list of cases,
;;; or a list of DO, first, last, optional increment,
;;; or (on typein) ALL which translates into such.
;;; We cons current state onto the front
;;; First value is next value output from spec, second value is T if wrapped around
(defun dc-step-addr-spec (frob)
  (prog ((current (car frob)) (spec (cdr frob)))
    (cond ((atom spec)
	   (return spec t))
	  ((not (eq (car spec) 'do)) ;Cases list
	   (and (null current) (setq current 0))
	   (return (nth current spec)
		   (progn (setq current (1+ current))
			  (cond ((>= current (length spec))
				 (rplaca frob 0) t)
				(t (rplaca frob current) nil)))))
	  (t (and (null current) (setq current (cadr spec)))
	     (return current
		     (progn (setq current (+ current (or (cadddr spec) 1)))
			    (cond ((>= current (caddr spec))
				   (rplaca frob (cadr spec)) t)
				  (t (rplaca frob current) nil))))))))

;;; Step a bunch of addr specs, return list of current state of each one.
;;; First steps first, list returned is in reverse order
(defun dc-step-addr-specs (specs)
  (do ((l specs (cdr l))
       (val)(wrap-p)
       (r nil))
      ((null l) r)
    (multiple-value (val wrap-p) (dc-step-addr-spec (car l)))
    (setq r (cons val r))
    (cond ((not wrap-p)  ;Rest don't step
	   (return (do ((l (cdr l) (cdr l))
			(current) (spec)
			(r r))
		       ((null l) r)
		     (setq current (caar l) spec (cdar l))
		     (setq r (cons (cond ((atom spec) spec)
					 ((eq (car spec) 'do)
					  (or current (cadr spec)))
					 (t (and (null current) (setq current 0))
					    (and (>= current (length spec)) (setq current 0))
					    (nth current spec)))
				   r))))))))

(defun dc-get-addr-spec (prompt all &optional response)
  (let ((spec (cond (response)
		    (t
		      (format t '|~% ~A:| prompt)
		      (cond ((= (tyipeek) #/?)
			     (tyi)
			     (prin1 all)))
		      (si:read-for-top-level)))))
    (and (eq spec 'all) (setq spec all))
    (cons nil spec)))

(defun dc-get-addr-specs (response-list all-list &optional no-pattern)
 (prog nil
  (let ((cyl (dc-get-addr-spec '|Cylinders|
			       (first all-list)
			       (first response-list)))
	(head (dc-get-addr-spec '|Heads|
				(second all-list)
				(second response-list)))
	(blk (dc-get-addr-spec '|Blocks (sectors)|
			       (third all-list)
			       (third response-list)))
	(pattern-func (if no-pattern (cons nil nil)
			  (dc-get-addr-spec '|Pattern func|
					    '(all-zero-pat all-one-pat alt-bits-pat
					      addr-pat floating-one-pat floating-zero-pat
					      gubbish-pat)
					    (fourth response-list)))))
    (return cyl head blk pattern-func))))

;;; User interface to write-read test
;;; This version is kludged up, you should step only one addr at a time!
(defun dc-write-read-test (&optional response-list
				     (all-list (if marksman-p
						   '( (do 0 210.)
						      (do 0 4)
						      (do 0 21.))
						   '( (do 0 815.)
						      (do 0 5)
						      (do 0 17.) ))))
  (multiple-value-bind (cyl head blk pattern-func)
      (dc-get-addr-specs response-list all-list)
    (do () ((kbd-char-available))
      (apply 'dc-write-read-test-0
	     (dc-step-addr-specs (list pattern-func blk head cyl))))))

(defun dc-wrt ()
  (dc-write-read-test '(all all all all)))

(defun dc-read-test (&optional response-list (all-list (if marksman-p
							   '( (do 0 210.)
							      (do 0 4)
							      (do 0 21.))
							   '( (do 0 815.)
							      (do 0 5)
							      (do 0 17.) ))))
  (multiple-value-bind (cyl head blk pattern-func)
      (dc-get-addr-specs response-list all-list T)
    (do () ((kbd-char-available))
      (apply 'dc-read-test-0
	     (dc-step-addr-specs (list pattern-func blk head cyl))))))


;;; Formatting stuff
;;; This is too slow for bulk use, but useful for figuring out how you've lost.

;Routines to access "buffer" memory, which is a bunch of halfwords
;starting at XBUS address zero.

;Since things seem very marginal, and for speed, we copy the stuff in and out
;of an array, being careful while copying.

(declare (special buffer-hwd buffer-bit))
(or (boundp 'buffer-hwd)    ;20. pages for decode-track plus 1 for channel program
    (setq buffer-hwd (make-array nil 'art-16b (* 1000 21.))
	  buffer-bit (make-array nil 'art-1b (* 1000 21. 16.)
				 buffer-hwd)))

;Get buffer out of other machine
(defun get-buffer ()
  (dbg-reset-status)
  (do ((i 0 (1+ i))
       (tem) (tem1)
       (n (array-length buffer-hwd)))
      ((= i n))
    (setq tem (rd-buffer i)
	  tem1 (rd-buffer i))
    (or (= tem tem1)
	(ferror nil "Halfword ~O read as ~O and as ~O" i tem tem1))
    (as-1 tem buffer-hwd i))
  (dbg-print-status))

;Put buffer into other machine
(defun put-buffer ()
  (dbg-reset-status)
  (do ((i 0 (1+ i))
       (tem)(tem1)
       (n (array-length buffer-hwd)))
      ((= i n))
    (wr-buffer i (setq tem (ar-1 buffer-hwd i)))
    (setq tem1 (rd-buffer i))
    (or (= tem tem1)
	(ferror nil "Halfword ~O wrote ~O read back as ~O" i tem tem1)))
  (dbg-print-status))

(defun rd-buffer (loc)
  (let ((ubus-loc (dbg-setup-unibus-map 17 (lsh loc -1))))
     (cond ((zerop (logand 1 loc))
	    (dbg-read ubus-loc))
	   (t (dbg-read ubus-loc)
	      (dbg-read (+ ubus-loc 2))))))

(defun wr-buffer (loc val)
  (let ((ubus-loc (dbg-setup-unibus-map 17 (lsh loc -1))) (tem))
     (cond ((zerop (logand 1 loc))
	    (dbg-read ubus-loc)
	    (dbg-write ubus-loc val)
	    (dbg-write (+ ubus-loc 2) (setq tem (dbg-read (+ ubus-loc 2))))
            ;(ck-buffer loc val tem (dbg-read ubus-loc) (dbg-read (+ ubus-loc 2)))
	    )
	   (t (dbg-write ubus-loc (setq tem (dbg-read ubus-loc)))
	      (dbg-write (+ ubus-loc 2) val)
              ;(ck-buffer loc tem val (dbg-read ubus-loc) (dbg-read (+ ubus-loc 2)))
	      ))))

(defun ck-buffer (loc good1 good2 wd1 wd2)
     (or (and (= good1 wd1)
              (= good2 wd2))
         (ferror nil "Loc ~O wrote ~O,,~O, read ~O,,~O" loc good2 good1 wd2 wd1)))

;Given a loc in the buffer, and a disk address, store a sector whose header
;claims it is at that address, and return the advanced loc.
;This uses the copy of the buffer in this machine.
(defun store-sector (loc cyl head blk next-address-code)
  ;;Preamble+VFO lock is 61. bytes of 1's, followed by sync which is a 177
  (do i 30. (1- i) (= i 0)		;Store 60. bytes (30. halfwords) of 1's
    (as-1 177777 buffer-hwd loc)
    (setq loc (1+ loc)))
  (as-1 077777 buffer-hwd loc)		;One byte of 1's and a byte of 177
  (setq loc (1+ loc))
  ;;Header.  A 32-bit word, see the manual for format.
  ;;Followed by 32 bits of ecc.
  (let ((head1 (+ (lsh head 8) blk))
	(head2 (+ (lsh next-address-code 14.) cyl))
	(ecc1 0)
	(ecc2 0))
     (as-1 head1 buffer-hwd loc)
     (as-1 head2 buffer-hwd (1+ loc))
     (multiple-value (ecc1 ecc2) (ecc16 head1 ecc1 ecc2))
     (multiple-value (ecc1 ecc2) (ecc16 head2 ecc1 ecc2))
     (as-1 ecc1 buffer-hwd (+ loc 2))
     (as-1 ecc2 buffer-hwd (+ loc 3))
     (setq loc (+ loc 4)))
  ;;VFO Relock - 20. bytes of 1's
  (do i 10. (1- i) (= i 0)
    (as-1 177777 buffer-hwd loc)
    (setq loc (1+ loc)))
  ;;Sync (177) and pad (377)
  (as-1 177577 buffer-hwd loc)
  (setq loc (1+ loc))
  ;;Data field - 1024. bytes of zeros.
  (do i 512. (1- i) (= i 0)
    (as-1 0 buffer-hwd loc)
    (setq loc (1+ loc)))
  ;;Data ecc, doesn't matter anyway, we'll just write zero (which is right for zero)
  (as-1 0 buffer-hwd loc)
  (setq loc (1+ loc))
  (as-1 0 buffer-hwd loc)
  (setq loc (1+ loc))
  ;;Postamble, 44. bytes of 1's
  (do i 22. (1- i) (= i 0)
    (as-1 177777 buffer-hwd loc)
    (setq loc (1+ loc)))
  loc)

;Compute ECC for 16 bits, given previous ecc halfword pair and returning new
(defun ecc16 (hwd ecc1 ecc2)
  (do ((i 16. (1- i))
       (hwd hwd (lsh hwd -1))
       (bit) (poly1) (poly2))
      ((zerop i) (return ecc1 ecc2))
    (setq bit (logxor (logand 1 hwd) (logand 1 ecc1)))  ;ecc.in
    (setq poly1 (* bit 002400)  ;1's in bits 8, 10
	  poly2 (* bit 120020)) ;1's in bits 20, 29, 31
    (setq ecc1 (+ (lsh ecc1 -1)	;Shift double right 1
		  (lsh (logand 1 ecc2) 15.))
	  ecc2 (lsh ecc2 -1))
    (setq ecc1 (logxor ecc1 poly1)
	  ecc2 (logxor ecc2 poly2))))

;Format a track.  Method is call store-sector enough times
;to make most of the 20160. bytes of the track (better to err on the side
;of less than more.)  Then set up a channel program and run the disk
;to write it all out.
(defun format-track (cyl head &aux loc)
  ;;First page is used for channel program
  (setq loc 1000) ;halfwords
  (do blk 0 (1+ blk) (= blk 17.)
    (setq loc (store-sector loc cyl head blk
			    (cond ((< blk 16.) 0) ;next block same track
				  ((< head 4) 1) ;block 0 next track
				  ((< cyl 815.) 2) ;block 0, head 0, next cylinder
				  (t 3)))))    ;end of disk
  (put-buffer) ;ship it over
  ;;Always write 19 pages, somewhat of a crock, should look at loc
  (do i 0 (1+ i) (= i 19.)
    (dbg-write-xbus i (+ (lsh (1+ i) 8)
			 (cond ((= i 18.) 0) (t 1)))))
  ;;Do it
  (dc-exec dc-write-all cyl head 0 0 nil dc-some-error-bits)
  )

(declare (special trklen))
(setq trklen (* 20160. 8))

;This function reads in a track and types out some approximation of what's on it
;If cyl is nil, decode what's in core
(defun decode-track (cyl head &optional (blk 0))
  (cond ((not (null cyl))
	 ;; First, read in 20. blocks, which is more than 20160. bytes
	 (do i 0 (1+ i) (= i 20.)
	   (dbg-write-xbus i (+ (lsh (1+ i) 8)
				(cond ((= i 19.) 0) (t 1)))))  
	 (dc-exec dc-read-all cyl head blk 0 nil dc-some-error-bits)
	 (get-buffer) ;gobble it down from other machine
	 ))
  ;; Map over sectors
  (do ((loc 0)
       (hwd1) (hwd2))
      ((or (> loc trklen) (kbd-tyi-no-hang)))
    (setq loc (decode-sync loc))
    (cond ((< loc trklen)
	   (setq hwd1 (rd-hwd loc)
		 hwd2 (rd-hwd (setq loc (+ loc 20)))
		 loc (+ loc 20))
	   (format t "~%Header: ~O,,~O" hwd2 hwd1)
	   (setq hwd1 (rd-hwd loc)
		 hwd2 (rd-hwd (setq loc (+ loc 20)))
		 loc (+ loc 20))
	   (format t " ... ecc ~O,,~O" hwd2 hwd1)
	   (setq loc (decode-sync loc)) ;VFO relock
	   (format t "~% Pad, data, ecc: ")
	   (decode-bits loc (* 8 1029.))
	   (setq loc (+ loc (* 8 1029.)))))))

;Get a bit out of the buffer, given a bit loc
(defmacro rd-bit (loc)
  `(ar-1 buffer-bit (+ 20000 ,loc)))  ;8K bits of first page skipped

(defmacro wr-bit (loc val)
  `(as-1 ,val buffer-bit (+ 20000 ,loc)))  ;8K bits of first page skipped

;Get a 16-bit halfword, given a bit loc.
(defun rd-hwd (loc)
  (do ((hwd 0 (+ (lsh hwd -1) (lsh (rd-bit (+ loc i)) 15.)))
       (i 0 (1+ i)))
      ((= i 20) hwd)))

;Just type out some bits run-length encoded
(defun decode-bits (loc nbits)
  ;;Do forever, until field exhausted
  (do ((endloc (+ loc nbits)))
      ((>= loc endloc))
    ;;Skip zeros
    (do ((zerc 0 (1+ zerc)))
	((or (>= loc endloc)
	     (not (zerop (rd-bit loc))))
	 (or (zerop zerc)
	     (format t "~D zeros " zerc)))
      (setq loc (1+ loc)))
    ;;Skip ones
    (do ((onec 0 (1+ onec)))
	((or (>= loc endloc)
	     (zerop (rd-bit loc)))
	 (or (zerop onec)
	     (format t "~D ones " onec)))
      (setq loc (1+ loc)))))

;Find a sync, type out 1's and 0's
;A sync is at least 64 1's followed by a 0.
(defun decode-sync (loc)
  (and (zerop (rd-bit loc))		;Skip leading zeros
       (do ((zerc 1 (1+ zerc)))
	   ((or (not (zerop (rd-bit (setq loc (1+ loc)))))
		(> loc trklen))
	    (format t "~%~D zeros" zerc))))
  (do ((onec 1 (1+ onec)))		;Skip ones
      ((or (zerop (rd-bit (setq loc (1+ loc))))
	   (> loc trklen))
       (format t "~%~D ones" onec)
       (cond ((> loc trklen) loc)
	     ((>= onec 64.)
	      (format t " 1 zero")
	      (1+ loc)) ;Skip the zero
	     (t (decode-sync loc))))))

;Simulated ECC errors.
(declare (special rd-all-wrt-all-offset))
(setq rd-all-wrt-all-offset 4)  ;Offset in halfwords

;The method is to read in a whole track, as 20 pages, then shift it down
;in buffer memory by the offset, to compensate for the way the hardware works.
;Next, find a specified bit in the data area of sector 0 and corrupt it.
;Then write the whole track back, as 19 pages.  This destroys sector 16., unfortunately.
;Now, read in sector 0, take the ECC error, and see if it is the correct bit.

;This function gets a track into core and offsets it
(defun read-whole-track (cyl head)
  ;; First, read in 20. blocks, which is more than 20160. bytes
  (do i 0 (1+ i) (= i 20.)
    (dbg-write-xbus i (+ (lsh (1+ i) 8)
			 (cond ((= i 19.) 0) (t 1)))))  
  (dc-exec dc-read-all cyl head 0 0 nil dc-some-error-bits)
  (get-buffer) ;gobble it down from other machine
  ;; Offset the buffer (not the first page)
  (do i (1- 25000) (1- i) (= i 1000)
    (as-1 (cond ((>= (- i rd-all-wrt-all-offset) 1000)
                 (ar-1 buffer-hwd (- i rd-all-wrt-all-offset)))
                (t 177777))
                buffer-hwd i))
  )

;This function writes a track back out
(defun write-whole-track (cyl head)
  (put-buffer) ;ship it over
  ;;Always write 19 pages, somewhat of a crock, should look at loc
  (do i 0 (1+ i) (= i 19.)
    (dbg-write-xbus i (+ (lsh (1+ i) 8)
			 (cond ((= i 18.) 0) (t 1)))))
  ;;Do it
  (dc-exec dc-write-all cyl head 0 0 nil dc-some-error-bits)
  )

;This function finds the start of sector 0 in the buffer and corrupts
;a specified bit in it.
;Find 64 1's, followed by a 0.  Skip 64-bit header, again find 64 1's
;followed by a 0.  Skip 8 bits and you are at the first data bit.
(defun corrupt-bit (bitno)
  (let ((start (+ 8. (find-sync (+ 64. (find-sync 0))))))
    (wr-bit (+ start bitno) (- 1 (rd-bit (+ start bitno))))))

;This function tests 1 bit
(defun test-ecc-1 (cyl head bitno)
  (read-whole-track cyl head)
  (corrupt-bit bitno)
  (write-whole-track cyl head)
  ;; Now read block 0 into page 1
  (dbg-write-xbus 0 400)
  (dc-exec dc-read cyl head 0 0 nil dc-some-error-bits)
  (let ((sts (phys-mem-read-24 dc-sts-adr))
	(ecc-loc (phys-mem-read dc-ecc-adr))
	ecc-pat)
    (setq ecc-pat (ldb 2020 ecc-loc)
	  ecc-loc (ldb 0020 ecc-loc))
    (cond ((not (and (bit-test (lsh 1 15.) sts)	;ECC-SOFT should be set
		     (not (bit-test (lsh 476775 6) sts)))) ;IPE, ECC-HARD, etc. should not be
	   (dc-print-status1 sts))
	  ((zerop ecc-pat) (format t "~%Error, ecc-pat is 0, loc=~O~%" ecc-loc))
	  ((do ((loc ecc-loc (1+ loc))
		(pat ecc-pat (lsh pat -1)))
	       ((oddp pat)
		(cond ((not (= pat 1))
		       (format t "~%Error, more than one bit on in pat, pat=~O, loc=~O~%"
			         pat loc))
		      ((not (= loc bitno))
		       (format t "~%Error, wrong bit number, pat=1, loc=~O~%" loc))
		      (t  ;OK
		       ))))))))

;This function stores specified crud in sector 0
(defun fill-sec0 (cyl head patname)
  (do i 0 (1+ i) (= i 1000)
    (wr-buffer (+ i 1000)
	       (selectq patname
		 (zero 0)
		 (one 177777)
		 (addr i)
		 (caddr (logxor 177777 i))
		 (rot1 (lsh 1 (\ i 20)))
		 (rot0 (logxor 177777 (lsh 1 (\ i 20))))
		 (otherwise (ferror nil "Unknown pattern name ~S, try zero, one, addr, caddr, rot1, or rot0" patname)))))
  (dc-exec dc-write cyl head 0 0 400 dc-all-error-bits))

;This function finds a sync pattern, which is at least 64 1's followed by a 0.
(defun find-sync (loc)
  (and (zerop (rd-bit loc))		;Skip leading zeros
       (do ((zerc 1 (1+ zerc)))
	   ((or (not (zerop (rd-bit (setq loc (1+ loc)))))
		(> loc trklen)))))
  (do ((onec 1 (1+ onec)))		;Skip ones
      ((or (zerop (rd-bit (setq loc (1+ loc))))
	   (> loc trklen))
       (cond ((> loc trklen) loc)
	     ((>= onec 64.)
	      (1+ loc)) ;Skip the zero
	     (t (find-sync loc))))))
