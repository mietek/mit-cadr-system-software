;-*- Mode:LISP; Package:SI -*-

;;; Printing Peek
;;; For when I just want some information, and not a hard time

(GLOBALIZE 'PPK)

(DEFVAR PPK #/%)

(DEFUN PPK (&OPTIONAL (MODE PPK))
  (SETQ MODE (CHAR-UPCASE (CHARACTER MODE)))
  (COND ((MEMQ MODE '(#/P #/% #/C))		;All modes must be listed here
	 (SETQ PPK MODE)			;Change default
	 (SELECTQ MODE
	   (#/% (DOLIST (CTR SYS:A-MEMORY-COUNTER-BLOCK-NAMES)
		  (FORMAT T "~&~50,1,1,'.<~A~; ~D.~>" CTR (READ-METER CTR))))
	   (#/P (DOLIST (P ACTIVE-PROCESSES)
		  (AND (SETQ P (CAR P))
		       (FORMAT T "~&~39,1,1,'.A ~A" (PROCESS-NAME P) (PROCESS-WHOSTATE P)))))
	   (#/C (DOLIST (C CHAOS:CONN-LIST)	;Simplistic
		  (FORMAT T "~&~S ~A to host ~A, local idx ~O foreign idx ~O~%"
			    C (CHAOS:STATE C) (CHAOS:HOST-DATA (CHAOS:FOREIGN-ADDRESS C))
			    (CHAOS:LOCAL-INDEX-NUM C) (CHAOS:FOREIGN-INDEX-NUM C))
		  (FORMAT T "   Packet number rcved ~D, read ~D, acked ~D, queued ~D~%"
			    (CHAOS:PKT-NUM-RECEIVED C) (CHAOS:PKT-NUM-READ C)
			    (CHAOS:PKT-NUM-ACKED C)
			    (- (CHAOS:PKT-NUM-RECEIVED C) (CHAOS:PKT-NUM-READ C)))
		  (FORMAT T "   Window available ~D, sent ~D, acked ~D, queued ~D"
			    (CHAOS:WINDOW-AVAILABLE C) (CHAOS:PKT-NUM-SENT C)
			    (CHAOS:SEND-PKT-ACKED C) (CHAOS:SEND-PKTS-LENGTH C))))
	   ))
	(T					;Invalid arg, give help, don't change default
	 (FORMAT T "~&% -- system meters.  P -- processes.  C -- Chaosnet."))))

(DEFUN P-BIGNUM (ADR)
  (DPB (%P-LDB 2020 ADR) 2020 (%P-LDB 0020 ADR)))

; This will get hairier later, e.g. check for wrap around
; Also this only understands the Trident controller I guess
(DEFUN PRINT-DISK-ERROR-LOG ()
  (FORMAT T "~&Disk error count ~D.~%" (READ-METER 'SYS:%COUNT-DISK-ERRORS))
  (DO I 600 (+ I 4) (= I 640)
    (LET ((CLP-CMD (P-BIGNUM I))
	  (DA (P-BIGNUM (1+ I)))
	  (STS (P-BIGNUM (+ I 2)))
	  (MA (P-BIGNUM (+ I 3))))
      (COND ((NOT (ZEROP CLP-CMD))
	     (FORMAT T "~%Command ~O ~@[(~A) ~]"
		       (LDB 0020 CLP-CMD)
		       (CDR (ASSQ (LDB 0004 CLP-CMD) '((0 . "Read")
						       (10 . "Read-Compare")
						       (11 . "Write")))))
	     (AND (BIT-TEST %DISK-COMMAND-DATA-STROBE-EARLY CLP-CMD)
		  (PRINC "Data-Strobe-Early "))
	     (AND (BIT-TEST %DISK-COMMAND-DATA-STROBE-LATE CLP-CMD)
		  (PRINC "Data-Strobe-Late "))
	     (AND (BIT-TEST %DISK-COMMAND-SERVO-OFFSET CLP-CMD)
		  (PRINC "Servo-offset "))
	     (AND (BIT-TEST %DISK-COMMAND-SERVO-OFFSET-FORWARD CLP-CMD)
		  (PRINC "S-O-Forward "))
	     (TERPRI)
	     (FORMAT T "CCW-list pointer ~O (low 16 bits)~%" (LDB 2020 CLP-CMD))
	     (FORMAT T "Disk address: unit ~O, cylinder ~O, head ~O, block ~O (~4:*~D ~D ~D ~D decimal)~%"
		       (LDB 3404 DA) (LDB 2014 DA) (LDB 1010 DA) (LDB 0010 DA))
	     (FORMAT T "Memory address: ~O (type bits ~O)~%"
		       (LDB 0026 MA) (LDB 2602 MA))
	     (FORMAT T "Status: ~O" STS)
	     (DO ((PPSS 2701 (- PPSS 100))
		  (L '("Internal-parity" "Read-compare" "CCW-cycle" "NXM" "Mem-parity"
		       "Header-Compare" "Header-ECC" "ECC-Hard" "ECC-Soft"
		       "Overrun" "Transfer-Aborted (or wr. ovr.)" "Start-Block-Error"
		       "Timeout" "Seek-Error" "Off-Line" "Off-Cylinder"
		       "Read-Only" "Fault" "No-Select" "Multiple-Select"
		       "Interrupt" "Sel-Unit-Attention" "Any-Unit-Attention" "Idle")
		     (CDR L)))
		 ((MINUSP PPSS) (TERPRI))
	       (AND (LDB-TEST PPSS STS) (FORMAT T "~<~%~8X~:;  ~A~>" (CAR L)))))))))

;Paging and other LM life-function monitoring stuff

(LOAD "LMFONT;MOUSE") ;New version
(LOAD "LMFONT;TVFONT") ;Not loaded by default

(DEFUN SB-DIAL-FACE (X Y NAME HEIGHT RANGE SHEET N-TICKS)
  (MULTIPLE-VALUE-BIND (CX CY) (TV:SHEET-READ-CURSORPOS SHEET)
    (TV:%DRAW-RECTANGLE 100 (+ HEIGHT 44) (- X 40) Y TV:ALU-ANDCA SHEET)
    (DO ((TICK-INTERVAL (// HEIGHT N-TICKS))
	 (YY (+ Y 10) (+ YY TICK-INTERVAL))
	 (VAL (* N-TICKS (// RANGE N-TICKS)) (- VAL (// RANGE N-TICKS)))
	 (N N-TICKS (1- N)))
	((MINUSP N)
	 (TV:%DRAW-RECTANGLE 1 (* TICK-INTERVAL N-TICKS) X (+ Y 10) TV:ALU-IOR SHEET))
      (TV:%DRAW-RECTANGLE 20 1 (- X 10) YY TV:ALU-IOR SHEET)
      (TV:SHEET-STRING-OUT-EXPLICIT SHEET (FORMAT NIL "~D" VAL)
				    (- X 34) (- YY 4) (TV:SHEET-INSIDE-RIGHT SHEET)
				    FONTS:TVFONT TV:ALU-IOR))
    (TV:SHEET-DISPLAY-CENTERED-STRING SHEET NAME (- X 40) (+ X 40) (+ Y HEIGHT 24))
    (TV:SHEET-SET-CURSORPOS SHEET CX CY)))

(DEFUN SB-DIAL-UPDATE (X Y VAL1 VAL2 VAL3 HEIGHT RANGE SHEET)
  (SB-DIAL-UPDATE1 X Y VAL1 "" HEIGHT RANGE SHEET)
  (SB-DIAL-UPDATE1 X Y VAL2 "" HEIGHT RANGE SHEET)
  (SB-DIAL-UPDATE1 X Y VAL3 "" HEIGHT RANGE SHEET))

;I wonder why this gets completely fucked if the window is de-exposed for a while?
(DEFUN SB-DIAL-UPDATE1 (X Y VAL STR HEIGHT RANGE SHEET)
  (SETQ VAL (MAX 0 (MIN RANGE VAL)))
  (TV:SHEET-STRING-OUT-EXPLICIT
	SHEET STR (+ X 2) (+ Y (FIX (// (* (- RANGE VAL) HEIGHT) RANGE)))
	(TV:SHEET-INSIDE-RIGHT SHEET) FONTS:MOUSE TV:ALU-XOR))

;;; Watch this guy with averaging intervals of 1, 5, 25 seconds
(DEFUN SB-METER-FORM (FORM LABEL RANGE
		      &OPTIONAL (N-TICKS 10.) (X 100) (Y 40) (HEIGHT 300) (SHEET TERMINAL-IO)
		      &AUX (RATIO2 0.870s0) (RATIO3 0.973s0) (CORR2 7.72s0) (CORR3 36.70s0)
			   VAL1 VAL2 VAL3 READING1 READING2 TIME1 TIME2)
  (AND (MEMQ FORM SYS:A-MEMORY-COUNTER-BLOCK-NAMES) ;kludge
       (SETQ FORM `(READ-METER ',FORM)))
  (SB-DIAL-FACE X Y LABEL HEIGHT RANGE SHEET N-TICKS)
  ;; Get an initial guess at the rate, and initialize everything
  (SETQ READING1 (EVAL FORM) TIME1 (TIME))
  (PROCESS-SLEEP 60.)
  (SETQ READING2 (EVAL FORM) TIME2 (TIME))
  (SETQ VAL1 (* (- READING2 READING1) (// 60.0s0 (TIME-DIFFERENCE TIME2 TIME1)))
	VAL2 (* VAL1 CORR2)
	VAL3 (* VAL1 CORR3))
  (DO () ((FUNCALL TERMINAL-IO ':TYI-NO-HANG))
    (SB-DIAL-UPDATE X Y VAL1 (// VAL2 CORR2) (// VAL3 CORR3) HEIGHT RANGE SHEET)
    (SETQ READING1 READING2 TIME1 TIME2)
    (PROCESS-SLEEP 60.)
    (SETQ READING2 (EVAL FORM) TIME2 (TIME))
    (SB-DIAL-UPDATE X Y VAL1 (// VAL2 CORR2) (// VAL3 CORR3) HEIGHT RANGE SHEET) ;undisplay
    (SETQ VAL1 (* (- READING2 READING1) (// 60.0s0 (TIME-DIFFERENCE TIME2 TIME1)))
	  VAL2 (+ (* VAL2 RATIO2) VAL1)
	  VAL3 (+ (* VAL3 RATIO3) VAL1))))

(DEFSTRUCT (METER-DESC :LIST)
  METER-NAME METER-LABEL METER-RANGE METER-N-TICKS METER-VAL1 METER-VAL2 METER-VAL3)

(DEFUN SB-BUNCHA-METERS (DESCL &OPTIONAL (HEIGHT 300) (X-SPACING 100) (X 60) (Y 40)
					 (SHEET TERMINAL-IO)
			 &AUX (OLDVALS (MAKE-LIST NIL (LENGTH DESCL)))
			      (NEWVALS (MAKE-LIST NIL (LENGTH DESCL)))
			      TIME1 TIME2 TCORR
			      (RATIO2 0.870s0) (RATIO3 0.973s0) (CORR2 7.72s0) (CORR3 36.70s0))
  ;; First draw all the dial faces
  (DO ((L DESCL (CDR L)) (X X (+ X X-SPACING))) ((NULL L))
    (SB-DIAL-FACE X Y (METER-LABEL (CAR L)) HEIGHT
		  (METER-RANGE (CAR L)) SHEET (METER-N-TICKS (CAR L))))
  ;; Now get all the initial values and rates
  (DO ((L DESCL (CDR L)) (V OLDVALS (CDR V))) ((NULL L))
    (RPLACA V (READ-METER (METER-NAME (CAR L)))))
  (SETQ TIME1 (TIME))
  (PROCESS-SLEEP 60.)
  (SETQ TIME2 (TIME) TCORR (// 60.0s0 (TIME-DIFFERENCE TIME2 TIME1)))
  (DO ((L DESCL (CDR L)) (OV OLDVALS (CDR OV)) (NV NEWVALS (CDR NV))) ((NULL L))
    (RPLACA NV (READ-METER (METER-NAME (CAR L))))
    (LET ((R (* (- (CAR NV) (CAR OV)) TCORR)))
      (SETF (METER-VAL1 (CAR L)) R)
      (SETF (METER-VAL2 (CAR L)) (* R CORR2))
      (SETF (METER-VAL3 (CAR L)) (* R CORR3))))
  ;; Now enter the redisplay loop
  (DO () ((FUNCALL TERMINAL-IO ':TYI-NO-HANG))
    (DO ((L DESCL (CDR L)) (X X (+ X X-SPACING))) ((NULL L))
      (SB-DIAL-UPDATE X Y (METER-VAL1 (CAR L)) (// (METER-VAL2 (CAR L)) CORR2)
		      (// (METER-VAL3 (CAR L)) CORR3) HEIGHT (METER-RANGE (CAR L)) SHEET))
    (PROCESS-SLEEP 60.)
    (DO ((L DESCL (CDR L)) (X X (+ X X-SPACING))) ((NULL L))
      (SB-DIAL-UPDATE X Y (METER-VAL1 (CAR L)) (// (METER-VAL2 (CAR L)) CORR2)
		      (// (METER-VAL3 (CAR L)) CORR3) HEIGHT (METER-RANGE (CAR L)) SHEET))
    (SETQ TIME1 TIME2 TIME2 (TIME) TCORR (// 60.0s0 (TIME-DIFFERENCE TIME2 TIME1)))
    (DO ((L DESCL (CDR L)) (OV OLDVALS (CDR OV)) (NV NEWVALS (CDR NV))) ((NULL L))
      (RPLACA OV (CAR NV))
      (RPLACA NV (READ-METER (METER-NAME (CAR L))))
      (LET ((R (* (- (CAR NV) (CAR OV)) TCORR)))
	(SETF (METER-VAL1 (CAR L)) R)
	(SETF (METER-VAL2 (CAR L)) (+ (* (METER-VAL2 (CAR L)) RATIO2) R))
	(SETF (METER-VAL3 (CAR L)) (+ (* (METER-VAL3 (CAR L)) RATIO3) R))))))

(DEFVAR SB-PAGE-METERS
   '( (%COUNT-DISK-PAGE-READS "Pg Rd" 100. 10. 0 0 0)
      (%COUNT-DISK-PAGE-WRITES "Pg Wr" 100. 10. 0 0 0)))


