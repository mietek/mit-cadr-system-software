;;; -*- MODE: LISP; PACKAGE: CADR -*-
;;; Diagnostic "monitor"  (will be anyway. pretty simpleminded for now).

(DEFVAR CC-MAIN-MEMORY-SIZE NIL)
  (ADD-INITIALIZATION "clear target machine core size"
		      '(SETQ CC-MAIN-MEMORY-SIZE NIL))

(DEFUN CC-RUN-MTEST (&OPTIONAL ALREADY-LOADED RANGE (MAP-OFFSET 0) &AUX PC CHAR)
       (COND ((AND (NULL CC-MAIN-MEMORY-SIZE)
		   (NULL RANGE))
	      (CC-DISCOVER-MAIN-MEMORY-SIZE)))
       (COND ((NULL ALREADY-LOADED)
	      (CC-ZERO-ENTIRE-MACHINE)
	      (CC-UCODE-LOADER NIL '(MEMD ULOAD DSK LISPM1) NIL)))
       (COND ((ZEROP MAP-OFFSET) (CC-FAST-LOAD-STRAIGHT-MAP))
	     (T (CC-LOAD-STRAIGHT-MAP MAP-OFFSET)))
       (COND ((NULL RANGE)
	      (CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-START 0)
	      (CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-SIZE CC-MAIN-MEMORY-SIZE))
	     (T (CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-START (CAR RANGE))
		(CC-SYMBOLIC-DEPOSIT-REGISTER 'A-MAIN-MEMORY-SIZE (CADR RANGE))))
       (LET ((CC-MODE-REG (+ 44 (LOGAND CC-MODE-REG 3))))  ;SAME SPEED, DISABLE PROM, ENABLE
	 (CC-RESET-MACH)				   ; ERROR STOPS.
	 (DO TEST 0 (1+ TEST) (= TEST 10)
	     (SETQ CC-UPDATE-DISPLAY-FLAG T)
	     (CC-SYMBOLIC-DEPOSIT-REGISTER 'M-TEST TEST)
	     (CC-REGISTER-DEPOSIT RASA (CC-SYMBOLIC-CMEM-ADR  'MEMORY-DATA-TEST))
	     (CC-REGISTER-DEPOSIT RAGO 0)
          L  (COND ((SETQ CHAR (KBD-TYI-NO-HANG))
		    (GO X1))
		   ((ZEROP (CC-REGISTER-EXAMINE RAGO)) (GO X)))
	     (PROCESS-SLEEP 30.)         ;WHY WAIT AS LONG?
	     (GO L)      
	  X1 (COND ((= CHAR #/ )
		    (FORMAT T "~%Aborting test ~D" TEST)
		    (CC-REGISTER-DEPOSIT RASTOP 0)
		    (GO E)))
	  X  (CC-REGISTER-DEPOSIT RASTOP 0)
	     (COND ((NOT (= (SETQ PC (CC-REGISTER-EXAMINE RAPC))
			    (CC-SYMBOLIC-CMEM-ADR 'MEMORY-TEST-OK)))
		    (FORMAT T "~%Test ~D halted at ~S (= ~O) "
			    TEST
			    (CC-FIND-CLOSEST-SYM (+ RACMO PC))
			    PC))
		   (T (FORMAT T "~%Test ~D OK" TEST)))
	   E  )))

(DEFUN CC-SYMBOLIC-CMEM-ADR (SYM)
   (LET ((VAL (CC-LOOKUP-NAME SYM)))
      (COND ((OR (< VAL RACMO)
		 (NOT (< VAL RACME)))
	     (FERROR NIL "The symbol ~s is not a C-MEM symbol" SYM)))
      (- VAL RACMO)))

;DISCOVER THE AMOUNT OF MAIN MEMORY, SETQ THE VARIABLE CC-MAIN-MEMORY-SIZE
(DEFUN CC-DISCOVER-MAIN-MEMORY-SIZE ()
  (SETQ CC-MAIN-MEMORY-SIZE (CC-RETURN-MAIN-MEMORY-SIZE))
  (FORMAT T "~%Main memory ~DK" (// CC-MAIN-MEMORY-SIZE 2000))
)

(defun CC-RETURN-MAIN-MEMORY-SIZE ()
  (do ((ADR 0 (+ ADR 40000))		;memory comes in 16k chunks
       (ZEROS 0)
       (ONES 37777777777)
       (EXIT-FLAG) (WRONG-BITS 0) (TEM))
      (())
    (PHYS-MEM-WRITE ADR ZEROS)
    (IF ( ZEROS (SETQ TEM (PHYS-MEM-READ ADR)))
	(SETQ EXIT-FLAG T WRONG-BITS TEM))
    (PHYS-MEM-WRITE ADR ONES)
    (IF ( ONES (SETQ TEM (PHYS-MEM-READ ADR)))
	(SETQ EXIT-FLAG T WRONG-BITS (LOGIOR WRONG-BITS (LOGXOR ONES TEM))))
    (IF EXIT-FLAG
	(PROGN (IF ( WRONG-BITS ONES)
		   (PROGN (FORMAT T "~%bits that crapped out at memory limit:")
			  (CC-PRINT-BITS WRONG-BITS)))
	       (RETURN ADR)))
    ))

;;; Read and write the sync program
(DEFUN CC-TV-READ-SYNC (ADR &OPTIONAL (TV-ADR 17377760))
	(PHYS-MEM-WRITE (+ TV-ADR 2) ADR)	;Set pointer
	(LOGAND 377 (PHYS-MEM-READ (+ TV-ADR 1))))

(DEFUN CC-TV-WRITE-SYNC (ADR DATA &OPTIONAL (TV-ADR 17377760))
	(PHYS-MEM-WRITE (+ TV-ADR 2) ADR)	;Set pointer
	(PHYS-MEM-WRITE (+ TV-ADR 1) DATA))

;;; Start and stop the sync program
(DEFUN CC-TV-START-SYNC (CLOCK BOW VSP &OPTIONAL (TV-ADR 17377760))
	(PHYS-MEM-WRITE TV-ADR (+ (LSH BOW 2) CLOCK))
	(PHYS-MEM-WRITE (+ TV-ADR 3) (+ 200 VSP)))

(DEFUN CC-TV-STOP-SYNC (&OPTIONAL (TV-ADR 17377760))
	(PHYS-MEM-WRITE (+ TV-ADR 3) 200))		;Disable output of sync

;;; Write into the sync program from a list with repeat-counts
;;; Sub-lists are repeated <car> times.
(DEFUN CC-TV-FILL-SYNC (L &OPTIONAL (ADR 0) (TV-ADR 17377760) &AUX X)
  (DO ((L L (CDR L))) ((NULL L) ADR)
    (SETQ X (CAR L))
    (COND ((ATOM X) (CC-TV-WRITE-SYNC ADR X TV-ADR) (SETQ ADR (1+ ADR)))
	  (T (DO N (CAR X) (1- N) (ZEROP N)
	       (SETQ ADR (CC-TV-FILL-SYNC (CDR X) ADR TV-ADR)))))))

(DEFUN CC-TV-CHECK-SYNC (L &OPTIONAL (ADR 0) (TV-ADR 17377760) &AUX X)
  (DO ((L L (CDR L))) ((NULL L) ADR)
    (SETQ X (CAR L))
    (COND ((ATOM X)
	   (CC-TV-CHECK-SYNC-WD ADR X TV-ADR)
	   (SETQ ADR (1+ ADR)))
	  (T (DO N (CAR X) (1- N) (ZEROP N)
	       (SETQ ADR (CC-TV-CHECK-SYNC (CDR X) ADR TV-ADR)))))))

(DEFUN CC-TV-CHECK-SYNC-WD (ADR DATA TV-ADR &AUX MACH)
  (COND ((NOT (= DATA (SETQ MACH (CC-TV-READ-SYNC ADR TV-ADR))))
	 (FORMAT T "~%ADR:~S MACH: ~S should be ~S" ADR MACH DATA))))

(DECLARE (SPECIAL SI:CPT-SYNC SI:CPT-SYNC1 SI:CPT-SYNC2 SI:COLOR-SYNC))

;;; Set up sync for CPT monitor 768. x 896.
(DEFUN CC-TV-SETUP-CPT (&OPTIONAL (SYNC-PROG SI:CPT-SYNC2) (TV-ADR 17377760))
  (CC-TV-STOP-SYNC TV-ADR)
  (CC-TV-FILL-SYNC SYNC-PROG 0 TV-ADR)
  (CC-TV-START-SYNC 0 1 0 TV-ADR))


(DEFUN CC-TV-CPT-CHECK-SYNC (&OPTIONAL (SYNC-PROG SI:CPT-SYNC2) (TV-ADR 17377760))
  (CC-TV-STOP-SYNC TV-ADR)
  (CC-TV-CHECK-SYNC SYNC-PROG 0 TV-ADR)
  (CC-TV-START-SYNC 0 1 0 TV-ADR))

(SETQ TV-SYNC-ZEROS '( (4096. 0)) )
(SETQ TV-SYNC-ONES  '( (4096. 377)) )

;FAST ADDRESS TEST WRITES ZEROS AND ONES INTO 2 LOCATIONS 
;WHOSE ADDRESSES DIFFER IN 1 BIT, CHECKS FOR INTERFERENCE.
;THIS DETECTS ADDRESS BITS STUCK AT ZERO OR ONE FOR SOME DATA
;BITS, BUT DOES NOT DETECT ADJACENT ADDRESS BITS SHORTED TOGETHER.
(DEFUN CC-FAST-ADDRESS-TEST-SYNC ()
  (CC-TV-STOP-SYNC)
  (DO ((N 2 (1- N))
       (PHASE T NIL)
       (ONES (SUB1 #Q (DPB 1 (+ (LSH 8 6) 1) 0)  #M(EXPT 2 8)))
       (ZEROS 0))
      ((= N 0))
    (DO ((BITNO 0 (1+ BITNO))
	 (GOOD1 (COND (PHASE ZEROS) (T ONES)))
	 (GOOD2 (COND (PHASE ONES) (T ZEROS)))
	 (BAD1)
	 (BAD2)
	 (BAD3)
	 (K)
         (CC-SUSPECT-BIT-LIST))
	((= BITNO 12.))
      (SETQ K (LSH 1 BITNO))
      (CC-TV-WRITE-SYNC K GOOD2)
      (COND ((NOT (EQUAL (SETQ BAD2 (CC-TV-READ-SYNC K)) GOOD2))
	     (PRINC " loc ") (PRIN1 K)
	     (CC-PRINT-BIT-LIST " fails in data bits "
				(CC-WRONG-BITS-LIST GOOD2 BAD2 8))))
      (CC-TV-WRITE-SYNC 0 GOOD1)		;Deposit in loc 0 second for A & M's sake
      (COND ((NOT (EQUAL (SETQ BAD1 (CC-TV-READ-SYNC 0)) GOOD1))
	     (PRINC " loc 0")
	     (CC-PRINT-BIT-LIST " fails in data bits "
				(CC-WRONG-BITS-LIST GOOD1 BAD1 8))))
      (COND ((NOT (EQUAL (SETQ BAD3 (CC-TV-READ-SYNC K)) GOOD2))
	     (PRINC " address bit ") (PRIN1-DECIMAL BITNO)
	     (CC-PRINT-BIT-LIST (COND (PHASE " fails storing 1's then 0 in data bits ")
				      (T " fails storing 0 then 1's in data bits "))
				(CC-WRONG-BITS-LIST GOOD2 BAD3 8)))))))

(defun cc-tv-sync-write-loop (&optional (adr 0) (data -1))
  (do () (())
    (cc-tv-write-sync adr data)))

(DEFUN CC-TV-SYNC-READ-LOOP (&OPTIONAL (ADR 0))
  (DO () (())
    (CC-TV-READ-SYNC ADR)))

(DEFUN CC-TEST-TV-MEMORY (&OPTIONAL ALREADY-LOADED)
  (CC-TV-SETUP-CPT)
  (CC-RUN-MTEST ALREADY-LOADED '(0 100000) (ASH 17000000 -8)))

(defun cc-test-color-tv-memory (&optional already-loaded)
  (cc-tv-setup-cpt color:sync 17377750)
  (cc-run-mtest already-loaded '(0 100000) (ash 17200000 -8)))

(defun cc-tv-read (adr &optional (base-adr 17000000))
  (phys-mem-read (+ adr base-adr)))

(defun cc-tv-write (adr data &optional (base-adr 17000000))
  (phys-mem-write (+ adr base-adr) data))

(defun cc-fill-tv-memory (data)
  (cc-fast-mem-fill 0 100000 data 'load-map-slowly (ash 17000000 -8)))

(defun cc-tv-write-loop (&optional (adr 0) (data -1))
  (do () (())
    (cc-tv-write adr data)))

(defun cc-tv-read-loop (&optional (adr 0))
  (do () (())
    (cc-tv-read adr)))

(DEFUN CC-FAST-ADDRESS-TEST-MEM (&OPTIONAL (MEM-NUMBER 0))
 (DOTIMES (BANK 4)
  (FORMAT T "~%bank ~s" BANK)
  (CC-FAST-ADDRESS-TEST-MAIN-MEM (+ (* MEM-NUMBER 200000)
				    (* BANK 40000))
				 32.
				 14.)))


;FAST ADDRESS TEST WRITES ZEROS AND ONES INTO 2 LOCATIONS 
;WHOSE ADDRESSES DIFFER IN 1 BIT, CHECKS FOR INTERFERENCE.
;THIS DETECTS ADDRESS BITS STUCK AT ZERO OR ONE FOR SOME DATA
;BITS, BUT DOES NOT DETECT ADJACENT ADDRESS BITS SHORTED TOGETHER.
(DEFUN CC-FAST-ADDRESS-TEST-MAIN-MEM (OFFSET N-DATA-BITS N-ADDRESS-BITS)
  (DO ((N 4 (1- N))
       (PHASE 0 (1+ PHASE))
       (ONES (SUB1 (EXPT 2 N-DATA-BITS)))
       (ADR-MASK (1- (EXPT 2 N-ADDRESS-BITS)))
       (ZEROS 0))
      ((= N 0))
    (DO ((BITNO 0 (1+ BITNO))
	 (GOOD1 (COND ((EVENP PHASE) ZEROS) (T ONES)))
	 (GOOD2 (COND ((EVENP PHASE) ONES) (T ZEROS)))
	 (BAD1)
	 (BAD2)
	 (BAD3)
	 (OTHER-LOC)
	 (K)
         (CC-SUSPECT-BIT-LIST))
	((= BITNO N-ADDRESS-BITS))
      (SETQ K (+ OFFSET (COND ((< PHASE 2)
			       (LSH 1 BITNO))
			      (T (LOGXOR ADR-MASK (LSH 1 BITNO))))))
      (SETQ OTHER-LOC (COND ((< PHASE 2) OFFSET)
			    (T (+ OFFSET ADR-MASK))))
      (PHYS-MEM-WRITE K GOOD2)
      (COND ((NOT (EQUAL (SETQ BAD2 (PHYS-MEM-READ K)) GOOD2))
	     (PRINC " loc ") (PRIN1 K)
	     (CC-PRINT-BIT-LIST " fails in data bits "
				(CC-WRONG-BITS-LIST GOOD2 BAD2 N-DATA-BITS))))
      (PHYS-MEM-WRITE OTHER-LOC GOOD1)
      (COND ((NOT (EQUAL (SETQ BAD1 (PHYS-MEM-READ OTHER-LOC)) GOOD1))
	     (PRINC " loc ") (PRIN1 OTHER-LOC)
	     (CC-PRINT-BIT-LIST " fails in data bits "
				(CC-WRONG-BITS-LIST GOOD1 BAD1 N-DATA-BITS))))
      (COND ((NOT (EQUAL (SETQ BAD3 (PHYS-MEM-READ K)) GOOD2))
	     (PRINC " address bit ") (PRIN1-DECIMAL BITNO)
	     (CC-PRINT-BIT-LIST (COND ((EVENP PHASE)
				       " fails storing 1's then 0 in data bits ")
				      (T " fails storing 0 then 1's in data bits "))
				(CC-WRONG-BITS-LIST GOOD2 BAD3 N-DATA-BITS)))))))

;; Fill main memory. Stop via statistics counter.
;; VMA gets loaded with starting address minus one.
;; MD has data.

;; CC-FAST-MEM-FILL should be adjusted in coordination with CC-FAST-LOAD-STRAIGHT-MAP
;; to do the right thing with memory size > 256K

(declare (special CADR:CC-MAIN-MEMORY-SIZE))

(defun CC-FAST-MEM-FILL (&OPTIONAL (FROM 0) (TO 1000000)	; FROM will be auto-adjusted
				   (FILL-DATA 0) 
				   LOAD-STRAIGHT-MAP-SLOWLY-P
				   (MAP-OFFSET 0))
  (cond ((or (not (boundp 'CC-MAIN-MEMORY-SIZE))
	     (null CC-MAIN-MEMORY-SIZE))
	 (setq CC-MAIN-MEMORY-SIZE (cc-return-main-memory-size))))
  (cond ((or (< FROM 0) (< TO FROM)) (ferror nil "Totally unreasonable bounds provided."))
	((and (not LOAD-STRAIGHT-MAP-SLOWLY-P)
	      (>= FROM CC-MAIN-MEMORY-SIZE))
	 (ferror nil "The machine has only ~DK (~O) words of memory. Fill not performed."
		 (// CC-MAIN-MEMORY-SIZE 2000) CC-MAIN-MEMORY-SIZE))
	((and (not LOAD-STRAIGHT-MAP-SLOWLY-P)
	      (>= TO CC-MAIN-MEMORY-SIZE))
	 (setq TO (sub1 CC-MAIN-MEMORY-SIZE))
	 (format T "~%The upper bound has been adjusted to ~O to reflect the main memory size of ~DK."
		 TO (// CC-MAIN-MEMORY-SIZE 2000))))
  (cond (LOAD-STRAIGHT-MAP-SLOWLY-P (cc-load-straight-map MAP-OFFSET))
	(t (cc-fast-load-straight-map)))	;Fast, so we always do it.
  (let ((SAVED-C-MEMORY (cc-multiple-read-c-mem 0 3)))
    (cc-stuff-memory-fill-loop)
    (cc-write-func-dest CONS-FUNC-DEST-VMA (sub1 FROM))
    (cc-write-stat-counter (- FROM TO 1))
    (cc-write-md FILL-DATA)
    (cc-run-test-loop 0)
    (let ((PC-STOP-ADDRESS (cc-read-pc)))
      (cc-multiple-write-c-mem 0 SAVED-C-MEMORY)
      (select PC-STOP-ADDRESS
	(2 (format t "~%Memory fill operation completed.") T)
	(4 (format t "~%Page fault encountered during memory fill operation.  VMA: ~O"
		   (cc-read-m-mem CONS-M-SRC-VMA)))
	(t (format t "~%Unanticipated error during memory fill operation. PC: ~O  VMA: ~O"
		   PC-STOP-ADDRESS (cc-read-m-mem CONS-M-SRC-MD)))))))

;;Microcode loop:

;FILL    ((VMA-START-WRITE) M+1 VMA STAT-BIT)
;        (JUMP-IF-NO-PAGE-FAULT FILL)
;ERROR-BAD-PAGE-FAULT-IN-FILL
;        (JUMP HALT-CONS ERROR-BAD-PAGE-FAULT)

;; MD has value to be stored.
;; VMA has starting address minus 1
;; Depends on loading of statistics counter to number of words
;;	      Saving of C-mem if so desired. [not currently done ?]
;; 	      Preloaded straight map

(defun CC-STUFF-MEMORY-FILL-LOOP ()
     (cc-execute (W-C-MEM 0)
	    cons-ir-stat-bit 1
	    cons-ir-m-src CONS-M-SRC-VMA
	    cons-ir-ob CONS-OB-ALU
	    cons-ir-aluf CONS-ALU-M+1
	    cons-ir-func-dest CONS-FUNC-DEST-VMA-START-WRITE)
     (cc-execute (W-C-MEM 1)
	    cons-ir-op CONS-OP-JUMP
	    cons-ir-jump-cond CONS-JUMP-COND-NO-PAGE-FAULT
	    cons-ir-jump-addr 0
	    cons-ir-n 1)
     (cc-execute (W-C-MEM 2)
	    cons-ir-mf CONS-MF-HALT))

;;; Have the debuggee machine load its own straight map.

;; Stat-counter causes stop, VMA has the data, MD the address.
;; A-A and A-B are used for data increment and address increment respectively
;; They are saved and restored

;; Note that the following are used and defined elsewhere (LCADRD ?)
;;  CONS-VMA-WRITE-LEVEL-1-MAP-BIT
;;  CONS-VMA-LEVEL-1-BYTE
;;  The level 2 map is written from bits 0-23 (0030) of the MD. (Too big for fixnum ops)

(defun CC-FAST-LOAD-STRAIGHT-MAP (&aux SAVED-A-A SAVED-A-B)	;This should take an offset
  (let ((A-A 5) (A-B 6) (PC-STOP-ADDRESS)
	(SAVED-C-MEM (cc-multiple-read-c-mem 0 3)))
    (setq SAVED-A-A (cc-read-a-mem A-A)		;Using A because A + Functional Source op.
	  SAVED-A-B (cc-read-a-mem A-B))
    (cc-stuff-load-straight-map-loop)		;For now just sets up for low 256K
;; FIRST WRITE THE LEVEL 1 MAP
    (cc-write-a-mem A-A (ASH 1 27.))		;map data increment
    (cc-write-a-mem A-B	(ASH 1 13.))		;map address increment
    (cc-write-func-dest CONS-FUNC-DEST-VMA 
			CONS-VMA-WRITE-LEVEL-1-MAP-BIT)
    (cc-write-stat-counter -40)
    (cc-write-md (dpb -1 CONS-VMA-LEVEL-1-BYTE 0))	; in correct field.
    (cc-run-test-loop 0)
    (setq PC-STOP-ADDRESS (cc-read-pc))
    (select PC-STOP-ADDRESS
      (1 (format t "~%Level 1 map loaded for the low 256K addresses.") T)
      (t (cc-multiple-write-c-mem 0 SAVED-C-MEM)
	 (cc-write-a-mem A-A SAVED-A-A) (cc-write-a-mem A-B SAVED-A-B)
	 (ferror nil "~%Level 1 load unsuccessful.   PC: ~O   MD: ~O   VMA: ~O"
		 PC-STOP-ADDRESS (cc-read-m-mem CONS-M-SRC-MD) (cc-read-m-mem CONS-M-SRC-VMA))))
 ;; NOW WRITE THE LEVEL 2 MAP
    (cc-write-a-mem A-A 1)			;map data increment
    (cc-write-a-mem A-B 400)			;map address increment
    (cc-write-func-dest CONS-FUNC-DEST-VMA
			(ash 13 22.))		;Appropriate access bits & loc 0 data.[meta ?]
    (cc-write-stat-counter -1024.)
    (cc-write-md (dpb -1 1020 0))		;Map Loc (0 minus 1)
    (cc-run-test-loop 0)
    (setq PC-STOP-ADDRESS (cc-read-pc))
    (cc-multiple-write-c-mem 0 SAVED-C-MEM)
    (select PC-STOP-ADDRESS
      (1 (format t "~%Level 2 map loaded.") T)
      (t (format t "~%Level 2 load unsuccessful.   PC: ~O   MD: ~O   VMA: ~O"
	      PC-STOP-ADDRESS (cc-read-m-mem CONS-M-SRC-MD) (cc-read-m-mem CONS-M-SRC-VMA))))
    (cc-write-a-mem A-A SAVED-A-A)
    (cc-write-a-mem A-B SAVED-A-B)
    T))

;; Microcode loop for loading maps. (address in MD, data in VMA, stop via STAT. COUNTER)
;; There may be timing problems here, the write might not get finished in time ?
;; The STAT-BIT must go at the end of the loop to insure that the final write gets done.

;LOOP	 ((MD-WRITE-MAP) ADD MD A-B)
;        (JUMP-XCT-NEXT LOOP)
;      ((VMA) ADD VMA A-A STAT-BIT)

(defun CC-STUFF-LOAD-STRAIGHT-MAP-LOOP ()	;For now just sets up for low 256K
     (cc-execute (W-C-MEM 0)
	    cons-ir-aluf CONS-ALU-ADD
	    cons-ir-m-src CONS-M-SRC-MD
	    cons-ir-a-src 6				;A-B has address increment
	    cons-ir-ob CONS-OB-ALU
	    cons-ir-func-dest CONS-FUNC-DEST-MD-WRITE-MAP)
     (cc-execute (W-C-MEM 1)
	    cons-ir-op CONS-OP-JUMP
	    cons-ir-jump-cond CONS-JUMP-COND-UNC
	    cons-ir-jump-addr 0			
	    CONS-IR-n 0)
     (cc-execute (W-C-MEM 2)
	    cons-ir-stat-bit 1
	    cons-ir-aluf CONS-ALU-ADD
	    cons-ir-m-src CONS-M-SRC-VMA
	    cons-ir-a-src 5			 	;A-A has data increment
	    cons-ir-ob CONS-OB-ALU
	    cons-ir-func-dest CONS-FUNC-DEST-VMA))
