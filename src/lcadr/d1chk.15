;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS -*-
;;; This program tests disk#1 in a multi-disk system.
;;; Don't run it on a single disk system, it will clobber disk#0!

(DECLARE (SPECIAL BYPASS-SEEKS))
(SETQ BYPASS-SEEKS NIL) ;NON-NIL INHIBITS SEEKING IN DCHECK

(DECLARE (SPECIAL CADR:CC-SUSPECT-BIT-LIST CADR:CC-DIAG-TRACE DC-RQB DC-UNIT
		  DCHECK-AND DCHECK-IOR DCHECK-ADR-AND DCHECK-ADR-IOR
		  DC-READ DC-WRITE DC-SEEK DC-RECAL DC-FAULT-CLEAR DC-READ-ALL DC-WRITE-ALL
		  DC-READ-COMPARE DC-AT-EASE DC-OFFSET-CLEAR DC-STOP
		  DC-SOME-ERROR-BITS DC-ALL-ERROR-BITS CCW-LOC NXM-LOC1 NXM-LOC2 NXM-LOC3))

(SETQ DC-READ 0 DC-READ-COMPARE 10 DC-WRITE 11 DC-READ-ALL 2 DC-WRITE-ALL 13
      DC-SEEK 4 DC-AT-EASE 5 DC-RECAL 1005 DC-FAULT-CLEAR 405 DC-OFFSET-CLEAR 6
      DC-STOP 16)

(SETQ DC-SOME-ERROR-BITS 06077560  ;MUL-SEL, NO-SEL, FLT, OFF-CYL, OFF-LINE, SEEK-ERR, TIMEOUT,
				   ;START-BLOCK, WRITE-OVERRUN, READ-OVERRUN, PAR, NXM
      DC-ALL-ERROR-BITS 47777560   ;ALSO ECC-SOFT, ECC-HARD, ECC-HDR, HCE, IPE
      CCW-LOC 777
      NXM-LOC1 16777777		 ;THESE 3 ATTEMPT TO GET 1 AND 0 IN ALL BITS
      NXM-LOC2 15000000		 ;ASSUMING MACHINE HAS LESS THAN 1792K CORE
      NXM-LOC3 07000000)

;;; Basic disk manipulation

(OR (BOUNDP'DC-RQB)
    (SETQ DC-UNIT 1
	  DC-RQB (GET-DISK-RQB)))

;; Call this if you haven't just done a command
(DEFUN DC-GET-REGISTERS-INTO-RQB ()
  (DC-RUN DC-RQB DC-UNIT 0 0 0 DC-STOP)) ;Essentially a no-op

;;; This is a modified version of DISK-RUN.  It doesn't do any error handling.
(DEFUN DC-RUN (RQB UNIT CYLINDER SURFACE SECTOR CMD)
  (WIRE-DISK-RQB RQB)
  (ASET CMD RQB %DISK-RQ-COMMAND)
  (ASET (+ (LSH SURFACE 8) SECTOR) RQB %DISK-RQ-SURFACE-SECTOR)
  (ASET (+ (LSH UNIT 12.) CYLINDER) RQB %DISK-RQ-UNIT-CYLINDER)
  (%DISK-OP RQB)
  (UNWIRE-DISK-RQB RQB))

(DEFUN DC-PRINT-STATUS ()
  (DC-GET-REGISTERS-INTO-RQB)
  (DC-PRINT-STATUS1 DC-RQB))

(DEFUN DC-PRINT-STATUS1 (RQB)
  (LET ((SS (AREF RQB %DISK-RQ-FINAL-SURFACE-SECTOR))
	(UC (AREF RQB %DISK-RQ-FINAL-UNIT-CYLINDER)))
    (FORMAT T "~&Unit ~O, Cylinder ~O, Surface ~O, Sector ~O~%"
	      (LDB 1403 UC) (LDB 0014 UC) (LDB 1010 SS) (LDB 0010 SS)))
  (LET ((LOW (AREF RQB %DISK-RQ-STATUS-LOW))
	(HIGH (AREF RQB %DISK-RQ-STATUS-HIGH)))
    (FORMAT T "Sel unit block counter = ~O,"
	    (LDB %%DISK-STATUS-HIGH-BLOCK-COUNTER HIGH))
    (MAPC #'(LAMBDA (PPSS NAME) (AND (LDB-TEST (SYMEVAL PPSS) HIGH)
				     (FORMAT T " ~A," NAME)))
	  '(%%DISK-STATUS-HIGH-INTERNAL-PARITY %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE
	    %%DISK-STATUS-HIGH-NXM %%DISK-STATUS-HIGH-MEM-PARITY %%DISK-STATUS-HIGH-CCW-CYCLE
	    %%DISK-STATUS-HIGH-HEADER-COMPARE %%DISK-STATUS-HIGH-HEADER-ECC
	    %%DISK-STATUS-HIGH-ECC-HARD)
	  '("internal-parity-error" "(read-compare-difference)"
	    "non-existent-memory" "memory-parity-error" "(ccw-cycle)"
	    "header-compare-error" "header-ecc-error" "ecc-hard"))
    (TERPRI)
    (MAPC #'(LAMBDA (PPSS NAME) (AND (LDB-TEST (SYMEVAL PPSS) LOW) 
				     (FORMAT T " ~A," NAME)))
	  '(%%DISK-STATUS-LOW-ECC-SOFT %%DISK-STATUS-LOW-READ-OVERRUN
	    %%DISK-STATUS-LOW-WRITE-OVERRUN %%DISK-STATUS-LOW-START-BLOCK-ERROR
	    %%DISK-STATUS-LOW-TIMEOUT %%DISK-STATUS-LOW-SEEK-ERROR %%DISK-STATUS-LOW-OFF-LINE
	    %%DISK-STATUS-LOW-OFF-CYLINDER %%DISK-STATUS-LOW-READ-ONLY
	    %%DISK-STATUS-LOW-FAULT %%DISK-STATUS-LOW-NO-SELECT
	    %%DISK-STATUS-LOW-MULTIPLE-SELECT %%DISK-STATUS-LOW-INTERRUPT
	    %%DISK-STATUS-LOW-SEL-UNIT-ATTENTION %%DISK-STATUS-LOW-ATTENTION
	    %%DISK-STATUS-LOW-READY)
	  '("ECC-soft" "read-overrun" "write-overrun" "start-block-error"
	    "timeout" "seek-error" "off-line" "off-cylinder" "read-only" "fault" "no-select"
	    "multiple-select" "interrupt" "sel-unit-attention" "attention" "ready"))
    (TERPRI)))

;;; Seek, print status if error
(DEFUN DC-SEEK (CYL)
  (DC-RUN DC-RQB DC-UNIT 0 0 0 DC-AT-EASE)
  (DC-RUN DC-RQB DC-UNIT CYL 0 0 DC-SEEK)
  (DO () ((DC-ATTENTION-P)) ;Await attention
    (KBD-CHAR-AVAILABLE))
  (DC-CHECK-STATUS DC-RQB DC-SOME-ERROR-BITS))

(DEFUN DC-ATTENTION-P ()
  (DC-GET-REGISTERS-INTO-RQB)
  (LDB-TEST %%DISK-STATUS-LOW-SEL-UNIT-ATTENTION (AREF DC-RQB %DISK-RQ-STATUS-LOW)))

;;; Perform a read or write, check specified status bits.
;;; CLP/CCW not currently used, the hacks with that are for testing controller/Xbus anyway
(DEFUN DC-EXEC (CMD CYL HEAD BLOCK CLP CCW ERR-BITS)
  (DC-RUN DC-RQB DC-UNIT 0 0 0 DC-AT-EASE)
  (DC-RUN DC-RQB DC-UNIT CYL HEAD BLOCK CMD)
  (DC-CHECK-STATUS DC-RQB ERR-BITS))

;;; Barf if any of specified bits on in status
(DEFUN DC-CHECK-STATUS (RQB MASK)
  (LET ((VAL (DPB (AREF RQB %DISK-RQ-STATUS-HIGH)
		  2020 (AREF RQB %DISK-RQ-STATUS-LOW))))
    (COND ((NOT (ZEROP (LOGAND MASK VAL)))
	   (DC-PRINT-STATUS1 RQB)))))

;;; Test function
;;; Cut down from the one in LMCONS;DCHECK, here we have only drive tests

(DEFUN DCHECK ()
  ;; Part 4 - Test disk bus bits and basic command logic by seeking
  (COND ((NOT BYPASS-SEEKS)
	 (DCHECK-SEEK 814.)
	 (DO I 512. (LSH I -1) (ZEROP I)
	   (DCHECK-SEEK I))))
  ;; Part 6 - Write and read block 1 of the disk.  Use a floating 1's and 0's
  ;;          pattern, and then an address pattern, and check for Xbus data path
  ;;	      and addressing failures.
  ;; This doesn't check high-order address bits
  (LET ((BUF (ARRAY-LEADER DC-RQB %DISK-RQ-LEADER-BUFFER)) ;ART-16B buffer page
	(CADR:CC-SUSPECT-BIT-LIST NIL)) ;used by CADR:CC-PRINT-BIT-LIST
    ;; Words 0-37 get floating 1's
    (DOTIMES (I 20)
      (ASET (LSH 1 I) BUF (* I 2))
      (ASET 0 BUF (1+ (* I 2))))
    (DOTIMES (I 20)
      (ASET 0 BUF (+ (* I 2) 40))
      (ASET (LSH 1 I) BUF (+ (* I 2) 41)))
    ;; Words 40-77 get floating 0's
    (DOTIMES (I 20)
      (ASET (LOGXOR 177777 (LSH 1 I)) BUF (+ (* I 2) 100))
      (ASET 177777 BUF (+ (* I 2) 101)))
    (DOTIMES (I 20)
      (ASET 177777 BUF (+ (* I 2) 140))
      (ASET (LOGXOR 177777 (LSH 1 I)) BUF (+ (* I 2) 141)))
    ;; Words 100-377 get address pattern
    (DO I 100 (1+ I) (= I 400)
      (ASET (+ (LSH (LOGXOR 377 I) 8) I) BUF (* I 2))
      (ASET 0 BUF (1+ (* I 2))))
    (PRINT 'WRITE)
    (DC-EXEC DC-WRITE 0 0 1 CCW-LOC 0 DC-ALL-ERROR-BITS)
    (DO I 0 (1+ I) (= I 1000)	;Clear buffer
      (ASET 0 BUF I))
    (PRINT 'READ)
    (DC-EXEC DC-READ 0 0 1 CCW-LOC 0 DC-ALL-ERROR-BITS)
    ;; Check pattern read back into core, see if it's correct
    (LET ((DCHECK-AND 37777777777) (DCHECK-IOR 0)  ;Accumulate error bits here
	  (DCHECK-ADR-AND 377) (DCHECK-ADR-IOR 0))
      (DO I 0 (1+ I) (= I 40)	;Loc 0-37 get floating 1's
	(DCHECK-COMPARE I (ASH 1 I)))
      (DO I 0 (1+ I) (= I 40)	;Loc 40-77 get floating 0's
	(DCHECK-COMPARE (+ 40 I) (- (ASH 1 32.) 1 (ASH 1 I))))
      (DO I 100 (1+ I) (= I 400)	;Loc 100-377 get address pattern
	(DCHECK-COMPARE I (+ (LSH (LOGXOR 377 I) 8) I)))
      (DCHECK-PM '|Data bits dropped during write to or read from disk: |
		 (LOGXOR 37777777777 DCHECK-IOR))
      (DCHECK-PM '|Data bits picked during write to or read from disk: |
		 DCHECK-AND)
      (DCHECK-PM '|Address bits 0 with bad data during write to or read from disk: |
		 (LOGXOR 377 DCHECK-ADR-AND))
      (DCHECK-PM '|Address bits 1 with bad data during write to or read from disk: |
		 DCHECK-ADR-IOR)))
    ;; Maybe there should be a test-loop for the above?
  ;; Part 7 - end
  (TERPRI)
  (PRINC '|End of DCHECK.  Now run the format program and the ECC test program.|))

;;; Compare pattern, set special variables if lose
;;; Also obeys CC-DIAG-TRACE
(DEFUN DCHECK-COMPARE (ADR VAL)
  (LET ((BUF (ARRAY-LEADER DC-RQB %DISK-RQ-LEADER-BUFFER))) ;ART-16B buffer page
    (LET ((MASK (DPB (AREF BUF (1+ (* ADR 2))) 2020 (AREF BUF (* 2 ADR)))))
      (SETQ DCHECK-AND (LOGAND DCHECK-AND MASK)
	    DCHECK-IOR (LOGIOR DCHECK-IOR MASK))
      (COND ((NOT (= MASK VAL))
	     (PRINT (LIST ADR VAL MASK))
	     (SETQ DCHECK-ADR-AND (LOGAND DCHECK-ADR-AND ADR)
		   DCHECK-ADR-IOR (LOGIOR DCHECK-ADR-IOR ADR))))
      NIL)))

;;; Print bit list given as mask
(DEFUN DCHECK-PM (MESSAGE MASK)
  (OR (ZEROP MASK)
      (CADR:CC-PRINT-BIT-LIST MESSAGE
			      (DO ((BITNO 0 (1+ BITNO))
				   (L NIL))
				  ((ZEROP MASK) L)
				(AND (ODDP MASK) (SETQ L (CONS BITNO L)))
				(SETQ MASK (ASH MASK -1))))))

;;; Alternating seek test
(DEFUN DCHECK-SEEK (CYL)
  (TERPRI)
  (PRINC '|Should be seeking between cylinders 0 and |)
  (LET ((BASE 10.) (*NOPOINT NIL))
    (PRIN1 CYL))
  (PRINC '| - type space when OK. |)
  (DO () ((KBD-TYI-NO-HANG))
    (DC-SEEK 0)
    (DC-SEEK CYL))
  (TERPRI))

;;; Read/Write test

(declare (special dc-write-read-trace))
(setq dc-write-read-trace nil)

;;; Low-level routine, does a write and a read and compares
(defun dc-write-read-test-0 (cyl head blk pattern-func)
  ;; Trace
  (and dc-write-read-trace
       (format t "~%WRITE-READ-TEST: cyl=~O, head=~O, blk=~O, pattern=~A"
	         cyl head blk pattern-func))
  ;; Fill memory with pattern
  (do i 0 (1+ i) (= i 400)
    (let ((pat (funcall pattern-func i)))
      (aset (ldb 0020 pat) (array-leader dc-rqb %disk-rq-leader-buffer) (* i 2))
      (aset (ldb 2020 pat) (array-leader dc-rqb %disk-rq-leader-buffer) (1+ (* i 2)))))
  ;; Write it out
  (dc-exec dc-write cyl head blk 777 0 dc-all-error-bits)
  ;; Read it back
  (dc-exec dc-read cyl head blk 777 0 dc-all-error-bits)  
  ;; Check pattern
  (do ((i 0 (1+ i))
       (good) (bad) (heading-printed nil))
      ((= i 400))
    (setq good (funcall pattern-func i)
	  bad (dpb (aref (array-leader dc-rqb %disk-rq-leader-buffer) (1+ (* i 2)))
		   2020 (aref (array-leader dc-rqb %disk-rq-leader-buffer) (* i 2))))
    (cond ((not (= good bad))
	   (cond ((not heading-printed)
		  (format t "~% Compare error for ~A pattern, cyl ~O, head ~O, blk ~O:~%Loc    Good      Bad"
			    pattern-func cyl head blk)
		  (setq heading-printed t)))
	   (format t "~%~3O  ~8O ~8O" i good bad)))))

;;; Patterns for above
(defun all-zero-pat (loc) 0)
(defun all-one-pat (loc) 37777777777)
(defun alt-bits-pat (loc) 25252525252)
(defun addr-pat (loc) (+ (lsh (logxor 377 loc) 8) loc))
(defun floating-one-pat (loc) (dpb 1 (1+ (lsh (\ loc 40) 6)) 0))
(defun floating-zero-pat (loc) (dpb 0 (1+ (lsh (\ loc 40) 6)) 37777777777))
(declare (special gubbish))
(setq gubbish 7700770066)
(defun gubbish-pat (loc) gubbish)

;;; An address specifier is a single number, a list of cases,
;;; or a list of DO, first, last, optional increment,
;;; or (on typein) ALL which translates into such.
;;; We cons current state onto the front
;;; First value is next value output from spec, second value is T if wrapped around
(defun dc-step-addr-spec (frob)
  (prog ((current (car frob)) (spec (cdr frob)) (wrap-p nil))
    (cond ((atom spec) (setq current spec wrap-p t))
	  ((not (eq (car spec) 'do)) ;Cases list
	   (and (null current) (setq current 0))
	   (and (>= current (length spec)) (setq current 0))
	   (return (nth current spec) (rplaca frob (1+ current))))
	  ((null current) (setq current (cadr spec)))
	  (t (setq current (+ current (or (cadddr spec) 1)))
	     (and (>= current (caddr spec)) (setq current (cadr spec)
						  wrap-p t))))
    (rplaca frob current)
    (return current wrap-p)))

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

(defun dc-get-addr-spec (prompt all)
  (format t "~% ~A:" prompt)
  (let ((spec (read)))
    (and (eq spec 'all) (setq spec all))
    (cons nil spec)))

;;; User interface to write-read test
;;; This version is kludged up, you should step only one addr at a time!
(defun dc-write-read-test ()
  (let ((cyl (dc-get-addr-spec '|Cylinders| '(do 0 815.)))
	(head (dc-get-addr-spec '|Heads| '(do 0 5)))
	(blk (dc-get-addr-spec '|Blocks (sectors)| '(do 0 17.)))
	(pattern-func (dc-get-addr-spec '|Pattern func|
					'(all-zero-pat all-one-pat alt-bits-pat
					  addr-pat floating-one-pat floating-zero-pat
					  gubbish-pat))))
    (do () ((kbd-char-available))
      (apply 'dc-write-read-test-0
	     (dc-step-addr-specs (list pattern-func blk head cyl))))))

;;; Formatting.

;A track is a sequence of blocks, each block has:
;	61. bytes of 1's (preamble)
;	Sync (a 177 byte)
;	Header word, header ecc word
;	20. bytes of 1's (VFO relock)
;	Sync (177) and Pad (377)
;	1024. bytes of zero data
;	4 bytes of zero ecc
;	44. bytes of 1's (postamble)
;A header is 8 bits of block, 8 bits of head, 12 bits of cylinder, 2 bits of
;zero, and 2 bits which are: 0 normal, 1 end track, 2 end cylinder, 3 end disk

(DEFUN FORMAT-DISK (UNIT T-300-P)
  (AND (ZEROP UNIT) (FERROR NIL "You don't want to do that."))
  (LET ((N-BLOCKS 17.)
	(N-HEADS (IF T-300-P 19. 5))
	(N-CYLS 815.)
	RQB1 RQB2 BUF1 BUF2 (IDX -1))
    (UNWIND-PROTECT
     (PROGN (SETQ RQB1 (GET-DISK-RQB 19.)
		  RQB2 (GET-DISK-RQB (* N-BLOCKS 5)))
	    (WIRE-DISK-RQB RQB1)
	    (WIRE-DISK-RQB RQB2)
	    (SETQ BUF1 (ARRAY-LEADER RQB1 %DISK-RQ-LEADER-BUFFER)
		  BUF2 (ARRAY-LEADER RQB2 %DISK-RQ-LEADER-BUFFER))
	    ;; Fill BUF2 with zeros for zeroing cylinders and BUF1 with prototype track
	    (DOTIMES (I (ARRAY-LENGTH BUF1))
	      (ASET 0 BUF1 I))
	    (DOTIMES (I (ARRAY-LENGTH BUF2))
	      (ASET 0 BUF2 I))
	    (DOTIMES (BLOCK N-BLOCKS)
	      ;; 60. bytes of 1's
	      (DOTIMES (N 30.)
		(ASET -1 BUF1 (SETQ IDX (1+ IDX))))
	      ;; Last preamble byte and sync byte
	      (ASET 77777 BUF1 (SETQ IDX (1+ IDX)))
	      ;; Skip 8 bytes for header and ecc
	      (SETQ IDX (+ 4 IDX))
	      ;; 20. bytes of 1's
	      (DOTIMES (N 10.)
		(ASET -1 BUF1 (SETQ IDX (1+ IDX))))
	      ;; Sync and pad
	      (ASET 177577 BUF1 (SETQ IDX (1+ IDX)))
	      (AND (= BLOCK (1- N-BLOCKS)) (RETURN NIL)) ;World's biggest kludge!
	      ;; 1024. bytes of zero, 4 bytes of ecc
	      (SETQ IDX (+ 514. IDX))
	      ;; 44. bytes of 1's
	      (DOTIMES (N 22.)
		(ASET -1 BUF1 (SETQ IDX (1+ IDX)))))
	    (DOTIMES (CYL N-CYLS)
	      (FORMAT T "~D " CYL)
	      (DOTIMES (HEAD N-HEADS)
		;; Fill in the headers in the prototype track
		(DO ((BLOCK 0 (1+ BLOCK))
		     (IDX 31. (+ IDX 582.))
		     (HDR)(ECC))
		    ((= BLOCK N-BLOCKS))
		  (SETQ HDR
			(DPB (COND ((< BLOCK (1- N-BLOCKS)) 0)
				   ((< HEAD (1- N-HEADS)) 1)
				   ((< CYL (1- N-CYLS)) 2)
				   (T 3))
			     3602
			     (DPB CYL 2014 (DPB HEAD 1010 BLOCK))))
		  (SETQ ECC (COMPUTE-ECC HDR))
		  (ASET (LDB 0020 HDR) BUF1 IDX)
		  (ASET (LDB 2020 HDR) BUF1 (1+ IDX))
		  (ASET (LDB 0020 ECC) BUF1 (+ IDX 2))
		  (ASET (LDB 2020 ECC) BUF1 (+ IDX 3)))
		;; Write the track
		(DC-RUN RQB1 UNIT CYL HEAD 0 DC-WRITE-ALL)
		(DC-CHECK-STATUS RQB1 DC-SOME-ERROR-BITS))
	      ;; Zero the cylinder
	      (DC-RUN RQB2 UNIT CYL 0 0 DC-WRITE)
	      (DC-CHECK-STATUS RQB2 DC-ALL-ERROR-BITS)
	      ;; That got the first 5 heads, if it's a T-300 get the rest.
	      (DO I 4. (+ I 5) (>= I N-HEADS)
		(DC-RUN RQB2 UNIT CYL I 0 DC-WRITE)
		(DC-CHECK-STATUS RQB2 DC-ALL-ERROR-BITS))))
     ;; Unwind protect forms
     (RETURN-DISK-RQB RQB1)
     (RETURN-DISK-RQB RQB2))))

(DEFUN COMPUTE-ECC (WD)
  (DO ((COUNT 32. (1- COUNT))
       (PPSS 0001 (+ 100 PPSS))
       (BIT)(ECC 0))
      ((ZEROP COUNT) ECC)
    (SETQ BIT (LOGXOR (LDB PPSS WD) (LDB 0001 ECC)))
    (SETQ ECC (// ECC 2))
    (OR (ZEROP BIT) (SETQ ECC (LOGXOR ECC 24004002400)))))
