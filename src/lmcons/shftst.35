;;; CADR SHIFTER TEST                                           -*-LISP-*-

(INCLUDE |LMDOC;.COMPL PRELUD|)
;(DECLARE (EVAL (READ)))
;(PROGN (LOAD '(MACROS > DSK LISPM))
;       (LOAD '(DEFMAC FASL DSK LISPM2))
;       (LOAD '(LMMAC > DSK LISPM2)))

(IF-FOR-MACLISP (DECLARE (EVAL (READ))))
(IF-FOR-MACLISP (DEFUN **STRING** MACRO (X) `',(CADR X)) ;Bubbles in my brain
)

(IF-FOR-MACLISP (INCLUDE ((LMCONS)CADMAC >)))

(DECLARE (FIXNUM SPY-IR-LOW (SPY-READ FIXNUM))
	 (NOTYPE (SPY-WRITE FIXNUM FIXNUM))
	 (SPECIAL SPY-IR-LOW)
	 (*EXPR SPY-READ SPY-WRITE))

(DEFMACRO ADD2L (ITEM LIST)
  `(OR (NUMERIC-LIST-MEMQ ,ITEM ,LIST)
       (SETQ ,LIST (CONS ,ITEM ,LIST))))

(DECLARE (FIXNUM (ROT32 FIXNUM FIXNUM)))

(IF-FOR-MACLISP
(DEFMACRO 32-LOGXOR (X Y) `(LOGXOR ,X ,Y)) )
(IF-FOR-MACLISP
(DEFMACRO 32-LOGAND (X Y) `(LOGAND ,X ,Y)) )

(IF-FOR-LISPM
(DEFMACRO 32-LOGXOR (X Y) `(+ (ASH (LOGXOR (LDB 2020 ,X) (LDB 2020 ,Y)) 20)
                              (LOGXOR (LDB 0020 ,X) (LDB 0020 ,Y)))) )
(IF-FOR-LISPM
(DEFMACRO 32-LOGAND (X Y) `(+ (ASH (LOGAND (LDB 2020 ,X) (LDB 2020 ,Y)) 20)
                              (LOGAND (LDB 0020 ,X) (LDB 0020 ,Y)))) )

(IF-FOR-MACLISP
(DEFUN ROT32 (NUM AMT)  ;FIXNUM VERSION OF 32-BIT ROTATE ROUTINE (ONLY ROTATES LEFT)
  (LOGAND 37777777777 (+ (LSH NUM AMT) (LOGAND (1- (LSH 1 AMT))
                                               (ROT NUM (+ 4 AMT)))))) )

(IF-FOR-LISPM
(DEFUN ROT32 (NUM AMT)
  (32-LOGAND 37777777777
             (COND ((< AMT 30) (+ (ASH NUM AMT) (LDB (+ (LSH (- 40 AMT) 6) AMT) NUM)))
                   (T (DPB (LDB (- 40 AMT) NUM)
                           (+ (LSH AMT 6) (- 40 AMT))
                           (ASH NUM (- AMT 40))))))) )

(DEFUN CC-TEST-BYTE-LOGIC ()
  (MAPC (FUNCTION (LAMBDA (F) (PRINT F) (FUNCALL F)))
	'(CC-TEST-SHIFTER CC-TEST-MASK-LEFT CC-TEST-MASK-RIGHT
	  ;;Here consider testing interaction of the two masks, or a full random-data test
	  CC-TEST-LC-AFFECTS-SHIFT))
  T)

;; Algorithm is to shift floating ones and zeros with all possible shifts.
;; Record bits that failed at shifter input, at shifter output, between
;; the two shifter stages, and also which shift counts fail.  Note that
;; if the masker proms aren't plugged in, selecting the 32-bit-wide byte
;; will work anyway due to pullups.  Prom problems will show up as failure
;; of particular bits at the shifter output, you can try unplugging the
;; offending prom.  To reduce randomness we bring 0 in
;; on the A-source.  This is now written so that it works whether or
;; not proms are present, it addresses 0 in the right mask which is all 1's
;; and 37 in the left mask which is also all 1's.
(DECLARE (SPECIAL CC-SUSPECT-BIT-LIST))
(DEFUN CC-TEST-SHIFTER ()
       (IF-FOR-LISPM (FORMAT T "~%********WARNING********  On the LISP Machine, this test
fails to win due to bugs in the bignum code.   BEWARE!!!!~%"))
  (CC-WRITE-A-MEM 2 0)
  (DO ((INPUT-ERRONEOUS-ZEROS NIL)
       (MIDDLE-ERRONEOUS-ZEROS NIL)
       (OUTPUT-ERRONEOUS-ZEROS NIL)
       (INPUT-ERRONEOUS-ONES NIL)
       (MIDDLE-ERRONEOUS-ONES NIL)
       (OUTPUT-ERRONEOUS-ONES NIL)
       (ERRONEOUS-SHIFT-COUNTS NIL)
       (CC-SUSPECT-BIT-LIST NIL)
       (BITNO 0 (1+ BITNO))) ;THE FLOATING BIT
      ((= BITNO 32.)
       (CC-PRINT-BIT-LIST "Shift counts with erroneous bits: " ERRONEOUS-SHIFT-COUNTS)
       (CC-PRINT-BIT-LIST "M bits with erroneous zeros: " INPUT-ERRONEOUS-ZEROS)
       (CC-PRINT-BIT-LIST "SA bits with erroneous zeros: " MIDDLE-ERRONEOUS-ZEROS)
       (CC-PRINT-BIT-LIST "R bits with erroneous zeros: " OUTPUT-ERRONEOUS-ZEROS)
       (CC-PRINT-BIT-LIST "M bits with erroneous ones: " INPUT-ERRONEOUS-ONES)
       (CC-PRINT-BIT-LIST "SA bits with erroneous ones: " MIDDLE-ERRONEOUS-ONES)
       (CC-PRINT-BIT-LIST "R bits with erroneous ones: " OUTPUT-ERRONEOUS-ONES))
    (DO ((BACKGROUND 37777777777 0))  ;FIRST FLOATING ZEROS, THEN FLOATING ONES
        (())
      (DECLARE (FIXNUM BACKGROUND))
      (CC-WRITE-MD (32-LOGXOR BACKGROUND #M (LSH 1 BITNO) #Q (ASH 1 BITNO)))  ;SHIFTER INPUT
      (CC-EXECUTE CONS-IR-OP CONS-OP-BYTE       ;INST TO SHIFT BY 0 INTO IR
                  CONS-IR-A-SRC 2
                  CONS-IR-M-SRC CONS-M-SRC-MD
                  CONS-IR-BYTL-1 37
                  CONS-IR-MROT 0
                  CONS-IR-BYTE-FUNC CONS-BYTE-FUNC-LDB) ;LDB = SR, NOT MR
      (DO ((MROT 0 (1+ MROT))
           (BAD)
           (GOOD (32-LOGXOR BACKGROUND #M (LSH 1 BITNO) #Q (ASH 1 BITNO)) ;EXPECTED OUTPUT
                 (ROT32 GOOD 1)))
          ((= MROT 32.))
        (DECLARE (FIXNUM MROT GOOD BAD))
        (COND ((NOT (= (SETQ BAD (CC-READ-OBUS)) GOOD)) ;HA! AN ERROR, STASH STUFF AWAY
               (ADD2L MROT ERRONEOUS-SHIFT-COUNTS)
               (FORMAT T "~S EXPECTED, ~S RECEIVED~%" GOOD BAD)
               (DO ((J 0 (1+ J))                ;BITS OF OUTPUT
                    (GOOD GOOD #M (LSH GOOD -1) #Q (ASH GOOD -1))
                    (BAD BAD #M (LSH BAD -1) #Q (ASH BAD -1)))
                   ((= J 32.))
                 (OR (= (32-LOGAND 1 GOOD) (32-LOGAND 1 BAD))
                     (COND ((ZEROP (32-LOGAND 1 GOOD))  ;AN ERRONEOUS ONE
                            (ADD2L J OUTPUT-ERRONEOUS-ONES)
                            (ADD2L (32-LOGAND (- J MROT) 37) INPUT-ERRONEOUS-ONES)
                            (ADD2L (32-LOGAND (- J (32-LOGAND MROT -4)) 37) MIDDLE-ERRONEOUS-ONES))
                           (T
                            (ADD2L J OUTPUT-ERRONEOUS-ZEROS)
                            (ADD2L (32-LOGAND (- J MROT) 37) INPUT-ERRONEOUS-ZEROS)
                            (ADD2L (32-LOGAND (- J (32-LOGAND MROT -4)) 37) MIDDLE-ERRONEOUS-ZEROS)
                            ))))))
        (SPY-WRITE SPY-IR-LOW (1+ (SPY-READ SPY-IR-LOW)))       ;INCREMENT MROT FIELD
        (CC-NOOP-DEBUG-CLOCK))
      (AND (ZEROP BACKGROUND) (RETURN NIL)))))

;; With the shift data paths known to work, read out all elements of the left
;; mask and verify that they contain the correct contents.  We continue to
;; select location 0 of the right mask, which is all 1's.
;; It may be helpful to pull out the right-mask proms at this stage.
(DEFUN CC-TEST-MASK-LEFT ()
  (CC-WRITE-A-MEM 1 0)
  (CC-WRITE-M-MEM 2 37777777777)
  ((LAMBDA (TEM)
      (DECLARE (FIXNUM TEM))
      (SETQ TEM (CC-READ-A-MEM 1))
      (OR (= 0 TEM)
	  (ERROR '|in 1@A - should be 0| TEM 'FAIL-ACT))
      (SETQ TEM (CC-READ-M-MEM 2))
      (OR (= 37777777777 TEM)
	  (ERROR '|in 2@M - should be 37777777777| TEM 'FAIL-ACT))
      (DO ((BYTL-1 0 (1+ BYTL-1))
	   (GOOD 1 (1+ #M (LSH GOOD 1) #Q (ASH GOOD 1))))
	  ((= BYTL-1 32.))
	(DECLARE (FIXNUM BYTL-1 GOOD))
	(CC-EXECUTE CONS-IR-OP CONS-OP-BYTE
		    CONS-IR-A-SRC 1
		    CONS-IR-M-SRC 2
		    CONS-IR-BYTL-1 BYTL-1
		    CONS-IR-MROT 0
		    CONS-IR-BYTE-FUNC CONS-BYTE-FUNC-LDB)  ;LDB = SR, NO MR
	(SETQ TEM (CC-READ-OBUS))
	(COND ((NOT (= TEM GOOD))
	       (PRINC '|/
BYTL-1=|)
	       (PRIN1 BYTL-1)
	       (PRINC '|, MROT=0, Left Mask=|)
	       (PRIN1 TEM)
	       (PRINC '|, should be |)
	       (PRIN1 GOOD)))))
   0))

;; With the shift data paths and the left mask known to work, read out
;; all locations of the right mask and verify that they are correct.
;; Here we hold the left mask at all 1's, which incidentally tests its
;; address adder.
(DEFUN CC-TEST-MASK-RIGHT ()
  (CC-WRITE-A-MEM 1 0)
  (CC-WRITE-M-MEM 2 37777777777)
  ((LAMBDA (TEM)
      (DECLARE (FIXNUM TEM))
      (SETQ TEM (CC-READ-A-MEM 1))
      (OR (= 0 TEM)
	  (ERROR '|in 1@A - should be 0| TEM 'FAIL-ACT))
      (SETQ TEM (CC-READ-M-MEM 2))
      (OR (= 37777777777 TEM)
	  (ERROR '|in 2@M - should be 37777777777| TEM 'FAIL-ACT))
      (DO ((MROT 0 (1+ MROT)) ;right mask address
	   (BYTL-1 37 (1- BYTL-1)) ;keeps the left mask address = 37
	   (GOOD 37777777777 (32-LOGXOR GOOD #M (LSH 1 MROT) #Q (ASH 1 MROT))))
	  ((= MROT 32.))
	(DECLARE (FIXNUM MROT BYTL-1 GOOD))
	(CC-EXECUTE CONS-IR-OP CONS-OP-BYTE
		    CONS-IR-A-SRC 1
		    CONS-IR-M-SRC 2
		    CONS-IR-BYTL-1 BYTL-1
		    CONS-IR-MROT MROT
		    CONS-IR-BYTE-FUNC CONS-BYTE-FUNC-SELECTIVE-DEPOSIT)  ;MR, NO SR
	(SETQ TEM (CC-READ-OBUS))
	(COND ((NOT (= TEM GOOD))
	       (PRINC '|/
BYTL-1=|)
	       (PRIN1 BYTL-1)
	       (PRINC '|, MROT=|)
	       (PRIN1 MROT)
	       (PRINC '|, Right Mask=|)
	       (PRIN1 TEM)
	       (PRINC '|, should be |)
	       (PRIN1 GOOD)))))
   0))

;; With the normal shift and mask logic known to work, test LC-modification.
;; Things to test are whether both halfwords and all 4 bytes properly mung
;; the MROT field.  Doesn't currently test whether automatic fetching and
;; LC incrementing works.  Eventually that should be tested.
(DEFUN CC-TEST-LC-AFFECTS-SHIFT ()
  (CC-WRITE-A-MEM 1 0)
  (CC-WRITE-M-MEM 2 37777777777)
  (CC-WRITE-FUNC-DEST CONS-FUNC-DEST-INT-CNTRL 1_29.) ;Put machine in byte mode
  (DO ((LC 1 (1+ LC))
       (LC-READBACK (+ 1_31. 1_29. 1) (1+ LC-READBACK)) ;Needfetch, Byte Mode, 1
       (GOOD 377 #M (LSH GOOD 8) #Q (ASH GOOD 8))
       (TEM))
      ((= LC 5))
    (DECLARE (FIXNUM LC LC-READBACK GOOD TEM))
    (CC-WRITE-FUNC-DEST CONS-FUNC-DEST-LC LC) ;Select byte (initially rightmost, LC=current+1)
    (SETQ TEM (CC-READ-M-MEM CONS-M-SRC-LC))
    (COND ((NOT (= TEM LC-READBACK))
	   (PRINC '|/
Wrong value in LC, is |)
	   (PRIN1 TEM)
	   (PRINC '|, but should be |)
	   (PRIN1 LC-READBACK)))
    (CC-EXECUTE CONS-IR-OP CONS-OP-BYTE
		CONS-IR-A-SRC 1
		CONS-IR-M-SRC 2
		CONS-IR-BYTL-1 7
		CONS-IR-MROT 0
		CONS-IR-MF 3
		CONS-IR-BYTE-FUNC CONS-BYTE-FUNC-SELECTIVE-DEPOSIT)  ;MR, NO SR
    (SETQ TEM (CC-READ-OBUS))
    (COND ((NOT (= TEM GOOD))
	   (PRINC '|/
LC=|)
	   (PRIN1 LC-READBACK)
	   (PRINC '| (byte mode), shifter output=|)
	   (PRIN1 TEM)
	   (PRINC '|, should be |)
	   (PRIN1 GOOD))))
  (CC-WRITE-FUNC-DEST CONS-FUNC-DEST-INT-CNTRL 0_29.) ;Put machine in word mode
  (DO ((LC 2 (+ LC 2))
       (LC-READBACK (+ 1_31. 2) (+ LC-READBACK 2)) ;Needfetch, no Byte Mode, 2 (=1 wd)
       (GOOD 177777 #M (LSH GOOD 16.) #Q (ASH GOOD 16.))
       (TEM))
      ((= LC 4))
    (DECLARE (FIXNUM LC LC-READBACK GOOD TEM))
    (CC-WRITE-FUNC-DEST CONS-FUNC-DEST-LC LC) ;Select halfword (initially rightmost, LC=current+1)
    (SETQ TEM (CC-READ-M-MEM CONS-M-SRC-LC))
    (COND ((NOT (= TEM LC-READBACK))
	   (PRINC '|/
Wrong value in LC, is |)
	   (PRIN1 TEM)
	   (PRINC '|, but should be |)
	   (PRIN1 LC-READBACK)))
    (CC-EXECUTE CONS-IR-OP CONS-OP-BYTE
		CONS-IR-A-SRC 1
		CONS-IR-M-SRC 2
		CONS-IR-BYTL-1 17
		CONS-IR-MROT 0
		CONS-IR-MF 3
		CONS-IR-BYTE-FUNC CONS-BYTE-FUNC-SELECTIVE-DEPOSIT)  ;MR, NO SR
    (SETQ TEM (CC-READ-OBUS))
    (COND ((NOT (= TEM GOOD))
	   (PRINC '|/
LC=|)
	   (PRIN1 LC-READBACK)
	   (PRINC '| (halfword mode), shifter output=|)
	   (PRIN1 TEM)
	   (PRINC '|, should be |)
	   (PRIN1 GOOD)))))
