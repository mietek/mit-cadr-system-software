;;;Random declarations, etc.			-*-lisp-*-
;;;*** This version runs on the Lisp machine itself.

;(INCLUDE ((LMCONS)CADMAC >))

;(DECLARE (FIXNUM I J K M N NBITS BITNO REGADR PPSS SHIFT RELAD)
;	 (SPECIAL CC-SUSPECT-BIT-LIST CC-DIAG-TRACE CC-TEST-ADR-BARFED)
;	 (FIXNUM (CC-SYNC-E FIXNUM) (DBG-READ-XBUS FIXNUM))
;	 (NOTYPE (CC-SYNC-D FIXNUM FIXNUM) (DBG-WRITE-XBUS FIXNUM FIXNUM))
;	 (MUZZLED T)) ;WHY?

;;;Subrs for access to sync program

(DEFUN CC-SYNC-E (ADR)
	(%XBUS-WRITE 377762 ADR)
	(LOGAND 377 (%XBUS-READ 377761)))

(DEFUN CC-SYNC-D (ADR DATA)
	(%XBUS-WRITE 377762 ADR)
	(%XBUS-WRITE 377761 DATA))

(DEFUN E (ADR) (%XBUS-READ (- ADR 17000000)))
(DEFUN D (ADR DATA) (%XBUS-WRITE (- ADR 17000000) DATA))

(DECLARE (SPECIAL TV TVB PTR SYNC VSP COLOR BOW SYNC-525))

(SETQ   TVB 17000000		;TV REGISTER
	TV 17377760		;control register
	PTR 17377761		;sync program address
	SYNC 17377762		;sync program memory
	VSP 17377763		;vertical spacing <0:6>, enable-sync
	COLOR 17377764)		;Color value <15:8>, Color sel <7:6>, <5:4>, 
				;  Color # <3:0>
(DEFUN TV-FILL (L)
  (TV-FILL-1 0 L))

(DEFUN TV-FILL-1 (I L)
   (DO ((L L (CDR L))
	(X))
       ((NULL L) I)
     (SETQ X (CAR L))
     (COND ((ATOM X) (CC-SYNC-D I X) (SETQ I (1+ I)))
	   (T (DO N (CAR X) (1- N) (ZEROP N)
		(SETQ I (TV-FILL-1 I (CDR X))))))))

(DEFUN TV-VERIFY (L)
  (TV-VERIFY-1 0 L))

(DEFUN TV-VERIFY-1 (I L)
   (DO ((L L (CDR L))
	(TEM)
	(X))
       ((NULL L) I)
     (SETQ X (CAR L))
     (COND ((ATOM X) (COND ((NOT (= (SETQ TEM (CC-SYNC-E I)) X))
			    (TERPRI)
			    (PRIN1 I) (TYO 211)
			    (PRIN1 TEM)
			    (PRINC '|, should be |)
			    (PRIN1 X)))
		     (SETQ I (1+ I)))
	   (T (DO N (CAR X) (1- N) (ZEROP N)
		  (SETQ I (TV-VERIFY-1 I (CDR X))))))))

(DEFUN TV-DUMP (START N)
       (DO ((I START (1+ I))
	    (N N (1- N)))
	   ((ZEROP N))
	 (TERPRI)
	 (PRINC I)
	 (TYO 211)
	 (PRIN1 (CC-SYNC-E I))))

(DEFUN TV-FIND (WRD MASK)
   (DO I 0 (1+ I) (= I 4096.)
       (COND ((= WRD (LOGAND MASK (CC-SYNC-E I)))
	      (TERPRI)
	      (PRIN1 I)
	      (TYO 211)
	      (PRIN1 (CC-SYNC-E I))))))

;Takes function to generate pattern (function is a BOOLE opcode) 
;Relies on location 0 to %XBUS-WRITE is start of TV buffer
(DEFUN TV-TEST (FCN)
   (PROG (LIM TEM)
	 (SETQ LIM 100000)
	 (DO I 0 (1+ I) (>= I LIM)
	   (%XBUS-WRITE I (BOOLE FCN I 0)))
	 (DO ((I 0 (1+ I)))
	     ((>= I LIM))
	   (COND ((NOT (= (SETQ TEM (%XBUS-READ I))
			  (BOOLE FCN I 0)))
		  (TERPRI)
		  (PRIN1 I) (TYO 211)
		  (PRIN1 TEM)
		  (PRINC '|, should be |)
		  (PRIN1 (BOOLE FCN I 0))
))))
)

(DEFUN TESTR ()
       (TV-TEST 0)
       (TV-TEST 17)
       (TV-TEST 5)
)

;sync program bits
;1  HSYNC
;2  VSYNC
;4  COMPOSITE - (not used really, encode on HSYNC)
;10  BLANKING
;     0  PROC CYC
;     20  REFRESH
;     40  INC TVMA
;     60  STEP TVMA
;     0    --
;     100  CLR TVMA
;     200  EOL
;     300  EOP


;Sync program for 525 line mode
; (assume 60 MHz clock for now, 94. sync intervals = 63.5 usec. )

;WITH INVERTED HORIZONTAL SYNC (ENCODED AS COMPOSITE)
(SETQ SYNC-525 '(
   1 (5 30) (42. 11) (5 10) (40. 11) 211 111		;equalizing pulses, clr-tvma
   2 (5 30) (42. 11) (5 10) (40. 11) 211 11		;equalizing pulses
   3 (5 32) (37. 12) (5 13) (42. 12) (3 13) 213 13	;vert sync
   3 (5 30) (42. 11) (5 10) (40. 11) 211 11		;equalizing pulses
   13. (5 30) (5 10) (82. 11) 211 11			;vert retrace
   240. (5 30) (5 10) (8. 41 (7 1))(1. 61 (7 1)) (10. 1) 201 1	;12 mhz video, 9x64 bits
;   240. (5 30) (5 10) (35. 41 1) (1. 61  1) (10. 1) 201 1	;12 mhz video, 9x64 bits
   1 (5 30) (42. 11) (5 10) (40. 11) 211 111		;equalizing pulses, clr-tvma
   2 (5 30) (42. 11) (5 10) (40. 11) 211 11		;equalizing pulses
   1 (5 30) (42. 11) (42. 12) (3 13) 213 73		;extra 1/2 line, step-tvma
   2 (5 32) (37. 12) (5 13) (42. 12) (3 13) 213 13	;vert sync
   1 (5 12) (37. 12) (5 13) (5 10) (40. 11) 211 11	;1/2 vert sync, equalizing pulses
   3 (5 30) (42. 11) (5 10) (40. 11) 211 11		;equalizing pulses
   13. (5 30) (5 10) (82. 11) 211 11			;vert retrace
   240. (5 30) (5 10) (8. 41 (7 1))(1. 61 (7 1)) (10. 1) 301 1	;12 mhz video, 9x64 bits
;   240. (5 30) (5 10) (35. 41 1) (1. 61  1) (10. 1) 301 1	;12 mhz video, 9x64 bits
   (30 1)))						;guard

(SETQ SYNC-IDLE '(
   5 (2 20) (4 0) 300 0))	;2 refresh, 6 processor cycles

(SETQ BOW 0)

(DEFUN OLD-TV (RELOAD)
    ;(DBG-RESET)					;DISABLES SYNC PULSES
    (D VSP 0)  ;DISABLE SYNC OUT TO TV, HOPEFULLY THIS IS ENOUGH
    (COND (RELOAD (TV-FILL SYNC-525)
		  (TV-VERIFY SYNC-525)))
    (D TV (+ (LSH BOW 2) 2))
    (D VSP 211)				;Re-enable, 9 word step
)
    
