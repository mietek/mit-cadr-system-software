;;;Random declarations, etc.			-*-lisp-*-
(DECLARE (EVAL (READ)))
(PROGN (LOAD '(MACROS > DSK LISPM))
       (LOAD '(DEFMAC FASL DSK LISPM2))
       (LOAD '(LMMAC > DSK LISPM2)))

(DECLARE (EVAL (READ)))
       (DEFUN **STRING** MACRO (X) `',(CADR X)) ;Bubbles in my brain

(INCLUDE ((LMCONS)CADMAC >))

(DECLARE (FIXNUM I J K M N NBITS BITNO REGADR PPSS SHIFT RELAD)
	 (SPECIAL CC-SUSPECT-BIT-LIST CC-DIAG-TRACE CC-TEST-ADR-BARFED)
	 (FIXNUM (CC-SYNC-E FIXNUM)(PHYS-MEM-READ FIXNUM))
	 (MUZZLED T)) ;WHY?

(DEFMACRO LOGAND* (&REST X) `(BOOLE 1 . ,X))

(DEFMACRO LOGXOR* (&REST X) `(BOOLE 6 . ,X))

;;;Subrs for data path diagnostics (which don't work) 

(DEFUN CC-SYNC-E (ADR)
	(PHYS-MEM-WRITE 17377762 ADR)
	(LOGAND 377 (PHYS-MEM-READ 17377761)))

(DEFUN CC-SYNC-D (ADR DATA)
	(PHYS-MEM-WRITE 17377762 ADR)
	(PHYS-MEM-WRITE 17377761 DATA))

(DEFUN E (ADR) (PHYS-MEM-READ ADR))
(DEFUN D (ADR DATA) (PHYS-MEM-WRITE ADR DATA))
(DEFUN S NIL (DBG-PRINT-STATUS) (DBG-RESET-STATUS))

(DECLARE (SPECIAL TV TVB PTR SYNC VSP COLOR BOW SYNC-525))

(SETQ   TVB 17000000		;TV REGISTER
	TV 17377760		;control register
	PTR 17377761		;sync program address
	SYNC 17377762		;sync program memory
	VSP 17377763		;vertical spacing <0:6>, enable-sync
	COLOR 17377764)		;Color value <15:8>, Color sel <7:6>, <5:4>, 
				;  Color # <3:0>
(DEFUN TV-FILL (L)
  (PHYS-MEM-WRITE VSP 0)		;RESET SYNC
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
			      (PRIN1 I) (TYO 11)
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
	   (TYO 11)
	   (PRIN1 (CC-SYNC-E I))))

(DEFUN TV-FIND (WRD MASK)
   (DO I 0 (1+ I) (= I 4096.)
       (COND ((= WRD (LOGAND MASK (CC-SYNC-E I)))
	      (TERPRI)
	      (PRIN1 I)
	      (TYO 11)
	      (PRIN1 (LOGAND (CC-SYNC-E I) 377))))))

;Takes function to generate pattern

(DECLARE (SPECIAL TV-TEST-SIZE))
(SETQ TV-TEST-SIZE 100000)

(DEFUN TV-TEST (FCN)
   (DECLARE (FIXNUM I TVB FCN LIM TEM TV-TEST-SIZE))
   (PROG (LIM TEM)
	 (SETQ LIM (+ TVB TV-TEST-SIZE))
	 (DO I TVB (1+ I) (>= I LIM)
	   (PHYS-MEM-WRITE I (BOOLE FCN I 0)))
	 (DO ((I TVB (1+ I)))
	     ((>= I LIM))
	     (COND ((NOT (= (SETQ TEM (LOGAND 37777777 (PHYS-MEM-READ I)))
			    (LOGAND 37777777 (BOOLE FCN I 0))))
		    (TERPRI)
		    (PRIN1 I) (TYO 11)
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

;;Sync program for Motorola M4408 monitor
;; 30.5kHz horizontal, interlaced.
;; Assuming Mode 1, 31.75 Mhz video rate
;; Each sync cycle is 16 bit times, or 504 ns.
;; Therefore one line is 66 sync cycles.
;; Approx vertical screen dimensions:
;;  22 lines blanking 	760 usec
;;  480 lines visible	15.7 ms
;;  6.5 lines blank	160 usec.
;; Approx horizontal screen dimensions:
;;   Retrace       12 sync cycles
;;   Left Margin   8 sync cycles
;;   Visible:      48 sync cycles
;;   Right Margin  8 sync cycles
;;                 --------------
;;   Line          66 sync cycles     32.8 usec.


(setq SYNC-M4408 '(
   3 (2. 33) (6. 13) (4. 12) (12. 12 12 12 12) 12 (5. 12) 212 112	;vert sync, clr tvma
   19. (2. 31) (6. 11) (4. 10) (12. 10 10 10 10) 10 (5. 10) 210 10	;vert blank
   255. (2. 31) (6. 11) (4. 10) (12. 40 0 0 0) 0 (5. 10) 260 10		;visible, step 1 line
   225.  (2. 31) (6. 11) (4. 10) (12. 40 0 0 0) 0 (5. 10) 260 10	;visible, step 1 line
   7. (2. 31) (6. 11) (4. 10) (12. 10 10 10 10) 10 (5. 10) 210 10	;vert blank
   ;;second frame
   1 (2. 31) (6. 11) (25. 10) (31. 12) 212 112				;vert sync, clr tvma
   1 (2. 33) (6. 13) (4. 12) (12. 12 12 12 12) 12 (5. 12) 212 72	;vert sync, step tvma
   1 (2. 31) (6. 11) (25. 12) (31. 10) 210 10				;vert sync, clr tvma
   19. (2. 31) (6. 11) (4. 10) (12. 10 10 10 10) 10 (5. 10) 210 10	;vert blank
   255. (2. 31) (6. 11) (4. 10) (12. 40 0 0 0) 0 (5. 10) 260 10		;visible, step 1 line
   225.  (2. 31) (6. 11) (4. 10) (12. 40 0 0 0) 0 (5. 10) 260 10	;visible, step 1 line
   6. (2. 31) (6. 11) (4. 10) (12. 10 10 10 10) 10 (5. 10) 210 10	;vert blank
))



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
   1 (5 32) (37. 12) (5 13) (5 10) (40. 11) 211 11	;1/2 vert sync, equalizing pulses
   3 (5 30) (42. 11) (5 10) (40. 11) 211 11		;equalizing pulses
   13. (5 30) (5 10) (82. 11) 211 11			;vert retrace
   240. (5 30) (5 10) (8. 41 (7 1))(1. 61 (7 1)) (10. 1) 301 1	;12 mhz video, 9x64 bits
;   240. (5 30) (5 10) (35. 41 1) (1. 61  1) (10. 1) 301 1	;12 mhz video, 9x64 bits
   (30 1)))						;guard

(SETQ SYNC-IDLE '(
   5 (2 20) (4 0) 300 0))	;2 refresh, 6 processor cycles

;Assume 60MHZ bit clock, therefore 15Mhz (66.7 ns) TTL clock, 533ns SYNC clk
; 30. sync clks per line, 10. for horz sync, 
;41.6 lines for 666 usec vertical
;1037 lines per 16.66 ms frame


; 640. X 896.
(SETQ SYNC-CPT '(
   1.  (2 33) (8 13) (18. 12) 212 112
   45.  (2 33) (8 13) (18. 12) 212 12
   8.  (2 33)  (6 13) 13 12 (18. 2) 202 2
   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   255. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   131. (2 31) (6 11) 11 50 (9. 0 40) 200 0
   8. (2 31) (6 11) 11 10 (8. 0 0) 0 0 300 0
))

;704. x 896.
(SETQ SYNC-CPT1 '(
   1.  (1 33) (5 13) 12 12 (10. 12 12) 212 113			;VERT SYNC, CLEAR TVMA
   53.  (1 33) (5 13) 12 12 (10. 12 12) 212 13			;VERT RETRACE
   8.  (1 31)  (5 11) 11 10 (10. 0 0) 200 21		;8 LINES OF MARGIN
   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   255. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   131. (1 31) (5 11) 11 50 (10. 0 40) 200 21
   7. (1 31) (5 11) 11 10 (10. 0 0) 200 21
   1. (1 31) (5 11) 11 10 (10. 0 0) 300 23
))
   
(SETQ SYNC-CPT-CONRAC '(
   42.  (2 30) (8 10) (18. 11) 211 11
   255. (2 32) (7 12) 12 53 (8. 3 43) 3 243 3
   255. (2 32) (7 12) 12 53 (8. 3 43) 3 243 3
   255. (2 32) (7 12) 12 53 (8. 3 43) 3 243 3
   231. (2 32) (7 12) 12 53 (8. 3 43) 3 343 3
))

(DEFUN CPT ()
    (cadpmi)
    (PHYS-MEM-WRITE VSP 0)
    (TV-FILL SYNC-CPT)
    (TV-VERIFY SYNC-CPT)
    (PHYS-MEM-WRITE TV (+ (LSH BOW 2) 0))
    (PHYS-MEM-WRITE VSP 200))

(SETQ BOW 0)

(DEFUN OLD-TV (RELOAD)
    (PHYS-MEM-WRITE VSP 0)					;DISABLES SYNC PULSES
    (COND (RELOAD (TV-FILL SYNC-525)
		  (TV-VERIFY SYNC-525)))
    (PHYS-MEM-WRITE TV (+ (LSH BOW 2) 2))
    (PHYS-MEM-WRITE VSP 211)				;Re-enable, 9 word step
)
    
