;;; -*- Mode: LISP; Package: COLOR  -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;***** Things to do *****
; Functions to save and restore copy of hardware color map


;SCREEN data structure for the color TV
(DECLARE (SPECIAL TV-COLOR-SCREEN))

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
;Assume 60MHZ bit clock, therefore 15Mhz (66.7 ns) TTL clock, 533ns SYNC clk
; 30. sync clks per line, 10. for horz sync, 
;41.6 lines for 666 usec vertical
;1037 lines per 16.66 ms frame

;;; 64 Mhz main clock, 12 Mhz video data rate.
;;; During each sync period, 4 8 bit nibbles are shifted out.
;;; Therefore a TV cycle is done every 2 sync periods.

(DECLARE (SPECIAL SYNC))

(SETQ SYNC '(
   1 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 111 ;equalizing pulses, clr-tvma
   2 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   3 32 32 12 32 32 32 (39. 12) (5 13) (45. 12) (3 13) 213 13   ;vert sync
   3   30 30 10 30 30 30 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   13. 30 30 10 30 30 30 (4 11) (88. 11) 211 11 	;vert retrace
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   227. 30 30 10 30 30 30 (9. 11) 11 (36. 41 1) 1 1 (8. 11) 211 71   ;12 mhz video, 9x64 bits
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   1 30 30 10 30 30 30 (9. 11) (83. 11) 211 11
   1 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 111 ;equalizing pulses, clr-tvma
   2 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   1 30 30 10 30 30 31 (44. 11) (45. 12) (3 13) 213 73  ;extra 1/2 line, step-tvma
   2 32 32 12 32 32 32 (39. 12) (5 13) (45. 12) (3 13) 213 13   ;vert sync
   1 32 32 12 32 32 32 (39. 12) (5 13) (5 10) (43. 11) 211 11   ;1/2 vert sync, equalizing
   3 30 30 10 30 30 31 (44. 11) (5 10) (43. 11) 211 11  ;equalizing pulses
   13. 30 30 10 30 30 30 (4 11) (88. 11) 211 11 	;vert retrace
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   227. 30 30 10 30 30 30 (9. 11) 11 (36. 41 1) 1 1 (8. 11) 211 71
   6 30 30 10 30 30 30 (10. 11) (74. 1) (8. 11) 211 11
   1 30 30 10 30 30 30 (9. 11) (83. 11) 311 11
   (30 1)))

;Function which reads an XBUS location with parity checking disabled.
;This is useful if it might be NXM.  Unfortunately the CADR machine
;mode register cannot be read back so we guess that the value that
;is supposed to be in it is 46 (prom-disable, error-stop-enable, normal,
; no stat-stop-enable, parity-stop-enable).
;The mode register is writable at location 766012
;XBUS-ADDR is an I/O address like %XBUS-READ.
(DEFUN XBUS-READ-NO-PARITY (XBUS-ADDR)
  (PROG2 (%UNIBUS-WRITE 766012 42)	;Turn off error-stop-enable
	 (%XBUS-READ XBUS-ADDR)
	 (%UNIBUS-WRITE 766012 46)))

;Function to determine if this machine has a color TV (in general if an XBUS
; I/O address exists).  This is a bit of a crock.
;The criterion is if BITS can be set in XBUS-ADDR.
(DEFUN XBUS-LOCATION-EXISTS-P (XBUS-ADDR BITS)
  (%XBUS-WRITE XBUS-ADDR BITS)
  (BIT-TEST BITS (XBUS-READ-NO-PARITY XBUS-ADDR)))

(DEFUN color-exists-p (&AUX (SCREEN TV-COLOR-SCREEN)
			    (TV-COLOR-ADR (logand (screen-buffer screen) #o377777)))
  (XBUS-LOCATION-EXISTS-P TV-COLOR-ADR 1))

;; This stuff should be part of the color screen structure.
(DECLARE (SPECIAL COLOR-MAP-ON COLOR-MAP-OFF))
(SETQ COLOR-MAP-ON 377 COLOR-MAP-OFF 0)

; FUNCTIONS TO SETUP COLOR MAP
; NOTE: As the hardware does not allow reading back of the color map, it is necessary
;       to maintain an array of what we expect to be in the color map.  As this array should
;       be kept accurate, the only function that should be used to write the color map
;       is WRITE-COLOR-MAP.  The added advantage is that the array can be saved and restored.

;Write one location in the color map
;Sync to horizontal sync.
(DEFUN WRITE-COLOR-MAP (LOC R G B
                        &OPTIONAL (SYNCHRONIZE NIL) (SCREEN TV-COLOR-SCREEN)
                        &AUX (TV-ADR (SCREEN-CONTROL-ADDRESS SCREEN))
                             (HARDWARE-COLOR-MAP (GET (LOCF (SCREEN-PROPERTY-LIST SCREEN))
                                                      ':HARDWARE-COLOR-MAP)))
  (SETQ LOC (LOGAND LOC 17)
        R (- 377 (LOGAND (FIX R) 377))
        G (- 377 (LOGAND (FIX G) 377))
        B (- 377 (LOGAND (FIX B) 377)))
  (AND SYNCHRONIZE (PROG NIL A (COND ((GREATERP (LOGAND 40 (%XBUS-READ TV-ADR)) 0)
				      (RETURN NIL)))(GO A)))
  (%XBUS-WRITE-SYNC (+ TV-ADR 4) (DPB R 1010 LOC) 0 TV-ADR 100 100)
  (%XBUS-WRITE-SYNC (+ TV-ADR 4) (DPB G 1010 (DPB 1 0602 LOC)) 0 TV-ADR 100 100)
  (%XBUS-WRITE-SYNC (+ TV-ADR 4) (DPB B 1010 (DPB 2 0602 LOC)) 0 TV-ADR 100 100)
  (ASET (- 377 R) HARDWARE-COLOR-MAP LOC 0)
  (ASET (- 377 G) HARDWARE-COLOR-MAP LOC 1)
  (ASET (- 377 B) HARDWARE-COLOR-MAP LOC 2))

;Use this when you know you are in the vertical retrace.  Dont waste time waiting
: for horizontal retrace.
(DEFUN WRITE-COLOR-MAP-IMMEDIATE (LOC R G B
                        &OPTIONAL (SCREEN TV-COLOR-SCREEN)
                        &AUX (TV-ADR (SCREEN-CONTROL-ADDRESS SCREEN))
                             (HARDWARE-COLOR-MAP (GET (LOCF (SCREEN-PROPERTY-LIST SCREEN))
                                                      ':HARDWARE-COLOR-MAP)))
  (SETQ LOC (LOGAND LOC 17)
        R (- 377 (LOGAND (FIX R) 377))
        G (- 377 (LOGAND (FIX G) 377))
        B (- 377 (LOGAND (FIX B) 377)))
  (%XBUS-WRITE (+ TV-ADR 4) (DPB R 1010 LOC))
  (%XBUS-WRITE (+ TV-ADR 4) (DPB G 1010 (DPB 1 0602 LOC)))
  (%XBUS-WRITE (+ TV-ADR 4) (DPB B 1010 (DPB 2 0602 LOC)))
  (ASET (- 377 R) HARDWARE-COLOR-MAP LOC 0)
  (ASET (- 377 G) HARDWARE-COLOR-MAP LOC 1)
  (ASET (- 377 B) HARDWARE-COLOR-MAP LOC 2))

;Write color map from supplied (16 by 3) array during vertical retrace
(DEFUN BLT-COLOR-MAP (ARRAY &OPTIONAL (SCREEN TV-COLOR-SCREEN)
			    &AUX (TV-ADR (SCREEN-CONTROL-ADDRESS SCREEN))
			         (TV-ADR4 (+ 4 TV-ADR))
			         (HARDWARE-COLOR-MAP (GET (LOCF (SCREEN-PROPERTY-LIST SCREEN))
							  ':HARDWARE-COLOR-MAP))
				 (I 0)
				 (TEM-ARRAY (MAKE-ARRAY NIL ART-16B 60)))
      (DO ((I 0 (1+ I)))			;Precompute xbus-writands
	  (( I 20))
	(ASET (DPB (- 377 (FIX (AREF ARRAY I 0))) 1010 I) TEM-ARRAY I)
	(ASET (DPB (- 377 (FIX (AREF ARRAY I 1))) 1010 (+ I 100)) TEM-ARRAY (+ I 20))
	(ASET (DPB (- 377 (FIX (AREF ARRAY I 2))) 1010 (+ I 200)) TEM-ARRAY (+ I 40)))
      (DO () ((ZEROP (LOGAND 40 (%XBUS-READ TV-ADR)))))	;til not vert retracing
      (DO () ((BIT-TEST (%XBUS-READ TV-ADR) 40))	;til retrace starts
	(COND ((< I 20) (ASET (AREF ARRAY I 0) HARDWARE-COLOR-MAP I 0)
	                (ASET (AREF ARRAY I 1) HARDWARE-COLOR-MAP I 1)
			(ASET (AREF ARRAY I 2) HARDWARE-COLOR-MAP I 2)
			(SETQ I (1+ I)))))      ;update internal version while-u-wait
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 0))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 1))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 2))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 3))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 4))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 5))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 6))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 7))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 8))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 11))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 12))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 13))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 14))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 15))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 16))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 17))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 20))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 21))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 22))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 23))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 24))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 25))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 26))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 27))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 30))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 31))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 32))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 33))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 34))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 35))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 36))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 37))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 40))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 41))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 42))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 43))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 44))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 45))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 46))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 47))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 50))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 51))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 52))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 53))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 54))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 55))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 56))
      (%XBUS-WRITE TV-ADR4 (AREF TEM-ARRAY 57))
      (DO ((I I (1+ I)))			;finish updating internal map version
	  (( I 20))
	(ASET (AREF ARRAY I 0) HARDWARE-COLOR-MAP I 0)
	(ASET (AREF ARRAY I 1) HARDWARE-COLOR-MAP I 1)
	(ASET (AREF ARRAY I 2) HARDWARE-COLOR-MAP I 2))
      (RETURN-ARRAY TEM-ARRAY))

;Read a map location.  Returns four values: R, G, B, LOC
(DEFUN READ-COLOR-MAP (LOC &OPTIONAL (SCREEN TV-COLOR-SCREEN))
  (PROG ((LOC (LOGAND LOC 17))
         (HARDWARE-COLOR-MAP (GET (LOCF (SCREEN-PROPERTY-LIST SCREEN)) ':HARDWARE-COLOR-MAP)))
    (RETURN (AREF HARDWARE-COLOR-MAP LOC 0)
            (AREF HARDWARE-COLOR-MAP LOC 1)
            (AREF HARDWARE-COLOR-MAP LOC 2)
            LOC)))

;Fill the color map with the specified color from the specified starting address
(DEFUN FILL-COLOR-MAP (R G B &OPTIONAL (START 1) (SCREEN TV-COLOR-SCREEN))
  (DO ((I START (1+ I)))
      ((> I 17))
      (WRITE-COLOR-MAP I R G B SCREEN)))

;Make a color screen
(DEFUN MAKE-SCREEN (&OPTIONAL (NAME "COLOR") (XBUS-ADR -600000) (CONTROL-ADR 377750))
  (SI:TV-DEFINE-SCREEN NAME ':BITS-PER-PIXEL 4 ':BUFFER XBUS-ADR ':PLANE-MASK 0
                         ':HEIGHT 454. ':WIDTH 576. ':WHO-LINE-P NIL
                         ':CONTROL-ADDRESS CONTROL-ADR
                         ':PROPERTIES
                           `(:VIDEO :COLOR
                             :CONTROLLER :SIMPLE
                             :HARDWARE-COLOR-MAP ,(MAKE-ARRAY NIL ':ART-8B '(20 3)))))

;Init a color screen
(DEFUN SETUP (&OPTIONAL (SYNC-PROG SYNC) (SCREEN TV-COLOR-SCREEN)
              &AUX (TV-COLOR-ADR (SCREEN-CONTROL-ADDRESS SCREEN)))
  (COND ((XBUS-LOCATION-EXISTS-P TV-COLOR-ADR 1)
         (SI:TV-STOP-SYNC TV-COLOR-ADR)
         (SI:TV-FILL-SYNC SYNC-PROG 0 TV-COLOR-ADR)
         (SI:TV-START-SYNC 3 0 36. TV-COLOR-ADR)
	 ;;Put some color into the damn thing
	 ; (RANDOM-COLOR-MAP 1 NIL SCREEN)
	 ; (WRITE-COLOR-MAP 0 COLOR-MAP-OFF COLOR-MAP-OFF COLOR-MAP-OFF SCREEN)
         ; (FILL-COLOR-MAP COLOR-MAP-ON COLOR-MAP-ON COLOR-MAP-ON 1 SCREEN)
	 )))

(ADD-INITIALIZATION "COLOR" '(SETQ TV-COLOR-SCREEN (MAKE-SCREEN)) '(ONCE))
(ADD-INITIALIZATION "COLOR" '(SETUP) '(WARM))

(DECLARE (SPECIAL BITBLT-ARRAY))
(OR (BOUNDP 'BITBLT-ARRAY)
    (SETQ BITBLT-ARRAY (MAKE-ARRAY NIL ':ART-4B '(8. 1.))))

(DEFMACRO COLOR-FILLARRAY (COLOR)
  `(LET ((COLOR ,COLOR))
        (DO ((I 0 (1+ I))) (( I 8.)) (ASET COLOR BITBLT-ARRAY I 0))))

(DEFUN BARS (SIZE &OPTIONAL (START 0) (SCREEN TV-COLOR-SCREEN))
  (DO ((COLOR 0 (1+ COLOR))
       (SCREEN-WIDTH (SCREEN-WIDTH SCREEN))
       (HEIGHT (SCREEN-HEIGHT SCREEN))
       (TARGET-ARRAY (SCREEN-BUFFER-PIXEL-ARRAY SCREEN)))
      ((> COLOR 16))
      (COLOR-FILLARRAY (1+ COLOR))
      (OR (> (+ START (* SIZE COLOR) SIZE) SCREEN-WIDTH)
          (BITBLT TV-ALU-SETA SIZE HEIGHT
                  BITBLT-ARRAY 0 0
                  TARGET-ARRAY (+ START (* SIZE COLOR)) 0))))

(DEFUN STRIPE (X Y WIDTH HEIGHT COLOR &OPTIONAL (SCREEN TV-COLOR-SCREEN))
  (OR HEIGHT (SETQ HEIGHT (SCREEN-HEIGHT SCREEN)))
  (LET ((TARGET-ARRAY (SCREEN-BUFFER-PIXEL-ARRAY SCREEN)))
       (COLOR-FILLARRAY COLOR)
       (BITBLT TV-ALU-SETA WIDTH HEIGHT
               BITBLT-ARRAY 0 0
               TARGET-ARRAY X Y)))

(DEFUN R-G-B-BARS (&OPTIONAL (WIDTH 20) (SCREEN TV-COLOR-SCREEN))
  (R-G-B-COLOR-MAP)
  (BARS WIDTH 0 SCREEN))

(DECLARE (SPECIAL R-G-B))
(SETQ R-G-B `((,COLOR-MAP-ON ,COLOR-MAP-OFF ,COLOR-MAP-OFF)
              (,COLOR-MAP-OFF ,COLOR-MAP-ON ,COLOR-MAP-OFF)
              (,COLOR-MAP-OFF ,COLOR-MAP-OFF ,COLOR-MAP-ON)))

(DEFUN R-G-B-COLOR-MAP ()
  (WRITE-COLOR-MAP 0 COLOR-MAP-OFF COLOR-MAP-OFF COLOR-MAP-OFF)
  (DO ((I 1 (1+ I)))
      ((> I 17))
      (LEXPR-FUNCALL (FUNCTION WRITE-COLOR-MAP) I
                     (NTH (\ I 3) R-G-B))))

(DEFUN CLEAR (&OPTIONAL (SCREEN TV-COLOR-SCREEN))
  (WITHOUT-INTERRUPTS 
    (TV-SELECT-SCREEN SCREEN)
    (TV-ERASE (* 4 (SCREEN-WIDTH SCREEN)) (SCREEN-HEIGHT SCREEN) 0 0 0)))

(DEFUN FLOOD (&OPTIONAL (SCREEN TV-COLOR-SCREEN))
  (WITHOUT-INTERRUPTS
    (TV-SELECT-SCREEN SCREEN)
    (TV-ERASE (* 4 (SCREEN-WIDTH SCREEN)) (SCREEN-HEIGHT SCREEN) 0 0 TV-ALU-SETA)))

(DEFUN ALL-BARS (&OPTIONAL (WIDTH 20) (SCREEN TV-COLOR-SCREEN))
  (R-G-B-COLOR-MAP)
  (CLEAR)
  (LET ((SCREEN-WIDTH (SCREEN-WIDTH SCREEN))
        (BARS-WIDTH (* WIDTH 17)))
       (DO ((START 0 (+ START BARS-WIDTH)))
           (( START SCREEN-WIDTH))
           (BARS WIDTH START SCREEN))))

(DEFUN CROSSHATCH (&OPTIONAL (BAR-SIZE 1) (SPACING 20) (SCREEN TV-COLOR-SCREEN))
  (CLEAR SCREEN)
  (LET ((WIDTH (SCREEN-WIDTH SCREEN))
        (HEIGHT (SCREEN-HEIGHT SCREEN))
        (NV) (NH))
       (SETQ NV (- (// WIDTH SPACING) (COND ((> (\ WIDTH SPACING) BAR-SIZE) 0)
                                               (T 1)))
             NH (- (// HEIGHT SPACING) (COND ((> (\ HEIGHT SPACING) BAR-SIZE) 0)
                                             (T 1))))
       (SETQ WIDTH (+ (* NV SPACING) BAR-SIZE)
             HEIGHT (+ (* NH SPACING) BAR-SIZE))
       (DO ((X 0 (+ X SPACING))
            (I 0 (1+ I)))
           ((> I NV))
           (STRIPE X 0 BAR-SIZE HEIGHT (1+ (\ I 16))))
       (DO ((Y 0 (+ Y SPACING))
            (I 0 (1+ I)))
           ((> I NH))
           (STRIPE 0 Y WIDTH BAR-SIZE (1+ (\ I 16))))))

(DEFUN WHITE-CROSSHATCH (&REST ARGS)
  (WRITE-COLOR-MAP 0 COLOR-MAP-OFF COLOR-MAP-OFF COLOR-MAP-OFF)
  (FILL-COLOR-MAP COLOR-MAP-ON COLOR-MAP-ON COLOR-MAP-ON 1)
  (LEXPR-FUNCALL (FUNCTION CROSSHATCH) ARGS))

(DEFUN SPECTRUM-COLOR-MAP ()
  (DO ((I 1 (1+ I))
       (OFFSET 0 (COND ((= I 7) 1) ((= I 16) 2) (T OFFSET))))
      ((> I 17))
      (WRITE-COLOR-MAP I (* (LDB 0001 (+ I OFFSET)) COLOR-MAP-ON)
                         (* (LDB 0101 (+ I OFFSET)) COLOR-MAP-ON)
                         (* (LDB 0201 (+ I OFFSET)) COLOR-MAP-ON))))

(DEFUN RECTANGLE (X Y WIDTH HEIGHT COLOR
                      &OPTIONAL (ALU TV-ALU-SETA) (SCREEN TV-COLOR-SCREEN))
  (COLOR-FILLARRAY COLOR)
  (BITBLT ALU WIDTH HEIGHT
          BITBLT-ARRAY 0 0
          (SCREEN-BUFFER-PIXEL-ARRAY SCREEN) X Y))

(DEFUN TRACK-MOUSE (&OPTIONAL (CURSOR-WIDTH 30) (CURSOR-HEIGHT 5) (COLOR 1))
  (DO ((BOX-X) (BOX-Y)
       (MAX-X (- (SCREEN-WIDTH TV-COLOR-SCREEN) CURSOR-WIDTH))
       (MAX-Y (- (SCREEN-HEIGHT TV-COLOR-SCREEN) CURSOR-HEIGHT)))
      ((KBD-TYI-NO-HANG)
       (COLOR:RECTANGLE BOX-X BOX-Y CURSOR-WIDTH CURSOR-HEIGHT COLOR TV-ALU-XOR))
      (COND (BOX-X
             (MOUSE-INPUT T)
             (COLOR:RECTANGLE BOX-X BOX-Y CURSOR-WIDTH CURSOR-HEIGHT COLOR TV-ALU-XOR)))
      (OR (ZEROP MOUSE-LAST-BUTTONS)
          (SETQ COLOR MOUSE-LAST-BUTTONS))
      (COLOR:RECTANGLE (SETQ BOX-X (MIN MOUSE-X MAX-X)) (SETQ BOX-Y (MIN MOUSE-Y MAX-Y))
                       CURSOR-WIDTH CURSOR-HEIGHT
                       COLOR TV-ALU-XOR)))

(DEFUN RANDOM-COLOR-MAP (&OPTIONAL (START 1) (SYNCHRONIZE NIL) (SCREEN TV-COLOR-SCREEN))
       (DO ((I START (1+ I)))
           ((= I 20))
           (WRITE-COLOR-MAP I (RANDOM (1+ COLOR-MAP-ON)) (RANDOM (1+ COLOR-MAP-ON))
			    (RANDOM (1+ COLOR-MAP-ON)) SYNCHRONIZE SCREEN)
	   (SETQ SYNCHRONIZE NIL)))		;ONLY SYNCHRONIZE THE FIRST CHANGE

(DEFUN GRAY-COLOR-MAP (&OPTIONAL (BASE 0))
       (DO ((I 0 (1+ I))
            (LEVEL))
           ((= I 20))
           (SETQ LEVEL (\ (+ BASE (* 20 I)) 400))
           (WRITE-COLOR-MAP I LEVEL LEVEL LEVEL)))

(DEFUN COLORIZE (&OPTIONAL (DELAY 4))
       (DO ()
           ((KBD-TYI-NO-HANG))
           (PROCESS-ALLOW-SCHEDULE)
           (RANDOM-COLOR-MAP 1 (> DELAY 2))	;IF THE DELAY IS > 2, SYNCHRONIZE WRITES
           (OR (ZEROP DELAY) (PROCESS-SLEEP DELAY))))

(DEFUN GRAY-COLORIZE (&OPTIONAL (DELAY 2))
       (DO ()
           ((KBD-TYI-NO-HANG))
           (PROCESS-ALLOW-SCHEDULE)
           (GRAY-COLOR-MAP (RANDOM 400))
           (OR (ZEROP DELAY) (PROCESS-SLEEP DELAY))))

;;; Choose two colors and melt each into the other, repeatedly.

(DEFUN COLORATE (&OPTIONAL (DELAY 4) (STEPS 1000.))
       (DO ()
           ((KBD-TYI-NO-HANG))
           (PROCESS-ALLOW-SCHEDULE)
	   (LET ((J (RANDOM 20))
		 (KFOO (RANDOM 17)))
	     (LET ((K (IF (< KFOO J) KFOO (1+ KFOO))))
	       (MULTIPLE-VALUE-BIND (JR JG JB) (READ-COLOR-MAP J)
		 (MULTIPLE-VALUE-BIND (KR KG KB) (READ-COLOR-MAP K)
		   (DO ((I STEPS (1- I))
			(FSTEPS (FLOAT STEPS))
			(FJR (FLOAT JR))
			(FJG (FLOAT JG))
			(FJB (FLOAT JB))
			(FKR (FLOAT KR))
			(FKG (FLOAT KG))
			(FKB (FLOAT KB)))
		       ((< I 0))
		     (LET ((P (// (FLOAT I) FSTEPS))
			   (1-P (// (FLOAT (- STEPS I)) FSTEPS)))
		       (WRITE-COLOR-MAP J
					(FIX (+ (* P FJR) (* 1-P FKR)))
					(FIX (+ (* P FJG) (* 1-P FKG)))
					(FIX (+ (* P FJB) (* 1-P FKB))))
		       (WRITE-COLOR-MAP K
					(FIX (+ (* P FKR) (* 1-P FJR)))
					(FIX (+ (* P FKG) (* 1-P FJG)))
					(FIX (+ (* P FKB) (* 1-P FJB))))))))))
           (OR (ZEROP DELAY) (PROCESS-SLEEP DELAY))))

(DECLARE (SPECIAL :10LEAF))

(DEFUN PUT-UP-PICT (&OPTIONAL (PICT :10LEAF)
                    &AUX MIN MAX HEIGHT WIDTH SC X0 Y0
                         (SCR (SCREEN-BUFFER-PIXEL-ARRAY TV-COLOR-SCREEN)))
  (CLEAR)
  (GRAY-COLOR-MAP)
  (SETQ MIN (CDR (ARRAYDIMS PICT)))
  (SETQ WIDTH (CAR MIN) HEIGHT (CADR MIN))
  (SETQ X0 (// (- (SCREEN-WIDTH TV-COLOR-SCREEN) WIDTH) 2)
        Y0 (// (- (SCREEN-HEIGHT TV-COLOR-SCREEN) HEIGHT) 2))
  (SETQ MIN 377 MAX 0)
  (DO I 0 (1+ I) (>= I WIDTH)
    (DO J 0 (1+ J) (>= J HEIGHT)
      (SETQ MIN (MIN MIN (AREF PICT I J)))
      (SETQ MAX (MAX MAX (AREF PICT I J)))))
  (SETQ SC (// (- MAX MIN) 20.0S0))
  (DO I 0 (1+ I) (>= I WIDTH)
    (DO J 0 (1+ J) (>= J HEIGHT)
      (ASET (FIX (// (- (AREF PICT I J) MIN) SC))
            SCR (+ I X0) (+ J Y0)))))

;;;Macros
(declare (special color-inks))

(DEFMACRO SWAP (A B)
   `(SETF ,A (PROG1 ,B (SETF ,B ,A))))


;;Color line primitive - screen coords
;; Probably should draw away from starting point

(defun color-draw-line (x1 y1 x2 y2
			   &optional (color 17) (alu tv-alu-seta))
    (and (> x1 x2) (swap x1 x2) (swap y1 y2))
    (let ((dx (- x2 x1))
          (dy (- y2 y1))
          (pixel-array (screen-buffer-pixel-array tv-color-screen)))
         (let ((dir-y (if (minusp dy) -1 1))
               (dy (abs dy)))
              (cond ((zerop dy) (color-bitblt x1 y1 x2 (1+ y1) color alu))
                    ((zerop dx) (color-bitblt x1 (min y1 y2) (1+ x2) (max y1 y2) color alu))
                    ((> dx dy)    ;X is larger step
                     (do ((x1 x1 (1+ x1))
                          (rem (// dy 2) (+ rem dy)))
                         ((> x1 x2))
                         (if ( rem dx) (setq y1 (+ y1 dir-y) rem (- rem dx)))
                         (aset color pixel-array x1 y1)))
                    (t				;Y is larger step
                     (do ((i 0 (1+ i))
                          (y y1 (+ y dir-y))
                          (rem (// dx 2) (+ rem dx)))
                         ((> i dy))
                         (if ( rem dy) (setq x1 (1+ x1) rem (- rem dy)))
                         (aset color pixel-array x1 y)))))))

;; Fill color into rectangle, x1,y1 < x2,y2

(defun color-bitblt (x1 y1 x2 y2 &optional (color 17) (alu tv-alu-seta))
    (bitblt alu (- x2 x1) (- y2 y1)
	    (aref color-inks color) 0 0
	    (screen-buffer-pixel-array tv-color-screen) x1 y1))


;;Make 4B array of color for BITBLT

(defun make-ink-arrays (&aux (ink-array (make-array nil 'art-q 20)))
   (do color 0 (1+ color) (= color 20)
     (let ((bitblt-array (make-array nil 'art-4b '(8 8))))
	  (aset bitblt-array ink-array color)
	  (do y 0 (1+ y) (= y 8)			;8 high
	      (do x 0 (1+ x) (= x 8)			;8 bytes wide
		  (aset color bitblt-array x y)))))
   ink-array)

(or (boundp 'color-inks) (setq color-inks (make-ink-arrays)))
