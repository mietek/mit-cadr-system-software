;;; -*-LISP-*-
;;; Lowest-level routines for talking to CADR

;This may be grossly SI-damaged.  The alternative would be to make P11N
;in CTALK a run-time parameter and fix DBG-READ and DBG-WRITE.  The names
;would then not be the greatest in the world.

(DECLARE (SPECIAL TALK-PATH		;Symbol for how we're getting there
					;Values are NO-BUSINT, BUSINT, TEN11
						;These names lose!
		  TEN11-ADDRESS		;Map 32K like a 16-bit-address pdp11
		  TALK-DEBUG-ADDRESS	;Unibus address of debug interface using
))

(DEFUN LET MACRO (X)
  (CONS (LIST 'LAMBDA (MAPCAR 'CAR (CADR X)) (CONS 'PROGN (CDDR X)))
	(MAPCAR 'CADR (CADR X))))

(DEFUN LOGAND MACRO (X)
  (CONS 'BOOLE (CONS 1 (CDR X))))

;This function sets up the TEN11 map, the value of TALK-PATH, and so forth
(DEFUN TALK-INIT (PATH)
  ;; Make sure we have address space allocated for 10-11
  (COND ((NOT (BOUNDP 'TEN11-ADDRESS))
	 (LET ((ADR (LH/| 16384. 'RANDOM)))
	   (AND (ZEROP ADR) (ERROR '|COULD NOT GET LH SPACE|))
	   (SYSCALL 0 'CORBLK 0 -1 (+ (LSH -16. 18.) (LSH ADR -10.))) ;Delete pages
	   (SETQ TEN11-ADDRESS ADR))))
  ;; Map in the appropriate 11
  (LET ((11NO (COND ((MEMQ PATH '(NO-BUSINT BUSINT))	;Via debugging interface in CONS
		     2)
		    (T 3))))				;Prototypical CADR
    (DECLARE (FIXNUM 11NO 11ADR 10ADR))
    (DO ((11ADR 0 (+ 11ADR 10000))
	 (10ADR TEN11-ADDRESS (+ 10ADR 2000)))
	((= 11ADR 160000))
      (TALK-MAP-11-PAGE 11NO 11ADR 2000 10ADR))
    (DO ((11ADR 760000 (+ 11ADR 10000))
	 (10ADR (+ TEN11-ADDRESS 34000) (+ 10ADR 2000)))
	((= 11ADR 1000000))
      (TALK-MAP-11-PAGE 11NO 11ADR 2000 10ADR)))
  (COND ((EQ PATH 'NO-BUSINT)
	 (SETQ TALK-DEBUG-ADDRESS 764500))	;Accesses SPY registers only
	((EQ PATH 'BUSINT)
	 (SETQ TALK-DEBUG-ADDRESS 764540))	;Accesses other machine's Unibus
	((EQ PATH 'TEN11) )
	(T (ERROR '|UNKNOWN PATH TO CADR| PATH)))
  (SETQ TALK-PATH PATH))

(DEFUN TALK-MAP-11-PAGE (11NO 11ADR 11SIZE 10ADR)
  (SYSCALL 0 'CORBLK 0 -1 (LSH 10ADR -10.))	;Delete pre-existing page
  (AND (SYSCALL 0 'T11MP (LSH 10ADR -10.)
		         (+ (LSH 6 33.)
			    (LSH 11NO 32)
			    (LSH (LSH 11ADR -2) 12)
			    (1- 11SIZE)))
       (ERROR '|T11MP CALL FAILED|)))

;;; These read and write the "wrong" Unibus
(DECLARE (FIXNUM TALK-READ-LOCAL-UNIBUS FIXNUM))
(DEFUN TALK-READ-LOCAL-UNIBUS (ADR)
  (LET ((WORD (EXAMINE (+ TEN11-ADDRESS (LSH (LOGAND 177777 ADR) -4)))))
    (DECLARE (FIXNUM WORD))
    (LOGAND 177777 (LSH WORD (COND ((ZEROP (LOGAND 2 ADR)) -24)
				   (T -4))))))

(DECLARE (NOTYPE (TALK-WRITE-LOCAL-UNIBUS FIXNUM FIXNUM)))
(DEFUN TALK-WRITE-LOCAL-UNIBUS (ADR VAL)
  (DEPOSIT (+ TEN11-ADDRESS (LSH (LOGAND 177777 ADR) -4))
	   (COND ((ZEROP (LOGAND 2 ADR))
		  (+ (LSH VAL 24) 4))
		 ((+ (LSH VAL 4) 10))))
  NIL)

;;; Read and write the Unibus on the other machine (if using two machines)
(DECLARE (FIXNUM (TALK-READ-UNIBUS FIXNUM))
	 (NOTYPE (TALK-WRITE-UNIBUS FIXNUM FIXNUM)))

(DEFUN TALK-READ-UNIBUS (ADR)
  (LET ((PATH TALK-PATH))
    (COND ((EQ PATH 'TEN11) (TALK-READ-LOCAL-UNIBUS ADR))
	  ((EQ PATH 'BUSINT)
	   (TALK-WRITE-LOCAL-UNIBUS (+ TALK-DEBUG-ADDRESS 4)	;Store high bit of address
				    (LSH ADR -17.))
	   (TALK-WRITE-LOCAL-UNIBUS (+ TALK-DEBUG-ADDRESS 6)	;Store rest of address
				    (LSH ADR -1))
	   (TALK-READ-LOCAL-UNIBUS TALK-DEBUG-ADDRESS))
	  ((ERROR '|BAD PATH - TALK-READ-UNIBUS| PATH)))))

(DEFUN TALK-WRITE-UNIBUS (ADR VAL)
  (LET ((PATH TALK-PATH))
    (COND ((EQ PATH 'TEN11) (TALK-WRITE-LOCAL-UNIBUS ADR VAL))
	  ((EQ PATH 'BUSINT)
	   (TALK-WRITE-LOCAL-UNIBUS (+ TALK-DEBUG-ADDRESS 4)	;Store high bit of address
				    (LSH ADR -17.))
	   (TALK-WRITE-LOCAL-UNIBUS (+ TALK-DEBUG-ADDRESS 6)	;Store rest of address
				    (LSH ADR -1))
	   (TALK-WRITE-LOCAL-UNIBUS TALK-DEBUG-ADDRESS VAL))
	  ((ERROR '|BAD PATH - TALK-READ-UNIBUS| PATH)))))