;;; CADR DISPATCH TEST                                           -*-LISP-*-

(INCLUDE |LMDOC;.COMPL PRELUD|)
;(DECLARE (EVAL (READ)))
;(PROGN (LOAD '(MACROS > DSK LISPM))
;       (LOAD '(DEFMAC FASL DSK LISPM2))
;       (LOAD '(LMMAC > DSK LISPM2)))

(IF-FOR-MACLISP (DECLARE (EVAL (READ))))
(IF-FOR-MACLISP (DEFUN **STRING** MACRO (X) `',(CADR X)) ;Bubbles in my brain
)

(INCLUDE ((LMCONS)CADMAC >))

;; Fill all of D memory with its own address, and no RPN bits
(DEFUN CC-FILL-D-MEM-W-ADR ()
  (DO ((I 0 (1+ I)))
      ((= I 2048.))
    (DECLARE (FIXNUM I))
    (CC-WRITE-D-MEM I I)))

;; Read back all possible bytes with MROT=0, make sure right address
;; comes back into the PC.  Here we always use a disp addr of 0.
(DEFUN CC-TEST-DISPATCH-BYTES ()
  (DO ((BYTL 0 (1+ BYTL))
       (MXVAL 1 (* MXVAL 2)))
      ((= BYTL 8))
    (DECLARE (FIXNUM BYTL MXVAL))
    (DO ((VAL 0 (1+ VAL))
	 (PC))
	((= VAL MXVAL))
      (DECLARE (FIXNUM VAL PC))
      (CC-WRITE-MD (- VAL MXVAL)) ;Turn on extra bits to detect improper masking
      (CC-EXECUTE CONS-IR-OP CONS-OP-DISPATCH	;Execute a dispatch
		  CONS-IR-M-SRC CONS-M-SRC-MD
		  CONS-IR-DISP-BYTL BYTL
		  CONS-IR-DISP-ADDR 0)
	     ;At this point the disp is in IR but has not yet been executed.
     (CC-CLOCK)				;Clock it so PC loads from disp mem
     (SETQ PC (CC-READ-PC))
     (COND ((NOT (= PC VAL))		;Read wrong location
	    (TERPRI)
	    (PRINC '|Dispatch error, BYTL=|)
	    (PRIN1 BYTL)
	    (PRINC '|, M=|)
	    (PRIN1 (LOGAND 37777777777 (- VAL MXVAL)))
	    (PRINC '|, DPC=|)
	    (PRIN1 PC)
	    (PRINC '|, but should be |)
	    (PRIN1 VAL))))))
