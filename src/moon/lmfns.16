(declare (special STANDARD-OUTPUT linel genfns genvars genconsts)	;-*-LISP-*-
	 (setq open-code-map-switch t))

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(defun map-over-all-packages (fcn &optional (pkg pkg-system-package))
  (funcall fcn pkg)
  (do l (si:pkg-subpackages pkg) (cdr l) (null l)
    (map-over-all-packages fcn (car l))))

(DEFUN PHLATC (F X)
	(COND (F (FLATC X))
	      ((+ 2 (FLATC (CAR X))))))

(DEFUN PRINCRUFT (M L)
    (PROG (MX NL NC AT LL (sp 0))
       (COND (L (SETQ L (COND ((SETQ AT (ATOM (CAR L)))
			       (SORT L (FUNCTION ALPHALESSP)))
			      ((SORTCAR L (FUNCTION ALPHALESSP)))))
		(TERPRI)
		(TERPRI)
		(PRINC M)
		(TERPRI)
		(TERPRI)
		(SETQ MX (DO ((X L (CDR X))
			      (I 0 (MAX I (PHLATC AT (CAR X)))))
			     ((NULL X) I)))
		(SETQ NL (// LINEL (+ MX 4)))
		(SETQ NC (// (+ (LENGTH L) (1- NL)) NL))
		(DO ((I NL (1- I)))
		    ((PROG2 (SETQ LL (CONS L LL)) (= I 1)))
		    (DO ((J NC (1- J)))
			((OR (NULL L) (ZEROP J)))
			(SETQ L (CDR L))))
		(SETQ LL (NREVERSE LL))
		(DO ((I NC (1- I)) (SP 0 0))
		    ((ZEROP I))
		    (MAP (FUNCTION (LAMBDA (X)
			    (COND ((CAR X)
				(PRINC '"    ")
				(DO ((J SP (1- J)))
				    ((ZEROP J))
				    (PRINC '" "))
				(COND (AT (PRINC (CAAR X)))
				      (T (PRINC (CDAAR X))
					 (PRINC '" ")
					 (PRINC (CAAAR X))))
				(SETQ SP (- MX (PHLATC AT (CAAR X))))
				(RPLACA X (CDAR X))))))
			 LL)
		    (TERPRI))))))

(defun genfns (&optional (linel 95.))
  ((lambda (STANDARD-OUTPUT genfns genvars genconsts)
      (map-over-all-packages (function genfns-p))
      (close STANDARD-OUTPUT))
   (open "lmdoc;lmfns >" '(out))
   nil
   nil
   nil)
  t)
  
(declare (setq open-code-map-switch nil))  ;compiler has wierd ideas about mapatoms

(declare (special pkg)) ;sigh

(defun genfns-p (pkg)
  (setq genfns nil genvars nil genconsts nil)  ;fns, vars, constants
  (mapatoms (function (lambda (x)
                         (cond ((eq (%p-contents-offset x 4) ;make sure it's really
                                    pkg)                     ; in this package!
                                (and (fboundp x)
                                     (setq genfns (cons-in-area x genfns si:fasl-table-area)))
                                (cond ((get x 'compiler:system-constant)
                                       (setq genconsts
                                             (cons-in-area x genconsts si:fasl-table-area)))
                                      ((or (boundp x) (get x 'special))
                                       (setq genvars 
                                             (cons-in-area x genvars si:fasl-table-area))))))))
            pkg nil)  ;Just this package
  (princruft1 "Functions" pkg genfns)
  (princruft1 "Variables" pkg genvars)
  (princruft1 "System Constants" pkg genconsts)
  (setq genfns nil genvars nil genconsts nil)
  (store (area-free-pointer si:fasl-table-area) 0))

(defun princruft1 (type pkg l &aux fefs arrays macros ucodes interps others y)
  (princruft (format-string nil "~a in ~a" type pkg) l))
