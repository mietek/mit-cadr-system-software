;;; -*- Mode:Lisp; Package:System-internals; Base 8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; Edited 13 JUL 79 by rWG to fix expt-hard

;;; Integer square-root
(defun isqrt (n)
  (cond ((<= n 0) 0)  ;Otherwise, it would loop, which isn't nice
	(t (do ((g (ash 1 (ash (1- (haulong n)) -1))
		   (+ g e))
		(e))
	       ((zerop (setq e (// (- n (* g g))
				   (ash g 1))))
		;; We are now within 1, but might be too high
		(cond ((> (* g g) n) (1- g))
		      (t g)))))))

(defun sqrt (n)
   (setq n (float n))
   (cond ((< n 0.0)(ferror t "~S was negative - SQRT" n))
         ((= n 0.0) 0.0)
         (t (let ((f (+ n 0.0))
                  (i2 (%float-double 0 1))
                  (exp (- (%p-ldb 1013 n) 1776)))
               (%p-dpb 2000 1013 f)
               (%p-dpb (+ 2000 (cond ((oddp exp)
                                      (1+ (dpb (ldb 0127 exp) 0027 exp)))
                                     (t (dpb (ldb 0127 exp) 0027 exp))))
                       1013 i2)
               (do ((i 0 (1+ i))
                    (an (* i2 (+ .4826004 f (cond ((oddp exp) -.25)
                                                  (t 0.0))))))
                   ((= i 4) an)
                 (setq an (* .5 (+ an (// n an)))))))))

(defun log (n)
      (if (small-floatp n) (small-float (log-aux n)) (log-aux n)))

(defun log-aux (n &aux (f (+ n 0.0)))
      (cond (( n 0) (cond ((zerop n)(ferror t "Zero argument to LOG"))
			   ((minusp n)(ferror t "~S Argument was negative to LOG" n))))
	    ((= n 1) 0.0)
	    (t (let ((i (- (%p-ldb 1013 f) 2001)))	;i gets the base 2 exponent
		 (%p-dpb 2001 1013 f)		;f gets the mantissa (1.0 to 2.0)
		 (setq f (// (- f 1.414213562374)
			     (+ f 1.414213562374)))
		 (setq f (+ .5
			    (* f (+ 2.885390073
				    (* (setq f (* f f))
				       (+ .9618007623
					  (* f (+ .5765843421
						  (* .4342597513 f)))))))))
		 (* .69314718056 (+ i f))))))

(defun exp (n)
      (if (small-floatp n)
	  (let ((m (* n 1.442695s0))			;lg e
		(f))
	    (setq n (fix m) f (- m n))
	    (ash (+ .5s0 (// f			;no doubt  a simpler approx for small-floats
			     (+ 9.954596s0
				(* .034657359s0 f f)
				(- f)
				(// -617.97227s0
				    (+ (* f f) 87.4174972s0)))))
		 (1+ n)))
	  (let ((m (* n 1.44269504))			;lg e
		(f))
	    (setq n (fix m) f (- m n))
	    (ash (+ .5 (// f			;replace this comment by a reference!
			   (+ 9.95459578
			      (* .03465735903 f f)
			      (- f)
			      (// -617.97226953
				  (+ (* f f) 87.417497202)))))
		 (1+ n)))))

(defun cosd (ang)
   (cos (* ang .0174532926)))

(defun sind (ang)
   (sin (* ang .0174532926)))

(defun cos (x)
   (sin (+ x 1.570796326)))

(defun sin (x)
   (cond ((< (abs x) 1.0s-3) (float x))
         (t (min (max (sin-aux x) -1.0) 1.0))))

(defun sin-aux (x &aux (pi%2 1.570796326))
   (let ((frac (// (abs x) pi%2))
         (d)
         (sign (cond ((> x 0) 1)
                     ((< x 0) -1))))
      (setq d (fix frac))
      (setq frac (- frac d))
      (selectq (ldb 0002 d)
          (1 (setq sign (minus sign) frac (- frac 1)))
          (2 (setq sign (minus sign)))
          (3 (setq frac (- frac 1))))
      (let ((y (* frac sign))
            (y2 (* frac frac)))
           (* y (+ 1.5707963185
                   (* y2 (+ -.6459637111
                            (* y2 (+ .07968967928
                                     (* y2 (+ -.00467376557
                                              (* y2 .00015148419))))))))))))
(defun atan2 (y x)
   (cond ((< y 0) (- (atan (- y) x)))
	 (t (atan y x))))

(defun atan (y x)
   (setq x (float x)
	 y (float y))
   (let ((absx (abs x))
         (absy (abs y))
         (temp)
         (temp2)
         (ans -0.004054058))
     (setq temp (// (- absy absx) (+ absy absx))
           temp2 (* temp temp))
     (do ((l '( 0.0218612288 -0.0559098861  0.0964200441
               -0.139085335   0.1994653499 -0.3332985605 0.9999993329)
             (cdr l)))
         ((null l))
       (setq ans (+ (* ans temp2) (car l))))
     (setq ans (* ans temp))
     (setq temp (abs ans))
     (cond ((or (>= temp .7855) (< temp .7853))
            (setq ans (+ ans 0.7853981634)))
           ((< ans 0) (setq ans (// absy absx)))
           (t (setq ans (+ (// absx absy) 1.5707963268))))
     (setq temp ans
           ans (- 3.1415926536 ans))
     (cond ((>= x 0) (setq temp (prog2 nil ans (setq ans temp)))))
     (setq temp (* temp 2))
     (cond ((< y 0)(setq ans (+ ans temp))))
     ans))


;;; Hard cases of ^ -- non-fixnum base.
;  Old code -- rewritten by rWG July 1979
;  Binary-expt always uses ASH now since works for bignums.

;  (defun expt-hard (*base *exp)		;numerous type conversion bugs
;      (cond ((zerop *base)
;  	   (and (zerop *exp) (ferror nil "(^ ~S ~S) not defined" *base *exp))
;  	   *base)
;  	  ((zerop *exp)
;  	   (selectq (typep *base)
;               (:fixnum 1)
;  	     (:bignum 1)
;  	     (:flonum 1.0)
;  	     (:small-flonum 1.0s0)))
;  	  ((< *exp 0) (// 1 (expt-hard *base (- *exp))))
;  	  ((fixp *exp)
;  	   (if (bigp *exp)
;  	       (slow-binary-expt *base *exp)
;  	       (fast-binary-expt *base *exp)))
;  	  (t (exp (* *exp (log *base))))))
;  
;  (defun fast-binary-expt (*base *exp)	;obsolete
;      (do ((*ans 1))
;  	((zerop *exp) *ans)
;        (if (bit-test *exp 1) (setq *ans (* *ans *base)))
;        (setq *base (* *base *base)
;  	    *exp (lsh *exp -1))))

(defun expt-hard (*base *exp)
      (setq *base (number-meets *base *exp))
      (cond ((zerop *base)
	     (or (plusp *exp) (ferror nil "(^ ~S ~S) not defined" *base *exp))	;eg 0^-1
	     *base)
	    ((< *exp 0) (// 1 (expt-hard *base (- *exp))))
	    ((fixp *exp)
	     (slow-binary-expt *base *exp))	;ash ucoded JUL 79 (?)
	    (t (exp (* *exp (log *base))))))


(defun slow-binary-expt (*base *exp)
      (do ((ans (if (oddp *exp) *base (number-meets 1 *base))	;coerce to right flavor of 1!
		(if (oddp *exp) (* ans *base) ans)))
	  ((zerop (setq *exp (ash *exp -1))) ans)
	(setq *base (* *base *base))))		;to avoid oflo, procrastinate squaring
	
(defun number-meets (ans influencer)		;ans in stronger type
      (min ans (max ans influencer)))

