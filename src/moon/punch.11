;-*- MODE:lisp; PACKAGE: UA -*- machine
;Punch locations 0-777 of I-MEM in PROM format.
;8-bit chunks.  Bit 46 is missing.  Bit 48 is odd parity.
;Output format is 6 files each with a title line, alternating
;octal address and data, and the symbol END at the end.
;Note that the damned address lines are inverted.

(defun punch (&optional (dir-fn1 (string-append user-id ";prom")))
  (mapc (function punch-1) ;args are byte specifier, byte extractor fcn, title/file-name-2
	'(	0010	1010	2010	3010	4010	nil)
	'(	ldb	ldb	ldb	ldb	ldb	ghb)
	 (list  dir-fn1 dir-fn1 dir-fn1 dir-fn1 dir-fn1 dir-fn1)
	'(	"1E17"	"1E19"	"1D16"	"1C20"	"1B17"	"1B19"))
  t)

(defun punch-1 (byte extract-fn dir-fn1 fn2)
  ((lambda (outfile)
	(princ "PROM location " outfile)
	(princ fn2 outfile)
	(terpri outfile)
	(do i 777 (1- i) (< i 0)
	  (print (- 777 i) outfile)
	  (prin1 (funcall extract-fn byte (or (i-mem i) 0)) outfile)
	  (tyo 40 outfile)) ;G D F A
	(terpri outfile)
	(princ "END " outfile)
	(close outfile))
   (open (string-append dir-fn1 " " fn2) '(print ascii block))))

(defun ghb (ignore word)
  (+ (ldb 5006 word)
     (lsh (ldb 5701 word) 6)
     (lsh (parity word) 7)))

;Parity of a bignum, return 1 if parity is even
(defun parity (word)
  (do ((ppss 5701 (- ppss 100))
       (par 1))
      ((= ppss -77) par)
    (or (zerop (ldb ppss word))
	(setq par (- 1 par)))))
