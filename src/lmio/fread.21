;;-*- Mode: LISP; Package: MICRO-ASSEMBLER -*-

(defun read-ucode (&optional (file "LCADR;UCADR >"))
  (let ((stream (open file '(in block))))
     (eval (fread stream))
     (close stream)))

;; Fast, simple reader for reading in ucode

(declare (special special-stream
		  special-line-in special-line-in-index special-line-in-length
		  special-unrchf 
		  special-char-type-table
                  special-fread-string))

;special-char-type-table 
; nil  -> break char
; t -> whitespace
; -n  -> number, ascii code n
; +n  -> character. n is code to store (identical to ascii except for upcasing).

(defmacro digitp (char) `(not (or (< ,char 60) (> ,char 71))))

(defmacro symbol-or-numberp (char)
  `(or (digitp ,char) (not (memq ,char '(40 50 51 56 73 47 137 211 212 213 214 215)))))

;(defmacro eat-whitespace (stream)
;  `(do ((char (tyi ,stream) (tyi ,stream)))
;       ((not (memq char '(40 211 212 213 214 215))) char)))

(defmacro fread-eat-whitespace nil
  `(prog nil
    l	 (cond ((eq (setq char-type (ar-1 special-char-type-table (setq char (fread-tyi))))
		    t)
		(go l)))
	 (return char)))

(defmacro fread-tyi nil
  `(prog nil 
	(cond (special-unrchf
	       (return (prog1 special-unrchf (setq special-unrchf nil)))))
     l  (cond ((>= special-line-in-index special-line-in-length)
	       (return-array special-line-in)
	       (multiple-value (special-line-in eof-flag)
			    (funcall special-stream ':line-in))
	       (and eof-flag (ferror nil "Premature EOF in fast reader"))
	       (setq special-line-in-length (array-active-length special-line-in))
	       (setq special-line-in-index 0)
	       (go l)))
       (return (prog1 (ar-1 special-line-in special-line-in-index)
		      (setq special-line-in-index (1+ special-line-in-index))))))

(defun fread (&optional (stream nil) (eofval -1) &aux special-stream)
   (multiple-value (special-stream eofval)
                   (si:decode-read-args stream eofval)) 
   (cond ((not (boundp 'special-char-type-table))
	  (setq special-char-type-table (make-array nil 'art-q 216))
	  (do ch 0 (1+ ch) (= ch 216)			       ;initialize to self
	      (as-1 ch special-char-type-table ch))
	  (do ch 60 (1+ ch) (= ch 72)			       ;numbers
	      (as-1 (minus ch) special-char-type-table ch))
	  (do ch '(#/( #/) #/. #// #/; #/' #/_) (cdr ch) (null ch)      ;breaks
	      (as-1 nil special-char-type-table (car ch)))
	  (do ch '(40 211 212 213 214 215) (cdr ch) (null ch)  ;white-space
	      (as-1 t special-char-type-table (car ch)))
	  (do ch 141 (1+ ch) (= ch 173)
	      (as-1 (- ch 40) special-char-type-table ch))
          (setq special-fread-string (make-array nil 'art-string 200 nil '(0)))))
   (setq special-line-in-length
	   (array-active-length (setq special-line-in (funcall special-stream ':line-in))))
   (setq special-line-in-index 0)
   (setq special-unrchf nil)
   (unwind-protect (fread-1)
	(return-array special-line-in)
	(setq special-line-in nil)))


(defun fread-1 nil
 (prog (idx ob char char-type number-possible dec oct acc number-finished sign
        digit-seen eof-flag)
       (setq idx -1 number-possible t dec 0 oct 0 sign 1)
  l00  (setq char (fread-eat-whitespace))
  l0   (cond ((not (numberp char-type))		;predicate true if not symbol constit.
	      (cond ((= char 56)			;dot
		     (cond ((and number-possible (not (= idx -1)))
			    (setq oct dec)
                            (setq char-type 56)	        ;can be symbol const
			    (setq number-finished t))	;error check
			   (t (break dot-context-error))))	;legit dots read at read-list
		    ((= char 137)			;underline (old leftarrow)
		     (setq oct (ash oct (fread-1)))
		     (go x))
		    ((= char #// )			;slash
                     (setq number-possible nil)
		     (setq char-type (fread-tyi))
                     (go s))
		    ((> idx -1)
		     (setq special-unrchf char)		;in middle of somthing,
		     (go x))
		    ((= char 50)			;Open-paren
		     (go read-list))
		    ((= char 47)			;Single-quote
		     (return (list 'quote (fread-1))))
		    ((= char 73)			;Semi-colon
		     (return-array special-line-in)
		     (setq special-line-in-length
			   (array-active-length
			    (setq special-line-in
				  (funcall special-stream ':line-in))))
		     (setq special-line-in-index 0)
		     (go l00))
                    ((= char 51)
                     (break unexpected-close))
                    (t (go l00))))   ;flush it.
            (number-possible 
             (cond ((> char-type 0)		;true if not digit
                    (cond ((and (= char 53) (null digit-seen)))
                          ((and (= char 55) (null digit-seen))
                           (setq sign (minus sign)))
                          (t (setq number-possible nil))))
                   (t 
                    (cond (number-finished (break no-floating-point)))
                    (setq digit-seen t)
                    (setq dec (+ (* 10. dec) (setq ob (- char 60)))
                          oct (+ (* 10 oct) ob))))))
  s   (aset (abs char-type) special-fread-string (setq idx (1+ idx)))
      (setq char-type (ar-1 special-char-type-table (setq char (fread-tyi))))
      (go l0)
  read-list
      (setq acc (cons (fread-1) acc))
  read-list0
      (setq char (fread-eat-whitespace))
      (cond ((eq char 51)			;close
             (return (nreverse acc)))
            ((= char 73)			;Semi-colon
             (return-array special-line-in)
             (setq special-line-in-length
                   (array-active-length
                    (setq special-line-in
                          (funcall special-stream ':line-in))))
             (setq special-line-in-index 0)
             (go read-list0))
            ((eq char #/.)
             (setq ob (cons (car acc) (fread-1)))
             (cond ((not (= (setq char (fread-eat-whitespace))
                            51))
                    (break dot-closing-error)))
             (return ob)))
      (setq special-unrchf char)
      (go read-list)
   x  (cond ((and number-possible digit-seen)		  ; return it.
							;digit seen so it wins on +, -
             (return (* sign oct)))
            (t (cond ((not (= idx -1))
                      (store-array-leader (1+ idx) special-fread-string 0)
                      (multiple-value (ob oct)
                                (intern-soft special-fread-string))
                      (cond ((null oct)
                             (setq ob (intern (let ((s (make-array nil 'art-string (1+ idx))))
                                                 (copy-array-contents special-fread-string
                                                                       s)
                                                 s)))))))
               (return ob)))))

