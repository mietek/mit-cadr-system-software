; -*- Mode:Lisp; Package:Cold; Lowercase:T; Base:8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;Loader of QFASL files into cold-loads

(declare (special evals-to-be-sent-over
		  last-fasl-eval  ;the element of evals-to-be-sent-over created by the last 
				  ; fasl-op-eval.
		  cold-list-area
		  current-function))		;debugging aid

(declare (special fasl-table fasl-table-fill-pointer fasl-return-flag 
		  fasl-group-bits fasl-group-type fasl-group-length fasl-group-flag
		  q-fasl-group-dispatch m-fasl-group-dispatch fasl-group-dispatch-size))

(declare (special qfasl-binary-file fdefine-file-symbol))

;Q-FASL-xxxx refers to functions which load into the cold load, and
; return a "Q", i.e. a list of data-type and address-expression.
;M-FASL-xxxx refers to functions which load into the current Lisp environment
; (M used to be for Maclisp), and return a Lisp object.
; However M-objects still have all their symbols in the SYM package.
;In the FASL-TABLE, each entry in both the prefix and the main part
; is a list whose car is the M (lisp) value and whose cadr is either
; NIL or the Q-value.  If it needs a Q-value and one hasn't been
; computed yet, it will compute one, but this may put it in the wrong area.

;These functions are used to refer to the FASL-TABLE

;Get a Q object from FASL table
(defun q-arft (x)
  (cond ((atom (setq x (aref fasl-table x)))
	 (ferror nil "not a q - q-arft"))
	((cadr x))
	(t (rplaca (cdr x) (make-q-list 'sym:init-list-area (car x)))
	   (cadr x))))

;Get an M object
(defun m-arft (x)
  (cond ((atom (setq x (aref fasl-table x)))
	 (ferror nil "not a q - m-arft"))
	(t (car x))))

;Store an M object
(defsubst m-asft (d x)
  (aset (list d nil) fasl-table x))

;Store both M and Q objects
(defsubst m-q-asft (d q x)
  (aset (list d q) fasl-table x))

;(DEFPROP USER/:SOURCE-FILE-NAME (USER SOURCE-FILE-NAME) PACKAGE-PATH)

(defun cold-fasload (filespec &aux qfasl-binary-file fdefine-file-symbol)
  (unwind-protect (progn
      (or (boundp 'q-fasl-group-dispatch) (initialize-fasl-environment))
      (setq filespec (file-expand-pathname filespec))	;Canonicalize
      (format t "~&Cold-fasload ~A" filespec)
      (setq qfasl-binary-file (open filespec '(in fixnum)))
      (multiple-value-bind (ignore fgs) (si:get-file-symbols filespec)
	(setq fdefine-file-symbol (qintern fgs))	;Make file-group-symbol
	(vstore-contents (+ fdefine-file-symbol 4)	;Set package cell to FILES
			 (qintern 'sym:files)))
      (or (and (= (qfasl-nibble) 143150)
	       (= (qfasl-nibble) 71660))
	  (ferror nil "~A is not a QFASL file" filespec))
      (do ()
	  ((eq (qfasl-whack) 'eof))))
    (and qfasl-binary-file (close qfasl-binary-file))))

;This is the function which gets a 16-bit "nibble" from the fasl file.
(defun qfasl-nibble ()
  (funcall qfasl-binary-file ':tyi))

;This function processes one "whack" (independent section) of a fasl file.
(defun qfasl-whack ()
  (let ((fasl-table-fill-pointer sym:fasl-table-working-offset)
	(fasl-return-flag nil))
    (or (boundp 'fasl-table)
	(setq fasl-table (make-array nil 'art-q sym:length-of-fasl-table)))
    (fillarray fasl-table '(nil))
    (initialize-qfasl-table)
    (do () (fasl-return-flag)
      (qfasl-group nil))
    fasl-return-flag)) 

;Initialize FASL-TABLE prefix
(defun initialize-qfasl-table ()
  (aset '(sym:nr-sym nil) fasl-table sym:fasl-symbol-head-area)
  (aset '(sym:p-n-string nil) fasl-table sym:fasl-symbol-string-area)
  (aset '(sym:control-tables nil) fasl-table sym:fasl-array-area) ;I GUESS
  (aset '(sym:macro-compiled-program nil) fasl-table sym:fasl-frame-area)
  (aset '(sym:init-list-area nil) fasl-table sym:fasl-list-area) ;Not FASL-CONSTANTS-AREA!!
  (aset '(sym:fasl-temp-area nil) fasl-table sym:fasl-temp-list-area)
  (aset '(sym:micro-code-exit-area nil) fasl-table sym:fasl-micro-code-exit-area))

(defun initialize-fasl-environment ()
  (setq fasl-group-dispatch-size (length sym:fasl-ops))
  (setq q-fasl-group-dispatch (make-array nil 'art-q fasl-group-dispatch-size))
  (setq m-fasl-group-dispatch (make-array nil 'art-q fasl-group-dispatch-size))
  (do ((i 0 (1+ i))
       (l sym:fasl-ops (cdr l))
       (package (pkg-find-package "cold"))
       (m-op) (q-op))
      ((= i fasl-group-dispatch-size))
    (setq m-op (intern (format nil "M-~A" (car l)))
	  q-op (intern (format nil "Q-~A" (car l))))
    (aset m-op m-fasl-group-dispatch i)
    (aset q-op q-fasl-group-dispatch i)))

;Process one "group" (a single operation)
;Argument is NIL for Q-FASL, T for M-FASL.
(defun qfasl-group (m-p &aux fasl-group-flag fasl-group-bits
			     fasl-group-type fasl-group-length)
  (setq fasl-group-bits (qfasl-nibble))
  (or (bit-test sym:%fasl-group-check fasl-group-bits)
      (ferror nil "fasl-group-nibble-without-check-bit"))
  (setq fasl-group-flag (bit-test sym:%fasl-group-flag fasl-group-bits)
	fasl-group-length (ldb sym:%%fasl-group-length fasl-group-bits))
  (and (= fasl-group-length 377)
       (setq fasl-group-length (qfasl-nibble)))
  (setq fasl-group-type (logand sym:%fasl-group-type fasl-group-bits))
  (or (< fasl-group-type fasl-group-dispatch-size)
      (ferror nil "~O erroneous fasl group type" fasl-group-type))
  (funcall (aref (if m-p m-fasl-group-dispatch q-fasl-group-dispatch) fasl-group-type)))

;Get next nibble out of current group
(defun qfasl-next-nibble ()
  (cond ((zerop fasl-group-length) (ferror nil "fasl-group-overflow"))
	(t (setq fasl-group-length (1- fasl-group-length))
	   (qfasl-nibble))))

;Get next value for current group.  Works by recursively evaluating a group.
;This one gets a Q value
(defun q-fasl-next-value ()
  (q-arft (qfasl-group nil)))

;This one gets an M value
(defun m-fasl-next-value ()
  (m-arft (qfasl-group t)))

;FASL-OP's that create a value end up by calling this.  The value is saved
;away in the FASL-TABLE for later use, and the index is returned (as the 
;result of QFASL-GROUP).
;This one enters an M object and a Q
(defun m-q-enter-fasl-table (m q)
  (cond ((not (< fasl-table-fill-pointer sym:length-of-fasl-table))
	 (ferror nil "fasl table overflow"))
	(t
	 (m-q-asft m q fasl-table-fill-pointer)
	 (prog1 fasl-table-fill-pointer
		(setq fasl-table-fill-pointer (1+ fasl-table-fill-pointer))))))

;This one enters an M value
(defun m-enter-fasl-table (v)
  (cond ((not (< fasl-table-fill-pointer sym:length-of-fasl-table))
	 (ferror nil "fasl table overflow"))
	(t
	 (m-asft v fasl-table-fill-pointer)
	 (prog1 fasl-table-fill-pointer
		(setq fasl-table-fill-pointer (1+ fasl-table-fill-pointer))))))

;--M-FASL ops

(defun m-fasl-op-noop () 0)

(defun m-fasl-op-index () (qfasl-next-nibble))

(defun m-fasl-op-string ()
  (m-enter-fasl-table (m-fasl-pname)))

(defun m-fasl-op-symbol ()
  (m-enter-fasl-table (cond (fasl-group-flag (make-symbol (m-fasl-pname)))
			    (t (intern (m-fasl-pname) sym-package)))))

(defun m-fasl-op-package-symbol ()
  (do ((i 0 (1+ i))
       (path nil)
       (sym)
       (len (qfasl-next-nibble)))
      ((= i len) 
       (setq path (nreverse path))
       (cond ((or (eq (car path) 'sym:si)
		  (eq (car path) 'sym:system-internals)) ;don't get faked out
	      (m-enter-fasl-table (cadr path)))
	     (t
	      (setq sym (intern (format nil "~{~A~^:~}" path) sym-package))
	      (putprop sym path 'package-path)
	      (m-enter-fasl-table sym))))
    (push (intern (m-fasl-next-value) sym-package) path)))  ;fasl-value is string

(defun m-fasl-pname ()	;Return a string
  (let ((str (make-array nil 'art-string (* fasl-group-length 2)))
	tem)
    (dotimes (i fasl-group-length)
      (setq tem (qfasl-next-nibble))
      (aset tem str (+ i i))
      (aset (setq tem (lsh tem -8)) str (+ i i 1)))
    (and (eq tem 200) (adjust-array-size str (1- (array-length str)))) ;padded
    str))

;Generate a FIXNUM (or BIGNUM) value.
(defun m-fasl-op-fixed ()
  (do ((pos (* (1- fasl-group-length) 20) (- pos 20))
       (c fasl-group-length (1- c))
       (ans 0))
      ((zerop c) (cond (fasl-group-flag (setq ans (minus ans))))
		 (m-enter-fasl-table ans))
    (setq ans (dpb (qfasl-next-nibble) (+ (lsh pos 6) 20) ans))))

(defun m-fasl-op-float ()
  (q-fasl-op-float))

(defun m-fasl-op-list () (q-fasl-op-list))

(defun m-fasl-op-temp-list () (m-fasl-op-list1))

(defun m-fasl-op-list1 ()
  (do ((list-length (qfasl-next-nibble) (1- list-length))
       (lst nil) (adr) (tem))
      ((zerop list-length)
       (m-q-enter-fasl-table lst '**screw**))
    (cond ((and fasl-group-flag (= list-length 1)) ;dotted
	   (rplacd adr (m-fasl-next-value)))
	  (t (setq tem (ncons (m-fasl-next-value)))
	     (and adr (rplacd adr tem))
	     (or lst (setq lst tem))
	     (setq adr tem)))))

;--Q-FASL ops

(defun q-fasl-op-noop () 0)

(defun q-fasl-op-index () (qfasl-next-nibble))

(defun q-fasl-op-string ()
  (let ((str (m-fasl-pname)))
    (m-q-enter-fasl-table str (store-string 'sym:p-n-string str))))

(defun q-fasl-op-package-symbol ()
  (let ((x (m-fasl-op-package-symbol)))
    (q-arft x)
    x))

(defun q-fasl-op-symbol ()
  (let ((sym (m-fasl-pname)))
    (m-q-enter-fasl-table (if fasl-group-flag (make-symbol sym)
			      (setq sym (intern sym sym-package)))
			  (if fasl-group-flag ;uninterned
			      (store-symbol-vector sym 'sym:nr-sym)
			      (qintern sym)))))

(defun q-fasl-op-fixed ()
  (let ((x (m-fasl-op-fixed)))
    (q-arft x)
    x))

(defun q-fasl-op-float ()
  (or fasl-group-flag (ferror nil "large flonums not supported"))
  (let ((num (%make-pointer dtp-small-flonum
			    (%logdpb (qfasl-next-nibble) 2010 (qfasl-next-nibble)))))
    (m-q-enter-fasl-table num (make-small-flonum num))))

;;; Total kludgery.  FASL-OP-TEMP-LIST makes an M list, assumed to be
;;; going to get fed to something like FASL-OP-ARRAY or FASL-OP-EVAL.
;;; FASL-OP-LIST, on the other hand, makes a Q list, assumed to
;;; be going to be used for something like a macro.  In either case the
;;; area specification in the FASL table is ignored.
;;; Hopefully this kludgery stands some chance of working.

(defun q-fasl-op-temp-list ()
  (m-fasl-op-list))
       
(defun q-fasl-op-list ()
  (let ((area cold-list-area)
	(list-length (qfasl-next-nibble))
	lst c-code maclisp-list fasl-idx)
    (or (memq area sym:list-structured-areas)
	(ferror nil "q-fasl-op-list in non-list-structured area"))
    (setq lst (allocate-block area list-length))
    (do ((adr lst (1+ adr))
	 (len list-length (1- len)))
	((zerop len))
      (setq c-code (cond ((and fasl-group-flag (= len 2)) sym:cdr-normal)
			 ((and fasl-group-flag (= len 1)) sym:cdr-error)
			 ((= len 1) sym:cdr-nil)
			 (t sym:cdr-next)))
      (setq fasl-idx (qfasl-group nil))
      (vwrite-cdr adr c-code (q-arft fasl-idx))
      (setq maclisp-list (nconc maclisp-list
				(if (and fasl-group-flag (= len 1)) (m-arft fasl-idx)
				    (ncons (m-arft fasl-idx))))))
    (m-q-enter-fasl-table maclisp-list (vmake-pointer sym:dtp-list lst))))

;Array stuff

;FASL-OP-ARRAY arguments are
; <value>  Area 
; <value>  Type symbol
; <value>  The dimension or dimension list (use temp-list)
; <value>  Displace pointer (NIL if none)
; <value>  Leader (NIL, number, or list) (use temp-list)
; <value>  Index offset (NIL if none)
; <value>  Named-structure (only present if flag bit set)
(defun q-fasl-op-array ()
  (let ((flag fasl-group-flag)
	(area (m-fasl-next-value))
	(type-sym (m-fasl-next-value))
	(dims (m-fasl-next-value))
	(displaced-p (m-fasl-next-value))  ;if non-nil, will it work?
	(leader (m-fasl-next-value))
	(index-offset (m-fasl-next-value)) ;if non-nil, will it work?
	(named-structure nil)
	(array nil) (data-length nil) (adr nil))
     (setq area 'sym:control-tables) ;kludge, may not be needed any more
     (cond (flag
	    (setq named-structure (m-fasl-next-value))))
     (and (not (atom leader))
	  (setq leader (mapcar (function (lambda (x) (make-q-list 'sym:init-list-area x)))
			       leader)))
     (setq array (init-q-array-named-str area
					 nil  ;return list of address and data-length
					 index-offset
					 type-sym
					 dims
					 displaced-p
					 leader
					 named-structure))
     (setq data-length (cadr array)
	   array (vmake-pointer sym:dtp-array-pointer (car array)))
     ;now store the data area
     (and displaced-p (ferror nil "displaced array not handled"))
     (setq adr (allocate-block area data-length))
     (cond ((cdr (assq type-sym sym:array-bits-per-element)) ;numeric
	    (dotimes (i data-length)
	      (vwrite (+ adr i) 0)))
	   (t
	    (cond ((and named-structure (not leader))
		   (vwrite adr (qintern named-structure))
		   (setq adr (1+ adr)
			 data-length (1- data-length))))
	    (dotimes (i data-length)
	      (vwrite (+ adr i) qnil))))
     (m-q-enter-fasl-table
       "note - you have been screwed to the wall by an array"
       array)))

;Get values and store them into an array.
(defun q-fasl-op-initialize-array ()
  (prog (array num hack ptr header long-flag ndims)
     (setq hack (qfasl-group nil))
     (setq array (q-arft hack))
     (or (= (vdata-type array) sym:dtp-array-pointer)
	 (ferror nil "fasl-op-initialize-array of non-array"))
     (setq num (m-fasl-next-value))	;number of values to initialize with
     ;; Take header apart to find address of data
     (setq ptr (logand q-pointer-mask array))
     (setq header (vread ptr))
     (setq long-flag (bit-test sym:array-long-length-flag header)
	   ndims (logand (// header sym:array-dim-mult) 7))
     (and (bit-test sym:array-displaced-bit header)
	  (ferror nil "attempt to initialize displaced array, give it up"))
     (setq ptr (+ ptr (if long-flag 1 0) ndims))	;To data
     (dotimes (n num)				;Initialize specified num of vals
       (vwrite ptr (q-fasl-next-value))
       (setq ptr (1+ ptr)))
     (return hack)))

;Get 16-bit nibbles and store them into an array.
(defun q-fasl-op-initialize-numeric-array ()
  (prog (array num hack ptr header long-flag ndims)
     (setq hack (qfasl-group nil))
     (setq array (q-arft hack))
     (or (= (vdata-type array) sym:dtp-array-pointer)
	 (ferror nil "fasl-op-initialize-array of non-array"))
     (setq num (m-fasl-next-value))	;number of values to initialize with
     ;; Take header apart to find address of data
     (setq ptr (logand q-pointer-mask array))
     (setq header (vread ptr))
     (setq long-flag (bit-test sym:array-long-length-flag header)
	   ndims (logand (// header sym:array-dim-mult) 7))
     (and (bit-test sym:array-displaced-bit header)
	  (ferror nil "attempt to initialize displaced array, give it up"))
     (setq ptr (+ ptr (if long-flag 1 0) ndims))	;To data
     (dotimes (n (// num 2))			;Initialize specified num of vals
       (vwrite ptr (+ (qfasl-nibble) (ash (qfasl-nibble) 16.)))
       (setq ptr (1+ ptr)))
     (cond ((oddp num)				;odd, catch last nibble
	    (vwrite ptr (qfasl-nibble))))
     (return hack)))

(defun qfasl-store-evaled-value (v)
  (aset v fasl-table sym:fasl-evaled-value)
  sym:fasl-evaled-value)

(defun q-fasl-op-eval ()
  (let ((exp (m-arft (qfasl-next-nibble))))
    (cond ((and (not (atom exp))
		(eq (car exp) 'sym:record-source-file-name)
		(not (atom (cadr exp)))
		(eq (caadr exp) 'sym:quote)
		(symbolp (cadadr exp)))
	   (store-source-file-name-property (qintern (cadadr exp))))
	  (t (setq evals-to-be-sent-over
		   (setq last-fasl-eval (cons exp evals-to-be-sent-over))))))
  (qfasl-store-evaled-value 'value-only-available-in-the-future))

(defun q-fasl-op-move ()
  (let ((from (qfasl-next-nibble))
	(to (qfasl-next-nibble)))
    (cond ((= to 177777) (m-q-enter-fasl-table (car (aref fasl-table from))
					       (cadr (aref fasl-table from))))
	  (t (aset (aref fasl-table from) fasl-table to)
	     to))))

;Macrocompiled code

(defun q-fasl-op-frame ()
  (let ((q-count (qfasl-next-nibble))		;number of boxed qs
	(unboxed-count (qfasl-next-nibble))	;number of unboxed qs (half num instructions)
	(fef)					;the fef being created
	(obj)
	(tem)
	(offset 0)
	(area 'sym:macro-compiled-program))	;(m-arft sym:fasl-frame-area)
     (setq fasl-group-length (qfasl-next-nibble))	;amount of stuff that follows
     (setq fef (vmake-pointer sym:dtp-fef-pointer	;Store header
			      (storeq area (vmake-pointer sym:dtp-header
							  (m-fasl-next-value)))))
     (qfasl-next-nibble)			;skip modifier nibble for header q
     (do i 1 (1+ i) (>= i q-count)		;fill in boxed qs
       (setq obj (q-fasl-next-value))		;get object to be stored
       (setq tem (qfasl-next-nibble))		;get ultra-kludgey modifier
       (or (zerop (setq offset (logand 17 tem)))	;add offset if necessary
	   (setq obj (+ obj offset)))
       (and (bit-test 420 tem)			;try not to get shafted totally
	    (or (= (vdata-type obj) sym:dtp-symbol)
		(ferror nil "about to get shafted totally - q-fasl-op-frame")))
       (and (bit-test 20 tem)			;make into external value cell pointer
	    (setq obj (vmake-pointer sym:dtp-external-value-cell-pointer obj)))
       (and (bit-test 400 tem)			;make into locative
	    (setq obj (vmake-pointer sym:dtp-locative obj)))
       (setq obj (dpb (lsh tem -6) sym:%%q-cdr-code obj))
       (and (bit-test 40 tem)			;flag bit
	    (setq obj (dpb 1 sym:%%q-flag-bit obj)))
       (storeq area obj))
     (begin-store-halfwords area unboxed-count)	;now store the unboxed qs
     (dotimes (n (* unboxed-count 2))
       (store-halfword (qfasl-next-nibble)))
     (end-store-halfwords)
     (m-q-enter-fasl-table
        "note - you have been screwed to the wall by a fef"
	fef)))

(defun q-fasl-op-function-header ()
  (prog (f-sxh)
	(setq current-function (m-fasl-next-value)
	      f-sxh (m-fasl-next-value))
	(return 0)))

(defun q-fasl-op-function-end () 0)

(defun q-fasl-storein-symbol-cell (n put-source-file-name-property)
  (prog (newp adr data sym nib)
     (setq nib (qfasl-next-nibble))
     (setq sym (m-fasl-next-value))
     (cond ((= nib sym:fasl-evaled-value) ;Setting symbol to result of evaluation
	    (cond ((atom sym)	          ;Modify the entry in EVALS-TO-BE-SENT-OVER
		   (cond ((null last-fasl-eval)
			  (ferror nil "~S invalid storein-symbol" sym)))
		   (rplaca last-fasl-eval ;SETQ not in cold load!
			   `(set (sym:quote ,sym) ,(car last-fasl-eval)))
		   (return 0))			;Skip the rest of this function
		  (t (ferror nil "Must be a sym evaled-value")))))
     (setq data (q-arft nib))
     (cond ((atom sym)
	    (setq sym (qintern sym))
	    (vstore-contents (+ sym n) data)
	    (cond (put-source-file-name-property
		    (store-source-file-name-property sym))))
	   ;; E.g. (DEFUN (FOO PROP) (X Y) BODY)
	   ;; - thinks it's storing function cell but really PUTPROP
	   ((not (= n 2))
	    (ferror nil "~S not a symbol or property spec" sym))
	   (t (setq adr (qintern (car sym)))
	      (setq newp (vmake-pointer sym:dtp-list
					(store-cdr-q 'sym:property-list-area sym:cdr-next
						     (qintern (cadr sym)))))
	      (store-cdr-q 'sym:property-list-area sym:cdr-normal data)
	      (store-cdr-q 'sym:property-list-area sym:cdr-error (vread (+ adr 3)))
	      (vstore-contents (+ adr 3) newp)))
     (return 0)))

(defun store-source-file-name-property (sym)
  (let ((newp (vmake-pointer sym:dtp-list
			     (store-cdr-q 'sym:property-list-area sym:cdr-next
					  (qintern 'sym:source-file-name)))))
						;was USER/:SOURCE-FILE-NAME
    (store-cdr-q 'sym:property-list-area sym:cdr-normal fdefine-file-symbol)
    (store-cdr-q 'sym:property-list-area sym:cdr-error (vread (+ sym 3)))
    (vstore-contents (+ sym 3) newp)))

(defun q-fasl-op-storein-symbol-value ()
  (q-fasl-storein-symbol-cell 1 nil))

(defun q-fasl-op-storein-function-cell ()
  (q-fasl-storein-symbol-cell 2 t))

(defun q-fasl-op-storein-property-cell ()
  (q-fasl-storein-symbol-cell 3 nil))

(defun q-fasl-op-storein-array-leader ()
  (let ((array (q-arft (qfasl-next-nibble)))
	(subscr (m-arft (qfasl-next-nibble)))
	(value (q-arft (qfasl-next-nibble))))
    ;;error checking might be nice
    ;(store-array-leader value array subscr)
    (vwrite (- array (+ 2 subscr)) value)
    0))

(defun q-fasl-fetch-symbol-cell (n)
  (vcontents (+ (q-fasl-next-value) n)))

(defun q-fasl-op-fetch-symbol-value ()
  (q-fasl-fetch-symbol-cell 1))

(defun q-fasl-op-fetch-function-cell ()
  (q-fasl-fetch-symbol-cell 2))

(defun q-fasl-op-fetch-property-cell ()
  (q-fasl-fetch-symbol-cell 3))

(defun q-fasl-op-end-of-whack ()
  (setq fasl-return-flag 'end-of-whack)
  0)

(defun q-fasl-op-end-of-file ()
  (setq fasl-return-flag 'eof)
  0)

(defun q-fasl-op-soak ()
  (dotimes (count (qfasl-next-nibble))
    (m-fasl-next-value))
  (qfasl-group t))

(defun q-fasl-op-set-parameter ()
  (let ((to (m-fasl-next-value))
	(from (qfasl-group t)))
    (aset (aref fasl-table from) fasl-table to)
    0))

(defun q-fasl-op-file-property-list () ;ignores it
  (m-fasl-next-value)
  0)
