; -*- Mode:Lisp; Package:Cold; Lowercase:T; Base:8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

; Utilities for cold-load generator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To compile this:			       ;;;
;;;   (1) Load the old QFASL of it	       ;;;
;;;   (2) Run (LOAD-PARAMETERS)		       ;;;
;;;   (3) Now you may compile it	       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Little variables that have to do with the word format
(defvar big-fixnum)
(defvar little-fixnum)
(defvar q-typed-pointer-mask)	;Due to deficiencies in LDB and DPB
(defvar q-pointer-mask)

;;; The virtual memory

(defvar n-vmem-pages 16.)

;(i,0) is virtual page number, (i,1) is rqb
;Both slots are nil if unused
(defvar vmem-pages (make-array nil 'art-q (list n-vmem-pages 2)))

(defvar vmem-page-reuse-pointer)

(defvar vmem-part-base)
(defvar vmem-part-size)

(defun vmem-initialize (part-name)
  (setq vmem-page-reuse-pointer 0)
  (multiple-value (vmem-part-base vmem-part-size) (sys:find-disk-partition part-name))
  (or vmem-part-base (ferror nil "~S partition not found on disk unit 0" part-name))
  (dotimes (i n-vmem-pages)
    (aset nil vmem-pages i 0)
    (aset nil vmem-pages i 1)))

;Write out all the buffered pages and return the rqb's
(defun vmem-finish (&aux rqb)
  (dotimes (i n-vmem-pages)
    (cond ((setq rqb (aref vmem-pages i 1))
	   (vmem-disk-io rqb (aref vmem-pages i 0) t)
	   (sys:return-disk-rqb rqb)
	   (aset nil vmem-pages i 1)))))

(defun vmem-disk-io (rqb vpn writep)
  (and (or (minusp vpn) ( vpn vmem-part-size))
       (ferror nil "Disk i//o outside of partition"))
  (funcall (if writep #'sys:disk-write #'sys:disk-read) rqb 0 (+ vpn vmem-part-base)))

;Given address returns art-16b array containing that page.  With second arg of nil
;initializes to dtp-free instead of reading in from disk.
(defun vmem-find-page (address &optional (get-from-disk-p t))
  (do ((i 0 (1+ i))
       (vpn (// ;(ldb sym:%%q-pointer address)
	        (logand q-pointer-mask address)
		sym:page-size))
       (rqb) (buf) (tem))
      (( i n-vmem-pages)
       (setq i vmem-page-reuse-pointer)
       (cond ((setq rqb (aref vmem-pages i 1))
	      (vmem-disk-io rqb (aref vmem-pages i 0) t))	;Swap this guy out
	     (t (setq rqb (sys:get-disk-rqb))
		(aset rqb vmem-pages i 1)))
       (aset vpn vmem-pages i 0)
       (setq buf (sys:rqb-buffer rqb))
       (cond (get-from-disk-p
	       (vmem-disk-io rqb vpn nil))
	     (t (setq tem (dpb sym:dtp-free sym:%%q-data-type (* vpn sym:page-size)))
		(do ((j 0 (1+ j))
		     (high (ldb 2020 tem))
		     (low (ldb 0020 tem)))
		    (( j sym:page-size))
		  (aset (+ low j) buf (+ j j))
		  (aset high buf (+ j j 1)))))
       buf)
    (cond ((eq (aref vmem-pages i 0) vpn)	;Already swapped in
	   (and (= vmem-page-reuse-pointer i)
		(setq vmem-page-reuse-pointer (\ (1+ i) n-vmem-pages)))
	   (return (sys:rqb-buffer (aref vmem-pages i 1)))))))

(defun vread (address)
  (let ((buf (vmem-find-page address))
	(i (* 2 (\ address sym:page-size))))
    (dpb (aref buf (1+ i)) 2020 (aref buf i))))

(defun vwrite (address value)
  (let ((buf (vmem-find-page address))
	(i (* 2 (\ address sym:page-size))))
    (aset (ldb 0020 value) buf i)
    (aset (ldb 2020 value) buf (1+ i))))

(defun vcontents (address)
  (logand q-typed-pointer-mask (vread address)))

(defun vcdr-code (address)
  (ldb sym:%%q-cdr-code (vread address)))

(defun vflag-bit (address)
  (ldb sym:%%q-flag-bit (vread address)))

(defun vstore-contents (address value)
  (let ((buf (vmem-find-page address))
	(i (* 2 (\ address sym:page-size))))
    (aset (ldb 0020 value) buf i)
    (aset (deposit-field (aref buf (1+ i))
			 (- sym:%%q-all-but-typed-pointer 2000)
			 (ldb 2020 value))
	  buf (1+ i))))

(defun vstore-cdr-code (address value)
  (let ((buf (vmem-find-page address))
	(i (* 2 (\ address sym:page-size))))
    (aset (dpb value (- sym:%%q-cdr-code 2000) (aref buf (1+ i))) buf (1+ i))))

(defun vstore-flag-bit (address value)
  (let ((buf (vmem-find-page address))
	(i (* 2 (\ address sym:page-size))))
    (aset (dpb value (- sym:%%q-flag-bit 2000) (aref buf (1+ i))) buf (1+ i))))

(defun vwrite-cdr (address cdr-code value)
  (vwrite address (dpb cdr-code sym:%%q-cdr-code value)))

(defsubst vmake-pointer (data-type address)
  (dpb data-type sym:%%q-all-but-pointer address))

(defsubst vdata-type (value)
  (ldb sym:%%q-data-type value))

(defsubst vfix (value)
  (vmake-pointer sym:dtp-fix value))

(defvar sym-package (pkg-find-package "cold-symbols"))
(defvar misc-function-list)
(defvar misc-instruction-list)

;;; Set up the sym: package by loading the appropriate files
(defun load-parameters ()
  (load "ai:lispm;qcom >" sym-package)
  (load "ai:lispm;qdefs >" sym-package)
  (setq misc-function-list nil)
  (setq misc-instruction-list nil)
  (load "ai:lispm;defmic >" sym-package)
  (dolist (l sym:system-constant-lists)	;Make declarations so can compile self
    (dolist (s (symeval l))
      (putprop s t 'special)))
  (setq big-fixnum (dpb -1 (1- sym:%%q-pointer) 0)
	little-fixnum (1- (- big-fixnum))
	q-typed-pointer-mask (1- (ash 1 sym:%%q-typed-pointer))
	q-pointer-mask (1- (ash 1 sym:%%q-pointer))))  

;These have to be explicitly declared special because they only exist in
;the cold-load generator, and are not sent over.
(declare (special sym:rm-area-sizes sym:scratch-pad-pointers sym:scratch-pad-parameters
		  sym:scratch-pad-parameter-offset sym:q-corresponding-variable-lists
		  sym:support-vector-contents sym:constants-page
		  sym:read-only-area-list sym:wired-area-list sym:pdl-buffer-area-list
		  sym:list-structured-areas sym:static-areas
		  sym:prin1 sym:base sym:ibase sym:*nopoint sym:for-cadr))

;Put on QLVAL and QINTCMP properties
;Creates MISC-FUNCTION-LIST for STORE-MISC-LINK  (CALLED FROM STORE-MISC-U-ENTRY-LINKS)
; and MISC-INSTRUCTION-LIST for STORE-MICRO-CODE-SYMBOL-NAMES
(defun defmic (&quote name opcode arglist lisp-function-p &optional no-qintcmp)
  (prog (function-name instruction-name)
    (cond ((atom name)
	   (setq function-name name instruction-name name))
	  ((setq function-name (car name) instruction-name (cdr name))))
    (cond ((not no-qintcmp)
	   (putprop instruction-name (length arglist) 'sym:qintcmp)
	   (or (eq function-name instruction-name)
	       (putprop function-name (length arglist) 'sym:qintcmp)))
	  (t ;The number of arguments is needed anyway for the cold-load generator
	   (putprop instruction-name (length arglist) 'qintcmp-kludge)
	   (or (eq function-name instruction-name)
	       (putprop function-name (length arglist) 'qintcmp-kludge))))
    (putprop instruction-name opcode 'sym:qlval)
    (setq misc-instruction-list (cons instruction-name misc-instruction-list))
    (and lisp-function-p
	 (setq misc-function-list (cons name misc-function-list)))))

;;; Basic area-processing and data-storing stuff

;Note that area names are always symbols in the sym: package

(defvar symbol-creation-trace-list nil)
(defvar qnil)
(defvar qtruth)
(defvar area-origins (make-array nil 'art-q 400))
(defvar area-alloc-pointers (make-array nil 'art-q 400))
(defvar area-alloc-bounds (make-array nil 'art-q 400))

(defvar area-corresponding-arrays
	'sym:(area-name region-origin region-length region-free-pointer
	      region-gc-pointer region-bits area-region-list area-region-size
	      area-maximum-size region-list-thread))

(defvar micro-code-entry-corresponding-arrays
	'sym:(constants-area micro-code-entry-area 
	      micro-code-entry-name-area micro-code-entry-args-info-area
	      micro-code-entry-arglist-area micro-code-exit-area 
	      micro-code-entry-max-pdl-usage micro-code-symbol-area
	      micro-code-symbol-name-area support-entry-vector))

(defvar areas-with-fill-pointers
	(append area-corresponding-arrays micro-code-entry-corresponding-arrays))

 ;areas in this list get art-q-list
(defvar list-referenced-areas areas-with-fill-pointers)

 ;areas in this list get art-q, all other areas get art-32b
(defvar array-referenced-areas 'sym:(system-communication-area page-table-area
				     region-sorted-by-origin))

(defun create-areas (&aux high-loc)
  (do l sym:rm-area-sizes (cddr l) (null l)	;Area sizes in pages
    (putprop (car l) (cadr l) 'area-size))
  (fillarray area-origins '(nil))
  ;; Set up the area origin and allocation tables
  (do ((l sym:area-list (cdr l))
       (i 0 (1+ i))
       (loc 0 (+ loc (* (get-area-size (car l)) sym:page-size))))
      ((null l) (setq high-loc loc))
    (aset loc area-origins i))
  (copy-array-contents area-origins area-alloc-pointers)
  (copy-array-portion area-origins 1 400 area-alloc-bounds 0 400)
  (aset high-loc area-alloc-bounds (1- (length sym:area-list)))
  ;; Fill various areas with default stuff
  (init-area-contents 'sym:area-region-size (vfix 40000))
  (init-area-contents 'sym:area-maximum-size (vfix big-fixnum))
  (init-area-contents 'sym:region-origin (vfix 0))  ;so good type in free region#'s
  (init-area-contents 'sym:region-length (vfix 0))  ;..
  (init-area-contents 'sym:region-free-pointer (vfix 0))
  (init-area-contents 'sym:region-gc-pointer (vfix 0))
  (init-area-contents 'sym:region-bits (vfix 0))  ;Suitable for free region
  ;; Set up contents of certain initial areas
  (do ((i 0 (1+ i))
       (al sym:area-list (cdr al))
       (fixed-p t))
      ((null al))
    (and (eq (car al) 'sym:working-storage-area) (setq fixed-p nil))
    (vwrite (+ (get-area-origin 'sym:area-region-list) i) (vfix i))
    (vwrite (+ (get-area-origin 'sym:region-list-thread) i) (vfix (+ i little-fixnum)))
    (vwrite (+ (get-area-origin 'sym:region-bits) i)
	    (vfix (+ (dpb (cond ((memq (car al) sym:read-only-area-list) 1200)	;ro
				((memq (car al) sym:wired-area-list) 1400)	;rw
				((memq (car al) sym:pdl-buffer-area-list)
				 500)			;may be in pdl-buffer, no access.
				(t 1300))			;rwf
			  sym:%%region-map-bits
			  0)
		     (dpb 1 sym:%%region-oldspace-meta-bit 0)
		     (dpb (if (eq (car al) 'sym:extra-pdl-area) 0 1)
			  sym:%%region-extra-pdl-meta-bit 0)
		     (dpb (if (memq (car al) sym:list-structured-areas) 0 1)
			  sym:%%region-representation-type 0)
		     (dpb (cond ((eq (car al) 'sym:extra-pdl-area)
				 sym:%region-space-extra-pdl)
				(fixed-p sym:%region-space-fixed)
				((memq (car al) sym:static-areas) sym:%region-space-static)
				(t sym:%region-space-new))
			  sym:%%region-space-type 0))))
    (vwrite (+ (get-area-origin 'sym:region-origin) i)
	    (vfix (aref area-origins i)))
    (vwrite (+ (get-area-origin 'sym:region-length) i)
	    (vfix (- (aref area-alloc-bounds i) (aref area-origins i))))))

(defun get-area-number (area)
  (cond ((numberp area) area)
	((find-position-in-list area sym:area-list))	;symeval??
	((ferror nil "~S bad area-name" area))))

(defun get-area-origin (area)
  (aref area-origins (get-area-number area)))

(defun allocate-block (area size &aux address high)
  (setq area (get-area-number area))
  (setq address (aref area-alloc-pointers area))
  (setq high (+ address size))
  (and (> high (aref area-alloc-bounds area))
       (ferror nil "~A area overflow" (nth area sym:area-list)))
  (aset high area-alloc-pointers area)
  ;Page in all the fresh pages without really paging them in, thus initializing them
  (do ((vpn (// (+ address sym:page-size -1) sym:page-size) (1+ vpn))
       (hpn (// (+ high sym:page-size -1) sym:page-size)))
      (( vpn hpn))
    (vmem-find-page (* vpn sym:page-size) nil))
  address)

;In pages
(defun get-area-size (area)
  (check-arg area (memq area sym:area-list) "an area-name")
  (cond ((eq area 'sym:free-area) 0)
	((get area 'area-size))
	(t 1)))

;Doesn't advance allocation pointer, i.e. sets it back to origin when done
(defun init-area-contents (area contents)
  (let ((count (* sym:page-size (get-area-size area))))
    (setq area (get-area-number area))
    (do ((adr (allocate-block area count) (1+ adr))
	 (n count (1- n)))
	((zerop n)
	 (store-nxtnil-cdr-code area)
	 (aset (aref area-origins area) area-alloc-pointers area))
      (vwrite-cdr adr sym:cdr-next contents))))

(defvar store-halfwords-address)
(defvar store-halfwords-count)
(defvar store-halfwords-buffer)

(defun begin-store-halfwords (area-name n-words)
  (let* ((area-number (get-area-number area-name))
	 (address (allocate-block area-number n-words)))
    (setq store-halfwords-address address
	  store-halfwords-count (* 2 n-words))
    address))

(defun store-halfword (hwd)
  (if (oddp (setq store-halfwords-count (1- store-halfwords-count)))
      (setq store-halfwords-buffer hwd)
      (vwrite store-halfwords-address (dpb hwd 2020 store-halfwords-buffer))
      (setq store-halfwords-address (1+ store-halfwords-address))))

(defun end-store-halfwords ()
  (or (zerop store-halfwords-count)
      (ferror nil "store-halfword called wrong number of times")))

(defun make-q-list (area s-exp &aux bsize value)
  (cond ((numberp s-exp)
	 (cond ((small-floatp s-exp) (make-small-flonum s-exp))
	       ((floatp s-exp) (store-flonum area s-exp))
	       ((and ( s-exp big-fixnum) ( s-exp little-fixnum)) (vfix s-exp))
	       (t (store-bignum area s-exp))))
	((symbolp s-exp) (qintern s-exp))
	((stringp s-exp) (store-string 'sym:p-n-string s-exp))
	((atom s-exp) (ferror nil "~S unknown type" s-exp))
	(t (or (memq area sym:list-structured-areas)
	       (ferror nil "make-q-list in non-list-structured area ~S" area))
	   (setq bsize (length s-exp))
	   (cond ((cdr (last s-exp)) (setq bsize (1+ bsize))))	;ends in dotted pair
	   (setq value (vmake-pointer sym:dtp-list (allocate-block area bsize)))
	   (do ((s-exp s-exp (cdr s-exp))
		(adr (logand q-pointer-mask value) (1+ adr))
		(c-code))
	       ((atom s-exp)
		(or (null s-exp)
		    (vwrite-cdr adr sym:cdr-error (make-q-list area s-exp))))
	     (setq c-code (cond ((null (cdr s-exp)) sym:cdr-nil)
				((atom (cdr s-exp)) sym:cdr-normal)
				(t sym:cdr-next)))
	     (vwrite-cdr adr c-code (make-q-list area (car s-exp))))
	   value)))

(defun make-small-flonum (s-exp)  ;I hope the format doesn't change!
  (vmake-pointer sym:dtp-small-flonum (%pointer s-exp)))

(defun magic-aref (a i n)
  (if (< i n) (aref a i) 200))

(defun store-string (area string)
   (and (memq area sym:list-structured-areas)
	(ferror nil "store-string in list-structured area"))
   (let* ((n-chars (string-length string))
	  (n-words (+ 1 (// (+ n-chars 3) 4)))
	  (adr (allocate-block area n-words)))
     (and (> n-chars sym:%array-max-short-index-length)
	  (ferror nil "I don't know how to make a long-array"))
     (vwrite adr (vmake-pointer sym:dtp-array-header
				(+ sym:array-dim-mult	;1-dim
				   sym:art-string
				   n-chars)))
     (do ((i 1 (1+ i))
	  (j 0 (+ j 4)))
	 ((= i n-words))
       (vwrite (+ adr i)
	       (+ (magic-aref string j n-chars)
		  (ash (magic-aref string (1+ j) n-chars) 8)
		  (ash (magic-aref string (+ j 2) n-chars) 16.)
		  (ash (magic-aref string (+ j 3) n-chars) 24.))))
     (vmake-pointer sym:dtp-array-pointer adr)))

(defun store-symbol-vector (atom-name area)
  (and (memq area sym:list-structured-areas)
       (ferror nil "store-symbol-vector in list-structured area ~S" area))
  (and (eq atom-name '**screw**)
       (ferror nil "you've probably encountered a bug in froid (coldld)" atom-name))
  (prog (adr sym path real-atom-name package-name pname)
     (cond ((setq path (get atom-name 'package-path))
	    (or (= (length path) 2)
		(ferror nil "package path ~S not 2 long - code not hairy enough"))
	    (setq package-name (qintern (car path))
		  real-atom-name (car (last path))))
	   (t (setq package-name qnil real-atom-name atom-name)))
     (cond (symbol-creation-trace-list  ;debugging tool to track down appears twice in 
	     (do ((l symbol-creation-trace-list (cdr l)))  ;cold load messages.
		 ((null l))
	       (cond ((inhibit-style-warnings
			(samepnamep real-atom-name (car l)))
		      (print (list 'a-flavor-of real-atom-name 'being-created
				   'atom-name atom-name 'path path
				   'package-name package-name)))))))
     (setq pname (store-string 'sym:p-n-string (string real-atom-name)))
     (setq adr (allocate-block area sym:length-of-atom-head))
     (vwrite-cdr adr sym:cdr-next (vmake-pointer sym:dtp-symbol-header pname))
     (vwrite-cdr (+ adr 1) sym:cdr-next (vmake-pointer sym:dtp-null adr))
     (vwrite-cdr (+ adr 2) sym:cdr-next (vmake-pointer sym:dtp-null adr))
     (vwrite-cdr (+ adr 3) sym:cdr-next qnil)
     (vwrite-cdr (+ adr 4) sym:cdr-nil package-name)
     (setq sym (vmake-pointer sym:dtp-symbol adr))
     (putprop atom-name sym 'q-atom-head)
     (return sym)))

;New version of qintern.  Machine builds obarray when it first comes up (easy enough).
(defun qintern (atom-name)
    (or (eq (car (package-cell-location atom-name)) sym-package)
	(setq atom-name (intern (string atom-name) sym-package)))
    (or (get atom-name 'q-atom-head)
	(store-symbol-vector atom-name 'sym:nr-sym)))

(defun store-nxtnil-cdr-code (area)
  (vstore-cdr-code (1- (aref area-alloc-pointers (get-area-number area))) sym:cdr-nil))

(defun store-list-of-atoms (area loa)
  (let ((adr (allocate-block area (length loa))))
    (do ((loa loa (cdr loa))
	 (adr adr (1+ adr)))
	((null loa))
      (vwrite-cdr adr (if (null (cdr loa)) sym:cdr-nil sym:cdr-next)
		      (q-convert-atom (car loa))))
    adr))

(defun q-convert-atom (atm)
  (if (numberp atm) (make-q-list nil atm) (qintern atm)))

(defun store-list (area lst)
  (let ((adr (allocate-block area (length lst))))
    (do ((lst lst (cdr lst))
	 (adr adr (1+ adr)))
	((null lst))
      (vwrite-cdr adr (if (null (cdr lst)) sym:cdr-nil sym:cdr-next)
		      (make-q-list 'init-list-area (car lst))))
    adr))

(defun store-nils (area number)
  (let ((adr (allocate-block area number)))
    (do ((number number (1- number))
	 (adr adr (1+ adr)))
	((zerop number))
      (vwrite-cdr adr (if (= number 1) sym:cdr-nil sym:cdr-next) qnil))
    adr))

(defun storeq (area data)
  (let ((adr (allocate-block area 1)))
    (vwrite adr data)
    adr))

(defun store-cdr-q (area cdr-code data)
  (let ((adr (allocate-block area 1)))
    (vwrite-cdr adr cdr-code data)
    adr))

;;; Hair for making arrays

(defun init-q-array (area name offset type dimlist displaced-p leader)
  (init-q-array-named-str area name offset type dimlist displaced-p leader nil))

;NOTE!! LEADER IS STOREQ ED DIRECTLY SO IT MUST ALREADY BE MAKE-Q-LIST IFIED
(defun init-q-array-named-str (area name offset type dimlist displaced-p leader named-str)
	;  leader is contents of array leader, if desired.  it is in "storage order"
	;which is reversed from index order.
	;  if leader is numeric, it means make leader consisting of that many q's
	;initialized to nil.
	;  if name -> nil, return (list <array-adr> <data-length>) and dont try
	;to store in function or value cell.
	;offset 1 for storing pointer to array in value cell, 2 for function cell
  (and (memq area sym:list-structured-areas)
       (ferror nil "init-q-array in list-structured area"))
  (prog (tem ndims index-length data-length tem1 leader-length header-q long-array-flag adr)
	(and (numberp dimlist) (setq dimlist (list dimlist)))
	(setq ndims (length dimlist))
	(setq index-length (list-product dimlist))
	(cond ((and (> index-length sym:%array-max-short-index-length)
		    (null displaced-p))
	       (setq long-array-flag t)))
	(setq leader-length (cond ((null leader) 0)
				  ((numberp leader) (+ 2 leader))
				  (t (+ 2 (length leader)))))
	(cond ((null (setq tem (assq type sym:array-elements-per-q)))
	       (ferror nil "~S bad array type" type)))
	(setq tem (cdr tem))
	(cond ((not (null leader))
	       (setq adr (allocate-block area leader-length))
	       (vwrite adr (vmake-pointer sym:dtp-header
					  (dpb sym:%header-type-array-leader
					       sym:%%header-type-field
					       leader-length)))
	       (cond ((numberp leader)
		      (dotimes (i leader)
			(vwrite (+ adr i 1) qnil))
		      (and named-str (vwrite (+ adr leader -1)	;(array-leader x 1)
					     (qintern named-str))))
		     (t (do ((l leader (cdr l))
			     (i 1 (1+ i)))
			    ((null l))
			  (vwrite (+ adr i) (car l)))))
	       (vwrite (+ adr leader-length -1) (vfix (- leader-length 2)))))
	(setq data-length (// (+ index-length (1- tem)) tem))
	(setq header-q (vmake-pointer sym:dtp-array-header
				      (+ (* sym:array-dim-mult ndims)
					 (symeval type))))
	(and leader (setq header-q (+ header-q sym:array-leader-bit)))
	(and named-str (setq header-q (+ header-q sym:array-named-structure-flag)))
	(cond (displaced-p   ;note, no index-offset arrays in cold-load
		(setq tem 1 header-q (+ header-q sym:array-displaced-bit 2)))
	      ((null long-array-flag)
		(setq tem 1 header-q (+ header-q index-length)))
	      (t (setq tem 2 header-q (+ header-q sym:array-long-length-flag))))
	(setq tem1 (setq adr (allocate-block area (+ tem ndims -1))))
	(vwrite adr header-q)
	(and (= tem 2) (vwrite (setq adr (1+ adr)) (vfix index-length)))
	;Store all dimensions except for last
	(do l dimlist (cdr l) (null (cdr l))
	  (vwrite (setq adr (1+ adr)) (vfix (car dimlist))))
	(cond ((null name) (return (list tem1 data-length))))
	(vstore-contents (+ (qintern name) offset)
			 (vmake-pointer sym:dtp-array-pointer tem1))
	(return data-length)))

(defun store-q-array-leader (arrayp idx data)
  (vwrite (- arrayp (+ 2 idx))			;1 for array header, 1 for ldr len
	  data))

;;; Setting up various magic data structures, mostly having to do with the
;;; microcode and the fixed-areas

(defun store-support-vector (item)
  (let ((adr (allocate-block 'sym:support-entry-vector 1)))
    (vwrite-cdr adr sym:cdr-next
		(cond ((eq (car item) 'sym:function)
		       (get-q-fctn-cell (cadr item)))
		      ((eq (car item) 'sym:quote)
		       (make-q-list 'sym:init-list-area (cadr item)))
		      (t (ferror nil "bad-support-code: ~S" item))))
    adr))

(defun get-q-fctn-cell (fctn &aux tem)
  (and (setq tem (get fctn 'q-atom-head))
       (vcontents (+ tem 2))))

(defun store-displaced-array-pointer (area)
 (prog (fillp area-array-type data-length adr)
    (setq fillp (memq area areas-with-fill-pointers))
    (setq area-array-type 
	  (cond ((memq area list-referenced-areas) 'sym:art-q-list)
		((memq area array-referenced-areas) 'sym:art-q)
		(t 'sym:art-32b)))
    (setq data-length 
	  (init-q-array 'sym:control-tables
			area  
			2 
			area-array-type  
			(list (* sym:page-size (get-area-size area)))
			t 
			(and fillp
			     (list (vfix (cond ((memq area area-corresponding-arrays)
						(length area-list))
					       ((memq area
						      micro-code-entry-corresponding-arrays)
						(length micro-code-entry-vector))
					       (t
						 (* sym:page-size (get-area-size area)))))))))
    (setq adr (allocate-block 'sym:control-tables 2))
    (vwrite adr (vfix (get-area-origin area)))
    (vwrite (1+ adr) (vfix data-length))))

;x is a symbol or cons function-name instruction-name
(defun store-misc-link (x)
  (cond ((atom x)
	 (misc-store-micro-entry x x))
	((misc-store-micro-entry (car x) (cdr x)))))

;special kludge which filters out *catch 
(defun store-misc-link-1 (x)
  (or (eq x 'sym:*catch)
      (store-misc-link x)))

;This creates an indirect through the MICRO-CODE-SYMBOL-AREA by using
;DTP-FIX and 200 less than the misc function index.  This makes
;the core image independent of the microcode version.
(defun misc-store-micro-entry (name me-name)
  (prog (misc-index u-entry-prop u-entry-index)
	(cond ((null (setq misc-index (get me-name 'sym:qlval)))
	       (ferror nil "No QLVAL property: ~S" me-name)))
	(setq u-entry-prop (vfix (- misc-index 200)))
	(setq u-entry-index (get-u-entry-index name))
	(vstore-contents (+ (qintern name) 2)	;function cell
			 (vmake-pointer sym:dtp-u-entry u-entry-index))
	(vstore-contents (+ (get-area-origin 'sym:micro-code-entry-area) u-entry-index)
			 u-entry-prop)
	(vstore-contents (+ (get-area-origin 'sym:micro-code-entry-args-info-area)
			    u-entry-index)
			 (make-q-list 'sym:init-list-area (get-q-args-prop name)))))

;This abbreviated version of the stuff in UTIL2 should be enough to get us off the ground
(defun get-q-args-prop (fctn &aux tem)
  (cond ((setq tem (get fctn 'sym:argdesc))
	 (get-q-args-prop-from-argdesc-prop tem))
	((setq tem (get fctn 'sym:qintcmp))
	 (+ (lsh tem 6) tem))
	;; You may think this is a kludge, but in the Maclisp cold-load generator
	;; it gets the number of arguments out of the Maclisp subr of the same name!
	((setq tem (get fctn 'qintcmp-kludge))
	 (+ (lsh tem 6) tem))
	(t (ferror nil "Cannot find arg desc for ~S" fctn))))

(defun get-q-args-prop-from-argdesc-prop (arg-desc)
  (prog (prop min-args max-args count item)
	(setq prop 0 min-args 0 max-args 0)
   l	(cond ((null arg-desc) (return (+ prop (lsh min-args 6) max-args))))
	(setq count (caar arg-desc))
	(setq item (cadar arg-desc)) ;list of arg syntax, quote type, other attributes
	(setq arg-desc (cdr arg-desc))
   l1	(cond ((= 0 count) (go l))
	      ((memq 'sym:fef-arg-rest item)
	       (setq prop (logior prop (if (or (memq 'sym:fef-qt-eval item)
					       (memq 'sym:fef-qt-dontcare item))
					   sym:%arg-desc-evaled-rest
					   sym:%arg-desc-quoted-rest)))
	       (go l))
	      ((memq 'sym:fef-arg-req item)
	       (setq min-args (1+ min-args)))
	      ((memq 'sym:fef-arg-opt item))
	      (t (go l)))
    	(setq max-args (1+ max-args))
	(or (memq 'sym:fef-qt-eval item)
	    (memq 'sym:fef-qt-dontcare item)
	    (setq prop (logior prop sym:%arg-desc-fef-quote-hair)))
	(setq count (1- count))
	(go l1)))

(defvar micro-code-entry-vector nil)

(defun get-u-entry-index (fctn)
  (prog (tem)
	(cond ((setq tem (find-position-in-list fctn micro-code-entry-vector))
	       (return tem)))
	(setq tem (length micro-code-entry-vector))
	(store-cdr-q 'sym:micro-code-entry-area sym:cdr-next qnil)  ;will be changed
	(store-cdr-q 'sym:micro-code-entry-name-area sym:cdr-next (qintern fctn))
	(store-cdr-q 'sym:micro-code-entry-args-info-area sym:cdr-next qnil)  ;will be chngd
	(store-cdr-q 'sym:micro-code-entry-arglist-area sym:cdr-next qnil) ;set on startup
	(setq micro-code-entry-vector (nconc micro-code-entry-vector 
					     (list fctn)))
	(return tem)))

(defun store-micro-code-symbol-name (name)
  (let ((opcode (get name 'sym:qlval)))
    (or opcode (ferror nil "no qlval property in store-micro-code-symbol-name: ~S" name))
    (vstore-contents (+ (get-area-origin 'sym:micro-code-symbol-name-area) (- opcode 200))
		     (qintern name))))

(defun store-lisp-value-list (x)
  (mapc (function store-lisp-value) (symeval x)))

(defun store-lisp-value (sym)
  (storein-q-value-cell sym (make-q-list 'sym:init-list-area (symeval sym))))

;Store cdr-coded list of 1000 NIL's.
(defun init-micro-code-symbol-name-area ()
  (store-nils 'sym:micro-code-symbol-name-area 1000))

(defun cold-load-time-setq (pair-list &aux var value)
  (do pair-list pair-list (cddr pair-list) (null pair-list)
    (setq var (car pair-list) value (cadr pair-list))
    (cond ((and (atom value) (or (numberp value)
				 (stringp value)
				 (memq value '(sym:t sym:nil)))))
	  ((eq (car value) 'sym:quote)
	   (setq value (cadr value)))
	  (t (ferror nil "(setq ~S ~S) no can do" var value)))
    (storein-q-value-cell var (make-q-list 'sym:init-list-area value))))

(defun storein-q-value-cell (sym data)
  (vstore-contents (1+ (qintern sym)) data))

(defun store-constant (c)
  (vwrite-cdr (allocate-block 'sym:constants-area 1)
	      sym:cdr-next
	      (make-q-list 'sym:init-list-area c)))

(defun init-scratch-pad-area ()
  (init-area-contents 'sym:scratch-pad-init-area (vfix 0))
  (scratch-store-q 'sym:initial-top-level-function
		   (vmake-pointer sym:dtp-locative
				  (+ (qintern 'sym:lisp-top-level) 2)))
  ;trap-handler (not used)
  (let ((initial-stack-group-pointer (make-initial-stack-group-structure)))
    (scratch-store-q 'sym:current-stack-group initial-stack-group-pointer)
    (scratch-store-q 'sym:initial-stack-group initial-stack-group-pointer))
  (scratch-store-q 'sym:error-handler-stack-group qnil)  ;initialized at run time
  (scratch-store-q 'sym:default-cons-area (vfix (get-area-number 'sym:working-storage-area))))

(defun scratch-store-q (symbolic-name data)
   (prog (tem origin)
      (setq origin (get-area-origin 'sym:scratch-pad-init-area))
      (cond ((setq tem (find-position-in-list symbolic-name sym:scratch-pad-pointers))
	     (vstore-contents (+ origin tem) data))
	    ((setq tem (find-position-in-list symbolic-name sym:scratch-pad-parameters))
	     (vstore-contents (+ origin sym:scratch-pad-parameter-offset tem) data))
	    (t (ferror nil "unknown-scratch-quantity: ~S" symbolic-name)))))

(defun store-a-mem-location-names ()
    (do ((name sym:a-memory-location-names (cdr name))
	 (locn (+ 40 sym:a-memory-virtual-address) (1+ locn)))
	((null name))
     (store-mem-location (car name) locn))
    (do name sym:m-memory-location-names (cdr name) (null name)
     (store-mem-location (car name) (get (car name) 'sym:forwarding-virtual-address))))

(defun store-mem-location (name locn)
  (storein-q-value-cell name (vmake-pointer sym:dtp-one-q-forward locn)))

(defun make-ordered-array-list (assoc-list)
  (mapcar (function (lambda (x) (cdr (assq x assoc-list))))
	  sym:array-types))

;The order store-misc-link is called determines the final micro-code-entry
; numbers that are assigned.  however, except for 0 which must be *catch,
; micro-code-entry numbers are unconstrained and independant from everything
; else.  So the other entries below may be in any order.
(defun store-misc-u-entry-links ()
  (store-misc-link 'sym:*catch)		;must be first
  (mapc (function store-misc-link-1) misc-function-list)
  ;; now set up the first 600 locations of micro-code-symbol-name-area
  (init-micro-code-symbol-name-area)
  (mapc (function store-micro-code-symbol-name) misc-instruction-list))

(defun make-initial-stack-group-structure ()
  (make-stack-group-structure 'sym:main-stack-group 'sym:control-tables
			      'sym:linear-pdl-area 'sym:linear-bind-pdl-area
			      sym:sg-state-active))
 
(defun make-stack-group-structure (name sg-area linear-area l-b-p-area initial-state) 
  (prog (sg pdl-array l-b-p-array reg-len spec-len)
	(setq sg (car (init-q-array sg-area nil nil 'sym:art-stack-group-head '(0)
				    nil (length sym:stack-group-head-leader-qs))))
	(setq pdl-array
	      (car (init-q-array linear-area nil nil 'sym:art-reg-pdl
			     (list (setq reg-len (- (* sym:page-size
						       (get-area-size 'sym:linear-pdl-area))
						    (+ (length sym:reg-pdl-leader-qs) 4))))
			;4: leader header + leader-length-q + array-header-q + long-length-q
			     nil (length sym:reg-pdl-leader-qs))))
	(allocate-block linear-area reg-len)	;advance free pointer
	(setq l-b-p-array
	      (car (init-q-array l-b-p-area nil nil 'sym:art-special-pdl
		        (list (setq spec-len (- (* sym:page-size
						   (get-area-size 'sym:linear-bind-pdl-area))
						(+ (length sym:special-pdl-leader-qs) 4))))
			nil (length sym:special-pdl-leader-qs))))
	(allocate-block l-b-p-area spec-len)	;advance free pointer
	(stack-group-linkup sg pdl-array l-b-p-array)
	(store-q-array-leader sg sym:sg-state (vfix initial-state))
	(store-q-array-leader sg sym:sg-name (make-q-list 'sym:init-list-area name))
	(store-q-array-leader sg sym:sg-regular-pdl-limit
			      (make-q-list 'sym:init-list-area (- reg-len 100)))
	(store-q-array-leader sg sym:sg-special-pdl-limit
			      (make-q-list 'sym:init-list-area (- spec-len 100)))
	(return (vmake-pointer sym:dtp-stack-group sg))))

(defun stack-group-linkup (sg pdl-arrayp l-b-p-arrayp)
  (store-q-array-leader l-b-p-arrayp sym:special-pdl-sg-head-pointer
			(vmake-pointer sym:dtp-stack-group sg))
  (store-q-array-leader pdl-arrayp sym:reg-pdl-sg-head-pointer
			(vmake-pointer sym:dtp-stack-group sg))
  (store-q-array-leader sg sym:sg-special-pdl
			(vmake-pointer sym:dtp-array-pointer l-b-p-arrayp))
  (store-q-array-leader sg sym:sg-regular-pdl
			(vmake-pointer sym:dtp-array-pointer pdl-arrayp))
  (store-q-array-leader sg sym:sg-initial-function-index (vfix 3)))

;This better agree with the order of the list of qs in QCOM
(defun init-system-communication-area (&aux (nqs 24.) adr)
  (setq adr (allocate-block 'sym:system-communication-area nqs))
  (vwrite (+ adr sym:%sys-com-area-origin-pntr)
	  (vmake-pointer sym:dtp-locative (get-area-origin 'sym:region-origin)))
  (vwrite (+ adr sym:%sys-com-valid-size) (vfix 0))	;fixed later
  (vwrite (+ adr sym:%sys-com-page-table-pntr)
	  (vmake-pointer sym:dtp-locative (get-area-origin 'sym:page-table-area)))
  (vwrite (+ adr sym:%sys-com-page-table-size)
	  (vfix (* (get-area-size 'sym:page-table-area) sym:page-size)))
  (vwrite (+ adr sym:%sys-com-obarray-pntr) (qintern 'sym:obarray))
  (vwrite (+ adr sym:%sys-com-remote-keyboard) (vfix 0))
  (vwrite (+ adr sym:%sys-com-micro-load-m-data) (vfix 0))
  (vwrite (+ adr sym:%sys-com-micro-load-a-data) (vfix 0))
  (vwrite (+ adr sym:%sys-com-micro-load-address) (vfix 0))
  (vwrite (+ adr sym:%sys-com-micro-load-flag) (vfix 0))
  (vwrite (+ adr sym:%sys-com-unibus-interrupt-list) (vfix 0))
  (vwrite (+ adr sym:%sys-com-temporary) (vfix 0))
  (vwrite (+ adr sym:%sys-com-free-area/#-list) 0)	;fixed later
  (vwrite (+ adr sym:%sys-com-free-region/#-list) 0)	;fixed later
  (vwrite (+ adr sym:%sys-com-memory-size) (vfix 100000))	;assume 32K, fixed later
  (vwrite (+ adr sym:%sys-com-wired-size)  ;region-free-pointer is the first pageable area
	  (vfix (get-area-origin 'sym:region-free-pointer)))
  (vwrite (+ adr sym:%sys-com-chaos-free-list) qnil)
  (vwrite (+ adr sym:%sys-com-chaos-transmit-list) qnil)
  (vwrite (+ adr sym:%sys-com-chaos-receive-list) qnil)
  (vwrite (+ adr sym:%sys-com-debugger-requests) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-keep-alive) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-data-1) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-data-2) (vfix 0))
  (vwrite (+ adr sym:%sys-com-major-version) qnil)	;I.e. fresh cold-load
  (or (= nqs (length sym:system-communication-area-qs))
      (ferror nil "QCOM and COLDUT disagree about system-communication-area")))

(defun q-storage-finalize ()
  (mapc (function store-support-vector) sym:support-vector-contents)
  (store-nxtnil-cdr-code 'sym:support-entry-vector)
  (mapc (function store-displaced-array-pointer) sym:area-list)
  (scratch-store-q 'sym:active-micro-code-entries (vfix (length micro-code-entry-vector)))
  ;; Transfer over free pointers
  (do ((area-number 0 (1+ area-number))
       (area-list sym:area-list (cdr area-list))
       (rfp (get-area-origin 'sym:region-free-pointer))
       (rgp (get-area-origin 'sym:region-gc-pointer))
       (f))
      ((null area-list))
    (setq f (vfix (- (aref area-alloc-pointers area-number) (aref area-origins area-number))))
    (vwrite (+ rfp area-number) f)
    (vwrite (+ rgp area-number) f))

  ;; Allocate rest of address space to the free area
  (let ((fa (get-area-number 'sym:free-area))
	(high-loc (aref area-alloc-bounds (1- (length sym:area-list)))))
    (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-valid-size)
	    (vfix high-loc))
    (multiple-value-bind (ignore n-pages) (sys:find-disk-partition "PAGE")
      (vwrite (+ (get-area-origin 'sym:region-origin) fa) (vfix high-loc))
      (vwrite (+ (get-area-origin 'sym:region-length) fa)
	      (vfix (- (* n-pages sym:page-size) high-loc)))
      (vwrite (+ (get-area-origin 'sym:region-bits) fa) (vfix 0))))
  ;; Set up the area# and region# free lists
  (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-free-area/#-list)
	  (vfix (length sym:area-list)))
  (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-free-region/#-list)
	  (vfix (length sym:area-list)))
  (do i (length sym:area-list) (1+ i) (= i sym:size-of-area-arrays)	;all but the last
    (vwrite (+ (get-area-origin 'sym:region-list-thread) i) (vfix (1+ i)))
    (vwrite (+ (get-area-origin 'sym:area-region-list) i) (vfix (1+ i))))
  (vwrite (+ (get-area-origin 'sym:region-list-thread) sym:size-of-area-arrays) (vfix 0))
  (vwrite (+ (get-area-origin 'sym:area-region-list) sym:size-of-area-arrays) (vfix 0))
  ;; Make certain areas look full
  (dolist (area 'sym:(region-origin region-length region-free-pointer region-gc-pointer
		      region-bits region-list-thread area-name area-region-list
		      area-region-size area-maximum-size
		      linear-pdl-area linear-bind-pdl-area))
    (vwrite (+ (get-area-origin 'sym:region-free-pointer) (get-area-number area))
	    (vfix (* (get-area-size area) sym:page-size))))
  ;; Initialize unused portions of the disk
  (initialize-unused-pages)
  (init-region-sorted-by-origin)
  ;; Set up the page hash table to be empty except for the wired areas
  ;; which will look like they are at virtual=real addresses.
  ;; Cold-booting into this band will then do the right thing with it
  (init-area-contents 'sym:page-table-area (vfix 0))
  (dolist (area sym:area-list)			;For each wired area
    (and (eq area 'sym:region-free-pointer) (return))	;first non-wired area
    (let* ((area-number (get-area-number area))
	   (area-base (aref area-origins area-number))
	   (area-bound (aref area-alloc-bounds area-number)))
      (do ((pn (// area-base sym:page-size) (1+ pn))	;For each page in that area
	   (n (// area-bound sym:page-size)))
	  (( pn n))
	(cold-create-page area area-number pn pn))))
  ;; Terminate areas which have overlying lists
  (store-nxtnil-cdr-code 'sym:constants-area)
  (store-nxtnil-cdr-code 'sym:scratch-pad-init-area)
  (store-nxtnil-cdr-code 'sym:area-name)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-name-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-args-info-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-arglist-area)
  (store-nxtnil-cdr-code 'sym:micro-code-exit-area))

(defun initialize-unused-pages (&aux area address high)
  (dolist (area-name (memq 'sym:extra-pdl-area sym:area-list))	;no trash low fixed areas
    (setq area (get-area-number area-name)
	  address (aref area-alloc-pointers area)
	  high (aref area-alloc-bounds area))
    ;Page in all the fresh pages without really paging them in, thus initializing them
    (do ((vpn (// (+ address sym:page-size -1) sym:page-size) (1+ vpn))
	 (hpn (// (+ high sym:page-size -1) sym:page-size)))
	(( vpn hpn))
      (vmem-find-page (* vpn sym:page-size) nil))))

;Set up REGION-SORTED-BY-ORIGIN area.  Special notes:
;    Zero-length regions have to come before other regions at the same address
;    [alternatively they could not appear at all]
(defun init-region-sorted-by-origin ()
  (do ((mum (make-sorted-region-list)
	    (or (cdr mum) mum)) ;replicate last entry
       (i 0 (1+ i))
       (rso (get-area-origin 'sym:region-sorted-by-origin)))
      ((> i sym:size-of-area-arrays))
    (vwrite (+ rso i) (vfix (cdar mum)))))

(defun make-sorted-region-list ()
  (sort (do ((i 0 (1+ i))
	     (al sym:area-list (cdr al))
	     (l nil))
	    ((null al)
	     (nreverse l))
	  (or (eq (car al) 'sym:free-area)
	      (push (cons (aref area-origins i) i) l)))
	(function (lambda (x y)
	   (cond ((= (car x) (car y))		;if one is zero length, it -must- go first
		  (cond ((= (aref area-origins (cdr x)) (aref area-alloc-bounds (cdr x))) t)
			((= (aref area-origins (cdr y)) (aref area-alloc-bounds (cdr y))) nil)
			((ferror nil "2 non-zero-length areas at same address"))))
		 ((< (car x) (car y))))))))

;Set up paging data structures
(defun cold-create-page (area area-number virpage phypage)
  (do ((pht-mask (- sym:size-of-page-table 2))
       (access-and-status-code
	  (cond ((memq area sym:pdl-buffer-area-list) 5)
		((memq area sym:read-only-area-list) 12)
		(t 14)))
       (meta-bits (cond ((memq area sym:list-structured-areas) 60)
			((eq area 'sym:extra-pdl-area) 44)
			(t 64))) ;symbolic?????
       (swap-status-code 
	  (cond ;((memq area sym:pdl-buffer-area-list) sym:%pht-swap-status-pdl-buffer)
		((memq area sym:wired-area-list) sym:%pht-swap-status-wired)
		(t sym:%pht-swap-status-normal)))
       (hash (logxor (ash virpage 2) (ash virpage -6)) (+ hash 2))
       (pht (get-area-origin 'sym:page-table-area))
       (ppd (get-area-origin 'sym:physical-page-data)))
      (nil)
    (cond ((= 0 (ldb sym:%%pht1-valid-bit
		     (vread (+ pht (setq hash (logand hash pht-mask))))))
	   (vwrite (+ pht hash)
		   (vfix (dpb virpage sym:%%pht1-virtual-page-number
			      (dpb swap-status-code sym:%%pht1-swap-status-code
				   (dpb 1 sym:%%pht1-valid-bit 0)))))
	   (vwrite (+ pht hash 1)
		   (vfix (dpb access-and-status-code sym:%%pht2-access-and-status-bits
			      (dpb meta-bits sym:%%pht2-meta-bits 
				   (dpb phypage sym:%%pht2-physical-page-number 0)))))
	   (vwrite (+ ppd phypage)
		   (dpb area-number 2020 hash))
	   (return t)))))

;;; Driver

(defvar qfasl-file-list '(	"AI:LMFONT;CPTFON QFASL"
				"AI:LISPM;QRAND QFASL"
				"AI:LMIO;QIO QFASL"
				;"AI:LMIO;RDTBL QFASL" ;done specially
				"AI:LMIO;READ QFASL"
				"AI:LMIO;PRINT QFASL"
				"AI:LMWIN;COLD QFASL"
				"AI:LISPM;SGFCTN QFASL"
				"AI:LISPM;QEV QFASL"
				"AI:LISPM;LTOP QFASL"
				"AI:LISPM;QFASL QFASL"
				"AI:LMIO;MINI QFASL"
				"AI:LISPM;LFL QFASL"  ))

(defvar cold-list-area 'sym:init-list-area)	;Where FROID (COLDLD) puts lists (usually)
(defvar evals-to-be-sent-over)

;User calls this to build a cold-load onto a band
(defun make-cold (part-name)
  (cond ((y-or-n-p (format nil "May I smash the /"~A/" partition, which contains /"~A/"?"
			       part-name (si:partition-comment part-name 0)))
	 (si:update-partition-comment part-name "cold" 0)
	 (or (boundp 'big-fixnum) (load-parameters))
	 ;; Flush old state
	 (mapatoms #'(lambda (x) (remprop x 'q-atom-head)) sym-package nil)
	 (setq evals-to-be-sent-over nil)
	 (unwind-protect (progn (vmem-initialize part-name)
				(make-cold-1)
				(format nil "Boot off the ~A partition to test it."
					    part-name))
	   (vmem-finish)))))

(defun make-cold-1 ()
  ;; Divide up virtual memory into areas and initialize tables
  (assign-values sym:area-list 0)
  (create-areas)
  (make-t-and-nil)
  ;; Initialize various fixed areas and really random data tables
  (init-area-contents 'sym:area-name qnil)
  (store-list-of-atoms 'sym:area-name sym:area-list)
  (storein-q-value-cell 'sym:area-list	;Is this going to win?
			(vmake-pointer sym:dtp-list (get-area-origin 'sym:area-name)))
  (mapc (function store-constant) sym:constants-page)	;set up constants page
  (storein-q-value-cell 'sym:constants-page
			(vmake-pointer sym:dtp-list (get-area-origin 'sym:constants-area)))
  (init-scratch-pad-area)
  (init-system-communication-area)
  (fix-certain-variables)
  (mapc (function store-lisp-value-list) sym:q-corresponding-variable-lists)
  (init-random-variables)
  (store-a-mem-location-names)
  (setq micro-code-entry-vector nil)
  (store-misc-u-entry-links)
  ;;Load up all those QFASL files
  (mapc 'cold-fasload qfasl-file-list)
  ;;Don't let list-structure portion of the readtable end up in a read-only area
  (let ((cold-list-area 'sym:property-list-area))  ;Random list-structured area
    (cold-fasload "AI:LMIO;RDTBL QFASL"))
  ;;THIS KLUDGE FIXES UP MACROS, SINCE THE FUNCTION MACRO IS NOT DEFINED YET
  ;;(BY SPECIAL DISPENSATION WE HAVE DEFPROP, PUTPROP, AND SPECIAL AROUND)
  ;;FURTHERMORE, SETQ ISN'T DEFINED YET, LOAD-TIME-SETQ FASL-OP SHOULD HAVE BEEN USED
  (do l evals-to-be-sent-over (cdr l) (null l)
    (cond ((memq (caar l) 'sym:(setq and or cond))
	   (ferror nil "~A will get undefined function during initialization" (car l)))
	  ((eq (caar l) 'sym:macro)
	   (rplaca l (sublis (list (cons 'fcn (cadar l))
				   (cons 'name (caddar l))
				   (cons 'body (cdddar l)))
			     '(sym:fset (sym:quote fcn)
					(sym:quote (sym:macro
						     . (sym:lambda name . body)))))))))
 (setq evals-to-be-sent-over (nreverse evals-to-be-sent-over)) ;do in order specified
 (storein-q-value-cell 'sym:lisp-crash-list
		       ;; This MAKE-Q-LIST must not use the FASL-TEMP-AREA,
		       ;; because the list structure being created includes
		       ;; definitions of important macros.  The area used
		       ;; must not be an immediate write area.
		       (make-q-list 'sym:init-list-area evals-to-be-sent-over))
 ;;Everything compiled, etc. close off and write it out
 (format t "~&q-storage-finalize...")
 (q-storage-finalize))

;nil and t must be stored manually since qnil and qtruth would not be bound when needed
(defun make-t-and-nil ()
  (setq qnil (vmake-pointer sym:dtp-symbol
		      (allocate-block 'sym:resident-symbol-area sym:length-of-atom-head)))
  (vwrite-cdr qnil sym:cdr-next (vmake-pointer sym:dtp-symbol-header
					       (store-string 'sym:p-n-string "NIL")))
  (vwrite-cdr (+ qnil 1) sym:cdr-next qnil)
  (vwrite-cdr (+ qnil 2) sym:cdr-next (vmake-pointer sym:dtp-null qnil))
  (vwrite-cdr (+ qnil 3) sym:cdr-next qnil)
  (vwrite-cdr (+ qnil 4) sym:cdr-next qnil)
  (putprop 'sym:nil qnil 'q-atom-head)
  (setq qtruth (vmake-pointer sym:dtp-symbol
		   (allocate-block 'sym:resident-symbol-area sym:length-of-atom-head)))
  (vwrite-cdr qtruth sym:cdr-next (vmake-pointer sym:dtp-symbol-header
						 (store-string 'sym:p-n-string "T")))
  (vwrite-cdr (+ qtruth 1) sym:cdr-next qtruth)
  (vwrite-cdr (+ qtruth 2) sym:cdr-next (vmake-pointer sym:dtp-null qtruth))
  (vwrite-cdr (+ qtruth 3) sym:cdr-next qnil)
  (vwrite-cdr (+ qtruth 4) sym:cdr-next qnil)
  (putprop 'sym:t qtruth 'q-atom-head))

;Fix the values of certain variables before they are sent over
(defun fix-certain-variables ()
  (dolist (sym '(sym:a-memory-virtual-address sym:unibus-virtual-address
		 sym:io-space-virtual-address))
    (set sym (lsh (ash (symeval sym) -3) 3)))	;Change from bignum to fixnum
  (setq sym:prin1 nil)
  (setq sym:base (setq sym:ibase 8))
  (setq sym:*nopoint nil)
  (setq sym:for-cadr t))	;Is this still used?

;Initializations of all sorts of random variables.  Must follow the map
;over q-corresponding-variable-lists, because previous initializations are stored over.
(defun init-random-variables ()
  ;;set up array-types symbol (both value and function cells).
  ;;  the function cell is an array which gives maps numeric array type to symbolic name.
  ;;  the value cell is a list pointer into the above array, so is an ordered list
  ;;   of the array types.
  (init-q-array 'sym:control-tables 'sym:array-types 2 'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables sym:array-types)
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  (storein-q-value-cell 'sym:array-types
    (vmake-pointer sym:dtp-list (- (aref area-alloc-pointers
					 (get-area-number 'sym:control-tables))
				   32.)))
  ;;set up the array-elements-per-q array.
  (init-q-array 'sym:control-tables 'sym:array-elements-per-q 2 ;fcn
		'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables (make-ordered-array-list sym:array-elements-per-q))
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  ;;value cell of array-elements-per-q has assq list, is not same as array.
  ;;set up the array-bits-per-element array, similar
  (init-q-array 'sym:control-tables 'sym:array-bits-per-element 2 ;fcn
		'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables
		       (make-ordered-array-list sym:array-bits-per-element))
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  ;;set up q-data-types
  (init-q-array 'sym:control-tables 'sym:q-data-types 2 'sym:art-q-list '(32.) nil 
		(list (make-q-list 'sym:init-list-area (length sym:q-data-types))))
  (store-list-of-atoms 'sym:control-tables sym:q-data-types)
  (store-nils 'sym:control-tables (- 32. (length sym:q-data-types)))
  (storein-q-value-cell 'sym:q-data-types
    (vmake-pointer sym:dtp-list (- (aref area-alloc-pointers
					 (get-area-number 'sym:control-tables))
				   32.))))
