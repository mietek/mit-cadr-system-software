;-*- Mode:Lisp; Package:User -*-		Timing Package
;Not for use with recursive functions

(declare (special time-function-alist))
 ;Entries are: (function-name old-definition timing-variable count-variable)
 ; timing-variable is number of microseconds, count-variable is number of calls.
 ; optionally, for the wherein feature, the list contains the following
 ;  additional elements: (function-it-is-in old-function-name fef-offset)

(defun time-function (function-name
		      &optional (timing-variable (gensym)) (count-variable (gensym))
				wherein-data)
  (set timing-variable 0)
  (set count-variable 0)
  (let ((old-function (fdefinition function-name)))
    (or (boundp 'time-function-alist) (setq time-function-alist nil))
    (push (list* function-name old-function timing-variable count-variable wherein-data)
	  time-function-alist)
    (fdefine function-name
	     `(lambda (&rest .args.)
		(local-declare ((special ,timing-variable ,count-variable))
		  (let ((.time. (time:fixnum-microsecond-time)))
		    (let ((.vals. (multiple-value-list (lexpr-funcall ',old-function
								      .args.))))
		      (setq ,timing-variable (+ (time-difference (time:fixnum-microsecond-time)
								 .time.)
						,timing-variable)
			    ,count-variable (1+ ,count-variable))
		      (prog () (return-list .vals.))))))
	     nil t)
    (let ((si:inhibit-fdefine-warnings t)) ;crockish error message inside compiler
      (compile function-name))))

(defun time-function-wherein (function-name wherein-function-name
			      &optional (timing-variable (gensym)) (count-variable (gensym))
			      &aux tem)
   (let ((replacement-function-name
	  (make-symbol (format nil "[~S wherein ~S]" function-name wherein-function-name)))
	(fef (cond ((setq tem (assoc wherein-function-name time-function-alist))
		    (cadr tem))
		   ((fdefinition wherein-function-name)))))
    (fset replacement-function-name (fdefinition function-name))
    (or (= (%data-type fef) dtp-fef-pointer)
	(ferror nil "~S - not a fef" fef))
    (do ((loc (function-cell-location function-name)) ;has to be a symbol I guess
	 (idx sys:%fef-header-length (1+ idx))
	 (lim (// (si:fef-initial-pc fef) 2))
	 (ptr))
	((>= idx lim) (ferror nil "~S not found in ~S" function-name wherein-function-name))
      (cond ((= (%p-ldb-offset %%q-data-type fef idx)
                dtp-external-value-cell-pointer)
             (setq ptr (%p-contents-as-locative-offset fef idx))
	     (cond ((eq ptr loc)
		    (without-interrupts
		      (%p-store-tag-and-pointer
			(+ (%pointer fef) idx)
			(%p-ldb-offset %%q-all-but-pointer fef idx)

			(function-cell-location replacement-function-name)))
		    (return (time-function replacement-function-name
					   timing-variable count-variable
					   (list fef function-name idx))))))))))
(defun untime-function (function-name)
  (let ((tem (assoc function-name time-function-alist)) (tem1))
    (cond (tem
	    (fdefine function-name (cadr tem) nil t)
	    (cond ((setq tem1 (cddddr tem))	;wherein
		   (let ((fef (car tem1))
			 (old-function-name (cadr tem1))
			 (idx (caddr tem1)))
		     (without-interrupts
		       (%p-store-tag-and-pointer
			 (+ (%pointer fef) idx)
			 (%p-ldb-offset %%q-all-but-pointer fef idx)
			 (function-cell-location old-function-name))))))
	    (setq time-function-alist (delq tem time-function-alist))))))

(defun untime-all ()
  (dolist (x time-function-alist)
    (untime-function (car x))))

(defun list-timed-functions ()
  (dolist (x time-function-alist)
    (format t "~&~S~%" (car x))))

(defun reset-times ()
  (dolist (x time-function-alist)
    (set (caddr x) 0)
    (set (cadddr x) 0)))

(defun print-times (&aux tm ct)
  (dolist (x time-function-alist)
    (format t "~&~S: ~S seconds, called ~D times, ~S seconds per call.~%"
	      (car x) (setq tm (// (symeval (caddr x)) 1e6))
	      (setq ct (symeval (cadddr x)))
	      (// tm (max 1 ct)))))

;;; Map measuring stuff

(defun map-time-call (form &aux val map1atb map2atb mapmatb pdlratb)
  (let ((initial-time (time:microsecond-time))
	(initial-map1 (si:read-meter 'si:%count-first-level-map-reloads))
	(initial-map2 (si:read-meter 'si:%count-second-level-map-reloads))
	(initial-mapm (si:read-meter 'si:%count-meta-bits-map-reloads))
	(initial-pdlr (si:read-meter 'si:%count-pdl-buffer-read-faults))
	(initial-pdlw (si:read-meter 'si:%count-pdl-buffer-write-faults))
	(initial-pdlm (si:read-meter 'si:%count-pdl-buffer-memory-faults))
	(initial-dskr (si:read-meter 'si:%count-disk-page-reads))
	(initial-dskw (si:read-meter 'si:%count-disk-page-writes)))
    (setq val (eval form))
    (let ((final-time (time:microsecond-time))
	  (final-map1 (si:read-meter 'si:%count-first-level-map-reloads))
	  (final-map2 (si:read-meter 'si:%count-second-level-map-reloads))
	  (final-mapm (si:read-meter 'si:%count-meta-bits-map-reloads))
	  (final-pdlr (si:read-meter 'si:%count-pdl-buffer-read-faults))
	  (final-pdlw (si:read-meter 'si:%count-pdl-buffer-write-faults))
	  (final-pdlm (si:read-meter 'si:%count-pdl-buffer-memory-faults))
	  (final-dskr (si:read-meter 'si:%count-disk-page-reads))
	  (final-dskw (si:read-meter 'si:%count-disk-page-writes)))
      (let ((time (- final-time initial-time))
	    (map1 (- final-map1 initial-map1))
	    (map2 (- final-map2 initial-map2))
	    (mapm (- final-mapm initial-mapm))
	    (pdlr (- final-pdlr initial-pdlr))
	    (pdlw (- final-pdlw initial-pdlw))
	    (pdlm (- final-pdlm initial-pdlm))
	    (dskr (- final-dskr initial-dskr))
	    (dskw (- final-dskw initial-dskw)))
	(format t "~&Elapsed time ~D usec, map1 ~D, map2 ~D, map-meta ~D, pdlr ~D, pdlw ~D, pdlm ~D, dskr ~D, dskw ~D~%"
		  time map1 map2 mapm pdlr pdlw pdlm dskr dskw)
	(format t "ATB usec: map1 ~D, map2 ~D, map-meta ~D, pdlr ~D, pdlw ~D, pdlm ~D, dskr ~D, dskw ~D~%"
		(setq map1atb (// time (max 1 map1)))
		(setq map2atb (// time (max 1 map2)))
		(setq mapmatb (// time (max 1 mapm)))
		(setq pdlratb (// time (max 1 pdlr)))
		(// time (max 1 pdlw))
		(// time (max 1 pdlm))
		(// time (max 1 dskr))
		(// time (max 1 dskw)))
	(format t "map-meta//map2 = ~D, map occupancy = ~D~%"
		  (// (float mapm) (max 1 map2))
		  (// (float (+ map2 mapm)) (max 1 (* 32. map1))))
	(format t "Overheads: map1 ~D%, map2 ~D%, mapm ~D%, pdlr ~D%~%"
		(// 2600.0 map1atb)
		(// 860.0 map2atb)
		(// 2500.0 mapmatb)
		(// 300.0 pdlratb)))))
  val)

(defun time-funcall (n-times function &rest args)
  (do ((start-time (time:fixnum-microsecond-time))
       (n n-times (1- n)))
      ((zerop n)
       (let ((end-time (time:fixnum-microsecond-time)))
	 (format t "~& Time for ~S is ~D microseconds."
		   (cons function args)
		   (// (time-difference end-time start-time) n-times))))
    (apply function args)))
