; -*- Mode:Lisp; Package:User; Lowercase:T -*-

; categories is a list of base flavors to separate out by, if nil is
;    included all flavors that don't fit get printed
; verbose-p means to show dependencies, instance variables, and methods
; name-font is the Bolio font number in which to print names of things

(defun list-all-flavors (output-filename
			 &optional (categories '(zwei:editor zwei:zwei
						 tv:basic-frame tv:basic-menu
						 tv:basic-scroll-window
						 tv:text-scroll-window
						 tv:basic-typeout-window
						 tv:screen tv:sheet tv:blinker
						 nil))
				   (verbose-p t)
			           (name-font 7)
			 &aux (s (open output-filename '(:print)))
			      flavlist fl doc tem flavor-flavor (pagep nil))
  ;; Document where this information came from
  (format s ".c -*- Mode:Text -*-~2%Flavor documentation for system ~A on ~A, as of"
	    sys:system-version-string (chaos:host-data))
  (chaos:what-date s)
  (format s "~2%.ragged_right 200~2%")
  ;; First make flavlist.  Entries are (category . list of flavor names)
  (setq flavlist (mapcar #'ncons categories))
  (dolist (f *all-flavor-names*)
    (dolist (x flavlist)
      (cond ((or (null (car x)) (flavor-depends-p f (car x)))
	     (push f (cdr x))
	     (return t)))))
  ;; Within each category, print flavor names alphabetically
  (dolist (ffl flavlist)
    (and pagep (format s ".page~%"))
    (setq pagep t)
    (format s "Flavors built on ~:[nothing in particular~;~D~S*~]~2%.table 8~%"
	      name-font (car ffl) (car ffl))
    (dolist (f (sort (cdr ffl) #'alphalessp))
      (setq fl (get f 'si:flavor)
	    doc (get (locf (si:flavor-plist fl)) ':documentation))
      (setq flavor-flavor (dolist (x doc) (and (symbolp x) (return x)))
	    doc (dolist (x doc) (and (stringp x) (return x))))
      (format s ".item ~S~:[~; 2~A*~]~%"
	        f flavor-flavor (string-downcase flavor-flavor))
      (cond ((not (null doc))
	     (funcall s ':string-out doc)
	     (funcall s ':tyo #\cr)))
      (cond (verbose-p
	     (and (setq tem (get f ':source-file-name))
		  (format s "~%Defined in file ~D~A*~%" name-font tem))
	     (and (setq tem (si:flavor-depends-on fl))
		  (format s "~%Directly depends on ~D~{~S~^, ~}*~%"
			    name-font (sort (copylist tem) #'alphalessp)))
	     (and (setq tem (si:flavor-includes fl))
		  (format s "~%Directly includes ~D~{~S~^, ~}*~%"
			    name-font (sort (copylist tem) #'alphalessp)))
	     (and (setq tem (si:flavor-depended-on-by fl))
		  (format s "~%Directly depended on by ~D~{~S~^, ~}*~%"
			    name-font (sort (copylist tem) #'alphalessp)))
	     (and (setq tem (si:flavor-local-instance-variables fl))
		  (format s "~%Defines instance variables ~D~{~S~^, ~}*~%"
			    name-font
			    (sort (mapcar #'(lambda (x) (if (atom x) x (car x))) tem)
				  #'alphalessp)))
	     (and (setq tem
		    (mapcan #'(lambda (mte)
			(mapcan #'(lambda (x)
			    (and (neq (car x) ':combined)
				 (ncons (list (car x) (car mte)))))
			    (cdddr mte)))
			 (si:flavor-method-table fl)))
		  (format s "~%Has methods ~D~:{~:[~;~:*:~A ~]:~A~:^, ~}*~%"
			    name-font
			    (sort tem #'(lambda (x y) (alphalessp (cadr x) (cadr y))))))
	     ))
      (terpri s))
    (format s ".end_table~%"))
  (close s))

(defun flavor-depends-p (fl1 fl2 &aux tem)
  (or (eq fl1 fl2)
      (and (setq tem (si:flavor-depends-on-all
		           (setq fl1 (get fl1 'si:flavor))))
	   (memq fl2 tem))
      (dolist (fl3 (si:flavor-depends-on fl1))
	(and (flavor-depends-p fl3 fl2) (return t)))
      (dolist (fl3 (si:flavor-includes fl1))
	(and (flavor-depends-p fl3 fl2) (return t)))))

(defun list-methods-for-object (object file-name)
  (let ((s (open file-name 'print))
	(wo (sort (copylist (funcall object ':which-operations)) #'alphalessp))
	(fl (typep object)))
    (format s "Methods for flavor ~S~%~%" fl)
    (dolist (m wo)
      (format s ":~A  ~A~%" m (arglist (get-handler-for object m))))
    (close s)))
