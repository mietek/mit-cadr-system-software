;;; -*- Mode: LISP; Package: SYSTEM-INTERNALS -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This package provides for the definition and use of SELECTQ type
;;; objects that use the DTP-SELECT-METHOD microcode feature to allow
;;; destructuring of args on a per-operation basis

;(DEFSELECT <function-name or (<function-name> <function-to-be-called-if-no-match>)>
;  (<keyword or (<keyword> <keyword> ... <keyword>)> <arglist>
;    <forms>))

; (DEFSELECT FILE-CHAOSNET-COMMAND
;   (:FOO (BAZ &RESET BAR)
;         (DO-SOME-WORK))
;   (:SPAZZ (&OPTIONAL (BAR 1))
;           (SPZZA)))

(declare (special **defselect-alist** **defselect-function**))

(defmacro DEFSELECT (**defselect-function** &rest methods &aux no-which-operations)
  (let ((**defselect-alist** nil)
        (tail-pointer)
        (wo-list-name))
     (cond ((listp **defselect-function**)
            (setq tail-pointer (cadr **defselect-function**)
		  no-which-operations (caddr **defselect-function**))
            (setq **defselect-function** (car **defselect-function**))))
     (setq wo-list-name
           (intern (string-append "{" **defselect-function** "-" "WHICH-OPERATIONS" "}")))
    `(progn 'COMPILE
       ,@(mapcar (function defselect-internal) methods)
       (declare (special ,wo-list-name))
       ,(or no-which-operations
	    (defselect-internal `(:WHICH-OPERATIONS (&rest ignore) ,wo-list-name)))
       (declare (*EXPR ,**defselect-function**))
       (defselect-add-methods ',**defselect-function** ',**defselect-alist** ',tail-pointer)
       ,(or no-which-operations
	    `(setq ,wo-list-name
		   (delq ':WHICH-OPERATIONS
			 (do ((ops (%make-pointer dtp-list (fsymeval ',**defselect-function**))
				   (cdr ops))
			      (list nil (cons (caar ops) list)))
			     ((atom ops) list))
			 1_22.))))))

(defun defselect-internal (method)
  (cond ((listp (car method))
         (prog1
          (defselect-internal (cons (caar method) (cdr method)))
          (do ((l (cdar method) (cdr l))
               (name (cond ((atom (cdr method)) (cdr method))
                           (t (intern (string-append "{" **defselect-function** "-"
                                                     (caar method) "}"))))))
              ((null l))
              (setq **defselect-alist** (cons (cons (car l) name) **defselect-alist**)))))
        (t (cond ((atom (cdr method))
                  (setq **defselect-alist** (cons (cons (car method) (cdr method))
                                                  **defselect-alist**))
                  nil)
                 (t (let ((name (intern (string-append "{" **defselect-function** "-"
                                                       (car method) "}"))))
                      (setq **defselect-alist** (cons (cons (car method) name)
                                                      **defselect-alist**))
                      `(defun ,name
                         ,(cons '**defselect-op**	;First argument is operation
				(cadr method))		;Remaining args are those specified
			 **defselect-op**		;Suppress bound but not used warning
			 . ,(cddr method))))))))

(defun defselect-add-methods (function alist &optional tail-pointer)
  (let ((current-definition (and (fboundp function)
				 (= (%data-type (fsymeval function)) dtp-select-method)
                                 (%make-pointer dtp-list (fsymeval function)))))
    (do ((next-alist alist (cdr next-alist)))
        ((null next-alist))
      (do ((next-current-def current-definition (cdr next-current-def)))
          ((cond ((atom next-current-def)
                  (setq current-definition (cons (car next-alist) current-definition))
                  t)
                 ((eq (caar next-alist) (caar next-current-def))
                  (rplaca next-current-def (car next-alist))
                  t)))))
    (and tail-pointer (rplacd (last current-definition) tail-pointer))
    (fset-carefully function (%make-pointer dtp-select-method current-definition))))
