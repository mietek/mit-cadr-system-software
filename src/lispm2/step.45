 ;;; Ultra-simple stepper for lisp-machine.   -*-LISP-*-
 ;;; Wins with multiple values
 ;;; Does not attempt to win with editor top level
 ;;; Compile with QC-FILE.


;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;NOTES:
; The way it decides whether it needs to reprint the form when showing
; you the values is pretty kludgey right now.  Can it check the cursorpos
; or ask itself whether it typed anything or something?
;
; Would like to be able to evaluate and/or substitute in atoms and forms
; without having to break first.
;
; Would like to be able to type A and have it stop after evaluating the
; args, before calling the function.
;
; Raid registers
;
; Hook up to DDT?
; 
; If an error happens, user should be able to throw back into the stepper.

(special step-level            ;depth in recursion
         step-array            ;contains pending forms
         step-max              ;max depth at which to type out
	 step-form	       ;current form, user may change in a breakpoint
         step-value	       ;first value, changing this in a bkpt returns it
         step-values	       ;list of values, may be changed in a breakpoint
	 evalhook
)

;Main entry point.
(defun step (form)
  (setq step-level -1 step-max 0)
  (or (boundp 'step-array)
      (setq step-array (make-array nil 'art-q 200)))
  (step-eval form))

;This is for TRACE, mainly.  The idea is to do an apply,
;stepping under it but not showing the user the apply itself.
(defun step-apply (fcn args &aux (evalhook (function step)))
  (bind (function-cell-location 'eval) (function evalhook1))
  (apply fcn args))

;Check for macros, they are treated specially.
(defun step-macro-p (form)
  (and (listp form)
       (symbolp (car form))
       (fboundp (car form))
       (listp (setq form (car (function-cell-location (car form)))))
       (eq (car form) 'macro)))

;Print a form, suitably indented, marked, and truncated to one line.
(defun step-print-form (form level)
  (terpri)
  (do n (* 2 level) (1- n) (= n 0)
    (tyo #\SP))
  (tyo (cond ((step-macro-p form) #/)
             (t #/)))
  (tyo #\SP)
  (print-truncated form 75.))

;print whatever is necessary, read a command, set special variables
;and return how to proceed:  eval (just eval), evalhook (recurse), more options later.
;If calling for eval, step-values is nil, otherwise calling for return.
(defun step-cmdr (form values print-form-p)
  (prog (ch ch1
	 (standard-input query-io)
	 (standard-output query-io))
    (and print-form-p (step-print-form form step-level))
 pv (do ((l values (cdr l))
         (ch #/ #/))
        ((null l))
      (terpri-if-insufficient-space 80.)
      (tyo #\SP) (tyo ch) (tyo #\SP)
      (print-truncated (car l) 100.))
 rd (setq ch1 (funcall standard-input ':tyi))
    (setq ch (char-upcase ch1))
    (cond ((= ch #\CALL) (break call t))
          ((= ch #\SP) (setq step-max step-level) (return 'eval))
          ((= ch 525) (setq step-max (max 0 (1- step-level))) (return 'eval))
          ((= ch 516) (setq step-max (1+ step-level)) (return 'evalhook))
          ((= ch 530) (setq step-max -1) (return 'eval))
          ((= ch 502)
           (break step t)
           (setq ch 0)
           (as-1 step-form step-array step-level)
           (go redis1))
          ((= ch 505)
           (ed)
           (setq ch 10.)
           (go redisplay))
          ((or (= ch 214) (= ch 514))
           (setq ch 10.)
           (go redisplay))
	  ((= ch 1114)
	   (setq ch 10.)
	   (go redis1))
          ((= ch 1514)
           (setq ch step-level)
           (go redisplay))
          ((or (= ch 507) (= ch 524))
           (setq ch (cond ((= ch 507) (function grind-top-level)) ((function print))))
           (cond ((null values) (funcall ch form))
                 ((do l values (cdr l) (null l)
                    (funcall ch (car l)))))
           (go rd))
	  ((memq (logand ch 377) '(#/? #\HELP))
	   (terpri)
	   (princ
	     (cond ((null step-values) "You are about to evaluate the above form.")
		   (t "You have evaluated a form and are about to return the above values.")))
	   (terpri)
	   (princ "Commands are single characters, usually control, which don't echo:")
	   (terpri)
	   (princ "N to next thing evaled, <space> next thing at same level, U up a level")
	   (terpri)
	   (princ "X up all levels (exit), E escape to editor, T retype in full, G grind")
	   (terpri)
	   (princ "B break: STEP-FORM is the form, STEP-VALUES is the list of values,")
	   (terpri)
	   (princ "          STEP-VALUE is the first value.  If you change these, it wins.")
	   (terpri)
	   (princ "L or <form> clear & show last 10., L don't clear, L clear & show all")
	   (terpri)
	   (princ "Just type and it will be read, evaluated, and printed")
	   (terpri)
	   (princ "Magic flags:  form,  macro,  values,  next value")
	   (setq ch 0)
	   (go redis1))
	  ((< ch 200)
	   (funcall standard-input ':untyi ch1)
	   (princ " Eval: ")
	   (print (eval (read-for-top-level)))
	   (terpri)
	   (setq ch 0)
	   (go redis1))
          (t (beep)
             (go rd)))
 redisplay 
    (funcall standard-output ':clear-screen)
 redis1 
    (do i (max 0 (- step-level ch)) (1+ i) (> i step-level)
      (step-print-form (ar-1 step-array i) i))
    (go pv)))

;This is evalhooked in in place of EVAL.  Works by calling step-cmdr
;to let the user see what's going on and say what to do, then continues
;evaluation using either EVAL or EVALHOOK based on what the user typed.
;Has special hair for macros and for atoms.
(defun step-eval (step-form)
  (prog ((step-level (1+ step-level)) step-value step-values tem val)
    (and (>= step-level (ARRAY-LENGTH step-array))
         (adjust-array-size step-array (+ 100 step-level)))
 mc (as-1 step-form step-array step-level)
    (cond ((nlistp step-form)
           (setq step-values (list (eval step-form)))
           (setq tem 'atom)
           (go rl))
          ((<= step-level step-max)
           (setq tem (step-cmdr step-form nil t)))
          (t (setq tem 'eval)))
    (cond ((step-macro-p step-form)
           (setq step-form (funcall (cdar (function-cell-location (car step-form)))
                                    step-form))
           (go mc))
          ((eq tem 'eval)
           (setq step-values (multiple-value-list (eval step-form))))
          ((eq tem 'evalhook)
           (setq step-values (multiple-value-list (evalhook step-form (function step-eval)))))
          ((ferror nil "Unknown function ~S" tem)))
 rl (setq step-value (setq val (car step-values)))
    (cond ((<= step-level step-max)
           (setq tem (step-cmdr step-form step-values (neq tem 'eval))))
          (t (setq tem 'eval)))
    (and (neq step-value val) (return step-value))
 rt (cond ((null (cdr step-values)) (return (car step-values)))
          (t (return-next-value (car step-values))
             (setq step-values (cdr step-values))
             (go rt)))))

;PRINT abbreviated spacewise rather than listwise

(SPECIAL PRINT-TRUNCATED) ;YECH

(DEFUN TERPRI-IF-INSUFFICIENT-SPACE (PERCENT-WIDTH)
  (LET ((X (// (* PERCENT-WIDTH (FUNCALL STANDARD-OUTPUT ':INSIDE-SIZE)) 100.)))
    (AND ( (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':PIXEL) X)
	 (TERPRI))))

(DEFUN PRINT-TRUNCATED (SEXP PERCENT-WIDTH)
  (LET ((PRINT-TRUNCATED (// (* PERCENT-WIDTH (FUNCALL STANDARD-OUTPUT ':INSIDE-SIZE)) 100.)))
    (*CATCH 'PRINT-TRUNCATED
	    (PRIN1 SEXP (CLOSURE '(PRINT-TRUNCATED STANDARD-OUTPUT)
				 (FUNCTION PRINT-TRUNCATED-STREAM))))))

(DEFUN PRINT-TRUNCATED-STREAM (OP &OPTIONAL ARG1 &REST REST)
  (SELECTQ OP
    (:TYO
     (COND (( (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':PIXEL)
		PRINT-TRUNCATED)
	    (*THROW 'PRINT-TRUNCATED NIL))
	   (T (FUNCALL STANDARD-OUTPUT ':TYO ARG1))))
    (:WHICH-OPERATIONS '(:TYO))
    (OTHERWISE
     (STREAM-DEFAULT-HANDLER 'PRINT-TRUNCATED-STREAM OP ARG1 REST))))
