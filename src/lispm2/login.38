; LISP Machine Package for Logging In and Out.		DLW 11/13/77 -*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;; List of forms to be evaluated on logout
;; to undo the things done at login.
;; The various LOGIN-MUMBLE functions push undo forms on this list.
(DEFVAR LOGOUT-LIST NIL)

;; MACHINE-OR-T is the machine to get the init and fix files from (default = AI)
;; or T meaning dont do any of that hair.
(DEFUN LOGIN (USER-NAME &OPTIONAL (MACHINE-OR-T "AI"))
  (LOGOUT)
  (SETQ USER-ID (STRING-TRIM '(#\SP) (STRING USER-NAME)))
  (FS:FILE-LOGIN USER-ID)
  (COND ((NOT (EQ MACHINE-OR-T T))
	 (SETQ MACHINE-OR-T (STRING MACHINE-OR-T)) ;canonicalize for ASSOC's in FS:FILE...
	 (LET ((FILE-NAME (FS:FILE-PARSE-NAME (FS:FILE-USER-ID-HSNAME MACHINE-OR-T T)
					      MACHINE-OR-T)))
	   (FUNCALL FILE-NAME ':INIT-FILE "LISPM")
	   (LOAD FILE-NAME "USER" T))))
  T)

(DEFUN LOGOUT ()
  (MAPC 'EVAL LOGOUT-LIST)
  (SETQ USER-ID ""
	FS:USER-HSNAMES NIL FS:USER-PERSONAL-NAME "" FS:USER-GROUP-AFFILIATION #/-
	FS:USER-LOGIN-MACHINE "AI")
  (SETQ LOGOUT-LIST NIL)
  (FS:FILE-LOGIN NIL)
  T)

(DEFUN LOGIN-EVAL (X)  ;Value returned by such a form is how to undo it
    (PUSH X LOGOUT-LIST))

(DEFUN LOGIN-SETQ (&QUOTE &REST L)  ;Undoing SETQ
    (DO L L (CDDR L) (NULL L)
      (COND ((BOUNDP (CAR L))
	     (PUSH `(SETQ ,(CAR L) ',(SYMEVAL (CAR L))) LOGOUT-LIST))
	    (T (PUSH `(MAKUNBOUND ',(CAR L)) LOGOUT-LIST)))
      (SET (CAR L) (EVAL (CADR L)))))

;Undoable FDEFINE.
;It would be nice if there were FUNDEFINE.
(DEFUN LOGIN-FDEFINE (FUNCTION-NAME DEFINITION)  ;Undoing FDEFINE
    (AND (FDEFINEDP FUNCTION-NAME)
	 (PUSH `(FDEFINE ',FUNCTION-NAME ',(FDEFINITION FUNCTION-NAME)) LOGOUT-LIST))
    (FDEFINE FUNCTION-NAME DEFINITION))
