;-*- MODE: LISP; PACKAGE: COMPILER -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; Compile many driven off a LOAD-FILE-LIST assoc-list 

(DECLARE (SPECIAL SYSTEM-FILE-ALIST))

;Compile all the files in the alist LST which have been edited since last compiled.
(DEFUN COMPILE-FILE-ALIST (&OPTIONAL (ALIST SYSTEM-FILE-ALIST)
				    (DONT-ASK-P 0)
				    (DONT-CARE-IF-UNCHANGED-P 0)
				    DONT-ASK-FOR-CONFIRMATION
				    PACKAGE-SPEC)
  (COMPILE-FILE-ALIST-MAP (FUNCTION FUNCALL)
			  ALIST
			  DONT-ASK-P
			  DONT-CARE-IF-UNCHANGED-P
			  DONT-ASK-FOR-CONFIRMATION
			  PACKAGE-SPEC))

;Like COMPILE-FILE-ALIST, but takes a functional arg which is called
; instead of compiling or loading anything.  Functional arg is called with
; args of function and args that would have been called.  
;  ie QC-FILE FN NIL NIL NIL PACKAGE-SPEC.
(DEFUN COMPILE-FILE-ALIST-MAP (FCTN &OPTIONAL (ALIST SYSTEM-FILE-ALIST)
				    (DONT-ASK-P 0)
				    (DONT-CARE-IF-UNCHANGED-P 0)
				    DONT-ASK-FOR-CONFIRMATION
				    PACKAGE-SPEC
			       &AUX COMPILE-LIST QFASL-DATE SOURCE-DATE
				    FILE-NAME FN1 TEM)
    (COND ((NUMBERP DONT-ASK-P)				;If not specified,
	   (SETQ DONT-ASK-P (NOT (Y-OR-N-P "Should I ask you about each file? ")))))
    (COND ((NUMBERP DONT-CARE-IF-UNCHANGED-P)
	   (SETQ DONT-CARE-IF-UNCHANGED-P
		 (Y-OR-N-P "Should I compile even if the file is unchanged? "))))
    (DO L ALIST (CDR L) (NULL L)
	;; Look at each source file's date and compare it with the QFASL file's date.
	;; If the source is newer, or if DONT-CARE-IF-UNCHANGED-P is T,
	;; the file should be compiled.  If DONT-ASK-P is NIL, we ask
	;; about each of those files.
	(COND ((CMANY-QFASL-P (SETQ FILE-NAME (CAAR L)))	;IF IT'S COMPILED
	       (SETQ TEM (STRING-REVERSE-SEARCH-CHAR 40 FILE-NAME))
	       (COND ((NULL TEM) (SETQ FN1 FILE-NAME))	;extract filename sans the "QFASL",
		     (T (SETQ FN1 (SUBSTRING FILE-NAME 0 TEM))))
	       (COND ((NOT DONT-CARE-IF-UNCHANGED-P)
		      (SETQ SOURCE-DATE
			    (SI:FILE-GET-CREATION-DATE (STRING-APPEND FN1 " >") T))
		      (SETQ QFASL-DATE
			    (SI:FILE-GET-CREATION-DATE (STRING-APPEND FN1 " QFASL") NIL))))
	       (AND (OR DONT-CARE-IF-UNCHANGED-P	;If either OK if already loaded,
			(NULL QFASL-DATE)		;or no QFASL file exists now,
			(ALPHALESSP QFASL-DATE SOURCE-DATE))
							;or QFASL is old,
		    (COND ((NOT DONT-ASK-P)		;then ask if necessary
			   (Y-OR-N-P (FORMAT NIL "Compile file ~A >? " FN1)))
			  (T T))
		    (PUSH FN1 COMPILE-LIST)))))		;and put this file on list to be compiled.
    ;; Now we know which files to compile.  Get confirmation and then load them.
    (COND ((NOT (NULL COMPILE-LIST))
	   (SETQ COMPILE-LIST (NREVERSE COMPILE-LIST))
	   (FORMAT QUERY-IO "~2%Files to be compiled:~%")
	   (DO L COMPILE-LIST (CDR L) (NULL L)
	       (FORMAT QUERY-IO "~A >~%" (CAR L)))
	   (COND ((OR DONT-ASK-FOR-CONFIRMATION
		      (Y-OR-N-P (IF (= (LENGTH COMPILE-LIST) 1) "Compile it?"
				    "Compile them?")))
		  (TERPRI)
		  (DO L COMPILE-LIST (CDR L) (NULL L)
		      (FORMAT STANDARD-OUTPUT "~%Compiling ~A >" (CAR L))
		      (FUNCALL FCTN 'QC-FILE (CAR L) NIL NIL NIL PACKAGE-SPEC))
		  T))))
    NIL)

(DEFUN CMANY-QFASL-P (FILE)
    (STRING-EQUAL FILE "QFASL" (- (ARRAY-ACTIVE-LENGTH FILE) 5)))

(COMMENT ;IN NQFILE
(DEFUN SI:FILE-GET-CREATION-DATE (FILENAME ERROR-P &AUX INFO FRONT BACK)
  (COND ((SETQ INFO (FILE-COMMAND "OPENRA " FILENAME))
	 (AND ERROR-P
	      (FERROR NIL "~A for ~A" INFO FILENAME)))
	(T (SETQ INFO (FILE-COMMAND "ININFO"))
	   (SETQ BACK (STRING-REVERSE-SEARCH-CHAR 40 INFO))  ;FIND SPACE BEFORE LENGTH
	   (SETQ FRONT (STRING-REVERSE-SEARCH-CHAR 40 INFO 
						   (STRING-REVERSE-SEARCH-CHAR 40 INFO BACK)))
	   (SETQ INFO (SUBSTRING INFO (1+ FRONT) BACK))		;COPY OUT DATE FIELD
	   (STRING-APPEND (SUBSTRING INFO 6 8)			;YEAR
			  "//"
			  (SUBSTRING INFO 0 5)			;MM/DD
			  (SUBSTRING INFO 8 17.)))))
)