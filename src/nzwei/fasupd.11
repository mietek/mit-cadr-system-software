;; -*-MODE:LISP; PACKAGE:ZWEI-*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCOM COM-FASL-UPDATE
             "Update the fasl file of the file you are visiting.
Uses the function definitions present in the environment,
compiling them if they are not already compiled.   Note that
you must have already compiled any functions you changed since
the fasl file you loaded was compiled.  Also note that
DECLAREs and EVAL-WHEN (COMPILE)s will be ignored!" ()
    (LET ((BUFFER (READ-BUFFER-NAME "Update fasl file of buffer:"
		       *INTERVAL*               ;Default is current buffer.
                       NIL)))
      (OR (BUFFER-FILE-ID BUFFER)
          (BARF "This buffer is not associated with a file"))
      (FASL-UPDATE BUFFER))
    DIS-NONE)

;; Write out the compilations of the functions whose sources are in BUFFER.
;; We assume that the user has compiled all the functions he has changed.
;; The QFASL file name is formed from the name of the buffer.
;; We don't actually do any compilation or evaluation of the buffer,
;; though we do expand the macros.

;; Normally, we read each form from the buffer and process it.
;; For forms starting with DEFUN and DEFMETHOD, we read only the
;; function name, which is enough to use to dump the function,
;; and then we skip the rest of the form and cons up a dummy DEFUN or DEFMETHOD
;; with no body or arglist to use in doing the dumping.

(DEFUN FASL-UPDATE (BUFFER &OPTIONAL OUTFILE
			   &AUX COMPILER:QC-FILE-LOAD-FLAG (COMPILER:QC-FILE-IN-CORE-FLAG T)
				INFILE INPUT-STREAM COMPILER:FASD-STREAM
				COMPILER:LAST-ERROR-FUNCTION
				DEFTYPE FNNAME
				(DEFAULT-CONS-AREA DEFAULT-CONS-AREA)
				(COMPILER:QC-FILE-OLD-DEFAULT-CONS-AREA DEFAULT-CONS-AREA))
  (SETQ INFILE (BUFFER-FILE-NAME BUFFER))
  (SETQ OUTFILE
	(IF OUTFILE
	    (SI:FILE-PARSE-NAME OUTFILE NIL (FUNCALL INFILE ':COPY-WITH-TYPE ':QFASL))
	    (FUNCALL INFILE ':COPY-WITH-TYPE ':QFASL)))
  (SETQ INPUT-STREAM (INTERVAL-STREAM BUFFER))
  (UNWIND-PROTECT
    (LET ((COMPILER:QC-FILE-IN-PROGRESS T)
	  (LOCAL-DECLARATIONS NIL)
	  (COMPILER:FILE-LOCAL-DECLARATIONS NIL))
      (COMPILER:FASD-OPEN OUTFILE)
      (COMPILER:FASD-INITIALIZE)
      ;; First thing in QFASL file must be property list
      ;; Only property supported just now is PACKAGE property
      (COMPILER:FASD-FILE-PROPERTY-LIST
	(LIST ':PACKAGE (INTERN (PKG-NAME PACKAGE) SI:PKG-USER-PACKAGE)))
      (COMPILER:QC-PROCESS-INITIALIZE)
      (DO ((EOF (NCONS NIL))
	   (BP)
	   (FORM))
	  (NIL)
	;; Start a new whack if FASD-TABLE is getting too big.
	(AND ( (COMPILER:FASD-TABLE-LENGTH) COMPILER:QC-FILE-WHACK-THRESHOLD)
	     (COMPILER:FASD-END-WHACK))
	;; Find next interesting object in buffer.
	(SETQ BP (SKIP-OVER-BLANK-LINES-AND-COMMENTS
		   (FUNCALL INPUT-STREAM ':READ-BP)))
	(OR BP (RETURN NIL))
	;; Read and macroexpand in temp area.
	(SETQ DEFAULT-CONS-AREA COMPILER:QC-FILE-TEMPORARY-AREA)
	;; This is intended to look at the form that follows,
	;; decide whether it is a defun, and if so
	;; just create a dummy, since we will not look at the body anyway.
	(MULTIPLE-VALUE (DEFTYPE FNNAME)
	  (FASL-UPDATE-CHECK-DEFUN BP))
	(COND ((AND DEFTYPE
		    (FDEFINEDP (IF (EQ DEFTYPE 'DEFMETHOD)
				   (CONS ':METHOD FNNAME)
				   FNNAME)))
	       (FUNCALL INPUT-STREAM ':SET-BP
			;; The memo-izing lisp parser can cons permanent information
			(LET ((DEFAULT-CONS-AREA COMPILER:QC-FILE-OLD-DEFAULT-CONS-AREA))
			  (FORWARD-SEXP BP)))
	       (SETQ FORM `(,DEFTYPE ,FNNAME NIL NIL)))
	      (T
	       (FUNCALL INPUT-STREAM ':SET-BP BP)
	       (LET ((COMPILER:QC-FILE-READ-IN-PROGRESS T))
		 (SETQ FORM (READ INPUT-STREAM EOF)))))
	(AND (EQ EOF FORM)
	     (RETURN NIL))
	(SETQ FORM (MACROEXPAND FORM T))
	(SETQ DEFAULT-CONS-AREA COMPILER:QC-FILE-OLD-DEFAULT-CONS-AREA)
	;; Output this form in the appropriate way.
	(COMPILER:COMPILE-DRIVER FORM (FUNCTION FASL-UPDATE-FORM) NIL))
      (COMPILER:FASD-END-WHACK)
      (COMPILER:FASD-END-FILE)
      (COMPILER:FASD-CLOSE OUTFILE))
    (COMPILER:QC-FILE-RESET)))

;; This is the list of types of form that we don't even need to read.
(DECLARE (SPECIAL FASL-UPDATE-DEFTYPES-ALIST))
(SETQ FASL-UPDATE-DEFTYPES-ALIST
      '(("DEFUN" DEFUN) ("DEFMETHOD" DEFMETHOD)))

(DEFUN FASL-UPDATE-CHECK-DEFUN (BP &AUX BP1 DEFTYPE FNNAME)
  ;; Now get the second word after BP.
  (AND (= (BP-CH-CHAR BP) #/()
       (SETQ BP (FORWARD-CHAR BP))
       (SETQ BP1 (FORWARD-ATOM BP))
       (SETQ DEFTYPE (CADR (ASSOC (STRING-INTERVAL BP BP1)
				  FASL-UPDATE-DEFTYPES-ALIST)))
       (SETQ BP (FORWARD-OVER *BLANKS* BP1))
       (SETQ BP1 (FORWARD-SEXP BP))
       (SETQ FNNAME (STRING-INTERVAL BP BP1))
       (MVRETURN DEFTYPE (READ-FROM-STRING FNNAME))))

;; Process one form, for COMPILE-DRIVER.
(DEFUN FASL-UPDATE-FORM (FORM TYPE)
    (SELECTQ TYPE
      (SPECIAL (COMPILER:FASD-FORM FORM NIL))
      (DECLARE)		;Ignore DECLAREs -- this may not always be right!
      ((BEGF COMMENT))
      ((DEFUN MACRO)	;Don't compile -- send over whatever is already compiled
        (OR (FDEFINEDP (CADR FORM))
	    (FERROR NIL "You forgot to compile ~S" (CADR FORM)))
        (PROG (TEM TEM1)
         LOOP
           (SETQ TEM (FDEFINITION (CADR FORM)))
           (AND (LISTP TEM) (EQ (CAR TEM) 'MACRO) (SETQ TEM (CDR TEM)))
	   (COND ((AND (LISTP TEM)
                       (MEMQ (CAR TEM) '(LAMBDA NAMED-LAMBDA)))
                  (COND ((AND (EQ (CAR TEM) 'NAMED-LAMBDA)
			      (LISTP (CADR TEM))
                              (SETQ TEM1 (ASSQ 'TRACE (CDADR TEM))))
                         (FORMAT ERROR-OUTPUT "~&Untracing ~S~%" (CADR FORM))
                         (FDEFINE (CADR FORM) (FDEFINITION (CADR TEM1)))
                         (GO LOOP)))
                  (FORMAT ERROR-OUTPUT "~&Compiling ~S~%" (CADR FORM))
                  (COMPILE (CADR FORM))))
           ;; This works on this bodiless DEFUN by virtue of the fact that FASD-FORM in
	   ;; Optimize mode calls FDEFINITION rather than looking at the form.
	   (COMPILER:FASD-FORM FORM T)))
      (OTHERWISE (COMPILER:FASD-FORM FORM T))))
