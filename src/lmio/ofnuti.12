;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains filename-parsing utilities which need to be in the cold load.

;;; Things for processing filenames.
;;; Nobody should know about the syntax of pathnames outside of this page.
;;; Since the format of path lists will change,
;;; nobody should know about them either outside of this page.
;;; The entry points are FILE-EXPAND-PATHNAME, FILE-DEFAULT-FN2,
;;; FILE-SET-FN2.

(DEFVAR FILE-LAST-DEVICE "DSK")
(DEFVAR FILE-LAST-DIRECTORY "LISPM")
(DEFVAR FILE-LAST-FN1 "FOO")
(DEFVAR FILE-DSK-DEVICE-NAME "AI")

;; Given a file name, return two symbols, first for the specific file
;; and second for the group of files with that FN1, (FN2 will be ">")
;; Must work both before and after packages exist.
;; Must work if STRING not loaded, we use a kludge.
(DEFUN GET-FILE-SYMBOLS (FILE-NAME &AUX FILE-SYMBOL FILE-GROUP-SYMBOL)
  (COND ((FBOUNDP 'INTERN-LOCAL)
	 (SETQ FILE-SYMBOL (INTERN-LOCAL (FILE-EXPAND-PATHNAME FILE-NAME) PKG-FILE-PACKAGE))
	 (SETQ FILE-GROUP-SYMBOL
	       (INTERN-LOCAL (FILE-SET-FN2 (GET-PNAME FILE-SYMBOL) ">") PKG-FILE-PACKAGE)))
	(T
	 (SETQ FILE-SYMBOL (INTERN (FILE-EXPAND-PATHNAME FILE-NAME)))
	 (SETQ FILE-GROUP-SYMBOL
	       (INTERN (COND ((FBOUNDP 'NSUBSTRING)
			      (FILE-SET-FN2 (GET-PNAME FILE-SYMBOL) ">"))
			     ((STRING-EQUAL FILE-NAME "LISPM;QFCTNS QFASL")
			      "AI: LISPM; QFCTNS >")
			     ((STRING-EQUAL FILE-NAME "LISPM2;STRING QFASL")
			      "AI: LISPM2; STRING >")
			     (T (FERROR NIL "File not known" FILE-NAME)))))
	 (RPLACA (PACKAGE-CELL-LOCATION FILE-SYMBOL) 'FILES)
	 (RPLACA (PACKAGE-CELL-LOCATION FILE-GROUP-SYMBOL) 'FILES)))
  (PROG () (RETURN FILE-SYMBOL FILE-GROUP-SYMBOL)))

;Convert a pathname string into a path list: (dev dir fn1 fn2).
;The elements of a path list are strings, or NIL for an
;unspecified position.
;Slash and control-Q () are quoting characters.  Colon, semicolon, space
;and tab separate filename components.
(DEFUN FILE-SPREAD-PATHNAME (PATHNAME &AUX DEV DEV-A DIR FN1 FN2)
  (COND ((SYMBOLP PATHNAME)
	 (SETQ PATHNAME (GET-PNAME PATHNAME))))
  (PROG ()
    (COND ((STRINGP PATHNAME)
	   (DO ((I 0) (CH) (TEM) (NEXT) (LEN (STRING-LENGTH PATHNAME)) (J 0 (1+ J)))
	       ((> J LEN))
	     (SETQ CH (COND ((= J LEN) #\SP)
			    (T (AR-1 PATHNAME J))))
	     (COND ((STRING-SEARCH-CHAR CH "//")
		    (SETQ J (1+ J)))
		   ;; Last two characters of the string are space and tab.
		   ((SETQ TEM (STRING-SEARCH-CHAR CH ":; 	"))
		    (SETQ NEXT (STRING-UPCASE (SUBSTRING PATHNAME I J)))
		    (COND ((NOT (ZEROP (STRING-LENGTH NEXT)))
			   (SELECTQ TEM
			     (0 (AND DEV (SETQ DEV-A DEV))
				(SETQ DEV NEXT))
			     (1 (SETQ DIR NEXT))
			     ((2 3) (COND (FN2)
					  (FN1 (SETQ FN2 NEXT))
					  (T (SETQ FN1 NEXT)))))))
		    (SETQ I (1+ J)))))
	   (RETURN (LIST DEV DIR FN1 FN2) (OR DEV-A DEV)))
	  ((LISTP PATHNAME)			;MACLISP FILE-LISTS
	   (RETURN
	     (MAPCAR #'(LAMBDA (X) (AND X (STRING X))) ;LEAVE NILS FOR UNSPECIFIED COMPONENTS
		     (COND ((LISTP (CAR PATHNAME))
			    (COND ((CDAR PATHNAME)
				   (LIST (CAAR PATHNAME) (CADAR PATHNAME)  ;BOTH DEV AND DIR
					 (CADR PATHNAME) (CADDR PATHNAME)))
				  (T (LIST NIL (CAAR PATHNAME)	;JUST DIR
					   (CADR PATHNAME) (CADDR PATHNAME)))))
			   (T (LIST (CADDR PATHNAME) (CADDDR PATHNAME)     ;N1 N2 DEV DIR
				    (CAR PATHNAME) (CADR PATHNAME)))))
	     NIL))
	  (T (FERROR NIL "~S is not an acceptable pathname" PATHNAME)))))

;Replace NILs in a path with the defaults.  Also update the
;defaults for the specified parts of the path.
(DEFUN FILE-DEFAULT-PATH (PATH)
    (APPLY (FUNCTION (LAMBDA (DEV DIR FN1 FN2)
	       (AND DEV (SETQ FILE-LAST-DEVICE DEV))
	       (AND DIR (SETQ FILE-LAST-DIRECTORY DIR))
	       (AND FN1 (SETQ FILE-LAST-FN1 FN1))
	       (OR FN2 (SETQ FN2 ">"))
	       (AND (EQUAL FILE-LAST-DEVICE "DSK")
		    (SETQ FILE-LAST-DEVICE FILE-DSK-DEVICE-NAME))
	       (LIST FILE-LAST-DEVICE FILE-LAST-DIRECTORY FILE-LAST-FN1 FN2)))
	   PATH))

;Turn a path list back into a pathname string.
(DEFUN FILE-UNSPREAD-PATH (PATH)
    (OR (THIRD PATH)
	(FERROR NIL "The path ~S contains no FN1" PATH))
    (STRING-APPEND (IF (FIRST PATH) (SIX-CHARACTERS (FIRST PATH)) "")
		   (IF (FIRST PATH) ": " "")
		   (IF (SECOND PATH) (SIX-CHARACTERS (SECOND PATH)) "")
		   (IF (SECOND PATH) "; " "")
		   (SIX-CHARACTERS (THIRD PATH))
		   " "
		   (IF (FOURTH PATH) (SIX-CHARACTERS (FOURTH PATH)) "")))

;Truncate to six characters, knowing about slash and control-Q as quoting characters
(DEFUN SIX-CHARACTERS (STR)
  (DO ((I 0 (1+ I))
       (NCH 0)
       (CH)
       (N (STRING-LENGTH STR)))
      (( I N) STR)
    (SETQ CH (AREF STR I))
    (AND (OR (= CH #// ) (= CH #/)) ;extra space is because editor parses this wrong
	 (SETQ I (1+ I)))	;Quotes next character
    (AND (= (SETQ NCH (1+ NCH)) 6)
	 (< (1+ I) N)
	 (RETURN (SUBSTRING STR 0 (1+ I))))))

;Given a pathname string, default it and return a new pathname string.
; Also, for MACLISP compatibility, will accept MACLISP type LIST file spec lists.
(DEFUN FILE-EXPAND-PATHNAME (PATHNAME)
  (PROG (SPREAD-PATH DEVICE-SPEC)
    (RETURN (COND ((FBOUNDP 'NSUBSTRING)
		   (FILE-UNSPREAD-PATH
		     (FILE-DEFAULT-PATH
		       (PROGN
			 (MULTIPLE-VALUE (SPREAD-PATH DEVICE-SPEC)
			   (FILE-SPREAD-PATHNAME PATHNAME))
			 SPREAD-PATH))))
		  (T PATHNAME))
	    DEVICE-SPEC)))

;Given two pathnames, default missing parts of first from second.
(DEFUN FILE-MERGE-PATHNAMES (PATHNAME1 PATHNAME2)
    (FILE-UNSPREAD-PATH (FILE-MERGE-PATHS (FILE-SPREAD-PATHNAME PATHNAME1)
					  (FILE-SPREAD-PATHNAME PATHNAME2))))

;Internal merge function.
(DEFUN FILE-MERGE-PATHS (PATH1 PATH2)
   (DO ((L1 PATH1 (CDR L1))
	(L2 PATH2 (CDR L2))
	(NPATH))
       ((NULL L2) (NREVERSE NPATH))
     (PUSH (OR (CAR L1) (CAR L2)) NPATH)))

;Old name for file-expand-pathname.
(DEFUN FILE-DEFAULT-FILENAMES (FILENAME)
    (FILE-EXPAND-PATHNAME FILENAME))

;Given a pathname string, return a new one like it with the FN2 defaulted
;to the default we specify, unless there was an FN2 in the original.     
(DEFUN FILE-DEFAULT-FN2 (PATHNAME DEFAULT-FN2 &AUX PATH)
    (COND ((FBOUNDP 'NSUBSTRING)
	   (SETQ PATH (FILE-SPREAD-PATHNAME PATHNAME))
	   (OR (FOURTH PATH) (SETF (FOURTH PATH) DEFAULT-FN2))
	   (FILE-UNSPREAD-PATH PATH))
	  (T PATHNAME)))

;Given a pathname string, return a new one like it but with the fn2
;replaced.
(DEFUN FILE-SET-FN2 (PATHNAME FN2)
   (FILE-UNSPREAD-PATH (LET ((PATH (FILE-SPREAD-PATHNAME PATHNAME)))
			 (SETF (FOURTH PATH) FN2)
			 PATH)))
