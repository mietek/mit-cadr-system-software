;-*- MODE: LISP; PACKAGE: SI -*-
;;; Really part of cold load
;;; New loader and system generator.             DLW 10/23/77   -*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DECLARE
 (SPECIAL INNER-SYSTEM-FILE-ALIST COLD-LOADED-FILE-LIST COMPILER-COLD-LOADED-FILE-LIST
	  PACKAGES-INITIALLY-LOADED
	  PACKAGE PKG-SYSTEM-INTERNALS-PACKAGE))

;Load a file, the appropriate way, into the appropriate package.
;If the FN2 is specified, we use it;  otherwise, we try QFASL and then >.
;NONEXISTENT-OK-FLAG if non-NIL means no error if file doesn't exist.
(DEFUN LOAD (FILENAME &OPTIONAL PKG NONEXISTENT-OK-FLAG &AUX TYPE)
    (COND ((FBOUNDP 'NSUBSTRING)
	   (LET ((FN (FS:FILE-PARSE-NAME FILENAME NIL T ':NO-DEFAULT)))
	     (COND ((NOT (NULL-S (FUNCALL FN ':TYPE)))
		    (SETQ TYPE (FILE-EXISTS-P FILENAME)))
		   ((SETQ TYPE (FILE-EXISTS-P		;FN2 not specified, try QFASL then >
				(SETQ FILENAME (FUNCALL FN ':COPY-WITH-TYPE ':QFASL)))))
		   ((SETQ TYPE (FILE-EXISTS-P
				(SETQ FILENAME (FUNCALL FN ':COPY-WITH-TYPE ':LISP))))))))
	  (T (SETQ TYPE (FILE-EXISTS-P FILENAME))))
    (SELECTQ TYPE
	 (NIL
	  ;; The file did not exist.  Cause an error.
	  (OR NONEXISTENT-OK-FLAG (FILE-QFASL-P FILENAME)))
	 (:QFASL
	  ;; The file was in QFASL format.  FASLOAD it.
	  (FASLOAD FILENAME PKG))
	 (OTHERWISE
	  ;; The file was not in QFASL format.  READFILE it.
	  (READFILE FILENAME PKG))))

;Put property on FILE-SYMBOL as if loaded.  Currently uses this to set up data for
; INNER-SYSTEM files which doesn't get set initially due to lack of STRING, etc.
; Also, this function may be useful for other purposes.
;Despite its name, this function does not take a file-symbol as an argument,
; which may be a crock.
;DEFAULT-FN2 is optional, if not supplied the file name already contains the correct FN2.
(DEFUN FILE-SYMBOL-SET-CURRENT (FILE-NAME &OPTIONAL DEFAULT-TYPE
				&AUX STREAM FILE-SYMBOL FILE-ID FILE-GROUP-SYMBOL)
  (SETQ FILE-NAME (FS:FILE-PARSE-NAME FILE-NAME NIL T DEFAULT-TYPE))
  (SETQ STREAM (OPEN FILE-NAME '(READ FIXNUM)))	;UNFORTUNATELY, FIXNUM MODE AFFECTS
						;THE ININFO WHICH IS A SCREW.
  (MULTIPLE-VALUE (FILE-SYMBOL FILE-GROUP-SYMBOL)
    (FS:GET-FILE-SYMBOLS FILE-NAME))
  (SETQ FILE-ID (FS:FILE-ININFO STREAM))
  (CLOSE STREAM)
  (SET-FILE-LOADED-ID FILE-SYMBOL FILE-ID
		      (SI:PKG-FIND-PACKAGE (GET FILE-GROUP-SYMBOL ':PACKAGE)))
  FILE-NAME)

(DEFUN Y-OR-N-P (&OPTIONAL MESSAGE (STREAM QUERY-IO))
    (COND ((AND MESSAGE (NOT (STRINGP MESSAGE)))
           ;; Temporary compatibility feature.
           (COND ((STRINGP STREAM)
                  (PSETQ MESSAGE STREAM STREAM MESSAGE))
                 (T (SETQ STREAM MESSAGE MESSAGE NIL)))))
    (PROG (CH)
       (AND MESSAGE (FORMAT STREAM "~&~A" MESSAGE))
     RETRY
       (SETQ CH (FUNCALL STREAM ':TYI))
       (COND ((MEMQ CH '(#/Y #/y #/T #/t #\SP))
	      (PRINC "Yes." STREAM)
	      (RETURN T))
	     ((MEMQ CH '(#\RUBOUT #/N #/n))
	      (PRINC "No." STREAM)
	      (RETURN NIL))
	     (T (AND MESSAGE (FORMAT STREAM "~&~A" MESSAGE))
		(PRINC "(Y or N)? " STREAM)
		(GO RETRY)))))

;;; LOAD-FILE-ALIST takes one required argument, an a-list of filenames,
;;; and forms which when evaled return T if the file is already in the machine.
;;; If either of the first two optional args,
;;; DONT-ASK-INDIVIDUALLY and DONT-CARE-IF-LOADED-P,
;;; is not given, the user will be queried.
;;; The third optional argument suppresses the listing of the file names
;;; and the single request for comfirmation just before loading.

(DEFUN LOAD-FILE-ALIST (ALIST &OPTIONAL (DONT-ASK-INDIVIDUALLY 0)
				        (DONT-CARE-IF-LOADED-P 0)
				        DONT-ASK-FOR-CONFIRMATION
                                        PKG)
  (LOAD-FILE-ALIST-MAP #'FUNCALL
		       ALIST
		       DONT-ASK-INDIVIDUALLY
		       DONT-CARE-IF-LOADED-P
		       DONT-ASK-FOR-CONFIRMATION
		       PKG))

;Like LOAD-FILE-ALIST, but takes functional arg which is called with args
 ; of function and args which would have been called to do the work.
(DEFUN LOAD-FILE-ALIST-MAP (FCTN ALIST &OPTIONAL (DONT-ASK-INDIVIDUALLY 0)
				  		 (DONT-CARE-IF-LOADED-P 0)
				  	         DONT-ASK-FOR-CONFIRMATION
						 PKG
			&AUX LOAD-LIST OLD-FILE-ID NEW-FILE-ID FILE-SYMBOL STREAM)
    (COND ((NUMBERP DONT-ASK-INDIVIDUALLY)				;If not specified,
	   (SETQ DONT-ASK-INDIVIDUALLY
		 (NOT (Y-OR-N-P "Should I ask you about each file? ")))))
    (COND ((NUMBERP DONT-CARE-IF-LOADED-P)
	   (SETQ DONT-CARE-IF-LOADED-P
		 (Y-OR-N-P "Should I load even if the file is loaded? "))))
    (DO L ALIST (CDR L) (NULL L)
      ;;; If STRING is loaded, try to see whether loaded version of file is most recent one.
      (COND ((FBOUNDP 'NSUBSTRING)
	     (SETQ FILE-SYMBOL (GET-FILE-SYMBOLS (CAAR L)))
	     (COND ((SETQ OLD-FILE-ID (GET-FILE-LOADED-ID FILE-SYMBOL PKG))
		    (SETQ STREAM (OPEN (CAAR L) '(READ :PROBE :ERROR)))
		    (SETQ NEW-FILE-ID (FUNCALL STREAM ':INFO)))))
	    ((SETQ OLD-FILE-ID NIL)))
      (AND (OR DONT-CARE-IF-LOADED-P			;If either OK if already loaded,
	       (AND (NULL OLD-FILE-ID)			;or no version yet loaded,
		    (NOT (AND (CDAR L) (EVAL (CDAR L)))))
	       (NOT (STRING-EQUAL OLD-FILE-ID NEW-FILE-ID)));or non-current version loaded,
           (COND ((NOT DONT-ASK-INDIVIDUALLY)		    ;then ask if necessary
		  (Y-OR-N-P (FORMAT NIL "Load file ~A? " (CAAR L))))
		 (T T))
	   (PUSH (CAAR L) LOAD-LIST)))			;and put this file on list to be loaded.
    ;; Now we know which files to load.  Get confirmation and then load them.
    (COND ((NOT (NULL LOAD-LIST))
	   (SETQ LOAD-LIST (NREVERSE LOAD-LIST))
	   (COND (DONT-ASK-FOR-CONFIRMATION
                  (COND ((NULL PKG)
                         (DOLIST (F LOAD-LIST)
			   (FUNCALL FCTN 'LOAD F)))
                        (T (DOLIST (F LOAD-LIST)
			     (FUNCALL FCTN 'LOAD F PKG)))))
		 (T
		  (FORMAT QUERY-IO "~2%Files to be loaded:~%")
		  (DO L LOAD-LIST (CDR L) (NULL L)
		      (PRINC (CAR L) QUERY-IO) (TERPRI QUERY-IO))
		  (COND ((Y-OR-N-P (COND ((= (LENGTH LOAD-LIST) 1) "Load it up?")
					 ((= (LENGTH LOAD-LIST) 2) "Load them both up?")
					 (T "Load them all up?")))
			 (TERPRI)
			 (PRINC "Loading.")
			 (TERPRI)
			 (COND ((NULL PKG)
				(DOLIST (F LOAD-LIST)
				  (FUNCALL FCTN 'LOAD F)))
			       (T (DOLIST (F LOAD-LIST)
				    (FUNCALL FCTN 'LOAD F PKG))))))))))

    NIL)

(DECLARE (SPECIAL QLD-GROUND-DONE QLD-MINI-DONE))

;;; Procedure for booting up a world load:
;;; 1. Use MINI to load QFCTNS, STRING, PACK4.  Create packages.
;;; 2. Use MINI to load kernel system.  Create processes, error handler, etc.
;;; 3. Use MINI to load the FORMAT package.
;;; 4. Use MINI to load the CHAOS package.  Turn on the chaos network.
;;; 5. Use MINI to load QFILE.
;;; 6. Do like a cold boot.  This turns on the real file system.
;;; 7. Use PKG-LOAD to load the rest of the world.
;;; 8. Copy the PLISTs and SELECT-METHOD-LISTS of the world so as to reduce the
;;;    number of pages they touch.
(DEFUN QLD (&OPTIONAL (FROM-THE-GROUND-UP-P NIL) (LOAD-KEYWORDS ':NOCONFIRM)
	    &AUX FILE-SYMBOL)
    (OR (BOUNDP 'QLD-GROUND-DONE)
	(SETQ QLD-GROUND-DONE NIL))
    (OR (BOUNDP 'QLD-MINI-DONE)
	(SETQ QLD-MINI-DONE NIL))
    (COND ((OR FROM-THE-GROUND-UP-P		;If we are starting from a cold-load
	       (NOT QLD-GROUND-DONE))
	   (TERPRI)
	   (PRINC "Loading QFCTNS and STRING")
	   ;; NOTE: be careful about the format of filenames in this section before
	   ;; all the filename canonicalization stuff is loaded.
	   (SI:MINI-FASLOAD "AI: LISPM; QFCTNS QFASL" "")
	   (SI:MINI-FASLOAD "AI: LISPM2; STRING QFASL" "")
	   (TERPRI)
	   (PRINC "Loading and installing PACK4")
	   (SI:MINI-FASLOAD "AI: LISPM; PACK4 QFASL" "")
	   (SI:PKG-INSTALL)
	   (SI:MINI-READFILE "AI: LISPM; PKGDCL >" "")
	   (SETQ QLD-GROUND-DONE T)))
    (TERPRI)
    (COND ((NULL QLD-MINI-DONE)
	   (PRINC "Loading inner system")
	   (DOLIST (F INNER-SYSTEM-FILE-ALIST)
	      (SI:MINI-LOAD (CAR F) (CADR F)))
	   (SI:LISP-REINITIALIZE)	;Turn on network, load error table, etc.
	   (LOGIN "LISPM" T)		;So that we can do file I/O
    ;If certain files which are obviously always loaded do not have FILE-ID
    ;properties, put them on, assuming the versions loaded in are the same
    ;as the current versions in the file system.  This happens as we are
    ;bootstrapping our way up from a cold-load.
	   (MAPC #'(LAMBDA (PACKAGE FILE-LIST)
		     (MAPC #'(LAMBDA (FILE-NAME)
			       (SETQ FILE-SYMBOL (GET-FILE-SYMBOLS FILE-NAME))
			       (OR (GET FILE-SYMBOL ':FILE-ID-PACKAGE-ALIST)
				   (FILE-SYMBOL-SET-CURRENT FILE-NAME)))
			   FILE-LIST))
		 (LIST PKG-SYSTEM-INTERNALS-PACKAGE (PKG-FIND-PACKAGE "COMPILER"))
		 (LIST COLD-LOADED-FILE-LIST COMPILER-COLD-LOADED-FILE-LIST))
	   (SETQ QLD-MINI-DONE T))
	  (T (LOGIN "LISPM" T)))	;So that we can do file I/O
    (PRINC "Loading rest of world")
    (DOLIST (PACK PACKAGES-INITIALLY-LOADED)
      (PKG-LOAD PACK LOAD-KEYWORDS))
    (FORMAT T "~%Loading CC symbols for UCADR ~DMCR" %MICROCODE-VERSION-NUMBER)
    (PKG-BIND "CADR"
      (CADR:CC-LOAD-UCODE-SYMBOLS
       (FORMAT NIL "AI: LISPM1; UCADR ~DSYM" %MICROCODE-VERSION-NUMBER)))
;    (FORMAT T "~%Compactifying")
;    (LET ((DEFAULT-CONS-AREA PROPERTY-LIST-AREA))
;      (MAPATOMS-ALL #'(LAMBDA (SYM) 
;			(COND ((CDDR (PLIST SYM))
;			       (SETPLIST SYM (COPYLIST (PLIST SYM))))))))
;    (LET ((DEFAULT-CONS-AREA PERMANENT-STORAGE-AREA))
;      (MAP-CLASS-HIERARCHY
;	#'(LAMBDA (CLASS)
;	    (LET ((CSM (<- CLASS ':CLASS-METHOD-SYMBOL)))
;	      (SET-METHOD-LIST CSM (COPYALIST (METHOD-LIST CSM)))) )))
    (PRINT-DISK-LABEL)
    (FORMAT T "~%OK, now do a DISK-SAVE~%"))

;Useful keywords are:
; :LOAD (after compiling, load in anything not in)
; :NOCONFIRM (don't ask for confirmation for each package of files)
; :SELECTIVE (ask for confirmation for every file)
(DEFUN RECOMPILE-WORLD (&REST KEYWORDS)
  (LOAD-FILE-LIST '(("AI: LISPM; PKGDCL >")) KEYWORDS)
  (LEXPR-FUNCALL #'RECOMPILE-WORLD-MAP #'FUNCALL KEYWORDS))

(DEFUN RECOMPILE-WORLD-MAP (FCTN &REST KEYWORDS)
  (SETQ KEYWORDS (CONS ':COMPILE (IF (MEMQ ':LOAD KEYWORDS) (REMQ ':LOAD KEYWORDS)
				     (CONS ':NOLOAD KEYWORDS))))
  (MAPC #'(LAMBDA (PKG) (SI:PKG-LOAD-MAP FCTN PKG KEYWORDS))
	'(SYSTEM-INTERNALS FORMAT COMPILER TV CHAOS SUPDUP EH ZWEI COLOR
	  FILE-SYSTEM PRESS FED CADR QFASL-REL MICRO-ASSEMBLER TIME))
  T)

(DEFUN LIST-OBSOLETE-FILES (&OPTIONAL (TOP-PKG PKG-GLOBAL-PACKAGE))
  (SETQ TOP-PKG (PKG-FIND-PACKAGE TOP-PKG))
  (DOLIST (FILE-ALIST-ELEMENT (PKG-FILE-ALIST TOP-PKG))
    (CHECK-FILE-CURRENCY FILE-ALIST-ELEMENT TOP-PKG))
  (MAPC #'LIST-OBSOLETE-FILES (PKG-SUBPACKAGES TOP-PKG)))

(DEFUN CHECK-FILE-CURRENCY (FILE-ALIST-ELEMENT PKG)
  (CHECK-FILE-SOURCE-CURRENCY FILE-ALIST-ELEMENT)
  (CHECK-FILE-QFASL-CURRENCY FILE-ALIST-ELEMENT PKG))

(DEFUN CHECK-FILE-SOURCE-CURRENCY (FILE-ALIST-ELEMENT
				    &AUX FILE-NAME SOURCE-DATE QFASL-DATE FN1 TEM)
  (COND ((COMPILER:CMANY-QFASL-P (SETQ FILE-NAME (CAR FILE-ALIST-ELEMENT)))
	 (SETQ TEM (STRING-REVERSE-SEARCH-CHAR 40 FILE-NAME))
	 (COND ((NULL TEM) (SETQ FN1 FILE-NAME))	;extract filename sans the "QFASL",
	       (T (SETQ FN1 (SUBSTRING FILE-NAME 0 TEM))))
	 (SETQ SOURCE-DATE (FS:FILE-GET-CREATION-DATE (STRING-APPEND FN1 " >") T))
	 (SETQ QFASL-DATE (FS:FILE-GET-CREATION-DATE (STRING-APPEND FN1 " QFASL") NIL))
	 (COND ((NULL QFASL-DATE)
		(FORMAT T "~%~A -- No QFASL exists" FILE-NAME))
	       ((STRING-LESSP QFASL-DATE SOURCE-DATE)
		(FORMAT T "~%~A -- Apparently needs to be recompiled" FILE-NAME))))))

(DEFUN CHECK-FILE-QFASL-CURRENCY (FILE-ALIST-ELEMENT PKG
				   &AUX FILE-NAME FILE-SYMBOL OLD-FILE-ID STREAM NEW-FILE-ID)
  (SETQ FILE-NAME (CAR FILE-ALIST-ELEMENT))
  (SETQ FILE-SYMBOL (GET-FILE-SYMBOLS FILE-NAME))
  (COND ((SETQ OLD-FILE-ID (GET-FILE-LOADED-ID FILE-SYMBOL PKG))
	 (SETQ STREAM (OPEN FILE-NAME '(READ :PROBE :ERROR)))
	 (SETQ NEW-FILE-ID (FUNCALL STREAM ':INFO))
	 (COND ((NOT (STRING-EQUAL OLD-FILE-ID NEW-FILE-ID))
		(FORMAT T "~%~A -- Newer QFASL file than currently loaded" FILE-NAME))))
	(T (FORMAT T "~%~A -- No QFASL file currently loaded" FILE-NAME))))
  

;;; [This replaces the FILE-LOADED-ID property.]

;;; These routines don't work before packages exist.

;;; The :FILE-ID-PACKAGE-ALIST property of a file-symbol is an a-list
;;; of packages and FILE-ID's for the version of that file loaded into
;;; that package.  The FILE-ID is in the CADR rather the CDR, for expansibility.

;Record the fact that a file has been loaded (in a certain package)
(DEFUN SET-FILE-LOADED-ID (FILE-SYMBOL FILE-ID PKG &AUX TEM PROP)
  (COND ((SETQ TEM (ASSQ PKG (SETQ PROP (GET FILE-SYMBOL ':FILE-ID-PACKAGE-ALIST))))
	 (RPLACA (CDR TEM) FILE-ID))
	(T
	 (PUTPROP FILE-SYMBOL
		  (CONS (LIST PKG FILE-ID) PROP)
		  ':FILE-ID-PACKAGE-ALIST))))

;Get the version of a file that was loaded into a particular package, NIL if never loaded.
;If the package is given as NIL, the file's :PACKAGE property is used.  If it doesn't
;have one, we don't have to go look at the file, since clearly it has never been loaded.
(DEFUN GET-FILE-LOADED-ID (FILE-SYMBOL PKG)
  (AND (NULL PKG)
       (MULTIPLE-VALUE-BIND (NIL FILE-GROUP-SYMBOL)
	   (SI:GET-FILE-SYMBOLS (STRING FILE-SYMBOL))
	 (SETQ PKG (GET FILE-GROUP-SYMBOL ':PACKAGE))))
  (AND PKG
       (CADR (ASSQ (PKG-FIND-PACKAGE PKG) (GET FILE-SYMBOL ':FILE-ID-PACKAGE-ALIST)))))

;These files are loaded into the SYSTEM-INTERNALS package before the
;world is loaded up.
;First thing in each entry is filename, second is package name
(SETQ INNER-SYSTEM-FILE-ALIST
      '(("AI: LISPM; QMISC QFASL" "SI")
	("AI: LISPM; SORT QFASL" "SI")		;Needed by FLAVOR
	("AI: LMIO; FORMAT QFASL" "FORMAT")	;ditto
	("AI: LISPM2; FLAVOR QFASL" "SI")	;Needed by PROCES
	("AI: LMWIN; PRODEF QFASL" "SI")	;Definitions for PROCES
	("AI: LMWIN; PROCES QFASL" "SI")
	("AI: LMWIN; EH QFASL" "EH")
	("AI: LMWIN; EHR QFASL" "EH")
	("AI: LMWIN; EHC QFASL" "EH")
	("AI: LISPM2; GC QFASL" "SI")	;Called by DISK
	("AI: LMIO; DISK QFASL" "SI")
	("AI: LISPM2; DEFSEL QFASL" "SI")	;Needed to load QFILE
	("AI: LISPM2; LOGIN QFASL" "SI")	;ditto
	("AI: LMIO; RDDEFS QFASL" "SI")	;Load this before trying to read any #\'s
	("AI: LMIO; CHSNCP QFASL" "CHAOS")
	("AI: LMIO; CHSTBL >" "CHAOS")
	("AI: LMIO; CHSAUX QFASL" "CHAOS")
	("AI: LMIO; QFILE QFASL" "FS")))

;Lists of names of files which are already present "before the Creation".
;These got here either by being in the cold load or being fasloaded in before
;the full file mechanism was set up.
;These are in the SYSTEM-INTERNALS package
(SETQ COLD-LOADED-FILE-LIST
      '("AI: LISPM; QFCTNS QFASL" "AI: LISPM2; STRING QFASL" "AI: LISPM; PACK4 QFASL"
	;Files below this line came over in the cold load
	"AI: LMWIN; COLD QFASL"
	"AI: LISPM; LFL QFASL"
	"AI: LISPM; LTOP QFASL"
	"AI: LMIO; PRINT QFASL"
	"AI: LISPM; QEV QFASL"
	"AI: LISPM; QFASL QFASL"
	"AI: LMIO; MINI QFASL"
	"AI: LMIO; QIO QFASL"
	"AI: LISPM; QRAND QFASL"
	"AI: LMIO; RDTBL QFASL"
	"AI: LMIO; READ QFASL"
	"AI: LISPM; SGFCTN QFASL"
	))

;These are in the COMPILER package.
(SETQ COMPILER-COLD-LOADED-FILE-LIST
      '("AI: LISPM; LFL QFASL"))

(SETQ PACKAGES-INITIALLY-LOADED '(
    FONTS		;Before SYSTEM-INTERNALS because things use fonts
    SYSTEM-INTERNALS
    FORMAT		;a NO-OP if loading for first time
    CHAOS		;likewise
    COMPILER
    FILE-SYSTEM
    QFASL-REL
    TIME		;must be before TV
    TV
    SUPDUP
    ZWEI
    FED
    COLOR
    EH
    CADR
    PRESS
    MICRO-ASSEMBLER))
