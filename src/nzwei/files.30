;;; -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This file contains utility functions for manipulating files, and various
;;; commands to do I/O to intervals.  It does not know about buffers and such,
;;; just intervals.

;;; (OPEN-FILE (<var> <filename> <options>) <form1> <form2> ...)
;;; Opens the file <filename>, using <options>, and lets <var> be the
;;; stream for the rest of the form.  An unwind-protect closes the file.
;;; OPEN-FILE is defined in the MACROS file.

;;; Copy from the stream into the interval until EOF.
;;; Leaves the stream at EOF but doesn't close it.
;;; Returns a BP to where the end of the inserted text is.
;;; HACK-FONTS means interpret 's in the file as font-change characters
(DEFUN STREAM-INTO-BP (STREAM BP &OPTIONAL HACK-FONTS)
  (LET ((INT-STREAM (INTERVAL-STREAM BP BP T HACK-FONTS)))
    (STREAM-COPY-UNTIL-EOF STREAM INT-STREAM (GET 'LINE 'SI:DEFSTRUCT-SIZE))
    (FUNCALL INT-STREAM ':READ-BP)))

;;; Copy from the interval into the stream.
;;; Leaves the stream open.
(DEFUN STREAM-OUT-INTERVAL (STREAM FROM-BP &OPTIONAL TO-BP IN-ORDER-P &OPTIONAL HACK-FONTS)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (STREAM-COPY-UNTIL-EOF
    (INTERVAL-STREAM FROM-BP TO-BP T HACK-FONTS)
    STREAM
    NIL))

;;; Get a filename from the user, return as a file-name actor.
(DEFUN READ-DEFAULTED-FILE-NAME (PROMPT DEFAULT &OPTIONAL SPECIAL-TYPE)
  (AND DEFAULT (SETQ DEFAULT (FS:FILE-PARSE-NAME DEFAULT)))
  (LET ((STRING (TEMP-KILL-RING *LAST-FILE-NAME-TYPED*
				(TYPEIN-LINE-READLINE "~A (~A)" PROMPT
						      (AND DEFAULT
							   (FUNCALL DEFAULT
								    ':STRING-FOR-PRINTING))
						      ))))
    (MAKE-DEFAULTED-FILE-NAME STRING DEFAULT SPECIAL-TYPE)))

(DEFUN READ-DEFAULTED-AUX-FILE-NAME (PROMPT &OPTIONAL SPECIAL-TYPE)
  (SETQ *DEFAULT-AUX-FILE-NAME*
	(READ-DEFAULTED-FILE-NAME PROMPT (OR *DEFAULT-AUX-FILE-NAME* (DEFAULT-FILE-NAME))
				  SPECIAL-TYPE)))

(DEFUN MAKE-DEFAULTED-FILE-NAME (STRING DEFAULT &OPTIONAL SPECIAL-TYPE)
  ;; STRING is what the user typed.  Remember it for next time if non-null.
  (IF (ZEROP (STRING-LENGTH STRING))
      ;; He didn't type anything, use the default.
      DEFAULT 
      (SETQ *LAST-FILE-NAME-TYPED* STRING)
      (FUNCALL DEFAULT ':DEFAULT-NAMESTRING STRING SPECIAL-TYPE)))

;;; Canonicalize filename for use as buffer name, etc.
(DEFUN EDITOR-FILE-NAME (FILE-NAME)
  (AND (STRINGP FILE-NAME)
       (SETQ FILE-NAME (SI:FILE-PARSE-NAME FILE-NAME)))
  (MVRETURN FILE-NAME (FUNCALL FILE-NAME ':STRING-FOR-EDITOR)))

;;; Various file-related commands on INTERVALs.

(DEFCOM COM-INSERT-FILE "Insert the contents of the specified file at point.
Reads a file name from the mini-buffer, and inserts the contents of that
file at point. Leaves point at the end of inserted text, and mark at the 
beginning.  Acts like Yank (Control-Y) with respect to the region." ()
  (MUNG-BP-INTERVAL (POINT))
  (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
  (MOVE-BP (MARK) (POINT))
  (SETQ *CURRENT-COMMAND-TYPE* ':YANK)
  (READ-DEFAULTED-AUX-FILE-NAME "Insert file:")
  (OPEN-FILE (STREAM *DEFAULT-AUX-FILE-NAME* '(IN))
	     (MOVE-BP (POINT) (STREAM-INTO-BP STREAM (POINT))))
  (MAYBE-DISPLAY-DIRECTORY ':READ *DEFAULT-AUX-FILE-NAME*)
  DIS-TEXT)

(DEFCOM COM-WRITE-REGION "Write out the region to the specified file." ()
  (REGION (BP1 BP2)
    (READ-DEFAULTED-AUX-FILE-NAME "Write region to:")
    (OPEN-FILE (STREAM *DEFAULT-AUX-FILE-NAME* '(OUT) T)
	       (STREAM-OUT-INTERVAL STREAM BP1 BP2 T)))
  DIS-NONE)

(DEFCOM COM-APPEND-TO-FILE "Append region to the end of the specified file." ()
  (REGION (BP1 BP2)
    (READ-DEFAULTED-AUX-FILE-NAME "Append region to end of file:")
    (OPEN-FILE (OSTREAM *DEFAULT-AUX-FILE-NAME* '(OUT) T)
      (OPEN-FILE (ISTREAM *DEFAULT-AUX-FILE-NAME* '(IN))
	(STREAM-COPY-UNTIL-EOF ISTREAM OSTREAM))
      (STREAM-OUT-INTERVAL OSTREAM BP1 BP2 T)))
  (MAYBE-DISPLAY-DIRECTORY ':READ *DEFAULT-AUX-FILE-NAME*)
  DIS-NONE)

(DEFCOM COM-PREPEND-TO-FILE "Append region to the beginning of the specified file." ()
  (REGION (BP1 BP2)
    (READ-DEFAULTED-AUX-FILE-NAME "Append region to start of file:")
    (OPEN-FILE (ISTREAM *DEFAULT-AUX-FILE-NAME* '(IN))
      (OPEN-FILE (OSTREAM *DEFAULT-AUX-FILE-NAME* '(OUT) T)
	(STREAM-OUT-INTERVAL OSTREAM BP1 BP2 T)
	(STREAM-COPY-UNTIL-EOF ISTREAM OSTREAM))))
  (MAYBE-DISPLAY-DIRECTORY ':READ *DEFAULT-AUX-FILE-NAME*)
  DIS-NONE)

(DEFCOM COM-VIEW-FILE "View contents of a file." ()
  (LET ((FILENAME (READ-DEFAULTED-FILE-NAME "View file:" (DEFAULT-FILE-NAME))))
    (PROMPT-LINE "Viewing ~A" FILENAME)
    (VIEW-FILE FILENAME))
  DIS-NONE)

;;; Show the file in the "display window".
;;; The caller should set up a reasonable prompt.
(COMMENT
(DEFUN VIEW-FILE (FILENAME &OPTIONAL (OUTPUT-STREAM STANDARD-OUTPUT))
  (FUNCALL OUTPUT-STREAM ':HOME-CURSOR)
  (FUNCALL OUTPUT-STREAM ':CLEAR-EOL)
  (OPEN-FILE (STREAM FILENAME '(READ))
    (STREAM-COPY-UNTIL-EOF STREAM OUTPUT-STREAM))
  (FUNCALL OUTPUT-STREAM ':CLEAR-EOF))
);COMMENT

(DEFUN VIEW-FILE (FILENAME)
  (OPEN-FILE (STREAM FILENAME '(READ))
    (VIEW-STREAM STREAM)))

(DEFUN VIEW-STREAM (STREAM &OPTIONAL (WINDOW (CREATE-OVERLYING-WINDOW *WINDOW*))
			   &AUX (INTERVAL (CREATE-BUFFER NIL)))
  (SETF (BUFFER-NAME INTERVAL) "")
  (FUNCALL (WINDOW-SHEET WINDOW) ':SET-LABEL "")
  (SET-WINDOW-INTERVAL WINDOW INTERVAL)
  (TEMPORARY-WINDOW-SELECT (WINDOW)
    (VIEW-WINDOW WINDOW STREAM)))

(DEFCOM COM-DELETE-FILE "Delete a file." ()
  (LET ((FILENAME (READ-DEFAULTED-FILE-NAME "Delete file:" (DEFAULT-FILE-NAME))))
    (DELETEF FILENAME))
  DIS-NONE)

(DEFCOM COM-RENAME-FILE "Rename one file to another." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (READ-TWO-DEFAULTED-FILE-NAMES "Rename" (DEFAULT-FILE-NAME))
    (RENAMEF FROM TO))
  DIS-NONE)

(DEFCOM COM-COPY-FILE "Copy one file to another." ()
  (MULTIPLE-VALUE-BIND (FROM TO)
      (READ-TWO-DEFAULTED-FILE-NAMES "Copy" (DEFAULT-FILE-NAME))
    (OPEN-FILE (FROM-STREAM FROM '(:IN))
      (OPEN-FILE (TO-STREAM TO '(:OUT) T)
        (STREAM-COPY-UNTIL-EOF FROM-STREAM TO-STREAM)
	(CLOSE TO-STREAM)
	(TYPEIN-LINE "Written: ~A" (FUNCALL TO-STREAM ':GET ':UNIQUE-ID)))))
DIS-NONE)

(DEFUN READ-TWO-DEFAULTED-FILE-NAMES (PROMPT DEFAULT &AUX FROM TO)
  (SETQ FROM (READ-DEFAULTED-FILE-NAME (FORMAT NIL "~A file:" PROMPT) DEFAULT)
	TO (READ-DEFAULTED-FILE-NAME (FORMAT NIL "~A ~A to:" PROMPT FROM) FROM))
  (MVRETURN FROM TO))

;;; Directory Listing stuff.

(DEFCOM COM-DISPLAY-DIRECTORY "Display current buffer's file's directory.
Use the directory listing function in the variable Directory Lister.
With an argument, accepts the name of a directory to list." ()
  (FUNCALL *DIRECTORY-LISTER*
	   (COND ((NOT *NUMERIC-ARG-P*)
		  (DEFAULT-FILE-NAME))
		 (T (READ-DIRECTORY-NAME "Directory:" (DEFAULT-FILE-NAME)))))
  DIS-NONE)

(DEFUN MAYBE-DISPLAY-DIRECTORY (TYPE &OPTIONAL (FILENAME *DEFAULT-FILE-NAME*))
  (COND ((OR (AND (EQ TYPE ':READ) (MEMQ *AUTO-DIRECTORY-DISPLAY* '(:READ T)))
	     (AND (EQ TYPE ':WRITE) (MEMQ *AUTO-DIRECTORY-DISPLAY* '(:WRITE T))))
	 (FUNCALL *DIRECTORY-LISTER* FILENAME))))

(DEFUN SUBSET-DIRECTORY-LISTING (FILENAME)
  (LET ((DEV (FUNCALL FILENAME ':DEVICE))
	(DIR (FUNCALL FILENAME ':DIRECTORY))
	(FN1 (FUNCALL FILENAME ':NAME))
	(FN2 (FUNCALL FILENAME ':FN2)))
    (FORMAT T "~&~A: ~A; ~A ~A~%" DEV DIR FN1 FN2)
    (LET ((LINE NIL)
	  (FREE-ARRAY (MAKE-ARRAY NIL 'ART-Q 10))
	  (USED-ARRAY (MAKE-ARRAY NIL 'ART-Q 10)))
      (OPEN-FILE (STREAM (FORMAT NIL "~A: ~A; .FILE. (DIR)" DEV DIR) '(READ))
		 ;; First find out how much space is free.
		 (SETQ LINE (FUNCALL STREAM ':LINE-IN))
		 (SETQ LINE (FUNCALL STREAM ':LINE-IN))
		 (DIRECTORY-FREE-SPACE LINE FREE-ARRAY)
		 ;; Make any pack that exists show up in the "used" display even if used=0
		 (DOTIMES (IDX 10)
		   (AND (AREF FREE-ARRAY IDX)
			(ASET 0 USED-ARRAY IDX)))
		 ;; Next, go through lines of dir, counting USED and printing some lines.
		 (DO ((KEY (STRING-APPEND " "
					  (IF (STRING-EQUAL FN1 "TS") FN2 FN1)
					  " "))
		      (LINE) (EOF))
		     (NIL)
		   (MULTIPLE-VALUE (LINE EOF)
		     (FUNCALL STREAM ':LINE-IN))
		   (AND (OR EOF (ZEROP (STRING-LENGTH LINE))) (RETURN NIL))
		   (AND (STRING-SEARCH KEY LINE)
			(FUNCALL STANDARD-OUTPUT ':LINE-OUT LINE))
		   (OR (= (AREF LINE 2) #/L)
		       (LET ((USED (PARSE-NUMBER LINE 20.))
			     (PACK (PARSE-NUMBER LINE 2)))
			 (LET ((IDX (IF (OR (< PACK 10.) (> PACK 16.)) 0
					(- PACK 9.))))
			   (ASET (+ (OR (AREF USED-ARRAY IDX) 0) USED) USED-ARRAY IDX)))))
		 (FORMAT-DISK-BLOCKS-ARRAY T "Free: " FREE-ARRAY)
		 (FORMAT-DISK-BLOCKS-ARRAY T ", Used: " USED-ARRAY)))))

;Element 0 of FREE-ARRAY is for packs other than 10.-16.
(DEFUN DIRECTORY-FREE-SPACE (LINE FREE-ARRAY)
  (DO ((I (STRING-SEARCH-CHAR #/# LINE)
	  (STRING-SEARCH-CHAR #/# LINE I))
       (NUM) (IDX) (BLKS))
      ((NULL I))
    (MULTIPLE-VALUE (NUM I)
      (PARSE-NUMBER LINE (1+ I)))
    (MULTIPLE-VALUE (BLKS I)
      (PARSE-NUMBER LINE (1+ I)))
    (SETQ IDX (IF (OR (< NUM 10.) (> NUM 16.)) 0
		  (- NUM 9.)))
    (ASET (+ (OR (AREF FREE-ARRAY IDX) 0) BLKS) FREE-ARRAY IDX)))

(DEFUN FORMAT-DISK-BLOCKS-ARRAY (STREAM TITLE ARRAY)
  (FORMAT STREAM TITLE)
  (DO ((IDX 0 (1+ IDX))
       (LIM (ARRAY-LENGTH ARRAY))
       (FIRSTP T)
       (BLKS))
      ((= IDX LIM))
    (COND ((SETQ BLKS (AREF ARRAY IDX))
	   (FORMAT STREAM "~:[+~]~D" FIRSTP BLKS)
	   (SETQ FIRSTP NIL)))))

;;; Returns spread pathname finally specified.
(DEFUN READ-DIRECTORY-NAME (PROMPT FILE-NAME)
  (LET ((DEFAULT (FORMAT NIL "~A: ~A; "
			 (FUNCALL FILE-NAME ':DEVICE) (FUNCALL FILE-NAME ':DIRECTORY))))
    (LET ((X (TYPEIN-LINE-READLINE "~A (Default is ~A)" PROMPT DEFAULT)))
      (SETQ X (COND ((EQUAL X "") DEFAULT)
		    ((NOT (STRING-SEARCH-SET '(#/; #\SP) X))
		     (STRING-APPEND X ";"))
		    (T X)))
      (SI:FILE-PARSE-NAME X NIL FILE-NAME))))

(DEFCOM COM-LIST-FILES "Brief directory listing.
Lists directory N entries to a line, with the following
special characters to the left of the filenames:
	: this is a link
	! this file has not been backed up to tape yet
	* this file has really been deleted but not yet
	  closed, or is otherwise locked.
	(blank) this is a plain normal file
Also the top line contains in order, the device being
listed from, the directory, Free: followed by the number of
free blocks on the device (separated into primary, secondary, etc.
packs), Used: followed by the number of blocks this directory is taking up." ()
  (LET ((FILENAME (READ-DIRECTORY-NAME "List Directory:" (DEFAULT-FILE-NAME)))
	(LINE NIL) (X NIL) (Y NIL) (X1 NIL) (Y1 NIL) (TEM1 NIL)
	(FREE-ARRAY (MAKE-ARRAY NIL 'ART-Q 10)) (USED-ARRAY (MAKE-ARRAY NIL 'ART-Q 10)))
    (OPEN-FILE (STREAM
		 (FUNCALL FILENAME ':DEFAULT-NAMESTRING ".FILE. (DIR)")
		 '(READ))
      (SETQ LINE (FUNCALL STREAM ':LINE-IN))
      (SETQ LINE (FUNCALL STREAM ':LINE-IN))
      (DIRECTORY-FREE-SPACE LINE FREE-ARRAY)
      (FORMAT T "~6A ~6A  " (FUNCALL FILENAME ':DEVICE) (FUNCALL FILENAME ':DIRECTORY))
      (FORMAT-DISK-BLOCKS-ARRAY STANDARD-OUTPUT "Free: " FREE-ARRAY)
      (FORMAT T ", Used: ")			;Filled in later
      (MULTIPLE-VALUE (X Y) (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':PIXEL))
      ;; Make any pack that exists show up in the "used" display even if used=0
      (DOTIMES (IDX 10)
	(AND (AREF FREE-ARRAY IDX)
	     (ASET 0 USED-ARRAY IDX)))
      (DO ((I 0 (\ (1+ I) 5)))
	  (NIL)
	(AND (ZEROP I) (TERPRI))
	(SETQ LINE (FUNCALL STREAM ':LINE-IN))
	(COND ((OR (NULL LINE)
		   (ZEROP (ARRAY-ACTIVE-LENGTH LINE))
		   (= (AREF LINE 0) #\FF))
	       (RETURN NIL)))
	(FUNCALL STANDARD-OUTPUT ':TYO
		 (COND ((= #/* (AREF LINE 0))
			#/*)
		       ((= #/L (AREF LINE 2))
			#/:)
		       (T (LET ((USED)
				(PACK (PARSE-NUMBER LINE 2)))
			    (MULTIPLE-VALUE (USED TEM1) (PARSE-NUMBER LINE 20.))
			    (LET ((IDX (IF (OR (< PACK 10.) (> PACK 16.)) 0
					   (- PACK 9.))))
			      (ASET (+ (OR (AREF USED-ARRAY IDX) 0) USED)
				    USED-ARRAY IDX)))
			  (COND ((= #/! (AREF LINE (1+ TEM1)))
				 #/!)
				(T #\SP)))))
	(FUNCALL STANDARD-OUTPUT ':STRING-OUT (NSUBSTRING LINE 6 19.))
	(FUNCALL STANDARD-OUTPUT ':STRING-OUT "  "))
      (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
      (MULTIPLE-VALUE (X1 Y1) (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':PIXEL))
      (FUNCALL STANDARD-OUTPUT ':SET-CURSORPOS X Y ':PIXEL)
      (FORMAT-DISK-BLOCKS-ARRAY STANDARD-OUTPUT "" USED-ARRAY)
      (FUNCALL STANDARD-OUTPUT ':SET-CURSORPOS X1 Y1 ':PIXEL)))
  DIS-NONE)

(DEFUN ROTATED-DIRECTORY-LISTING (FILENAME)
  (*CATCH 'ABORT
     (LET ((DEV (FUNCALL FILENAME ':DEVICE))
           (DIR (FUNCALL FILENAME ':DIRECTORY))
           (FN1 (FUNCALL FILENAME ':NAME))
           (FN2 (FUNCALL FILENAME ':FN2))
           (FN NIL))
       (SETQ FN (FORMAT NIL "~A: ~A; .FILE. (DIR)" DEV DIR))
       (PROMPT-LINE "Directory Listing")
       (FORMAT T "~A  ~A    --   ~A: ~A; ~A ~A~%" DEV DIR DEV DIR FN1 FN2)
       (LET ((LINE NIL) (X 0) (Y 0))
	 (OPEN-FILE (STREAM FN '(IN))
	   (SETQ LINE (FUNCALL STREAM ':LINE-IN))
	   (FORMAT T "~A~%" (FUNCALL STREAM ':LINE-IN))
	   (DO ((LINE (SETQ LINE (FUNCALL STREAM ':LINE-IN))
		      (SETQ LINE (FUNCALL STREAM ':LINE-IN)))
		(LFN1 (STRING-LENGTH FN1))
		(LFN16 (+ (STRING-LENGTH FN1) 6))
		)
	       ((NULL LINE)
		(FORMAT T "There is no file named ~A in the directory.~%" FN1))
	     (COND ((STRING-EQUAL LINE FN1 6 0 LFN16 LFN1)
		    ;; Found one.
		    (LET ((FIRST LINE))
		      (SETQ LINE (DO ((LINE LINE (FUNCALL STREAM ':LINE-IN)))
				     ((OR (= (AREF LINE 0) #\FF)
					  (NOT (STRING-EQUAL LINE FN1 6 0 LFN16 LFN1)))
				      LINE)
				   (FORMAT T "~A~%" LINE)))
		      (FORMAT T "==MORE==")
		      (COND ((NOT (= (FUNCALL STANDARD-INPUT ':TYI) #\SP))
			     ;;(SETQ ED-INHIBIT-REDISPLAY-P NIL)
			     (*THROW 'ABORT NIL)))
		      (MULTIPLE-VALUE (X Y)
			(FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':PIXEL))
		      (FUNCALL STANDARD-OUTPUT ':SET-CURSORPOS ':PIXEL 0 Y)
		      (FUNCALL STANDARD-OUTPUT ':CLEAR-EOL)
		      (DO ((LINE LINE (FUNCALL STREAM ':LINE-IN)))
			  ((EQUAL LINE FIRST))
			(COND ((= (AREF LINE 0) #\FF)
			       (FORMAT T "------------------------------------------------~%")
			       (CLOSE STREAM)
			       (SETQ STREAM (OPEN FN '(IN)))
			       (FUNCALL STREAM ':LINE-IN)
			       (FUNCALL STREAM ':LINE-IN)
			       (SETQ LINE (FUNCALL STREAM ':LINE-IN))))
			(FORMAT T "~A~%" LINE)))
		    (RETURN NIL)))))))))
