;;; -*- Mode:Lisp; Package:System-Internals; Base:8 -*-

;;; Disk label editor

;;; Simple routines for manipulating own label
;;; These are to be called by the user

(DEFUN SET-CURRENT-MICROLOAD (BAND &OPTIONAL (UNIT 0))
  (SET-CURRENT-BAND BAND UNIT T))

(DEFUN SET-CURRENT-BAND (BAND &OPTIONAL (UNIT 0) MICRO-P &AUX RQB)
  (SETQ UNIT (DECODE-UNIT-ARGUMENT UNIT
	         (FORMAT NIL "(SET-CURRENT-~:[BAND~;MICROLOAD~] ~D)" MICRO-P BAND)))
  (UNWIND-PROTECT
   (PROGN
    (WITHOUT-INTERRUPTS
     (SETQ RQB (GET-DISK-RQB)))
    (SETQ BAND (COND ((NUMBERP BAND) (FORMAT NIL (COND (MICRO-P "MCR~D") (T "LOD~D")) BAND))
		     ((STRINGP BAND) (STRING-UPCASE BAND))
		     (T (GET-PNAME BAND))))
    (OR (FIND-DISK-PARTITION BAND RQB UNIT)	;Does a READ-DISK-LABEL
        (FERROR NIL "Partition ~A does not exist" BAND))      
    (PUT-DISK-STRING RQB BAND (COND (MICRO-P 6) (T 7)) 4)
    (WRITE-DISK-LABEL RQB UNIT))
   (RETURN-DISK-RQB RQB))
  (DISPOSE-OF-UNIT UNIT)
  NIL)

(DEFUN PRINT-DISK-LABEL (&OPTIONAL (UNIT 0) (STREAM STANDARD-OUTPUT)
                         &AUX RQB)
  (SETQ UNIT (DECODE-UNIT-ARGUMENT UNIT "reading label"))
  (UNWIND-PROTECT
   (PROGN (WITHOUT-INTERRUPTS
	     (SETQ RQB (GET-DISK-RQB)))
	  (READ-DISK-LABEL RQB UNIT)
	  (PRINT-DISK-LABEL-FROM-RQB STREAM RQB NIL))
   (RETURN-DISK-RQB RQB))
  (DISPOSE-OF-UNIT UNIT))

(DECLARE (SPECIAL LE-STRUCTURE))
;;; LE-STRUCTURE is a list of items, each item looks like:
;;; (name value start-x start-y width)

;;; This is a subroutine for PRINT-DISK-LABEL-FROM-RQB which implements this.
;;; Note that if not consing up a structure, this must work on a non-display stream
(DEFUN LE-OUT (NAME VALUE STREAM CONS-UP-LE-STRUCTURE-P
	       &AUX X Y WIDTH)
  (AND CONS-UP-LE-STRUCTURE-P
       (MULTIPLE-VALUE (X Y) (FUNCALL TERMINAL-IO ':READ-CURSORPOS)))
  (FORMAT STREAM (IF (NUMBERP VALUE) "~D" "~A") VALUE)
  (COND (CONS-UP-LE-STRUCTURE-P
	 (SETQ WIDTH (- (FUNCALL TERMINAL-IO ':READ-CURSORPOS) X))
	 (AND (MINUSP WIDTH) (SETQ WIDTH (- (TV:SHEET-INSIDE-RIGHT TERMINAL-IO) X)))
	 (AND (ZEROP WIDTH) (SETQ WIDTH 4))
	 (PUSH (LIST NAME VALUE X Y WIDTH) LE-STRUCTURE)))
  NIL)

(DEFUN PRINT-DISK-LABEL-FROM-RQB (STREAM RQB CONS-UP-LE-STRUCTURE-P
				  &AUX N-PARTITIONS WORDS-PER-PART THIS-END NEXT-BASE
				  CURRENT-MICROLOAD CURRENT-BAND)
  (TERPRI STREAM)
  (LE-OUT 'PACK-NAME (GET-DISK-STRING RQB 20 32.) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC ": " STREAM)
  (LE-OUT 'DRIVE-NAME (GET-DISK-STRING RQB 10 32.) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC ", " STREAM)
  (LE-OUT 'COMMENT (GET-DISK-STRING RQB 30 96.) STREAM CONS-UP-LE-STRUCTURE-P)
  (FORMAT STREAM "~%~A version ~D, "	;You can't edit these
	  (GET-DISK-STRING RQB 0 4) (GET-DISK-FIXNUM RQB 1))
  (LE-OUT 'N-CYLINDERS (GET-DISK-FIXNUM RQB 2) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " cylinders, " STREAM)
  (LE-OUT 'N-HEADS (GET-DISK-FIXNUM RQB 3) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " heads, " STREAM)
  (LE-OUT 'N-BLOCKS-PER-TRACK (GET-DISK-FIXNUM RQB 4) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " blocks//track, " STREAM)
  (FORMAT T "~D" (GET-DISK-FIXNUM RQB 5))
  (PRINC " blocks//cylinder" STREAM)
  (TERPRI STREAM)
  (PRINC "Current microload = " STREAM)
  (LE-OUT 'CURRENT-MICROLOAD (SETQ CURRENT-MICROLOAD (GET-DISK-STRING RQB 6 4))
	  STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC ", current virtual memory load (band) = " STREAM)
  (LE-OUT 'CURRENT-BAND (SETQ CURRENT-BAND (GET-DISK-STRING RQB 7 4))
	  STREAM CONS-UP-LE-STRUCTURE-P)
  (TERPRI STREAM)
  (LE-OUT 'N-PARTITIONS (SETQ N-PARTITIONS (GET-DISK-FIXNUM RQB 200))
	  STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " partitions, " STREAM)
  (LE-OUT 'WORDS-PER-PART (SETQ WORDS-PER-PART (GET-DISK-FIXNUM RQB 201))
	  STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC "-word descriptors:" STREAM)
  (DO ((I 0 (1+ I))
       (PARTITION-NAME)
       (LOC 202 (+ LOC WORDS-PER-PART)))
      ((= I N-PARTITIONS))
    (SETQ PARTITION-NAME (GET-DISK-STRING RQB LOC 4))
    (IF (OR (STRING-EQUAL PARTITION-NAME CURRENT-MICROLOAD)
	    (STRING-EQUAL PARTITION-NAME CURRENT-BAND))
	(FORMAT STREAM "~%* ")
	(FORMAT STREAM "~%  "))
    (LE-OUT 'PARTITION-NAME (GET-DISK-STRING RQB LOC 4) STREAM CONS-UP-LE-STRUCTURE-P)
    (PRINC " at block " STREAM)
    (LE-OUT 'PARTITION-START (GET-DISK-FIXNUM RQB (1+ LOC)) STREAM CONS-UP-LE-STRUCTURE-P)
    (PRINC ", " STREAM)
    (LE-OUT 'PARTITION-SIZE (GET-DISK-FIXNUM RQB (+ LOC 2)) STREAM CONS-UP-LE-STRUCTURE-P)
    (PRINC " blocks long" STREAM)
    (COND ((> WORDS-PER-PART 3)	;Partition comment
	   (PRINC ", /"" STREAM)
	   (LE-OUT 'PARTITION-COMMENT (GET-DISK-STRING RQB (+ LOC 3) 16.)
		    STREAM CONS-UP-LE-STRUCTURE-P)
	   (TYO #/" STREAM)))
    (SETQ THIS-END (+ (GET-DISK-FIXNUM RQB (1+ LOC)) (GET-DISK-FIXNUM RQB (+ LOC 2)))
	  NEXT-BASE (COND ((= (1+ I) N-PARTITIONS)
			   (* (GET-DISK-FIXNUM RQB 2) (GET-DISK-FIXNUM RQB 5)))
			  ((GET-DISK-FIXNUM RQB (+ LOC 1 WORDS-PER-PART)))))
    (COND ((> (- NEXT-BASE THIS-END) 0)
	   (FORMAT STREAM ", ~D blocks free at ~D" (- NEXT-BASE THIS-END) THIS-END))
	  ((< (- NEXT-BASE THIS-END) 0)
	   (FORMAT STREAM ", ~D blocks overlap" (- THIS-END NEXT-BASE))))))

(DEFUN P-BIGNUM (ADR)
  (DPB (%P-LDB 2020 ADR) 2020 (%P-LDB 0020 ADR)))

; This will get hairier later, e.g. check for wrap around
; Also this only understands the Trident controller I guess
(DEFUN PRINT-DISK-ERROR-LOG ()
  (FORMAT T "~&Disk error count ~D.~%" (READ-METER 'SYS:%COUNT-DISK-ERRORS))
  (DO I 600 (+ I 4) (= I 640)
    (LET ((CLP-CMD (P-BIGNUM I))
	  (DA (P-BIGNUM (1+ I)))
	  (STS (P-BIGNUM (+ I 2)))
	  (MA (P-BIGNUM (+ I 3))))
      (COND ((NOT (ZEROP CLP-CMD))
	     (FORMAT T "~%Command ~O ~@[(~A) ~]"
		       (LDB 0020 CLP-CMD)
		       (CDR (ASSQ (LDB 0004 CLP-CMD) '((0 . "Read")
						       (10 . "Read-Compare")
						       (11 . "Write")))))
	     (AND (BIT-TEST %DISK-COMMAND-DATA-STROBE-EARLY CLP-CMD)
		  (PRINC "Data-Strobe-Early "))
	     (AND (BIT-TEST %DISK-COMMAND-DATA-STROBE-LATE CLP-CMD)
		  (PRINC "Data-Strobe-Late "))
	     (AND (BIT-TEST %DISK-COMMAND-SERVO-OFFSET CLP-CMD)
		  (PRINC "Servo-offset "))
	     (AND (BIT-TEST %DISK-COMMAND-SERVO-OFFSET-FORWARD CLP-CMD)
		  (PRINC "S-O-Forward "))
	     (TERPRI)
	     (FORMAT T "CCW-list pointer ~O (low 16 bits)~%" (LDB 2020 CLP-CMD))
	     (FORMAT T "Disk address: unit ~O, cylinder ~O, head ~O, block ~O (~4:*~D ~D ~D ~D decimal)~%"
		       (LDB 3404 DA) (LDB 2014 DA) (LDB 1010 DA) (LDB 0010 DA))
	     (FORMAT T "Memory address: ~O (type bits ~O)~%"
		       (LDB 0026 MA) (LDB 2602 MA))
	     (FORMAT T "Status: ~O" STS)
	     (DO ((PPSS 2701 (- PPSS 100))
		  (L '("Internal-parity" "Read-compare" "CCW-cycle" "NXM" "Mem-parity"
		       "Header-Compare" "Header-ECC" "ECC-Hard" "ECC-Soft"
		       "Overrun" "Transfer-Aborted (or wr. ovr.)" "Start-Block-Error"
		       "Timeout" "Seek-Error" "Off-Line" "Off-Cylinder"
		       "Read-Only" "Fault" "No-Select" "Multiple-Select"
		       "Interrupt" "Sel-Unit-Attention" "Any-Unit-Attention" "Idle")
		     (CDR L)))
		 ((MINUSP PPSS) (TERPRI))
	       (AND (LDB-TEST PPSS STS) (FORMAT T "~<~%~8X~:;  ~A~>" (CAR L)))))))))

;;; Label editor

(DECLARE (SPECIAL LE-ITEM-NUMBER LE-UNIT LE-RQB))

;;; Change n-words-per-partition of a label sitting in an RQB
(DEFUN CHANGE-PARTITION-MAP (RQB NEW-N-WORDS)
  (LET ((OLD-N-WORDS (GET-DISK-FIXNUM RQB 201))
	(N-PARTITIONS (GET-DISK-FIXNUM RQB 200)))
    (LET ((SAVE (MAKE-ARRAY NIL 'ART-Q (LIST N-PARTITIONS (MAX OLD-N-WORDS NEW-N-WORDS)))))
      ;; Fill with zeros
      (DOTIMES (I N-PARTITIONS)
	(DOTIMES (J (MAX OLD-N-WORDS NEW-N-WORDS))
	  (ASET 0 SAVE I J)))
      ;; Copy out
      (DOTIMES (I N-PARTITIONS)
	(DOTIMES (J OLD-N-WORDS)
	  (ASET (GET-DISK-FIXNUM RQB (+ 202 (* I OLD-N-WORDS) J)) SAVE I J)))
      ;; Copy back in
      (PUT-DISK-FIXNUM RQB NEW-N-WORDS 201)
      (DOTIMES (I N-PARTITIONS)
	(DOTIMES (J NEW-N-WORDS)
	  (PUT-DISK-FIXNUM RQB (AREF SAVE I J) (+ 202 (* I NEW-N-WORDS) J)))))))

;;; Known pack types.  The first on this list is the default.
;;; Each element is a 4-list of
;;;   Pack brand name (32 or fewer chars) (as a symbol).
;;;   Number of cylinders.
;;;   Number of heads.
;;;   Number of blocks per track.
;;;   Partition list: name, size (- blocks, + cylinders at cyl bndry)
;;;   First partition starts at block 17. (first track reserved)
(DECLARE (SPECIAL PACK-TYPES))
(SETQ PACK-TYPES
      '((|Trident T-80| 815. 5. 17.
		((MCR1 -224) (MCR2 -224) (PAGE 300.) (FILE 61.)
		 (LOD1 150.) (LOD2 150.) (LOD3 150.)))
	(|Trident T-300| 815. 19. 17.
		((MCR1 -224) (MCR2 -224) (PAGE 202.) ;Full address space
		 (FILE 312.)
		 (LOD1 75.) (LOD2 75.) (LOD3 75.) (LOD4 75.)))
	;; Unfortunately there is not quite room enough for 3 partitions
	;; adequate to hold an initial load.  So we can only have a PAGE
	;; partition and a single save band.
	(|Marksman M-20| 210. 4. 21.
		((MCR1 -224) (MCR2 -224) (LOD1 80.) (PAGE 126.)))))

(DEFUN LE-INITIALIZE-LABEL (RQB PACK-TYPE)
  (PUT-DISK-STRING RQB "LABL" 0 4)		;Checkword
  (PUT-DISK-FIXNUM RQB 1 1)			;Version number
  (PUT-DISK-FIXNUM RQB (CADR PACK-TYPE) 2)	;Number of cylinders
  (PUT-DISK-FIXNUM RQB (CADDR PACK-TYPE) 3)	;Number of heads
  (PUT-DISK-FIXNUM RQB (CADDDR PACK-TYPE) 4)	;Blocks per track
  (PUT-DISK-FIXNUM RQB (* (CADDR PACK-TYPE) (CADDDR PACK-TYPE)) 5)
  (PUT-DISK-STRING RQB "MCR1" 6 4)		;Current microload
  (PUT-DISK-STRING RQB "LOD1" 7 4)		;Current band
  (PUT-DISK-STRING RQB (STRING (CAR PACK-TYPE)) 10 40)	;Brand name of drive
  (PUT-DISK-STRING RQB "(name)" 20 40)		;Name of pack
  (PUT-DISK-STRING RQB "(comment)" 30 140)	;Comment
  (PUT-DISK-FIXNUM RQB (LENGTH (FIFTH PACK-TYPE)) 200) ;Number of partitions
  (PUT-DISK-FIXNUM RQB 7 201)			;Words per partition descriptor
  (DO ((LOC 202 (+ LOC 7))
       (BLOCK (CADDDR PACK-TYPE) (+ BLOCK SZ))
       (SZ)
       (BPC (* (CADDR PACK-TYPE) (CADDDR PACK-TYPE)))
       (PARTS (FIFTH PACK-TYPE) (CDR PARTS)))
      ((NULL PARTS))
    (SETQ SZ (COND ((MINUSP (CADAR PARTS)) (MINUS (CADAR PARTS)))
		   (T (SETQ BLOCK (* BPC (// (+ BLOCK -1 BPC) BPC)))
		      (* (CADAR PARTS) BPC))))
    (PUT-DISK-STRING RQB (STRING (CAAR PARTS)) LOC 4)
    (PUT-DISK-FIXNUM RQB BLOCK (1+ LOC))
    (PUT-DISK-FIXNUM RQB SZ (+ LOC 2))
    (PUT-DISK-STRING RQB "" (+ LOC 3) 16.)))

;;; Display the label which is sitting in an RQB 
(DEFUN LE-DISPLAY-LABEL (RQB UNIT)
  (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
  (COND ((NUMBERP UNIT)
	 (FORMAT T "Editing label for unit ~D~%" UNIT))
	(T (FORMAT T "Editing label for unit ~D on ~A~%"
		     (FUNCALL UNIT ':UNIT-NUMBER)
		     (FUNCALL UNIT ':MACHINE-NAME)))) 
  (SETQ LE-STRUCTURE NIL)
  (PRINT-DISK-LABEL-FROM-RQB STANDARD-OUTPUT RQB T)
  (SETQ LE-STRUCTURE (NREVERSE LE-STRUCTURE))
  (FORMAT T "~%~%~%")
  (LE-UNDERSCORE))

;; Underscore the selected item
(DEFUN LE-UNDERSCORE ()
  (LET ((ITEM (NTH LE-ITEM-NUMBER LE-STRUCTURE)))
    (AND ITEM
	 (FUNCALL TERMINAL-IO ':DRAW-RECTANGLE
		  (FIFTH ITEM) 1
		  (THIRD ITEM) (+ (FOURTH ITEM)
				  (- (TV:SHEET-LINE-HEIGHT TERMINAL-IO) 2))
		   TV:ALU-XOR))))

(DEFUN EDIT-DISK-LABEL (LE-UNIT &OPTIONAL (INIT-P NIL) ;If t, dont try to save page 1
   ; since current label is garbage.  It can bomb setting blocks-per-track to 0, etc.
				&AUX LE-RQB LE-STRUCTURE (LE-ITEM-NUMBER 0) CH COM)
  (SETQ LE-UNIT (DECODE-UNIT-ARGUMENT LE-UNIT "editing label" INIT-P))
  (UNWIND-PROTECT
     (PROGN (WITHOUT-INTERRUPTS
	     (SETQ LE-RQB (GET-DISK-RQB)))
	    (LE-INITIALIZE-LABEL LE-RQB (CAR PACK-TYPES))
	    (LE-DISPLAY-LABEL LE-RQB LE-UNIT)
	    (FORMAT T "Use Control-R to read and edit existing label; hit HELP for help.~%")
	    (*CATCH 'LE-EXIT
		    (DO ()
			(NIL)
		      (SETQ CH (FUNCALL STANDARD-INPUT ':TYI))
		      (SETQ COM (INTERN-SOFT (STRING-UPCASE (FORMAT NIL "LE-COM-~:C" CH))
					     "SI"))
		      (COND ((OR (NULL COM) (NOT (FBOUNDP COM))) (BEEP))
			    (T (FUNCALL COM))))))
     (RETURN-DISK-RQB LE-RQB))
  (DISPOSE-OF-UNIT LE-UNIT))

(DEFUN LE-COM-FORM ()		;Redisplay
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

(DEFUN LE-COM-END ()		;Exit
  (*THROW 'LE-EXIT NIL))

(DEFUN LE-COM-CONTROL-B ()	;Previous item
  (LE-UNDERSCORE)
  (SETQ LE-ITEM-NUMBER (MAX 0 (1- LE-ITEM-NUMBER)))
  (LE-UNDERSCORE))

(DEFUN LE-COM-CONTROL-F ()	;Next item
  (LE-UNDERSCORE)
  (SETQ LE-ITEM-NUMBER (MIN (1- (LENGTH LE-STRUCTURE))
			    (1+ LE-ITEM-NUMBER)))
  (LE-UNDERSCORE))

(DEFUN LE-COM-CONTROL-N ()	;First item on next line
  (LE-UNDERSCORE)
  (DO ((L (NTHCDR LE-ITEM-NUMBER LE-STRUCTURE) (CDR L))
       (N LE-ITEM-NUMBER (1+ N))
       (Y0 (OR (FOURTH (NTH LE-ITEM-NUMBER LE-STRUCTURE)) 0)))
      ((OR (NULL L) (> (FOURTH (CAR L)) Y0))
       (SETQ LE-ITEM-NUMBER (MIN (1- (LENGTH LE-STRUCTURE)) N))
       (LE-UNDERSCORE))))

(DEFUN LE-COM-CONTROL-P ()	;First item on previous line
  (LE-UNDERSCORE)
  (DO ((Y0 (OR (FOURTH (NTH LE-ITEM-NUMBER LE-STRUCTURE)) 0))
       (L LE-STRUCTURE (CDR L))
       (N 0 (1+ N))
       (Y) (CAND-Y -1) (CAND-N 0))
      (())
    (SETQ Y (FOURTH (CAR L)))
    (COND ((= Y Y0) (SETQ LE-ITEM-NUMBER CAND-N)
		    (LE-UNDERSCORE)
		    (RETURN NIL))
	  ((= Y CAND-Y) ) ;Next thing on same line
	  (T (SETQ CAND-Y Y CAND-N N))))) ;First thing on a line

(DEFUN LE-COM-CONTROL-R ()	;Read in the label
  (READ-DISK-LABEL LE-RQB LE-UNIT)
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

(DEFUN LE-COM-CONTROL-W ()	;Write out the label
  (AND (Y-OR-N-P "Do you want to write out this label?")
       (WRITE-DISK-LABEL LE-RQB LE-UNIT)))

(DEFUN LE-COM-CONTROL-I ()	;Initialize
  (FORMAT T "Pack types are:~%")
  (DO ((L PACK-TYPES (CDR L))
       (N 0 (1+ N)))
      ((NULL L))
    (FORMAT T " ~S  ~A~%" N (CAAR L)))
  (FORMAT T "Enter desired number: ")
  (LET ((TEM (NTH (READ) PACK-TYPES)))
    (TYI)
    (AND TEM (LE-INITIALIZE-LABEL LE-RQB TEM)))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

(DEFUN LE-COM-CONTROL-D ()	;Delete this partition
  (LET ((PLOC (LE-CURRENT-PARTITION)))
    (FORMAT T "~&Delete the ~S partition? " (GET-DISK-STRING LE-RQB PLOC 4))
    (COND ((Y-OR-N-P)
	   (LET ((NPARTS (GET-DISK-FIXNUM LE-RQB 200))
		 (NWORDS (GET-DISK-FIXNUM LE-RQB 201))
		 (BUF (RQB-BUFFER LE-RQB)))
	     (PUT-DISK-FIXNUM LE-RQB (MAX (1- NPARTS) 0) 200)
	     (COPY-ARRAY-PORTION BUF (* (+ PLOC NWORDS) 2) (ARRAY-LENGTH BUF)
				 BUF (* PLOC 2) (ARRAY-LENGTH BUF)))
	   (LE-DISPLAY-LABEL LE-RQB LE-UNIT)))))

(DEFUN LE-COM-CONTROL-O ()	;Add a partition
  (LET ((PLOC (LE-CURRENT-PARTITION)))
    (LET ((NPARTS (1+ (GET-DISK-FIXNUM LE-RQB 200)))
	  (NWORDS (GET-DISK-FIXNUM LE-RQB 201))
	  (BUF (RQB-BUFFER LE-RQB)))
      (COND ((> (+ (* NPARTS NWORDS) 202) 400)
	     (FORMAT T "~&Partition table full"))
	    (T (PUT-DISK-FIXNUM LE-RQB NPARTS 200)
	       (LET ((FOO (MAKE-ARRAY NIL 'ART-16B 1000)))
		 (COPY-ARRAY-PORTION BUF (* PLOC 2) (ARRAY-LENGTH BUF) FOO (* NWORDS 2) 1000)
		 (COPY-ARRAY-PORTION FOO 0 1000 BUF (* PLOC 2) (ARRAY-LENGTH BUF))
		 (PUT-DISK-STRING LE-RQB "????" PLOC 4)
		 (PUT-DISK-FIXNUM LE-RQB (GET-DISK-FIXNUM LE-RQB (+ PLOC NWORDS 1)) (1+ PLOC))
		 (RETURN-ARRAY FOO))
	       (LE-DISPLAY-LABEL LE-RQB LE-UNIT))))))

(DEFUN LE-COM-CONTROL-S ()	;Sort partitions by address (2nd word) and redisplay
  (DO ((NPARTS (GET-DISK-FIXNUM LE-RQB 200) (1- NPARTS))
       (NWORDS (GET-DISK-FIXNUM LE-RQB 201))
       (FROB NIL NIL)
       (PART-LIST NIL (CONS (CONS (GET-DISK-FIXNUM LE-RQB (1+ LOC)) FROB) PART-LIST))
       (LOC 202 (+ LOC NWORDS))
       (BUF (RQB-BUFFER LE-RQB)))
      ((ZEROP NPARTS)
       (SETQ PART-LIST (SORTCAR PART-LIST #'<))
       (DO ((L PART-LIST (CDR L))
	    (LOC 202 (+ LOC NWORDS)))
	   ((NULL L))
	 (DO ((K (CDAR L) (CDR K))
	      (I (1- (* 2 NWORDS)) (1- I)))
	     ((MINUSP I))
	   (ASET (CAR K) BUF (+ LOC LOC I)))))
    (DOTIMES (I (* 2 NWORDS))
      (PUSH (AREF BUF (+ LOC LOC I)) FROB)))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

;; This, my friends, is the hairy part
(DEFUN LE-COM-CONTROL-E ()	;Edit the selected item
  (LET ((ITEM (NTH LE-ITEM-NUMBER LE-STRUCTURE)))
    (LET ((NAME (FIRST ITEM))
	  (VALUE (SECOND ITEM)))
      (FORMAT T "Change the ~A from ~D to: " NAME VALUE)
      (SETQ VALUE (COND ((NUMBERP VALUE) (LET ((IBASE 10.)) (READ)))
			(T (READLINE))))
      (SELECTQ NAME
	(PACK-NAME (PUT-DISK-STRING LE-RQB VALUE 20 32.))
	(DRIVE-NAME (PUT-DISK-STRING LE-RQB VALUE 10 32.))
	(COMMENT (PUT-DISK-STRING LE-RQB VALUE 30 96.))
	(N-CYLINDERS (PUT-DISK-FIXNUM LE-RQB VALUE 2))
	(N-HEADS (PUT-DISK-FIXNUM LE-RQB VALUE 3)
		 (PUT-DISK-FIXNUM LE-RQB (* VALUE (GET-DISK-FIXNUM LE-RQB 4)) 5))
	(N-BLOCKS-PER-TRACK (PUT-DISK-FIXNUM LE-RQB VALUE 4)
			    (PUT-DISK-FIXNUM LE-RQB (* VALUE (GET-DISK-FIXNUM LE-RQB 3)) 5))
	(CURRENT-MICROLOAD (PUT-DISK-STRING LE-RQB VALUE 6 4))
	(CURRENT-BAND (PUT-DISK-STRING LE-RQB VALUE 7 4))
	(N-PARTITIONS (PUT-DISK-FIXNUM LE-RQB VALUE 200))
	(WORDS-PER-PART (CHANGE-PARTITION-MAP LE-RQB VALUE))
	;; These occur in multiple instances; hair is required
	((PARTITION-NAME PARTITION-START PARTITION-SIZE PARTITION-COMMENT)
	 (LET ((PLOC (LE-CURRENT-PARTITION)))
	   (SELECTQ NAME
	     (PARTITION-NAME (PUT-DISK-STRING LE-RQB VALUE PLOC 4))
	     (PARTITION-START (PUT-DISK-FIXNUM LE-RQB VALUE (1+ PLOC)))
	     (PARTITION-SIZE (PUT-DISK-FIXNUM LE-RQB VALUE (+ PLOC 2)))
	     (PARTITION-COMMENT (PUT-DISK-STRING LE-RQB VALUE (+ PLOC 3) 16.)))))
	(OTHERWISE (FERROR NIL "No editor for ~S" NAME)))))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

;Returns the word number of the start of the descriptor for the partition
;containing the current item.
(DEFUN LE-CURRENT-PARTITION ()
  (DO ((WORDS-PER-PARTITION (GET-DISK-FIXNUM LE-RQB 201))
       (PNO 0)
       (L LE-STRUCTURE (CDR L))
       (N LE-ITEM-NUMBER (1- N)))
      ((ZEROP N)
       (+ 202 (* PNO WORDS-PER-PARTITION)))
    (AND (EQ (CAAR L) 'PARTITION-COMMENT) (SETQ PNO (1+ PNO)))))

(DEFUN LE-COM-HELP () (LE-COM-?))
(DEFUN LE-COM-? ()		;Give help
  (PRINC "Commands are:
B back, F forward, P up, N down
R read label from disk, W write label to disk, I initialize the label
E edit selected item
O add partition, D delete partition, S sort partitions
<END> exit"))
