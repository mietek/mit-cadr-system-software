;-*- Mode:LISP; Package:SYSTEM-INTERNALS -*-

; Some metering/stepping/debugging stuff

(DEFUN SINGLE-STEP-SG (SG &AUX TEM)
  ;; Cause the error handler to funcall us back
  (IF (SETQ TEM (ASSQ SG EH:SG-STEPPING-TABLE))
      (RPLACD TEM %CURRENT-STACK-GROUP)
      (PUSH (CONS SG %CURRENT-STACK-GROUP) EH:SG-STEPPING-TABLE))
  (SETF (SG-INST-DISP SG) 2) ;SG-SINGLE-STEP-DISPATCH
  (SETQ TEM (FUNCALL SG NIL))
  (OR (EQ TEM SG) (EQ TEM 'EXHAUSTED) (FERROR NIL "Hmm, ~S  ~S" TEM SG))
  (SETF (SG-INST-DISP SG) 0) ;SG-MAIN-DISPATCH
  TEM)

(DEFUN CEASE-STEPPING-SG (SG)
  (DOLIST (X EH:SG-STEPPING-TABLE)
    (AND (EQ (CAR X) SG) (SETQ EH:SG-STEPPING-TABLE (DELQ X EH:SG-STEPPING-TABLE)))))



(DEFVAR MAGIC-SG)

;notes - safe must be 0, but defaults to 1
;beware getting into the background, should pass terminal-io over
(DEFUN SETUP-MAGIC-SG-TO-EVAL (FORM &AUX TEM)
  (OR (BOUNDP 'MAGIC-SG)
      (SETQ MAGIC-SG (MAKE-STACK-GROUP "Magic SG" ':SAFE 0
						  ':REGULAR-PDL-SIZE 10000
						  ':SPECIAL-PDL-SIZE 2000)))
  (SETF (SG-INST-DISP MAGIC-SG) 0)  ;Could have been left around if crash
  (STACK-GROUP-PRESET MAGIC-SG #'(LAMBDA (FORM TERMINAL-IO CALLING-SG)
				   (FUNCALL CALLING-SG 'OK)
				   (EVAL FORM)
				   (FUNCALL CALLING-SG 'EXHAUSTED))
		      FORM TERMINAL-IO %CURRENT-STACK-GROUP)
  (OR (EQ (SETQ TEM (FUNCALL MAGIC-SG)) 'OK) (FERROR NIL "Hmm, ~S  ~S" TEM 'OK)))

(DEFUN COUNT-OPCODE-USAGE (FORM &OPTIONAL (N-POPULAR 20.))
  (SETUP-MAGIC-SG-TO-EVAL FORM)
  (LET ((COUNTS (MAKE-ARRAY NIL 'ART-Q 2000))	;Indexed by high 10 bits of instruction
	(MISC-COUNTS (MAKE-ARRAY NIL 'ART-Q 1000))  ;Indexed by misc function number
	(RP (SG-REGULAR-PDL MAGIC-SG))
	(AP)
	(TOTAL 0))
    (FILLARRAY COUNTS '(0))
    (FILLARRAY MISC-COUNTS '(0))
    (WITHOUT-INTERRUPTS ;in case other SG does WITHOUT-INTERRUPTS and needs it
      (DO () (NIL)
	(AND (EQ (SINGLE-STEP-SG MAGIC-SG) 'EXHAUSTED) (RETURN))
	(SETQ AP (SG-AP MAGIC-SG))
	(LET ((INST (EH:FEF-INSTRUCTION (RP-FUNCTION-WORD RP AP) (RP-EXIT-PC RP AP))))
	    ;; That is the next instruction to be executed (hasn't been done yet)
	    (AND (= (LDB 1104 INST) 15)		;MISC
		 (LET ((MISCNO (LOGAND 777 INST)))
		   (ASET (1+ (AREF MISC-COUNTS MISCNO)) MISC-COUNTS MISCNO)))
	    ;; Punt the delta field, count all the rest 
	    (SETQ INST (LSH INST -6))
	    (ASET (1+ (AREF COUNTS INST)) COUNTS INST)
	    (SETQ TOTAL (1+ TOTAL)))))
    ;; Got data, now print it out various ways.
    ;; opcode counts, dest counts, address mode counts
    (PRINT-FIELD-COUNTS COUNTS TOTAL "Opcode"
			'(CALL CALL0 MOVE CAR CDR CADR CDDR CDAR
			  CAAR ND1 ND2 ND3 BRANCH MISC OP-16? OP-17?)
			#'(LAMBDA (X) (LDB 0304 X))
			4)
    (PRINT-FIELD-COUNTS COUNTS TOTAL "Extended Opcode"
	     '((CALL 8) (CALL0 8) (MOVE 8) (CAR 8) (CDR 8) (CADR 8) (CDDR 8) (CDAR 8) (CAAR 8)
	       ND1-0? PLUS MINUS TIMES QUOTIENT LOGAND LOGXOR LOGIOR
	       = < > EQ SETE-CDR SETE-CDDR SETE-1+ SETE-1-
	       BIND-OBS? BIND-NIL BIND-POP SET-NIL SET-ZERO PUSH-E MOVEM POP
	       BR BR-NIL BR-NOT-NIL BR-NIL-POP BR-NOT-NIL-POP BR-ATOM BR-NOT-ATOM BR-7?
	       (MISC 8) (OP-16? 8) (OP-17? 8))
	     #'(LAMBDA (X) (DPB (LDB 0304 X) 0304 (LDB 0703 X)))  ;Opcode!dest
	     7)
    (PRINT-FIELD-COUNTS COUNTS TOTAL "Address"
			'(FEF FEF+100 FEF+200 FEF+300 CONSTANT LOCAL ARG PDL-POP)
			#'(LAMBDA (N) (AND (< (LDB 0304 N) 14)	;Not BRANCH and MISC
					   (LOGAND 7 N)))
			3)
    (PRINT-FIELD-COUNTS COUNTS TOTAL "Destination"
			'(D-IGNORE D-PDL D-NEXT D-LAST D-RETURN D-5? D-6? D-NEXT-LIST)
			#'(LAMBDA (N)
			    (LET ((OP (LDB 0304 N)))
			      (AND (NOT (AND (>= OP 11) (< OP 15))) ;Not BRANCH, ND
				   (LDB 0703 N))))
			3)
    ;;-- matrix of combination usage for each opcode (?)
    ;; The 20 most popular combinations (smashes COUNTS a little)
    (LET ((COUNTS2 (MAKE-ARRAY NIL 'ART-Q 2000)) (POPULAR NIL) THRESH)
      ;; For the branch and misc instructions, the register field is ignored here
      (CONGLOMERATE-FIELDS COUNTS 140)
      (CONGLOMERATE-FIELDS COUNTS 150)
      (COPY-ARRAY-CONTENTS COUNTS COUNTS2)
      (SORT COUNTS2 #'>)
      (SETQ THRESH (MAX 1 (AREF COUNTS2 (1- N-POPULAR))))
      (DOTIMES (I 2000)
	(AND (>= (AREF COUNTS I) THRESH) (PUSH (CONS (AREF COUNTS I) I) POPULAR)))
      (SETQ POPULAR (SORTCAR POPULAR #'>))
      (FORMAT T "~&~%The most popular combinations:~%")
      (DOLIST (X POPULAR)
	(FORMAT T "~<~%~:;   ~40<~4,'0O00 (~A)~;~D time~:[s~; ~]~>~>"
		  (CDR X) (DISASS-INSTRUCTION-HIGH10 (CDR X)) (CAR X) (= (CAR X) 1)))
      ;; The 20 most popular MISC instructions
      (SETQ POPULAR NIL)
      (ADJUST-ARRAY-SIZE COUNTS2 1000)
      (COPY-ARRAY-CONTENTS MISC-COUNTS COUNTS2)
      (SORT COUNTS2 #'>)
      (SETQ THRESH (MAX 1 (AREF COUNTS2 (1- N-POPULAR))))
      (DOTIMES (I 1000)
	(AND (>= (AREF MISC-COUNTS I) THRESH) (PUSH (CONS (AREF MISC-COUNTS I) I) POPULAR)))
      (SETQ POPULAR (SORTCAR POPULAR #'>))
      (FORMAT T "~&~%The most popular MISC functions:~%")
      (DOLIST (X POPULAR)
	(FORMAT T "~<~%~:;   ~40<~3,'0O ~A~;~D time~:[s~; ~]~>~>"
		  (CDR X) (DISASS-MISC-INSTRUCTION (CDR X)) (CAR X) (= (CAR X) 1)))
      (RETURN-ARRAY COUNTS2))
    ))

(DEFUN CONGLOMERATE-FIELDS (COUNTS BASE-OP)
  (DO BOP BASE-OP (+ BOP 200) (>= BOP 2000)	;Base of a group with same destination
    (DO OP (1+ BOP) (1+ OP) (= OP (+ BOP 8))	;Add in and zero out equivalent ops
      (ASET (+ (AREF COUNTS OP) (AREF COUNTS BOP)) COUNTS BOP)
      (ASET 0 COUNTS OP))))

(DEFUN DISASS-INSTRUCTION-HIGH10 (N)
  (LET ((DATA
	  '((CALL 8) (CALL0 8) (MOVE 8) (CAR 8) (CDR 8) (CADR 8) (CDDR 8) (CDAR 8) (CAAR 8)
	    ND1-0? PLUS MINUS TIMES QUOTIENT LOGAND LOGXOR LOGIOR
	    = < > EQ SETE-CDR SETE-CDDR SETE-1+ SETE-1-
	    BIND-OBS? BIND-NIL BIND-POP SET-NIL SET-ZERO PUSH-E MOVEM POP
	    BR BR-NIL BR-NOT-NIL BR-NIL-POP BR-NOT-NIL-POP BR-ATOM BR-NOT-ATOM BR-7?
	    (MISC 8) (OP-16? 8) (OP-17? 8)))
	(OP (LDB 0304 N))
	(DEST (LDB 0703 N))
	(REG (LDB 0003 N)))
    (DO ((XOP (DPB OP 0304 DEST))	;Opcode!dest
	 (L DATA (CDR L)))
	(NIL)
      (COND ((ATOM (CAR L))	;Non-destination case
	     (COND ((MINUSP (SETQ XOP (1- XOP)))
		    (RETURN (IF (= OP 14)	;BR
				(STRING (CAR L))
				(FORMAT NIL "~A ~[FEF~;FEF+100~;FEF+200~;FEF+300~;CONSTANT~;LOCAL~;ARG~;PDL-POP~]"
				            (CAR L) REG))))))
	    (T			;Destination case, use up several XOPs
	     (COND ((MINUSP (SETQ XOP (- XOP (CADAR L))))
		    (RETURN (FORMAT NIL "~A ~[D-IGNORE~;D-PDL~;D-NEXT~;D-LAST~;D-RETURN~;D-5?~;D-6?~;D-NEXT-LIST~]~:[ ~[FEF~;FEF+100~;FEF+200~;FEF+300~;CONSTANT~;LOCAL~;ARG~;PDL-POP~]~]"
				        (CAAR L) DEST (= OP 15) REG)))))))))

(DEFUN DISASS-MISC-INSTRUCTION (DISP)
  (COND ((< DISP 100) (FORMAT NIL "LIST ~D long" DISP))
	((< DISP 200) (FORMAT NIL "LIST-IN-AREA ~D long" (- DISP 100)))
	((< DISP 220) (FORMAT NIL "UNBIND ~D bindings" (- DISP 177)))
	((< DISP 240) (FORMAT NIL "POP-PDL ~D times" (- DISP 217)))
	(T
	 (LET ((OP (MICRO-CODE-SYMBOL-NAME-AREA (- DISP 200))))
	   (COND ((NULL OP) (FORMAT NIL "#~O" DISP))
		 (T (FORMAT NIL "~A" OP)))))))

;Entries in SYMBOLIC-NAMES can be a symbol or (symbol repeatcount)
;FCN is a function which given the high 10 bits returns NIL or a translation of it
;for looking up in SYMBOLIC-NAMES
(DEFUN PRINT-FIELD-COUNTS (COUNTS TOTAL FIELDNAME SYMBOLIC-NAMES FCN N-BITS
			   &AUX (SUBCOUNTS (MAKE-ARRAY NIL 'ART-Q (^ 2 N-BITS)))
			        M N (MAXLEN 0) (MAXDIG 1))
  (FILLARRAY SUBCOUNTS '(0))
  (DOTIMES (I 2000)
    (SETQ N (AREF COUNTS I))	;Number of counts
    (COND ((SETQ M (FUNCALL FCN I))	;If applicable, M gets field value to count
	   (ASET (+ (AREF SUBCOUNTS M) N) SUBCOUNTS M))
	  (T (SETQ TOTAL (- TOTAL N)))))
  (FORMAT T "~&~%~A distribution:  " FIELDNAME)
  (COND ((ZEROP TOTAL) (PRINC "none."))
	(T (FORMAT T "(total of ~D)~%" TOTAL)
	   (DOLIST (F SYMBOLIC-NAMES)
	     (SETQ MAXLEN (MAX (STRING-LENGTH (IF (SYMBOLP F) F (CAR F))) MAXLEN)))
	   (DOTIMES (I (ARRAY-LENGTH SUBCOUNTS))
	     (SETQ MAXDIG (MAX (DHAULONG (AREF SUBCOUNTS I)) MAXDIG)))
	   (DO ((I 0 (1+ I))
		(L SYMBOLIC-NAMES (CDR L))
		(NAME))
	       ((NULL L))
	     (SETQ N (AREF SUBCOUNTS I))
	     (COND ((NOT (ATOM (SETQ NAME (CAR L))))
		    (DOTIMES (J (1- (CADR NAME)))
		      (SETQ N (+ (AREF SUBCOUNTS (SETQ I (1+ I))) N)))
		    (SETQ NAME (CAR NAME))))
	     (FORMAT T "~<~%~:;   ~VA ~VD (0.~3,'0D)~>"
		       MAXLEN NAME MAXDIG N (// (* N 1000.) TOTAL)))))
  (RETURN-ARRAY SUBCOUNTS))

(DEFUN DHAULONG (NUM)
  "Number of digits in decimal representation of an integer.
  Plus 1 for the sign if negative.  0 is 1 digit, not 0 digits."
  (DO ((LNG (COND ((MINUSP NUM) (SETQ NUM (- NUM)) 2)
		  (T 1))
	    (+ LNG 6))
       (NUM NUM (// NUM 1000000.)))
      ((< NUM 1000000.)
       (DO ((LNG LNG (1+ LNG))
	    (NUM NUM (// NUM 10.)))
	   ((< NUM 10.) LNG)))))

;Pdl-buffer analysis

(DEFUN PDL-BUFFER-ANALYSIS (FORM &OPTIONAL (BUFFER-SIZES '(128. 256. 384. 512. 640.
							   768. 896. 1024. 1152. 1280.
							   2048. 4096. 8192.)))
  (SETUP-MAGIC-SG-TO-EVAL FORM)
  (LET ((FRAME-SIZE-COUNTS (MAKE-ARRAY NIL 'ART-Q 300.))
	(BUFFER-OVERHEADS (MAKE-ARRAY NIL 'ART-Q (LENGTH BUFFER-SIZES)))
	(BUFFER-N-FRAMES (MAKE-ARRAY NIL 'ART-Q (LENGTH BUFFER-SIZES)))
	(BUFFER-TOTAL-N-FRAMES (MAKE-ARRAY NIL 'ART-Q (LENGTH BUFFER-SIZES)))
	(TOTAL-N-FRAMES-COUNT 0)
	(PDL-FRAME-SIZES NIL)
	(TOTAL-PUSHES 0)
	(TOTAL-POPS 0)
	(AP (SG-AP MAGIC-SG))
	(FRAME-SIZE-NUM 0)
	CUR-AP FRAME-SIZE)
    (FILLARRAY FRAME-SIZE-COUNTS '(0))
    (FILLARRAY BUFFER-OVERHEADS '(0))
    (FILLARRAY BUFFER-N-FRAMES '(0))
    (FILLARRAY BUFFER-TOTAL-N-FRAMES '(0))
    (WITHOUT-INTERRUPTS ;in case other SG does WITHOUT-INTERRUPTS and needs it
      (DO () (NIL)
	(AND (EQ (SINGLE-STEP-SG MAGIC-SG) 'EXHAUSTED) (RETURN))
	(SETQ CUR-AP AP)
	(SETQ AP (SG-AP MAGIC-SG))
	(COND (( AP CUR-AP)		;If have pushed or popped
	       (SETQ TOTAL-N-FRAMES-COUNT (1+ TOTAL-N-FRAMES-COUNT))
	       (COND ((> (- AP CUR-AP) 1000)	;Wrap-around
		      (SETQ CUR-AP (+ CUR-AP 2000)))
		     ((< (- AP CUR-AP) -1000)
		      (SETQ CUR-AP (- CUR-AP 2000))))
	       (COND ((> AP CUR-AP)
		      (SETQ FRAME-SIZE (- AP CUR-AP))
		      (SETQ TOTAL-PUSHES (+ TOTAL-PUSHES FRAME-SIZE))
		      (ASET (1+ (AREF FRAME-SIZE-COUNTS FRAME-SIZE))
			    FRAME-SIZE-COUNTS FRAME-SIZE)
		      (SETQ FRAME-SIZE-NUM (1+ FRAME-SIZE-NUM))
		      (PUSH FRAME-SIZE PDL-FRAME-SIZES)
		      (LOOP FOR BUFFER-SIZE IN BUFFER-SIZES AND I UPFROM 0
			    AS N-FRAMES = (1+ (AREF BUFFER-N-FRAMES I))
			    DO (ASET N-FRAMES BUFFER-N-FRAMES I)
			       (LOOP FOR SZ IN PDL-FRAME-SIZES AND NF FROM 0 BELOW N-FRAMES
				     SUM SZ INTO CUR-FILL
				     WHEN (> CUR-FILL BUFFER-SIZE)
				       DO (ASET (+ (LOOP FOR SZ IN (NTHCDR NF PDL-FRAME-SIZES)
							 FOR N FROM NF BELOW N-FRAMES
							 SUM SZ)
						   (AREF BUFFER-OVERHEADS I))
						BUFFER-OVERHEADS I)
				          (SETQ N-FRAMES NF)
					  (ASET NF BUFFER-N-FRAMES I)
					  (RETURN))
			       (ASET (+ (AREF BUFFER-TOTAL-N-FRAMES I) N-FRAMES)
				     BUFFER-TOTAL-N-FRAMES I)))
		     (T ;; Popped one or more frames
		        (SETQ FRAME-SIZE (- CUR-AP AP))
			(SETQ TOTAL-POPS (+ TOTAL-POPS FRAME-SIZE))
			(LOOP FOR N-POPPED UPFROM 1
			      WHILE (NOT (NULL PDL-FRAME-SIZES))
			      AS LASTSIZE = (POP PDL-FRAME-SIZES)
			      DO (SETQ FRAME-SIZE (- FRAME-SIZE LASTSIZE))
			      WHILE (PLUSP FRAME-SIZE)
			      FINALLY (COND ((MINUSP FRAME-SIZE)
					     ;; Function called to D-LAST, that last frame
					     ;; wasn't popped, instead it called back out
					     ;; having gotten probably 4 words smaller
					     (SETQ N-POPPED (1- N-POPPED))
					     (PUSH (MINUS FRAME-SIZE) PDL-FRAME-SIZES)))
				      (LOOP FOR BUFFER-SIZE IN BUFFER-SIZES AND I UPFROM 0
					 WHEN (< (ASET (- (AREF BUFFER-N-FRAMES I) N-POPPED)
							  BUFFER-N-FRAMES I)
						 1)
					   DO (ASET 1 BUFFER-N-FRAMES I)
					      (ASET (+ (OR (CAR PDL-FRAME-SIZES) 0)
						       (AREF BUFFER-OVERHEADS I))
						    BUFFER-OVERHEADS I)
					 DO (ASET (+ (AREF BUFFER-N-FRAMES I)
						     (AREF BUFFER-TOTAL-N-FRAMES I))
						  BUFFER-TOTAL-N-FRAMES I)))))))))
    ;; Got data, print it out
    (FORMAT T "~&Note that this does not include frames that never call out.")
    (FORMAT T "~&Total words pushed ~D, total words popped ~D, total calls//returns ~D"
	      TOTAL-PUSHES TOTAL-POPS TOTAL-N-FRAMES-COUNT)
    (FORMAT T "~&~22A~26A~12A" "Usable Buffer size" "Avg # frames in buffer" "Overhead")
    (LOOP FOR BUFFER-SIZE IN BUFFER-SIZES AND I UPFROM 0
	  DO (FORMAT T "~&~18D~4X~22D~4X~8D"
		       BUFFER-SIZE
		       (// (AREF BUFFER-TOTAL-N-FRAMES I) TOTAL-N-FRAMES-COUNT)
		       (AREF BUFFER-OVERHEADS I)))
    (FORMAT T "~&Frame size cumulative distribution (per mille):~%~5X")
    (LOOP FOR S FROM 0 TO 19. DO (FORMAT T "~4D" S))
    (*CATCH 'DONE
      (LOOP WITH CUM = 0
	    FOR SZ FROM 0 BY 20. BELOW 300. 
	    DO (FORMAT T "~&~3D: " SZ)
	       (LOOP FOR S FROM SZ BY 1 TO (+ SZ 19.)
		     DO (SETQ CUM (+ (AREF FRAME-SIZE-COUNTS S) CUM))
			(IF (< CUM FRAME-SIZE-NUM)
			    (FORMAT T "~4D" (// (* CUM 1000.) FRAME-SIZE-NUM))
			    (FORMAT T " 1000")
			    (*THROW 'DONE NIL)))))))
