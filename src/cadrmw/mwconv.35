; -*- Mode:Lisp; Package:User; -*-

(OR (FBOUNDP 'LOOP) (LOAD "moon;loop"))

(DEFSTRUCT (PIN :LIST)
   PIN-TYPE					;DIP, DEC, CAP, XY
   PIN-ROW					;Row letter, or X
   PIN-COLUMN					;Column number, or Y
   PIN-POST					;Post within DIP socket
)

(DEFVAR HASH-LENGTH 77)
(DEFVAR POINTS (MAKE-ARRAY NIL 'ART-Q HASH-LENGTH))

(DEFMACRO IS-NUMBER (X)
  `(AND ( ,X #/0) ( ,X #/9)))

(DEFVAR SCALE 32.)

(DEFUN WL-READ-LINE (&OPTIONAL (STREAM STANDARD-INPUT) &AUX STR POS EOF)
    (MULTIPLE-VALUE (STR EOF) (FUNCALL STREAM ':LINE-IN NIL))
    (OR EOF (IF (SETQ POS (STRING-SEARCH-CHAR #/\ STR))
		(DO ((POS (1+ POS)) (ANS))
		    ((NULL POS) (NREVERSE ANS))
		  (PUSH (NSUBSTRING STR (1+ POS)
				    (IF (SETQ POS (STRING-SEARCH-CHAR #/, STR (1+ POS)))
					POS
					(STRING-LENGTH STR)))
			ANS))
		(FORMAT T "~&;Flushing ~A" STR))))

;XY = 'ROUTE to force routing, T for just XY points
(DEFUN CONVERT-XANADU (FILE &OPTIONAL OUT-FILE XY SUPPRESS-OUTPUT)
   (ZWEI:OPEN-FILE (IFILE (FILE-DEFAULT-FN2 FILE "XAN") ':IN)
       (ZWEI:OPEN-FILE (OFILE (FILE-DEFAULT-FN2
			       (OR OUT-FILE (FILE-SET-FN2 FILE "MWTAPE")) "MWTAPE")
			      ':OUT)
	  ;;WRITE FAKE TAPE LABEL
	  ()
	  ;;
	  (DOTIMES (I HASH-LENGTH) (ASET NIL POINTS I))
	  (DO ((LINE (WL-READ-LINE IFILE) (WL-READ-LINE IFILE))
	       (RUN-NUMBER 1 (1+ RUN-NUMBER)))
	      ((EQ LINE T))
	    (COND (LINE
		    (SETQ LINE (MAPCAR #'CONVERT-PIN LINE))
		    (IF XY (SETQ LINE (STITCH LINE XY)))       ;Convert to numeric coordinates
		    (AND SCALE (STITCH-DISPLAY-LINE LINE))
		    (LOOP FOR ITEM IN LINE
			  WITH TEM = NIL AND X = NIL AND Y = NIL AND XY = NIL
			  DO (MULTIPLE-VALUE (X Y) (MAP-XY ITEM))
			     (SETQ XY (MILS X Y))
			     (LET ((HASH (\ (SECOND XY) HASH-LENGTH)))
			       (COND ((SETQ TEM (MEM #'(LAMBDA (X Y) (EQUAL X (CAR Y)))
						     XY (AREF POINTS HASH)))
				      (FORMAT T "~&Points duplicated - ~A, ~A"
					      (CONVERT-PIN-TO-STRING ITEM)
					      (CONVERT-PIN-TO-STRING (CDAR TEM))))
				     (T (PUSH (CONS XY ITEM) (AREF POINTS HASH))))))
		    (DO ((COLUMN-LEFT 74. 74.))
			((OR SUPPRESS-OUTPUT (NULL LINE)))
		      (LET ((NUM (FORMAT NIL "~D" RUN-NUMBER)))
			(FUNCALL OFILE ':STRING-OUT NUM)
			(DOTIMES (I (- 7 (STRING-LENGTH NUM)))
			  (FUNCALL OFILE ':TYO #\SPACE)))
		      (LOOP UNTIL (NULL LINE)
			    FOR STRING = (CONVERT-PIN-TO-STRING (CAR LINE))
			    FOR LENGTH = (STRING-LENGTH STRING)
			    UNTIL ( COLUMN-LEFT LENGTH)
			    AS ITEM = (POP LINE)
			    DO (FUNCALL OFILE ':STRING-OUT STRING)
			       (FUNCALL OFILE ':TYO #\SPACE)
			       (SETQ COLUMN-LEFT (- COLUMN-LEFT LENGTH 1)))
		      (DOTIMES (I (1- COLUMN-LEFT))
			(FUNCALL OFILE ':TYO #\SPACE))
		      (FUNCALL OFILE ':TYO #\CR))))))))

;Pin formats understood:
;   L#-#	- DIP pin number, (Row,Column,Post)
;   L#C#	- CAP pin number, post should be 1
;   LL#		- DEC connector number (Paddle,Conn,Side)

(DEFUN CONVERT-PIN (PIN &AUX COL POST I)
  (COND ((IS-NUMBER (AREF PIN 1))			;DIP? L#
	 (MULTIPLE-VALUE (COL I) (STRING-NUMBER PIN 1))
	 (MULTIPLE-VALUE (POST NIL) (STRING-NUMBER PIN (1+ I)))
	 (LIST (IF (= (AREF PIN I) #/C) 'CAP 'DIP) (AREF PIN 0) COL POST))
	((IS-NUMBER (AREF PIN 0)) (FERROR T "~&CRUFTY PIN - ~A" PIN))
	(T (LIST 'DEC (AREF PIN 0) (AREF PIN 1) (STRING-NUMBER PIN 2)))))

(DEFUN CONVERT-PIN-TO-STRING (PIN)
  (SELECTQ (PIN-TYPE PIN)
    (:XY (FORMAT NIL "~D ~D" (SECOND PIN)
		(THIRD PIN)))
    (:DIP (FORMAT NIL "~D~C~D"
		  (PIN-COLUMN PIN)
		  (PIN-ROW PIN)
		  (PIN-POST PIN)))
    (:CAP (FORMAT NIL "~D~C~DC"
		  (PIN-COLUMN PIN)
		  (PIN-ROW PIN)
		  (PIN-POST PIN)))
    (:DEC (FORMAT NIL "~C~C~D" (SECOND PIN) (THIRD PIN) (FOURTH PIN)))))

(DEFUN STITCH-DISPLAY-LINE (LINE &AUX X Y X1 Y1)
  (LOOP INITIALLY (MULTIPLE-VALUE (X Y) (MAP-XY (CAR LINE)))
	FOR PT IN (CDR LINE)
	DO (MULTIPLE-VALUE (X1 Y1) (MAP-XY PT))
	(STITCH-DISPLAY X Y X1 Y1)
	(SETQ X X1 Y Y1)))


(DEFUN STITCH-DISPLAY (X Y X1 Y1)
  (AND X Y
       (LET ((X (+ 100 (FIXR (* X SCALE))))
	     (Y (FIXR (* (+ .5 Y) SCALE)))
	     (X1 (+ 100 (FIXR (* X1 SCALE))))
	     (Y1 (FIXR (* (+ .5 Y1) SCALE))))
	 (AND (OR (< X 0) (< X1 0) (> X 1400) (> X1 1400)
		  (< Y 0) (< Y1 0) (> Y 1600) (> Y1 1600))
	     (FERROR T "~&Points off-screen ~o,~o ~o,~o" X Y X1 Y1))
	 (OR (AND (= X X1) (= Y Y1)) (TV-DRAW-LINE Y X Y1 X1 TV-ALU-IOR TV-DEFAULT-SCREEN)))))

(DEFMACRO STITCH-WIRE (&REST PATH)
  `(STITCH-WIRE-1 ,@(MAPCAN #'(LAMBDA (X) `(',(CAR X) ,@(CDR X))) PATH)))

(DEFMACRO APPROX= (X Y)
  `(< (ABS (- ,X ,Y)) .01))

(DEFUN STITCH-WIRE-1 (&REST PATH &AUX X Y)
  (DO ((ANS))
      ((NULL PATH) (NREVERSE ANS))
    (SELECTQ (CAR PATH)
      (XY (PUSH (MILS (SETQ X (SECOND PATH)) (SETQ Y (THIRD PATH))) ANS)
	  (SETQ PATH (REST3 PATH)))
      (X (OR (APPROX= X (SECOND PATH)) (PUSH (MILS (SETQ X (SECOND PATH)) Y) ANS))
	 (SETQ PATH (REST2 PATH)))
      (Y (OR (APPROX= Y (SECOND PATH))(PUSH (MILS X (SETQ Y (SECOND PATH))) ANS))
	 (SETQ PATH (REST2 PATH))))))

(DEFUN STITCH (LINE MODE &AUX X Y X1 Y1 ANS)
  (MULTIPLE-VALUE (X Y) (MAP-XY (CAR LINE)))
  (PUSH (MILS X Y) ANS)
  (LOOP FOR POINT IN (CDR LINE)
	DO (MULTIPLE-VALUE (X1 Y1) (MAP-XY POINT))
	   (SETQ ANS (NCONC ANS (IF (EQ MODE 'ROUTE)
				    (STITCH-1 X Y X1 Y1)
				    (LIST (MILS X Y)))))
	   (SETQ X X1 Y Y1))
  ANS)

(DEFUN STITCH-1 (X Y X1 Y1)
  (COND ((APPROX= Y Y1)
	 (IF (APPROX= (ABS (- X X1)) 0.1)
	     (STITCH-WIRE (XY X1 Y1))		;Only .1 horiz
	     (LET ((DIR (IF (> X1 X) 0.05 -0.05)))
	       (STITCH-WIRE (XY (+ X DIR) (- Y 0.05))
			    (X (- X1 DIR))
			    (XY X1 Y1)))))
	((Approx= X X1)
	 (IF (APPROX= (ABS (- Y Y1)) 0.1)
	     (STITCH-WIRE (XY X1 Y1))
	     (LET ((DIR (IF (> Y1 Y) 0.05 -0.05)))
	       (STITCH-WIRE (XY (+ X 0.05) (+ Y DIR))
			    (Y (- Y1 DIR))
			    (XY X1 Y1)))))
	((AND (APPROX= (ABS (- X X1)) 0.1) (APPROX= (ABS (- Y Y1)) 0.1))
	 (STITCH-WIRE (XY X1 Y1)))
	((AND (< X X1) (< Y Y1))		;+X, +Y
	 (STITCH-WIRE (XY (+ X 0.05) (+ Y 0.05))
		      (Y (- Y1 0.05))
		      (X (- X1 0.05))
		      (XY X1 Y1)))
	((< X X1)				;+X, -Y
	 (STITCH-WIRE (XY (+ X 0.05) (- Y 0.05))
		      (Y (+ Y1 0.05))
		      (X (- X1 0.05))
		      (XY X1 Y1)))
	((< Y Y1)				;-X, +Y
	 (STITCH-WIRE (XY (- X 0.05) (+ Y 0.05))
		      (X (+ X1 0.05))
		      (Y (- Y1 0.05))
		      (XY X1 Y1)))
	(T (STITCH-WIRE (XY (- X 0.05) (- Y 0.05))	;-X, -Y
			(X (+ X1 0.05))
			(Y (+ Y1 0.05))
			(XY X1 Y1)))))

(DEFUN MILS (X Y)
  (LIST 'XY (FIXR (* X 1000.)) (FIXR (* Y 1000.))))


(DEFUN STRING-NUMBER (STR &OPTIONAL (I 0))
  (LOOP WITH VAL = 0 AND LENGTH = (STRING-LENGTH STR)
	UNTIL ( I LENGTH)
	AS CH = (AREF STR I)
	UNTIL (OR (< CH #/0) (> CH #/9))
	DO (SETQ VAL (+ (* VAL 10.) (- CH #/0)))
	(SETQ I (1+ I))
	FINALLY (RETURN VAL I)))

;;Accepts either #,# pair (if NUMERIC-P), #L# for DIP pin, LL# for DEC connector pin
(DEFUN MAP-XY (PIN)
  (PROG (COLUMN ROW POST)
	(SELECTQ (PIN-TYPE PIN)
	  (:XY (RETURN (// (SECOND PIN) 1000s0) (// (THIRD PIN) 1000s0)))
	  (:DIP (SETQ COLUMN (PIN-COLUMN PIN)
		      POST (PIN-POST PIN)
		      ROW (FIND-POSITION-IN-LIST (CHAR-UPCASE (PIN-ROW PIN))
						'(#/J #/H #/F #/E #/D #/C #/B #/A)))
	       (COND ((OR ( COLUMN 4) (= ROW 0))	;20 pin DIP area
		      (COND ((= ROW 1) (FORMAT T "~&Illegal pin, ~A" PIN))
			    (( ROW 1) (SETQ ROW (1- ROW))))	;flush non-existent H row
		      (RETURN (+ (* (1- COLUMN) 0.5) (IF (> POST 10.) 0.3 0))
			      (+ (* 1.2 ROW) (IF (> POST 10.)
						 (* 0.1 (- POST 11.))
						 (* 0.1 (- 10. POST))))))
		     (T (LET ((X (* (1- COLUMN) 0.5))
			      (Y (+ 1.6 (* 0.9 (1- ROW)))))
			  (COND (( POST 8)
				 (RETURN X (+ Y (* 0.1 (- 8 POST)))))
				(T (RETURN (+ X 0.3)
					   (+ Y (* 0.1 (- POST 9))))))))))
	  (:CAP (SETQ COLUMN (PIN-COLUMN PIN)
		      POST (PIN-POST PIN)
		      ROW (FIND-POSITION-IN-LIST (CHAR-UPCASE (PIN-ROW PIN))
						'(#/J #/H #/F #/E #/D #/C #/B #/A)))
		(COND ((OR (< COLUMN 5) (> COLUMN 29.) ( POST 1)
			   (NOT (MEMQ (PIN-ROW PIN) '(#/A #/B #/C #/D #/E #/F #/H))))
		       (FORMAT T "~&Illegal capacitor pin - ~A" PIN))
		      (T (LET ((X (* (1- COLUMN) 0.5))
			      (Y (+ 1.6 (* 0.9 (1- ROW)))))			
			   (RETURN (+ X 0.4)
				   (+ Y 0.7))))))
	  (:DEC (RETURN (+ (* 2.5 (- #/F (CHAR-UPCASE (SECOND PIN))))
			   (* 0.1 (- 18. (FIND-POSITION-IN-LIST (CHAR-UPCASE (THIRD PIN))
					      '(#/A #/B #/C #/D #/E #/F #/H #/J
						#/K #/L #/M #/N
						#/P #/R #/S #/T #/U #/V)))))
			(COND ((= (FOURTH PIN) 1) -.3)
			      ((=  (FOURTH PIN) 2) -.5)
			      (T (FERROR T "~&Illegal format DEC connector - ~A" PIN))))))))
