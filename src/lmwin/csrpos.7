;-*- Mode:LISP; Package:TV; Base:8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;For Maclisp compatibility
;No arguments returns (line . column)
;One argument does magic functions
;Two arguments sets cursorpos to there (args there of NIL mean don't change)
;Returns T if it succeeded, NIL if it didn't.
;Hmm, NEWIO seems to blow out rather than returning NIL now.  Change this?

;If the first argument is T (meaning TERMINAL-IO)
;or a stream, then it is applied to that stream.  Otherwise it is applied
;to STANDARD-OUTPUT.  Anything other than a number or a 1-character long
;symbol or NIL is assumed to be a stream.

(DEFUN CURSORPOS (&REST ARGS)
  (LET ((ARG1 (CAR ARGS))
	(STREAM STANDARD-OUTPUT)
	WO)
    (COND ((NULL ARGS))			;If any args, look for stream as 1st arg
	  ((NULL ARG1))
	  ((EQ ARG1 T) (SETQ STREAM TERMINAL-IO ARGS (CDR ARGS)))
	  ((NUMBERP ARG1))
	  ((OR (NOT (SYMBOLP ARG1)) (> (ARRAY-LENGTH (GET-PNAME ARG1)) 1))
	   (SETQ STREAM ARG1 ARGS (CDR ARGS))))
    (SETQ ARG1 (CAR ARGS)
	  WO (FUNCALL STREAM ':WHICH-OPERATIONS))
    (COND ((NULL ARGS)
	   (IF (MEMQ ':READ-CURSORPOS WO)
	       (MULTIPLE-VALUE-BIND (X Y) (FUNCALL STREAM ':READ-CURSORPOS ':CHARACTER)
		 (CONS Y X))
	       (FERROR NIL "~S stream does not support cursor positioning" STREAM)))
	  ((CDDR ARGS)
	   (FERROR NIL "Too many arguments"))	;Why bother signalling the correct condition?
	  ((OR (CDR ARGS) (NUMBERP ARG1))	;2 arguments or one numeric argument
	   (IF (MEMQ ':SET-CURSORPOS WO)
	       (MULTIPLE-VALUE-BIND (X Y) (FUNCALL STREAM ':READ-CURSORPOS ':CHARACTER)
		 (FUNCALL STREAM ':SET-CURSORPOS
			  (OR (SECOND ARGS) X) (OR (FIRST ARGS) Y) ':CHARACTER))
	       (FERROR NIL "~S stream does not support cursor positioning" STREAM)))
	  ((= (SETQ ARG1 (CHAR-UPCASE (AREF (GET-PNAME ARG1) 0))) #/F)
	   (CURSORPOS-INTERNAL STREAM +1 0 #\SP WO))	;F forward space
	  ((= ARG1 #/B)				;B backspace
	   (CURSORPOS-INTERNAL STREAM -1 0 #\BS WO))
	  ((= ARG1 #/D)				;D down a line
	   (CURSORPOS-INTERNAL STREAM 0 +1 NIL WO))
	  ((= ARG1 #/U)				;U up a line
	   (CURSORPOS-INTERNAL STREAM 0 -1 NIL WO))
	  ((= ARG1 #/C)				;C clear screen
	   (IF (MEMQ ':CLEAR-SCREEN WO) (FUNCALL STREAM ':CLEAR-SCREEN)
	       (FUNCALL STREAM ':TYO #\FORM))
	   T)
	  ((= ARG1 #/T)				;T top of screen
	   (IF (MEMQ ':HOME-CURSOR WO) (FUNCALL STREAM ':HOME-CURSOR)
	       (FUNCALL STREAM ':TYO #\FORM))
	   T)
	  ((= ARG1 #/E)				;E erase to end of screen
	   (COND ((MEMQ ':CLEAR-EOF WO) (FUNCALL STREAM ':CLEAR-EOF) T)))
	  ((= ARG1 #/L)				;L erase to end of line
	   (COND ((MEMQ ':CLEAR-EOL WO) (FUNCALL STREAM ':CLEAR-EOL) T)))
	  ((= ARG1 #/K)				;K erase character
	   (COND ((MEMQ ':CLEAR-CHAR WO) (FUNCALL STREAM ':CLEAR-CHAR) T)))
	  ((= ARG1 #/X)				;X erase character backward
	   (CURSORPOS STREAM 'B)
	   (CURSORPOS STREAM 'K))
	  ((= ARG1 #/Z)				;Z home down
	   (IF (MEMQ ':HOME-DOWN WO) (FUNCALL STREAM ':HOME-DOWN)
	       (FUNCALL STREAM ':FRESH-LINE))
	   T)
	  ((= ARG1 #/A)				;A fresh line
	   (FUNCALL STREAM ':FRESH-LINE)
	   T)
	  ((FERROR NIL "~C not a recognized CURSORPOS option" ARG1)))))

(DEFUN CURSORPOS-INTERNAL (STREAM DX DY ALTERNATE-CHARACTER WO)
  (COND ((MEMQ ':SET-CURSORPOS WO)
	 (MULTIPLE-VALUE-BIND (X Y) (FUNCALL STREAM ':READ-CURSORPOS ':CHARACTER)
	   (FUNCALL STREAM ':SET-CURSORPOS (+ X DX) (+ Y DY) ':CHARACTER)
	   T))
	((NOT (NULL ALTERNATE-CHARACTER))
	 (FUNCALL STREAM ':TYO ALTERNATE-CHARACTER)
	 T)
	(T NIL)))	;Or should it give an error?
