;;; -*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(DECLARE (ERROR '|If you are compiling this, and not using RTC, you are losing|))

(MAC  NUMBER '(// #/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
      PLUS-MINUS '(// #/+ #/-)
      PLUS #/+
      POINT #/.
      EE '(// #/E #/e)
      SS '(// #/S #/s)
      LSH-SCALE '(// #/_ #/^)
      SHARP-SIGN #/#
      OPEN #/(
      CLOSE #/)
      STRING-START-CHAR #/"
      QUOTED-SYMBOL-START-CHAR #/|
      SEMICOLON #/;
      COLON #/:
      CRLF #\CR
      NULL '(//)
      ALPHA-BETA-EPSILON '(// #/ #/ #/ #/ #/ˆ)
      SLASHIFIED-CHAR -1				;SLASHIFIED CHARS TO BE MAPED TO -1.
      EOF-CHAR -2					;EOF IS MAPPED TO -2.
      WHITE-SPACE-CHAR '(// #\SP 211 212 213 214 215 -5)	;-5 IS WHITESPACE SYNTAX
      BREAK (NCONC '(// #/( #/) #/' #/` #/, #/" #/| #/; -2 -3 -4 -6)	;-3 IS BREAK SYNTAX
		   (CDR WHITE-SPACE-CHAR))
      MACRO-CHARACTER '(// #/' #/, #/; #/` -6)		;-6 IS MACRO SYNTAX
      STANDALONE-CHAR '(// -4)				;-4 IS SINGLE SYNTAX
      ANY (CONS '// (DO ((I -7 (1+ I))			;-7 IS ALPHABETIC SYNTAX
			 (X NIL (CONS I X)))
			((> I 215) X)))
      ANY-BUT-EOF (DELETE -2 ANY))

;;; A readtable definition looks like (DEF name regular-expression type).
;;; "name" is the name of the kind of token.  It has a function to process the
;;;   string, on its property list, OR it is a symbol to be returned.
;;; "regular-expression" is a regular expression.
;;; "type" is a symbol indicating what to do with the last character
;;; recognized by the regular expression.

(DEF STRING
     STRING-START-CHAR
     LAST-CHAR)

(DEF QUOTED-SYMBOL
     QUOTED-SYMBOL-START-CHAR
     LAST-CHAR)

(DEF FIXNUM
     (! (U NULL PLUS-MINUS)
	(+ NUMBER)
	(U NULL POINT)
	(U NULL
	   (! LSH-SCALE
	      (U NULL PLUS)
	      (+ NUMBER)
	      (U NULL POINT)))
	BREAK)
     UNTYI-FUNCTION)

(DEF FLONUM
     (! (U (! (U NULL PLUS-MINUS)
	      (* NUMBER)
	      POINT
	      (+ NUMBER)
	      (U NULL
		 (! EE
		    (U NULL PLUS-MINUS)
		    (+ NUMBER))))
	   (! (U NULL PLUS-MINUS)
	      (+ NUMBER)
	      (U NULL POINT)
	      EE
	      (U NULL PLUS-MINUS)
	      (+ NUMBER)))
	BREAK)
     UNTYI-FUNCTION)

(DEF SMALL-FLONUM
     (! (U NULL PLUS-MINUS)
	(U (! (+ NUMBER)
	      (U NULL POINT))
	   (! (* NUMBER)
	      POINT
	      (+ NUMBER)))
	SS
	(U NULL PLUS-MINUS)
	(+ NUMBER)
	BREAK)
     UNTYI-FUNCTION)

(DEF SHARP-SLASH
     (! SHARP-SIGN
	(U NULL (+ ALPHA-BETA-EPSILON) (+ NUMBER))
	SLASHIFIED-CHAR)
     LAST-CHAR)

(DEF SHARP-THING
     (! SHARP-SIGN
	(U NULL (+ ALPHA-BETA-EPSILON) (+ NUMBER))
	(- ANY-BUT-EOF (U ALPHA-BETA-EPSILON NUMBER)))
     LAST-CHAR)

(DEF LIST OPEN LAST-CHAR)

(DEF CONSING-DOT
     (! POINT BREAK)
     UNTYI-QUOTE)

(DEF CLOSE CLOSE NO-UNTYI-QUOTE)

(DEF EOF EOF-CHAR NO-UNTYI-QUOTE)

(DEF MACRO-CHAR
     MACRO-CHARACTER
     LAST-CHAR)

(DEF SC-SYMBOL
     STANDALONE-CHAR
     NO-UNTYI-FUNCTION)

(DEF PACKAGE-PREFIX
     (! (* (- ANY-BUT-EOF (U COLON BREAK)))
	COLON)
     LAST-CHAR)

(DEF SYMBOL
     (! (+ (- ANY-BUT-EOF BREAK))
	BREAK)
     UNTYI-FUNCTION)

(OPT WHITE-SPACE-CHAR (CDR WHITE-SPACE-CHAR))			;Options to RTC
(OPT MACRO-ALIST '((#/' . XR-QUOTE-MACRO)
		   (#/; . XR-COMMENT-MACRO)
		   (#/` . XR-BACKQUOTE-MACRO)
		   (#/, . XR-COMMA-MACRO)))
(OPT /#-MACRO-ALIST '((#/' . XR-/#/'-MACRO)
		      (#/\ . XR-/#/\-MACRO)
		      (#/^ . XR-/#/^-MACRO)
		      (#/, . XR-/#/,-MACRO)
		      (#/. . XR-/#/.-MACRO)
		      (#/Q . XR-/#/Q-MACRO)
		      (#/M . XR-/#/M-MACRO)
		      (#/N . XR-/#/N-MACRO)
		      (#/+ . XR-/#/+-MACRO)
		      (#/- . XR-/#/--MACRO)
		      (#/O . XR-/#/O-MACRO)
		      (#/R . XR-/#/R-MACRO)
		      (#/X . XR-/#/X-MACRO)))
(OPT READ-FUNCTION-PROPERTY 'STANDARD-READ-FUNCTION)
(OPT SLASH #//)
(OPT CIRCLECROSS 26)
(OPT SLASHIFIED-CHAR SLASHIFIED-CHAR)
(OPT EOF-CHAR EOF-CHAR)
(OPT A-BREAK-CHAR -3)					;For the reader to use.
(OPT MAKE-SYMBOL '(SC-SYMBOL))				;Who makes symbols
(OPT MAKE-SYMBOL-BUT-LAST '(SYMBOL			;and how.
			    SLASHED-SYMBOL))
(OPT BITS '((#/" 10)					;Bits to be ored into readtable.
	    (#/| 20)))
(OPT SAVE-SYNTAX '(SINGLE -4				;Placed in plist of readtable
		   SLASH #//				; with syntax bits replacing
		   CIRCLECROSS 26			; character numbers.
		   WHITESPACE -5
		   MACRO -6
		   BREAK -3
		   ALPHABETIC -7
		   DOUBLEQUOTE #/"
		   VERTICALBAR #/|
		   ))
(OPT TRANSLATIONS '(((#/a  #/z)  (#/A  #/Z))))		;Translations may be pairs of
							;intervals (inclusive) or just chars

(END READTABLE)						;The symbol whose value cell will
							;be loaded with the readtable
