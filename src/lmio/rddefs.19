;;; Definitions for the -*-LISP-*- machine reader

;;; WARNING! if you change anything in this file be sure and look in
;;; LMIO;RTCMAC > as well. Not only might you have to recompile READ and PRINT,
;;; but you may have to recompile LMIO;RTC > as well (using QC and QCMP), and you may have
;;; to recompile LMIO;RDTBL > (using RTC).
;;; Sorry, Alan, this defstruct needs colons which means it probably won't work in Maclisp

(DEFSTRUCT (RDTBL :ARRAY-LEADER
                  (:CONSTRUCTOR MAKE-RDTBL)
		  (MAKE-ARRAY (NIL 'ART-16B (RDTBL-ARRAY-DIMS)))
		  (:DEFAULT-POINTER RDTBL)
		  (:SIZE-MACRO RDTBL-SIZE))
	   RDTBL-FSM					;sacred
	   RDTBL-NAMED-STRUCTURE-SYMBOL
	   RDTBL-N-STATES
	   RDTBL-N-BUCKETS
	   RDTBL-STARTING-STATE
	   RDTBL-SLASH-CODE
	   RDTBL-EOF-CODE
	   RDTBL-BREAK-CODE
	   RDTBL-MACRO-ALIST
	   RDTBL-READ-FUNCTION-PROPERTY
	   RDTBL-PLIST
           RDTBL-DEFINITION
	   RDTBL-MAKE-SYMBOL
	   RDTBL-MAKE-SYMBOL-BUT-LAST
           RDTBL-SLASH
           RDTBL-WHITESPACE
           RDTBL-CIRCLECROSS
	   (PTTBL-SPACE			40	)
	   (PTTBL-NEWLINE		215	)
	   (PTTBL-CONS-DOT 		" . "	)
	   (PTTBL-MINUS-SIGN 		#/-	)
	   (PTTBL-DECIMAL-POINT 	#/.	)
	   (PTTBL-SLASH 		#//	)
	   (PTTBL-PRINLEVEL 		"**"	)
	   (PTTBL-PRINLENGTH 		" ...)"	)
	   (PTTBL-OPEN-RANDOM 		"#<"	)
	   (PTTBL-CLOSE-RANDOM 		">"	)
	   (PTTBL-OPEN-PAREN 		#/(	)
	   (PTTBL-CLOSE-PAREN 		#/)	)
	   (PTTBL-OPEN-QUOTE-STRING 	#/"	)
	   (PTTBL-CLOSE-QUOTE-STRING	#/"	)
	   (PTTBL-OPEN-QUOTE-SYMBOL 	#/|	)
	   (PTTBL-CLOSE-QUOTE-SYMBOL 	#/|	)
	   (PTTBL-PACKAGE-CHAR 		#/:	)
	   RDTBL-/#-MACRO-ALIST
	   )

(DEFVAR RDTBL-ARRAY-SIZE 240)

(DEFMACRO RDTBL-ARRAY (&OPTIONAL (P 'RDTBL))
	  P)

(DEFMACRO RDTBL-ARRAY-DIMS ()
	  `',(LIST 3 RDTBL-ARRAY-SIZE))

(DEFMACRO RDTBL-BITS (RDTBL CHAR)
	    `(AR-2 ,RDTBL 0 ,CHAR))

(DEFMACRO RDTBL-CODE (RDTBL CHAR)
	    `(AR-2 ,RDTBL 1 ,CHAR))

(DEFMACRO RDTBL-TRANS (RDTBL CHAR)
	    `(AR-2 ,RDTBL 2 ,CHAR))

;;; Names of special characters, as an a-list.  FORMAT searches this list to
;;; get the inverse mapping (numbers to names), so the preferred name for a value
;;; should be earliest in the list.  New-keyboard names are preferred.
;;; This variable is used by quite a few other programs as well, even
;;; though it may look like it is internal to READ.
;;; Here rather than in READ, because this expression cannot be evaluated
;;; in the cold-load.

(DEFVAR XR-SPECIAL-CHARACTER-NAMES
      (APPEND '( (:NULL . 200)
		 (:BREAK . 201) (:BRK . 201)
		 (:CLEAR-INPUT . 202) (:CLEAR . 202) (:CLR . 202)
		 (:CALL . 203)
		 (:TERMINAL . 204) (:ESC . 204) (:ESCAPE . 204)
		 (:MACRO . 205) (:BACK-NEXT . 205) 
		 (:HELP . 206)
		 (:RUBOUT . 207)
		 (:OVERSTRIKE . 210) (:BACKSPACE . 210) (:BS . 210)
		 (:TAB . 211)
		 (:LINE . 212) (:LF . 212) (:LINEFEED . 212)
		 (:DELETE . 213) (:VT . 213)
		 (:CLEAR-SCREEN . 214) (:FORM . 214) (:FF . 214)
		 (:RETURN . 215) (:CR . 215) (:NEWLINE . 215)
		 (:QUOTE . 216)
		 (:HOLD-OUTPUT . 217)
		 (:STOP-OUTPUT . 220)
		 (:ABORT . 221)
		 (:RESUME . 222)
		 (:STATUS . 223)
		 (:END . 224)
		 (:I . 225) (:II . 226) (:III . 227) (:IV . 230)
		 (:HAND-UP . 231) (:HAND-DOWN . 232)
		 (:HAND-LEFT . 233) (:HAND-RIGHT . 234)
		 (:SYSTEM . 235) (:NETWORK . 236)
		 (:SPACE . 40) (:SP . 40) (:ALTMODE . 33) (:ALT . 33)
		 (:LAMBDA . 10) (:GAMMA . 11) (:DELTA . 12)
		 (:UPARROW . 13) (:PLUS-MINUS . 14)
		 (:CIRCLE-PLUS . 15) (:INTEGRAL . 177))
	      (MAPCAR #'(LAMBDA (X) (CONS (CAR X) (DPB 1 %%KBD-MOUSE (CDR X))))
		  '( (:MOUSE-L-1 . 0) (:MOUSE-L-2 . 10) (:MOUSE-L-3 . 20)
		     (:MOUSE-M-1 . 1) (:MOUSE-M-2 . 11) (:MOUSE-M-3 . 21)
		     (:MOUSE-R-1 . 2) (:MOUSE-R-2 . 12) (:MOUSE-R-3 . 22)
		     (:MOUSE-1-1 . 0) (:MOUSE-1-2 . 10)
		     (:MOUSE-2-1 . 1) (:MOUSE-2-2 . 11)
		     (:MOUSE-3-1 . 2) (:MOUSE-3-2 . 12)))))
