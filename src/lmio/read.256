;;; -*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; A BONUS DRIVEN READER!

(SPECIAL READTABLE INITIAL-READTABLE STANDARD-INPUT)

(DEFVAR READ-PRESERVE-DELIMITERS NIL)
	;If NIL, syntatically useless characters that delimit symbols and numbers
	;are discarded.  If T, they are untyi'ed so that they can be seen by the
	;caller of READ.  This variable should always be globally NIL so that
	;interactive READ-EVAL-PRINT loops work right, but certain reader macros
	;or other specialized applications may want to bind it to T.

;One peculiar feature of this reader is the ability to construct a table of correspondences
;between the s-expression read in and positions in the stream it was read from.
;This assumes that the stream responds to a :READ-BP operation with a position.
;The feature is activated by reading with XR-CORRESPONDENCE-FLAG set non-nil.
;The table is accumulated in XR-CORRESPONDENCE.
;The table is a list containing three elements for each list in the s-expression
;(not counting cdrs of other lists).
;The first of the three is the list that the three elements pertain to.
;The second is the position at which that list started.
;The third is a list of positions of the elements of that list which are atoms.
;NIL appears in that list for elements which are not atoms, since they have
;their own sets of three elements.
(DECLARE (SPECIAL XR-SPECIAL-CHARACTER-NAMES	;a-list of names known to #\
		  XR-CORRESPONDENCE-FLAG	;T if inside READ-ESTABLISH-CORRESPONDENCE.
		  XR-CORRESPONDENCE))		;Each list we read puts its correspondence
						;entry on this list.

(DECLARE (SPECIAL XR-SHARP-ARGUMENT))

(SETQ RDTBL-ARRAY-SIZE #.RDTBL-ARRAY-SIZE)	;Have a reasonable value in the cold load

;XR-XRTYI takes a stream and returns three values after reading a
;"character" from the stream.
;first value:    translated character (this is what you normally use)
;second value:   magic number (for lookup in the FSM)
;third value:    untranslated character (for people like XR-READ-STRING, also XR-XRUNTYI)
;the third value is the same as the first if the "character" was slashified or on EOF.
(DEFUN XR-XRTYI (STREAM)
       (PROG (CH BITS)
	NEXT (AND (NULL (SETQ CH (FUNCALL STREAM ':TYI)))
		  (RETURN CH (RDTBL-EOF-CODE READTABLE) CH))
	     (AND (> CH RDTBL-ARRAY-SIZE)
		  (GO NEXT))
	     (SETQ BITS (RDTBL-BITS READTABLE CH))
	     (COND ((ZEROP (LOGAND 6 BITS))		;Not slash or circle cross
		    (RETURN (RDTBL-TRANS READTABLE CH)
			    (RDTBL-CODE READTABLE CH)
			    CH))
		   ((ZEROP (LOGAND 2 BITS))		;Circle cross
		    (RETURN (SETQ CH (XR-READ-CIRCLECROSS STREAM))
			    (RDTBL-SLASH-CODE READTABLE)
			    CH))
		   (T					;Slash
		    (RETURN (COND ((NULL (SETQ CH (FUNCALL STREAM ':TYI)))
				   (FERROR NIL "EOF after a slash"))
				  (T CH))
			    (RDTBL-SLASH-CODE READTABLE)
			    CH)))))

(DEFUN XR-READ-CIRCLECROSS (STREAM &AUX CH1 CH2 CH3)
       (AND (NULL (SETQ CH1 (FUNCALL STREAM ':TYI)))
	    (FERROR NIL "EOF during a circlecross"))
       (AND (NULL (SETQ CH2 (FUNCALL STREAM ':TYI)))
	    (FERROR NIL "EOF during a circlecross"))
       (AND (NULL (SETQ CH3 (FUNCALL STREAM ':TYI)))
	    (FERROR NIL "EOF during a circlecross"))
       (IF (OR (< CH1 #/0) (> CH1 #/9)
	       (< CH2 #/0) (> CH2 #/9)
	       (< CH3 #/0) (> CH3 #/9))
	   ;; The lack of an explicit  character here is to get around
	   ;; a stupid bug in the cold-load generator.
	   (FERROR NIL "The three characters immediately following a circlecross must be numeric -- ~C~C~C~C" #/ CH1 CH2 CH3))
       (+ (* 100 (- CH1 #/0))
	  (+ (* 10 (- CH2 #/0))
	     (- CH3 #/0))))

;XR-XRUNTYI takes a stream to untyi to, a character to untyi (third result of XR-XRTYI please)
;and the magic number that character was read in with.
;This is where READ-PRESERVE-DELIMITERS is implemented.
(DEFUN XR-XRUNTYI (STREAM CH NUM)
       (AND (= NUM (RDTBL-SLASH-CODE READTABLE))
	    (FERROR NIL "The character /"~C/" was slashified and cannot be UNTYIed" CH))
       (AND CH
	    (OR READ-PRESERVE-DELIMITERS (ZEROP (LOGAND 1 (RDTBL-BITS READTABLE CH))))
	    (FUNCALL STREAM ':UNTYI CH)))

;XR-XRTYI-WHITE-OUT is like XR-XRTYI but ignores any preceding white space.
(DEFUN XR-XRTYI-WHITE-OUT (STREAM)
       (PROG (CH BITS)
	   L (AND (NULL (SETQ CH (FUNCALL STREAM ':TYI)))
		  (RETURN CH (RDTBL-EOF-CODE READTABLE) CH))
	     (AND (> CH RDTBL-ARRAY-SIZE)
		  (GO L))
	     (SETQ BITS (RDTBL-BITS READTABLE CH))
	     (COND ((ZEROP (LOGAND 6 BITS))		;Not a slash or a circle cross
		    (COND ((ZEROP (LOGAND 1 BITS))	;Not a whitespace character
			   (RETURN (RDTBL-TRANS READTABLE CH)
				   (RDTBL-CODE READTABLE CH)
				   CH))
			  (T
			   (GO L))))
		   ((ZEROP (LOGAND 2 BITS))		;Circle cross (or equivalent)
		    (RETURN (SETQ CH (XR-READ-CIRCLECROSS STREAM))
			    (RDTBL-SLASH-CODE READTABLE)
			    CH))
		   (T					;Slash (or equivalent)
		    (RETURN (COND ((NULL (SETQ CH (FUNCALL STREAM ':TYI)))
				   (FERROR NIL "EOF after a slash"))
				  (T CH))
			    (RDTBL-SLASH-CODE READTABLE)
			    CH)))))

;The specific functions called by XR-READ-THING can return anything as a second value
;if that thing is the symbol "READER-MACRO" then the first thing is called as a
;standard reader macro.
(DEFUN XR-READ-THING (STREAM)
       (PROG (CH NUM A B STRING (INDEX 0) (STLEN 100) REAL-CH
		 (READTABLE-FSM (RDTBL-FSM READTABLE))
		 (FNPROP (RDTBL-READ-FUNCTION-PROPERTY READTABLE))
		 (STATE (RDTBL-STARTING-STATE READTABLE)))
	     (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI-WHITE-OUT STREAM))
	     (SETQ STATE (AR-2 READTABLE-FSM STATE NUM))
	     (COND ((NOT (NUMBERP STATE))
		    (LET ((FLAG (CAR STATE))
			  (TODO (CDR STATE)))
			 (SELECTQ FLAG
				  (NO-UNTYI-QUOTE
				   (SETQ A TODO)
				   (SETQ B 'SPECIAL-TOKEN))
				  (LAST-CHAR
				   (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP)
								  STREAM NIL CH)))
				  (NO-UNTYI-FUNCTION
				   (SETQ STRING (MAKE-ARRAY P-N-STRING ART-STRING 1))
				   (AS-1 CH STRING 0)
				   (MULTIPLE-VALUE (A B)
						   (FUNCALL (GET TODO FNPROP) STREAM STRING)))
				  ((UNTYI-QUOTE UNTYI-FUNCTION)
				   (FERROR NIL
				     "Reader in infinite loop reading character: /"~C/""
				     REAL-CH))
				  (OTHERWISE
				   (FERROR NIL
				     "The reader found ~S in the finite state machine"
				     FLAG)))
			 (RETURN A B))))
	     (SETQ STRING (MAKE-ARRAY P-N-STRING ART-STRING 100))
	   L (AS-1 CH STRING INDEX)
	     (SETQ INDEX (1+ INDEX))
	     (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM))
	     (SETQ STATE (AR-2 READTABLE-FSM STATE NUM))
	     (COND ((NUMBERP STATE)
		    (COND ((= INDEX STLEN)
			   (SETQ STLEN (+ 40 STLEN))
			   (ADJUST-ARRAY-SIZE STRING STLEN)))
		    (GO L)))
	     (LET ((FLAG (CAR STATE))
		   (TODO (CDR STATE)))
		  (SELECTQ FLAG
			   (UNTYI-FUNCTION
			    (XR-XRUNTYI STREAM REAL-CH NUM)
			    (ADJUST-ARRAY-SIZE STRING INDEX)
			    (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP) STREAM STRING)))
			   (LAST-CHAR
			    (ADJUST-ARRAY-SIZE STRING INDEX)
			    (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP)
							   STREAM STRING CH)))
			   (NO-UNTYI-FUNCTION
			    (ADJUST-ARRAY-SIZE STRING (1+ INDEX))
			    (AS-1 CH STRING INDEX)
			    (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP) STREAM STRING)))
			   (UNTYI-QUOTE
			    (XR-XRUNTYI STREAM REAL-CH NUM)
			    (RETURN-ARRAY STRING)
			    (SETQ A TODO)
			    (SETQ B 'SPECIAL-TOKEN))
			   (NO-UNTYI-QUOTE
			    (RETURN-ARRAY STRING)
			    (SETQ A TODO)
			    (SETQ B 'SPECIAL-TOKEN))
			   (OTHERWISE
			    (FERROR NIL
			      "The reader found ~S in the finite state machine"
			      FLAG)))
		  (RETURN A B))))

;The next 2 functions are for reading in old-format "ascii" readtables
(DEFUN XR-LOAD (STREAM)
       (SETQ READTABLE (XR-GET-READTABLE STREAM)))

(DEFUN XR-GET-READTABLE (STREAM &AUX RDTBL RT-LEN RT-LEADER-LEN FSM STATES CHARS)
       (AND (STRINGP STREAM)
	    (SETQ STREAM (OPEN STREAM '(READ))))
       (SETQ RT-LEN (READ STREAM))
       (SETQ RT-LEADER-LEN (READ STREAM))
       (SETQ STATES (READ STREAM))
       (SETQ CHARS (READ STREAM))
       (SETQ RDTBL (MAKE-ARRAY NIL ART-16B
			       (LIST 3 RT-LEN) NIL RT-LEADER-LEN NIL T))
       (DO I 0 (1+ I) (= I RT-LEN)
	   (AS-2 (READ STREAM) RDTBL 0 I)
	   (AS-2 (READ STREAM) RDTBL 1 I)
	   (AS-2 (READ STREAM) RDTBL 2 I))
       (DO I 1 (1+ I) (= I RT-LEADER-LEN)
	   (STORE-ARRAY-LEADER (READ STREAM) RDTBL I))
       (SETQ FSM (MAKE-ARRAY NIL ART-Q (LIST STATES CHARS)))
       (SETF (RDTBL-FSM RDTBL) FSM)
       (DO I 0 (1+ I) (= I STATES)
	   (DO J 0 (1+ J) (= J CHARS)
	       (AS-2 (READ STREAM) FSM I J)))
       (CLOSE STREAM)
       RDTBL)

;This function is compatible with the regular Maclisp TYI.  If you want speed,
;FUNCALL the stream directly.  We have to echo, but cannot use the rubout handler
;because the user wants to see rubout, form, etc. characters.  Inside the rubout
;handler, we do not echo since echoing will have occurred already.
(DEFUN TYI (&OPTIONAL STREAM EOF-OPTION &AUX CH)
  (MULTIPLE-VALUE (STREAM EOF-OPTION) (DECODE-READ-ARGS STREAM EOF-OPTION))
  (OR EOF-OPTION (SETQ EOF-OPTION -1))	;Result must be fixnum, so user cannot
					; have explicitly specified NIL
					;Crockish but Maclisp compatible
  (COND	((NULL (SETQ CH (FUNCALL STREAM ':TYI)))	;Get a character, check for EOF
	 EOF-OPTION)
	((OR RUBOUT-HANDLER				;If inside rubout handler, or
	     (NOT (MEMQ ':RUBOUT-HANDLER (FUNCALL STREAM ':WHICH-OPERATIONS))))
	 CH)						; ordinary device, just return char
	(T (OR (= CH #\RUBOUT)				;Echo anything but rubout
	       (FORMAT STREAM "~C" CH))			;Even control and meta chars
	   CH)))

(DEFUN READCH (&OPTIONAL STREAM EOF-OPTION &AUX CH)
  (MULTIPLE-VALUE (STREAM EOF-OPTION) (DECODE-READ-ARGS STREAM EOF-OPTION))
  (IF (MINUSP (SETQ CH (TYI STREAM))) EOF-OPTION
      (INTERN (STRING CH) PKG-USER-PACKAGE))) ;Character objects are keywords

;This function is compatible, more or less, with the regular Maclisp TYIPEEK.
;It does not echo, since the echoing will occur when READ or TYI is called.
;It does echo characters which it discards.
(DEFUN TYIPEEK (&OPTIONAL PEEK-TYPE STREAM EOF-OPTION)
  (MULTIPLE-VALUE (STREAM EOF-OPTION) (DECODE-READ-ARGS STREAM EOF-OPTION))
  (OR EOF-OPTION (SETQ EOF-OPTION -1))	;Result must be fixnum, so user cannot
					; have explicitly specified NIL
  (AND (NUMBERP PEEK-TYPE) (>= PEEK-TYPE 1000)
       (FERROR NIL "The ~S flavor of TYIPEEK is not implemented" PEEK-TYPE))
  (DO ((CH))  ;Pass over characters until termination condition reached
      (())
    (OR (SETQ CH (FUNCALL STREAM ':TYI))
	(RETURN EOF-OPTION))		;EOF, give up
    (FUNCALL STREAM ':UNTYI CH)		;Put it back
    (AND (COND ((NULL PEEK-TYPE))	;Break on every
	       ((EQ CH PEEK-TYPE))	;Break on specified character
	       ((EQ PEEK-TYPE T)	;Break on start-of-object
		(AND (< CH RDTBL-ARRAY-SIZE)
		     (ZEROP (LOGAND (RDTBL-BITS READTABLE CH) 1)))))
	 (RETURN CH))			;Break here
    (TYI STREAM)))			;Echo and eat this character

;This is like READ, but ignores extra closeparens and eofs.
(DEFUN READ-FOR-TOP-LEVEL (&AUX W-O)
  (COND ((MEMQ ':READ (SETQ W-O (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS)))
	 (FUNCALL STANDARD-INPUT ':READ))
	((AND (NOT RUBOUT-HANDLER) (MEMQ ':RUBOUT-HANDLER W-O))
	 ;;We must get inside the rubout handler's top-level CATCH
	 (FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '() #'READ-FOR-TOP-LEVEL))
	((PROG (THING TYPE SPLICEP XR-SHARP-ARGUMENT)
	    A (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STANDARD-INPUT))
	      (COND ((EQ TYPE 'READER-MACRO)
		     (MULTIPLE-VALUE (THING TYPE SPLICEP)
		       (FUNCALL THING ':TOPLEVEL STANDARD-INPUT))
		     (AND SPLICEP (GO A)))
		    ((EQ TYPE 'SPECIAL-TOKEN)
		     (COND ((EQ THING 'EOF))
			   ((EQ THING 'CLOSE) (GO A))
			   (T
			    (FERROR NIL 
				    "The special token ~S was read in at top level" THING)))))
	      (RETURN THING TYPE)))))

;READ is almost like XR-READ-THING except READER-MACROs are invoked and SPECIAL-TOKENS
;are barfed at. Also this is the function to be called by the general public.
(DEFUN READ (&OPTIONAL STREAM EOF-OPTION &AUX W-O)
  (MULTIPLE-VALUE (STREAM EOF-OPTION) (DECODE-READ-ARGS STREAM EOF-OPTION))
  (COND ((MEMQ ':READ (SETQ W-O (FUNCALL STREAM ':WHICH-OPERATIONS)))
	 (FUNCALL STREAM ':READ EOF-OPTION))
	((AND (NOT RUBOUT-HANDLER) (MEMQ ':RUBOUT-HANDLER W-O))
	 ;;We must get inside the rubout handler's top-level CATCH
	 (FUNCALL STREAM ':RUBOUT-HANDLER '() #'READ STREAM EOF-OPTION))
	((PROG (THING TYPE SPLICEP XR-SHARP-ARGUMENT)
	   A (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
	     (COND ((EQ TYPE 'READER-MACRO)
		    (MULTIPLE-VALUE (THING TYPE SPLICEP)
				    (FUNCALL THING ':TOPLEVEL STREAM))
		    (AND SPLICEP (GO A))
		    (RETURN THING TYPE))
		   ((EQ TYPE 'SPECIAL-TOKEN)
		    (COND ((EQ THING 'EOF)
			   (AND EOF-OPTION (RETURN EOF-OPTION))
			   (FERROR NIL "End of file on stream ~S" STREAM))
			  (T (FERROR NIL "The special token ~S was read in at top level"
				     THING))))
		   (T (RETURN THING TYPE)))))))

;This ends the reader proper. The things from here on are called only if they appear in
;the readtable itself. Although XR-READ-LIST is somewhat special in that it handles splicing
;macros. Note that the second arg (FIFTY) should be a number (50) rather than a string ("(")
;due to the LAST-CHAR hack.
(DEFUN (LIST STANDARD-READ-FUNCTION) (STREAM SHOULD-BE-NIL FIFTY)
       SHOULD-BE-NIL ;Ignored.  This would be the string if there was one
       (PROG (LIST THING TYPE SPLICEP END-OF-LIST BP CORRESPONDENCE-ENTRY)
             (SETQ END-OF-LIST (VALUE-CELL-LOCATION 'LIST))
	     (COND (XR-CORRESPONDENCE-FLAG
		    (FUNCALL STREAM ':UNTYI FIFTY)
		    (SETQ CORRESPONDENCE-ENTRY
			  `(NIL ,(FUNCALL STREAM ':READ-BP) NIL . ,XR-CORRESPONDENCE))
		    (SETQ XR-CORRESPONDENCE CORRESPONDENCE-ENTRY)
		    (FUNCALL STREAM ':TYI)))
	   A (AND XR-CORRESPONDENCE-FLAG
		  (PUSH (FUNCALL STREAM ':READ-BP)
			(CADDR CORRESPONDENCE-ENTRY)))
	     (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
	     (COND ((EQ TYPE 'READER-MACRO)
		    (COND (XR-CORRESPONDENCE-FLAG
			   (FUNCALL STREAM ':UNTYI FIFTY)
			   (SETQ BP (FUNCALL STREAM ':READ-BP))
			   (FUNCALL STREAM ':TYI)))
		    (MULTIPLE-VALUE (THING TYPE SPLICEP)
				    (FUNCALL THING LIST STREAM))
		    (COND (SPLICEP (SETQ LIST THING)
				   (AND XR-CORRESPONDENCE-FLAG
					(SETF (CADDR CORRESPONDENCE-ENTRY)
					      (FIRSTN (LENGTH LIST)
						      (CADDR CORRESPONDENCE-ENTRY))))
				   (SETQ END-OF-LIST
					 (COND ((ATOM LIST) (VALUE-CELL-LOCATION 'LIST))
					       (T (LAST LIST)))))
                          (T (RPLACD END-OF-LIST (SETQ END-OF-LIST (NCONS THING)))
			     (AND XR-CORRESPONDENCE-FLAG
				  (SETQ XR-CORRESPONDENCE
					`(,THING ,BP NIL . ,XR-CORRESPONDENCE)))))
		    (GO A))
		   ((EQ TYPE 'SPECIAL-TOKEN)
		    (COND ((EQ THING 'CONSING-DOT)
			   (AND (NULL LIST)
				(FERROR NIL
				  "A dot was read before any list was accumulated"))
			   (GO RDOT))
			  ((EQ THING 'CLOSE)
			   (AND XR-CORRESPONDENCE-FLAG
				(RPLACA CORRESPONDENCE-ENTRY LIST)
				(SETF (CADDR CORRESPONDENCE-ENTRY)
				      (NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
			   (RETURN LIST 'LIST))
			  (T (FERROR NIL
			       "The ~:[special token ~S was read~;end of file was reached~*~] in the middle of the list ~:S"
			       (EQ THING 'EOF)
			       THING
			       LIST))))
		   (T
		    (RPLACD END-OF-LIST (SETQ END-OF-LIST (NCONS THING)))		    
		    (GO A)))
	RDOT (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
	     (AND (EQ TYPE 'SPECIAL-TOKEN)
		  (FERROR NIL
		    "The special token ~S was read after a dot"
		    THING))
	     (COND ((EQ TYPE 'READER-MACRO)
		    (MULTIPLE-VALUE (THING TYPE SPLICEP)
				    (FUNCALL THING ':AFTER-DOT STREAM))
		    (AND SPLICEP (GO RDOT))))
	     (RPLACD END-OF-LIST THING)
      RDOT-1 (MULTIPLE-VALUE (THING TYPE) (XR-READ-THING STREAM))
	     (COND ((AND (EQ THING 'CLOSE) (EQ TYPE 'SPECIAL-TOKEN))
		    (AND XR-CORRESPONDENCE-FLAG
			 (RPLACA CORRESPONDENCE-ENTRY LIST)
				(SETF (CADDR CORRESPONDENCE-ENTRY)
				      (NREVERSE (CADDR CORRESPONDENCE-ENTRY))))
		    (RETURN LIST 'LIST))
		   ((EQ TYPE 'READER-MACRO)
		    (MULTIPLE-VALUE (THING TYPE SPLICEP)
				    (FUNCALL THING ':AFTER-DOT STREAM))
		    (AND SPLICEP (GO RDOT-1))
		    (FERROR NIL
		      "~S was read instead of a close paren (returned by a reader macro)"
		      THING))
		   (T (FERROR NIL
			"~S was read instead of a close paren"
			THING)))))

(DEFPROP SYMBOL XR-READ-SYMBOL STANDARD-READ-FUNCTION)
(DEFPROP SHARP-SYMBOL XR-READ-SYMBOL STANDARD-READ-FUNCTION)
(DEFPROP SC-SYMBOL XR-READ-SYMBOL STANDARD-READ-FUNCTION)
(DEFUN XR-READ-SYMBOL (STREAM STRING)
       STREAM ;ignored, doesn't do any additional reading
       (PROG (R FLAG)
	     (MULTIPLE-VALUE (R FLAG) (INTERN STRING))
	     (AND FLAG (RETURN-ARRAY STRING))
	     (RETURN R 'SYMBOL)))

(DEFUN (MACRO-CHAR STANDARD-READ-FUNCTION) (STREAM SHOULD-BE-NIL LAST-CHAR)
       STREAM ;ignored, doesn't do any additional reading
       SHOULD-BE-NIL ;ignored, no string
       (PROG (TEM)
	     (COND ((SETQ TEM (ASSQ LAST-CHAR (RDTBL-MACRO-ALIST READTABLE)))
		    (RETURN (CDR TEM) 'READER-MACRO))
		   (T (FERROR NIL
			"No reader macro definition found for the character ~C" LAST-CHAR)))))

;FOO: switches us to the package associated with the string "FOO"
(DEFUN (PACKAGE-PREFIX STANDARD-READ-FUNCTION) (STREAM STRING LAST-CH)
       LAST-CH ;ignored
       (PROG (THING TYPE PK)
	     (SETQ PK (PKG-FIND-PACKAGE (OR STRING "") NIL PACKAGE))
	     ((LAMBDA (PACKAGE)
		      (MULTIPLE-VALUE (THING TYPE)
				      (READ STREAM)))
	      PK)
	     (OR (NULL STRING)
		 (RETURN-ARRAY STRING))
	     (RETURN THING TYPE)))

;We use the winning LAST-CHAR hack to get the character to match
(DEFUN (STRING STANDARD-READ-FUNCTION) (STREAM SHOULD-BE-NIL MATCH)
       SHOULD-BE-NIL ;ignored, no string token
       (PROG (CH NUM REAL-CH (I 0) (LEN 100) STRING TEM)
	     (SETQ STRING (MAKE-ARRAY P-N-STRING ART-STRING 100))
	     (SETQ TEM (RDTBL-SLASH-CODE READTABLE))
	   L (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM))
	     (COND ((NULL CH)
		    (FERROR NIL "EOF in the middle of a string"))
		   ((AND (= REAL-CH MATCH)
			 (NOT (= NUM TEM)))
		     (ADJUST-ARRAY-SIZE STRING I)
		     (RETURN STRING 'STRING))
		   (T (AS-1 REAL-CH STRING I)
		      (SETQ I (1+ I))
		      (COND ((= I LEN)
			     (SETQ LEN (+ LEN 40))
			     (ADJUST-ARRAY-SIZE STRING LEN)))
		      (GO L)))))

(DEFUN (QUOTED-SYMBOL STANDARD-READ-FUNCTION) (STREAM SHOULD-BE-NIL MATCH)
       SHOULD-BE-NIL ;ignored, no string token
       (PROG (CH NUM REAL-CH (I 0) (LEN 100) STRING R FLAG TEM)
	     (SETQ STRING (MAKE-ARRAY P-N-STRING ART-STRING 100))
	     (SETQ TEM (RDTBL-SLASH-CODE READTABLE))
	   L (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM))
	     (COND ((NULL CH)
		    (FERROR NIL "EOF in the middle of a quoted symbol"))
		   ((AND (= REAL-CH MATCH)
			 (NOT (= NUM TEM)))
		     (ADJUST-ARRAY-SIZE STRING I)
		     (MULTIPLE-VALUE (R FLAG) (INTERN STRING))
		     (AND FLAG (RETURN-ARRAY STRING))
		     (RETURN R 'SYMBOL))
		   (T (AS-1 REAL-CH STRING I)
		      (SETQ I (1+ I))
		      (COND ((= I LEN)
			     (SETQ LEN (+ LEN 40))
			     (ADJUST-ARRAY-SIZE STRING LEN)))
		      (GO L)))))

;; This function is given a string, an index into the string, and the length
;; of the string.  It looks for a signed, possibly decimal-pointed number,
;; computes it, and returns two values: the fixnum, and the index in the
;; string of where the first char was that caused it to stop.  The second
;; value will equal the "length" argument if it got to the end of the string.
;; it takes a base as well.
(DEFUN XR-READ-FIXNUM-INTERNAL (STRING II LEN &OPTIONAL (IBS IBASE) &AUX (SIGN 1) (NUM 0)
					  CH (SECONDVAL LEN))
  (PROG ();MULTIPLE-VALUE
    (SETQ CH (AR-1 STRING II))
    (COND ((= CH #/+)
	   (SETQ II (1+ II)))
	  ((= CH #/-)
	   (SETQ II (1+ II))
	   (SETQ SIGN -1)))
    (RETURN
     (DO ((I II (1+ I)))
	 ((>= I LEN)
	  (* SIGN NUM))
	 (SETQ CH (AR-1 STRING I))
	 (COND ((OR (< CH 60) (> CH 71))
		(COND ((= CH #/.)
		       (COND ((= IBS 10.)
			      (SETQ SECONDVAL (1+ I))
			      (RETURN (* SIGN NUM)))
			     (T
			      (SETQ IBS 10.)
			      (SETQ NUM 0)
			      (SETQ I (1- II)))))
		      (T (SETQ SECONDVAL I)
			 (RETURN (* SIGN NUM)))))
	       (T (SETQ NUM (+ (* NUM IBS) (- CH 60))))))
     SECONDVAL)))

;; This function takes a string which represents a fixnum (and a stream
;; which it doesn't use), and returns the fixnum.  It ASSUMES that the string
;; follows the format of the standard readtable.
(DEFUN (FIXNUM STANDARD-READ-FUNCTION) (STREAM STRING &AUX NUM LEN I)
    STREAM ;ignored, doesn't do any additional reading
    (OR (FIXP IBASE)	;If it was a flonum, confusing things would happen
	(FERROR NIL "~S bad value for IBASE.  Has been reset to 8"
		    (PROG1 IBASE (SETQ IBASE 8))))	   
    (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING))
    (MULTIPLE-VALUE (NUM I)
       (XR-READ-FIXNUM-INTERNAL STRING 0 LEN))
    (PROG ()
    (RETURN
     (PROG1 (COND ((= I LEN) NUM)
		  (T (LET ((NUM2 (XR-READ-FIXNUM-INTERNAL STRING (1+ I) LEN)))
			  (COND ((= (AR-1 STRING I) #/_)
				 (ASH NUM NUM2))
				(T (* NUM (^ IBASE NUM2)))))))
	    (RETURN-ARRAY STRING))
     'FIXNUM)))

(DEFMACRO SKIP-CHAR ()
  '(SETQ COUNT (1- COUNT)
	 INDEX (1+ INDEX)))

(DECLARE (SPECIAL HIGH-PART LOW-PART NDIGITS INDEX COUNT POWER-10
		  XR-SMALL-POWER-TABLE XR-LARGE-POWER-TABLE))

(DEFUN XR-READ-FLONUM (STRING SFL-P &AUX (POWER-10 0)(INDEX 0) (POSITIVE T)
                                         (HIGH-PART 0) (LOW-PART 0) (NDIGITS 12.)
                                         COUNT CHAR STRING-LENGTH)
   (SETQ COUNT (STRING-LENGTH STRING)
         STRING-LENGTH COUNT)
   (SETQ CHAR (AR-1 STRING INDEX))
   ;;; CHECK FOR PLUS OR MINUS
   (COND ((OR (= CHAR #/+) (= CHAR #/-))
          (SKIP-CHAR)
          (SETQ POSITIVE (= CHAR #/+))))
   ;;; SKIP LEADING ZEROS
   (DO NIL
       ((NOT (= (AR-1 STRING INDEX) #/0)))
       (SKIP-CHAR))
   (COND ((= (AR-1 STRING INDEX) #/.) ;IF WE HIT A POINT, KEEP STRIPPING 0'S
	  (SKIP-CHAR)
	  (DO NIL
	      ((OR (< COUNT 2)    ;LEAVE ONE DIGIT AT LEAST
		   (NOT (= (AR-1 STRING INDEX) #/0))))	;OR NON-ZERO DIGIT
	      (SKIP-CHAR)
	      (SETQ POWER-10 (1+ POWER-10)))
	  (XR-ACCUMULATE-DIGITS STRING T))
   ;;; ACCUMULATE DIGITS UP TO THE POINT OR EXPONENT (THESE ARE FREE)
	 (T (XR-ACCUMULATE-DIGITS STRING NIL)
	    (COND ((= (AR-1 STRING INDEX) #/. )
		   (SKIP-CHAR)
		   ;; SKIP TRAILING ZEROS AFTER THE POINT.  THIS AVOIDS HAVING A
		   ;; ONE IN THE LSB OF 2.0 DUE TO DIVIDING 20. BY 10.
		   (LET ((IDX (STRING-SEARCH-NOT-SET '(#/0) STRING INDEX)))
		     (COND ((NULL IDX) (SETQ COUNT 0))	;NOTHING BUT ZEROS THERE
			   ((NOT (MEMQ (AR-1 STRING IDX) '(#/1 #/2 #/3 #/4 #/5
							   #/6 #/7 #/8 #/9)))
			    (SETQ INDEX IDX		;NOT DIGITS THERE EXCEPT ZEROS
				  COUNT (- STRING-LENGTH INDEX)))
			   (T				;REAL DIGITS PRESENT, SCAN NORMALLY
			    (XR-ACCUMULATE-DIGITS STRING T))))))))
   ;;; HERE WE HAVE READ SOMETHING UP TO EXPONENT IF IT EXISTS, OR END OF STRING
   (COND ((> COUNT 0)
          (SKIP-CHAR)                           ;SKIP THE EXPONENT CHARACTER
          (SETQ POWER-10 (- POWER-10
                            (SI:XR-READ-FIXNUM-INTERNAL STRING
                                                     INDEX
                                                     STRING-LENGTH
                                                     10.)))))
   (LET ((NUM (COND (SFL-P (SMALL-FLOAT (XR-FLONUM-CONS HIGH-PART LOW-PART POWER-10)))
                    (T (XR-FLONUM-CONS HIGH-PART LOW-PART POWER-10)))))
        (COND (POSITIVE NUM) (T (- NUM)))))

(DEFUN XR-ACCUMULATE-DIGITS (STRING POST-DECIMAL &AUX CHAR)
   (DO NIL ((= COUNT 0))
       (SETQ CHAR (AR-1 STRING INDEX))
       (COND ((OR (> CHAR #/9) (< CHAR #/0))
              (RETURN NIL)))
       (COND ((> NDIGITS 0) 
	      (SETQ HIGH-PART (+ (* HIGH-PART 10.)
                                 (LSH (%MULTIPLY-FRACTIONS LOW-PART 10.) 1))
                    LOW-PART (%24-BIT-PLUS (%24-BIT-TIMES LOW-PART 10.) (- CHAR #/0)))
	      (COND ((MINUSP LOW-PART)		;Carried into sign-bit
		     (SETQ HIGH-PART (1+ HIGH-PART)
			   LOW-PART (LOGAND LOW-PART 37777777))))
              (AND POST-DECIMAL (SETQ POWER-10 (1+ POWER-10))))
             (T (OR POST-DECIMAL  (SETQ POWER-10 (1- POWER-10)))))
       (SKIP-CHAR)
       (SETQ NDIGITS (1- NDIGITS))))

(DECLARE (UNSPECIAL HIGH-PART LOW-PART NDIGITS INDEX COUNT POWER-10))

;;; HERE POWER-10 IS THE POWER OF 10 THAT THE NUMBER IN HIGH,,LOW SHOULD BE
;;; DIVIDED BY.
(DEFUN XR-FLONUM-CONS (HIGH LOW POWER-10 &AUX FLOAT-NUMBER)
   (SETQ FLOAT-NUMBER (SYS:%FLOAT-DOUBLE (LSH HIGH -1) (%LOGDPB HIGH 2701 LOW)))
   (COND ((< POWER-10 0) (* FLOAT-NUMBER (XR-GET-POWER-10 (- POWER-10))))
	 ((> POWER-10 0) (// FLOAT-NUMBER (XR-GET-POWER-10 POWER-10)))
	 (T FLOAT-NUMBER)))

(DEFUN XR-GET-POWER-10 (POWER)
   (DO ((I POWER)
	(NUM (FLOAT 1.)))
       ((< I 14.)
	(* NUM (AR-1 XR-SMALL-POWER-TABLE I)))
       (SETQ NUM (* NUM (AR-1 XR-LARGE-POWER-TABLE (HAULONG I)))
	     I (- I (LSH 1 (1- (HAULONG I)))))))

(DEFUN XR-TABLE-SETUP ()
  (COND (FOR-CADR
	 (SETQ XR-SMALL-POWER-TABLE (MAKE-ARRAY NIL 'ART-Q 14.)
	       XR-LARGE-POWER-TABLE (MAKE-ARRAY NIL 'ART-Q 10.))
	 (DO ((I 0 (1+ I))
	      (NUM (FLOAT 1.) (* NUM TEN))
	      (TEN (FLOAT 10.)))
	     ((= I 14.))
	     (AS-1 NUM XR-SMALL-POWER-TABLE I))
	 (DO ((I 2 (1+ I))
	      (NUM (FLOAT 10.)))
	     ((= I 10.)
	      (AS-1 (FLOAT 1.) XR-LARGE-POWER-TABLE 0)
	      (AS-1 (FLOAT 10.) XR-LARGE-POWER-TABLE 1))
	     (SETQ NUM (* NUM NUM))
	     (AS-1 NUM XR-LARGE-POWER-TABLE I)))))

(DEFUN (FLONUM STANDARD-READ-FUNCTION) (STREAM STRING)
   STREAM ;ignored, doesn't do any additional reading
   (PROG2 NIL (XR-READ-FLONUM STRING NIL) (RETURN-ARRAY STRING)))

(DEFUN (SMALL-FLONUM STANDARD-READ-FUNCTION) (STREAM STRING)
   STREAM ;ignored, doesn't do any additional reading
   (PROG2 NIL (XR-READ-FLONUM STRING T) (RETURN-ARRAY STRING)))

;These can have an alpha, beta, or epsilon, or a bunch of digits, between the
;# and operative character.  Because there is no reasonable way to pass this
;argument to the invoked reader-macro (in the case of SHARP-THING), we
;use the global variable XR-SHARP-ARGUMENT, which can be NIL, or a number,
;which may be suitable to DPB into %%KBD-CONTROL-META of a character.

(DEFUN PARSE-SHARP-ARGUMENT (STRING)
  (SETQ XR-SHARP-ARGUMENT
	(DO ((I 1 (1+ I))
	     (N (STRING-LENGTH STRING))
	     (R 0)
	     (R1 0)
	     (DIG))
	    (( I N)
	     (AND (> N 1) (+ R R1)))
	  (COND ((SETQ DIG (CDR (ASSQ (AREF STRING I)
				      '( (#/ . 1) (#/ . 2) (#/ . 3)
					 (#/ . 4) (#/ˆ . 8) ))))
		 (SETQ R1 (+ R1 DIG)))
		((SETQ R (+ (* R 10.) (- (AREF STRING I) #/0))))))))

(DEFUN (SHARP-SLASH STANDARD-READ-FUNCTION) (STREAM STRING LAST-CHAR)
  STREAM ;ignored, doesn't do any additional reading
  (PARSE-SHARP-ARGUMENT STRING)
  (RETURN-ARRAY STRING)
  (PROG () (RETURN (DPB (OR XR-SHARP-ARGUMENT 0)
			%%KBD-CONTROL-META
			LAST-CHAR)
		   'FIXNUM)))

(DEFUN (SHARP-THING STANDARD-READ-FUNCTION) (STREAM STRING LAST-CHAR)
   STREAM ;ignored, doesn't do any additional reading
   (PROG ()
    (PARSE-SHARP-ARGUMENT STRING)
    (OR (< LAST-CHAR #/a)				;Upper case it
	(> LAST-CHAR #/z)
	(SETQ LAST-CHAR (- LAST-CHAR 32.)))
    (LET ((PAIR (ASSQ LAST-CHAR (RDTBL-/#-MACRO-ALIST READTABLE))))
	 (COND ((NULL PAIR)
		(FERROR NIL "The reader has encountered a ~A~C, an undefined # macro"
			STRING LAST-CHAR))
	       (T (RETURN-ARRAY STRING)
		  (RETURN (CDR PAIR) 'READER-MACRO))))))

(DEFUN XR-/#/O-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (LET ((IBASE 8.)) (READ STREAM)))

(DEFUN XR-/#/X-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (LET ((IBASE 16.)) (READ STREAM)))

(DEFUN XR-/#/R-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (OR (FIXP XR-SHARP-ARGUMENT)
	   (FERROR NIL "The # macro #r was called without any digits"))
       (LET ((IBASE XR-SHARP-ARGUMENT)) (READ STREAM)))

(DEFUN XR-/#/'-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (LIST 'FUNCTION (READ STREAM)))

(DEFUN XR-/#/,-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (COND ((AND (BOUNDP 'COMPILER:QC-FILE-READ-IN-PROGRESS)
		   COMPILER:QC-FILE-READ-IN-PROGRESS)
	      (CONS 'SI:**EXECUTION-CONTEXT-EVAL**
		    (READ STREAM '*EOF*)))
	     (T (EVAL (READ STREAM '*EOF*)))))

(DEFUN XR-/#/.-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (EVAL (READ STREAM '*EOF*)))

(DEFUN XR-/#/\-MACRO (IGNORE STREAM)
  (PKG-BIND "USER"
	    (LET ((FROB (READ STREAM)) CH)		;Get symbolic name of character
	      (OR (SETQ CH (CDR (ASSQ FROB XR-SPECIAL-CHARACTER-NAMES)))
		  (FERROR NIL "#\~A is not a defined character-name" FROB))	      
	      (DPB (OR XR-SHARP-ARGUMENT 0)
		   %%KBD-CONTROL-META
		   CH))))

(DEFUN XR-/#/^-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR	;Ignored
       (LET ((CH (FUNCALL STREAM ':TYI)))
	    (OR (< CH #/a)
		(> CH #/z)
		(SETQ CH (- CH 32.)))
	    (DPB 1 %%KBD-CONTROL CH)))

(DEFUN XR-/#/Q-MACRO (LIST-SO-FAR STREAM)	;IN LISPM, GOBBLE FROB
  (PROG NIL
	(COND ((OR (NULL LIST-SO-FAR)
		   (LISTP LIST-SO-FAR))
	       (RETURN (NCONC LIST-SO-FAR (LIST (READ STREAM '*EOF*))) NIL T))
	      (T
	       (RETURN (READ STREAM '*EOF*))))))

(DEFUN XR-/#/M-MACRO (LIST-SO-FAR STREAM)	;IN LISPM, FLUSH FROB
  (PROG NIL 
	(READ STREAM '*EOF*)
        (RETURN LIST-SO-FAR NIL T)))

(DEFUN XR-/#/N-MACRO (LIST-SO-FAR STREAM)	;IN NIL, FLUSH FROB
  (PROG NIL 
	(READ STREAM '*EOF*)
        (RETURN LIST-SO-FAR NIL T)))

;  Read-time conditionalization macros
;  <feature-form> ::= <symbol> | (NOT <feature-form>) | (AND . <feature-forms>) |
;		      (OR . <feature-forms>)

;  As an example, (AND MACSYMA (OR LISPM AMBER)) is a feature form
;  which represents the predicate
;  (AND (STATUS FEATURE MACSYMA) (OR (STATUS FEATURE LISPM) (STATUS FEATURE AMBER))).
;  The use of these forms in conjuction with the #+ reader macro
;  enables the read-time environment to conditionalize the
;  reading of forms in a file.

;  #+<FEATURE-FORM> <FORM> is read as <FORM> if <FEATURE-FORM> is true,
;  i.e. if the predicate associated with <FEATURE-FORM> is non-NIL when
;  evaluated in the read-time environment.
;  #+<FEATURE-FORM> <FORM> is read as whitespace if <FEATURE-FORM> is false.

;  #+LISPM <FORM> makes <FORM> exist if being read by the Lisp Machine.
;  #+(OR LISPM LISPM-COMPILER) <FORM> makes <FORM> exist if being
;  read either by the Lisp Machine or by QCMP.  This is equivalent
;  to #Q <FORM>.  Similarly, #+(AND MACLISP (NOT LISPM-COMPILER)) is
;  equivalent to #M.

(DEFUN XR-/#/+-MACRO (LIST-SO-FAR STREAM)
    (PROG NIL
	  (LET ((FEATURE (READ STREAM '*EOF*))	;feature or feature list
		(FORM (READ STREAM '*EOF*)))	;protected form
	    (COND ((NOT (XR-FEATURE-PRESENT FEATURE))
		   (RETURN LIST-SO-FAR NIL T))
		  ((OR (NULL LIST-SO-FAR)
		       (LISTP LIST-SO-FAR))
		   (RETURN (NCONC LIST-SO-FAR (LIST FORM)) NIL T))
		  (T (RETURN FORM))))))

;  #-<FEATURE-FORM> is equivalent to #+(NOT FEATURE-FORM).

(DEFUN XR-/#/--MACRO (LIST-SO-FAR STREAM)
    (PROG NIL
	  (LET ((FEATURE (READ STREAM '*EOF*))	;feature or feature list
		(FORM (READ STREAM '*EOF*)))	;protected form
	    (COND ((XR-FEATURE-PRESENT FEATURE)
		   (RETURN LIST-SO-FAR NIL T))
		  ((OR (NULL LIST-SO-FAR)
		       (LISTP LIST-SO-FAR))
		   (RETURN (NCONC LIST-SO-FAR (LIST FORM)) NIL T))
		  (T (RETURN FORM))))))

;  Here, FEATURE is either a symbol to be looked up in (STATUS FEATURES) or
;  a list whose car is either AND, OR, or NOT.

(DEFUN XR-FEATURE-PRESENT (FEATURE)
    (COND ((SYMBOLP FEATURE)
	   (MEMQ FEATURE (INHIBIT-STYLE-WARNINGS
			   (STATUS FEATURES))))
	  ((ATOM FEATURE)
	   (FERROR NIL "Unknown form in #+ or #- feature list -- ~S" FEATURE))
	  ((EQ (CAR FEATURE) 'NOT)
	   (NOT (XR-FEATURE-PRESENT (CADR FEATURE))))
	  ((EQ (CAR FEATURE) 'AND)
	   (EVERY (CDR FEATURE) #'XR-FEATURE-PRESENT))
	  ((EQ (CAR FEATURE) 'OR)
	   (SOME (CDR FEATURE) #'XR-FEATURE-PRESENT))
	  (T (FERROR NIL "Unknown form in #+ or #- feature list -- ~S" FEATURE))))

(XR-TABLE-SETUP)

;Standard reader macros:
(DEFUN XR-QUOTE-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (PROG () (RETURN (LIST 'QUOTE (READ STREAM)) 'LIST)))

(DEFUN XR-COMMENT-MACRO (LIST-SO-FAR STREAM)
       (PROG (CH)
	   A (SETQ CH (FUNCALL STREAM ':TYI))
	     (AND (OR (NULL CH)
		      (EQ CH 215))
		  (RETURN LIST-SO-FAR NIL T))
	     (GO A)))

;;;BACKQUOTE:
;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a			;the NIL flag is used only when a is NIL
;;;      T: [a] => a			;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a)
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;    \ car   ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|      |
;;;  cdr \     ||		  |    T or NIL     |                |		      |
;;;====================================================================================
;;;    |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d]) |
;;;    NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a    |
;;; QUOTE or T || LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC  (a [d]) |
;;;   APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC  (a [d]) |
;;;   NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a . d) |
;;;    LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC  (a [d]) |
;;;    LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d]) |
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead of ",@a)"

(DECLARE (SPECIAL **BACKQUOTE-COUNT** **BACKQUOTE-/,-FLAG**
		  **BACKQUOTE-/,/@-FLAG** **BACKQUOTE-/,/.-FLAG**))

(SETQ **BACKQUOTE-COUNT** 0
      **BACKQUOTE-/,-FLAG** (MAKE-SYMBOL ",")
      **BACKQUOTE-/,/@-FLAG** (MAKE-SYMBOL ",@")
      **BACKQUOTE-/,/.-FLAG** (MAKE-SYMBOL ",.")
      )

;Expansions of backquotes actually use these five functions
;so that one can recognize what came from backquote and what did not.

(DEFMACRO XR-BQ-CONS (CAR CDR)
  `(CONS ,CAR ,CDR))

(DEFMACRO XR-BQ-LIST (&REST ELEMENTS)
  `(LIST . ,ELEMENTS))

(DEFMACRO XR-BQ-LIST* (&REST ELEMENTS)
  `(LIST* . ,ELEMENTS))

(DEFMACRO XR-BQ-APPEND (&REST ELEMENTS)
  `(APPEND . ,ELEMENTS))

(DEFMACRO XR-BQ-NCONC (&REST ELEMENTS)
  `(NCONC . ,ELEMENTS))

(DEFUN XR-BACKQUOTE-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (PROG ((FLAG NIL)
	      (THING NIL)
	      (**BACKQUOTE-COUNT** (1+ **BACKQUOTE-COUNT**)))
	     (MULTIPLE-VALUE (FLAG THING) (BACKQUOTIFY (READ STREAM '*EOF*)))
	     (AND (EQ FLAG **BACKQUOTE-/,/@-FLAG**)
		  (FERROR NIL
		    " /",@/" right after a /"`/": `,@~S" THING))
	     (AND (EQ FLAG **BACKQUOTE-/,/.-FLAG**)
		  (FERROR NIL
		    " /",./" right after a /"`/": `,.~S" THING))
	     (RETURN (BACKQUOTIFY-1 FLAG THING) 'LIST)))

(DEFUN XR-COMMA-MACRO (LIST-SO-FAR STREAM)
       LIST-SO-FAR ;not used
       (OR (> **BACKQUOTE-COUNT** 0)
	   (FERROR NIL
	     "Comma not inside a backquote"))
       (PROG ((C (FUNCALL STREAM ':TYI))
	      (**BACKQUOTE-COUNT** (1- **BACKQUOTE-COUNT**)))
	     (RETURN
	      (COND ((= C #/@)
		     (CONS **BACKQUOTE-/,/@-FLAG** (READ STREAM '*EOF*)))
		    ((= C #/.)
		     (CONS **BACKQUOTE-/,/.-FLAG** (READ STREAM '*EOF*)))
		    (T (FUNCALL STREAM ':UNTYI C)
		       (CONS **BACKQUOTE-/,-FLAG** (READ STREAM '*EOF*))))
	      'LIST)))

(DEFUN BACKQUOTIFY (CODE)
       (PROG (AFLAG A DFLAG D)
	     (COND ((ATOM CODE)
		    (COND ((NULL CODE) (RETURN NIL NIL))
			  ((OR (NUMBERP CODE)
			       (EQ CODE T))
			   (RETURN T CODE))
			  (T (RETURN 'QUOTE CODE))))
		   ((EQ (CAR CODE) **BACKQUOTE-/,-FLAG**)
		    (SETQ CODE (CDR CODE))
		    (GO COMMA))
		   ((EQ (CAR CODE) **BACKQUOTE-/,/@-FLAG**)
		    (RETURN **BACKQUOTE-/,/@-FLAG** (CDR CODE)))
		   ((EQ (CAR CODE) **BACKQUOTE-/,/.-FLAG**)
		    (RETURN **BACKQUOTE-/,/.-FLAG** (CDR CODE))))
             (MULTIPLE-VALUE (AFLAG A) (BACKQUOTIFY (CAR CODE)))
	     (MULTIPLE-VALUE (DFLAG D) (BACKQUOTIFY (CDR CODE)))
	     (AND (EQ DFLAG **BACKQUOTE-/,/@-FLAG**)
		  (FERROR NIL
		    " /",@/" after a /"./": .,@~S in ~S" D CODE))
	     (AND (EQ DFLAG **BACKQUOTE-/,/.-FLAG**)
		  (FERROR NIL
		    " /",./" after a /"./": .,.~S in ~S" D CODE))
	     (COND ((EQ AFLAG **BACKQUOTE-/,/@-FLAG**)
		    (COND ((NULL DFLAG)
			   (SETQ CODE A)
			   (GO COMMA)))
		    (RETURN 'APPEND
			    (COND ((EQ DFLAG 'APPEND)
				   (CONS A D))
				  (T (LIST A (BACKQUOTIFY-1 DFLAG D))))))
		   ((EQ AFLAG **BACKQUOTE-/,/.-FLAG**)
		    (COND ((NULL DFLAG)
			   (SETQ CODE A)
			   (GO COMMA)))
		    (RETURN 'NCONC
			    (COND ((EQ DFLAG 'NCONC)
				   (CONS A D))
				  (T (LIST A (BACKQUOTIFY-1 DFLAG D))))))
		   ((NULL DFLAG)
		    (COND ((MEMQ AFLAG '(QUOTE T NIL))
			   (RETURN 'QUOTE (LIST A)))
			  (T (RETURN 'LIST (LIST (BACKQUOTIFY-1 AFLAG A))))))
		   ((MEMQ DFLAG '(QUOTE T))
		    (COND ((MEMQ AFLAG '(QUOTE T NIL))
			   (RETURN 'QUOTE (CONS A D)))
			  (T (RETURN 'LIST* (LIST (BACKQUOTIFY-1 AFLAG A)
						  (BACKQUOTIFY-1 DFLAG D)))))))
	     (SETQ A (BACKQUOTIFY-1 AFLAG A))
	     (AND (MEMQ DFLAG '(LIST LIST*))
		  (RETURN DFLAG (CONS A D)))
	     (RETURN 'LIST* (LIST A (BACKQUOTIFY-1 DFLAG D)))
       COMMA (COND ((ATOM CODE)
		    (COND ((NULL CODE)
			   (RETURN NIL NIL))
			  ((OR (NUMBERP CODE)
			       (EQ CODE 'T))
			   (RETURN T CODE))
			  (T (RETURN **BACKQUOTE-/,-FLAG** CODE))))
		   ((EQ (CAR CODE) 'QUOTE)
		    (RETURN (CAR CODE) (CADR CODE)))
		   ((MEMQ (CAR CODE) '(APPEND LIST LIST* NCONC))
		    (RETURN (CAR CODE) (CDR CODE)))
		   ((EQ (CAR CODE) 'CONS)
		    (RETURN 'LIST* (CDR CODE)))
		   (T (RETURN **BACKQUOTE-/,-FLAG** CODE)))))

(DEFUN BACKQUOTIFY-1 (FLAG THING)
       (COND ((OR (EQ FLAG **BACKQUOTE-/,-FLAG**)
		  (MEMQ FLAG '(T NIL)))
	      THING)
	     ((EQ FLAG 'QUOTE)
	      (LIST 'QUOTE THING))
	     ((EQ FLAG 'LIST*)
	      (COND ((NULL (CDDR THING))
		     (CONS 'XR-BQ-CONS THING))
		    (T (CONS 'XR-BQ-LIST* THING))))
	     (T (CONS (CDR (ASSQ FLAG `((CONS . XR-BQ-CONS)
					(LIST . XR-BQ-LIST)
					(APPEND . XR-BQ-APPEND)
					(NCONC . XR-BQ-NCONC))))
		      THING))))


;; SETSYNTAX etc.
;; Note: INITIAL-READTABLE is set up by LISP-REINITIALIZE to be the initial
;; readtable (not a copy, should it be?).

;; Set the syntax of CHAR in A-READTABLE to be that of
;; KNOWN-CHAR in KNOWN-READTABLE.
(DEFUN SET-SYNTAX-FROM-CHAR (CHAR KNOWN-CHAR
			     &OPTIONAL (A-READTABLE READTABLE)
			               (KNOWN-READTABLE INITIAL-READTABLE))
       (SETF (RDTBL-BITS A-READTABLE CHAR) (RDTBL-BITS KNOWN-READTABLE KNOWN-CHAR))
       (SETF (RDTBL-CODE A-READTABLE CHAR) (RDTBL-CODE KNOWN-READTABLE KNOWN-CHAR)))

;; Set the translation of CHAR in A-READTABLE to VALUE.
(DEFUN SET-CHARACTER-TRANSLATION (CHAR VALUE &OPTIONAL (A-READTABLE READTABLE))
       (SETF (RDTBL-TRANS A-READTABLE CHAR) VALUE))

;;;; Set the character CHAR in A-READTABLE to be a single-character
;;;; macro with the given FUNCTION.
(DEFUN SET-SYNTAX-MACRO-CHAR (CHAR FUNCTION
			      &OPTIONAL (A-READTABLE READTABLE))
       (LET ((SYNTAX (GET (LOCF (RDTBL-PLIST A-READTABLE)) 'MACRO)))
	    (OR (AND (LISTP SYNTAX)
		     (FIXP (CAR SYNTAX))
		     (FIXP (CDR SYNTAX)))
		(FERROR NIL
		  "No saved syntax found for defining macro characters"))
	    (SETF (RDTBL-BITS A-READTABLE CHAR) (CAR SYNTAX))
	    (SETF (RDTBL-CODE A-READTABLE CHAR) (CDR SYNTAX))
	    (LET ((X (ASSQ CHAR (RDTBL-MACRO-ALIST A-READTABLE))))
		 (IF (NULL X)
		     (SETF (RDTBL-MACRO-ALIST A-READTABLE)
			   (CONS (CONS CHAR FUNCTION) (RDTBL-MACRO-ALIST A-READTABLE)))
		     (SETF (CDR X) FUNCTION))))
       CHAR)

;;;; Define a # character macro. Similar to SET-SYNTAX-MACRO-CHAR
(DEFUN SET-SYNTAX-/#-MACRO-CHAR (CHAR FUNCTION
				      &OPTIONAL (A-READTABLE READTABLE))
  (OR (< CHAR #/a)					;Upper case!
      (> CHAR #/z)
      (SETQ CHAR (- CHAR 32.)))
  (LET ((X (ASSQ CHAR (RDTBL-/#-MACRO-ALIST A-READTABLE))))
    (IF (NULL FUNCTION)
	(OR (NULL X) (SETF (RDTBL-/#-MACRO-ALIST A-READTABLE)
			   (DELQ X (RDTBL-/#-MACRO-ALIST A-READTABLE))))
	(IF (NULL X)
	    (SETF (RDTBL-/#-MACRO-ALIST A-READTABLE)
		  (CONS (CONS CHAR FUNCTION) (RDTBL-/#-MACRO-ALIST A-READTABLE)))
	    (SETF (CDR X) FUNCTION))))
  CHAR)
       

;; Set the syntax of CHAR to be DESCRIPTION. Sample DESCRIPTIONs: SINGLE, SLASH,
;; CIRCLECROSS, WHITESPACE etc.
(DEFUN SET-SYNTAX-FROM-DESCRIPTION (CHAR DESCRIPTION &OPTIONAL (A-READTABLE READTABLE))
       (LET ((SYNTAX (GET (LOCF (RDTBL-PLIST A-READTABLE)) DESCRIPTION)))
	    (OR (AND (LISTP SYNTAX)
		     (FIXP (CAR SYNTAX))
		     (FIXP (CDR SYNTAX)))
		(FERROR NIL
		  "No syntax of description: ~A found" DESCRIPTION))
	    (SETF (RDTBL-BITS A-READTABLE CHAR) (CAR SYNTAX))
	    (SETF (RDTBL-CODE A-READTABLE CHAR) (CDR SYNTAX))))

;; Retrieves the syntax of a character so you can save it or whatever.
(DEFUN GET-SYNTAX-BITS (CHAR &OPTIONAL (A-READTABLE READTABLE))
       (CONS (RDTBL-BITS A-READTABLE CHAR)
	     (RDTBL-CODE A-READTABLE CHAR)))

(DEFUN SET-SYNTAX-BITS (CHAR SYNTAX &OPTIONAL (A-READTABLE READTABLE))
       (SETF (RDTBL-BITS A-READTABLE CHAR) (CAR SYNTAX))
       (SETF (RDTBL-CODE A-READTABLE CHAR) (CDR SYNTAX)))

;; Returns a copy of the readtable
;; or copys the readtable into another readtable (and returns that)
(DEFUN COPY-READTABLE (&OPTIONAL (A-READTABLE READTABLE) (ANOTHER-READTABLE NIL))
       (LET ((X (ARRAY-DIMENSION-N 1 A-READTABLE))
	     (Y (ARRAY-DIMENSION-N 2 A-READTABLE))
	     (L (ARRAY-DIMENSION-N 0 A-READTABLE)))
	    (LET ((NEW-READTABLE (OR ANOTHER-READTABLE
				     (MAKE-ARRAY NIL
						 'ART-16B
						 (LIST X Y)
						 NIL
						 L))))
		 (DO I 0 (1+ I) (= I X)
		     (DO J 0 (1+ J) (= J Y)
			 (AS-2 (AR-2 A-READTABLE I J) NEW-READTABLE I J)))
		 (DO I 0 (1+ I) (= I L)
		     (STORE-ARRAY-LEADER (ARRAY-LEADER A-READTABLE I) NEW-READTABLE I))
		 ;; Certain elements of the leader should not be shared.
		 (SETF (RDTBL-MACRO-ALIST NEW-READTABLE)  ;Copy Two levels since
		       (DO ((L (RDTBL-MACRO-ALIST NEW-READTABLE) (CDR L))
			    (NL NIL (CONS (CONS (CAAR L) (CDAR L)) NL)))
			   ((NULL L)
			    (NREVERSE NL)))) ;gets smashed.
		 (SETF (RDTBL-/#-MACRO-ALIST NEW-READTABLE)  ;Copy Two levels since
		       (DO ((L (RDTBL-/#-MACRO-ALIST NEW-READTABLE) (CDR L))
			    (NL NIL (CONS (CONS (CAAR L) (CDAR L)) NL)))
			   ((NULL L)
			    (NREVERSE NL)))) ;gets smashed.
		 (SETF (RDTBL-PLIST NEW-READTABLE)
		       (APPEND (RDTBL-PLIST NEW-READTABLE) NIL))
                 (AND (NAMED-STRUCTURE-P A-READTABLE)
                      (MAKE-ARRAY-INTO-NAMED-STRUCTURE NEW-READTABLE))
		 NEW-READTABLE)))

;;; MacLisp compatible (sort of) setsyntax:

(DECLARE (SPECIAL SETSYNTAX-FUNCTION))

(DEFUN SETSYNTAX (CHAR MAGIC MORE-MAGIC)
       ;;Convert MacLisp character object (a symbol) to Lispm character object
       ;;(a fixnum).
       (SETQ CHAR (CHARACTER CHAR))
       ;;Keywords being used, so disable package feature.
       (IF (SYMBOLP MAGIC) (SETQ MAGIC (INTERN MAGIC "USER")))
       (COND ((EQ MAGIC ':MACRO)
	      ;;MacLisp reader macros get invoked on zero arguments.
	      (SET-SYNTAX-MACRO-CHAR CHAR
				     `(LAMBDA (*IGNORED* STANDARD-INPUT)
					      (,MORE-MAGIC))))
	     ((EQ MAGIC ':SPLICING)
	      (LET ((SETSYNTAX-FUNCTION MORE-MAGIC))
		(SET-SYNTAX-MACRO-CHAR CHAR
				       (CLOSURE '(SETSYNTAX-FUNCTION)
						#'SETSYNTAX-1))))
	     ((FIXP MAGIC)
	      (FERROR NIL
		"You cannot give a fixnum syntax to SETSYNTAX (~O)" MAGIC))
	     (T
	       (OR (NOT (FIXP MORE-MAGIC))
		   (SET-CHARACTER-TRANSLATION CHAR MORE-MAGIC))
	       (COND ((EQ MAGIC ':SINGLE)
		      (SET-SYNTAX-FROM-DESCRIPTION CHAR 'SINGLE))
		     ((NULL MAGIC))
		     (T
		       (SET-SYNTAX-FROM-CHAR CHAR (CHARACTER MAGIC))))))
       T)

(DEFUN SETSYNTAX-1 (LIST-SO-FAR STANDARD-INPUT)
       (PROG (LST)
	     (SETQ LST (FUNCALL SETSYNTAX-FUNCTION))
	     (COND ((NULL LST)
		    (RETURN LIST-SO-FAR NIL T))
		   ((MEMQ LIST-SO-FAR '(:TOPLEVEL :AFTER-DOT))
		    (IF (AND (NOT (ATOM LST))
			     (NULL (CDR LST)))
			(RETURN (CAR LST) NIL NIL)
			(FERROR NIL
			    ;;*** Note- you can't have carriage returns in strings in files
			    ;;*** that are in the cold load, due to unfortunate misfeature.
			    "A SPLICING macro defined with SETSYNTAX attempted to return ~S~% in the context: ~S" LST LIST-SO-FAR)))
		   (T
		    (RETURN (NCONC LIST-SO-FAR LST) NIL T)))))

;;; MacLisp compatible (sort of) setsyntax-sharp-macro

(DECLARE (SPECIAL SETSYNTAX-SHARP-MACRO-FUNCTION
		  SETSYNTAX-SHARP-MACRO-CHARACTER))

(DEFUN SETSYNTAX-SHARP-MACRO (CHAR TYPE FUN &OPTIONAL (RDTBL READTABLE)
			      &AUX ALIST)
  (SETQ CHAR (CHARACTER CHAR))
  (OR (< CHAR #/a)
      (> CHAR #/z)
      (SETQ CHAR (- CHAR 32.)))
  (AND (SYMBOLP TYPE) (SETQ TYPE (INTERN TYPE "USER")))
  (LET ((X (ASSQ CHAR (SETQ ALIST (RDTBL-/#-MACRO-ALIST RDTBL))))
	(SETSYNTAX-SHARP-MACRO-FUNCTION FUN)
	(SETSYNTAX-SHARP-MACRO-CHARACTER
	  (SELECTQ TYPE
	    ((:PEEK-MACRO :PEEK-SPLICING) CHAR)
	    ((:MACRO :SPLICING) NIL))))
    (IF (NULL FUN)
	(OR (NULL X)
	    (SETF (RDTBL-/#-MACRO-ALIST RDTBL)
		  (DELQ X ALIST)))
	(LET ((F (SELECTQ TYPE
		   ((:MACRO :PEEK-MACRO)
		    (CLOSURE '(SETSYNTAX-SHARP-MACRO-FUNCTION
				SETSYNTAX-SHARP-MACRO-CHARACTER)
			     #'SETSYNTAX-SHARP-MACRO-1))
		   ((:SPLICING :PEEK-SPLICING)
		    (CLOSURE '(SETSYNTAX-SHARP-MACRO-FUNCTION
				SETSYNTAX-SHARP-MACRO-CHARACTER)
			     #'SETSYNTAX-SHARP-MACRO-2))
		   (OTHERWISE
		     (FERROR NIL
			     "SETSYNTAX-SHARP-MACRO never heard of the type ~S" TYPE)))))
	  (IF (NULL X)
	      (SETF (RDTBL-/#-MACRO-ALIST RDTBL)
		    (CONS (CONS CHAR F) ALIST))
	      (RPLACD X F)))))
  CHAR)

(DEFUN SETSYNTAX-SHARP-MACRO-1 (LIST-SO-FAR STANDARD-INPUT)
       LIST-SO-FAR	;Ignored
       (OR (NULL SETSYNTAX-SHARP-MACRO-CHARACTER)
	   (FUNCALL STANDARD-INPUT ':UNTYI SETSYNTAX-SHARP-MACRO-CHARACTER))
       (FUNCALL SETSYNTAX-SHARP-MACRO-FUNCTION XR-SHARP-ARGUMENT))

(DEFUN SETSYNTAX-SHARP-MACRO-2 (LIST-SO-FAR STANDARD-INPUT)
       (PROG (LST)
	     (OR (NULL SETSYNTAX-SHARP-MACRO-CHARACTER)
		 (FUNCALL STANDARD-INPUT ':UNTYI SETSYNTAX-SHARP-MACRO-CHARACTER))
	     (SETQ LST (FUNCALL SETSYNTAX-SHARP-MACRO-FUNCTION XR-SHARP-ARGUMENT))
	     (COND ((NULL LST)
		    (RETURN LIST-SO-FAR NIL T))
		   ((MEMQ LIST-SO-FAR '(:TOPLEVEL :AFTER-DOT))
		    (IF (AND (NOT (ATOM LST))
			     (NULL (CDR LST)))
			(RETURN (CAR LST) NIL NIL)
			(FERROR NIL
			  "A SPLICING sharp macro defined with SETSYNTAX-SHARP-MACRO attempted~% to return ~S in the context: ~S" LST LIST-SO-FAR)))
		   (T
		    (RETURN (NCONC LIST-SO-FAR LST) NIL T)))))
