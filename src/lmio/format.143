;; Function for printing or creating nicely formatted strings.   -*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

; FORMAT prints several arguments according to a control argument.
; The control argument is either a string or a list of strings and lists.
; The strings and lists are interpreted consecutively.
; Strings are for the most part just printed, except that the character ~
; starts an escape sequence which directs other actions.
; A ~ escape sequence has an (optional) numeric parameter followed by a mode character.
; These escape actions can use up one or more of the non-control arguments.
; A list in the control-argument list is also interpreted as an escape.
; Its first element is the mode, a symbol which may be any length,
; and its remaining elements are parameters.  The list (D 5) is equivalent
; to the ~ escape "~5D";  similarly, each ~ escape has an equivalent list.
; However, there are list escapes which have no ~ equivalent.

; Any undefined list escape is simply evaluated.

;Further documentation is at the head of the function FORMAT.

; (FORMAT <stream> <control arg> &REST <args>)
; If <stream> is NIL, cons up and return a string.
; If <stream> is T, use STANDARD-OUTPUT (saves typing).

(DECLARE (SPECIAL FORMAT-STRING		 ;The string used for (FORMAT NIL ...)
		  FORMAT-PACKAGE	 ;The FORMAT package.
		  CTL-STRING		 ;The control string.
		  CTL-LENGTH		 ;STRING-LENGTH of CTL-STRING.
		  CTL-INDEX		 ;Our current index into the control string.  This
					 ;  is used by the conditional command. (NYI)
		  ATSIGN-FLAG		 ;Modifier
		  COLON-FLAG		 ;Modifier
		  FORMAT-PARAMS		 ;Array for pushing parameters
		  FORMAT-TEMPORARY-AREA	 ;For temporary consing
		  FORMAT-ARGLIST	 ;The original arg list, for ~G.
		  LOOP-ARGLIST		 ;Loop arglist, for ~:^.
		  FORMAT-CHAR-TABLE	 ;Table of single-char symbols, for fast interning.
		  ROMAN-OLD
		  ))

(SETQ FORMAT-TEMPORARY-AREA COMPILER:FASL-TEMP-AREA) ;For now, use this one
(SETQ FORMAT-PACKAGE PACKAGE)	;The package I was loaded into

;; Make FORMAT-CHAR-TABLE into an array whose i'th element is the
;; symbol, in FORMAT-PACKAGE, whose pname is the character whose code is i.
;; This provides a fast alternative to INTERN-SOFT, for single-character symbols.
;; CHARS is the list of symbols to put in the table.
;; All single-character FORMAT operations must be included.
(DEFUN FORMAT-INIT-CHAR-TABLE (CHARS)
    (SETQ FORMAT-CHAR-TABLE (MAKE-ARRAY NIL 'ART-Q '(200)))
    (DO ((CHARS CHARS (CDR CHARS))) ((NULL CHARS))
       (ASET (CAR CHARS) FORMAT-CHAR-TABLE (AREF (GET-PNAME (CAR CHARS)) 0))))

(LET ((CHARS '(A B C D E F G O P R Q S T V X [ ] /; % /| < > * & ^ { } ~)))
     (FORMAT-INIT-CHAR-TABLE CHARS)
;Following is more of a hindrance than a help
;     (DOLIST (C CHARS)
;	     (REMPROP C 'FORMAT-CTL-ONE-ARG)
;	     (REMPROP C 'FORMAT-CTL-NO-ARG)
;	     (REMPROP C 'FORMAT-CTL-MULTI-ARG)
;	     (REMPROP C 'FORMAT-CTL-REPEAT-CHAR))
     )

(DEFPROP FORMAT-STRING-STREAM T SI:IO-STREAM-P)

;; (FORMAT NIL ...) outputs to this stream, which just puts the characters
;; into the string FORMAT-STRING.
(DEFUN FORMAT-STRING-STREAM (OP &OPTIONAL ARG1 &REST REST)
  (COND ((EQ OP ':TYO)
	 (ARRAY-PUSH-EXTEND FORMAT-STRING ARG1))
	((EQ OP ':STRING-OUT)
	 (LET ((FIRST (OR (CAR REST) 0))
	       (LAST (OR (CADR REST) (ARRAY-ACTIVE-LENGTH ARG1)))
	       (NEW-LENGTH))
	   (SETQ NEW-LENGTH (+ (ARRAY-LEADER FORMAT-STRING 0) (- LAST FIRST)))
	   (AND (< (ARRAY-LENGTH FORMAT-STRING) NEW-LENGTH)
		(ADJUST-ARRAY-SIZE FORMAT-STRING NEW-LENGTH))
	   (COPY-ARRAY-PORTION ARG1 FIRST LAST
			       FORMAT-STRING (ARRAY-LEADER FORMAT-STRING 0) NEW-LENGTH)
	   (STORE-ARRAY-LEADER NEW-LENGTH FORMAT-STRING 0)))
	((EQ OP ':READ-CURSORPOS)
	 (MULTIPLE-VALUE-CALL
	     ((LAMBDA ()
		(PROG ()
		      (RETURN (LET ((POS (STRING-REVERSE-SEARCH-SET
					     '(#\RETURN #\LINE #\FORM)
					     FORMAT-STRING)))
				(- (STRING-LENGTH FORMAT-STRING)
				   (IF POS (+ POS 1) 0)))
			      0))))))
	((EQ OP ':WHICH-OPERATIONS) '(:TYO :STRING-OUT :READ-CURSORPOS))
	(T (MULTIPLE-VALUE-CALL
	       (STREAM-DEFAULT-HANDLER 'FORMAT-STRING-STREAM OP ARG1 REST)))))

(DEFUN FORMAT (STREAM CTL-STRING &REST ARGS)
    "Format arguments according to a control string and print to a stream.  (If the stream
is T, STANDARD-OUTPUT is used; if NIL, a string is returned containing the formatted text.)
The control string is copied to the stream, but ~ indicates special formatting commands:
~D  ~mincol,padchar,commacharD   Print number as a decimal integer.
    ~:D  Print the comma character every three digits.
    ~@D  Always print the sign.   ~:@D  Both.
~O  Analogous to ~D, but prints in octal.
~F  ~F  Print a floating point number.   ~nF  Round it to n digits.
~E  ~E  Print a floating-point number in exponential notation.   ~nE  Round to n digits.
~R  ~R  Print number as an English cardinal number.
    ~:R  English ordinal number.   ~@R  Roman numeral.   ~:@R  Old Roman numeral.
    ~nR  Print number in radix n.  Thus ~8R = ~O, and ~10R = ~D.
    Extra parameters are as for ~D (~n,mincol,padchar,commacharR).
~A  Ascii output (PRINC).  Good for printing strings.  ~mincol,colinc,minpad,padcharA.
    ~@A  Right-justify the string.   ~:A  Make NIL print as ().  ~:@A  Both.
~S  Analogous to ~A, but uses PRIN1, not PRINC.
~C  Print a character.  Mouse characters print in standard format.
    ~C  Code, preceded by: control , meta , control-meta , hyper ˆ, super .   quotes.
    ~:C  Format effectors print as names.  Names of control bits (/"Control-/") precede.
    ~@C  Prints the character in READ format, using #// or #\.
    ~:@C  Like ~:C, but top/front/greek characters are followed by remark, e.g. /" (Top-S)/".
~*  Ignore an argument.   ~n*  Ignore n arguments.   ~:n*  Back up n arguments (default 1).
~%  Insert a newline.     ~n%  Insert n newlines.
~X  Insert a space.       ~nX  Insert n spaces.
~~  Insert a tilde.       ~n~  Insert n tildes.
~|  Insert a form.        ~n|  Insert n forms.
    ~:|  Do :CLEAR-SCREEN if the stream supports it, otherwise insert a form.   ~:n|  Similar.
~<cr>  Ignore a CR and following whitespace in the control string.
    ~:<cr> Ignore the CR, retain the whitespace.  ~@<cr> Retain the CR, ignore the whitespace.
~&  Do a :FRESH-LINE.     ~n&  Do a FRESH-LINE, then insert n-1 newlines.
~^  Terminate processing if no more arguments.  Within ~{...~}, just terminate the loop.
    ~n;  Terminate if n is zero.  ~n,m;  Terminate if n=m.  ~n,m,p;  Terminate if nmp.
    ~:^  When within ~:{...~}, ~^ terminates this iteration.  Use ~:^ to exit the loop.
~T  ~mincol,colincT  Tab to column mincol+p*colinc, for the smallest integer p possible.
    ~mincol,colinc:T  Same, but tabs in TV pixels rather than characters.
~Q  Apply next argument to no arguments.  ~a,b,c,...,zQ  Apply next argument to parameters
    a,b,c,...z.  In (Q ...) form, apply argument to unevaled parameters.
~P  Pluralize.  Insert /"s/", unless argument is 1.
    ~:P  Use previous argument, not next one (i.e. do ~:* first).
    ~@P  Insert /"y/" if argument is 1, otherwise insert /"ies/".   ~:@P  Both.
~G  Goto.  ~nG goes to the nth argument (0-origin).  Operates relative to ~{...~} lists.
~<  ~mincol,colinc,minpad,padchar<str0~;str1~;...~;strn~>  Do formatting for all formatting
    strings strj; then output all strings with padding between them at the ~; points.
    Each padding point must have at least minpad padding characters.  Subject to that,
    the total width must be at least mincol, and must be mincol+p*colinc for some p.
    If str0 is followed by ~:; instead of ~;, then str0 is not normally output, and the
    ~:; is not a padding point.  Instead, after the total width has been determined,
    if the text will not fit into the current line of output, then str0 is output before
    outputting the rest.  (Doesn't work when producing a string.)  An argument n (~:n;)
    means that the text plus n more columns must fit to avoid outputting str0.  A second
    argument m (~n,m:;) provides the line width to use instead of the stream's width.
    ~:<  Also have a padding point at the left.  Hence ~n:<x~> right-justifies x in n columns.
    ~@<  Also have a padding point at the right.   ~:@<  Both.   Hence ~n:@<x~> centers x.
~[  ~[str0~;str1~;...~;strn~]  Cases.  Argument selects one case to do.  If argument is not
    between 0 and n inclusive, then no alternative is performed.  If a parameter is given,
    then use the parameter instead of an argument.  (The only useful one is /"#/".)
    If the last string is preceded by ~:;, it is an /"else/" clause, and is processed if
    no other string is selected.
    One can also tag the cases explicitly by giving arguments to ~;.  In this case the
    first string must be null, and arguments to ~; tag the following string.  The
    argument is matched against the list of parameters for each ~;.  One can get ranges
    of tags by using ~:;.  Pairs of parameters serve as inclusive range limits.
    A ~:; with no parameters is still an /"else/" case.
    Example:  ~[~'+,'-,'*,'////;operator~:'A,'Z,'a,'z;letter~:'0,'9;digit~:;other~]
    will produce /"operator/", /"letter/", /"digit/", or /"other/" as appropriate.
    ~:[iffalse~;iftrue~]  The argument selects the first case if nil, the second if non-nil.
    ~@[str~]  If the argument is non-nil, then it is not swallowed, and str is processed.
    Otherwise, the nil is swallowed and str is ignored.  Thus ~@[~S~] will PRIN1 a
    non-null thing.
~{  ~{str~}  Use str as a format string for each element in the argument.  More generally,
    the argument is a list of things to be used as successive arguments, and str is used
    repeatedly as a format string until the arguments are exhausted (or ~^ is used).
    Within the iteration the commands ~* and ~G move among the iteration arguments,
    not among all the arguments given to FORMAT.
    ~n{str~} repeats the string at most n times.
    Terminating with ~:} forces str to be processed at least once.
    ~:{str}  The argument is a list of lists, and each repetition sees one sublist.
    ~@{str}  All remaining arguments are used as the list.
    ~:@{str}  Each remaining argument is a list.
    If the str within a ~{ is empty, then an argument (which must be a string) is used.
    This argument precedes any that are iterated over as loop arguments.
In place of a numeric parameter, one may use V, which uses an argument to supply the number;
or one may use #, which represents the number of arguments remaining to be processed;
or one may use 'x, which uses the ascii value of x (good for pad characters).
The control string may actually be a list of intermixed strings and sublists.  The first
atom in a sublist should be the name of a command, and remaining elements are parameters."
    (COND ((NULL STREAM)
	   ;;; Only bind FORMAT-STRING if STREAM is NIL.  This avoids lossage if
	   ;;; FORMAT with a first arg of NIL is called recursively (e.g. if
	   ;;; printing a named structure).
	   (BIND (VALUE-CELL-LOCATION 'FORMAT-STRING)
		 (MAKE-ARRAY FORMAT-TEMPORARY-AREA 'ART-STRING '(200) NIL '(0))))
	  ((STRINGP STREAM)
	   (BIND (VALUE-CELL-LOCATION 'FORMAT-STRING) STREAM)))
    (LET ((STANDARD-OUTPUT (COND ((OR (NULL STREAM) (STRINGP STREAM)) 'FORMAT-STRING-STREAM)
				 ((EQ STREAM T) STANDARD-OUTPUT)
				 (T STREAM)))
	  (FORMAT-ARGLIST ARGS)
	  (LOOP-ARGLIST NIL))
      (*CATCH 'FORMAT-/:^-POINT
	 (*CATCH 'FORMAT-^-POINT
            (COND ((STRINGP CTL-STRING)
		   (FORMAT-CTL-STRING ARGS CTL-STRING))
		  ((SYMBOLP CTL-STRING)
		   (FORMAT-CTL-STRING ARGS (GET-PNAME CTL-STRING)))
		  (T (DO ((CTL-STRING CTL-STRING (CDR CTL-STRING))) ((NULL CTL-STRING))
			 (SETQ ARGS (COND ((STRINGP (CAR CTL-STRING))
					   (FORMAT-CTL-STRING ARGS (CAR CTL-STRING)))
					  (T (FORMAT-CTL-LIST ARGS (CAR CTL-STRING)))))))))))
    ;; Copy returned string out of temporary area and reclaim
    (COND ((NULL STREAM)
	   (PROG1 (SUBSTRING FORMAT-STRING 0)
		  (RETURN-ARRAY FORMAT-STRING)))
	  (T NIL)))

;;; Call this to signal an error in FORMAT processing.  If CTL-STRING is a string, then
;;; CTL-INDEX should point one beyond the place to be indicated in the error message.

(DEFUN FORMAT-ERROR (STRING &REST ARGS)
       (COND ((STRINGP CTL-STRING)
	      (FERROR NIL "~1{~:}~%~VT~%~3X/"~A/"~%" STRING ARGS (+ CTL-INDEX 3) CTL-STRING))
	     (T (FERROR NIL "~1{~:}" STRING ARGS))))

(DEFUN FORMAT-CTL-LIST (ARGS CTL-LIST &AUX (ATSIGN-FLAG NIL) (COLON-FLAG NIL))
    (FORMAT-CTL-OP (COND ((GETL (CAR CTL-LIST)
				'(FORMAT-CTL-ONE-ARG FORMAT-CTL-NO-ARG
				  FORMAT-CTL-MULTI-ARG FORMAT-CTL-REPEAT-CHAR))
			  (CAR CTL-LIST))
			 (T (INTERN-LOCAL-SOFT (CAR CTL-LIST) FORMAT-PACKAGE)))
		   ARGS (CDR CTL-LIST)))

(DEFUN FORMAT-CTL-STRING (ARGS CTL-STRING)
    (DO ((CTL-INDEX 0)
	 (TEM)
	 (STR)
	 (CTL-LENGTH (STRING-LENGTH CTL-STRING)))
	((>= CTL-INDEX CTL-LENGTH) ARGS)
	(SETQ TEM (OR (STRING-SEARCH-CHAR #/~ CTL-STRING CTL-INDEX) CTL-LENGTH))
	(COND ((NEQ TEM CTL-INDEX)		;Put out some literal string
	       (SETQ STR (NSUBSTRING CTL-STRING CTL-INDEX TEM FORMAT-TEMPORARY-AREA))
	       (FUNCALL STANDARD-OUTPUT ':STRING-OUT STR)
	       (RETURN-ARRAY STR)
	       (AND (>= (SETQ CTL-INDEX TEM) CTL-LENGTH)
		    (RETURN ARGS))))
	;; (AREF CTL-STRING CTL-INDEX) is a tilde.
	(LET ((ATSIGN-FLAG NIL)
	      (COLON-FLAG NIL)
	      (FORMAT-PARAMS (MAKE-ARRAY FORMAT-TEMPORARY-AREA 'ART-Q-LIST 10. NIL '(0))))
	     (MULTIPLE-VALUE (TEM ARGS) (FORMAT-PARSE-COMMAND ARGS T))
	     (SETQ ARGS (FORMAT-CTL-OP TEM ARGS (G-L-P FORMAT-PARAMS)))
	     (RETURN-ARRAY FORMAT-PARAMS))))

;;; Expects ATSIGN-FLAG, COLON-FLAG, and FORMAT-PARAMS to be bound.
;;; CTL-INDEX points to a tilde.  Returns command name and new ARGS,
;;; leaving CTL-INDEX after the command.  NIL for the command name
;;; means no command there
;;; If SWALLOW-ARGS is NIL, we are not executing commands, just parsing,
;;; e.g. to find a matching ~}, ~], or ~>.  So don't swallow any args (e.g. for ~V).
(DEFUN FORMAT-PARSE-COMMAND (ARGS SWALLOW-ARGS)
  (DO ((PARAM-FLAG NIL)		;If T, a parameter has been started in PARAM
       (START CTL-INDEX)	;for error message
       (CH)
       (TEM)
       (STR)
       (SYM)
       (PARAM NIL))		;PARAM is the parameter currently being constructed
      ((>= (SETQ CTL-INDEX (1+ CTL-INDEX)) CTL-LENGTH)
       (SETQ CTL-INDEX (1+ START))
       (FORMAT-ERROR "Command fell off end of control string"))
    (SETQ CH (AREF CTL-STRING CTL-INDEX))
    (AND (>= CH #/a) (<= CH #/z) (SETQ CH (- CH 40))) ;lower-case to upper
    (COND ((AND (>= CH #/0) (<= CH #/9))
	   (SETQ PARAM (+ (* (OR PARAM 0) 10.) (- CH #/0))
		 PARAM-FLAG T))
	  ((= CH #/@)
	   (SETQ ATSIGN-FLAG T))
	  ((= CH #/:)
	   (SETQ COLON-FLAG T))
	  ((= CH #/V)
	   (COND ((AND (NULL ARGS) SWALLOW-ARGS)
		  (SETQ CTL-INDEX (1+ CTL-INDEX))
		  (FORMAT-ERROR "No argument for V parameter to use")))
	   (SETQ PARAM (POP ARGS) PARAM-FLAG T))
	  ((= CH #/#)
	   (SETQ PARAM (LENGTH ARGS) PARAM-FLAG T))
	  ((= CH #/')
	   (SETQ PARAM (AREF CTL-STRING (SETQ CTL-INDEX (1+ CTL-INDEX))) PARAM-FLAG T))
	  ((= CH #/,)	;comma, begin another parameter
	   (ARRAY-PUSH FORMAT-PARAMS PARAM)
	   (SETQ PARAM NIL PARAM-FLAG T))  ;omitted arguments made manifest by the
					   ;presence of a comma come through as NIL
	  ((= CH #\CR)		;No command, just ignoring a CR
	   (SETQ CTL-INDEX (1+ CTL-INDEX))	;Skip the newline
	   (OR COLON-FLAG	;Unless colon, skip whitespace on the next line
	       (DO () ((OR (>= CTL-INDEX CTL-LENGTH)
			   (NOT (MEMQ (AREF CTL-STRING CTL-INDEX) '(#\SP #\TAB)))))
		 (SETQ CTL-INDEX (1+ CTL-INDEX))))
	   (RETURN 'CRLF ARGS))
	  (T			       ;Must be a command character
	      (SETQ CTL-INDEX (1+ CTL-INDEX))	;Advance past command character
	      (AND PARAM-FLAG (ARRAY-PUSH FORMAT-PARAMS PARAM))
	      (SETQ PARAM-FLAG NIL PARAM NIL TEM NIL)
	      ;; SYM gets the symbol for the operation to be performed.
	      ;; If SYM is NIL (and maybe otherwise), TEM gets a string
	      ;; which is the operation name as found in the control string.
	      (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		   (COND ((= CH #/\)
			  (LET ((I (STRING-SEARCH-CHAR #/\ CTL-STRING (1+ CTL-INDEX))))
			       (AND (NULL I)
				    (FORMAT-ERROR "Unmatched \ in control string."))
			       (SETQ TEM (NSUBSTRING CTL-STRING CTL-INDEX I))
			       (SETQ CTL-INDEX (1+ I))
			       (SETQ STR (STRING-UPCASE TEM))
			       (SETQ SYM (INTERN-SOFT STR FORMAT-PACKAGE))
			       (RETURN-ARRAY STR)
			       (RETURN-ARRAY TEM)))
			 (T (SETQ SYM (OR (AREF FORMAT-CHAR-TABLE CH)
					  (PROG2 (SETQ TEM (STRING CH))
						 (INTERN-SOFT TEM FORMAT-PACKAGE)
						 (RETURN-ARRAY TEM)))))))
	      (RETURN SYM ARGS)))))

;Perform a single formatted output operation on specified args.
;Return the remaining args not used up by the operation.
(DEFUN FORMAT-CTL-OP (OP ARGS PARAMS &AUX TEM)
    (COND ((NULL OP) (FORMAT-ERROR "Undefined FORMAT command.") ARGS) ;e.g. not interned
	  ((SETQ TEM (GET OP 'FORMAT-CTL-ONE-ARG))
	   (FUNCALL TEM (CAR ARGS) PARAMS)
	   (CDR ARGS))
	  ((SETQ TEM (GET OP 'FORMAT-CTL-NO-ARG))
	   (FUNCALL TEM PARAMS)
	   ARGS)
	  ((SETQ TEM (GET OP 'FORMAT-CTL-MULTI-ARG))
	   (FUNCALL TEM ARGS PARAMS))
	  ((SETQ TEM (GET OP 'FORMAT-CTL-REPEAT-CHAR))
	   (FORMAT-CTL-REPEAT-CHAR (OR (CAR PARAMS) 1) TEM)
	   ARGS)
	  (T (FORMAT-ERROR "/"~S/" is not defined as a FORMAT command." OP)
	     ARGS)))

(DEFUN (CRLF FORMAT-CTL-NO-ARG) (IGNORE)
  (AND ATSIGN-FLAG (FUNCALL STANDARD-OUTPUT ':TYO #\CR)))

(DEFPROP D FORMAT-CTL-DECIMAL FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-DECIMAL (ARG PARAMS &OPTIONAL (BASE 10.)	;Also called for octal
			   &AUX (*NOPOINT T)
			        (WIDTH (FIRST PARAMS))
				(PADCHAR (SECOND PARAMS))
				(COMMACHAR (THIRD PARAMS))
				(PLUS-P (AND ATSIGN-FLAG (NUMBERP ARG) (NOT (MINUSP ARG)))))
  (SETQ PADCHAR (COND ((NULL PADCHAR) #\SP)
		      ((NUMBERP PADCHAR) PADCHAR)
		      (T (AREF (STRING PADCHAR) 0))))
  (SETQ COMMACHAR (COND ((NULL COMMACHAR) #/,)
			((NUMBERP COMMACHAR) COMMACHAR)
			(T (AREF (STRING COMMACHAR) 0))))
  (AND WIDTH (FORMAT-CTL-JUSTIFY WIDTH
				 (+ (FLATC ARG)
				    (IF PLUS-P 1 0)
				    (IF (AND COLON-FLAG (NUMBERP ARG))
					(// (1- (FLATC (ABS ARG))) 3)	;Number of commas
					0))
				 PADCHAR))
  (AND PLUS-P (FUNCALL STANDARD-OUTPUT ':TYO #/+))
  ;; This is PRINC rather than PRIN1 so you can have a string instead of a number
  (COND ((NOT (AND COLON-FLAG (NUMBERP ARG))) (PRINC ARG))
	;; Random hair with commas.  I'm not going to bother not consing.
	(T (COND ((MINUSP ARG) (FUNCALL STANDARD-OUTPUT ':TYO #/-) (SETQ ARG (- ARG))))
	   (SETQ ARG (NREVERSE (INHIBIT-STYLE-WARNINGS ;Give up!
				 (EXPLODEN ARG))))
	   (DO ((L ARG (CDR L))
		(I 2 (1- I)))
	       ((NULL (CDR L)))
	     (COND ((ZEROP I)
		    (RPLACD L (CONS COMMACHAR (CDR L)))
		    (SETQ I 3 L (CDR L)))))
	   (DOLIST (CH (NREVERSE ARG))
	     (FUNCALL STANDARD-OUTPUT ':TYO CH)))))

(DEFPROP O FORMAT-CTL-OCTAL FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-OCTAL (ARG PARAMS)
  (FORMAT-CTL-DECIMAL ARG PARAMS 8))

(DEFPROP F FORMAT-CTL-F-FORMAT FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-F-FORMAT (ARG PARAMS)
  (AND (NUMBERP ARG) (NOT (FLOATP ARG)) (SETQ ARG (FLOAT ARG)))
  (IF (NOT (FLOATP ARG))
      (FORMAT-CTL-DECIMAL ARG NIL)
      (SI:PRINT-FLONUM ARG STANDARD-OUTPUT NIL (SMALL-FLOATP ARG)
		       (CAR PARAMS) NIL)))

(DEFPROP E FORMAT-CTL-E-FORMAT FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-E-FORMAT (ARG PARAMS)
  (AND (NUMBERP ARG) (NOT (FLOATP ARG)) (SETQ ARG (FLOAT ARG)))
  (IF (NOT (FLOATP ARG))
      (FORMAT-CTL-DECIMAL ARG NIL)
      (SI:PRINT-FLONUM ARG STANDARD-OUTPUT NIL (SMALL-FLOATP ARG)
		       (CAR PARAMS) T)))

(DEFPROP R FORMAT-CTL-ROMAN FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-ROMAN (ARG PARAMS)
  (COND ((CAR PARAMS) (FORMAT-CTL-DECIMAL ARG (CDR PARAMS) (CAR PARAMS)))
	((AND ATSIGN-FLAG
	      (< ARG 4000.)
	      (> ARG 0))
	 (LET ((ROMAN-OLD COLON-FLAG))
	   (ROMAN-STEP ARG 0)))
	(ATSIGN-FLAG
	 (LET ((BASE 10.) (*NOPOINT T))
	   (PRIN1 ARG)))
	((NOT COLON-FLAG)
	 (ENGLISH-PRINT ARG))
	(T (ENGLISH-ORDINAL-PRINT ARG))))

(DEFPROP A FORMAT-CTL-ASCII FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-ASCII (ARG PARAMS &OPTIONAL PRIN1P)
    (LET ((EDGE (CAR PARAMS))
	  (PERIOD (CADR PARAMS))
          (MIN (CADDR PARAMS))
	  (PADCHAR (CADDDR PARAMS)))
	 (COND ((NULL PADCHAR)
		(SETQ PADCHAR #\SP))
	       ((NOT (NUMBERP PADCHAR))
		(SETQ PADCHAR (CHARACTER PADCHAR))))
         (COND (ATSIGN-FLAG)			;~@5nA right justifies
	       ((AND COLON-FLAG (NULL ARG)) (PRINC "()"))
	       (PRIN1P (PRIN1 ARG))
               (T (PRINC ARG)))
	 (COND ((NOT (NULL EDGE))
		(LET ((WIDTH (FUNCALL (COND (PRIN1P 'FLATSIZE) (T 'FLATC)) ARG)))
		  (COND ((NOT (NULL MIN))
			 (FORMAT-CTL-REPEAT-CHAR MIN PADCHAR)
			 (SETQ WIDTH (+ WIDTH MIN))))
		  (COND (PERIOD
			 (FORMAT-CTL-REPEAT-CHAR
			  (- (+ EDGE (* (// (+ (- (MAX EDGE WIDTH) EDGE 1)
					       PERIOD)
					    PERIOD)
					PERIOD))
			     WIDTH)
			  PADCHAR))
			(T (FORMAT-CTL-JUSTIFY EDGE WIDTH PADCHAR))))))
         (COND ((NOT ATSIGN-FLAG))
	       ((AND COLON-FLAG (NULL ARG)) (PRINC "()"))
	       (PRIN1P (PRIN1 ARG))
               (T (PRINC ARG)))))

(DEFPROP S FORMAT-CTL-SEXP FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-SEXP (ARG PARAMS)
    (FORMAT-CTL-ASCII ARG PARAMS T))

(DEFPROP T FORMAT-CTL-TAB FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-TAB (PARAMS &AUX (DEST (OR (FIRST PARAMS) 1))
			           (EXTRA (OR (SECOND PARAMS) 1)))
    (COND ((MEMQ ':SET-CURSORPOS (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
	   (LET ((FLAVOR (IF COLON-FLAG ':PIXEL ':CHARACTER)))
;	     (FUNCALL STANDARD-OUTPUT ':HANDLE-EXCEPTIONS)
	     (LET (X Y)
	       (MULTIPLE-VALUE (X Y) (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS FLAVOR))
	       (FUNCALL STANDARD-OUTPUT ':SET-CURSORPOS
                        (+ DEST (* (// (+ (- (MAX DEST X) DEST) (1- EXTRA)) EXTRA) EXTRA))
			Y
			FLAVOR))))
	  (T (FUNCALL STANDARD-OUTPUT ':STRING-OUT "   "))))

(DEFPROP C FORMAT-CTL-CHARACTER FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-CHARACTER (ARG IGNORE &AUX CHNAME BITS)
    (SETQ ARG (CHARACTER ARG))
    (COND ((LDB-TEST %%KBD-MOUSE ARG)
	   (COND ((AND (NOT COLON-FLAG) ATSIGN-FLAG)
		  (OR (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		      (FORMAT-ERROR "~O unknown mouse character given to ~~@C" ARG))
		  (FUNCALL STANDARD-OUTPUT ':STRING-OUT "#\")
		  (PRIN1 CHNAME))
		 (T (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Mouse-")
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT (NTH (LDB 0003 ARG)
							       '("Left" "Middle" "Right")))
		    (FUNCALL STANDARD-OUTPUT ':STRING-OUT (NTH (LDB 0303 ARG)
							       '("" "-Twice" "-Thrice"))))))
          ((NOT COLON-FLAG)
	   (AND ATSIGN-FLAG (FUNCALL STANDARD-OUTPUT ':TYO #/#))
	   (FUNCALL STANDARD-OUTPUT ':STRING-OUT (NTH (LDB %%KBD-CONTROL-META ARG)
						      '("" "" "" ""
							"" "" "" ""
							"ˆ" "ˆ" "ˆ" "ˆ"
							"ˆ" "ˆ" "ˆ" "ˆ")))
	   (COND ((AND ATSIGN-FLAG
		       (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (LDB %%KBD-CHAR ARG))))
		  (FUNCALL STANDARD-OUTPUT ':TYO #/\)
		  (PRIN1 CHNAME))
		 (T (COND (ATSIGN-FLAG (FUNCALL STANDARD-OUTPUT ':TYO #//))
			  ((MEMQ ARG '(#/ #/ #/ #/ #/ˆ #/))
			   (FUNCALL STANDARD-OUTPUT ':TYO #/)))
		    (FUNCALL STANDARD-OUTPUT ':TYO (LDB %%KBD-CHAR ARG)))))
	  (T
	   (SETQ BITS (LDB %%KBD-CONTROL-META ARG))
	   (AND (BIT-TEST 8 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Hyper-"))
	   (AND (BIT-TEST 4 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Super-"))
	   (AND (BIT-TEST 1 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Control-"))
	   (AND (BIT-TEST 2 BITS) (FUNCALL STANDARD-OUTPUT ':STRING-OUT "Meta-"))
	   (SETQ ARG (LDB %%KBD-CHAR ARG))
	   (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
		  (LET ((DEFAULT-CONS-AREA FORMAT-TEMPORARY-AREA))
		    (LET ((STR (STRING-DOWNCASE CHNAME)))
		      (ASET (CHAR-UPCASE (AREF STR 0)) STR 0)
		      (FUNCALL STANDARD-OUTPUT ':STRING-OUT STR)
		      (RETURN-ARRAY STR))))
                 ((AND ATSIGN-FLAG (< ARG 40) ( ARG #/))
		  (FORMAT-PRINT-TOP-CHARACTER ARG))
                 (T (FUNCALL STANDARD-OUTPUT ':TYO ARG))))))

(DEFUN FORMAT-GET-CHARACTER-NAME (CHAR)
  (DO ((L SI:XR-SPECIAL-CHARACTER-NAMES (CDR L)))
      ((NULL L) NIL)
    (AND (= (CDAR L) CHAR) (RETURN (CAAR L)))))

(DEFUN FORMAT-PRINT-TOP-CHARACTER (CHAR &AUX NAME CHNAME)
  (FUNCALL STANDARD-OUTPUT ':TYO CHAR)
  (COND (( (LDB 0003 (%UNIBUS-READ 764102)) 1)	;Last character new keyboard?
	 (SETQ NAME " (Top-"			;No
	       CHNAME (DOTIMES (I 100)
			(AND (= CHAR (AREF SI:KBD-TRANSLATE-TABLE 2 I))
			     (RETURN (AREF SI:KBD-TRANSLATE-TABLE 1 I))))))
	((SETQ CHNAME (DOTIMES (I 200)
			(AND (= CHAR (AREF SI:KBD-NEW-TABLE 2 I))
			     (RETURN (AREF SI:KBD-NEW-TABLE 1 I)))))
	 (SETQ NAME " (Top-"))
	((SETQ CHNAME (DOTIMES (I 200)
			(AND (= CHAR (AREF SI:KBD-NEW-TABLE 3 I))
			     (RETURN (AREF SI:KBD-NEW-TABLE 0 I)))
			(AND (= CHAR (AREF SI:KBD-NEW-TABLE 4 I))
			     (RETURN (AREF SI:KBD-NEW-TABLE 1 I)))))
	 (SETQ NAME (IF (OR (AND ( CHNAME #/A) ( CHNAME #/Z))
			    (AND ( CHNAME #/a) ( CHNAME #/z)))
			" (Greek-" " (Front-"))))
  (COND (CHNAME	 
	 (FUNCALL STANDARD-OUTPUT ':STRING-OUT NAME)
	 ;; I'm not sure what to pass for the second arg, since it is not used.
	 ;; It currently doesn't matter.
	 (LET ((ATSIGN-FLAG NIL))
	   (FORMAT-CTL-CHARACTER CHNAME NIL))
	 (FUNCALL STANDARD-OUTPUT ':TYO #/)))))

(DEFPROP P FORMAT-CTL-PLURAL FORMAT-CTL-MULTI-ARG)
(DEFUN FORMAT-CTL-PLURAL (ARGS IGNORE)
    (AND COLON-FLAG (SETQ ARGS (FORMAT-CTL-IGNORE ARGS NIL)))	;crock: COLON-FLAG is set
    (COND (ATSIGN-FLAG (COND ((EQUAL (CAR ARGS) 1) (TYO #/y))
			     (T (FUNCALL STANDARD-OUTPUT ':STRING-OUT "ies"))))
	  (T (OR (EQUAL (CAR ARGS) 1) (TYO #/s))))
    (CDR ARGS))

(DEFPROP * FORMAT-CTL-IGNORE FORMAT-CTL-MULTI-ARG)
(DEFUN FORMAT-CTL-IGNORE (ARGS PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
       (COND (COLON-FLAG
		 (DO ((A FORMAT-ARGLIST (CDR A))
		      (B (NTHCDR COUNT FORMAT-ARGLIST) (CDR B)))
		     ((NULL A) (FORMAT-ERROR "Can't back up properly for a ~:*"))
		   (AND (EQ B ARGS) (RETURN A))))
	     (T (NTHCDR COUNT ARGS))))

(DEFPROP G FORMAT-CTL-GOTO FORMAT-CTL-MULTI-ARG)
(DEFUN FORMAT-CTL-GOTO (IGNORE PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
    (NTHCDR COUNT FORMAT-ARGLIST))

(DEFPROP % FORMAT-CTL-NEWLINES FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-NEWLINES (PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
    (DO I 0 (1+ I) (= I COUNT)
	(TERPRI)))

(DEFPROP & FORMAT-CTL-FRESH-LINE FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-FRESH-LINE (PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
    (FUNCALL STANDARD-OUTPUT ':FRESH-LINE)
    (DO I (1- COUNT) (1- I) (= I 0)
	(TERPRI)))

(DEFPROP ^ FORMAT-CTL-TERMINATE FORMAT-CTL-MULTI-ARG)
(DEFUN FORMAT-CTL-TERMINATE (ARGS PARAMS)
       (AND (IF (CAR PARAMS)
		(IF (CADR PARAMS)
		    (IF (CADDR PARAMS)
			(AND (NOT (> (CAR PARAMS) (CADR PARAMS)))
			     (NOT (> (CADDR PARAMS) (CADR PARAMS))))
			(= (CAR PARAMS) (CADR PARAMS)))
		    (ZEROP (CAR PARAMS)))
		(NULL (IF COLON-FLAG LOOP-ARGLIST ARGS)))
	    (*THROW (IF COLON-FLAG 'FORMAT-/:^-POINT 'FORMAT-^-POINT) NIL))
       ARGS)

(DEFPROP X   #\SP   FORMAT-CTL-REPEAT-CHAR)
(DEFPROP ~   #/~    FORMAT-CTL-REPEAT-CHAR)

(DEFUN FORMAT-CTL-REPEAT-CHAR (COUNT CHAR)
    (DO I 0 (1+ I) (= I COUNT)
	(TYO CHAR)))

(DEFPROP /| FORMAT-CTL-FORMS FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-FORMS (PARAMS)
  (COND ((AND COLON-FLAG (MEMQ ':CLEAR-SCREEN (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS)))
	 (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN))
	(T (FORMAT-CTL-REPEAT-CHAR (OR (CAR PARAMS) 1) #\FORM))))

;; Several commands have a SIZE long object which they must print
;; in a WIDTH wide field.  If WIDTH is specified and is greater than
;; the SIZE of the thing to be printed, this put out the right
;; number of  CHARs to fill the field.  You can call this before
;; or after printing the thing, to get leading or trailing padding.
(DEFUN FORMAT-CTL-JUSTIFY (WIDTH SIZE &OPTIONAL (CHAR #\SP))
    (AND WIDTH (> WIDTH SIZE) (FORMAT-CTL-REPEAT-CHAR (- WIDTH SIZE) CHAR)))

(DEFPROP Q FORMAT-CTL-APPLY FORMAT-CTL-ONE-ARG)
(DEFUN FORMAT-CTL-APPLY (ARG PARAMS)
    (APPLY ARG PARAMS))

(DECLARE (SPECIAL ENGLISH-SMALL ENGLISH-MEDIUM ENGLISH-LARGE ENGLISH-100
		  ENGLISH-ORDINAL-SMALL ENGLISH-ORDINAL-MEDIUM ENGLISH-ORDINAL-LARGE
		  ENGLISH-ORDINAL-100))

(DEFUN MAKE-LIST-ARRAY (LIST)
       (LET ((A (MAKE-ARRAY NIL 'ART-Q (LENGTH LIST))))
	    (FILLARRAY A LIST)
	    A))

(SETQ ENGLISH-SMALL (MAKE-LIST-ARRAY '("one" "two" "three" "four" "five" "six"
				       "seven" "eight" "nine" "ten" "eleven" "twelve"
				       "thirteen" "fourteen" "fifteen" "sixteen"
				       "seventeen" "eighteen" "nineteen")))

(SETQ ENGLISH-ORDINAL-SMALL (MAKE-LIST-ARRAY '("first" "second" "third" "fourth" "fifth"
					       "sixth" "seventh" "eighth" "ninth"
					       "tenth" "eleventh" "twelfth" "thirteenth"
					       "fourteenth" "fifteenth" "sixteenth"
					       "seventeenth" "eighteenth" "nineteenth")))

(SETQ ENGLISH-MEDIUM (MAKE-LIST-ARRAY '("twenty" "thirty" "forty" "fifty" "sixty" "seventy"
				        "eighty" "ninety")))

(SETQ ENGLISH-ORDINAL-MEDIUM (MAKE-LIST-ARRAY '("twentieth" "thirtieth" "fortieth"
						"fiftieth" "sixtieth" "seventieth"
						"eightieth" "ninetieth")))

(SETQ ENGLISH-LARGE (MAKE-LIST-ARRAY '("thousand" "million" "billion" "trillion" "quadrillion"
				       "quintillion")))

(SETQ ENGLISH-ORDINAL-LARGE (MAKE-LIST-ARRAY '("thousandth" "millionth" "billionth"
					       "trillionth" "quadrillionth" "quintillionth")))

(SETQ ENGLISH-100 "hundred"
      ENGLISH-ORDINAL-100 "hundredth")

(DEFUN ENGLISH-PRINT-THOUSAND (N STREAM)
       (LET ((N (\ N 100.))
	     (H (// N 100.)))
	    (COND ((> H 0)
		   (FUNCALL STREAM ':STRING-OUT (AREF ENGLISH-SMALL (1- H)))
		   (FUNCALL STREAM ':TYO #\SP)
		   (FUNCALL STREAM ':STRING-OUT ENGLISH-100)
		   (AND (> N 0) (FUNCALL STREAM ':TYO #\SP))))
	    (COND ((= N 0))
		  ((< N 20.)
		   (FUNCALL STREAM ':STRING-OUT (AREF ENGLISH-SMALL (1- N))))
		  (T
		   (FUNCALL STREAM ':STRING-OUT (AREF ENGLISH-MEDIUM (- (// N 10.) 2)))
		   (COND ((ZEROP (SETQ H (\ N 10.))))
			 (T
			  (FUNCALL STREAM ':TYO #/-)
			  (FUNCALL STREAM ':STRING-OUT (AREF ENGLISH-SMALL (1- H)))))))))

(DEFUN ENGLISH-PRINT (N &OPTIONAL (STREAM STANDARD-OUTPUT))
       (COND ((ZEROP N)
	      (FUNCALL STREAM ':STRING-OUT "zero"))
	     ((< N 0)
	      (FUNCALL STREAM ':STRING-OUT "minus")
	      (FUNCALL STREAM ':TYO #\SP)
	      (ENGLISH-PRINT (MINUS N) STREAM))
	     (T
	      (DO ((N N)
                   (P)
		   (FLAG)
		   (LIMIT 1000000.	  ;There is some cleverness here for bignums
			  (// LIMIT 1000.))
		   (I 1 (1- I)))
		  ((< I 0)
		   (COND ((> N 0)
			  (AND FLAG (FUNCALL STREAM ':TYO #\SP))
			  (ENGLISH-PRINT-THOUSAND N STREAM))))
		  (COND ((NOT (< N LIMIT))
			 (SETQ P (// N LIMIT)
			       N (\ N LIMIT))
			 (COND (FLAG (FUNCALL STREAM ':TYO #\SP))
			       (T (SETQ FLAG T)))
			 (ENGLISH-PRINT-THOUSAND P STREAM)
			 (FUNCALL STREAM ':TYO #\SP)
			 (FUNCALL STREAM ':STRING-OUT (AREF ENGLISH-LARGE I))))))))

(DEFUN ENGLISH-ORDINAL-PRINT (N &OPTIONAL (STREAM STANDARD-OUTPUT))
       (COND ((ZEROP N)
	      (FUNCALL STREAM ':STRING-OUT "zeroth"))
	     (T (DO ((I (IF (= (\ (// N 10.) 10.) 0) 10. 100.)			    
			    (* I 10.))
		     (TEM) (TEM1))
		    (( (SETQ TEM (\ N I)) 0)
		     (COND (( (SETQ TEM1 (- N TEM)) 0)
			    (ENGLISH-PRINT (- N TEM) STREAM)
			    (FUNCALL STREAM ':TYO  #\SP)))
		     (LET ((ENGLISH-SMALL (IF (AND (= (\ TEM 10.) 0)  ( TEM 10.))
					      ENGLISH-SMALL
					      ENGLISH-ORDINAL-SMALL))
			   (ENGLISH-MEDIUM (IF (= (\ TEM 10.) 0)
					       ENGLISH-ORDINAL-MEDIUM
					       ENGLISH-MEDIUM))
			   (ENGLISH-100 ENGLISH-ORDINAL-100)
			   (ENGLISH-LARGE ENGLISH-ORDINAL-LARGE))
		       (ENGLISH-PRINT TEM)))))))

(DEFUN ROMAN-STEP (X N)
    (COND ((> X 9.)
	   (ROMAN-STEP (// X 10.) (1+ N))
	   (SETQ X (\ X 10.))))
    (COND ((AND (= X 9) (NOT ROMAN-OLD))
	   (ROMAN-CHAR 0 N)
	   (ROMAN-CHAR 0 (1+ N)))
	  ((= X 5)
	   (ROMAN-CHAR 1 N))
	  ((AND (= X 4) (NOT ROMAN-OLD))
	   (ROMAN-CHAR 0 N)
	   (ROMAN-CHAR 1 N))
	  (T (COND ((> X 5)
		    (ROMAN-CHAR 1 N)
		    (SETQ X (- X 5))))
	     (DO I 0 (1+ I) (>= I X)
	       (ROMAN-CHAR 0 N)))))

(DEFUN ROMAN-CHAR (I X)
    (FUNCALL STANDARD-OUTPUT ':TYO (NTH (+ I X X) '(#/I #/V #/X #/L #/C #/D #/M))))

;Funny bases
(DEFUN (:ENGLISH SI:PRINC-FUNCTION) (X STREAM)
    (FORMAT STREAM "~R" (- X)))

(DEFUN (:ROMAN SI:PRINC-FUNCTION) (X STREAM)
   (FORMAT STREAM "~@R" (- X)))

(DEFUN (:ROMAN-OLD SI:PRINC-FUNCTION) (X STREAM)
   (FORMAT STREAM "~:@R" (- X)))

;;; Parse a set of clauses separated by ~; and terminated by ~closechar.
;;; (If SEMIP is nil, however, then ~; is ignored.)
;;; Returns an array; G-L-P of this array is a list whose length is a multiple of 3.
;;; Every three elements are <string> <bits> <paramarray>, where <string> is a control
;;; string separated by ~;, <bits> encodes the : and @ flags for the ~; or ~closechar
;;; that followed this string (: = 1, @ = 2), and <paramarray> is () or the parameter array
;;; for that ~; or ~closechar.  The arrays and strings are consed in the temporary area.
;;; FORMAT-RECLAIM-CLAUSES should be used to return the arrays and strings.

(DEFUN FORMAT-PARSE-CLAUSES (CLOSECHAR SEMIP)
  (DO ((I CTL-INDEX) (J) (TEM) (START (+ 4 (1- CTL-INDEX)))
       (CLAUSES (MAKE-ARRAY FORMAT-TEMPORARY-AREA 'ART-Q-LIST 30. NIL '(NIL 0)))
       (STACK (MAKE-ARRAY FORMAT-TEMPORARY-AREA 'ART-Q-LIST 10. NIL '(0))))
      (())					;do forever (until explicit return)
    (OR (SETQ J (SETQ CTL-INDEX (STRING-SEARCH-CHAR #/~ CTL-STRING CTL-INDEX)))
	(FERROR NIL
		"Missing ~{~*~~~A and ~}~~~A in format string:~%~VT~{~*~VT~}~%~3X/"~A/"~%"
		(G-L-P STACK) CLOSECHAR START (REVERSE (G-L-P STACK)) CTL-STRING))
    (LET ((ATSIGN-FLAG NIL)
	  (COLON-FLAG NIL)
	  (FORMAT-PARAMS (MAKE-ARRAY FORMAT-TEMPORARY-AREA 'ART-Q-LIST 10. NIL '(0))))
      (MULTIPLE-VALUE-BIND (COMMAND IGNORE) (FORMAT-PARSE-COMMAND NIL NIL)
	;; Now I points to beginning of clause, J to ~, and CTL-INDEX after command.
	(COND ((SETQ TEM (ASSQ COMMAND '(([ . ]) ({ . }) (< . >))))
	       (ARRAY-PUSH STACK (+ 4 (1- CTL-INDEX)))	;for error message only
	       (ARRAY-PUSH STACK (CDR TEM)))
	      ((NOT (NULL (G-L-P STACK)))
	       (COND ((EQ COMMAND (AREF STACK (1- (ARRAY-LEADER STACK 0))))
		      (ARRAY-POP STACK)		;pop twice
		      (ARRAY-POP STACK))))
	      ((OR (EQ COMMAND CLOSECHAR) (AND SEMIP (EQ COMMAND '/;)))
	       (COND ((NULL (G-L-P FORMAT-PARAMS))
		      (RETURN-ARRAY FORMAT-PARAMS)
		      (SETQ FORMAT-PARAMS NIL)))
	       (ARRAY-PUSH CLAUSES
			   (NSUBSTRING CTL-STRING I J FORMAT-TEMPORARY-AREA))
	       (ARRAY-PUSH CLAUSES (+ (IF COLON-FLAG 1 0) (IF ATSIGN-FLAG 2 0)))
	       (ARRAY-PUSH CLAUSES FORMAT-PARAMS)
	       (SETQ I CTL-INDEX)
	       (COND ((EQ COMMAND CLOSECHAR)
		      (STORE-ARRAY-LEADER STACK CLAUSES 1)
		      (RETURN CLAUSES))))
	      (T (RETURN-ARRAY FORMAT-PARAMS)))))))

(DEFUN FORMAT-RECLAIM-CLAUSES (CLAUSES)
       (DO ((I (ARRAY-LEADER CLAUSES 0) (- I 3)))
	   ((= I 0)
	    (RETURN-ARRAY (ARRAY-LEADER CLAUSES 1))
	    (RETURN-ARRAY CLAUSES))
	 (RETURN-ARRAY (AREF CLAUSES (- I 3)))
	 (AND (AREF CLAUSES (- I 1))
	      (RETURN-ARRAY (AREF CLAUSES (- I 1))))))

(DEFPROP /; FORMAT-CTL-DELIMIT-CASE FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-DELIMIT-CASE (IGNORE)
       (FORMAT-ERROR "Stray ~~; in FORMAT control string"))

(DEFPROP [ FORMAT-CTL-START-CASE FORMAT-CTL-MULTI-ARG)
(DEFUN FORMAT-CTL-START-CASE (ARGS PARAMS &AUX (ARG (CAR ARGS)))
    (COND (COLON-FLAG
	      (COND (ATSIGN-FLAG (FORMAT-ERROR "~~:@[ is not a defined FORMAT command"))
		    (T (SETQ ARG (COND (ARG 1) (T 0))) (POP ARGS))))
	  (ATSIGN-FLAG (SETQ ARG (COND (ARG 0) (T (POP ARGS) -1))))
	  ((CAR PARAMS) (SETQ ARG (CAR PARAMS)))
	  (T (POP ARGS)))
    (OR (NUMBERP ARG)
	(FORMAT-ERROR "The argument to the FORMAT /"~~[/" command must be a number"))
    (LET ((START CTL-INDEX)			;for error message only
	  (CLAUSES (FORMAT-PARSE-CLAUSES '] T)))
       (DO ((L (G-L-P CLAUSES) (CDDDR L))
	    (STATE (AND (NOT (ZEROP (STRING-LENGTH (CAR (G-L-P CLAUSES))))) 'SIMPLE)))
	   ((NULL (CDDDR L))
	    (LET ((STRING
		      (COND ((EQ STATE 'HAIRY)
			     (DO ((Z (G-L-P CLAUSES) (CDDDR Z)))
				 ((NULL (CDDDR Z)) NIL)
			       (AND (COND ((NULL (CADDR Z)) T)
					  ((ODDP (CADR Z))
					   (DO ((Q (G-L-P (CADDR Z)) (CDDR Q)))
					       ((NULL Q) NIL)
					     (AND (OR (NULL (CAR Q)) (NOT (< ARG (CAR Q))))
						  (OR (NULL (CADR Q)) (NOT (> ARG (CADR Q))))
						  (RETURN T))))
					  (T (MEMQ ARG (G-L-P (CADDR Z)))))
				    (RETURN (CADDDR Z)))))
			    (T (DO ((Z (G-L-P CLAUSES) (CDDDR Z))
				    (A ARG (1- A)))
				   ((NULL Z) NIL)
				 (AND (ZEROP A) (RETURN (CAR Z)))
				 (AND (ODDP (CADR Z))
				      (NOT (NULL (CDDDR Z)))
				      (RETURN (CADDDR Z))))))))
		 (LET ((NEWARGS (COND (STRING (FORMAT-CTL-STRING ARGS STRING))
				      (T ARGS))))
		      (FORMAT-RECLAIM-CLAUSES CLAUSES)
		      NEWARGS)))
	 (COND ((NOT (NULL (CADDR L)))
		(COND ((EQ STATE 'SIMPLE)
		       (SETQ CTL-INDEX START)
		       (FORMAT-ERROR "Mixture of simple and tagged clauses in ~~[")))
		(SETQ STATE 'HAIRY))
	       ((NOT (ODDP (CADR L)))
		(COND ((EQ STATE 'HAIRY)
		       (SETQ CTL-INDEX START)
		       (FORMAT-ERROR "Mixture of simple and tagged clauses in ~~[")))
		(SETQ STATE 'SIMPLE))))))

(DEFPROP ] FORMAT-CTL-END-CASE FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-END-CASE (IGNORE)
       (FORMAT-ERROR "Stray ~~] in FORMAT control string"))

(DEFPROP { FORMAT-ITERATE-OVER-LIST FORMAT-CTL-MULTI-ARG)
(DEFUN FORMAT-ITERATE-OVER-LIST (ARGS PARAMS)
       (LET ((LIMIT (OR (FIRST PARAMS) -1))
	     (CLAUSES (FORMAT-PARSE-CLAUSES '} NIL)))
	 (OR (NULL (CDDDR (G-L-P CLAUSES))) (FORMAT-ERROR "Bug in FORMAT's ~{ processor"))
	 (LET ((STR (CAR (G-L-P CLAUSES))))
	   (AND (ZEROP (STRING-LENGTH STR))
		(OR (STRINGP (SETQ STR (POP ARGS)))
		    (FORMAT-ERROR "~~{~~} argument not a string")))
	   (LET ((LOOP-ARGLIST (IF ATSIGN-FLAG ARGS (CAR ARGS))))
	     (*CATCH 'FORMAT-/:^-POINT
		     (*CATCH 'FORMAT-^-POINT
			     (DO ((OKAY-TO-EXIT (NOT (ODDP (CADR (G-L-P CLAUSES)))) T))
				 ((OR (AND OKAY-TO-EXIT (NULL LOOP-ARGLIST)) (= LIMIT 0)))
			       (COND ((NOT COLON-FLAG)
				      (LET ((FORMAT-ARGLIST LOOP-ARGLIST))
					(SETQ LOOP-ARGLIST
					      (FORMAT-CTL-STRING LOOP-ARGLIST STR))))
				     (T (LET ((FORMAT-ARGLIST (POP LOOP-ARGLIST)))
					  (*CATCH 'FORMAT-^-POINT
						  (FORMAT-CTL-STRING FORMAT-ARGLIST
								     STR)))))
			       (SETQ LIMIT (1- LIMIT)))))
	     (FORMAT-RECLAIM-CLAUSES CLAUSES)
	     (IF ATSIGN-FLAG LOOP-ARGLIST (CDR ARGS))))))

(DEFPROP } FORMAT-CTL-END-ITERATE-OVER-LIST FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-END-ITERATE-OVER-LIST (IGNORE)
       (FORMAT-ERROR "Stray ~~} in FORMAT control string"))

;;; This function is like FORMAT-CTL-STRING except that instead of sending to
;;; STANDARD-OUTPUT it sends to a string and returns that as its second value.
;;; The returned string is in the temporary area.
(DEFUN FORMAT-CTL-STRING-TO-STRING (ARGS STR)
  (LET ((FORMAT-STRING (MAKE-ARRAY FORMAT-TEMPORARY-AREA 'ART-STRING '(200) NIL '(0)))
	(STANDARD-OUTPUT 'FORMAT-STRING-STREAM))
    (PROG () (RETURN (FORMAT-CTL-STRING ARGS STR)
		     (ADJUST-ARRAY-SIZE FORMAT-STRING (ARRAY-ACTIVE-LENGTH FORMAT-STRING))))))


;This is not so hairy as to work with ~T, tabs, crs.  I really don't see how to do that.
;It makes a list of strings, then decides how much spacing to put in,
;then goes back and outputs.
(DEFPROP < FORMAT-HAIRY-JUSTIFICATION FORMAT-CTL-MULTI-ARG)
(DEFUN FORMAT-HAIRY-JUSTIFICATION (ARGS PARAMS)
  (LET ((MINCOL (OR (FIRST PARAMS) 0))
	(COLINC (OR (SECOND PARAMS) 1))
	(MINPAD (OR (THIRD PARAMS) 0))
	(PADCHAR (OR (FOURTH PARAMS) #\SP))
	(NEWLINE NIL)
	(EXTRA 0)
	(LINEWIDTH NIL)
	(STRINGS NIL)
	(STRING-NCOL 0)
	(CLAUSES)
	(N-PADDING-POINTS -1)
	(TOTAL-PADDING)
	(N-PADS)
	(N-EXTRA-PADS))
    (AND COLON-FLAG (SETQ N-PADDING-POINTS (1+ N-PADDING-POINTS)))
    (AND ATSIGN-FLAG (SETQ N-PADDING-POINTS (1+ N-PADDING-POINTS)))
    (*CATCH 'FORMAT-^-POINT
	    (PROGN (SETQ CLAUSES (FORMAT-PARSE-CLAUSES '> T))
		   (DO ((SPECS (G-L-P CLAUSES) (CDDDR SPECS)) (STR))
		       ((NULL SPECS)
			(SETQ STRINGS (NREVERSE STRINGS))
			(COND ((AND (G-L-P CLAUSES) (ODDP (CADR (G-L-P CLAUSES))))
			       (SETQ NEWLINE (POP STRINGS))
			       (AND (CADDR (G-L-P CLAUSES))
				    (SETQ EXTRA (OR (CAR (G-L-P (CADDR (G-L-P CLAUSES)))) 0)
					  LINEWIDTH (CADR (G-L-P (CADDR (G-L-P CLAUSES))))))
			       (SETQ STRING-NCOL (- STRING-NCOL (STRING-LENGTH NEWLINE)))
			       (SETQ N-PADDING-POINTS (1- N-PADDING-POINTS)))))
		     (MULTIPLE-VALUE (ARGS STR) (FORMAT-CTL-STRING-TO-STRING ARGS (CAR SPECS)))
		     (SETQ STRING-NCOL (+ (STRING-LENGTH STR) STRING-NCOL))
		     (SETQ N-PADDING-POINTS (1+ N-PADDING-POINTS))
		     (SETQ STRINGS (CONS-IN-AREA STR STRINGS FORMAT-TEMPORARY-AREA)))))
    (AND (ZEROP N-PADDING-POINTS)	;With no options and no ~; right-justify
	 (SETQ COLON-FLAG T N-PADDING-POINTS 1))
    ;; Get the amount of space needed to print the strings and MINPAD padding
    (SETQ TOTAL-PADDING (+ (* N-PADDING-POINTS MINPAD) STRING-NCOL))
    ;; Now bring in the MINCOL and COLINC constraint, i.e. the total width is
    ;; at least MINCOL and exceeds MINCOL by a multiple of COLINC, and
    ;; get the total amount of padding to be divided among the padding points
    (SETQ TOTAL-PADDING (- (+ MINCOL (* COLINC (// (+ (MAX (- TOTAL-PADDING MINCOL) 0)
						      (1- COLINC))
						   COLINC)))
			   STRING-NCOL))
    ;; Figure out whether a newline is called for or not.
    (COND ((AND NEWLINE
		(MEMQ ':READ-CURSORPOS (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
		(MULTIPLE-VALUE-BIND (X IGNORE)
		    (FUNCALL STANDARD-OUTPUT ':READ-CURSORPOS ':CHARACTER)
		  (> (+ X STRING-NCOL TOTAL-PADDING EXTRA) (OR LINEWIDTH 95.))))  ;linel (should be computed)
	   (FUNCALL STANDARD-OUTPUT ':STRING-OUT NEWLINE)))
    ;; Decide how many pads at each padding point + how many of the leftmost
    ;; padding points need one extra pad.
    (SETQ N-PADS (// TOTAL-PADDING N-PADDING-POINTS)
	  N-EXTRA-PADS (\ TOTAL-PADDING N-PADDING-POINTS))
    (OR (ZEROP N-EXTRA-PADS) (SETQ N-PADS (1+ N-PADS)))
    ;; Output the stuff
    (DO ((STRINGS STRINGS (CDR STRINGS))
	 (PAD-BEFORE-P COLON-FLAG T))
	((NULL STRINGS))
      (COND (PAD-BEFORE-P
	      (FORMAT-CTL-REPEAT-CHAR N-PADS PADCHAR)
	      (AND (ZEROP (SETQ N-EXTRA-PADS (1- N-EXTRA-PADS))) (SETQ N-PADS (1- N-PADS)))))
      (FUNCALL STANDARD-OUTPUT ':STRING-OUT (CAR STRINGS)))
    ;; Finally spacing at the right
    (AND ATSIGN-FLAG (FORMAT-CTL-REPEAT-CHAR N-PADS PADCHAR))
    ;; Reclamation
    (DOLIST (STR (NREVERSE STRINGS))
       (RETURN-ARRAY STR))
    (AND NEWLINE (RETURN-ARRAY NEWLINE))
    (FORMAT-RECLAIM-CLAUSES CLAUSES)
    ARGS))

(DEFPROP > FORMAT-CTL-END-HAIRY-JUSTIFICATION FORMAT-CTL-NO-ARG)
(DEFUN FORMAT-CTL-END-HAIRY-JUSTIFICATION (IGNORE)
       (FORMAT-ERROR "Stray ~~> in FORMAT control string"))


;; Less messy interface to list-printing stuff -- but it conses
(DEFUN PRINT-LIST (DESTINATION ELEMENT-FORMAT-STRING LIST
		   &OPTIONAL (SEPARATOR-FORMAT-STRING ", ")
			     (START-LINE-FORMAT-STRING "   ")
			     (TILDE-BRACE-OPTIONS ""))
  "Print the elements of list without lapping across line boundaries"
  (LET ((FSTRING (FORMAT NIL "~~~A{~~<~~%~A~~~D:;~A~~>~~^~A~~}"
			 TILDE-BRACE-OPTIONS
			 START-LINE-FORMAT-STRING
			 (STRING-LENGTH SEPARATOR-FORMAT-STRING)
			 ELEMENT-FORMAT-STRING
			 SEPARATOR-FORMAT-STRING)))
    (PROG1 (FORMAT DESTINATION FSTRING LIST)
	   (RETURN-ARRAY FSTRING))))
