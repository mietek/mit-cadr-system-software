;LISP machine character level I/O stuff -*-LISP-*-
; ** (c) Copyright 1980 Massachusetts Institute of Technology **

; This file documents what a stream is, what the operations to streams
; are, and the initial setup of streams in the Lisp Machine environment.
; It also contains some of the basic declarations and functions.

;An I/O stream is presently a function which is called with first
;argument the symbolic name of an operation to be performed;  and
;succeeding arguments, arguments to that operation.  If you want to
;have more than one stream serviced by the same function, you
;make closures.
;
;Since a stream may find itself being called on any implemented options,
;it should be prepared to take that many arguments.  The correct thing
;to do is to take ARG1 as an optional and the rest of the arguments as a rest-arg.
;
;"Characters" are fixnums usually between 0 and 217, in any case 0-377 .
;Some streams e.g. keyboard input) give characters with control and
;meta bits added to them, producing values as high as 1777 .
;
;Operations are:
;	:WHICH-OPERATIONS returns a list of the operations the specific
;			stream can support.  This is the only operation which
;			every stream is required to support, and is not included
;			in the list WHICH-OPERATIONS returns.
;	:TYI		ARG1 is NIL to return NIL at EOF,
;			anything else to give an error with ARG1 as message.
;			Presently, input read with rubout processing in effect
;			is echoed, and other input is not.  If this doesn't prove
;			to be the right thing, it could be changed in the future.
;	:LISTEN		Returns NIL if no immediately-available input,
;			or non-NIL if one is available.  The character is
;			not removed from the "input stream."  
;	:UNTYI		ARG1 is character to be "put back".
;	:TYO		ARG1 is character to be typed out.
;	:CLOSE		no args.  Closes PDP10 file.
;	:READ-POINTER	return character position in file.
;	:CLEAR-INPUT	discard buffered input
;	:CLEAR-OUTPUT	discard buffered output
;	:FORCE-OUTPUT	Force buffered output to be sent to the device.
;	:FINISH		Await I/O completion.
;	:NAME		Returns the name of the file open.
;	:LINE-IN	read in a line at a time, returning a string.
;			ARG1 is NIL meaning don't bother copying line,
;			T meaning do copy it, or a number meaning
;			copy and return a string with a leader that
;			many words long.  The newline character at
;			the end of the line is not included.  If
;			there is no newline, because the end of the
;			file was reached, a second value of T is
;			returned.
;	:STRING-OUT	ARG1 is a string, whose characters are all
;			TYO'd to the stream.  When it exists, it
;			is faster than repeated TYO operations.
;	:LINE-OUT	ARG1 is a string.  Its characters are all
;			TYO'd, followed by a newline.  Sometimes this
;			is faster than a STRING-OUT;  eg, for buffer-point
;			streams, LINE-OUT of a string with the appropriate
;			size leader simply sticks the string into the buffer.
;	:RUBOUT-HANDLER	ARG1 is an a-list of options, valid ones described below:
;			(:FULL-RUBOUT foo) causes the values NIL and foo
;			to be returned if the user rubs out everything he typed.
;			(:REPROMPT function) calls function on the stream before the
;			buffered input whenever CLEAR, VT, or FORM is typed.  The
;			character typed is the second argument to the function.
;			(:PROMPT function) calls function on the stream
;			when the handler is first entered, as well as in the above cases.
;			(:PASS-THROUGH . chars) causes each of the characters in chars to
;			be treated as self-inserting, thus functions like qsend can read
;			until control-c or control-g.
;
;			ARG2 is a function to be called, and the rest of the
;			arguments will be passed as arguments to it.  While
;			control is inside that function, TYI operations on
;			this stream will be under control of the rubout handler.
;			If characters are rubbed out, control will be thrown
;			back out of that function, and it will be re-entered to
;			rescan the input before the rubbed-out characters.
;			Special care is taken to pass back multiple values correctly.
;			The symbol RUBOUT-HANDLER is T while control is inside
;			the rubout handler, NIL otherwise.  The rubout-handler
;			should not be called recursively.  BREAK binds
;			RUBOUT-HANDLER to NIL so a new catch WILL be established
;			by re-calling of the rubout handler.
;			There is only one RUBOUT-HANDLER variable even though there
;			might be several streams which handle rubout, since you
;			can only be reading from one at a time within a single stack group.
;			The symbol RUBOUT-HANDLER is also used as the catch-tag.
;	:FRESH-LINE	Output a newline unless output is already at the beginning
;			of a new line.
;	:READ-CURSORPOS Takes an argument which is either :CHARACTER or :PIXEL
;			and returns the horizontal and vertical cursorpositions
;			(two values) in the specified units.
;	:SET-CURSORPOS  Takes an argument to specify the units (:CHARACTER or :PIXEL)
;			followed by two arguments which are the X and Y positions to go to.
;	:TRIGGER-MORE	Triggers **MORE** processing on a stream that supports that.
;			A no-op on all other streams.
;       :PC-PPR		Hands back the PC-PPR this stream uses (for those streams which do).
;       :SET-PC-PPR     Simply set the PC-PPR this stream uses to ARG1.  Does not
;			do any TV-ACTIVATE-PC-PPRs or anything.
;The next two operations are special for GRIND, and only work
;on some streams.  GRIND takes an argument for whether to try to use them.
;	:UNTYO-MARK	returns an object which "points to" the current
;			amount of stuff outputted to this stream.
;	:UNTYO		ARG1 is a mark returned by UNTYO-MARK.  Backs up
;			output to that point.
;Other operations will be added in the future.

(SETQ STREAM-INPUT-OPERATIONS
      '(:TYI :LISTEN :UNTYI :LINE-IN :RUBOUT-HANDLER))

(SETQ STREAM-OUTPUT-OPERATIONS
      '(:TYO :FORCE-OUTPUT :FINISH :STRING-OUT :LINE-OUT :FRESH-LINE :UNTYO-MARK :UNTYO))

; Naming conventions:
;   Symbols whose names end in "-INPUT", "-OUTPUT", or "-IO" should
;      normally be BOUND to streams; which of the three you use depends on
;      what directions the stream normally supports.
;   Symbols whose names end in "-STREAM" are DEFINED as streams.


; Synonyms.
; MAKE-SYN-STREAM takes a symbol, and returns a stream which will forward all operations
;   to the binding of the symbol.  After (SETQ BAR (MAKE-SYN-STREAM 'FOO)), one says
;   that BAR is SYNned to FOO.


; The initial environment.
;   The initial binding of streams (set up by LISP-REINITIALIZE) is
;      as follows:
;   TERMINAL-IO     - This is how to get directly to the user's terminal.  It is set
;                     up to go to the TV initially.  Other places it might go are to
;                     the SUPDUP server, etc.  It is initially bound to a TV-MAKE-STREAM
;                     of CONSOLE-IO-PC-PPR.
;   STANDARD-INPUT  - This is initially bound to SYN to TERMINAL-IO.
;   STANDARD-OUTPUT - This is initially bound to SYN to TERMINAL-IO. STANDARD-INPUT
;                     and STANDARD-OUTPUT are the default streams for READ, PRINT and
;                     other things.  STANDARD-OUTPUT gets hacked when the session is
;                     being scripted, for example.
;   ERROR-OUTPUT    - This is where error messages should eventually get sent. Initially
;                     SYNned to TERMINAL-IO.
;   QUERY-IO        - This is for unexpected user queries
;                     of the "Do you really want to ..." variety.  Initially SYNned to
;                     TERMINAL-IO.  It supersedes "QUERY-INPUT".
;   TRACE-OUTPUT    - Output produced by TRACE goes here.  Initially SYNned to ERROR-OUTPUT.


(DECLARE
(SPECIAL STANDARD-INPUT		;DEFAULT STREAM FOR INPUT
	 STANDARD-OUTPUT	;DEFAULT STREAM FOR OUTPUT
	 TERMINAL-IO		;STREAM GUARANTEED TO BE CONNECTED TO THE TERMINAL
	 ERROR-OUTPUT		;STREAM UNANTICIPATED MESSAGES GO TO (SEE ALSO TRACE-OUTPUT)
	 QUERY-IO		;STREAM UNANTICIPATED QUESTIONS SHOULD GO TO AND GET
							;ANSWERS FROM.
	 RUBOUT-HANDLER		;T IF CONTROL IS INSIDE THE RUBOUT HANDLER, TYI FROM BUFFER
))

;;; For purposes of the arguments to READ, a stream specifier is T or
;;; NIL or a stream.  A stream is an instance, an entity, a closure, a fef,
;;; or a symbol with an SI:IO-STREAM-P property.
;;; T means TERMINAL-IO and NIL means STANDARD-INPUT.
(DEFUN IO-STREAM-P (X)
  (SELECT (%DATA-TYPE X)
    (DTP-INSTANCE T)
    (DTP-ENTITY T)
    (DTP-CLOSURE T)
    (DTP-FEF-POINTER T)
    (DTP-SYMBOL (GET X 'IO-STREAM-P))
    (T NIL)))

;;; Given the 2 arguments to READ (or TYI or READCH or TYIPEEK or READLINE)
;;; this returns the input stream and the eof option.  Note that
;;; the first arg would rather be the stream than the eof option.
;;; This is set up for Maclisp compatibility.
;;; HOWEVER, if the second argument is NIL or unsupplied, the first is
;;; assumed to be a stream, which is not compatible with Maclisp but more winning
(DEFUN DECODE-READ-ARGS (ARG1 ARG2)
  (PROG NIL  ;to return multiple values
    (COND ((OR (EQ ARG1 NIL) (EQ ARG1 T) (IO-STREAM-P ARG1))
	   (RETURN (COND ((EQ ARG1 NIL) STANDARD-INPUT)
			 ((EQ ARG1 T) TERMINAL-IO)
			 (T ARG1))
		   ARG2))
	  ((OR (EQ ARG2 T) (IO-STREAM-P ARG2))
	   (RETURN (COND ((EQ ARG2 NIL) STANDARD-INPUT)
			 ((EQ ARG2 T) TERMINAL-IO)
			 (T ARG2))
		   ARG1))
	  (T (RETURN ARG1 ARG2))))) ;Neither one seems to be a stream, assume arg1

;;; Given the second argument to PRINT (and friends), return
;;; a stream, processing for Maclisp compatibility
(DEFUN DECODE-PRINT-ARG (X)
  (COND ((EQ X NIL) STANDARD-OUTPUT)
	((EQ X T) TERMINAL-IO)
	((IO-STREAM-P X) X)
	((AND (LISTP X) (OR (IO-STREAM-P (CAR X)) (EQ (CAR X) NIL) (EQ (CAR X) T)))
	 (FERROR NIL "Output to multiple streams ~S not yet supported." X))
	(T X))) ;Unrecognizable, hope funcalling it works.


(DEFUN MAKE-SYN-STREAM (STREAM-SYMBOL)
  ;; This has to work in the cold-load, where there is no FORMAT, STRING-APPEND, 
  ;; or even GENSYM.
  (LET ((NML (ARRAY-ACTIVE-LENGTH (GET-PNAME STREAM-SYMBOL))))
    (LET ((PNAME (MAKE-ARRAY P-N-STRING 'ART-STRING (+ NML 11.))))
      (COPY-ARRAY-CONTENTS (GET-PNAME STREAM-SYMBOL) PNAME)
      (COPY-ARRAY-PORTION "-SYN-STREAM" 0 11. PNAME NML (+ NML 11.))
      (LET ((PACKAGE (CAR (PACKAGE-CELL-LOCATION STREAM-SYMBOL)))) ;can't give as arg!
	(MULTIPLE-VALUE-BIND (SYM FLAG) (INTERN PNAME)
	  (AND FLAG (RETURN-ARRAY PNAME))
	  (%P-STORE-TAG-AND-POINTER (FUNCTION-CELL-LOCATION SYM)
				    DTP-EXTERNAL-VALUE-CELL-POINTER
				    (VALUE-CELL-LOCATION STREAM-SYMBOL))
	  (PUTPROP SYM T 'IO-STREAM-P)
	  SYM)))))

(LOCAL-DECLARE ((SPECIAL BROADCAST-STREAM-STREAMS))
;This only works for output.
;If a value is needed it is always the value returned by the last stream.
;:WHICH-OPERATIONS is probably the only thing with a meaningful value.
(DEFUN MAKE-BROADCAST-STREAM (&REST BROADCAST-STREAM-STREAMS)
  (SETQ BROADCAST-STREAM-STREAMS (COPYLIST BROADCAST-STREAM-STREAMS))
  (CLOSURE '(BROADCAST-STREAM-STREAMS)
	   (FUNCTION (LAMBDA (&REST ARGS)
		       (DO ((L BROADCAST-STREAM-STREAMS (CDR L)))
			   ((NULL (CDR L))	;Last one gets to return multiple values
			    (APPLY (CAR L) ARGS))
			 (APPLY (CAR L) ARGS))))))
)

(DEFUN STREAM-DEFAULT-HANDLER (FCTN OP ARG1 ARGS &AUX TEM)
  (PROG () ;For multiple-value return
    (SELECTQ OP
        (:LISTEN
	 (RETURN (COND ((SETQ TEM (FUNCALL FCTN ':TYI NIL))
			(FUNCALL FCTN ':UNTYI TEM)
			TEM))))
	((:CLEAR-OUTPUT :CLEAR-INPUT :FORCE-OUTPUT :FINISH :CLOSE)
	 (RETURN NIL))
	(:FRESH-LINE
	 (FUNCALL FCTN ':TYO #\CR))
	((:STRING-OUT :LINE-OUT)
	 (SETQ TEM (STRING ARG1))
	 (DO ((LEN (COND ((SECOND ARGS))
			 (T (STRING-LENGTH TEM))))
	      (I (COND ((FIRST ARGS)) (T 0))
		 (1+ I)))
	     ((>= I LEN) NIL)
	   (FUNCALL FCTN ':TYO (AR-1 TEM I)))
	 (AND (EQ OP ':LINE-OUT)
	      (FUNCALL FCTN ':TYO #\CR)))
	(:LINE-IN
	 (LET ((BUF (MAKE-ARRAY NIL ART-STRING 100 NIL (COND ((NUMBERP ARG1) ARG1)
							     (T 1)))))
	    (STORE-ARRAY-LEADER 0 BUF 0) ;Fill pointer
	    (RETURN BUF
		    (DO ((TEM (FUNCALL FCTN ':TYI NIL) (FUNCALL FCTN ':TYI NIL)))
			((OR (NULL TEM) (= TEM #\CR))
			 (ADJUST-ARRAY-SIZE BUF (ARRAY-ACTIVE-LENGTH BUF))
			 (NULL TEM))
		      (ARRAY-PUSH-EXTEND BUF TEM)))))
;	(:HANDLE-EXCEPTIONS NIL)
	(OTHERWISE
	 (RETURN (FERROR NIL "The stream operation ~S is not supported by ~S"
			 OP FCTN))))))
