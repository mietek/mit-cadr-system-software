;;;The error handler commands -*- Mode:LISP; Package:EH -*-

(DEFVAR WINDOW-ERROR-HANDLER NIL)			;Flag when inside window error handler

(DEFUN COMMAND-LOOP (ERROR-SG ETE &AUX FUNCTION SEXP 
				       (EVALHOOK NIL) PKG
				       (BASE 8) (IBASE 8)
				       (*NOPOINT NIL) (PACKAGE PACKAGE)
				       (WINDOW-ERROR-HANDLER NIL)
				       IO-BUFFER)
  ;; Discard type-ahead
  (COND ((MEMQ ':IO-BUFFER (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
	 (SETQ IO-BUFFER (FUNCALL STANDARD-INPUT ':IO-BUFFER))
	 (BIND (LOCF (TV:IO-BUFFER-OUTPUT-FUNCTION IO-BUFFER)) 'IO-BUFFER-OUTPUT-FUNCTION)
	 (BIND (LOCF (TV:IO-BUFFER-INPUT-FUNCTION IO-BUFFER)) NIL)))
  (FUNCALL STANDARD-INPUT ':CLEAR-INPUT)
  (DO ((NUMERIC-ARG)
       (-)
       (+ (SYMEVAL-IN-STACK-GROUP '- ERROR-SG))
       (* (SYMEVAL-IN-STACK-GROUP '* ERROR-SG)))
      (())
    (SETQ PKG (SYMEVAL-IN-STACK-GROUP 'PACKAGE ERROR-SG))
    (SETQ PACKAGE (IF (EQ (TYPEP PKG) 'PACKAGE) PKG SI:PKG-USER-PACKAGE))
    (*CATCH 'SI:TOP-LEVEL
      (*CATCH 'QUIT
	(PROGN
	  (OR NUMERIC-ARG
	      (FORMAT T "~&"))
	  ;; Read the next command or sexp, with combined rubout processing.
	  (MULTIPLE-VALUE (FUNCTION SEXP)
	    (COMMAND-LOOP-READ))
	  ;; If it's a character, execute the definition or complain.
	  (COND ((NUMBERP FUNCTION)
		 (SETQ NUMERIC-ARG
		       (IF (NULL NUMERIC-ARG) FUNCTION (+ FUNCTION (* 10. NUMERIC-ARG)))))
		(FUNCTION
		 (IF (NOT NUMERIC-ARG)
		     (FUNCALL FUNCTION ERROR-SG ETE)
		     (FUNCALL FUNCTION ERROR-SG ETE NUMERIC-ARG)
		     (SETQ NUMERIC-ARG NIL)))
		;; If there was no command, there was a sexp, so eval it.
		(T
		 (LET ((RESULTS (SG-EVAL ERROR-SG (SETQ - SEXP) T)))
		   (SETQ + -)
		   (COND ((NEQ RESULTS ERROR-FLAG)
			  (SETQ * (CAR RESULTS))
			  (MAPC 'PRINT RESULTS))))))
	  )))))

;; Read from STANDARD-INPUT either a control-character (or ? or Help)
;; or a s-expression.  Return CHAR or return NIL and the s-expression.
(DEFUN COMMAND-LOOP-READ ()
  (PROG (CHAR SEXP FLAG FUNCTION)
    RETRY
     ;; Read a character.
     (SETQ CHAR (FUNCALL STANDARD-INPUT ':TYI))
     ;; Now, if the char is special, echo and return it.
     (COND ((OR (LDB-TEST %%KBD-CONTROL-META CHAR)
		(= CHAR #\RESUME)
		(= CHAR #\HELP)
		(= CHAR #/?))
	    (COND ((SETQ FUNCTION (COMMAND-LOOKUP CHAR))
		   (AND (EQ FUNCTION 'COM-NUMBER)
			(SETQ FUNCTION (- (LDB %%CH-CHAR CHAR) #/0)))
		   (FORMAT T "~C" CHAR)
		   (RETURN FUNCTION))))
	   ((= CHAR #\RUBOUT) (GO RETRY)))	;Ignore rubouts
     ;; Otherwise, unread it and read an s-exp instead.
     (FUNCALL STANDARD-INPUT ':UNTYI CHAR)
     (COND ((AND (NOT RUBOUT-HANDLER)
		 (MEMQ ':RUBOUT-HANDLER (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS)))
	    (MULTIPLE-VALUE (SEXP FLAG)
	      (FUNCALL STANDARD-INPUT ':RUBOUT-HANDLER '((:FULL-RUBOUT :FULL-RUBOUT))
		       #'SI:READ-FOR-TOP-LEVEL))
	    (AND (EQ FLAG ':FULL-RUBOUT) (GO RETRY))
	    (RETURN NIL SEXP))
	   ;; If stream has no rubout handler, degrade gracefully.
	   ((RETURN NIL (SI:READ-FOR-TOP-LEVEL))))))

(DEFUN IO-BUFFER-OUTPUT-FUNCTION (IGNORE CHAR)
  (PROG ()
    (SELECTQ CHAR
      ((#/G #/g)
       (FORMAT T "~C" CHAR)
       (*THROW 'SI:TOP-LEVEL NIL))
      (#\BREAK
       (BREAK BREAK T)
       (RETURN CHAR T)))
    (RETURN CHAR NIL)))

(DEFUN COMMAND-LOOKUP (CHAR)
  (AREF COMMAND-DISPATCH-TABLE (LDB %%KBD-CONTROL-META CHAR)
			       (LDB %%KBD-CHAR (CHAR-UPCASE CHAR))))

;; Utility function used by the top level, and various commands.

;; Print out the error message of the error.
(DEFUN PRINT-ERROR-MESSAGE (SG ETE &OPTIONAL BRIEF-FLAG
				   &AUX (PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
					(PRINLENGTH ERROR-MESSAGE-PRINLENGTH)
					INFORM)
  (FORMAT T "~&>>")				;Flag beginning of error message 
  (PRINT-DEBUGGING-ERROR-MESSAGE SG ETE)	;Give type of error & microcode info
  (COND ((SETQ INFORM (GET (CAR ETE) 'INFORM))	;Give actual text of error message
	 (FUNCALL INFORM SG ETE))
	((EQ (CAR ETE) ':BREAK))
	(T
	 (FORMAT T "There is no error message provided for this error!~%")))
  (COND ((NOT BRIEF-FLAG)			;If not suppressed give backtrace
	 (FORMAT T "~&While in the function ")
	 (SHORT-BACKTRACE SG NIL (OR (GET (CAR ETE) 'BACKTRACE-LENGTH)
				     ERROR-MESSAGE-BACKTRACE-LENGTH)
			  (OR (GET (CAR ETE) 'BACKTRACE-SKIP)
			      0))
	 (TERPRI)
	 (AND					;Check for user message hook
	   (SETQ INFORM (SYMEVAL-IN-STACK-GROUP 'ERROR-MESSAGE-HOOK SG))
	   (FUNCALL INFORM))))	;Call user function to explain
  (FORMAT T "~&"))		;Compensate if it forgot to CRLF

;;; This function just prints the introduction telling you the type of error,
;;; and where it was in the microcode in the case of a microcode error
(DEFUN PRINT-DEBUGGING-ERROR-MESSAGE (SG ETE &AUX TEM FLAG)
  (COND ((EQ (CAR ETE) 'BREAK)
	 (PRINC "BREAK"))
	((EQ (CAR ETE) 'FERROR)
	 (PRINC "ERROR: "))
	(T
	 (FORMAT T ">>TRAP ~A ~A"
		 (SG-TRAP-MICRO-PC SG) ETE)
	 (COND ((LDB-TEST %%LP-EXS-MICRO-STACK-SAVED
		      (RP-EXIT-WORD (SG-REGULAR-PDL SG) (SG-AP SG)))
		;; Process the micro-stack of the active frame, if any
		(DO I (SG-SPECIAL-PDL-POINTER SG) (1- I) NIL
		    (LET ((PC (AREF (SG-SPECIAL-PDL SG) I)))
		      (SETQ TEM (ASSQ (1- (%POINTER PC)) CALLS-SUB-LIST))
		      (COND (TEM
			     (OR FLAG (FORMAT T " ->"))
			     (SETQ FLAG T)
			     (FORMAT T "  ~A " (CDR TEM)))))
		    (OR (ZEROP (%P-FLAG-BIT (ALOC (SG-SPECIAL-PDL SG) I)))
			(RETURN NIL)))))
	 (TERPRI))))

(DEFUN CONSTANT-FORM-P (X)
  (COND ((SYMBOLP X)
	 (OR (EQ X 'T) (NULL X)))
	((LISTP X) (EQ (CAR X) 'QUOTE))
	(T T)))

;; This is the function used by proceed routines to ask for
;; a form to read and eval.  When proceeding is done under control
;; of a condition handler, this function arranges to get the
;; object supplied by the handler instead of asking.
(DEFUN READ-OBJECT (PROMPT &AUX FORM OBJECT)
  (COND ((EQ CONDITION-PROCEED-FLAG T)
	 (SETQ CONDITION-PROCEED-FLAG 'GOBBLED)
	 CONDITION-PROCEED-VALUE)
	((EQ CONDITION-PROCEED-FLAG 'GOBBLED)
	 (FERROR NIL "READ-OBJECT called twice by proceed routine for ~S"
		 (CAR (SG-TRAP-TAG ERROR-SG))))
	(WINDOW-ERROR-HANDLER
	 (WINDOW-READ-OBJECT PROMPT))
	(T
	 (DO ()
	     ((PROGN
		(FORMAT T "~A~%" PROMPT)
		(SETQ FORM (SI:READ-FOR-TOP-LEVEL))
		(SETQ OBJECT (CAR (SG-EVAL ERROR-SG FORM)))
		(TERPRI)
		(COND ((CONSTANT-FORM-P FORM) T)
		      (T (FORMAT T "The object is ~S, ok? " OBJECT)
			 (Y-OR-N-P))))
	      OBJECT)))))

;;;When about to exit the error handler, maybe get rid of the window
(DEFUN LEAVING-ERROR-HANDLER ()
  (AND WINDOW-ERROR-HANDLER
       (TV:DELAYING-SCREEN-MANAGEMENT
	 (IF (EQ WINDOW-ERROR-HANDLER T)
	     (FUNCALL ERROR-HANDLER-WINDOW ':DESELECT T)
	     (FUNCALL WINDOW-ERROR-HANDLER ':SELECT))
	 ;;If this doesn't leave the window still on the screen, it is useless, so bury it.
	 (OR (TV:SHEET-EXPOSED-P ERROR-HANDLER-WINDOW)
	     (FUNCALL ERROR-HANDLER-WINDOW ':BURY)))))

;;;Continue the stack group, returning VAL if VAL-P is specified
(DEFUN PROCEED-SG (SG VAL-P &OPTIONAL VAL)
  (LEAVING-ERROR-HANDLER)
  (WITHOUT-INTERRUPTS
    (FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP)
    (IF VAL-P (FUNCALL SG VAL) (FUNCALL SG))))

;; Backtrace.
;; These initial implementations make no attempt to censor the stack,
;; but clearly something will be needed for interpreted functions.

;; Both versions take arguments the same way.  The first is the SG, and the
;; second is ignored so that the functions may be used as commands.
;; They will print no more than N frames if N is present.
;; If SKIP is nonzero, the first that many frames are skipped before the
;; N frames are printed.  Used to put them in curly brackets, but that was
;; fairly useless.

;; This prints out like the Maclisp BAKTRACE, and does not TERPRI at beginning
;; nor end.
(DEFUN SHORT-BACKTRACE (SG IGNORE &OPTIONAL (N 777777) (SKIP 0) UNINTERESTING-FLAG)
  (DO ((AP (SG-AP SG) (SG-PREVIOUS-ACTIVE SG AP))
       (RP (SG-REGULAR-PDL SG)))
      ((OR (NULL AP) (MINUSP (SETQ SKIP (1- SKIP))))
       (DO ((AP AP (FUNCALL (IF UNINTERESTING-FLAG #'SG-PREVIOUS-ACTIVE
				#'SG-PREVIOUS-INTERESTING-ACTIVE)
			    SG AP))
	    (I 0 (1+ I)))
	   ((OR (>= I N) (NULL AP)) NIL)
	 (OR (ZEROP I) (PRINC "  "))
	 (PRIN1 (FUNCTION-NAME (RP-FUNCTION-WORD RP AP)))))))

(DEFUN FULL-BACKTRACE (SG IGNORE &OPTIONAL (N 777777) (SKIP 0) UNINTERESTING-FLAG)
  (DO ((AP (SG-AP SG) (SG-PREVIOUS-ACTIVE SG AP)))
      ((OR (NULL AP) (MINUSP (SETQ SKIP (1- SKIP))))
       (DO ((AP AP (FUNCALL (IF UNINTERESTING-FLAG #'SG-PREVIOUS-ACTIVE
				#'SG-PREVIOUS-INTERESTING-ACTIVE)
			    SG AP))
	    (I 0 (1+ I)))
	   ((OR (>= I N) (NULL AP)) NIL)
	 (PRINT-FUNCTION-AND-ARGS SG AP)))))

(DEFUN FULL-BACKTRACE-UNINTERESTING (SG IGNORE &OPTIONAL (N 777777) (SKIP 0))
  (FULL-BACKTRACE SG NIL N SKIP T))

;Return list of the function and args that were invoked (as best as it can).
;Doesn't work, of course, for functions which modify their arguments.
;Note that this tries to get the original name of the function so that
;if it has been redefined and you are doing c-m-R the new version will be called.
(DEFUN GET-FRAME-FUNCTION-AND-ARGS (SG AP &AUX FUNCTION NARGS-SUPPLIED
					       (RP (SG-REGULAR-PDL SG))
					       LEXPR-CALL REST-ARG-VALUE ANS)
      (SETQ FUNCTION (RP-FUNCTION-WORD RP AP)
	    NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP))  ;Really slots on stack
      (MULTIPLE-VALUE (REST-ARG-VALUE NIL LEXPR-CALL)
	(SG-REST-ARG-VALUE SG AP))
      ;; Analyze the function
      (SETQ FUNCTION (FUNCTION-APPLICABLE-NAME FUNCTION))
      ;; Get the individual args.
      (DO ((I NARGS-SUPPLIED (1- I)))		;Cons them up in reverse order
	  ((ZEROP I))
	(SETQ ANS (CONS (AREF RP (+ AP I)) ANS)))   ;+1 -1
      ;; NCONC the rest arg if any was supplied separately from the regular args
      (AND LEXPR-CALL (SETQ ANS (NCONC ANS (COPYLIST REST-ARG-VALUE))))
      (CONS FUNCTION ANS))

;Given a function, returns the same or another function.  Tries to convert
;things to symbols, but in case always returns something you can APPLY.
(DEFUN FUNCTION-APPLICABLE-NAME (FUNCTION &AUX (NAME (FUNCTION-NAME FUNCTION)))
  (COND ((EQ NAME FUNCTION) FUNCTION)
	((ATOM NAME) NAME)
	((AND (EQ (CAR NAME) ':METHOD) (GET (CADR NAME) ':FLAVOR))
	 (SI:FLAVOR-METHOD-SYMBOL NAME))
	(T FUNCTION)))

(DEFUN PRINT-FUNCTION-AND-ARGS (SG AP &AUX FUNCTION (RP (SG-REGULAR-PDL SG)))
  (SETQ FUNCTION (FUNCTION-NAME (RP-FUNCTION-WORD RP AP)))
  (ERRSET (FORMAT T "~%~S:" FUNCTION) NIL)
  (AND (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
       (FORMAT T " (P.C. = ~O)"		;Note that this displays the return-pc,
	       (RP-EXIT-PC RP AP)))	; which is one greater than the D-LAST.  
  (TERPRI)
  (PRINT-FRAME-ARGS SG AP 3))

;; Returns T if it displayed the rest arg, NIL otherwise.  See SHOW-ALL-MACRO.
(DEFUN PRINT-FRAME-ARGS (SG AP INDENT
			 &AUX (PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
			      (PRINLENGTH ERROR-MESSAGE-PRINLENGTH)
			      FUNCTION NARGS-SUPPLIED NARGS-TO-PRINT
			      (RP (SG-REGULAR-PDL SG))
			      NARGS-EXPECTED NARGS-REQUIRED
			      LEXPR-CALL REST-ARG-P REST-ARG-VALUE)
  (SETQ FUNCTION (RP-FUNCTION-WORD RP AP)
	NARGS-SUPPLIED (RP-NUMBER-ARGS-SUPPLIED RP AP))
  (COND ((OR (= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER) (LISTP FUNCTION))
	 (SETQ NARGS-REQUIRED
	       (LDB %%ARG-DESC-MIN-ARGS (ARGS-INFO FUNCTION)))
	 (SETQ NARGS-EXPECTED
	       (LDB %%ARG-DESC-MAX-ARGS (ARGS-INFO FUNCTION)))))
  (MULTIPLE-VALUE (REST-ARG-VALUE REST-ARG-P LEXPR-CALL)
    (SG-REST-ARG-VALUE SG AP))
  (SETQ NARGS-TO-PRINT (SG-NUMBER-OF-SPREAD-ARGS SG AP))
  ;; Print the individual args.
  (DOTIMES (I NARGS-TO-PRINT)
    (AND (= I NARGS-SUPPLIED)
	 (COND ((AND NARGS-REQUIRED (< I NARGS-REQUIRED))
		(FORMAT T "   --Missing args:--~%"))
	       (T
		(FORMAT T "   --Defaulted args:--~%")))) ;These "args" weren't supplied
    (AND NARGS-EXPECTED (= I NARGS-EXPECTED)    ;Called with too many args
	 (FORMAT T "   --Extraneous args:--~%"))
    (FORMAT T "~VTArg ~D" INDENT I)
    (DISPLAY-ARG-NAME " (~A)" FUNCTION I)
    ;; Print the arg value unless the arg is missing (val is garbage).
    (OR (AND NARGS-REQUIRED
	     (> NARGS-REQUIRED NARGS-SUPPLIED)
	     ( I NARGS-SUPPLIED))
	(PROGN (PRINC ": ")
	       (ERRSET (PRIN1 (AREF RP (+ AP I 1))) NIL)))
    (TERPRI))
  ;; Print the rest arg if any.
  (COND (REST-ARG-P
	 (FORMAT T "~VTRest arg" INDENT)
	 (DISPLAY-LOCAL-NAME " (~A)" FUNCTION 0)
	 (PRINC ": "))
	(LEXPR-CALL
	 (FORMAT T "~VTExtraneous Rest Arg: " INDENT)))
  (COND ((OR REST-ARG-P LEXPR-CALL)
	 (ERRSET (PRIN1 REST-ARG-VALUE) NIL)
	 (TERPRI)))
  REST-ARG-P)

(DEFUN DISPLAY-LOCAL-NAME (FORMAT-STRING FUNCTION LOCALNO &AUX NAME)
  (SETQ NAME (LOCAL-NAME FUNCTION LOCALNO))
  (AND NAME (FORMAT T FORMAT-STRING NAME)))

(DEFUN DISPLAY-ARG-NAME (FORMAT-STRING FUNCTION ARGNO &AUX NAME)
  (SETQ NAME (ARG-NAME FUNCTION ARGNO))
  (AND NAME (FORMAT T FORMAT-STRING NAME)))

;; Commands in the dispatch table.  These are given the SG and the ETE.
;; Any command which wants to return out of the error handler should
;; do a throw to FINISHED after restarting the erring stack group.

;; Basic commands for inspecting specific stack frames.
;; UP means closer to the top of the stack, DOWN means the base of the stack.

;; Control-P, <^>
(DEFUN COM-UP-STACK (SG IGNORE &OPTIONAL COUNT SHOW-ALL-FLAG REVERSE-FLAG UNINTERESTING-FLAG
			       &AUX AP COUNT1)
  (SETQ COUNT1 (OR COUNT 1))
  (AND REVERSE-FLAG (SETQ COUNT1 (- COUNT1)))
  (SETQ AP (FUNCALL (IF UNINTERESTING-FLAG #'SG-NEXT-NTH-ACTIVE
			#'SG-NEXT-NTH-INTERESTING-ACTIVE)
		    SG CURRENT-FRAME COUNT1))
  (COND ((= AP CURRENT-FRAME)
	 (FORMAT T (COND (REVERSE-FLAG
			  "You are already at the bottom of the stack.~%")
			 (T "You are already at the top of the stack.~%"))))
	(T (SETQ CURRENT-FRAME AP)
	   (COND ((NOT SHOW-ALL-FLAG) (SHOW-FUNCTION-AND-ARGS SG))
		 (T (SHOW-ALL SG)))))
  NIL)

;; Control-N, <line>
(DEFUN COM-DOWN-STACK (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT NIL T))

;; Meta-P.
(DEFUN COM-UP-STACK-ALL (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT T))

;; Meta-N.
(DEFUN COM-DOWN-STACK-ALL (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT T T))

;; Meta->.
(DEFUN COM-TOP-STACK (SG &REST IGNORE)
  (SETQ CURRENT-FRAME (SG-OUT-TO-INTERESTING-ACTIVE SG (SG-AP SG)))
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Meta-<.
(DEFUN COM-BOTTOM-STACK (SG &REST IGNORE)
  (SETQ CURRENT-FRAME
	(DO ((AP (SG-AP SG) (SG-PREVIOUS-ACTIVE SG AP))
	     (PREV-AP NIL AP))
	    ((NULL AP) PREV-AP)))
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Control-Meta-P.
(DEFUN COM-UP-STACK-UNINTERESTING (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT NIL NIL T))

;; Control-Meta-N.
(DEFUN COM-DOWN-STACK-UNINTERESTING (SG ETE &OPTIONAL COUNT)
  (COM-UP-STACK SG ETE COUNT NIL T T))

;; Control-Meta-U.
(DEFUN COM-UP-TO-INTERESTING (SG IGNORE &OPTIONAL IGNORE)
  (SETQ CURRENT-FRAME (SG-OUT-TO-INTERESTING-ACTIVE SG CURRENT-FRAME))
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Control-L, form.
(DEFUN COM-CLEAR-AND-SHOW (SG ETE &REST IGNORE)
  (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
  (PRINT-ERROR-MESSAGE SG ETE)
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; This is how the error message is printed when the error handler starts up.
(DEFUN SHOW (SG ETE &REST IGNORE)
  (TERPRI)
  (PRINT-ERROR-MESSAGE SG ETE)
  (SHOW-FUNCTION-AND-ARGS SG)
  NIL)

;; Meta-L.
(DEFUN COM-CLEAR-AND-SHOW-ALL (SG &REST IGNORE)
  (SHOW-ALL SG)
  NIL)

(DEFUN COM-EDIT-FRAME-FUNCTION (SG &REST IGNORE)
  (LET ((RP (SG-REGULAR-PDL SG)))
    (ED (FUNCTION-NAME (RP-FUNCTION-WORD RP CURRENT-FRAME)))))

;; The guts of the commands on the previous page.

;; SHOW-FUNCTION-AND-ARGS is regular printing tty stuff.
;; SHOW-ALL clears the screen and then fill is up.
(DEFUN SHOW-FUNCTION-AND-ARGS (SG)
  (PRINT-FUNCTION-AND-ARGS SG CURRENT-FRAME))

(DEFUN SHOW-ALL (SG &AUX RP FUNCTION)
  (SETQ RP (SG-REGULAR-PDL SG)
	FUNCTION (RP-FUNCTION-WORD RP CURRENT-FRAME))
  (FUNCALL STANDARD-OUTPUT ':CLEAR-SCREEN)
  (SELECT (%DATA-TYPE FUNCTION)
    (DTP-FEF-POINTER (SHOW-ALL-MACRO SG RP FUNCTION))
    (OTHERWISE (SHOW-FUNCTION-AND-ARGS SG))))
	  
(DEFUN SHOW-ALL-MACRO (SG RP FUNCTION &AUX N-LOCALS PC-NOW NAME REST-ARG-PRINTED (NLINES 0)
			  LIM-PC)
  (SETQ N-LOCALS (FEF-NUMBER-OF-LOCALS FUNCTION)
	NAME (FEF-NAME FUNCTION)
	PC-NOW (RP-EXIT-PC RP CURRENT-FRAME)
	LIM-PC (COMPILER:DISASSEMBLE-LIM-PC FUNCTION))
  ;; Print the header, including the underlined function name
  (FORMAT T "~14XMacro-compiled frame.  Frame address = ~O~2%~S~2%"
	  CURRENT-FRAME NAME)
  ;; Print the arguments, including the rest-arg which is the first local
  (SETQ REST-ARG-PRINTED (PRINT-FRAME-ARGS SG CURRENT-FRAME 0))
  ;; Print the rest of the locals
  (DO ((I 0 (1+ I))
       (J (+ (RP-LOCAL-BLOCK-ORIGIN RP CURRENT-FRAME) CURRENT-FRAME) (1+ J)))
      ((>= I N-LOCALS))
    (COND ((NOT (AND REST-ARG-PRINTED (ZEROP I)))	;Don't show rest arg twice
	   (FORMAT T "Local ~D" I)
	   (DISPLAY-LOCAL-NAME " (~A)" FUNCTION I)
	   (LET ((PRINLEVEL ERROR-MESSAGE-PRINLEVEL)
		 (PRINLENGTH ERROR-MESSAGE-PRINLENGTH))
	     (FORMAT T ": ~S~%" (AREF RP J))))))
  (FORMAT T "~%Disassembled code:")
  ;; Figure out how many instructions will fit in the stream we are using.
  (AND (MEMQ ':SIZE-IN-CHARACTERS (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))
       (MULTIPLE-VALUE (NIL NLINES) (FUNCALL STANDARD-OUTPUT ':SIZE-IN-CHARACTERS)))
  ;; Disassemble assuming we have NLINES lines of space to use.
  ;; the 1- is to prevent moring, i dunno if it is right.
  (SETQ NLINES (MAX DISASSEMBLE-INSTRUCTION-COUNT (1- NLINES)))
  (DO ((I 0 (1+ I))
       (PC (MAX (FEF-INITIAL-PC FUNCTION) (- PC-NOW (// NLINES 2)))
	   (+ PC (COMPILER:DISASSEMBLE-INSTRUCTION-LENGTH FUNCTION PC))))
      ((OR ( I NLINES) ( PC LIM-PC))
       (COND ((= PC PC-NOW)				;If arrow should point after all code,
	      (TERPRI) (PRINC "=> "))))
    (TERPRI)
    (PRINC (IF (= PC PC-NOW) "=> " "   "))
    (COMPILER:DISASSEMBLE-INSTRUCTION FUNCTION PC)))

;; Other commands.

;; Control-Z.
(DEFUN COM-TOP-LEVEL-THROW (SG ETE &OPTIONAL (COUNT 1))
  ETE COUNT
  (LEAVING-ERROR-HANDLER)
  (COND ((NULL ERROR-HANDLER-RUNNING)
	 ;; Explicitly invoked error handler => return from it.
	 (*THROW 'EXIT NIL))
	((EQ SG SI:SCHEDULER-STACK-GROUP)
	 (FORMAT T "~&Restarting the scheduler.")
	 (COND (REAL-CURRENT-PROCESS
		(SI:PROCESS-BLAST REAL-CURRENT-PROCESS)
		(FORMAT T "~%Blasting ~S so this won't happen again" REAL-CURRENT-PROCESS)))
	 (STACK-GROUP-PRESET SG #'SI:PROCESS-SCHEDULER)
	 (SG-RUN-GOODBYE SG))
	(T
	 (COND ((AND (NEQ SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS))
		     (NOT (MEMQ SG (FUNCALL CURRENT-PROCESS ':COROUTINE-STACK-GROUPS))))
		(UNWIND-SG SG %CURRENT-STACK-GROUP NIL NIL)
		(SETQ SG (PROCESS-INITIAL-STACK-GROUP CURRENT-PROCESS))))
	 (SG-THROW SG 'SI:TOP-LEVEL NIL)))
  NIL)

;; Meta-Z command.  Throw one level of error handler in this stack group.
(DEFUN COM-THROW-ONE-ERROR (SG ETE)
  (LEAVING-ERROR-HANDLER)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL))
	((SG-FOOTHOLD-DATA SG)
	 (SG-THROW SG 'FOOTHOLD NIL))
	(T (COM-TOP-LEVEL-THROW SG ETE))))

;; Control-T.
(DEFUN COM-THROW (SG &REST IGNORE TAG VAL)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (FORMAT T "Throw a value to a tag.~%")
  (SETQ TAG (READ-OBJECT "What is the tag?")
	VAL (READ-OBJECT "What value do you wish to throw?"))
  (LEAVING-ERROR-HANDLER)
  (SG-THROW SG TAG VAL)
  NIL)

;; ?, <help>
(DEFUN COM-HELP (&REST IGNORE)
  (FORMAT T "~&~A~%"
"    You are in the error handler.  If you type in a Lisp form, it will be
evaluated, and the result printed.  You may also type one of the following:
<help> or ? gets this text.  To just get back to top level, type a Z.
    N or <line> goes down a frame, P or <return> goes up.  N and P
are similar but give more info.  L or <form> clears screen and retypes info,
L clears screen and types more info.  < goes to top of stack, >
goes to the bottom.
    B gives a brief backtrace, B a fuller one.  A prints the arglist of the
function in the current frame.
    N, P and B resemble N, P and B except that they show
all the internal EVALs, PROGs, CONDs, etc. of interpreted code.
    A prints an argument to the current function, and sets * to be that argument
to let you do more complicated things with it.  + is set to a locative to that argument,
should you want to modify it.  L does likewise for the function's locals.
F does likewise for the function itself.
    E calls the editor to edit the current function.
    W switches to the window-based error handler.
    R returns a value from the current frame.  R offers to reinvoke the current
frame with the originally supplied arguments (as best as they can be determined).
    T throws to a specific tag.
    C corrects the error and continues; it may ask you to input a value.
    C for an unbound-variable or undefined-function error will proceed, setq'ing
       or defining the symbol unless you have already.
    C continues from an ERROR-RESTART special form.
    Z is like Z, but when there are recursive errors it only pops one level.
While in the error hander, G quits back to the error handler top level."
  ))

;; Control-R.
(DEFUN COM-RETURN-A-VALUE (SG &REST IGNORE &AUX VALUE)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (FORMAT T "Return a value from the function ~S.~%"
	  (FUNCTION-NAME (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME)))
  (SETQ VALUE (READ-OBJECT "Form to evaluate and return:"))
  (LEAVING-ERROR-HANDLER)
  (SG-UNWIND-TO-FRAME SG CURRENT-FRAME T VALUE)
  NIL)

;; Meta-R.
(DEFUN COM-RETURN-MANY-VALUES (&REST IGNORE)
);WRITE THIS

;; Control-Meta-R
(DEFUN COM-RETURN-REINVOCATION (SG &REST IGNORE &AUX FORM RP PP)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))	;Why??
  (FORMAT T "Evaluating ~S, OK?"
	  (SETQ FORM (GET-FRAME-FUNCTION-AND-ARGS SG CURRENT-FRAME)))
  (COND ((Y-OR-N-P)
	 ;; Unwind back to point where frame to be retried is about to return.
	 ;; This gets rid of its unwind-protects but not its special bindings
	 ;; and leaves any ADI associated with calling it on the stack too.
	 (SG-UNWIND-TO-FRAME SG CURRENT-FRAME NIL NIL)
	 ;; Now we would like to get rid of any associated special bindings
	 ;; but unfortunately we can't distinguish closure/instance bindings
	 ;; made before function entry with those made by the function itself.
	 ;; So leave them all and hope for the best.
	 ;; Get rid of the saved microstack for that frame.  There will at least
	 ;; be an entry for XUWR1+1.
	 (SETQ RP (SG-REGULAR-PDL SG)
	       PP (SG-REGULAR-PDL-POINTER SG))
	 (AND (ZEROP (RP-MICRO-STACK-SAVED RP CURRENT-FRAME))
	      (FERROR NIL "Where's my saved microstack?"))
	 (DO ((SP (SG-SPECIAL-PDL SG))
	      (SPP (SG-SPECIAL-PDL-POINTER SG) (1- SPP))
	      (P))
	     (NIL)
	   (SETQ P (ALOC SP SPP))
	   (OR (= (%P-DATA-TYPE P) DTP-FIX) (FERROR NIL "Where's my saved microstack?"))
	   (AND (%P-FLAG-BIT P)
		(RETURN (SETF (SG-SPECIAL-PDL-POINTER SG) (1- SPP)))))
	 (SETF (RP-MICRO-STACK-SAVED RP CURRENT-FRAME) 0)
	 ;; Now rebuild the frame as if it was an open call block about to be called
	 (SETF (SG-PDL-PHASE SG)		;PP gets M-AP minus one
	       (LOGAND (- (SG-PDL-PHASE SG) (- PP (SETQ PP (1- CURRENT-FRAME)))) 1777))
	 (SETF (SG-REGULAR-PDL-POINTER SG) PP)
	 (DOLIST (X FORM)			;Put function and args back
	   (SG-REGPDL-PUSH X SG))
	 (%P-STORE-CDR-CODE (ALOC RP (SG-REGULAR-PDL-POINTER SG)) CDR-NIL)
	 (SETF (SG-IPMARK SG) CURRENT-FRAME)
	 (SETF (SG-AP SG) (SG-PREVIOUS-ACTIVE SG CURRENT-FRAME))
	 ;; Now send the SG on its way
	 (SETF (SG-CURRENT-STATE SG) SG-STATE-INVOKE-CALL-ON-RETURN)
	 (WITHOUT-INTERRUPTS
	   (AND ERROR-HANDLER-RUNNING
		(FREE-SECOND-LEVEL-ERROR-HANDLER-SG %CURRENT-STACK-GROUP))
	   (FUNCALL SG)))))

;; Control-A.
(DEFUN COM-ARGLIST (SG &REST IGNORE)
  (LET ((FUNCTION (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME)))
    (FORMAT T "Argument list for ~S is ~A.~%"
	    (FUNCTION-NAME FUNCTION)
	    (ARGLIST FUNCTION)))
  NIL)

;; Control-C.
(DEFUN COM-PROCEED (SG ETE &REST IGNORE &AUX TEM OBJ)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (COND ((AND (EQ (CAR ETE) 'FERROR) (CADR ETE))
	 (SETQ OBJ (COND ((SETQ TEM (GET (FOURTH ETE) 'PROCEED))	;Condition's property
			  (FUNCALL TEM SG ETE))
			 (T
			  (AND (EQ (CADR ETE) T)
			       (READ-OBJECT
				 "Form to evaluate and return from FERROR//CERROR:")))))
	 ;; Now restart the stack group, causing CERROR to return OBJ
	 (PROCEED-SG SG T OBJ))
	((AND (EQ (CAR ETE) ':BREAK)		;Break
	      (CDR ETE))			; and want retry.
	 (FORMAT T " Continue from break.~%")
	 (PROCEED-SG SG T OBJ))			;Everything set at SECOND-LEVEL. Do it.
	(T
	 (LET ((FUNCTION (GET (FIRST ETE) 'PROCEED)))
	   (COND ((NULL FUNCTION)
		  (FORMAT T "There is no way to proceed from this error.~%"))
		 (T ;; Call the specific proceed routine.  It should call RESTART
		  ;; to mung the micro-stack appropriately.
		  (FUNCALL FUNCTION SG ETE)
		  ;; Now restart the stack group, as that routine left it.
		  (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
		  (PROCEED-SG SG NIL))))))
  NIL)

;; Meta-C.
(DEFUN COM-BASH-AND-PROCEED (SG ETE &REST IGNORE)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (LET ((FUNCTION (GET (FIRST ETE) 'BASH-AND-PROCEED)))
    (COND ((NULL FUNCTION)
	   (FORMAT T "There is no way to bash-and-proceed from this error.~%"))
	  (T ;; Call the specific proceed routine.  It should call RESTART
	   ;; to mung the micro-stack appropriately.
	   (FUNCALL FUNCTION SG ETE)
	   ;; Now restart the stack group, as that routine left it.
	   (SETF (SG-CURRENT-STATE SG) SG-STATE-RESUMABLE)
	   (PROCEED-SG SG NIL))))
  NIL)

;; Comtrol-meta-C.
(DEFUN COM-ERROR-RESTART (SG &REST IGNORE)
  (COND ((NULL ERROR-HANDLER-RUNNING) (*THROW 'QUIT NIL)))
  (COND ((OR CONDITION-PROCEED-FLAG
	     (Y-OR-N-P "Are you SURE you want to restart? "))
	 (SG-THROW SG 'ERROR-RESTART NIL)))
  (FORMAT T "Flushed.~%"))

;; Control-S.
(DEFUN COM-SEARCH (SG IGNORE &OPTIONAL IGNORE FLAG &AUX KEY AP)
  (FORMAT T "String to search for (end with RETURN):~%")
  (SETQ KEY (READLINE))
  (SETQ AP 
	(DO ((AP (SG-AP SG) (SG-PREVIOUS-ACTIVE SG AP))
	     (RP (SG-REGULAR-PDL SG))
	     (NAME)
	     )
	    ((NULL AP) NIL)
	  (SETQ NAME (FUNCTION-NAME (RP-FUNCTION-WORD RP AP)))
	  (SETQ NAME
		(COND ((STRINGP NAME) NAME)
		      ((SYMBOLP NAME) (STRING NAME))
		      (T (FORMAT NIL "~S" NAME))))
	  (AND (STRING-SEARCH KEY NAME)
	       (RETURN AP))))
  (COND ((NULL AP)
	 (FORMAT T "Search failed.~%"))
	(T
	 (SETQ CURRENT-FRAME AP)
	 (COND ((NOT FLAG) (SHOW-FUNCTION-AND-ARGS SG))
	       (T (SHOW-ALL SG))))))

;; Meta-S.
(DEFUN COM-SEARCH-AND-SHOW-ALL (SG ETE &OPTIONAL (COUNT 1))
  (COM-SEARCH SG ETE COUNT T))

;; Control-Meta-A
(DEFUN COM-GET-ARG (SG IGNORE &OPTIONAL (ARG 0) &AUX RP TEM)
  (SETQ RP (SG-REGULAR-PDL SG)
	TEM (+ ARG 1 CURRENT-FRAME)
	+ (ALOC RP TEM)
	* (AREF RP TEM))
  (FORMAT T "~&~S" *))

;; Control-Meta-L
(DEFUN COM-GET-LOCAL (SG IGNORE &OPTIONAL (ARG 0) &AUX RP TEM)
  (SETQ RP (SG-REGULAR-PDL SG)
	TEM (+ ARG CURRENT-FRAME (RP-LOCAL-BLOCK-ORIGIN RP CURRENT-FRAME))
	+ (ALOC RP TEM)
	* (AREF RP TEM))
  (FORMAT T "~&~S" *))

;; c-m-F
(DEFUN COM-GET-FUNCTION (SG IGNORE &OPTIONAL IGNORE)
  (SETQ * (RP-FUNCTION-WORD (SG-REGULAR-PDL SG) CURRENT-FRAME))
  (FORMAT T "~&~S" *))

;; This is the dispatch table of error handler commands, a 4 by 240 array.
(DEFVAR COMMAND-DISPATCH-TABLE)
;; This is a list, setq'd in this file, from which COMMAND-DISPATCH-TABLE is initialized.
(DEFVAR COMMAND-DISPATCH-LIST)
;; This file will set COMMAND-DISPATCH-LIST make sure COMMAND-DISPATCH-TABLE is recomputed
;; from it by ASSURE-DISPATCH-SET-UP.
(MAKUNBOUND 'COMMAND-DISPATCH-TABLE)

(DEFUN ASSURE-DISPATCH-SET-UP ()
  (COND ((NOT (BOUNDP 'COMMAND-DISPATCH-TABLE))
	 (SETQ COMMAND-DISPATCH-TABLE (MAKE-ARRAY NIL 'ART-Q '(4 240)))
	 (DOLIST (X COMMAND-DISPATCH-LIST)
	   (LET ((CHAR (CAR X))
		 (COM (CADR X))
		 (REPEAT (CADDR X)))
	     (LET ((I (LDB %%KBD-CONTROL-META CHAR)) (J (LDB %%KBD-CHAR CHAR)))
	       (DOTIMES (N (OR REPEAT 1))
		 (ASET COM COMMAND-DISPATCH-TABLE I J)
		 (SETQ J (1+ J)))))))))

;; The initial dispatch table.
(SETQ COMMAND-DISPATCH-LIST '(
       (#/? COM-HELP)
       (#\HELP COM-HELP)
       (#\LINE COM-DOWN-STACK)
       (#\FORM COM-CLEAR-AND-SHOW)
       (#\RETURN COM-UP-STACK)
       (#\RESUME COM-PROCEED)

       (#/0 COM-NUMBER 10.)		;control-digits
       (#/A COM-ARGLIST)
       (#/B SHORT-BACKTRACE)
       (#/C COM-PROCEED)
       (#/E COM-EDIT-FRAME-FUNCTION)
       (#/L COM-CLEAR-AND-SHOW)
       (#/N COM-DOWN-STACK)
       (#/P COM-UP-STACK)
       (#/R COM-RETURN-A-VALUE)
       (#/S COM-SEARCH)
       (#/T COM-THROW)
       (#/Z COM-TOP-LEVEL-THROW)

       (#/0 COM-NUMBER 10.)		;meta-digits
       (#/< COM-TOP-STACK)
       (#/> COM-BOTTOM-STACK)
       (#/B FULL-BACKTRACE)
       (#/C COM-BASH-AND-PROCEED)
       (#/L COM-CLEAR-AND-SHOW-ALL)
       (#/N COM-DOWN-STACK-ALL)
       (#/P COM-UP-STACK-ALL)
       (#/R COM-RETURN-MANY-VALUES)
       (#/S COM-SEARCH-AND-SHOW-ALL)
       (#/Z COM-THROW-ONE-ERROR)

       (#/0 COM-NUMBER 10.)		;control-meta-digits
       (#/A COM-GET-ARG)
       (#/B FULL-BACKTRACE-UNINTERESTING)
       (#/C COM-ERROR-RESTART)
       (#/F COM-GET-FUNCTION)
       (#/L COM-GET-LOCAL)
       (#/N COM-DOWN-STACK-UNINTERESTING)
       (#/P COM-UP-STACK-UNINTERESTING)
       (#/R COM-RETURN-REINVOCATION)
       (#/U COM-UP-TO-INTERESTING)
       (#/W COM-WINDOW-ERROR-HANDLER)
       ))
