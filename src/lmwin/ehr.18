;; New error handler routines.		DLW 1/6/78 -*-Mode:LISP; Package:EH-*-

;;;; Conventions for the error handler routines:  how to add new ones.

;; Each microcode error has associated with it a call to the ERROR-TABLE "pseudo-op"
;; of CONSLP.  The form in CONSLP looks like:
;;    (ERROR-TABLE ARGTYP FIXNUM M-T 0)
;; (for example).  The CDR of this list is the ETE.  So, the FIRST element
;; is the name of the error, and the SECOND is the first "argument" to that
;; error's associated routines.
;;
;; A Lisp routine calling the error handler can make up any ETE it wants.
;;
;; All ETEs should be a list whose car is a symbol.  That symbol
;; must have an INFORM property, and may optionally have other
;; properties.  The INFORM property should be a function which can 
;; be applied to the SG and the ETE, and print out an error message.
;;
;; The only other property defined is PROCEED, which is used to proceed
;; after getting the error (i.e. the c-C command of the error handler).
;; It too should be a function to be applied to the SG and ETE,
;; and what it should do is to fix up the SAVED stack group state
;; appropriately, and return; the stack group will be started up in
;; SG-RESUMABLE state.
;;
;; Look at a few of these to get the idea of what they should look like.
;;
;; For errors signalled by macrocode (not microcode), the ETE looks like
;;	(CERROR proceedable-flag restartable-flag condition format-string . args)


;; This is like SG-CONTENTS, but always returns a fixnum with the
;; pointer part of the contents.  Useful if contents might be a small
;; untyped integer.
;; As a special kludge, if the tag is a number, the result is simply that
;; number.  The ARRAY-NUMBER-DIMENSIONS error, for example, is one.
(DEFUN SG-FIXNUM-CONTENTS (SG LOC)
  (IF (NUMBERP LOC) LOC (%P-LDB %%Q-POINTER (SG-LOCATE SG LOC))))

(DEFUN SG-FIXNUM-STORE (X SG LOC)
  (%P-DPB X %%Q-POINTER (SG-LOCATE SG LOC)))

;;;; This class can happen from within the compilations of Lisp functions.
;;;; They are the kind for which ERRING-FUNCTION is useful.

;; ARGTYP.
;; First arg is what it should have been (or a list of such).
;; Second arg is where offender is.
;; Third arg is which argument is was (zero based) (T = one and only, NIL = ?)
;; Fourth arg (optional) is RESTART address.  If it is NIL or missing,
;; then there is no way to recover.
;; Fifth arg (optional) is name of sub-function (typically CAR or CDR)
;; to which the value was an argument.  Just to make error messages nicer.

(DEFUN (ARGTYP INFORM) (SG ETE)
  (LET ((ARGNUM (FOURTH ETE)))
    (FORMAT T "~:[Some~*~*~;The~:[ ~:R~;~*~]~] argument to ~S, "
	    ARGNUM (EQ ARGNUM T) (AND (NUMBERP ARGNUM) (1+ ARGNUM))
	    (OR (SIXTH ETE) (SG-ERRING-FUNCTION SG))))
  (P-PRIN1-CAREFUL (SG-LOCATE SG (THIRD ETE)))
  (FORMAT T ", was of the wrong type.~%The function expected ")
  (LET ((TYPE (SECOND ETE)))
    (COND ((SYMBOLP TYPE)
	   (PRINC-TYPE-NAME TYPE))
	  ((LISTP TYPE)
	   (IF (< (LENGTH TYPE) 2)
	       (PRINC-TYPE-NAME (CAR TYPE))
	       (DO TL TYPE (CDR TL) (NULL TL)
		   (PRINC-TYPE-NAME (CAR TL))
		   (PRINC (COND ((NULL (CDR TL)) "")
				((NULL (CDDR TL)) " or ")
				(T ", "))))))
	  (T (BAD-HACKER TYPE " is not a type."))))
  (FORMAT T ".~%"))

(DEFUN PRINC-TYPE-NAME (TYPE)
  (PRINC (OR (CADR (ASSQ TYPE DATA-TYPE-NAMES)) TYPE)))

(DEFVAR DATA-TYPE-NAMES
      '((FIXNUM "a fixnum")
	(NUMBER "a number")
	(BIGNUM "a bignum")
	(INTEGER "a fixnum or a bignum")
	(POSITIVE-FIXNUM "a fixnum  zero")
	(PLUSP "a number > zero")
	(SYMBOL "a symbol")
	(CONS "a cons")
	(LIST "a list")
	(NIL "the symbol NIL")
	(NON-NIL "something other than NIL")
	(LOCATIVE "a locative")
	(CLOSURE "a closure")
	(INSTANCE "an instance")
	(STACK-GROUP "a stack group")
	(ARRAY "an array")
	(ART-Q-LIST-ARRAY "an ART-Q-LIST array")
	(Q-ARRAY "an array of Lisp objects")
	(BYTE-ARRAY "an array of numbers")
	(ART-4B-ARRAY "an array of 4-bit bytes")
	(AREA "an area number, NIL (default), or a symbol whose value is one")
	(FIXNUM-FIELD "a byte pointer to a field that fits in a fixnum")))

(DEFUN (ARGTYP SIGNAL) (SG ETE)
  `(:WRONG-TYPE-ARGUMENT ,(SECOND ETE) ,(SG-CONTENTS SG (THIRD ETE))))

(DEFUN (ARGTYP PROCEED) (SG ETE)
  (COND ((NULL (FIFTH ETE))
	 (FORMAT T "You cannot recover from this error.~%")
	 (*THROW 'QUIT NIL)))
  (SG-STORE (READ-OBJECT "Form to evaluate and use as replacement argument:")
	    SG (THIRD ETE))
  (SG-PROCEED-MICRO-PC SG (FIFTH ETE)))

;; This routine should be called by the PROCEED routines for
;; microcode (non-FERROR) errors.  A restart micro pc is pushed onto
;; the saved micro-stack.  If TAG is NIL, this is the trap pc plus one,
;; continuing from the point of the trap.  Else look up the numerical
;; value of TAG, set by the RESTART-PC pseudo-op in the microcode.
;; If an PROCEED routine doesn't call SG-PROCEED-MICRO-PC, then
;; control will be returned from the micro-routine that got the error.
(DEFUN SG-PROCEED-MICRO-PC (SG TAG)
  (LET ((PC (IF TAG (CDR (ASSQ TAG RESTART-LIST)) (1+ (SG-TRAP-MICRO-PC SG)))))
    (COND ((NULL PC)
	   (BAD-HACKER TAG " no such restart!")
	   (*THROW 'QUIT NIL)))
    ;; Since the micro stack is saved backwards, the top of the stack is buried
    ;; where it is hard to get at.
    (LET ((RP (SG-REGULAR-PDL SG))
	  (SP (SG-SPECIAL-PDL SG))
	  (SPP (SG-SPECIAL-PDL-POINTER SG))
	  (AP (SG-AP SG)))
      (OR (ZEROP (RP-MICRO-STACK-SAVED RP AP))	;Shuffle up stack to make room
	  (DO ((FLAG 0)) ((NOT (ZEROP FLAG)))
	    (ASET (AREF SP SPP) SP (1+ SPP))
	    (%P-STORE-FLAG-BIT (ALOC SP (1+ SPP)) 0)
	    (SETQ FLAG (%P-FLAG-BIT (ALOC SP SPP)))
	    (SETQ SPP (1- SPP))))
      (ASET PC SP (SETQ SPP (1+ SPP)))
      (%P-STORE-FLAG-BIT (ALOC SP SPP) 1)
      (SETF (SG-SPECIAL-PDL-POINTER SG) (1+ (SG-SPECIAL-PDL-POINTER SG)))
      (SETF (RP-MICRO-STACK-SAVED RP AP) 1))))

;; FIXNUM-OVERFLOW
;; First arg is M-T to show that that is where the value should
;;   get stored.  Maybe it will someday be other things, too.
;; Second is either PUSH or NOPUSH.
;; Recover by storing a new value in the place where the
;;   value would have been stored if it hadn't overflowed.
;;   This is M-T, and also the regpdl if the second arg is PUSH.
;;   Force return from the microroutine executing at the time.

(DEFUN (FIXNUM-OVERFLOW INFORM) (SG IGNORE)
  (FORMAT T "~S got a fixnum overflow.~%" (SG-ERRING-FUNCTION SG)))

(DEFUN (FIXNUM-OVERFLOW PROCEED) (SG ETE &AUX NUM)
  (OR (MEMQ (THIRD ETE) '(PUSH NOPUSH))
      (BAD-HACKER ETE "Bad ETE, must be PUSH or NOPUSH."))
  (SETQ NUM (READ-OBJECT "Fixnum to return instead:"))
  (CHECK-ARG NUM FIXP "a fixnum")
  (SG-FIXNUM-STORE NUM SG (SECOND ETE))
  (AND (EQ (THIRD ETE) 'PUSH)
       (SG-REGPDL-PUSH SG NUM)))

(DEFPROP FIXNUM-OVERFLOW :FIXNUM-OVERFLOW CONDITION)
(DEFPROP FIXNUM-OVERFLOW SIGNAL-WITH-CONTENTS SIGNAL)

;; FLOATING-EXPONENT-UNDERFLOW
;; Arg is SFL or FLO

(DEFUN (FLOATING-EXPONENT-UNDERFLOW INFORM) (SG ETE)
  (FORMAT T "~S produced a result too small in magnitude to be
representable as a ~:[~;small~] flonum.~%"
	  (SG-ERRING-FUNCTION SG)
	  (EQ (SECOND ETE) 'SFL)))

(DEFPROP FLOATING-EXPONENT-UNDERFLOW :FLOATING-EXPONENT-UNDERFLOW CONDITION)
(DEFPROP FLOATING-EXPONENT-UNDERFLOW FLOATING-EXPONENT-UNDERFLOW-SIGNAL SIGNAL)
(DEFUN FLOATING-EXPONENT-UNDERFLOW-SIGNAL (IGNORE ETE)
  (LIST (GET (CAR ETE) 'CONDITION)))

(DEFUN (FLOATING-EXPONENT-UNDERFLOW PROCEED) (SG ETE)
  (FORMAT T " Proceed using 0.0~:[s0~] as the value instead? " (EQ (SECOND ETE) 'FLO))
  (OR (Y-OR-N-P) (*THROW 'QUIT NIL))
  (SG-PROCEED-MICRO-PC SG NIL))

;; FLOATING-EXPONENT-OVERFLOW
;; Result is to be placed in M-T and pushed on the pdl.
;; Arg is SFL or FLO
;; In the case of SFL the pdl has already been pushed.

(DEFUN (FLOATING-EXPONENT-OVERFLOW INFORM) (SG ETE)
  (FORMAT T "~S produced a result too large in magnitude to be
representable as a ~:[~;small~] flonum.~%"
	  (SG-ERRING-FUNCTION SG)
	  (EQ (SECOND ETE) 'SFL)))

(DEFPROP FLOATING-EXPONENT-OVERFLOW :FLOATING-EXPONENT-OVERFLOW CONDITION)
(DEFPROP FLOATING-EXPONENT-OVERFLOW FLOATING-EXPONENT-UNDERFLOW-SIGNAL SIGNAL)

(DEFUN (FLOATING-EXPONENT-OVERFLOW PROCEED) (SG ETE &AUX NUM)
  (DO () (())
    (SETQ NUM (READ-OBJECT (IF (EQ (SECOND ETE) 'SFL) "Small-flonum to return instead:"
						      "Flonum to return instead:")))
    (COND ((AND (EQ (SECOND ETE) 'SFL)
		(SMALL-FLOATP NUM))
	   (RETURN NIL))
	  ((FLOATP NUM)
	   (RETURN NIL)))
    (FORMAT T "Please use a ~:[~;small~] flonum.~%" (EQ (SECOND ETE) 'SFL)))
  (SG-STORE NUM SG 'M-T)
  (AND (EQ (FIRST ETE) 'FLOATING-EXPONENT-OVERFLOW)
       (EQ (SECOND ETE) 'SFL)
       (SG-REGPDL-POP SG))
  (SG-REGPDL-PUSH NUM SG))

;; DIVIDE-BY-ZERO
;; You cannot recover.

(DEFPROP DIVIDE-BY-ZERO DIVIDE-BY-ZERO-INFORM INFORM)
(DEFUN DIVIDE-BY-ZERO-INFORM (SG IGNORE)
  (FORMAT T "There was an attempt to divide a number by zero in ~S.~%"
	  (SG-ERRING-FUNCTION SG)))

;; ARRAY-NUMBER-DIMENSIONS
;; First arg is how many we gave.
;; Second arg is how many is right.
;; Third arg is the array
;; You cannot recover.

(DEFUN (ARRAY-NUMBER-DIMENSIONS INFORM) (SG ETE)
  (FORMAT T "~S was given ~S, a ~S-dimensional array; it expected a ~S-dimensional one.~%"
	  (SG-ERRING-FUNCTION SG)
	  (SG-CONTENTS SG (FOURTH ETE))
	  (SG-FIXNUM-CONTENTS SG (SECOND ETE))
	  (SG-FIXNUM-CONTENTS SG (THIRD ETE))))

;; IALLB-TOO-SMALL
;; First arg is how many we asked for.

(DEFUN (IALLB-TOO-SMALL INFORM) (SG ETE)
  (FORMAT T "There was a request to allocate ~S cells.~%"
	  (SG-FIXNUM-CONTENTS SG (SECOND ETE))))

;; NUMBER-ARRAY-NOT-ALLOWED
;; First arg is where to find the array.
;; You cannot recover.

(DEFUN (NUMBER-ARRAY-NOT-ALLOWED INFORM) (SG ETE)
  (FORMAT T "The array ~S, which was given to ~S, is not allowed to be a number array.~%"
	  (SG-CONTENTS SG (SECOND ETE))
	  (SG-ERRING-FUNCTION SG)))

;; SUBSCRIPT-OOB
;; First arg is how many we gave.
;; Second is the legal limit.
;; Third optional arg is a restart tag.

(DEFUN (SUBSCRIPT-OOB INFORM) (SG ETE)
  (LET ((USED (SG-FIXNUM-CONTENTS SG (SECOND ETE)))
	(IN (SG-ERRING-FUNCTION SG)))
    (COND ((< USED 0)
	   (FORMAT T "The subscript, ~S, was negative in ~S~%"
		   USED IN))
	  (T 
	   (FORMAT T "The subscript, ~S, was beyond the length, ~S, in ~S~%"
		   USED
		   (SG-FIXNUM-CONTENTS SG (THIRD ETE))
		   IN)))))

(DEFUN (SUBSCRIPT-OOB PROCEED) (SG ETE &AUX NUM)
  (COND ((NULL (FOURTH ETE))
	 (FORMAT T "You cannot recover from this error.~%")
	 (*THROW 'QUIT NIL)))
  (DO () (())
    (SETQ NUM (READ-OBJECT "Subscript to use instead:"))
    (AND (FIXP NUM) (RETURN))
    (FORMAT T "Please use a fixnum.~%"))
  (SG-FIXNUM-STORE NUM SG (SECOND ETE))
  (SG-PROCEED-MICRO-PC SG (FOURTH ETE)))

;; BAD-ARRAY-TYPE
;; First arg is where array header is. Note that it may well have a data type of DTP-TRAP.
;; You cannot recover.

(DEFUN (BAD-ARRAY-TYPE INFORM) (SG ETE)
  (FORMAT T "The array type, ~S, was invalid in ~S.~%"
	  (LDB %%ARRAY-TYPE-FIELD (%P-POINTER (SG-LOCATE SG (SECOND ETE))))
	  (SG-ERRING-FUNCTION SG)))

;; ARRAY-HAS-NO-LEADER
;; Arg is where array pointer is.
;; Recover from this by simply returning something else, by putting it in
;; M-T and discarding the return addr and restarting.

(DEFUN (ARRAY-HAS-NO-LEADER INFORM) (SG ETE)
  (FORMAT T "The array given to ~S, ~S, has no leader.~%"
	  (SG-ERRING-FUNCTION SG)
	  (SG-CONTENTS SG (SECOND ETE))))

(DEFPROP ARRAY-HAS-NO-LEADER :ARRAY-HAS-NO-LEADER CONDITION)
(DEFPROP ARRAY-HAS-NO-LEADER SIGNAL-WITH-CONTENTS SIGNAL)

(DEFUN SIGNAL-WITH-CONTENTS (SG ETE)
  (LIST (GET (CAR ETE) 'CONDITION)
	(SG-CONTENTS SG (SECOND ETE))))

(DEFUN (ARRAY-HAS-NO-LEADER PROCEED) (SG IGNORE)
  (SG-STORE (READ-OBJECT "Form to evaluate and return instead:") SG 'M-T))

;; FILL-POINTER-NOT-FIXNUM
;; Arg is where array pointer is.
;; Recover by returning an arbitrary frob, just flush spurious return addr and restart.

(DEFUN (FILL-POINTER-NOT-FIXNUM INFORM) (SG ETE)
  (FORMAT T "The fill-pointer of the array given to ~S, ~S, is not a fixnum.~%"
	  (SG-ERRING-FUNCTION SG)
	  (SG-CONTENTS SG (SECOND ETE))))

(DEFPROP FILL-POINTER-NOT-FIXNUM :FILL-POINTER-NOT-FIXNUM CONDITION)
(DEFPROP FILL-POINTER-NOT-FIXNUM SIGNAL-WITH-CONTENTS SIGNAL)

(DEFUN (FILL-POINTER-NOT-FIXNUM PROCEED) (SG IGNORE)
  (SG-STORE (READ-OBJECT "Form to evaluate and return instead:") SG 'M-T))

;; More random losses.

;arg is number which was called.
(DEFUN (NUMBER-CALLED-AS-FUNCTION INFORM) (SG ETE)
  (FORMAT T "The number, ~S, was called as a function~%"
	  (SG-CONTENTS SG (SECOND ETE))))

(DEFPROP NUMBER-CALLED-AS-FUNCTION :INVALID-FUNCTION CONDITION)
(DEFPROP NUMBER-CALLED-AS-FUNCTION SIGNAL-WITH-CONTENTS SIGNAL)

;; FLONUM-NO-GOOD.  The argument has been lost at this point,
;; so can't use ARGTYP error, and cannot recover.
(DEFUN (FLONUM-NO-GOOD INFORM) (SG IGNORE)
  (FORMAT T "~S does not accept floating-point arguments"
	  (SG-ERRING-FUNCTION SG)))

;; WRONG-SG-STATE
;; Arg is where sg is.
;; You cannot recover.

(DEFUN (WRONG-SG-STATE INFORM) (SG ETE)
  (FORMAT T "The state of the stack group, ~S, given to ~S, was invalid.~%"
	  (SG-CONTENTS SG (SECOND ETE))
	  (SG-ERRING-FUNCTION SG)))

(DEFPROP WRONG-SG-STATE :INVALID-SG-STATE CONDITION)
(DEFPROP WRONG-SG-STATE SIGNAL-WITH-CONTENTS SIGNAL)

;; SG-RETURN-UNSAFE
;; No args, since the frob is in the previous-stack-group of the current one.
;; You cannot recover.

(DEFUN (SG-RETURN-UNSAFE INFORM) (IGNORE IGNORE)
  (FORMAT T "An /"unsafe/" stack group attempted to STACK-GROUP-RETURN.~%"))

;; TV-ERASE-OFF-SCREEN
;; No arg.

(DEFUN (TV-ERASE-OFF-SCREEN INFORM) (IGNORE IGNORE)
  (FORMAT T "The %DRAW-RECTANGLE function attempted to erase past the end of the screen.~%"))

;; THROW-TAG-NOT-SEEN
;; The tag has been moved to M-A for the EH to find it!
;; The value being thrown is in M-T, the *UNWIND-STACK count and action are in M-B and M-C.

(DEFUN (THROW-TAG-NOT-SEEN INFORM) (SG IGNORE)
  (FORMAT T "There was no pending *CATCH for the tag ~S.~%"
	  (SG-AC-A SG))
  (FORMAT T "The value being thrown was ~S~%" (SG-AC-T SG))
  (AND (SG-AC-B SG)
       (FORMAT T "While in a *UNWIND-STACK with remaining count of ~D.~%" (SG-AC-B SG)))
  (AND (SG-AC-C SG)
       (FORMAT T "While in a *UNWIND-STACK with action ~S.~%" (SG-AC-C SG))))

(DEFUN (THROW-TAG-NOT-SEEN SIGNAL) (SG IGNORE)
  `(:UNDEFINED-CATCH-TAG ,(SG-AC-A SG) ,(SG-AC-T SG) ,(SG-AC-B SG) ,(SG-AC-C SG)))

;; MVR-BAD-NUMBER
;; Where the # is.

(DEFUN (MVR-BAD-NUMBER INFORM) (SG ETE)
  (FORMAT T "The function attempted to return ~D. values.~%"
	  (SG-FIXNUM-CONTENTS SG (SECOND ETE))))

;;;; General Machine Lossages.

;; PDL-OVERFLOW
;; Arg is either SPECIAL or REGULAR

;; NOTE: If you make PDL-OVERFLOW signal a condition, there is going to be problem
;; since the call to SIGNAL will expand the pdl which will type out when the streams
;; haven't really been set up yet.
(DEFUN (PDL-OVERFLOW INFORM) (IGNORE ETE)
  (FORMAT T "The ~A push-down list has overflown.  Type control-C to allocate more.~%"
	  (CADR (ASSQ (SECOND ETE) '((REGULAR "regular") (SPECIAL "special"))))))

(DEFUN (PDL-OVERFLOW PROCEED) (SG IGNORE)
  (FORMAT T " Continuing with more pdl.~%")
  (SG-MAYBE-GROW-PDLS SG)		;Make very sure that there is enough room
  (SG-PROCEED-MICRO-PC SG NIL))		;Then continue after microcode check for room

;; ILLEGAL-INSTRUCTION
;; No args.

(DEFUN (ILLEGAL-INSTRUCTION INFORM) (SG IGNORE)
  (FORMAT T "There was an attempt to execute an invalid instruction: ~O"
	  (LET ((AP (SG-PREVIOUS-ACTIVE SG (SG-AP SG))))
	    (FEF-INSTRUCTION (AREF (SG-REGULAR-PDL SG) AP)
			     (RP-EXIT-PC (SG-REGULAR-PDL SG) AP)))))

;; BAD-CDR-CODE
;; Arg is where loser is.
(DEFUN (BAD-CDR-CODE INFORM) (SG ETE)
  (FORMAT T "A bad cdr-code was found in memory (at address ~O)~%"
	  (SG-FIXNUM-CONTENTS SG (SECOND ETE))))  ;Can't use Lisp print since will err again

;; DATA-TYPE-SCREWUP
;; This happens when some internal data structure contains wrong data type.  arg is name.
;; As it happens, all the names either start with a vowel or do if pronounced as letters
;; Not continuable
(DEFUN (DATA-TYPE-SCREWUP INFORM) (IGNORE ETE)
  (FORMAT T "A bad data-type was found in the internal guts of an ~A~%" (SECOND ETE)))

;; STACK-FRAME-TOO-LARGE
(DEFUN (STACK-FRAME-TOO-LARGE INFORM) (IGNORE IGNORE)
  (FORMAT T "Attempt to make a stack frame larger than 256. words"))

;; AREA-OVERFLOW
;; arg is register containing area#
(DEFUN (AREA-OVERFLOW INFORM) (SG ETE)
  (LET ((AREA-NUMBER (SG-FIXNUM-CONTENTS SG (SECOND ETE))))
    (FORMAT T "Allocation in the /"~A/" area exceeded the maximum of ~D. words.~%"
	    (AREA-NAME AREA-NUMBER)
	    (AREA-MAXIMUM-SIZE AREA-NUMBER))))

;; VIRTUAL-MEMORY-OVERFLOW
(DEFUN (VIRTUAL-MEMORY-OVERFLOW INFORM) (IGNORE IGNORE)
  (FORMAT T "You've used up all available virtual memory!~%"))

;; REGION-TABLE-OVERFLOW
(DEFUN (REGION-TABLE-OVERFLOW INFORM) (IGNORE IGNORE)
  (FORMAT T "Unable to create a new region because the region tables are full.~%"))

;; RPLACD-WRONG-REPRESENTATION-TYPE
;; arg is first argument to RPLACD
(DEFUN (RPLACD-WRONG-REPRESENTATION-TYPE INFORM) (SG ETE)
  (FORMAT T "Attempt to RPLACD a list which is embedded in a structure and therefore/
cannot be RPLACD'ed.  The list is ~S~%"
	  (SG-CONTENTS SG (SECOND ETE))))

;;;; Special cases.

;; MAR-BREAK

(DEFUN (MAR-BREAK INFORM) (SG IGNORE)
  (FORMAT T "The MAR has gone off because of an attempt to ~[read~;write~].~%"
	  (SG-FLAGS-PGF-WRITE SG)))

;(DEFPROP MAR-BREAK MAR-BREAK-PROCEED PROCEED)
;Doesn't work because SGENT takes a page fault, causeing recursive page faults.
(DEFUN MAR-BREAK-PROCEED (SG IGNORE)
  (FORMAT T "Proceed from MAR break? ")
  (OR (Y-OR-N-P)
      (*THROW 'QUIT NIL))
  (SG-PROCEED-MICRO-PC SG NIL))

;; TRANS-TRAP

(DEFUN (TRANS-TRAP INFORM) (SG IGNORE)
  (PROG ((VMA (SG-SAVED-VMA SG))  ;I need to use a RETURN
	 SYMBOL OFFSET PROP)
    (COND ((= (%P-DATA-TYPE VMA) DTP-NULL)
	   (SETQ SYMBOL (%MAKE-POINTER DTP-SYMBOL (%P-CONTENTS-AS-LOCATIVE VMA))
		 OFFSET (%POINTER-DIFFERENCE VMA SYMBOL))
	   (SELECTQ OFFSET	;The function and value cells are special-cased
	     (1 (RETURN (FORMAT T "The variable ~S is unbound.~%" SYMBOL)))
	     (2 (FORMAT T "The function ~S is undefined.~%" SYMBOL)
		(AND (SETQ PROP (GETL SYMBOL '(EXPR FEXPR MACRO SUBR FSUBR LSUBR AUTOLOAD)))
		     (FORMAT T
	"Note: the symbol has a ~S property, this may be a Maclisp compatibility problem.~%"
			     (CAR PROP)))
		(RETURN NIL))
	     (OTHERWISE
	      ;; Might be an external value cell.  Unfortunately this isn't
	      ;; being executed in the binding environment of the error, so
	      ;; we can't easily look in the internal value-cell and see
	      ;; if it points to this external value cell.  So we'll just
	      ;; do a couple simple checks and jump to conclusions.
	      ;; Also we don't allow here for the possibility of closures
	      ;; of the function cell.
	      (AND (OR (MINUSP OFFSET) (> OFFSET 4))
		   (= (%P-DATA-TYPE SYMBOL) DTP-SYMBOL-HEADER)
		   (RETURN (FORMAT T
				   "The variable ~S is unbound (in a closure value-cell).~%"
				   SYMBOL)))))))
    ;; If it gets here, it's not a special case
    (FORMAT T "The word #<~S ~S> was read from ~O .~%"
	    (Q-DATA-TYPES (%P-DATA-TYPE VMA)) (%P-POINTER VMA) (%POINTER VMA))))

(DEFUN (TRANS-TRAP SIGNAL) (SG IGNORE)
  (LET ((VMA (SG-SAVED-VMA SG)))
    (AND (= (%P-DATA-TYPE VMA) DTP-NULL)
	 (LET ((SYMBOL (%MAKE-POINTER DTP-SYMBOL (%P-CONTENTS-AS-LOCATIVE VMA))))
	   (SELECTQ (%POINTER-DIFFERENCE VMA SYMBOL)
	     (1 `(:UNDEFINED-VARIABLE ,SYMBOL))
	     (2 `(:UNDEFINED-FUNCTION ,SYMBOL)))))))

(DEFUN (TRANS-TRAP PROCEED) (SG IGNORE)
  (COND ((NOT (MEMQ (Q-DATA-TYPES (%P-DATA-TYPE (SG-SAVED-VMA SG))) GOOD-DATA-TYPES))
	 (SG-REGPDL-PUSH (READ-OBJECT "Form to evaluate and use as replacement value?") SG)
	 (SG-PROCEED-MICRO-PC SG 'TRANS-TRAP-RESTART)) ;Use replacement data on stack
	(T (SG-PROCEED-MICRO-PC SG NIL))))	;Drop through, will do transport again

(DEFUN (TRANS-TRAP BASH-AND-PROCEED) (SG IGNORE)
  (COND ((NOT (MEMQ (Q-DATA-TYPES (%P-DATA-TYPE (SG-SAVED-VMA SG))) GOOD-DATA-TYPES))
	 (SG-STORE (READ-OBJECT
		     (LET ((VMA (SG-SAVED-VMA SG)))
		       (OR (AND (= (%P-DATA-TYPE VMA) DTP-NULL)
				(SELECTQ (%POINTER-DIFFERENCE VMA
					          (%P-CONTENTS-AS-LOCATIVE VMA))
				  (1 (FORMAT NIL "Form to evaluate and SETQ ~S to?"
						 (%FIND-STRUCTURE-HEADER VMA)))
				  (2 (FORMAT NIL "Form to evaluate and FSET' ~S to?"
						 (%FIND-STRUCTURE-HEADER VMA)))))
			   "Form to evaluate and store back?")))
		   SG 'RMD)))
  (SG-PROCEED-MICRO-PC SG NIL)) ;Drop through, will do transport again

;; FUNCTION-ENTRY
;; Special case.
;; The ucode kindly leaves the M-ERROR-SUBSTATUS pushed onto the
;; regular pdl so that we can find it.
;; The meanings of %%M-ESUBS-BAD-QUOTED-ARG, %%M-ESUBS-BAD-EVALED-ARG
;; and %%M-ESUBS-BAD-QUOTE-STATUS are not clear, as they are not used
;; by the microcode.

(DEFUN (FUNCTION-ENTRY INFORM) (SG IGNORE)
  (FORMAT T "The function ~S was called with "
	  (FUNCTION-NAME (AREF (SG-REGULAR-PDL SG) (SG-AP SG))))
  (LET ((ERR (AREF (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG))))
    (DO ((STL '("too few arguments." "too many arguments."
		"an argument of bad data type.") (CDR STL))
	 (SBL '(%%M-ESUBS-TOO-FEW-ARGS %%M-ESUBS-TOO-MANY-ARGS
				       %%M-ESUBS-BAD-DT) (CDR SBL)))
	((NULL SBL))
      (AND (= (LDB (SYMEVAL (CAR SBL)) ERR) 1)
	   (FORMAT T (CAR STL)))))
  (TERPRI))

(DEFUN (FUNCTION-ENTRY SIGNAL) (SG IGNORE)
  (LET ((ERR (AREF (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG))))
    (COND ((OR (LDB-TEST %%M-ESUBS-TOO-FEW-ARGS ERR)
	       (LDB-TEST %%M-ESUBS-TOO-MANY-ARGS ERR))
	   `(:WRONG-NUMBER-OF-ARGUMENTS
	      ,(- (SG-REGULAR-PDL-POINTER SG) (SG-AP SG))
	      (ALOC (SG-REGULAR-PDL SG) (1+ (SG-AP SG))))))))

(DEFUN (BREAKPOINT INFORM) (IGNORE IGNORE)
  (FORMAT T "Breakpoint~%"))

(DEFUN (STEP-BREAK INFORM) (SG IGNORE)
  (FORMAT T "Step break~%")
  (SETF (SG-INST-DISP SG) 0))

(DEFUN (:WRONG-TYPE-ARGUMENT PROCEED) (IGNORE ETE)
  (READ-OBJECT
    (FORMAT NIL "Form to be evaluated and used as replacement value for ~A" (NTH 7 ETE))))

(DEFUN (:BREAK PROCEED) (IGNORE IGNORE)
  NIL)

;;; List problems with currently-loaded error table
(DEFUN LIST-PROBLEMS ()
  (LET ((ERRORS-WITH-NO-ERROR-MESSAGES NIL)
	(MISSING-RESTART-TAGS NIL)
	(TEM))
    (DOLIST (ETE ERROR-TABLE)
      (OR (GET (SETQ TEM (SECOND ETE)) 'INFORM)
	  (MEMQ TEM ERRORS-WITH-NO-ERROR-MESSAGES)
	  (PUSH TEM ERRORS-WITH-NO-ERROR-MESSAGES))
      (AND (SETQ TEM (ASSQ TEM		;Anything that calls SG-PROCEED-MICRO-PC
			   '((ARGTYP . 5) (SUBSCRIPT-OOB . 4))))
	   (SETQ TEM (NTH (CDR TEM) ETE))
	   (NOT (ASSQ TEM RESTART-LIST))
	   (NOT (MEMQ TEM MISSING-RESTART-TAGS))
	   (PUSH TEM MISSING-RESTART-TAGS)))
    (AND ERRORS-WITH-NO-ERROR-MESSAGES
	 (FORMAT T "~&Errors with no error messages: ~S" ERRORS-WITH-NO-ERROR-MESSAGES))
    (AND MISSING-RESTART-TAGS
	 (FORMAT T "~&Missing RESTART tags: ~S" MISSING-RESTART-TAGS))))

;;;; Not errors at all.

;; RESTART
;; This is not an error!
;; Arg is name of frob which you restart here.

;; CALLS-SUB
;; Arg is name of Lisp function which did the call, or something.


;;; Macro-code errors work by funcalling the error handler stack group
;;; with a list that looks like
;;; (FERROR proceedable-flag restartable-flag condition
;;;         format-control-string arg1 arg2 ...)

;;; (ERROR <message> &optional <object> <interrupt>)
;;; is for Maclisp compatibility.  It makes the error message
;;; out of <message> and <object>, and the condition out of <interrupt>'s
;;; CONDITION-NAME property.  The error is proceedable if
;;; <interrupt> is given.
(DEFUN ERROR (MESSAGE &OPTIONAL OBJECT INTERRUPT)
  (SIGNAL-ERROR (NOT (NULL INTERRUPT)) NIL
		(OR (GET INTERRUPT 'CONDITION-NAME)
		    INTERRUPT)
		(COND ((NULL OBJECT) "~*~A")
		      (T "~S ~A"))
		(LIST OBJECT MESSAGE)))

;;; (CERROR <proceed> <restart>
;;;         <condition> <format-control-string> <format-arg1> <format-arg2> ...)
;;; is the general way to report an error.  <format-control-string>
;;; and the args that follow it are arguments to FORMAT, for printing a message.
;;; <condition> together with <format-arg1> and following are the condition-list
;;; for signaling the condition.  <proceed> if non-NIL says that it is legal
;;; to proceed from the error.  If <proceed> is T, the value returned
;;; by the CERROR will be used instead of some bad value.  In this case,
;;; the error handler asks the user for a value to return.
;;; If <restart> is non-NIL, it is legal for the user or a condition handler
;;; to ask to restart.  Restarting works by throwing to ERROR-RESTART.
;;; See the definition of the ERROR-RESTART macro.
(DEFUN CERROR (PROCEEDABLE-FLAG RESTARTABLE-FLAG CONDITION FORMAT &REST ARGS)
  (SIGNAL-ERROR PROCEEDABLE-FLAG RESTARTABLE-FLAG CONDITION FORMAT ARGS))

(DEFPROP FERROR 3 BACKTRACE-LENGTH)
(DEFPROP FERROR 3 BACKTRACE-SKIP)

;;; (FERROR <condition> <format-control-string> <format-arg1> <format-arg2> ...)
;;; indicates an uncorrectable error.  <error-type> is the keyword
;;; to be used in signalling the error, together with <format-arg1>, ...
(DEFUN FERROR (CONDITION FORMAT &REST ARGS)
  (SIGNAL-ERROR NIL NIL CONDITION FORMAT ARGS))

(DEFUN (FERROR INFORM) (IGNORE ETE)
  (IF (OR (< (LENGTH ETE) 5)
	  (NOT (STRINGP (FIFTH ETE))))
      (FORMAT T "Uh-oh, bad arguments to ~S: ~S~%" (CAR ETE) (CDR ETE))
      (LEXPR-FUNCALL 'FORMAT T (CDDDDR ETE))))

(DEFUN SIGNAL-ERROR (PROCEEDABLE-FLAG RESTARTABLE-FLAG CONDITION FORMAT ARGS &AUX TEM1 TEM2)
  (MULTIPLE-VALUE (TEM1 TEM2) (LEXPR-FUNCALL 'SIGNAL CONDITION FORMAT ARGS))
  (COND ((EQ TEM1 'RETURN)
	 (IF PROCEEDABLE-FLAG TEM2
	     (FERROR NIL "Error-handler attempted to proceed when that wasn't possible")))
	((EQ TEM1 'RETURN-VALUE)
	 TEM2)
	((EQ TEM1 'ERROR-RESTART)
	 (AND RESTARTABLE-FLAG (*THROW 'ERROR-RESTART TEM2))
	 (FERROR NIL "Error-handler attempted to restart when that wasn't possible"))
	((NULL TEM1)
	 ;; SIGNAL did not find any handler willing to take the buck.
	 (FUNCALL %ERROR-HANDLER-STACK-GROUP
		  `(FERROR ,PROCEEDABLE-FLAG ,RESTARTABLE-FLAG
			   ,CONDITION ,FORMAT . ,ARGS)))
	(T
	 (FERROR NIL
		 "Error-handler said it handled an error but returned ~S" TEM1))))

;CONDITION-HANDLERS is a list of handling specs, each of which
;contains first either a condition name, a list of such names, or
;NIL meaning all condition names, and second
;a function to call to handle the condition.
;When you signal a condition with (SIGNAL condition-name info info info...)
;condition-handlers is searched for an element that applies to this
;condition name, and that element's function is called
;with the same arguments that signal was given (however many there were).
;If the function's first value is NIL, this means that the condition
;has not been handled, and the remaining handlers on the list should
;be given the chance to look at it.  Otherwise, the function's one
;or two values are returned by SIGNAL.

(DEFVAR CONDITION-HANDLERS NIL)

(DEFUN SIGNAL (&REST ARGS)
  (SIGNAL-1 CONDITION-HANDLERS ARGS))

(DEFUN SIGNAL-1 (HANDLER-LIST CONDITION-LIST &AUX (CNAME (CAR CONDITION-LIST)) TEM1 TEM2)
  (DO ((HANDLER-LIST HANDLER-LIST (CDR HANDLER-LIST))
       (H))
      ((NULL HANDLER-LIST) NIL)
    (SETQ H (CAR HANDLER-LIST))
    (COND ((COND ((NULL (CAR H)) T)
		 ((NLISTP (CAR H))
		  (EQ (CAR H) CNAME))
		 (T (MEMQ CNAME (CAR H))))
	   (MULTIPLE-VALUE (TEM1 TEM2)
	     (APPLY (CADR H) CONDITION-LIST))
	   (AND TEM1 (RETURN TEM1 TEM2))))))

(DEFMACRO-DISPLACE CONDITION-BIND (HANDLERS . BODY)
  `(LET ((CONDITION-HANDLERS (APPEND ',HANDLERS CONDITION-HANDLERS)))
     . ,BODY))

;; Does a stack group have anything that could try to handle this condition?
(DEFUN SG-CONDITION-HANDLED-P (SG CONDITION)
  (DOLIST (H (SYMEVAL-IN-STACK-GROUP 'CONDITION-HANDLERS SG))
    (AND (COND ((NULL (CAR H)) T)
	       ((NLISTP (CAR H)) (EQ (CAR H) CONDITION))
	       (T (MEMQ CONDITION (CAR H))))
	 (RETURN T))))

;; Make a stack-group's pdls larger if necessary
;; Note that these ROOM numbers need to be large enough to avoid getting into
;; a recursive trap situation, which turns out to be mighty big because footholds
;; are large and because the microcode is very conservative.
(DEFUN SG-MAYBE-GROW-PDLS (SG &OPTIONAL (MESSAGE-P T) (REGULAR-ROOM 2000) (SPECIAL-ROOM 400)
			   &AUX (RPP (SG-REGULAR-PDL-POINTER SG))
				(RPL (SG-REGULAR-PDL-LIMIT SG))
				(SPP (SG-SPECIAL-PDL-POINTER SG))
				(SPL (SG-SPECIAL-PDL-LIMIT SG)) TEM)
  (COND ((> (SETQ TEM (+ RPP REGULAR-ROOM)) RPL)
	 (AND MESSAGE-P (FORMAT ERROR-OUTPUT "~&[Growing regular pdl of ~S from ~S to ~S]~%"
					     SG RPL TEM))
	 (SETF (SG-REGULAR-PDL SG) (SG-GROW-PDL (SG-REGULAR-PDL SG) RPP TEM))
	 (SETF (SG-REGULAR-PDL-LIMIT SG) TEM)))
  (COND ((> (SETQ TEM (+ SPP SPECIAL-ROOM)) SPL)
	 (AND MESSAGE-P (FORMAT ERROR-OUTPUT "~&[Growing special pdl of ~S from ~S to ~S]~%"
					     SG SPL TEM))
	 (SETF (SG-SPECIAL-PDL SG) (SG-GROW-PDL (SG-SPECIAL-PDL SG) SPP TEM))
	 (SETF (SG-SPECIAL-PDL-LIMIT SG) TEM))))

;; Make a new array, copy the contents, store forwarding pointers, and return the new
;; array.  Also we have to relocate the contents of the array as we move it because the
;; microcode does not always check for forwarding pointers (e.g. in MKWRIT when
;; returning multiple values).
(DEFUN SG-GROW-PDL (PDL PDL-PTR NEW-LIMIT
		    &AUX (NEW-SIZE (MAX (// (* (ARRAY-LENGTH PDL) 4) 3) (+ NEW-LIMIT 100)))
			 NEW-PDL TEM TEM1 AREA)
  (SETQ PDL (FOLLOW-STRUCTURE-FORWARDING PDL))
  (COND (( (+ NEW-LIMIT 100) (ARRAY-LENGTH PDL)) PDL)	;Big enough, just adjust limit
	(T (COND ((= (SETQ AREA (%AREA-NUMBER PDL)) LINEAR-PDL-AREA)	;Stupid crock
		  (SETQ AREA PDL-AREA))			; with non-extendible areas
		 ((= AREA LINEAR-BIND-PDL-AREA)
		  (SETQ AREA WORKING-STORAGE-AREA)))
	   (SETQ NEW-PDL (MAKE-ARRAY AREA (ARRAY-TYPE PDL) NEW-SIZE
				     NIL (ARRAY-LEADER-LENGTH PDL)))
	   (DOTIMES (I (ARRAY-LEADER-LENGTH PDL))
	     (STORE-ARRAY-LEADER (ARRAY-LEADER PDL I) NEW-PDL I))
	   ;Can't do next line because of funny-looking data types and because
	   ;we must preserve the flag bits and cdr codes.
	   ;(COPY-ARRAY-PORTION PDL 0 (1+ PDL-PTR) NEW-PDL 0 (1+ PDL-PTR))
	   (DO ((N PDL-PTR (1- N))
		(FROM-P (ALOC-CAREFUL PDL 0) (%MAKE-POINTER-OFFSET DTP-LOCATIVE FROM-P 1))
		(TO-P (ALOC NEW-PDL 0) (%MAKE-POINTER-OFFSET DTP-LOCATIVE TO-P 1))
		(BASE (ALOC-CAREFUL PDL 0)))
	       ((MINUSP N))
	     (SELECT (%P-DATA-TYPE FROM-P)
	       ((DTP-FIX DTP-SMALL-FLONUM DTP-U-ENTRY)	;The only inum types we should see
		(%P-STORE-TAG-AND-POINTER TO-P (%P-LDB %%Q-ALL-BUT-POINTER FROM-P)
					       (%P-LDB %%Q-POINTER FROM-P)))
	       ((DTP-HEADER-FORWARD DTP-BODY-FORWARD)
		(FERROR NIL "Already forwarded? -- get help"))
	       (OTHERWISE
		(SETQ TEM (%P-CONTENTS-AS-LOCATIVE FROM-P)
		      TEM1 (%POINTER-DIFFERENCE TEM BASE))
		(AND (NOT (MINUSP TEM1)) ( TEM1 PDL-PTR)
		     (SETQ TEM (ALOC-CAREFUL NEW-PDL TEM1)))
		(%P-STORE-TAG-AND-POINTER TO-P (%P-LDB %%Q-ALL-BUT-POINTER FROM-P)
					       TEM))))
	   (STRUCTURE-FORWARD PDL NEW-PDL)
	   NEW-PDL)))
