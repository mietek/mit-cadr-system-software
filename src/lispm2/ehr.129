;; New error handler routines.		DLW 1/6/78 -*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

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
;; must have an EH-INFORM property, and may optionally have other
;; properties.  The EH-INFORM property should be a function which can 
;; be applied to the SG and the ETE, and print out an error message.
;;
;; The only other property defined is EH-PROCEED, which is used to proceed
;; after getting the error (i.e. the c-C command of the error handler).
;; It too should be a function to be applied to the SG and ETE,
;; and what it should do is to fix up the SAVED stack group state
;; appropriately, and return; the stack group will be started up in
;; SG-RESUMABLE state.
;;
;; Look at a few of these to get the idea of what they should look like.
;;
;; For errors signalled by macrocode (not microcode), the ETE looks like
;;	(FERROR proceedable-flag restartable-flag condition format-string . args)


;; This is like EH-CONTENTS, but always returns a fixnum with the
;; pointer part of the contents.  Useful if contents might be a small
;; untyped integer.
;; As a special kludge, if the tag is a number, the result is simply that
;; number.  The ARRAY-NUMBER-DIMENSIONS error, for example, is one.
(DEFUN EH-FIXNUM-CONTENTS (LOC SG)
  (COND ((NUMBERP LOC) LOC)
	((%P-LDB %%Q-POINTER (EH-LOCATE LOC SG)))))

(DEFUN EH-FIXNUM-STORE (X LOC SG)
    (%P-DPB X %%Q-POINTER (EH-LOCATE LOC SG)))

;;;; This class can happen from within the compilations of Lisp functions.
;;;; They are the kind for which EH-ERRING-FUNCTION is useful.

;; ARGTYP.
;; First arg is what it should have been (or a list of such).
;; Second arg is where offender is.
;; Third arg is which argument is was (zero based) (T = one and only, NIL = ?)
;; Fourth arg (optional) is RESTART address.  If it is NIL or missing,
;; then there is no way to recover.  [This code used to think that if the
;; restart address was missing then one should drop through to proceed, but
;; the microcode didn't think so, and in fact that case never happens.]

(DECLARE (SPECIAL EHR-ORDINAL-NAMES EHR-TYPE-NAMES EH-GOOD-TYPES))

(DEFUN (ARGTYP EH-INFORM) (SG ETE)
    (FORMAT T "~A argument to ~S, "
	    (CADR (ASSOC (FOURTH ETE) EHR-ORDINAL-NAMES))
	    (EH-ERRING-FUNCTION SG))
    (EH-P-PRIN1 (EH-LOCATE (THIRD ETE) SG))
    (FORMAT T ", was of the wrong type.~%The function expected ")
    (LET ((TYPE (SECOND ETE)))
	 (COND ((SYMBOLP TYPE)
		(EH-PRINC-TYPE TYPE))
	       ((LISTP TYPE)
		(COND ((< (LENGTH TYPE) 2)
		       (EH-PRINC-TYPE (CAR TYPE)))
		      (T (DO TL TYPE (CDR TL) (NULL TL)
			     (EH-PRINC-TYPE (CAR TL))
			     (PRINC (COND ((NULL (CDR TL)) "")
					  ((NULL (CDDR TL)) " or ")
					  (T ", ")))))))
	       (T (EH-BAD-HACKER TYPE " is not a type."))))
    (FORMAT T ".~%"))

(SETQ EHR-ORDINAL-NAMES
      '((T "The")	     ;There is only one argument.
	(0 "The first")	     ;Numbers in the file are zero-based, but
	(1 "The second")     ;  English ordinals are one based.
	(2 "The third")
	(3 "The fourth")
	(4 "The fifth")
	(5 "The sixth")
	(6 "The seventh")
	(NIL "Some")))	     ;Don't know which argument.

(DEFUN EH-PRINC-TYPE (TYPE)
    (PRINC (OR (CADR (ASSQ TYPE EHR-TYPE-NAMES)) TYPE)))

(SETQ EHR-TYPE-NAMES
      '((FIXNUM "a fixnum")
	(NUMBER "a number")
	(INTEGER "a fixnum or a bignum")
	(SYMBOL "a symbol")
	(CONS "a cons")
	(LIST "a list")
	(NIL "the symbol NIL")
	(LOCATIVE "a locative")
	(CLOSURE "a closure")
	(STACK-GROUP "a stack group")
	(ARRAY "an array")
	(ART-Q-LIST-ARRAY "an ART-Q-LIST array")
	(Q-ARRAY "an array of Lisp objects")
	(BYTE-ARRAY "an array of numbers")
	(BIGNUM "a bignum")
	(AREA "an area number, NIL (default), T (temporary), or a symbol whose value is one")
	(FIXNUM-FIELD "a byte pointer to a field that fits in a fixnum")))

(DEFUN (ARGTYP EH-SIGNAL) (SG ETE)
    `(:WRONG-TYPE-ARGUMENT ,(SECOND ETE) ,(EH-CONTENTS (THIRD ETE) SG)))

(DEFUN (ARGTYP EH-PROCEED) (SG ETE)
  (COND ((NULL (FIFTH ETE))
	 (FORMAT T "You cannot recover from this error.~%")
	 (*THROW 'EH-QUIT NIL)))
  (EH-STORE (EH-GET-OBJECT "Form to evaluate and use as replacement argument:")
	    (THIRD ETE)
	    SG)
  (EH-PROCEED-MICRO-PC SG (FIFTH ETE)))

;; This routine should be called by the EH-PROCEED routines for
;; microcode (non-FERROR) errors.  A restart micro pc is pushed onto
;; the saved micro-stack.  If TAG is NIL, this is the trap pc plus one,
;; continuing from the point of the trap.  Else look up the numerical
;; value of TAG, set by the RESTART-PC pseudo-op in the microcode.
;; If an EH-PROCEED routine doesn't call EH-PROCEED-MICRO-PC, then
;; control will be returned from the micro-routine that got the error.
(DEFUN EH-PROCEED-MICRO-PC (SG TAG)
  (LET ((PC (IF TAG (CDR (ASSQ TAG EH-RESTART-LIST)) (1+ (SG-TRAP-MICRO-PC SG)))))
    (COND ((NULL PC)
	   (EH-BAD-HACKER TAG " no such restart!")
	   (*THROW 'EH-QUIT NIL)))
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

(DEFUN (FIXNUM-OVERFLOW EH-INFORM) (SG IGNORE)
    (FORMAT T "~S got a fixnum overflow.~%"
	    (EH-ERRING-FUNCTION SG)))

(DEFUN (FIXNUM-OVERFLOW EH-PROCEED) (SG ETE &AUX NUM)
    (OR (MEMQ (THIRD ETE) '(PUSH NOPUSH))
	(EH-BAD-HACKER ETE "Bad ETE, must be PUSH or NOPUSH."))
    (SETQ NUM (EH-GET-OBJECT "Fixnum to return instead:"))
    (CHECK-ARG NUM FIXP "a fixnum")
    (EH-FIXNUM-STORE NUM (SECOND ETE) SG)
    (AND (EQ (THIRD ETE) 'PUSH)
	 (EH-REGPDL-PUSH NUM SG)))

(DEFPROP FIXNUM-OVERFLOW :FIXNUM-OVERFLOW EH-CONDITION)
(DEFPROP FIXNUM-OVERFLOW EH-SIGNAL-WITH-CONTENTS EH-SIGNAL)

;; FLOATING-EXPONENT-UNDERFLOW
;; Result is to be placed in M-T and pushed on the pdl.
;; Arg is SFL or FLO

(DEFUN (FLOATING-EXPONENT-UNDERFLOW EH-INFORM) (SG ETE)
  (FORMAT T "~S produced a result too small in magnitude to be
representable as a ~:[~;small ~] flonum.~%"
	  (EH-ERRING-FUNCTION SG)
	  (EQ (SECOND ETE) 'SFL)))

(DEFPROP FLOATING-EXPONENT-UNDERFLOW :FLOATING-EXPONENT-UNDERFLOW EH-CONDITION)
(DEFPROP FLOATING-EXPONENT-UNDERFLOW EHR-FLOATING-EXPONENT-UNDERFLOW-SIGNAL EH-SIGNAL)
(DEFUN EHR-FLOATING-EXPONENT-UNDERFLOW-SIGNAL (IGNORE ETE)
    `(,(GET (CAR ETE) 'EH-CONDITION)))

(DEFPROP FLOATING-EXPONENT-UNDERFLOW EHR-FLOATING-EXPONENT-PROCEED EH-PROCEED)

(DEFUN EHR-FLOATING-EXPONENT-PROCEED (SG ETE &AUX NUM)
  (DO () (())
      (SETQ NUM (EH-GET-OBJECT (COND ((EQ (SECOND ETE) 'SFL) "Small-flonum to return instead:")
				     (T "Flonum to return instead:"))))
      (COND ((AND (EQ (SECOND ETE) 'SFL)
		  (SMALL-FLOATP NUM))
	     (RETURN NIL))
	    ((FLOATP NUM)
	     (RETURN NIL)))
      (FORMAT T "Please use a ~:[~;small ~] flonum.~%" (EQ (SECOND ETE) 'SFL)))
    (EH-STORE NUM 'M-T SG)
    (AND (EQ (FIRST ETE) 'FLOATING-EXPONENT-OVERFLOW)
	 (EQ (SECOND ETE) 'SFL)
	 (EH-REGPDL-POP SG))
    (EH-REGPDL-PUSH NUM SG))

;; FLOATING-EXPONENT-OVERFLOW
;; Result is to be placed in M-T and pushed on the pdl.
;; Arg is SFL or FLO
;; In the case of SFL the pdl has already been pushed.

(DEFUN (FLOATING-EXPONENT-OVERFLOW EH-INFORM) (SG ETE)
  (FORMAT T "~S produced a result too large in magnitude to be
representable as a ~:[~;small ~] flonum.~%"
	  (EH-ERRING-FUNCTION SG)
	  (EQ (SECOND ETE) 'SFL)))

(DEFPROP FLOATING-EXPONENT-OVERFLOW :FLOATING-EXPONENT-OVERFLOW EH-CONDITION)
(DEFPROP FLOATING-EXPONENT-OVERFLOW EHR-FLOATING-EXPONENT-UNDERFLOW-SIGNAL EH-SIGNAL)

(DEFPROP FLOATING-EXPONENT-OVERFLOW EHR-FLOATING-EXPONENT-PROCEED EH-PROCEED)

;; DIVIDE-BY-ZERO
;; You cannot recover.

(DEFPROP DIVIDE-BY-ZERO EHR-DIVIDE-BY-ZERO-INFORM EH-INFORM)

(DEFUN EHR-DIVIDE-BY-ZERO-INFORM (SG IGNORE)
    (FORMAT T "There was an attempt to divide a number by zero in ~S.~%"
	    (EH-ERRING-FUNCTION SG)))

;; ARRAY-NUMBER-DIMENSIONS
;; First arg is how many we gave.
;; Second arg is how many is right.
;; You cannot recover.

(DEFUN (ARRAY-NUMBER-DIMENSIONS EH-INFORM) (SG ETE)
    (FORMAT T "~S was given a ~S-dimensional array; it expected a ~S-dimensional one.~%"
	    (EH-ERRING-FUNCTION SG)
	    (EH-FIXNUM-CONTENTS (SECOND ETE) SG)
	    (EH-FIXNUM-CONTENTS (THIRD ETE) SG)))

;; IALLB-TOO-SMALL
;; First arg is how many we asked for.

(DEFUN (IALLB-TOO-SMALL EH-INFORM) (SG ETE)
   (FORMAT T "There was a request to allocate ~S cells.~%"
	   (EH-FIXNUM-CONTENTS (SECOND ETE) SG)))

;; NUMBER-ARRAY-NOT-ALLOWED
;; First arg is where to find the array.
;; You cannot recover.

(DEFUN (NUMBER-ARRAY-NOT-ALLOWED EH-INFORM) (SG ETE)
  (FORMAT T "The array ~S, which was given to ~S, is not allowed to be a number array.~%"
	  (EH-CONTENTS (SECOND ETE) SG)
	  (EH-ERRING-FUNCTION SG)))

;; SUBSCRIPT-OOB
;; First arg is how many we gave.
;; Second is the legal limit.
;; Third optional arg is a restart tag.

(DEFUN (SUBSCRIPT-OOB EH-INFORM) (SG ETE)
    (LET ((USED (EH-FIXNUM-CONTENTS (SECOND ETE) SG))
	  (IN (EH-ERRING-FUNCTION SG)))
      (COND ((< USED 0)
	     (FORMAT T "The subscript, ~S, was negative in ~S~%"
		     USED IN))
	    (T 
	     (FORMAT T "The subscript, ~S, was beyond the length, ~S, in ~S~%"
		     USED
		     (EH-FIXNUM-CONTENTS (THIRD ETE) SG)
		     IN)))))

(DEFUN (SUBSCRIPT-OOB EH-PROCEED) (SG ETE &AUX NUM)
  (COND ((NULL (FOURTH ETE))
	 (FORMAT T "You cannot recover from this error.~%")
	 (*THROW 'EH-QUIT NIL)))
  (DO () (())
    (SETQ NUM (EH-GET-OBJECT "Subscript to use instead:"))
    (AND (FIXP NUM) (RETURN))
    (FORMAT T "Please use a fixnum.~%"))
  (EH-FIXNUM-STORE NUM (SECOND ETE) SG)
  (EH-PROCEED-MICRO-PC SG (FOURTH ETE)))

;; BAD-ARRAY-TYPE
;; First arg is where array header is. Note that it may well have a data type of DTP-TRAP.
;; You cannot recover.

(DEFUN (BAD-ARRAY-TYPE EH-INFORM) (SG ETE)
    (FORMAT T "The array type, ~S, was invalid in ~S.~%"
	    (LDB %%ARRAY-TYPE-FIELD (%P-POINTER (EH-LOCATE (SECOND ETE) SG)))
	    (EH-ERRING-FUNCTION SG)))

;; ARRAY-HAS-NO-LEADER
;; Arg is where array pointer is.
;; Recover from this by simply returning something else, by putting it in
;; M-T and discarding the return addr and restarting.

(DEFUN (ARRAY-HAS-NO-LEADER EH-INFORM) (SG ETE)
    (FORMAT T "The array given to ~S, ~S, has no leader.~%"
	    (EH-ERRING-FUNCTION SG)
	    (EH-CONTENTS (SECOND ETE) SG)))

(DEFPROP ARRAY-HAS-NO-LEADER :ARRAY-HAS-NO-LEADER EH-CONDITION)
(DEFPROP ARRAY-HAS-NO-LEADER EH-SIGNAL-WITH-CONTENTS EH-SIGNAL)

(DEFUN EH-SIGNAL-WITH-CONTENTS (SG ETE)
    (LIST (GET (CAR ETE) 'EH-CONDITION)
	  (EH-CONTENTS (SECOND ETE) SG)))

(DEFUN (ARRAY-HAS-NO-LEADER EH-PROCEED) (SG IGNORE)
  (EH-STORE (EH-GET-OBJECT "Form to evaluate and return instead:") 'M-T SG))

;; FILL-POINTER-NOT-FIXNUM
;; Arg is where array pointer is.
;; Recover by returning an arbitrary frob, just flush spurious return addr and restart.

(DEFUN (FILL-POINTER-NOT-FIXNUM EH-INFORM) (SG ETE)
    (FORMAT T "The fill-pointer of the array given to ~S, ~S, is not a fixnum.~%"
	    (EH-ERRING-FUNCTION SG)
	    (EH-CONTENTS (SECOND ETE) SG)))

(DEFPROP FILL-POINTER-NOT-FIXNUM :FILL-POINTER-NOT-FIXNUM EH-CONDITION)
(DEFPROP FILL-POINTER-NOT-FIXNUM EH-SIGNAL-WITH-CONTENTS EH-SIGNAL)

(DEFUN (FILL-POINTER-NOT-FIXNUM EH-PROCEED) (SG IGNORE)
  (EH-STORE (EH-GET-OBJECT "Form to evaluate and return instead:") 'M-T SG))

;; More random losses.

;arg is number which was called.
(DEFUN (NUMBER-CALLED-AS-FUNCTION EH-INFORM) (SG ETE)
   (FORMAT T "The number, ~S, was called as a function~%"
	     (EH-CONTENTS (SECOND ETE) SG)))

(DEFPROP NUMBER-CALLED-AS-FUNCTION :INVALID-FUNCTION EH-CONDITION)
(DEFPROP NUMBER-CALLED-AS-FUNCTION EH-SIGNAL-WITH-CONTENTS EH-SIGNAL)

;; FLONUM-NO-GOOD.  The argument has been lost at this point,
;; so can't use ARGTYP error, and cannot recover.

(DEFUN (FLONUM-NO-GOOD EH-INFORM) (SG IGNORE)
    (FORMAT T "~S does not accept floating-point arguments"
	    (EH-ERRING-FUNCTION SG)))

;; WRONG-SG-STATE
;; Arg is where sg is.
;; You cannot recover.

(DEFUN (WRONG-SG-STATE EH-INFORM) (SG ETE)
    (FORMAT T "The state of the stack group, ~S, given to ~S, was invalid.~%"
	    (EH-CONTENTS (SECOND ETE) SG)
	    (EH-ERRING-FUNCTION SG)))

(DEFPROP WRONG-SG-STATE :INVALID-SG-STATE EH-CONDITION)
(DEFPROP WRONG-SG-STATE EH-SIGNAL-WITH-CONTENTS EH-SIGNAL)

;; SG-RETURN-UNSAFE
;; No args, since the frob is in the previous-stack-group of the current one.
;; You cannot recover.

(DEFUN (SG-RETURN-UNSAFE EH-INFORM) (IGNORE IGNORE)
    (FORMAT T "An /"unsafe/" stack group attempted to STACK-GROUP-RETURN.~%"))

;; TV-ERASE-OFF-SCREEN
;; No arg.

(DEFUN (TV-ERASE-OFF-SCREEN EH-INFORM) (IGNORE IGNORE)
  (FORMAT T "The TV-ERASE function attempted to erase past the end of the screen.~%"))

;; THROW-TAG-NOT-SEEN
;; The tag has been moved to M-A for the EH to find it!
;; The value being thrown is in M-T, the *UNWIND-STACK count and action are in M-B and M-C.

(DEFUN (THROW-TAG-NOT-SEEN EH-INFORM) (SG IGNORE)
    (FORMAT T "There was no pending *CATCH for the tag ~S.~%"
	    (SG-AC-A SG))
    (FORMAT T "The value being thrown was ~S~%" (SG-AC-T SG))
    (AND (SG-AC-B SG)
	 (FORMAT T "While in a *UNWIND-STACK with remaining count of ~D.~%" (SG-AC-B SG)))
    (AND (SG-AC-C SG)
	 (FORMAT T "While in a *UNWIND-STACK with action ~S.~%" (SG-AC-C SG)))
)

(DEFUN (THROW-TAG-NOT-SEEN EH-SIGNAL) (SG IGNORE)
    `(:UNDEFINED-CATCH-TAG ,(SG-AC-A SG) ,(SG-AC-T SG) ,(SG-AC-B SG) ,(SG-AC-C SG)))

;; MVR-BAD-NUMBER
;; Where the # is.

(DEFUN (MVR-BAD-NUMBER EH-INFORM) (SG ETE)
    (FORMAT T "The function attempted to return ~D. values.~%"
	    (EH-FIXNUM-CONTENTS (SECOND ETE) SG)))

;;;; General Machine Lossages.

;; PDL-OVERFLOW
;; Arg is either SPECIAL or REGULAR

(DEFUN (PDL-OVERFLOW EH-INFORM) (IGNORE ETE)
    (FORMAT T "The ~A push-down list has overflown.~%"
	    (CADR (ASSQ (SECOND ETE) '((REGULAR "regular") (SPECIAL "special"))))))

;; ILLEGAL-INSTRUCTION
;; No args.

(DEFUN (ILLEGAL-INSTRUCTION EH-INFORM) (SG IGNORE)
    (FORMAT T "There was an attempt to execute an invalid instruction: ~O"
	    (LET ((AP (EH-PREVIOUS-ACTIVE SG (SG-AP SG))))
	       (EH-FEF-INSTRUCTION (AR-1 (SG-REGULAR-PDL SG) AP)
				   (RP-EXIT-PC (SG-REGULAR-PDL SG) AP)))))

;; BAD-CDR-CODE
;; Arg is where loser is.
(DEFUN (BAD-CDR-CODE EH-INFORM) (SG ETE)
    (FORMAT T "A bad cdr-code was found in memory (at address ~O)~%"
	    (EH-FIXNUM-CONTENTS (SECOND ETE) SG)))  ;Can't use Lisp print since will err again

;; DATA-TYPE-SCREWUP
;; This happens when some internal data structure contains wrong data type.  arg is name.
;; As it happens, all the names either start with a vowel or do if pronounced as letters
;; Not continuable
(DEFUN (DATA-TYPE-SCREWUP EH-INFORM) (IGNORE ETE)
  (FORMAT T "A bad data-type was found in the internal guts of an ~A~%" (SECOND ETE)))

;; STACK-FRAME-TOO-LARGE
(DEFUN (STACK-FRAME-TOO-LARGE EH-INFORM) (IGNORE IGNORE)
  (FORMAT T "Attempt to make a stack frame larger than 256. words"))

;; AREA-OVERFLOW
;; arg is register containing area#
(DEFUN (AREA-OVERFLOW EH-INFORM) (SG ETE)
  (LET ((AREA-NUMBER (EH-FIXNUM-CONTENTS (SECOND ETE) SG)))
    (FORMAT T "Allocation in the /"~A/" area exceeded the maximum of ~D. words.~%"
	      (AREA-NAME AREA-NUMBER)
	      (AREA-MAXIMUM-SIZE AREA-NUMBER))))

;; VIRTUAL-MEMORY-OVERFLOW
(DEFUN (VIRTUAL-MEMORY-OVERFLOW EH-INFORM) (IGNORE IGNORE)
  (FORMAT T "You've used up all available virtual memory!~%"))

;; REGION-TABLE-OVERFLOW
(DEFUN (REGION-TABLE-OVERFLOW EH-INFORM) (IGNORE IGNORE)
  (FORMAT T "Unable to create a new region because the region tables are full.~%"))

;; RPLACD-WRONG-REPRESENTATION-TYPE
;; arg is first argument to RPLACD
(DEFUN (RPLACD-WRONG-REPRESENTATION-TYPE EH-INFORM) (SG ETE)
  (FORMAT T "Attempt to RPLACD a list which is embedded in a structure and therefore/
cannot be RPLACD'ed.  The list is ~S~%"
	    (EH-CONTENTS (SECOND ETE) SG)))

;;;; Special cases.

;; MAR-BREAK
;; Special case.

(DEFUN (MAR-BREAK EH-INFORM) (SG IGNORE)
    (FORMAT T "The MAR has gone off because of an attempt to ~[read~;write~].~%"
	    (SG-FLAGS-PGF-WRITE SG)))

;(DEFPROP MAR-BREAK EHR-MAR-BREAK-PROCEED EH-PROCEED)
;Doesn't work because SGENT takes a page fault, causeing recursive page faults.
(DEFUN EHR-MAR-BREAK-PROCEED (SG IGNORE)
    (FORMAT T "Proceed from MAR break? ")
    (OR (Y-OR-N-P)
	(*THROW 'EH-QUIT NIL))
    (EH-PROCEED-MICRO-PC SG NIL))

;; TRANS-TRAP
;; Special case.

(DEFUN (TRANS-TRAP EH-INFORM) (SG IGNORE)
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
	      (Q-DATA-TYPES (%P-DATA-TYPE VMA))	;MAY NOT BE RIGHT. LATER,
	      (%P-POINTER VMA)		; THE BAD STUFF READ WILL GET SAVED
	      (%POINTER VMA))))		; BY UCODE AS 2 WDS: DTP & PNTR

(DEFUN (TRANS-TRAP EH-SIGNAL) (SG IGNORE)
    (LET ((VMA (SG-SAVED-VMA SG)))
      (AND (= (%P-DATA-TYPE VMA) DTP-NULL)
	   (LET ((SYMBOL (%MAKE-POINTER DTP-SYMBOL (%P-CONTENTS-AS-LOCATIVE VMA))))
	     (SELECT (%POINTER-DIFFERENCE VMA SYMBOL)
	       (1 `(:UNDEFINED-VARIABLE ,SYMBOL))
	       (2 `(:UNDEFINED-FUNCTION ,SYMBOL)))))))

(DEFUN (TRANS-TRAP EH-PROCEED) (SG IGNORE)
   (COND ((NOT (MEMQ (Q-DATA-TYPES (%P-DATA-TYPE (SG-SAVED-VMA SG))) EH-GOOD-TYPES))
	  (%P-STORE-CONTENTS (SG-SAVED-VMA SG)
			     (EH-GET-OBJECT "Form to evaluate and store back?"))))
   (EH-PROCEED-MICRO-PC SG NIL)) ;Drop through, will do transport again

;; FUNCTION-ENTRY
;; Special case.
;; The ucode kindly leaves the M-ERROR-SUBSTATUS pushed onto the
;; regular pdl so that we can find it.
;; The meanings of %%M-ESUBS-BAD-QUOTED-ARG, %%M-ESUBS-BAD-EVALED-ARG
;; and %%M-ESUBS-BAD-QUOTE-STATUS are not clear, as they are not used
;; by the microcode.

(DEFUN (FUNCTION-ENTRY EH-INFORM) (SG IGNORE)
    (FORMAT T "The function ~S was called with "
	    (EH-FUNCTION-NAME (AR-1 (SG-REGULAR-PDL SG)
				    (SG-AP SG))))
    (LET ((ERR (AR-1 (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG))))
      (DO ((STL '("too few arguments." "too many arguments."
		  "an argument of bad data type.") (CDR STL))
	   (SBL '(%%M-ESUBS-TOO-FEW-ARGS %%M-ESUBS-TOO-MANY-ARGS
		  %%M-ESUBS-BAD-DT) (CDR SBL)))
	  ((NULL SBL))
      	(AND (= (LDB (SYMEVAL (CAR SBL)) ERR) 1)
	     (FORMAT T (CAR STL)))))
    (TERPRI))

(DEFUN (FUNCTION-ENTRY EH-SIGNAL) (SG IGNORE)
    (LET ((ERR (AR-1 (SG-REGULAR-PDL SG) (SG-REGULAR-PDL-POINTER SG))))
      (COND ((OR (LDB-TEST %%M-ESUBS-TOO-FEW-ARGS ERR)
		 (LDB-TEST %%M-ESUBS-TOO-MANY-ARGS ERR))
	     `(:WRONG-NUMBER-OF-ARGUMENTS
	         ,(- (SG-REGULAR-PDL-POINTER SG) (SG-AP SG))
		 (AP-1 (SG-REGULAR-PDL SG) (1+ (SG-AP SG))))))))

(DEFUN (BREAKPOINT EH-INFORM) (IGNORE IGNORE)
    (FORMAT T "Breakpoint~%"))

(DEFUN (STEP-BREAK EH-INFORM) (SG IGNORE)
    (FORMAT T "Step break~%")
    (SETF (SG-INST-DISP SG) 0))


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
;;; SI:CONDITION-NAME property.  The error is proceedable if
;;; <interrupt> is given.
(DEFUN ERROR (MESSAGE &OPTIONAL OBJECT INTERRUPT)
    (EHR-SIGNAL-ERROR (NOT (NULL INTERRUPT)) NIL
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
    (EHR-SIGNAL-ERROR PROCEEDABLE-FLAG RESTARTABLE-FLAG CONDITION FORMAT ARGS))

(DEFPROP FERROR 3 EH-BACKTRACE-LENGTH)
(DEFPROP FERROR 3 EH-BACKTRACE-SKIP)

;;; (FERROR <condition> <format-control-string> <format-arg1> <format-arg2> ...)
;;; indicates an uncorrectable error.  <error-type> is the keyword
;;; to be used in signalling the error, together with <format-arg1>, ...
(DEFUN FERROR (CONDITION FORMAT &REST ARGS)
     (EHR-SIGNAL-ERROR NIL NIL CONDITION FORMAT ARGS))

(DEFUN (FERROR EH-INFORM) (IGNORE ETE)
    (COND ((OR (< (LENGTH ETE) 5)
	       (NOT (STRINGP (FIFTH ETE))))
	   (FORMAT T "Uh-oh, bad arguments to ~S: ~S~%" (CAR ETE) (CDR ETE)))
	  (T 
	   (LEXPR-FUNCALL 'FORMAT T (CDDDDR ETE)))))

(DEFUN EHR-SIGNAL-ERROR (PROCEEDABLE-FLAG RESTARTABLE-FLAG CONDITION FORMAT ARGS
					  &AUX TEM1 TEM2)
    (MULTIPLE-VALUE (TEM1 TEM2) (LEXPR-FUNCALL 'SIGNAL CONDITION FORMAT ARGS))
    (COND ((EQ TEM1 'RETURN)
	   (COND (PROCEEDABLE-FLAG TEM2)
		 (T
		  (FERROR NIL
			  "Error-handler attempted to proceed when that wasn't possible"))))
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

(DEFUN (:WRONG-TYPE-ARGUMENT EH-PROCEED) (IGNORE ETE)
  (EH-GET-OBJECT
    (FORMAT NIL "Form to be evaluated and used as replacement value for ~A" (NTH 7 ETE))))


(DEFUN (:BREAK EH-PROCEED) (IGNORE IGNORE)
  NIL)

;;; List problems with currently-loaded error table
(DEFUN EH-LIST-PROBLEMS ()
  (LET ((ERRORS-WITH-NO-ERROR-MESSAGES NIL)
	(MISSING-RESTART-TAGS NIL)
	(TEM))
    (DOLIST (ETE EH-ERROR-TABLE)
      (OR (GET (SETQ TEM (SECOND ETE)) 'EH-INFORM)
	  (MEMQ TEM ERRORS-WITH-NO-ERROR-MESSAGES)
	  (PUSH TEM ERRORS-WITH-NO-ERROR-MESSAGES))
      (AND (SETQ TEM (ASSQ TEM		;Anything that calls EH-PROCEED-MICRO-PC
			   '((ARGTYP . 5) (SUBSCRIPT-OOB . 4))))
	   (SETQ TEM (NTH (CDR TEM) ETE))
	   (NOT (ASSQ TEM EH-RESTART-LIST))
	   (NOT (MEMQ TEM MISSING-RESTART-TAGS))
	   (PUSH TEM MISSING-RESTART-TAGS)))
    (AND ERRORS-WITH-NO-ERROR-MESSAGES
	 (FORMAT T "~&Errors with no error messages: ~S" ERRORS-WITH-NO-ERROR-MESSAGES))
    (AND MISSING-RESTART-TAGS
	 (FORMAT T "~&Missing RESTART tags: ~S" MISSING-RESTART-TAGS))))
