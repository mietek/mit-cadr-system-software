; Discompiler			-*- Mode: Lisp; Package: Compiler -*-

;This was all completely wrong, I'm afraid.  Instead of this elaborate data
;structure it should simply make Lisp, with a separate table associating
;PC values with non-atomic S-expressions.  I'm not sure if the current
;plan for un-flattening goto's and lambda-bindings is correct.  Note
;that something needs to be done about order of evaluation, currently
;PROG1 and PROGN aren't distinguished.

;;; Remains to be written:
;;; Branch-reducer:
;;;   the pattern (... (branch tag) ... code ... tag ... )
;;;   where tag has only one branch to it, and code has no
;;;   branches into it from outside, turns into
;;;   (... (COND (condition ...code...)) ...)
;;;   then adjacent COND's have to be merged, some have to
;;;   be changed into AND, OR, or IF
;;; Conversion of effects to nothing, PROGN, or PROG
;;; Insertion of internal lambdas?  Probably can't be done
;;; Handling of D-RETURN (has to be done last?) and D-NEXT-LIST
;;; Special misc functions
;;; Effects isn't being done right.
;;; Stack entries need to be labelled with PC's?

; The simulated stack is a list which represents the contents of the stack
; when execution is at the current place.

(DECLARE (SPECIAL DISCOMPILE-PC			;Current program counter
		  DISCOMPILE-PC-TABLE		;alist of (sexp . pc) for non-atomic s-exps
		  DISCOMPILE-STACK-LEVEL	;Next index to fill in the stack
		  DISCOMPILE-STACK-CONTENTS	;Array of nodes, usually
		  DISCOMPILE-STACK-FLAVOR	;Array of flavor symbols:
						; LISP - contents is an S-expression
						; CALL - contents is list of
						;   function to call, dest, effects, pc,
						;   catch-info
		  DISCOMPILE-EFFECTS		;Current effects list consed up here
						;Things that use indicators can take off here
		  DISCOMPILE-LAST-DEST		;Indicates whether thing in indicators
						; is on the stack or on the effects
		  DISCOMPILE-LABELS		;List of elements:
						; (pc-of-label . node-which-goes-to)
						;Opening a *CATCH constitutes a goto.
		  ))

;;; Code manipulation

;;; Generate a non-atomic piece of code, this is just to update the PC table
(DEFUN DISCOMPILE-CODE (&REST ELEMENTS) ;function and arguments
  (LET ((CODE (APPEND ELEMENTS NIL)))
    (PUSH (CONS CODE DISCOMPILE-PC) DISCOMPILE-PC-TABLE)
    CODE))

;;; Add effects to code.  I'm not sure if this completely correct as it stands...
(DEFUN DISCOMPILE-INCORPORATE (CODE EFFECTS)
  (COND ((NULL EFFECTS) CODE)
	(T (LET ((NCODE (CONS 'PROGN		;May change into PROG later
			      (NCONC (REVERSE EFFECTS) (LIST CODE))))
		 (PC (CDR (ASSQ CODE DISCOMPILE-PC-TABLE))))
	     (AND PC (PUSH (CONS NCODE PC) DISCOMPILE-PC-TABLE))
	     NCODE))))

;;; Stack manipulation
(DEFUN DISCOMPILE-PUSH (FLAVOR CONTENTS)
  (ASET FLAVOR DISCOMPILE-STACK-FLAVOR DISCOMPILE-STACK-LEVEL)
  (ASET CONTENTS DISCOMPILE-STACK-CONTENTS DISCOMPILE-STACK-LEVEL)
  (SETQ DISCOMPILE-STACK-LEVEL (1+ DISCOMPILE-STACK-LEVEL)))

;;; Will only pop LISP flavor stuff
(DEFUN DISCOMPILE-POP ()
  (SETQ DISCOMPILE-STACK-LEVEL (1- DISCOMPILE-STACK-LEVEL))
  (OR (EQ (AREF DISCOMPILE-STACK-FLAVOR DISCOMPILE-STACK-LEVEL) 'LISP)
      (FERROR NIL "stack screwed"))
  (AREF DISCOMPILE-STACK-CONTENTS DISCOMPILE-STACK-LEVEL))

;;; Here when I say node I mean any S-expression actually

;;; Create a node of GO flavor.  There will never be any effects
;;; since the enclosing node will have taken care of them.
(DEFUN DISCOMPILE-GOTO (DEST-PC)
  (LET ((NODE (DISCOMPILE-CODE 'GO DEST-PC)))
    (PUSH (CONS DEST-PC NODE) DISCOMPILE-LABELS)
    NODE))

;;; Send a node to a destination (symbolic)
(DEFUN DISCOMPILE-TO-DESTINATION (NODE DEST)
  (SETQ DISCOMPILE-LAST-DEST DEST)
  (SELECTQ DEST
    (D-IGNORE (PUSH NODE DISCOMPILE-EFFECTS))
    ((D-PDL D-NEXT D-LAST)
     (SETQ NODE (DISCOMPILE-INCORPORATE NODE DISCOMPILE-EFFECTS)) ;? this isn't quite right?
     (SETQ DISCOMPILE-EFFECTS NIL)
     (DISCOMPILE-PUSH 'LISP NODE)
     (AND (EQ DEST 'D-LAST) (DISCOMPILE-ACTIVATE-OPEN-CALL-BLOCK)))
    (OTHERWISE (FERROR NIL "FOO"))))

;;; Open a call block.  Pushes the effects.
(DEFUN DISCOMPILE-OPEN-CALL-BLOCK (FCN DEST)
  (DISCOMPILE-PUSH 'CALL (LIST FCN DEST DISCOMPILE-EFFECTS DISCOMPILE-PC))
  (SETQ DISCOMPILE-EFFECTS NIL))

;;; Remove all the arguments from the stack, and the open-call marker, and
;;; send the results of the function to the saved destination
(DEFUN DISCOMPILE-ACTIVATE-OPEN-CALL-BLOCK ()
  (DO ((PP (1- DISCOMPILE-STACK-LEVEL) (1- PP))
       (ARGS NIL (CONS (AREF DISCOMPILE-STACK-CONTENTS PP) ARGS))
       (CALL-INFO)(CALL-PC)(FCN)(EFFECTS)(RESULT)(DEST))
      ((NEQ (AREF DISCOMPILE-STACK-FLAVOR PP) 'LISP)
       (SETQ DISCOMPILE-STACK-LEVEL PP)
       (SETQ CALL-INFO (AREF DISCOMPILE-STACK-CONTENTS PP))
       (SETQ FCN (CAR CALL-INFO)
	     DEST (CADR CALL-INFO)
	     EFFECTS (CADDR CALL-INFO)		;Effects before opening the call
	     CALL-PC (CADDDR CALL-INFO))
       (SETQ RESULT (CONS FCN ARGS))
       (PUSH (CONS RESULT CALL-PC) DISCOMPILE-PC-TABLE)
       (DISCOMPILE-TO-DESTINATION (DISCOMPILE-INCORPORATE RESULT EFFECTS) DEST))))

;;; For a miscellaneous function which is just a fast function call
(DEFUN DISCOMPILE-ORDINARY-MISC-FUNCTION (FCN DEST)
  (DO ((NARGS (LDB %%ARG-DESC-MAX-ARGS (%ARGS-INFO FCN-NAME)) (1- NARGS))
       (PP (1- DISCOMPILE-STACK-LEVEL) (1- PP))
       (ARGS NIL (CONS (AREF DISCOMPILE-STACK-CONTENTS PP) ARGS))
       (RESULT)(CALL-PC NIL))
      ((ZEROP NARGS)
       (SETQ DISCOMPILE-STACK-LEVEL (1+ PP))
       (DOLIST (A ARGS)				;Find earliest associated PC
	 (AND (NOT (ATOM A))
	      (SETQ CALL-PC (CDR (ASSQ A DISCOMPILE-PC-TABLE)))
	      (RETURN)))
       (OR CALL-PC (SETQ CALL-PC DISCOMPILE-PC))
       (SETQ RESULT (CONS FCN ARGS))
       (PUSH (CONS RESULT CALL-PC) DISCOMPILE-PC-TABLE)
       (DISCOMPILE-TO-DESTINATION RESULT DEST))))

;;; Driver
(DEFUN DISCOMPILE (FUNCTION &AUX FEF LIM-PC ILEN)
  (COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	 (SETQ FEF FUNCTION))
        ((SETQ FEF (FDEFINITION FUNCTION))))
  (OR (= (%DATA-TYPE FEF) DTP-FEF-POINTER)
      (FERROR NIL "Can't find FEF for ~S" FUNCTION))
  (SETQ LIM-PC (DISASSEMBLE-LIM-PC FEF))
  (DO ((DISCOMPILE-PC (SI:FEF-INITIAL-PC FEF) (+ PC ILEN))
       (DISCOMPILE-PC-TABLE NIL)
       (DISCOMPILE-STACK-LEVEL 0)
       (DISCOMPILE-STACK-CONTENTS (MAKE-ARRAY NIL 'ART-Q 400))
       (DISCOMPILE-STACK-FLAVOR (MAKE-ARRAY NIL 'ART-Q 400))
       (DISCOMPILE-EFFECTS NIL)
       (DISCOMPILE-LAST-DEST NIL)
       (DISCOMPILE-LABELS NIL))
      ((>= PC LIM-PC)
       (DISCOMPILE-FINISH))
    (SETQ ILEN (DISCOMPILE-INSTRUCTION FEF DISCOMPILE-PC))))

;Make a node for the address of an instruction.
;No effects are included, higher-level code should take care of this.
(DEFUN DISCOMPILE-ADDRESS (FEF REG DISP &AUX (SDISP (LOGAND 77 DISP)) EVCP SYMBOL OFFSET)
  (COND ((= DISP 777)				;pop PDL
	 (DISCOMPILE-POP))
	(T					;anything else will be atomic
	 (COND ((= REG 4)			;constant
		(SETQ REG (CONSTANTS-AREA SDISP))
		(OR (EQ REG NIL) (EQ REG T) (NUMBERP REG) ;Quote if not self-quoting
		    (SETQ REG `',REG))
		REG)
	       ((= REG 5)			;local
		(DISASSEMBLE-LOCAL-NAME FEF SDISP))
	       ((= REG 6)			;arg
		(DISASSEMBLE-ARG-NAME FEF SDISP))
	       ((= REG 7)			;illegal
		(FERROR NIL "Illegal address here"))
	       ;; FEF reference of some sort
	       ((= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF DISP)
		   DTP-EXTERNAL-VALUE-CELL-POINTER)
		(SETQ EVCP (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF DISP)
		      SYMBOL (%FIND-STRUCTURE-HEADER EVCP)
		      OFFSET (%POINTER-DIFFERENCE EVCP SYMBOL))
		(COND ((= OFFSET 1) SYMBOL)
		      ((= OFFSET 2) (LIST 'FUNCTION SYMBOL))
		      (T (FERROR NIL "Illegal EVCP here"))))
	       (T (SETQ REG (%P-CONTENTS-OFFSET FEF DISP))
		  (OR (EQ REG NIL) (EQ REG T) (NUMBERP REG) ;Quote if not self-quoting
		      (SETQ REG `',REG))
		  REG)))))

(DEFUN DISCOMPILE-2-ARGS-ND (OP DEST ARG2)
  (DISCOMPILE-TO-DESTINATION DEST
    (DISCOMPILE-CODE OP (DISCOMPILE-POP) ARG2)))

;Returns the length of the instruction, usually 1. 
(DEFUN DISCOMPILE-INSTRUCTION (FEF PC
			       &AUX WD OP DEST REG DISP ARG)
  (PROG NIL  ;PROG so that RETURN can be used to return unusual instruction lengths. 
    (SETQ WD (DISASSEMBLE-FETCH FEF PC))
    (SETQ OP (LDB 1104 WD)
	  DEST (LDB 1503 WD)
	  DISP (LDB 0011 WD)
	  REG (LDB 0603 WD))
    (COND ((ZEROP WD)) ;Ignore zero padding at the end of the function
	  ((< OP 11)     ;DEST/ADDR 
	   (SETQ OP (NTH OP '(CALL CALL0 MOVE CAR CDR CADR CDDR CDAR CAAR))
		 DEST (NTH DEST '(D-IGNORE D-PDL D-NEXT D-LAST
				  D-RETURN D-NEXT-Q D-LAST-Q D-NEXT-LIST))
		 ARG (DISCOMPILE-ADDRESS FEF REG DISP))
	   (SELECTQ OP
	     (CALL (DISCOMPILE-OPEN-CALL-BLOCK ARG DEST))
	     (CALL0 (DISCOMPILE-OPEN-CALL-BLOCK ARG DEST)
		    (DISCOMPILE-ACTIVATE-OPEN-CALL-BLOCK))
	     (MOVE (DISCOMPILE-TO-DESTINATION ARG DEST))
	     (OTHERWISE (DISCOMPILE-TO-DESTINATION (DISCOMPILE-CODE OP ARG)
						   DEST))))
	  ((= OP 11)	    ;ND1
	   (SETQ OP (NTH DEST '(ND1-UNUSED + - * // LOGAND LOGXOR LOGIOR)))
	   (DISCOMPILE-2-ARGS-ND OP 'D-PDL (DISCOMPILE-ADDRESS REF REG DISP)))
	  ((= OP 12)	    ;ND2
	   (SETQ OP (NTH DEST '(= > < EQ ;first 4 are 2 args, second 4 are SETE's
			        CDR CDDR 1+ 1-)))
	   (SETQ ARG (DISCOMPILE-ADDRESS FEF REG DISP))
	   (SELECTQ OP
	     ((= > < EQ)
	      (DISCOMPILE-2-ARGS-ND OP 'D-IGNORE ARG))
	     (OTHERWISE
	      (DISCOMPILE-TO-DESTINATION (DISCOMPILE-CODE 'SETQ ARG `(,OP ,ARG))
					 'D-IGNORE))))
	  ((= OP 13)	    ;ND3
	   (SETQ OP (NTH DEST '(BIND-OBSOLETE? BIND-NIL BIND-POP SET-NIL
				SET-ZERO PUSH-E MOVEM POP)))
	   (SETQ ARG (DISCOMPILE-ADDRESS FEF REG DISP))
	   (SELECTQ DEST
	     ((0 1 2)	;Binders...with more hair could generate LAMBDA?
	      (DISCOMPILE-TO-DESTINATION (DISCOMPILE-CODE 'BIND
							  ARG
							  (COND ((= DEST 1) NIL)
								((= DEST 2) (DISCOMPILE-POP))
								((= DEST 3) '??)))
					 'D-IGNORE))
	     ((3 4)	;SET-NIL, SET-ZERO
	       (DISCOMPILE-TO-DESTINATION
		 (DISCOMPILE-CODE 'SETQ ARG (NTH (- DEST 3) '(NIL 0)))
		 'D-IGNORE))
	     (5		;PUSH-ADDRESS
	       (DISCOMPILE-PUSH 'LISP (DISCOMPILE-CODE 'LOCF ARG)))
	     (6		;MOVEM
	       (DISCOMPILE-TO-DESTINATION
		 (DISCOMPILE-CODE 'SETQ ARG
				  (AREF DISCOMPILE-STACK-CONTENTS (1- DISCOMPILE-STACK-LEVEL)))
		 'D-IGNORE))
	     (7		;POP
	       (DISCOMPILE-TO-DESTINATION
		 (DISCOMPILE-CODE 'SETQ ARG (DISCOMPILE-POP))
		 'D-IGNORE))))
;Below here not done
	  ((= OP 14)	    ;BRANCH
	   (PRINC (NTH DEST '(BR BR-NIL BR-NOT-NIL BR-NIL-POP
			      BR-NOT-NIL-POP BR-ATOM BR-NOT-ATOM BR-ILL-7)))
	   (TYO 40)
	   (AND (> DISP 400) (SETQ DISP (LOGIOR -400 DISP))) ;SIGN-EXTEND
	   (COND ((NEQ DISP -1)	    ;ONE WORD
		  (PRIN1 (+ PC DISP 1)))
		 (T	    ;LONG BRANCH
		  (SETQ DISP (DISASSEMBLE-FETCH FEF (SETQ PC (1+ PC))))
		  (AND (> DISP 100000) (SETQ DISP (LOGIOR -100000 DISP)))
		  (PRINC "*")	;INDICATE LONG BRANCH FOR USER.
		  (PRIN1 (+ PC DISP 1))
		  (RETURN 2))))
	  ((= OP 15)	    ;MISC
	   (PRINC "(MISC) ") ;Moon likes to see this
	   (COND ((< DISP 100) (FORMAT T "LIST ~D long " DISP))
		 ((< DISP 200) (FORMAT T "LIST-IN-AREA ~D long " (- DISP 100)))
		 ((< DISP 220) (FORMAT T "UNBIND ~D bindings " (- DISP 177)))
		 ((< DISP 240) (FORMAT T "POP-PDL ~D times " (- DISP 217)))
		 (T
                  (LET ((OP (MICRO-CODE-SYMBOL-NAME-AREA (- DISP 200))))
                    (COND ((NULL OP) (FORMAT T "#~D " DISP))
                          (T (FORMAT T "~A " OP))))))
	   (PRINC (NTH DEST '(D-IGNORE D-PDL D-NEXT D-LAST
			      D-RETURN D-NEXT-Q D-LAST-Q D-NEXT-LIST))))
	  (T		    ;UNDEF
	   (PRINC 'UNDEF-)
	   (PRIN1 OP)))
    (RETURN 1)))

(DEFUN DISCOMPILE-2-ARGS-ND (OP DEST ARG2)
  (LET ((ARG1 (AREF DISCOMPILE-STACK-CONTENTS
		    (SETQ DISCOMPILE-STACK-LEVEL (1- DISCOMPILE-STACK-LEVEL)))))
    (DISCOMPILE-TO-DESTINATION (LIST DISCOMPILE-PC
				     'CALL
				     (LIST OP ARG1 ARG2)))))
