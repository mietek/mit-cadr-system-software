;-*- MODE: LISP; PACKAGE: COMPILER; BASE: 8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

; these should probably eventually go in DEFMIC
(DEFPROP %DATA-TYPE DTP-FIX RESULT-DATA-TYPE)
(DEFPROP M-EQ T-OR-NIL RESULT-DATA-TYPE)
(DEFPROP M-=  T-OR-NIL RESULT-DATA-TYPE)


;An optimization is a several-to-several source to source transformation in MCLAP.
; It is expressed in pattern - action form.
; The pattern spans some number of instructions, and can match against various aspects
;  of the instruction including code and operands.
; The action corresponds with the pattern and usually acts on the matched code.

;Optimization pattern matching "language"
; a list of transformations each of which is
;   a list whose first element is
;     a list each element of which is to be matched the code of an instruction.
;   whose second element is
;     a list of actions, the elements of which are corresponded with matched instructions

;Pattern variables are denoted by (== <name> <restriction predicates> ..).
; <name> can be a list, see discussion of name chaining below.
; Similiar to most pattern matching languages, pattern variables start out unassigned.
;   The first time the variable is seen, it becomes assigned to whatever it was matched
;   against.  If the same variable is seen again, it must be matched against EQUAL
;   list structure to its assignment in order for the match to succeed.
; The following pattern variables are special:
;   ? matches anything, always.
;   *INST* is the instruction being matched.
;   *FUNCTION-ARG* is the argument to the current function.
; <restriction predicates> are LISP expressions.  However, the proposed value for
;   the variable is "stuck in" as the first arg.  For example, (MEMQ (JUMP JUMP-NOT-EQUAL))
;   would restrict the variable to matching one of those symbols.
; Non-NIL symbols in the CDR position of lists are also pattern variables.

;variable chaining or "non-atomic" variables.
; The MAP-MATCH construct matches a single pattern to each of the members of a list.
;For example, the list of instructions that can preceed the current one.
; Since the same pattern is being reused multiple times, a mechanism
;is needed to "index" the variables, otherwise, they would be shared between all
;matches.  This capability is provided by associating a control variable
;with the MAP-MATCH, then using this variable as part of a chained
;variable name.  Since the control variable will change as the MAP-MATCH 
;progresses, this provides unique variables for each application
;of the pattern.  
;  The variable *namechain* is the list of the control variables of currently
;nested MAP-MATCHs.  This chain is postfixed by default to all generated
;pattern variables with names that are symbols.  With a pattern variable whose
;name is a list, this postfixing can be suppressed or modified.  In particular,
;(== (= foo)) suppresses all prefixing, and references foo. 
;(== (* foo)) cdr's *namechain* before postfixing, thus referencing the previous
;"lexical" level.
;  When the variable is "seen", all symbols but the first are replaced
;with their current values, thus forming the desired name.  Note chained variables
;cannot be used in the dotted REST position, since they could not be recognized.
;However, an ordinary variable can be used to receive the value, and then the value
;moved into a "safe" chained variable via LET.

; MATCH-FUNCTIONs allow recursive patterns as well as "subroutinizing" pattern expressions.
;The "argument" of the function is the new binding for *namechain*.
; Similarily to a optimization, MATCH-FUNCTION has two parts, a pattern part
;and an action part.


; MATCH-CASE provides an OR capability (without duplicating the entire pattern).
;Furthermore, a control variable is associated which gets set to the path number
;that was successful.  This variable is then available at ACTION time to select
;a corresponding ACTION via PUT-CASE.

(DEFCONST *MA-OPT-TRACE-VARS* NIL)	;print when vars on this list assigned.
(DEFCONST *MA-OPT-TRACE-ACTS* NIL)
(DEFCONST *MA-OPT-MATCH-TRACE* NIL)
(DEFCONST *MA-OPT-PRINT-INPUT* NIL)

(DEFVAR *NAMECHAIN* NIL)		;postfixed to pattern variables, see above.

(DEFCONST *MA-OPTIMIZATIONS* '(
  ;flush move to self
    ( ( ((CODE (MOVE (== O) (== O))  )) )
      ( ((FLUSH) )   	      ) )
  ;combine push followed immediately by pop.
    ( ( ((CODE (MOVE (PUSH-PDL (== ?)) (== S) . REST) ))
        ((CODE (MOVE (== D) (PDL-POP)) )) )
      ( ((FLUSH))
        ((PUT  (MOVE (== D) (== S) . REST)))  ) )
  ;combine move x s, ... move d,x where ... is within sequence and doesnt clobber x.
    ( ( ((CODE (MOVE (== D) (== X (SYMBOLP))))
	 (OP1-LIST (MA-ONE-AND-ONLY-USE-P))
	 (LET I2 (== *INST*))
	 (LET-APPLY-TEST I1 (MA-OP1-SOURCE-INST (== *INST*)))
	 (MATCH (== I1)
		((PRED (MA-SAME-SEQUENCE (== *INST*) (== I2)))
		 (CODE (MOVE (== X) (== S) . REST))
		 (MAP-RANGE (== *INST*) (== I2) (== D) NO-REFERENCE)
		 (MAP-RANGE (== *INST*) (== I2) (== X) NO-CLOBBER)))))
      ( ((FLUSH)
	 (PUT-INST (== I1)
		   ((PUT (MOVE (== D) (== S) . REST)))))))
  ;combine MOVE into following JUMP if arg can live in A-MEM.  
    ( ( ((CODE (MOVE (== R) (== O (MA-CAN-LIVE-IN-A-MEM))))
	 (RESULT-OP (MA-OPERAND-ONE-USE-P)))   ;not used elsewhere
        ((CODE ((== I (MEMQ (JUMP-EQUAL JUMP-NOT-EQUAL))) (== M) (== R) (UTAG (== TAG)))) ) )
      ( ((FLUSH))
        ((PUT  ((== I) (== M) (== O) (UTAG (== TAG)))))))
  ;flush lossage if a (MOVE T (PDL-POP)) is reached only by
  ;     	 (MOVE (PDL-PUSH) T)  or
  ;		 (MOVE (PDL-PUSH) T) (JUMP ..) or
  ;		 (MOVE (PDL-PUSH) T) (<CONDITIONAL-JUMP> T ..>) (DISCARD-TOP-OF-STACK)
  ;This happens quite frequently, due to CONDs.

    ( ( ((CODE (MOVE T (PDL-POP)))
	 (MATCH-FUNCTION OP-IN-T-AND-PDL ?)))
     (  ((MATCH-FUNCTION OP-IN-T-AND-PDL ?)
	 (FLUSH))))
))

;If there is a PUSH-PDL in all paths to the current instruction, factor
;it out and leave the result in T instead.  If a non-? argument is supplied,
;it is a source which must have the same data as was pushed, or, in other
;words, the push must have been a (MOVE (PUSH-PDL ?) <source>).  This is of
;interest when <source> is being tested by a conditional jump.
(DEFPROP OP-IN-T-AND-PDL
   ( ((LET IN (== *INST*))
      (MAP-MATCH %%1 (MA-PRECEEDING-INSTS-LIST)
	((MATCH-CASE OP
	    ( ((CODE (MOVE (PUSH-PDL (== ?)) (== SOURCE)))	 ;drop thru
	       (PRED (MA-MATCH-EQUAL (== (* *FUNCTION-ARG*))
				     (== SOURCE))))
	      ((CODE (JUMP NIL NIL (UTAG (== TAG))))		 ;unconditional jump
	       (MATCH-FUNCTION OP-IN-T-AND-PDL (== (* *FUNCTION-ARG*))))
	      ((CODE ((== I (MEMQ (JUMP-EQUAL JUMP-NOT-EQUAL)))  ;conditional jump
		      T
		      (== A)
		      (UTAG (== TAG))))
	       (MATCH-FUNCTION OP-IN-T-AND-PDL T)
	       (MAP-MATCH %%2 (MA-FOLLOWING-INSTS-LIST)
			  ((MATCH-CASE C
			       ( ((PRED (EQ (== *INST*) (== (* (* IN))))))
				 ((CODE (DISCARD-TOP-OF-STACK))))))))
	     )) )))
     ((MAP-PUT %%1 (MA-PRECEEDING-INSTS-LIST)
	 ((PUT-CASE OP
	    ( ((PUT (MOVE T (== SOURCE))))	     		 ;drop thru
	      ((MATCH-FUNCTION OP-IN-T-AND-PDL
			       (== (* *FUNCTION-ARG*))))	 ;unconditional jump
	      ((MATCH-FUNCTION OP-IN-T-AND-PDL T)    		 ;conditional jump
	       (MAP-PUT %%2 (MA-FOLLOWING-INSTS-LIST)
			((PUT-CASE C
			   ( ()
			    ((FLUSH)))))))))))))
   MA-OPT-FUNCTION)

(DEFPROP M-EQ (
  ;open code EQ
    ( ( ((CODE (MOVE (PUSH-PDL (== ?)) (== S))))
        ((CODE (CALL (POPS 2) (== ?) (MISC-ENTRY M-EQ))))
        ((CODE ((== I (MEMQ (JUMP-EQUAL JUMP-NOT-EQUAL))) T A-V-NIL (UTAG (== TAG))))
	 (OP1-LIST (MA-ONE-AND-ONLY-USE-P))) )
      ( ((PUT  (MOVE C (PDL-POP))))
	((PUT  (MOVE B (== S))) )
	((PUT  ((== I (MA-INVERT (JUMP-EQUAL JUMP-NOT-EQUAL))) C B (UTAG (== TAG)))))	) )
    )
    MA-OPTIMIZATIONS)

(DEFPROP M-= (
  ;open code =.  Must know both operands are fixnums.
    ( ( ((CODE (MOVE (PUSH-PDL (== ?)) (== S) . REST)))
        ((CODE (CALL (POPS 2) (== ?) (MISC-ENTRY M-=)))
	 (STACK-OPERANDS-MAP (MA-OPERAND-FIX-P NIL)))
        ((CODE ((== I (MEMQ (JUMP-EQUAL JUMP-NOT-EQUAL))) T A-V-NIL (UTAG (== TAG))))
	 (OP1-LIST (MA-ONE-AND-ONLY-USE-P))) )
      ( ((PUT  (MOVE C (PDL-POP))))
	((PUT  (MOVE B (== S) . REST)) )
	((PUT  ((== I (MA-INVERT (JUMP-EQUAL JUMP-NOT-EQUAL))) C B (UTAG (== TAG)))))	) )
    )
    MA-OPTIMIZATIONS)

(DEFPROP %DATA-TYPE (
    ( ( ((CODE (MOVE (PUSH-PDL (== ?)) (== S))))
        ((CODE (CALL (POPS 1) (== ?) (MISC-ENTRY %DATA-TYPE)))) )
      ( ((PUT  (MOVE T (== S) (DTP-FIX 5 24.))))
        ((FLUSH)) ) ))
    MA-OPTIMIZATIONS)

(DEFPROP %POINTER (
    ( ( ((CODE (MOVE (PUSH-PDL (== ?)) (== S))))
        ((CODE (CALL (POPS 1) (== ?) (MISC-ENTRY %POINTER)))) )
      ( ((PUT  (MOVE T (== S) (DTP-FIX 24. 0))))
        ((FLUSH)) ) ))
    MA-OPTIMIZATIONS)

(DEFUN MA-OPTIMIZE NIL
  (PROG (*MA-OPT-FLAG*)
	(IF *MA-OPT-PRINT-INPUT*
	    (MA-PRINT-CODE))
	(DOLIST (SEQ *MA-SEQUENCES*)
	  (MA-OPT-SEQUENCE SEQ))
	(RETURN *MA-OPT-FLAG*)))

(DEFUN MA-OPT-SEQUENCE (*SEQ*)
 (*CATCH 'OPT-SEQUENCE 
	(PROG (INSTS MISC-ENTRIES CODE)
	      (SETQ INSTS (MA-ELEM-MEMBERS *SEQ*))
	      (DOLIST (I INSTS)		;misc-entries gets all such that appear in seq.
		(COND ((NOT (SYMBOLP I))
		       (SETQ CODE (MA-INST-CODE I))
		       (COND ((AND (LISTP CODE)
				   (EQ (CAR CODE) 'CALL)
				   (LISTP (CAR (LAST CODE)))
				   (EQ (CAAR (LAST CODE)) 'MISC-ENTRY))
			      (SETQ MISC-ENTRIES (CONS (CADAR (LAST CODE)) MISC-ENTRIES)))))))
	   L	(COND ((NULL INSTS) (RETURN NIL)))
	      (MA-OPT-MATCH-LIST INSTS *MA-OPTIMIZATIONS*)
	      (DOLIST (ME MISC-ENTRIES)
		(MA-OPT-MATCH-LIST INSTS (GET ME 'MA-OPTIMIZATIONS)))
	      (SETQ INSTS (CDR INSTS))
	      (GO L))))

(DEFUN MA-OPT-MATCH-LIST (INSTS OPT-LIST)
  (DOLIST (OPT OPT-LIST)
    (LET ((PATTERN (CAR OPT))
	  (ACTIONS (CADR OPT)))
      (IF (>= (LENGTH INSTS) (LENGTH PATTERN))
	  (MULTIPLE-VALUE-BIND (MATCH-P ALIST)
	      (MA-OPT-MATCH INSTS PATTERN NIL)
	    (IF MATCH-P
		(MA-OPT-ACT INSTS ACTIONS ALIST)))))))

(DEFUN MA-OPT-MATCH (INSTS PATTERN ALIST)
 (PROG (MATCH-P P I)
    L  (COND ((NULL PATTERN)
	      (RETURN T ALIST))
	     ((NULL INSTS)	;should not get here unless enuf insts to maybe win
	      (FERROR NIL "insts too short")))	
       (SETQ P (CAR PATTERN)
	     I (CAR INSTS))
       (MULTIPLE-VALUE (MATCH-P ALIST)
	 (MA-OPT-MATCH-INST I P ALIST NIL))
       (COND ((NULL MATCH-P) (RETURN NIL ALIST)))
       (SETQ INSTS (CDR INSTS)
	     PATTERN (CDR PATTERN))
       (GO L)))

(DEFUN MA-OPT-MATCH-INST (*INST* *PATTERN* *ALIST* *NAMECHAIN*)
 (LET ((*MA-OPT-MATCH-TRACE* *MA-OPT-MATCH-TRACE*)) 
  (PROG (MATCH-P P-ELEM FCTN)
	(COND ((NULL *INST*) (RETURN NIL *ALIST*)))
	(COND (*MA-OPT-MATCH-TRACE*
	       (FORMAT T "~% begin match ~S, pat ~S, alist ~s" *INST* *PATTERN* *ALIST*)))
     L  (COND ((NULL *PATTERN*)
	       (COND (*MA-OPT-MATCH-TRACE*
		      (FORMAT T "~% match on ~s succeeded" *INST*)))
	       (RETURN T *ALIST*)))
	(SETQ P-ELEM (CAR *PATTERN*))
	(IF (NULL (SETQ FCTN (GET (CAR P-ELEM) 'MA-OPT-MATCHER)))
	    (FERROR NIL "unknown type match")
	    (SETQ MATCH-P (FUNCALL FCTN (CDR P-ELEM))))
	(COND ((NULL MATCH-P)
	       (COND (*MA-OPT-MATCH-TRACE*
		      (FORMAT T "~% match on ~S failed, P-ELEM ~S" *INST* P-ELEM)))
	       (RETURN NIL *ALIST*)))
	(SETQ *PATTERN* (CDR *PATTERN*))
	(GO L))))

(DEFUN (CODE MA-OPT-MATCHER) (PAT)
   (MA-OPT-MATCH-WD (MA-INST-CODE *INST*)
			 (CAR PAT)))

(DEFUN (OP1 MA-OPT-MATCHER) (PAT) 
       (AND (NULL (MA-INST-CHANGED *INST*))
	    (NULL (DOLIST (OP (CDR (MA-INST-OP1 *INST*)))  ;pred must be true of
		    (COND ((NULL (MA-OPT-APPLY OP (CAR PAT))) 
			   (RETURN T)))))))	;all possibilities

(DEFUN (OP2 MA-OPT-MATCHER) (PAT)
  (AND (NULL (MA-INST-CHANGED *INST*))
       (NULL (DOLIST (OP (CDR (MA-INST-OP2 *INST*)))
	       (COND ((NULL (MA-OPT-APPLY OP (CAR PAT)))
		      (RETURN T)))))))

(DEFUN (RESULT-OP MA-OPT-MATCHER) (PAT)
  (MA-OPT-APPLY-INST (MA-INST-RESULT-OPERAND *INST*)
			  (CAR PAT)))

(DEFUN (OP1-LIST MA-OPT-MATCHER) (PAT)
  (MA-OPT-APPLY-INST (CDR (MA-INST-OP1 *INST*)) (CAR PAT)))

(DEFUN (OP2-LIST MA-OPT-MATCHER) (PAT)
  (MA-OPT-APPLY-INST (CDR (MA-INST-OP2 *INST*)) (CAR PAT)))

(DEFUN (STACK-OPERANDS-MAP MA-OPT-MATCHER) (PAT)
  (AND (NULL (MA-INST-CHANGED *INST*))
       (NULL (*CATCH 'VAL
		     (DOLIST (OPL (MA-INST-OP1 *INST*))
		       (DOLIST (OP (CDR OPL))
			 (COND ((NULL (MA-OPT-APPLY
					OP
					(CAR PAT)))
				(*THROW 'VAL T)))))))))
(DEFUN (LET MA-OPT-MATCHER) (PAT)
  (MA-OPT-LET (CAR PAT) (CADR PAT))
  T)

(DEFUN MA-OPT-LET (SYM VAL)
  (LET ((ISYM (MA-OPT-EXPAND-CHAIN-VAR SYM)))
    (SETQ *ALIST* (CONS (CONS ISYM
			      (MA-OPT-SUBS VAL))
		      *ALIST*))))

(DEFUN (LET-APPLY-TEST MA-OPT-MATCHER) (PAT)
  (LET ((SYM (MA-OPT-EXPAND-CHAIN-VAR (CAR PAT))))
    (LET* ((EXP (MA-OPT-SUBS (CADR PAT)))
	   (VAL (APPLY (CAR EXP) (CDR EXP))))
      (IF (NULL VAL)
	  NIL
	  (SETQ *ALIST* (CONS (CONS SYM VAL) *ALIST*))
	  T))))

(DEFUN (MATCH MA-OPT-MATCHER) (PAT)
  (MULTIPLE-VALUE (NIL *ALIST*)
    (MA-OPT-MATCH-INST (MA-OPT-SUBS (CAR PAT))
			    (CADR PAT)
			    *ALIST*
			    *NAMECHAIN*)))


(DEFUN (MAP-MATCH MA-OPT-MATCHER) (PAT)
  (PROG (CONTROL-VAR CONTROL-VAR-PNTR LIST-TO-MAP PATTERN)
	(SETQ CONTROL-VAR (CAR PAT)
	      LIST-TO-MAP (MA-OPT-APPLY *INST* (CADR PAT))
	      PATTERN (CADDR PAT))
	(SETQ CONTROL-VAR (MA-OPT-EXPAND-CHAIN-VAR CONTROL-VAR))
	(SETQ *ALIST* (CONS (SETQ CONTROL-VAR-PNTR (CONS CONTROL-VAR 0)) *ALIST*))
	(SETQ *NAMECHAIN* (CONS CONTROL-VAR *NAMECHAIN*))
     L  (COND ((NULL LIST-TO-MAP)
	       (RETURN T)))
	(COND ((NULL (MULTIPLE-VALUE (NIL *ALIST*)
		       (MA-OPT-MATCH-INST (CAR LIST-TO-MAP)
					       PATTERN
					       *ALIST*
					       *NAMECHAIN*)))
	       (RETURN NIL)))
	(SETQ LIST-TO-MAP (CDR LIST-TO-MAP))
	(RPLACD CONTROL-VAR-PNTR (1+ (CDR CONTROL-VAR-PNTR)))
	(GO L)))

(DEFUN (MAP-PUT MA-OPT-ACT) (ACT)
  (PROG (CONTROL-VAR CONTROL-VAR-PNTR LIST-TO-MAP ACT-LIST)
	(SETQ CONTROL-VAR (CAR ACT)
	      LIST-TO-MAP (MA-OPT-APPLY *INST* (CADR ACT))
	      ACT-LIST (CADDR ACT))
	(SETQ CONTROL-VAR (MA-OPT-EXPAND-CHAIN-VAR CONTROL-VAR))
	(SETQ *ALIST* (CONS (SETQ CONTROL-VAR-PNTR (CONS CONTROL-VAR 0)) *ALIST*))
	(SETQ *NAMECHAIN* (CONS CONTROL-VAR *NAMECHAIN*))
     L  (COND ((NULL LIST-TO-MAP)
	       (RETURN T)))
	(MA-OPT-ACT-ON (CAR LIST-TO-MAP)
			    ACT-LIST
			    *NAMECHAIN*)
	(SETQ LIST-TO-MAP (CDR LIST-TO-MAP))
	(RPLACD CONTROL-VAR-PNTR (1+ (CDR CONTROL-VAR-PNTR)))
	(GO L)))

(DEFUN (MATCH-CASE MA-OPT-MATCHER) (PAT)
  (PROG (CONTROL-VAR CONTROL-VAR-PNTR LIST-OF-CASES TEM-ALIST)
	(SETQ CONTROL-VAR (CAR PAT)
	      LIST-OF-CASES (CADR PAT))
	(SETQ CONTROL-VAR (MA-OPT-EXPAND-CHAIN-VAR CONTROL-VAR))
	(COND ((ASSOC CONTROL-VAR *ALIST*)
	       (FERROR NIL "~% control variable has reused name ~S" CONTROL-VAR)))
	(SETQ *ALIST* (CONS (SETQ CONTROL-VAR-PNTR (CONS CONTROL-VAR 0)) *ALIST*))
    L	(COND ((NULL LIST-OF-CASES) (RETURN NIL))
	      ((MULTIPLE-VALUE (NIL TEM-ALIST)
		 (MA-OPT-MATCH-INST *INST* (CAR LIST-OF-CASES) *ALIST* *NAMECHAIN*))
	       (SETQ *ALIST* TEM-ALIST)	;this one won, gobble
	       (RETURN T)))
	(SETQ LIST-OF-CASES (CDR LIST-OF-CASES))
	(RPLACD CONTROL-VAR-PNTR (1+ (CDR CONTROL-VAR-PNTR)))
	(GO L)
))

(DEFUN (PUT-CASE MA-OPT-ACT) (ACT)
  (PROG (CONTROL-VAR CONTROL-VAR-VAL LIST-OF-CASES)
	(SETQ CONTROL-VAR (CAR ACT)
	      LIST-OF-CASES (CADR ACT))
	(SETQ CONTROL-VAR (MA-OPT-EXPAND-CHAIN-VAR CONTROL-VAR))
	(IF (NOT (NUMBERP (SETQ CONTROL-VAR-VAL (MA-OPT-SYMEVAL CONTROL-VAR))))
	    (FERROR NIL "control var not number"))
	(IF *MA-OPT-TRACE-ACTS*
	    (FORMAT T "~%Taking case ~s, control var ~s, namechain ~s"
		    CONTROL-VAR-VAL CONTROL-VAR *NAMECHAIN*))
	(RETURN (MA-OPT-ACT-ON *INST* (NTH CONTROL-VAR-VAL LIST-OF-CASES) *NAMECHAIN*))
))

(DEFUN (MATCH-FUNCTION MA-OPT-MATCHER) (PAT)
  (LET* ((FCTN (CAR PAT))
	 (EXP (GET FCTN 'MA-OPT-FUNCTION)))
    (MA-OPT-LET '*FUNCTION-ARG* (CADR PAT))
    (MULTIPLE-VALUE (NIL *ALIST*)
      (MA-OPT-MATCH-INST *INST* (CAR EXP) *ALIST* *NAMECHAIN*))))

(DEFUN (MATCH-FUNCTION MA-OPT-ACT) (ACT)
  (LET* ((FCTN (CAR ACT))
	 (EXP (GET FCTN 'MA-OPT-FUNCTION)))
    (MA-OPT-ACT-ON *INST* (CADR EXP) *NAMECHAIN*)))

(DEFUN (PRED MA-OPT-MATCHER) (PAT)
  (LET ((F (CAAR PAT))
	(ARGS (MA-OPT-SUBS (CDAR PAT))))
    (IF *MA-OPT-MATCH-TRACE* (FORMAT T "~%pred: ~s args ~s" F ARGS))
    (APPLY F ARGS)))

(DEFUN MA-MATCH-EQUAL (A B)
  (OR (EQ A '?)
      (EQ B '?)
      (EQUAL A B)))

(DEFUN (MAP-RANGE MA-OPT-MATCHER) (PAT)
  (LEXPR-FUNCALL
    'MA-SEQ-MAP-RANGE 
    *SEQ*	    ;args: from-inst to-inst slot pred
    (MA-OPT-SUBS PAT)))

(DEFUN (TRACE MA-OPT-MATCHER) (PAT) PAT
  (FORMAT T "~% begin trace ~S, pat ~S, alist ~S" *INST* *PATTERN* *ALIST*)
  (SETQ *MA-OPT-MATCH-TRACE* T))	;returns T

(DEFUN (BREAK MA-OPT-MATCHER) (PAT) PAT
  (CERROR T NIL NIL "break")
  T)

;test instructions after FROM and before TO
(DEFUN MA-SEQ-MAP-RANGE (SEQ FROM TO SLOT OPER)
  (PROG (INSTS)
	(SETQ INSTS (MA-ELEM-MEMBERS SEQ))
    L   (COND ((NULL INSTS) (FERROR NIL "p1"))
	      ((EQ (CAR INSTS) FROM)
	       (GO E1)))
	(SETQ INSTS (CDR INSTS))
	(GO L)
    E1  (SETQ INSTS (CDR INSTS))	;flush from INST.
    L1  (COND ((NULL INSTS) (FERROR NIL "p2"))
	      ((EQ (CAR INSTS) TO)
	       (RETURN T))		;doesnt clobber
	      ((MA-INST-MAP-OPER-P (CAR INSTS) SLOT OPER)
	       (COND (*MA-OPT-MATCH-TRACE*
		      (FORMAT T "~%    ~s violates ~s ~s" (CAR INSTS) OPER SLOT)))
	       (RETURN NIL)))		;does clobber
	(SETQ INSTS (CDR INSTS))
	(GO L1)
  ))

(DEFUN MA-INST-MAP-OPER-P (INST SLOT OPER)
  (SELECTQ OPER
    (NO-CLOBBER
     (COND ((MA-INST-CLOBBERS-CONTEXT-P INST))
	   ((EQUAL SLOT (MA-DEST-CODE (MA-INST-CODE INST))))))
    (NO-REFERENCE
     (OR (MA-OPERAND-REFS-SLOT SLOT (MA-INST-OP1 INST))
	 (MA-OPERAND-REFS-SLOT SLOT (MA-INST-OP2 INST))))))

(DEFUN MA-INST-CLOBBERS-CONTEXT-P (INST)
  (MA-CONTEXT-CLOBBERAGE (MA-INST-CODE INST))	;this may eventually be too conservative.
)

(DEFUN MA-OPERAND-REFS-SLOT (SLOT OP)
  (COND ((NULL OP) NIL)
	((EQUAL SLOT (CAR OP)))))

(DEFUN MA-SAME-SEQUENCE (I1 I2)
  (EQ (MA-INST-SEQUENCE I1) (MA-INST-SEQUENCE I2)))

(DEFUN MA-OP1-SOURCE-INST (INST)
  (LET ((OP1 (MA-INST-OP1 INST)))
    (COND ((OR (NLISTP OP1)
	       (NOT (= (LENGTH (CDR OP1)) 1)))	  ;flush register spec
	   (FERROR NIL ""))
	  (T (MA-OPERAND-SOURCE (CADR OP1))))))

(DEFUN MA-OP1-SOURCE-INST-LIST (INST)
  (LET ((OP1 (MA-INST-OP1 INST))
	ANS)
    (COND ((NLISTP OP1)
	   (FERROR NIL ""))
	  (T (DOLIST (OP (CDR OP1))	;flush register spec.
	       (SETQ ANS (CONS (MA-OPERAND-SOURCE OP) ANS)))
	     (NREVERSE ANS)))))

(DEFUN MA-PRECEEDING-INSTS-LIST (INST)
  (LET* ((BS (MA-INST-BEFORE-STATE INST))
	 (PS (MA-STATE-PRECEEDING-STATES BS)))
    (AND (NULL (MA-INST-CHANGED INST))
	 (MAPCAR (FUNCTION (LAMBDA (X) (MA-STATE-INST X)))
		 PS))))

(DEFUN MA-FOLLOWING-INSTS-LIST (INST)
  (LET* ((AS (MA-INST-AFTER-STATE INST))
	 (FS (MA-STATE-FOLLOWING-STATES AS)))
    (AND (NULL (MA-INST-CHANGED INST))
	 (MAPCAR (FUNCTION (LAMBDA (X) (MA-STATE-INST X)))
		 FS))))

(DEFUN MA-OPERAND-ONE-USE-P (OP)
  (= (LENGTH (MA-OPERAND-USES OP)) 1))

(DEFUN MA-ONE-AND-ONLY-USE-P (OL)
  (AND OL (NULL (CDR OL)) (MA-OPERAND-ONE-USE-P (CAR OL))))

(DEFUN MA-OPERAND-FIX-P (OP PATH &AUX SOURCE-INST SOURCE-INST-CODE SOURCE-INST-OP1)
  (OR (EQ (MA-OPERAND-TYPE OP) 'DTP-FIX)
      (AND (SETQ SOURCE-INST (MA-OPERAND-SOURCE OP))
	   (NULL (MA-INST-CHANGED SOURCE-INST))
	   (LISTP (SETQ SOURCE-INST-CODE (MA-INST-CODE SOURCE-INST)))
	   (EQ (CAR SOURCE-INST-CODE) 'MOVE)
	   (NULL (CDDDR SOURCE-INST-CODE))	;no byte specifier
	   (SETQ SOURCE-INST-OP1 (CDR (MA-INST-OP1 SOURCE-INST)))
	   (NOT (MEMQ SOURCE-INST-OP1 PATH))	;avoid infinite loop in degenerate case
	   (NULL (DOLIST (OP SOURCE-INST-OP1)
		   (COND ((NULL (MA-OPERAND-FIX-P OP (CONS OP PATH)))
			  (RETURN T))))))))     ;if any one not a fix, it isnt

;return nil if more than one.
(DEFUN MA-OPT-PRECEEDING-INST (INST)
  (LET* ((BS (MA-INST-BEFORE-STATE INST))
	 (PS (MA-STATE-PRECEEDING-STATES BS)))
    (AND (NULL (MA-INST-CHANGED INST))
	 (NULL (CDR PS))		;just one preceeding state
	 (LET* ((PRECEEDING-STATE (CAR PS))
		(PRECEEDING-INST (MA-STATE-INST PRECEEDING-STATE)))
	   (AND (NULL (MA-INST-CHANGED PRECEEDING-INST))
		PRECEEDING-INST)))))

(DEFUN MA-OPT-FOLLOWING-INST (INST)
  (LET* ((AS (MA-INST-AFTER-STATE INST))
	 (FS (MA-STATE-FOLLOWING-STATES AS)))
    (AND (NULL (MA-INST-CHANGED INST))
	 (NULL (CDR FS))
	 (LET* ((FOLLOWING-STATE (CAR FS))
		(FOLLOWING-INST (MA-STATE-INST FOLLOWING-STATE)))
	   (AND (NULL (MA-INST-CHANGED FOLLOWING-INST))
		FOLLOWING-INST)))))

(DEFUN MA-OPT-IS-A-FOLLOWER (INST BEFORE-INST)
  (LET* ((AS (MA-INST-AFTER-STATE BEFORE-INST))
	 (FS (MA-STATE-FOLLOWING-STATES AS)))
    (AND (NULL (MA-INST-CHANGED INST))
	 (DOLIST (F FS)
	   (COND ((EQ INST (MA-STATE-INST F))
		  (RETURN T)))))))

(DEFUN MA-OPT-ONLY-PRECEEDER (BEFORE-INST INST)
  (LET* ((BS (MA-INST-BEFORE-STATE INST))
	 (PS (MA-STATE-PRECEEDING-STATES BS)))
    (AND (NULL (MA-INST-CHANGED INST))
	 (NULL (CDR PS))
	 (EQ BEFORE-INST (MA-STATE-INST (CAR PS))))))

;return nil if not defined.
(DEFUN MA-OPT-OTHER-FOLLOWING-INST (INST NOT-INST)
  (LET* ((AS (MA-INST-AFTER-STATE INST))
	 (FS (MA-STATE-FOLLOWING-STATES AS)))
    (AND (NULL (MA-INST-CHANGED INST))
	 (NULL (MA-INST-CHANGED NOT-INST))
	 (NULL (CDDR FS))			;two following states
	 (LET* ((FS1 (CAR FS))
		(FI1 (MA-STATE-INST FS1))
		(FS2 (CADR FS))
		(FI2 (MA-STATE-INST FS2)))
	   (AND (NULL (MA-INST-CHANGED FI1))
		(NULL (MA-INST-CHANGED FI2))
		(PROG2 (IF (NOT (OR (EQ FI1 NOT-INST) (EQ FI2 NOT-INST)))
			   (FERROR NIL "not inst not one of choices"))
		       (IF (EQ FI1 NOT-INST) FI2 FI1)))))))

(DEFUN MA-OPT-APPLY-INST (ARG PC)
  (AND (NULL (MA-INST-CHANGED *INST*))
       (MA-OPT-APPLY ARG PC)))

(DEFUN MA-OPT-APPLY (ARG PC)
  (LEXPR-FUNCALL (CAR PC) ARG (CDR PC)))

(DEFUN MA-OPT-SYMEVAL (SYM)
  (COND ((EQ SYM '*INST*) *INST*)		;special frob
	(T (CDR (ASSOC SYM *ALIST*)))))


(DEFUN MA-OPT-MATCH-WD (CODE P)
  (PROG ()
	(COND ((ATOM P) (RETURN (EQ P CODE))))
    L   (COND ((NULL P)
	       (RETURN (NULL CODE)))
	      ((ATOM P) 	;dotted rest variable
	       (RETURN (MATCH-OPT-MATCH-VAR CODE P NIL)))
	      ((AND (LISTP P)
		    (EQ (CAR P) '==))
	       (RETURN (MATCH-OPT-MATCH-VAR CODE (CADR P) (CDDR P))))
	      ((NLISTP CODE)
	       (RETURN NIL)))
	(COND ((NULL (MA-OPT-MATCH-WD (CAR CODE) (CAR P))) (RETURN NIL)))
	(SETQ CODE (CDR CODE) P (CDR P))
	(GO L))
)

(DEFUN MATCH-OPT-MATCH-VAR (CODE VAR RESTRICTIONS)
  (LET ((SYM (MA-OPT-EXPAND-CHAIN-VAR VAR))
	TEM)
    (COND ((EQ SYM '?)
	   T)
	  ((SETQ TEM (ASSOC SYM *ALIST*))
	   (EQUAL (CDR TEM) CODE))
	  ((AND RESTRICTIONS
		(DOLIST (E RESTRICTIONS)
		  (COND ((NULL (APPLY (CAR E)
				      (CONS CODE (CDR E))))
			 (RETURN T)))))
	   NIL)			;predicate not true.
	  (T (COND ((MEMBER SYM *MA-OPT-TRACE-VARS*)
		    (FORMAT T "~%Assigning ~s: ~s" SYM CODE)))
	     (SETQ *ALIST* (CONS (CONS SYM CODE) *ALIST*))
	     T))))

(DEFUN MA-OPT-EXPAND-CHAIN-VAR (VAR)
  (PROG (NC)
	(COND ((MEMQ VAR '(? *INST*))
	       (RETURN VAR)))
	(SETQ NC *NAMECHAIN*)
    L   (COND ((SYMBOLP VAR))
	      ((EQ (CAR VAR) '=)
	       (SETQ NC NIL VAR (CADR VAR))
	       (GO L))
	      ((EQ (CAR VAR) '*)
	       (SETQ NC (CDR NC) VAR (CADR VAR))
	       (GO L)))
	(COND ((AND (SYMBOLP VAR)
		    (NULL NC))
	       (RETURN VAR))
	      (T (RETURN (CONS VAR (MAPCAR (FUNCTION MA-OPT-SYMEVAL) NC)))))))

(DEFUN MA-OPT-ACT (INSTS ACTIONS *ALIST*)
  (DO ((I INSTS (CDR I))
       (A ACTIONS (CDR A)))
      ((NULL A)
       (COND ((MA-SEQ-CHANGED *SEQ*)
	      (*THROW 'OPT-SEQUENCE T))))   ;dont hack this sequence since its changed
    (MA-OPT-ACT-ON (CAR I) (CAR A) NIL)))

(DEFUN MA-OPT-ACT-ON (*INST* ACT-LIST *NAMECHAIN*)
 (PROG (ACT FCTN ICODE)
      (COND (*MA-OPT-TRACE-ACTS*
	     (FORMAT T "~%Enter ACT ~S, namechain ~S, ==>" *INST* *NAMECHAIN*)))
      (SETQ ICODE (MA-INST-CODE *INST*))
   L  (COND ((NULL ACT-LIST)
	     (COND (*MA-OPT-TRACE-ACTS* (FORMAT T "~% ~S ==> ~S, namechain ~S"
						ICODE (MA-INST-CODE *INST*) *NAMECHAIN*)))
	     (RETURN T)))
      (SETQ ACT (CAR ACT-LIST))
      (IF (NULL (SETQ FCTN (GET (CAR ACT) 'MA-OPT-ACT)))
	  (FERROR NIL "~%unknown action")
	  (FUNCALL FCTN (CDR ACT)))
      (SETQ ACT-LIST (CDR ACT-LIST))
      (GO L)))

(DEFUN (FLUSH MA-OPT-ACT) (ACT) ACT
  (MA-OPT-FLUSH))

(DEFUN MA-OPT-FLUSH NIL
  (SETF (MA-INST-OP1 *INST*) (MA-INST-CODE *INST*))  ;save for debugging
  (SETF (MA-INST-CODE *INST*) NIL)
  (SETF (MA-INST-CHANGED *INST*) T)
  (MA-FLUSH-INST *INST*)	  ;flush it.
  (SETQ *MA-OPT-FLAG* T)
  (SETF (MA-SEQ-CHANGED (MA-INST-SEQUENCE *INST*)) T))

(DEFUN (PUT MA-OPT-ACT) (ACT)
  (SETF (MA-INST-CODE *INST*) (MA-OPT-SUBS (CAR ACT)))
  (SETF (MA-INST-CHANGED *INST*) T)
  (SETQ *MA-OPT-FLAG* T)
  (SETF (MA-SEQ-CHANGED (MA-INST-SEQUENCE *INST*)) T))

(DEFUN (PUT-INST MA-OPT-ACT) (ACT)
  (MA-OPT-ACT-ON (MA-OPT-SUBS (CAR ACT))
		 (CADR ACT)
		 *NAMECHAIN*))

(DEFUN MA-OPT-SUBS (PAT)
  (PROG (V P)
	(COND ((NULL PAT) (RETURN NIL))
	      ((NLISTP PAT) (RETURN PAT))
	      ((EQ (CAR PAT) '==)
	       (RETURN (MA-OPT-SUBS-VAR (CADR PAT) (CDDR PAT))))
	      ((EQ (CAR PAT) 'QUOTE)
	       (RETURN PAT)))
	(SETQ P (VALUE-CELL-LOCATION 'V))
   L	(COND ((NULL PAT) (RETURN V))
	      ((SYMBOLP PAT)		;dotted rest var.
	       (RPLACD P (MA-OPT-SUBS-VAR PAT NIL))
	       (RETURN V)))
	(RPLACD P (SETQ P (LIST (MA-OPT-SUBS (CAR PAT)))))
	(SETQ PAT (CDR PAT))
	(GO L)))

(DEFUN MA-OPT-SUBS-VAR (VAR ALTERATION)
  (LET* ((SYM (MA-OPT-EXPAND-CHAIN-VAR VAR))
	 (VAL (MA-OPT-SYMEVAL SYM)))
    (IF ALTERATION
	(SETQ VAL (APPLY (CAAR ALTERATION)	;call function to alter value
			 (CONS VAL (CDAR ALTERATION)))))
    VAL))

(DEFUN MA-INVERT (V 2-LIST)
  (COND ((EQ V (CAR 2-LIST)) (CADR 2-LIST))
	((EQ V (CADR 2-LIST)) (CAR 2-LIST))
	(T (FERROR NIL ""))))
