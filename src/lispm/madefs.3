;-*- MODE: LISP; PACKAGE: COMPILER; BASE: 8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **



(DEFVAR *UCADR-STATE-LIST* NIL)	 ;assembler state list for current ucode assembly
				 ;The idea is this eventually will have all necessary
				 ; linkage info
(DEFVAR *MC-LINKAGE-ALIST* NIL)
(DEFVAR *MA-PARAM-LIST*)	 ;alist of parameter assignments

; cubbyholes
;  a cubbyhole is the basic home of a variable, although due to optimizations
;it does not necessarily always "contain" the variable.  Cubbyholes are identified
;by specifiers carried thru from the macrocode, ie (ARG n) or (LOCBLOCK n).
;(Note, however, that (LOCBLOCK n) just serves as a name and does not imply
;anything about actual position on the stack).
;(SPECIAL FOO) is also possible.  However, this is only of significance until a
;DO-SPECBIND is seen, since after that value is in the special cell.
;Cubbyholes for the required args are created at MA-MAKE-INITIAL-STATE.
;The micro-compiler "signals" creation of other cubbyholes by inserting a START-CUBBYHOLE,
;followed by code, then by (CREATE-CUBBYHOLE <name>) which says the thing on top of
;the stack is to be considereda cubbyhole and gives the specifier.  This delimits
;the code to be flushed in case it is decided to colapse the cubbyhole.

(DEFVAR *MA-CUBBYHOLES*)
(DEFVAR *MA-CUBBYHOLE-ALIST*)	;alist <name> . <cubbyhole-structure>
(DEFVAR *MA-FUNCTION-EXITS*)	;all insts which can leave function.

(DEFSTRUCT (MA-CUBBYHOLE :NAMED :ARRAY)
  MA-CUBBYHOLE-NAME
  MA-CUBBYHOLE-ALL-NAMES
  MA-CUBBYHOLE-REFS)	;a list (<FETCH or STORE>  <inst>)
  
;cubbyhole optimizations:
;  two stores with no intervening reference

;Colapsing cubbyholes:
; Sometimes two cubbyholes have non-overlapping lifetimes.  If so, the same storage
;slot can be used for both.  A particularily important case is where one is initialized
;from the other.  Then the two can be merged into one, also saving the initialization.
;This often happens as a result of the (lambda (n) (do ((n n (1-n)) ..) ..) ..) style.


; A ordinary sequence is an ordered list of instructions which necessarily follow each
;other logically.
;There can be no branches into the middle of a sequence, nor can an a fork in control
;structure occur in the middle of a sequence.  Thus, a merge point may only be the first
;instruction of a sequence, and a conditional branch the last.  Every instruction belongs
;to exactly one sequence.

; *MA-SEQUENCES* is created by a traversal of the code in a possible execution order.

(DEFVAR *MA-SEQUENCES*)
(DEFVAR *MA-FIRST-SEQUENCE*)

(DEFSTRUCT (MA-ELEM :NAMED)	;never instantiated, just included below.
  MA-ELEM-MEMBERS	;must be INSTs for sequences, can be bubbles or loops for
			; bubbles or loops.
  MA-ELEM-BUBBLES	;BUBBLES "directly" a member of.
  MA-ELEM-LOOPS)	;LOOPS "directly" a member of.

(DEFSTRUCT (MA-SEQUENCE :NAMED :ARRAY (:INCLUDE MA-ELEM))
  MA-SEQ-NEXT-SEQUENCE	;seq this one "drops into"
  MA-SEQ-PRECEEDING-SEQUENCES	;sequences which execute immediately before this one.
  MA-SEQ-FOLLOWING-SEQUENCES	;sequences which execute immediately after this one.
  MA-SEQ-ALL-LOOPS	;all loops a member of, including those of my bubbles and loops, etc
  MA-SEQ-CHANGED	;instruction altered during optimization
 ;following used by CHART-TOPOLOGY
  MA-SEQ-PENDING-FS
  MA-SEQ-APATHS		;Access paths by which we get here.
  MA-SEQ-LOOP-PATHS
  MA-SEQ-BUBBLE-PATHS
  MA-SEQ-LOOP-HEADS
  MA-SEQ-BUBBLE-HEADS 
)  

;Charting things out:
; A BUBBLE has a top, a bottom and two or more ways from getting from one to the other.
; A LOOP is any kind of a cyclic structure.  

;*** For a BUBBLE to be SIMPLE, each path must be "direct", ie cannot contain branches
;to unrelated stuff. A branch can have a fully contained loop or bubble, however.
; For a LOOP it must be a straighforward cycle of elements, "singularily connected".
;Ie, there must be a single entry point and a single exit point which connect to the
;rest of the world.  However, in addition to the main exit point,
;multiple "exit stubs" (paths which exit the entire function) are allowed.
; Simple BUBBLEs and LOOPs both look like simple sequences from above.

; Finding BUBBLES and LOOPS simplemindedly.  We start with initial sequence and walk
;the program recursively, keeping a list of the current path.  If a segment on this list
;is encountered, the walker has closed a loop.  The members of the loop, (ie the segment
;of the path list after the previous occurance), is added to the MA-SEQ-LOOP-HEADS
;list of the intersecting segment.
; Also, each time a sequence is encountered, the current path is added to the
;MA-SEQ-APATHS component of the sequence.  So if we encounter a sequence for the
;second time, with the current path and the previous path we can find the BUBBLE, if any,
;and add it to the  MA-SEQ-BUBBLE-PATHS component of the head sequence.
; One problem is avoiding pseudo-duplicate loops and/or bubbles when several loops
;and/or bubbles are nested.  For example, if there is a bubble inside a loop, we dont
;want to find two loops, one through each fork of the bubble.  To help deal with this,
;we first find PATH sequences.  When the recursive walk is about to ascend from a
;sequence, the MA-SEQ-LOOP-PATH and MA-SEQ-BUBBLE-PATH components are examined.
;At this time all "lower" nodes have been exhaustively examined, thus, the HEAD nodes
;of all lower loops and bubbles have been identified.  The members of the PATHS
;list are examined, and if any of them are head nodes of a loop or bubble, they are
;replaced with that loop or bubble structure, and all following nodes which are members
;of the same loop or bubble are deleted.  (Loops are considered more "outer" than bubbles).
;Finally, duplicates are deleted, and the node becomes the head node for any remaining
;loops and/or bubbles.

(DEFVAR *MA-BUBBLES*)

(DEFSTRUCT (MA-BUBBLE :NAMED :ARRAY (:INCLUDE MA-ELEM))
 MA-BUBBLE-TOP
 MA-BUBBLE-BOTTOM
 MA-BUBBLE-PATHS	  ;members of this list can be sequences, bubbles, or loops.
			  ;each path includes the head-sequence.
)

(DEFVAR *MA-LOOPS*)

;A loop is a cyclic structure.  It may or may not partially coincide with other
;loops.
(DEFSTRUCT (MA-LOOP :NAMED :ARRAY (:INCLUDE MA-ELEM))
  MA-LOOP-ENTRIES   ;A list, (loop-seq . non-loop seq)
  MA-LOOP-EXITS     ; likewise.
)

;A simple-loop is a straighforward cycle of elements.  It must
;  be "singularily connected", ie, there must be a single entry
;  point and at most a single exit point which connect to the
; rest of the world.  However, in addition to the main exit point,
; multiple "exit stubs" (paths which exit the entire function) 
; are allowed.	       

;Ordering relations between states
; Given two states and a context, exactly one of the following relations holds: 
;BEFORE AFTER INDETERMINANT EXCLUSIVE.  The context is usually the whole function or a 
;loop.  The relation is relative to a particular execution
;of context.  Thus if the context is the whole function, all instructions
;within a loop are INDETERMINANT relative to each other.

;a STATE is a specification of what logical register slots are in existance,
; and the current contents of each.  The current contents of a register are
; described by a OPERAND structure.  

;A program is a sequence of instructions.  Each instruction may take 1 or 2 operands
; and may produce a result.  If produced, the result is described by an OPERAND structure.
;Before each instruction the machine is in some state (BEFORE-STATE) and following
;the instruction it is in some other state (AFTER-STATE).  The AFTER-STATE
;is produced by copying the BEFORE-STATE, the meta-simulating the operation
;of the instruction.  The copying is only to one level, however, ie the AFTER-STATE
;points to EQ quantity structures as the BEFORE state, except as modified by
;the meta-simulation.

;An OPERAND structure consists mainly of a list of QUANTITY descriptions.
;Usually, this list is only a single element long, but it may be longer if
;there have been merges in the flow of control.  It that case, each element
;represents an alternative possibility for the OPERAND.

;A QUANTITY structure contains the state serial number the quantity was generated at,
;  a pointer to the generating instruction, the data type of the quantity,
;  and a property list where notes can be made about the quantity.
;  Each QUANTITY comes to have a list of all the instructions that may USE it.

;All methods of getting to a particular instruction had better lead to the same
; form of STATE, or you are clearly losing.

;Getting everything all hooked up..
;  Until the hook-up pass over the program is completed, all OPERANDs may be
;incomplete, ie, we may discover branches which lead to additional possible
;quantities.  

;  First generate an output state for every instruction that needs one.

;  After an unconditional branch, the state can be "discontinuous" between
;contiguous instructions.

;The pdl buffer index
; This hardware register addresses the PDL-BUFFER.  It is not saved across
;subroutine calls.  (Even CAR might get an error or send a message and clobber
;it).

;The pdl buffer pass around path.
; Unfortunately, the CADR processor doesnt have one.  The result is that new data
;written is not "visible" on the cycle immediately following the writing cycle.
;This rarely matters if no optimizations are being attempted since it normally
;takes a cycle to set up the PDL-BUFFER-INDEX to the desired variable, during which
;a previous write can "happen".  However, if one tries to be clever, one can lose.
;Also a C-PDL-BUFFER-POINTER-PUSH destination followed by C-PDL-BUFFER-POINTER or 
;C-PDL-BUFFER-POINTER-POP source will lose.  For now, this problem arises in inline
;code only; any jump takes a cycle which lets the writes really happen.  However, if
;one were to get fancy with -XCT-NEXT frobs, one could lose.

;coding conventions:
; The TYPED-POINTER part of the word (low 29. bits) is of interest.  Unfortunately 
;the rest is there.  The general assumption is that quantities in the A and M registers
;are clean, while those in functional sources (notably MD) and PDL-BUFFER are dirty.
;MOVE automatically masks when comming from dirty sources.

;optimization switches.  Hopefully it helps to isolate bugs to be able to turn various
; cleverness off.
(DEFCONST *ma-optimize-pdl-index* t)
(DEFCONST *ma-colapse-cubbyholes* t)
(DEFCONST *ma-chart-topology* t)  ;do hairy topology analysis
(DEFCONST *ma-optimize* t)
(DEFCONST *ma-make-mclap-sequence-wise* t)  ;otherwise do it INST wise
(DEFCONST *ma-hack-xct-next* t)

(defvar *ma-first-inst*)	;Main input. List of MA-INST defstructs.
(defvar *ma-inst-tail*)		;current or last MA-INST.
(defvar *ma-initial-state*)	;state prior to first inst.

(defvar *ma-opt-flag*)	;set if optimizer manages to do something.

;for expansion phase
(DEFVAR *SEQ*)			;current sequence (optimizer phase)
(DEFVAR *INST*)			;current instruction
(DEFVAR *ALIST*)		;current alist (optimizer phase)
(DEFVAR *PATTERN*)		;current pattern (optimizer phase)
(DEFVAR *EMIT-LIST*)		;list of instructions emitted so far
(DEFVAR *PDL-BUFFER-INDEX*)	;absolute slot number pdl buffer is set to or NIL.
				; (ie arg 1 is absolute slot 0, etc)
(DEFVAR *PDL-BUFFER-WRITE-HAPPENING*) ;absolute slot number stored in on last cycle
				;either via pdl-buffer-index, c-pdl-buffer-pointer,
				;or c-pdl-buffer-pointer-push.  This is cleared by
				;MA-EMIT-EXECUTE, so should be set up after MA-EMIT ing
				;the inst that does the write.
(DEFVAR *MA-SPECBIND-DONE*)     ;Until this, ref special vars that have cubbyholes
				; on stack instead of in special cells.

(DEFCONST *M-REGISTERS* '(T B R C TEM))    ;M-registers.

(DEFMACRO DOINSTS ((VAR FORM) . BODY)
  `(DO ((,VAR ,FORM (MA-INST-NEXT-INST ,VAR)))
	 ((NULL ,VAR))
     . ,BODY))

(DEFSTRUCT (MA-INST :NAMED :ARRAY)
  MA-INST-CODE			;instruction as produced by micro-compiler
  MA-INST-TAGS-BEFORE		;list of tags immediately before this inst.
  MA-INST-PREVIOUS-INST		;textually previous guy
  MA-INST-NEXT-INST		;textually next guy
  MA-INST-BEFORE-STATE
  MA-INST-AFTER-STATE
  MA-INST-OP1			;slot quantity (ie element of ma-state-register-alist or
				; ma-state-stack-alist). Thus CAR is NIL or the
				; cubbyhole specifier, CDR is a list of alternative OPERANDS.
				;may be a list of such if INST pops pdl.
  MA-INST-OP2			;likewise  operand 2
  MA-INST-RESULT-OPERAND	;result
  MA-INST-EXPANSION		;list of MCLAP words
  MA-INST-SEQUENCE		;sequence this belongs to
  MA-INST-CHANGED		;if T, CODE changed by  optimizer.  Operands may
				; not be right.
)  ;update ma-clear-code when adding....


(DEFSTRUCT (MA-STATE :NAMED :ARRAY)	;machine state
  MA-STATE-INST			;instruction this belongs to or BEGINNING-OF-FUNCTION
  MA-STATE-FILLED		;If NIL, none of the other field have been filled in yet.
  MA-STATE-PRECEEDING-STATES	;A list of the states which logically immediately
				; preceed this one. Normally the AFTER state of the
				; preceeding instruction plus any branches in.
  MA-STATE-FOLLOWING-STATES	;A list of states which logically immediately follow
				; this one.  The "drop thru" case, if any, is first on the
				; list.
  MA-STATE-REGISTER-ALIST	;Associates LAP ADDRESS with operand-list
				; Keeps track of state in M, A registers, etc
  ; These operand lists are normally one long, but can be longer as a result of 
  ;merges in the flow of control.The first element of the operand list may be INVALID
  ;which means the slot was completely unspecified by some state which was merged in.
  ;Note that since just the top level list is copied
  MA-STATE-STACK-ALIST		;List corresponds to stack slots.  Key is cubbyhole,
				;or NIL if temporary part of the stack.  Used to compute
				;stack indexing (particularily considering the fact that
				;stack slots may be "removed" to registers.)  Each entry is
				;a CONS, CDR of which is a QUANTITY list.
  MA-STATE-PDL-BUFFER-INDEX
  MA-STATE-PDL-BUFFER-WRITE-HAPPENING
)  ;update MA-CLEAR-STATE when adding these ...

;duplicate state so modifications wont affect original.
(DEFUN MA-COPY-STATE (FROM-STATE TO-STATE)
  (SETF (MA-STATE-REGISTER-ALIST TO-STATE)
	(APPEND (MA-STATE-REGISTER-ALIST FROM-STATE) NIL))
  (SETF (MA-STATE-STACK-ALIST TO-STATE)
	(APPEND (MA-STATE-STACK-ALIST FROM-STATE) NIL))
  (SETF (MA-STATE-FILLED TO-STATE) T)
  TO-STATE)

;Actually represents an operand-instance, ie, this is not modified once it is generated
; except to notate additional uses.
(defstruct (ma-operand :named :array)
 ma-operand-source	;inst that generated this, or nil if "magic" (ie initial)
 ma-operand-name	;identification for printing
 ma-operand-type	;NIL for normal, DTP-FIX, T-OR-NIL
 ma-operand-defects	;funny things about this frob.  For now, mostly whether there
			; is garbage in the CDR-CODE and FLAG bits.
;ma-original-source	;attempts to be transparent to MOVEs.  This loses because other
			; paths may merge in.
 ma-operand-uses	;list of insts that use this.
)


(DEFUN MA-INST (MESSAGE SELF &REST ARGS)
  (SELECTQ MESSAGE
    (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
    ((:PRINT :PRINT-SELF)
     (FORMAT (CAR ARGS) "#<MA-INST ~A>"
	     (MA-INST-CODE SELF)))
    (OTHERWISE (FERROR NIL "~S unknown" MESSAGE))))

(DEFUN MA-STATE (MESSAGE SELF &REST ARGS)
  (SELECTQ MESSAGE
    (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
    ((:PRINT :PRINT-SELF)
     (FORMAT (CAR ARGS) "#<MA-STATE ~A ~A, PDL LEN ~D>"
	     (COND ((EQ SELF (MA-INST-BEFORE-STATE (MA-STATE-INST SELF)))
		    'BEFORE)
		   (T 'AFTER))
	     (MA-STATE-INST SELF)
	     (LENGTH (MA-STATE-STACK-ALIST SELF))))
    (OTHERWISE (FERROR NIL "~S unknown" MESSAGE))))

(DEFUN MA-OPERAND (MESSAGE SELF &REST ARGS)
  (SELECTQ MESSAGE
    (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
    ((:PRINT :PRINT-SELF)
     (FORMAT (CAR ARGS) "#<MA-OPERAND ~A>"
	     (MA-OPERAND-NAME SELF)))
    (OTHERWISE (FERROR NIL "~S unknown" MESSAGE))))

(DEFUN MA-CUBBYHOLE (MESSAGE SELF &REST ARGS)
  (SELECTQ MESSAGE
    (:WHICH-OPERATIONS '(:PRINT :PRINT-SELF))
    ((:PRINT :PRINT-SELF)
     (FORMAT (CAR ARGS) "#<MA-CUBBYHOLE ~A>"
	     (MA-CUBBYHOLE-NAME SELF)))
    (OTHERWISE (FERROR NIL "~S unknown" MESSAGE))))


(DEFMACRO MA-EMIT (&REST FIELD-VALUE-PAIRS)
  `(MA-EMIT-EXECUTE1 (MA-EVAL ,@FIELD-VALUE-PAIRS)))

(DEFUN MA-EMIT-EXECUTE1 (VAL)
  (SETQ *EMIT-LIST* (NCONC *EMIT-LIST* (LIST VAL)))
  (SETQ *PDL-BUFFER-WRITE-HAPPENING* NIL))

;arg is a list of <field> <value>.  <field> is usually a constant (number or symbol
; with value) at compile time.  If <value> is also a constant, the combination is 
; performed at macro-expand time.  Otherwise, it is expanded into a call to MA-RUNTIME-EVAL.

(DEFUN MA-EVAL MACRO (X)
  (LET ((CONSTANT 0)
	(SPECIFIED-BITS 0)
	(RUNTIME-LETLIST NIL)
	(RUNTIME-EXPS NIL))
    (PROG (FIELD VALUE L)
	  (SETQ L (CDR X))
      L   (COND ((NULL L) (RETURN NIL)))
	  (SETQ FIELD (CAR L) VALUE (CADR L))
      L1  (COND ((AND (NUMBERP FIELD) (NUMBERP VALUE))
		 (COND ((NOT (ZEROP (LDB FIELD SPECIFIED-BITS)))
			(FERROR NIL "
Overlapping fields. This field ~S, this value ~S, previously specified mask ~S"
				FIELD VALUE SPECIFIED-BITS)))
		 (SETQ CONSTANT (DPB VALUE FIELD CONSTANT))
		 (SETQ SPECIFIED-BITS (DPB -1 FIELD SPECIFIED-BITS)))
		((AND (SYMBOLP FIELD)
		      (BOUNDP FIELD)
		      (NUMBERP (SYMEVAL FIELD)))
		 (SETQ FIELD (SYMEVAL FIELD))
		 (GO L1))
		((AND (SYMBOLP VALUE)
		      (BOUNDP VALUE)
		      (NUMBERP (SYMEVAL VALUE)))
		 (SETQ VALUE (SYMEVAL VALUE))
		 (GO L1))
		((NOT (OR (SYMBOLP FIELD) (NUMBERP FIELD)))
		 (LET ((GS (GENSYM)))
		   (SETQ RUNTIME-LETLIST
			 (NCONC RUNTIME-LETLIST
				(LIST `(,GS ,FIELD))))
		   (SETQ FIELD GS))
		 (GO L1))
		((NOT (OR (SYMBOLP VALUE) (NUMBERP VALUE)))
		 (LET ((GS (GENSYM)))
		   (SETQ RUNTIME-LETLIST
			 (NCONC RUNTIME-LETLIST
				(LIST `(,GS ,VALUE))))
		   (SETQ VALUE GS))
		 (GO L1))
		(T (SETQ RUNTIME-EXPS (NCONC RUNTIME-EXPS (LIST FIELD VALUE)))))
	  (SETQ L (CDDR L))
	  (GO L))
    `(LET ,RUNTIME-LETLIST
       (MA-RUNTIME-EVAL ,CONSTANT ,SPECIFIED-BITS ,@RUNTIME-EXPS))
    ))

;SPECIFIED-BITS is just for error checking.
(DEFUN MA-RUNTIME-EVAL (CONSTANT SPECIFIED-BITS &REST FIELD-VALUES)
  (PROG (FIELD VALUE L sym-fields)
 	(SETQ L FIELD-VALUES)
     L	(COND ((NULL L)
	       (RETURN (COND (SYM-FIELDS
			       (LIST CONSTANT SYM-FIELDS))
			     (T CONSTANT)))))
 	(SETQ FIELD (CAR L) VALUE (CADR L))
     L1 (COND ((AND (NUMBERP FIELD) (NUMBERP VALUE))
 	       (COND ((NOT (ZEROP (LDB FIELD SPECIFIED-BITS)))
 		      (FERROR NIL "
Overlapping fields. This field ~S, this value ~S, previously specified mask ~S"			      FIELD VALUE SPECIFIED-BITS)))
	       (SETQ CONSTANT (DPB VALUE FIELD CONSTANT))
 	       (SETQ SPECIFIED-BITS (DPB -1 FIELD SPECIFIED-BITS)))
 	      ((AND (SYMBOLP FIELD)
 		    (BOUNDP FIELD)
 		    (NUMBERP (SYMEVAL FIELD)))
 	       (SETQ FIELD (SYMEVAL FIELD))
 	       (GO L1))
	      ((AND (SYMBOLP VALUE)
		    (BOUNDP VALUE)
		    (NUMBERP (SYMEVAL VALUE)))
	       (SETQ VALUE (SYMEVAL VALUE))
	       (GO L1))
	      (T
	       (SETQ SPECIFIED-BITS (DPB -1 FIELD SPECIFIED-BITS))
	       (SETQ SYM-FIELDS
		     (CONS (LIST FIELD VALUE) SYM-FIELDS))))
	(SETQ L (CDDR L))
	(GO L)))
