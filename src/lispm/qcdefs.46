;Definitions and specials for the Lisp machine Lisp compiler  -*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

(IF-FOR-MACLISP (DECLARE (GENPREFIX *COMP*)))
(DECLARE (SETQ RUN-IN-MACLISP-SWITCH T))

;MACLISP AND LISP-MACHINE-LISP
; IT WOULD BE NICE IF THIS FILE COULD WIN IN ALL THE FOLLOWING DISTINCT MODES:
;(1) READ IN IN MACLISP, RUN INTERPRETIVELY IN MACLISP.
;(2) READ IN IN MACLISP, COMPILE WITH QCOMPL, RESULTING COMPILER RUNS IN MACLISP.

;(3) READ IN IN MACLISP, COMPILE WITH QCMP, RESULTING COMPILER RUNS IN LM-LISP.

;(4) READ IN IN LM-LISP, RUN INTERPRETIVELY IN LM-LISP.
;(5) READ IN IN LM-LISP, COMPILE WITH QCMP, RESULTING COMPILER RUNS IN LM-LISP.

;INCOMPATABILITIES
;   THIS FILE IS ALMOST ENTIRELY SIMPLE-MINDED LISP WHICH IS COMPATABILE. HOWEVER,
; (1) MACLISP STORES FUNCTIONS ON PROPERTY LISTS, LM-LISP IN FUNCTION CELLS  (GET-LOSSAGE).
; (2) MACLISP HAS FEXPRS, LM-LISP &QUOTE &REST, ETC  (FEXPR-LOSSAGE).
; (3) GIVEN A FUNCTION, PROCEEDURE FOR DETERMINING HOW MANY ARGS IT HAS, ETC,
;     IS DIFFERENT.  THIS IS REFLECTED IN A COMPLETELY DIFFERENT GET-Q-ARG-DESC FUNCTION.
; (4) CERTAIN FUNCTIONS ARE NOT DESIRED IN LM-LISP (MOSTLY BECAUSE THEY'RE ALREADY THERE).
;     THEY PROBABLY SHOULDN'T BE IN THIS FILE AT ALL, BUT.
; (5) ERRSET, CATCH AND THROW WORK SOMEWHAT DIFFERENTLY.  (ERRSET-LOSSAGE)

(DECLARE (SPECIAL QC-ERROR-OUTPUT-FILE QC-BARF-P))

(DECLARE (SPECIAL LAMBDA-LIST-KEYWORDS))

;; This is an area used by the compiler to cons in.
;; It is reset from time to time during the compilation.
(DEFVAR QCOMPILE-TEMPORARY-AREA)

;This is a function which is called on each broken-off function's compiled lap code.
;Usually it is an entry point into lap.
(DEFVAR QCOMPILE-POST-PROC)

;This is T if the compiler is being used to generate macro-code
;which will be passed to the microcompiler.
;In that case, the code is generated a little differently
;so as to lead to more optimal microcode.
;(Actually, it can fail to be valid macrocode, in little ways).
(DEFVAR GENERATING-MICRO-COMPILER-INPUT-P NIL)

;FUNCTION-BEING-PROCESSED is the function name which the compiler was called on.
;It is NOT bound for broken-off internal functions.  LAST-ERROR-FUNCTION
;is what FUNCTION-BEING-PROCESSED was the last time we printed a warning.
(DEFVAR FUNCTION-BEING-PROCESSED)
(DEFVAR LAST-ERROR-FUNCTION)

;If HOLDPROG is nil, the lap instructions are typed out on the terminal
;instead of being saved up for lap.  It is normally set to T globally.
(DEFVAR HOLDPROG T)

;SPECIALFLAG is T if function binds any special variables (or BIND is called).
;This information goes into the FEF.
(DEFVAR SPECIALFLAG)

;LOCAL-DECLARATIONS (on SYSTEM) is a list of local declarations.
;Each local declaration is a list starting with an atom which says
;what type of declaration it is.  The meaning of the rest of the
;list depends on the type of declaration.
;The compiler is interested only in SPECIAL and UNSPECIAL declarations,
;for which the rest of the list contains the symbols being declared,
;and MACRO declarations, which look like (DEF symbol MACRO LAMBDA args ..body...),
;and ARGLIST declarations, which specify arglists to go in the debugging info
;(to override the actual arglist of the function, for user information)
;which look like (ARGLIST FOO &OPTIONAL BAR ...), etc.

;Things get onto LOCAL-DECLARATIONS in two ways:
;1) inside a LOCAL-DECLARE, the specified declarations are bound onto the front.
;2) if UNDO-DECLARATIONS-FLAG is T, some kinds of declarations
;   in a file being compiled into a QFASL file
;   are consed onto the front, and not popped off until LOCAL-DECLARATIONS
;   is unbound at the end of the whole file.
(DEFVAR LOCAL-DECLARATIONS NIL)
(DEFVAR UNDO-DECLARATIONS-FLAG NIL)

;FILE-LOCAL-DECLARATIONS is just like LOCAL-DECLARATIONS except that it is
;local to the file being compiled.  The reason this exists is so that if
;you have a (LOCAL-DECLARE ((ARGLIST ...)) ...) around a (MACRO...),
;at compile-time the macro wants to be saved on LOCAL-DECLARATIONS, but that
;is bound by the LOCAL-DECLARE, so it uses FILE-LOCAL-DECLARATIONS instead.
(DEFVAR FILE-LOCAL-DECLARATIONS NIL)

;BARF-SPECIAL-LIST is a list of all variables automatically declared special
;by the compiler.  Those symbols are special merely by virtue of being on
;this list, which is bound for the duration of the compilation
;(for the whole file, whole editor buffer, or just the one function in COMPILE).
;All users of QC-TRANSLATE-FUNCTION MUST bind this variable.
;NOTE!! This list must not be CONSed in FASD-TEMPORARY-AREA!!  It lives across
; whack boundaries.
(DEFVAR BARF-SPECIAL-LIST)

;SPECIAL-PKG-LIST is a list of packages all of whose symbols should be special.
(DEFVAR SPECIAL-PKG-LIST (LIST (PKG-FIND-PACKAGE "FONTS")))

;BINDP on pass 1 is T if BIND is called in the current PROG.
;It is then consed into the internal form of the PROG, for pass 2's sake.
(DEFVAR BINDP)

;Pass 2 variables needed only in QCP2 except for binding in QCOMPILE0:
;See the beginning of QCP2 for more information on them.
(DECLARE (SPECIAL PDLLVL MAXPDLLVL TAGOUT DROPTHRU CALL-BLOCK-PDL-LEVELS))

;QCMP-OUTPUT on the Lisp machine is an ART-Q-LIST array into which the
;lap-instructions are stored by pass 2.  In Maclisp, it is a list onto which
;the instructions are PUSHed;  it is NREVERSEd and sent to lap.
(DEFVAR QCMP-OUTPUT)

;TLEVEL on pass 1 is T if we are at "top level" within the function being compiled,
;not within any actual function calls.
;If a PROG is seen when TLEVEL is set, the locals of the prog can
;be initialized by the entry to the function.
(DEFVAR TLEVEL)

;TLFUNINIT on pass 1 is T if we have already seen a variable initialized to the
;result of a function call.  Such initializations can't be done except
;by compiled code, and once we have initialized one thing that way
;all succeeding variables must be initialized by code as well.
;(This applies to SPROGs.  PPROGs are a little different).
(DEFVAR TLFUNINIT)

;FAST-ARGS-POSSIBLE on pass 1 is T if we haven't come across
;any argument to this function with a non-NIL initialization.
;If this remains T after all the arguments are processed,
;then it is an optimization to make top-level prog vars
;be initialized at function entry instead of by code.
(DEFVAR FAST-ARGS-POSSIBLE)

;P1VALUE is T on pass 1 when compiling a form for value, NIL if for effect.
;On pass 2, "destinations" are used instead, with many more alternatives.
(DEFVAR P1VALUE)

;BINDS is set to T on pass 1 when a call to BIND is seen.
;It is bound by each PROG.
(DEFVAR BINDS)

;MACROLIST is an alist of macro definitions to be used only while compiling.
;While compiling a file, macros in the file get put on MACROLIST temporarily.
(DEFVAR MACROLIST NIL)

;PEEP-ENABLE, if T, means that the peephole optimizer should be used.
(DEFVAR PEEP-ENABLE NIL)

;FUNCTIONS-DEFINED is a list of all functions defined in the file being compiled.
(DEFVAR FUNCTIONS-DEFINED)

;FUNCTIONS-REFERENCED is a list of all functions referred to in the file being
;compiled, and not defined in the world.  Each element has as its CAR the
;name of the function, and as its CDR a list of the names of the functions
;which referenced it.
(DEFVAR FUNCTIONS-REFERENCED)


;Compiler switches:  set these with (DECLARE (SETQ ...))
;These are initialized in QC-PROCESS-INITIALIZE

;This controls which bound variables have their names saved for debugging.
;I think it no longer has any effect, though, and all of them are saved.
(DEFVAR RETAIN-VARIABLE-NAMES-SWITCH)

;This, if T, causes MAP, etc. to be open-coded.  It is normally T.
(DEFVAR OPEN-CODE-MAP-SWITCH)

;This, if T, causes a check to be made for the use of a local variable
;as a function to be called, meaning funcall.  This should be set to T
;only for compiling old-fashioned Maclisp code.
(DEFVAR ALLOW-VARIABLES-IN-FUNCTION-POSITION-SWITCH)

;This, if T, makes all variabes special.
(DEFVAR ALL-SPECIAL-SWITCH)

;This, if T (as it usually is), warns the user if any obsolete
;Maclisp functions are used.
(DEFVAR OBSOLETE-FUNCTION-WARNING-SWITCH)

;This, if T, warns the user if he does anything that clearly
;cannot work in Maclisp.
(DEFVAR RUN-IN-MACLISP-SWITCH)

;This, if T, prevents warnings about a lot of stylistic losses.
(DEFVAR INHIBIT-STYLE-WARNINGS-SWITCH)

;If non-null, this is the name of an editor buffer in which warnings are saved
(DEFVAR COMPILER-WARNINGS-BUFFER "Compiler Warnings")
;Switch to enable saving of all warnings.  Default is to flush buffer
;each time a new compilation is started.
(DEFVAR CONCATENATE-COMPILER-WARNINGS-P NIL)

;;; Variables data bases:

;Bound (local or special) variables are described by two lists of variable descriptors:
;VARS, which describes only variables visible from the current point of compilation,
;and ALLVARS, which describes all variables seen so far in the current compilation.
(DEFVAR VARS)
;ALLVARS is passed to lap to allocate slots, while VARS is used on both passes
;for figuring out what to do with a variable.
(DEFVAR ALLVARS)

;In addition, FREEVARS is a list of all special variables referred to free.
(DEFVAR FREEVARS)

;ARG-MAP and LOCAL-MAP are given the arg map and local map for the debugging info.
;This is done by ASSIGN-LAP-ADDRESSES, so that special vars that get a slot
;can be put in the map even though their places in it will not be recogizable
;from their lap addresses.
(DEFVAR ARG-MAP)
(DEFVAR LOCAL-MAP)

;Each element of VARS or ALLVARS describes one variable, and is called a VAR or a "home".
;A VAR has these components:
(DEFMACRO VAR-NAME (VAR) `(CAR ,VAR))	;NAME must be first since we use ASSQ on it.
(DEFMACRO VAR-KIND (VAR) `(CADR ,VAR))
(DEFMACRO VAR-TYPE (VAR) `(CADDR ,VAR))
(DEFMACRO VAR-USE-COUNT (VAR) `(CADDDR ,VAR))
(DEFMACRO VAR-LAP-ADDRESS (VAR) `(CAR (CDDDDR ,VAR)))
(DEFMACRO VAR-INIT (VAR) `(CADR (CDDDDR ,VAR)))
(DEFMACRO VAR-EVAL (VAR) `(CADDR (CDDDDR ,VAR)))
(DEFMACRO VAR-MISC (VAR) `(CADDDR (CDDDDR ,VAR)))
(DEFMACRO VAR-DECLARATIONS (VAR) `(CAR (CDDDDR (CDDDDR ,VAR))))

(DEFMACRO SETF-VAR-INIT (VAR VALUE) `(RPLACA (CDR (CDDDDR ,VAR)) ,VALUE))
(DEFMACRO SETF-VAR-KIND (VAR VALUE) `(RPLACA (CDR ,VAR) ,VALUE))
(DEFMACRO SETF-VAR-LAP-ADDRESS (VAR VALUE) `(RPLACA (CDDDDR ,VAR) ,VALUE))

;The KIND is one of
; (FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST FEF-ARG-AUX FEF-ARG-INTERNAL-AUX) 
;The TYPE is either FEF-LOCAL, FEF-SPECIAL, or FEF-REMOTE.
;The USE-COUNT is the number of times the variable is referred to (read or written),
;  not counting the binding and initialization.
;The LAP-ADDRESS is an instruction address for Lap to refer to this variable.
;  It specifies the either the argument block or the local block, and an offset.
;  For special variables, it is (SPECIAL varname).
;For the INIT, see below.
;The EVAL is FEF-QT-QT, FEF-QT-EVAL, etc., saying how an argument needs to be evaluated.
;The MISC is a list of other FEF-mumble-mumble symbols for this variable.
;  These include FEF-ARG-FUNCTIONAL meaning set the "functional" flag on this arg,
;  and FEF-ARG-SPECIFIED-FLAG meaning this is the specified-flag of an optional arg.
;  Lap has a tendency to eval these symbols and add them into the ADL word.
;  So any not intended for Lap should be given the value 0, here.
(DEFVAR FEF-ARG-SPECIFIED-FLAG 0)

;VAR-DECLARATIONS is an ALIST recording declarations pertaining to this variable.
;  It is produced by examining LOCAL-DECLARATIONS and FILE-LOCAL-DECLARATIONS
; in the context the variable's creation, as well as static declarations
; on the variables' PLIST.

; These keys, seen in any of the above contexts, are assumed to apply to lists of
;variables.
(DEFVAR VARIABLE-DECLARATION-KEYS '(FIXNUM FLONUM NOTYPE))


;Ordinary arguments are allocated slots in the argument portion of the pdl frame.
;ARGN counts the number of them.
(DEFVAR ARGN)
;Rest args, aux variables and nonspecial variables of kind internal-aux
;are allocated slots in the local portion of the stack frame.
;LVCNT counts the number of them.
(DEFVAR LVCNT)
;Special variables, free or bound, require slots in a portion of the FEF.
;SVCNT counts the number of them.
(DEFVAR SVCNT)

;The INIT is of the form ( <type> <data> . <arg-supplied-flag home>)
;The arg-supplied-flag name is the home of FOOP in &OPTIONAL (FOO NIL FOOP).
;It appears only for optional arguments which have such a flag.
;If there is none, the cddr of INIT will be nil.
;The type is of of several symbols starting with "FEF-INI-", that
;signify one of the ways of initializing the variable.
;FEF-INI-COMP-C indicates that compiled code will be used to
;do the initialization.  It is the most general.  The other types
;exist to make special cases more efficient.  They are:

;FEF-INI-NONE		No initialization (for a local variable which should be nil).
;FEF-INI-SELF		Initialize to self (for special variable).
;FEF-INI-NIL		Initialize to NIL (for special variable).
;FEF-INI-PNTR		Initialize to a constant.  <data> is that constant.
;FEF-INI-C-PNTR		Initialize to the contents of a location.  <data> points to it.
;FEF-INI-EFF-ADR	Initialize to the contents of an "effective address".
;			This is used to copy the value of a previous arg or local variable.
;			<data> specifies which one, using an instruction source field
;			which will specify the arg block or the local block, plus offset.
;FEF-INI-OPT-SA		For an optional variable with a complicated default value.
;			<data> specifies a starting address inside the function
;			which is where to start if the argument IS supplied.
;			It follows the code used to compute and store the default value.
;FEF-INI-COMP-C		Indicates that the variable will be initialized by the
;			compiled code of the function.

;;;	- IN GENERAL -
;;;	INTERNAL VARIABLES ARE BOUND BY INTERNAL LAMBDA'S AND PROGS
;;;	OTHERS ARE BOUND AT ENTRY TIME
;;;	ALL INTERNAL VARIABLES ARE INITIALIZED BY CODE
;;;	ARG VARIABLES ARE NEVER INITIALIZED
;;;	OPTIONAL AND AUX VARIABLES ARE INITIALIZED AT BIND TIME
;;;	IF POSSIBLE OTHERWISE BY CODE
;;;	THIS "POSSIBILITY" IS DETERMINED AS FOLLOWS:
;;;		INITIALLY, IT IS POSSIBLE
;;;		IT REMAINS POSSIBLE UNTIL YOU COME TO A VARIABLE
;;;		INITIALIZED TO A FCTN, AT WHICH POINT IT IS NO LONGER POSSIBLE
;;;	IF VAR TO BE INITIALIZED BY CODE, CODE 0 (SPECIAL) OR
;;;	1 (LOCAL) IS USED IN INITIALIZATION FLD

;PROG, GO and RETURN data bases.

;ALLGOTAGS is a list of all prog-tags defined so far in the current function,
;whether the progs defining them contain the current one or not.
;ALLGOTAGS is used to determine when a new tag must be renamed.
(DEFVAR ALLGOTAGS)

;The variable GOTAGS contains an alist describing all the prog tags
;of progs the code we are currently compiling is contained in.
;Each element of GOTAGS is a GOTAG:
(DEFVAR GOTAGS)

;Mustn't use DEFSTRUCT, since must run in Maclisp!
(DEFMACRO GOTAG-PROG-TAG (GOTAG) `(CAR ,GOTAG))	;Name of prog-tag described.
(DEFMACRO GOTAG-LAP-TAG (GOTAG) `(CADR ,GOTAG))	;Name of corresponding lap-tag.
(DEFMACRO GOTAG-PDL-LEVEL (GOTAG) `(CADDR ,GOTAG))  ;Pdl level to pop back to when going there.
(DEFMACRO GOTAG-PROG-NAME (GOTAG) `(CADDDR ,GOTAG)) ;Name of PROG this tag is in, or NIL.

;Mustn't use SETF either.
(DEFMACRO SETF-GOTAG-PDL-LEVEL (GOTAG LEVEL) `(RPLACA (CDDR ,GOTAG) ,LEVEL))

(DEFMACRO MAKE-GOTAG (&REST ARGS) `(LIST . ,ARGS))

;PROGDESCS is a list of descriptors of PROGs which are active, on pass 2.
;Each descriptor looks like (progname rettag idest m-v-target pdl-level nbinds).
;The <progname> is the name of the prog, or NIL for an anonymous one.
;A progname of T indicates an "invisible" compiler-generated PROG
;which an explicit RETURN should return past.
;The <rettag> is a gensym tag which is defined at the end of the prog body.
;The <idest> is the destination which return's in the prog send values to.
;That may not be the same as the ultimate destination of the prog.
;If they are different, then there is code after the <rettag> to
;move from <idest>, which is D-PDL, to the real destination.
;The <m-v-target> is the M-V-TARGET of the call to PROG.
;If it is NIL, only one value is wanted from the prog.
;If it is MULTIPLE-VALUE-LIST, then the prog should really
;return the list of the values that RETURN wants to return.
;If it is a list of variables, then returning should SETQ those variables
;and also return the first value in the ordinary way.
;If <idest> is D-RETURN, then all this is taken care of by the
;function calling mechanism, and isn't known at compile time, so
;in that case <m-v-target> is ignored.
;The <pdl-level> is the pdl level at entry to the prog,
;which is also the level in between statements in the prog.
;<nbinds> is the number of special bindings to undo at exit from the prog,
;or a list containing the number of specials bound at entry to the prog,
;if the prog contains calls to BIND which must be unbound first by
;calling output-unbind-to-index.

;The CAR of PROGDESCS describes the innermost prog.
(DEFVAR PROGDESCS)

;Mustn't use DEFSTRUCT, since must run in Maclisp!
(DEFMACRO PROGDESC-NAME (DESC) `(CAR ,DESC))
(DEFMACRO PROGDESC-RETTAG (DESC) `(CADR ,DESC))
(DEFMACRO PROGDESC-IDEST (DESC) `(CADDR ,DESC))
(DEFMACRO PROGDESC-M-V-TARGET (DESC) `(CADDDR ,DESC))
(DEFMACRO PROGDESC-PDL-LEVEL (DESC) `(CAR (CDDDDR ,DESC)))
(DEFMACRO PROGDESC-NBINDS (DESC) `(CADR (CDDDDR ,DESC)))

(DEFMACRO MAKE-PROGDESC (PROGNAME RETTAG IDEST M-V-TARGET PDL-LEVEL NBINDS)
	  `(LIST ,PROGNAME ,RETTAG ,IDEST ,M-V-TARGET ,PDL-LEVEL ,NBINDS))

;RETPROGDESC is the descriptor of the prog for RETURN, RETURN-LIST, etc.
;to return from.  Normally, it is the descriptor of the innermost prog
;(not counting "invisible" PROGs named T),
;or nil if no prog is active (return is an error),
;but (RETURN-FROM <progname> ...) binds it to the descriptor for the specified prog.
(DEFVAR RETPROGDESC)

;Several variables must be declared explicitly in Maclisp.
;These must be special not declare (special ..), because this file is read into 
;the compiler, not compiled
(IF-FOR-MACLISP
 (SPECIAL FASD-TEMPORARY-AREA DEFAULT-CONS-AREA *LEXPR-ARGLIST*))
