;;; ZWEI Definitions (ZWEI Was EINE Initially) -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Global parameters.  These variables will have global values,
;;; and any editor may bind or not bind them as it pleases.
;;; NOTE: Those specials that are ZWEI variables are defined with DEFVARIABLE
;;; in the MACROS file.  They are of the same status as these variables.
(DEFVAR *GLOBAL-INITIALIZATION-LIST*)
(SETQ *GLOBAL-INITIALIZATION-LIST* NIL)
(DEFMACRO DEFGLOBAL (VAR &OPTIONAL (INITIAL-VALUE NIL IVP))
  `(PROGN 'COMPILE
     (SPECIAL ,VAR)
     . ,(AND IVP
	     `((PUSH (CONS ',VAR ,INITIAL-VALUE) *GLOBAL-INITIALIZATION-LIST*)))))

(DEFGLOBAL *TICK* 0)			;A time-stamp, used for redisplay.
(DEFGLOBAL *LINE-AREA* SYS:PERMANENT-STORAGE-AREA)	;The area in which to make lines.
					;Very few lines become garbage, so put them in
					;the permanent area.
(DEFGLOBAL *WORD-SYNTAX-TABLE*)		;These are 1-d arrays indexed by the
(DEFGLOBAL *LIST-SYNTAX-TABLE*)		;%%CH-CHAR, holding each chararcters syntax.
(DEFGLOBAL *ATOM-WORD-SYNTAX-TABLE*)	;*WORD-SYNTAX-TABLE* for Atom Word Mode.
(DEFGLOBAL *UTILITY-PACKAGE*)		;A random hash table of things like qregs.
(DEFGLOBAL *Q-REG-LIST* NIL)		;List of q-registers currently defined.
(DEFGLOBAL *STANDARD-COMTAB*)		;The standard set of harmless ZWEI commands.
(DEFGLOBAL *STANDARD-CONTROL-X-COMTAB*)	;Similarly, the simple control-X commands.
(DEFGLOBAL *COMPLETING-READER-COMTAB*)	;Comtab for the completing reader environment.
(DEFGLOBAL *CONTROL-R-COMTAB*)		;Comtab for recursive edits on the same buffer.
(DEFGLOBAL *RECURSIVE-EDIT-COMTAB*)	;Comtab for recursive edits on a new buffer.
(DEFGLOBAL *STANDALONE-COMTAB*)		;Comtab for simple standalone editors
(DEFGLOBAL *WORD-ABBREV-TABLE*)		;For each character, non-0 => this expands abbrevs.
(DEFGLOBAL *PREVIOUS-MODE-LINE* NIL)	;List of strings that make up the state of mode line.
(DEFGLOBAL *TAB-STOP-BUFFER*)		;Buffer used by tab to tab stop
(DEFGLOBAL *KILL-RING* NIL)		;List of intervals.
(DEFGLOBAL *SEARCH-RING* NIL)		;List of strings.
(DEFGLOBAL *LISP-PARSE-PREPARSED-FLAG* NIL)	;If this is T, LISP-PARSE-FROM-DEFUN assumes
					;that the lines are already parsed.
(DEFGLOBAL *MOUSE-X*)			;Position of the mouse for the previous command
(DEFGLOBAL *MOUSE-Y*)			;relative to *WINDOW*
(DEFGLOBAL *UNDO-START-BP*)		;Previous start of region saved for undoing
(DEFGLOBAL *UNDO-END-BP*)		;End of that region
(DEFGLOBAL *UNDO-OLD-INTERVAL*)		;Copy of old contents
(DEFGLOBAL *UNDO-TYPE*)			;Type of command that caused undo saving

;;; Redisplay levels.  These are symbolic constants.
;;; They have global values and should never be bound.
(DEFVAR DIS-NONE 0)		;No redisplay needed.
(DEFVAR DIS-MARK-GOES 1)	;No redisplay needed except maybe removing region underlining.
(DEFVAR DIS-BPS 2)		;Point and mark may have moved, but text is unchanged.
(DEFVAR DIS-LINE 3)		;Text in one line may have changed.
				;WINDOW-REDISPLAY-LINE is that line.
				;WINDOW-REDISPLAY-INDEX says where in the line changes start.
(DEFVAR DIS-TEXT 4)		;Any text might have changed.
(DEFVAR DIS-ALL 5)		;Global parameters of the window have changed.
				;Clean the window and redisplay all lines from scratch.

;;; Syntax codes in *LISP-SYNTAX-TABLE*
(DEFVAR LIST-ALPHABETIC 0)	;Part of an atom.
(DEFVAR LIST-DELIMITER 1)	;Separates things but has no other significance.
(DEFVAR LIST-SLASH 2)		;Quotes the following character.
(DEFVAR LIST-DOUBLE-QUOTE 3)	;Starts a grouping terminated by another of itself.
(DEFVAR LIST-SINGLE-QUOTE 4)	;Tacks onto the front of a sexp to make another sexp.
(DEFVAR LIST-CLOSE 5)		;Closeparentheses
(DEFVAR LIST-OPEN 6)		;Openparentheses
(DEFVAR LIST-COMMENT 7)		;Starts a comment.

;;; Syntax codes in *WORD-SYNTAX-TABLE*.
(DEFVAR WORD-ALPHABETIC 0)	;Part of words.
(DEFVAR WORD-DELIMITER 1)	;Separates words.

;;; Definitions of the ZWEI data structures.

;;; An editor is a command loop and the associated variables.
;;; Since almost all the code in the editor runs within one, all their
;;; variables are declared special.

(DEFFLAVOR EDITOR
       (
	*INTERVAL*			;The interval which you are editing
	*WINDOW*			;The default window.
	*COMTAB*			;Current comtab (for Install Command to hack).
	(*FONT* 0)			;Current font for self-inserting.
	*NUMERIC-ARG*			;The value of the numeric argument, or 1.
	*NUMERIC-ARG-P*			;T if there is a numeric argument.
	*NUMERIC-ARG-N-DIGITS*		;Number of characters typed to get this command's arg
	*LAST-COMMAND-CHAR*		;The character typed to get this command.
	(*CURRENT-COMMAND* NIL)		;The command (symbol) now being executed.
	(*CURRENT-COMMAND-TYPE* NIL)	;The "type" (a symbol) of the current command.
	(*LAST-COMMAND-TYPE* NIL)	;The "type" of the last command executed.
	*REAL-LINE-GOAL-XPOS*		;Used by real-line commands.
	*MARK-STAYS*			;Tells command loop whether to preserve region.
	(*CENTERING-FRACTION* *CENTER-FRACTION*);Tells redisplay where to recenter, if needed.
	*QUANTITY-MODE*			;Current MODE, also free var for the generic cmds.
	(*QUANTITY-MODE-SAVE* NIL)	;Array to save bindings if MODE feature is used.
	(*MODE-QUANTITY-NAME* NIL)	;This is for the mode line
	*IN-COM-DOC-P*			;T => We are inside COM-DOCUMENTATION.
	*REPEAT-DOC-P*			;T => COM-DOCUMENTATION is repeating what it did last.
	(*COM-DOC-LAST-CHAR* #/B)	;Last char typed to COM-DOCUMENTATION.
	(*LAST-FILE-NAME-TYPED* "")	;Last thing the guy typed when asked for a file name.
	(*LAST-EXPANDED* NIL)		;The last thing expanded
	(*LAST-EXPANSION* NIL)		;What it expanded to
	(*LAST-EXPANSION-BP* NIL)	;Where the expansion begins
	*LAST-EXPANSION-USAGE-PROP*	;Its usage parameter
	*LAST-EXPANSION-SYMBOL*		;The symbol it is stored as in the utility-package
	(*WORD-ABBREV-PREFIX-MARK* NIL)	;A BP for expanding prefixed word abbrevs
	(*STANDARD-COMMAND* 'COM-SELF-INSERT)	;The command to be performed by alphanumerics.
	(*FONT-NAME* NIL)		;As is this
	(*MACRO-LEVEL* NIL)		;And this
	(*MINI-BUFFER-COMMAND* NIL)	;This is for the C-X  command
	)
       ()
  (:INITABLE-INSTANCE-VARIABLES *COMTAB* *WINDOW*)
 )

;;;Here's something you can call from outside
(DEFFLAVOR TOP-LEVEL-EDITOR
       ((*MODE-LINE-LIST* '("ZWEI " "(" *MODE-NAME-LIST*
					(*MODE-QUANTITY-NAME* " <" *MODE-QUANTITY-NAME* ">")
				    ")"))
	(*MODE-LIST* NIL)		;List of modes in effect; see MODES >.
	(*MAJOR-MODE* *DEFAULT-MAJOR-MODE*)	;Current major mode; see MODES >.
	(*MODE-NAME-LIST* NIL)		;This is for the mode line
	*MODE-COMTAB*			;A sparse comtab for mode redefinitions
	*MODE-WORD-SYNTAX-TABLE*	;A sparse syntax table for mode redefinitions
	(*WINDOW-LIST* NIL)		;List of windows belonging to this editor
	(*COMMAND-HOOK* NIL)		;List of functions to be applied to command char.
	(*POST-COMMAND-HOOK* NIL)	;Same for after normal function has been done.
	*TYPEOUT-WINDOW*		;The menu-like typeout window
	*MODE-LINE-WINDOW*		;Where the mode line is displayed
	*TYPEIN-WINDOW*			;For prompts
	*MINI-BUFFER-WINDOW*		;A special editor window

	TV:IO-BUFFER
	)
       (EDITOR)
 (:INITABLE-INSTANCE-VARIABLES *MODE-LINE-LIST* *MAJOR-MODE*)
 (:GETTABLE-INSTANCE-VARIABLES TV:IO-BUFFER)
 (:DOCUMENTATION :MIXIN "A callable editor"))

(DEFFLAVOR ZMACS-EDITOR () ()
  (:INCLUDED-FLAVORS EDITOR)
  (:DOCUMENTATION :SPECIAL-PURPOSE "An editor for ZMACS"))

(DEFFLAVOR ZMACS-TOP-LEVEL-EDITOR
       ((*MODE-LINE-LIST* '("ZMACS " "(" *MODE-NAME-LIST*
				     (*MODE-QUANTITY-NAME* " <" *MODE-QUANTITY-NAME* ">")
				     ") " *ZMACS-BUFFER-NAME*
				     (*FONT-NAME* "  Font: " *FONT-NAME*)
				     (*MACRO-LEVEL* "  Macro-level: " *MACRO-LEVEL*)
				     *BUFFER-MODIFIED-P*))
	STANDARD-INPUT
	(PACKAGE PACKAGE)	;Must not be unbound or who-line will blow out
	)
       (ZMACS-EDITOR TOP-LEVEL-EDITOR)
  (:INITABLE-INSTANCE-VARIABLES STANDARD-INPUT)
  (:GETTABLE-INSTANCE-VARIABLES STANDARD-INPUT)
  (:DOCUMENTATION :SPECIAL-PURPOSE "The actual (ED) editor"))

;;; This declare all instance variables special whose name starts with an *.
(DEFMACRO GLOBALLY-DECLARE-FLAVOR-INSTANCE-VARIABLES (FLAVOR-NAME)
  `(SPECIAL . ,(DO ((L (SI:FLAVOR-LOCAL-INSTANCE-VARIABLES (GET FLAVOR-NAME 'SI:FLAVOR))
		       (CDR L))
		    (VAR)
		    (NL NIL))
		   ((NULL  L) (NREVERSE NL))
		 (AND (LISTP (SETQ VAR (CAR L)))
		      (SETQ VAR (CAR VAR)))
		 (AND (= (AREF (GET-PNAME VAR) 0) #/*)
		      (PUSH VAR NL)))))

(GLOBALLY-DECLARE-FLAVOR-INSTANCE-VARIABLES EDITOR)
(GLOBALLY-DECLARE-FLAVOR-INSTANCE-VARIABLES TOP-LEVEL-EDITOR)

(DEFSTRUCT (LINE :ARRAY-LEADER)
     LINE-LENGTH			;Number of characters.
     LINE-NEXT				;Next line.
     LINE-PREVIOUS			;Previous line.
     LINE-BP-LIST			;List of permanent BPs.
     LINE-TICK				;Last time modified.
     LINE-CONTENTS-PLIST		;Plist cleared out by MUNG-LINE.
					;Holds properties of the text in the line.
     LINE-PLIST				;Plist not cleared out.  Props of line itself.
     )

(DEFSTRUCT (TEMP-BP :LIST)
     BP-LINE				;Line at which we point.
     BP-INDEX				;Character position in line.
     )

(DEFSTRUCT (BP :LIST (:INCLUDE TEMP-BP))
     BP-STATUS				;:NORMAL or :MOVES
     BP-INTERVAL                        ;Interval in which this lives
     )

(DEFSTRUCT (INTERVAL :ARRAY)
     INTERVAL-FIRST-BP			;First line of the set, NIL if not yet in.
     INTERVAL-LAST-BP			;Last line of the set.
     )

(DEFSTRUCT (INTERVAL-WITH-TICK :ARRAY (:INCLUDE INTERVAL))
     INTERVAL-TICK                      ;Time this buffer was last munged
					;:READ-ONLY here means not changeable.
     )

(DEFSTRUCT (WINDOW :ARRAY-LEADER :NAMED)
     WINDOW-N-PLINES			;Number of lines long.

     ;; Associated data structures
     WINDOW-INTERVAL			;Interval being displayed.
     WINDOW-SHEET			;Screen system window

     ;; Redisplay
     WINDOW-REDISPLAY-DEGREE		;DIS-xxx
     WINDOW-REDISPLAY-LINE		;For DIS-LINE
     WINDOW-REDISPLAY-INDEX		;ditto
     WINDOW-LAST-POINT-PLINE		;Hint to REDISPLAY, last PLINE point was on.
     WINDOW-START-BP			;Where to start redisplaying.

     ;; Point, mark, etc.
     WINDOW-POINT
     WINDOW-MARK
     WINDOW-MARK-P
     WINDOW-POINT-PDL
     WINDOW-POINT-BLINKER
     WINDOW-SPECIAL-BLINKER-LIST

     WINDOW-FONT-ALIST	     		;Font settings for this window
     WINDOW-LAST-BP-DISPLAYED-P)	;Maintained by redisplay
     
(DEFVAR *NUMBER-OF-PLINE-PARAMETERS*)	;For the window 2-d manual defstruct.
(DEFMACRO DEFINE-WINDOW-MACROS NAMES
    (DO ((NAMES NAMES (CDR NAMES))
	 (I 0 (1+ I))
	 (RESULT NIL (CONS `(DEFMACRO ,(CAR NAMES) (WINDOW PLINE)
                               `(AREF ,WINDOW ,',I ,PLINE))
			   RESULT)))
	((NULL NAMES)
	 `(PROGN 'COMPILE
		 (SETQ *NUMBER-OF-PLINE-PARAMETERS* ,I)
		 . ,RESULT))))

(DEFINE-WINDOW-MACROS PLINE-LINE PLINE-FROM-INDEX PLINE-TO-INDEX PLINE-TICK
		      PLINE-MARKING-LEFT PLINE-MARKING-WIDTH PLINE-TEXT-WIDTH)

;;; ZMACS definitions
(DEFSTRUCT (BUFFER :ARRAY (:INCLUDE INTERVAL-WITH-TICK))
     BUFFER-NAME			;A string.  Name of file if any.
     BUFFER-FILE-GROUP-SYMBOL		;If not a file, this gets gensymmed.
     BUFFER-FILE-ID			;ID string from file job.
     BUFFER-TICK			;If FILE-ID is a string, this is the tick
					;at which we got that ID.  Else meaningless.
     BUFFER-FILE-NAME			;The filename object for if a file, else meaningless
     BUFFER-SAVED-POINT			;POINT the last time this was on a window.
     BUFFER-SAVED-MARK			;Same for MARK.
     BUFFER-SAVED-MODE-LIST		;Saved *MODE-LIST* for this buffer.
     BUFFER-SAVED-MAJOR-MODE		;Saved *MAJOR-MODE* for this buffer.
     BUFFER-SAVED-FONT-ALIST		;Saved font mapping for this buffer
     BUFFER-SAVED-WINDOW-START-BP	;BP to top of window
     )

(DEFMACRO ZMACS-BUFFER-P (INTERVAL &OPTIONAL (ELEMENT 'BUFFER-NAME))
  `(> (ARRAY-LENGTH ,INTERVAL)
     ,(IF (EQ ELEMENT T)
	  (1- (GET 'BUFFER 'SI:DEFSTRUCT-SIZE))
	  (FIND-POSITION-IN-LIST ELEMENT (GET 'BUFFER 'SI:DEFSTRUCT-ITEMS)))))

(DEFVAR *ZMACS-BUFFER-LIST*)		; This is a list of all buffers.
(DEFVAR *ZMACS-BUFFER-HISTORY*)		; See UPDATE-BUFFER-HISTORY
(DEFVAR *ZMACS-BUFFER-COUNTER* 0)	; Used to generate new buffer names.
(DEFVAR *ZMACS-COMTAB*)			; Main comtab for ZMACS
(DEFVAR *ZMACS-CONTROL-X-COMTAB*)	; Control-X prefix comtab for ZMACS.
(DEFVAR *ZMACS-MAIN-FRAME*)		; Superior to initial ZMACS windows
(DEFVAR *ZMACS-BUFFER-NAME*)		; This is for the ZMACS mode line
(DEFVAR *ZMACS-BUFFER-NAME-ALIST*)	; This is for the completing reader (C-X B)
(DEFVAR *ZMACS-COMPLETION-AARRAY*)	; As is this for (m-.)
(DEFVAR *ZMACS-TAG-TABLE-ALIST*)	; List of tag tables read in
(DEFVAR *ZMACS-COMMAND-LOOP*)		; The command-loop closure
(DEFVAR *ZMACS-STREAM*)			; Recording stream
(DEFVAR *DEFINITIONS-TO-GO* NIL)	; List of remaining definitions yet to be gone to
					; for the last function we did EDIT-DEFINITION on.
(DEFVAR *LAST-DEFINITION-SYMBOL*)	; SYMBOL of last call to EDIT-DEFINITION.
(DEFVAR *LOOK-ALIKE-DEFINITION-SYMBOLS*); List of symbols that have the same completion entry
(DEFVAR *ZMACS-WINDOW-PROCESS* NIL)	; Process to run ZMACS in
(DEFVAR *ZMACS-CALLERS-TO-BE-EDITED*)	; List of functions for Edit Next Caller to go to.
(DEFVAR *TYPEOUT-COMMAND-ALIST* NIL)	; Typeout window's "menu" handling.
