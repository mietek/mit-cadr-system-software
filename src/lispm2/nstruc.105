;; New DEFSTRUCT.			ALAN & DLW 12/11/77 -*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;; CAUTION! This file must be compilable both by NCOMPLR and by QC; be quite
;;    careful not to use anything which is dependent on one or the other Lisp dialect.

(DECLARE (COND ((STATUS FEATURE LISPM))
	       ((NULL (MEMQ 'NEWIO (STATUS FEATURES)))
		(BREAK 'YOU-HAVE-TO-COMPILE-THIS-WITH-QCOMPL T))
	       ((NULL (GET 'IF-FOR-MACLISP 'MACRO))
		(LOAD '(MACROS > DSK LISPM))
		(LOAD '(DEFMAC FASL DSK LISPM2))
		(LOAD '(LMMAC > DSK LISPM2))
		(MACROS T))))	;SEND OVER THE REST OF THE MACROS IN THIS FILE

(DECLARE (SETQ RUN-IN-MACLISP-SWITCH T))

(IF-FOR-MACLISP (SETSYNTAX '/: '/  NIL)) ;Ignore colon prefixes in Maclisp

;; This still needs some more hair, for optimization of the simple cases of the
;; constructor macros.

;;; The bullshit in here with the SUBST and so forth is temporary to
;;; fix problems with temporary-area lossage in the compiler
;;; This will still lose if there are strings in inits in the temporary
;;; area, but it will at least avoid putting properties which are whole
;;; lists consed up in the temporary area, which should alleviate the
;;; symptoms for now.
(DEFMACRO DEFSTRUCT-PUTPROP (SYM VAL IND)
  `(PROGN
      ,(AND (SYMBOLP VAL) (NEQ VAL T) (NEQ VAL NIL)  ;VAL is a variable
	    `(AND (NOT (ATOM ,VAL))
		  (NEQ (%AREA-NUMBER ,VAL) WORKING-STORAGE-AREA)
		  (SETQ ,VAL (SUBST NIL NIL ,VAL))))
      (PUSH `(PUTPROP ',,SYM ',,VAL ',,IND) RETURNS)
      (PUTPROP ,SYM ,VAL ,IND)))

(DEFMACRO DEFSTRUCT ((NAME . OPTIONS) . ITEMS)
    (DEFSTRUCT-1 NAME OPTIONS ITEMS))

;; Returns a list of the form (PROGN 'COMPILE ...)
(DEFUN DEFSTRUCT-1 (NAME OPTIONS ITEMS)
  (LET ((TYPE NIL)		;ARRAY, ARRAY-LEADER, or LIST.
	(GROUPED-P NIL)		;NIL or T
	(SIZE-SYMBOL NIL)
	(SIZE-MACRO NIL)
	(CONSTRUCTOR T)		;NIL for none at all, T for generated name, else name.
	(NAMED-STRUCTURE-P NIL)	;If T, NAMED option was specified
	(DEFAULT-POINTER NIL)
	(INCLUDE NIL)		;Name of DEFSTRUCT to include.

	(OFFSET 0)		;Number of reserved words at beginning.
	(TEM NIL)		;Random temporary.
	(SIZE NIL)		;Size of this DEFSTRUCT.
	(RETURNS NIL)		;The list which we return.
	(ITEM-LIST NIL)		;A list of names of access macros.
	(INIT-LIST NIL)		;The initialization for each.
	(CONSTRUCTOR-ARRAY-OP NIL)	;MAKE-ARRAY-DEFAULT parameter.
	(CONSTRUCTOR-TIMES-OP NIL)	;GROUPED-ARRAY parameter.
        (NOINIT NIL)            ;Set if initalization not allowed (types %P-LDB and FIXNUM).
	)
    (PUSH `',NAME RETURNS)
    (DO ((OPL OPTIONS (CDR OPL))
	 (OP)
	 (OPARGS)
	 (OPSOFAR NIL (CONS OP OPSOFAR)))
	((NULL OPL))
      (SETQ OP (CAR OPL))
      (COND ((EQ (TYPEP OP) 'LIST)
	     (SETQ OPARGS (CDR OP) OP (CAR OP)))
	    (T (SETQ OPARGS NIL)))
      (AND (MEMQ OP OPSOFAR)
	   (ERROR '|This optional was given more than once -- DEFSTRUCT| OPTIONS))
      (SELECTQ OP
	  ((:ARRAY :ARRAY-LEADER :LIST :%P-LDB :FIXNUM)
	   (AND TYPE (ERROR '|The type was given more than once -- DEFSTRUCT| OPTIONS))
	   (SETQ TYPE OP))
	  (:GROUPED-ARRAY
	   (SETQ CONSTRUCTOR-TIMES-OP (COND ((NULL OPARGS) 1)
					    (T (CAR OPARGS))))
	   (SETQ GROUPED-P T))
	  (:TIMES
	   (SETQ CONSTRUCTOR-TIMES-OP (FIRST OPARGS)))
	  (:MAKE-ARRAY
	   (SETQ CONSTRUCTOR-ARRAY-OP (FIRST OPARGS)))
	  (:SIZE
	   (SETQ SIZE-SYMBOL (FIRST OPARGS)))
	  (:SIZE-MACRO
	   (SETQ SIZE-MACRO (FIRST OPARGS)))
	  (:CONSTRUCTOR
	   (SETQ CONSTRUCTOR (FIRST OPARGS)))
	  (:INITIAL-OFFSET
	   (SETQ OFFSET (FIRST OPARGS)))
	  (:NAMED
	   (SETQ NAMED-STRUCTURE-P T))
	  (:DEFAULT-POINTER
	   (SETQ DEFAULT-POINTER (FIRST OPARGS)))
	  (:INCLUDE
	   (SETQ INCLUDE (FIRST OPARGS)))
	  (OTHERWISE
	   (ERROR '|Unrecognized option -- DEFSTRUCT| OP))))
    (OR TYPE (SETQ TYPE 'ARRAY))
    (COND ((MEMQ TYPE '(%P-LDB FIXNUM))
	   (OR (EQ CONSTRUCTOR T)
	       (ERROR '|DEFSTRUCT type doesn't allow a constructor| TYPE))
	   (SETQ CONSTRUCTOR NIL)
           (SETQ NOINIT T)))                    ;If can't construct, don't need inits.
    (AND (EQ CONSTRUCTOR T)
	 (SETQ CONSTRUCTOR (IMPLODE (APPEND '(M A K E -) (EXPLODE NAME)))))
;Drops through.

;Drops in.

;Done parsing the options, now do some random hacking and error checking.
    (AND (MEMQ TYPE '(:LIST :%P-LDB :FIXNUM)) NAMED-STRUCTURE-P
	 (ERROR '|This type of structure cannot be a named structure -- DEFSTRUCT| OPTIONS))

    (COND (GROUPED-P
	   (AND NAMED-STRUCTURE-P
		(ERROR '|A grouped-array cannot be a named-structure -- DEFSTRUCT| OPTIONS))
	   (OR (EQ TYPE ':ARRAY)
	       (ERROR '|A grouped-array must be an array -- DEFSTRUCT| TYPE))))

    (DEFSTRUCT-PUTPROP NAME TYPE 'DEFSTRUCT-TYPE)
    (AND NAMED-STRUCTURE-P (DEFSTRUCT-PUTPROP NAME T 'DEFSTRUCT-NAMED-P))
    (AND GROUPED-P (DEFSTRUCT-PUTPROP NAME T 'DEFSTRUCT-GROUPED-P))

    (COND (INCLUDE
	   (OR (NUMBERP (SETQ TEM (GET INCLUDE 'DEFSTRUCT-SIZE)))
	       (ERROR '|The included DEFSTRUCT has not yet been defined -- DEFSTRUCT| INCLUDE))
	   (COND
	    ((AND NAMED-STRUCTURE-P
		  (NULL (GET INCLUDE 'DEFSTRUCT-NAMED-P)))
	     (ERROR '|A named-structure may not include a non-named-structure -- DEFSTRUCT|
		    NAME))
	    ((AND (NOT NAMED-STRUCTURE-P)
		  (GET INCLUDE 'DEFSTRUCT-NAMED-P))
	     (ERROR '|A non-named-structure may not include a named-structure -- DEFSTRUCT|
		    NAME))
	    ((NEQ (GET INCLUDE 'DEFSTRUCT-TYPE) TYPE)
	     (ERROR '|INCLUDE types did not match -- DEFSTRUCT| TYPE))
	    ((GET INCLUDE 'DEFSTRUCT-GROUPED-P)
	     (ERROR '|A structure may not include a grouped-array -- DEFSTRUCT| NAME))
	    (GROUPED-P
	     (ERROR '|A grouped array may not include another structure -- DEFSTRUCT| NAME)))
	   (SETQ OFFSET (+ OFFSET TEM))))

    ;Now we have OFFSET and can get SIZE.
    (SETQ SIZE (+ OFFSET (LENGTH ITEMS)))
    (AND NAMED-STRUCTURE-P (SETQ SIZE (1+ SIZE)))	;Allocate named-structure-symbol slot
    (DEFSTRUCT-PUTPROP NAME SIZE 'DEFSTRUCT-SIZE)
    (AND SIZE-SYMBOL
	 (PUSH `(SETQ ,SIZE-SYMBOL ,SIZE) RETURNS))
    (AND SIZE-MACRO
	 (PUSH `(MACRO ,SIZE-MACRO (IGNORE) ,SIZE) RETURNS))
;Drops through.

;Drops in.

;For named-structures, create an additional item for the named-structure-symbol slot
;The kludge is that this item is called NAMED-STRUCTURE-SYMBOL, but we don't
;define a macro by that name since there is already a function by that name.
    (AND NAMED-STRUCTURE-P
	 (NOT INCLUDE)
	 (COND ((EQ TYPE ':ARRAY) (PUSH 'NAMED-STRUCTURE-SYMBOL ITEMS))
	       (T (SETQ ITEMS (CONS (CAR ITEMS)		;Array-leader element 1
				    (CONS 'NAMED-STRUCTURE-SYMBOL
					  (CDR ITEMS)))))))
;Define the accessor macros.  Also, stick the field names onto ITEM-LIST
;and their initializations (or *NOINIT*)  onto INIT-LIST, in parallel order.
    (DO ((IL ITEMS (CDR IL))
	 (WHOLE-COMPONENT-INITED NIL NIL)  ;SET THIS IF A "COMPONENT" THAT ACTUALLY REFERS
					   ; TO THE WHOLE THING HAS INITIALIZATION.
					   ;IF THIS SET, OTHER COMPONENTS THAT REALLY
					   ;DO REFER TO JUST COMPONENTS WILL GET NO 
					   ;INITIALIZATION UNLESS SPECIFICALLY SPECIFIED.
	 (ITEM)
	 ;; OFFSET is the index into the structure of the component about to be defined.
         (N OFFSET (1+ N)))
	((NULL IL)
	 (SETQ ITEM-LIST (REVERSE ITEM-LIST)
	       INIT-LIST (REVERSE INIT-LIST)))
      (SETQ ITEM (CAR IL))

      (COND ((SYMBOLP ITEM)
	     ;; Handle a word represented by just a symbol - no init specified.
             (PUSH ITEM ITEM-LIST)
	     (OR NOINIT (PUSH '*NOINIT* INIT-LIST))
	     (OR (EQ ITEM 'NAMED-STRUCTURE-SYMBOL)
		 (PUSH (DEFSTRUCT-MAKE-ACCESS-MACRO ITEM N NIL DEFAULT-POINTER
						    TYPE GROUPED-P)
		       RETURNS)))
            ;; Handle a word which looks like (FOO FOO-INIT)
	    ((SYMBOLP (CAR ITEM))
	     (PUSH (CAR ITEM) ITEM-LIST)
             (OR NOINIT
                 (PUSH (COND ((NULL (CDR ITEM)) '*NOINIT*)
                             (T (CADR ITEM)))
                       INIT-LIST))
	     (PUSH (DEFSTRUCT-MAKE-ACCESS-MACRO (CAR ITEM) N NIL DEFAULT-POINTER
						TYPE GROUPED-P)
		   RETURNS))
	    ;; Handle a word which is a list of things like (FOO 1010 FOO-INIT).
            (T
	     (DO ((L ITEM (CDR L))
		  (NAME)
		  (PPSS NIL NIL)
		  (INIT NIL NIL))
		 ((NULL L))
	       (SETQ NAME (CAAR L))
	       (COND ((CDAR L)
		      (SETQ PPSS (CADAR L))
		      (SETQ INIT (COND ((CDDAR L) (CADDAR L))
				       (PPSS (COND (WHOLE-COMPONENT-INITED '*NOINIT*)
						   (T 0)))))
		      (COND ((AND (NULL PPSS)	;THIS COMPONENT REALLY REFERS TO WHOLE THING
				  (CDDAR L))    ;AND HAS INIT.
			     (SETQ WHOLE-COMPONENT-INITED T)))))  ;DONT INIT OTHER COMPONENTS
						;UNLESS THEY SPECIFICALLY HAVE INITS
	       (PUSH NAME ITEM-LIST)
	       (OR NOINIT (PUSH INIT INIT-LIST))
	       (PUSH (DEFSTRUCT-MAKE-ACCESS-MACRO NAME N PPSS DEFAULT-POINTER
						  TYPE GROUPED-P)
		     RETURNS)
               ;; If (CAR L) is (FOO 0002 0 (FOO-A FOO-B FOO-C FOO-D)),
               ;; then we make code to define FOO-A ... FOO-D to be 0 through 3.
               (AND (CADDDR (CAR L))
                    (PUSH (DO ((VN (CADDDR (CAR L)) (CDR VN)) (I 0 (1+ I)) (RESULT))
                              ((NULL VN) (CONS 'SETQ RESULT))
                             (SETQ RESULT `(,(CAR VN) ,I . ,RESULT)))
                          RETURNS))))))

    (COND (INCLUDE
	   (SETQ ITEM-LIST (APPEND (GET INCLUDE 'DEFSTRUCT-ITEMS) ITEM-LIST))
	   (OR NOINIT (SETQ INIT-LIST (APPEND (GET INCLUDE 'DEFSTRUCT-INITS) INIT-LIST)))))

    (DEFSTRUCT-PUTPROP NAME ITEM-LIST 'DEFSTRUCT-ITEMS)
    (OR NOINIT (DEFSTRUCT-PUTPROP NAME INIT-LIST 'DEFSTRUCT-INITS))
;Drops through.

;Drops in.

;;; Creation of constructor macro.

    (AND CONSTRUCTOR
	 (PUSH `(MACRO ,CONSTRUCTOR (X)	;Create a closure...
		       (DEFSTRUCT-GRAND-CONSTRUCTOR (CDR X)
						    ',TYPE
						    ',SIZE 
						    ',GROUPED-P
						    ',(AND NAMED-STRUCTURE-P NAME)
						    ',ITEM-LIST
						    ',INIT-LIST
						    ',CONSTRUCTOR-ARRAY-OP
						    ',CONSTRUCTOR-TIMES-OP))
	       RETURNS))

    `(PROGN 'COMPILE . ,RETURNS)))

;;; Creation of access macros.

;;; Make an access macro named NAME, which references a
;;; structure of TYPE type at element INDEX.
;;; If PPSS is not NIL then it is either a number specifying a byte
;;; (which the access macro will LDB out) or it is a function or macro
;;; to call upon the value of the structure's element.
;;; If DEFAULT-POINTER is not NIL then the generated macro will use
;;; that object as the operand if no operand is specified.
;;; If GROUPED-P is T then this should be a "grouped" access macro.

(DEFUN DEFSTRUCT-MAKE-ACCESS-MACRO (NAME INDEX PPSS DEFAULT-POINTER TYPE GROUPED-P)
  (LET (ARGS BODY)
    (SETQ ARGS
	  (COND (DEFAULT-POINTER `(&OPTIONAL (Z ,DEFAULT-POINTER)))
		(T `(Z))))
    (AND GROUPED-P (SETQ ARGS (CONS 'I ARGS)
                         INDEX (COND ((ZEROP INDEX) 'I)
                                     (T `(+ ,INDEX I)))))
    (SETQ BODY (SELECTQ TYPE
		   (ARRAY `(AR-1 Z ,INDEX))
		   (ARRAY-LEADER `(ARRAY-LEADER Z ,INDEX))
		   (FIXNUM 'Z)
		   (LIST `(NTH ,INDEX Z))
		   (%P-LDB
		      (COND ((NUMBERP PPSS)
			     `(%P-LDB-OFFSET ,PPSS Z ,INDEX)
			     (SETQ PPSS NIL))
			    (T `(%P-CONTENTS-OFFSET Z ,INDEX))))))
    (COND ((NUMBERP PPSS)
	   (SETQ BODY `(LDB ,PPSS ,BODY)))
	  (PPSS (SETQ BODY `(,PPSS ,BODY))))
    `(DEFSUBST ,NAME (,@ARGS) ,BODY)))

(DEFUN DEFSTRUCT-GRAND-CONSTRUCTOR (ARGS TYPE SIZE GROUPED-P NAMED-STRUCTURE
					 ITEM-LIST INIT-LIST CONSTRUCTOR-ARRAY-OP
					 CONSTRUCTOR-TIMES-OP)
  ;; Process special arguments, and plug in user-specified initializations
  (LET ((ARGNAME NIL)
	(MAKER NIL)
	(GEN (GENSYM))
	(GEN2 (GENSYM))
	(WHOLESIZE NIL)
	(INITED NIL)
	(MAKE-ARRAY-ARG NIL))
    (SETQ INIT-LIST (APPEND INIT-LIST NIL))
    (DO ARGPAIR ARGS (CDDR ARGPAIR) (NULL ARGPAIR)
      (SETQ ARGNAME (CAR ARGPAIR))
      (SELECTQ ARGNAME
	 (MAKE-ARRAY (SETQ CONSTRUCTOR-ARRAY-OP (CADR ARGPAIR)))
	 (TIMES (SETQ CONSTRUCTOR-TIMES-OP (CADR ARGPAIR)))
	 (OTHERWISE
	  (DO ((ITEML ITEM-LIST (CDR ITEML))
	       (INITL INIT-LIST (CDR INITL)))
	      ((NULL ITEML)
	       (FERROR NIL "The keyword ~S is not known in a constructor macro." ARGNAME))
	    (COND ((EQ (CAR ITEML) ARGNAME)
		   (RPLACA INITL (CADR ARGPAIR))
		   (RETURN NIL)))))))

    (COND ((NEQ TYPE 'LIST)
	   (SETQ WHOLESIZE (COND (GROUPED-P `(* ,SIZE ,CONSTRUCTOR-TIMES-OP))
				   (T SIZE))
		   MAKE-ARRAY-ARG (COND ((EQ TYPE 'ARRAY)
					 (LIST 'NIL ''ART-Q (COND (GROUPED-P GEN2)
								  (T WHOLESIZE))
					       'NIL 'NIL 'NIL `',NAMED-STRUCTURE))
					(T (LIST 'NIL ''ART-Q '0 'NIL
						 WHOLESIZE 'NIL `',NAMED-STRUCTURE))))

	     ;; Merge the user-specified MAKE-ARRAY option with the facts
	     ;;   that we have figured out.
	     (DO ((MAA MAKE-ARRAY-ARG (CDR MAA))
		  (CAO CONSTRUCTOR-ARRAY-OP (CDR CAO))
		  (FOO (COND ((EQ TYPE 'ARRAY)
			      '(T T NIL T T T NIL))
			     (T '(T T T T NIL T NIL))) (CDR FOO)))
		 ((OR (NULL MAA) (NULL CAO)))
		 (AND (CAR FOO)
		      (RPLACA MAA (CAR CAO))))
	     (SETQ MAKER `(MAKE-ARRAY . ,MAKE-ARRAY-ARG)))
	  (T (SETQ MAKER `(MAKE-LIST DEFAULT-CONS-AREA ,SIZE))))

    ;; See whether we know the type of the array or list, and if so set up INITED
    ;; as a function to detect elements being initialized to what they already are.
    ;; Numeric arrays are already zeros, others are already NILs.
    (COND ((EQ TYPE 'ARRAY-LEADER)
	   (SETQ INITED (FUNCTION NULL)))
	  ((EQ TYPE 'LIST)
	   (SETQ INITED (FUNCTION NULL)))
	  (T
	    (LET ((FORM (CADR MAKE-ARRAY-ARG)))
	      (COND ((AND (NOT (ATOM FORM))
			  (EQ (CAR FORM) 'QUOTE))
		     (SETQ FORM (CADR FORM))))
	      (LET ((X (ASSQ FORM ARRAY-BITS-PER-ELEMENT)))
		(SETQ INITED
		      (COND ((NULL X) (FUNCTION FALSE))
			    ((NULL (CDR X)) (FUNCTION NULL))
			    (T (FUNCTION ZEROP))))))))

    ;; Generate code to make the structure and to perform the initializations
    ;; Go over the user-specified initializations and plug them into the initialization list
    ;; In the list case I would like to plug them directly into the call to LIST,
    ;; but currently not enough information is generated to do that, so we will
    ;; generate much less optimal code.  This should be fixed!! ***
    (DO ((ITEML ITEM-LIST (CDR ITEML))
	 (INITL INIT-LIST (CDR INITL))
	 (GEN1 (GENSYM))
	 (CODE NIL))	;NOTE: CODE MUST GET BUILT UP IN THE "STRAIGHT" (NOT REVERSED)
	((NULL ITEML)   ; ORDER SINCE, WITH COMPONENTS, IT CAN MATTER IN WHICH ORDER
			; THE INITIALIZING IS DONE
	 (COND (GROUPED-P
		 `(LET ((,GEN2 (* ,SIZE ,CONSTRUCTOR-TIMES-OP)))
		    (DO ((,GEN1 0 (+ ,GEN1 ,SIZE))
			 (,GEN ,MAKER))
			((= ,GEN1 ,GEN2)
			 ,GEN)
		      . ,CODE)))
	       (T
		 `(LET ((,GEN ,MAKER))
		    ,@CODE
		    ,GEN))))
      (OR (EQ (CAR INITL) '*NOINIT*)
	  (FUNCALL INITED (CAR INITL))
	  (SETQ CODE (NCONC CODE (LIST `(SETF (,(CAR ITEML)
					       ,@(AND GROUPED-P (NCONS GEN1))
					       ,GEN)
					      ,(CAR INITL)))))))))

;; Property names used herein are:
;; DEFSTRUCT-TYPE
;; DEFSTRUCT-NAMED-P
;; DEFSTRUCT-GROUPED-P
;; DEFSTRUCT-SIZE
;; DEFSTRUCT-ITEMS
;; DEFSTRUCT-INITS


;(SETF (element pntr) value)

(DEFUN SETF MACRO (X) (SETF-1 X))

(DEFUN SETF-1 (X)
    (OR (= (LENGTH X) 3)
	(ERROR "SETF called with wrong number of arguments" X)) 
    (DO ((REF (CADR X)) (VAL (CADDR X)) (FCN)) (NIL)
       (COND ((SYMBOLP REF)				;SPECIAL CASE NEEDED.
	      (RETURN (LIST 'SETQ REF VAL)))
	     ((SETQ FCN (GET (CAR REF) 'SETF))
	      (RETURN (LOCF-APPLY FCN REF T VAL)))
	     ((SETQ FCN (GET (CAR REF) 'SETF-EXPANDER))
	      (SETQ REF (LOCF-APPLY FCN REF NIL NIL)))
	     ((NOT (EQ REF (SETQ REF (MACROEXPAND-1 REF T)))))
	     (T (ERROR "No SETF property found, can't invert this reference" X)))))

;(LOCF (element pntr))
;Constructs a form which returns a locative pointer to the "referenced" element
;of the structure.
(DEFUN LOCF MACRO (X) (LOCF-1 X))

(DEFUN LOCF-1 (X)
    (OR (= (LENGTH X) 2)
	(ERROR "LOCF called with wrong number of arguments" X))
    (DO ((REF (CADR X)) (FCN)) (NIL)
       (COND ((SYMBOLP REF)			;SPECIAL CASE NEEDED.
	      (RETURN `(VALUE-CELL-LOCATION ',REF)))
	     ((SETQ FCN (GET (CAR REF) 'LOCF))
	      (RETURN (LOCF-APPLY FCN REF NIL NIL)))
	     ((SETQ FCN (GET (CAR REF) 'SETF-EXPANDER))
	      (SETQ REF (LOCF-APPLY FCN REF NIL NIL)))
	     ((NOT (EQ REF (SETQ REF (MACROEXPAND-1 REF T)))))
	     (T (ERROR "No LOCF property found, can't work." X)))))

(DEFUN LOCF-APPLY (FCN REF VAL-P VAL)
    (COND ((ATOM FCN)
	   (COND (VAL-P (FUNCALL FCN REF VAL))
		 (T (FUNCALL FCN REF))))
	  (T (DO ((PATTERN (CDAR FCN) (CDR PATTERN))
		  (REF (CDR REF) (CDR REF))
		  (SUBS
		     (AND VAL-P (LIST (CONS 'VAL VAL)))
		     (CONS (CONS (CAR PATTERN) (CAR REF)) SUBS)))
		 ((OR (NULL PATTERN) (NULL REF))
		  (AND (OR PATTERN REF)
		       (ERROR "Reference not same length as pattern - LOCF or SETF" REF))
		  (SUBLIS SUBS (CDR FCN)))))))

;(GET-LIST-POINTER-INTO-STRUCT (element pntr))

(DEFUN GET-LIST-POINTER-INTO-STRUCT MACRO (X)
  (PROG (REF)
    (SETQ REF (MACROEXPAND (CADR X) T))	;EXPAND MACROS LOOKING AT BAG-BITING MACRO LIST
    (COND ((EQ (CAR REF) 'AR-1)
	   (RETURN (LIST 'GET-LIST-POINTER-INTO-ARRAY
			 (LIST 'FUNCALL (CADR REF) (CADDR REF)))))
	  ((ERROR "LOSES - GET-LIST-POINTER-INTO-STRUCT" X)))))

;Load time defprops for SETF and LOCF.
;Value of the SETF property is either an symbol which is a function
; which is applied to two arguments: the reference and the value 
; to be stored into it, or it is CONS of a 1-level pattern to
; match against REF and a form in which substitutions
; are made for the symbol VAL and the pattern atoms.
;The value of the LOCF property is very similar; if it is
;a symbol then it is a function to be applied to one argument,
;the reference.  Otherwise it is a pattern as in SETF, except
;that the symbol VAL is not special.

;A SETF-EXPANDER property looks like a LOCF property,
;but instead of telling how to get the location of the value
;it gives another expression for the same value.
;The idea is that that expression will be amenable to SETF/LOCF.

;;; (DEFPROP AREF ((AREF ARRAY . SUBSCRIPTS)
;;;                 ASET VAL ARRAY . SUBSCRIPTS) SETF)
;;; (DEFPROP AREF ((AREF ARRAY . SUBSCRIPTS)
;;;                 ALOC ARRAY . SUBSCRIPTS) LOCF)

(DEFPROP AREF AREF-SETF SETF)
(DEFUN AREF-SETF (REF VAL)
    `(ASET ,VAL . ,(CDR REF)))
(DEFPROP AREF AREF-LOCF LOCF)
(DEFUN AREF-LOCF (REF)
    `(ALOC . ,(CDR REF)))

(DEFPROP AR-1 ((AR-1 ARRAY INDEX)
	       AS-1 VAL ARRAY INDEX) SETF)
(DEFPROP AR-1 ((AR-1 ARRAY INDEX)
	       AP-1 ARRAY INDEX) LOCF)

(DEFPROP AR-2 ((AR-2 ARRAY INDEX1 INDEX2)
	       AS-2 VAL ARRAY INDEX1 INDEX2) SETF)
(DEFPROP AR-2 ((AR-2 ARRAY INDEX1 INDEX2)
	       AP-2 ARRAY INDEX1 INDEX2) LOCF)

(DEFPROP AR-3 ((AR-3 ARRAY INDEX1 INDEX2 INDEX3)
	       AS-3 VAL ARRAY INDEX1 INDEX2 INDEX3) SETF)
(DEFPROP AR-3 ((AR-3 ARRAY INDEX1 INDEX2 INDEX3)
	       AP-3 ARRAY INDEX1 INDEX2 INDEX3) LOCF)

(DEFPROP ARRAY-LEADER ((ARRAY-LEADER ARRAY INDEX)
		       STORE-ARRAY-LEADER VAL ARRAY INDEX) SETF)
(DEFPROP ARRAY-LEADER ((ARRAY-LEADER ARRAY INDEX)
		       AP-LEADER ARRAY INDEX) LOCF)

(DEFPROP CDR ((CDR ITEM) . (RPLACD ITEM VAL)) SETF)
(DEFPROP CDR ((CDR LIST) . LIST) LOCF)

(DEFPROP CAR ((CAR LIST) . (RPLACA LIST VAL)) SETF)
(DEFPROP CAR ((CAR LIST) . (CAR-LOCATION LIST)) LOCF)

(DEFPROP CDDR ((CDDR ITEM) . (CDR (CDR ITEM))) SETF-EXPANDER)
(DEFPROP CDDDR ((CDDDR ITEM) . (CDR (CDDR ITEM))) SETF-EXPANDER)
(DEFPROP CDDDDR ((CDDDDR ITEM) . (CDR (CDDDR ITEM))) SETF-EXPANDER)
(DEFPROP CDDDAR ((CDDDAR ITEM) . (CDR (CDDAR ITEM))) SETF-EXPANDER)
(DEFPROP CDDAR ((CDDAR ITEM) . (CDR (CDAR ITEM))) SETF-EXPANDER)
(DEFPROP CDDADR ((CDDADR ITEM) . (CDR (CDADR ITEM))) SETF-EXPANDER)
(DEFPROP CDDAAR ((CDDAAR ITEM) . (CDR (CDAAR ITEM))) SETF-EXPANDER)
(DEFPROP CDAR ((CDAR ITEM) . (CDR (CAR ITEM))) SETF-EXPANDER)
(DEFPROP CDADR ((CDADR ITEM) . (CDR (CADR ITEM))) SETF-EXPANDER)
(DEFPROP CDADDR ((CDADDR ITEM) . (CDR (CADDR ITEM))) SETF-EXPANDER)
(DEFPROP CDADAR ((CDADAR ITEM) . (CDR (CADAR ITEM))) SETF-EXPANDER)
(DEFPROP CDAADR ((CDAADR ITEM) . (CDR (CAADR ITEM))) SETF-EXPANDER)
(DEFPROP CDAAAR ((CDAAAR ITEM) . (CDR (CAAAR ITEM))) SETF-EXPANDER)
(DEFPROP CADR ((CADR ITEM) . (CAR (CDR ITEM))) SETF-EXPANDER)
(DEFPROP CADDR ((CADDR ITEM) . (CAR (CDDR ITEM))) SETF-EXPANDER)
(DEFPROP CADDDR ((CADDDR ITEM) . (CAR (CDDDR ITEM))) SETF-EXPANDER)
(DEFPROP CADDAR ((CADDAR ITEM) . (CAR (CDDAR ITEM))) SETF-EXPANDER)
(DEFPROP CADAR ((CADAR ITEM) . (CAR (CDAR ITEM))) SETF-EXPANDER)
(DEFPROP CADADR ((CADADR ITEM) . (CAR (CDADR ITEM))) SETF-EXPANDER)
(DEFPROP CADAAR ((CADAAR ITEM) . (CAR (CDAAR ITEM))) SETF-EXPANDER)
(DEFPROP CAAR ((CAAR ITEM) . (CAR (CAR ITEM))) SETF-EXPANDER)
(DEFPROP CAADR ((CAADR ITEM) . (CAR (CADR ITEM))) SETF-EXPANDER)
(DEFPROP CAADDR ((CAADDR ITEM) . (CAR (CADDR ITEM))) SETF-EXPANDER)
(DEFPROP CAADAR ((CAADAR ITEM) . (CAR (CADAR ITEM))) SETF-EXPANDER)
(DEFPROP CAAADR ((CAAADR ITEM) . (CAR (CAADR ITEM))) SETF-EXPANDER)
(DEFPROP CAAAAR ((CAAAAR ITEM) . (CAR (CAAAR ITEM))) SETF-EXPANDER)
(DEFPROP NTH ((NTH N LIST) . (CAR (NTHCDR N LIST))) SETF-EXPANDER)

(DEFPROP FSYMEVAL ((FSYMEVAL SYMBOL) . (FSET SYMBOL VAL)) SETF)
(DEFPROP FSYMEVAL ((FSYMEVAL SYMBOL) . (FUNCTION-CELL-LOCATION SYMBOL)) LOCF)

(DEFPROP SYMEVAL ((SYMEVAL SYMBOL) . (SET SYMBOL VAL)) SETF)
(DEFPROP SYMEVAL ((SYMEVAL SYMBOL) . (VALUE-CELL-LOCATION SYMBOL)) LOCF)

(DEFPROP SYMEVAL-IN-CLOSURE ((SYMEVAL-IN-CLOSURE CLOSURE PTR)
			     SET-IN-CLOSURE CLOSURE PTR VAL) SETF)
(DEFPROP SYMEVAL-IN-CLOSURE ((SYMEVAL-IN-CLOSURE CLOSURE PTR)
			     LOCATE-IN-CLOSURE CLOSURE PTR) LOCF)

;;; This really should be called SEND or something like that
(DEFPROP FUNCALL FUNCALL-SETF SETF)
(DEFUN FUNCALL-SETF (REF VAL)
  (OR (AND (= (LENGTH REF) 3)
	   (NOT (ATOM (CADDR REF))) (EQ (CAADDR REF) 'QUOTE))
      (ERROR "Can only setf message sending funcalls" REF))
  `(FUNCALL ,(CADR REF) ',(INTERN (STRING-APPEND "SET-" (CADR (CADDR REF))) "") ,VAL))

(defprop function function-setf setf)
(defun function-setf (ref val)
       (or (symbolp (cadr ref))
	   (error "Cannot setf this." ref))
       `(fset ',(cadr ref) ,val))

(defprop function function-locf locf)
(defun function-locf (ref)
       (or (symbolp (cadr ref))
	   (error "Cannot locf this." ref))
       `(function-cell-location ',(cadr ref)))

(defprop plist ((plist foo) . (setplist foo val)) setf)
(defprop plist ((plist foo) . (property-cell-location foo)) locf)

;The old thing.  Also evals ref twice, lose lose.
(DEFPROP LDB ((LDB PPSS REF) . (SETF REF (DPB VAL PPSS REF))) SETF)
;The following tried to fix a hairy bug associated with (setf (ldb (cdr x)) 105).
; Unfortunately, it suffers from a worse problem, namely, the ref can be a
; array element of a numeric array, in which case it is illegal (and impossible)
; to make a locative pointer.
;(DEFPROP LDB ((LDB PPSS REF) . (DPB-VIA-LOCATIVE VAL PPSS (LOCF REF))) SETF)
;(IF-FOR-LISPM
;(DEFUN DPB-VIA-LOCATIVE (VAL PPSS LOCATIVE)  ;THIS MUST BE IN QRAND BECAUSE IT MUST BE
;    (RPLACD LOCATIVE (DPB VAL PPSS (CDR LOCATIVE)))))  ;IN THE COLD LOAD

(DEFPROP GET ((GET ATOM PROP) . (PUTPROP ATOM VAL PROP)) SETF)
(DEFPROP GET ((GET ATOM PROP) . (GET-LOCATION ATOM PROP)) LOCF)

(DEFPROP ARG ((ARG N) . (SETARG N VAL)) SETF)

(DEFPROP %UNIBUS-READ ((%UNIBUS-READ ADDR) . (%UNIBUS-WRITE ADDR VAL)) SETF)
(DEFPROP %XBUS-READ ((%XBUS-READ ADDR) . (%XBUS-WRITE ADDR VAL)) SETF)

(DEFPROP %P-CONTENTS-OFFSET ((%P-CONTENTS-OFFSET BASE OFFSET)
			     %P-STORE-CONTENTS-OFFSET VAL BASE OFFSET) SETF)
(DEFPROP %P-CONTENTS-OFFSET ((%P-CONTENTS-OFFSET POINTER OFFSET)
			     %MAKE-POINTER-OFFSET DTP-LOCATIVE POINTER OFFSET) LOCF)

(DEFPROP %P-LDB ((%P-LDB PPSS POINTER)
		 %P-DPB VAL PPSS POINTER) SETF)

(DEFPROP %P-LDB-OFFSET ((%P-LDB-OFFSET PPSS POINTER OFFSET)
			%P-DPB-OFFSET VAL PPSS POINTER OFFSET) SETF)

(DEFPROP %P-MASK-FIELD ((%P-MASK-FIELD PPSS POINTER)
			%P-DEPOSIT-FIELD VAL PPSS POINTER) SETF)

(DEFPROP %P-MASK-FIELD-OFFSET ((%P-MASK-FIELD-OFFSET PPSS POINTER OFFSET)
			       %P-DEPOSIT-FIELD-OFFSET VAL PPSS POINTER OFFSET) SETF)

(DEFPROP %P-POINTER ((%P-POINTER POINTER)
		     %P-STORE-POINTER POINTER VAL) SETF)

(DEFPROP %P-DATA-TYPE ((%P-DATA-TYPE POINTER)
		       %P-STORE-DATA-TYPE POINTER VAL) SETF)

(DEFPROP %P-CDR-CODE ((%P-CDR-CODE POINTER)
		      %P-STORE-CDR-CODE POINTER VAL) SETF)

(DEFPROP %P-FLAG-BIT ((%P-FLAG-BIT POINTER)
		      %P-STORE-FLAG-BIT POINTER VAL) SETF)

;Handle SETF of backquote expressions, for decomposition.
;For example, (SETF `(A ,B (D ,XYZ)) FOO)
;sets B to the CADR and XYZ to the CADADDR of FOO.
;The constants in the pattern are ignored.

;Backquotes which use ,@ or ,. other than at the end of a list
;expand into APPENDs or NCONCs and cannot be SETF'd.

(COMMENT
;This was used for making (setf `(a ,b) foo) return t if
;foo matched the pattern (had A as its car).
;The other change for reinstalling this
;would be to replace the PROGNs with ANDs
;in the expansions produced by (LIST SETF), etc.
(DEFUN SETF-MATCH (PATTERN OBJECT)
  (COND ((NULL PATTERN) T)
	((SYMBOLP PATTERN)
	 `(PROGN (SETQ ,PATTERN ,OBJECT) T))
	((EQ (CAR PATTERN) 'QUOTE)
	 `(EQUAL ,PATTERN ,OBJECT))
	((MEMQ (CAR PATTERN)
	       '(CONS LIST LIST*))
	 `(SETF ,PATTERN ,OBJECT))
	(T `(PROGN (SETF ,PATTERN ,OBJECT) T)))))

;This is used for ignoring any constants in the
;decomposition pattern, so that (setf `(a ,b) foo)
;always sets b and ignores a.
(DEFUN SETF-MATCH (PATTERN OBJECT)
  (COND ((AND (NOT (ATOM PATTERN)) (EQ (CAR PATTERN) 'QUOTE))
	 NIL)
	(T `(SETF ,PATTERN ,OBJECT))))

(DEFUN (LIST SETF) (PATTERN VALUE-FORM &AUX VARIABLE)
  (COND ((SYMBOLP VALUE-FORM)
	 (SETQ VARIABLE VALUE-FORM
	       VALUE-FORM NIL))
	(T (SETQ VARIABLE (GENSYM))))
  (DO ((I 0 (1+ I))
       (ACCUM)
       (ARGS (CDR PATTERN) (CDR ARGS)))
      ((NULL ARGS)
       (COND (VALUE-FORM
	       `(LET ((,VARIABLE ,VALUE-FORM))
		  (PROGN . ,(NREVERSE ACCUM))))
	     (T (CONS 'PROGN (NREVERSE ACCUM)))))
    (PUSH (SETF-MATCH (CAR ARGS) `(NTH ,I ,VARIABLE)) ACCUM)))

(DEFUN (LIST* SETF) (PATTERN VALUE-FORM &AUX VARIABLE)
  (COND ((SYMBOLP VALUE-FORM)
	 (SETQ VARIABLE VALUE-FORM
	       VALUE-FORM NIL))
	(T (SETQ VARIABLE (GENSYM))))
  (DO ((I 0 (1+ I))
       (ACCUM)
       (ARGS (CDR PATTERN) (CDR ARGS)))
      ((NULL ARGS)
       (COND (VALUE-FORM
	       `(LET ((,VARIABLE ,VALUE-FORM))
		  (PROGN . ,(NREVERSE ACCUM))))
	     (T (CONS 'PROGN (NREVERSE ACCUM)))))
    (COND ((CDR ARGS)
	   (PUSH (SETF-MATCH (CAR ARGS) `(NTH ,I ,VARIABLE)) ACCUM))
	  (T (PUSH (SETF-MATCH (CAR ARGS) `(NTHCDR ,I ,VARIABLE)) ACCUM)))))

(DEFUN (CONS SETF) (PATTERN VALUE-FORM &AUX VARIABLE)
  (COND ((SYMBOLP VALUE-FORM)
	 (SETQ VARIABLE VALUE-FORM
	       VALUE-FORM NIL))
	(T (SETQ VARIABLE (GENSYM))))
  (LET ((TEM `(PROGN ,(SETF-MATCH (CADR PATTERN) `(CAR ,VARIABLE))
		     ,(SETF-MATCH (CADDR PATTERN) `(CDR ,VARIABLE)))))
    (COND (VALUE-FORM
	    `(LET ((,VARIABLE ,VALUE-FORM))
	       ,TEM))
	  (T TEM))))
