;;; -*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Keywords used by Lisp machine system functions.
;;; For now, these are in the system package as well as the keywords package.

;;; EACH SYMBOL NAME MUST BE FOLLOWED BY A SPACE!!!

;; On general principles:

 NIL
 T
 LAMBDA
 MACRO
 EXPR
 FEXPR

;; For OPEN:

 IN
 OUT
 READ
 WRITE
 BINARY
 BLOCK 

;; For I/O streams

 WHICH-OPERATIONS
 TYI
 TYO
 UNTYI
 UNTYO
 LISTEN
 UNTYO-MARK
 LINE-IN
 LINE-OUT
 STRING-OUT
 RUBOUT-HANDLER
 FINISH
 CLOSE
 CLEAR-INPUT
 CLEAR-OUTPUT
 READ-POINTER
 NAME
 FORCE-OUTPUT
 FRESH-LINE
 PC-PPR
 SET-PC-PPR

;; For TV-DEFINE-PC-PPR and TV-REDEFINE-PC-PPR

 FONTS
 FONT-MAP
 TOP
 BOTTOM
 LEFT
 RIGHT
 BLINKER-P
 REVERSE-VIDEO-P
 MORE-P
 VSP
 LEFT-MARGIN
 RIGHT-MARGIN
 BOTTOM-MARGIN
 TOP-MARGIN
 END-LINE-FCN
 END-SCREEN-FCN
 MORE-FCN
 BLINK-FCN
 SIDEWAYS-P
 PLANE-MASK
 ACTIVATE-P
 INTEGRAL-P

;; For TV-DEFINE-BLINKER and TV-BLINKER-VISIBILITY:

 HEIGHT
 ARG2
 WIDTH
 ARG1
 FUNCTION
 VISIBILITY
 FOLLOW-P
 ROVING-P
 ACTIVATE-P
 HALF-PERIOD
 PLANE-MASK
 SIDEWAYS-P
 BLINK
 NORMAL

;; For TV-WHITE-ON-BLACK-STATE
 BLACK
 BOTH
 WHITE

;; For TRACE:  no longer needed, since TRACE has colons.

;; For STATUS/SSTATUS.  Many of these are on SYSPKG anyway.

 SORT
 FASLOAD
 STRINGS
 NEWIO
 ROMAN
 TRACE
 GRINDEF
 GRIND
 LISPM

 FEATURE
 FEATURES 
 NOFEATURE
 STATUS
 SSTATUS

;; For PKG-LOAD:

 CONFIRM
 NOCONFIRM
 RELOAD
 NORELOAD
 SELECTIVE
 COMPLETE
 COMPILE
 NOCOMPILE
 LOAD
 NOLOAD
 DEFS
 RECURSIVE
 REFERENCES

;; For EVAL-WHEN (these are all system functions anyway, but might as well mention them):

 EVAL
 COMPILE
 LOAD

;; For WHO-CALLS

 UNBOUND-FUNCTION

;; For DEFSTRUCT  (I don't think these are really necessary --Moon)

 ARRAY
 ARRAY-LEADER
 LIST
 CONSTRUCTOR
 GROUPED-ARRAY
 TIMES
 MAKE-ARRAY
 SIZE
 SIZE-MACRO
 DEFAULT-POINTER
 INCLUDE
 
;; For MAKE-STACK-GROUP

 SG-AREA
 REGULAR-PDL-AREA
 SPECIAL-PDL-AREA
 REGULAR-PDL-SIZE
 SPECIAL-PDL-SIZE
 CAR-SYM-MODE
 CAR-NUM-MODE
 CDR-SYM-MODE
 CDR-NUM-MODE
 SWAP-SV-ON-CALL-OUT
 SWAP-SV-OF-SG-THAT-CALLS-ME
 TRAP-ENABLE
 SAFE

;; For SCREEN, so we don't conflict with the SELECT macro (temporary!)

 SELECT

;; For ITER

 STACK
 STACK-GLOBAL
 MAPC
 STEP
 STEP-GLOBAL
 LIST-OUT
 LIST-OUT-ORDERED
 LOCAL
 ESCAPE

 ;; For the compiler
 ARGS
 ALL

;; These have to be global because MAKE-AREA has to be in QFCTNS.  MAKE-AREA
;; has to be loaded before packages exist, because packages use areas.  If these
;; aren't global, the keywords in the compiled code end up in SI instead of USER.

 NAME SIZE REGION-SIZE REPRESENTATION GC READ-ONLY PDL COMPACT-CONS
 STATIC DYNAMIC LIST STRUCTURE

;; This is here due to duplicate-symbol problems in the cold-load due to not
;; having packages.  This is a keyword argument to SETSYNTAX and also an SI: symbol
;; in the RDTBL-PLIST

 SINGLE

;; This is here because of problems with #\FORM vs FORM in some random cold-load file

 FORM

;; EOF.
 