;;;-*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Package declarations for the Lisp machine system packages.

;;; Any package in here which is not under GLOBAL may need to be
;;; in the magic list at the front of PACK4, if a cold-load file
;;; refers to a symbol in that package.

(PACKAGE-DECLARE SYSTEM-INTERNALS SYSTEM 7000.
	 (("AI: LISPM2; DEFMAC QFASL" DEFS)
	  ("AI: LISPM2; LMMAC QFASL" DEFS)
	  ("AI: LISPM2; STRING QFASL")
	  ("AI: LISPM2; NSTRUC QFASL" DEFS)
	  ("AI: LISPM; QMISC QFASL")
;;; These files are in the cold-load
	  ("AI: LISPM; LFL QFASL")
	  ("AI: LISPM; LTOP QFASL")
	  ("AI: LMWIN; COLD QFASL")
	  ("AI: LMIO; PRINT QFASL")
	  ("AI: LISPM; QEV QFASL")
	  ("AI: LISPM; QFASL QFASL")
	  ("AI: LMIO; MINI QFASL")
	  ("AI: LMIO; QIO QFASL")
	  ("AI: LISPM; QRAND QFASL")
	  ("AI: LMIO; RDTBL QFASL")
	  ("AI: LMIO; READ QFASL")
	  ("AI: LISPM; SGFCTN QFASL")
;;; Please try to keep this in order by FN1.
	  ("AI: LISPM2; BAND QFASL")
	  ("AI: LISPM2; DEFSEL QFASL")
	  ("AI: LMIO; DISK QFASL")
	  ("AI: LMIO; DLEDIT QFASL")
	  ("AI: LMIO; DRIBBL QFASL")
	  ("AI: LISPM2; FLAVOR QFASL")
	  ("AI: LISPM2; GC QFASL")
          ("AI: LMIO; GRIND QFASL")
	  ("AI: LISPM2; ITER QFASL")
	  ("AI: LISPM2; LOGIN QFASL")
	  ("AI: LISPM2; METH QFASL")
	  ("AI: LISPM2; CLASS QFASL")		;Cannot be loaded before METH.
	  ("AI: LISPM2; NUMER QFASL")
	  ("AI: LISPM; PACK4 QFASL")
	  ("AI: LISPM2; PLANE QFASL")
	  ("AI: LMWIN; PRODEF QFASL" DEFS)
	  ("AI: LMWIN; PROCES QFASL")
	  ("AI: LISPM; QFCTNS QFASL")
	  ("AI: LISPM2; QTRACE QFASL")
	  ("AI: LMIO; RDDEFS QFASL" DEFS)
	  ("AI: LISPM2; SGDEFS QFASL" DEFS)
	  ("AI: LISPM2; SELEV QFASL")
	  ("AI: LMIO1; SERIAL QFASL")
	  ("AI: LISPM; SORT QFASL")
	  ("AI: LISPM2; STEP QFASL")
	  ("AI: LMIO; UNIBUS QFASL")
	  ("AI: LMIO1; XGP QFASL")))

(PACKAGE-DECLARE FONTS GLOBAL 100
	(("AI: LMFONT; 43VXMS QFASL")
	 ("AI: LMFONT; 5X5 QFASL")
	 ("AI: LMFONT; BIGFNT QFASL")
	 ("AI: LMFONT; HL10 QFASL")
	 ("AI: LMFONT; HL10B QFASL")
	 ("AI: LMFONT; HL12B QFASL")
	 ("AI: LMFONT; HL12I QFASL")
	 ("AI: LMFONT; HL12BI QFASL")
	 ("AI: LMFONT; HL6 QFASL")
	 ("AI: LMFONT; HL7 QFASL")
	 ("AI: LMFONT; MEDFNT QFASL")
	 ("AI: LMFONT; MEDFNB QFASL")
	 ("AI: LMFONT; METS QFASL")
	 ("AI: LMFONT; METSI QFASL")
	 ("AI: LMFONT; MOUSE QFASL")
	 ("AI: LMFONT; SEARCH QFASL")
	 ("AI: LMFONT; TR10I QFASL")
	 ("AI: LMFONT; TR8 QFASL")
	 ("AI: LMFONT; TR8B QFASL")
	 ("AI: LMFONT; TR8I QFASL")))

(PACKAGE-DECLARE CHAOS SYSTEM 2000
	(("AI: LMIO1; CHATST QFASL")	;Its hard to get this when you need it!
	 ("AI: LMIO; CHSNCP QFASL")
	 ("AI: LMIO1; EFTP QFASL")
	 ("AI: LMIO; CHSTBL >")
	 ("AI: LMIO; CHSAUX QFASL"))
	(EXTERNAL HOSTAT QSEND PRINT-SENDS) ;Someone wants this to be GLOBAL
	(SHADOW SEND OPEN STATUS CLOSE LISTEN FINISH)
	(MYREFNAME GLOBAL CHAOS))

(PACKAGE-DECLARE TIME GLOBAL 400
	(("AI: LMIO1; TIME QFASL")))

(PACKAGE-DECLARE SUPDUP GLOBAL 400
	(("AI: LMWIN; SUPDUP QFASL"))
	(EXTERNAL SUPDUP TELNET))

(PACKAGE-DECLARE PRESS GLOBAL 400
	(("AI: LMIO1; PRESS QFASL")
	 ("AI: LMIO1; RFONTW QFASL")))

(PACKAGE-DECLARE FORMAT GLOBAL 150.
	 (("AI: LMIO; FORMAT QFASL"))
	 (EXTERNAL FORMAT))

;Loading and dumping relocatable QFASL files.
(PACKAGE-DECLARE QFASL-REL SYSTEM 300.
  (("AI: LMIO1; RELLD QFASL")
   ("AI: LMIO1; RELDMP QFASL"))
  (MYREFNAME GLOBAL QFASL-REL))

(PACKAGE-DECLARE COMPILER SYSTEM 6000
	 ( ;PLEASE LOAD THIS AS SOON AS POSSIBLE, FOR EH'S SAKE
	  ("AI: LISPM2; DISASS QFASL")
	   ;THIS MUST BE BEFORE QCFILE, TO SET UP FASD-TEMPORARY-AREA
	  ("AI: LISPM; MADEFS QFASL" DEFS)
	  ("AI: LISPM; MA QFASL")		;"assembler" for microcompiler
	  ("AI: LISPM; MAOPT QFASL")		;Optimizer for above.
	  ("AI: LISPM; MC QFASL")		;microcompiler
	  ("AI: LISPM; MLAP QFASL")		;loader, sort of, for microcompiler
	  ("AI: LISPM2; QFASD QFASL")
	  ("AI: LISPM; QCDEFS QFASL" DEFS)
	  ("AI: LISPM2; QCFILE QFASL")
	  ("AI: LISPM; QCP1 QFASL")
	  ("AI: LISPM; QCP2 QFASL")
	  ("AI: LISPM2; PEEP QFASL")
	  ("AI: LISPM; QLF QFASL")
	  ("AI: LISPM2; CMANY QFASL")
;	  ("AI: LISPM; LFL QFASL") ;ACTUALLY, IS IN COLD LOAD therefore in SI like it or not
	  ("AI: LISPM; DEFMIC >") ;MUST BE AFTER QCFILE
	  ("AI: LISPM; DOCMIC >"))
	 (EXTERNAL COMPILE LOAD FASLOAD COMPILE-FILE-ALIST COMPILE-FILE-ALIST-MAP
		   LOAD-FILE-ALIST QC-FILE
		   QC-FILE-LOAD DISASSEMBLE UNCOMPILE FASD-UPDATE-FILE SPECIAL
		   UNSPECIAL DEFUN-COMPATIBILITY UNBIND ;SHADOW THIS? NO, it's a machine insn!
		   BUTLAST LDIFF FIRSTN  ;In QMOD since QMOD needs them in Maclisp.
		   FILE-SYMBOL-SET-CURRENT Y-OR-N-P QLD RECOMPILE-WORLD ;In LFL.
		   SET-FILE-LOADED-ID GET-FILE-LOADED-ID
		   ;-- ZILLIONS OF FASL FUNCTIONS --
		   FASL-APPEND GET-FILE-DATE *LEXPR *EXPR *FEXPR))

(PACKAGE-DECLARE COLOR SYSTEM 1000
  (("AI: LMWIN; COLOR QFASL"))
  (MYREFNAME GLOBAL COLOR))

(PACKAGE-DECLARE ZWEI GLOBAL 10000
	(("AI: NZWEI; DEFS >" DEFS)		;Structure definitions and declarations.
	 ("AI: NZWEI; MACROS QFASL" DEFS)	;Lisp macros used in the ZWEIs source.

	 ("AI: NZWEI; COMTAB QFASL")		;Functions regarding comtabs and command loop.
	 ("AI: NZWEI; DISPLA QFASL")		;Redisplay, and screen-related functions.
	 ("AI: NZWEI; FOR QFASL")		;Forward-this, forward-that functions.
         ("AI: NZWEI; INDENT QFASL")		;Indention functions
	 ("AI: NZWEI; INSERT QFASL")		;Insertion and deletion, and related functions
	 ("AI: NZWEI; PRIMIT QFASL")		;Random primitives and utilities.
	 ("AI: NZWEI; FONT QFASL")		;Font hacking stuff
	 ("AI: NZWEI; KBDMAC QFASL")		;Keyboard macro stream
	 ("AI: NZWEI; SEARCH QFASL")		;Searching functions

	 ("AI: NZWEI; COMA QFASL")		;Vanilla commands.
	 ("AI: NZWEI; COMB QFASL")		;More vanilla commands.
	 ("AI: NZWEI; COMC QFASL")		;Yet more vanilla commands.
	 ("AI: NZWEI; COMD QFASL")		;Still more vanilla commands.
	 ("AI: NZWEI; COME QFASL")		;Even more vanilla commands.
	 ("AI: NZWEI; COMF QFASL")		;More and more vanilla commands
	 ("AI: NZWEI; COMS QFASL")		;Searching and replacing commands.
	 ("AI: NZWEI; DIRED QFASL")		;Directory editor.
	 ("AI: NZWEI; DOC QFASL")		;Self-documentation commands and functions.
         ("AI: NZWEI; FASUPD QFASL")		;Update fasl file from core.
	 ("AI: NZWEI; FILES QFASL")		;File commands and utilities.
	 ("AI: NZWEI; LPARSE QFASL")		;Parsing lisp code.
	 ("AI: NZWEI; MODES QFASL")		;Major and minor mode functions and commands
	 ("AI: NZWEI; MOUSE QFASL")		;Mouse commands less screen interface
	 ("AI: NZWEI; PL1MOD QFASL")		;PL/I mode commands.
	 ("AI: NZWEI; SCREEN QFASL")		;Interface to screen system
	 ("AI: NZWEI; STREAM QFASL")		;Editor stream

	 ("AI: NZWEI; SECTIO QFASL")		;Some section specific command for ZMACS
	 ("AI: NZWEI; ZMACS QFASL")		;Multiple-buffer and file commands.

	 ("AI: NZWEI; ZYMURG QFASL")		;Last file loaded, flavors and initializations
	 )
	(EXTERNAL ED ZED ZDT MAIL BUG DIRED COMPLETING-READ)
	(MYREFNAME GLOBAL NZWEI))

;This is a kludge so that symbols don't type out with a prefix of NZWEI:
;The refname is needed because it appears here and there in files.
SI:(RPLACD (LAST (PKG-REFNAME-ALIST PKG-GLOBAL-PACKAGE))
	   (NCONS (LIST "ZWEI" (PKG-FIND-PACKAGE "ZWEI"))))

;; This is the package FED for font handling functions.
;; The package FONTS is used for font names only.

(PACKAGE-DECLARE FED SYSTEM 1000.
         (("AI: LMIO1; FNTDEF >" DEFS)
	  ("AI: LMWIN; FED QFASL")
	  ("AI: LMIO1; FNTCNV QFASL"))
         (EXTERNAL FED)
	 (MYREFNAME GLOBAL FED))

(PACKAGE-DECLARE CADR SYSTEM 1000
   (("AI: LISPM; COMPAT QFASL" DEFS)
    ("AI: LCADR; LQFMAC QFASL" DEFS)
    ("AI: LCADR; CADLDB QFASL" DEFS)
    ("AI: LCADR; LCADMC QFASL" DEFS)
    ("AI: LMCONS; CC QFASL")
    ("AI: LMCONS; CCGSYL QFASL")
    ("AI: LMCONS; CADREG >")
    ("AI: LCADR; LCADRD QFASL")
;;;
;   ("AI: LMCONS; DPDIAG QFASL")
;   ("AI: LMCONS; ADRTST QFASL")
;   ("AI: LMCONS; SHFTST QFASL")
;   ("AI: LMCONS; DSPTST QFASL")
;;;THE ABOVE FOUR ARE SUPERCEEDED BY DIAGS
    ("AI: LCADR; DIAGS QFASL")
    ("AI: LCADR; DMON QFASL")
;;;
    ("AI: LCADR; LDBG QFASL")
    ("AI: LMCONS; ZERO QFASL")
    ("AI: LMCONS; CADLD QFASL")
    ("AI: LCADR; QF QFASL")
    ("AI: LCADR; CCDISK QFASL")
    ("AI: LMCONS; DCHECK QFASL")
    ("AI: LCADR; PACKED QFASL")
    ("AI: LCADR; CHPLOC QFASL"))
   (MYREFNAME GLOBAL CADR)
   (MYREFNAME GLOBAL CC)
   (EXTERNAL CC CC-LOAD-UCODE-SYMBOLS)
   (EXTERNAL FIXNUM INCLUDE))   ;CADR (COMPAT) DEFINES THESE, BUT THE SYMBOLS ARE ON GLOBAL
				; FOR OTHER REASONS.  THE EXTERNAL AVOIDS A SPURIOUS ERROR
				; MESSAGE FROM FSET-CAREFULLY.

; Package definition for microcode assembler.
; In a separate package since it wants its own copy of the stuff in QCOM
; since it may not be the same as in the running machine.  Note that this
; should not ever look at anything such as areas or system constants
; in the running machine, it should always have its own copy so you can
; change them and reassemble the microcode for the new incompatible world.
;
; As it stands, this doesn't win 100%, it should really shadow anything
; in QCOM that's global.  Otherwise, if you change those it will clobber
; the copy used by the running machine.

(PACKAGE-DECLARE MICRO-ASSEMBLER GLOBAL 5000.  ;actually uses about 3028.
   (("AI: LISPM; COMPAT QFASL" DEFS)	;Needed to compile, I guess also to run
    ;Order of these files may be important, I'm not sure...
    ("AI: LISPM; CADRLP QFASL")	;Assembler itself
     ;Load QCOM after CADRLP because it needs LOGDPB
    ("AI: LISPM; QCOM >")		;System constant definitions
    ("AI: LISPM; CDMP QFASL")		;Writing out the results
    ("AI: LCADR; QWMCR QFASL")		;Writing out the results also
    ("AI: LMIO; FREAD QFASL")		;Fast reader
    ("AI: LISPM; DEFMIC >")		;Microcode entry definitions
    ("AI: LISPM; CADSYM >")		;Machine definitions
    ("AI: LISPM2; USYMLD QFASL"))	;Stuff to augment microcode of running machine, etc.
   (SHADOW FIXNUM INCLUDE)	;COMPAT redefines these as functions
   (SHADOW AREA-LIST)		;Things which QCOM would bash nicely
   (MYREFNAME GLOBAL UA))

(PACKAGE-DECLARE EH SYSTEM 1000.
	(("AI: LMWIN; EH QFASL")
	 ("AI: LMWIN; EHR QFASL")
	 ("AI: LMWIN; EHC QFASL")
	 ("AI: LMWIN; EHW QFASL"))
	(EXTERNAL FERROR CERROR ERROR CONDITION-BIND EH SYMEVAL-IN-STACK-GROUP ENABLE-TRAPPING
		  TRAPPING-ENABLED-P SIGNAL LISP-ERROR-HANDLER)
	(MYREFNAME GLOBAL EH))

(PACKAGE-DECLARE TV SYSTEM 5000.
   (("AI: LMWIN; TVDEFS QFASL" DEFS)
    ("AI: LMWIN; SCRMAN QFASL")
    ("AI: LMWIN; SHEET QFASL")
    ("AI: LMWIN; SHWARM QFASL")
    ("AI: LMWIN; BASWIN QFASL")
    ("AI: LMWIN; MOUSE QFASL")
    ("AI: LMWIN; BASSTR QFASL")
    ("AI: LMWIN; STREAM QFASL")
    ("AI: LMWIN; MENU QFASL")
    ("AI: LMWIN; COMETH QFASL")
    ;; The above must be loaded before any windows get created
    ("AI: LMWIN; SYSMEN QFASL")
    ("AI: LMWIN; SCRED QFASL")
    ("AI: LMWIN; TYPWIN QFASL")
    ("AI: LMWIN; SCROLL QFASL")
    ("AI: LMWIN; TSCROL QFASL")
    ("AI: LMWIN; CHOICE QFASL")
    ("AI: LMWIN; PEEK QFASL")
    ("AI: LMWIN; FRAME QFASL")
    ("AI: LMWIN; CSRPOS QFASL")
    ("AI: LMWIN; INSPCT QFASL"))
   (MYREFNAME GLOBAL TV)
   (BORROW SYSTEM-INTERNALS SI:COLD-LOAD-STREAM SI:KBD-CONVERT-TO-SOFTWARE-CHAR
	   SI:KBD-GET-HARDWARE-CHAR SI:KBD-HARDWARE-CHAR-AVAILABLE)
   (EXTERNAL DEFRESOURCE WITH-RESOURCE ALLOCATE-RESOURCE DEALLOCATE-RESOURCE BEEP
	     FONT FONT-FILL-POINTER FONT-NAME FONT-CHAR-HEIGHT FONT-CHAR-WIDTH
	     FONT-RASTER-HEIGHT FONT-RASTER-WIDTH FONT-RASTERS-PER-WORD FONT-WORDS-PER-CHAR 
	     FONT-BASELINE FONT-CHAR-WIDTH-TABLE FONT-LEFT-KERN-TABLE FONT-INDEXING-TABLE
	     FONT-NEXT-PLANE FONT-BLINKER-WIDTH FONT-BLINKER-HEIGHT FONT-CHARS-EXIST-TABLE
	     KBD-TYI KBD-TYI-NO-HANG KBD-CHAR-AVAILABLE
	     PEEK INSPECT CURSORPOS
	     DEFINE-USER-OPTION-ALIST DEFINE-USER-OPTION RESET-USER-OPTIONS
	     CHOOSE-USER-OPTIONS WRITE-USER-OPTIONS))

(PACKAGE-DECLARE FILE-SYSTEM SYSTEM 300.
  (("AI: LMIO; QFILE QFASL")
   ("AI: LMIO; FNUTIL QFASL"))
  (MYREFNAME GLOBAL FILE-SYSTEM)
  (MYREFNAME GLOBAL FS)
  (BORROW SYSTEM-INTERNALS
	  ;; MINI
	  SI:GET-FILE-SYMBOLS SI:FILE-PARSE-NAME SI:FILE-SPREAD-ITS-PATHNAME
	  ;; STRING
	  SI:NULL-S)
  (EXTERNAL OPEN CLOSE RENAMEF DELETEF PROBEF FILE-QFASL-P FILE-EXISTS-P FILE-ERROR-STATUS
	    READFILE FILE-EXPAND-PATHNAME
	    SI:FILE-PARSE-NAME))
