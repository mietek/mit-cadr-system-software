; -*- LISP -*-
; Package definition for assembling microcode on the real machine

; In a separate package since it wants its own copy of the stuff in QCOM
; since it may not be the same as in the running machine.  Note that this
; should not ever look at anything such as areas or system constants
; in the running machine, it should always have its own copy so you can
; change them and reassemble the microcode for the new incompatible world.
;
; As it stands, this doesn't win 100%, it should really shadow anything
; in QCOM that's global.  Otherwise, if you change those it will clobber
; the copy used by the running machine.

; Note, you need explicit QFASL's in the file names due to at least CMANY-QFASL-P

(PACKAGE-DECLARE MICRO-ASSEMBLER GLOBAL 5000.  ;actually uses about 3028.
   (("LISPM;COMPAT QFASL" DEFS)	;Needed to compile, I guess also to run
    ;Order of these files may be important, I'm not sure...
    ("LISPM;CADRLP QFASL")	;Assembler itself
     ;Load QCOM after CADRLP because it needs LOGDPB
    ("LISPM;QCOM >")		;System constant definitions
    ("LISPM;CDMP QFASL")	;Writing out the results
    ("LCADR;QWMCR QFASL")	;Writing out the results also
    ("LMIO;FREAD QFASL")	;Fast reader
    ("LISPM;DEFMIC >")		;Microcode entry definitions
    ("LISPM;CADSYM >")		;Machine definitions
    ("LISPM2;USYMLD QFASL"))	;Stuff to augment microcode of running machine, etc.
   (SHADOW FIXNUM INCLUDE)	;COMPAT redefines these as functions
   (SHADOW AREA-LIST)		;Things which QCOM would bash nicely
   (MYREFNAME GLOBAL UA))

