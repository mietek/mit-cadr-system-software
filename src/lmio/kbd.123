; -*-LISP-*- MACHINE KNIGHT KEYBOARD HANDLER
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;A lot of functions relating to KBD are in LISPM2;PROCES  notably KBD-PROCESS-TOP-LEVEL
(SPECIAL %SYS-COM-REMOTE-KEYBOARD)		;CHAR FROM PDP10 IN RAW FORM
(SPECIAL TV-MORE-PROCESSING-GLOBAL-ENABLE)
;(SPECIAL TV-CONTROL-REGISTER-2-ADDR TV-CONTROL-REGISTER-VIDEO-SWITCH)
(SPECIAL KBD-LAST-ACTIVITY-TIME)

(DEFVAR WHO-LINE-JUST-COLD-BOOTED-P NIL)	;Use SET' not SETQ for cold-load benefit
(ADD-INITIALIZATION "WHO-LINE-JUST-COLD-BOOTED-P" '(SET' WHO-LINE-JUST-COLD-BOOTED-P T)
		    '(COLD))

(SPECIAL KBD-TRANSLATE-TABLE KBD-SIMULATED-CLOCK-FCN-LIST INHIBIT-CLOCK-INTERRUPTS)

;; If this is T, then it is KBD-INTERRUPT-PROCESS, rather than
;; SELECTED-PROCESS, which is allowed to read input.
(DEFVAR KBD-PROCESS-WANTS-INPUT-FLAG NIL)

;; KBD-TYI calls this function on each character.
;; It is for handling "synchronous interrupt characters"
;; that do something when read that has nothing to do with just who is reading them.
;; The argument is the character.  The value can be the same character,
;; a translated character to return from KBD-TYI instead,
;; or NIL to make KBD-TYI ignore this character and read another.
;; The default one handles C-Z with a THROW to SI:TOP-LEVEL and BREAK with a breakpoint.
(DEFVAR KBD-TYI-HOOK 'DEFAULT-KBD-TYI-HOOK)

(DEFUN KBD-SUPER-IMAGE-P ()
    (AND SELECTED-PROCESS
	 (GET (LOCF (PROCESS-PLIST SELECTED-PROCESS)) ':KBD-SUPER-IMAGE-P)))

;KEYBOARD INTERFACE HARDWARE DOCUMENTATION
;764100 READS THE KEYBOARD DATA, LOW 16 BITS
;764102 READS THE KEYBOARD DATA, HIGH 16 BITS
;764104, 764106 ARE THE MOUSE
;764110 - REFERENCING THIS COMPLEMENTS THE BEEPER OUTPUT
;764112 IS THE STATUS REGISTER FOR KEYBOARD, MOUSE, AND CLOCK
;  0 REMOTE MOUSE ENABLE
;  1-3 INTERRUPT ENABLES
;  4 MOUSE READY
;  5 KEYBOARD READY
;  6 CLOCK READY
;FORMAT OF DATA IN 764100 (IF USING OLD KEYBOARD):
; 00077   0006	  ;KEY CODE
; 00300   0602    ;SHIFT LEFT,RIGHT
; 01400   1002    ;TOP LEFT,RIGHT
; 06000   1202    ;CONTROL LEFT,RIGHT
; 30000   1402    ;META LEFT,RIGHT
; 40000   1601    ;SHIFT LOCK

;In the latest CADR version, the microcode takes interrupts and
;stores keyboard characters into a ring-buffer stored in locations
;500-577 inclusive.  See the file LMIO; UNIBUS for more documentation on these buffers.

(ENDF HEAD)

;; This is here rather than with the scheduler because it has to be
;; in the cold-load.  It checks for the non-existence of a scheduler
;; and does it itself in that case.

;; Takes a predicate and arguments to it.  The process becomes blocked
;; until the application of the predicate to those arguments returns T.
;; Note that the function is run in the SCHEDULER stack group, not the
;; process's stack group!  This means that bindings in effect at the
;; time PROCESS-WAIT is called will not be in effect; don't refer to
;; variables "freely" if you are binding them.
;;    Kludge:  if the scheduler seems broken, or we ARE the scheduler
;; (i.e. a clock function tries to block), then loop-wait (no blinkers...)

;; In case of a sequence-break while waiting, this function can get "reinvoked".
;; Therefore, it must not modify its arguments, and must observe other restrictions.
;; see EH-REINVOKE.
(DEFUN PROCESS-WAIT (WHOSTATE FUNCTION &REST ARGUMENTS)
  (COND ((APPLY FUNCTION ARGUMENTS)	;Test condition before doing slow stack-group switch
	 NIL)				;Hmm, no need to wait after all
	((OR (NOT SCHEDULER-EXISTS)
	     (EQ SCHEDULER-STACK-GROUP %CURRENT-STACK-GROUP)
	     (NULL CURRENT-PROCESS)
	     (LET ((STATE (SG-CURRENT-STATE SCHEDULER-STACK-GROUP)))
	       (NOT (OR (= STATE SG-STATE-AWAITING-INITIAL-CALL)
			(= STATE SG-STATE-AWAITING-RETURN)))))
	 (DO () (NIL)
	   (AND (APPLY FUNCTION ARGUMENTS)
		(RETURN NIL))))
	(T
	 (SETF (PROCESS-WHOSTATE CURRENT-PROCESS) WHOSTATE)
	 (AND (EQ CURRENT-PROCESS TV-WHO-LINE-PROCESS)
	      (TV-WHO-LINE-RUN-STATE-UPDATE))		;Mark who-line as waiting
	 (WITHOUT-INTERRUPTS	;Dont allow below frobs to get reset by SB
	   (SET-PROCESS-WAIT CURRENT-PROCESS FUNCTION ARGUMENTS)
	   (FUNCALL SCHEDULER-STACK-GROUP))
	 (COND ((EQ CURRENT-PROCESS TV-WHO-LINE-PROCESS) ;Mark who-line as running
		(TV-WHO-LINE-RUN-STATE-UPDATE))))))


;; Read one character from the keyboard, processing BREAK and C-Z
;; (or whatever else the function which is the value of KBD-TYI-HOOK implements)
;; The optional argument is the wholine state string.
;; KBD-TYI-HOOK is funcalled with each character.
;; It can return the character, a different character to return instead,
;; or NIL to ignore the character aside from what the hook function already did.
(DEFUN KBD-TYI (&OPTIONAL (WHOSTATE "TYI"))
  (PROG (CH)
TOP (SETQ CH (KBD-TYI-1 WHOSTATE))	    ;Get a character.
    (SETQ CH (FUNCALL KBD-TYI-HOOK CH))
    (COND ((NULL CH) (GO TOP))
	  ((LISTP CH) (GO TOP))		    ;Ignore mouse characters.
	  (T (RETURN CH)))))

;; This is the default hook function for KBD-TYI.
;; We call BREAK if BREAK is typed, and throw to TOP-LEVEL is C-Z is typed.
;; Anything else we allow to be returned by KBD-TYI.
(DEFUN DEFAULT-KBD-TYI-HOOK (CH)
  (COND ((LISTP CH) CH)
	((= CH #\BREAK) (BREAK BREAK T) NIL)
	((OR (= CH #/Z) (= CH #/z))	;Don't call CHAR-UPCASE, not defined in cold-load
	 (PRINC "Z Quit")
	 (THROW NIL TOP-LEVEL))
	(T CH)))

;; Return a character read from the keyboard.
(DEFUN KBD-TYI-1 (&OPTIONAL (WHOSTATE "TYI") &AUX TEM)
  (DO () (NIL) ;forever.
    (COND ((SETQ TEM (KBD-TYI-NO-HANG))
	   (SETQ KBD-LAST-ACTIVITY-TIME (TIME) WHO-LINE-JUST-COLD-BOOTED-P NIL)
	   (AND TEM (RETURN TEM))))
    (TV-NOTE-INPUT) ;Have hung waiting for user
    (PROCESS-WAIT WHOSTATE
		  (FUNCTION KBD-CHAR-AVAILABLE))))

;; Input a character in Lisp-machine ascii code.  NIL if none immediately available.
(DEFUN KBD-TYI-NO-HANG (&AUX KBD ASC PROCESS (INHIBIT-SCHEDULING-FLAG T))
  (COND ((NOT (KBD-CHAR-AVAILABLE)) NIL)  ;Nothing available, return NIL
	;; Check for forced input associated with this process.
	((AND CURRENT-PROCESS
	      (SETQ KBD (PROCESS-FORCED-INPUT CURRENT-PROCESS)))
	 (SETQ PROCESS CURRENT-PROCESS)
	 (COND ((OR (NUMBERP KBD) (LISTP KBD))
		(SETF (PROCESS-FORCED-INPUT PROCESS) NIL)
		KBD)
	       ((ARRAYP KBD)
		(PROG1 (AR-1 KBD (SETQ ASC (PROCESS-FORCED-INPUT-INDEX PROCESS)))
		       (SETF (PROCESS-FORCED-INPUT-INDEX PROCESS) (SETQ ASC (1+ ASC)))
		       (OR (< ASC (ARRAY-ACTIVE-LENGTH KBD))
			   (SETF (PROCESS-FORCED-INPUT PROCESS) NIL))))
	       (T
		(SETF (PROCESS-FORCED-INPUT PROCESS) NIL)  ;In hopes of not bombing on next TYI.
		(FERROR NIL "~S is invalid PROCESS-FORCED-INPUT" KBD))))
	;; Check for input over the remote keyboard link
	((PLUSP (SETQ KBD (SYSTEM-COMMUNICATION-AREA %SYS-COM-REMOTE-KEYBOARD)))
	 (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-REMOTE-KEYBOARD) -1)
	 KBD)
	;; Check for hardware input, this will have to be code-converted
	(KBD-BUFFER           ;Put here by SI:KBD-GET-HARDWARE-CHAR-IF-ANY
	 (SETQ KBD KBD-BUFFER
	       KBD-BUFFER NIL)
         (KBD-CONVERT KBD))))

;Convert a character as read from the keyboard hardware
;to one in the standard Lisp machine character set.
(DEFUN KBD-CONVERT (KBD &AUX ASC SHIFT BUCKY)
  (COND ((= (LDB 2003 KBD) 1) ;Check source ID for new keyboard
	 (KBD-CONVERT-NEW KBD))
	(T
	 (SETQ SHIFT (COND ((BIT-TEST 1400 KBD) 2)	;TOP
			   ((BIT-TEST 300 KBD) 1)	;SHIFT
			   (T 0)))			;VANILLA
	 (SETQ BUCKY (+ (COND ((BIT-TEST 06000 KBD) 0400) (T 0))	;CONTROL
			(COND ((BIT-TEST 30000 KBD) 1000) (T 0))))	;META
	 (SETQ ASC (AR-2 KBD-TRANSLATE-TABLE SHIFT (LOGAND 77 KBD)))
	 (AND (BIT-TEST 40000 KBD)			;SHIFT LOCK
	      (NOT (< ASC 141))
	      (NOT (> ASC 172))
	      (SETQ ASC (- ASC 40)))
	 (+ ASC BUCKY))))

;; This function is for those who like to eat it raw.  It is identical to
;; KBD-TYI-NO-HANG except that it gives you raw hardware code and consequently
;; only works for input coming from the hardware.
(DEFUN KBD-TYI-RAW-NO-HANG (&AUX (INHIBIT-SCHEDULING-FLAG T))
  (COND ((NOT (KBD-CHAR-AVAILABLE)) NIL)  ;Nothing available, return NIL
	;; Check for processed (unhealthy) input
	((OR (AND CURRENT-PROCESS
		  (PROCESS-FORCED-INPUT CURRENT-PROCESS))
	     (PLUSP (SYSTEM-COMMUNICATION-AREA %SYS-COM-REMOTE-KEYBOARD)))
	 (FERROR NIL "Attempt to read raw keyboard code from non-hardware source"))
	;; Return raw code from KBD-BUFFER
	(T (PROG1 KBD-BUFFER
		  (SETQ KBD-BUFFER NIL)))))

;; This function decides whether a character is available to this process.
;; If so, it returns T, if not it returns NIL.  This is suitable
;; for use as a process-wait function.
;; Importantly, this function simulates interrupts by checking
;; for special characters and signalling the keyboard process.
;; The 3 sources of characters are the hardware, the remote keyboard,
;; and job-forced-input, which cannot supply special characters.
;; The hardware is the only one which needs to be code-converted.
;; This function is the only one which reads anything out of the hardware,
;; moving it into the variable KBD-BUFFER.  Later, some of this function
;; will be in the microcoded keyboard interrupt handler.
;; JUST-INTERRUPTS means only check for interrupts.
(DEFUN KBD-CHAR-AVAILABLE (&OPTIONAL JUST-INTERRUPTS)
 (WITHOUT-INTERRUPTS
  (PROG (CH CH6)
   TRY-AGAIN
    ;; Get hardware input if any
    (AND (NULL KBD-BUFFER)
	 (KBD-GET-HARDWARE-CHAR-IF-ANY))
    ;; Bypass interrupt check if interrupts not to be taken now
    (AND (KBD-SUPER-IMAGE-P) (GO NOINT))
    (AND (NOT JUST-INTERRUPTS)
	 (OR (NULL CURRENT-PROCESS) 
	     (EQ CURRENT-PROCESS KBD-INTERRUPT-PROCESS))
	 (GO NOINT))
    ;; Check hardware input for interrupt
    (COND ((NULL KBD-BUFFER))			;No character
	  ((= (LDB 2003 KBD-BUFFER) 7)		;Old keyboard
	   (AND (OR (= (SETQ CH6 (LOGAND KBD-BUFFER 77)) 1)  ;ESC in raw code
		    (= CH6 20))			;CALL in raw code
		(GO INT)))
	  ((= (LDB 2003 KBD-BUFFER) 1)		;New keyboard
	   (AND (OR (= (SETQ CH6 (LDB 0020 KBD-BUFFER)) 107)	;CALL
		    (= CH6 40))			;ESC (terminal escape)
		(GO INT))
	   (COND ((BIT-TEST 100400 KBD-BUFFER)	;Do unreal characters
		  (KBD-CONVERT-NEW KBD-BUFFER)
		  (SETQ KBD-BUFFER NIL)
		  (GO TRY-AGAIN)))))
    ;; Check remote input for interrupt
    (AND (PLUSP (SETQ CH (SYSTEM-COMMUNICATION-AREA %SYS-COM-REMOTE-KEYBOARD)))
	 (OR (= (SETQ CH6 (LDB %%KBD-CHAR CH)) 204)	;ESC
	     (= CH6 203))			;CALL
	 (GO INT))
   NOINT 
    ;; No interrupt to be taken, check for input
    (AND JUST-INTERRUPTS (RETURN NIL))
    (RETURN (COND ((AND CURRENT-PROCESS (PROCESS-FORCED-INPUT CURRENT-PROCESS)))  ;Forced input
		  ((AND CURRENT-PROCESS
			(NEQ CURRENT-PROCESS
			     (OR (AND KBD-PROCESS-WANTS-INPUT-FLAG KBD-INTERRUPT-PROCESS)
				 SELECTED-PROCESS)))
		   NIL)	;This process not allowed to look at kbd
		  (KBD-BUFFER)
		  ((PLUSP (SYSTEM-COMMUNICATION-AREA %SYS-COM-REMOTE-KEYBOARD)))
		  (T NIL)))
   INT
    ;; Take interrupt
    (SETQ KBD-INTERRUPT-KLUDGE T)
    (RETURN NIL))))

;;; Sys com locations 500-577 are reserved for the wired keyboard buffer:
;;; Locations 501 through 511 contain the buffer header; 520-577 are the buffer (48. chars)

;; Refresh KBD-BUFFER if hardware has a character
;; Call this only from KBD-CHAR-AVAILABLE with INTERRUPTS OFF.
(DEFUN KBD-GET-HARDWARE-CHAR-IF-ANY (&AUX P)
  (COND (( (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
	    (SETQ P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))))
	 (SETQ KBD-BUFFER (%P-LDB %%Q-POINTER P))
	 (SETQ P (1+ P))
	 (AND (= P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-END)))
	      (SETQ P (%P-LDB %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-START))))
	 (%P-DPB P %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR)))))

;; Translate from a Unibus address to a Lisp machine virtual address, returning a fixnum.
(DEFUN VIRTUAL-UNIBUS-ADDRESS (ADR)
  (%24-BIT-PLUS (LSH 7740 12.) (LSH ADR -1)))

;; This is called when the machine is booted, warm or cold.  It's not an
;; initialization because it has to happen before all other initializations.
(DEFUN INITIALIZE-WIRED-KBD-BUFFER ()
  (DO I 500 (1+ I) (= I 600)
    (%P-STORE-TAG-AND-POINTER I 0 0))
  (%P-DPB 260 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-VECTOR-ADDRESS))
  (%P-DPB (VIRTUAL-UNIBUS-ADDRESS 764112) %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-CSR-ADDRESS))
  (%P-DPB 40 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-CSR-BITS))
  (%P-DPB (VIRTUAL-UNIBUS-ADDRESS 764100) %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-DATA-ADDRESS))
  (%P-DPB 1 %%Q-FLAG-BIT (+ 500 %UNIBUS-CHANNEL-DATA-ADDRESS))
  (%P-DPB 520 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-START))
  (%P-DPB 600 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-END))
  (%P-DPB 520 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-IN-PTR))
  (%P-DPB 520 %%Q-POINTER (+ 500 %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST) 500)
  (%UNIBUS-WRITE 764112 4))	;Keyboard interrupt enable, local mouse

;KEYBOARD TRANSLATE TABLE IS A 3 X 64 ARRAY.
;3 ENTRIES FOR EACH OF 100 KEYS.  FIRST IS VANILLA, SECOND SHIFT, THIRD TOP.
;THE FUNCTION KBD-INITIALIZE IS ONLY CALLED ONCE, IN ORDER TO SET UP THIS ARRAY.

(DEFUN KBD-INITIALIZE ()
  (SETQ TV-WHO-LINE-RUN-LIGHT-LOC 51765
	USER-ID "")
  (SETQ KBD-TRANSLATE-TABLE (MAKE-ARRAY WORKING-STORAGE-AREA 'ART-8B '(3 100)))
  (DO ((I 0 (1+ I))  ;2ND DIMENSION
       (L '(
	201	201	201		;0 BREAK
	204	204	204		;1 ESCAPE
	61	41	41		;2 ONE
	62	42	42		;3 TWO
	63	43	43		;4 THREE
	64	44	44		;5 FOUR
	65	45	45		;6 FIVE
	66	46	46		;7 SIX
	67	47	47		;10 SEVEN
	70	50	50		;11 EIGHT
	71	51	51		;12 NINE
	60	137	137		;13 ZERO
	55	75	75		;14 EQUAL
	100	140	140		;15 ATSIGN
	136	176	176		;16 TILDE
	210	210	210		;17 BS
	203	203	203		;20 CALL
	202	202	202		;21 CLEAR
	211	211	211		;22 TAB
	33	33	33		;23 ALTMODE
	161	121	4		;24 CONJUNCTION
	167	127	37		;25 DISJUNCTION
	145	105	22		;26 UPLUMP
	162	122	23		;27 DOWNLUMP
	164	124	20		;30 LEFTLUMP
	171	131	21		;31 RIGHTLUMP
	165	125	5		;32 ELBOW
	151	111	26		;33 WHEEL
	157	117	1		;34 DOWNARROW
	160	120	13		;35 UPARROW
	133	173	173		;36 LEFT BRACKET
	135	175	175		;37 RIGHT BRACE
	134	174	174		;40 VERTICAL BAR
	57	16	16		;41 INFINITY
	14	12	0		;42 PLUS MINUS / DELTA (WRONG KEYTOP)
	15	11	177		;43 CIRCLE PLUS / GAMMA (WRONG KEYTOP)
	214	214	214		;44 FORM
	213	213	213		;45 VT
	207	207	207		;46 RUBOUT
	141	101	34		;47 LESS OR EQUAL
	163	123	35		;50 GREATER OR EQUAL
	144	104	36		;51 EQUIVALENCE
	146	106	17		;52 PARTIAL
	147	107	32		;53 NOT EQUAL
	150	110	206		;54 HELP
	152	112	30		;55 LEFTARROW
	153	113	31		;56 RIGHTARROW
	154	114	27		;57 BOTHARROW
	73	53	53		;60 PLUS
	72	52	52		;61 STAR
	215	215	215		;62 RETURN
	212	212	212		;63 LINE
	205	205	205		;64 BACKNEXT
	172	132	2		;65 ALPHA
	170	130	3		;66 BETA
	143	103	6		;67 EPSILON
	166	126	10		;70 LAMBDA
	142	102	7		;71 PI
	156	116	24		;72 UNIVERSAL
	155	115	25		;73 EXISTENTIAL
	54	74	74		;74 LESS THAN
	56	76	76		;75 GREATER THAN
	57	77	77		;76 QUESTION MARK
	40	40	40		;77 HORIZONTAL BAR
	) (CDDDR L)))
      ((NULL L))
    (AS-2 (CAR L) KBD-TRANSLATE-TABLE 0 I)
    (AS-2 (CADR L) KBD-TRANSLATE-TABLE 1 I)
    (AS-2 (CADDR L) KBD-TRANSLATE-TABLE 2 I)))

;Support for new keyboard

(DEFVAR KBD-SHIFTS 0)			;Bit mask of shifting keys held down

(DECLARE (SPECIAL KBD-NEW-TABLE))	;Array used as translation table.
;The second dimension is 200 long and indexed by keycode.
;The first dimension is the shifts:
; 0 unshifted
; 1 shift
; 2 caps lock and no shift
; 3 top
; 4 greek
; 5 shift greek
;Elements in the table are 16-bit unsigned numbers.
;Bit 15 on and bit 14 on means undefined code, ignore and beep.
;Bit 15 on and bit 14 off means low bits are shift for bit in KBD-SHIFTS
;Bit 15 off is ordinary code.

;Can return NIL if character wasn't really a character.
(DEFUN KBD-CONVERT-NEW (CH)
  (COND ((BIT-TEST 1_15. CH)		;An all-keys-up code, just update shifts mask
	 (SETQ KBD-SHIFTS (LDB 0012 CH))
	 NIL)
	(T (LET ((NCH (AREF KBD-NEW-TABLE	;NCH gets translate-table entry
			    (COND ((BIT-TEST 2 KBD-SHIFTS)	;Greek
				   (+ (LOGAND 1 KBD-SHIFTS) 4))
				  ((BIT-TEST 4 KBD-SHIFTS) 3)	;Top
				  ((BIT-TEST 1 KBD-SHIFTS) 1)	;Shift
				  ((BIT-TEST 10 KBD-SHIFTS) 2)	;Caps lock
				  (T 0))
			    (LDB 0007 CH))))
	     (COND ((BIT-TEST 1_15. NCH)	;Not a real character
		    (COND ((BIT-TEST 1_14. NCH)	;Undefined key, beep if key-down
			   (OR (BIT-TEST 1_8 CH)
			       (TV-BEEP)))
			  (T			;A shifting key, update KBD-SHIFTS
			    (SETQ KBD-SHIFTS (BOOLE (IF (BIT-TEST 1_8 CH) 2 7)
						    (LSH 1 (LOGAND NCH 77))
						    KBD-SHIFTS))))
		    NIL)
		   ((BIT-TEST 1_8 CH) NIL)	 ;Just an up-code
		   (T (DPB (LDB 0402 KBD-SHIFTS) ;Control and Meta
			   1002 NCH)))))))	 ;A real character pushed down

(DEFUN KBD-MAKE-NEW-TABLE ()
  (SETQ TV-BEEP NIL)			;Temporary, kbd has no beeper
  (LET ((TBL (MAKE-ARRAY PERMANENT-STORAGE-AREA 'ART-16B '(6 200))))
    (DO ((J 0 (1+ J))
	 (L '( 
	()					 ;0 not used
	()					 ;1 Roman II
	()					 ;2 Roman IV
	100011					 ;3 Mode lock
	()					 ;4 not used
	100006					 ;5 Left super
	()					 ;6 not used
	()					 ;7 not used
	()					 ;10 not used
	(#/4 #/$ #/4 #/$)			 ;11 Four
	(#/r #/R #/R #/)			 ;12 R
	(#/f #/F #/F)				 ;13 F
	(#/v #/V #/V)				 ;14 V
	100008					 ;15 Alt Lock
	()					 ;16 not used
	()					 ;17 Hand Right
	100004					 ;20 Left control
	(#/: 14 #/: 14)				 ;21 plus-minus
	#\TAB					 ;22 tab
	#\RUBOUT				 ;23 rubout
	100000					 ;24 Left Shift
	100000					 ;25 Right Shift
	100004					 ;26 Right control
	()					 ;27 not used
	()					 ;30 hold output
	(#/8 #/* #/8 #/*)			 ;31 Eight
	(#/i #/I #/I #/)			 ;32 I
	(#/k #/K #/K #/)			 ;33 K
	(#/, #/< #/, #/<)			 ;34 comma
	100001					 ;35 Right Greek
	#\LINE					 ;36 Line
	(#/\ #/| #/\ #/|)			 ;37 Backslash
	#\ESC					 ;40 terminal
	()					 ;41 not used
	()					 ;42 network
	()					 ;43 not used
	100001					 ;44 Left Greek
	100005					 ;45 Left Meta
	()					 ;46 status
	()					 ;47 resume
	#\FORM					 ;50 clear screen
	(#/6 #/^ #/6 #/^)			 ;51 Six
	(#/y #/Y #/Y #/)			 ;52 Y
	(#/h #/H #/H #/)			 ;53 H
	(#/n #/N #/N #/)			 ;54 N
	()					 ;55 not used
	()					 ;56 not used
	()					 ;57 not used
	()					 ;60 not used
	(#/2 #/@ #/2 #/@)			 ;61 Two
	(#/w #/W #/W #/)			 ;62 W
	(#/s #/S #/S)				 ;63 S
	(#/x #/X #/X)				 ;64 X
	100006					 ;65 Right Super
	()					 ;66 not used
	()					 ;67 Abort
	()					 ;70 not used
	(#/9 #/( #/9 #/( )			 ;71 Nine
	(#/o #/O #/O #/)			 ;72 O
	(#/l #/L #/L #/ 10)			 ;73 L/lambda
	(#/. #/> #/. #/>)			 ;74 period
	()					 ;75 not used
	()					 ;76 not used
	(#/` #/~ #/` #/~ #/)			 ;77 back quote
	#\BACK-NEXT				 ;100 macro
	()					 ;101 Roman I
	()					 ;102 Roman III
	()					 ;103 not used
	100002					 ;104 Left Top
	()					 ;105 not used
	()					 ;106 Up Thumb
	#\CALL					 ;107 Call
	#\CLEAR					 ;110 Clear Input
	(#/5 #/% #/5 #/%)			 ;111 Five
	(#/t #/T #/T #/)			 ;112 T
	(#/g #/G #/G #/ 11)			 ;113 G/gamma
	(#/b #/B #/B #/ #/)			 ;114 B
	()					 ;115 Repeat
	#\HELP					 ;116 Help
	()					 ;117 Hand Left
	()					 ;120 Quote
	(#/1 #/! #/1 #/!)			 ;121 One
	(#/q #/Q #/Q #/)			 ;122 Q
	(#/a #/A #/A 140000 #/)		 ;123 A
	(#/z #/Z #/Z)				 ;124 Z
	100003					 ;125 Caps Lock
	(#/= #/+ #/= #/+)			 ;126 Equals
	()					 ;127 not used
	()					 ;130 not used
	(#/- #/_ #/- #/_)			 ;131 Minus
	(#/( #/[ #/( #/[)			 ;132 Open parenthesis
	(#/' #/" #/' #/" 0)			 ;133 Apostrophe/center-dot
	#\SP					 ;134 Space
	()					 ;135 not used
	#\CR					 ;136 Return
	(#/) #/] #/) #/])			 ;137 Close parenthesis
	()					 ;140 not used
	()					 ;141 system
	()					 ;142 not used
	#/					 ;143 Alt Mode
	()					 ;144 not used
	100007					 ;145 Left Hyper
	(#/} 140000 #/} 140000)			 ;146 }
	()					 ;147 not used
	()					 ;150 not used
	(#/7 #/& #/7 #/&)			 ;151 Seven
	(#/u #/U #/U #/)			 ;152 U
	(#/j #/J #/J #/)			 ;153 J
	(#/m #/M #/M #/)			 ;154 M
	100002					 ;155 Right Top
	()					 ;156 End
	()					 ;157 Delete
	()					 ;160 Overstrike
	(#/3 #/# #/3 #/#)			 ;161 Three
	(#/e #/E #/E #/ #/)			 ;162 E
	(#/d #/D #/D 140000 15)			 ;163 D/delta
	(#/c #/C #/C #/)			 ;164 C
	100005					 ;165 Right Meta
	(#/{ 140000 #/{ 140000)			 ;166 {
	#\BREAK					 ;167 Break
	()					 ;170 Stop Output
	(#/0 #/) #/0 #/))			 ;171 Zero
	(#/p #/P #/P #/ #/)			 ;172 P
	(#/; #/: #/; #/:)			 ;173 Semicolon
	(#// #/? #// #/? 177)			 ;174 Question/Integral
	100007					 ;175 Right Hyper
	()					 ;176 Down Thumb
	()					 ;177 Not used
	      ) (CDR L)))
	((= J 200) (SETQ KBD-NEW-TABLE TBL))
      (DO ((I 0 (1+ I))
	   (K (CAR L)))
	  ((= I 6))
	(ASET (COND ((ATOM K) (OR K 140000))
		    ((NULL (CAR K)) 140000)
		    (T (CAR K)))
	      TBL I J)
	(AND (LISTP K) (SETQ K (CDR K)))))))

(SETQ KBD-NEW-TABLE (KBD-MAKE-NEW-TABLE))
