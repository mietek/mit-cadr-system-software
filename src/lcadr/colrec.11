;;; Given a rectangle, an ART-4B array, and a 16 values which specify new values
;;; for the pixels (indexed by current pixel value), hacks the ART-4B array appropriatly

;;; (%DRAW-COLOR-RECTANGLE N17 N16 N15 N14 N13 N12 N11 N10 N7 N6 N5 N4 N3 N2 N1 N0
;;;			   WIDTH HEIGHT ARRAY START-X START-Y)

(PROGN
(UA-DEFMIC %DRAW-COLOR-RECTANGLE 701
	 (N17 N16 N15 N14 N13 N12 N11 N10 N7 N6 N5 N4 N3 N2 N1 N0
	  WIDTH HEIGHT ARRAY START-X START-Y)
	T T)

(SETQ COLREC '(

;;; *** Copied from LCADR;UCADR
(ASSIGN CHECK-PAGE-READ (PLUS CALL-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-R-I))
(ASSIGN CHECK-PAGE-WRITE (PLUS CALL-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-W-I))

(DEF-DATA-FIELD OAL-MROT 5 0)
(DEF-DATA-FIELD Q-POINTER 30 0)
;;; *** End copy


(LOCALITY A-MEM)
A-COLR-COUNT (0)

(LOCALITY I-MEM)
XDRAW-COLOR-RECTANGLE  (MISC-INST-ENTRY %DRAW-COLOR-RECTANGLE)
	(CALL XAR2)
	((M-TEM) (LISP-BYTE %%ARRAY-TYPE-FIELD) M-B)
	(CALL-NOT-EQUAL M-TEM (A-CONSTANT 3) TRAP)
	    (ERROR-TABLE ARGTYP ART-4B-ARRAY M-A)
	((M-1) Q-POINTER C-PDL-BUFFER-POINTER-POP)	;Height
	((M-2) Q-POINTER C-PDL-BUFFER-POINTER-POP)	;Width
	(JUMP-LESS-OR-EQUAL M-2 A-ZERO COLR-DONE)	;Don't do anything if width is zero
	((M-D) Q-POINTER M-D)				;Get rid of data type

	;;; Outer loop over all Y's
	;;; M-E is array base address
	;;; M-J is current array index
	;;; M-S is array length
	;;; M-1 is row counter
	;;; M-2 is rectangle width
	;;; M-D is total array width
	;;; M-Q holds array index of start of next row
COLR-NEXT-Y
	(JUMP-LESS-OR-EQUAL-XCT-NEXT M-1 A-ZERO COLR-DONE)
       ((M-1) SUB M-1 (A-CONSTANT 1))			;One fewer iterations
	((A-COLR-COUNT) M-2)				;Do this many bytes
	((M-TEM) (BYTE-FIELD 21. 3) M-Q)		;Word offset in array
	((VMA-START-READ) ADD M-E A-TEM)
	((M-J) M-Q)					;Copy array index
	((M-Q) ADD M-Q A-D)				;Start of next row
COLR-NEXT-WORD
	(CALL-GREATER-OR-EQUAL M-J A-S TRAP)		;Bounds checking
	    (ERROR-TABLE SUBSCRIPT-OOB M-J M-S)
	(CHECK-PAGE-READ)
	((M-4) READ-MEMORY-DATA)			;Word containing up to 8 bytes
COLR-NEXT-BYTE
	((A-COLR-COUNT) ADD M-MINUS-ONE A-COLR-COUNT)
	((M-K) DPB M-J (BYTE-FIELD 3 2) A-ZERO)		;Rotation amount in bits
	((M-TEM) SUB (M-CONSTANT 40) A-K)		;To rotate byte to low end of word
	((OA-REG-LOW) DPB M-TEM OAL-MROT A-ZERO)
	((M-3) (BYTE-FIELD 4 0) M-4 A-ZERO)		;Get byte field
	((PDL-BUFFER-INDEX) SUB PDL-BUFFER-POINTER A-3)	;Offset to new byte
	((OA-REG-LOW) DPB M-K OAL-MROT A-ZERO)
	((M-4) DPB C-PDL-BUFFER-INDEX (BYTE-FIELD 4 0) A-4)	;Replace byte
	(JUMP-GREATER-OR-EQUAL M-ZERO A-COLR-COUNT COLR-WRITE-WORD)	;Jump if row done
	(JUMP-LESS-THAN-XCT-NEXT M-K (A-CONSTANT 34) COLR-NEXT-BYTE)	;Jump if word not done
       ((M-J) ADD M-J (A-CONSTANT 1))			;One more byte rotation
COLR-WRITE-WORD
	((WRITE-MEMORY-DATA-START-WRITE) M-4)		;Ran out of the word, store it back
	(CHECK-PAGE-WRITE)
	(JUMP-GREATER-OR-EQUAL M-ZERO A-COLR-COUNT COLR-NEXT-Y) ;Terminate if no more
	(JUMP-XCT-NEXT COLR-NEXT-WORD)			;Else read next word and go on
       ((VMA-START-READ) ADD VMA (A-CONSTANT 1))

COLR-DONE
	(JUMP-XCT-NEXT XTRUE)
       ((PDL-BUFFER-POINTER) SUB PDL-BUFFER-POINTER (A-CONSTANT 16.))
))
)
