;;; FEF Disassembler			-*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This stuff is used by LISPM2; EH >.  If you change things around,
;;; make sure not to break that program.

(DEFVAR DISASSEMBLE-OBJECT-OUTPUT-FUN NIL)
(DEFUN DISASSEMBLE (FUNCTION &AUX FEF LIM-PC ILEN (DISASSEMBLE-OBJECT-OUTPUT-FUN NIL))
  
  (COND ((= (%DATA-TYPE FUNCTION) DTP-FEF-POINTER)
	 (SETQ FEF FUNCTION))
        ((SETQ FEF (FDEFINITION FUNCTION))))
  (COND ((AND (LISTP FEF)
	      (EQ (CAR FEF) 'MACRO))
	 (FORMAT T "~%Definition as macro")
	 (SETQ FEF (CDR FEF))))
  (OR (= (%DATA-TYPE FEF) DTP-FEF-POINTER)
      (FERROR NIL "Can't find FEF for ~S" FUNCTION))
  (SETQ LIM-PC (DISASSEMBLE-LIM-PC FEF))
  (DO PC (SI:FEF-INITIAL-PC FEF) (+ PC ILEN) (>= PC LIM-PC)
    (TERPRI)
    (SETQ ILEN (DISASSEMBLE-INSTRUCTION FEF PC)))
  (TERPRI))

(DEFUN DISASSEMBLE-LIM-PC (FEF &AUX LIM-PC)
  (SETQ LIM-PC (* 2 (SI:FEF-LENGTH FEF)))
  (COND ((ZEROP (DISASSEMBLE-FETCH FEF (1- LIM-PC)))
	 (1- LIM-PC))
	(T LIM-PC)))

;; Return the length of the instruction in FEF at PC.
(defun disassemble-instruction-length (fef pc &aux wd op disp)
    (setq wd (disassemble-fetch fef pc))
    (setq op (ldb 1104 wd)
	  disp (ldb 0011 wd))
    (cond ((and (= op 14) (= disp 777)) 2)
	  (t 1)))

;Returns the length of the instruction, usually 1. 
(DEFUN DISASSEMBLE-INSTRUCTION (FEF PC &AUX &SPECIAL (BASE 8) &LOCAL
			       WD OP DEST REG DISP)
  (PROG NIL  ;PROG so that RETURN can be used to return unusual instruction lengths. 
    (SETQ WD (DISASSEMBLE-FETCH FEF PC))
    (PRIN1 PC)
    (TYO 40)
    (SETQ OP (LDB 1104 WD)
	  DEST (LDB 1503 WD)
	  DISP (LDB 0011 WD)
	  REG (LDB 0603 WD))
    (COND ((ZEROP WD)
	   (PRINC "0"))
	  ((< OP 11)     ;DEST/ADDR 
	   (PRINC (NTH OP '(CALL CALL0 MOVE CAR CDR CADR CDDR CDAR CAAR)))
	   (TYO 40)
	   (PRINC (NTH DEST '(D-IGNORE D-PDL D-NEXT D-LAST
			      D-RETURN D-NEXT-Q D-LAST-Q D-NEXT-LIST)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP))
	  ((= OP 11)	    ;ND1
	   (PRINC (NTH DEST '(ND1-UNUSED + - * // LOGAND LOGXOR LOGIOR)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP))
	  ((= OP 12)	    ;ND2
	   (PRINC (NTH DEST '(= > < EQ SETE-CDR SETE-CDDR SETE-1+ SETE-1-)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP))
	  ((= OP 13)	    ;ND3
	   (PRINC (NTH DEST '(BIND-OBSOLETE? BIND-NIL BIND-POP SET-NIL SET-ZERO PUSH-E MOVEM POP)))
	   (DISASSEMBLE-ADDRESS FEF REG DISP))
	  ((= OP 14)	    ;BRANCH
	   (PRINC (NTH DEST '(BR BR-NIL BR-NOT-NIL BR-NIL-POP
			      BR-NOT-NIL-POP BR-ATOM BR-NOT-ATOM BR-ILL-7)))
	   (TYO 40)
	   (AND (> DISP 400) (SETQ DISP (LOGIOR -400 DISP))) ;SIGN-EXTEND
	   (COND ((NEQ DISP -1)	    ;ONE WORD
		  (PRIN1 (+ PC DISP 1)))
		 (T	    ;LONG BRANCH
		  (SETQ DISP (DISASSEMBLE-FETCH FEF (SETQ PC (1+ PC))))
		  (AND (> DISP 100000) (SETQ DISP (LOGIOR -100000 DISP)))
		  (PRINC "*")	;INDICATE LONG BRANCH FOR USER.
		  (PRIN1 (+ PC DISP 1))
		  (RETURN 2))))
	  ((= OP 15)	    ;MISC
	   (PRINC "(MISC) ") ;Moon likes to see this
	   (COND ((< DISP 100) (FORMAT T "LIST ~D long " DISP))
		 ((< DISP 200) (FORMAT T "LIST-IN-AREA ~D long " (- DISP 100)))
		 ((< DISP 220) (FORMAT T "UNBIND ~D bindings " (- DISP 177)))
		 ((< DISP 240) (FORMAT T "POP-PDL ~D times " (- DISP 217)))
		 (T
                  (LET ((OP (MICRO-CODE-SYMBOL-NAME-AREA (- DISP 200))))
                    (COND ((NULL OP) (FORMAT T "#~O " DISP))
                          (T (FORMAT T "~A " OP))))))
	   (PRINC (NTH DEST '(D-IGNORE D-PDL D-NEXT D-LAST
			      D-RETURN D-NEXT-Q D-LAST-Q D-NEXT-LIST))))
	  (T		    ;UNDEF
	   (PRINC 'UNDEF-)
	   (PRIN1 OP)))
    (RETURN 1)))

;; Print out the disassembly of an instruction source address.
;; REG is the register number of the address, and DISP is the displacement.
(DEFUN DISASSEMBLE-ADDRESS (FEF REG DISP &AUX PTR OFFSET TEM LOC)
  (TYO 40)
  (COND ((< REG 4)
	 (SETQ LOC (%MAKE-POINTER-OFFSET DTP-LOCATIVE FEF DISP))
         (FORMAT T "FEF|~D ~30,8T;" DISP)
	 (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF DISP) DTP-EXTERNAL-VALUE-CELL-POINTER)
		(SETQ PTR (%FIND-STRUCTURE-HEADER
			   (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF DISP)))
		      OFFSET (%POINTER-DIFFERENCE TEM PTR))
		(LET ((CELL (NTH OFFSET '("@+0?? " "" "#'"
					  "@PLIST-HEAD-CELL " "@PACKAGE-CELL "))))
		  (IF DISASSEMBLE-OBJECT-OUTPUT-FUN
		      (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN PTR CELL LOC)
		      (PRINC CELL)
		      (PRIN1 PTR) )))
	       (T
		(IF DISASSEMBLE-OBJECT-OUTPUT-FUN
		    (FUNCALL DISASSEMBLE-OBJECT-OUTPUT-FUN (CAR LOC) "'" LOC)
		    (PRINC '/')
		    (PRIN1 (%P-CONTENTS-OFFSET FEF DISP))))))
	((= REG 4)
	 (PRINC '/')
	 (PRIN1 (CONSTANTS-AREA (LOGAND 77 DISP))))
	((= DISP 777)
	 (PRINC 'PDL-POP))
        ((= REG 5)
         (FORMAT T "LOCAL|~D" (LOGAND 77 DISP))
         (SETQ TEM (DISASSEMBLE-LOCAL-NAME FEF (LOGAND 77 DISP)))
         (AND TEM (FORMAT T " ~30,8T;~A" TEM)))
        ((= REG 6)
         (FORMAT T "ARG|~D" (LOGAND 77 DISP))
         (SETQ TEM (DISASSEMBLE-ARG-NAME FEF (LOGAND 77 DISP)))
         (AND TEM (FORMAT T " ~30,8T;~A" TEM)))
	(T
         (FORMAT T "PDL|-~D" (LOGAND 77 DISP)))))

;; Given a fef and the number of a slot in the local block,
;; return the name of that local (or NIL if unknown).
;; If it has more than one name due to slot-sharing, we return a list of
;; the names, but if there is only one name we return it.
(DEFUN DISASSEMBLE-LOCAL-NAME (FEF LOCALNUM)
  (LET ((FDI (SI:FUNCTION-DEBUGGING-INFO FEF)))
    (LET ((NAMES (NTH LOCALNUM (CADR (ASSQ 'COMPILER:LOCAL-MAP FDI)))))
      (COND ((NULL NAMES) NIL)
	    ((NULL (CDR NAMES)) (CAR NAMES))
	    (T NAMES)))))

;; Given a fef and the number of a slot in the argument block,
;; return the name of that argument (or NIL if unknown).
;; First we look for an arg map, then we look for a name in the ADL.
(DEFUN DISASSEMBLE-ARG-NAME (FEF ARGNUM &AUX
                                 (FDI (SI:FUNCTION-DEBUGGING-INFO FEF))
                                 (ARGMAP (CADR (ASSQ 'COMPILER:ARG-MAP FDI))))
    (COND (ARGMAP (CAR (NTH ARGNUM ARGMAP)))
          (T (DO ((ADL (GET-MACRO-ARG-DESC-POINTER FEF) (CDR ADL))
                  (IDX 0 (1+ IDX))
                  (ADLWORD))
                 ((NULL ADL))
               (SETQ ADLWORD (CAR ADL))
               (SELECT (MASK-FIELD %%FEF-ARG-SYNTAX ADLWORD)
                 ((FEF-ARG-REQ FEF-ARG-OPT))
                 (OTHERWISE (RETURN)))
               (AND (= 1 (LDB %%FEF-NAME-PRESENT ADLWORD))
                    (SETQ ADL (CDR ADL)))
               (COND ((= IDX ARGNUM)
                      (RETURN (AND (= 1 (LDB %%FEF-NAME-PRESENT ADLWORD)) (CAR ADL)))))
               (SELECT (MASK-FIELD %%FEF-INIT-OPTION ADLWORD)
                 ((FEF-INI-PNTR FEF-INI-C-PNTR FEF-INI-OPT-SA FEF-INI-EFF-ADR)
                  (SETQ ADL (CDR ADL))))))))

;; Given a FEF and a PC, returns the corresponding 16-bit macro instruction.
;; There is no error checking.
(DEFUN DISASSEMBLE-FETCH (FEF PC &AUX IDX)
  (SETQ IDX (// PC 2))
  (COND ((ZEROP (LOGAND 1 PC))
	 (%P-LDB-OFFSET %%Q-LOW-HALF FEF IDX))
	((%P-LDB-OFFSET %%Q-HIGH-HALF FEF IDX))))
