;;; -*- Mode: Lisp; Package: System-Internals -*-

;;; Miniature Chaosnet program.  Only good for reading ascii and binary files.

(DECLARE (SPECIAL MINI-PKT MINI-PKT-STRING MINI-FILE-ID MINI-OPEN-P MINI-CH-IDX MINI-UNRCHF
		  MINI-LOCAL-INDEX MINI-LOCAL-HOST MINI-REMOTE-INDEX MINI-REMOTE-HOST
		  MINI-IN-PKT-NUMBER MINI-OUT-PKT-NUMBER MINI-EOF-SEEN))

;;; Initialization, usually only called once.
(DEFUN MINI-INIT ()
  ;; Init lists microcode looks at
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-FREE-LIST) NIL)
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-RECEIVE-LIST) NIL)
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-TRANSMIT-LIST) NIL)
  ;; Fake up a packet buffer for the microcode, locations 200-x through 377
  (%P-STORE-TAG-AND-POINTER 177 DTP-ARRAY-HEADER
			    (DPB 1 %%ARRAY-NUMBER-DIMENSIONS
				 (DPB 400 %%ARRAY-INDEX-LENGTH-IF-SHORT
				      (DPB 1 %%ARRAY-LEADER-BIT
					   ART-16B))))
  (%P-STORE-TAG-AND-POINTER 176 DTP-FIX (LENGTH CHAOS-BUFFER-LEADER-QS))
  (%P-STORE-TAG-AND-POINTER (- 176 1 (LENGTH CHAOS-BUFFER-LEADER-QS))
			    DTP-HEADER
			    (DPB %HEADER-TYPE-ARRAY-LEADER %%HEADER-TYPE-FIELD
				 (+ 2 (LENGTH CHAOS-BUFFER-LEADER-QS))))
  (SETQ MINI-PKT (%MAKE-POINTER DTP-ARRAY-POINTER 177))
  (SETQ MINI-PKT-STRING (MAKE-ARRAY NIL 'ART-8B 760 204)) ;Just the data part of the packet
  (OR (BOUNDP 'MINI-LOCAL-INDEX)
      (SETQ MINI-LOCAL-INDEX 0))
  (SETQ MINI-OPEN-P NIL))

;;; Get a connection to a file server
(DEFUN MINI-OPEN-CONNECTION (HOST CONTACT-NAME)
  (OR (BOUNDP 'MINI-PKT) (MINI-INIT))
  (SETQ MINI-LOCAL-HOST (%UNIBUS-READ 764142)
	MINI-REMOTE-HOST HOST
	MINI-OUT-PKT-NUMBER 1)
  (SETQ MINI-LOCAL-INDEX (1+ MINI-LOCAL-INDEX))
  (AND (= MINI-LOCAL-INDEX 200000) (SETQ MINI-LOCAL-INDEX 1))
  (SETQ MINI-REMOTE-INDEX 0
	MINI-IN-PKT-NUMBER 0)
  (DO ((RETRY-COUNT 10. (1- RETRY-COUNT)))
      ((ZEROP RETRY-COUNT) (MINI-BARF "RFC fail"))
    ;; Store contact name into packet
    (COPY-ARRAY-CONTENTS CONTACT-NAME MINI-PKT-STRING)
    (MINI-SEND-PKT 1 (ARRAY-LENGTH CONTACT-NAME))  ;Send RFC
    (COND ((EQ (MINI-NEXT-PKT NIL) 2)	;Look for a response of OPN
	   (SETQ MINI-REMOTE-INDEX (AREF MINI-PKT 5)
		 MINI-IN-PKT-NUMBER (AREF MINI-PKT 6))
	   (SETQ MINI-OUT-PKT-NUMBER (1+ MINI-OUT-PKT-NUMBER))
	   (MINI-SEND-STS)
	   (SETQ MINI-OPEN-P T)
	   (RETURN T)))))	;and exit.  Otherwise, try RFC again.

;;; Send a STS
(DEFUN MINI-SEND-STS ()
  (ASET MINI-IN-PKT-NUMBER MINI-PKT 10) ;Receipt
  (ASET 1 MINI-PKT 11) ;Window size
  (MINI-SEND-PKT 7 4)) ;STS

;;; Open a file for read
(DEFUN MINI-OPEN-FILE (FILENAME BINARY-P)
  (SETQ MINI-CH-IDX 1000 MINI-UNRCHF NIL MINI-EOF-SEEN NIL)
  (OR MINI-OPEN-P
      (MINI-OPEN-CONNECTION 2026 "MINI")) ;Open server on AI
  (DO ((OP)) ;Retransmission loop
      (NIL)
    ;; Send opcode 200 (ascii open) or 201 (binary open) with file name
    (COPY-ARRAY-CONTENTS FILENAME MINI-PKT-STRING)
    (MINI-SEND-PKT (IF BINARY-P 201 200) (ARRAY-ACTIVE-LENGTH FILENAME))
    ;; Get back opcode 202 (win) or 203 (lose) or OPN if old STS lost
    (SETQ OP (MINI-NEXT-PKT NIL))
    (COND ((NULL OP))		;no response, retransmit
	  ((= OP 2)		;OPN
	   (MINI-SEND-STS))	;send STS and then retransmit
	  ((OR (= OP 202) (= OP 203)) ;Win or Lose
	   (SETQ MINI-IN-PKT-NUMBER (LOGAND 177777 (1+ MINI-IN-PKT-NUMBER))
		 MINI-OUT-PKT-NUMBER (LOGAND 177777 (1+ MINI-OUT-PKT-NUMBER)))
	   (SETQ MINI-FILE-ID (MAKE-ARRAY NIL 'ART-STRING (LOGAND 7777 (AREF MINI-PKT 1))))
	   (COPY-ARRAY-CONTENTS MINI-PKT-STRING MINI-FILE-ID)
	   (MINI-SEND-STS)	;Acknowledge packet just received
	   (COND ((= OP 202)
		  (RETURN T))
		 (T ;Lose
		  (MINI-BARF MINI-FILE-ID FILENAME))))))
  (IF BINARY-P #'MINI-BINARY-STREAM #'MINI-ASCII-STREAM))

;; Doesn't use symbols for packet fields since not loaded yet
;; This sends a packet and doesn't return until it has cleared microcode.
;; You fill in the data part before calling, this fills in the header.
(DEFUN MINI-SEND-PKT (OPCODE N-BYTES)
  (ASET (LSH OPCODE 8) MINI-PKT 0)
  (ASET N-BYTES MINI-PKT 1)
  (ASET MINI-REMOTE-HOST MINI-PKT 2)
  (ASET MINI-REMOTE-INDEX MINI-PKT 3)
  (ASET MINI-LOCAL-HOST MINI-PKT 4)
  (ASET MINI-LOCAL-INDEX MINI-PKT 5)
  (ASET MINI-OUT-PKT-NUMBER MINI-PKT 6) ;PKT#
  (ASET MINI-IN-PKT-NUMBER MINI-PKT 7)  ;ACK#
  (LET ((WC (+ 8 (// (1+ N-BYTES) 2) 1))) ;Word count including header and hardware dest word
    (STORE-ARRAY-LEADER WC MINI-PKT %CHAOS-LEADER-WORD-COUNT)
    (ASET (DPB (LDB 1010 MINI-LOCAL-HOST) 1010 MINI-REMOTE-HOST) ;El cheapo routing
          MINI-PKT (1- WC))) ;Store hardware destination
  (STORE-ARRAY-LEADER NIL MINI-PKT %CHAOS-LEADER-THREAD)
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-TRANSMIT-LIST) MINI-PKT)
  (%CHAOS-WAKEUP)
  (DO ()	;Await completion of transmission
      ((NULL (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-TRANSMIT-LIST))))
  ;; Disallow use of the packet by the receive side, flush any received packet that snuck in
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-FREE-LIST) NIL)
  (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-RECEIVE-LIST) NIL))

;; Return opcode of next packet other than those that are no good.
;; If the arg is NIL, can return NIL if no packet arrives after a while.
;; If T, waits forever.  Return value is the opcode of the packet in MINI-PKT.
(DEFUN MINI-NEXT-PKT (MUST-RETURN-A-PACKET &AUX OP)
  (DO ((TIMEOUT 20. (1- TIMEOUT)))	;A couple seconds
      ((AND (ZEROP TIMEOUT) (NOT MUST-RETURN-A-PACKET)) NIL)
    ;; Enable microcode to receive a packet
    (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-RECEIVE-LIST) NIL)
    (STORE-ARRAY-LEADER NIL MINI-PKT %CHAOS-LEADER-THREAD)
    (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-FREE-LIST) MINI-PKT)
    (%CHAOS-WAKEUP)
    (DO ((N 2000. (1- N)))	;Give it time
	((OR (ZEROP N) (SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-RECEIVE-LIST))))
    (COND ((SYSTEM-COMMUNICATION-AREA %SYS-COM-CHAOS-RECEIVE-LIST)
	   (SETQ OP (LSH (AREF MINI-PKT 0) -8))
	   (COND ((AND (NOT (LDB-TEST %%CHAOS-CSR-CRC-ERROR
				       (ARRAY-LEADER MINI-PKT %CHAOS-LEADER-CSR-1)))
		       (NOT (LDB-TEST %%CHAOS-CSR-CRC-ERROR
				       (ARRAY-LEADER MINI-PKT %CHAOS-LEADER-CSR-2)))
		       (= (LDB 0004 (ARRAY-LEADER MINI-PKT %CHAOS-LEADER-BIT-COUNT)) 0)
		       (>= (ARRAY-LEADER MINI-PKT %CHAOS-LEADER-BIT-COUNT) 48.)
		       (ZEROP (LOGAND 377 (AREF MINI-PKT 0)))	;Header version 0
		       (= (// (ARRAY-LEADER MINI-PKT %CHAOS-LEADER-BIT-COUNT)
			      20)
			  (+ 10	;FIRST-DATA-WORD-IN-PKT
			     (LSH (1+ (LDB 14 (AREF MINI-PKT 1))) -1)  ;PKT-NWORDS
			     3))	;HEADER
		       (= (AREF MINI-PKT 2) MINI-LOCAL-HOST)
		       (= (AREF MINI-PKT 3) MINI-LOCAL-INDEX)
		       (OR (AND (MEMQ OP '(14 202 203 200 300))  ;EOF, win, lose, data
				(= (AREF MINI-PKT 6) (LOGAND 177777 (1+ MINI-IN-PKT-NUMBER))))
			   (MEMQ OP '(2 3 11))))  ;OPN, CLS, LOS
		  ;; This packet not to be ignored, return to caller
		  (COND ((MEMQ OP '(3 11))  ;CLS, LOS
			 (LET ((MSG (MAKE-ARRAY NIL 'ART-STRING
						(LOGAND 7777 (AREF MINI-PKT 1)))))
			   (COPY-ARRAY-CONTENTS MINI-PKT-STRING MSG)
			   (MINI-BARF "Connection broken" MSG))))
		  (RETURN OP)))
	   ;; This packet to be ignored, get another
	   (AND MINI-OPEN-P		;Could be getting a retransmission of
		(MINI-SEND-STS))	; an old pkt due to lost STS
	   ))))

;Stream which does only 16-bit TYI, doesn't even need UNTYI
(DEFUN MINI-BINARY-STREAM (OP &OPTIONAL IGNORE)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYI))
    (:TYI (COND ((< MINI-CH-IDX (// (LOGAND 7777 (AREF MINI-PKT 1)) 2))
		 (PROG1 (AREF MINI-PKT (+ 10 MINI-CH-IDX))
			(SETQ MINI-CH-IDX (1+ MINI-CH-IDX))))
		(T ;Get another packet
		 (MINI-SEND-STS)  ;Acknowledge packet just processed
		 (SETQ OP (MINI-NEXT-PKT T))
		 (SETQ MINI-IN-PKT-NUMBER (LOGAND 177777 (1+ MINI-IN-PKT-NUMBER)))
		 (COND ((= OP 14) ;EOF
			(MINI-SEND-STS) ;Acknowledge the EOF
			(SETQ MINI-EOF-SEEN T)
			NIL)		;and tell caller
		       ((= OP 300) ;Data
			(SETQ MINI-CH-IDX 0)
			(MINI-BINARY-STREAM ':TYI))
		       (T (MINI-BARF "Bad opcode received" OP))))))
    (OTHERWISE (MINI-BARF "Unknown stream operation" OP))))

(DEFUN MINI-ASCII-STREAM (OP &OPTIONAL ARG1)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYI :UNTYI))
    (:TYI (COND (MINI-UNRCHF
		 (PROG1 MINI-UNRCHF (SETQ MINI-UNRCHF NIL)))
		((< MINI-CH-IDX (LOGAND 7777 (AREF MINI-PKT 1)))
		 (PROG1 (AREF MINI-PKT-STRING MINI-CH-IDX)
			(SETQ MINI-CH-IDX (1+ MINI-CH-IDX))))
		(T ;Get another packet
		 (MINI-SEND-STS)  ;Acknowledge packet just processed
		 (SETQ OP (MINI-NEXT-PKT T))
		 (SETQ MINI-IN-PKT-NUMBER (LOGAND 177777 (1+ MINI-IN-PKT-NUMBER)))
		 (COND ((= OP 14) ;EOF
			(MINI-SEND-STS) ;Acknowledge the EOF
			(SETQ MINI-EOF-SEEN T)
			(AND ARG1 (ERROR ARG1))
			NIL)		;and tell caller
		       ((= OP 200) ;Data
			(SETQ MINI-CH-IDX 0)
			(MINI-ASCII-STREAM ':TYI))
		       (T (MINI-BARF "Bad opcode received" OP))))))
    (:UNTYI (SETQ MINI-UNRCHF ARG1))
    (OTHERWISE (MINI-BARF "Unknown stream operation" OP))))

(DEFUN MINI-BARF (&REST ARGS)
  (TERPRI)
  (PRINC "MINI: ")
  (DOLIST (ARG ARGS)
    (PRINC ARG)
    (TYO #\SP))
  (SETQ MINI-OPEN-P NIL)	;Force re-open of connection
  (BREAK MINI-BARF))

;;; Higher-level stuff

(DEFUN MINI-LOAD (FILE-NAME PKG &AUX LEN)
  (COND ((AND (>= (SETQ LEN (ARRAY-ACTIVE-LENGTH FILE-NAME)) 6)
	      (STRING-EQUAL FILE-NAME "QFASL" (- LEN 5) 0 LEN 5))
	 (MINI-FASLOAD FILE-NAME PKG))
	((MINI-READFILE FILE-NAME PKG))))

(DECLARE (SPECIAL FASL-STREAM FASLOAD-FILE-PROPERTY-LIST-FLAG FASL-GROUP-DISPATCH
                  FASL-OPS FASL-FILE-GROUP-SYMBOL FDEFINE-FILE-SYMBOL
		  FASL-STREAM-BYPASS-P))

(DECLARE (SPECIAL ACCUMULATE-FASL-FORMS))

(DEFUN MINI-FASLOAD (FILE-NAME PKG
		     &AUX FASL-STREAM W1 W2 FILE-SYMBOL FDEFINE-FILE-SYMBOL
		          FASL-FILE-GROUP-SYMBOL FASLOAD-FILE-PROPERTY-LIST-FLAG
			  (FASL-TABLE NIL)
			  (FASL-STREAM-BYPASS-P NIL))
      (SETQ FILE-SYMBOL (FASL-START FILE-NAME))
      ;;Open the input stream in binary mode, and start by making sure
      ;;the file type in the first word is really SIXBIT/QFASL/.
      (SETQ FASL-STREAM (MINI-OPEN-FILE FILE-NAME T))
      (SETQ W1 (FUNCALL FASL-STREAM ':TYI)
	    W2 (FUNCALL FASL-STREAM ':TYI))
      (COND ((AND (= W1 143150) (= W2 71660))		;If magic ID checks,
	     (LET ((PACKAGE (IF (FBOUNDP 'INTERN-LOCAL)	;If packages exist now 
				(PKG-FIND-PACKAGE PKG)
				NIL)))
	       (FASL-TOP-LEVEL)			;load it.
	       ;; Doesn't really read to EOF, must read rest to avoid getting out of phase
	       (DO () (MINI-EOF-SEEN)
		 (FUNCALL FASL-STREAM ':TYI))
	       (AND PACKAGE ;If packages don't exist yet, will be fixed later
		    (SET-FILE-LOADED-ID FILE-SYMBOL MINI-FILE-ID PACKAGE))))
	    ((FERROR NIL "~A is not a QFASL file" FILE-NAME)))	;Otherwise, barf out.
      FILE-NAME)

(DEFUN MINI-READFILE (FILE-NAME PKG)
  (MULTIPLE-VALUE-BIND (FILE-SYMBOL FDEFINE-FILE-SYMBOL)
	      (GET-FILE-SYMBOLS FILE-NAME)
    (LET ((EOF '(()))
	  (STANDARD-INPUT (MINI-OPEN-FILE FILE-NAME NIL)))
      (LET ((PACKAGE (PKG-FIND-PACKAGE PKG)))
	 (DO FORM (READ STANDARD-INPUT EOF) (READ STANDARD-INPUT EOF) (EQ FORM EOF)
	   (EVAL FORM))
	 (SET-FILE-LOADED-ID FILE-SYMBOL MINI-FILE-ID PACKAGE)
	 ))))

;;; Filename-parsing utilities which need to be in the cold load.

;; Given a file name, return two symbols, first for the specific file
;; and second for the group of files with that FN1, (FN2 will be ">")
;; Must work both before and after packages exist.
;; Must work if STRING or flavors not loaded, we use some kludges.
(DEFUN GET-FILE-SYMBOLS (FILE-NAME)
  (AND (STRINGP FILE-NAME)
       (SETQ FILE-NAME (FILE-PARSE-NAME FILE-NAME)))
  (FUNCALL FILE-NAME ':FILE-SYMBOLS))

;Convert a pathname string into a path list: (dev dir fn1 fn2).
;The elements of a path list are strings, or NIL for an
;unspecified position.
;Slash and control-Q () are quoting characters.  Colon, semicolon, space
;and tab separate filename components.
(DEFUN FILE-SPREAD-ITS-PATHNAME (PATHNAME &AUX DEV DEV-A DIR FN1 FN2)
  (COND ((SYMBOLP PATHNAME)
	 (SETQ PATHNAME (GET-PNAME PATHNAME))))
  (PROG ()
    (COND ((STRINGP PATHNAME)
	   (DO ((I 0) (CH) (TEM) (NEXT) (LEN (STRING-LENGTH PATHNAME)) (J 0 (1+ J)))
	       ((> J LEN))
	     (SETQ CH (COND ((= J LEN) #\SP)
			    (T (AR-1 PATHNAME J))))
	     (COND ((STRING-SEARCH-CHAR CH "//")
		    (SETQ J (1+ J)))
		   ;; Last two characters of the string are space and tab.
		   ((SETQ TEM (STRING-SEARCH-CHAR CH ":; 	"))
		    (SETQ NEXT (STRING-UPCASE (SUBSTRING PATHNAME I J)))
		    (COND ((NOT (ZEROP (STRING-LENGTH NEXT)))
			   (SELECTQ TEM
			     (0 (AND DEV (SETQ DEV-A DEV))
				(SETQ DEV NEXT))
			     (1 (SETQ DIR NEXT))
			     ((2 3) (COND (FN2)
					  (FN1 (SETQ FN2 NEXT))
					  (T (SETQ FN1 NEXT)))))))
		    (SETQ I (1+ J)))))
	   (RETURN (LIST DEV DIR FN1 FN2) (OR DEV-A DEV)))
	  ((LISTP PATHNAME)			;MACLISP FILE-LISTS
	   (RETURN
	     (MAPCAR #'(LAMBDA (X) (AND X (STRING X))) ;LEAVE NILS FOR UNSPECIFIED COMPONENTS
		     (COND ((LISTP (CAR PATHNAME))
			    (COND ((CDAR PATHNAME)
				   (LIST (CAAR PATHNAME) (CADAR PATHNAME)  ;BOTH DEV AND DIR
					 (CADR PATHNAME) (CADDR PATHNAME)))
				  (T (LIST NIL (CAAR PATHNAME)	;JUST DIR
					   (CADR PATHNAME) (CADDR PATHNAME)))))
			   (T (LIST (CADDR PATHNAME) (CADDDR PATHNAME)     ;N1 N2 DEV DIR
				    (CAR PATHNAME) (CADR PATHNAME)))))
	     NIL))
	  (T (FERROR NIL "~S is not an acceptable pathname" PATHNAME)))))

;;; Temporary definition, only work for ITS, no defaulting, etc.
(LOCAL-DECLARE ((SPECIAL THE-FILE-NAME))
(DEFUN FILE-PARSE-NAME (THE-FILE-NAME &REST IGNORE)
  (IF (NOT (STRINGP THE-FILE-NAME)) THE-FILE-NAME
      (CLOSURE '(THE-FILE-NAME) 'COLD-PARSE-FILE-NAME-INTERNAL)))

(DEFUN COLD-PARSE-FILE-NAME-INTERNAL (OP &REST REST)
  (SELECTQ OP
    (:TYPE (FOURTH (FILE-SPREAD-ITS-PATHNAME THE-FILE-NAME)))
    ((:STRING-FOR-PRINTING :STRING-FOR-HOST :STRING-FOR-WHOLINE) THE-FILE-NAME)
    (:OPEN 
     (LEXPR-FUNCALL #'FS:OPEN-CHAOS (FIRST (FILE-SPREAD-ITS-PATHNAME THE-FILE-NAME))
		    'COLD-PARSE-FILE-NAME-INTERNAL REST))
    (:FILE-SYMBOLS
     (PROG (FILE-SYMBOL FILE-GROUP-SYMBOL)
       (SETQ FILE-GROUP-SYMBOL (COND ((FBOUNDP 'NSUBSTRING)
				      (LET ((PATH (FILE-SPREAD-ITS-PATHNAME THE-FILE-NAME)))
					(STRING-APPEND (FIRST PATH) ": "
						       (SECOND PATH) "; "
						       (THIRD PATH) " >")))
				     ((STRING-EQUAL THE-FILE-NAME "AI: LISPM; QFCTNS QFASL")
				      "AI: LISPM; QFCTNS >")
				     ((STRING-EQUAL THE-FILE-NAME "AI: LISPM2; STRING QFASL")
				      "AI: LISPM2; STRING >")
				     (T (FERROR NIL "File not known" THE-FILE-NAME))))
       (COND ((FBOUNDP 'INTERN-LOCAL)
	      (SETQ FILE-SYMBOL (INTERN-LOCAL THE-FILE-NAME PKG-FILE-PACKAGE))
	      (SETQ FILE-GROUP-SYMBOL (INTERN-LOCAL FILE-GROUP-SYMBOL PKG-FILE-PACKAGE)))
	     (T
	      (SETQ FILE-SYMBOL (INTERN THE-FILE-NAME))
	      (SETQ FILE-GROUP-SYMBOL (INTERN FILE-GROUP-SYMBOL))
	      (RPLACA (PACKAGE-CELL-LOCATION FILE-SYMBOL) 'FILES)
	      (RPLACA (PACKAGE-CELL-LOCATION FILE-GROUP-SYMBOL) 'FILES)))
       (RETURN FILE-SYMBOL FILE-GROUP-SYMBOL)))))
);LOCAL-DECLARE

 ;Note that SETQ may not be used in the below
(ADD-INITIALIZATION "MINI" '(SET' MINI-OPEN-P NIL) '(WARM FIRST))
