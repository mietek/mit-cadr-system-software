;-*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;NOTES: THINK ABOUT 16-BIT STRINGS.

;The string functions:

;(STRING-LENGTH string) returns the number of characters in a given string.
;(SUBSTRING string from to) returns an arbitrary substring of a given string, copied.
; Omitting <to> means go all the way to the end of the string.
;(NSUBSTRING string from to) is like SUBSTRING, but returns a shared substring, not copied.
;(STRING-APPEND string ...) concatenates strings.
;(STRING-SEARCH-CHAR char string from to) searches a string for a given character.
;  Returns index if found, else NIL.
;(STRING-REVERSE-SEARCH-CHAR char string from to) searches backwards, as above.
;(STRING-SEARCH-NOT-CHAR char string from to) searches a string for anything other
;   than a given char.  Returns index if found, else NIL.
;(STRING-REVERSE-SEARCH-NOT-CHAR char string from to) searches backwards, as above.
;(STRING-SEARCH key string from to) searches for <key> in <string>.
;  Returns index if found, else NIL.
;(STRING-REVERSE-SEARCH key string from to) searches backwards for <key> in <string>.
;(STRING-SEARCH-SET charlist string from to) searches in <string> from <from>
; for a char in <charlist>.
;(STRING-SEARCH-NOT-SET charlist string from to) searches in <string> from <from>
; for a char not in <charlist>.
;(STRING-REVERSE-SEARCH-SET charlist string from to) searches backwards in <string>
; from <from> for a char in <charlist>.
;(STRING-REVERSE-SEARCH-NOT-SET charlist string from to) searches backwards in <string>
; from <from> for a char not in <charlist>.
;(STRING-TRIM charlist string) returns a copy of <string> with all leading and
; trailing members of <charlist> truncated.
;(STRING-LEFT-TRIM charlist string) is like STRING-TRIM but only hacks leading characters.
;(STRING-RIGHT-TRIM charlist string) is analogous.
;(STRING-NREVERSE string) reverses the elements of <string>, in place.
;(STRING-REVERSE string) returns a copy of <string> with the characters reversed.
;(STRING-UPCASE string) returns string copied and converted to all upper case.
;(STRING-DOWNCASE string) returns string copied and converted to all lower case.
;(CHAR-UPCASE char) returns the character converted to upper case.
;(CHAR-DOWNCASE char) returns the character converted to lower case.
;(STRING-LESSP s1 s2) says whether s1 is less than s2, in dictionary ordering.
;(ARRAY-TYPE array) returns the type of an array, as a symbol (eg, ART-STRING).
;(SUBSTRING-AFTER-CHAR char string) "" if char not in string.

;SUBSTRING and NSUBSTRING take an optional area argument.

;Note that most of the functions in this package will consider a number
;to be a string one character long.  However, they will never return
;a number instead of a string one character long.
;Symbols given as arguments will be converted into their pnames.


;String functions in other files (eventually move to this one?):

;STRING turns a character (ie, a number) into a string containing just that character.
; It turns a symbol into its pname.  A string is returned unchanged.
;STRINGP returns T if its arg is a string.
;MAKE-STRING takes a list of characters (numbers) and returns a string containing them.
; The inverse of MAKE-STRING is EXPLODEC, sort of.
; You probably want to use STRING-APPEND instead.
;STRING-EQUAL what EQUAL uses to compare strings

(DECLARE
  (OR (GET 'STRING 'Q-LAMBDA-LIST)			;This must be done to avoid
      (NOT (GET 'CAR 'SUBR))				;QCMP confusion and spurious errors.
      (DEFPROP STRING (STRING) Q-LAMBDA-LIST)))


; STRING-APPEND concatenates any number of strings.
; It will copy a single string.  Numbers and symbols are coerced.
; Actually, it will concatenate any type of arrays, making the result the same
; type as its first argument.

(DEFUN STRING-APPEND (&REST STRINGS &AUX (LENGTH 0) STRING COERCED (I 0))
    (DOLIST (S STRINGS)
      (SETQ LENGTH (+ LENGTH (STRING-LENGTH S))))
    (SETQ STRING (MAKE-ARRAY NIL
			     (IF (ARRAYP (CAR STRINGS)) (ARRAY-TYPE (CAR STRINGS))
				 'ART-STRING)
			     LENGTH))
    (DOLIST (S STRINGS)
      (COND ((NUMBERP S)
	     (ASET S STRING I)
	     (SETQ I (1+ I)))
	    (T (SETQ COERCED (STRING S))
	       (COPY-ARRAY-PORTION COERCED 0 (SETQ LENGTH (ARRAY-ACTIVE-LENGTH COERCED))
				   STRING I (SETQ I (+ I LENGTH))))))
    STRING)

(DEFUN STRING-NCONC (MUNG &REST STRINGS &AUX LEN FINAL-LEN S2LEN)
  "STRING-NCONC extends the first string and tacks on any number of additional strings.
   The first argument must be a string with a fill-pointer.
   Returns the first argument, which may have been moved and forwarded,
   just like ADJUST-ARRAY-SIZE."
  (SETQ FINAL-LEN (SETQ LEN (ARRAY-LEADER MUNG 0)))
  (DOLIST (STR2 STRINGS)
    (SETQ FINAL-LEN (+ FINAL-LEN (STRING-LENGTH STR2))))
  (AND (> FINAL-LEN (ARRAY-LENGTH MUNG))
       (ADJUST-ARRAY-SIZE MUNG FINAL-LEN))
  (DOLIST (STR2 STRINGS)
    (COND ((NUMBERP STR2)
	   (ARRAY-PUSH MUNG STR2)
	   (SETQ LEN (1+ LEN)))
	  (T
	   (SETQ STR2 (STRING STR2) S2LEN (ARRAY-ACTIVE-LENGTH STR2))
	   (COPY-ARRAY-PORTION STR2 0 S2LEN MUNG LEN (SETQ LEN (+ LEN S2LEN)))
	   (STORE-ARRAY-LEADER LEN MUNG 0))))
  MUNG)

; NSUBSTRING creates a substring of a given string.  The first
; arg is the string, the second is the FROM char-pos, and the third is the
; TO char-pos.  TO omitted or NIL means "all the way to the end",
; The substring is an index-offset array, so clobbering
; the original string will also clobber the substring.  Taking
; a substring of a substring will not produce a long chain; it
; will look at the given substring and create a new pointer to the
;  original array.

(DEFUN NSUBSTRING (STRING FROM &OPTIONAL TO (AREA NIL)
			  &AUX LENGTH ARRAYTYPE)
       (SETQ STRING (STRING STRING))
       (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
       (SETQ LENGTH (- TO FROM))
       (OR (AND (>= LENGTH 0) (>= FROM 0) (<= TO (ARRAY-ACTIVE-LENGTH STRING)))
	   (FERROR NIL "Args ~S and ~S out of range for ~S" FROM TO STRING))
       (SETQ ARRAYTYPE (ARRAY-TYPE STRING))
       (COND ((NOT (ARRAY-INDEXED-P STRING))
	      (MAKE-ARRAY AREA ARRAYTYPE
			  LENGTH			;ARRAY LENGTH
			  STRING			;DISPLACED
			  NIL				;NO LEADER
			  FROM))			;INDEX OFFSET
	     ;; OTHERWISE, PROBABLY A SUBSTRING OF A SUBSTRING
	     (T
	      (MAKE-ARRAY AREA ARRAYTYPE
			  LENGTH			;ARRAY LENGTH
			  (%P-CONTENTS-OFFSET STRING 1)	;POINT TO ARRAY POINTED
							;TO ORIGINALLY
			  NIL				;NO LEADER
			  (+ FROM
			     (%P-CONTENTS-OFFSET STRING 3))))))

;SUBSTRING copies out an arbitrary substring of a given string.
(DEFUN SUBSTRING (STRING FROM &OPTIONAL TO (AREA NIL))
  ;Nice and modular but conses up the wazoo
  ;(STRING-APPEND (NSUBSTRING STRING FROM TO))
  (SETQ STRING (STRING STRING))
  (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
  (OR (AND (>= TO FROM) (>= FROM 0) (<= TO (ARRAY-ACTIVE-LENGTH STRING)))
      (FERROR NIL "Args ~S and ~S out of range for ~S" FROM TO STRING))
  (LET ((RES (MAKE-ARRAY AREA (ARRAY-TYPE STRING) (- TO FROM))))
    (COPY-ARRAY-PORTION STRING FROM TO
			RES 0 (ARRAY-LENGTH RES))
    RES))

(DEFUN SUBSTRING-AFTER-CHAR (CHAR STRING &OPTIONAL (AREA NIL))
       (LET ((IDX (STRING-SEARCH-CHAR CHAR STRING)))
	 (COND ((NULL IDX) "")
	       (T (SUBSTRING STRING (1+ IDX) NIL AREA)))))

;STRING-LENGTH returns the length in characters of a given string.
(DEFUN STRING-LENGTH (STRING)
       (COND ((ARRAYP STRING) (ARRAY-ACTIVE-LENGTH STRING))
	     ((NUMBERP STRING) 1)
	     (T (ARRAY-ACTIVE-LENGTH (GET-PNAME STRING)))))

;RETURN SYMBOLIC NAME FOR THE TYPE OF A GIVEN ARRAY, TO MAKE MAKE-ARRAY HAPPY
;MAKE-ARRAY DOESN'T CARE, BUT USERS MAY WANT TO KNOW THE SYMBOLIC TYPE.
(DEFUN ARRAY-TYPE (ARRAY)
  (NTH (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0) ARRAY-TYPES))

;STRING-SEARCH-CHAR returns the index in STRING of the first occurrence of CHAR past FROM.
;Uses microcode assist now
(DEFUN STRING-SEARCH-CHAR (CHAR STRING &OPTIONAL (FROM 0) TO)
  (OR (STRINGP STRING) (SETQ STRING (STRING STRING)))
  (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
  (%STRING-SEARCH-CHAR CHAR STRING FROM TO))

(DEFUN STRING-REVERSE-SEARCH-CHAR (CHAR STRING &OPTIONAL FROM (TO 0))
       (SETQ STRING (STRING STRING))
       (OR FROM (SETQ FROM (ARRAY-ACTIVE-LENGTH STRING)))
       (DO ((I (1- FROM) (1- I)))
	   ((< I TO) NIL)
	   (AND (CHAR-EQUAL CHAR (AREF STRING I))
		(RETURN I))))

;STRING-SEARCH-NOT-CHAR returns the index in STRING of the first occurrence of
;  a character other than CHAR past FROM.
(DEFUN STRING-SEARCH-NOT-CHAR (CHAR STRING &OPTIONAL (FROM 0) TO)
       (SETQ STRING (STRING STRING))
       (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
       (DO ((I FROM (1+ I)))
	   ((>= I TO) NIL)
	   (OR (CHAR-EQUAL CHAR (AREF STRING I))
               (RETURN I))))

(DEFUN STRING-REVERSE-SEARCH-NOT-CHAR (CHAR STRING &OPTIONAL FROM (TO 0))
       (SETQ STRING (STRING STRING))
       (OR FROM (SETQ FROM (ARRAY-ACTIVE-LENGTH STRING)))
       (DO ((I (1- FROM) (1- I)))
	   ((< I TO) NIL)
	   (OR (CHAR-EQUAL CHAR (AREF STRING I))
               (RETURN I))))

;(STRING-SEARCH <key> <string> <from>) returns the index in <string> of the
;first occurrence of <key> past index <from>, or NIL if there is none.
;Uses microcode assist
(DEFUN STRING-SEARCH (KEY STRING &OPTIONAL (FROM 0) TO &AUX KEY-LEN)
  (SETQ STRING (STRING STRING) KEY (STRING KEY))
  (SETQ KEY-LEN (ARRAY-ACTIVE-LENGTH KEY))
  (COND ((ZEROP KEY-LEN) FROM)
	(T
	 (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
	 (SETQ TO (1+ (- TO KEY-LEN)))  ;Last position at which key may start +1
	 (PROG (CH1)
	   (COND ((MINUSP TO) (RETURN NIL)))
	   (SETQ CH1 (AREF KEY 0))
	  LOOP	;Find next place key might start
	   (OR (SETQ FROM (%STRING-SEARCH-CHAR CH1 STRING FROM TO))
	       (RETURN NIL))
	   (AND (%STRING-EQUAL KEY 0 STRING FROM KEY-LEN)
		(RETURN FROM))
	   (SETQ FROM (1+ FROM))  ;Avoid infinite loop.  %STRING-SEARCH-CHAR does right
	   (GO LOOP)))))	  ; thing if from  to.

;(STRING-REVERSE-SEARCH <key> <string> <from> <to>) returns the index in <string> of the
;last occurrence of <key> ending before index <from>, or NIL if there is none.
;Uses microcode assist
(DEFUN STRING-REVERSE-SEARCH (KEY STRING &OPTIONAL FROM (TO 0) &AUX KEY-LEN)
       (SETQ STRING (STRING STRING) KEY (STRING KEY))
       (SETQ KEY-LEN (ARRAY-ACTIVE-LENGTH KEY))
       (OR FROM (SETQ FROM (ARRAY-ACTIVE-LENGTH STRING)))
       (SETQ TO (+ TO (1- KEY-LEN)))  ;First position at which last char of key may be
       (COND ((ZEROP KEY-LEN) FROM)
	     (T
	      (DO ((N (1- FROM) (1- N))
		   (CH1 (AREF KEY (1- KEY-LEN))))
		  ((< N TO) NIL)
		  (AND (CHAR-EQUAL (AREF STRING N) CH1)
		       (%STRING-EQUAL KEY 0 STRING (1+ (- N KEY-LEN)) KEY-LEN)
		       (RETURN (1+ (- N KEY-LEN)))))
	      )))

;Return a copy of our argument, converted to all upper-case.
;Any bits in the characters, above the %%CH-CHAR part are ignored and not changed.
(DEFUN STRING-UPCASE (STRING)
  (SETQ STRING (STRING-APPEND STRING))		;Copy so we don't mung original string
  (DO ((LEN (ARRAY-ACTIVE-LENGTH STRING))
       (CHAR) (SUBCHAR)
       (I 0 (1+ I)))
      ((= I LEN))
    (SETQ CHAR (AREF STRING I)
	  SUBCHAR (LDB %%CH-CHAR CHAR))
    (AND (>= SUBCHAR #/a)
	 (<= SUBCHAR #/z)
	 (ASET (LOGXOR 40 CHAR) STRING I)))
  STRING)

;Return a copy of our argument, converted to all lower-case.
;Any bits in the characters, above the %%CH-CHAR part are ignored and not changed.
(DEFUN STRING-DOWNCASE (STRING)
  (SETQ STRING (STRING-APPEND STRING))		;Copy so we don't mung original string
  (DO ((LEN (ARRAY-ACTIVE-LENGTH STRING))
       (CHAR) (SUBCHAR)
       (I 0 (1+ I)))
      ((= I LEN))
    (SETQ CHAR (AREF STRING I)
	  SUBCHAR (LDB %%CH-CHAR CHAR))
    (AND (>= SUBCHAR #/A)
	 (<= SUBCHAR #/Z)
	 (ASET (LOGXOR 40 CHAR) STRING I)))
  STRING)

;Return the uppercase form of this character.
(DEFUN CHAR-UPCASE (CHAR &AUX SUBCHAR)
    (SETQ SUBCHAR (LDB %%CH-CHAR CHAR))
    (COND ((AND (>= SUBCHAR #/a)
		(<= SUBCHAR #/z))
	   (LOGXOR 40 CHAR))
	  (T CHAR)))

;Return the lowercase form of this character.
(DEFUN CHAR-DOWNCASE (CHAR &AUX SUBCHAR)
    (SETQ SUBCHAR (LDB %%CH-CHAR CHAR))
    (COND ((AND (>= SUBCHAR #/A)
		(<= SUBCHAR #/Z))
	   (LOGXOR 40 CHAR))
	  (T CHAR)))

;Reverse the characters in a string, in place.
(DEFUN STRING-NREVERSE (STRING &AUX LEN)
  (COND ((NUMBERP STRING) STRING)
	(T (SETQ STRING (STRING STRING))
	   (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING))
	   (DO ((I 0 (1+ I))
		(J (1- LEN) (1- J))
		(CHAR))
	       ((< J I))
	     (SETQ CHAR (AREF STRING I))
	     (ASET (AREF STRING J) STRING I)
	     (ASET CHAR STRING J))
	   STRING)))

;Make a reversed copy of a string
(DEFUN STRING-REVERSE (STRING)
       (STRING-NREVERSE (STRING-APPEND STRING)))

;STRING-SEARCH-SET returns the index in STRING of the first char which belongs to CHAR-LIST,
;or NIL if there is none.
(DEFUN STRING-SEARCH-SET (CHAR-LIST STRING &OPTIONAL (FROM 0) TO)
       (SETQ STRING (STRING STRING))
       (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
       (DO ((I FROM (1+ I)))
	   ((>= I TO) NIL)
	   (AND (MEM #'CHAR-EQUAL (AREF STRING I) CHAR-LIST)
		(RETURN I))))

;STRING-REVERSE-SEARCH-SET returns the index in STRING 
;of the char AFTER the last char which belongs to CHAR-LIST, or NIL if there is none.
(DEFUN STRING-REVERSE-SEARCH-SET (CHAR-LIST STRING &OPTIONAL FROM (TO 0))
       (SETQ STRING (STRING STRING))
       (OR FROM (SETQ FROM (ARRAY-ACTIVE-LENGTH STRING)))
       (DO ((I (1- FROM) (1- I)))
	   ((< I TO) NIL)
	   (AND (MEM #'CHAR-EQUAL (AREF STRING I) CHAR-LIST)
		(RETURN (1+ I)))))

;STRING-SEARCH-NOT-SET returns the index in STRING of the first char not in CHAR-LIST,
;or NIL if there is none.
(DEFUN STRING-SEARCH-NOT-SET (CHAR-LIST STRING &OPTIONAL (FROM 0) TO)
       (SETQ STRING (STRING STRING))
       (OR TO (SETQ TO (ARRAY-ACTIVE-LENGTH STRING)))
       (DO ((I FROM (1+ I)))
	   ((>= I TO) NIL)
	   (OR (MEM #'CHAR-EQUAL (AREF STRING I) CHAR-LIST)
	       (RETURN I))))

;STRING-REVERSE-SEARCH-NOT-SET returns the index in STRING 
;of the char AFTER the last char not in CHAR-LIST, or NIL if there is none.
(DEFUN STRING-REVERSE-SEARCH-NOT-SET (CHAR-LIST STRING &OPTIONAL FROM (TO 0))
       (SETQ STRING (STRING STRING))
       (OR FROM (SETQ FROM (ARRAY-ACTIVE-LENGTH STRING)))
       (DO ((I (1- FROM) (1- I)))
	   ((< I TO) NIL)
	   (OR (MEM #'CHAR-EQUAL (AREF STRING I) CHAR-LIST)
	       (RETURN (1+ I)))))

;Strip off the beginning and end of STRING all characters in CHAR-LIST.
(DEFUN STRING-TRIM (CHAR-LIST STRING &AUX I J)
       (SETQ I (STRING-SEARCH-NOT-SET CHAR-LIST STRING))
       (SETQ J (STRING-REVERSE-SEARCH-NOT-SET CHAR-LIST STRING))
       (COND (I (SUBSTRING STRING I J))
	     (T "")))

;Strip off the beginning of STRING all characters in CHAR-LIST.
(DEFUN STRING-LEFT-TRIM (CHAR-LIST STRING &AUX I)
    (SETQ I (STRING-SEARCH-NOT-SET CHAR-LIST STRING))
    (COND (I (SUBSTRING STRING I (STRING-LENGTH STRING)))
	  (T "")))

;Strip off the end of STRING all characters in CHAR-LIST.
(DEFUN STRING-RIGHT-TRIM (CHAR-LIST STRING &AUX I)
    (SETQ I (STRING-REVERSE-SEARCH-NOT-SET CHAR-LIST STRING))
    (COND (I (SUBSTRING STRING 0 I))
	  (T "")))

;Compare two strings using dictionary order.
(DEFUN STRING-LESSP (S1 S2)
    (PROG (IDX1 IDX2 LEN1 LEN2 CH1 CH2)
	  (SETQ S1 (STRING S1)
		S2 (STRING S2)
		LEN1 (ARRAY-ACTIVE-LENGTH S1)
		LEN2 (ARRAY-ACTIVE-LENGTH S2)
		IDX1 0
		IDX2 0)
     LOOP (AND (= IDX1 LEN1)
	       (RETURN (< IDX2 LEN2)))
	  (AND (= IDX2 LEN2)
	       (RETURN NIL))
	  (SETQ CH1 (AREF S1 IDX1)
		CH2 (AREF S2 IDX2))
	  (AND (CHAR-LESSP CH2 CH1)
	       (RETURN NIL))
	  (AND (CHAR-LESSP CH1 CH2)
	       (RETURN T))
	  (SETQ IDX1 (1+ IDX1)
		IDX2 (1+ IDX2))
	  (GO LOOP)))

;(WITH-INPUT-FROM-STRING (var string &optional index limit)
;   body)
;body is executed with var bound to a stream which reads
;from string (or the given substring)
;Value is value of last form in body.
;If index is included and non-NIL, it is a variable or SETF'able field which
; contains the index into the string.  It is not bound by the form
; and must be initialized on the outside to the starting index,
; usually zero.  Upon normal completion it contains the index
; of the first character not read (the length of the string if
; it read all the way to the end and got an eof).
;If limit is supplied non-NIL, it is the string length to use.
;string and limit are evaluated.
;A "downward closure" is used, so be careful what you do with var.
;You cannot save it away and cannot nest two of these and try
;to use both var's inside the inner one.
;Multiple values are returned from body only if there is no index.

(DEFMACRO WITH-INPUT-FROM-STRING ((VAR STRING INDEX LIMIT) . BODY)
  `(LET ((,VAR #'WITH-INPUT-FROM-STRING-INTERNAL-FUNCTION)
	 (WITH-INPUT-FROM-STRING-INTERNAL-STRING ,STRING)
	 (WITH-INPUT-FROM-STRING-INTERNAL-INDEX ,(OR INDEX 0))
	 (WITH-INPUT-FROM-STRING-INTERNAL-LIMIT ,LIMIT))
     ,(IF (NULL LIMIT)
	  `(SETQ WITH-INPUT-FROM-STRING-INTERNAL-LIMIT
		 (ARRAY-ACTIVE-LENGTH WITH-INPUT-FROM-STRING-INTERNAL-STRING))
	  `(OR WITH-INPUT-FROM-STRING-INTERNAL-LIMIT
	       (SETQ WITH-INPUT-FROM-STRING-INTERNAL-LIMIT
		     (ARRAY-ACTIVE-LENGTH WITH-INPUT-FROM-STRING-INTERNAL-STRING))))
     ,(IF (NULL INDEX) `(PROGN . ,BODY)
	  `(PROG1 (PROGN . ,BODY)
		  (SETF ,INDEX WITH-INPUT-FROM-STRING-INTERNAL-INDEX)))))

(SPECIAL WITH-INPUT-FROM-STRING-INTERNAL-STRING
	 WITH-INPUT-FROM-STRING-INTERNAL-INDEX
	 WITH-INPUT-FROM-STRING-INTERNAL-LIMIT)

(DEFUN WITH-INPUT-FROM-STRING-INTERNAL-FUNCTION (OP &OPTIONAL ARG1 &REST REST)
  (SELECTQ OP
    (:TYI (IF (< WITH-INPUT-FROM-STRING-INTERNAL-INDEX
		 WITH-INPUT-FROM-STRING-INTERNAL-LIMIT)
	      (PROG1 (AREF WITH-INPUT-FROM-STRING-INTERNAL-STRING
			   WITH-INPUT-FROM-STRING-INTERNAL-INDEX)
		     (SETQ WITH-INPUT-FROM-STRING-INTERNAL-INDEX
			   (1+ WITH-INPUT-FROM-STRING-INTERNAL-INDEX)))
	      (AND ARG1 (ERROR ARG1))))
    (:UNTYI (SETQ WITH-INPUT-FROM-STRING-INTERNAL-INDEX
		  (1- WITH-INPUT-FROM-STRING-INTERNAL-INDEX)))
    (:WHICH-OPERATIONS '(:TYI :UNTYI))
    (OTHERWISE (STREAM-DEFAULT-HANDLER
		   #'WITH-INPUT-FROM-STRING-INTERNAL-FUNCTION OP ARG1 REST))))

;(WITH-OUTPUT-TO-STRING (var &optional string index)
;   body)
;body is executed with var bound to a stream which outputs to
; a string.  If string and index are not supplied, the returned
; value is the string.  If string and index are supplied, string
; is evaluated to supply the string to be modified and index
; is a variable or SETF'able field which contains the index of the next character
; to be stored; it must be initialized on the outside and
; is updated upon normal exit.  In this case the returned value
; is the value of the last form in the body.
; If string is supplied and index is omitted, string is "nconced"
; onto.
; If string has an array-leader, the fill-pointer is updated.
; If the user does not supply a string, the system-supplied string
; will not have an array leader.
; A system-supplied string will be adjusted to just the right length,
; but a user-supplied string will not be adjusted downwards in size,
; only upwards, hence may end up bigger than necessary.
;A "downward closure" is used, so be careful what you do with var.
;You cannot save it away and cannot nest two of these and try
;to use both var's inside the inner one.
;Multiple values are never returned.

(DEFMACRO WITH-OUTPUT-TO-STRING ((VAR STRING INDEX) . BODY)
  `(LET ((,VAR #'WITH-OUTPUT-TO-STRING-INTERNAL-FUNCTION)
	 (WITH-OUTPUT-TO-STRING-INTERNAL-STRING
		,(OR STRING `(MAKE-ARRAY NIL 'ART-STRING 100)))
	 (WITH-OUTPUT-TO-STRING-INTERNAL-INDEX ,(OR INDEX 0)))
     (,(IF STRING 'PROG1 'PROGN) ;PROGN if have to return string as value
	    (PROGN . ,BODY)
	    ,(AND INDEX `(SETF ,INDEX WITH-OUTPUT-TO-STRING-INTERNAL-INDEX))
	    ,(IF STRING
		 `(AND (ARRAY-HAS-LEADER-P WITH-OUTPUT-TO-STRING-INTERNAL-STRING)
		       (STORE-ARRAY-LEADER WITH-OUTPUT-TO-STRING-INTERNAL-INDEX
					   WITH-OUTPUT-TO-STRING-INTERNAL-STRING
					   0))
		 `(ADJUST-ARRAY-SIZE WITH-OUTPUT-TO-STRING-INTERNAL-STRING
				     WITH-OUTPUT-TO-STRING-INTERNAL-INDEX))
	    WITH-OUTPUT-TO-STRING-INTERNAL-STRING)))

(SPECIAL WITH-OUTPUT-TO-STRING-INTERNAL-STRING
	 WITH-OUTPUT-TO-STRING-INTERNAL-INDEX)

(DEFUN WITH-OUTPUT-TO-STRING-INTERNAL-FUNCTION (OP &OPTIONAL ARG1 &REST REST)
  (SELECTQ OP
    (:TYO (OR (< WITH-OUTPUT-TO-STRING-INTERNAL-INDEX
		 (ARRAY-LENGTH WITH-OUTPUT-TO-STRING-INTERNAL-STRING))
	      (ADJUST-ARRAY-SIZE WITH-OUTPUT-TO-STRING-INTERNAL-STRING
				 (+ WITH-OUTPUT-TO-STRING-INTERNAL-INDEX 100)))
	  (ASET ARG1 WITH-OUTPUT-TO-STRING-INTERNAL-STRING
		     WITH-OUTPUT-TO-STRING-INTERNAL-INDEX)
	  (SETQ WITH-OUTPUT-TO-STRING-INTERNAL-INDEX
		(1+ WITH-OUTPUT-TO-STRING-INTERNAL-INDEX)))
    (:STRING-OUT (LET ((START (OR (CAR REST) 0))
		       (END (OR (CADR REST) (ARRAY-ACTIVE-LENGTH ARG1))))
		   (SETQ REST (+ (- END START)
				 WITH-OUTPUT-TO-STRING-INTERNAL-INDEX))
		   (OR (< REST (ARRAY-LENGTH WITH-OUTPUT-TO-STRING-INTERNAL-STRING))
		       (ADJUST-ARRAY-SIZE WITH-OUTPUT-TO-STRING-INTERNAL-STRING
			  (MAX (+ (ARRAY-LENGTH WITH-OUTPUT-TO-STRING-INTERNAL-STRING)
				  100)
			       REST)))
		   (COPY-ARRAY-PORTION ARG1 START END
				       WITH-OUTPUT-TO-STRING-INTERNAL-STRING
				       WITH-OUTPUT-TO-STRING-INTERNAL-INDEX REST)
		   (SETQ WITH-OUTPUT-TO-STRING-INTERNAL-INDEX REST)))
    (:LINE-OUT (LEXPR-FUNCALL #'WITH-OUTPUT-TO-STRING-INTERNAL-FUNCTION
			      ':STRING-OUT ARG1 REST)
	       (WITH-OUTPUT-TO-STRING-INTERNAL-FUNCTION ':TYO #\CR))
    (:WHICH-OPERATIONS '(:TYO :STRING-OUT :LINE-OUT))
    (OTHERWISE (STREAM-DEFAULT-HANDLER
		  #'WITH-OUTPUT-TO-STRING-INTERNAL-FUNCTION OP ARG1 REST))))
