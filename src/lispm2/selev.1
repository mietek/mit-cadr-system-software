;;; -*- Mode: LISP;  Package: SYSTEM-INTERNALS;  Base: 8 -*-

;;; Macros to do things similar to BLISS' SELECT.

(DEFMACRO-DISPLACE COND-EVERY (&REST CLAUSES)
  "COND-EVERY has a COND-like syntax.  Unlike COND, though, it executes all the
clauses whose tests succede.  It also recognizes two special keywords (instead of
a test):  :ALWAYS executes in all cases, and :OTHERWISE executes if no previous
clause has executed.  The value returned is that of the last clause executed,
or NIL if no clauses executed,  and the macro will not return multiple-values."
  (LET ((FLAG (GENSYM))
	(VALUE (GENSYM)))
    `(LET ((,FLAG) (,VALUE))
       ,@(DO ((CS CLAUSES (CDR CS))
	      (CLAUSE (CAR CLAUSES) (CADR CS))
	      (FORMS NIL)
	      (SEEN-OTHERWISE-OR-ALWAYS))
	     ((NULL CS) (NREVERSE FORMS))
	   (PUSH
	    (SELECTQ (CAR CLAUSE)
	      ((:ALWAYS T)
	       (SETQ SEEN-OTHERWISE-OR-ALWAYS ':ALWAYS)
	       `(SETQ ,VALUE (PROGN . ,(CDR CLAUSE))))
	      ((:OTHERWISE)
	       (IF SEEN-OTHERWISE-OR-ALWAYS
		   (FERROR NIL ":OTHERWISE after a previous :OTHERWISE or :ALWAYS")
		   (SETQ SEEN-OTHERWISE-OR-ALWAYS ':OTHERWISE)
		   `(OR ,FLAG
			(SETQ ,VALUE (PROGN . ,(CDR CLAUSE))))))
	      (OTHERWISE
	       `(AND ,(CAR CLAUSE)
		     (SETQ ,VALUE (PROGN . ,(CDR CLAUSE))
			   . ,(IF SEEN-OTHERWISE-OR-ALWAYS
				  NIL
				  `(,FLAG T))))))
	    FORMS))
       ,VALUE)))

(DEFMACRO-DISPLACE SELECTQ-EVERY (OBJ . CLAUSES)
  "Just like COND-EVERY but with SELECTQ-like syntax."
  (IF (ATOM OBJ)
      (SELECTQ-EVERY-GENERATE-CODE OBJ CLAUSES)
      (LET ((SYM (GENSYM)))
	`(LET ((,SYM ,OBJ))
	   ,(SELECTQ-EVERY-GENERATE-CODE SYM CLAUSES)))))

(DEFUN SELECTQ-EVERY-GENERATE-CODE (COMPARE-AGAINST CLAUSES)
  `(COND-EVERY
    . ,(DO ((CS CLAUSES (CDR CS))
	    (CLAUSE (CAR CLAUSES) (CADR CS))
	    (FORMS NIL))
	   ((NULL CS) (NREVERSE FORMS))
	   (PUSH
	    (COND ((MEMQ (CAR CLAUSE) '(:OTHERWISE :ALWAYS T))
		   CLAUSE)
		  (T
		   `((,(IF (LISTP (CAR CLAUSE)) 'MEMQ 'EQ) ,COMPARE-AGAINST ',(CAR CLAUSE))
		     . ,(CDR CLAUSE))))
	    FORMS))))
