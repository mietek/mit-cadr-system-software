;This file contains the Lisp machine Lisp compiler peephole optimizer.  -*-LISP-*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

(declare (cond ((status feature lispm))
	       ((null (memq 'newio (status features)))
		(break 'you-have-to-compile-this-with-qcompl t))
	       ((null (get 'if-for-maclisp 'macro))
		(load '(macros > dsk lispm))
		(load '(defmac fasl dsk lispm2))
		(load '(lmmac > dsk lispm2)))))

(declare (setq run-in-maclisp-switch t))

(defun peep (code)
    (peep-branches (cdr (memq 'progsa code)))
    code)

;;; Perform peephole optimization including unreferenced tags, and branch tensioning.
(defun peep-branches (code)
    (do ((retry t)) ((not retry))
       (setq retry nil)

       ;; Remove unused tags first, since that can facilitate non-branch optimization,
       ;; whereas non-branch optimization can't remove references to tags.
       (peep-tags code)
       (peep-non-branches code)

       ;; Now perform branch optimization.
       ;; If anything is done, we must perform the previous two steps over again.
       (prog (tem tem1 tag tail)
	    (setq tail code)

	    loop
	    ;; Find next branch.
	    (or tail (return nil))
	    (and (or (atom (car tail))
		     (neq (caar tail) 'branch))
		 (progn (pop tail) (go loop)))
	    ;; Extract tag name.
	    (setq tag (fifth (car tail)))
	    ;; Find insn following the branch, or else find that the tag comes between.
	    (setq tem (peep-find-next (cdr tail) tag))
	    ;; if the tag comes before the next insn, this is BRANCH .+1, so flush it.
	    (cond ((eq tem tag)
		   (rplaca tail nil)
		   (pop tail)
                   (peep-can-drop-through tail)
		   (setq retry t)
		   (go loop)))
	    ;; Find the second following instruction, or find that the tag precedes it.
	    (setq tem1 (peep-find-next (cdr tem) tag))
	    ;; If the tag precedes the second following insn, this is BRANCH .+2.
	    ;; If we have BRANCH-CONDITIONAL .+2 ? BRANCH-ALWAYS FOO
	    ;; turn it into BRANCH-INVERSE-CONDITIONAL FOO.
	    (cond ((and (eq tem1 tag) (eq (caar tem) 'branch) (eq (cadar tem) 'always))
		   (rplaca (cddddr (car tail)) (fifth (car tem)))
		   (rplaca (cddar tail) (other (caddar tail)))
		   (rplaca tem nil)
                   (peep-can-drop-through (cdr tail))
		   (setq retry t)
		   (go loop)))
	    ;; Find where the tag appears, then find the next instruction,
	    ;; to tension branches to unconditional branches.
            ;; Unconditional branches to returns can also be tensioned.
	    (setq tem (peep-find-next (memq tag code) nil))
	    (cond ((and (eq (caar tem) 'branch) (eq (cadar tem) 'always))
		   (rplaca (cddddr (car tail)) (fifth (car tem)))
		   (setq retry t)
		   (go loop))
                  ((and (eq (cadar tail) 'always)
                        (eq (cadar tem) 'd-return)
                        (memq (caar tem) '(move car cdr caar cadr cdar cddr misc false true call0)))
                   (rplaca tail (car tem))
                   (setq retry t)))
	    (pop tail)
	    (go loop))))

;;; Flush any (NO-DROP-THROUGH)s following a deleted branch.
(defun peep-can-drop-through (code)
    (do ((tail code (cdr tail))) ((null tail))
       (or (atom (car tail))
           (cond ((eq (caar tail) 'no-drop-through)
                  (rplaca tail nil))
                 ((eq (caar tail) 'comment))
                 (t (return nil))))))

;;; Find the next actual instruction in the list called tail
;;; and return the tail of tail that points at it.
;;; If tag is non-nil, then also stop if we find tag as an element
;;; of the list before the next instruction, but in that case return the tag.
(defun peep-find-next (ugh tag)
    (do ((tail ugh (cdr tail))) ((null tail) nil)
       (cond ((atom (car tail))
	      (and tag (eq (car tail) tag) (return tag)))
	     ((memq (caar tail) '(no-drop-through comment)))
	     (t (return tail)))))

;;; Delete dead code and perform all optimizations on non-branch instructions.
(defun peep-non-branches (code)
   (prog (x y z x-ptr y-ptr z-ptr)
	 (setq z-ptr code)
	 (go refill-y)

	 delete-z
	 (rplaca z-ptr nil)
	 (go refill-z)

	 refill-x
	 (setq x-ptr y-ptr x (car x-ptr))

	 refill-y
	 (setq y-ptr z-ptr y (car y-ptr))

	 refill-z
	 (or (setq z-ptr (cdr z-ptr)) (return code))
	 (or (setq z (car z-ptr)) (go refill-z))

	 reconsider

	 ;; Take care of tags.  A tag stops all optimization across it,
	 ;; so at a tag we empty the stream and start refilling it
	 ;; until (at least) we have skipped all the tags and found
	 ;; two instructions in y and z again.
	 (cond ((atom z)
		(setq x nil y nil)
		(go refill-z))
	       ((null y)
		(go refill-y)))


	 ;; Ok, we have instructions in Y and Z, but X may be NIL.
	 (cond ((equal y '(no-drop-through))
		;; Delete dead code following a (NO-DROP-THROUGH)
		(go delete-z))
	       ((eq (car z) 'move)
		(and (equal (caddr y) (caddr z))
		     (cond ((and (eq (cadr z) 'd-inds)
				 (memq (car y) '(pop sete move movem setnil setzero)))
			    ;; Delete MOVE D-INDS X after something that stores or fetches X.
			    (go delete-z))
			   ((and (eq (cadr z) 'd-pdl)
				 (eq (car y) 'pop))
			    ;; Turn POP X ? MOVE D-PDL X into MOVEM X
			    (rplaca y 'movem)
			    (go delete-z))))
		(and (eq (car y) 'branch)
		     (cadddr y)
		     (eq (cadr z) 'd-pdl)
		     (equal x z)
		     ;; Turn MOVE D-PDL X ? BRANCH-OR-POP ? MOVE D-PDL X
		     ;; into MOVE D-PDL X ? BRANCH
		     (progn (rplaca (cdddr y) nil)
			    (go delete-z)))))
	 (go refill-x)))

;;; Replace all unreferenced tags with nil in code.
(defun peep-tags (code)
    (prog (used-tags)
	 (mapc (function (lambda (insn)
		   (and (not (atom insn))
			(cond ((and (eq (car insn) 'branch)
				    (not (memq (fifth insn) used-tags)))
			       (push (fifth insn) used-tags))
			      ((eq (car insn) 'adi-call)
			       (do ((ins insn (cdr ins))) ((null ins))
				   (and (not (atom (car ins)))
					(eq (caar ins) 'restart-pc)
					(return (let ((tem (cadar ins)))
					     (and (eq (car tem) 'quote-vector)
							  (eq (caadr tem) 'tag)
							  (not (memq (cadadr tem) used-tags))
						  (push (cadadr tem) used-tags)))))))))))
	       code)
	 (map (function (lambda (insn-ptr)
		  (and (atom (car insn-ptr))
		       (not (memq (car insn-ptr) used-tags))
		       (rplaca insn-ptr nil))))
	      code)))
