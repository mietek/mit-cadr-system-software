;;; -*-mode:lisp; package:system-internals; lowercase:t-*-

;This binds STANDARD-OUTPUT and STANDARD-INPUT and enters a new read-eval-print
;loop.  SETQ'ing them would be global for all processes and would leave you
;totally shafted if the file connection broke.

(defun dribble-start (filename &optional editor-p)
  "Copy input and output to a file, or an editor buffer with second arg of T"
  (let* ((standard-input (make-dribble-stream terminal-io
			   (if (not editor-p) (open filename '(:write))
			       (zwei:make-file-buffer-stream filename))))
	 (standard-output standard-input))
    (*catch 'dribble-end
	    (lisp-top-level1 terminal-io))))

(defun dribble-end ()
  (*throw 'dribble-end (funcall standard-input ':dribble-end)))

(local-declare ((special *unrchf* *tv-stream* *file-stream* *rubout-handler-buffer*))
(defun make-dribble-stream (*tv-stream* *file-stream*)
  (let ((*unrchf* nil)
	(*rubout-handler-buffer* (make-array nil 'art-string 100. nil '(0))))
    (closure '(*unrchf* *tv-stream* *file-stream* *rubout-handler-buffer*)
	     'dribble-stream-io)))

(defun dribble-stream-io (op &rest args)
  (selectq op
    ((:tyo :string-out :line-out :fresh-line)
     (lexpr-funcall *tv-stream* op args)
     (lexpr-funcall *file-stream* op args))
    (:tyi
     (if *unrchf*
	 (prog1 *unrchf* (setq *unrchf* nil))
	 (prog ()
	   (*catch (if rubout-handler 'rubout-handler 'dummy-tag)
	     (let ((ch (funcall *tv-stream* op)))
	       (and rubout-handler (array-push-extend *rubout-handler-buffer* ch))
	       (return ch)))
	   ;;get here if someone threw to rubout-handler
	   ;;reset our buffer and continue the throw
	   (store-array-leader 0 *rubout-handler-buffer* 0)
	   (*throw 'rubout-handler nil))))
    (:untyi
     (setq *unrchf* (car args)))
    (:listen
     (or *unrchf* (funcall *tv-stream* op)))
    (:rubout-handler
     (store-array-leader 0 *rubout-handler-buffer* 0)	;reset the buffer
     (prog (vals)
       (setq vals (multiple-value-list (lexpr-funcall *tv-stream* op args)))
       (funcall *file-stream* ':string-out *rubout-handler-buffer*)
       (return-list vals)))
    (:dribble-end
     (close *file-stream*)
     (and (memq ':get (funcall *file-stream* ':which-operations))
	  (funcall *file-stream* ':get ':unique-id)))
    (otherwise
     (lexpr-funcall *tv-stream* op args))))
);local-declare
