(defun SMESS (f n)
       (renable)
       (do ((i 0 (1+ i)))
           ((= i n)
            (%unibus-write 764142 10)
            (%unibus-read 764152))
           (%unibus-write 764142 (funcall f i))))

(defun oneplus (n) n)

(defun renable ()
       (%unibus-write 764140 (logior 10 (%unibus-read 764140))))

(defun rcv ()
       (prog (words)
             (setq words (lsh (1+ (%unibus-read 764146)) -4))
             (do ((i words (1- i)))
                 ((= i 0))
                 (print (%unibus-read 764144)))))

(defun cread ()
       (%unibus-read 764140))

(defun altbit (n)
       (cond ((zerop (\ n 2)) 125252)
             (t 52525)))

(defun shift (n)
       (setq n (\ n 16.))
       (lsh 1 n))