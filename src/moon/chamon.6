;;; -*- Mode: Lisp; Package: Chaos -*- chaos net monitor.

(declare (special %%csr-receive-done %%csr-lost-count %%csr-receive-all-messages
		  %%csr-crc-error))
(setq %%csr-receive-done 1701 %%csr-lost-count 1104 %%csr-receive-all-messages 0201
      %%csr-crc-error 1601)

(defun monitor ()
  (interface-reset)
  (%unibus-write control-status-register (dpb 1 %%csr-receive-all-messages 0))
  (do ()
      (nil)
    (or (ldb-test %%csr-receive-done (%unibus-read control-status-register))
	(process-wait "Receive Done" (function (lambda ()
	      (or (kbd-char-available)
		  (ldb-test %%csr-receive-done (%unibus-read control-status-register)))))))
    (and (kbd-tyi-no-hang) (return t))
    (let ((pkt (net-receive-any)))
      (cond ((not (null pkt))
	     (print-pkt-sp pkt)
	     (free-pkt pkt))
            (t (format t "~&CRC ERR~%")))))
  (%unibus-write control-status-register (dpb 0 %%csr-receive-all-messages 0))
  t)

(defun net-receive-any nil
  (prog (status words bits dest source crc crc-1-p pkt)
    (setq crc-1-p (ldb-test %%csr-crc-error (%unibus-read control-status-register)))
    (setq bits (1+ (%unibus-read bit-count-register)))
    (setq words (- (lsh bits -4) 3))
    (setq pkt (allocate-pkt))
    (do ((count words (1- count))
	 (i 0 (1+ i)))
	((zerop count))
	(as-1 (%unibus-read read-buffer-register) pkt i))
    (setq dest (%unibus-read read-buffer-register))	;destination.
    (setq source (%unibus-read read-buffer-register))	;source.
    (setq crc (%unibus-read read-buffer-register))	;crc code.
    (setq pkts-lost (+ (ldb %%csr-lost-count
			   (setq status (%unibus-read control-status-register)))
		       pkts-lost))
    (receiver-reset)
    (store-array-leader (min 770 (pkt-nbytes pkt)) (pkt-string pkt) 0)
  (format t "~& src ~O dst ~O bits ~O words ~O ~:[~;CRC~]~%"
	    source dest bits words crc-1-p)

    (return (cond (crc-1-p (free-pkt pkt) nil)
                  (t pkt)))))
;Print out a packet, if SHORT-DISPLAY is T only 1 line is printed.
(DEFUN PRINT-PKT-sp (PKT &OPTIONAL (SHORT-DISPLAY NIL))
    (TERPRI)
    (AND SHORT-DISPLAY (FORMAT T "   "))
    (FORMAT T "Number: ~O (~O)  Opcode: ~O (~A).  Number of bytes = ~O ."
	       (PKT-NUM PKT)
	       (%POINTER PKT)
	       (PKT-OPCODE PKT)
	       (COND ((< (PKT-OPCODE PKT) (LENGTH OPCODE-LIST))
		      (NTH (PKT-OPCODE PKT) OPCODE-LIST))
		     (( (PKT-OPCODE PKT) DAT-OP) 'DAT)
		     (T (FORMAT NIL "==> ~O <==" (PKT-OPCODE PKT))))
	       (PKT-NBYTES PKT))
    (COND ((NOT SHORT-DISPLAY)
	   (FORMAT T "~%From ~O-~O to ~O-~O .~%"
		   (PKT-SOURCE-ADDRESS PKT) (PKT-SOURCE-INDEX-NUM PKT)
		   (PKT-DEST-ADDRESS PKT) (PKT-DEST-INDEX-NUM PKT))
	   (FORMAT T "Contents:/"~A/"~%   " (PKT-STRING PKT))
	   (DO ((I 0 (1+ I))) (( I (PKT-NWORDS PKT)))
	       (FORMAT T "~6,48O~:[,~;~%~]" (AR-1 PKT I) (= (1+ I) (PKT-NWORDS PKT))))
	   (FORMAT T "Pkt number = ~O, Ack number = ~O, Forwarded ~O times.~%"
		   (PKT-NUM PKT) (PKT-ACK-NUM PKT) (PKT-FWD-COUNT PKT))
           ))
    NIL)
