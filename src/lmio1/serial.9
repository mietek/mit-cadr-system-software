; -*- Mode:Lisp; Package:System-Internals -*-

; Handler for the serial I/O interface

(declare (special syn1-character syn2-character dle-character ;Can't read these back
		  uart-registers ;3 bytes, command, mode1, mode2.
		  serial-unibus-channel serial-unrchf serial-error-mask))

(defun make-serial-stream (&rest keywords
			   &aux stream (syn1-character 0) (syn2-character 0) (dle-character 0)
			        (uart-registers 0)
				(serial-unibus-channel nil) (serial-unrchf nil)
				(serial-error-mask 0)
				(request-to-send t) (data-terminal-ready t)
				(synchronous-mode nil) (number-of-stop-bits 1)
				;; Default to even parity and 7 data bits,
				;; but don't check received parity.  This causes the
				;; input stream to return 7-bit characters, to avoid
				;; faking out Lisp-machine-oriented programs.
				(parity ':even) (number-of-data-bits 7)
				(baud 300.))
  (serial-check-existence)		;Barf if machine doesn't have a serial I/O port
  (do l keywords (cddr l) (null l)
    (selectq (car l)
      (:check-parity-errors
	(and (cadr l) (setq serial-error-mask (logior 10 serial-error-mask))))
      (:check-over-run-errors
	(and (cadr l) (setq serial-error-mask (logior 20 serial-error-mask))))
      (:check-framing-errors
	(and (cadr l) (setq serial-error-mask (logior 40 serial-error-mask))))
      (:request-to-send (setq request-to-send (cadr l)))
      (:data-terminal-ready (setq data-terminal-ready (cadr l)))
      (:synchronous-mode (setq synchronous-mode (cadr l)))
      (:number-of-stop-bits (setq number-of-stop-bits (cadr l)))
      (:parity (setq parity (cadr l)))
      (:number-of-data-bits (setq number-of-data-bits (cadr l)))
      (:baud (setq baud (cadr l)))
      (:otherwise (ferror nil "~S unknown keyword" (car l)))))
  (%unibus-write 764166 20)		;Reset
  (%unibus-write 764166 5)		;Reset command register, rcv & xmt enable
  (serial-write-mode 60)		;Reset modes, set to internal clocks
  (setq uart-registers 30005)		;60 in mode, 5 in command
  (setq stream (closure '(syn1-character syn2-character dle-character uart-registers
			  serial-unibus-channel serial-unrchf serial-error-mask)
			'serial-stream))
  (funcall stream ':put ':synchronous-mode synchronous-mode)
  (funcall stream ':put ':request-to-send request-to-send)
  (funcall stream ':put ':data-terminal-ready data-terminal-ready)
  (funcall stream ':put ':number-of-stop-bits number-of-stop-bits)
  (funcall stream ':put ':parity parity)
  (funcall stream ':put ':number-of-data-bits number-of-data-bits)
  (funcall stream ':put ':baud baud)
  (funcall stream ':clear-input)	;Sets up the unibus channel
  stream)

(defselect (serial-stream serial-stream-default-handler)
  (:tyo (char)
    (process-wait "Serial TYO" #'(lambda () (bit-test 1 (%unibus-read 764162))))
    (%unibus-write 764160 char))
  (:tyi (&optional ignore &aux ch status)
    (cond ((null serial-unrchf)
	   (process-wait "Serial TYI" #'unibus-channel-not-empty serial-unibus-channel)
	   (multiple-value (ch status) (read-unibus-channel serial-unibus-channel))
	   (setq ch (logand 377 ch))
	   (cond ((bit-test serial-error-mask status)
		  ;; Reset the stupid error flags
		  (%unibus-write 764166 (logior 20 uart-registers))
		  (%unibus-write 764166 uart-registers)
		  (cerror t nil nil
			  "Serial input ~:[framing ~]~:[over-run ~]~:[parity ~]error: ~O"
			  (zerop (logand 40 status serial-error-mask))
			  (zerop (logand 20 status serial-error-mask))
			  (zerop (logand 10 status serial-error-mask))
			  ch)))
	   ch)
	  (t (prog1 serial-unrchf (setq serial-unrchf nil)))))
  (:untyi (char)
    (setq serial-unrchf char))
  (:listen ()
	   (unibus-channel-not-empty serial-unibus-channel))
  (:tyi-no-hang ()
    (and (unibus-channel-not-empty serial-unibus-channel)
	 (serial-stream ':tyi)))
  (:clear-input ()		;This fully resets the hardware.
    (serial-stream ':close)
    ;; Reset the stupid error flags
    (%unibus-write 764166 (logior 20 uart-registers))
    (%unibus-write 764166 uart-registers)
    (serial-write-mode (%logldb 1020 uart-registers))	;Restore mode registers
    (%unibus-read 764160)	;Flush buffered character if any
    (setq serial-unibus-channel (get-unibus-channel 264 764162 2 764160 2))
    (%unibus-write 764112 (dpb 1 0701 (%unibus-read 764112)))) ;Turn on interrupt
  (:close ()
    (%unibus-write 764112 (dpb 0 0701 (%unibus-read 764112))) ;Turn off interrupt
    (return-unibus-channel (prog1 serial-unibus-channel (setq serial-unibus-channel nil))))
  (:get . serial-get-property)
  (:put . serial-put-property))
  
(defun serial-stream-default-handler (op arg1 &rest rest)
  (multiple-value-call (stream-default-handler #'serial-stream op arg1 rest)))

;Subroutines for use by the stream

;Read the mode register, MODE1 in high byte
(defun serial-read-mode ()
  (%unibus-read 764166)		;reset MODE1/MODE2 phase
  (dpb (%unibus-read 764164) 1010 (%unibus-read 764164)))

;Write it
(defun serial-write-mode (mode)
  (setq uart-registers (%logdpb mode 1020 uart-registers)) ;remember it
  (%unibus-read 764166)		;reset MODE1/MODE2 phase
  (%unibus-write 764164 (ldb 1010 mode))
  (%unibus-write 764164 mode))

(defun serial-write-command (cmd)
  (setq uart-registers (dpb cmd 0010 uart-registers))  ;remember it
  (%unibus-write 764166 cmd))

(defun serial-write-sync-chars (syn1 syn2 dle)
  (%unibus-read 764166)		;reset MODE1/MODE2 phase
  (%unibus-write 764162 syn1)
  (%unibus-write 764162 syn2)
  (%unibus-write 764162 dle))

;Test existence of device.  If IOB not wired for it, will read back
;all zero.  If PCI not plugged in, will read back all ones.
(defun serial-check-existence (&aux zeros ones)
  (%unibus-write 764166 0)
  (setq zeros (ldb 0010 (%unibus-read 764166)))
  (%unibus-write 764166 100)
  (setq ones (ldb 0010 (%unibus-read 764166)))
  (cond ((zerop ones) (ferror nil "This IOB does not have serial I/O"))
	((= zeros 377) (ferror nil "This IOB does not contain a PCI"))))

(defun serial-get-property (ignore prop)	;:GET operation on the stream
    (let ((mode (serial-read-mode))
	  (status (%unibus-read 764162))
	  (command (%unibus-read 764166))
	  (synchronous-p nil))
      (and (zerop (ldb 1002 mode)) (setq synchronous-p t))
      (selectq prop
	(:data-set-ready (bit-test 200 status))	;This might want to be wired to CTS?
	(:carrier-detect (bit-test 100 status))
	(:request-to-send (bit-test 40 command))
	(:data-terminal-ready (bit-test 2 command))
	(:number-of-stop-bits (if synchronous-p 0
				  (nth (ldb 1602 mode) '(? 1 1.5 2))))
	(:parity (nth (ldb 1402 mode) '(nil :odd nil :even)))
	(:number-of-data-bits (+ 5 (ldb 1202 mode)))
	(:baud (nth (ldb 0004 mode) '(50. 75. 110. 134. 150. 300. 600.
				      1200. 1800. 2000. 2400. 3600.
				      4800. 7200. 9600. 19200.)))
	(:synchronous-mode synchronous-p)
	(:syn1-character syn1-character)
	(:syn2-character syn2-character)
	(:dle-character dle-character)
	(:single-sync-char-mode (and synchronous-p (bit-test 100000 mode)))
	(:sync-transparent-mode (and synchronous-p (bit-test 40000 mode)))
	(:automatic-echo-mode (and (not synchronous-p) (= (ldb 0602 command) 1)))
	(:sync-dle-stripping-mode (and synchronous-p (= (ldb 0602 command) 1)))
	(:local-loop-back (= (ldb 0602 command) 2))
	(:remote-loop-back (= (ldb 0602 command) 3))
	(:receive-enable (bit-test 4 command))
	(:transmit-enable (bit-test 1 command))
	(:check-parity-errors (bit-test 10 serial-error-mask))
	(:check-over-run-errors (bit-test 20 serial-error-mask))
	(:check-framing-errors (bit-test 40 serial-error-mask))
	(:otherwise (ferror nil "~S not a valid property name" prop)))))

(defun serial-put-property (ignore prop val)	;:PUT operation on the stream
    (let ((mode (serial-read-mode))
	  (command (%unibus-read 764166))
	  (synchronous-p nil))
      (and (zerop (ldb 1002 mode)) (setq synchronous-p t))
      (selectq prop
	(:request-to-send (serial-write-command (logior 40 command)))
	(:data-terminal-ready (serial-write-command (logior 2 command)))
	(:number-of-stop-bits
	  (setq val (select val (1 1) (1.5 2) (2 3)
			    (otherwise (ferror nil "~S must be 1, 1.5, or 2" val))))
	  (serial-write-mode (dpb val 1602 mode)))
	(:parity
	  (setq val (selectq val (nil 0) (:odd 1) (:even 3)
			     (otherwise (ferror nil "~S must be NIL, :ODD, or :EVEN" val))))
	  (serial-write-mode (dpb val 1402 mode)))
	(:number-of-data-bits
	  (setq val (if (and (> val 4) (< val 9)) (- val 5)
			(ferror nil "~S must be 5, 6, 7, or 8" val)))
	  (serial-write-mode (dpb val 1202 mode)))
	(:baud
	  (setq val (find-position-in-list val '(50. 75. 110. 134. 150. 300. 600.
						 1200. 1800. 2000. 2400. 3600.
						 4800. 7200. 9600. 19200.)))
	  (or val (ferror nil "invalid baud rate"))
	  (serial-write-mode (dpb val 0004 mode)))
	(:synchronous-mode
	  (serial-write-mode (dpb (if val 0 1) 1002 mode)))
	(:syn1-character
	  (setq syn1-character val)
	  (serial-write-sync-chars syn1-character syn2-character dle-character))
	(:syn2-character
	  (setq syn2-character val)
	  (serial-write-sync-chars syn1-character syn2-character dle-character))
	(:dle-character
	  (setq dle-character val)
	  (serial-write-sync-chars syn1-character syn2-character dle-character))
	(:single-sync-char-mode
	  (serial-write-mode (dpb (if val 1 0) 1701 mode)))
	(:sync-transparent-mode
	  (serial-write-mode (dpb (if val 1 0) 1601 mode)))
	(:automatic-echo-mode
	  (and synchronous-p (ferror nil "does not apply in synchronous mode"))
	  (serial-write-command (dpb (if val 1 0) 0602 command)))
	(:sync-dle-stripping-mode
	  (and (not synchronous-p) (ferror nil "does not apply in asynchronous mode"))
	  (serial-write-command (dpb (if val 1 0) 0602 command)))
	(:local-loop-back
	  (serial-write-command (dpb (if val 2 0) 0602 command)))
	(:remote-loop-back
	  (serial-write-command (dpb (if val 3 0) 0602 command)))
	(:receive-enable
	  (serial-write-command (dpb (if val 1 0) 0201 command)))
	(:transmit-enable
	  (serial-write-command (dpb (if val 1 0) 0001 command)))
	(:check-parity-errors
	  (setq serial-error-mask (dpb (if val 1 0) 0301 serial-error-mask)))
	(:check-over-run-errors
	  (setq serial-error-mask (dpb (if val 1 0) 0401 serial-error-mask)))
	(:check-framing-errors
	  (setq serial-error-mask (dpb (if val 1 0) 0501 serial-error-mask)))
	(:otherwise (ferror nil "~S not a valid property name" prop)))))

;Print the complete status
(defun serial-status ()
  (let ((mode (serial-read-mode))
	(status (%unibus-read 764162))
	(command (%unibus-read 764166))
	(synchronous-p nil))
    (and (zerop (ldb 1002 mode)) (setq synchronous-p t))
    (selectq (ldb 0602 command)
      (0 )
      (1 (princ (if synchronous-p "syn//dle-stripping " "auto-echo ")))
      (2 (princ "local-loop-back "))
      (3 (princ "remote-loop-back ")))
    (and (bit-test 40 command) (princ "request-to-send "))
    (and (bit-test 2 command) (princ "data-terminal-ready "))
    (and (bit-test 10 command) (princ (if synchronous-p "send-dle" "send-break")))
    (format t "receiver-~[off~;on~] " (ldb 0201 command))
    (format t "transmitter-~[off~;on~]~%" (ldb 0001 command))
    (format t "interrupt-~[dis~;en~]able " (ldb 0701 (%unibus-read 764112)))
    (and (bit-test 200 status) (princ "data-set-ready "))
    (and (bit-test 100 status) (princ "carrier-detect "))
    (and (bit-test 40 status) (princ (if synchronous-p "sync-detect " "break-received ")))
    (and (bit-test 20 status) (princ "receive-overrun "))
    (and (bit-test 10 status) (princ (if synchronous-p "par-err-or-dle " "parity-error ")))
    (and (bit-test 4 status) (princ "idle//data-set-change "))
    (and (bit-test 2 status) (princ "receive-ready "))
    (and (bit-test 1 status) (princ "transmit-ready "))
    (terpri)
    (format t "~[illegal~;1~;1.5~;2~] stop bits, " (ldb 1602 mode))
    (format t "~[no~;odd~;no~;even~] parity, " (ldb 1402 mode))
    (format t "~D data bits, " (+ 5 (ldb 1202 mode)))
    (format t "~[synchronous~;asynchronous~;asynchronous//16~;asynchronous//64~]~%"
	      (ldb 1002 mode))
    (format t "~[external~;internal~] transmit clock, " (ldb 0501 mode))
    (format t "~[external~;internal~] receive clock, " (ldb 0401 mode))
    (format t "~D baud" (nth (ldb 0004 mode) '(50. 75. 110. 134. 150. 300. 600.
					       1200. 1800. 2000. 2400. 3600.
					       4800. 7200. 9600. 19200.)))))

