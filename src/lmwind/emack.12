; -*- Mode:Lisp; Lowercase:True -*-

; Bolio init file for window system documentation

(declare (load '|liblsp;loop fasl|)
	 (include |ml:bolio;justdf >|)
	 (crunit dsk lmwind))

; Why aren't these declared??
(declare (special defun-pre-leading environment-type request-eol-p))

; Pessimal strings

(setsyntax '/" 'macro 'string-quote)

(defun string-quote ()
  (do ((l nil) (ch) (str))
      (nil)
    (setq ch (tyi))
    (cond ((= ch 42) (setq l (nreverse l) str (make-string (length l)))
		     (do ((i 0 (1+ i)) (l l (cdr l))) ((null l))
		       (as-1 (car l) str i))
		     (reclaim l t)
		     (return str))
	  ((= ch 57) (setq ch (tyi))))
    (push ch l)))

; Flavor documentation guy punches out forms like
;  (defprop flavor-name string flavor-documentation)

(defprop flavor-documentation flavor-documentation-request request)
(defun flavor-documentation-request ()
  (let ((flavor-name-string (get-word-string)) (flavor-name) (tem))
    (setq flavor-name (string-intern flavor-name-string))
    (flush-request-line)
    (cond ((setq tem (get flavor-name 'flavor-documentation))
	   (jin-push tem)				;I wonder if all this BS works?
	   (jin-push flavor-name-string)
	   (jin-push '|/
/|)
	   (let ((request-eol-p nil))
	     (defflavor-request))
	   (end-defflavor-request))
	  (t (barf flavor-name '|has no flavor-documentation property|)))))

; Message and flavor and initoption indices

(setq flavor-index nil)
(defprop flavor-index |Flavor Index| index-title)
(defprop flavor-index 2 index-columns)

(setq message-index nil)
(defprop message-index |Message Index| index-title)
(defprop message-index 2 index-columns)

(setq initoption-index nil)
(defprop initoption-index |Window Creation Options| index-title)
(defprop initoption-index 2 index-columns)

;.defflavor and .defmessage requests

(defprop defmessage defmessage-request request)
(defprop defmessage1 defmessage1-request request)
(defprop end_defmessage end-defmessage-request request)

(defun defmessage-request ()
  (check-env 'text 'defmessage)
  (or (need-space 200.) ;1 inch
      (output-leading defun-pre-leading))
  (check-font-status 1)
  ((lambda (left-indent environment-type extra-left-indent-first-line-of-paragraph)
      (defmessage1-request)  ;Gobble the arguments, put out line, index, etc.
      (flush-request-line)
      (defun-horrible-tab-crock)
      (catch (main-loop) defmessage))
   96. ;1/2 inch indent
   'defmessage
   0))

(defun defmessage1-request ()
  (check-env 'defmessage 'defmessage1)
  (setq cur-hpos 0)
  ((lambda (message-name jin-cur-font)
      (or message-name (barf '|Message name missing in .defmessage or .defmessage1|))
      (add-to-index message-name 'message-index)
      (auto-setq message-name '|message|)
      (set-hpos left-margin)
      (put-string-flush-left message-name)
      (defun-line-proc))
   (get-word-string)
   8)
  (setq begin-new-paragraph nil))

(defun end-defmessage-request ()
  (check-env 'defmessage 'end_defmessage)
  (check-font-status 1)
  (throw nil defmessage))

(defprop defflavor defflavor-request request)
(defprop end_defflavor end-defflavor-request request)

(defun defflavor-request ()
  (check-env 'text 'defflavor)
  (or (need-space 200.)	;1 inch
      (output-leading defun-pre-leading))
  (check-font-status 1)
  (setq cur-hpos 0)
  ((lambda (left-indent environment-type extra-left-indent-first-line-of-paragraph
	    flavor-name jin-cur-font)
      (or flavor-name (barf '|Flavor name missing in .defflavor|))
      (add-to-index flavor-name 'flavor-index)
      (auto-setq flavor-name '|flavor|)
      (set-hpos left-margin)
      (put-string-flush-left flavor-name)
      (jout-white-space 40.)
      (setq jin-cur-font 2)  ;italic
      (cond (request-eol-p (put-string-flush-left (string '|Flavor|)))
	    (t (put-string-flush-left (get-line-string))))
      (setq jin-cur-font 1)
      (line-advance)
      (setq begin-new-paragraph nil)
      (defun-horrible-tab-crock)
      (catch (main-loop) defflavor))
   96. ;1/2 inch indent
   'defflavor
   0
   (get-word-string)
   8))

(defun end-defflavor-request ()
  (check-env 'defflavor 'end_defflavor)
  (check-font-status 1)
  (throw nil defflavor))

(defprop definitoption definitoption-request request)
(defprop definitoption1 definitoption1-request request)
(defprop end_definitoption end-definitoption-request request)

(defun definitoption-request ()
  (check-env 'text 'definitoption-request)
  (or (need-space 200.)	;1 inch
      (output-leading defun-pre-leading))
  (check-font-status 1)
  ((lambda (left-indent environment-type extra-left-indent-first-line-of-paragraph)
      (definitoption1-request)
      (setq begin-new-paragraph nil)
      (defun-horrible-tab-crock)
      (catch (main-loop) definitoption))
   96. ;1/2 inch indent
   'definitoption
   0))

(defun definitoption1-request ()
  (let ((option-name (get-word-string))
	(jin-cur-font 8))
    (check-env 'definitoption 'definitoption1-request)
    (setq cur-hpos 0)
    (or option-name (barf '|Option name missing in .definitoption|))
    (add-to-index option-name 'initoption-index)
    (set-hpos left-margin)
    (put-string-flush-left option-name)
    (defun-line-proc)))

(defun end-definitoption-request ()
  (check-env 'definitoption 'end_definitoption)
  (check-font-status 1)
  (throw nil definitoption))

(defprop nopara nopara-request request)
(defun nopara-request ()
  (output-leading (* (or (get-numeric-arg) 1) xgp-line-height))
  (setq begin-new-paragraph nil))

; Set up style and fonts

(default-manual-style nil '|New Window System|)
(standard-dover-fonts 10.)
