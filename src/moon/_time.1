;-*- Mode:LISP; Package:USER -*-

(defun foo (n-times)
  (format t "~D usec (spec pdl ptr = ~D, reg pdl ptr = ~D)"
    (// (loop for i from 1 to n-times
	      summing (let ((tm (si:fixnum-microsecond-time)))
			(process-allow-schedule)
			(time-difference (si:fixnum-microsecond-time) tm)))
	n-times)
    sys:(sg-special-pdl-pointer %current-stack-group)
    sys:(sg-regular-pdl-pointer %current-stack-group)))


;These times are in UCADR 593 and system 20.2:
;19 ms if in lisp listener, 7, 88 are pdl ptrs
;25 ms if in editor (either break or meta-altmode), 95, 166 (m-), 141, 210 (break)
;Note that the special-pdl pointer is 97. in the editor and 27. in the lisp listener.
;Additional 70. spec vars swapped in and out should only account for 1 ms though
;I would think.  What gives?  The extra 80 words of pdl should only be 1/2 ms
;even with the slow ucode

(defun foo-with-n-specials (n-times n-specials)
  (loop for i from 1 to n-specials
	do (bind (value-cell-location (gensym)) nil)
	finally (foo n-times)))

;This gets 33 microseconds per variable.  I would have expected half this.
;However, 70 vars makes only 2 ms, we have 6 to account for!

(defun foo-with-n-pdls (n-times n-pdls)
  (loop for i from 1 to n-pdls
	do (%push nil)
	finally (foo n-times)))

;In UCADR 600:
;For lisp listener, 18.2, 7, 88
;For editor (break) 23.9, 141, 210, (m-) 23.0, 95, 166
;Time per special variable:  16 microseconds, but there is a lot of variance
;Time per pdl word: unmeasurable.

;This version doesn't call the scheduler, just another sg
(defun foo (n-times &aux sg)
  (setq sg (make-stack-group "foo"))
  (stack-group-preset sg #'(lambda ()
			     (loop doing (stack-group-return nil))))
  (format t "~D usec (spec pdl ptr = ~D, reg pdl ptr = ~D)"
    (// (loop for i from 1 to n-times
	      summing (let ((tm (si:fixnum-microsecond-time)))
			(funcall sg)
			(time-difference (si:fixnum-microsecond-time) tm)))
	n-times)
    sys:(sg-special-pdl-pointer %current-stack-group)
    sys:(sg-regular-pdl-pointer %current-stack-group)))

;683 in lisp listener, 1578 in editor (break).  These are microseconds in UCADR 600.
;Time per special variable: ~ 15 usec
;Time per pdl word: unmeasurable (I see, because frame not restored)
;  With fixed frob: 3.3 usec.

(defun frob-with-n-pdls (n-times n-pdls &aux sg)
  (setq sg (make-stack-group "foo"))
  (stack-group-preset sg #'(lambda ()
			     (loop doing (stack-group-return nil))))
  (loop for i from 1 to n-pdls
	do (%push nil)
	finally
  (format t "~D usec (spec pdl ptr = ~D, reg pdl ptr = ~D)"
    (// (loop for i from 1 to n-times
	      summing (let ((tm (si:fixnum-microsecond-time)))
			(funcall sg)
			(time-difference (si:fixnum-microsecond-time) tm)))
	n-times)
    sys:(sg-special-pdl-pointer %current-stack-group)
    sys:(sg-regular-pdl-pointer %current-stack-group))))

;Why is the editor 1 ms slower with an sg, but >5 ms slower with the scheduler?
;Commenting out the clock functions part of the scheduler doesn't affect
; the difference but does change each time by about 4 ms.  What the clock
; has to do is the same in both environments (blink a single pc ppr, plus mouse)
;Aha!  Most of the discrepancy is that the wait-function for ZED takes nearly
; 3 ms to evaluate, because it sends a message.  The rest may be the editor having
; more blinkers.
;Even the MOUSE-INPUT wait-function takes 1/2 ms to call.

;With no other processes, takes 13.5 ms. from a lisp listener.
;Turning off kbd-simulated-clock-fcn-list saves another 2 to 3 ms, somewhat inconsistently.
;Commenting out the whole section is about the same, strangely enough.
;Ho, ho, said the Cow!  TV-WHO-LINE-RUN-STATE-UPDATE takes 12 ms.
;About 8 ms of this is in CHAOS:NWATCH-WHO-FUNCTION.  The rest is somewhere
;in TV-WHO-LINE-UPDATE, but not in one obvious lump.  The other who-line
;update functions take <1 ms each.  I made the nwatch-who-function a little
;faster, but it's still unreasonably slow to update the who-line on every
;process switch.  Making TV-WHO-LINE-RUN-STATE-UPDATE not call TV-WHO-LINE-UPDATE
;(which of course breaks it) cuts the time from 19 ms to 8 ms.

;With TV-WHO-LINE-RUN-STATE-UPDATE fixed to not update the whole who-line,
;only the run-state, and in UCADR 601: 9 ms in the lisp-listener, 14 ms in the editor
