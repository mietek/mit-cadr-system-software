;;; -*- Mode: Lisp; Package: TV; Base: 8 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;This file is loaded after the kernel of the window system and
;before any windows are instantiated.  It contains the combined
;methods and such.  SHEET and SCREEN have been done already.

;WINDOW because those methods will get shared then, and
;because it may even be instantiated itself.  Plus all the
;types of windows used in code loaded up til now.
(COMPILE-FLAVOR-METHODS WINDOW LISP-LISTENER LISP-INTERACTOR BACKGROUND-LISP-INTERACTOR
			POP-UP-TEXT-WINDOW POP-UP-NOTIFICATION-WINDOW
			TRUNCATING-POP-UP-TEXT-WINDOW)

;Later modules generally have their own COMPILE-FLAVOR-METHODS at the
;end of their own file.  This file exists for bootstrapping reasons.

;;; Resources

;;; These resources are intended so that it's easy to get a menu/pop up window for
;;; very short-term use.  Since they may be on any superior, it is advisable to do
;;; a :SET-SUPERIOR before you use them
(DEFRESOURCE MOMENTARY-MENU-RESOURCE
  (WINDOW-CREATE 'MOMENTARY-MENU ':HEIGHT (// (SHEET-HEIGHT MOUSE-SHEET) 4.)))

(DEFRESOURCE POP-UP-TEXT-WINDOW-RESOURCE
  (WINDOW-CREATE 'POP-UP-TEXT-WINDOW ':HEIGHT (// (SHEET-HEIGHT MOUSE-SHEET) 4.)))

(OR (BOUNDP 'POP-UP-FINGER-WINDOW)
    (SETQ POP-UP-FINGER-WINDOW (WINDOW-CREATE 'TRUNCATING-POP-UP-TEXT-WINDOW)))

(DEFRESOURCE BACKGROUND-LISP-INTERACTORS
  (WINDOW-CREATE 'BACKGROUND-LISP-INTERACTOR
		 ':PROCESS CURRENT-PROCESS
		 ':SUPERIOR DEFAULT-SCREEN
		 ':HEIGHT (// (SHEET-HEIGHT DEFAULT-SCREEN) 3)))

; It is now time to initialize the window system, which will create and expose
; the initial-lisp-listener and turn on blinkers.
(ADD-INITIALIZATION "WINDOW" '(WINDOW-INITIALIZE) '(:SYSTEM))
