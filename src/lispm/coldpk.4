; -*- Mode:Lisp; Lowercase:T; Base:8 -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;This package is the program
(package-declare cold global 1000.
   (("AI:LISPM;COLDUT QFASL")
    ("AI:LISPM;COLDLD QFASL")))

;This package is really under NIL, not GLOBAL, but there is a bug in the package system.
;This package is used to contain symbols which stand for symbols in the cold-load
;being built; this includes all the system data structure definition symbols.
;These shadow the ones in GLOBAL, so that you can make incompatible changes.
(package-declare cold-symbols global 10000.
   ()
   (refname global global)
   (myrefname global cold-symbols)
   (myrefname cold sym)
   ;; Borrow symbols needed to load QCOM and QDEFS
   ;; But don't actually get under global, want to shadow all the system parameter symbols
   ;; Code here and there relies on the fact that NIL is the same.
   (borrow global defun defvar setq cond and or ibase defprop putprop + - > = < 1+ 1-
	   	  t nil error length special
		  assign-values assign-alternate get-alternate assign-values-init-delta)
   (borrow cold defmic))

(fset 'cold-symbols:logdpb #'dpb)

(setf (pkg-super-package (pkg-find-package "cold-symbols")) nil)
(setf (si:pkg-subpackages pkg-global-package) (delq (pkg-find-package "cold-symbols")
						    (si:pkg-subpackages pkg-global-package)))
