;;; -*- Mode: Lisp; Package: System-Internals -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This file contains the Lisp-coded support for the Garbage Collector
;;; Some GC-related functions that need to be in the cold-load can be found in QRAND.

;*** Needs a facility which continuously maintains a second who-line with gc stats
;*** Needs a way to sound a warning when you are close to running out of virtual memory


(DEFVAR GC-REPORT-STREAM T)
	;Where junk output from the garbage collector goes:
	;NIL - discard
	;stream - send there
	;T - notify (this is the default)

(DEFVAR GC-PROCESS)		;Process that runs the flipper

;; These are lists of forms which are evaluated after reclaiming oldspace
;; and before flipping newspace into oldspace.
(DEFVAR GC-EVERY-FLIP-LIST NIL)	;Forms to evaluate on every flip
(DEFVAR GC-NEXT-FLIP-LIST NIL)	;Forms to evaluate just on the next flip
(DEFVAR GC-SECOND-NEXT-FLIP-LIST NIL)	;Forms to evaluate just on the flip after that
(DEFVAR GC-AFTER-FLIP-LIST NIL)	;Forms to evaluate after flipping

(DEFVAR GC-PAGE-CONS-ALARM-MARK 0)	;Value that %PAGE-CONS-ALARM
					; must be greater than in order to do a flip.
					; Set by the GC process.  If %GC-FLIP-READY
					; is off, this is ignored

(DEFVAR GC-FLIP-RATIO 1)		;If the product of this number and
					;committed free space is greater
					;then the amount of free space,
					;then a flip will take place.

(DEFVAR GC-RECLAIM-IMMEDIATELY NIL)	;If non-NIL, then as soon as a flip
					;takes place, a GC-RECLAIM-OLDSPACE
					;occurs.  This essentially flushes
					;the "Await Scavenge" state of the
					;garbage collector, and removes the
					;real-time aspect, making it more
					;like a copying garbage collector.

;;; Fix some things which the cold-load generator sets up wrong.
(DEFUN GC-ONCE-INIT (&AUX BITS)
  ;; The cold-load does not set up the %%REGION-SCAVENGE-ENABLE bits, and gets
  ;; the REGION-GC-POINTER's wrong, at least in LINEAR-PDL-AREA and LINEAR-BIND-PDL-AREA.
  ;; This then gets propagated through all newly-created regions.
  (DO REGION SIZE-OF-AREA-ARRAYS (1- REGION) (MINUSP REGION)
    (STORE (REGION-GC-POINTER REGION) 0) ;assuming no compact-consing regions yet
    (SETQ BITS (REGION-BITS REGION))
    (STORE (REGION-BITS REGION)
	   (%LOGDPB (SELECT (LDB %%REGION-SPACE-TYPE BITS)
		  ;; These should not be scavenged
		  ((%REGION-SPACE-FREE %REGION-SPACE-EXITED
		    %REGION-SPACE-EXTRA-PDL ;Very important!! This does not follow the
					    ;prescribed protocol for use of header-forward
					    ;and body-forward.  Also this area gets randomly
					    ;reset without interfacing with the scavenger.
		    %REGION-SPACE-WIRED %REGION-SPACE-USER-PAGED) 0)
		  ;; These usually should be scavenged, except for efficiency certain ones
		  ;; that only contain fixnums will be bypassed
		  (%REGION-SPACE-FIXED
		    (COND ((OR (= REGION MICRO-CODE-SYMBOL-AREA)
			       (= REGION PAGE-TABLE-AREA)
			       (= REGION PHYSICAL-PAGE-DATA)
			       (= REGION REGION-ORIGIN)
			       (= REGION REGION-LENGTH)
			       (= REGION REGION-BITS)
			       (= REGION REGION-SORTED-BY-ORIGIN)
			       (= REGION REGION-FREE-POINTER)
			       (= REGION REGION-GC-POINTER)
			       (= REGION REGION-LIST-THREAD)
			       (= REGION AREA-REGION-LIST)
			       (= REGION AREA-REGION-SIZE)
			       (= REGION AREA-MAXIMUM-SIZE)
			       (= REGION MICRO-CODE-ENTRY-AREA)
			       (= REGION MICRO-CODE-ENTRY-MAX-PDL-USAGE))
			   0)
			  (T 1)))
		  ;; Newspace doesn't need scavenging
		  (%REGION-SPACE-NEW 0)
		  ;; Other regions should be scavenged
		  (OTHERWISE 1))
		%%REGION-SCAVENGE-ENABLE BITS)))
  ;; Crank up the default region-size for certain areas
  (DO L '(WORKING-STORAGE-AREA 200000 MACRO-COMPILED-PROGRAM 200000
	  P-N-STRING 200000 NR-SYM 100000)
      (CDDR L) (NULL L)
    (STORE (AREA-REGION-SIZE (SYMEVAL (CAR L))) (CADR L))))

(ADD-INITIALIZATION "GC-ONCE" '(GC-ONCE-INIT) '(ONCE))

;;; Check the size of the free regions in case this is a band
;;; that was shipped over from a machine with a different size paging partition.
(DEFUN GC-CHECK-FREE-REGIONS (PAGE-PART-SIZE)
  ;; Find the size of the paging partition and adjust the free area if necessary.
  ;; The microcode already knows this, but it isn't left around, so read the label again.
  (LET ((HIGHEST-PAGE-USED 0) (PAGE-NUMBER-FIELD 1020))
    (WITHOUT-INTERRUPTS  ;Don't let any allocation happen
      (DO REGION SIZE-OF-AREA-ARRAYS (1- REGION) (MINUSP REGION)
	(LET ((REGION-TOP (+ (LDB PAGE-NUMBER-FIELD (REGION-ORIGIN REGION))
			     (LDB PAGE-NUMBER-FIELD (REGION-LENGTH REGION)))))
	  (SETQ HIGHEST-PAGE-USED (MAX REGION-TOP HIGHEST-PAGE-USED))))
      (DO REGION (AREA-REGION-LIST FREE-AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
	(LET ((REGION-TOP (+ (LDB PAGE-NUMBER-FIELD (REGION-ORIGIN REGION))
			     (LDB PAGE-NUMBER-FIELD (REGION-LENGTH REGION)))))
	  (COND ((OR (> REGION-TOP PAGE-PART-SIZE) (= REGION-TOP HIGHEST-PAGE-USED))
		 (STORE (REGION-LENGTH REGION)
			(%LOGDPB (MAX 0 (- PAGE-PART-SIZE
					   (LDB PAGE-NUMBER-FIELD (REGION-ORIGIN REGION))))
				 PAGE-NUMBER-FIELD 0)))))))))

(DEFUN GC-REPORT-STREAM ()
  (IF (EQ GC-REPORT-STREAM T) (TV:GET-NOTIFICATION-STREAM) GC-REPORT-STREAM))

;;; Flipper

;;; This function performs a flip only if "a good idea", i.e. if the scavenger
;;; is done and we are anywhere near running out of free space.
;;; How close we are to running out of free space is determined by FREE-SPACE-RATIO.
;;; This number should be greater than or equal to 1 if most of the committed free
;;; space contains meaningful data.  If there is a lot of garbage around, then
;;; this number can be less than 1 to reduce the frequency of flips.  The higher
;;; this number is, the greater the chance of actually doing the flip.
;;; The RECLAIM-IMMEDIATELY parameter will cause the scavenger to take off
;;; as soon as the flip is done and reclaim all oldspace.  If you want to
;;; flush all garbage "immediately", call this function with a large ratio
;;; and with a second argument of T.
;;; Returns T if it flipped and NIL if it didn't.
(DEFUN GC-FLIP-MAYBE (&OPTIONAL (FLIP-RATIO GC-FLIP-RATIO) (RECLAIM-IMMEDIATELY NIL))
  (AND %GC-FLIP-READY
       (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
	   (GC-GET-COMMITTED-FREE-SPACE)
	 (COND (( (* FLIP-RATIO COMMITTED-FREE-SPACE) FREE-SPACE)
		(GC-FLIP-NOW)
		(IF RECLAIM-IMMEDIATELY (GC-RECLAIM-OLDSPACE))
		T)))))

;;; This function performs a flip.  It can be called either by the user
;;; or by the GC process, at any time (much faster if scavenger is done already!)
;;; Must return T for GC-FLIP-MAYBE.
(DEFUN GC-FLIP-NOW ()
  (IF (NOT %GC-FLIP-READY) (GC-RECLAIM-OLDSPACE)) ;In case not reclaimed already
  (SETQ %PAGE-CONS-ALARM 0 %REGION-CONS-ALARM 0)  ;avoid overflow in these fixnums
  (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE)
		(GC-GET-SPACE-SIZES)
    (AND GC-REPORT-STREAM
	 (FORMAT (GC-REPORT-STREAM) ;separate static from exited when exited exists?
	     "~&[GC: About to flip.  Dynamic space=~D., Static space=~D., Free space=~D.]~%"
	     DYNAMIC-SIZE (+ STATIC-SIZE EXITED-SIZE) FREE-SIZE))
    ;; Perform whatever actions other programs need to do on flips
    (MAPC #'EVAL GC-EVERY-FLIP-LIST)
    (MAPC #'EVAL (PROG1 GC-NEXT-FLIP-LIST
			(SETQ GC-NEXT-FLIP-LIST GC-SECOND-NEXT-FLIP-LIST
			      GC-SECOND-NEXT-FLIP-LIST NIL)))
    ;; Reset the GC scan pointers of all regions, actually only in static and fixed areas
    ;; is it necessary.
    (DO REGION SIZE-OF-AREA-ARRAYS (1- REGION) (MINUSP REGION)
      (STORE (REGION-GC-POINTER REGION) 0))
    ;; Don't forget to actually flip! (Change newspace to oldspace in all dynamic areas)
    (%GC-FLIP T)
    (MAPC #'EVAL GC-AFTER-FLIP-LIST)
    T))

;;; Compute total occupation of dynamic space, static space, exited space, free space,
;;; and old space.
;;; Returns those as five values
(DEFUN GC-GET-SPACE-SIZES ()
  (DO ((REGION SIZE-OF-AREA-ARRAYS (1- REGION))
       (SZ)
       (DYNAMIC-SIZE 0)
       (STATIC-SIZE 0)
       (EXITED-SIZE 0)
       (FREE-SIZE 0)
       (OLD-SIZE 0))
      ((MINUSP REGION)
       (DO REGION (AREA-REGION-LIST FREE-AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
         (SETQ FREE-SIZE (+ (24-BIT-UNSIGNED (REGION-LENGTH REGION)) FREE-SIZE)))
       (RETURN DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE OLD-SIZE))
    (SETQ SZ (24-BIT-UNSIGNED (REGION-FREE-POINTER REGION)))
    (SELECT (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION))
      ((%REGION-SPACE-NEW %REGION-SPACE-COPY)
	(SETQ DYNAMIC-SIZE (+ SZ DYNAMIC-SIZE)))
      (%REGION-SPACE-OLD
	(SETQ OLD-SIZE (+ SZ OLD-SIZE)))
      ((%REGION-SPACE-STATIC %REGION-SPACE-FIXED %REGION-SPACE-EXIT)
	(SETQ STATIC-SIZE (+ SZ STATIC-SIZE)))
      (%REGION-SPACE-EXITED
        (SETQ EXITED-SIZE (+ SZ EXITED-SIZE))))))

;;; If called when %GC-FLIP-READY is true, returns a conservative (over) estimate of
;;; the amount of free space which will be used up during the next cycle before
;;; %GC-FLIP-READY can set again.  This is based on the way consing drives scavenging.
;;; Also returns the current amount of free space since it happens to know it.

;;; In the below, the size of static and dynamic spaces are at the time of the flip,
;;; which is bigger than the current values.  The objective is to compute how much
;;; bigger they can be allowed to grow.

;;; Total scavenger work = amount of static space to be scavenged
;;;			   + 2 x dynamic space (which is both scavenged and copied)
;;;			   + scavenging of static space consed after the flip
;;;				[dynamic space consed after the flip is newspace
;;;				 rather than copyspace and need not be scavenged]
;;; Total consing (consumption of free space) =
;;;			(1/K) x scav work
;;;			+ amount of dynamic space which is copied
;;;			+ region breakage
;;; K=4 in the current microcode
;;;
;;; Uncertainties which can use up more free space:
;;;	Consing after the flip in static space rather than dynamic or exited space
;;;	Region breakage
;;;	Consing during GC process wakeup delay
;;; Uncertainties which decrease consumption of free space:
;;;	Scavenging by the idle-loop rather than by CONS
;;;	Certain fixed areas which count as static space but aren't actually scavenged
;;;	Shrinkage of dynamic space (generally some is garbage and will be
;;;		neither copied nor scavenged)
;;;	Consing of additional static space before the flip, which is less
;;;		expensive than additional dynamic space.
;;;	Space already assigned to regions but not yet allocated by CONS
;;;
;;; For maximum delay of fliping, we want to allow enough consing before the
;;; flip so that the remaining free space is exactly equal to the consing after
;;; the flip.  The algebraic manipulation is as follows (incorporating the
;;; worst case assumptions: no garbage, all consing before flip is dynamic,
;;; all consing after flip is static).  Normally I wouldn't bother commenting
;;; this but several people have got it wrong, so it must be hard.
;;;	F0 = free space now
;;;	D0 = dynamic space now
;;;	ND = additional dynamic space consed before the flip
;;;	S0 = static space now
;;;	W  = scavenger work to do after the flip
;;;	C  = consing required after the flip.
;;;
;;; C = D0 + ND + W/k		;copy all dynamic plus do necessary scavenger work
;;; W = S0 + 2(D0 + ND) + C	;scav static, scav and copy all dynamic, scav new static
;;; F0 = ND + C			;free space divided between before & after consing
;;; 
;;; (k-1)C = (k+2)(D0+ND) + S0		;plugging in for W and collecting C on the left
;;; (k-1)C = (k+2)D0 + S0 + (k+2)(F0-C)	;plugging in for ND
;;; C = [ (k+2)D0 + S0 + (k+2)F0 ] / (2k+1)	;solving for C
;;; Note that old-space is counted as free.

(DEFUN GC-GET-COMMITTED-FREE-SPACE ( &AUX (K 4) )	;K is the magic constant
  (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE FREE-SIZE OLD-SIZE)
	(GC-GET-SPACE-SIZES)
    (SETQ FREE-SIZE (+ FREE-SIZE OLD-SIZE))	;Old space will be reclaimed
    EXITED-SIZE					;Scavenger never deals with exited space
    (LET ((CONSING (// (+ (* (+ K 2) (+ DYNAMIC-SIZE FREE-SIZE)) STATIC-SIZE)
		       (+ (* 2 K) 1))))
      (PROG () (RETURN (+ CONSING 1000000) ;add 256K as a fudge for region breakage
		       FREE-SIZE)))))

;;; Print various statistics
(DEFUN GC-ROOM (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
		(GC-GET-COMMITTED-FREE-SPACE)
    (MULTIPLE-VALUE-BIND (DYNAMIC-SIZE STATIC-SIZE EXITED-SIZE IGNORE OLD-SIZE)
		(GC-GET-SPACE-SIZES)
	 (FORMAT STREAM "~&Dynamic (new+copy) space ~D., Old space ~D., Static space ~D.,
Exited space ~D., Free space ~D., Committed guess ~D., leaving ~D.~%"
		   DYNAMIC-SIZE OLD-SIZE STATIC-SIZE
		   EXITED-SIZE FREE-SPACE COMMITTED-FREE-SPACE
		   (- FREE-SPACE COMMITTED-FREE-SPACE))))
  (FORMAT STREAM "Garbage collector process state: ~A~%"
	  (IF (AND (BOUNDP 'GC-PROCESS) (ASSQ GC-PROCESS ACTIVE-PROCESSES))
	      (IF %GC-FLIP-READY "Await Flip" "Await Scavenge")
	      "Disabled"))
  (FORMAT STREAM "Scavenging during cons: ~:[On~;Off~], Idle scavenging: ~:[On~;Off~]~%"
	  INHIBIT-SCAVENGING-FLAG INHIBIT-IDLE-SCAVENGING-FLAG)
  (FORMAT STREAM "GC Flip Ratio: ~D, GC Reclaim Immediately: ~:[Off~;On~]~%"
	  GC-FLIP-RATIO GC-RECLAIM-IMMEDIATELY))

;;; This function gets rid of oldspace.
(DEFUN GC-RECLAIM-OLDSPACE ()
  ;; Make sure all regions are clean (no pointers to oldspace)
  (DO ((%SCAVENGER-WS-ENABLE NIL))  ;Use all of memory as long as using all of processor
      (%GC-FLIP-READY)	;Stop when scavenger says all is clean
    (%GC-SCAVENGE 10000))
  ;; Report oldspace statistics
  (COND (GC-REPORT-STREAM
	 (DO ((REGION SIZE-OF-AREA-ARRAYS (1- REGION))
	      (OLD-TOTAL-SIZE 0)
	      (OLD-USED-SIZE 0))
	     ((MINUSP REGION)
	      (FORMAT (GC-REPORT-STREAM)
		      "~&[GC: Flushing oldspace.  allocated=~D., used=~D.]~%"
		      OLD-TOTAL-SIZE OLD-USED-SIZE))
	   (COND ((= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)) %REGION-SPACE-OLD)
		  (SETQ OLD-TOTAL-SIZE (+ (24-BIT-UNSIGNED (REGION-LENGTH REGION))
					  OLD-TOTAL-SIZE)
			OLD-USED-SIZE (+ (24-BIT-UNSIGNED (REGION-FREE-POINTER REGION))
					 OLD-USED-SIZE)))))))
  ;; Discard oldspace
  (DOLIST (AREA AREA-LIST)
    (LET ((AREA-NUMBER (SYMEVAL AREA)))
      (AND (OR (MINUSP AREA-NUMBER) (> AREA-NUMBER SIZE-OF-AREA-ARRAYS))
           (FERROR NIL "Area-symbol ~S clobbered" AREA)) ;don't get grossly faked out
      (GC-RECLAIM-OLDSPACE-AREA AREA-NUMBER))))

;;; GC-RECLAIM-OLDSPACE-AREA - deletes all old-space regions of a specified area,
;;; unthreading from the lists, and returning the virtual memory to free.
;;; Call this for each area, if %GC-FLIP-READY is true and before calling %GC-FLIP.
;;; Note that if an area has only one oldspace region, we have a problem with
;;; losing the REGION-BITS.  For now just keep around one region.  This only
;;; happens when an area is completely disused.
(DEFUN GC-RECLAIM-OLDSPACE-AREA (AREA)
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
	     "an area number")
  (OR %GC-FLIP-READY
      (FERROR NIL "You cannot reclaim oldspace now, there may be pointers to it"))
  (WITHOUT-INTERRUPTS
    (DO ((REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION))
	 (REGION-TO-FREE)
	 (PREV-REGION NIL REGION))
	(NIL)
     NEXTLOOP ;May GO here to avoid advancing DO variables
      (AND (MINUSP REGION) (RETURN NIL))
      (AND (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)) %REGION-SPACE-OLD)
	   ;; Free this region unless that would leave the area without any regions
	   ;; at all, which would lose since there would be no place to remember its bits.
	   ;; Before freeing, unthread from area's region list.
	   (COND ((OR PREV-REGION (NOT (MINUSP (REGION-LIST-THREAD REGION))))
		  (SETQ REGION-TO-FREE REGION
			REGION (REGION-LIST-THREAD REGION))
		  (IF PREV-REGION (STORE (REGION-LIST-THREAD PREV-REGION) REGION)
				  (STORE (AREA-REGION-LIST AREA) REGION))
		  (%GC-FREE-REGION REGION-TO-FREE)
		  (GO NEXTLOOP))))
      (AND (= (LDB %%REGION-SPACE-TYPE (REGION-BITS REGION)) %REGION-SPACE-COPY)
           ;;; Change this region to NEW space so that it can be used for normal
           ;;; consing
           (STORE (REGION-BITS REGION) (%LOGDPB 0 %%REGION-SCAVENGE-ENABLE
						(%LOGDPB %REGION-SPACE-NEW
							 %%REGION-SPACE-TYPE
							 (REGION-BITS REGION))))))))

;;; GC Process

;;; This function runs in a separate process.  It wakes up when oldspace needs
;;; to be reclaimed and when a flip is required, and does them.
;;;*** Doesn't yet know about finite number of regions ***
(DEFUN GC-PROCESS ()
  (DO () (NIL)	;Do forever
    (IF (NOT GC-RECLAIM-IMMEDIATELY)
	;;Wait until scavenger done, unless we aren't doing real-time
	;;garbage collection.
	(PROCESS-WAIT "Await scav" #'(LAMBDA () %GC-FLIP-READY)))
    ;;Then flush oldspace and print "flushing oldspace" message.  A complete
    ;;scavenge will take place here if %GC-FLIP-READY is NIL.
    (GC-RECLAIM-OLDSPACE)
    (DO () (NIL) ;May iterate a few times before flipping
      (OR %GC-FLIP-READY (RETURN NIL))	;Some other process must have flipped first
      (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
	     (GC-GET-COMMITTED-FREE-SPACE)
	;;Hook to let the user influence how conservative the garbage
	;;collector will be.  GC-FLIP-RATIO may be a flonum.
	(SETQ COMMITTED-FREE-SPACE (FIX (* GC-FLIP-RATIO COMMITTED-FREE-SPACE)))
	(COND (( COMMITTED-FREE-SPACE FREE-SPACE)	;Better flip now
	       (RETURN (GC-FLIP-NOW))) ;*** slight window for other process to flip first ***
	      (T		;Wait a while before flipping, then compute frob again
	       (SETQ %PAGE-CONS-ALARM 0)
	       (AND GC-REPORT-STREAM
		    (FORMAT (GC-REPORT-STREAM)
			    "~&[GC: Allowing ~D. words more consing before flip.]~%"
			    (- FREE-SPACE COMMITTED-FREE-SPACE)))
	       (SETQ GC-PAGE-CONS-ALARM-MARK
		     (// (- FREE-SPACE COMMITTED-FREE-SPACE) PAGE-SIZE))
	       (PROCESS-WAIT "Await flip"
			     #'(LAMBDA () (OR (NOT %GC-FLIP-READY)
					      (> %PAGE-CONS-ALARM
						 GC-PAGE-CONS-ALARM-MARK))))))))))

;;; Function to turn on the garbage collector
(DEFUN GC-ON ()
  (OR (BOUNDP 'GC-PROCESS)
      (SETQ GC-PROCESS (PROCESS-CREATE "Garbage Collector")))
  (PROCESS-PRESET GC-PROCESS #'GC-PROCESS)
  (PROCESS-ENABLE GC-PROCESS 'GC-PROCESS) ;Start flipper process
  (SETQ INHIBIT-SCAVENGING-FLAG NIL ;Enable scavenging during cons
        INHIBIT-IDLE-SCAVENGING-FLAG NIL) ;Enable scavenging by scheduler during idle
  (ADD-INITIALIZATION "GC-PROCESS" '(GC-ON) '(WARM))) ;Do this on future warm boots

;;; Function to shut off the garbage collector
(DEFUN GC-OFF ()
  (DELETE-INITIALIZATION "GC-PROCESS" '(WARM)) ;Don't start GC on warm boots anymore
  (PROCESS-DISABLE GC-PROCESS 'GC-PROCESS) ;Disable flipper process
  (SETQ INHIBIT-SCAVENGING-FLAG T ;Disable scavenging during cons
        INHIBIT-IDLE-SCAVENGING-FLAG T)) ;Disable scavenging during idle time

;;; Function to be called by user if running for a long time with interrupts off.
;;; Does a flip if necessary
(DECLARE (SPECIAL GC-RECLAIMED-OLDSPACE))

(SETQ GC-RECLAIMED-OLDSPACE NIL)

(DEFUN GC-FLIP-IF-NECESSARY ()
  (WITHOUT-INTERRUPTS				;Don't give the other process a chance to
						; have timing screws and get flipped twice
   (COND ((AND %GC-FLIP-READY (NOT GC-RECLAIMED-OLDSPACE))
	  (GC-RECLAIM-OLDSPACE)
	  (SETQ GC-RECLAIMED-OLDSPACE T)))
   (COND ((AND %GC-FLIP-READY
	       (> %PAGE-CONS-ALARM GC-PAGE-CONS-ALARM-MARK))
	  (MULTIPLE-VALUE-BIND (COMMITTED-FREE-SPACE FREE-SPACE)
	      (GC-GET-COMMITTED-FREE-SPACE)
	    (COND ((>= COMMITTED-FREE-SPACE FREE-SPACE)	;Better flip now
		   (SETQ GC-PAGE-CONS-ALARM-MARK -1
			 GC-RECLAIMED-OLDSPACE NIL)
		   (GC-FLIP-NOW))
		(T		;Wait a while before flipping, then compute frob again
		 (AND GC-REPORT-STREAM
		      (FORMAT (GC-REPORT-STREAM)
			      "~&[GC: Allowing ~D. words more consing before flip.]~%"
			      (- FREE-SPACE COMMITTED-FREE-SPACE)))
		 (SETQ %PAGE-CONS-ALARM 0
		       GC-PAGE-CONS-ALARM-MARK (// (- FREE-SPACE COMMITTED-FREE-SPACE)
						   PAGE-SIZE)))))))))

;;; Make a dynamic area static.  On the next flip, when it's all been compacted
;;; into new/copy space, change the space-type to static.
(DEFUN MAKE-AREA-STATIC (AREA)
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
	     "an area number")
  (PUSH `(MAKE-AREA-STATIC-INTERNAL ,AREA) GC-NEXT-FLIP-LIST)
  T)

(DEFUN MAKE-AREA-STATIC-INTERNAL (AREA)
  (WITHOUT-INTERRUPTS
    (DO REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
      (LET ((BITS (REGION-BITS REGION)))
	(SELECT (LDB %%REGION-SPACE-TYPE BITS)
	  ((%REGION-SPACE-NEW %REGION-SPACE-COPY)
	     (STORE (REGION-BITS REGION)
		    (%LOGDPB 1 %%REGION-SCAVENGE-ENABLE
			     (%LOGDPB %REGION-SPACE-STATIC %%REGION-SPACE-TYPE BITS)))))))))

;;; Make a static area dynamic.  This can happen right away, although it really
;;; only takes effect on the next flip, when the area will acquire its first oldspace.
(DEFUN MAKE-AREA-DYNAMIC (AREA)
  (CHECK-ARG AREA (AND (NUMBERP AREA) ( AREA 0) ( AREA SIZE-OF-AREA-ARRAYS))
	     "an area number")
  (WITHOUT-INTERRUPTS
    (DO REGION (AREA-REGION-LIST AREA) (REGION-LIST-THREAD REGION) (MINUSP REGION)
      (LET ((BITS (REGION-BITS REGION)))
	(AND (= (LDB %%REGION-SPACE-TYPE BITS) %REGION-SPACE-STATIC)
	     (STORE (REGION-BITS REGION)
		    (%LOGDPB %REGION-SPACE-NEW %%REGION-SPACE-TYPE BITS)))))))

;;; "Clean up" a static area by garbage collecting it once, thus compactifying
;;; it and freeing anything it points to.  This works by changing the area to dynamic,
;;; then after the next flip it will all be in oldspace.  On the flip after that,
;;; the non-garbage contents will have moved into new/copy space, and we can change
;;; the area's type back to static.  Note, while all this is going on, you better
;;; not change your mind.
(DEFUN CLEAN-UP-STATIC-AREA (AREA)
  (CHECK-ARG AREA (AND (NUMBERP AREA)
		       ( AREA 0)
		       ( AREA SIZE-OF-AREA-ARRAYS)
		       (= (LDB %%REGION-SPACE-TYPE (REGION-BITS (AREA-REGION-LIST AREA)))
			  %REGION-SPACE-STATIC))
	     "the area number of a static area")
  (PUSH `(MAKE-AREA-STATIC-INTERNAL ,AREA) GC-SECOND-NEXT-FLIP-LIST)
  (MAKE-AREA-DYNAMIC AREA))

;;; Peek display

;;; Hash arrays

;;; Weak links
