;-*-LISP-*-

(DECLARE (SETQ OPEN-CODE-MAP-SWITCH T))

;A PLANE is an array whose bounds, in each dimension,
;are plus-infinity and minus-infinity.  All integers are legal as indices.
;Planes are distinguished not by size and shape, but by number of dimensions alone.
;When a plane is created, a "default value" must be specified.
;At that moment, every component of the plane has that value.
;As you can't ever change more than a finite number of components,
;only a finite region of the plane need actually be stored.

;You can use MAKE-PLANE to create a plane,
;PLANE-REF or PLANE-AR-N to get the value of a component,
;PLANE-STORE or PLANE-AS-N to store into a component.
;ARRAY-#-DIMS will work on a plane.

;A plane is actually stored as an array with a leader.
;The array corrsponds to a rectangular, aligned region of the plane,
;containing all the components in which a PLANE-STORE has been done
;(and others, in general, whcih have never been altered).
;The lowest-co-ordinate corner of that rectangular region is
;given by the PLANE-ORIGIN in the array leader.
;The highest co-ordinate corner can be found by adding the PLANE-ORIGIN
;to the ARRAY-DIMENSIONS of the array.
;The PLANE-DEFAULT is the contents of all the
;elements of the plane which are not actually stored in the array.
;The PLANE-EXTENSION is the amount to extend a plane by in any direction
;when the plane needs to be extended.  The default is 32.

(DEFMACRO PLANE-ORIGIN (PLANE) `(ARRAY-LEADER ,PLANE 0))
(DEFMACRO PLANE-DEFAULT (PLANE) `(ARRAY-LEADER ,PLANE 1))
(DEFMACRO PLANE-EXTENSION (PLANE) `(ARRAY-LEADER ,PLANE 2))

(DEFUN PLANE-AR-N (PLANE &REST POINT)
    (PLANE-REF PLANE POINT))

(DEFUN PLANE-AS-N (DATUM PLANE &REST POINT)
    (PLANE-STORE DATUM PLANE POINT))

;Access the element of PLANE at co-ordinates POINT.
;Absolutely any point is legal.
(DEFUN PLANE-REF (PLANE POINT)
    (DO ((PT POINT (CDR PT)) (PO (PLANE-ORIGIN PLANE) (CDR PO))) ((NULL PT))
        (RPLACA PT (- (CAR PT) (CAR PO))))
    (COND ((LEXPR-FUNCALL 'ARRAY-IN-BOUNDS-P PLANE POINT)
	   (APPLY PLANE POINT))
	  (T (PLANE-DEFAULT PLANE))))

;Store DATUM in PLANE at co-ordinates POINT.
;PLANE is extended if necessary.
(DEFUN PLANE-STORE (DATUM PLANE POINT &AUX POINT1)
    (SETQ POINT1 (MAPCAR (FUNCTION -) POINT (PLANE-ORIGIN PLANE)))
    (COND ((NOT (APPLY 'ARRAY-IN-BOUNDS-P (CONS PLANE POINT1)))
	   (PLANE-EXTEND PLANE POINT)
	   (STORE (APPLY PLANE (MAPCAR (FUNCTION -) POINT (PLANE-ORIGIN PLANE))) DATUM))
	  (T (STORE (APPLY PLANE POINT1) DATUM))))

(DEFUN PLANE-EXTEND (PLANE POINT 
			   &AUX TOP-EXTEND BOTTOM-EXTEND NEW-PLANE
			   TEM OLD-DIMS (MIN (PLANE-EXTENSION PLANE)))
       (SETQ OLD-DIMS (ARRAY-DIMENSIONS PLANE))
       (SETQ BOTTOM-EXTEND
	     (MAPCAR (FUNCTION (LAMBDA (PT OLD-BOT)
		         (SETQ TEM (- OLD-BOT PT))
			 (COND ((<= TEM 0) 0)
			       (T (MAX TEM MIN)))))
		     POINT
		     (PLANE-ORIGIN PLANE)))
       (SETQ TOP-EXTEND
	     (MAPCAR (FUNCTION (LAMBDA (PT OLD-BOT OLD-LEN)
		         (SETQ TEM (1+ (- PT OLD-BOT OLD-LEN)))
			 (COND ((<= TEM 0) 0)
			       (T (MAX TEM MIN)))))
		     POINT
		     (PLANE-ORIGIN PLANE)
		     OLD-DIMS))
       (COND ((AND (ZEROP (APPLY '+ BOTTOM-EXTEND))
		   (ZEROP (APPLY '+ TOP-EXTEND))))
	     (T
	      (SETQ NEW-PLANE (MAKE-PLANE-INTERNAL
					  (ARRAY-TYPE PLANE)
					  (MAPCAR (FUNCTION +)
						  OLD-DIMS
						  BOTTOM-EXTEND
						  TOP-EXTEND)
					  (MAPCAR (FUNCTION -)
						  (PLANE-ORIGIN PLANE)
						  BOTTOM-EXTEND)
					  (PLANE-DEFAULT PLANE)
                                          (PLANE-EXTENSION PLANE)))
	      (PLANE-COPY PLANE NEW-PLANE)
	      (STRUCTURE-FORWARD PLANE NEW-PLANE)))
       PLANE)

;Make a new plane, for the user.  Specify the array type, the number of dimensions,
;and the default element.
(DEFUN MAKE-PLANE (TYPE RANK DEFAULT &OPTIONAL (EXTENSION 32.) &AUX SIZE ORIGIN)
  ;; SIZE gets a list of 1's, as many as there are dimensions.
  (SETQ SIZE (MAKE-LIST DEFAULT-CONS-AREA RANK))
  (DO L SIZE (CDR L) (NULL L)
    (SETF (CAR L) 1))
  ;; ORIGIN gets a similar list of zeros.
  (SETQ ORIGIN (MAKE-LIST DEFAULT-CONS-AREA RANK))
  (DO L ORIGIN (CDR L) (NULL L)
    (SETF (CAR L) 0))
  (MAKE-PLANE-INTERNAL TYPE SIZE ORIGIN DEFAULT EXTENSION))

;Create a new plane of specified type (an array type) and default value,
;with a specified region in actual existence.
(DEFUN MAKE-PLANE-INTERNAL (TYPE SIZE ORIGIN DEFAULT EXTENSION &AUX PLANE INDEX)
       (SETQ PLANE (MAKE-ARRAY NIL TYPE SIZE NIL 3))
       (SETF (PLANE-DEFAULT PLANE) DEFAULT)
       (SETF (PLANE-ORIGIN PLANE) ORIGIN)
       (SETF (PLANE-EXTENSION PLANE) EXTENSION)
       (SETQ INDEX (MAPCAR (FUNCTION -) SIZE SIZE))
       (PROG ()
	   LOOP
	     (STORE (APPLY PLANE INDEX) DEFAULT)
	     (AND (DO ((OI INDEX (CDR OI))
		      (DIMS SIZE (CDR DIMS)))
		     ((NULL OI))
		     (RPLACA OI (1+ (CAR OI)))
		     (OR (< (CAR OI) (CAR DIMS))
			 (RPLACA OI 0))
		     (OR (ZEROP (CAR OI))
			 (RETURN T)))
		 (GO LOOP))
	     (RETURN PLANE)))

(DEFUN PLANE-COPY (OLD NEW &AUX OLD-ORIGIN NEW-ORIGIN OLD-DIMS
		                OLD-INDICES NEW-INDICES)
  (PROG NIL
    ;; OLD-ORIGIN and NEW-ORIGIN are the origins (lowest corners) of the planes.
    ;; OLD-DIMS is the list of actual dimensions of the old plane. 
    (SETQ OLD-ORIGIN (PLANE-ORIGIN OLD))
    (SETQ NEW-ORIGIN (PLANE-ORIGIN NEW))
    (SETQ OLD-DIMS (ARRAY-DIMENSIONS OLD))
    (AND (ZEROP (APPLY '+ OLD-DIMS)) (RETURN NEW))
    ;; OLD-INDICES has the real indices in the old plane of a point.
    ;; NEW-INDICES has the corresponding indices in the new plane.
    ;; We update both lists simultaneously by RPLACA to avoid consing.
    (SETQ OLD-INDICES (MAPCAR (FUNCTION -) OLD-ORIGIN OLD-ORIGIN))
    (SETQ NEW-INDICES (MAPCAR (FUNCTION -) OLD-ORIGIN NEW-ORIGIN))
  LOOP
    (STORE (APPLY NEW NEW-INDICES)
	   (APPLY OLD OLD-INDICES))
    (OR (DO ((OI OLD-INDICES (CDR OI))
	     (NI NEW-INDICES (CDR NI))
	     (DIMS OLD-DIMS (CDR DIMS))
	     (NEW-ORIGIN NEW-ORIGIN (CDR NEW-ORIGIN))
	     (OLD-ORIGIN OLD-ORIGIN (CDR OLD-ORIGIN)))
	    ((NULL OI))
	  (RPLACA OI (1+ (CAR OI)))
	  (OR (< (CAR OI) (CAR DIMS))
	      (RPLACA OI 0))
	  (RPLACA NI (+ (- (CAR OI) (CAR NEW-ORIGIN)) (CAR OLD-ORIGIN)))
	  (OR (ZEROP (CAR OI))
	      (RETURN T)))
	(RETURN NEW))
    (GO LOOP)))
