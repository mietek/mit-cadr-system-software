;-*-Mode: Lisp; Package:System-Internals-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;A frame is a window which holds one or more other windows side-by-side.
;The contained windows are called the panes of the frame.
;Each of them has a pointer back to the frame in its FRAME variable.

;No window which is a pane is ever actually exposed or active at top level
;in that none is ever on EXPOSED-WINDOWS or ACTIVE-WINDOWS.  Instead, the frame is on them.
;Similarly, SELECTED-WINDOW can never be anything which is a pane.
;However, a pane's STATUS can be :SELECTED or :EXPOSED.  A pane's status is
;:EXPOSED whenever the frame's is so.  A pane's status can be :SELECTED if
;the frame is selected.  A pane can still
;be sent a :EXPOSE or :SELECT message, which will work by being passed along to the frame.

;(At most) One of the panes is the selected pane.
;When the frame is selected, it "passes on" its selection to the selected pane, if any.
;When as a result of selection or deselection of the frame
;or a change in which pane is selected this status changes,
;the appropriate pane(s) will receive :select or :deselect messages.
;Sending a random :select message to a pane will pass it along
;to the frame and also make the frame consider that pane the selected one.

;When the mouse is inside the frame, the frame passes along handling of
;the mouse to whichever pane the mouse is inside.  This pane gets to handle the mouse
;just as it would if the pane were actually a top-level window.
;The frame together with MOUSE-DEFAULT-HANDLER contrive to transfer control
;of the mouse from one pane to another when appropriate.

;Assign panes to a frame by sending the frame a :PANES<- message.
;This will tell the panes they now have a frame.  Do not send any window
;a :FRAME<- message if you are not a frame!
;The :PANE<- message can be used to give a frame a single pane which fills it entirely,
;and selects it.  This saves you the trouble of setting the edges of the pane.
;When you do have to set the edges of panes, an edge which is up against the edge
;of the frame should be set to the value of the edge of the frame plus (or minus)
;the margin of the frame at that edge (an element of MARGINS, either 0 or 2 except
;for the bottom one).  Two touching panes should have edges which differ by 3
;if they are to have a single-width line separating them.  When the size of a frame
;is changed, the sizes of the panes will be scaled with it linearly so as to preserve
;such relationships.

;It is sometimes useful to change panes in a frame temporarily.
;The macro (WINDOW-FRAME-SAVE-PANES frame . body) is useful for this.
;It saves the current settings with an UNWIND-PROTECT.  Inside the body
;send the frame a :PANE<- or :PANES<- message to make the temporary change.

;A frame has, in addition to its panes, a label.  This is displayed at the bottom
;of the frame, inside its bottom margin.  By default the label text is just the name
;of the frame.  It can be overridden with any string using the :LABEL<- operation.
;Labels computed for each redisplay can be had by redefining the :PRINT-LABEL method
;in a subclass of WINDOW-FRAME-CLASS.  Send the frame a :UPDATE-LABEL message to cause it
;to redisplay its label (and nothing else).

;WINDOW-SINGLE-FRAME-CLASS is designed specifically for serving as a box around one
;other window, which will be the sole pane.  This type of frame gives control over what is
;displayed in the label to the pane, or to the selected pane if there are several panes.
;This is done by passing the operations :LABEL-HEIGHT and :PRINT-LABEL to the pane.
;The operations :LINE-HEIGHT and :MOVE-NEAR are also passed along.

;(defclass window-frame-class window-class
;    (panes selected-pane reset-panes reset-selected-pane active-flag
;     label label-position label-needs-updating-flag margins))

;RESET-PANES and RESET-SELECTED-PANE are used to set the panes and selected pane
;when the frame receives a :RESET message.
;LABEL if non-NIL is the string to display in the label.  If NIL, the window name is used.
;LABEL-POSITION is a list of four edges of the region of screen used for the label.
;It is set when the edges are set.
;LABEL-NEEDS-UPDATING-FLAG if T means that the label should be redisplayed.
;MARGINS is the list of four margin-widths.  It is returned by :MARGINS.

(defmethod (window-frame-class :born) ()
    (<-as window-class ':born)
    (funcall self ':edges<- left top right bottom))

;Assign the frame a new list of panes.
;At the moment, nothing checks that they don't overlap.
;For that matter, when you change their edges, nothing checks that
;or checks that you haven't moved them outside the frame!
;Something will be done about that.
(defmethod (window-frame-class :panes<-) (new-panes &aux pane-edges)
  (maybe-lock-screen-layout status
    (or (and left top right bottom)
	(ferror nil "Attempt to assign panes to ~S before edges" self))
    (dolist (pane new-panes)
      (setq pane-edges (<- pane ':edges))
      (or (and ( (+ left (first margins)) (first pane-edges))
	       ( (+ top (second margins)) (second pane-edges))
	       ( (third pane-edges) (- right (third margins)))
	       ( (fourth pane-edges) (- bottom (fourth margins))))
	  (ferror nil "~S doesn't fit within ~S" pane self)))
    (and selected-pane (eq status ':selected)
	 (<- selected-pane ':deselect))
    (and active-flag (funcall self ':revoke-run-reason))
    (dolist (pane panes)
      (<- pane ':frame<- nil)
      ;Don't deexpose unless necessary becuase screen layout  may not be locked.
      ;Assumption:  if the frame is not exposed, niether are the panes.
      ;Otherwise we get an extraneous "screen layout not locked" message.
      (and (<- pane ':status) (<- pane ':deexpose)))
    (setq panes new-panes)
    (and status (funcall self ':clean))
    (dolist (pane panes)
      (<- pane ':deactivate)
      (<- pane ':frame<- self)
      (and status (<- pane ':expose)))
    (and active-flag (funcall self ':run-reason))
    (setq selected-pane nil)))

;Specify just one pane, set its edges automatically, and select it.
(defmethod (window-frame-class :pane<-) (new-pane)
    (or (and left top right bottom)
	(ferror nil "Attempt to assign panes to ~S before edges" self))
    (funcall new-pane ':edges<-
	     (+ left (first margins))
	     (+ top (second margins))
	     (- right (third margins))
	     (- bottom (fourth margins)))
    (funcall self ':panes<- (list new-pane))
    (<- self ':selected-pane<- new-pane))

;Remember the current set of panes, before changing them temporarily.
;Returns something that you can give to the :restore-panes operation
;to restore them.  Also arranges for a :reset to restore them.
(defmethod (window-frame-class :save-panes) ()
    (cond ((null reset-panes)
	   (setq reset-panes panes
		 reset-selected-pane selected-pane)))
    (cons selected-pane panes))

;To restore the panes saved by a :save-panes, call this operation
;with the value returned by :save-panes as argument.
(defmethod (window-frame-class :restore-panes) (save-info)
    (and (eq (cdr save-info) reset-panes)
	 (setq reset-panes nil reset-selected-pane nil))
    (funcall self ':panes<- (cdr save-info))
    (funcall self ':selected-pane<- (car save-info)))

;(defmethod (window-frame-class :margins) () margins)

(defmethod (window-frame-class :margins-for) (pane &aux pane-edges)
    (cond ((memq pane panes)
	   (setq pane-edges (<- pane ':edges))
	   (list (- (first pane-edges) left)
		 (- (second pane-edges) top)
		 (- right (third pane-edges))
		 (- bottom (fourth pane-edges))))
	  (t (ferror nil "~S is not a pane of ~S" pane self))))

;Return T if the specified edges would be ok to use.
(defmethod (window-frame-class :edges-ok-p) (new-left new-top new-right new-bottom
					     &aux new-margins old-xorg old-yorg
					     new-xorg new-yorg hscale vscale tem)
    (multiple-value (tem new-left new-top new-right new-bottom)
	(<-as window-class ':edges-ok-p new-left new-top new-right new-bottom))
    (prog ()
	  (or tem (return nil))
	  (or panes (return t new-left new-top new-right new-bottom))
	  (cond ((and (eq new-left (screen-x1 screen))
		      (eq new-top (screen-y1 screen))
		      (eq new-right (screen-x2 screen))
		      (eq new-bottom (screen-y2 screen)))
		 (setq new-margins (list 0 0 0 (<- self ':label-height))))
		(t
		 (setq new-margins (list 2 2 2 (+ 2 (<- self ':label-height))))))
	  ;; Compute parameters for scaling the panes' edges with a linear transformation.
	  ;; Each pane is scaled, not by its actual edges, but by the place where the pane's
	  ;; own box should go.  The transformation is chosen so that a pane which used to
	  ;; fill the whole frame will still do so.
	  (setq old-xorg (+ left (first margins) -2)
		old-yorg (+ top (second margins) -2)
		new-xorg (+ new-left (first new-margins) -2)
		new-yorg (+ new-top (second new-margins) -2))
	  (setq hscale (// (float (- new-right -1 (third new-margins) new-xorg))
			   (- right -1 (third margins) old-xorg))
		vscale (// (float (- new-bottom -1 (fourth new-margins) new-yorg))
			   (- bottom -1 (fourth margins) old-yorg)))
	(return
	  (not (dolist (pane panes)
		 (or (funcall pane ':edges-ok-p
			      (+ new-xorg 2 (fix (+ .5S0 (* hscale (- (funcall pane ':left)
								      2 old-xorg)))))
			      (+ new-yorg 2 (fix (+ .5S0 (* vscale (- (funcall pane ':top)
								      2 old-yorg)))))
			      (+ new-xorg -1 (fix (+ .5S0 (* hscale (- (funcall pane ':right)
								       -1 old-xorg)))))
			      (+ new-yorg -1 (fix (+ .5S0 (* vscale (- (funcall pane ':bottom)
								       -1 old-yorg))))))
		     (return t))))
	  new-left new-top new-right new-bottom)))

;Set the edges of the frame to new settings.  Error if too small.
;All the panes scale in proportion to the frame.
(defmethod (window-frame-class :edges<-)
	   (n-left n-top n-right n-bottom
		&aux new-margins hscale vscale tem
		     old-xorg new-xorg old-yorg new-yorg)		     
    (let ((old-status status) (o-left left) (o-top top)
          (o-right right) (o-bottom bottom)
	  ;; Copy args so they will still be there if we give "too small" error
	  (new-left n-left) (new-top n-top) (new-right n-right) (new-bottom n-bottom)) 
      (lock-screen-layout
        (unwind-protect
         (progn
          (and status (null frame) (<- self ':deexpose))
	  (multiple-value (tem new-left new-top new-right new-bottom)
	     (funcall self ':edges-ok-p new-left new-top new-right new-bottom))
	  (or tem (ferror nil "Specified new edges too small for ~S" self))
          (cond ((and (eq new-left (screen-x1 screen))
                      (eq new-top (screen-y1 screen))
                      (eq new-right (screen-x2 screen))
                      (eq new-bottom (screen-y2 screen)))
                 (setq new-margins (list 0 0 0 (<- self ':label-height))))
                (t
                 (setq new-margins (list 2 2 2 (+ 2 (<- self ':label-height))))))
	  (cond (panes
	 ;; Compute parameters for scaling the panes' edges with a linear transformation.
	 ;; Each pane is scaled, not by its actual edges, but by the place where the pane's
	 ;; own box should go.  The transformation is chosen so that a pane which used to
	 ;; fill the whole frame will still do so.
		 (setq old-xorg (+ left (first margins) -2)
		       old-yorg (+ top (second margins) -2)
		       new-xorg (+ new-left (first new-margins) -2)
		       new-yorg (+ new-top (second new-margins) -2))
		 (setq hscale (// (float (- new-right -1 (third new-margins) new-xorg))
				  (- right -1 (third margins) old-xorg))
		       vscale (// (float (- new-bottom -1 (fourth new-margins) new-yorg))
				  (- bottom -1 (fourth margins) old-yorg)))
		 (dolist (pane panes)
		   (funcall pane ':edges<-
			    (+ new-xorg 2 (fix (+ .5S0 (* hscale (- (funcall pane ':left)
								    2 old-xorg)))))
			    (+ new-yorg 2 (fix (+ .5S0 (* vscale (- (funcall pane ':top)
								    2 old-yorg)))))
			    (+ new-xorg -1 (fix (+ .5S0 (* hscale (- (funcall pane ':right)
								     -1 old-xorg)))))
			    (+ new-yorg -1 (fix (+ .5S0 (* vscale (- (funcall pane ':bottom)
								     -1 old-yorg)))))))))
          (setq label-position
                (list (+ new-left (car new-margins))
                      (- new-bottom (cadddr new-margins))
                      (- new-right (caddr new-margins))
                      (- new-bottom (cadr new-margins))))
          (setq left new-left top new-top right new-right bottom new-bottom)
          (setq margins new-margins))
	(cond ((and old-status (null frame))
	       ;; Bring us back on screen.
	       (funcall self ':expose)
	       ;; Bring back what was under where we used to be.
	       (window-restore-partial self screen o-left o-top o-right o-bottom)
	       (and (eq old-status ':selected)
		    (funcall self ':select)))))
	(or frame (windows-autoexpose)))))

;;; Selection and deselection must be passed along to the selected pane,
;;; When the pane gets selected or deselected, it will give us a
;;; :select-pane or :deselect-pane message.

(defmethod (window-frame-class :select) ()
    (<-as window-class ':select)
    (and selected-pane (<- selected-pane ':select)))

(defmethod (window-frame-class :deselect) ()
    (<-as window-class ':deselect)
    (and selected-pane (<- selected-pane ':deselect)))

;Set which pane is selected within the frame.
;Frame may be selected or not; we don't change that, and do right thing either way.
(defmethod (window-frame-class :selected-pane<-) (pane)
    (cond ((eq status ':selected)
	   (lock-screen-layout
	     (cond ((and selected-pane (neq selected-pane pane))
		    (<- selected-pane ':deselect)
		    (setq selected-pane nil)))
	     (cond ((neq selected-pane pane)
		    (setq selected-pane pane)
		    (<- pane ':select))))))
    (setq selected-pane pane))

;When a pane gets a :select message, it sends us this.
;We make sure we are selected with that pane selected inside us.
(defmethod (window-frame-class :select-pane) (pane-to-select)
    (or (eq selected-pane pane-to-select)
	(funcall self ':selected-pane<- pane-to-select))
    (or (eq status ':selected)
        (<-as window-class ':select)))

;Deselect a pane as our selected pane.
;Only happens when that pane receives a :deselect message.
;Since that should only happen when we send it,
;we do nothing here.
(defmethod (window-frame-class :deselect-pane) (pane-to-deselect)
  pane-to-deselect  ;argument ignored
  nil)

;;; Exposure and deexposure must also be passed along.
;;; The pane will pass them back.  We use checking the value of status
;;; to prevent an infinite recursion.

(defmethod (window-frame-class :expose) ()
    (cond ((null status)
           (<-as window-class ':expose)
	   (dolist (pane panes) (<- pane ':expose)))))

(defmethod (window-frame-class :clobber-screen) (&aux (value t))
    (funcall self ':clobber-label)
    (dolist (pane panes)
      (or (<- pane ':clobber-screen)
	  (setq value nil)))
    value)

(defmethod (window-frame-class :deexpose) (&optional ignore)
    (cond (status
	    (and (eq status ':selected)
		 (<- self ':deselect))
	    ;; We must deexpose ourself before the panes, to break the recursion of :deexpose.
	    (let ((inhibit-screen-saving-flag t))
	      (<-as window-class ':deexpose))
	    (dolist (pane panes) (<- pane ':deexpose))
	    ;; However, we can't save the screen until the panes are finished deexposing.
	    (funcall self ':save-screen))))

;;; Miscellaneous operations also passed along.

(defmethod (window-frame-class :reset) ()
    (and reset-panes (funcall self ':panes<- reset-panes))
    (and reset-selected-pane (funcall self ':selected-pane<- reset-selected-pane))
    (setq reset-selected-pane nil reset-panes nil)
    (dolist (pane panes) (<- pane ':reset)))

(defmethod (window-frame-class :initialize) ()
    (setq status nil)
    (funcall self ':run-reason)
    (dolist (pane panes) (<- pane ':initialize)))

(defmethod (window-frame-class :activate) ()
  (<-as window-class ':activate)
  (setq active-flag t))

(defmethod  (window-frame-class :deactivate) ()
  (<-as window-class ':deactivate)
  (setq active-flag nil))

(defmethod (window-frame-class :arrest-reason) (&optional (reason ':user))
    (dolist (pane panes) (<- pane ':arrest-reason reason)))

(defmethod (window-frame-class :revoke-arrest-reason) (&optional (reason ':user))
    (dolist (pane panes) (<- pane ':revoke-arrest-reason reason)))

(defmethod (window-frame-class :run-reason) ()
    (dolist (pane panes) (<- pane ':run-reason)))

(defmethod (window-frame-class :revoke-run-reason) ()
    (dolist (pane panes) (<- pane ':revoke-run-reason)))

(defmethod (window-frame-class :force-kbd-input) (input)
    (and selected-pane (<- selected-pane ':force-kbd-input input)))

(defmethod (window-frame-class :note-input) ()
  (dolist (pane panes)
    (<- pane ':note-input)))

;;; Labels and printing labels

;(defmethod (window-frame-class :label) () label)
(defmethod (window-frame-class :label<-) (new-label)
    (setq label new-label
	  label-needs-updating-flag t))

(defmethod (window-frame-class :clobber-label) ()
    (setq label-needs-updating-flag t))

(defmethod (window-frame-class :clean) ()
    (or (zerop (car margins)) (<- self ':draw-box))
    (dolist (pane panes)
      (<- pane ':clean)
      (<- pane ':draw-box 2
	  (+ left (first margins)) (+ top (second margins))
	  (- right (third margins)) (- bottom (fourth margins))))
    (<- self ':update-label))

(defmethod (window-frame-class :update) ()
    (dolist (pane panes) (<- pane ':update))
    (cond (label-needs-updating-flag
	   (<- self ':update-label))))

(defmethod (window-frame-class :update-label) ()
  (cond ((not (zerop (- (fourth label-position) (second label-position))))
    (let (pc-ppr-1)
	 (setq pc-ppr-1 (per-screen-pc-ppr screen label-position))
         (tv-clear-pc-ppr pc-ppr-1)
	 (*catch 'update-label
	    (<- self ':print-label pc-ppr-1))
	 (and halftone-labels-flag
              (tv-prepare-pc-ppr (pc-ppr-1)
		  (bitblt labels-halftone-alu
			  (- (pc-ppr-current-x pc-ppr-1) (pc-ppr-left-margin pc-ppr-1))
			  (- (pc-ppr-bottom-margin pc-ppr-1) (pc-ppr-top-margin pc-ppr-1))
			  labels-halftone 0 0
			  (screen-buffer-pixel-array screen)
			  (pc-ppr-left-margin pc-ppr-1)
			  (pc-ppr-top-margin pc-ppr-1))))
	 (and complement-labels-flag
	      (tv-prepare-pc-ppr (pc-ppr-1)
		  (tv-erase (- (pc-ppr-current-x pc-ppr-1) ;only complement part printed
			       (pc-ppr-left-margin pc-ppr-1))
			    (- (pc-ppr-bottom-margin pc-ppr-1)
			       (pc-ppr-top-margin pc-ppr-1))
			    (pc-ppr-left-margin pc-ppr-1)
			    (pc-ppr-top-margin pc-ppr-1)
			    tv-alu-xor))))))
  (setq label-needs-updating-flag nil))

(defmethod (window-frame-class :update-label-if-necessary) ()
    (cond (label-needs-updating-flag
	   (funcall self ':update-label)
	   (setq label-needs-updating-flag nil))))

(defmethod (window-frame-class :print-label) (pc-ppr-1)
    (tv-string-out pc-ppr-1 (or label name)))

(defmethod (window-frame-class :label-height) ()
    (font-char-height (screen-default-font screen)))

;(defclass window-single-frame-class window-frame-class ())

(defmethod (window-single-frame-class :pane) () selected-pane)

(defmethod (window-single-frame-class :print) (stream &rest ignore)
  (<-as window-single-frame-class ':print-self stream))

(defmethod (window-single-frame-class :print-self) (stream &rest ignore)
    (cond (selected-pane
	   (format stream "#<WINDOW-SINGLE-FRAME ~S>" selected-pane))
	  (t (<-as window-class ':print stream nil nil nil))))

(defmethod (window-single-frame-class :name) ()
    (cond (selected-pane (<- selected-pane ':name))
	  (t name)))

(defmethod (window-single-frame-class :command) (character)
    (cond (selected-pane (<- selected-pane ':command character))))

(defmethod (window-single-frame-class :print-label) (pc-ppr-1)
    (cond (label (tv-string-out pc-ppr-1 label))
	  (selected-pane (<- selected-pane ':print-label pc-ppr-1))
          (t (tv-string-out pc-ppr-1 name))))

(defmethod (window-single-frame-class :label) ()
    (cond ((and selected-pane
		(<- selected-pane ':handler-for ':label))
	   (<- selected-pane ':label))
	  (t label)))

(defmethod (window-single-frame-class :label-height) ()
    (cond (selected-pane (<- selected-pane ':label-height))
          (t (font-char-height (screen-default-font screen)))))

(defmethod (window-single-frame-class :supervisory-signal) (type)
  (cond (selected-pane (<- selected-pane ':supervisory-signal type))
	(t (<-as window-class ':supervisory-signal type))))

(defmethod (window-frame-class :move-near) (xpos ypos)
    (cond (selected-pane (<- selected-pane ':move-near xpos ypos))
          (t (<-as window-class ':move-near xpos ypos))))

(defmethod (window-frame-class :line-height) ()
    (or (and selected-pane (<- selected-pane ':line-height)) 1))

;Frames handle the mouse by deciding which pane it is in and letting that pane
;handle the mouse until the mouse leaves it.
(defmethod (window-frame-class :handle-mouse) ()
    (do () (())
	;; Exit if mouse moves out of the frame.
	(or (window-owns-mouse-p self)
	    (return nil))
	;; Set the blinker to an X between panes.
	;; Each pane can change it without worrying.  about restoring it.
	(tv-set-blinker-function mouse-blinker 'tv-character-blinker
              fonts:arrow #/X)
	(mouse-set-x-blinker-cursorpos)	      
	(do ((panes1 panes (cdr panes1)))
	    ((null panes1)
	     (mouse-default-handler self))
	  (and (window-owns-mouse-p (car panes1))
	       (return (<- (car panes1) ':handle-mouse))))))

;Set the edges of a bunch of panes to make them fit into a frame.
;A frame-layout is either a window or a list whose car is :VERTICAL
;or :HORIZONTAL, and the rest is alternating frame-layouts and flonums
;which are the fraction of that dimension to be given to that frame-layout.
;The last flonum isn't used and need not be supplied; the residue is used.
;In place of a flonum you may have a fixnum, which is used as is,
;or you may have NIL for one window which means use all that's left over,
;in which case you do have to specify the last fraction.
;You should normally set the edges of the frame before sending this message.
;This probably won't work if the frame or the panes is exposed.
(defmethod (window-frame-class :pane-layout<-) (frame-layout)
  (let ((frame-edges (<- self ':edges))
	(frame-margins (<- self ':margins)))
    (frame-layout-panes frame-layout 
			(+ (first frame-edges) (first frame-margins))
			(+ (second frame-edges) (second frame-margins))
			(- (third frame-edges) (third frame-margins))
			(- (fourth frame-edges) (fourth frame-margins))))
  ;; Must do this after setting the edges of the panes or it blows out
  (<- self ':panes<- (frame-layout-extract-panes frame-layout)))

(defun frame-layout-extract-panes (frame-layout)
  (cond ((atom frame-layout) (ncons frame-layout))
	(t (do ((l (cdr frame-layout) (cddr l))
		(r nil (nconc r (frame-layout-extract-panes (car l)))))
	       ((null l) r)))))

(defun frame-layout-panes (frame-layout left top right bottom &aux nilval)
  (cond ((atom frame-layout) (<- frame-layout ':edges<- left top right bottom))
	((memq (car frame-layout) '(:vertical :horizontal))
	 (and (memq nil frame-layout)		;Compute value to plug in in place of NIL
	      (do ((dimension (if (eq (car frame-layout) ':horizontal)
				  (- right left) (- bottom top)))
		   (dimension-used -3)
		   (dimension-this-guy)
		   (l (cdr frame-layout) (cddr l)))
		  ((null l) (setq nilval (- dimension dimension-used)))
		(setq dimension-this-guy (cond ((null (cadr l)) 0)	;filled in later
					       ((fixp (cadr l)) (+ (cadr l) 3))
					       (t (fix (* (cadr l) dimension)))))
		(setq dimension-used (+ dimension-used dimension-this-guy))))
	 (do ((dimension (if (eq (car frame-layout) ':horizontal)
			     (- right left) (- bottom top)))
	      (dimension-used -3)
	      (dimension-this-guy)
	      (l (cdr frame-layout) (cddr l)))
	     ((null l))
	   (setq dimension-this-guy (cond ((and (null (cddr l)) (null nilval))
					   (- dimension dimension-used))
					  ((null (cadr l)) nilval)
					  ((fixp (cadr l)) (+ (cadr l) 3))
					  (t (fix (* (cadr l) dimension)))))
	   (setq dimension-used (+ dimension-used dimension-this-guy))
	   (cond ((eq (car frame-layout) ':horizontal)
		  (frame-layout-panes (car l) left top
				      (- (setq left (+ left dimension-this-guy)) 3) bottom))
		 ((frame-layout-panes (car l) left top right
				      (- (setq top (+ top dimension-this-guy)) 3))))))
	(t (ferror nil "~S - garbage" frame-layout))))
