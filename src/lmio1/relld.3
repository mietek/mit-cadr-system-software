;-*-Mode:Lisp; Package:QFASL-REL-*-

;Load a relocatable file.

;This is the number of the temporary area we use for tables.
(defvar temp-area)

;This is the stream we are loading from.
(defvar load-stream)

;This is T if the stream allows us to read a whole buffer at a time.
(defvar stream-bypass-p)
(defvar stream-count)
(defvar stream-index)
(defvar stream-array)

;This is the number of sections in the file.
(defvar n-sections)

;This vector indexed by section gives the section length.
(defvar section-length)

;This vector indexed by section number gives the address of the start
;of the core area for the section.
(defvar section-relocation)

;This vector indexed by section number gives the code
;for which area the section should be in.
;The code is a number used to index area-vector.
(defvar section-area-code)

;This vector indexed by section number gives
;T if the section contains structures, NIL if lists.
(defvar section-structure-flag)

;This array holds the data of section 0 (the interned symbols).
(defvar section-0-array)

;This vector is in parallel with section 1.
;Each element starts a fixup list of threaded locatives
;that go through all the words that ought eventually to contain
;the value of the corresponding form in section 1 after it is evaluated.
(defvar fixup-list-vector)

;This vector indexed by "area code" gives the actual area number to use.
;The file contains only area codes which correspond to different
;types of storage, and this vector maps them into areas.
(defvar area-vector)
(defvar area-code-list `(temp-area macro-compiled-program nr-sym p-n-string
				working-storage-area permanent-storage-area
				fasl-constants-area fasl-constants-area))

;The high ten bits of a relocatable pointer are the section number.
(defvar section-number-bp 1612)

;The low 14 bits of a relocatable pointer are the index within the section.
(defvar address-within-section-bp 0016)

;Vector indexed by data type of Q, T if Q points at something.
(defvar data-type-pointer-p)

;Non-NIL => this is package specified to load file into.
(defvar pkg-specified)

;Increment this when the format of data storage changes
;such that old relocatable files will no longer be valid.
(defvar current-storage-format-version 1)

;This is the value which current-storage-format-version had
;when the file was dumped.
;If things aren't too far gone, we might be able to fix it up.
;The default is to refuse to load the file.
(defvar file-storage-format-version)

;When READ-BYTE is being used, after reading an odd byte,
;this contains the even byte to be read next.
(defvar next-byte)

;;; Format of a relocatable file.

;A relocatable file is a special sort of QFASL file;
;that is, it begins like a QFASL file, but contains an escape code
;which tells FASLOAD to switch over to a different format.

;The file is made up of halfwords.

;First come two halfwords which contain SIXBIT/QFASL/.

;Then comes a halfword containing the number FASL-OP-REL-FILE.
;This tells FASLOAD to call FASL-OP-REL-FILE which calls this loader.

;Then comes a halfword containing the op-code READ-STORAGE-FORMAT-VERSION
; (that is, the index in op-list of that symbol)
;followed by the version number.  This rejects files full of old format data
;which may now be invalid.

;Then comes a halfword containing the op-code READ-PACKAGE-NAME
; followed by the package name (see read-string for the format).

;Then comes a halfword containing the op-code READ-SYMBOLS
; followed by all the symbols referred to by the file.
;These are read in and interned at this time.
;See the definition of read-symbols for the format of this.

;Then comes a halfword containing the op-code READ-DATA
;followed by the section dictionary which says how many
; sections there are and how big each one is and where it should be loaded,
;followed by the data of all sections except 0,
;in order of increasing section number.

;Then comes a halfword containing zero.
;This causes this loader to exit.
;Then comes a halfword containing FASL-OP-END-OF-FILE,
;or more qfasl format data.

;What are sections?

;The data in the file is divided into sections.  Each section is loaded
;contiguously into a specific area and region type (list or structure).
;Then all pointers in the data are relocated.
;Pointers are represented in the data as a ten-bit section number
;and a fourteen-bit index within the section. 
;The section number is used to determine the relocation offset.
;Pointers are recognized from the data types, etc. in the data Qs.

;Section zero is special.  Its contents are the pointers to all the symbols
;interned by read-symbols, and pointers to their value cells and function cells.
;When a pointer to section 0 is found, it is
;replaced by a pointer to the symbol itself, or to the value cell or
;function cell of the symbol.  Data for section 0 is not present
;where it would be expected since the contents are already known.
;Section 0 is not relocated.  See read-symbols for more details.

;Section one is also special.
;Its contents are loaded and relocated normally
;After relocation is finished, it is treated as a sequence
;of forms to be evaluated, or commands to store an object in
;a value cell or function cell.
;See the definition of eval-section-1 for details.
;When a pointer to section 1 is found during relocation,
;the Q containing it is strung onto a fixup list.
;After a form in section 1 is evaluated,
;the value is stored in all the Qs on that form's fixup list.

;Load a relocatable file from the stream LOAD-STREAM.
;PKG-SPECIFIED is the package argument to FASLOAD, or NIL.
;The other three args are the data for bypassing the stream
;and reading directly out of the chaosnet buffer.
;They are passed along because FASLOAD already started using them.
;We return the same three quantities, as updated, so FASLOAD can continue.
;See READ-HALFWORD for more information.
(defun rel-load-stream (load-stream
			  stream-array stream-index stream-count pkg-specified)
  (prog (section-length section-relocation
	 section-area-code section-structure-flag
	 section-0-array n-sections
	 (stream-bypass-p stream-array)
	 (package (cond (pkg-specified (pkg-find-package pkg-specified))
			(t package))))
	(or (boundp 'temp-area)
	    (setq temp-area (make-area 'name 'load-temp-area)))
	(reset-temporary-area temp-area)
	(setq area-vector (make-array temp-area art-q (length area-code-list)))
	(do ((i 0 (1+ i))
	     (l area-code-list (cdr l)))
	    ((null l))
	  (aset (symeval (car l)) area-vector i))
	(init-data-type-tables)
	(top-level)
	(return stream-array stream-index stream-count)))

(defun init-data-type-tables ()
  (setq data-type-pointer-p (make-array working-storage-area art-q
					(lsh 1 (logand %%q-data-type 77))))
  (aset t data-type-pointer-p dtp-symbol)
  (aset t data-type-pointer-p dtp-symbol-header)
  (aset t data-type-pointer-p dtp-extended-number)
  (aset t data-type-pointer-p dtp-gc-forward)
  (aset t data-type-pointer-p dtp-external-value-cell-pointer)
  (aset t data-type-pointer-p dtp-one-q-forward)
  (aset t data-type-pointer-p dtp-header-forward)
  (aset t data-type-pointer-p dtp-body-forward)
  (aset t data-type-pointer-p dtp-locative)
  (aset t data-type-pointer-p dtp-list)
  (aset t data-type-pointer-p dtp-fef-pointer)
  (aset t data-type-pointer-p dtp-array-pointer)
  (aset t data-type-pointer-p dtp-stack-group)
  (aset t data-type-pointer-p dtp-closure)
  (aset t data-type-pointer-p dtp-select-method)
  (aset t data-type-pointer-p dtp-instance)
  (aset t data-type-pointer-p dtp-instance-header)
  (aset t data-type-pointer-p dtp-entity)

  )

;OP-VECTOR is used for converting opcodes in the file to
;functions to do the work for them.
(defvar op-list '(nil read-storage-format-version read-package-name read-symbols read-data))
(defvar op-vector
	(let ((v (make-array nil art-q (length op-list))))
	  (fillarray v op-list)
	  v))

;With the file already open, process it and do the work.
(defun top-level ()
  (do () (())
    (let ((op (read-halfword)))
      (and (zerop op) (return))
      (funcall (aref op-vector op)))))

(defun read-storage-format-version ()
  (or (= current-storage-format-version
	 (setq file-storage-format-version (read-halfword)))
      (ferror nil "Obsolete relocatable QFASL file")))

(defun read-package-name ()
  (let ((tem (read-string nil working-storage-area)))
    (or pkg-specified (setq package (pkg-find-package tem)))))

;Read a string from the file.
;The format in the file is a halfword holding one plus the length in bytes
;followed that many bytes padded out to a whole halfword.
(defun read-string (&optional n-bytes-plus-one area)
  (or n-bytes-plus-one (setq n-bytes-plus-one (read-halfword)))
  (setq next-byte nil)
  (let ((str (make-array (or area temp-area) art-string (1- n-bytes-plus-one))))
    (dotimes (i (1- n-bytes-plus-one)) (aset (read-byte) str i))
    str))

;Read in and intern all the symbols used by the file.
;First comes the number of symbols, then the data
;for the symbols, one by one.
;Each symbol is represented by its name (a string for read-string)
;followed by a sequence of package refnames (also strings)
;needed to reach the package to intern the symbol in,
;followed by a halfword of zero.
;For a symbol in the current package, there are no package names.

;Each symbol produces four Qs in section 0.
;The first contains the symbol,
;the second contains a locative pointing to the value cell,
;the third contains a locative pointing to the function cell.
;The fourth is not used.
(defun read-symbols ()
  (do ((pkg package (pkg-super-package pkg)))
      ((null pkg))
    (si:page-in-structure pkg))
  (let ((n-symbols (read-halfword)))
    (setq section-0-array (make-array temp-area art-q (* 4 n-symbols)))
    (dotimes (i n-symbols)
      (let ((string (read-string))
	    sym)
	(setq sym (intern string (read-symbol-pkg)))
	(and (eq string (get-pname sym))
	     (%p-store-pointer (copy-string string p-n-string) sym))
	(aset sym section-0-array (* 4 i))
	(aset (locf (symeval sym)) section-0-array (+ 1 (* 4 i)))
	(aset (locf (fsymeval sym)) section-0-array (+ 2 (* 4 i)))))))

(defun copy-string (string area)
  (let ((newstring (make-array area art-string (string-length string))))
    (copy-array-contents string newstring)
    newstring))

;Read in something which specifies what package a symbol is in.
;It is either a string in read-string format,
;or a single halfword of zero meaning use the default package.
(defun read-symbol-pkg ()
  (do ((n-bytes)
       (str)
       (pkg package)) (())
    (setq n-bytes (read-halfword))
    (cond ((zerop n-bytes) (return pkg))
	  (t (setq str (read-string n-bytes working-storage-area))
	     (setq pkg (pkg-find-package str nil pkg))))))

(defun read-data ()
  (read-section-sizes)
  (allocate-sections)
  (let ((inhibit-scavenging-flag t))
    ;; Make the "start" of section 1 be the first element
    ;; of the array we allocated for it.
    ;; Unlike other sections, section 1 must remain consecutive
    ;; after we turn GC back on.
    ;; So, don't overwrite the array header.
    (aset (locf (aref (aref section-relocation 1) 0))
	  section-relocation 1)
    ;; Make all elements of section-relocation be fixnums for %24-bit-plus.
    (do ((i 1 (1+ i))) ((= i n-sections))
      (setf (aref section-relocation i)
	    (%pointer (aref section-relocation i))))
    (read-contents)
    (relocate))
  (eval-section-1))

;Read in the section dictionary which says what sections
;there are, how big they are, and where they go.

;First comes the number of sections.
;Then for each section, two halfwords.
; The first is the length.
; The second has the area code number in the low 12 bits,
; and the 10000 bit is on if the section contains structures (not lists).
;Section 0 appears in this list even though the information is redundant.
(defun read-section-sizes ()
  (setq n-sections (read-halfword))
  (or (< n-sections (lsh 1 (logand section-number-bp 77)))
      (ferror nil "Invalid relocatable QFASL file: too many sections"))
  (setq section-length (make-array temp-area art-q n-sections))
  (setq section-relocation (make-array temp-area art-q n-sections))
  (setq section-area-code (make-array temp-area art-q n-sections))
  (setq section-structure-flag (make-array temp-area art-q n-sections))
  (dotimes (i n-sections)
    (aset (read-halfword) section-length i)
    (or (< (aref section-length i)
	   (lsh 1 (logand address-within-section-bp 77)))
	(ferror nil "Invalid relocatable QFASL file: section ~D too long" i))
    (let ((tem (read-halfword)))
      (and (bit-test tem 10000)
	   (aset t section-structure-flag i))
      (or (< (ldb 0014 tem) (array-length area-vector))
	  (ferror nil "Invalid relocatable QFASL file: bad area code for section ~D" i))
      (aset (ldb 0014 tem) section-area-code i)))
  (or (= (aref section-length 0) (array-length section-0-array))
      (ferror nil "Invalid relocatable QFASL file: inconsistent section zero size")))

;Allocate space for all sections
;and remember the addresses in section-relocation.
;Note that the space for section 0 is in section-0-array.
;We allocate an array for section 1's data even though it contains lists.
(defun allocate-sections ()
  (aset (locf (aref section-0-array 0)) section-relocation 0)
  (aset (alloc-space-in-area  (aref section-length 1)
			      (aref area-vector (aref section-area-code 1))
			      t)
	section-relocation 1)
  (do ((i 2 (1+ i))) ((= i n-sections))
    (aset (alloc-space-in-area (aref section-length i)
			       (aref area-vector (aref section-area-code i))
			       (aref section-structure-flag i))
	  section-relocation i))
  (setq fixup-list-vector (make-array temp-area art-q
				      (aref section-length 1))))

(defun alloc-space-in-area (length area structure-flag)
  (cond ((null structure-flag) (make-list area length))
	(t (make-array area art-q length))))

;Read the data of all the sections into the allocated homes.
(defun read-contents ()
  (do ((i 1 (1+ i))) ((= i n-sections))
    (read-many-words (aref section-relocation i)
		     (aref section-length i))))

;Relocate all pointers in all sections other than 0.
;Done with GC off.
(defun relocate ()
  (do ((i 1 (1+ i))) ((= i n-sections))
    (cond ((aref section-structure-flag i)
	   (relocate-structures (aref section-relocation i)
				(%24-bit-plus (aref section-relocation i)
					      (aref section-length i))))
	  (t (relocate-lists (aref section-relocation i)
			     (%24-bit-plus (aref section-relocation i)
					   (aref section-length i)))))))

(defun relocate-structures (start end)
  (do ((addr start))
      ((= addr end))
    (relocate-lists addr (%24-bit-plus addr (%structure-boxed-size addr)))
    (setq addr (%24-bit-plus addr (%structure-total-size addr)))))

;Relocate the pointers from start to end.
;Return the address of the last Q handled.
(defun relocate-lists (start end)
  (do ((addr start (%24-bit-plus 1 addr)))
      ((= addr end)
       addr)
    (and (aref data-type-pointer-p (%p-data-type addr))
	 (let ((secnum (%p-ldb section-number-bp addr)))
	   (cond ((> secnum 1)
		  (%p-store-pointer addr
				    (%24-bit-plus (%p-ldb address-within-section-bp addr)
						  (aref section-relocation secnum))))
		 ((zerop secnum)
		  (%p-store-pointer addr
				    (aref section-0-array
					  (%p-ldb address-within-section-bp addr))))
		 (t
		  (let ((idx (%p-ldb address-within-section-bp addr)))
		    (%p-store-contents addr (aref fixup-list-vector idx))
		    (aset (%make-pointer dtp-locative addr) fixup-list-vector idx))))))))

;Process section 1.  This is done after relocation is complete.
;The meaning of a Q in section 1 depends on the cdr code.
;If the cdr code is 0, the contents are a form to be evaluated.
;The value should be stored in all the Qs threaded in the
;fixup list pointed to by the appropriate element of fixup-list-vector.
;If the cdr code is not 0, then the contents are a symbol,
;and the contents of the next Q are a value to be stored
;in that symbol using the cdr code as offset (1 => value cell, 2 => function cell).
;For code 2, a third Q follows, containing the symbol to record
;as the file-symbol of the function definition.
;This function works with GC turned on.
(defun eval-section-1 ()
  (let ((len (aref section-length 1))
	(org (aref section-relocation 1)))
    (si:page-in-words org len)
    (dotimes (i len)
      (let ((type (%p-ldb-offset %%q-cdr-code org i))
	    (form (%p-contents-offset org i)))
	(cond ((zerop type)
	       (let ((val (eval form)))
		 ;; Trace the fixup list for this form and store the value thru it.
		 (do ((ptr (aref fixup-list-vector i) next)
		      (next))
		     ((null ptr))
		   (setq next (cdr ptr))
		   (rplacd ptr val))))
	      ((= type 1)
	       (set form (%p-contents-offset org (setq i (1+ i)))))
	      ((= type 2)
	       (prog (tem si:fdefine-file-symbol)
		     (setq tem (%p-contents-offset org (setq i (1+ i)))
			   si:fdefine-file-symbol (%p-contents-offset org (setq i (1+ i))))
		     (fset-carefully form tem))))))))

;Read the next halfword from the file.
(defun read-halfword ()
    (cond (stream-bypass-p
	   (cond ((<= stream-count 0)
		  (cond (stream-array
			 (funcall load-stream ':advance-input-buffer)))
		  (multiple-value (stream-array stream-index stream-count)
		    (funcall load-stream ':get-input-buffer))))
	   (prog1 (aref stream-array stream-index)
		  (setq stream-index (1+ stream-index))
		  (setq stream-count (1- stream-count))))
	  (t (funcall load-stream ':tyi))))

;Read from the stream a byte at a time.
;You must set next-byte to nil before using this.
;If an odd number of bytes are read,
;the next use of read-halfword will automatically
;skip the extra byte.
(defun read-byte (&aux tem)
  (cond (next-byte (prog1 next-byte (setq next-byte nil)))
	(t (setq tem (read-halfword))
	   (setq next-byte (ldb 1010 tem))
	   (ldb 0010 tem))))

;Read data from the file into a block of Qs
;Specify the address of the first and the number of Qs.
(defun read-many-words (start count)
  (si:page-in-words start count)
  (cond (stream-bypass-p
	  (setq count (* 2 count))
	  (do ((start-index 0)
	       (array (make-array temp-area art-16b count start)))
	      ((zerop count))
	    (let ((number-read (read-many-halfwords-bypassed array start-index count)))
	      (setq start-index (+ start-index number-read)
		    count (- count number-read)))))
	(t
	  (do ((addr start (%24-bit-plus addr 1))
	       (end (%24-bit-plus start count)))
	      ((= addr end))
	    (let (lh hh)
	      (setq lh (read-halfword) hh (read-halfword))
	      (%p-dpb lh %%q-low-half addr)
	      (%p-dpb hh %%q-high-half addr))))))

(defun read-many-halfwords-bypassed (array start-index count)
  (cond ((<= stream-count 0)
	 (cond (stream-array
		 (funcall load-stream ':advance-input-buffer)))
	 (multiple-value (stream-array stream-index stream-count)
	   (funcall load-stream ':get-input-buffer))))
  (setq count (min count stream-count))
  (copy-array-portion stream-array stream-index (+ stream-index count)
		      array start-index (+ start-index count))
  (setq stream-index (+ stream-index count)
	stream-count (- stream-count count))
  count)
