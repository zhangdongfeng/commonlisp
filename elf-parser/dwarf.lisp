(in-package :elf-parser)
(defparameter *endian* :little)

(defun bytes-to-int (bytes &optional signed-p &aux steps)
  (dotimes (n (length bytes)) (setf steps (cons (* n 8) steps)))
  (unless (listp bytes) (setf bytes (coerce bytes 'list)))
  (let ((value 0))
    (mapc (lambda (bit b) (setf (ldb (byte 8 bit) value) b))
          (if (eq *endian* :little) (reverse steps) steps) bytes)
    (if (and signed-p (> value (expt 2 (1- (* 8 (length bytes))))))
        (- (expt 2 (1- (* 8 (length bytes)))) value)
        value)))

(defun int-to-bytes (int size &optional signed-p &aux steps)
  (dotimes (n size) (setf steps (cons (* n 8) steps)))
  (let ((buf (make-array size
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0)))
    (when (and signed-p (< int 0))
      (setf int (+ (expt 2 (1- (* 8 size))) (- 0 int))))
    (mapc (lambda (bit) (vector-push (ldb (byte 8 bit) int) buf))
          (if (eq *endian* :little) (reverse steps) steps))
    buf))

(defun bytes-from (in bytes &optional signed-p)
  (unless (zerop bytes)
    (let ((buf (make-array bytes :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      (bytes-to-int (coerce buf 'list) signed-p))))

(defun bytes-to (out bytes value &optional signed-p)
  (write-sequence (int-to-bytes value bytes signed-p) out))

(define-binary-type unsigned-integer (bytes)
  (:reader (in) (bytes-from in bytes))
  (:writer (out value) (bytes-to out bytes value)))

(define-binary-type signed-integer (bytes)
  (:reader (in) (bytes-from in bytes 'signed))
  (:writer (out value) (bytes-to out bytes value 'signed)))

(defmacro define-dwarf-dictionary (name type dictionary)
  `(define-binary-type ,name ()
     (:reader (in)
              (let ((byte (read-value ',type in)))
                (or (cdr (assoc byte ',dictionary)) byte)))
     (:writer (out val)
              (error out val))))

(define-binary-type char   () (unsigned-integer :bytes 1))
(define-binary-type half   () (unsigned-integer :bytes 2))
(define-binary-type word   () (unsigned-integer :bytes 4))
(define-binary-type sword  () (signed-integer   :bytes 4))

(define-dwarf-dictionary dw-tag-name leb128
  ((1 . DW_TAG_ARRAY_TYPE)
   (2 . DW_TAG_CLASS_TYPE)
   (3 . DW_TAG_ENTRY_POINT)
   (4 . DW_TAG_ENUMERATION_TYPE)
   (5 . DW_TAG_FORMAL_PARAMETER)
   (8 . DW_TAG_IMPORTED_DECLARATION)
   (10 . DW_TAG_LABEL)
   (11 . DW_TAG_LEXICAL_BLOCK)
   (13 . DW_TAG_MEMBER)
   (15 . DW_TAG_POINTER_TYPE)
   (16 . DW_TAG_REFERENCE_TYPE)
   (17 . DW_TAG_COMPILE_UNIT)
   (18 . DW_TAG_STRING_TYPE)
   (19 . DW_TAG_STRUCTURE_TYPE)
   (21 . DW_TAG_SUBROUTINE_TYPE)
   (22 . DW_TAG_TYPEDEF)
   (23 . DW_TAG_UNION_TYPE)
   (24 . DW_TAG_UNSPECIFIED_PARAMETERS)
   (25 . DW_TAG_VARIANT)
   (26 . DW_TAG_COMMON_BLOCK)
   (27 . DW_TAG_COMMON_INCLUSION)
   (28 . DW_TAG_INHERITANCE)
   (29 . DW_TAG_INLINED_SUBROUTINE)
   (30 . DW_TAG_MODULE)
   (31 . DW_TAG_PTR_TO_MEMBER_TYPE)
   (32 . DW_TAG_SET_TYPE)
   (33 . DW_TAG_SUBRANGE_TYPE)
   (34 . DW_TAG_WITH_STMT)
   (35 . DW_TAG_ACCESS_DECLARATION)
   (36 . DW_TAG_BASE_TYPE)
   (37 . DW_TAG_CATCH_BLOCK)
   (38 . DW_TAG_CONST_TYPE)
   (39 . DW_TAG_CONSTANT)
   (40 . DW_TAG_ENUMERATOR)
   (41 . DW_TAG_FILE_TYPE)
   (42 . DW_TAG_FRIEND)
   (43 . DW_TAG_NAMELIST)
   (44 . DW_TAG_NAMELIST_ITEM)
   (45 . DW_TAG_PACKED_TYPE)
   (46 . DW_TAG_SUBPROGRAM)
   (47 . DW_TAG_TEMPLATE_TYPE_PARAMETER)
   (48 . DW_TAG_TEMPLATE_VALUE_PARAMETER)
   (49 . DW_TAG_THROWN_TYPE)
   (50 . DW_TAG_TRY_BLOCK)
   (51 . DW_TAG_VARIANT_PART)
   (52 . DW_TAG_VARIABLE)
   (53 . DW_TAG_VOLATILE_TYPE)
   (54 . DW_TAG_DWARF_PROCEDURE)
   (55 . DW_TAG_RESTRICT_TYPE)
   (56 . DW_TAG_INTERFACE_TYPE)
   (57 . DW_TAG_NAMESPACE)
   (58 . DW_TAG_IMPORTED_MODULE)
   (59 . DW_TAG_UNSPECIFIED_TYPE)
   (60 . DW_TAG_PARTIAL_UNIT)
   (61 . DW_TAG_IMPORTED_UNIT)
   (63 . DW_TAG_CONDITION)
   (64 . DW_TAG_SHARED_TYPE)
   (65 . DW_TAG_TYPE_UNIT)
   (66 . DW_TAG_RVALUE_REFERENCE_TYPE)
   (67 . DW_TAG_TEMPLATE_ALIAS)))

(define-dwarf-dictionary dw-attribute-name leb128
  ((1 . DW_AT_SIBLING)
   (2 . DW_AT_LOCATION)
   (3 . DW_AT_NAME)
   (9 . DW_AT_ORDERING)
   (11 . DW_AT_BYTE_SIZE)
   (12 . DW_AT_BIT_OFFSET)
   (13 . DW_AT_BIT_SIZE)
   (16 . DW_AT_STMT_LIST)
   (17 . DW_AT_LOW_PC)
   (18 . DW_AT_HIGH_PC)
   (19 . DW_AT_LANGUAGE)
   (21 . DW_AT_DISCR)
   (22 . DW_AT_DISCR_VALUE)
   (23 . DW_AT_VISIBILITY)
   (24 . DW_AT_IMPORT)
   (25 . DW_AT_STRING_LENGTH)
   (26 . DW_AT_COMMON_REFERENCE)
   (27 . DW_AT_COMP_DIR)
   (28 . DW_AT_CONST_VALUE)
   (29 . DW_AT_CONTAINING_TYPE)
   (30 . DW_AT_DEFAULT_VALUE)
   (32 . DW_AT_INLINE)
   (33 . DW_AT_IS_OPTIONAL)
   (34 . DW_AT_LOWER_BOUND)
   (37 . DW_AT_PRODUCER)
   (39 . DW_AT_PROTOTYPED)
   (42 . DW_AT_RETURN_ADDR)
   (44 . DW_AT_START_SCOPE)
   (46 . DW_AT_BIT_STRIDE)
   (47 . DW_AT_UPPER_BOUND)
   (49 . DW_AT_ABSTRACT_ORIGIN)
   (50 . DW_AT_ACCESSIBILITY)
   (51 . DW_AT_ADDRESS_CLASS)
   (52 . DW_AT_ARTIFICIAL)
   (53 . DW_AT_BASE_TYPES)
   (54 . DW_AT_CALLING_CONVENTION)
   (55 . DW_AT_COUNT)
   (56 . DW_AT_DATA_MEMBER_LOCATION)
   (57 . DW_AT_DECL_COLUMN)
   (58 . DW_AT_DECL_FILE)
   (59 . DW_AT_DECL_LINE)
   (60 . DW_AT_DECLARATION)
   (61 . DW_AT_DISCR_LIST)
   (62 . DW_AT_ENCODING)
   (63 . DW_AT_EXTERNAL)
   (64 . DW_AT_FRAME_BASE)
   (65 . DW_AT_FRIEND)
   (66 . DW_AT_IDENTIFIER_CASE)
   (67 . DW_AT_MACRO_INFO)
   (68 . DW_AT_NAMELIST_ITEM)
   (69 . DW_AT_PRIORITY)
   (70 . DW_AT_SEGMENT)
   (71 . DW_AT_SPECIFICATION)
   (72 . DW_AT_STATIC_LINK)
   (73 . DW_AT_TYPE)
   (74 . DW_AT_USE_LOCATION)
   (75 . DW_AT_VARIABLE_PARAMETER)
   (76 . DW_AT_VIRTUALITY)
   (77 . DW_AT_VTABLE_ELEM_LOCATION)
   (78 . DW_AT_ALLOCATED)
   (79 . DW_AT_ASSOCIATED)
   (80 . DW_AT_DATA_LOCATION)
   (81 . DW_AT_BYTE_STRIDE)
   (82 . DW_AT_ENTRY_PC)
   (83 . DW_AT_USE_UTF8)
   (84 . DW_AT_EXTENSION)
   (85 . DW_AT_RANGES)
   (86 . DW_AT_TRAMPOLINE)
   (87 . DW_AT_CALL_COLUMN)
   (88 . DW_AT_CALL_FILE)
   (89 . DW_AT_CALL_LINE)
   (90 . DW_AT_DESCRIPTION)
   (91 . DW_AT_BINARY_SCALE)
   (92 . DW_AT_DECIMAL_SCALE)
   (93 . DW_AT_SMALL)
   (94 . DW_AT_DECIMAL_SIGN)
   (95 . DW_AT_DIGIT_COUNT)
   (96 . DW_AT_PICTURE_STRING)
   (97 . DW_AT_MUTABLE)
   (98 . DW_AT_THREADS_SCALED)
   (99 . DW_AT_EXPLICIT)
   (100 . DW_AT_OBJECT_POINTER)
   (101 . DW_AT_ENDIANITY)
   (102 . DW_AT_ELEMENTAL)
   (103 . DW_AT_PURE)
   (104 . DW_AT_RECURSIVE)
   (105 . DW_AT_SIGNATURE)
   (106 . DW_AT_MAIN_SUBPROGRAM)
   (107 . DW_AT_DATA_BIT_OFFSET)
   (108 . DW_AT_CONST_EXPR)
   (109 . DW_AT_ENUM_CLASS)
   (110 . DW_AT_LINKAGE_NAME)))

(define-dwarf-dictionary dw-form-value leb128
  ((1 . DW_FORM_ADDR)
   (3 . DW_FORM_BLOCK2)
   (4 . DW_FORM_BLOCK4)
   (5 . DW_FORM_DATA2)
   (6 . DW_FORM_DATA4)
   (7 . DW_FORM_DATA8)
   (8 . DW_FORM_STRING)
   (9 . DW_FORM_BLOCK)
   (10 . DW_FORM_BLOCK1)
   (11 . DW_FORM_DATA1)
   (12 . DW_FORM_FLAG)
   (13 . DW_FORM_SDATA)
   (14 . DW_FORM_STRP)
   (15 . DW_FORM_UDATA)
   (16 . DW_FORM_REF_ADDR)
   (17 . DW_FORM_REF1)
   (18 . DW_FORM_REF2)
   (19 . DW_FORM_REF4)
   (20 . DW_FORM_REF8)
   (21 . DW_FORM_REF_UDATA)
   (22 . DW_FORM_INDIRECT)
   (23 . DW_FORM_SEC_OFFSET)
   (24 . DW_FORM_EXPRLOC)
   (25 . DW_FORM_FLAG_PRESENT)
   (32 . DW_FORM_REF_SIG8)))

(defun read-leb128 (in)
  (let* ((bytes (loop
                   for char = (read-byte in)
                   collect char into result
                   do (when (< char  128) (return result)))))
    (loop with value = 0
       with  bit = 0
       for b in bytes
       do (setf (ldb (byte 7 bit) value) b
                bit (+ bit 7))
       finally (return value))))

(define-binary-type leb128 ()
  (:reader (in)
           (read-leb128 in))
  (:writer (out value)
           (error out value)))

(define-binary-type leb128-raw-bytes ()
  (:reader (in)
           (read-value 'raw-bytes  in :length (read-leb128 in)))
  (:writer (out value)
           (error out value)))

(define-binary-type block-form (bytes)
  (:reader (in)
           (let ((len (read-value 'unsigned-integer in :bytes bytes)))
             (read-value 'raw-bytes in :length len )))
  (:writer (out value)
           (error out value)))

(define-binary-type indirect-form ()
  (:reader (in)
           (let ((form (read-value 'dw-form-value in )))
             (read-value form in)))
  (:writer (out value)
           (error out value)))

(define-binary-type DW_FORM_ADDR   () (unsigned-integer :bytes 4))
(define-binary-type DW_FORM_DATA2   () (unsigned-integer :bytes 2))
(define-binary-type DW_FORM_DATA4   () (unsigned-integer :bytes 4))
(define-binary-type DW_FORM_DATA8   () (unsigned-integer :bytes 8))
(define-binary-type DW_FORM_STRING    () (terminated-string))
(define-binary-type DW_FORM_BLOCK    ()  (leb128-raw-bytes))
(define-binary-type DW_FORM_BLOCK1    () (block-form :bytes 1))
(define-binary-type DW_FORM_BLOCK2   () (block-form :bytes 2))
(define-binary-type DW_FORM_BLOCK4   () (block-form :bytes 4))
(define-binary-type DW_FORM_DATA1    () (unsigned-integer :bytes 1))
(define-binary-type DW_FORM_FLAG    () (unsigned-integer :bytes 1))
(define-binary-type DW_FORM_SDATA    () (leb128))
(define-binary-type DW_FORM_UDATA    () (leb128))
(define-binary-type DW_FORM_REF_ADDR    () (unsigned-integer :bytes 4))
(define-binary-type DW_FORM_REF1    () (unsigned-integer :bytes 1))
(define-binary-type DW_FORM_REF2    () (unsigned-integer :bytes 2))
(define-binary-type DW_FORM_REF4    () (unsigned-integer :bytes 4))
(define-binary-type DW_FORM_REF8    () (unsigned-integer :bytes 8))
(define-binary-type DW_FORM_REF_UDATA    () (leb128))
(define-binary-type DW_FORM_INDIRECT    () (indirect-form))
(define-binary-type DW_FORM_SEC_OFFSET    () (unsigned-integer :bytes 4))
(define-binary-type DW_FORM_EXPRLOC    () (leb128-raw-bytes))
(define-binary-type DW_FORM_FLAG_PRESENT    () (unsigned-integer :bytes 0))
(define-binary-type DW_FORM_REF_SIG8    () (leb128-raw-bytes))


(defun string-at (offset)
  (coerce (loop
             for i = offset then (1+ i)
             for ch = (aref *debug-str* i)
             until (zerop ch)
             collect (code-char ch))
          'string))

(define-binary-type DW_FORM_STRP ()
  (:reader (in)
           (string-at (read-value 'word in)))
  (:writer (out val)
           (error out val )))


(define-binary-type string (length)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (code-char (read-byte in))))
             string))
  (:writer (out string)
           (dotimes (i length)
             (write-byte (char-code (char string i)) out)))  )

(define-binary-type terminated-string ()
  (:reader (in)
           (with-output-to-string (s)
             (loop for char = (code-char (read-byte in))
                until (char= char +null+) do (write-char char s))))
  (:writer (out string)
           (loop for char across string
              do (write-byte (char-code char) out)
              finally (write-byte (char-code +null+) out))))

;; raw bytes
(define-binary-type raw-bytes (length)
  (:reader (in)
           (let ((buf (make-array length :element-type '(unsigned-byte 8))))
             (read-sequence buf in)
             buf))
  (:writer (out buf)
           (write-sequence buf out)))

(define-binary-type dw-address-range ()
  (:reader (in)
           (let* ((len (read-value 'word in))
                  (ver (read-value 'half in))
                  (offset (read-value 'word in))                  )
             (list offset ver (read-value 'raw-bytes in :length (- len 6)))))
  (:writer (out val) (error out val)))

(define-binary-type dw-attributes ()
  (:reader (in)
           (loop
              for name  = (read-value 'dw-attribute-name in)
              for form  = (read-value 'dw-form-value in)
              until (and (numberp name) (numberp form) (zerop name) (zerop form))
              collect (list name form)))
  (:writer (out val) (error out val)))

(define-binary-class dw-abbrev ()
  ((id leb128)
   (tag dw-tag-name)
   (haschild char)
   (attributes dw-attributes)))

(define-binary-class dw-compile-unit ()
  ((len word)
   (version half)
   (abbrev word)
   (pointer-size char)))

(defun dw-get-address-range-table (elf  file)
  "get dwart address range table, return offset&segments list"
  (declare (optimize debug))
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let* ((sh (elf:sh (elf:named-section elf ".debug_aranges")))
           (ofs (elf:offset sh))
           (end (+ ofs (elf:size sh))))
      (file-position in ofs)
      (loop for addr-range = (read-value 'dw-address-range in )
         until (> (file-position in) end ) collect addr-range))))


(defun dw-get-abbrev-table (elf file &key (offset 0))
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (with-slots (elf:offset elf:size)
        (elf:sh (elf:named-section elf ".debug_abbrev"))
      (let* ((end (+ elf:offset elf:size))
             (start (+ elf:offset offset)))
        (file-position in start)
        (when *debug* (format t "~& abbrev offset ~d start ~d  end ~d~%" offset start end))
        (loop for abbrev  = (read-value 'dw-abbrev in)
           until (let ((pos (file-position in)))
                   (or (> pos end)
                       (and (zerop (id abbrev))
                            (> pos start))))
           collect abbrev)))))




(defun dw-get-compile-unit-debug-info (elf file offset sh)
  "get dwarf compile unit's  debug info in elf file,
  offset: is offset into .debug_info segment
  result: (tag attributs children)"
  (declare (optimize debug))
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (with-slots (elf:offset elf:size) sh
      (file-position in (+ offset elf:offset))
      (when *debug* (format t "~%compile-unit start: ~d offset ~d~%"
                            (file-position in) offset))
      (with-slots (len abbrev) (read-value 'dw-compile-unit in)
        (let ((unit-end (+ len elf:offset offset))
              (abb (dw-get-abbrev-table elf file :offset abbrev)))
          (labels
              ((read-debug-entry ()
                 (let* ((file-ofs (file-position in))
                        (info-ofs (- file-ofs elf:offset)))
                   (when *debug* (format t "~&~x ~x ~%" file-ofs info-ofs))
                   (if (> (file-position in) unit-end)
                       nil
                       (let ((index (read-value 'char in)))
                         (if (zerop index)
                             nil
                             (with-slots (tag haschild attributes)
                                 (nth (1- index ) abb)
                               (let ((result (loop for (name form) in attributes
                                                collect (list name (read-value form in)))))
                                 (if (eq haschild 1)
                                     (cons (list tag result
                                                 (read-debug-entry))
                                           (read-debug-entry))
                                     (cons (list tag result)
                                           (read-debug-entry)))))))))))
            (loop
               until (> (file-position in) unit-end)
               nconc (read-debug-entry))))))))


(defun dw-get-compile-unit-offset (elf file sh)
  "get dwarf compile unit's  debug info in elf file, offset is offset into .debug_info segment
elf:  elf object
file:   elf file
sh: debug-info section
==> list of compile unit's offset in file"
  (declare (optimize debug))
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (with-slots (elf:offset elf:size) sh
      (file-position in elf:offset)
      (let ((result nil))
        (loop
           for off = 0 then (+ 4  off (len unit))
           for unit = (read-value 'dw-compile-unit in)
           until (> (file-position in) (+ elf:offset elf:size))
           do (progn
                (push off result)
                (file-position in (+ 4 elf:offset off (len unit)))))
        (nreverse result)))))



(defparameter *debug-str* nil)
(defparameter *debug* nil)

(defun dw-get-debug-info (elf file sh)
  "get dwarf debug info in elf file"
  (declare (optimize debug))
  (let (( *debug-str* (if (elf:named-section elf ".debug_str")
                          (elf:data  (elf:named-section elf ".debug_str")))))
    (loop for offset in (dw-get-compile-unit-offset elf file sh)
       nconc (dw-get-compile-unit-debug-info elf file offset sh))))
