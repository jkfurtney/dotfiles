;;; npy-view.el --- Quickly view NumPy .npy files -*- lexical-binding: t; -*-

;; Commentary:
;;
;; Opens NumPy `.npy' files (as written by `numpy.save') as a
;; read-only formatted view: shape / dtype / byte-order header
;; followed by a readable rendering of the array data.  Intended as
;; a quick-look tool, not a full binary editor.
;;
;; Installation:
;;   (add-to-list 'load-path "/path/to/this/file")
;;   (require 'npy-view)
;;
;; Usage:
;;   Just find-file a .npy file (C-x C-f foo.npy) and it will open in
;;   npy-view-mode automatically.  Or call `M-x npy-view-file' and
;;   pick a file explicitly.
;;
;;   In the resulting buffer:
;;     g    - re-read and re-render the file from disk
;;     q    - bury the buffer
;;
;; Customization: see `npy-view-max-elements', `npy-view-table-max-rows',
;; and `npy-view-table-max-cols'.
;;
;; Supported dtypes: signed/unsigned ints (any width), float16/32/64,
;; complex64/128, bool, fixed-width unicode ('U') and byte ('S')
;; strings.  datetime64/timedelta64 are shown as raw integer counts.
;; Object arrays ('O') and structured/void dtypes are not decoded
;; (shown as raw byte counts) since they have no fixed-width, portable
;; binary layout to parse without a Python pickle reader.

;;; Code:

(defgroup npy-view nil
  "Quickly view NumPy .npy files."
  :group 'files
  :prefix "npy-view-")

(defcustom npy-view-max-elements 10000
  "Maximum number of elements to show for a 1-D array's flat list display.
Has no effect on 2-D/N-D display, which is instead bounded by
`npy-view-table-max-rows', `npy-view-table-max-cols', and
`npy-view-max-slices' — those decode only the cells actually shown,
by direct index, so they work correctly for Fortran-ordered arrays
too."
  :type 'integer
  :group 'npy-view)

(defcustom npy-view-table-max-rows 20
  "Maximum number of rows to display for any single 2-D matrix/slice."
  :type 'integer
  :group 'npy-view)

(defcustom npy-view-table-max-cols 12
  "Maximum number of columns to display for any single 2-D matrix/slice."
  :type 'integer
  :group 'npy-view)

(defcustom npy-view-max-slices 5
  "Maximum number of 2-D slices to display for arrays with 3+ dimensions."
  :type 'integer
  :group 'npy-view)

(defvar-local npy-view--rendered nil
  "Non-nil once the current buffer has been rendered from raw bytes.")

;;; Low-level binary reading helpers

(defun npy-view--read-uint (bytes off n little)
  "Read N bytes from unibyte string BYTES at offset OFF as an unsigned int.
LITTLE non-nil means little-endian."
  (let ((val 0))
    (if little
        (dotimes (i n)
          (setq val (logior val (ash (aref bytes (+ off i)) (* 8 i)))))
      (dotimes (i n)
        (setq val (logior (ash val 8) (aref bytes (+ off i))))))
    val))

(defun npy-view--to-signed (val nbytes)
  "Reinterpret unsigned VAL (NBYTES wide) as two's-complement signed."
  (let ((bits (* 8 nbytes)))
    (if (>= val (ash 1 (1- bits)))
        (- val (ash 1 bits))
      val)))

(defun npy-view--decode-ieee754 (bits total-bits exp-bits mantissa-bits bias)
  "Decode BITS (an unsigned integer) as an IEEE-754 float and return a Lisp float."
  (let* ((sign (if (= 1 (logand (ash bits (- 1 total-bits)) 1)) -1 1))
         (exponent (logand (ash bits (- mantissa-bits)) (1- (ash 1 exp-bits))))
         (mantissa (logand bits (1- (ash 1 mantissa-bits))))
         (max-exp (1- (ash 1 exp-bits))))
    (cond
     ((and (= exponent 0) (= mantissa 0)) (* sign 0.0))
     ((= exponent max-exp)
      (if (= mantissa 0) (* sign 1.0e+INF) 0.0e+NaN))
     ((= exponent 0)
      (* sign (expt 2.0 (float (- 1 bias mantissa-bits))) mantissa))
     (t
      (* sign (expt 2.0 (float (- exponent bias mantissa-bits)))
         (+ mantissa (ash 1 mantissa-bits)))))))

(defun npy-view--read-float (bytes off size little)
  "Read a SIZE-byte (2, 4 or 8) IEEE float at OFF from BYTES."
  (let ((bits (npy-view--read-uint bytes off size little)))
    (pcase size
      (2 (npy-view--decode-ieee754 bits 16 5 10 15))
      (4 (npy-view--decode-ieee754 bits 32 8 23 127))
      (8 (npy-view--decode-ieee754 bits 64 11 52 1023))
      (_ 0.0))))

(defun npy-view--read-unicode (bytes off charcount little)
  "Read CHARCOUNT UCS-4 code points starting at OFF from BYTES."
  (let (chars)
    (dotimes (i charcount)
      (let ((cp (npy-view--read-uint bytes (+ off (* i 4)) 4 little)))
        (unless (= cp 0) (push cp chars))))
    (apply #'string (nreverse chars))))

(defun npy-view--read-bytes-str (bytes off itemsize)
  "Read ITEMSIZE raw bytes at OFF from BYTES as a string, trimming NULs."
  (replace-regexp-in-string "\0+\\'" "" (substring bytes off (+ off itemsize))))

;;; Header parsing

(defun npy-view--parse-shape (shape-str)
  "Parse a Python tuple body SHAPE-STR (e.g. \"3, 4\") into a list of ints."
  (let ((parts (split-string shape-str "," t "[ \t]+")))
    (mapcar #'string-to-number parts)))

(defun npy-view--parse-header (bytes)
  "Parse raw .npy file contents BYTES, returning a plist header description."
  (unless (and (>= (length bytes) 10)
               (= (aref bytes 0) 147)
               (string= (substring bytes 1 6) "NUMPY"))
    (error "Not a valid .npy file (bad magic bytes)"))
  (let* ((major (aref bytes 6))
         (header-len-bytes (if (= major 1) 2 4))
         (header-start (+ 8 header-len-bytes))
         (header-len (npy-view--read-uint bytes 8 header-len-bytes t))
         (header-str (substring bytes header-start (+ header-start header-len)))
         (data-start (+ header-start header-len)))
    (unless (string-match "'descr':[ \t]*'\\([^']*\\)'" header-str)
      (error "Could not find 'descr' in .npy header"))
    (let ((descr (match-string 1 header-str)))
      (unless (string-match "'fortran_order':[ \t]*\\(True\\|False\\)" header-str)
        (error "Could not find 'fortran_order' in .npy header"))
      (let ((fortran (string= (match-string 1 header-str) "True")))
        (unless (string-match "'shape':[ \t]*(\\([^)]*\\))" header-str)
          (error "Could not find 'shape' in .npy header"))
        (list :descr descr
              :fortran-order fortran
              :shape (npy-view--parse-shape (match-string 1 header-str))
              :data-start data-start)))))

(defun npy-view--parse-descr (descr)
  "Split a dtype string DESCR (e.g. \"<f8\") into byte order/type/itemsize."
  (let* ((first (aref descr 0))
         (has-order (memq first '(?< ?> ?= ?\|)))
         (byteorder (if has-order first ?=))
         (rest (if has-order (substring descr 1) descr))
         (typechar (aref rest 0))
         (sizestr (substring rest 1))
         (itemsize (if (> (length sizestr) 0) (string-to-number sizestr) 1)))
    (list :byteorder byteorder :typechar typechar :itemsize itemsize)))

(defun npy-view--elem-byte-size (descr-plist)
  "Number of storage bytes per element for DESCR-PLIST.
DESCR-PLIST is as returned by `npy-view--parse-descr'."
  (let ((typechar (plist-get descr-plist :typechar))
        (itemsize (plist-get descr-plist :itemsize)))
    (if (eq typechar ?U) (* 4 itemsize) itemsize)))

(defun npy-view--make-reader (descr-plist)
  "Return a function (BYTES OFF) -> value that reads one element per DESCR-PLIST."
  (let* ((byteorder (plist-get descr-plist :byteorder))
         (typechar (plist-get descr-plist :typechar))
         (itemsize (plist-get descr-plist :itemsize))
         (little (not (eq byteorder ?>))))
    (pcase typechar
      (?i (lambda (bytes off)
            (npy-view--to-signed (npy-view--read-uint bytes off itemsize little) itemsize)))
      (?u (lambda (bytes off) (npy-view--read-uint bytes off itemsize little)))
      (?b (lambda (bytes off) (if (= 0 (aref bytes off)) "False" "True")))
      (?f (lambda (bytes off) (npy-view--read-float bytes off itemsize little)))
      (?c (lambda (bytes off)
            (let* ((half (/ itemsize 2))
                   (re (npy-view--read-float bytes off half little))
                   (im (npy-view--read-float bytes (+ off half) half little)))
              (format "%s%s%sj" (npy-view--fmt-num re) (if (>= im 0) "+" "") (npy-view--fmt-num im)))))
      ((or ?m ?M) (lambda (bytes off)
                    (npy-view--to-signed (npy-view--read-uint bytes off itemsize little) itemsize)))
      (?U (lambda (bytes off) (npy-view--read-unicode bytes off itemsize little)))
      (?S (lambda (bytes off) (npy-view--read-bytes-str bytes off itemsize)))
      (_ (lambda (_bytes _off) (format "<%d raw bytes>" itemsize))))))

;;; Value / strides helpers

(defun npy-view--fmt-num (x)
  "Format number X the way a quick numeric viewer should."
  (cond
   ((and (floatp x) (isnan x)) "nan")
   ((and (floatp x) (= x 1.0e+INF)) "inf")
   ((and (floatp x) (= x -1.0e+INF)) "-inf")
   ((floatp x) (if (= x (ftruncate x)) (format "%.1f" x) (format "%.6g" x)))
   ((integerp x) (number-to-string x))
   (t (format "%s" x))))

(defun npy-view--val-str (v)
  "String form of a decoded array element V for display."
  (if (numberp v) (npy-view--fmt-num v) (format "%s" v)))

(defun npy-view--strides (shape fortran)
  "Element strides (not bytes) for SHAPE, C-order unless FORTRAN is non-nil."
  (let* ((n (length shape))
         (strides (make-list n 1)))
    (if fortran
        (let ((acc 1))
          (dotimes (i n)
            (setcar (nthcdr i strides) acc)
            (setq acc (* acc (nth i shape)))))
      (let ((acc 1))
        (dotimes (i n)
          (let ((idx (- n 1 i)))
            (setcar (nthcdr idx strides) acc)
            (setq acc (* acc (nth idx shape)))))))
    strides))

(defun npy-view--flat-index (indices shape fortran)
  "Flat storage index for multi-dim INDICES given SHAPE/FORTRAN order."
  (let ((strides (npy-view--strides shape fortran)) (idx 0) (k 0))
    (dolist (i indices)
      (setq idx (+ idx (* i (nth k strides))))
      (setq k (1+ k)))
    idx))

(defun npy-view--pad-left (s width)
  "Right-align string S within WIDTH columns by left-padding with spaces."
  (let ((len (length s)))
    (if (>= len width) s (concat (make-string (- width len) ?\s) s))))

(defun npy-view--make-getter (bytes data-start elem-size reader shape fortran)
  "Return a function (INDICES) -> decoded value at multi-dim INDICES.
Decodes directly from BYTES at the byte offset implied by SHAPE/FORTRAN
strides, rather than relying on any flat storage-order prefix.  This
matters for Fortran-ordered arrays, where the elements needed for, say,
row 0 of every displayed column are scattered far apart in the file."
  (let ((strides (npy-view--strides shape fortran))
        (total-bytes (length bytes)))
    (lambda (indices)
      (let ((fi 0) (k 0))
        (dolist (i indices)
          (setq fi (+ fi (* i (nth k strides))))
          (setq k (1+ k)))
        (let ((off (+ data-start (* fi elem-size))))
          (if (<= (+ off elem-size) total-bytes)
              (funcall reader bytes off)
            "?"))))))

;;; Data rendering

(defun npy-view--insert-scalar (getter)
  (insert (format "Value: %s\n" (npy-view--val-str (funcall getter '())))))

(defun npy-view--insert-1d (getter n)
  (let ((start (point)))
    (insert "[")
    (dotimes (i n)
      (insert (npy-view--val-str (funcall getter (list i))))
      (when (< i (1- n)) (insert ", ")))
    (insert "]\n")
    (let ((fill-column 78)) (fill-region start (point)))))

(defun npy-view--insert-2d-generic (getter lead-idx rows cols)
  "Insert a table for the trailing ROWSxCOLS matrix at LEAD-IDX prefix indices."
  (let* ((show-rows (min rows npy-view-table-max-rows))
         (show-cols (min cols npy-view-table-max-cols))
         (colwidths (make-vector (max show-cols 1) 0))
         (cellstrs (make-vector (* (max show-rows 1) (max show-cols 1)) "")))
    (dotimes (r show-rows)
      (dotimes (c show-cols)
        (let ((s (npy-view--val-str (funcall getter (append lead-idx (list r c))))))
          (aset cellstrs (+ (* r show-cols) c) s)
          (when (> (length s) (aref colwidths c)) (aset colwidths c (length s))))))
    (dotimes (r show-rows)
      (dotimes (c show-cols)
        (insert (npy-view--pad-left (aref cellstrs (+ (* r show-cols) c)) (aref colwidths c)))
        (insert "  "))
      (when (< show-cols cols) (insert "..."))
      (insert "\n"))
    (when (< show-rows rows)
      (insert (format "  ... (%d more rows)\n" (- rows show-rows))))
    (when (< show-cols cols)
      (insert (format "  (showing first %d of %d columns)\n" show-cols cols)))))

(defun npy-view--insert-2d (getter shape)
  (npy-view--insert-2d-generic getter '() (nth 0 shape) (nth 1 shape)))

(defun npy-view--iterate-combos (dims prefix fn)
  "Depth-first call FN with each full index combo over DIMS, extending PREFIX."
  (if (null dims)
      (funcall fn (reverse prefix))
    (dotimes (i (car dims))
      (npy-view--iterate-combos (cdr dims) (cons i prefix) fn))))

(defun npy-view--insert-nd (getter shape)
  (let* ((ndim (length shape))
         (lead-shape (butlast shape 2))
         (rows (nth (- ndim 2) shape))
         (cols (nth (1- ndim) shape))
         (total-slices (apply #'* (if lead-shape lead-shape '(1))))
         (count 0))
    (insert (format "Array has %d dimensions (%s).\nShowing up to %d slices of the trailing %dx%d matrix.\n\n"
                     ndim (mapconcat #'number-to-string shape " x ")
                     npy-view-max-slices rows cols))
    (catch 'npy-view-done
      (npy-view--iterate-combos
       lead-shape '()
       (lambda (idx)
         (insert (format "slice [%s, :, :]:\n" (mapconcat #'number-to-string idx ", ")))
         (npy-view--insert-2d-generic getter idx rows cols)
         (insert "\n")
         (setq count (1+ count))
         (when (>= count npy-view-max-slices) (throw 'npy-view-done nil)))))
    (when (> total-slices npy-view-max-slices)
      (insert (format "... (%d more slices not shown)\n" (- total-slices npy-view-max-slices))))))

;;; Top-level render

(defun npy-view--build-display (bytes parsed)
  "Return the formatted display string for parsed .npy PARSED / raw BYTES."
  (let* ((descr (plist-get parsed :descr))
         (fortran (plist-get parsed :fortran-order))
         (shape (plist-get parsed :shape))
         (data-start (plist-get parsed :data-start))
         (descr-plist (npy-view--parse-descr descr))
         (elem-size (max 1 (npy-view--elem-byte-size descr-plist)))
         (reader (npy-view--make-reader descr-plist))
         (ndim (length shape))
         (total (if (= ndim 0) 1 (apply #'* shape)))
         (getter (npy-view--make-getter bytes data-start elem-size reader shape fortran))
         ;; The element cap only matters for the flat 1-D list rendering.
         ;; 2-D tables and N-D slices only ever decode the handful of cells
         ;; they actually display (bounded by npy-view-table-max-rows/cols
         ;; and npy-view-max-slices), computed by direct index rather than
         ;; a flat storage-order prefix, so they are unaffected by it and
         ;; unaffected by Fortran vs C ordering.
         (n1d (and (= ndim 1) (max 0 (min total npy-view-max-elements)))))
    (with-temp-buffer
      (insert "NumPy array viewer\n")
      (insert (make-string 60 ?-) "\n")
      (insert (format "shape        : %s\n" (if shape (mapconcat #'number-to-string shape " x ") "() [scalar]")))
      (insert (format "dtype        : %s\n" descr))
      (insert (format "order        : %s\n" (if fortran "Fortran (column-major)" "C (row-major)")))
      (insert (format "total elems  : %d\n" total))
      (insert (format "item size    : %d bytes\n" elem-size))
      (insert (format "data bytes   : %d\n" (* total elem-size)))
      (when (and n1d (< n1d total))
        (insert (format "NOTE: showing first %d of %d elements (see `npy-view-max-elements')\n" n1d total)))
      (insert (make-string 60 ?-) "\n\n")
      (cond
       ((= total 0) (insert "(empty array - no data)\n"))
       ((= ndim 0) (npy-view--insert-scalar getter))
       ((= ndim 1) (npy-view--insert-1d getter n1d))
       ((= ndim 2) (npy-view--insert-2d getter shape))
       (t (npy-view--insert-nd getter shape)))
      (buffer-string))))

(defun npy-view--render-current-buffer ()
  "Parse the current buffer's raw bytes as .npy and replace with a formatted view."
  (let ((raw (buffer-string)) (inhibit-read-only t))
    (erase-buffer)
    (condition-case err
        (insert (npy-view--build-display raw (npy-view--parse-header raw)))
      (error
       (insert (format "npy-view: failed to parse file as .npy\n\nError: %s\n"
                        (error-message-string err)))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun npy-view--refuse-save (&rest _)
  (user-error "npy-view buffers are read-only views; the underlying .npy file is untouched"))

(defun npy-view--revert (&optional _ignore-auto _noconfirm)
  "Reload the visited .npy file from disk and re-render it."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (set-buffer-multibyte nil)
    (insert-file-contents-literally buffer-file-name)
    (setq npy-view--rendered nil)
    (npy-view--render-current-buffer)
    (setq npy-view--rendered t)))

;;;###autoload
(define-derived-mode npy-view-mode special-mode "Npy-View"
  "Major mode for quickly viewing NumPy .npy array files.

Displays the array header (shape, dtype, byte order) followed by a
readable rendering of the array contents.

\\{special-mode-map}"
  (setq-local revert-buffer-function #'npy-view--revert)
  (setq-local write-contents-functions (list #'npy-view--refuse-save))
  (unless npy-view--rendered
    (npy-view--render-current-buffer)
    (setq npy-view--rendered t)))

;;;###autoload
(defun npy-view-file (file)
  "Open FILE (a NumPy .npy array) in a read-only npy-view buffer."
  (interactive "fNpy file to view: ")
  (let* ((file (expand-file-name file))
         (buf (generate-new-buffer (format "*npy-view: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (setq buffer-file-name file)
      (npy-view-mode))
    (switch-to-buffer buf)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.npy\\'" . npy-view-mode))
;;;###autoload
(modify-coding-system-alist 'file "\\.npy\\'" 'no-conversion)

(provide 'npy-view)
;;; npy-view.el ends here
