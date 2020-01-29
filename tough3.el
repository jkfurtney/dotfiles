(require 'generic-x)

(defconst tough3-mode-keywords '(ROCKS START PARAM RPCAP ELEME CONNE INCON GENER FOFT GOFT OUTPU ENDCY SOLVR MULTI MESHMAKER1 ENDFI DIFFU COFT HYSTE TIMES INDOM TIMBC))

(define-generic-mode 'tough3-mode
  '(";")
  (mapcar 'symbol-name tough3-mode-keywords)
  (list
   (cons "@[a-zA-Z0-9_]+" 'font-lock-builtin-face)
   )
  '("\\.dat$" "\\.fis$" "\\.fin$")
  (list (lambda ()
          (set (make-local-variable 'mode-name) "Tough3")))
  "Mode for Tough3 data files")

(defconst tough3-color-1  "#808080")
(defconst tough3-color-2  "#669999")

(defun tough3--highlight-region (boundaries color)
  (put-text-property (car boundaries) (cdr boundaries)
                     'font-lock-face `(:background ,color)))

(defun tough3--expand-fmt (fmt)
  (let ((ret))
    (dotimes (i (length fmt))
      (if (consp (elt fmt i))
          (dotimes (j (first (elt fmt i)))
            (push (second (elt fmt i)) ret))
        (push (elt fmt i) ret)))
    (reverse ret)))
;(tough3--expand-fmt tough3-fmt-ROCKS)

(defun tough3-stripe-current-line (fmt)
  (let* ((numbers (tough3--expand-fmt fmt))
         (current-char 0)
         (show 0))
    (dotimes (i (length numbers))
      (when (< 0 (elt numbers i))
        (when (<= (+ (point) current-char (elt numbers i)) (point-at-eol))
          (tough3--highlight-region
           (cons
            (+ (point-at-bol) current-char)
            (+ (point-at-bol) current-char (elt numbers i)))
           (if (evenp show) tough3-color-1 tough3-color-2)))
        (incf show))
      (incf current-char (abs (elt numbers i))))))

;(tough3-stripe-current-line '(3 2 5 5 3 2 10 10 10 10))
;(tough3-stripe-current-line tough3-fmt-ROCKS)


(defconst tough3-fmt-ELEM '((4 5) (9 10)))
(defconst tough3-fmt-CONNE '((6 5)  (5 10)))
(defconst tough3-fmt-PARAM-1 '((2 2) (3 4) (24 1) -10 (2 10)))
(defconst tough3-fmt-ROCKS '((2 5) (7 10)))
(defconst tough3-fmt-PARAM-2 '((4 10) 5 -5 (3 10)))
(defconst tough3-fmt-PARAM-2.1 '((8 10)))
(defconst tough3-fmt-PARAM-3 '((8 10)))
(defconst tough3-fmt-PARAM-4 '((4 20)))
(defconst tough3-fmt-SOLVR-1 '(1 -2 2 -3 2 (2 10)))
(defconst tough3-fmt-GENER-1 '((2 5) (4 5) 5 4 1 (3 10)))
(defconst tough3-fmt-FOFT-1 '((3 5)))
(defconst tough3-fmt-MULTI-1 '((5 5)))
