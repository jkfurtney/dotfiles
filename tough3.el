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

(defun tough3-highlight-region (boundaries color)
  (put-text-property (car boundaries) (cdr boundaries)
                     'font-lock-face `(:background ,color)))

(defun tough3-stripe-current-line (numbers)
  (let* ((current-char 0)
         (show 0))
    (dotimes (i (length numbers))
      (if (<= (+ (point) current-char (elt numbers i)) (point-at-eol))
          (progn (tough3-highlight-region (cons (+ (point-at-bol) current-char) (+ (point-at-bol) current-char (elt numbers i)))
                                          (if (evenp show) tough3-color-1 tough3-color-2))))
      (incf show)
      (incf current-char (elt numbers i)))))

(defun jkf/uf-region (a b)
  (interactive "r")
  (font-lock-unfontify-region a b))

(defun ufine () (interactive)
       (font-lock-unfontify-region (point-at-bol) (point-at-eol)))
(tough3-stripe-current-line '(3 2 5 5 3 2 10 10 10 10))


(defconst tough3-fmt-ELEM '(5 5 5 5 10 10 10 10 10 10 10 10 10))
(defconst tough3-fmt-CONNE '(5 5 5 5 5 5 10 10 10 10 10))
(defconst tough3-fmt-PARAM-1 '(2 2 4 4 4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 10 10 10))
