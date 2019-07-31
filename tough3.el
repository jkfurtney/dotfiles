(require 'generic-x)

(defconst tough3-mode-keywords '(ROCKS START PARAM RPCAP ELEME CONNE INCON GENER FOFT GOFT OUTPU ENDCY FRACT CONBD SOLVR MULTI MESHMAKER1 ENDFI DIFFU COFT HYSTE TIMES INDOM TIMBC))



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

(defvar tough3-columns-colors '("#DCDCDC" "#808080" "#404040"))
(defvar tough3-color  "#808080")

(defun tough3-highlight-region (boundaries)
  (put-text-property (car boundaries) (cdr boundaries)
                     'font-lock-face `(:background ,tough3-color)))

(defun tough3-stripe-current-line (numbers)
  (let* ((current-char 0)
         (show 0))
    (dotimes (i (length numbers))
      (if (and (< (+ (point) current-char) (point-at-eol)) (evenp show))
          (tough3-highlight-region (cons (+ (point-at-bol) current-char) (+ (point-at-bol) current-char (elt numbers i)))))
      (incf show)
      (incf current-char (elt numbers i)))))

(defun jkf/uf-region (a b)
  (interactive "r")
  (font-lock-unfontify-region a b))

(defun ufine () (interactive)
       (font-lock-unfontify-region (point-at-bol) (point-at-eol)))
;(tough3-stripe-current-line '(3 2 5 5 3 2 10 10 10 10))
