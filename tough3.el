(require 'generic-x)

(defconst tough3-mode-keywords '(ROCKS TRANS START PARAM RPCAP ELEME CONNE INCON GENER FOFT GOFT OUTPU ENDCY FRACT CONBD SOLVR MULTI MESHMAKER1 ENDFI MULTI))


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
