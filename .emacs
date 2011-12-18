(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "black")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "red")

(add-to-list 'exec-path "C:/Program Files/Aspell/bin/")
(setq ispell-program-name "aspell")

(defun dont-kill-emacs ()
 (interactive)
 (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(global-set-key "\C-x\C-c" 'dont-kill-emacs)

(defun remove-hard-wrap () 
  "Make several lines into a single long line." 
  (interactive) 
  (let ((fill-column 90002000)) 
    (fill-paragraph nil)))
(global-set-key "\C-x\M-q" 'remove-hard-wrap)

(global-set-key "\C-c;" 'comment-region)

;(add-to-list 'load-path "/home/Jason/lib/emacs")
;(setq load-path (append (mapcar 'expand-file-name '("~/lib/emacs"))  
;load-path
;))

;;(setq load-path (cons (expand-file-name "~/elisp") load-path))
;;(load-library "word-count")

;; (autoload 'word-count-mode "word-count"
;;          "Minor mode to count words." t nil)
;; (global-set-key "\M-+" 'word-count-mode)


(setq-default transient-mark-mode t)
(setq-default global-font-lock-mode t)




(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

(tool-bar-mode nil)
(menu-bar-mode nil)

(require 'mwheel) ; Emacs


(setq-default ispell-program-name "aspell")

(fset 'date
    [?\C-u escape ?! ?d ?a ?t ?e return])


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "black")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "red")

(setq visible-bell t)
 
(require 'generic-x) ;; we need this
(define-generic-mode 
  'itasca-mode                         ;; name of the mode to create
  '(";")                           ;; comments start with '!!'
  '("def" "loop" "endloop" "end_loop" "then" "else" "elseif" "else_if" 
    "while" "while_stepping" "config" "array" "end" "if" "endif" "end_if"
    "loop" "endloop" "end_loop" "command" "endcommand" "end_command")
  '(("=" . 'font-lock-operator)
    ("\\<\\(a\\(?:bs\\|cos\\|n\\(?:d\\|gle\\|isotropic\\)\\|pp\\(?:_pnt\\|gw_pnt\\|ly\\|th_pnt\\)\\|r\\(?:ea\\|ray\\)\\|s\\(?:in\\|pect\\|x[xy]\\|yy\\|zz\\)\\|t\\(?:an2?\\|t\\(?:_pnt\\|ach\\)\\)\\|[34]\\)\\|b\\(?:a\\(?:ck\\|ud\\)\\|icoe\\|s\\(?:x[xy]\\|yy\\|zz\\)\\)\\|c\\(?:a\\(?:ll\\|se\\(?:_?of\\)?\\)\\|f_\\(?:axi\\|creep\\|dyn\\|ext\\|gw\\|ps\\|therm\\)\\|ga\\|har\\|lo\\(?:ck\\|se\\)\\|m_max\\|o\\(?:lumns\\|mmand\\|n\\(?:fig\\|stitutive\\(?:_?model\\)\\)\\|s\\)\\|parse\\|r\\(?:dt\\|eep\\|t\\(?:del\\|ime\\)\\)\\|s\\(?:c\\|x[xy]\\|yy\\|zz\\)\\|ycle\\)\\|d\\(?:a\\(?:mp\\(?:ing\\)?\\|tum\\)\\|e\\(?:fine\\|grad\\|nsity\\)\\|o_update\\|s\\(?:x[xy]\\|yy\\|zz\\)\\|ump\\|y\\(?:_state\\|dt\\(?:_gp[ij]\\)?\\|namic\\|t\\(?:del\\|ime\\)\\)\\|[ty]\\)\\|e\\(?:_p\\|cho\\|ga\\|l\\(?:astic\\|se\\)\\|nd\\(?:_\\(?:c\\(?:ase\\|ommand\\)\\|if\\|loop\\|section\\)\\|c\\(?:ase\\|ommand\\)\\|if\\|loop\\|section\\)?\\|rror\\|v_\\(?:p\\|tot\\)\\|x\\(?:it\\|p\\)\\)\\|f\\(?:2mod\\|_prop\\|c_arg\\|i\\(?:lcolor\\|sh\\(?:_msg\\|call\\)?\\|x\\)\\|l\\(?:ags\\|o\\(?:at\\|w\\)\\|prop\\)\\|m\\(?:em\\|od\\)\\|o\\(?:b[lu]\\|rce\\|s\\(?:_f\\)?\\)\\|r\\(?:ee\\|iend\\)\\|s\\(?:tring\\|[ir]\\)\\|tens\\)\\|g\\(?:2flow\\|e\\(?:n\\|t_mem\\)\\|flow\\|msmul\\|p\\(?:_copy\\|p\\)\\|r\\(?:\\(?:an\\|i\\)d\\)\\|w\\(?:dt\\|t\\(?:del\\|ime\\)\\)\\)\\|h\\(?:b[ms]\\|elp\\|is\\(?:file\\)?\\)\\|i\\(?:e\\(?:b\\(?:_pnt\\)?\\|rr\\)\\|face\\|gp\\|m\\(?:em\\|plicit\\)\\|n\\(?:formation\\|i\\(?:\\(?:mode\\|tia\\)l\\)\\|t\\(?:_pnt\\|erface\\)?\\)\\|tasca\\|zones\\|[fn]\\)\\|j\\(?:err\\|gp\\|zones\\)\\|l\\(?:arge\\|egend\\|ff_pnt\\|i\\(?:mits\\|st\\)\\|mul\\|n\\|o\\(?:g\\|op\\|se_mem\\)\\)\\|m\\(?:a\\(?:rk\\|t_\\(?:\\(?:inver\\|transpo\\)se\\)\\|x\\(?:dt\\)?\\)\\|e\\(?:chanical\\|m\\(?:ory\\)?\\|ssage\\)\\|in\\(?:dt\\)?\\|o\\(?:del?\\|hr-coulomb\\|\\(?:nchrom\\|vi\\)e\\)\\)\\|n\\(?:c\\(?:ontours\\|write\\)\\|e\\(?:rr\\(?:_fish\\)?\\|w\\)\\|grwater\\|mechanical\\|ot\\|step\\|thermal\\|ull\\|wgpp\\)\\|o\\(?:pen\\|r\\|ut\\)\\|p\\(?:_stress\\|a\\(?:c\\|\\(?:lett\\|[ru]s\\)e\\)\\|fast\\|l\\(?:ot\\|t\\(?:angle\\|\\(?:cohes\\|frict\\|tens\\)ion\\)\\)\\|o\\(?:ro2\\|wer\\)\\|r\\(?:e\\(?:_?parse\\)\\|int\\|op\\)\\|slow\\|[ip]\\)\\|quit\\|r\\(?:_integrate\\|a\\(?:nge\\|yleigh\\)\\|e\\(?:ad\\|s\\(?:et\\|tore\\)\\|turn\\|z_exe\\)\\|\\(?:ff_pn\\|sa\\)t\\)\\|s\\(?:_\\(?:3dd\\|dyn\\|echo\\|flow\\|imp\\|log\\|m\\(?:e\\(?:ch\\|ss\\)\\|ovie\\)\\|therm\\)\\|a\\(?:t\\|ve\\)\\|cl\\(?:in\\|ose\\)\\|e\\(?:ction\\|t\\)\\|gn\\|i\\(?:g[12]\\|n\\)\\|m\\(?:_max\\|all\\)\\|o\\(?:lve\\|pen\\)\\|qrt\\|read\\|s\\(?:[ir]3d\\|[ir]\\)?\\|t\\(?:ate\\|ep\\|op\\|r\\(?:_pnt\\|ing\\|ucture\\)\\)\\|write\\|x[xy]\\|y[sy]\\|zz\\)\\|t\\(?:a\\(?:b\\(?:_pnt\\|le\\(?:_size\\)?\\)\\|n\\)\\|e\\(?:mperature\\|n\\(?:flg\\|sion\\)\\)\\|flow\\|h\\(?:dt\\|e\\(?:n\\|rmal\\|ta\\)\\|t\\(?:del\\|ime\\)\\)\\|itle\\|olerance\\|rac\\(?:_pnt\\|k\\)\\|ype\\)\\|u\\(?:biquitous\\|cs\\|d\\(?:coe\\|m_pnt\\)\\|mul\\|n\\(?:b\\(?:al\\|flow\\)\\|mark\\)\\|rand\\)\\|v\\(?:_n\\(?:gw\\|mech\\|therm\\)\\|ector\\|g\\(?:a\\|p\\(?:0\\|c\\(?:n?w\\)\\)\\)\\|is\\(?:cous\\|rat\\)\\|ol_strain\\|s\\(?:x[xz]\\|yy\\|zz\\|[ir]\\)?\\)\\|w\\(?:ater\\|b\\(?:iot\\|ulk\\)\\|dens\\|hile\\(?:_?stepping\\)?\\|i\\(?:ndow\\|pp\\)\\|k\\(?:1[12]\\|22\\)\\|rite\\)\\|x\\(?:acc\\|body\\|disp\\|f\\(?:low\\|or\\(?:ce\\|m\\)\\)\\|grav\\|nwflow\\|reaction\\|table\\|vel\\|ywrite\\)\\|y\\(?:acc\\|body\\|disp\\|f\\(?:low\\|orce\\)\\|grav\\|nwflow\\|reaction\\|table\\|vel\\)\\|z\\(?:_\\(?:copy\\|group\\|hyst\\|model\\|prop\\)\\|art\\|d\\(?:e\\(?:1[12]\\|22\\|33\\)\\|pp\\|rot\\)\\|msmul\\|poros\\|s\\(?:1[12]\\|22\\|33\\|ub\\)\\|t\\(?:e[a-d]\\|s[a-d]\\)\\|visc\\|xbar\\)\\|[rxy]\\)\\>"
 . 'font-lock-type-face)
    ("[0-9]+" . 'font-lock-variable-name-face))   
  '("\\.dat$" "\\.fis$")                      ;; files for which to activate this mode 
   nil                              ;; other functions to call
  "A mode for Itasca code data files"            ;; doc string for this mode
)


; "\\<\\(close\\|fsr\\|open\\|s\\(?:tring\\|xx\\|yy\\|zz\\)\\|write\\|xvel\\)\\>"

; eval with eval-print-last-sexp
(regexp-opt '(
"string" 
"sxx" 
"syy" 
"szz" 
"open" 
"close" 
"xvel"
"fsr"
"write"
) t)
