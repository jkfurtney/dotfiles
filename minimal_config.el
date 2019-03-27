(setq-default inhibit-startup-screen t)
(menu-bar-mode 0)

(global-set-key "\C-o" 'find-file)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)


(global-set-key (kbd "C-c c") 'calc)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'revert-buffer)

(define-key input-decode-map (kbd "C-i") (kbd "H-i")); hack needed to unset tab
(global-set-key (kbd "H-i") 'isearch-forward)
(define-key isearch-mode-map (kbd "H-i") 'isearch-repeat-forward)

(global-set-key (kbd "M-s") 'ispell-word)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "<f1>") 'kill-this-buffer)

(global-unset-key (kbd "C-x C-s"))
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-x u"))
(global-unset-key [prior])  ; page up
(global-unset-key [next])   ; page down
(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])
(global-unset-key (kbd "<insert>"))
(global-set-key (kbd "C-x r q") 'kill-emacs)

