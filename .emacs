; basic key bindings

; C-q C-j to insert a newline in the mini-buffer, I can never remember this.

; C-o for find file
(global-set-key "\C-o" 'find-file)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

; C-i for search forward
(define-key input-decode-map (kbd "C-i") (kbd "H-i")); hack needed to unset tab
(global-set-key (kbd "H-i") 'isearch-forward)
(define-key isearch-mode-map (kbd "H-i") 'isearch-repeat-forward)

(global-set-key (kbd "M-s") 'ispell-word)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "<f1>") 'kill-buffer)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "M-k") ; kill the entire line
                '(lambda () (interactive)
                  (move-beginning-of-line nil)
                  (kill-line)))

; Unset problematic keys
(global-unset-key (kbd "C-x C-s"))
(global-unset-key (kbd "C-x k"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-x u"))
(global-unset-key [prior])  ; page up
(global-unset-key [next])   ; page down
(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])
(global-unset-key "\C-z")
(global-unset-key "\C-j")
(global-unset-key (kbd "M-$"))

(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "black")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "red")

(setq c-default-style "bsd"
      c-basic-offset 2)

(setq ispell-program-name "aspell")

(setq calendar-latitude 44.954109)
(setq calendar-longitude -93.187408)
(setq calendar-location-name "Minneapolis/St. Paul")

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
(global-set-key "\M-]" 'next-buffer)
(global-set-key "\M-[" 'previous-buffer)

(setq-default transient-mark-mode t)
(setq-default global-font-lock-mode t)
(setq-default  inhibit-startup-screen t)
(setq visible-bell t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(when (< 23 emacs-major-version)
  (electric-pair-mode 1))
(column-number-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq make-backup-files nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key "\C-jk" 'kill-all-buffers)
(global-set-key "\C-jK" 'kill-other-buffers)

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; OS specific setup

;; Linux specific setup
(if  (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    ;;; Lisp (SLIME) interaction -- linux only
    (progn
      (add-to-list 'load-path "~/src/dotfiles/")
      (add-to-list 'custom-theme-load-path "~/dotfiles/themes/")
      (setq x-select-enable-clipboard t)
      (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")))

;; OS X specific setup
(if (eq system-type 'darwin)
    (progn
      (setq x-select-enable-clipboard t)
      (add-to-list 'exec-path "/opt/local/bin/")))

;; windows specific setup
(if  (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (progn
      (remove-hook 'find-file-hooks 'vc-find-file-hook)

      (add-to-list 'load-path "c:/src/dotfiles/")
      (add-to-list 'custom-theme-load-path "c:/src/dotfiles/themes/")
      (load "./w32-browser.el")
      (load "./dired+.el")

      (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
      (add-to-list 'exec-path "C:/Program Files (x86)/GnuWin32/bin/")

      (let ((file-name "C:/src/Blo-Up/interpreter/sign.el"))
        (when (file-exists-p file-name)
          (load file-name)))
      (load "C:/src/itasca-emacs/itasca.el")

      ; windows specific magit init
      (defun magit-escape-for-shell (str)
        (if (or (string= str "git")
                (string-match "^--" str))
            str
          (concat "'" (replace-regexp-in-string "'" "'\\''" str) "'")))
      (custom-set-variables
       '(magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git"))

      ;; windows specific font stuff
      (setq w32-get-true-file-attributes nil)
      (set-default-font
       "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
      (set-face-attribute 'default nil :height 140)))


;; computer specific setup
(cond
 ; vaio
 ((equal (system-name) "SHOTOVER")
  (setq initial-frame-alist '((width . 80) (height . 37)))
  (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
  (add-to-list 'load-path "C:/src/slime/")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy)))

 ; vaio Ubuntu virtual machine
 ((equal (system-name) "u64")
  (setq initial-frame-alist '((width . 80) (height . 40)))
  (setq inferior-lisp-program "ecl")
  (add-to-list 'load-path "~/src/slime/")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy)))

 ((equal (system-name) "UNSER")
  (setq initial-frame-alist '((width . 80) (height . 41)))
  (set-face-attribute 'default nil :height 140)
  (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
  (add-to-list 'load-path "C:/src/slime/")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy)))

 ; default
 (t (setq initial-frame-alist '((width . 80) (height . 34)))))

;; note on windows $HOME is different in bash and emacs!
;; cp ~/.gitconfig ~/AppData/Roaming/
;; to get magit to recognize user.name and user.email

(load "sphinx.el")
(load "jkf-python.el")
(load "jkf-c.el")

(add-hook 'dired-mode-hook
          (function (lambda ()
                      (local-unset-key (kbd "C-o")))))

(add-hook 'emacs-lisp-mode-hook
          (function (lambda ()
		      (eldoc-mode t)
                      (local-unset-key (kbd "C-j")))))


(defun a2ps-file () (interactive)
  (let ((template  "a2ps.exe --columns=2 -o %s.ps -M letter --portrait %s")
        (fn (dired-get-filename)))
    (shell-command (format template fn fn ))))

(fset 'yes-or-no-p 'y-or-n-p)

(require 'smart-operator)
(require 'auto-complete-config)
(ac-config-default)
(require 'ein)
;(setq ein:use-auto-complete t)
(setq ein:use-auto-complete-superpack t)
(global-set-key [(shift return)] 'ein:worksheet-execute-cell)
(global-set-key (kbd "C-j n") 'ein:notebooklist-open)


(add-hook 'ein:notebook-multilang-mode-hook
          (function (lambda ()
		      (local-set-key (kbd "C-s")
				     'ein:notebook-save-notebook-command))))




;(add-to-list 'load-path "~/.emacs.d/")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'magit)
(require 'magit-svn)
(global-set-key "\C-js" 'magit-status)

;(load-theme 'zenburn)

(require 'ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(ido-vertical-mode t)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-M-z") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-z") 'ace-jump-mode)

;(require 'skeleton-complete)

; Display Visited Files Path in the Frame Title
; via emacs Redux
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(define-key global-map (kbd "C-S-n") 'move-line-down)
(define-key global-map (kbd "C-S-p") 'move-line-up)
