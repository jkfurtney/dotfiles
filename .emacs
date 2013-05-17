(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;;; packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(ace-jump-mode dired+ dropdown-list ein auto-complete expand-region helm helm-descbinds ido-hacks ido-ubiquitous ido-vertical-mode macrostep markdown-mode magit melpa paredit popup projectile dash request s slime smex uuid websocket yasnippet rainbow-delimiters minimap diminish elisp-slime-nav goto-last-change idomenu)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (if (y-or-n-p (format "Package %s is missing. Install it? " p))
        (package-install p))))

;;;; basic key bindings
(require 'dired+)
(global-set-key "\C-o" 'find-file) ; C-o for find file
(add-hook 'dired-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "<backspace>") 'kill-this-buffer)
                      (local-unset-key (kbd "<f1>"))
                      (local-unset-key (kbd "C-o")))))

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e s") '(lambda ()
                                   (interactive)
                                   (switch-to-buffer "*scratch*")
                                   (insert ";; scratch buffer")
                                   (newline)))
(global-set-key (kbd "C-c e m") 'macrostep-expand)

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
(global-set-key (kbd "<f1>") 'kill-this-buffer)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "<f6>") 'ido-switch-buffer)
(global-set-key (kbd "M-k") ; kill the entire line
                '(lambda () (interactive)
                  (move-beginning-of-line nil)
                  (kill-line)))
(global-set-key (kbd  "C-z") '(lambda () (interactive) nil))

; Unset problematic keys
(global-unset-key (kbd "C-x C-s"))
;(global-unset-key (kbd "C-x k"))
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

(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "black")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "red")

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

(global-set-key (kbd "C-x M-q") 'remove-hard-wrap)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

(setq-default transient-mark-mode t)
(setq-default global-font-lock-mode t)
(setq-default inhibit-startup-screen t)
(setq visible-bell t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(setq show-paren-delay 0)

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

(global-set-key (kbd "C-c k") 'kill-all-buffers)
(global-set-key (kbd "C-c K") 'kill-other-buffers)

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)

(require 'yasnippet)
(yas/global-mode 1)

(require 'auto-complete-config)
(ac-config-default)

;;;; OS specific setup

     ; get load-path first
(if (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (progn
      (add-to-list 'load-path "~/src/dotfiles/"))
  (progn
      (add-to-list 'load-path "c:/src/dotfiles/")))

     ;; Linux specific setup
(if  (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    ;;; Lisp (SLIME) interaction -- linux only
    (progn
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

      (load "./w32-browser.el")
      (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
      (add-to-list 'exec-path "C:/Program Files (x86)/GnuWin32/bin/")

      (add-to-list 'yas/snippet-dirs "c:/src/itasca-emacs/snippets")
      (add-to-list 'yas/snippet-dirs "c:/src/dotfiles/snippets")
      (add-to-list 'ac-dictionary-directories "c:/src/itasca-emacs/ac-dict")

      (let ((file-name "C:/src/Blo-Up/interpreter/sign.el"))
        (when (file-exists-p file-name)
          (load file-name)))

      (add-to-list 'load-path "C:/src/itasca-emacs")
      (require 'itasca)
      (progn
        (add-to-list 'ac-modes 'itasca-general-mode)
        (add-to-list 'ac-modes 'itasca-pfc-mode)
        (add-to-list 'ac-modes 'itasca-flac-mode)
        (add-to-list 'ac-modes 'itasca-flac3d-mode)
        (add-to-list 'ac-modes 'itasca-udec-mode))
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


;;;; computer specific setup
(cond
 ; vaio
 ((equal (system-name) "SHOTOVER")
  (setq initial-frame-alist '((width . 80) (height . 37)))
  (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
  (require 'slime)

  (display-time-mode 1)
                                        ; org mode
  (setq org-mobile-directory "c:/Users/jfurtney/Dropbox/Apps/MobileOrg")
  (setq org-directory "c:/Users/jfurtney/Dropbox/org/")
  (setq org-mobile-inbox-for-pull "c:/Users/jfurtney/Dropbox/org/flagged.org")

  (let ((org-note-file
         "c:/Users/jfurtney/Dropbox/org/notes.org"))
    (setq org-default-notes-file org-note-file)
    (setq org-agenda-files (list org-note-file))
    (set-register ?n `(file . ,org-note-file)))

  (set-register ?d '(file . "c:/Users/jfurtney/downloads")))
                  ;(slime-setup '(slime-repl slime-fancy))

  ; vaio Ubuntu virtual machine
 ((equal (system-name) "u64")
  (setq initial-frame-alist '((width . 80) (height . 40)))
  (setq inferior-lisp-program "ecl")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy)))

 ((equal (system-name) "UNSER")
  (setq initial-frame-alist '((width . 80) (height . 41)))
  (set-face-attribute 'default nil :height 140)
  (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
  ; org mode
  (setq org-mobile-directory "c:/Users/Itasca/Dropbox/Apps/MobileOrg")
  (setq org-directory "c:/Users/Itasca/Dropbox/org/")
  (setq org-mobile-inbox-for-pull "c:/Users/Itasca/Dropbox/org/flagged.org")

  (let ((org-note-file
         "c:/Users/Itasca/Dropbox/org/notes.org"))
    (setq org-default-notes-file org-note-file)
    (setq org-agenda-files (list org-note-file))
    (set-register ?n `(file . ,org-note-file)))

  (set-register ?d '(file . "c:/Users/Itasca/downloads")))

  (require 'slime)
  (slime-setup '(slime-repl slime-fancy))

                                        ; default
  (t (setq initial-frame-alist '((width . 80) (height . 34)))))

 ;; note on windows $HOME is different in bash and emacs!
 ;; cp ~/.gitconfig ~/AppData/Roaming/
 ;; to get magit to recognize user.name and user.email


(load "jkf-sphinx.el")
(load "jkf-python.el")
(load "jkf-c.el")

(defun a2ps-file () (interactive)
  (let ((template  "a2ps.exe --columns=2 -o %s.ps -M letter --portrait %s")
        (fn (dired-get-filename)))
    (shell-command (format template fn fn ))))

(fset 'yes-or-no-p 'y-or-n-p)

(require 'smart-operator)

(require 'ein)
(setq ein:use-auto-complete-superpack t)
(global-set-key [(shift return)] 'ein:worksheet-execute-cell)
(global-set-key (kbd "C-c n") 'ein:notebooklist-open)

(add-hook 'ein:notebook-multilang-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "C-s")
                                     'ein:notebook-save-notebook-command))))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'magit)
(require 'magit-svn)
(global-set-key (kbd "C-c s") 'magit-status)

(require 'ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(ido-vertical-mode t)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

(require 'ace-jump-mode)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-M-z") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-z") 'ace-jump-mode)

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

(require 'paredit)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(setq edebug-trace nil)
(setq sentence-end-double-space nil)
(setq next-line-add-newlines t)

(require 'helm-config)
(require 'helm-descbinds)
(require 'imenu-anywhere)
(global-set-key (kbd "C-.") 'helm-imenu-anywhere)
(global-set-key (kbd "C-h b") 'helm-descbinds)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-S-o") 'helm-recentf)

(define-key global-map (kbd "RET") 'newline-and-indent)

(set-register ?e '(file . "c:/src/dotfiles/.emacs"))
(set-register ?s '(file . "c:/src/"))


; Dont prompt me if I try to kill a buffer with an active process
; via http://www.masteringemacs.org/
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;(which-function-mode 1)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(yas-reload-all)

(require 'pair-jump-mode)
(pair-jump-mode 1)

;(require 'helm-gtags)
;(setq helm-gtags-ignore-case t)

;; (require 'ggtags)
;; (add-hook 'c++-mode-hook 'ggtags-mode)
;; (add-hook 'c-mode-hook 'ggtags-mode)
;; (add-hook 'fortran-mode-hook 'ggtags-mode)

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(require 'elisp-slime-nav)
(require 'eldoc)
(require 'diminish)
(require 'ggtags)
(diminish 'paredit-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'pair-jump-mode)
(diminish 'yas-minor-mode)
(diminish 'smart-operator-mode)
(diminish 'eldoc-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-fill-function)
(diminish 'abbrev-mode)
(diminish 'ggtags-mode)

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c `") 'menu-bar-mode)

  ;; .emacs section navigation
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(global-set-key (kbd "C-c a") 'org-agend)
(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)

(defun jkf-setup-fortran-mode () (interactive)
  (pair-jump-mode 1)
  (which-function-mode 1))

(add-hook 'fortran-mode-hook 'jkf-setup-fortran-mode)

(defun udec-string (s)
  "Prompt for a string and insert it at point as a FORTRAN char
array literal. A training space character is added, the total
number of characters is written to the message area."
  (interactive "Mstring for conversion: ")
  (dolist (c (string-to-list s))
    (insert (format "'%c'," c)))
  (insert "' ',")
  (message "%i chars " (1+ (length s))))

(defun copy-call-buffer-filename-as-kill ()
  "Insert the string: 'call file-name' to the clipboard where
file-name is the full path and filename of the current buffer.
Useful when editing a datafile in emacs any loading it into an Itasca code."
  (interactive)
  (let ((s (format "call \"%s\"" (buffer-file-name))))
    (kill-new s)
    (message "Copied: %s to clipboard" s)))

; from http://www.emacswiki.org/emacs/buffer-extension.el
(defun copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(global-set-key (kbd "C-c j c") 'goto-last-change)
(global-set-key (kbd "C-c j b") 'beginning-of-buffer)
(global-set-key (kbd "C-c j e") 'end-of-buffer)
(global-set-key (kbd "C-c j f") 'idomenu)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . fortran-mode))
(global-set-key (kbd "M-x") 'smex)
