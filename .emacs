(setq-default inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;;; packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(ace-jump-mode dired+ dropdown-list ein auto-complete expand-region helm helm-descbinds ido-hacks ido-ubiquitous ido-vertical-mode macrostep markdown-mode magit melpa smartparens popup projectile dash request s slime smex uuid websocket yasnippet rainbow-delimiters minimap diminish elisp-slime-nav goto-last-change idomenu multiple-cursors ac-slime jedi cyberpunk-theme clojure-mode nrepl)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (if (y-or-n-p (format "Package %s is missing. Install it? " p))
        (package-install p))))

     ; get load-path first
(if (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (progn
      (add-to-list 'load-path "~/src/dotfiles/"))
  (progn
      (add-to-list 'load-path "c:/src/dotfiles/")))

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
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "<f1>") 'kill-this-buffer)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "<apps> /") 'ido-switch-buffer)
(global-set-key (kbd "<apps> .") 'smex)
(global-set-key (kbd "C-S-x") 'ido-switch-buffer)
(global-set-key (kbd "M-k") ; kill the entire line
                '(lambda () (interactive)
                  (move-beginning-of-line nil)
                  (kill-line)))
(global-set-key (kbd  "C-z") '(lambda () (interactive) nil))

; Unset problematic keys
(global-unset-key (kbd "C-x C-s"))
;(global-unset-key (kbd "C-x k"))
;(global-unset-key (kbd "C-x 0"))
;(global-unset-key (kbd "C-x 1"))
;(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-x u"))
(global-unset-key [prior])  ; page up
(global-unset-key [next])   ; page down
(global-unset-key [left])
(global-unset-key [right])
(global-unset-key [up])
(global-unset-key [down])
(global-unset-key (kbd "<insert>"))

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
(setq calendar-week-start-day 1)

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

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default transient-mark-mode t)
(setq-default global-font-lock-mode t)
(setq sentence-end-double-space nil)
(setq next-line-add-newlines t)
(setq visible-bell t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq calendar-week-start-day 1)

;; (when (< 23 emacs-major-version)
;;   (electric-pair-mode 1))
(column-number-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq make-backup-files nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Sphinx reStructuredText Setup

(defun chunk-start ()
  "move point to begining of white-space seperated chunk"
  (interactive)
  (search-backward-regexp "\\s-")
  (forward-char))
(defun chunk-end ()
  "move point to end of white-space separated chunk"
  (interactive)
  (search-forward-regexp "\\s-")
  (backward-char))
(defun rest-wrap-math ()
  "wrap :math:`__` around the current word"
  (interactive)
  (chunk-start)
  (insert ":math:`")
  (chunk-end)
  (insert "`"))

(defun s-compile-cmd (cmd)
  "build sphinx documentation. First call prompts for a directory"
  (interactive)
  (unless (boundp 'sphinx-build-dir)
    (setq sphinx-build-dir (read-directory-name "sphinx build dir ")))
  (let ((default-directory sphinx-build-dir))
       (compile cmd)))
(defun s-compile () (interactive) (s-compile-cmd "make html"))
(defun s-pcompile () (interactive) (s-compile-cmd "make latexpdf"))

(defun sphinx-reset () (interactive) (makunbound 'sphinx-build-dir))

(defun sphinx-open-pdf () (interactive)
  (when (boundp 'sphinx-build-dir)
    (w32-browser (car (file-expand-wildcards
                       (concat sphinx-build-dir "build/latex/*.pdf"))))))
(require 'rst)
(define-key rst-mode-map (kbd "C-c p") 'sphinx-open-pdf)
(define-key rst-mode-map (kbd "C-c C") 's-compile)
(define-key rst-mode-map (kbd "C-c c") 's-pcompile)
(define-key rst-mode-map (kbd "C-c m") 'rest-wrap-math)

;;;; FORTRAN Setup

(add-to-list 'auto-mode-alist '("\\.inc\\'" . fortran-mode))

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

;;;; C/C++ Setup

(defun filename-comment ()
  "Insert filename as c++ comment eg. //filename.h"
  (interactive)
  (insert (concat "//" (file-name-nondirectory buffer-file-name))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(fset 'move-comment-above
   [?\C-s ?/ ?\C-b ?\C-k ?\C-a ?\C-y return ?\C-n])

(defun move-region-to-file (a b fname)
  "Text in the region is moved to the given new file"
 (interactive "r\nFMove region to new file:")
 (if (file-exists-p fname) (error "File already exists"))
 (kill-region a b)
 (find-file fname)
 (yank))

(defun move-region-to-header-file (a b fname)
  "Text in the region is moved to the given new file \n #include \"filename.h\" is inserted at the current location"
 (interactive "r\nFMove region to new header file:")
 (if (file-exists-p fname) (error "File already exists"))
 (kill-region a b)
 (insert (concat "#include \"" (file-name-nondirectory fname) "\"\n"))
 (find-file fname)
 (yank))

;(setq before-save-hook nil)
(add-hook 'before-save-hook
                   (lambda ()
                     (unless
                         (string= "Makefile" (buffer-name))
                       (untabify (point-min) (point-max)))))

;;;; Python Setup

(require 'cython-mode)
(setq python-check-command "pep8 -r --ignore=E221")
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))

(defun p-compile ()
  "build python extension module. First call prompts for a directory"
  (interactive)
  (unless (boundp 'python-build-dir)
    (setq python-build-dir (read-directory-name "python build dir ")))
  (let ((default-directory python-build-dir)
        (extra-arg (if (eq  system-type 'windows-nt)
                       " " " --user")))
    (compile (concat "python setup.py install" extra-arg))))

(define-key python-mode-map (kbd "C-c M-c") 'copy-run-buffer-filename-as-kill)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(require 'ein)
(setq ein:use-auto-complete-superpack t)
(global-set-key [(shift return)] 'ein:worksheet-execute-cell)
(global-set-key (kbd "C-c n") 'ein:notebooklist-open)

(add-hook 'ein:notebook-multilang-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "C-s")
                                     'ein:notebook-save-notebook-command))))

(add-hook 'python-mode-hook (function (lambda ()
                                        (setq python-indent-offset 4))))

;;;; Lisp Setup

;; (require 'paredit)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'pair-jump-mode)


;; (eval-after-load 'slime
;;   (add-hook 'slime-mode-hook
;;          (progn
;;            (function (lambda ()
;;                        (local-set-key
;;                         (kbd "M-n" 'backward-paragraph))))
;;            (function (lambda ()
;;                        (local-set-key
;;                         (kbd "M-p" 'forward-paragraph)))))))

;(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'pair-jump-mode)
(setq edebug-trace nil)

;; .emacs section navigation by ;;;; Section name
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; Org-mode Setup

;(global-set-key (kbd "C-c a") 'org-agend)
(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)
(require 'org-tree-slide)
(define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
(define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
(define-key org-mode-map (kbd "C-c =") 'calc-eval-line-and-insert)

(setq org-tree-slide-slide-in-effect nil)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook 'pair-jump-mode)


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
     ;; Linux specific setup
(if  (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    ;;; Lisp (SLIME) interaction -- linux only
    (progn
      (setq x-select-enable-clipboard t)
      (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
      (set-register ?e '(file . "~/src/dotfiles/.emacs"))
      (set-register ?n '(file . "~/src/orgfile/notes.org"))

      (global-unset-key (kbd "<menu>"))
      (global-set-key (kbd "<menu> /") 'ido-switch-buffer)
      (global-set-key (kbd "<menu> .") 'smex)

      ; this is OK when you remap menu....
      (global-set-key (kbd "s-/") 'ido-switch-buffer)
      (global-set-key (kbd "s-.") 'smex)


      (add-to-list 'yas/snippet-dirs "~/src/itasca-emacs/snippets")
      (add-to-list 'yas/snippet-dirs "~/src/dotfiles/snippets")
      (add-to-list 'ac-dictionary-directories "~/src/itasca-emacs/ac-dict")
      (setq eshell-rc-script "~/src/dotfiles/eshellrc")

      (add-to-list 'load-path "~/src/itasca-emacs" )
      (require 'itasca)
      (progn
        (add-to-list 'ac-modes 'itasca-general-mode)
        (add-to-list 'ac-modes 'itasca-pfc-mode)
        (add-to-list 'ac-modes 'itasca-flac-mode)
        (add-to-list 'ac-modes 'itasca-flac3d-mode)
        (add-to-list 'ac-modes 'itasca-udec-mode))

                                        ; clojure
      (progn
        (defadvice nrepl-eval-last-expression (after nrepl-flash-last activate)
          (if (fboundp 'slime-flash-region)
              (slime-flash-region (save-excursion (backward-sexp) (point)) (point))))

        (defadvice nrepl-eval-expression-at-point (after nrepl-flash-at activate)
          (if (fboundp 'slime-flash-region)
              (apply #'slime-flash-region (nrepl-region-for-expression-at-point))))

        (defadvice nrepl-default-err-handler (after nrepl-focus-errors activate)
          "Focus the error buffer after errors, like Emacs normally does."
          (select-window (get-buffer-window "*nrepl-error*"))))))

     ;; OS X specific setup
(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :family "Monaco"
                          :height 145 :weight 'normal)
      (setq eshell-rc-script "~/src/dotfiles/eshellrc_osx")
      (add-to-list 'yas/snippet-dirs "~/src/dotfiles/snippets")
      (let ((org-note-file
             "~/Dropbox/org/notes.org"))
        (setq org-default-notes-file org-note-file)
        (setq org-agenda-files (list org-note-file))
        (set-register ?n `(file . ,org-note-file)))
      (setq x-select-enable-clipboard t)
      (add-to-list 'exec-path "/opt/local/bin/")))

     ;; windows specific setup
(if  (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (progn
      (setq doc-view-ghostscript-program
            "c:/Program Files (x86)/gs/gs9.02/bin/gswin32c.exe")
      (setq explicit-shell-file-name
            "C:/Program Files (x86)/Git/bin/bash.exe")
      ;; (setq image-dired-cmd-create-thumbnail-program
      ;;            "C:/Program Files (x86)/ImageMagick-6.8.5-Q16/convert")
      ;; (setq image-dired-cmd-create-standard-thumbnail-command
      ;;            (s-replace "convert" image-dired-cmd-create-thumbnail-program ))
      (setq shell-file-name explicit-shell-file-name)
      (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")

      (add-to-list 'custom-theme-load-path "c:/src/dotfiles/")

                                        ; to get grep working?
      (defadvice shell-quote-argument
        (after windows-nt-special-quote (argument) activate)
        "Add special quotes to ARGUMENT in case the system type is 'windows-nt."
        (when
            (and (eq system-type 'windows-nt) (w32-shell-dos-semantics))
          (if (string-match "[\\.~]" ad-return-value)
              (setq ad-return-value
                    (replace-regexp-in-string
                     "\\([\\.~]\\)"
                     "\\\\\\1"
                     ad-return-value)))))

      (load "./w32-browser.el")
      (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
      (add-to-list 'exec-path "C:/Program Files (x86)/GnuWin32/bin/")
      (add-to-list 'exec-path "c:/Program Files (x86)/Git/bin/")
      (add-to-list 'exec-path "C:/Program Files (x86)/ImageMagick-6.8.5-Q16/")

      (add-to-list 'yas/snippet-dirs "c:/src/itasca-emacs/snippets")
      (add-to-list 'yas/snippet-dirs "c:/src/dotfiles/snippets")
      (add-to-list 'ac-dictionary-directories "c:/src/itasca-emacs/ac-dict")
      (setq eshell-rc-script "c:/src/dotfiles/eshellrc")

      (set-register ?e '(file . "c:/src/dotfiles/.emacs"))
      (set-register ?s '(file . "c:/src/"))

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
 ((equal (system-name) "SHOTOVER")                            ; vaio
  (setq initial-frame-alist '((width . 80) (height . 37)))
  (set-face-attribute 'default nil :height 140)
  (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy))
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))

  (setq doc-view-ghostscript-program
        "c:/Program Files (x86)/gs/gs9.07/bin/gswin32c.exe")


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

  (set-register ?b '(file . "c:/Users/jfurtney/dropbox"))
  (set-register ?d '(file . "c:/Users/jfurtney/downloads")))
                                        ;(slime-setup '(slime-repl slime-fancy))

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

                                        ; vaio Ubuntu virtual machine
 ((equal (system-name) "u64")
  (setq initial-frame-alist '((width . 80) (height . 40)))
  (setq inferior-lisp-program "ecl")
  (require 'slime)
;  (slime-setup '(slime-repl slime-fancy))
)

                                        ;(require 'slime) ;; ? this is broken
                                        ;(slime-setup '(slime-repl slime-fancy))

                                        ; default
 ((equal (system-name) "uvb64") ; work virtual machine
  (set-face-attribute 'default nil :height 140)
  ;(set-default-font "-*-terminus-medium-r-*-*-*-140-75-75-*-*-iso8859-15")
  ;(set-default-font "-raster-Fixedsys-normal-r-normal-normal-12-90-96-96-c-*-*-*")
  )
 (t (setq initial-frame-alist '((width . 80) (height . 34)))))

 ;; note on windows $HOME is different in bash and emacs!
 ;; cp ~/.gitconfig ~/AppData/Roaming/
 ;; to get magit to recognize user.name and user.email

(defun a2ps-buffer ()
  "call a2ps on the file the current buffer is visiting. Opens
the resulting postscript file"
  (interactive)
  (let ((template  "a2ps.exe --columns=2 -o %s.ps -M letter --portrait %s")
        (fn (buffer-file-name)))
    (shell-command (format template fn fn ))
    (w32-browser (format "%s.ps" fn))))

(defun a2ps-file () (interactive)
  "in dired call this function on a selected file to process the
file with a2ps"
  (let ((template  "a2ps.exe --columns=2 -o %s.ps -M letter --portrait %s")
        (fn (dired-get-filename)))
    (shell-command (format template fn fn ))))

(require 'smart-operator)
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
(setq ido-case-fold t)

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

(require 'helm-config)
(require 'helm-descbinds)
(require 'imenu-anywhere)
;(global-set-key (kbd "C-.") 'helm-imenu)
(global-set-key (kbd "C-M-.") 'helm-imenu)
(global-set-key (kbd "C-h b") 'helm-descbinds)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-S-o") 'helm-recentf)

(define-key global-map (kbd "RET") 'newline-and-indent)

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

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(require 'elisp-slime-nav)
(require 'eldoc)
(require 'diminish)
;(diminish 'paredit-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'pair-jump-mode)
(diminish 'yas-minor-mode)
(diminish 'smart-operator-mode)
(diminish 'eldoc-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-fill-function)
(diminish 'abbrev-mode)

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(defun copy-run-buffer-filename-as-kill ()
  "Insert the string: '%run file-name' to the clipboard where
file-name is the full path and filename of the current buffer.
Useful when editing a datafile in emacs and loading it IPython."
  (interactive)
  (let ((s (format "%%run %s" (buffer-file-name))))
    (kill-new s)
    (message "Copied: %s to clipboard" s)))

(defun copy-load-buffer-filename-as-kill ()
  "Insert the string: '%run file-name' to the clipboard where
file-name is the full path and filename of the current buffer.
Useful when editing a datafile in emacs and loading it a lisp."
  (interactive)
  (let ((s (format "(load \"%s\")" (buffer-file-name))))
    (kill-new s)
    (message "Copied: %s to clipboard" s)))

; from http://www.emacswiki.org/emacs/buffer-extension.el
(defun copy-buffer-file-name-as-kill (choice)
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
(global-set-key (kbd "C-c M-c") 'copy-buffer-file-name-as-kill)

(global-set-key (kbd "C-c j c") 'goto-last-change)
(global-set-key (kbd "C-c j b") 'beginning-of-buffer)
(global-set-key (kbd "C-c j e") 'end-of-buffer)
(global-set-key (kbd "C-c j f") 'idomenu)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(global-set-key (kbd "M-x") 'smex)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

; on windows the magit process hangs constantly
(defun kill-magit ()
  "Kill the Magit process buffer"
  (interactive)
  (delete-process "*magit-process*"))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun =-transpose ()
  "Transpose the text before and after the first equals sign"
  (interactive)
  (let ((lhs (buffer-substring (point-at-bol) (point)))
        (rhs (buffer-substring (1+ (point)) (point-at-eol))))
    (end-of-line)
    (newline-and-indent)
    (insert (format "%s = %s" (chomp rhs) (chomp lhs)))))

(load-theme 'cyberpunk t)

(defun calc-eval-and-insert (&optional start end)
  (interactive "r")
  (let ((result (calc-eval (buffer-substring-no-properties start end))))
    (goto-char (point-at-eol))
    (insert " = " result)))

(defun calc-eval-line-and-insert ()
  (interactive)
  (calc-eval-and-insert (point-at-bol) (point-at-eol)))

(global-set-key (kbd "C-c =") 'calc-eval-line-and-insert)

(require 'smartparens-config)
(smartparens-global-mode t)

(global-set-key (kbd "C-}") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-{") 'sp-backward-slurp-sexp)
(global-set-key (kbd "M-}") 'sp-forward-barf-sexp)
(global-set-key (kbd "M-{") 'sp-backward-barf-sexp)
