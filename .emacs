(setq-default inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq magit-last-seen-setup-instructions "1.4.0")

;;;; packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; ein expand-region projectile goto-last-change clojure-mode diff-hl
(defvar my-packages '(ace-jump-mode dired+ dropdown-list  auto-complete  helm helm-descbinds ido-hacks ido-ubiquitous ido-vertical-mode macrostep markdown-mode magit smartparens popup dash request s slime smex uuid websocket yasnippet rainbow-delimiters minimap diminish elisp-slime-nav idomenu multiple-cursors ac-slime jedi cyberpunk-theme fold-dwim htmlize god-mode connection ox-reveal)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (if (y-or-n-p (format "Package %s is missing. Install it? " p))
        (package-install p))))

                                        ; install org and org-plus-extras from here:
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(defvar dotfile-dir nil "location of .emacs and other stuff")

(if (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (setq dotfile-dir (expand-file-name "~/src/dotfiles/"))
  (setq dotfile-dir "c:/src/dotfiles/"))
(add-to-list 'load-path dotfile-dir)
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
(global-set-key (kbd "M-<apps>") 'ido-switch-buffer)
(global-set-key (kbd "C-<apps>") 'other-window)
(global-set-key (kbd "C-<lwindow>") 'smex)
(global-set-key (kbd "M-<lwindow>") 'other-window)
(global-set-key (kbd "<apps> .") 'smex)
;(global-set-key (kbd "C-S-x") 'ido-switch-buffer)
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
(global-set-key (kbd "C-x r q") 'kill-emacs)

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

(defun jkf/dont-kill-emacs ()
 (interactive)
 (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
(global-set-key "\C-x\C-c" 'jkf/dont-kill-emacs)

(defun jkf/remove-hard-wrap ()
  "Make several lines into a single long line."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(global-set-key (kbd "C-x M-q") 'jkf/remove-hard-wrap)
(global-set-key (kbd "C-c ;") 'comment-region)

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
;(add-hook 'text-mode-hook 'refill-mode)
;(add-hook 'latex-mode-hook 'refill-mode)

;;;; Sphinx reStructuredText Setup

(defun jkf/chunk--start ()
  "move point to begining of white-space seperated chunk"
  (interactive)
  (search-backward-regexp "\\s-")
  (forward-char))
(defun jkf/chunk--end ()
  "move point to end of white-space separated chunk"
  (interactive)
  (search-forward-regexp "\\s-")
  (backward-char))
(defun jkf/rest-wrap-math ()
  "wrap :math:`__` around the current word"
  (interactive)
  (jkf/chunk--start)
  (insert ":math:`")
  (jkf/chunk--end)
  (insert "`"))

(defun jkf/sphinx--compile-cmd (cmd)
  "build sphinx documentation. First call prompts for a directory"
  (interactive)
  (unless (boundp 'sphinx-build-dir)
    (setq sphinx-build-dir (read-directory-name "sphinx build dir ")))
  (let ((default-directory sphinx-build-dir))
       (compile cmd)))
(defun jkf/sphinx-html-compile () (interactive)
  (jkf/sphinx--compile-cmd "make html"))
(defun jkf/sphinx-pdf-compile () (interactive)
  (jkf/sphinx--compile-cmd "make latexpdf"))

(defun jkf/sphinx-reset () (interactive) (makunbound 'sphinx-build-dir))

(defun jkf/sphinx-open-pdf-windows () (interactive)
  (when (boundp 'sphinx-build-dir)
    (w32-browser (car (file-expand-wildcards
                       (concat sphinx-build-dir "build/latex/*.pdf"))))))
(require 'rst)
(define-key rst-mode-map (kbd "C-c p") 'jkf/sphinx-open-pdf-windows)
(define-key rst-mode-map (kbd "C-c C") 'jkf/sphinx-html-compile)
(define-key rst-mode-map (kbd "C-c c") 'jkf/sphinx-pdf-compile)
(define-key rst-mode-map (kbd "C-c m") 'jkf/rest-wrap-math)
;(add-hook 'rst-mode-hook 'refill-mode)

;;;; FORTRAN Setup

(add-to-list 'auto-mode-alist '("\\.inc\\'" . fortran-mode))

(defun jkf/setup-fortran-mode () (interactive)
  ;(pair-jump-mode 1)
  (which-function-mode 1))

(add-hook 'fortran-mode-hook 'jkf/setup-fortran-mode)

(defun jkf/udec-string (s)
  "Prompt for a string and insert it at point as a FORTRAN char
array literal. A training space character is added, the total
number of characters is written to the message area."
  (interactive "Mstring for conversion: ")
  (dolist (c (string-to-list s))
    (insert (format "'%c'," c)))
  (insert "' ',")
  (message "%i chars " (1+ (length s))))

;;;; C/C++ Setup

(defun jkf/filename-comment ()
  "Insert filename as c++ comment eg. //filename.h"
  (interactive)
  (insert (concat "//" (file-name-nondirectory buffer-file-name))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(fset 'move-comment-above
   [?\C-s ?/ ?\C-b ?\C-k ?\C-a ?\C-y return ?\C-n])

(defun jkf/move-region-to-file (a b fname)
  "Text in the region is moved to the given new file"
 (interactive "r\nFMove region to new file:")
 (if (file-exists-p fname) (error "File already exists"))
 (kill-region a b)
 (find-file fname)
 (yank))

(defun jkf/move-region-to-header-file (a b fname)
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

(defun jkf/python-extension-compile ()
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
(add-hook 'python-mode-hook 'hs-minor-mode)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;(require 'ein)
;(setq ein:use-auto-complete-superpack t)
;(global-set-key [(shift return)] 'ein:worksheet-execute-cell)
;(global-set-key (kbd "C-c n") 'ein:notebooklist-open)

;; (add-hook 'ein:notebook-multilang-mode-hook
;;           (function (lambda ()
;;                       (local-set-key (kbd "C-s")
;;                                      'ein:notebook-save-notebook-command))))

(add-hook 'python-mode-hook (function (lambda ()
                                        (setq python-indent-offset 4))))

;;;; Lisp Setup

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'lisp-mode-hook 'pair-jump-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;(add-hook 'emacs-lisp-mode-hook 'pair-jump-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(setq edebug-trace nil)

;; .emacs section navigation by ;;;; Section name
(defun jkf/imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'jkf/imenu-elisp-sections)

;;;; Org-mode Setup

;(global-set-key (kbd "C-c a") 'org-agend)
(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)
(require 'org-tree-slide)
(define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
(define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
(define-key org-mode-map (kbd "C-c =") 'jkf/calc-eval-line-and-insert)
(define-key org-mode-map (kbd "C-M-k") 'kill-sentence)

(setq org-tree-slide-slide-in-effect nil)
(setq org-src-fontify-natively t)
;(add-hook 'org-mode-hook 'pair-jump-mode)
;(add-hook 'org-mode-hook 'refill-mode)
(setq org-startup-truncated nil)


(defun jkf/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key (kbd "C-c k") 'jkf/kill-all-buffers)
(global-set-key (kbd "C-c K") 'jkf/kill-other-buffers)

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun jkf/compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    (cons msg code))
(setq compilation-exit-message-function 'jkf/compilation-exit-autoclose)

(require 'yasnippet)
(yas/global-mode 1)

(require 'auto-complete-config)
(ac-config-default)


;;;; Linux specific setup
(if  (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (progn
      (setq x-select-enable-clipboard t)
      (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
      (set-register ?e '(file . "~/src/dotfiles/.emacs"))
      (set-register ?n '(file . "~/src/orgfile/notes.org"))

      (global-unset-key (kbd "<menu>"))
      (global-set-key (kbd "M-<menu>") 'ido-switch-buffer)
      (global-set-key (kbd "<menu> /") 'smex)

      ; this is OK when you remap menu....
      ;(global-set-key (kbd "s-/") 'ido-switch-buffer)
      ;(global-set-key (kbd "s-.") 'smex)


      (add-to-list 'yas/snippet-dirs "~/src/itasca-emacs/snippets")
      (add-to-list 'yas/snippet-dirs "~/src/dotfiles/snippets")
      (add-to-list 'ac-dictionary-directories "~/src/itasca-emacs/ac-dict")
      (setq eshell-rc-script "~/src/dotfiles/eshellrc")

      (add-to-list 'load-path "~/src/itasca-emacs" )
      (require 'itasca)
      (progn
        (add-to-list 'ac-modes 'itasca-general-mode)
        (add-to-list 'ac-modes 'itasca-pfc-mode)
        (add-to-list 'ac-modes 'itasca-pfc5-mode)
        (add-to-list 'ac-modes 'itasca-flac-mode)
        (add-to-list 'ac-modes 'itasca-flac3d-mode)
        (add-to-list 'ac-modes 'itasca-udec-mode)
        (add-to-list 'ac-modes 'itasca-3dec-mode))

                                        ; clojure
      ;; (progn
      ;;   (defadvice nrepl-eval-last-expression (after nrepl-flash-last activate)
      ;;     (if (fboundp 'slime-flash-region)
      ;;         (slime-flash-region (save-excursion (backward-sexp) (point)) (point))))

      ;;   (defadvice nrepl-eval-expression-at-point (after nrepl-flash-at activate)
      ;;     (if (fboundp 'slime-flash-region)
      ;;         (apply #'slime-flash-region (nrepl-region-for-expression-at-point))))

      ;;   (defadvice nrepl-default-err-handler (after nrepl-focus-errors activate)
      ;;     "Focus the error buffer after errors, like Emacs normally does."
      ;;     (select-window (get-buffer-window "*nrepl-error*"))))
      ))

;;;; OS X specific setup
(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "<M-268632080>") 'ido-switch-buffer)
      (set-face-attribute 'default nil :family "Monaco"
                          :height 145 :weight 'normal)
      (setq initial-frame-alist '((width . 80) (height . 52)))
      (setq eshell-rc-script "~/src/dotfiles/eshellrc_osx")
      (add-to-list 'yas/snippet-dirs "~/src/dotfiles/snippets")
      (let ((org-note-file
             "~/Dropbox/org/notes.org"))
        (setq org-default-notes-file org-note-file)
        (setq org-agenda-files (list org-note-file))
        (set-register ?n `(file . ,org-note-file)))
      (setq x-select-enable-clipboard t)
      (add-to-list 'exec-path "/opt/local/bin/")))

;;;; windows specific setup
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
      (add-to-list 'exec-path "c:/Program Files/Git/bin/")
      (add-to-list 'exec-path "C:/Program Files (x86)/ImageMagick-6.8.5-Q16/")
      (add-to-list 'exec-path "C:/Program Files (x86)/ImageMagick-6.8.5-Q16/")
      (add-to-list 'exec-path "C:/Program Files (x86)/MiKTeX 2.9/miktex/bin/")

      (defun jkf/add-to-path (path-str)
        (setenv "PATH" (concat (getenv "PATH")
                               ";" path-str)))

      (jkf/add-to-path "C:/Program Files (x86)/GnuWin32/bin/")
      (jkf/add-to-path "C:/Program Files (x86)/MiKTeX 2.9/miktex/bin/")

      (add-to-list 'yas/snippet-dirs "c:/src/itasca-emacs/snippets")
      (add-to-list 'yas/snippet-dirs "c:/src/dotfiles/snippets")
      (add-to-list 'ac-dictionary-directories "c:/src/itasca-emacs/ac-dict")
      (add-to-list 'ac-dictionary-directories "c:/src/dotfiles/ac-dict")
      (setq eshell-rc-script "c:/src/dotfiles/eshellrc")

      (set-register ?e '(file . "c:/src/dotfiles/.emacs"))
      (set-register ?s '(file . "c:/src/"))

      (let ((file-name "C:/src/Blo-Up/interpreter/sign.el"))
        (when (file-exists-p file-name)
          (load file-name)))
      (let ((file-name "C:/src/svn_bu/interpreter/sign.el"))
        (when (file-exists-p file-name)
          (load file-name)))

      (add-to-list 'load-path "C:/src/itasca-emacs")
      (require 'itasca)
      (progn
        (add-to-list 'ac-modes 'itasca-general-mode)
        (add-to-list 'ac-modes 'itasca-pfc-mode)
        (add-to-list 'ac-modes 'itasca-pfc5-mode)
        (add-to-list 'ac-modes 'itasca-flac-mode)
        (add-to-list 'ac-modes 'itasca-flac3d-mode)
        (add-to-list 'ac-modes 'itasca-3dec-mode)
        (add-to-list 'ac-modes 'itasca-udec-mode))
                                        ; windows specific magit init
      (defun magit-escape-for-shell (str)
        (if (or (string= str "git")
                (string-match "^--" str))
            str
          (concat "'" (replace-regexp-in-string "'" "'\\''" str) "'")))
      (custom-set-variables
       '(magit-git-executable "C:\\Program Files\\Git\\bin\\git"))

      ;; windows specific font stuff
      (setq w32-get-true-file-attributes nil)
      (set-default-font
       "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
      (set-face-attribute 'default nil :height 140)))

;;;; computer specific setup

(pcase system-name

  ("ABITA" ; 6 core i7
   (let ((org-note-file
          "c:/Users/jfurtney/Dropbox/org/notes.org"))
     (setq initial-frame-alist '((width . 80) (height . 44)))
     (setq org-default-notes-file org-note-file)
     (setq org-agenda-files (list org-note-file))
     (progn
       (require 'ac-slime)
       (require 'slime-autoloads)
       (slime-setup '(slime-fancy slime-banner slime-autodoc))
       (add-hook 'slime-mode-hook 'set-up-slime-ac)
       (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
       (add-to-list 'ac-modes 'slime-repl-mode)
       (add-to-list 'ac-modes 'slime-mode))


     (set-register ?n `(file . ,org-note-file)))
 ;;;;; hack because we use Anaconda which does not have virtual env
   (setq jedi:server-command '("python" "c:/Users/jfurtney/AppData/Roaming/.emacs.d/elpa/jedi-20140321.1323/jediepcserver.py")))

  ("SHOTOVER"                            ; vaio
   (setq initial-frame-alist '((width . 80) (height . 37)))
   (set-face-attribute 'default nil :height 140)
   (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
   (require 'ac-slime)
   (require 'slime-autoloads)
   (slime-setup '(slime-fancy slime-banner slime-autodoc))
   (add-hook 'slime-mode-hook 'set-up-slime-ac)
   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
   (add-to-list 'ac-modes 'slime-repl-mode)
   (add-to-list 'ac-modes 'slime-mode)
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

  ("UNSER"        ; old work computer
   (setq initial-frame-alist '((width . 80) (height . 41)))
   (set-face-attribute 'default nil :height 140)
                                        ; org mode
   (setq org-mobile-directory "c:/Users/Itasca/Dropbox/Apps/MobileOrg")
   (setq org-directory "c:/Users/Itasca/Dropbox/org/")

   (setq org-mobile-inbox-for-pull "c:/Users/Itasca/Dropbox/org/flagged.org")

   (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
   (require 'ac-slime)
   (require 'slime-autoloads)
   (slime-setup '(slime-fancy slime-banner slime-autodoc))
   (add-hook 'slime-mode-hook 'set-up-slime-ac)
   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
   (add-to-list 'ac-modes 'slime-repl-mode)
   (add-to-list 'ac-modes 'slime-mode)

   (let ((org-note-file
          "c:/Users/Itasca/Dropbox/org/notes.org"))
     (setq org-default-notes-file org-note-file)
     (setq org-agenda-files (list org-note-file))
     (set-register ?n `(file . ,org-note-file)))
   (set-register ?d '(file . "c:/Users/Itasca/downloads")))

                                        ; vaio Ubuntu virtual machine
  ("u64"
   (setq initial-frame-alist '((width . 80) (height . 40)))
   (setq inferior-lisp-program "ecl")
   (require 'ac-slime)
   (require 'slime-autoloads)
   (slime-setup '(slime-fancy slime-banner slime-autodoc))
   (add-hook 'slime-mode-hook 'set-up-slime-ac)
   (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
   (add-to-list 'ac-modes 'slime-repl-mode)
   (add-to-list 'ac-modes 'slime-mode))

  ("uvb64" ; work virtual machine
   (set-face-attribute 'default nil :height 140)
   (require 'slime-autoloads)
   (slime-setup '(slime-fancy slime-banner slime-autodoc))
   (setq inferior-lisp-program "sbcl"))

  ("jason-furtneys-imac.local"
   (setq initial-frame-alist '((width . 80) (height . 52))))

  ("LAKEMAIDEN" ; build server
   (setq initial-frame-alist '((width . 80) (height . 28)))
   (set-face-attribute 'default nil :height 140)
   (let ((org-note-file
          "c:\\Users\\Jason\\Documents\\My Dropbox\\org\\notes.org"))
     (setq org-default-notes-file org-note-file)
     (setq org-agenda-files (list org-note-file))
     (progn
       (require 'ac-slime)
       (require 'slime-autoloads)
       (slime-setup '(slime-fancy slime-banner slime-autodoc))
       (add-hook 'slime-mode-hook 'set-up-slime-ac)
       (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
       (add-to-list 'ac-modes 'slime-repl-mode)
       (add-to-list 'ac-modes 'slime-mode))
     (set-register ?n `(file . ,org-note-file))))

  (_ (setq initial-frame-alist '((width . 80) (height . 34)))))

 ;; note on windows $HOME is different in bash and emacs!
 ;; cp ~/.gitconfig ~/AppData/Roaming/
 ;; to get magit to recognize user.name and user.email

(defun jkf/a2ps-buffer ()
  "call a2ps on the file the current buffer is visiting. Opens
the resulting postscript file"
  (interactive)
  (let ((template  "a2ps --columns=2 -o %s.ps -M letter --portrait %s")
        (fn (buffer-file-name)))
    (shell-command (format template fn fn ))
    ))

(defun jkf/a2ps-buffer-legal ()
  "call a2ps on the file the current buffer is visiting. Opens
the resulting postscript file"
  (interactive)
  (let ((template  "a2ps --columns=2 -o %s.ps -M legal --portrait %s")
        (fn (buffer-file-name)))
    (shell-command (format template fn fn ))
    ))


(defun jkf/a2ps-file () (interactive)
  "in dired call this function on a selected file to process the
file with a2ps"
  (let ((template  "a2ps.exe --columns=2 -o %s.ps -M letter --portrait %s")
        (fn (dired-get-filename)))
    (shell-command (format template fn fn ))))

;(require 'smart-operator)
;(require 'expand-region)
;(global-set-key (kbd "C-=") 'er/expand-region)

(require 'magit)
;(require 'magit-svn)
(global-set-key (kbd "C-c s") 'magit-status)

(require 'ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
; (ido-vertical-mode t)
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

(defun jkf/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun jkf/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(define-key global-map (kbd "C-S-n") 'jkf/move-line-down)
(define-key global-map (kbd "C-S-p") 'jkf/move-line-up)

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
(diminish 'pair-jump-mode " pj")
(diminish 'yas-minor-mode)
;(diminish 'smart-operator-mode)
(diminish 'eldoc-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-fill-function)
(diminish 'abbrev-mode)


(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(defun jkf/copy-run-buffer-filename-as-kill ()
  "Insert the string: '%run file-name' to the clipboard where
file-name is the full path and filename of the current buffer.
Useful when editing a datafile in emacs and loading it IPython."
  (interactive)
  (let ((s (format "%%run %s" (buffer-file-name))))
    (kill-new s)
    (message "Copied: %s to clipboard" s)))

(defun jkf/copy-load-buffer-filename-as-kill ()
  "Insert the string: '%run file-name' to the clipboard where
file-name is the full path and filename of the current buffer.
Useful when editing a datafile in emacs and loading it a lisp."
  (interactive)
  (let ((s (format "(load \"%s\")" (buffer-file-name))))
    (kill-new s)
    (message "Copied: %s to clipboard" s)))

; from http://www.emacswiki.org/emacs/buffer-extension.el
(defun jkf/copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (f) Full, (d) Directory, (n) Name")
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
(global-set-key (kbd "C-c M-c") 'jkf/copy-buffer-file-name-as-kill)

;(global-set-key (kbd "C-c j c") 'goto-last-change)
(global-set-key (kbd "C-c j b") 'beginning-of-buffer)
(global-set-key (kbd "C-c j e") 'end-of-buffer)
(global-set-key (kbd "C-c j f") 'idomenu)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(global-set-key (kbd "M-x") 'smex)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

; on windows the magit process hangs constantly
(defun jkf/kill-magit ()
  "Kill the Magit process buffer"
  (interactive)
  (delete-process "*magit-process*"))

(defun jkf/=-transpose ()
  "Transpose the text before and after the first equals sign"
  (interactive)
  (flet ((chomp (str)
                (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
                  (setq str (replace-match "" t t str)))
                str))
    (let ((lhs (buffer-substring (point-at-bol) (point)))
          (rhs (buffer-substring (1+ (point)) (point-at-eol))))
      (end-of-line)
      (newline-and-indent)
      (insert (format "%s = %s" (chomp rhs) (chomp lhs))))))

(load-theme 'cyberpunk t)
(load-theme 'cyberpunk t) ; for some reason this needs to be called 2x?

(defun jkf/calc-eval-and-insert (&optional start end)
  (interactive "r")
  (let ((result (calc-eval (buffer-substring-no-properties start end))))
    (goto-char (point-at-eol))
    (insert " = " result)))

(defun jkf/calc-eval-line-and-insert ()
  (interactive)
  (jkf/calc-eval-and-insert (point-at-bol) (point-at-eol)))

(global-set-key (kbd "C-c =") 'jkf/calc-eval-line-and-insert)

(require 'smartparens-config)
(smartparens-global-mode t)

(global-set-key (kbd "C-}") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-{") 'sp-backward-slurp-sexp)
(global-set-key (kbd "M-}") 'sp-forward-barf-sexp)
(global-set-key (kbd "M-{") 'sp-backward-barf-sexp)

(global-set-key (kbd "<backspace>") 'sp-backward-delete-char)
(global-set-key (kbd "<delete>") 'sp-delete-char)
(global-set-key (kbd "C-c <delete>") 'delete-char)
(global-set-key (kbd "C-c <backspace>") 'backward-delete-char)

(setq sp-autoinsert-if-followed-by-same 3) ; this is the default
(setq sp-autoinsert-if-followed-by-word t)
(sp-local-pair 'org-mode "$" "$")
(sp-local-pair 'rst-mode "`" "`")
(sp-local-pair 'rst-mode ":" ":")
(sp-local-pair 'lisp-mode "#|" "|#") ; does not work with slurp/barf?
(setq sp-autoescape-string-quote nil) ;; fix for using pair jump mode

(defun jkf/comment-sexp ()
  "wrap a common lisp sexp in #| |# style comments"
  (interactive)
  (insert "#|")
  (sp-forward-sexp)
  (insert "|#"))

(defun jkf/copy-current-defun ()
  "copy current defun to kill ring"
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (mark-sexp)
    (kill-ring-save (region-beginning) (region-end))))

(defun jkf/wrap-sexp ()
  "insert a pair of parenthesis and forward slurp the next sexp. Remove any space."
  (interactive)
  (insert "()")
  (backward-char 1)
  (sp-forward-slurp-sexp)
  (if (looking-at " ") (delete-char 1)))
(global-set-key (kbd "M-(") 'jkf/wrap-sexp)

(defadvice sp-forward-slurp-sexp (after jkf/slurp-remove-whitespace)
  "Removes the whitespace inserted after a sp-forward-slurp-sexp"
  (if (looking-at " ") (delete-char 1)))
(ad-activate 'sp-forward-slurp-sexp)



(defun jkf/insert-basename ()
  "insert the string of the buffername without extension"
  (interactive)
  (insert "\"")
  (insert (file-name-sans-extension (buffer-name)))
  (insert "\""))

(defun jkf/line-incriment ()
  "look at the current line, find the first numerical part,
incriment it and write on a new line below. Leave the origional inplace"
  (interactive)
  (save-match-data
    (let* ((current-line  (buffer-substring
                           (point-at-bol)
                           (point-at-eol)))
           (tmp (string-match "\\([0-9]+\\)"current-line))
           (old-number (match-string 1 current-line))
           (new-number (number-to-string (1+ (string-to-int old-number))))
           (new-line (replace-regexp-in-string old-number
                                               new-number current-line)))
      (move-end-of-line nil)
      (newline-and-indent )
      (insert new-line))))

(defun jkf/open-next ()
  "try to incriment current filename and open"
  (interactive)
  (save-match-data
    (let* ((old-casename (file-name-sans-extension (buffer-name)))
           (tmp (string-match "\\([0-9]+\\)" old-casename))
           (old-number (match-string 1 old-casename))
           (new-number
            (number-to-string (1+ (string-to-int old-number))))
           (new-filename
            (replace-regexp-in-string old-number new-number (buffer-name))))
      (find-file new-filename))))

(defun jkf/increment-buffer ()
  "Write the current buffer with an incremented filename. The
  (first) integer portion in the filename is incremented to
  create the new filename. Also queries to call replace the old
  base filename with the new base filename in the new buffer.

  e.g. case3.dat becomes case4.dat and the string case3 is
  replaced with case4"
  (interactive)
  (save-match-data
    (let* ((old-casename (file-name-sans-extension (buffer-name)))
           (tmp (string-match "\\([0-9]+\\)" old-casename))
           (old-number (match-string 1 old-casename))
           (new-number
            (read-from-minibuffer "new case number: "
                                  (number-to-string (1+ (string-to-int old-number)))))
           (new-casename
            (replace-regexp-in-string old-number new-number old-casename))
           (new-filename
            (replace-regexp-in-string old-casename new-casename (buffer-name))))
      (write-file new-filename 1)
      (replace-string old-casename new-casename nil (point-min) (point-max)))))


(require 'fold-dwim)
(global-set-key (kbd "C-c f") 'fold-dwim-toggle)
(global-set-key (kbd "C-c C-f") 'fold-dwim-toggle)
(global-set-key (kbd "C-c M-f") 'fold-dwim-hide-all)
(global-set-key (kbd "C-c M-F") 'fold-dwim-show-all)

; search for a defun, copy the name of it, wrap it in the timing macro
(fset 'jkf/wtdef
   [?\C-i ?d ?e ?f ?u ?n ?\C-f ?\C-\M-  ?\M-w ?\C-a ?\( ?w ?i ?t ?h ?- ?t ?i ?m ?i ?n ?g ?  ?\" ?\C-y ?\C-f ?  ?\C-\} return ?\C-\M-e ?\C-a])

(defun jkf/extract-bu-ac () (interactive)
  "paste blo-up lisp documentation dump in a buffer and call this
function to make an autocomplete list"
  (keep-lines "^\.\. func.*")
  (replace-regexp "(.*$" "")
  (beginning-of-buffer)
  (replace-regexp ".. function:: " ""))

(defun jkf/setup-slime ()
     (interactive)
     (rainbow-delimiters-mode 1)
     ;(pair-jump-mode 1)
     (define-key slime-mode-map (kbd "M-n") nil)
     (define-key slime-mode-map (kbd "M-p") nil)
     (define-key slime-mode-map (kbd "C-M-.") nil)
     (diminish 'slime-mode " SL"))

(add-hook 'slime-mode-hook 'jkf/setup-slime)

(defun jkf/lisp-code-to-c-comment (start end)
  (interactive "r")
  (save-excursion
    (replace-string "\"" "\\\"" nil start end)
    (replace-regexp "^" "\"  " nil start end)
    (replace-regexp "$" "  \\\\n\"" nil start end)))

(defadvice slime-compilation-finished (after jkf/post-slime-compile-defun (result))
  "open the intermediate C file generated by ECL when compiling a defun."
  (jkf/show-ecl-defun-c-code))
(ad-activate 'slime-compilation-finished)

(defun jkf--sort-file-modification-times (a b)
  (let* ((a-time (fifth (file-attributes a)))
         (b-time (fifth (file-attributes b)))
         (a-0 (first a-time))
         (a-1 (second a-time))
         (a-2 (third a-time))
         (b-0 (first b-time))
         (b-1 (second b-time))
         (b-2 (third b-time)))
    (if (> a-0 b-0)
        t
      (if (> a-1 b-1)
          t
        (if (> a-2 b-2)
            t)
        nil))))

(defun jkf--find-last-modified-temporary-c-file ()
  (interactive)
  (first (sort
          (directory-files temporary-file-directory t "\\.c$")
          'jkf--sort-file-modification-times)))

(defun jkf/show-ecl-defun-c-code ()
  (interactive)
  (find-file
   (jkf--find-last-modified-temporary-c-file)))

(defun jkf/toggle-slime ()
  (interactive)
  (if (slime-connected-p)
      (slime-disconnect)
    (slime-connect "127.0.0.1" 4005)))
(global-set-key (kbd "C-c l") 'jkf/toggle-slime)

(global-set-key (kbd "C-c L") 'jkf/launch-blo-up-swank)
(global-set-key (kbd "C-c M-l") 'jkf/launch-blo-up)


;;; Blo-Up SLIME Emacs integration.
;;;
;;; Starting Blo-Up with -test -swank -script <location of ecl-boot
;;; code> runs the ecl boot code which initializes the swank server,
;;; Emacs/SLIME then trys to connect to the swank server running inside Blo-Up.
;;
;; For this to work SLIME has to be installed in Emacs.

;(setf blo-up-exe-name "c:/src/svn_bu/binaries/x64Release/bloup206_64.exe")
(setf blo-up-exe-name "c:/Program Files/HSBM/Blo-Up_2.7/exe64/bloup206_64.exe")
(setf blo-up-swank-location "c:/src/dotfiles/ecl-swank.lisp")

;;; This code finds the slime installation directory and sets it to an
;;; environmental variable the child process can read.
(setenv "BLOUP_SWANK"
 (concat (file-name-directory (buffer-file-name (car (find-definition-noselect 'slime-eval-buffer nil)))) "swank-loader.lisp"))

(defun jkf/launch-blo-up-swank ()
  (interactive)
  (start-process "Blo-Up" "bub"
                 blo-up-exe-name
                 "-test"
                 "-swank"
                 "-script"
                 blo-up-swank-location)
   ; need to poll here with idle timer
  (sleep-for 2)
  (jkf/toggle-slime))

(defun jkf/launch-blo-up ()
  (interactive)
  (start-process "Blo-Up" "bub"
                 blo-up-exe-name
                 "-test"
                 "-script"
                 blo-up-swank-location))

;(add-hook 'slime-repl-mode-hook 'pair-jump-mode)

(defun jkf/increment-number-at-point ()
  "incriment integer at point"
  (interactive)
  (save-match-data
   (save-excursion
     (skip-chars-backward "0123456789")
     (or (looking-at "[0123456789]+")
         (error "no number at point"))
     (replace-match (number-to-string
                     (1+ (string-to-number (match-string 0))))))))
(global-set-key (kbd "C-c +") 'jkf/increment-number-at-point)

(defun jkf/eval-replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

; column-enforce-mode
(require 'ox-reveal)
(require 'ob-python)

(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(setq org-confirm-babel-evaluate nil)

(defun jkf/wrap-code-org ()
  "wrap the current word in =word="
  (interactive)
  (insert "=")
  (forward-word)
  (insert "="))

(define-key rst-mode-map (kbd "C-c C-c") 'rst-adjust)
(global-set-key (kbd "C-x r v") 'helm-register)


(require 'god-mode)
(diminish 'god-local-mode " g")
(global-set-key (kbd "<home>") 'god-mode-all)
(global-set-key (kbd "<insert>") 'god-mode-all)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'hbox
                      'box)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd "C-<tab>") 'god-local-mode)
(global-set-key (kbd "C-<tab>") 'god-local-mode)

(setq c-default-style "linux"
          c-basic-offset 4)
;(c++-set-offset 'substatement-open 0)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 2)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun jkf/svn-get-ids ()
  (interactive)
  (beginning-of-buffer)
  (keep-lines "^Revision: ")
  (replace-regexp "^Revision: " ""))

(setq org-latex-table-scientific-notation "%s\\times10^{%s}")

(define-key python-mode-map (kbd "C-c M-c") 'itasca-python-copy-as-execfile)

(defun jkf/unix-file ()
      "Change the current buffer to Latin 1 with Unix line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun jkf/dos-file ()
      "Change the current buffer to Latin 1 with DOS line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-dos t))

; http://en.wikipedia.org/wiki/Spaced_repetition
; http://en.wikipedia.org/wiki/Leitner_system

(defvar jkf/atest-data nil "store results of training")

(defun jkf/atest ()
  (interactive)
  "mental arithmetic trainer"
  (setq jkf/atest-data nil)
  (unwind-protect
      (progn
        (let (io op op-name range)
          (if (equal "voice" (ido-completing-read
                              "text or voice:" '("text" "voice")))
              (fset 'io (function speech-read-from-minibuffer))
            (fset 'io (function read-from-minibuffer)))
          (if (equal "addition" (ido-completing-read
                                 "operator: "
                                 '("addition" "multiplication")))
              (progn
                (fset 'op (function +))
                (setq op-name "plus")
                (fset 'rand (lambda () (random 100))))
            (progn
              (fset 'op (function *) )
              (setq op-name "times")
              (fset 'rand (lambda () (+ 2 (random 11))))))

          (loop
           (let ((t0 (float-time)) solve-time)
             (let* ((n1 (rand))
                    (n2 (rand))
                    (res (op n1 n2))
                    (problem (format "%d %s %d " n1 op-name n2))
                    (trial (string-to-int (io problem))))
               (while (not  (= trial res))
                 (setq trial (string-to-int
                              (io (format "no: %s " problem)))))

               (setq solve-time (truncate (* 1e3 (- (float-time) t0))))

               (with-current-buffer (get-buffer-create "*atest*")
                 (insert
                  (format "%s: %d ms \n" problem solve-time)))

               (push (cons solve-time problem) jkf/atest-data)
               (io "yes"))))))

    ;; clean up
    (progn
      (message "saving data...")
      (with-temp-file
          (concat dotfile-dir
                  (replace-regexp-in-string ":" "_" (replace-regexp-in-string " " "_" (current-time-string))) ".atest")
        (dolist (datum jkf/atest-data)
          (insert (format "%d, %s\n" (car datum) (cdr datum))))))))

;; text to speech pacakage. requires python, pyttsx and speak.py
(defvar tts nil "text to speech process")

(defun tts-up ()
  "true if the tts process us up"
  (interactive)
  (and (not (null tts))
       (eq (process-status tts) 'run)))

(defun tts-start ()
  "start the tts process if it is not already up"
  (interactive)
  (if (not (tts-up))
      (setq tts
            (start-process "tts-python"
                           "*tts-python*"
                           "python" (concat dotfile-dir "speak.py")))))

(defun tts-end ()
  "close the tts process."
  (interactive)
  (delete-process tts)
  (setq tts nil))

(defun tts-say (text)
  "speak the given string."
  (interactive)
  (tts-start)
  (process-send-string tts (concat text "\n")))

(defun speech-read-from-minibuffer (text)
  "speak the given string and read from the minibuffer"
  (interactive)
  "say the message and read from the minibuffer"
  (tts-say text)
  (read-from-minibuffer "<speech>"))

(defun jkf/tuple-pack-refactor ()
  "a,b = c,d into a=c newline b=d "
  (interactive)
  (save-excursion
    (flet ((chomp (str)
                  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
                    (setq str (replace-match "" t t str)))
                  str))
      (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
                ;;; find number of leading whitespace chars
             (nindent (loop for c in (string-to-list line)
                           sum (if (char-equal c ?\ ) 1 0) into count do
                           (when (not (char-equal c ?\ )) (return count))))
             (lhs-rhs (split-string line "=" t))
             (lhs (split-string (first lhs-rhs) "," t))
             (rhs (split-string (second lhs-rhs) "," t)))
        (move-beginning-of-line nil)
        (kill-line)
        (loop for l in lhs for r in rhs do
              (dotimes (n nindent) (insert " "))
              (insert (format "%s = %s\n" (chomp l) (chomp r))))
        (backward-delete-char 1)))))
(require 'ox-latex)

(setq ido-file-extension-order '(".py" ".dat" ".f3dat" ".lisp"))
(global-set-key (kbd "C-c c") 'calc)

;C:\Program Files (x86)\Git\bin\bash.exe
(global-set-key (kbd "C-c n") 'jkf/open-temp-file)

(defun jkf/open-temp-file ()
  "opens a new temporary file in c:\src "
  (interactive)
  (let* ((base "c:/src/tmp_%d.txt")
        (i 0)
        (name (format base i)))
    (while (file-exists-p name)
      (incf i)
      (setf name (format base i)))
    (find-file name)))
