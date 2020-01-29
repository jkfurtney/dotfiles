(defvar dotfile-dir nil "location of .emacs and other stuff")
(defvar jkf/src-dir nil "location of src folder")
(defvar jkf/dropbox-dir nil "location of dropbox")
(if (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (setq dotfile-dir (expand-file-name "~/src/dotfiles/"))
  (setq dotfile-dir "c:/src/dotfiles/"))
(add-to-list 'load-path dotfile-dir)

(require 'pair-jump-mode)
(pair-jump-mode 1)

(defmacro disable (&rest body))

(setq-default inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq magit-last-seen-setup-instructions "2.4.0")

;;;; packages
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ))

(require 'package)
(package-initialize)

(defvar my-packages '(ace-jump-mode   auto-complete helm helm-descbinds  macrostep markdown-mode magit smartparens popup dash request s slime uuid websocket yasnippet rainbow-delimiters diminish elisp-slime-nav multiple-cursors ac-slime jedi cyberpunk-theme fold-dwim htmlize  connection  cython-mode nsis-mode w32-browser guide-key powerline itasca nyan-mode swift-mode js2-mode jinja2-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


; for new installs
(disable (progn
           (package-install "ace-jump-mode")
           (dolist (p my-packages)
             (when (not (package-installed-p p))
               (package-install p)))))

(defun jkf/get-package-dir (pname)
  (interactive)
  "return the directory in which the package pname is installed."
  (if (package-installed-p pname)
      (file-name-as-directory
       (package-desc-dir (cadr (assq pname package-alist))))
    (error "package not installed")))

                                        ; install org and org-plus-extras from here:
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
                                        ; then install ox-reveal


(defun jkf/replace-regexp (from to)
  "like replace-string but for calling from lisp."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while
        (search-forward-regexp from nil 'noerror)
      (replace-match to))))

(require 'smartparens-config)
(smartparens-global-mode t)
(diminish 'smartparens-mode)
(diminish 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el")))


;;;; basic key bindings
(require 'python)
                                        ;(require 'dired+)

(global-set-key "\C-o" 'helm-find-files)
(add-hook 'dired-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "<backspace>") 'kill-this-buffer)
                      (local-unset-key (kbd "<f1>"))
                      (local-unset-key (kbd "C-o")))))

(add-hook 'swift-mode-hook
          (function (lambda ()
                      (local-unset-key (kbd "M-j")))))



(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-x M-q") 'jkf/remove-hard-wrap)

(defmacro jkf/func-ff (filename) `(lambda () (interactive)
                                           (find-file ,filename)))

;;;; C-c bindings
(global-set-key (kbd "C-c c") 'calc)
(global-set-key (kbd "C-c j") 'jkf/journal)
(global-set-key (kbd "C-c n") 'jkf/open-temp-file)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c =") 'jkf/calc-eval-line-and-insert)
(global-set-key (kbd "C-c l") 'jkf/toggle-slime)
(global-set-key (kbd "C-c L") 'jkf/launch-blo-up-swank)
(global-set-key (kbd "C-c k") 'jkf/kill-all-buffers)
(global-set-key (kbd "C-c K") 'jkf/kill-other-buffers)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'revert-buffer)
(global-set-key (kbd "C-c t") (jkf/func-ff (concat jkf/dropbox-dir "/org/todo.txt")))
(global-set-key (kbd "C-c p") 'jkf/proselint-buffer)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c +") 'jkf/increment-number-at-point)


;; windows specific interaction
(global-set-key (kbd "C-c w e") 'w32explore)
(global-set-key (kbd "C-c w b") 'jkf/open-bash-here)
(global-set-key (kbd "C-c w w") 'helm-w32-launcher)


;; org-mode C-c bindings
(defun jkf/open-notes () (interactive)
       (find-file (concat jkf/dropbox-dir "/org/notes.org"))
                  ;(find-file jkf/notes-file)
       (org-overview))
(global-set-key (kbd "C-c o o") 'org-capture)
(global-set-key (kbd "C-c o n") 'jkf/open-notes)
(global-set-key (kbd "C-c o i") 'helm-org-agenda-files-headings)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)

(define-key python-mode-map (kbd "C-c d") 'jedi:show-doc)

(global-set-key (kbd "C-c f") 'fold-dwim-toggle)
(global-set-key (kbd "C-c M-f") 'fold-dwim-hide-all)
(global-set-key (kbd "C-c M-F") 'fold-dwim-show-all)

(global-set-key (kbd "C-c M-l") 'jkf/launch-blo-up)
(global-set-key (kbd "C-c M-c") 'jkf/copy-buffer-file-name-as-kill)

; emacs lisp specific
(defun jkf/to-init () (interactive)
       "open .emacs file and jump to the end of the file."
       (find-file (concat jkf/src-dir "dotfiles/.emacs"))
       (goto-char (point-max)))

(global-set-key (kbd "C-c e m") 'macrostep-expand)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e s") 'jkf/switch-to-scratch)
(global-set-key (kbd "C-c e q") 'jkf/eval-replace-last-sexp)
(global-set-key (kbd "C-c e c") 'jkf/to-init)

; C-i for search forward
(define-key input-decode-map (kbd "C-i") (kbd "H-i")); hack needed to unset tab
(global-set-key (kbd "H-i") 'isearch-forward)
(define-key isearch-mode-map (kbd "H-i") 'isearch-repeat-forward)

(global-set-key (kbd "M-s") 'ispell-word)
(global-set-key (kbd "M-s") 'flyspell-correct-word-before-point)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "<f1>") 'kill-this-buffer)
(global-set-key (kbd "<f12>") 'other-window)
;(global-set-key (kbd "<apps> /") 'ido-switch-buffer)
(global-set-key (kbd "<apps> /") 'helm-buffers-list)
;(global-set-key (kbd "M-<apps>") 'ido-switch-buffer)
(global-set-key (kbd "M-<apps>") 'helm-buffers-list)
(global-set-key (kbd "C-<apps>") 'other-window)
;(global-set-key (kbd "C-<lwindow>") 'smex)
(global-set-key (kbd "M-<lwindow>") 'other-window)
;(global-set-key (kbd "<apps> .") 'smex)
;(global-set-key (kbd "C-S-x") 'ido-switch-buffer)
(global-set-key (kbd "M-k") ; kill the entire line
                '(lambda () (interactive)
                  (move-beginning-of-line nil)
                  (kill-line)))
(global-set-key (kbd  "C-z") '(lambda () (interactive) nil))

(define-key help-mode-map (kbd "<backspace>") 'help-go-back)
(define-key help-mode-map (kbd "M-<backspace>") 'help-go-forward)

; Unset problematic keys
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


;; (set-background-color "black")
;; (set-face-background 'default "black")
;; (set-face-background 'region "black")
;; (set-face-foreground 'default "white")
;; (set-face-foreground 'region "gray60")
;; (set-foreground-color "white")
;; (set-cursor-color "red")


(setq calendar-latitude 44.954109)
(setq calendar-longitude -93.187408)
(setq calendar-location-name "Minneapolis/St. Paul")
(setq calendar-week-start-day 1)

(defun jkf/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (insert ";; scratch buffer")
  (newline))

(defun jkf/dont-kill-emacs ()
 (interactive)
 (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
(global-set-key (kbd "C-x C-c") 'jkf/dont-kill-emacs)

(defun jkf/remove-hard-wrap ()
  "Make several lines into a single long line."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))


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

(column-number-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq make-backup-files nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(require 'rst)
(add-hook 'rst-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;;;; Sphinx reStructuredText Setup
(disable
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

 (define-key rst-mode-map (kbd "C-c p") 'jkf/sphinx-open-pdf-windows)
 (define-key rst-mode-map (kbd "C-c C") 'jkf/sphinx-html-compile)
 (define-key rst-mode-map (kbd "C-c c") 'jkf/sphinx-pdf-compile)
 (define-key rst-mode-map (kbd "C-c m") 'jkf/rest-wrap-math))

;;;; FORTRAN Setup
(add-to-list 'auto-mode-alist '("\\.inc\\'" . fortran-mode))

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

(fset 'jkf/move-comment-above
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

(defvar jkf/python-build-dir nil)
(defun jkf/python-extension-compile ()
  "build python extension module. First call prompts for a directory"
  (interactive)
  (unless jkf/python-build-dir
    (setq jkf/python-build-dir (read-directory-name "python build dir ")))
  (let ((default-directory jkf/python-build-dir)
        (extra-arg (if (eq  system-type 'windows-nt)
                       " " " --user")))
    (compile (concat "python setup.py install" extra-arg))))

(define-key python-mode-map (kbd "C-c M-c")
  'copy-run-buffer-filename-as-kill)
;(add-hook 'python-mode-hook 'jedi:setup)
;(remove-hook 'python-mode-hook 'jedi-setup)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)


(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook (function (lambda ()
                                        (setq python-indent-offset 4))))

(add-hook 'jedi-mode-hook (function (lambda ()
                                      (define-key jedi-mode-map (kbd "C-c r") nil))))


;;;; Lisp Setup
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(setq edebug-trace nil)

(require 'ac-slime)
(require 'slime-autoloads)
(defun jkf/setup-slime ()
  (interactive)
  (slime-setup '(slime-fancy slime-banner slime-autodoc))
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (add-to-list 'ac-modes 'slime-repl-mode)
  (add-to-list 'ac-modes 'slime-mode))
(jkf/setup-slime)

;; .emacs section navigation by ;;;; Section name
(defun jkf/imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'jkf/imenu-elisp-sections)

;;;; Org-mode Setup
;(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)
(setq org-imenu-depth 3)
(disable
 (require 'org-tree-slide)
 (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
 (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
 (setq org-tree-slide-slide-in-effect nil))

(require 'org)
(setq org-cycle-separator-lines 0)
(define-key org-mode-map (kbd "C-M-k") 'kill-sentence)
(define-key org-mode-map (kbd "C-c <up>") 'org-move-subtree-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-move-subtree-down)
(setq org-src-fontify-natively t)
;(setq org-startup-truncated nil)

                  ; Show full paths for refiling
(defun jkf/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun jkf/compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    (cons msg code))
;(setq compilation-exit-message-function 'jkf/compilation-exit-autoclose)
;(setq compilation-exit-message-function nil)

(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-complete-config)
(ac-config-default)

(defun jkf/setup-itasca ()
  (interactive)
  (require 'ert)
  (require 'itasca)
  (let* ((itasca-pkg-dir (jkf/get-package-dir 'itasca))
         (itasca-snippets (concat itasca-pkg-dir "snippets"))
         (itasca-ac (concat itasca-pkg-dir "ac-dict")))
    (add-to-list 'yas/snippet-dirs itasca-snippets)
    (add-to-list 'ac-dictionary-directories itasca-ac))

  (add-to-list 'ac-modes 'itasca-general-mode)
  (add-to-list 'ac-modes 'itasca-pfc-mode)
  (add-to-list 'ac-modes 'itasca-pfc5-mode)
  (add-to-list 'ac-modes 'itasca-flac-mode)
  (add-to-list 'ac-modes 'itasca-flac3d-mode)
  (add-to-list 'ac-modes 'itasca-udec-mode)
  (add-to-list 'ac-modes 'itasca-3dec-mode))
(jkf/setup-itasca)

;;;; Linux specific setup
(if  (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (progn
      (setq jkf/src-dir "~/src/")
      (setq jkf/dropbox-dir "~/Dropbox")
      (setq x-select-enable-clipboard t)
      (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
      (setq jkf/dropbox-dir "~/Dropbox")
      (global-unset-key (kbd "<menu>"))
      (global-set-key (kbd "M-<menu>") 'helm-buffers-list)
      (global-set-key (kbd "<menu> /") 'helm-M-x)

      ; this is OK when you remap menu....
      ;(global-set-key (kbd "s-/") 'ido-switch-buffer)
      ;(global-set-key (kbd "s-.") 'smex)

      (add-to-list 'yas/snippet-dirs "~/src/dotfiles/snippets")
      (setq eshell-rc-script "~/src/dotfiles/eshellrc")))

;;;; OS X specific setup
(if (eq system-type 'darwin)
    (progn
      (setq jkf/src-dir "~/src/")
      (setq jkf/dropbox-dir "~/Dropbox")
      (global-set-key (kbd "<M-268632080>") 'helm-buffers-list)
      (set-face-attribute 'default nil :family "Monaco"
                          :height 145 :weight 'normal)
      (setq eshell-rc-script "~/src/dotfiles/eshellrc_osx")
      (setq x-select-enable-clipboard t)
      (add-to-list 'exec-path "/opt/local/bin/")))


(require 'json)
(defun jkf/windows-get-dropbox-folder ()
 (let* ((dropbox-file
         (if (file-exists-p
              (concat (getenv "LOCALAPPDATA") "/Dropbox/info.json"))
             (concat (getenv "LOCALAPPDATA") "/Dropbox/info.json")
           (concat (getenv "APPDATA") "/Dropbox/info.json")))
        (data (json-read-file dropbox-file)))
   (cdr (assoc 'path (cdr (assoc 'personal data))))))

;;;; windows specific setup
(if  (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (progn
      (require 'w32-win)
      (setq jkf/src-dir "c:/src/")
      (setq jkf/dropbox-dir (jkf/windows-get-dropbox-folder))

;;;; Jedi setup
      ;;; hack because we use Anaconda which does not have virtual env
      (setq jedi:server-command
            `("python"
              ,(concat (jkf/get-package-dir 'jedi-core)
                      "jediepcserver.py")))

      (if (file-exists-p "C:/Program Files (x86)/Git/bin/bash.exe")
          (setq explicit-shell-file-name
                "C:/Program Files (x86)/Git/bin/bash.exe")
        (setq explicit-shell-file-name
                "C:/Program Files/Git/bin/bash.exe"))
      (setq shell-file-name explicit-shell-file-name)

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

      ;; (setq ispell-program-name "aspell")
      ;; (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
      (add-to-list 'exec-path "C:/Program Files (x86)/GnuWin32/bin/")
      (add-to-list 'exec-path "c:/Program Files/Git/bin/")
      (add-to-list 'exec-path "c:/Program Files (x86)/Git/bin/")
      (add-to-list 'exec-path "C:/Program Files (x86)/ImageMagick-6.8.5-Q16/")
      (add-to-list 'exec-path "C:/Program Files (x86)/ImageMagick-6.8.5-Q16/")
      (add-to-list 'exec-path "C:/Program Files (x86)/MiKTeX 2.9/miktex/bin/")

      (defun jkf/add-to-path (path-str)
        (setenv "PATH" (concat (getenv "PATH")
                               ";" path-str)))

      (jkf/add-to-path "C:/Program Files (x86)/GnuWin32/bin/")
      (jkf/add-to-path "C:/Program Files (x86)/MiKTeX 2.9/miktex/bin/")

      (add-to-list 'yas/snippet-dirs "c:/src/dotfiles/snippets")
      (add-to-list 'ac-dictionary-directories "c:/src/dotfiles/ac-dict")
      (setq eshell-rc-script "c:/src/dotfiles/eshellrc")

                                        ; windows specific magit init
      (disable
       (defun magit-escape-for-shell (str)
         (if (or (string= str "git")
                 (string-match "^--" str))
             str
           (concat "'" (replace-regexp-in-string "'" "'\\''" str) "'")))
       (setq magit-git-executable "C:\\Program Files\\Git\\bin\\git"))

      ;; windows specific font stuff
      (setq w32-get-true-file-attributes nil)
      (set-frame-font
       "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
      (set-face-attribute 'default nil :height 140)))

(add-to-list 'yas/snippet-dirs (concat jkf/src-dir "dotfiles/snippets"))
(set-register ?e `(file . ,(concat jkf/src-dir "dotfiles/.emacs")))

(setq jkf/org-note-file (concat jkf/dropbox-dir "/org/notes.txt"))
(setq jkf/org-todo-file (concat jkf/dropbox-dir "/org/todo.txt"))
(setq jkf/journal-file (concat jkf/dropbox-dir "/org/journal.txt"))
(setq jkf/run-file (concat jkf/dropbox-dir "/org/run.org"))


(setq org-refile-targets `((nil :maxlevel . 3)
                           (,jkf/org-note-file :maxlevel . 3 )
                           (,jkf/org-todo-file :maxlevel . 5)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)

(setq org-default-notes-file jkf/org-note-file)
(setq org-agenda-files (list jkf/org-todo-file jkf/journal-file))
(set-register ?t `(file . ,jkf/org-todo-file))
(set-register ?n `(file . ,jkf/org-note-file))

(setq display-time-default-load-average nil)
;;;; computer specific setup
(pcase system-name
  ("ABITA" ; 6 core i7

   (display-time)
   (let ((org-note-file
          "c:/Users/jfurtney/Dropbox/org/notes.org"))
     (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git")
     (setq initial-frame-alist '((width . 80) (height . 44)))))

  ("SHOTOVER"                            ; vaio
   (setq initial-frame-alist '((width . 80) (height . 37)))
   (set-face-attribute 'default nil :height 140)
   ;(setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
   (display-time-mode 1))

  ("UNSER"        ; old work computer
   (setq initial-frame-alist '((width . 80) (height . 41)))
   (set-face-attribute 'default nil :height 140))

  ("FUNKY"
   (set-face-attribute 'default nil :height 100))


  ("u64" ; vaio Ubuntu virtual machine
   (setq initial-frame-alist '((width . 80) (height . 40))))

  ("uvb64" ; work virtual machine
   (set-face-attribute 'default nil :height 140)
   ;(setq inferior-lisp-program "sbcl")
   )

  ("jason-furtneys-imac.local"
   (setq initial-frame-alist '((width . 80) (height . 48))))

  ("LAKEMAIDEN" ; build server
   (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git")
   (setq initial-frame-alist '((width . 80) (height . 28)))
   (set-face-attribute 'default nil :height 140))

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

;(require 'magit)

(require 'ido)
;; (require 'ido-ubiquitous)
;; (require 'ido-vertical-mode)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-case-fold t)

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
;; (setq header-line-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))
;; (add-hook 'buffer-list-update-hook
;;        (lambda nil (setq header-line-format (abbreviate-file-name (buffer-file-name)))))


(progn
  (defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))
  (defun sl/make-header ()
    ""
    (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
           (sl/header (file-name-directory sl/full-header))
           (sl/drop-str "[...]"))
      (if (> (length sl/full-header)
             (window-body-width))
          (if (> (length sl/header)
                 (window-body-width))
              (progn
                (concat (with-face sl/drop-str
                                   :background "blue"
                                   :weight 'bold
                                   )
                        (with-face (substring sl/header
                                              (+ (- (length sl/header)
                                                    (window-body-width))
                                                 (length sl/drop-str))
                                              (length sl/header))
                                   ;; :background "red"
                                   :weight 'bold
                                   )))
            (concat (with-face sl/header
                               ;; :background "red"
                               :foreground "#8fb28f"
                               :weight 'bold
                               )))
        (concat (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "#8fb28f"
                           )
                (with-face (file-name-nondirectory buffer-file-name)
                           :weight 'bold
                           ;; :background "red"
                           )))))
  (defun sl/display-header ()
    (setq header-line-format
          '("" ;; invocation-name
            (:eval (if (buffer-file-name)
                       (sl/make-header)
                     "%b")))))
(add-hook 'buffer-list-update-hook
          'sl/display-header))

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
(helm-mode 1)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(setq helm-ff-newfile-prompt-p nil)

;;; see http://emacs.stackexchange.com/questions/3798/how-do-i-make-pressing-ret-in-helm-find-files-open-the-directory
(setq helm-boring-file-regexp-list '("\\.$" "\\.\\.$"))
(setf helm-ff-skip-boring-files t)

(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))
(advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-j") 'helm-maybe-exit-minibuffer)

(defun fu/helm-find-files-navigate-back (orig-fun &rest args)
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(global-set-key (kbd "C-S-o") 'helm-recentf)

(define-key global-map (kbd "<RET>") 'newline-and-indent)

; Dont prompt me if I try to kill a buffer with an active process
; via http://www.masteringemacs.org/
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;(which-function-mode 1)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(yas-reload-all)


(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(require 'elisp-slime-nav)
(require 'eldoc)
(require 'diminish)
(diminish 'elisp-slime-nav-mode)
(diminish 'pair-jump-mode)
(diminish 'yas-minor-mode)
(diminish 'eldoc-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-fill-function)
(diminish 'abbrev-mode)
(diminish 'helm-mode)
(diminish 'flyspell-mode)

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

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(global-set-key (kbd "M-x") 'helm-M-x)

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
  (cl-flet ((chomp (str)
                (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
                  (setq str (replace-match "" t t str)))
                str))
    (let ((lhs (buffer-substring (point-at-bol) (point)))
          (rhs (buffer-substring (1+ (point)) (point-at-eol))))
      (end-of-line)
      (newline-and-indent)
      (insert (format "%s = %s" (chomp rhs) (chomp lhs))))))

(load-theme 'cyberpunk t) ; for some reason this needs to be called 2x?

(defun jkf/calc-eval-and-insert (&optional start end)
  (interactive "r")
  (let ((result (calc-eval (buffer-substring-no-properties start end))))
    (goto-char (point-at-eol))
    (insert " = " result)))

(defun jkf/calc-eval-line-and-insert ()
  (interactive)
  (jkf/calc-eval-and-insert (point-at-bol) (point-at-eol)))

(global-set-key (kbd "C-}") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-{") 'sp-backward-slurp-sexp)
(global-set-key (kbd "M-}") 'sp-forward-barf-sexp)
(global-set-key (kbd "M-{") 'sp-backward-barf-sexp)

;(global-set-key (kbd "<backspace>") 'sp-backward-delete-char)
(global-set-key (kbd "<delete>") 'sp-delete-char)
(global-set-key (kbd "C-c <delete>") 'delete-char)
(global-set-key (kbd "C-c <backspace>") 'backward-delete-char)

(setq sp-autoinsert-if-followed-by-same 3) ; this is the default
(setq sp-autoinsert-if-followed-by-word t)
(sp-local-pair 'org-mode "$" "$")
(sp-local-pair 'rst-mode "`" "`")
(sp-local-pair 'rst-mode ":" ":")
(sp-local-pair 'lisp-mode "#|" "|#") ; does not work with slurp/barf?
;(setq sp-autoescape-string-quote nil) ;; fix for using pair jump mode

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
           (new-number (number-to-string (1+ (string-to-number old-number))))
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
            (number-to-string (1+ (string-to-number old-number))))
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
    (let* ((old-casename (file-name-sans-extension (buffer-file-name)))
           (sans-dir (file-name-nondirectory old-casename))
           (tmp (string-match "\\([0-9]+\\)" sans-dir))
           (old-number (match-string 1 sans-dir))
           (new-number
            (read-from-minibuffer "new case number: "
                                  (number-to-string
                                   (1+ (string-to-number old-number)))))
           (new-casename
            (replace-regexp-in-string old-number new-number sans-dir))
           (new-filename
            (replace-regexp-in-string old-casename new-casename (buffer-file-name))))
      (write-file new-filename 1)
      (jkf/replace-regexp sans-dir new-casename))))


(require 'fold-dwim)

(defun jkf/extract-bu-ac () (interactive)
  "paste Blo-Up lisp documentation dump in a buffer and call this
function to make an autocomplete list"
  (keep-lines "^\.\. func.*")
  (jkf/replace-regexp "(.*$" "")
  (goto-char (point-min))
  (jkf/replace-regexp ".. function:: " ""))

(defun jkf/setup-slime-live ()
     (interactive)
     (rainbow-delimiters-mode 1)
     (define-key slime-mode-map (kbd "M-n") nil)
     (define-key slime-mode-map (kbd "M-p") nil)
     (define-key slime-mode-map (kbd "C-M-.") nil)
     (diminish 'slime-mode " SL"))
(add-hook 'slime-mode-hook 'jkf/setup-slime-live)

(defun jkf/lisp-code-to-c-comment (start end)
  (interactive "r")
  (save-excursion
    (jkf/replace-regexp "\"" "\\\"")
    (jkf/replace-regexp "^" "\"  ")
    (jkf/replace-regexp "$" "  \\\\n\"")))

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
    (if (> a-0 b-0) t (if (> a-1 b-1) t (if (> a-2 b-2) t) nil))))

;; this was a hack used when debugging the ecl compiler
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


;;; Blo-Up SLIME Emacs integration.
;;;
;;; Starting Blo-Up with -test -swank -script <location of ecl-boot
;;; code> runs the ecl boot code which initializes the swank server,
;;; Emacs/SLIME then trys to connect to the swank server running inside Blo-Up.
;;
;; For this to work SLIME has to be installed in Emacs.

(setf blo-up-exe-name "c:/Program Files/HSBM/Blo-Up_2.7/exe64/bloup206_64.exe")
;(setf blo-up-exe-name "c:/src/bu_july17_x64Release/x64Release/bloup206_64.exe")
(setf blo-up-swank-location "c:/src/dotfiles/ecl-swank.lisp")

;;; This code finds the slime installation directory and sets it to an
;;; environmental variable the child process can read.
(setenv "BLOUP_SWANK"
        (concat (jkf/get-package-dir 'slime)
                "swank-loader.lisp"))
 ;;; (file-exists-p (getenv "BLOUP_SWANK"))

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

(defun jkf/eval-replace-last-sexp ()
  "Evaluate the previous sexp, remove it and insert the result into the buffer"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

;(require 'ox-reveal)
;(require 'ob-python)

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

;(require 'god-mode)
;(global-set-key (kbd "<home>") 'god-mode-all)
;(global-set-key (kbd "<insert>") 'god-mode-all)
;; (defun my-update-cursor ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                         'hbox
;;                       'box)))
;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)
;; (define-key god-local-mode-map (kbd "i") 'god-local-mode)
;; (define-key god-local-mode-map (kbd "C-<tab>") 'god-local-mode)
;; (global-set-key (kbd "C-<tab>") 'god-local-mode)

(setq c-default-style "linux"
          c-basic-offset 4)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 2)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

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
                    (trial (string-to-number (io problem))))
               (while (not  (= trial res))
                 (setq trial (string-to-number
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
                  (replace-regexp-in-string ":" "_"
                                            (replace-regexp-in-string
                                             " " "_" (current-time-string))) ".atest")
        (dolist (datum jkf/atest-data)
          (insert (format "%d, %s\n" (car datum) (cdr datum))))))))

;; text to speech pacakage. requires python, the pyttsx python module
;; and speak.py.
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
    (cl-flet ((chomp (str)
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

(defun jkf/clear-ispell-local-words ()
  (interactive)
  (setq ispell-buffer-session-localwords nil))


(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
                                 nsis-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
                                 nsis-mode)) auto-mode-alist))

(defun jkf/journal ()
  (interactive)
  (find-file jkf/journal-file)
  (goto-char (point-max)))

(require 'guide-key)
(diminish 'guide-key-mode)
(setq guide-key/guide-key-sequence '("C-c" "C-c e" "C-c w" "C-c o"))
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/idle-delay 0.25)
(guide-key-mode 1)

;;;;; setup flyspell
(defun flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
                       (sort (car (cdr (cdr poss))) 'string<)
                     (car (cdr (cdr poss)))))
         (cor-menu (if (consp corrects)
                       (mapcar (lambda (correct)
                                 (list correct correct))
                               corrects)
                     '()))
         (affix (car (cdr (cdr (cdr poss)))))
         show-affix-info
         (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                     (list
                                      (list (concat "Save affix: " (car affix))
                                            'save)
                                      '("Accept (session)" session)
                                      '("Accept (buffer)" buffer))
                                   '(("Save word" save)
                                     ("Accept (session)" session)
                                     ("Accept (buffer)" buffer)))))
                       (if (consp cor-menu)
                           (append cor-menu (cons "" save))
                         save)))
         (menu (mapcar
                (lambda (arg) (if (consp arg) (car arg) arg))
                base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))
(eval-after-load "flyspell"
      '(progn
         (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

;; http://stackoverflow.com/questions/5194294/how-to-remove-all-newlines-from-selected-region-in-emacs
(defun remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(defun jkf/add-word-at-point-to-ispell-buffer-session-localwords () (interactive)
       (add-to-list 'ispell-buffer-session-localwords
                    (substring-no-properties (word-at-point))))
(flyspell-mode 1)



(defun jkf/open-bash-here ()
  (interactive)
  ;; but how to start in the current working directory
  (w32-browser "c:/Program Files (x86)/Git/Git Bash.vbs"))

(disable (flycheck-define-checker proselint
                                  "A linter for prose."
                                  :command ("proselint" source-inplace)
                                  :error-patterns
                                  ((warning line-start (file-name) ":" line ":" column ": "
                                            (id (one-or-more (not (any " "))))
                                            (message) line-end))
                                  :modes (text-mode markdown-mode gfm-mode))

         (add-to-list 'flycheck-checkers 'proselint))

(defun jkf/proselint-buffer () (interactive)
       (compile (format "proselint.exe %s" (buffer-file-name))))
(put 'narrow-to-region 'disabled nil)

(defun jkf/active-minor-modes () (interactive)
       (--filter (and (boundp it) (symbol-value it)) minor-mode-list))

(disable (powerline-default-theme)
(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil))
;(add-hook 'desktop-after-read-hook 'powerline-reset)

(add-hook 'org-mode-hook 'flyspell-mode)
(setq org-startup-truncated nil)  ; linewrap for org-mode
(setq org-log-done 'time)
;(define-key dired-mode-map (kbd "f") 'dired-filter-mode)
; f /. to filter by extension


(setq org-capture-templates
      '(
        ("r" "Run" item (file+headline jkf/run-file "Running")
         "%^t %^{distance}" :immediate-finish t)
        ("j" "Journal (today)" plain (file+olp+datetree jkf/journal-file)
         "\n%?")
        ("J" "Journal (other)" plain (file+datetree+prompt jkf/journal-file)
         "\n%?")
        ("w" "Work TODO" entry (file+headline jkf/org-todo-file "Work")
         "** TODO %?\n    DEADLINE: %^{deadline}t")
        ("f" "free software TODO" entry (file+headline jkf/org-todo-file "free software")
         "** SOMEDAY %?\n    ")
        ("h" "Home TODO" entry (file+headline jkf/org-todo-file "Home")
         "** TODO %?\n    DEADLINE: %^{deadline}t")))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-custom-commands
      '(("w" agenda "work agenda"
        ((org-agenda-skip-function '(org-agenda-skip-subtree-if
                                     'todo 'done))))))

(defun jkf---skip-unless-work-tree ()
  "Skip trees that are not under the work tree"
  (save-excursion
    (search-backward-regexp "^* " 0 t)
    (looking-at "* work")))


(setq org-todo-keywords
      '((sequence "TODO(t)" "SOMEDAY(s)" "DONE(d)" "WAITING(w)")))
(setq org-tags-column 50)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)


;; helm patch to put filename into kill ring
(defun helm-ff-insert-file-full-path-into-killring (filename) (kill-new filename))
(defun helm-ff-insert-file-basename-into-killring (filename)
  (kill-new (file-name-nondirectory filename)))
(eval-after-load 'helm-files
  '(nconc helm-find-files-actions
          (list
           (cons "Insert file base name into kill ring"
                 #'helm-ff-insert-file-basename-into-killring )
           (cons "Insert full path of file into kill ring"
                 #'helm-ff-insert-file-full-path-into-killring ))))

; easy way to clock into a job
(global-set-key (kbd "C-c o i") 'jkf/work-clock-in)
(setq jkf/clock-into-work-helm-source
      '((name . "Clock into which job?")
        (candidates . jkf/get-headers)
        (action . (lambda (candidate)
                    (progn
                      (find-file jkf/org-todo-file)
                      (message "%s" candidate)
                      (goto-line candidate)
                      (org-clock-in))))))
(defun jkf/work-clock-in ()
  (interactive)
  (helm :sources '(jkf/clock-into-work-helm-source)))
(defun jkf/get-line-and-number ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (cons (buffer-substring-no-properties (point-at-bol) (point-at-eol))
            (line-number-at-pos)))))
(defun jkf/get-headers ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents jkf/org-todo-file)
    (org-mode)
    (goto-char (point-min))
    ;(search-forward-regexp "^\\* work")
    (move-beginning-of-line 1)
    ;(org-map-entries 'jkf/get-line-and-number nil 'tree)
    (org-map-entries 'jkf/get-line-and-number nil nil)))

(disable (defun jkf/scale-stl ()
   (interactive)
   (jkf/replace-regexp "E-06" "E-09")
   (jkf/replace-regexp "E-05" "E-08")
   (jkf/replace-regexp "E-04" "E-07")
   (jkf/replace-regexp "E-03" "E-06")
   (jkf/replace-regexp "E-02" "E-05")
   (jkf/replace-regexp "E-01" "E-04")
   (jkf/replace-regexp "E+00" "E-03")
   (jkf/replace-regexp "E+01" "E-02")
   (jkf/replace-regexp "E+02" "E-01")
   (jkf/replace-regexp "E+03" "E+00")))

(let* ((fname (concat jkf/dropbox-dir "/org/itasca-telephone.el")))
  (when (file-exists-p fname)
    (load fname)
    (global-set-key (kbd "C-c o t") 'jkf/itasca-phone-book)))
(setq ispell-personal-dictionary "c:/src/dotfiles/jkf_ispell.txt")


;; (setq default-abbrev-mode t)
;; (define-abbrev-table
;;   'global-abbrev-table '(("mbf" "\\mathbf{}" nil 1)))

;;; (setq byte-compile-error-on-warn t)

(defun wc (&optional start end)
   "Prints number of lines, words and characters in region or whole buffer."
   (interactive)
   (let ((n 0)
         (start (if mark-active (region-beginning) (point-min)))
         (end (if mark-active (region-end) (point-max))))
     (save-excursion
       (goto-char start)
       (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
     (message "%3d %3d %3d" (count-lines start end) n (- end start))))

(defun jkf/mean (data) (/ (reduce '+ data) (float (length data))))

(defun jkf/percent-change (a b) (* 100 (/ (abs (- a b)) (max (abs a) (abs b))  )))

(defun jkf/decrypt-string (data)
  (interactive)
  (let ((i 0))
    (apply #'string
           (mapcar (lambda (a) (prog1 (- a (mod i 5)) (incf i))) data))))

(defun jkf/encrypt-string (data)
  (interactive)
  (let ((i 0))
    (kill-new (apply #'string
           (mapcar (lambda (a) (prog1 (+ a (mod i 5)) (incf i))) data)))))


(nyan-mode)
(setq nyan-bar-length 26)

(defun int-to-binary-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(add-hook 'org-mode-hook
          (lambda ()
            (when (equal (buffer-name) "todo.txt")
             (org-content 2))))

(setq vc-handled-backends nil)

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(defun jkf/decrypt-string-clipboard () (interactive)
       (with-temp-buffer (yank) (jkf/decrypt-string (buffer-string))))
;(jkf/decrypt-string-clipboard)

(defun jkf/miles-in-region (a b)
  "sum of numbers in region after orgmode datestamps are removed."
  (interactive "r")
  (save-excursion
    (kill-ring-save a b)
    (with-temp-buffer
      (yank)
      (goto-char (point-min))
      (replace-regexp "\s*-\s<....-..-..\s...>" "")
      (goto-char (point-min))
      (insert "(+ ")
      (goto-char (point-max))
      (insert ")")
      (call-interactively 'eval-last-sexp))))


(require 'ob-python)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)))

(defun jkf/fix-reveal-output ()
  "set theme and remove extra ... and >>>"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (jkf/replace-regexp "\\.\\.\\. " "")
    (goto-char (point-min))
    (jkf/replace-regexp "&gt;&gt;&gt; " "")
    (goto-char (point-min))
    (jkf/replace-regexp "moon\\.css" "white.css")))

(add-hook 'jkf/fix-reveal-output 'org-export-html-final-hook)

(defun jkf/decrypt-to-message () (interactive)
       (message (jkf/decrypt-string (with-temp-buffer (yank) (buffer-string)))))

(defun jkf/inplace-encrypt (x y) (interactive "r")
       (kill-region x y)
       (insert (jkf/encrypt-string (with-temp-buffer (yank) (buffer-string))))
       (move-end-of-line nil)
       (insert " // ")
       (insert (yank)))

(defun jkf/insert-random-string ()
  (interactive)
  (dotimes (_ 25)
    (insert
     (let ((x (random 36)))
       (if (< x 10) (+ x ?0) (+ x (- ?a 10)))))))

;; CloudCommunicator::decrypt_string(L"it\"dese\"dwd!cipseho$")  // is aasd asd aflsdfl

(defun sign-buffer ()
  "this function reads an emacs buffer and inserts a comment with
   a digest string at the top of the buffer. This digest is used
   by Blo-Up to verify that the code is signed by Itasca and can
   be run in secure mode."
  (interactive)
  (goto-char (point-min))
  (when (string= ";signed-code " (buffer-substring
                                  (point-at-bol)
                                  (+ 13 (point-at-bol))))
    (kill-line) (kill-line))
  (insert "XBRf3IPiRcd6ILtaelEH fGaXGe4ORz7tyALX9RNY daP5P4qzSMh1bLzlNUq0")
  (let ((digest (sha1 (current-buffer))))
    (goto-char (point-min))
    (delete-char 62)
    (insert (format ";signed-code %s\n" digest))))


(defun ienc (start end)
  "rot 13 encoding for security error messages."
  (interactive "r")
  (let ((mys (buffer-substring start end)))
    (loop for i below (length mys) do
          (aset mys i (+ (elt mys i) (% i 5))))
  (insert " " mys)))


(defconst *inplace-prefix* "CloudCommunicator::decrypt_string(")
(defconst *inplace-suffix* ")")
(defun inplace-encrypt (x y)
  (interactive "r")
  (save-excursion
    (kill-region x y)
    (let* ((raw (substring-no-properties (car kill-ring)))
           (inner (substring raw 1 (- (length raw) 1)))
           (encr (jkf/encrypt-string inner))
           (escaped (s-replace "\"" "\\\"" encr))
           (wrapped (concat *inplace-prefix* "L\"" escaped "\"" *inplace-suffix*)))
      (insert wrapped)
      (move-end-of-line nil)
      (insert (concat " // " inner))
      (message wrapped))))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((lines) (end (copy-marker end)))
      (goto-char start)
      (while (and (< (point) (marker-position end))
                  (not (eobp)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (if (member line lines)
              (delete-region (point) (progn (forward-line 1) (point)))
            (push line lines)
            (forward-line 1)))))))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'normal :height 1.0)))

(add-hook 'org-mode-hook 'my/org-mode-hook)
(require 'blacken)
(global-set-key (kbd "C-c b") 'blacken-buffer)

(require 'ispell)

(add-to-list 'exec-path "C:/unix_bin/hunspell-1.3.2-3-w32-bin/bin")
(setq ispell-local-dictionary-alist '(

       (nil
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           t
           ("-d" "en_US" "-p" "C:\\unix_bin\\hunspell-1.3.2-3-w32-bin\\share\\hunspell\\personal.en")
           nil
           iso-8859-1)

       ("american"
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           t
           ("-d" "en_US" "-p" "C:\\unix_bin\\hunspell-1.3.2-3-w32-bin\\share\\hunspell\\personal.en")
           nil
           iso-8859-1)
        ))

(setq ispell-program-name (locate-file "hunspell"
                                       exec-path exec-suffixes 'file-executable-p))
;https://lists.gnu.org/archive/html/help-gnu-emacs/2014-04/msg00030.html
