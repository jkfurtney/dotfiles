; basic key bindings

; C-o for find file
(global-set-key "\C-o" 'find-file)
(require 'dired)
(define-key dired-mode-map (kbd "C-o") 'find-file)

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

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'c++-mode-hook
      '(lambda ()
         (add-hook 'before-save-hook
                   (lambda ()
                     (untabify (point-min) (point-max))))))

(tool-bar-mode 0)
(menu-bar-mode 0)
(setq make-backup-files nil)

(defun sum-region (a b)
  "sum numbers in the region"
  (interactive "r")
  (message "sum: %d"
    (apply '+ (mapcar 'string-to-number
      (split-string (buffer-substring a b)))))
  (insert (number-to-string (apply '+ (mapcar 'string-to-number (split-string (buffer-substring a b)))))))

(defun filename-comment ()
  "Insert filename as c++ comment eg. //filename.h"
  (interactive)
  (insert (concat "//" (file-name-nondirectory buffer-file-name))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;name-last-keyboard-macro
;insert-kbd-macro

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

(setq python-check-command "pep8 -r --ignore=E221")

(setq x-select-enable-clipboard t)

(add-to-list 'load-path "~/.emacs.d/")

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'magit)
(require 'magit-svn)

(global-set-key "\C-js" 'magit-status)

;; OS specific setup

;; Linux specific setup
(if  (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    ;;; Lisp (SLIME) interaction -- linux only
    (progn
      (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
      (setq inferior-lisp-program "clisp")
      (add-to-list 'load-path "~/.slime")
      (require 'slime)
      (slime-setup)))

;; OS X specific setup
(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/opt/local/bin/"))

;; windows specific setup
(if  (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (progn

      (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")

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


(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key "\C-jk" 'kill-all-buffers)

(defun p-compile ()
  "build python extension module. First call prompts for a directory"
  (interactive)
  (unless (boundp 'python-build-dir)
    (setq python-build-dir (read-directory-name "python build dir ")))
  (let ((default-directory python-build-dir))
       (compile "python setup.py install --user")))


;; computer specific setup
(cond
 ; vaio
 ((equal (system-name) "SHOTOVER")
  (setq initial-frame-alist '((width . 80) (height . 37)))
  (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
  (add-to-list 'load-path "C:/src/slime/")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy)))

 ((equal (system-name) "UNSER")
  (setq initial-frame-alist '((width . 80) (height . 41)))
  (set-face-attribute 'default nil :height 150)
  (setq inferior-lisp-program "C:/src/ecl/msvc/ecl2.exe")
  (add-to-list 'load-path "C:/src/slime/")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy)))

 ; default
 (t (setq initial-frame-alist '((width . 80) (height . 34)))))

;; note on windows $HOME is different in bash and emacs!
;; cp ~/.gitconfig ~/AppData/Roaming/
;; to get magit to recognize user.name and user.email

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code))
  ;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; reStructuredText / Sphinx stuff
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

(global-set-key "\C-jm" 'rest-wrap-math)

(defun s-compile-cmd (cmd)
  "build sphinx documentation. First call prompts for a directory"
  (interactive)
  (unless (boundp 'sphinx-build-dir)
    (setq sphinx-build-dir (read-directory-name "sphinx build dir ")))
  (let ((default-directory sphinx-build-dir))
       (compile cmd)))
(defun s-compile () (interactive) (s-compile-cmd "make html"))
(defun s-pcompile () (interactive) (s-compile-cmd "make latexpdf"))

; the vc-find-file-hook seems to cause a big slowdown in windows
(remove-hook 'find-file-hooks 'vc-find-file-hook)
