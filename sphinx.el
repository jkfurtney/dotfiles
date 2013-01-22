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

(defun sphinx-reset () (interactive) (makunbound 'sphinx-build-dir))
