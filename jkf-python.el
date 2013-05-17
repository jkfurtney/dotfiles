;; emacs stuff specific to python programming

(require 'cython-mode)
(setq python-check-command "pep8 -r --ignore=E221")
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(defun p-compile ()
  "build python extension module. First call prompts for a directory"
  (interactive)
  (unless (boundp 'python-build-dir)
    (setq python-build-dir (read-directory-name "python build dir ")))
  (let ((default-directory python-build-dir))
       (compile "python setup.py install --user")))
