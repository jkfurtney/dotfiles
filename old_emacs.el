
(defun jkf/fish-binary-file-p (filename)
  (interactive)
  "Returns true if a given file is a FISH binary file."
  (eq 178278912
      (cdr (assoc :fc
                  (bindat-unpack '((:fc u32r) (:dummy u32r))
                                 (with-temp-buffer
                                   (insert-file-literally filename)
                                   (string-to-unibyte (buffer-string))))))))

(defun jkf/setup-fortran-mode ()
  (interactive)
  (which-function-mode 1))
(add-hook 'fortran-mode-hook 'jkf/setup-fortran-mode)
