; emacs configuration for c/c++ programming

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

(add-hook 'c++-mode-hook
      '(lambda ()
         (add-hook 'before-save-hook
                   (lambda ()
                     (untabify (point-min) (point-max))))))
