
(make-variable-buffer-local
 (defvar pair-jump-list '(")" "]" "}" ?\' ?\")
   "List of closing characters to jump over"))
;(memq ")" pair-jump-list)

(defun pair-jump-function ()
  "causes space to jump over  ) or ] if a space precedes it"
  (interactive)
  (if (and (eql (char-before) ?\s)
	   (or (eql (char-after) ?\))
	       (eql (char-after) ?\])))
      (progn
	(message "Jumping over %c" (char-after))
	(backward-delete-char 1)
	(forward-char))
    (insert ?\s)))

;;;###autoload
(define-minor-mode pair-jump-mode
  "Docstring for mode"
  :lighter " pj"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'pair-jump-function)
            map))

;;;###autoload
;(add-hook 'text-mode-hook 'pair-jump-mode)

(provide 'pair-jump-mode)
