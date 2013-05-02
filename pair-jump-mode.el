
(defun pair-jump-function (supress)
  "Insert a space or jump over closing pairs eg. ) ] ' \" if
proceeded by a space. A prefix argument negates this behavior.
The list of characters considered for jumping over are given as
strings in the buffer local variable `pair-jump-list'."
  (interactive "p")
  (if (not (equal 1 supress))
      (insert ?\s)
    (if (and (eql (char-before) ?\s)
             (member (string (char-after)) pair-jump-list))
        (progn
          (message "Jumping over %c" (char-after))
          (backward-delete-char 1)
          (forward-char))
      (insert ?\s))))

;;;###autoload
(define-minor-mode pair-jump-mode
  "When space bar is pressed either insert a space or move the
point over the closing pair character at point. Closing pair
characters are jumped over if preceded by a space. The list of
characters considered for jumping over are given as strings in
the buffer local variable `pair-jump-list'. A prefix argument to
space negates this behavior."
  :lighter " pj"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'pair-jump-function)
            map)
  (set (make-local-variable 'pair-jump-list) '(")" "]" "'" "\"" "}")))

;;;###autoload
(add-hook 'python-mode-hook 'pair-jump-mode)
(add-hook 'c++-mode-hook 'pair-jump-mode)

(provide 'pair-jump-mode)
