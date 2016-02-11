; use syntax table to find closing pairs
;

(defun pair-jump-function ()
  "Insert a space or jump over closing pairs eg. ) ] ' \" if
proceeded by a space. A prefix argument negates this behavior.
The list of characters considered for jumping over are given as
strings in the buffer local variable `pair-jump-list'."
  (interactive)
  (when (and (eql (char-before) ?\s)
             (eql (char-before (1- (point))) ?\s)
             (not (eobp))
             (member (string (char-after)) pair-jump-list))
    (progn
      (message "pair-jump-mode: Jumping over %c" (char-after))
      (backward-char 2)
      (delete-forward-char 2)
      (forward-char 1))))

;;;###autoload
(define-minor-mode pair-jump-mode
  "Two spaces to jump over closing pair characters like ) or ].
For the lazy typist; sometimes hitting the space bar twice is
eaiser than C-f or ). Works well with electric-pair mode.

When space bar is pressed either insert a space or move the point
over the closing pair character at point. Closing pair characters
are jumped over if preceded by a space. The list of characters
considered for jumping over are given as strings in the buffer
local variable `pair-jump-list'."
  :lighter " pj"
  :global t
  (setq pair-jump-list '(")" "]" "'" "\"" "}" "`"))
  (if pair-jump-mode
      (add-hook 'post-self-insert-hook #'pair-jump-function)
    (remove-hook 'post-self-insert-hook #'pair-jump-function)))

;;;###autoload
(add-hook 'python-mode-hook 'pair-jump-mode)
(add-hook 'c++-mode-hook 'pair-jump-mode)

(require 'ert)
(ert-deftest test-pair-jump ()
  (with-temp-buffer
    (pair-jump-mode 1)
    (let ((starting-string  ")]}'\"")
          test-string)

      (insert starting-string)
      (goto-char (point-min))
      (pair-jump-function)
      (insert ?\s)
      (setq test-string (buffer-substring (point-at-bol) (point-at-eol)))
      (should (equal test-string (concat " " starting-string)))

      (insert ?\s)
      (pair-jump-function) ; test that jump occured
      (setq test-string (buffer-substring (point-at-bol) (point-at-eol)))
      (should (equal test-string ")]}'\"")))))

(provide 'pair-jump-mode)
