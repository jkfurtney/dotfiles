
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
          (message "pair-jump-mode: jumping over %c" (char-after))
          (backward-delete-char 1)
          (forward-char)
          (when pair-jump-keep-trailing-space (insert ?\s)))
      (insert ?\s))))

;;;###autoload
(define-minor-mode pair-jump-mode
  "Two spaces to jump over closing pair characters like ) or ].
For the lazy typist; sometimes hitting the space bar twice is
eaiser than C-f or ). Works well with electric-pair mode.

When space bar is pressed either insert a space or move the point
over the closing pair character at point. Closing pair characters
are jumped over if preceded by a space. The list of characters
considered for jumping over are given as strings in the buffer
local variable `pair-jump-list'. A prefix argument to space
negates this behavior."
  :lighter " pj"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'pair-jump-function)
            map)
  (set (make-local-variable 'pair-jump-list) '(")" "]" "'" "\"" "}"))
  (set (make-local-variable 'pair-jump-keep-trailing-space) nil))

;;;###autoload
(add-hook 'python-mode-hook 'pair-jump-mode)
(add-hook 'c++-mode-hook 'pair-jump-mode)

(require 'ert)
(ert-deftest test-pair-jump ()
  (with-temp-buffer
    (pair-jump-mode)
    (let ((starting-string  ")]}'\"")
          test-string)

      (insert starting-string)
      (goto-char (point-min))
      (pair-jump-function 1)
      (setq test-string (buffer-substring (point-at-bol) (point-at-eol)))
      (should (equal test-string (concat " " starting-string)))

      (pair-jump-function 1) ; test that jump occured
      (setq test-string (buffer-substring (point-at-bol) (point-at-eol)))
      (should (equal test-string ")]}'\"")))))

(provide 'pair-jump-mode)
