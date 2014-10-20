(defconst hyai-indent-offset 4)

(defun hyai-indent-line ()
  (let* ((offset (hyai-current-offset))
         (indents (hyai-indent-candidates))
         (nexts (member offset indents)))
    (indent-line-to (car (or (cdr nexts) indents)))))

(defun hyai-indent-candidates ()
  '(4))

(defun hyai-current-offset ()
  (beginning-of-line-text 1)
  (current-column))

;;;###autoload
(define-minor-mode hyai-mode
  "Haskell yet another indentation minor mode."
  :lighter " HYAI"
  (kill-local-variable 'indent-line-function)
  (when hyai-mode
    (set (make-local-variable 'indent-line-function) 'hyai-indent-line)))

(defun turn-on-hyai ()
  (interactive)
  (hyai-mode 1))

(defun turn-off-hyai ()
  (interactive)
  (hyai-mode 0))

(provide 'hyai)
