;;; hyai.el --- Haskell Yet Another Indentation. -*- lexical-binding: t -*-

;; Copyright (C) 2014 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/hyai
;; Version:   0.0.0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;;; Commentary:
;;; Code:

(require 'cl-lib)

(defconst hyai-basic-offset 4)

(defun hyai-indent-line ()
  (let* ((oh (hyai-current-offset-head))
         (offset (car oh))
         (head (cdr oh))
         (indents (hyai-indent-candidates head))
         (nexts (when (eq this-command 'indent-for-tab-command)
                  (cdr (member offset indents)))))
    (when indents
      (indent-line-to (car (or nexts indents)))
      (unless (string= head "")
        (end-of-line)))))

(defun hyai-indent-candidates (head)
  (save-excursion
    (or (hyai-indent-candidates-from-current head)
        (hyai-indent-candidates-from-previous))))

(defun hyai-indent-candidates-from-current (head)
  (pcase head
    (`"where" '(2))
    (_ nil)))

(defun hyai-indent-candidates-from-previous ()
  (skip-syntax-backward " >")
  (if (bobp)
      '(0)
    (cl-case (char-syntax (char-before))
      (?w (pcase (hyai-grab-word)
            (`"do" (list (+ 4 (car (hyai-current-offset-head)))))
            (`"where"
             (pcase (hyai-current-offset-head)
               (`(,offset . "module") (list offset))
               (_ '(4))))
            (_ (hyai-generate-offsets (car (hyai-current-offset-head))))))
      (t (hyai-generate-offsets (car (hyai-current-offset-head)))))))

(defun hyai-current-offset-head ()
  (beginning-of-line-text)
  (if (eobp)
     '(0 . "")
    (let ((head (cl-case (char-syntax (char-after))
                  (?w (looking-at "\\sw*")
                      (match-string-no-properties 0))
                  (?_ (looking-at "\\s_*")
                      (match-string-no-properties 0))
                  (t ""))))
      (cons (current-column) head))))

(defun hyai-grab-word ()
  (when (looking-back "\\<[[:word:]]+")
    (match-string-no-properties 0)))

(defun hyai-generate-offsets (current)
  (let ((i (- current hyai-basic-offset))
        result)
    (while (>= i 0)
      (push i result)
      (setq i (- i hyai-basic-offset)))
    (push (+ current hyai-basic-offset) result)
    (push current result)))

;;;###autoload
(define-minor-mode hyai-mode
  "Haskell Yet Another Indentation minor mode."
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
;;; hyai.el ends here
