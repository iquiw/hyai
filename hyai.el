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
(defconst hyai-where-offset 2)

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
    (`"where" (let ((offset (car (hyai-previous-offset-head))))
                (list (+ offset (if (= offset 0)
                                    hyai-where-offset
                                  hyai-basic-offset)))))
    (_ nil)))

(defun hyai-indent-candidates-from-previous ()
  (skip-syntax-backward " >")
  (if (bobp)
      '(0)
    (cl-case (char-syntax (char-before))
      (?w (pcase (hyai-grab-word)
            (`"do" (list (+ 4 (car (hyai-current-offset-head)))))
            (`"where"
             (goto-char (match-beginning 0))
             (if (save-excursion
                   (= (point) (progn (beginning-of-line-text) (point))))
                 (list (+ (current-column) hyai-where-offset))
               (pcase (hyai-previous-offset-head)
                 (`(,poffset . "module") (list poffset))
                 (`(,poffset . ,_) (list (+ poffset hyai-basic-offset))))))
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

(defun hyai-previous-offset-head ()
  (if (re-search-backward "\\(^[^[:space:]#]+\\|where\\)" nil t)
      (cons (current-column) (match-string-no-properties 1))
    '(0 . "")))

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
