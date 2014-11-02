;;; hyai.el --- Haskell Yet Another Indentation -*- lexical-binding: t -*-

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
      (indent-line-to (car (or nexts indents))))))

(defun hyai-indent-candidates (head)
  (save-excursion
    (or (hyai-indent-candidates-from-current head)
        (hyai-indent-candidates-from-previous))))

(defun hyai-indent-candidates-from-current (head)
  (pcase head
    (`"where" (pcase (hyai-previous-offsets-keyword "where")
                (`(,offsets . "where")
                 (list (+ (car offsets) hyai-basic-offset)))
                (`(,offsets . ,_)
                 (list (+ (car offsets) hyai-where-offset)))))

    (`"then" (list (+ (car (last (car (hyai-previous-offsets-keyword "if"))))
                      hyai-basic-offset)))
    (`"else" (last (car (hyai-previous-offsets-keyword "then"))))

    ((or `"(" `"{" `"[")
     (list (+ (hyai-previous-offset) hyai-basic-offset)))
    ((or `")" `"}" `"]")
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`","
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`"->" (last (car (hyai-previous-offsets-operator "::"))))

    (_ nil)))

(defun hyai-indent-candidates-from-previous ()
  (skip-syntax-backward " >")
  (if (bobp)
      '(0)
    (cl-case (char-syntax (char-before))
      (?w (pcase (hyai-grab-word)
            (`"do"
             (list (+ (car (hyai-current-offset-head)) hyai-basic-offset)))
            (`"where"
             (goto-char (match-beginning 0))
             (if (save-excursion
                   (= (point) (progn (beginning-of-line-text) (point))))
                 (list (+ (current-column) hyai-where-offset))
               (pcase (hyai-previous-offsets-keyword "where")
                 (`(,offsets . "module") offsets)
                 (`(,offsets . ,_)
                  (list (+ (car offsets) hyai-basic-offset))))))
            (`"of"
             (goto-char (match-beginning 0))
             (pcase (hyai-previous-offsets-keyword "case")
               (`(,offsets . "case")
                (mapcar (lambda (x) (+ x hyai-basic-offset)) offsets))
               (_ (hyai-generate-offsets (car (hyai-current-offset-head))))))
            (_ (hyai-generate-offsets (car (hyai-current-offset-head))))))

      (?. (let* ((off1 (hyai-previous-offset))
                 (off2 (hyai-search-backward-open-bracket nil)))
            (list (or (and off2
                           (progn
                             (forward-char)
                             (skip-syntax-forward " ")
                             (current-column)))
                      off1))))

      (?\(  (list (+ (current-column) 1)))

      (t (hyai-generate-offsets (car (hyai-current-offset-head)))))))

(defun hyai-current-offset-head ()
  (beginning-of-line)
  (skip-syntax-forward " ")
  (if (eobp)
     '(0 . "")
    (let ((head (cl-case (char-syntax (char-after))
                  (?w (looking-at "\\sw*")
                      (match-string-no-properties 0))
                  (?_ (looking-at "\\s_*")
                      (match-string-no-properties 0))
                  (?\( (string (char-after)))
                  (?\) (string (char-after)))
                  (?. (string (char-after)))
                  (t ""))))
      (cons (current-column) head))))

(defun hyai-previous-offsets-keyword (keyword)
  (hyai-previous-offsets-token
   (concat "\\(^[^[:space:]#]+\\|\\<" keyword "\\>\\)")))

(defun hyai-previous-offsets-operator (operator)
  (hyai-previous-offsets-token
   (concat "\\(^[^[:space:]#]+\\|\\_<" operator "\\_>\\)")))

(defun hyai-previous-offsets-token (regexp)
  (if (re-search-backward regexp nil t)
      (let ((off1 (current-indentation))
            (off2 (progn (goto-char (match-beginning 1))
                         (current-column))))
        (cons (cons off1 (when (/= off1 off2) (list off2)))
              (match-string-no-properties 1)))
    '((0) . "")))

(defun hyai-previous-offset ()
  (skip-syntax-backward " >")
  (current-indentation))

(defun hyai-grab-word ()
  (when (looking-back "\\<[[:word:]]+")
    (match-string-no-properties 0)))

(defun hyai-search-backward-open-bracket (across-lines)
  (catch 'result
    (while (< (skip-syntax-backward (if across-lines "^()" "^()>")) 0)
      (let ((c (char-before)))
        (cond
         ((not c) (throw 'result nil))
         ((= (char-syntax c) ?\))
          (condition-case nil (backward-sexp)
            (error (throw 'result nil))))
         (t (backward-char)
            (throw 'result c)))))))

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
