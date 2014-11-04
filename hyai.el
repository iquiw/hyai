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
    (`"where" (if (hyai-search-previous-token nil '("where"))
                  (list (+ (current-indentation) hyai-basic-offset))
                (list (+ (current-indentation) hyai-where-offset))))

    (`"then" (list (+ (hyai-search-previous-token nil '("if"))
                      hyai-basic-offset)))
    (`"else" (list (hyai-search-previous-token nil '("then"))))

    ((or `"(" `"{" `"[")
     (list (+ (hyai-previous-offset) hyai-basic-offset)))
    ((or `")" `"}" `"]")
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`","
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`"->" (let ((offset (hyai-search-previous-token '("::") nil)))
             (if offset
                 (list offset)
               (list hyai-basic-offset))))

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
             (if (save-excursion
                   (= (point) (progn (beginning-of-line-text) (point))))
                 (list (+ (current-column) hyai-where-offset))
               (let ((offset (hyai-search-previous-token nil '("where"))))
                 (cond
                  (offset (list (+ offset hyai-basic-offset)))
                  ((looking-at-p "module") (list (current-indentation)))
                  (t (list (+ (current-indentation) hyai-basic-offset)))))))
            (`"of"
             (let ((offset (hyai-search-previous-token nil '("case"))))
               (if offset
                   (mapcar (lambda (x) (+ x hyai-basic-offset))
                           (list (current-indentation) offset))
                 (_ (hyai-generate-offsets
                     (car (hyai-current-offset-head)))))))
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
    (let* ((c (char-after))
           (head (cl-case (char-syntax c)
                   (?w (save-excursion (hyai-grab-syntax-forward "w")))
                   (?_ (save-excursion (hyai-grab-syntax-forward "_")))
                   (?\( (string c))
                   (?\) (string c))
                   (?. (string c))
                   (t ""))))
      (cons (current-column) head))))

(defun hyai-search-previous-token (symbols words)
  (skip-syntax-backward " >")
  (catch 'result
    (while (not (bobp))
      (let ((syn (char-syntax (char-before))))
        (cl-case syn
          (?w (if (null words)
                  (skip-syntax-backward "w")
                (when (member (hyai-grab-syntax-backward "w") words)
                  (throw 'result (current-column)))))
          (?_ (if (null symbols)
                  (skip-syntax-backward "_")
                (when (member (hyai-grab-syntax-backward "_") symbols)
                  (throw 'result (current-column)))))
          (?> (if (/= (char-syntax (char-after)) ? )
                  (throw 'result 0)
                (backward-char)))
          (?\) (condition-case nil (backward-sexp)
                 (error (throw 'result nil))))
          (?\" (condition-case nil (backward-sexp)
                 (error (throw 'result nil))))
          (t (skip-syntax-backward (string syn))))))
    nil))

(defun hyai-previous-offset ()
  (skip-syntax-backward " >")
  (current-indentation))

(defun hyai-grab-word ()
  (hyai-grab-syntax-backward "w"))

(defun hyai-grab-syntax-forward (sc)
  (buffer-substring-no-properties
   (point)
   (progn (skip-syntax-forward sc) (point))))

(defun hyai-grab-syntax-backward (sc)
  (buffer-substring-no-properties
   (point)
   (progn (skip-syntax-backward sc) (point))))

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
