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
        (hyai-indent-candidates-from-previous)
        (hyai-indent-candidates-from-backward))))

(defun hyai-indent-candidates-from-current (head)
  (pcase head
    (`"where" (if (hyai-search-token-backward nil '("where"))
                  (list (+ (current-indentation) hyai-basic-offset))
                (list (+ (current-indentation) hyai-where-offset))))

    (`"then" (list (+ (hyai-search-token-backward nil '("if"))
                      hyai-basic-offset)))
    (`"else" (list (hyai-search-token-backward nil '("then"))))

    ((or `"(" `"{" `"[")
     (list (+ (hyai-previous-offset) hyai-basic-offset)))
    ((or `")" `"}" `"]")
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`","
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`"->" (let ((offset (hyai-search-token-backward '("::") nil)))
             (if offset
                 (list offset)
               (list hyai-basic-offset))))

    (`"|" (let (limit ctx)
            (save-excursion
              (setq ctx (hyai-search-context))
              (setq limit (point)))
            (or (if (equal ctx "data")
                    (hyai-search-vertical-equal limit)
                  (hyai-search-vertical limit))
                (list (+ (current-indentation) hyai-basic-offset)))))))

(defun hyai-indent-candidates-from-previous ()
  (skip-syntax-backward " >")
  (if (bobp)
      '(0)
    (cl-case (char-syntax (char-before))
      (?w (pcase (hyai-grab-syntax-backward "w")
            (`"do"
             (list (+ (car (hyai-current-offset-head)) hyai-basic-offset)))
            (`"where"
             (if (save-excursion
                   (= (point) (progn (beginning-of-line-text) (point))))
                 (list (+ (current-column) hyai-where-offset))
               (let ((offset (hyai-search-token-backward nil '("where"))))
                 (cond
                  (offset (list (+ offset hyai-basic-offset)))
                  ((looking-at-p "module") (list (current-indentation)))
                  (t (list (+ (current-indentation) hyai-basic-offset)))))))
            (`"of"
             (let ((offset (hyai-search-token-backward nil '("case"))))
               (when offset
                 (mapcar (lambda (x) (+ x hyai-basic-offset))
                         (list (current-indentation) offset)))))))

      (?. (let* ((off1 (hyai-previous-offset))
                 (off2 (hyai-search-backward-open-bracket nil)))
            (list (or (and off2
                           (progn
                             (forward-char)
                             (skip-syntax-forward " ")
                             (current-column)))
                      off1))))

      (?\( (list (+ (current-column) 1))))))

(defun hyai-indent-candidates-from-backward ()
  (let* ((offs (hyai-possible-offsets))
         (offset (current-indentation)))
    (or offs
        (progn
          (push (+ offset hyai-basic-offset) offs)
          (while (and (> offset hyai-basic-offset)
                      (>= (forward-line -1) 0))
            (setq offset (current-indentation))
            (push offset offs))
          (when (= offset hyai-basic-offset)
            (push offset offs))
          (append offs '(0))))))

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

(defun hyai-search-token-backward (symbols words)
  (skip-syntax-backward " >")
  (catch 'result
    (condition-case nil
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
              (?> (if (/= (char-syntax (char-after)) ?\s)
                      (throw 'result nil)
                    (backward-char)))
              (?\) (backward-sexp))
              (?\" (backward-sexp))
              (t (skip-syntax-backward (string syn))))))
      (error (throw 'result nil)))))

(defun hyai-possible-offsets ()
  (let (offs prev beg curr)
    (catch 'result
      (condition-case nil
          (while (not (bobp))
            (let ((syn (char-syntax (char-before))))
              (cl-case syn
                (?  (setq prev (current-column))
                    (skip-syntax-backward " "))
                (?_ (push (or prev (current-column)) offs)
                    (skip-syntax-backward "_")
                    (setq beg (current-column)))
                (?> (throw 'result t))
                (?\) (backward-sexp))
                (?\" (backward-sexp))
                (t (skip-syntax-backward (string syn))))))
        (error t)))
    (setq curr (current-indentation))
    (cond
     ((and beg (/= beg curr)) (append offs (list curr)))
     (offs (append offs '(0))))))

(defun hyai-search-vertical (limit)
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn)
       (when (= syn ?_)
         (let ((s (hyai-grab-syntax-backward "_")))
           (when (string= s "|")
             (push (current-column) result))
           'next))
       'cont)
     limit)
    (cl-remove-duplicates result)))

(defun hyai-search-vertical-equal (limit)
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn)
       (when (= syn ?_)
         (let ((s (hyai-grab-syntax-backward "_")) offset)
           (setq offset (current-column))
           (cond
            ((or (string= s "|")
                 (= offset (current-indentation)))
             (push offset result))
            ((string= s "=") (push offset result)))
           'next))
       'cont)
     limit)
    (cl-remove-duplicates result)))

(defun hyai-process-syntax-backward (callback &optional limit stop-beginning)
  (setq limit (or limit 0))
  (skip-syntax-backward " >")
  (let ((res 'cont))
    (while (and (not (eq res 'stop))
                (> (point) limit)
                (not (bobp)))
      (let ((syn (char-syntax (char-before))))
        (setq res (funcall callback syn))
        (when (eq res 'cont)
          (condition-case nil
              (cl-case syn
                (?> (if (and stop-beginning
                             (/= (char-syntax (char-after)) ?\s))
                        (setq res 'stop)
                      (backward-char)))
                (?\) (backward-sexp))
                (?\" (backward-sexp))
                (t (skip-syntax-backward (string syn))))
            (error (setq res 'stop))))))))

(defun hyai-search-context ()
  (when (re-search-backward "^\\([^#[:space:]]+\\)" nil t)
    (let ((ctx (match-string-no-properties 1)))
      (when (member ctx '("data" "class" "type" "newtype" "module"))
        ctx))))

(defun hyai-previous-offset ()
  (skip-syntax-backward " >")
  (current-indentation))

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
