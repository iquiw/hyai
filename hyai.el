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
  (let* ((cc (current-column))
         (oh (hyai-current-offset-head))
         (offset (car oh))
         (head (cdr oh))
         (indents (hyai-indent-candidates head))
         (nexts (when (eq this-command 'indent-for-tab-command)
                  (cdr (member offset indents)))))
    (when indents
      (indent-line-to (car (or nexts indents)))
      (when (> cc offset)
        (forward-char (- cc offset))))))

(defun hyai-indent-candidates (head)
  (save-excursion
    (skip-syntax-backward " >")
    (or (save-excursion (hyai-indent-candidates-from-current head))
        (save-excursion (hyai-indent-candidates-from-previous))
        (save-excursion (hyai-indent-candidates-from-backward)))))

(defun hyai-indent-candidates-from-current (head)
  (pcase head
    (`"where" (if (hyai-search-token-backward nil '("where"))
                  (list (+ (current-indentation) hyai-basic-offset))
                (list (+ (current-indentation) hyai-where-offset))))

    (`"then" (hyai-offsetnize (hyai-search-token-backward nil '("if"))
                              hyai-basic-offset))
    (`"else" (hyai-offsetnize (hyai-search-token-backward nil '("then"))))

    (`"in" (hyai-offsetnize (hyai-search-token-backward nil '("let"))))

    (`"("
     (let (offset)
       (save-excursion
         (when (member (hyai-search-context) '("import" "module"))
           (setq offset (current-column))
           (list (+ offset hyai-basic-offset))))))

    ((or `"{" `"[")
     (list (+ (hyai-previous-offset) hyai-basic-offset)))

    ((or `")" `"}" `"]")
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`","
     (and (hyai-search-backward-open-bracket t) (list (current-column))))

    (`"->" (or (hyai-offsetnize (hyai-search-token-backward '("::") nil))
               (list hyai-basic-offset)))

    (`"|" (let (limit ctx)
            (save-excursion
              (setq ctx (hyai-search-context))
              (setq limit (point)))
            (or (if (equal ctx "data")
                    (hyai-search-vertical-equal limit)
                  (hyai-search-vertical limit))
                (list (+ (current-indentation) hyai-basic-offset)))))))

(defun hyai-indent-candidates-from-previous ()
  (if (bobp)
      '(0)
    (cl-case (char-syntax (char-before))
      (?w (pcase (hyai-grab-syntax-backward "w")
            (`"do"
             (list (+ (current-indentation) hyai-basic-offset)))
            (`"where"
             (if (save-excursion
                   (= (point) (progn (beginning-of-line-text) (point))))
                 (list (+ (current-column) hyai-where-offset))
               (or (hyai-offsetnize
                    (hyai-search-token-backward nil '("where"))
                    hyai-basic-offset)
                   (if (looking-at-p "module")
                       (list (current-indentation))
                     (list (+ (current-indentation) hyai-basic-offset))))))
            (`"of"
             (let ((offset (hyai-search-token-backward nil '("case"))))
               (when offset
                 (mapcar (lambda (x) (+ x hyai-basic-offset))
                         (list (current-indentation) offset)))))))

      (?_ (pcase (hyai-grab-syntax-backward "_")
            (`"="
             (list (+ (current-indentation) hyai-basic-offset)))))

      (?. (let* ((off1 (hyai-previous-offset))
                 (off2 (hyai-search-backward-open-bracket nil)))
            (list (or (and off2
                           (progn
                             (forward-char)
                             (skip-syntax-forward " ")
                             (current-column)))
                      off1))))

      (?\( (pcase (char-before)
             (?\( (list (+ (current-column) 1)))
             ((or ?\{ ?\[)
              (list (+ (hyai-previous-offset) hyai-basic-offset))))))))

(defun hyai-indent-candidates-from-backward ()
  (let* ((offs1 (hyai-possible-offsets))
         offs2
         (offset (current-indentation))
         (minoff (or (car offs1) offset))
         (poffset 0))
    (if (and offs1 (eq (char-before) ?\())
        offs1
      (unless offs1
        (push (+ offset hyai-basic-offset) offs1)
        (push offset offs1))
      (while (and (> offset hyai-basic-offset)
                  (>= (forward-line -1) 0))
        (when (and (< offset minoff) (/= offset poffset))
          (push offset offs2)
          (setq poffset offset))
        (setq offset (current-indentation)))
      (when (and (= offset hyai-basic-offset)
                 (< offset minoff))
        (push offset offs2))
      (when (< 0 minoff)
        (push 0 offs2))
      (append offs1 offs2))))

(defun hyai-current-offset-head ()
  (forward-line 0)
  (save-excursion
    (skip-syntax-forward " ")
    (if (eobp)
        '(0 . "")
      (let* ((c (char-after))
             (cc (current-column))
             (head (cl-case (char-syntax c)
                     (?w (hyai-grab-syntax-forward "w"))
                     (?_ (hyai-grab-syntax-forward "_"))
                     (?\( (string c))
                     (?\) (string c))
                     (?. (string c))
                     (t ""))))
        (cons cc head)))))

(defun hyai-search-token-backward (symbols words)
  (skip-syntax-backward " >")
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn)
       (cl-case syn
         (?> (if (/= (char-syntax (char-after)) ?\s)
                 (setq res 'stop)
               (backward-char)))
         (?w (if (null words)
                 (skip-syntax-backward "w")
               (if (member (hyai-grab-syntax-backward "w") words)
                   (progn (setq result (current-column))
                          'stop)
                 'next)))
         (?_ (if (null symbols)
                 (skip-syntax-backward "_")
               (if (member (hyai-grab-syntax-backward "_") symbols)
                   (progn (setq result (current-column))
                          'stop)
                 'next)))
         (t 'cont))))
    result))

(defun hyai-possible-offsets ()
  (let (offs prev beg curr)
    (hyai-process-syntax-backward
     (lambda (syn)
       (cl-case syn
         (?\s (setq prev (current-column))
              (skip-syntax-backward " ")
              'next)
         (?w (setq curr (current-column))
             (when (string= (hyai-grab-syntax-backward "w") "let")
               (push (or prev curr) offs)
               'stop))
         (?_ (setq curr (current-column))
             (when (member (hyai-grab-syntax-backward "_")
                           '("=" "->" "<-"))
               (push (or prev curr) offs)
               (setq beg (current-column)))
             'next)
         (?\( (setq curr (current-column))
              (push (if (= (char-syntax (char-after)) ?\s) prev curr) offs)
              'stop)
         (?> 'stop)
         (t 'cont))))
    (setq curr (current-indentation))
    (cond
     ((and beg (/= beg curr) (/= curr 0))
      (cons curr offs))
     (t offs))))

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

(defun hyai-process-syntax-backward (callback &optional limit)
  (setq limit (or limit 0))
  (let ((res 'cont))
    (while (and (not (eq res 'stop))
                (> (point) limit)
                (not (bobp)))
      (let ((syn (char-syntax (char-before))))
        (setq res (funcall callback syn))
        (when (eq res 'cont)
          (condition-case nil
              (cl-case syn
                (?> (backward-char))
                (?\) (backward-sexp))
                (?\" (backward-sexp))
                (t (skip-syntax-backward (string syn))))
            (error (setq res 'stop))))))))

(defun hyai-search-context ()
  (when (re-search-backward "^\\([^#[:space:]]+\\)" nil t)
    (let ((ctx (match-string-no-properties 1)))
      (when (member ctx '("data" "class" "import" "module" "newtype" "type"))
        ctx))))

(defun hyai-search-backward-open-bracket (across-lines)
  (catch 'result
    (while (<= (skip-syntax-backward (if across-lines "^()" "^()>")) 0)
      (let ((c (char-before)))
        (cond
         ((not c) (throw 'result nil))
         ((= (char-syntax c) ?\))
          (condition-case nil (backward-sexp)
            (error (throw 'result nil))))
         (t (backward-char)
            (throw 'result c)))))))

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

(defun hyai-offsetnize (obj &optional plus)
  (setq plus (or plus 0))
  (cond
   ((listp obj) (mapcar (lambda (x) (+ x plus)) obj))
   ((numberp obj) (list (+ obj plus)))
   (t nil)))

;;;###autoload
(define-minor-mode hyai-mode
  "Haskell Yet Another Indentation minor mode."
  :lighter " HYAI"
  (kill-local-variable 'indent-line-function)
  (when hyai-mode
    (set (make-local-variable 'indent-line-function) 'hyai-indent-line)))

;;;###autoload
(defun turn-on-hyai ()
  (interactive)
  (hyai-mode 1))

(defun turn-off-hyai ()
  (interactive)
  (hyai-mode 0))

(provide 'hyai)
;;; hyai.el ends here
