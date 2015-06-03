;;; hyai.el --- Haskell Yet Another Indentation -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 by Iku Iwasa

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
  (pcase-let* ((cc (current-column))
               (ppss (syntax-ppss))
               (`(,offset . ,head) (hyai-current-offset-head))
               (indents)
               (nexts))
    (cond
     ((string= head "-}")
      (forward-line 0)
      (indent-line-to (save-excursion
                        (hyai-goto-comment-start)
                        (current-column)))
      (when (> cc offset)
        (forward-char (- cc offset))))
     ((member head '("{-" "--"))
      (unless (hyai-in-comment-p ppss)
        (indent-line-to offset)))
     ((hyai-in-comment-p ppss) (indent-relative))
     ((hyai-in-string-p ppss) nil)
     (t
      (setq indents (hyai-indent-candidates head))
      (if (null indents)
          (indent-line-to offset)
        (when (hyai-previous-line-empty-p)
          (setq indents (hyai-cycle-zero-first indents)))
        (setq nexts (when (eq this-command 'indent-for-tab-command)
                    (cdr (member offset indents))))
        (indent-line-to (car (or nexts indents)))
        (when (> cc offset)
          (forward-char (- cc offset))))))))

(defun hyai-indent-candidates (head)
  (if (member head '("{-" "--"))
      '()
    (save-excursion
      (forward-line 0)
      (hyai-skip-space-backward)
      (if (bobp)
          '(0)
        (or (save-excursion (hyai-indent-candidates-from-current head))
            (save-excursion (hyai-indent-candidates-from-previous))
            (save-excursion (hyai-indent-candidates-from-backward)))))))

(defun hyai-indent-candidates-from-current (head)
  (pcase head
    (`"module" '(0))
    (`"where" (if (hyai-search-token-backward nil '("where"))
                  (list (+ (current-indentation) hyai-basic-offset))
                (list (+ (current-indentation) hyai-where-offset))))

    (`"then" (hyai-offsetnize (hyai-search-token-backward nil '("if"))
                              hyai-basic-offset))
    (`"else" (let ((offset (hyai-search-token-backward nil '("then"))))
               (hyai-offsetnize
                (if (equal offset (current-indentation))
                    offset
                  (hyai-search-token-backward nil '("if"))))))

    (`"in" (hyai-offsetnize (hyai-search-token-backward nil '("let"))))

    (`"("
     (let (offset)
       (save-excursion
         (when (member (hyai-search-context) '("import" "module"))
           (setq offset (current-column))
           (list (+ offset hyai-basic-offset))))))

    (`"{"
     (list (+ (hyai-previous-offset) hyai-basic-offset)))

    (`")"
     (hyai-offsetnize (hyai-search-comma-bracket ?\))))

    (`"]"
     (hyai-offsetnize (hyai-search-comma-bracket ?\])))

    (`"}"
     (hyai-offsetnize (hyai-search-comma-bracket ?\})))

    (`","
     (hyai-offsetnize (hyai-search-comma-bracket ?,)))

    ((or `"->" `"=>")
     (let (limit)
       (or (hyai-offsetnize
            (save-excursion
              (prog1 (hyai-search-token-backward '("::") nil)
                (setq limit (point)))))
           (hyai-offsetnize (hyai-search-vertical limit t))
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
  (if (bobp)
      '(0)
    (cl-case (char-syntax (char-before))
      (?w (pcase (hyai-grab-syntax-backward "w")
            (`"do"
             (list (+ (current-indentation) hyai-basic-offset)))
            (`"where"
             (if (hyai-botp)
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
                         (list (current-indentation) offset)))))
            ((or `"then" `"else")
             (if (hyai-botp)
                 (list (+ (current-column) hyai-basic-offset))
               (hyai-offsetnize
                (hyai-search-token-backward nil '("if"))
                hyai-basic-offset)))))

      (?. (pcase (hyai-grab-syntax-backward ".")
            (`"="
             (list (+ (current-indentation) hyai-basic-offset)))
            (`"->"
             (let ((off1 (hyai-search-equal-line))
                   (off2 (current-indentation)))
               (if off1
                   (list (+ off2 hyai-basic-offset) off1)
                 (list off2 (+ off2 hyai-basic-offset)))))
            (","
             (let* ((off1 (hyai-previous-offset))
                    (off2 (hyai-search-comma-bracket ?,)))
               (list (or (and off2
                              (progn
                                (forward-char)
                                (skip-syntax-forward " ")
                                (unless (eolp)
                                  (current-column))))
                         off1))))))

      (?\( (cl-case (char-before)
             (?\( (list (+ (current-column) 1)))
             ((?\{ ?\[)
              (let ((cc (current-column))
                    (offset (hyai-previous-offset)))
                (if (= offset (- cc 1))
                    (list (+ offset 2))
                  (list (+ offset hyai-basic-offset)))))))

      (?\) (cl-case (char-before)
             (?\) (when (equal (hyai-search-context) "import")
                    '(0))))))))

(defun hyai-indent-candidates-from-backward ()
  (pcase-let* ((`(,offs1 . ,token) (hyai-possible-offsets))
               (offs2)
               (`(,offset . ,head) (hyai-current-offset-head))
               (minoff (or (car offs1) offset))
               (poffset minoff))
    (if (and offs1 (member token '("(" "[" "{" "then")))
        offs1

     (when (equal token "else")
       (hyai-search-token-backward nil '("if"))
       (setq offset (current-column)))

     (unless offs1
       (push (+ offset hyai-basic-offset) offs1)
       (push offset offs1))

     (while (and (> offset hyai-basic-offset)
                 (>= (forward-line -1) 0))
       (when (and (< offset minoff) (< offset poffset)
                  (not (member head '("|" "->"))))
         (push offset offs2)
         (setq poffset offset))
       (pcase-let ((`(,o . ,h) (hyai-current-offset-head)))
         (setq offset o)
         (setq head h)))
     (when (and (= offset hyai-basic-offset)
                (< offset minoff))
       (push offset offs2))
     (when (< 0 minoff)
       (push 0 offs2))
     (append offs1 offs2))))

(defun hyai-current-offset-head ()
  (save-excursion
    (forward-line 0)
    (skip-syntax-forward " ")
    (if (eobp)
        (cons (current-column) "")
      (let* ((c (char-after))
             (cc (current-column))
             (head (cl-case (char-syntax c)
                     (?w (hyai-grab-syntax-forward "w"))
                     (?_ (hyai-grab-syntax-forward "_"))
                     (?\( (if (looking-at-p "{-") "{-" (string c)))
                     (?\) (string c))
                     (?. (if (looking-at-p "-}") "-}"
                           (hyai-grab-syntax-forward ".")))
                     (t ""))))
        (cons cc head)))))

(defun hyai-search-token-backward (symbols words)
  (skip-syntax-backward " >")
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn c)
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
         (?. (if (null symbols)
                 (skip-syntax-backward ".")
               (if (member (hyai-grab-syntax-backward ".") symbols)
                   (progn (setq result (current-column))
                          'stop)
                 'next)))
         (t 'cont))))
    result))

(defun hyai-possible-offsets ()
  (let (offs prev beg curr last-token)
    (hyai-process-syntax-backward
     (lambda (syn c)
       (cl-case syn
         (?\s (setq prev (current-column))
              (skip-syntax-backward " ")
              'next)
         (?w (setq curr (current-column))
             (setq last-token (hyai-grab-syntax-backward "w"))
             (cond
              ((member last-token '("let" "then" "else"))
               (push (or prev curr) offs) 'stop)
              (t 'next)))
         (?. (setq curr (current-column))
             (setq last-token (hyai-grab-syntax-backward "."))
             (when (member last-token '("=" "->" "<-"))
               (push (or prev curr) offs)
               (setq beg (current-column)))
             'next)
         (?\( (setq curr (current-column))
              (setq last-token (string c))
              (push (if (= (char-syntax (char-after)) ?\s) prev curr) offs)
              'stop)
         (?> 'stop)
         (t 'cont))))
    (setq curr (current-indentation))
    (cons
     (cond
      ((and beg (/= beg curr) (/= curr 0))
       (cons curr offs))
      (t offs))
     last-token)))

(defun hyai-search-vertical (limit &optional after-blank)
  (let (result prev)
    (hyai-process-syntax-backward
     (lambda (syn c)
       (cl-case syn
         (?\s
          (if after-blank
                 (progn (setq prev (current-column))
                        (skip-syntax-backward " ")
                        'next)
               'cont))
         (?. (let ((s (hyai-grab-syntax-backward ".")))
               (when (string= s "|")
                 (push (or prev (current-column)) result))
               'next))
         (t 'cont)))
     limit)
    (cl-remove-duplicates result)))

(defun hyai-search-vertical-equal (limit)
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn c)
       (if (= syn ?.)
         (let ((s (hyai-grab-syntax-backward ".")) offset)
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

(defun hyai-search-equal-line ()
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn c)
       (cl-case syn
         (?> (setq result nil)
             'stop)
         (?\s (setq result (current-column))
              (skip-syntax-backward " ")
              'next)
         (?. (if (string= (hyai-grab-syntax-backward ".") "=")
                 'stop
               'next))
         (t 'cont))))
    result))

(defun hyai-search-comma-bracket (origin)
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn c)
       (cl-case syn
         (?\s (when (hyai-botp)
                (setq result (current-column)))
              'cont)
         (?\( (backward-char)
              (cond
               ((null result) (setq result (current-column)))
               ((= origin ?,) (setq result (current-column)))
               ((= c ?\{) (setq result (current-indentation))))
              'stop)
         (?. (pcase (hyai-grab-syntax-backward ".")
               (`"|" (if (= origin ?,)
                         (progn (setq result (current-column)) 'stop)
                       'next))
               (`","
                (if (hyai-botp)
                    (progn (setq result (current-column)) 'stop)
                  (setq result nil) 'next))
               (_ 'next)))
         (?> (backward-char)
             (skip-syntax-backward " ")
             'next)
         (t 'cont))))
    result))

(defun hyai-skip-space-backward ()
  (hyai-process-syntax-backward
   (lambda (syn c)
     (cl-case syn
       (?\s (skip-syntax-backward " ")
            'next)
       (?> (backward-char)
           (skip-syntax-backward " ")
           'next)
       (t 'stop)))))

(defun hyai-process-syntax-backward (callback &optional limit)
  (setq limit (or limit 0))
  (let ((res 'cont))
    (while (and (null (eq res 'stop))
                (> (point) limit)
                (null (bobp)))
      (let* ((c (char-before))
             (syn (char-syntax c))
             (ppss (syntax-ppss))
             (comm-type (nth 4 ppss)))
        (cond
         (comm-type (hyai-goto-comment-start ppss))
         ((and (= c ?\}) (looking-back "-}"))
          (backward-char)
          (hyai-goto-comment-start ppss))
         (t
          (setq res (funcall callback syn c))
          (when (eq res 'cont)
            (condition-case nil
                (cl-case syn
                  (?> (backward-char))
                  (?\) (backward-sexp))
                  (?\" (backward-sexp))
                  (t (if (= c ?')
                         (backward-sexp)
                       (skip-syntax-backward (string syn)))))
              (error (setq res 'stop))))))))))

(defun hyai-search-context ()
  (catch 'result
    (while (re-search-backward "^\\([^#[:space:]]+\\)" nil t)
      (unless (hyai-in-comment-p)
        (throw 'result (match-string-no-properties 1))))))

(defun hyai-previous-offset ()
  (skip-syntax-backward " >")
  (current-indentation))

(defun hyai-botp ()
  (= (current-column) (current-indentation)))

(defun hyai-in-string-p (&optional ppss)
  (nth 3 (or ppss (syntax-ppss))))

(defun hyai-in-comment-p (&optional ppss)
  (nth 4 (or ppss (syntax-ppss))))

(defun hyai-goto-comment-start (&optional ppss)
  (let ((p (nth 8 (or ppss (syntax-ppss)))))
    (when p (goto-char p))))

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

(defun hyai-cycle-zero-first (indents)
  (or
   (catch 'result
     (let (lst i (rest indents))
       (while (setq i (car rest))
         (if (= i 0)
             (throw 'result (nconc rest (nreverse lst)))
           (push i lst))
         (setq rest (cdr rest)))))
   indents))

(defun hyai-previous-line-empty-p ()
  (save-excursion
    (and (>= (forward-line -1) 0)
         (looking-at-p "^[[:space:]]*$"))))

;;;###autoload
(define-minor-mode hyai-mode
  "Haskell Yet Another Indentation minor mode."
  :lighter " HYAI"
  (kill-local-variable 'indent-line-function)
  (when hyai-mode
    (set (make-local-variable 'indent-line-function) 'hyai-indent-line)))

(provide 'hyai)
;;; hyai.el ends here
