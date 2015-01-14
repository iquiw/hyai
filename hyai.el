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
         (in-comment (hyai-in-comment-p))
         (oh (hyai-current-offset-head))
         (offset (car oh))
         (head (cdr oh))
         indents nexts)
    (cond
     ((string= head "-}")
      (forward-line 0)
      (indent-line-to (save-excursion
                        (hyai-skip-comment-backward (syntax-ppss))
                        (current-column)))
      (when (> cc offset)
        (forward-char (- cc offset))))
     ((member head '("{-" "--"))
      (unless in-comment
        (indent-line-to offset)))
     (in-comment
      (indent-relative))
     (t
      (setq indents (hyai-indent-candidates head))
      (setq nexts (when (eq this-command 'indent-for-tab-command)
                    (cdr (member offset indents))))
      (if (null indents)
          (indent-line-to offset)
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

    (`")"
     (hyai-offsetnize (hyai-search-comma-bracket ?\))))

    (`"]"
     (hyai-offsetnize (hyai-search-comma-bracket ?\])))

    (`"}"
     (hyai-offsetnize (hyai-search-comma-bracket ?\})))

    (`","
     (hyai-offsetnize (hyai-search-comma-bracket ?,)))

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
                         (list (current-indentation) offset)))))))

      (?_ (pcase (hyai-grab-syntax-backward "_")
            (`"="
             (list (+ (current-indentation) hyai-basic-offset)))))

      (?. (let* ((off1 (hyai-previous-offset))
                 (off2 (hyai-search-comma-bracket (char-before))))
            (list (or (and off2
                           (progn
                             (forward-char)
                             (skip-syntax-forward " ")
                             (unless (eolp)
                               (current-column))))
                      off1))))

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
  (let* ((off-tok (hyai-possible-offsets))
         (offs1 (car off-tok))
         (token (cdr off-tok))
         offs2
         (offset (current-indentation))
         (minoff (or (car offs1) offset))
         (poffset 0))
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
  (save-excursion
    (forward-line 0)
    (skip-syntax-forward " ")
    (if (eobp)
        '(0 . "")
      (let* ((c (char-after))
             (cc (current-column))
             (head (cl-case (char-syntax c)
                     (?w (hyai-grab-syntax-forward "w"))
                     (?_ (if (looking-at-p "-}") "-}"
                           (hyai-grab-syntax-forward "_")))
                     (?\( (if (looking-at-p "{-") "{-" (string c)))
                     (?\) (string c))
                     (?. (string c))
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
         (?_ (if (null symbols)
                 (skip-syntax-backward "_")
               (if (member (hyai-grab-syntax-backward "_") symbols)
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
         (?_ (setq curr (current-column))
             (setq last-token (hyai-grab-syntax-backward "_"))
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

(defun hyai-search-vertical (limit)
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn c)
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
     (lambda (syn c)
       (if (= syn ?_)
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

(defun hyai-search-comma-bracket (origin)
  (let (result)
    (hyai-process-syntax-backward
     (lambda (syn c)
       (cl-case syn
         (?\s (when (hyai-botp)
                (setq result (current-column)))
              'cont)
         (?_ (if (and (= origin ?,)
                      (string= (hyai-grab-syntax-backward "_") "|"))
                 (progn
                   (setq result (current-column))
                   'stop)
               'cont))
         (?\( (backward-char)
              (cond
               ((null result) (setq result (current-column)))
               ((= origin ?,) (setq result (current-column)))
               ((= c ?\{) (setq result (current-indentation))))
              'stop)
         (?. (backward-char)
             (if (hyai-botp)
                 (progn (setq result (current-column)) 'stop)
               (setq result nil)
               'next))
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
         (comm-type (hyai-skip-comment-backward ppss))
         ((and (= c ?\}) (looking-back "-}"))
          (backward-char)
          (hyai-skip-comment-backward ppss))
         (t
          (setq res (funcall callback syn c))
          (when (eq res 'cont)
            (condition-case nil
                (cl-case syn
                  (?> (backward-char))
                  (?\) (backward-sexp))
                  (?\" (backward-sexp))
                  (t (skip-syntax-backward (string syn))))
              (error (setq res 'stop))))))))))

(defun hyai-search-context ()
  (when (re-search-backward "^\\([^#[:space:]]+\\)" nil t)
    (let ((ctx (match-string-no-properties 1)))
      (when (member ctx '("data" "class" "import" "module" "newtype" "type"))
        ctx))))

(defun hyai-previous-offset ()
  (skip-syntax-backward " >")
  (current-indentation))

(defun hyai-botp ()
  (= (current-column) (current-indentation)))

(defun hyai-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun hyai-skip-comment-backward (&optional ppss)
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
