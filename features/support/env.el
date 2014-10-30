(require 'f)

(defvar hyai-support-path
  (f-dirname load-file-name))

(defvar hyai-features-path
  (f-parent hyai-support-path))

(defvar hyai-root-path
  (f-parent hyai-features-path))

(add-to-list 'load-path hyai-root-path)

(require 'hyai)
(require 'espuds)
(require 'ert)
(require 'haskell-mode)

(Setup
 (defvar hyai-test-candidates-output)
 (switch-to-buffer
  (get-buffer-create "*hyai*"))
 (haskell-mode))

(Before
 (setq hyai-test-candidates-output nil))
