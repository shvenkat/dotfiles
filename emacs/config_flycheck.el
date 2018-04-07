;; Flycheck.
;; http://www.flycheck.org/en/latest/
(use-package flycheck
    :defer t
    :config  ;; TODO: Perhaps init?
    (global-flycheck-mode 1)
    (if (featurep 'evil)
        (evil-leader/set-key "e" 'flycheck-list-errors)))

;; Configure python checkers - mypy and flake8.
(require 'flycheck)
(defun flycheck-python--find-project-root (_checker)
    (and
        buffer-file-name
        (locate-dominating-file buffer-file-name "setup.py")))
(flycheck-define-checker python-mypy
  "Run mypy from the project root, followed by flake8."
    :command ("mypy" source-original)
    :working-directory flycheck-python--find-project-root
    :error-patterns
    (
        (error   line-start (file-name) ":" line ": error:"   (message) line-end)
        (warning line-start (file-name) ":" line ": warning:" (message) line-end)
        (info    line-start (file-name) ":" line ": info:"    (message) line-end))
    :modes python-mode
    :next-checkers ((error . python-flake8)))
;; Make python-mypy the default python checker.
(add-to-list 'flycheck-checkers 'python-mypy)
