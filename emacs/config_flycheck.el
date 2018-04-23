;; Flycheck.
;; http://www.flycheck.org/en/latest/
(use-package flycheck
    :defer t
    :config  ;; TODO: Perhaps init?
    (global-flycheck-mode 1)
    (if (featurep 'evil)
        (evil-leader/set-key "e" 'flycheck-list-errors)))

;; Configure python checkers - mypy and flake8.
;; As of 2018-04-23, the python checkers included in flycheck run the
;; corresponding checker in the parent directory of the source file. This can
;; cause namespace issues and fail to use the checker config files. To fix this
;; issue, the checkers defined here run the checkers from the project root.
(require 'flycheck)

(defun flycheck-python--find-project-root (_checker)
    (and
        buffer-file-name
        (locate-dominating-file buffer-file-name "setup.py")))

(flycheck-define-checker python-mypy-project-root
  "Run mypy from the project root, followed by flake8."
    :command ("mypy" source-original)
    :working-directory flycheck-python--find-project-root
    :error-patterns
    (
        (error   line-start (file-name) ":" line ": error:"   (message) line-end)
        (warning line-start (file-name) ":" line ": warning:" (message) line-end)
        (info    line-start (file-name) ":" line ": info:"    (message) line-end))
    :modes python-mode
    :next-checkers ((error . python-flake8-project-root)))

(flycheck-define-checker python-flake8-project-root
  "Run flake8 from the project root."
    :command ("flake8" source-original)
    :working-directory flycheck-python--find-project-root
    :error-patterns
    ((warning
         line-start (file-name) ":" line ":" (optional column ":") " "
         (id (one-or-more (any alpha)) (one-or-more digit)) " "
         (message (one-or-more not-newline)) line-end))
    :modes python-mode)

;; Make python-mypy the default python checker.
(add-to-list 'flycheck-checkers 'python-flake8-project-root)
(add-to-list 'flycheck-checkers 'python-mypy-project-root)
