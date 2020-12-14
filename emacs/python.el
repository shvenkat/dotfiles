

;; ----------  PYTHON IDE CONFIG  ----------

;; Configure python linters - mypy, flake8, pylint.
;; Run mypy -> flake8 -> pylint in a chain, regardless of errors.
(require 'flycheck)
(add-hook 'python-mode-hook '(lambda () (setq-local flycheck-checker 'python-mypy)))
(flycheck-add-next-checker 'python-mypy 'python-flake8)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(defun flycheck-python-disable-all ()
    (setq-default flycheck-disabled-checkers
        '(python-mypy python-flake8 python-pylint)))

;; Configure python checkers - mypy, flake8 and pylint.
;; As of 2018-04-23, the python checkers included in flycheck run the
;; corresponding checker in the same directory as the source file. This can
;; cause namespace issues and fail to use the checker config files. To fix this
;; issue, the checkers defined here run the checkers from the project root.

;; (defun flycheck-python--find-project-root (_checker)
;;     (and
;;         buffer-file-name
;;         (locate-dominating-file buffer-file-name ".git")))

;; (flycheck-define-checker python-mypy-project-root
;;   "Run mypy from the project root, followed by flake8."
;;     :command ("python3" "-m" "mypy" source-original)
;;     :working-directory flycheck-python--find-project-root
;;     :error-patterns
;;     (
;;         (error   line-start (file-name) ":" line ": error:"   (message) line-end)
;;         (warning line-start (file-name) ":" line ": warning:" (message) line-end)
;;         (info    line-start (file-name) ":" line ": note:"    (message) line-end))
;;     :modes python-mode
;;     :next-checkers ((error . python-flake8-project-root)))

;; (flycheck-define-command-checker 'python-flake8-project-root
;;   "Run flake8 from the project root, followed by pylint."
;;     :command `("python3" "-m" "flake8"
;;                   "--config"
;;                   ,(expand-file-name ".flake8"
;;                        (locate-dominating-file buffer-file-name ".flake8"))
;;                   ;; ,@(if buffer-file-name
;;                   ;;       `("--config"
;;                   ;;           ,(expand-file-name ".flake8"
;;                   ;;               (locate-dominating-file buffer-file-name
;;                   ;;                   ".flake8")))
;;                   ;;       ())
;;                   source-original)
;;     :working-directory 'flycheck-python--find-project-root
;;     :error-patterns
;;     '((warning
;;          line-start (file-name) ":" line ":" (optional column ":") " "
;;          (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;          (message (one-or-more not-newline)) line-end))
;;     :modes 'python-mode
;;     :next-checkers '((error . python-pylint-project-root)))

;; (flycheck-define-checker python-pylint-project-root
;;   "Run pylint from the project root."
;;     :command ("python3" "-m" "pylint"
;;               "--reports" "n"
;;               "--output-format" "text"
;;               "--score" "n"
;;               "--msg-template" "{path}:{line}:{column}:{C}:{msg_id}:{msg}"
;;               source-original)
;;     :working-directory flycheck-python--find-project-root
;;     :error-patterns
;;     ((error
;;          line-start (file-name) ":" line ":" column ":" (or "E" "F") ":"
;;          (id (one-or-more (not (any ":")))) ":" (message) line-end)
;;      (warning
;;          line-start (file-name) ":" line ":" column ":" (or "W" "R") ":"
;;          (id (one-or-more (not (any ":")))) ":" (message) line-end)
;;      (info
;;          line-start (file-name) ":" line ":" column ":" "C:"
;;          (id (one-or-more (not (any ":")))) ":" (message) line-end))
;;     :modes python-mode)

;; ;; Make python-mypy the default python checker.
;; (add-to-list 'flycheck-checkers 'python-pylint-project-root)
;; (add-to-list 'flycheck-checkers 'python-flake8-project-root)
;; (add-to-list 'flycheck-checkers 'python-mypy-project-root)


;; Configure python formatters - black and isort.

;; Use isort to sort import statements in the module header.
;; https://github.com/paetzke/py-isort.el
(define-minor-mode isort-mode
  "Automatically run isort before saving."
  :lighter " Isort"
  (if isort-mode
      (add-hook 'before-save-hook #'py-isort-before-save nil t)
      (remove-hook 'before-save-hook #'py-isort-before-save t)))
(use-package py-isort
    :defer t
    :commands (py-isort-before-save)
    :init
    (add-hook 'python-mode-hook 'isort-mode))

;; Use blacken to automatically format code.
;; https://github.com/proofit404/blacken
;; Minor mode hooks are set up per https://emacs.stackexchange.com/a/18897.
(use-package blacken
    :defer t
    :commands (blacken-mode)
    :init
    (add-hook 'python-mode-hook '(lambda () (setq-local blacken-line-length fill-column)))
    (add-hook 'python-mode-hook #'blacken-mode))
