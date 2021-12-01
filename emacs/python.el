

;; ----------  PYTHON IDE CONFIG  ----------

;; Configure python linters - mypy, flake8, pylint.
;; Run mypy -> flake8 -> pylint in a chain, regardless of errors.
(require 'flycheck)
(defun flycheck-python-disable-all ()
    (progn
        (add-to-list 'flycheck-disabled-checkers 'python-pycompile)
        (add-to-list 'flycheck-disabled-checkers 'python-mypy)
        (add-to-list 'flycheck-disabled-checkers 'python-flake8)
        (add-to-list 'flycheck-disabled-checkers 'python-pylint)))
(defun flycheck-python-enable-all ()
    (progn
        (setq-local flycheck-checker 'python-mypy)
        (flycheck-add-next-checker 'python-mypy 'python-flake8)
        (flycheck-add-next-checker 'python-flake8 'python-pylint)))
(defun enable-python-checkers ()
    (progn
        (remove-hook 'python-mode-hook 'flycheck-python-disable-all)
        (add-hook 'python-mode-hook 'flycheck-python-enable-all)))
(defun disable-python-checkers ()
    (progn
        (remove-hook 'python-mode-hook 'flycheck-python-enable-all)
        (add-hook 'python-mode-hook 'flycheck-python-disable-all)))
(disable-python-checkers)


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
    :commands (py-isort-before-save))
    ;; :init
    ;; (add-hook 'python-mode-hook 'isort-mode))

;; Use blacken to automatically format code.
;; https://github.com/proofit404/blacken
;; Minor mode hooks are set up per https://emacs.stackexchange.com/a/18897.
(use-package blacken
    :defer t
    :commands (blacken-mode)
    :init
    (add-hook 'python-mode-hook '(lambda () (setq-local blacken-line-length fill-column))))
    ;; (add-hook 'python-mode-hook #'blacken-mode))
