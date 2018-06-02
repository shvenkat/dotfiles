;; Python IDE configuration.

;; isort to sort import statements in the module header.
;; https://github.com/paetzke/py-isort.el
(use-package py-isort
    :defer t
    :config
    (add-hook 'before-save-hook 'py-isort-before-save))

;; blacken to automatically format the code.
;; https://github.com/proofit404/blacken
