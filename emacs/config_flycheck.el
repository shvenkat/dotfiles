;; Flycheck.
;; http://www.flycheck.org/en/latest/
(use-package flycheck
    :defer t
    :config  ;; TODO: Perhaps init?
    (global-flycheck-mode 1)
    (if (featurep 'evil)
        (progn
            (evil-leader/set-key "c" 'flycheck-buffer)
            (evil-leader/set-key "e" 'flycheck-list-errors))))
