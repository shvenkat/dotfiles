

;; ----------  BASH  ----------

; Disable the POSIX dash checker.
(add-hook 'sh-mode-hook
    #'(lambda () (add-to-list 'flycheck-disabled-checkers 'sh-posix-dash)))
