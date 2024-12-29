

;; ----------  MISC. KEYS  ----------

(if (featurep 'evil)
    (progn
        ;; Revert a buffer, for instance to reset syntax highlighting.
        (evil-leader/set-key "h" 'normal-mode)
        ;; Adjust a window to be only as wide as necessary.
        (evil-leader/set-key "q" 'set-window-snug)))
