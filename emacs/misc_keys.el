

;; ----------  MISC. KEYS  ----------

(if (featurep 'evil)
    (progn
        ;; Fast switching between two buffers.
        (evil-leader/set-key "s" 'mode-line-other-buffer)
        ;; Revert a buffer, for instance to reset syntax highlighting.
        (evil-leader/set-key "h" 'revert-buffer)
        ;; Adjust a window to be only as wide as necessary.
        (evil-leader/set-key "w" 'set-window-snug)))
