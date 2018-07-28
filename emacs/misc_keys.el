

;; ----------  MISC. KEYS  ----------

(if (featurep 'evil)
    (progn
        ;; Fast switching between two buffers.
        (evil-leader/set-key "s" 'mode-line-other-buffer)
        ;; Revert a buffer, for instance to reset syntax highlighting.
        (evil-leader/set-key "h" 'revert-buffer)
        ;; Adjust a window to be only as wide as necessary.
        (evil-leader/set-key "w" '(lambda () (interactive)
                                      (evil-window-set-width fill-column)
                                      (set-window-margins (selected-window) 1 1)
                                      (set-window-fringes (selected-window) 0 0)
                                      (set-window-scroll-bars (selected-window) 0 nil 0 nil)))))
