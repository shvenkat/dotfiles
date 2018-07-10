;; Add an evil keybinding for fast switching between two buffers.
(if (featurep 'evil)
    (progn
        (evil-leader/set-key "s" 'mode-line-other-buffer)
        (evil-leader/set-key "h" 'revert-buffer)
        (evil-leader/set-key "w" '(lambda () (interactive)
                                      (evil-window-set-width fill-column)
                                      (set-window-margins (selected-window) 1 1)
                                      (set-window-fringes (selected-window) 0 0)
                                      (set-window-scroll-bars (selected-window) 0 nil 0 nil)))))
