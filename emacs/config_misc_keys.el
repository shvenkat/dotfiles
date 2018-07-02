;; Add an evil keybinding for fast switching between two buffers.
(if (featurep 'evil)
    (progn
        (evil-leader/set-key "s" 'mode-line-other-buffer)
        (evil-leader/set-key "h" 'revert-buffer)
        (evil-leader/set-key "w" '(lambda () (interactive) (evil-window-set-width (+ 2 fill-column))))))
