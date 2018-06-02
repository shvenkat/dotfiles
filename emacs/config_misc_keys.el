;; Add an evil keybinding for fast switching between two buffers.
(if (featurep 'evil)
    (progn
        (evil-leader/set-key "s" 'mode-line-other-buffer)
        (evil-leader/set-key "h" 'revert-buffer)))
