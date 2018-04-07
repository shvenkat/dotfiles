;; Add an evil keybinding for fast switching between two buffers.
(if (featurep 'evil)
    (evil-leader/set-key "s" 'mode-line-other-buffer))
