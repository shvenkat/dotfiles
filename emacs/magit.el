

;; ----------  MAGIT  ----------

;; Use evil for vim-like keybindings.
(use-package magit
    :init
    ;; Don't refresh the git status buffer unless it is active.
    (setq magit-refresh-status-buffer nil)
    ;; Magit WIP mode adds a noticeable lag when saving file buffers.
    ;; (magit-wip-mode)
    )
