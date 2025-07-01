

;; ----------  MAGIT  ----------

;; Use evil for vim-like keybindings.
(use-package magit
    :init
    (add-hook 'magit-mode-hook
        #'(lambda ()
              (setq-local fill-column 110)
              (setq-local display-line-numbers nil)))
    ;; Don't refresh the git status buffer unless it is active.
    (setq magit-refresh-status-buffer nil)
    (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-topleft-v1)
    (setq magit-diff-refine-hunk t)
    ;; Magit WIP mode adds a noticeable lag when saving file buffers.
    ;; (magit-wip-mode)
    )
