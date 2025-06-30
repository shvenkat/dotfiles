

;; ----------  CUSTOMIZATIONS  ----------

;; Place customizations in a separate file.
;; https://github.com/brainlessdeveloper/emacs.d/
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Keep a few context lines above and below the cursor when scrolling.
(setq scroll-margin 3)

;; Reload buffers that have changed on disk, if there are no unsaved changes.
(global-auto-revert-mode 1)
(setq auto-revert-use-notify t)
(setq auto-revert-interval 5)
(setq auto-revert-remote-files nil)
