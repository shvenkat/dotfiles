

;; ----------  CUSTOMIZATIONS  ----------

;; Place customizations in a separate file.
;; https://github.com/brainlessdeveloper/emacs.d/
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Keep a few context lines above and below the cursor when scrolling.
(setq scroll-margin 3)
