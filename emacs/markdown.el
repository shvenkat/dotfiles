

;; ----------  MARKDOWN  ----------

;; Use a markdown mode for .md and .markdown files. Use Github-flavored markdown
;; (sigh!) for README.md files.
(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode
    (("README\\.md\\'" . gfm-mode)
     ("\\.md\\'" . markdown-mode)
     ("\\.markdown\\'" . markdown-mode))
    :init
    (setq markdown-command "multimarkdown")
    (setq markdown-enable-math t)
    :config
    (add-hook 'markdown-mode-hook
        '(lambda () (set-face-attribute 'markdown-italic-face nil :underline nil))))
