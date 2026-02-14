

;; ----------  WHITESPACE  ----------

;; Hard-wrap text past 'fill-column' by automatically insert newlines. Apply
;; this setting to all major modes.
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq-default auto-fill-function 'do-auto-fill)

;; Don't use two spaces between sentences or after a colon.
(setq-default sentence-end-double-space nil)
(setq-default colon-double-space nil)

;; Indicate (by highlighting):
;; - long lines (the portion longer than 'fill-column').
;; - trailing whitespace.
;; https://www.emacswiki.org/emacs/WhiteSpace
(global-whitespace-mode)
;; If a list of major modes is specified for whitespace-global-modes,
;; highlighting does not work. So, enable it for all major modes.
;; (setq-default whitespace-global-modes
;;     '(markdown-mode org-mode python-mode shell-script-mode text-mode))
(setq-default whitespace-global-modes t)
(setq-default whitespace-style
    (quote (face trailing lines-tail space-before-tab::space)))
;; Long line highlighting should match line wrapping. For this, set
;; whitespace-line-column to nil, which makes highlighting use the fill-column
;; value, as set by editorconfig. Don't set to the value of fill-column.
(setq-default whitespace-line-column nil)
;; https://stackoverflow.com/a/11444423
;; (add-hook 'after-change-major-mode-hook
;;           '(lambda () (setq whitespace-line-column fill-column)))

;; Use 4 spaces instead of tabs.
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)

;; Highlight non-ASCII characters, including Unicode spaces.
;; See https://elpa.gnu.org/packages/markchars.html for smaller sets of spaces.
(global-hi-lock-mode 1)
(setq-default hi-lock-auto-select-face t)
(add-hook 'after-change-major-mode-hook
          #'(lambda () (hi-lock-set-pattern "[[:nonascii:]]" 'hi-yellow)))
