

;; ----------  WHITESPACE  ----------

;; Automatically insert newlines to maintain a maximum line length of
;; 'fill-column'. Apply this setting to all major modes.
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq-default auto-fill-function 'do-auto-fill)

;; Indicate (by highlighting):
;; - long lines (the portion longer than 'fill-column').
;; - trailing whitespace.
;; https://www.emacswiki.org/emacs/WhiteSpace
(global-whitespace-mode)
(setq-default whitespace-global-modes t)
(setq-default whitespace-style
    (quote (face trailing lines-tail space-before-tab::space)))
;; https://stackoverflow.com/a/11444423
(add-hook 'after-change-major-mode-hook
          '(lambda () (setq whitespace-line-column fill-column)))

;; Use 4 spaces instead of tabs.
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
