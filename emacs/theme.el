

;; ----------  COLOR THEME  ----------

;; Define ansi-16 theme.
(deftheme ansi-16 "A 16-color theme for light backgrounds like Solarized.")
(custom-theme-set-faces
    'ansi-16
    ;; Built-in.
    '(default
        ((t (:foreground "brightgreen" :slant normal))))
    '(error
        ((t (:foreground "brightred" :slant normal))))
    '(warning
        ((t (:foreground "yellow" :slant normal))))
    '(shadow
        ((t (:foreground "brightcyan" :slant normal))))
    '(region
        ((t (:inverse-video t))))
    '(show-paren-match
        ((t (:foreground "brightred" :background "white"))))
    '(help-key-binding
        ((t (:foreground "brightblack" :background "white"))))
    '(line-number
        ((t (:foreground "white"))))
    '(line-number-major-tick
        ((t (:foreground "brightcyan" :background "brightwhite" :weight normal))))
    '(line-number-current-line
        ((t (:foreground "white"))))
    ;; font-lock.
    '(font-lock-builtin-face
        ((t (:foreground "brightgreen" :slant normal))))
    '(font-lock-comment-face
        ((t (:foreground "brightcyan" :slant normal))))
    '(font-lock-constant-face
        ((t (:foreground "brightgreen" :slant normal))))
    '(font-lock-doc-face
        ((t (:foreground "brightgreen" :background "white" :extend t :slant normal))))
    '(font-lock-function-name-face
        ((t (:foreground "brightgreen" :slant normal))))
    '(font-lock-keyword-face
        ((t (:foreground "brightgreen" :slant normal))))
    '(font-lock-preprocessor-face
        ((t (:foreground "brightgreen" :slant normal))))
    '(font-lock-string-face
        ((t (:foreground "brightgreen" :background "white" :slant normal))))
    '(font-lock-type-face
        ((t (:foreground "brightgreen" :slant normal))))
    '(font-lock-variable-name-face
        ((t (:foreground "brightgreen" :slant normal))))
    '(font-lock-warning-face
        ((t (:foreground "red" :slant normal))))
    ;; Mode line.
    '(mode-line
        ((t (:foreground "brightcyan" :background "white" :box nil))))
    '(mode-line-inactive
        ((t (:foreground "brightcyan" :background "white" :box nil))))
    ;; Occur/Replace.
    '(match
        ((t (:foreground "blue" :background unspecified))))
    ;; Magit.
    '(magit-section
         ((t (:foreground "black" :background "brightwhite"))))
    '(magit-section-highlight
         ((t (:foreground "black" :background "brightcyan"))))
    '(magit-diff-file-heading
         ((t (:foreground "brightblack" :background "brightwhite"))))
    '(magit-diff-file-heading-highlight
         ((t (:foreground "brightblack" :background "white"))))
    '(magit-diff-hunk-heading
         ((t (:foreground "blue" :background "brightwhite"))))
    '(magit-diff-hunk-heading-highlight
         ((t (:foreground "blue" :background "white"))))
    '(magit-diff-added
         ((t (:foreground "cyan" :background "brightwhite"))))
    '(magit-diff-added-highlight
         ((t (:foreground "cyan" :background "white"))))
    '(magit-diff-removed
         ((t (:foreground "magenta" :background "brightwhite"))))
    '(magit-diff-removed-highlight
         ((t (:foreground "magenta" :background "white"))))
    '(magit-diff-context
         ((t (:foreground "brightcyan" :background "brightwhite"))))
    '(magit-diff-context-highlight
         ((t (:foreground "black" :background "brightwhite"))))
    '(magit-diff-file-heading-selection
         ((t (:foreground "brightblack" :background "brightcyan"))))
    '(magit-diff-file-hunk-selection
         ((t (:foreground "blue" :background "brightcyan"))))
    '(magit-diff-lines-heading
         ((t (:foreground "blue" :background "brightcyan"))))
    '(magit-diff-lines-boundary
         ((t (:foreground "blue" :background "brightcyan"))))
    '(magit-diff-hunk-region
         ((t (:background "brightcyan")))))

;; Highlight matching delimiter.
;; (set-face-attribute 'show-paren-match nil
;;     :foreground (face-attribute 'default :foreground)
;;     :background (face-attribute 'default :background)
;;     :inverse-video t)

;; Enable my custom theme for the initial frame and all new ones.
(enable-theme 'ansi-16)
(add-hook
    'after-make-frame-functions
    (lambda (frame)
        (enable-theme 'ansi-16)))
