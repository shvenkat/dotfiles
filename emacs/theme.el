

;; ----------  COLOR THEME  ----------

;; Enable my custom theme for all new frames.
(add-hook
    'after-make-frame-functions
    (lambda (frame)
        (enable-theme 'ansi-16)))

;; Define ansi-16 theme.
(deftheme ansi-16 "A 16-color theme for light backgrounds like Solarized.")
(custom-theme-set-faces
    'ansi-16
    ;; Built-in.
    '(default
        ((t (:foreground "brightyellow" :slant normal))))
    '(error
        ((t (:foreground "brightred" :slant normal))))
    '(warning
        ((t (:foreground "yellow" :slant normal))))
    '(region
        ((t (:inverse-video t))))
    ;; font-lock.
    '(font-lock-builtin-face
        ((t (:foreground "brightyellow" :slant normal))))
    '(font-lock-comment-face
        ((t (:foreground "brightcyan" :slant normal))))
    '(font-lock-constant-face
        ((t (:foreground "brightyellow" :slant normal))))
    '(font-lock-doc-face
        ((t (:foreground "brightyellow" :background "white" :extend t :slant normal))))
    '(font-lock-function-name-face
        ((t (:foreground "brightyellow" :slant normal))))
    '(font-lock-keyword-face
        ((t (:foreground "brightyellow" :slant normal))))
    '(font-lock-preprocessor-face
        ((t (:foreground "brightyellow" :slant normal))))
    '(font-lock-string-face
        ((t (:foreground "brightyellow" :background "white" :slant normal))))
    '(font-lock-type-face
        ((t (:foreground "brightyellow" :slant normal))))
    '(font-lock-variable-name-face
        ((t (:foreground "brightyellow" :slant normal))))
    '(font-lock-warning-face
        ((t (:foreground "red" :slant normal))))
    ;; Mode line.
    '(mode-line
        ((t (:foreground "brightcyan" :background "white" :box nil))))
    '(mode-line-inactive
        ((t (:foreground "brightcyan" :background "white" :box nil))))
    )

;; Highlight matching delimiter.
;; (set-face-attribute 'show-paren-match nil
;;     :foreground (face-attribute 'default :foreground)
;;     :background (face-attribute 'default :background)
;;     :inverse-video t)
