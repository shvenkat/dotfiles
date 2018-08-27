

;; ----------  COLOR THEME  ----------

;; Use a light background for the initial frame.
(set-frame-parameter nil 'background-mode 'light)
(set-terminal-parameter nil 'background-mode 'light)
;; Use a light background for all new frames.
(add-hook
    'after-make-frame-functions
    (lambda (frame)
        (set-frame-parameter frame 'background-mode 'light)
        (set-terminal-parameter frame 'background-mode 'light)
        (enable-theme 'solarized)))

;; Use the solarized color theme. Make the mode line less conspicuous.
;; https://emacs.stackexchange.com/q/28940
(use-package color-theme-solarized
    :defer t
    :init
    (load-theme 'solarized t)
    (custom-theme-set-faces
        'solarized
        '(mode-line
             ((t (:foreground "brightcyan" :background "white" :box nil))))
        '(mode-line-inactive
             ((t (:foreground "brightcyan" :background "white" :box nil))))))

;; Highlight matching delimiter.
(set-face-attribute 'show-paren-match nil
    :foreground (face-attribute 'default :foreground)
    :background (face-attribute 'default :background)
    :inverse-video t)
