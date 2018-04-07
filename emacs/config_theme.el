;; Use the solarized color theme. Make the mode line less conspicuous.
;; https://emacs.stackexchange.com/q/28940
(use-package color-theme-solarized
    :defer t
    :init
    (load-theme 'solarized t)
    (custom-theme-set-faces
        'solarized
        '(mode-line
             ((t (:foreground "brightgreen" :background "black" :box nil))))
        '(mode-line-inactive
             ((t (:foreground "brightgreen" :background "black" :box nil))))))
