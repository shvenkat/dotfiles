

;; ----------  PROJECT NAVIGATION  ----------

;; File browser.
(use-package neotree
    :config
    (if (featurep 'evil)
        (progn
            (evil-leader/set-key "t" 'neotree-toggle))
            (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
            (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
            (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
            (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)))
