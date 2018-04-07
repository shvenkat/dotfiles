;; Use ido to quickly jump to buffers or files.
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Maintain a list of recent files and use ido to jump to any of them.
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
        (when file
            (find-file file))))

;; Add evil keybindings for file and buffer navigation.
(if (featurep 'evil)
    (progn
        (evil-leader/set-key "b" 'ido-switch-buffer)
        (evil-leader/set-key "f" 'ido-find-file)
        (evil-leader/set-key "r" 'recentf-ido-find-file)))
