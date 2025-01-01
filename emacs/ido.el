

;; ----------  FILE / BUFFER NAVIGATION  ----------

;; Display items vertically in the completion minibuffer.
(setq completions-format "vertical")

;; Use ido to quickly jump to buffers or files.
(require 'ido)
(setq
    ido-enable-flex-matching t
    ido-everywhere t
    ido-ignore-directories '("\\`.git/" "\\`__pycache__/" "\\`\\.\\./" "\\`\\./"))
(ido-mode 1)

;; Maintain a list of recent files and use ido to jump to any of them.
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
        (when file
            (find-file file))))

;; Add evil keybindings for file and buffer navigation.
(if (featurep 'evil)
    (progn
        ;; Fast switching between two buffers.
        (evil-leader/set-key "s" 'mode-line-other-buffer)
        ;; Switch buffers using ido.
        (evil-leader/set-key "b" 'ido-switch-buffer)
        ;; Rotate through windows.
        (evil-leader/set-key "w" 'evil-window-next)
        ;; Open files using ido: fuzzy search in current directory and past working directories.
        (evil-leader/set-key "f" 'ido-find-file)
        ;; Open files in the current project.
        (evil-leader/set-key "v" 'project-find-file)
        ;; Open files from the recentf history list.
        (evil-leader/set-key "r" 'recentf-ido-find-file)
        ;; Find by regexp in the current project.
        (evil-leader/set-key "x" 'project-find-regexp)))
