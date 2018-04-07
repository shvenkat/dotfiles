;; Store all backup and autosave files in the system temporary directory, using
;; the name prefix 'emacs-'.
(defconst
    emacs-tmp-dir
    (expand-file-name
        (format "emacs-%d" (user-uid))
        temporary-file-directory))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

;; Do not create lock files. This assumes files are NOT concurrently edited by
;; multiple users.
(setq create-lockfiles nil)
