;; Bootstrap package managers.

;; Configure repos to find and install new packages.
;; https://www.emacswiki.org/emacs/InstallingPackages
(require 'package)
(add-to-list 'package-archives
    '("MELPA" . "http://melpa.org/packages/")
    ;; '("MELPA Stable" . "https://stable.melpa.org/packages/")
)
(package-initialize)

;; To minimize startup time, do not refresh package lists unless a new package
;; is to be installed.
;; https://github.com/jwiegley/use-package/issues/256#issuecomment-263313693
(defun my-package-install-refresh-contents (&rest args)
    (package-refresh-contents)
    (advice-remove 'package-install 'my-package-install-refresh-contents))
(advice-add 'package-install :before 'my-package-install-refresh-contents)

;; Load use-package to manage package installation, loading and configuration.
;; https://github.com/jwiegley/use-package
;; https://stackoverflow.com/questions/21064916/auto-install-emacs-packages-with-melpa
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))
    ;; (progn
    ;;   (package-refresh-contents)
    ;;   (package-install 'use-package)))
(require 'use-package)
;; Install new packages automatically.
(setq use-package-always-ensure t)

;; Load QUELPA integration for use-package to install packages from source repos.
;; https://github.com/quelpa/quelpa-use-package
(use-package quelpa)
(use-package quelpa-use-package)
;; Do not automatically upgrade quelpa or melpa.
(setq quelpa-self-upgrade-p nil)
(setq quelpa-update-melpa-p nil)
(require 'quelpa-use-package)
;; Install non-quelpa packages using the default ELPA repo.
(quelpa-use-package-activate-advice)
