;; Use Proof General to integrate with proof assistants.
;; Proof General is a multi-directory package, so the directory structure of the
;; source must be preserved when installed. With use-package and/or quelpa,
;; source files and directories must be listed explicitly.
(use-package proof-site
    :defer t
    :quelpa
    (proof-general
        :fetcher github :repo "ProofGeneral/PG"
        :files ("*.el" "acl2" "ccc" "coq" "doc" "easycrypt" "etc" "generic"
                "hol-light" "hol98" "isar" "lego" "lib" "obsolete" "pghaskell"
                "pgocaml" "pgshell" "phox" "twelf"))
    :config
    (setq proof-splash-seen t))

;; Use company-coq for additional integration.
;; https://github.com/cpitclaudel/company-coq
(use-package company-coq
    :defer t
    :config (add-hook 'coq-mode-hook #'company-coq-mode))
