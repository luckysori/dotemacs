;;; emacs-profiles.el --- Emacs configuration profiles -*- lexical-binding: t; -*-

;; This file handles the profile-based package activation system.
;; It defines package enable flags and sets them based on the EMACS_PROFILE environment variable.

;;; Package enable flags:

;; Default: disable all packages (enabled based on EMACS_PROFILE)
(defvar my/enable-org nil)
(defvar my/enable-org-download nil)
(defvar my/enable-org-tree-slide nil)
(defvar my/enable-org-asciidoc nil)
(defvar my/enable-org-pomodoro nil)
(defvar my/enable-magit nil)
(defvar my/enable-auctex nil)
(defvar my/enable-pdf-tools nil)
(defvar my/enable-add-node-modules-path nil)
(defvar my/enable-ace-window nil)
(defvar my/enable-expand-region nil)
(defvar my/enable-multiple-cursors nil)
(defvar my/enable-erc nil)
(defvar my/enable-rust nil)
(defvar my/enable-dart-mode nil)
(defvar my/enable-lsp-mode nil)
(defvar my/enable-lsp-ui nil)
(defvar my/enable-lsp-tailwindcss nil)
(defvar my/enable-smartparens nil)
(defvar my/enable-diff-hl nil)
(defvar my/enable-json-mode nil)
(defvar my/enable-exec-path-from-shell nil)
(defvar my/enable-markdown-mode nil)
(defvar my/enable-corfu nil)
(defvar my/enable-flycheck nil)
(defvar my/enable-flycheck-color-mode-line nil)
(defvar my/enable-winner nil)
(defvar my/enable-ibuffer nil)
(defvar my/enable-projectile nil)
(defvar my/enable-elisp-def nil)
(defvar my/enable-elisp-autofmt nil)
(defvar my/enable-elfeed nil)
(defvar my/enable-display-line-numbers nil)
(defvar my/enable-yaml-mode nil)
(defvar my/enable-restclient nil)
(defvar my/enable-yasnippet nil)
(defvar my/enable-vterm nil)
(defvar my/enable-eldoc nil)
(defvar my/enable-nmcli-wifi nil)
(defvar my/enable-dprint-fmt nil)
(defvar my/enable-nix-fmt nil)
(defvar my/enable-cider nil)
(defvar my/enable-helpful nil)
(defvar my/enable-dictionary nil)
(defvar my/enable-plantuml-mode nil)
(defvar my/enable-xterm-color nil)
(defvar my/enable-olivetti nil)
(defvar my/enable-git-link nil)
(defvar my/enable-nix-mode nil)
(defvar my/enable-go-mode nil)
(defvar my/enable-sdcv-mode nil)
(defvar my/enable-vundo nil)
(defvar my/enable-moody nil)
(defvar my/enable-minions nil)
(defvar my/enable-edbi nil)
(defvar my/enable-advent nil)
(defvar my/enable-prolog nil)
(defvar my/enable-just-mode nil)
(defvar my/enable-justl nil)
(defvar my/enable-gptel nil)
(defvar my/enable-which-key nil)
(defvar my/enable-lsp-ltex nil)
(defvar my/enable-zone-words nil)
(defvar my/enable-rg nil)
(defvar my/enable-logview nil)
(defvar my/enable-docker nil)
(defvar my/enable-envrc nil)
(defvar my/enable-treesit nil)
(defvar my/enable-vertico nil)
(defvar my/enable-consult nil)
(defvar my/enable-consult-flycheck nil)
(defvar my/enable-marginalia nil)
(defvar my/enable-orderless nil)
(defvar my/enable-embark nil)
(defvar my/enable-apheleia nil)
(defvar my/enable-wgrep nil)
(defvar my/enable-eat nil)
(defvar my/enable-claude-code nil)
(defvar my/enable-vc-jj nil)
(defvar my/enable-lean4-mode nil)
(defvar my/enable-toml-mode nil)

;;; Profile activation logic:

;; Parse environment variables to set configuration flavor
(let ((emacs-profile (getenv "EMACS_PROFILE")))
  (cond
   ((and emacs-profile (string= emacs-profile "MAGIT"))
    ;; MAGIT profile: only enable magit and git-related packages
    (setq
     my/enable-magit t
     my/enable-git-link t
     my/enable-diff-hl t
     my/enable-vc-jj t))
   ;; FULL profile or no profile specified: enable all packages
   ((or (not emacs-profile)
        (string= emacs-profile "")
        (string= emacs-profile "FULL"))
    (setq
     my/enable-org t
     my/enable-org-download t
     my/enable-org-tree-slide t
     my/enable-org-asciidoc t
     my/enable-org-pomodoro t
     my/enable-magit t
     my/enable-auctex t
     my/enable-pdf-tools t
     my/enable-add-node-modules-path t
     my/enable-ace-window t
     my/enable-expand-region t
     my/enable-multiple-cursors t
     my/enable-erc t
     my/enable-rust t
     my/enable-dart-mode t
     my/enable-lsp-mode t
     my/enable-lsp-ui t
     my/enable-lsp-tailwindcss t
     my/enable-smartparens t
     my/enable-diff-hl t
     my/enable-json-mode t
     my/enable-exec-path-from-shell t
     my/enable-markdown-mode t
     my/enable-corfu t
     my/enable-flycheck t
     my/enable-flycheck-color-mode-line t
     my/enable-winner t
     my/enable-ibuffer t
     my/enable-projectile t
     my/enable-elisp-def t
     my/enable-elisp-autofmt t
     my/enable-elfeed t
     my/enable-display-line-numbers t
     my/enable-yaml-mode t
     my/enable-restclient t
     my/enable-yasnippet t
     my/enable-vterm t
     my/enable-eldoc t
     my/enable-nmcli-wifi t
     my/enable-dprint-fmt t
     my/enable-nix-fmt t
     my/enable-cider t
     my/enable-helpful t
     my/enable-dictionary t
     my/enable-plantuml-mode t
     my/enable-xterm-color t
     my/enable-olivetti t
     my/enable-git-link t
     my/enable-nix-mode t
     my/enable-go-mode t
     my/enable-sdcv-mode t
     my/enable-vundo t
     my/enable-moody t
     my/enable-minions t
     my/enable-edbi t
     my/enable-advent t
     my/enable-prolog t
     my/enable-just-mode t
     my/enable-justl t
     my/enable-gptel t
     my/enable-which-key t
     my/enable-lsp-ltex t
     my/enable-zone-words t
     my/enable-rg t
     my/enable-logview t
     my/enable-docker t
     my/enable-envrc t
     my/enable-treesit t
     my/enable-vertico t
     my/enable-consult t
     my/enable-consult-flycheck t
     my/enable-marginalia t
     my/enable-orderless t
     my/enable-embark t
     my/enable-apheleia t
     my/enable-wgrep t
     my/enable-eat t
     my/enable-claude-code t
     my/enable-vc-jj t
     my/enable-lean4-mode t
     my/enable-toml-mode t))
   ;; Unknown profile: warn user and fall back to minimal configuration
   (t
    (when emacs-profile
      (message "WARNING: Unknown EMACS_PROFILE value: '%s'"
               emacs-profile)
      (message
       "Supported profiles: MAGIT, FULL, or unset (defaults to FULL)")
      (message
       "Falling back to minimal configuration (no packages enabled)")
      (sit-for 3)) ; Give user time to see the warning
    ;; All packages remain disabled (nil) by default
    )))

(provide 'emacs-profiles)

;;; emacs-profiles.el ends here
