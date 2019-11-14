;;; Package initialization:

(package-initialize)
;; FIXME:
;; Failed to verify signature archive-contents.sig:Failed to verify signature archive-contents.sig:
;; No public key for 066DAFCB81E42C40 created at 2019-10-15T08:10:02+1100 using RSA
;; Command output:
;; gpg: Signature made mar 15 oct 2019 08:10:02 AEDT
;; gpg:                using RSA key C433554766D3DDC64221BFAA066DAFCB81E42C40
;; gpg: Can't check signature: No public key
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; Add lisp folder to load-path:

(let ((default-directory (concat user-emacs-directory "lisp")))
  (normal-top-level-add-to-load-path '("erc" "narrow" "nmcli" "prettier" "org-asciidoc")))

;;; use-package:

;; TODO: what version should be loaded?
(eval-when-compile (add-to-list 'load-path
                                "/home/luckysori/.emacs.d/elpa/use-package-20190405.2047")
                   (require 'use-package))
(setq use-package-always-ensure t)

;;; General rebinds:

;; other-window
(global-set-key (kbd "M-o") 'other-window)

;; jump to beginning and end of buffer
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-[") 'beginning-of-buffer)
(global-set-key (kbd "M-]") 'end-of-buffer)

;; disable suspend-frame command
(global-unset-key (kbd "C-z"))

;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; revert-buffer
(global-set-key (kbd "C-c r b") 'revert-buffer)

;; kill-this-buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; close emacs client and server
(global-set-key (kbd "C-c C-x C-c") 'save-buffers-kill-emacs)

;;; diminish:

(use-package
  diminish)

;;; utf-8 as default coding system:

(prefer-coding-system 'utf-8)

;;; custom-set-variables and custom-set-faces:

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; Line wrapping:

(setq-default truncate-lines nil)

;;; Disable the splash screen:

(setq inhibit-splash-screen t)

;;; Disable toolbar, scrollbar and menubar:

(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (menu-bar-mode -1)
(when (eq system-type 'windows-nt)
  (toggle-frame-fullscreen))

;;; Enable transient mark mode:

(transient-mark-mode 1)

;;; Fonts:

;;(use-package unicode-fonts)
;;(unicode-fonts-setup)

(add-to-list 'default-frame-alist '(font . "Hack-20"))

;;; org-mode:

(use-package
  org
  :bind ("C-c a" . 'org-agenda)
  :hook (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  :custom (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE" "CANCELLED")))
  (org-todo-keyword-faces '(("TODO" . org-warning)
                            ("IN-PROGRESS" . "yellow")
                            ("BLOCKED" . "dim gray")
                            ("DONE" . org-done)
                            ("CANCELLED" . "blue")))
  :config (;; New org heading or list item doesn't generate new line
           setf org-blank-before-new-entry '((heading . nil)
                                             (plain-list-item . nil))))

(require 'ox-asciidoc)

;;; Back up files:

(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

(setq delete-old-versions t kept-new-versions 6 kept-old-versions 2 version-control t)

;;; Auto-save files:

;; TODO: figure out .auto-saves vs auto-save-list

(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory ".auto-saves/") t)))

;;; Deleted files go to recycle bin:

(setq delete-by-moving-to-trash t)

;;; Access emacs config quickly:

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)

;;; Visual bell:

(setq visible-bell 1)

;;; magit:

(use-package
  magit
  :bind ("C-c g" . 'magit-status)
  ("C-c m b" . 'magit-blame-addition))

;;; outline-mode for config file:

;; TODO: replace outline-mode with literate org-mode

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (make-local-variable 'outline-regexp)
                                  (setq outline-regexp "^;;; ")
                                  (make-local-variable 'outline-heading-end-regexp)
                                  (setq outline-heading-end-regexp ":\n")
                                  (outline-minor-mode 1)))

;;; Configuring AUCTeX:

;; TODO: update LaTeX support (this section was originally for Windows)

;; AUCTeX replaces latex-mode-hook with LaTeX-mode-hook
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq TeX-auto-save t)
                             (setq TeX-parse-self t)
                             ;; (setq-default TeX-master nil)
                             (reftex-mode t)
                             (TeX-fold-mode t)))

;;(setq preview-image-type 'pnm)

;;; Configuring LaTeX preview:

;;(setq preview-gs-command "gs/gs9.09/bin/gswin32c.exe")

;;; web-mode:

(use-package
  web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-comment-style 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-comment-keywords t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-auto-indentation t)
  (web-mode-enable-auto-quoting nil)
  :config (add-hook 'web-mode-hook 'web-mode-init-prettier-hook)
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook (lambda ()
                             (when (string-equal "tsx" (file-name-extension buffer-file-name))
                               (setup-tide-mode))))
  ;; disable default jslint
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint
                                                                                json-jsonlist)))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

;;; Indent with spaces:

(setq-default indent-tabs-mode nil)

;;; Sentences:

(setq sentence-end-double-space nil)

;;; avy:

(use-package
  avy
  :bind ("M-s" . 'avy-goto-word-1))

;;; Haskell configs:

;; TODO: fix this mess. Also make it platform agnostic (was introduced for Windows)

(setq haskell-process-type 'stack-ghci) ;; it works now because global stack config uses ghc-8.0.2

;; may cause problems!!!
(setq haskell-process-path-ghci "stack")

;;(add-to-list 'load-path "C:/Users/Lucas/AppData/Roaming/.emacs.d/elpa/hindent-5.2.5")
;;(use-package hindent)
;;(add-hook 'haskell-mode-hook #'hindent-mode)

(setq tags-revert-without-query 1)

(eval-after-load 'haskell-mode '(progn (define-key haskell-mode-map (kbd "C-c C-l")
                                         'haskell-process-load-file)
                                       (define-key haskell-mode-map (kbd "C-c C-z")
                                         'haskell-interactive-switch)
                                       (define-key haskell-mode-map (kbd "C-c C-n C-t")
                                         'haskell-process-do-type)
                                       (define-key haskell-mode-map (kbd "C-c C-n C-i")
                                         'haskell-process-do-info)
                                       (define-key haskell-mode-map (kbd "C-c C-n C-c")
                                         'haskell-process-cabal-build)
                                       (define-key haskell-mode-map (kbd "C-c C-n c")
                                         'haskell-process-cabal)
                                       (define-key haskell-mode-map (kbd "<f8>")
                                         'haskell-navigate-imports)))
(eval-after-load 'haskell-cabal '(progn (define-key haskell-cabal-mode-map (kbd "C-c C-z")
                                          'haskell-interactive-switch)
                                        (define-key haskell-cabal-mode-map (kbd "C-c C-k")
                                          'haskell-interactive-mode-clear)
                                        (define-key haskell-cabal-mode-map (kbd "C-c C-c")
                                          'haskell-process-cabal-build)
                                        (define-key haskell-cabal-mode-map (kbd "C-c c")
                                          'haskell-process-cabal)))

;;; ace-window:

;; TODO: buffer number is massive with spacemacs theme
(use-package
  ace-window
  :custom
  ;; TODO: figure out if this is working
  (aw-dispatch-always nil)
  :bind ("C-c M-o" . ace-window))

;;; expand-region:

(use-package
  expand-region
  :bind ("C-=" . 'er/expand-region))

;;; multiple-cursors:

(use-package
  multiple-cursors
  :custom (mc/always-run-for-all 1)
  :bind ("C-c m c" .'mc/edit-lines)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  :config (define-key mc/keymap (kbd "<return>") nil))

;;; Copy to end of line:

(global-set-key (kbd "C-c k")
                (kbd "C-SPC C-e M-w C-u C-SPC"))

;;; Remove ^L, ^M and ^D characters:

;; TODO: figure out if this is needed or can be done better

(defun remove-dos-eol ()
  "Do not show ^L, ^M, ^D in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^L [])
  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^D []))

(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'help-mode-hook 'remove-dos-eol)
(add-hook 'shell-mode-hook 'remove-dos-eol)

;;; Prevent shell prompt from being deleted:

(setq comint-prompt-read-only t)

;;; Attempt at fixing shell encoding:

;; TODO: figure out what this is for
;; (setq default-process-coding-system '(cp936-dos . utf-8-unix))

;;; ERC:

(use-package
  erc
  :init (and
         (require 'erc-highlight-nicknames)
         (add-to-list 'erc-modules 'highlight-nicknames)
         (erc-update-modules))
  :config (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;;; Theme:

(setq custom-safe-themes t)
(load-theme 'spacemacs-dark)

;;; Rust:

(use-package
  rust-mode
  :hook (rust-mode . cargo-minor-mode)
  (rust-mode . eldoc-mode)
  :custom (rust-format-on-save t)
  (rust-rustfmt-bin "~/.rustup/toolchains/nightly-2019-04-30-x86_64-unknown-linux-gnu/bin/rustfmt")
  :config
  ;; TODO: replace this hack
  ;; Add commit-rs to compilation-search-path
  (add-to-list 'compilation-search-path "~/work/swap/"))

;;; lsp-mode:

(use-package
  lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp
  :custom (lsp-prefer-flymake nil)
  (lsp-enable-snippet nil))

(use-package
  lsp-ui
  :after (lsp-mode flycheck)
  :commands lsp-ui-mode
  :custom (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 25))

(use-package
  company-lsp
  :commands company-lsp)

(use-package
  helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package
  dap-mode)

;;; smartparens:

(use-package
  smartparens
  :diminish smartparens-mode
  :custom (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :bind ("C-M-u" . sp-backward-up-sexp)
  ("C-M-d" . sp-down-sexp)
  ("C-M-f" . sp-forward-sexp)
  ("C-M-b" . sp-backward-sexp)
  ;; sp-kill-sexp doesn't play nice with self-closing html tags and
  ;; I don't like how hungry sp-kill-sexp is anyway
  ("C-M-k" . kill-sexp)
  ("C-M-<backspace>" . sp-backward-kill-sexp)
  ("C-(" . sp-rewrap-sexp)
  ("M-D" . sp-splice-sexp)
  ("C-M-w" . sp-copy-sexp)
  ("C-M-<SPC>" . sp-mark-sexp)
  ("C-M-t" . sp-transpose-sexp)
  :config (require 'smartparens-config)
  (global-set-key (kbd "C-M-<backspace>") 'sp-backward-kill-sexp))

;;; expand-region:

(use-package
  expand-region
  :bind ("C-=" . 'er/expand-region))

;;; dired:

;; press a to access file/directory in same buffer in dired-mode
(put 'dired-find-alternate-file 'disabled nil)

;; moving up to parent directory doesn't open another buffer
(add-hook 'dired-mode-hook (lambda ()
                             (define-key dired-mode-map (kbd "^")
                               (lambda ()
                                 (interactive)
                                 (find-alternate-file "..")))))

;; hide details and sort
(add-hook 'dired-mode-hook (lambda ()
                             (dired-hide-details-mode)
                             (dired-sort-toggle-or-edit)))

;;; global-diff-hl-mode:

;; TODO: figure out what this does
(use-package
  diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; global-subword-mode:

(global-subword-mode 1)

;;; JavaScript:

;; Default JS mode set to js2-mode
(use-package
  js2-mode
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; JS formatter

(use-package
  prettier-js
  :config (add-hook 'js2-mode-hook 'prettier-js-mode))

;; JSON:

(use-package
  json-mode
  :custom (json-reformat:indent-width 2))

(setq js-indent-level 2)

;;; helm:

(use-package
  helm
  :bind (("C-x C-f" . 'helm-find-files)
         ("C-x c b" . 'helm-resume)
         ("C-x b" . 'helm-mini)
         ("C-x C-r" . 'helm-recentf)
         ("M-x" . 'helm-M-x))
  :custom (helm-mode 1)
  (helm-autoresize-mode t)
  (helm-split-window-in-side-p t)
  (helm-autoresize-max-height 30)
  (helm-autoresize-min-height 30)
  (helm-display-header-line nil)
  (helm-echo-input-in-header-line t)
  (set-face-attribute 'helm-source-header nil
                      :height 0.1)
  (helm-flx-mode t)
  (helm-mode-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-M-x-fuzzy-match t)
  (helm-flx-for-helm-find-files t)
  (helm-flx-for-helm-locate t)
  (helm-flx-for-helm-M-x t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-show-completion-display-function #'helm-show-completion-default-display-function)
  :config (require 'helm-config)
  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell-cmpl-initialize)
                                (define-key eshell-mode-map [remap eshell-pcomplete]
                                  'helm-esh-pcomplete)
                                (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))))

(use-package
  helm-swoop
  :ensure helm
  :bind (("M-i" . 'helm-swoop-without-pre-input)
         ("M-I" . 'helm-swoop-back-to-last-point)
         ("C-c M-i" . 'helm-multi-swoop)
         ("C-x M-i" . 'helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-i" . 'helm-swoop-from-isearch)
         :map helm-swoop-map ("M-i" . 'helm-multi-swoop-all-from-helm-swoop)
         ("M-m" . 'helm-multi-swoop-current-mode-from-helm-swoop)
         ("C-r" . 'helm-previous-line)
         ("C-s" . 'helm-next-line)
         :map helm-multi-swoop-map ("C-r" . 'helm-previous-line)
         ("C-s" . 'helm-next-line))
  :custom (helm-multi-swoop-edit-save t)
  (helm-swoop-split-with-multiple-windows nil)
  (helm-swoop-split-direction 'split-window-below)
  (helm-swoop-speed-or-color t)
  (helm-swoop-move-to-line-cycle t)
  (helm-swoop-use-line-number-face nil)
  (helm-swoop-use-fuzzy-match nil))

;;; wgrep:

(use-package
  wgrep-helm)

;;; helm-projectile:

(use-package
  helm-projectile
  :ensure helm
  :config  (helm-projectile-on))

;;; exec-path-from-shell:

;; TODO: does not load path when using emacsclient
(use-package
  exec-path-from-shell
  :config
  ;; TODO: figure out if this is needed
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Markdown:

;; TODO: generalise this for all text modes
(use-package
  markdown-mode
  :config (add-hook 'markdown-mode-hook 'visual-line-mode))

;; Set markdown-command to pandoc
;; TODO: something is missing here

;;; Typescript:

(use-package
  typescript-mode
  :custom (typescript-indent-level 2)
  :config (add-hook 'typescript-mode-hook 'typescript-init-prettier-hook)
  (add-hook 'typescript-mode-hook 'setup-tide-mode))

(defun typescript-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

;;; Tide:

(use-package
  tide
  :after (exec-path-from-shell typescript-mode company flycheck)
  :config (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode))

;; TODO: Delete this and declare all this with use-package
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode)
  (flycheck-mode)
  (company-mode)
  (tide-hl-identifier-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;;; company-mode:

(use-package
  company
  :config (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t))

;; TODO: figure out why this is useful
(use-package
  company-quickhelp
  :init (company-quickhelp-mode 1)
  (use-package
    pos-tip))

;;; flycheck:

(use-package
  flycheck
  :config (add-hook 'typescript-mode-hook 'flycheck-mode))

(use-package
  flycheck-color-mode-line
  :after (flycheck)
  :config (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;; helm-ls-git:

(use-package
  helm-ls-git
  :bind ("C-c f g" . 'helm-ls-git-ls))

;;; Trying exwm:

;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)

;;; winner-mode:

(use-package
  winner
  :config (when (fboundp 'winner-mode)
            (winner-mode 1)))

;;; ibuffer:

(use-package
  ibuffer
  :bind ("C-x C-b" . ibuffer))

;;; Always add copied text to kill ring, even if it's not yanked:

(setq save-interprogram-paste-before-kill t)

;;; Follow link to vc'd file:

(setq vc-follow-symlinks t)

;;; Smooth cursor scrolling:

(use-package
  smooth-scrolling
  :config (smooth-scrolling-mode))

;;; Confirm using y and n:

(fset 'yes-or-no-p 'y-or-n-p)

;;; Clean up whitespace before saving a file:

(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Projectile:

(use-package
  projectile
  :diminish projectile-mode
  :custom (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map)))

;;; Emacs lisp:

(defun elisp-before-save-hook ()
  (when (eq major-mode 'emacs-lisp-mode)
    (elisp-format-buffer)))

(add-hook 'before-save-hook #'elisp-before-save-hook)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;;; RSS feeds:

(use-package
  elfeed
  :bind ("C-x w" . elfeed)
  :custom (elfeed-feeds '("https://irreal.org/blog/?feed=rss2"
                          "https://www.youtube.com/feeds/videos.xml?channel_id=UCI6keWArpxmfeiuAATv7jZw"
                          "https://bitcoinops.org/feed.xml")))

;;; Narrowing:

(put 'narrow-to-region 'disabled nil)
(put 'widen-to-region 'disabled nil)

(require 'narrow-indirect)

;;; nmcli-wifi:

(require 'nmcli-wifi)
