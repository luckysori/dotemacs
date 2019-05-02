;;; Package initialization:

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; use-package:

(eval-when-compile (add-to-list 'load-path "/home/luckysori/.emacs.d/elpa/use-package-2.3") 
                   (require 'use-package))
(setq use-package-always-ensure t)

;;; utf-8 as default coding system:

(prefer-coding-system 'utf-8)

;;; custom-set-variables and custom-set-faces:

(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)

;;; Line wrapping:

(setq-default truncate-lines nil)

;;; Disable the splash screen:

(setq inhibit-splash-screen t)

;;; Disable toolbar, scrollbar and menubar:

(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (menu-bar-mode -1)

;;; Enable transient mark mode:

(transient-mark-mode 1)

;;; Fonts:

;;(use-package unicode-fonts)
;;(unicode-fonts-setup)

(add-to-list 'default-frame-alist '(font . "Hack-14"))

;;; org-mode:

(use-package 
  org 
  :bind ("C-c a" . 'org-agenda) 
  :config (;; New org heading or list item doesn't generate new line
           setf org-blank-before-new-entry '((heading . nil) 
                                             (plain-list-item . nil))))

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
  :bind ("C-c g" . 'magit-status))

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
  :hook ((web-mode . (lambda () 
                       (when (string-equal "tsx" (file-name-extension buffer-file-name)) 
                         (setup-tide-mode)))) 
         (web-mode . web-mode-init-prettier-hook) 
         (flycheck-mode . add-node-modules-path)) 
  :config
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

;;; Beginning/end of buffer:

(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-[") 'beginning-of-buffer)
(global-set-key (kbd "M-]") 'end-of-buffer)

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

;;; Disable suspend-frame command:

(global-unset-key (kbd "C-z"))

;;; ace-window:

;; TODO: buffer number is massive with spacemacs theme
(use-package 
  ace-window 
  :custom
  ;; TODO: figure out if this is working
  (aw-dispatch-always t) 
  :bind ("M-o" . ace-window))

;;; emacs maximised on startup:

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; expand-region:

(use-package 
  expand-region 
  :bind ("C-=" . 'er/expand-region))

;;; multiple-cursors:

(use-package 
  multiple-cursors 
  :bind ("C-c m c" .'mc/edit-lines) 
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
  :init (add-to-list 'load-path (concat user-emacs-directory "lisp")) 
  (and 
   (require 'erc-highlight-nicknames) 
   (add-to-list 'erc-modules 'highlight-nicknames) 
   (erc-update-modules)) 
  :config (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;;; Theme:

(load-theme 'spacemacs-dark)

;;; Rust:

(use-package 
  rust-mode 
  :custom (rust-disable-format-on-save t) 
  (rust-rustfmt-bin "~/.cargo/bin/rustfmt")
  ;; (company-tooltip-align-annotations t)
  :hook ((rust-mode-hook . racer-mode) 
         (rust-mode-hook . cargo-minor-mode) 
         (racer-mode-hook . eldoc-mode)
         ;; this is too slow
         ;; (racer-mode-hook . company-mode)
         ;; TODO: figure out whether I want to use this or not
         ;; (rust-mode-hook . yas-minor-mode)
         ) 
  :config
  ;; TODO: replace this hack
  ;; Add commit-rs to compilation-search-path
  (add-to-list 'compilation-search-path "~/work/swap/")

  ;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

  ;; (add-to-list 'load-path (concat user-emacs-directory "lisp/rust-snippets"))
  ;; (autoload 'rust-snippets/initialize "rust-snippets")
  ;; (eval-after-load 'yasnippet
  ;;   '(rust-snippets/initialize))
  )

;;; smartparens:

(use-package 
  smartparens 
  :custom ((smartparens-global-mode t)) 
  :bind ("C-M-u" . sp-backward-up-sexp) 
  ("C-M-d" . sp-down-sexp) 
  ("C-M-f" . sp-forward-sexp) 
  ("C-M-b" . sp-backward-sexp) 
  ("C-M-k" . sp-kill-sexp) 
  ("C-K" . sp-kill-hybrid-sexp) 
  ("C-(" . sp-rewrap-sexp) 
  ("M-D" . sp-splice-sexp) 
  ("C-M-w" . sp-copy-sexp) 
  ("C-M-<SPC>" . sp-mark-sexp) 
  ("C-M-t" . sp-transpose-sexp) 
  :config (require 'smartparens-config)
  (defun sp-backward-kill-sexp () 
    "Kill the balanced expression behind point." 
    (interactive) 
    (sp-backward-sexp) 
    (sp-kill-sexp)) 
  (global-set-key (kbd "C-M-<backspace>") 'sp-backward-kill-sexp))

;;; expand-region:

(use-package 
  expand-region 
  :bind ("C-=" . 'er/expand-region))

;;; dired:

;; press a to access file/directory in same buffer in dired-mode
(put 'dired-find-alternate-file 'disabled nil)

;;; global-diff-hl-mode:

;; TODO: figure out what this does
(setq global-diff-hl-mode 1)

;;; global-subword-mode:

(setq global-subword-mode 1)

;;; JavaScript:

;; Default JS mode set to js2-mode
(use-package 
  js2-mode 
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; JS formatter

;; TODO: express this using use-package if possible
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)

;;; helm:

(use-package 
  helm 
  :bind (( "C-x C-f" . 'helm-find-files) 
         ( "C-x c b" . 'helm-resume) 
         ( "C-x b" . 'helm-mini) 
         ( "C-x C-r" . 'helm-recentf) 
         ( "M-x" . 'helm-M-x)) 
  :custom (helm-autoresize-mode t) 
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
  (helm-completion-in-region-fuzzy-match t))

(use-package 
  helm-swoop 
  :ensure helm 
  :bind (( "M-i" . 'helm-swoop) 
         ( "M-I" . 'helm-swoop-back-to-last-point) 
         ( "C-c M-i" . 'helm-multi-swoop) 
         ( "C-x M-i" . 'helm-multi-swoop-all) 
         :map isearch-mode-map 
         ( "M-i" . 'helm-swoop-from-isearch) 
         :map helm-swoop-map ( "M-i" . 'helm-multi-swoop-all-from-helm-swoop) 
         ( "M-m" . 'helm-multi-swoop-current-mode-from-helm-swoop) 
         ( "C-r" . 'helm-previous-line) 
         ( "C-s" . 'helm-next-line) 
         :map helm-multi-swoop-map ( "C-r" . 'helm-previous-line) 
         ( "C-s" . 'helm-next-line)) 
  :custom (helm-multi-swoop-edit-save t) 
  (helm-swoop-split-with-multiple-windows nil) 
  (helm-swoop-split-direction 'split-window-vertically) 
  (helm-swoop-speed-or-color t) 
  (helm-swoop-move-to-line-cycle t) 
  (helm-swoop-use-line-number-face nil) 
  (helm-swoop-use-fuzzy-match nil))

;;; wgrep:

(use-package 
  wgrep-helm)

;;; exec-path-from-shell:

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
  :hook ((markdown-mode-hook . visual-line-mode)))

;; Set markdown-command to pandoc
;; TODO: something is missing here

;;; Typescript:

(use-package 
  typescript-mode 
  :hook ((typescript-mode-hook . add-node-modules-path)))

;; TODO: delete if above works
;; (eval-after-load 'typescript-mode '(add-hook 'typescript-mode-hook
;;                                                        #'add-node-modules-path))

;;; Tide:

(use-package 
  tide 
  :after (typescript-mode company flycheck) 
  :hook ((typescript-mode . tide-setup) 
         (typescript-mode . tide-hl-identifier-mode)))

(defun setup-tide-mode () 
  (interactive) 
  (tide-setup) 
  (flycheck-mode +1) 
  (setq flycheck-check-syntax-automatically '(save mode-enabled)) 
  (eldoc-mode +1) 
  (tide-hl-identifier-mode +1) 
  (company-mode +1))

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
  :hook( (typescript-mode-hook . flycheck-mode)))

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

;;; hippie-expand:

(global-set-key (kbd "M-/") 'hippie-expand)

;;; Always add copied text to kill ring, even if it's not yanked:

(setq save-interprogram-paste-before-kill t)