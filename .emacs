;;; Package initialization:

(package-initialize)
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

;; other-frame
(global-set-key (kbd "C-M-o") 'other-frame)

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
(global-set-key (kbd "C-c C-r") 'revert-buffer)
(setq revert-without-query '(".*"))

;; kill-this-buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; close emacs client and server
(global-set-key (kbd "C-c C-x C-c") 'save-buffers-kill-emacs)

;; next-error and previous-error
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; kill-whole-line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; modify case
(bind-key "M-c" 'capitalize-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-u" 'upcase-dwim)

;;; General variable setting:

(setq set-mark-command-repeat-pop t)

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
(menu-bar-mode -1)

;;; Edit scratch message:

(setq initial-scratch-message ";; Wash your hands!")

;;; Enable transient mark mode:

(transient-mark-mode 1)

;;; Fonts:

;;(use-package unicode-fonts)
;;(unicode-fonts-setup)

;; TODO: Dynamic font size depending on monitor resolution and size using
;; using display-monitor-attributes-list
(add-to-list 'default-frame-alist '(font . "Hack-22"))

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
  (org-tags-column 0)
  (org-export-with-tasks 'done)
  :config (;; New org heading or list item doesn't generate new line
           setf org-blank-before-new-entry '((heading . nil)
                                             (plain-list-item . nil)))
  (setq org-export-backends '(ascii html icalendar latex md odt)))

(use-package
  org-clock
  :ensure nil
  :bind ("C-c C-x C-i" . 'org-clock-in)
  ("C-c C-x C-o" . 'org-clock-out))

(use-package
  org-capture
  :ensure nil
  :bind ("C-c c" . 'org-capture)
  :config (defun my/org-daily-work-buffer ()
            "Set org-capture-target-buffer as Org buffer with today's date as name"
            (interactive)
            (let* ((file-name (format-time-string "%F.org"))
                   (file-path (expand-file-name file-name "~/work/org/daily/")))
              (set-buffer (org-capture-target-buffer file-path))
              (goto-char (point-max))))
  (setq org-capture-templates '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
                                 "* TODO %?\n  %i\n  %a")
                                ("l" "Link" plain (file "~/org/links.org") "- %?\n %x\n")
                                ("d" "Daily work task" plain (function my/org-daily-work-buffer)
                                 "* %?"))))

(use-package
  org-download
  :custom (org-download-display-inline-images nil))

;; TODO: Use it for research and configure accordingly
(use-package
  org-noter)

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

;;; isearch:

(use-package
  isearch
  :ensure nil
  :config (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq isearch-lazy-count t)
  (setq isearch-allow-scroll t))

;;; magit:

(use-package
  magit
  :bind ("C-c C-g" . 'magit-status)
  ("C-c C-j" . 'magit-status-here)
  ("C-c m b" . 'magit-blame-addition)
  ("C-c l r" . 'magit-list-repositories)
  :config (unbind-key "C-x g" magit-file-mode-map)
  (setq magit-repository-directories '(("~/work" . 3))))

;; TODO: create function to stage a region. Probably would only work by line

;;; outline-mode for config file:

;; TODO: replace outline-mode with literate org-mode

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (make-local-variable 'outline-regexp)
                                  (setq outline-regexp "^;;; ")
                                  (make-local-variable 'outline-heading-end-regexp)
                                  (setq outline-heading-end-regexp ":\n")
                                  (outline-minor-mode 1)))

;;; smerge:

(use-package
  smerge-mode
  :ensure nil
  :bind ("C-c C-n" . 'smerge-next)
  ("C-c C-p" . 'smerge-prev)
  ("C-c C-u" . 'smerge-keep-upper)
  ("C-c C-l" . 'smerge-keep-lower)
  ("C-c C-RET" . 'smerge-keep-current)
  ("C-c C-a" . 'smerge-keep-all))

;;; LaTeX mode:

(use-package
  tex
  :ensure auctex
  :custom (TeX-auto-save t)
  (TeX-parse-self t)
  (preview-image-type 'pnm)
  (preview-gs-command "/usr/bin/gs")
  (reftex-mode t)
  (TeX-fold-mode t)
  :config (setq-default TeX-master nil)
  (use-package
    latex-preview-pane
    :bind ("M-U" . 'latex-preview-pane-update)
    :hook (LaTeX-mode . latex-preview-pane-mode))
  (use-package
    company-auctex
    :hook (LaTeX-mode . company-mode)
    :init (company-auctex-init)))

;;; pdf-tools

(use-package
  pdf-tools
  :config (pdf-tools-install))

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
  :custom (aw-dispatch-always t)
  :bind ("C-c M-o" . ace-window))

;;; expand-region:

(use-package
  expand-region
  :bind ("C-=" . 'er/expand-region))

;;; multiple-cursors:

(use-package
  multiple-cursors
  :custom (mc/always-run-for-all 1)
  :bind ("C-c m c" . 'mc/edit-lines)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  ("C-c m n" . 'mc/insert-numbers)
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
(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame (load-theme 'spacemacs-dark
                                                                                   t))))
  (load-theme 'spacemacs-dark t))

;;; Rust:

(use-package
  rust-mode
  :bind ("C-c C-f" . rust-format-goto-problem)
  :custom (rust-format-on-save t)
  (rust-rustfmt-bin "~/.rustup/toolchains/nightly-2020-01-15-x86_64-unknown-linux-gnu/bin/rustfmt")
  :config (unbind-key "C-c C-n" rust-mode-map))

;; TODO: enable cargo-minor-mode when the file is Cargo.toml
;; TODO: enable cargo-minor-mode when on magit-status for a Rust project?
(use-package
  cargo
  :hook (rust-mode . cargo-minor-mode)
  :config (setq cargo-process--command-check "clippy --all-targets"))

;;; lsp-mode:

(use-package
  lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp
  :bind ("C-x C-a" . lsp-execute-code-action )
  ("C-x C-." . lsp-find-type-definition)
  :custom (lsp-prefer-flymake nil)
  (lsp-enable-snippet t)
  :config (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-rust-analyzer-cargo-override-command "clippy --all-targets --message-format=json")
  ;; To improve performance:
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500)
  ;; flycheck:
  (lsp-flycheck-enable t))

(use-package
  lsp-ui
  :after (lsp-mode flycheck)
  :commands lsp-ui-mode
  :custom (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-max-height 10)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-actions-kind-regex ".*")
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 25)
  :config (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

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

(use-package
  dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-sort-toggle-or-edit)
  ;; moving up to parent directory doesn't open another buffer
  :bind (:map dired-mode-map
              ("^" . (lambda ()
                       (interactive)
                       (find-alternate-file ".."))))
  :config (global-set-key (kbd "C-x C-j") 'dired-jump)
  ;; press a to access file/directory in same buffer in dired-mode
  (put 'dired-find-alternate-file 'disabled nil)
  (use-package
    diredfl
    :custom (diredfl-global-mode t))
  (use-package
    dired-narrow
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

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
  :custom (prettier-js-show-errors 'echo)
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
         ("M-O" . 'helm-mini)
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
  (helm-M-x-use-completion-styles nil)
  (helm-M-x-fuzzy-match t)
  (helm-flx-for-helm-find-files t)
  (helm-flx-for-helm-locate t)
  (helm-flx-for-helm-M-x t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-show-completion-display-function #'helm-show-completion-default-display-function)
  :config (require 'helm-config))

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

;;; helm-rg:

(use-package
  helm-rg)

;;; helm-projectile:

(use-package
  helm-projectile
  :ensure helm
  :bind (("C-x g" . helm-projectile-rg))
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

;;; Typescript:

(use-package
  typescript-mode
  :custom (typescript-indent-level 4)
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
  :config (add-hook 'typescript-mode-hook 'flycheck-mode)
  :bind ("C-c C-n" . 'flycheck-next-error)
  ("C-c C-p" . 'flycheck-previous-error))

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

;; do not try to find Emacs C source code
(setq find-function-C-source-directory nil)

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

;;; display-line-numbers for pair-programming:

;; (use-package
;;   display-line-numbers-mode
;;   :ensure nil
;;   :hook  (rust-mode . display-line-numbers-mode)
;;   )

;;; yaml-mode:

(use-package
  yaml-mode)

;;; restclient-mode:

(use-package
  restclient)

;;; yasnippet:

(use-package
  yasnippet
  :config (use-package
            yasnippet-snippets)
  (yas-global-mode 1))

;;; dumb-jump:

;; TODO: Use ripgrep or ag instead of grep
(use-package
  dumb-jump
  :custom (dumb-jump-mode 1)
  (dumb-jump-selector 'helm))

;;; vterm:

(use-package
  vterm
  :bind ("C-c C-s" . vterm)
  ("C-c p C-s" . projectile-run-vterm)
  ("C-c g" . vterm-send-C-g)
  :config (unbind-key "C-c C-g" vterm-mode-map))

;;; scheme:

(use-package
  geiser)
