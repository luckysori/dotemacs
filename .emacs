;;; straight package manager bootstrapping:

(setf straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(setq straight-host-usernames '((github . "luckysori")))

;;; use-package config manager:

(straight-use-package 'use-package)

;;; Garbage Collector Magic Hack:

(use-package gcmh :init (gcmh-mode 1))

;;; Add priv folder to load-path:

;; TODO: Consider using use-package's :load-path instead
(defvar private-dir (concat user-emacs-directory "priv")
  "Private elisp directory")

(if (file-exists-p private-dir)
    (add-to-list 'load-path private-dir))

;;; General rebinds:

;; other-window
(global-set-key (kbd "C-c M-o") 'other-window)

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

;; horizontal-recenter
;; grabbed from https://stackoverflow.com/a/1249665
(defun my/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len
         (save-excursion
           (end-of-line)
           (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window) (- cur mid)))))

(global-set-key (kbd "C-S-l") 'my/horizontal-recenter)

;; modify case
(bind-key "M-c" 'capitalize-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-u" 'upcase-dwim)

;;; Make it easier to work with .dir-locals.el:

;; TODO: Replace with sidecar-locals by ideasman42!
(setq enable-local-variables :all)
(setq enable-local-eval :all)

;;; General variable setting:

;; Ensure that the minibuffer (and the echo area) do not grow if there
;; is a long message to display
;;
;; TODO: This is not great, as it interferes with `straight.el'.
(setq resize-mini-windows nil)

(setq warning-minimum-level :error)

(setq undo-limit 800000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 120000000)

(setq set-mark-command-repeat-pop t)

(setq scroll-preserve-screen-position t)

;; speed up next-line
;; (see https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag)
(setq auto-window-vscroll nil)

;;; utf-8 as default coding system:

(prefer-coding-system 'utf-8)

;;; Don't store customizations:

(use-package cus-edit+ :defer t :custom (custom-file null-device))

;;; Line wrapping:

;; Enabled so that `lsp-ui-mode' doesn't cause code to jump around.
;; Not sure if I want this everywhere.
(setq-default truncate-lines t)

;;; Disable the splash screen:

(setq inhibit-splash-screen t)

;;; Disable toolbar, scrollbar and menubar:

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; Disable lockfiles:

(setq create-lockfiles nil)

;;; Scratch buffer:

(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn
        (bury-buffer)
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'immortal-scratch)

(setq initial-scratch-message ";; scratch")

;;; Enable transient mark mode:

(transient-mark-mode 1)

;;; Fonts:

;; TODO: Dynamic font size depending on monitor resolution and size using
;; using display-monitor-attributes-list
(add-to-list 'default-frame-alist '(font . "Hack-20"))
(set-face-attribute 'default nil :family "Hack")
(set-face-attribute 'italic nil
                    :slant 'italic
                    :underline nil
                    :family "Hack")

;;; org-mode:

(use-package
 org
 :bind
 ("C-c a" . 'org-agenda)
 ("C-M-<return>" . 'org-insert-subheading)
 :hook ((org-mode . visual-line-mode) (org-mode . org-indent-mode))
 :custom
 (org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE" "CANCELLED")))
 (org-todo-keyword-faces
  '(("TODO" . org-todo)
    ("DONE" . org-done)
    ("IN-PROGRESS" . 'modus-themes-fg-yellow-intense)
    ("BLOCKED" . 'modus-themes-fg-magenta-faint)
    ("CANCELLED" . 'modus-themes-fg-blue-intense)))
 (org-tags-column 0)
 (org-export-with-tasks t)
 (org-md-headline-style 'setext)
 :config (unbind-key "C-c RET" org-mode-map)
 ;; TODO: Make this work
 (define-key org-mode-map (kbd "<M-return>") nil)
 ;; New org heading or list item doesn't generate new line
 (setf org-blank-before-new-entry
       '((heading . nil) (plain-list-item . nil)))
 (setq org-export-backends '(ascii html icalendar latex md odt))
 (setq org-highlight-latex-and-related
       `(latex script entities native))
 (require 'agenda-files nil 'noerror))

(use-package
 org-clock
 :straight (org-clock :type built-in)
 :bind
 (("C-c C-x C-i" . 'org-clock-in) ("C-c C-x C-o" . 'org-clock-out)))

(use-package
 org-capture
 :straight (org-capture :type built-in)
 :bind ("C-c c" . 'org-capture)
 :config
 (setq org-capture-templates
       '(("w"
          "Work task"
          entry
          (file+olp "~/notes/work.org" "Tasks")
          "* TODO %?\n %i\n %a")
         ("h"
          "Home task"
          entry
          (file+olp "~/notes/home.org" "Tasks")
          "* TODO %?\n %i\n %a")
         ("k"
          "Work knowledge"
          entry
          (file+olp "~/notes/work.org" "Knowledge")
          "* %?\n %i\n %a")
         ("n"
          "Home knowledge"
          entry
          (file+olp "~/notes/home.org" "Knowledge")
          "* %?\n %i\n %a"))))

(use-package
 org-download
 :custom (org-download-display-inline-images nil))

(use-package org-tree-slide)

;;; org-asciidoc:

(use-package
 ox-asciidoc
 :straight
 (ox-asciidoc :type git :host github :repo "yashi/org-asciidoc"))

;;; org-pomodoro:

(use-package
 org-pomodoro
 :custom (org-pomodoro-manual-break t) (org-pomodoro-play-sounds nil))

;;; Back up files:

(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

(setq
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;; Auto-save files:

;; TODO: Figure out .auto-saves vs auto-save-list

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".auto-saves/") t)))

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
 :straight (isearch :type built-in)
 :config
 (setq search-highlight t)
 (setq isearch-lazy-highlight t)
 (setq isearch-lazy-count t)
 (setq isearch-allow-scroll t)
 (setq search-whitespace-regexp ".*?"))

;;; interactive regexp builder:

(use-package
 re-builder
 :straight (re-builder :type built-in)
 :config (setq reb-re-syntax 'string))

;;; magit:

(use-package
 magit
 :straight (:branch "main")
 :bind
 (("C-c C-m" . 'magit-status)
  ("C-x C-m" . 'magit-dispatch)
  ("C-c C-j" . 'magit-status-here)
  ("C-c m b" . 'magit-blame-addition))
 :config (unbind-key "C-x g" magit-section-mode-map)
 (setq magit-repository-directories
       '(("~/github/" . 2)
         ("~/prolog/" . 1)
         ("~/programming/" . 2)
         ("~/gitea/" . 2)
         ("~/.emacs.d/straight/repos/" . 1)))
 (setq magit-revision-insert-related-refs-display-alist
       '((parents . t)
         (merged . nil)
         (contained . t)
         (follows . t)
         (precedes . t))))

(use-package transient :straight (:branch "main"))

(use-package
 magit-delta
 :hook (magit-mode . magit-delta-mode)
 :custom (magit-delta-hide-plus-minus-markers nil))

;; TODO: Create function to stage a region. Would probably only work by line

;;; git-timemachine:

(use-package git-timemachine)

;;; outline-mode for files using emacs-lisp-mode such as .emacs:

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (make-local-variable 'outline-regexp)
   (setq outline-regexp "^;;; ")
   (make-local-variable 'outline-heading-end-regexp)
   (setq outline-heading-end-regexp ":\n")
   (outline-minor-mode 1)))

;;; smerge:

(use-package
 smerge-mode
 :straight (smerge-mode :type built-in)
 :bind
 (("C-c C-x C-n" . 'smerge-next)
  ("C-c C-x C-p" . 'smerge-prev)
  ("C-c C-u" . 'smerge-keep-upper)
  ("C-c C-l" . 'smerge-keep-lower)
  ("C-c C-RET" . 'smerge-keep-current)
  ("C-c C-a" . 'smerge-keep-all)))

;;; ediff:

(use-package
 ediff
 :straight (ediff :type built-in)
 :config
 (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;; LaTeX mode:

(use-package
 tex
 :straight auctex
 :init
 (reftex-mode t)
 (TeX-fold-mode t)
 :custom
 (TeX-auto-save t)
 (TeX-parse-self t)
 (preview-image-type 'pnm)
 (preview-gs-command "/usr/bin/gs")
 :config (setq-default TeX-master nil)
 ;; (use-package latex-preview-pane
 ;;   :bind ("M-U" . 'latex-preview-pane-update)
 ;;   :hook (LaTeX-mode . latex-preview-pane-mode)
 ;; )
 (use-package
  company-auctex
  :hook (LaTeX-mode . company-mode)
  :init (company-auctex-init)))

;;; pdf-tools:

(use-package pdf-tools :config (pdf-tools-install))

;;; web-mode:

(use-package
 web-mode
 :mode
 (("\\.html?\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  ("\\.jsx\\'" . web-mode))
 :custom
 (web-mode-markup-indent-offset 2)
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
 :config
 (unbind-key "C-c RET" web-mode-map)
 (add-hook 'flycheck-mode-hook 'add-node-modules-path)
 (add-hook
  'web-mode-hook
  (lambda ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))
 ;; disable default jslint
 (setq-default flycheck-disabled-checkers
               (append
                flycheck-disabled-checkers
                '(javascript-jshint json-jsonlist)))
 ;; enable typescript-tslint checker
 (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package add-node-modules-path)

;;; Indent with spaces:

(setq-default indent-tabs-mode nil)

;;; Sentences:

(setq sentence-end-double-space nil)

;;; avy:

(use-package avy :bind ("M-s" . 'avy-goto-char-timer))

;;; ace-window:

(use-package
 ace-window
 :bind ("M-o" . ace-window)
 :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; expand-region:

(use-package expand-region :bind ("C-=" . 'er/expand-region))

;;; multiple-cursors:

(use-package
 multiple-cursors
 :custom (mc/always-run-for-all 1)
 :bind
 (("C-c m c" . 'mc/edit-lines)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  ("C-c m n" . 'mc/insert-numbers))
 :config (define-key mc/keymap (kbd "<return>") nil))

;;; Copy to end of line:

(global-set-key (kbd "C-c k") (kbd "C-SPC C-e M-w C-u C-SPC"))

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

;;; ERC:

(use-package
 erc
 :straight (erc :type built-in)
 :init
 (and (use-package erc-highlight-nicknames)
      (add-to-list 'erc-modules 'highlight-nicknames)
      (erc-update-modules))
 :config (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;;; Themes:

(setq custom-safe-themes t)

(use-package ef-themes :config (load-theme 'ef-rosa t))

(use-package modus-themes)

;;; Rust:

(use-package
 rust-mode
 :straight
 (rust-mode
  :type git
  :flavor melpa
  :host github
  :remote "luckysori"
  :repo "luckysori/rust-mode")
 :custom (rust-format-on-save t)
 :config (unbind-key "C-c C-n" rust-mode-map))

;; TODO: Enable cargo-minor-mode when the file is Cargo.toml
;; TODO: Enable cargo-minor-mode when on magit-status for a Rust project?
(use-package
 cargo
 :hook (rust-mode . cargo-minor-mode)
 :config
 (setq cargo-process--command-check "clippy --all-targets"))

(use-package toml-mode)

;;; Dart:

(use-package dart-mode)
(use-package lsp-dart)

(use-package
 dart-format
 :straight
 (dart-format :type git :host github :repo "luckysori/dart-format"))

;;; lsp-mode:

(use-package
 lsp-mode
 :init
 ;; prevent warnings caused by lsp-execute-code-action keybinding
 (setq gud-key-prefix (kbd "C-c C-x C-a"))
 (setq lsp-keymap-prefix "C-c l")
 :hook
 (rust-mode . lsp)
 (c++-mode .lsp)
 (go-mode . lsp)
 (dart-mode . lsp)
 (lsp-mode . lsp-enable-which-key-integration)
 :commands lsp
 :bind-keymap ("C-c l" . lsp-command-map)
 :bind
 ("C-x C-a" . lsp-execute-code-action)
 ("C-x C-." . lsp-find-type-definition)
 ("C-x C-r" . lsp-rename)
 ("C-x C-," . helm-lsp-workspace-symbol)
 :custom
 (lsp-file-watch-threshold 10000)
 (lsp-keep-workspace-alive nil)
 (lsp-enable-snippet t)
 (lsp-prefer-capf t)
 (lsp-headerline-breadcrumb-enable nil)
 ;; rust
 (lsp-rust-clippy-preference "on")
 (lsp-rust-analyzer-cargo-watch-command "clippy")
 (lsp-rust-analyzer-proc-macro-enable t)
 (lsp-rust-all-features t)
 (lsp-rust-analyzer-import-granularity "item")
 (lsp-rust-all-features nil)
 (lsp-rust-analyzer-import-prefix "by_crate")
 (lsp-rust-analyzer-completion-auto-import-enable t)
 ;; TODO: Consider setting
 ;; `lsp-rust-analyzer-completion-add-call-parenthesis' to nil.
 ;; (lsp-rust-target-dir "/tmp/lsp-rust-target")
 ;; To improve performance
 (gc-cons-threshold 100000000)
 (read-process-output-max (* 1024 1024))
 (lsp-idle-delay 0.500)
 :config
 (with-eval-after-load 'lsp-mode
   (lsp-register-client
    (make-lsp-client
     :new-connection (lsp-stdio-connection "nixd")
     :major-modes '(nix-mode)
     :priority 0
     :server-id 'nixd)))
 (lsp-register-client
  (make-lsp-client
   :new-connection
   (lsp-stdio-connection
    (list
     "swipl"
     "-g"
     "use_module(library(lsp_server))."
     "-g"
     "lsp_server:main"
     "-t"
     "halt"
     "--"
     "stdio"))
   :major-modes '(prolog-mode)
   :priority 1
   :multi-root t
   :server-id 'prolog-ls)))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))
(advice-add
 (if (progn
       (require 'json)
       (fboundp 'json-parse-buffer))
     'json-parse-buffer
   'json-read)
 :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and
         (not test?) ;; for check lsp-server-present?
         (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
         lsp-use-plists
         (not (functionp 'json-rpc-connection)) ;; native json-rpc
         (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add
 'lsp-resolve-final-command
 :around #'lsp-booster--advice-final-command)

(use-package
 lsp-ui
 :after (lsp-mode flycheck)
 :commands lsp-ui-mode
 :bind ("C-x C-d" . lsp-ui-doc-toggle)
 :custom
 ;; doc
 (lsp-ui-doc-enable t)
 (lsp-ui-doc-show-with-mouse nil)
 (lsp-ui-doc-show-with-cursor nil)
 (lsp-ui-doc-use-childframe t)
 (lsp-ui-doc-position 'top)
 (lsp-ui-doc-include-signature t)
 (lsp-ui-doc-max-height 10)
 (lsp-ui-doc-delay 0.5)
 ;; sideline
 (lsp-ui-sideline-show-diagnostics t)
 (lsp-ui-sideline-enable t)
 (lsp-ui-sideline-show-hover nil)
 (lsp-ui-sideline-ignore-duplicate t)
 (lsp-ui-sideline-show-code-actions nil)
 (lsp-ui-sideline-update-mode 'line)
 ;; modeline
 (lsp-modeline-code-actions-mode t)
 (lsp-modeline-code-actions-kind-regex ".*")
 (lsp-modeline-code-actions-segments '(count))
 (lsp-modeline-workspace-status-mode t)
 (lsp-modeline-diagnostics-mode nil)
 ;;flycheck
 (lsp-ui-flycheck-list-position 'right)
 (lsp-ui-flycheck-live-reporting t)
 ;;peek
 (lsp-ui-peek-enable t)
 (lsp-ui-peek-list-width 60)
 (lsp-ui-peek-peek-height 25)
 :config
 (define-key
  lsp-ui-mode-map
  [remap xref-find-definitions]
  #'lsp-ui-peek-find-definitions)
 (define-key
  lsp-ui-mode-map
  [remap xref-find-references]
  #'lsp-ui-peek-find-references)
 ;; Ensure that code is not pushed around. Based on
 ;; https://github.com/emacs-lsp/lsp-ui/issues/597#issuecomment-1094139301.
 (defun lsp-ui-sideline--compute-height nil
   '(height unspecified)))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;;; dap-mode:

(use-package
 dap-mode
 :config
 (dap-ui-mode)
 (dap-ui-controls-mode 1)
 (require 'dap-lldb)
 (require 'dap-gdb-lldb)
 (dap-gdb-lldb-setup)
 (dap-register-debug-template
  "Rust::LLDB Run Configuration"
  (list
   :type "lldb"
   :request "launch"
   :name "LLDB::Run"
   :gdbpath "rust-lldb")))

;;; smartparens:

(use-package
 smartparens
 :init
 (smartparens-global-mode)
 (show-smartparens-global-mode)
 :bind
 ("C-M-u" . sp-backward-up-sexp)
 ("C-M-d" . sp-down-sexp)
 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)
 ;; sp-kill-sexp doesn't play nice with self-closing html tags and
 ;; I don't like how hungry sp-kill-sexp is anyway
 ("C-M-k" . kill-sexp)
 ("C-<backspace>" . sp-backward-kill-sexp)
 ("C-(" . sp-rewrap-sexp)
 ("M-D" . sp-splice-sexp)
 ("C-M-w" . sp-copy-sexp)
 ("C-M-<SPC>" . sp-mark-sexp)
 ("C-M-t" . sp-transpose-sexp)
 ("C-M-]" . sp-unwrap-sexp)
 :config (require 'smartparens-config))

;;; dired:

(use-package
 dired
 :straight (dired :type built-in)
 :hook
 (dired-mode . dired-hide-details-mode)
 (dired-mode . dired-sort-toggle-or-edit)
 ;; moving up to parent directory doesn't open another buffer
 :bind
 (:map
  dired-mode-map
  ("^" .
   (lambda ()
     (interactive)
     (find-alternate-file ".."))))
 :config
 (global-set-key (kbd "C-x C-j") 'dired-jump)
 (setq dired-dwim-target t)
 ;; press a to access file/directory in same buffer in dired-mode
 (put 'dired-find-alternate-file 'disabled nil)
 (use-package diredfl :custom (diredfl-global-mode t))
 (use-package
  dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow))))

;;; global-diff-hl-mode:

(use-package
 diff-hl
 :hook
 ((after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))
 :config
 (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
 (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; global-subword-mode:

(use-package
 subword
 :straight (subword :type built-in)
 :config (global-subword-mode 1))

;;; JavaScript:

;; Default JS mode set to js2-mode
(use-package
 js2-mode
 :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;;; JSON:

(use-package
 json-mode
 :custom (json-reformat:indent-width 2) (js-indent-level 2))

;;; helm:

(use-package
 helm
 :bind
 (("C-x C-f" . 'helm-find-files)
  ("C-x c b" . 'helm-resume)
  ("M-O" . 'helm-mini)
  ("M-x" . 'helm-M-x)
  ("C-c C-y" . 'helm-show-kill-ring)
  (:map helm-find-files-map ("C-c C-s" . my/helm-vterm)))
 :init (helm-mode 1)
 :custom
 (helm-autoresize-mode t)
 (helm-split-window-in-side-p t)
 (helm-autoresize-max-height 30)
 (helm-autoresize-min-height 30)
 (helm-display-header-line nil)
 (helm-echo-input-in-header-line t)
 (set-face-attribute 'helm-source-header nil :height 0.1)
 (helm-mode-fuzzy-match t)
 (helm-completion-in-region-fuzzy-match t)
 (helm-M-x-use-completion-styles nil)
 (helm-show-completion-display-function
  #'helm-show-completion-default-display-function)
 (helm-move-to-line-cycle-in-source nil)
 :config
 (defun my/helm-vterm ()
   "Open vterm in helm directory"
   (interactive)
   (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (progn
         (let
             ;; TODO: this action doesn't always open the vterm
             ;; buffer in the expected directory
             ((default-directory (file-name-directory candidate)))
           (vterm))))))))

(use-package
 helm-swoop
 :bind
 (("M-i" . 'helm-swoop-without-pre-input)
  ("M-I" . 'helm-swoop-back-to-last-point)
  ("C-c M-i" . 'helm-multi-swoop)
  ("C-x M-i" . 'helm-multi-swoop-all)
  :map
  isearch-mode-map
  ("M-i" . 'helm-swoop-from-isearch)
  :map
  helm-swoop-map
  ("M-i" . 'helm-multi-swoop-all-from-helm-swoop)
  ("M-m" . 'helm-multi-swoop-current-mode-from-helm-swoop)
  ("C-r" . 'helm-previous-line)
  ("C-s" . 'helm-next-line)
  :map
  helm-multi-swoop-map
  ("C-r" . 'helm-previous-line)
  ("C-s" . 'helm-next-line))
 :custom
 (helm-multi-swoop-edit-save t)
 (helm-swoop-split-with-multiple-windows nil)
 (helm-swoop-split-direction 'split-window-below)
 (helm-swoop-speed-or-color t)
 (helm-swoop-move-to-line-cycle t)
 (helm-swoop-use-line-number-face nil)
 (helm-swoop-use-fuzzy-match nil))

;;; wgrep:

(use-package wgrep-helm)

;;; helm-ag:

(use-package
 helm-ag
 :bind
 ("C-x g" . helm-do-ag-project-root)
 ("C-x G" . helm-do-ag)
 ("C-x M-g" . helm-ag-pop-stack)
 :config
 (setq helm-ag-base-command "rg --ignore-case --no-heading --trim")
 (setq helm-ag-success-exit-status '(0 2)))

;;; helm-projectile:

(use-package helm-projectile :config (helm-projectile-on))

;;; exec-path-from-shell:

;; TODO: Does not load path when using emacsclient
(use-package
 exec-path-from-shell
 :config
 ;; TODO: Figure out if this is needed
 (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))

;;; Markdown:

(use-package
 markdown-mode
 ;; TODO: Generalise this for all text modes
 :config
 (add-hook 'markdown-mode-hook 'visual-line-mode)
 (unbind-key "M-RET" markdown-mode-map))

;;; Typescript:

(use-package
 typescript-mode
 :custom (typescript-indent-level 2)
 :config (add-hook 'typescript-mode-hook 'setup-tide-mode))

;;; Tide:

(use-package
 tide
 :after (exec-path-from-shell typescript-mode company flycheck)
 :config
 (add-hook 'typescript-mode-hook 'tide-setup)
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
 :hook (after-init . global-company-mode)
 :bind ("C-." . company-complete)
 :config
 (setq company-show-numbers t)
 (setq company-tooltip-align-annotations t)

 (use-package
  company-quickhelp
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip)
  :custom (company-quickhelp-max-lines 4))

 (use-package
  helm-company
  :config (define-key company-active-map (kbd "C-/") 'helm-company)))


;;; flycheck:

(use-package
 flycheck
 :config (add-hook 'typescript-mode-hook 'flycheck-mode)
 :bind
 ("C-c C-n" . 'flycheck-next-error)
 ("C-c C-p" . 'flycheck-previous-error))

(use-package
 flycheck-color-mode-line
 :after (flycheck)
 :config
 (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;; helm-ls-git:

(use-package
 helm-ls-git
 :bind ("C-c f g" . 'helm-browse-project)
 :custom (helm-ls-git-status-command 'magit-status-setup-buffer))

;;; winner-mode:

(use-package
 winner
 :config
 (when (fboundp 'winner-mode)
   (winner-mode 1)))

;;; ibuffer:

(use-package ibuffer :bind ("C-x C-b" . ibuffer))

;;; Always add copied text to kill ring, even if it's not yanked:

(setq save-interprogram-paste-before-kill t)

;;; Follow link to vc'd file:

(setq vc-follow-symlinks t)

;;; Confirm using y and n:

(fset 'yes-or-no-p 'y-or-n-p)

;;; Clean up whitespace before saving a file:

(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Projectile:

;; TODO: Configure projectile-commander to look for cargo aliases
(use-package
 projectile
 :init (projectile-mode 1)
 :bind
 (:map projectile-mode-map ("C-c p" . 'projectile-command-map)))

;;; Emacs lisp:

(use-package
 elisp-def
 :config
 (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
   (add-hook hook #'elisp-def-mode)))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook
 (emacs-lisp-mode . elisp-autofmt-mode)
 (lisp-data-mode . elisp-autofmt-mode))

;; do not try to find Emacs C source code
(setq find-function-C-source-directory nil)

;;; RSS feeds:

(use-package
 elfeed
 :bind ("C-x w" . elfeed)
 :config (require 'elfeeds nil 'noerror))

;;; Narrowing:

(put 'narrow-to-region 'disabled nil)
(put 'widen-to-region 'disabled nil)

(use-package narrow-indirect)

;;; prot's display-line-numbers mode:

(use-package
 display-line-numbers
 :straight nil
 ;; TODO: Allow to enable and disable this mode via a command
 ;; :hook (prog-mode . prot/display-line-numbers-mode)
 :config
 ;; Set absolute line numbers.  A value of "relative" is also useful.
 (setq display-line-numbers-type 't)
 (define-minor-mode prot/display-line-numbers-mode
   "Toggle `display-line-numbers-mode' and `hl-line-mode'."
   :init-value nil
   :global
   nil
   (if prot/display-line-numbers-mode
       (progn
         (display-line-numbers-mode 1)
         (hl-line-mode 1))
     (display-line-numbers-mode -1)
     (hl-line-mode -1)))
 :bind ("<f7>" . prot/display-line-numbers-mode))

;; (define-globalized-minor-mode
;;   my/pair-programming-mode
;;   prot/display-line-numbers-mode
;;   :init-value nil
;;   (lambda ()
;;     (when derived-mode-p
;;       'prog-mode)
;;     (prot/display-line-numbers-mode 1)))

;;; yaml-mode:

(use-package yaml-mode)

;;; restclient-mode:

(use-package
 restclient
 :config
 ;; From https://github.com/purcell/emacs.d/blob/a97dc5a44242f7f78c70335a9532bc657ea0a8d8/lisp/init-http.el#L9
 (defun my/restclient ()
   (interactive)
   (with-current-buffer (get-buffer-create "*restclient*")
     (restclient-mode)
     (pop-to-buffer (current-buffer)))))

;;; yasnippet:

(use-package
 yasnippet
 :config (use-package yasnippet-snippets) (yas-global-mode 1))

;;; dumb-jump:

;; TODO: Use ripgrep or ag instead of grep
(use-package
 dumb-jump
 :init (dumb-jump-mode 1)
 :disabled
 :config (setq dumb-jump-selector 'helm))

;;; vterm:

(use-package
 vterm
 :load-path "~/.emacs.d/straight/repos/emacs-libvterm"
 :bind
 ("C-c C-s" . vterm-other-window)
 ("C-c p C-s" . projectile-run-vterm)
 ("C-c g" . vterm-send-C-g)
 :custom (vterm-max-scrollback 100000)
 :config
 (unbind-key "C-c C-g" vterm-mode-map)
 (unbind-key "M-O" vterm-mode-map)
 (unbind-key "C-M-m" vterm-mode-map))

;;; eldoc:

(use-package eldoc :straight (eldoc :type built-in))

;;; nmcli-mode:

(use-package
 nmcli-wifi
 :straight
 (nmcli-wifi :type git :host github :repo "luckysori/nmcli-wifi"))

;;; dprint-fmt:

(use-package
 dprint-fmt
 :straight
 (dprint-fmt :type git :host github :repo "luckysori/dprint-fmt"))

;;; clojure:

(use-package cider)

;;; Enhanced help mode:

(use-package
 helpful
 :config
 (global-set-key (kbd "C-h f") 'helpful-callable)
 (global-set-key (kbd "C-h v") 'helpful-variable)
 (global-set-key (kbd "C-h k") 'helpful-key))

;;; Dictionary client:

(use-package dictionary)

;;; PlantUML:

(use-package
 plantuml-mode
 :custom
 (plantuml-jar-path "~/plantuml.jar")
 (plantuml-default-exec-mode 'jar)
 (plantuml-output-type "svg"))

;;; compilation-mode:

(use-package
 compile
 :straight (compile :type built-in)
 :custom (compilation-environment '("TERM=xterm-256color"))
 :config
 (defun my/advice-compilation-filter (f proc string)
   (funcall f proc (xterm-color-filter string)))
 (advice-add
  'compilation-filter
  :around #'my/advice-compilation-filter))

;;; xterm-color:

(use-package xterm-color)

;;; olivetti:

(use-package olivetti)

(use-package
 conf-mode
 :straight (conf-mode :type built-in)
 :config
 (unbind-key "C-c C-a" conf-mode-map)
 (unbind-key "C-c C-u" conf-mode-map)
 (unbind-key "C-c C-p" conf-mode-map)
 (unbind-key "C-c C-j" conf-mode-map)
 (unbind-key "C-c C-s" conf-mode-map))

(use-package git-link :custom (git-link-use-commit t))

(use-package
 savehist
 :straight (savehist :type built-in)
 :init (savehist-mode 1)
 :config (setq savehist-file (concat user-emacs-directory "savehist"))
 (setq savehist-autosave-interval nil) ; save on kill only
 (setq savehist-additional-variables
       '(kill-ring
         search-ring regexp-search-ring shell-command-history)))

(use-package
 browse-url
 :straight (browse-url :type built-in)
 :config
 (defun my/browse (text)
   (interactive "sSearch: ")
   (let* (
          ;; TODO: a string such as "google.com" should be parsed as
          ;; the host but is instead identified as the filename
          (candidate (url-generic-parse-url text))
          (is-url (not (null (url-type candidate))))
          url)
     (if is-url
         (setq url text)
       (setq url (concat "https://www.google.com/search?q=" text)))
     (browse-url-xdg-open url))))

(use-package
 dash
 :config
 (with-eval-after-load 'info-look
   (dash-register-info-lookup)))

;;; nix:

(use-package nix-mode)

;;; go:

(use-package go-mode)

;;; sdcv-mode:

(use-package
 sdcv-mode
 :straight
 (sdcv-mode :type git :host github :repo "gucong/emacs-sdcv")
 :custom (sdcv-dictionary-path "/home/luckysori/.stardict/"))

;;; vundo:

(use-package
 vundo
 :straight (vundo :type git :host github :repo "casouri/vundo"))

;;; Mode-line:

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:propertize
                 (""
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote)
                 display (min-width (5.0)))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(use-package
 moody
 :config (moody-replace-mode-line-buffer-identification))

(use-package minions :config (minions-mode 1))

(defun my/toggle-local-modeline ()
  (interactive)
  (if (null mode-line-format)
      (kill-local-variable 'mode-line-format)
    (setq-local mode-line-format nil)
    (force-mode-line-update)))

;;; edbi:

(use-package edbi)

(use-package edbi-sqlite :after edbi)

;;; sidecar-locals:

;; TODO: Make it work
;; (use-package sidecar-locals
;;   :config
;;   (sidecar-locals-mode)
;;   (setq sidecar-locals-paths-allow (list "~/" "~/github/*")))

(defun my/toggle-window-split ()
  (interactive)
  (unless (= (count-windows) 2)
    (error ("Can only toggle window split for exactly 2 windows")))
  (let ((is-horizontal
         (or (window-in-direction 'right)
             (window-in-direction 'left)))
        (original (current-buffer)))
    (other-window 1)
    (delete-other-windows)
    (if is-horizontal
        (split-window-below)
      (split-window-right))
    (switch-to-buffer original)))

(defun my/swap-buffers ()
  (interactive)
  (let ((visible-buffers (window-list)))
    (cond
     ((not (= (length visible-buffers) 2))
      (message "This command requires exactly two visible buffers."))
     (t
      (let ((current-buffer (current-buffer))
            (other-window-buffer (window-buffer (next-window))))
        (set-window-buffer (selected-window) other-window-buffer)
        (set-window-buffer (next-window) current-buffer)
        (select-window (next-window)))))))

(use-package
 advent
 :straight
 (advent
  :type git
  :host github
  :repo "luckysori/advent"
  :files ("advent.el")))

(use-package
 prolog
 :straight (prolog :type built-in)
 :config (add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode)))

(use-package just-mode)

(use-package justl)

(use-package
 gptel
 :custom (gptel-model "gpt-4o")
 :config
 (setq gptel-api-key 'my/chat-gpt-api-key)
 (setq gptel-default-mode 'org-mode)
 (require 'chat-gpt-api-key nil 'noerror)
 (gptel-make-anthropic "Claude" :stream t :key 'my/claude-api-key)
 (require 'claude-api-key nil 'noerror))

;;; which-key:

(use-package which-key)

;;; LSP mode for the LTEX Language Server:

(use-package
 lsp-ltex
 :hook
 (markdown-mode . lsp)
 (rst-mode . lsp)
 :init (setq lsp-ltex-version "16.0.0"))

;;; Zone-words:

(use-package
 zone-words
 :straight
 (zone-words
  :type git
  :host github
  :repo "xenodium/dotsies"
  :files ("emacs/ar/zone-words*")))

;;; rg.el:

(use-package rg :custom (rg-group-result nil))

;;; logview:

(use-package
 logview
 :custom
 (logview-additional-timestamp-formats
  '(("RustTimestamp" (java-pattern . "yyyy-MM-dd HH:mm:ss"))))
 (logview-additional-submodes
  '(("Rust" (format .
             "TIMESTAMP LEVEL NAME: MESSAGE")
     (levels . "SLF4J")
     (timestamp . "RustTimestamp")))))

;;; docker:

(use-package docker)

;;; prettier:

(use-package prettier)

;; Local variables:
;; elisp-autofmt-load-packages-local: ("use-package")
;; end:
