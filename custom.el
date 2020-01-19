(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0"
                            "#b2b2b2"])
 '(auth-source-save-behavior nil)
 '(comint-process-echoes t)
 '(compilation-message-face 'default)
 '(compilation-search-path '("nil"))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(emojify-emoji-styles '(ascii github unicode))
 '(erc-modules '(fill log highlight-nicknames completion netsplit fill button match track readonly
                      networks ring autojoin noncommands irccontrols move-to-prompt stamp menu
                      list))
 '(erc-nick "luckysori")
 '(fci-rule-color "#eee8d5")
 '(frame-brackground-mode 'dark)
 '(helm-completing-read-handlers-alist '((describe-function . helm-completing-read-symbols)
                                         (describe-variable . helm-completing-read-symbols)
                                         (describe-symbol . helm-completing-read-symbols)
                                         (debug-on-entry . helm-completing-read-symbols)
                                         (find-function . helm-completing-read-symbols)
                                         (disassemble . helm-completing-read-symbols)
                                         (trace-function . helm-completing-read-symbols)
                                         (trace-function-foreground . helm-completing-read-symbols)
                                         (trace-function-background . helm-completing-read-symbols)
                                         (find-tag . helm-completing-read-default-find-tag)
                                         (org-capture . helm-org-completing-read-tags)
                                         (org-set-tags . helm-org-completing-read-tags)
                                         (ffap-alternate-file)
                                         (tmm-menubar)
                                         (find-file)
                                         (find-file-at-point .
                                                             helm-completing-read-sync-default-handler)
                                         (ffap . helm-completing-read-sync-default-handler)
                                         (execute-extended-command)
                                         (dired-do-rename . helm-read-file-name-handler-1)
                                         (dired-do-copy . helm-read-file-name-handler-1)
                                         (dired-do-symlink . helm-read-file-name-handler-1)
                                         (dired-do-relsymlink . helm-read-file-name-handler-1)
                                         (dired-do-hardlink . helm-read-file-name-handler-1)
                                         (basic-save-buffer . helm-read-file-name-handler-1)
                                         (write-file . helm-read-file-name-handler-1)
                                         (write-region . helm-read-file-name-handler-1)
                                         (read-file-name . helm-completing-read-symbols)))
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25)
                                  '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16"
                                    "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors '(("#eee8d5" . 0)
                           ("#B4C342" . 20)
                           ("#69CABF" . 30)
                           ("#69B7F0" . 50)
                           ("#DEB542" . 60)
                           ("#F2804F" . 70)
                           ("#F771AC" . 85)
                           ("#eee8d5" . 100)))
 '(hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name
                                                                       try-expand-all-abbrevs
                                                                       try-expand-dabbrev
                                                                       try-expand-list
                                                                       try-expand-line
                                                                       try-expand-dabbrev-all-buffers
                                                                       try-expand-dabbrev-from-kill
                                                                       try-complete-lisp-symbol-partially
                                                                       try-complete-lisp-symbol))
 '(hl-bg-colors '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(input-method-highlight-flag nil)
 '(magit-diff-use-overlays nil)
 '(mc/always-run-for-all 1)
 '(org-agenda-files '("~/org/general.org" "~/work/org/tasks.org"))
 '(org-download-display-inline-images nil)
 '(org-export-with-tasks 'done)
 '(org-tags-column 0)
 '(org-todo-keyword-faces '(("TODO" . org-warning)
                            ("IN-PROGRESS" . "yellow")
                            ("BLOCKED" . "dim gray")
                            ("DONE" . org-done)
                            ("CANCELLED" . "blue")))
 '(org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE" "CANCELLED")))
 '(package-selected-packages '(org-noter helm-fuzzier tide typescript-mode lsp-ui helm-ag latex-mode
                                         LaTeX-mode company-latex latex-preview-pane company-auctex
                                         async forge dired-git-info dired diredfl restclient indium
                                         yaml-mode helm-org helm-projectile projectile adoc-mode
                                         dap-rust dap-mode helm-lsp company-lsp docker-tramp
                                         rjsx-mode flycheck-color-mode-line elfeed helm-rg json-mode
                                         avy smart-parens hippie-expand electric-pair-mode
                                         electric-pair erc-highlight-nicknames comint-mode comint
                                         visible-bell-mode visible-bell ibuffer-mode prettier
                                         prettier-js helm-swoop elisp-format helm-flx try
                                         winner-mode helm-ls-git wgrep-helm add-node-modules-path
                                         company-quickhelp flymd exec-path-from-shell wget js2-mode
                                         diff-hl browse-kill-ring cargo company rust-mode
                                         haskell-mode which-key cl-lib multiple-cursors
                                         expand-region ace-window intero hindent emojify auctex
                                         pdf-tools geiser))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(preview-gs-command "/usr/bin/gs" t)
 '(preview-image-type 'pnm t)
 '(reftex-mode t t)
 '(set-face-attribute 'helm-source-header t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(web-mode-block-padding 2 t)
 '(web-mode-code-indent-offset 2 t)
 '(web-mode-comment-style 2 t)
 '(web-mode-css-indent-offset 2 t)
 '(web-mode-enable-auto-indentation t t)
 '(web-mode-enable-auto-pairing t t)
 '(web-mode-enable-auto-quoting nil t)
 '(web-mode-enable-comment-keywords t t)
 '(web-mode-enable-css-colorization t t)
 '(web-mode-enable-current-element-highlight t t)
 '(web-mode-markup-indent-offset 2 t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
