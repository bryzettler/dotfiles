;; -*- lexical-binding: t; -*-
;;; init.el --- Modern Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A clean, modern Emacs configuration optimized for terminal use (emacs -nw)
;; Uses use-package, vertico/corfu completion, eglot LSP, and catppuccin theme

;;; Code:

;; Performance: increase thresholds during init
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 4 1024 1024))

;; =============================================================================
;; Package Management
;; =============================================================================

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

;; =============================================================================
;; Basic Settings
;; =============================================================================

(use-package emacs
  :straight nil
  :init
  ;; User info
  (setq user-full-name "Bryan Zettler"
        user-mail-address "bryanzettler@gmail.com")

  ;; Better defaults
  (setq-default
   indent-tabs-mode nil
   tab-width 2
   fill-column 80)

  ;; Simplify yes/no prompts
  (setq use-short-answers t)

  ;; Disable backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; UTF-8 everywhere
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  ;; UI settings (works in GUI, no-op in terminal)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 10)
  (setq visible-bell t
        use-dialog-box nil
        mouse-wheel-scroll-amount '(3 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse t
        scroll-step 1)
  (column-number-mode)

  ;; Enable mouse in terminal
  (xterm-mouse-mode 1)

  ;; Better scrolling
  (setq scroll-margin 3
        scroll-conservatively 101
        scroll-preserve-screen-position t)

  ;; Line numbers
  (setq display-line-numbers-type 'relative)

  ;; Delete selection on typing
  (delete-selection-mode 1)

  ;; Show matching parens
  (show-paren-mode 1)
  (setq show-paren-delay 0)

  ;; Remember cursor position
  (save-place-mode 1)

  ;; Auto-refresh buffers
  (global-auto-revert-mode 1)

  ;; No bell
  (setq ring-bell-function 'ignore)

  ;; Disable startup screen
  (setq inhibit-startup-message t
        initial-scratch-message nil)

  ;; Shorter prompts
  (setq confirm-kill-emacs nil)
  (file-name-shadow-mode 1)

  ;; Abbreviate home directory
  (setq abbreviated-home-dir "\\`~\\(/\\|\\'\\)")

  ;; Enable line numbers in prog modes
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)))

;; Keep .emacs.d clean
(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "data/" user-emacs-directory)))

;; =============================================================================
;; Theme & UI
;; =============================================================================

;; Catppuccin theme
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project))

;; Hide minor modes in modeline
(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (minions-mode-line-lighter ""))

;; Nerd icons (works in terminal with nerd fonts)
(use-package nerd-icons)

;; Which-key for discoverability
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-prefix-prefix "+"
        which-key-sort-order 'which-key-key-order-alpha))

;; =============================================================================
;; Completion (Vertico + Corfu stack)
;; =============================================================================

;; Vertico - vertical completion UI
(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-count 15))

;; Orderless - flexible matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Jinx - modern spell checking
(use-package jinx
  :hook (text-mode . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

;; Marginalia - rich annotations
(use-package marginalia
  :init (marginalia-mode))

;; Consult - search/navigation commands
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find))
  :config
  (setq consult-preview-key "M-."))

;; Fuzzy project-wide file search (C-x f)
(autoload 'consult-fd "consult" nil t)
(defun my/find-file-fuzzy ()
  "Find file with fuzzy matching from git root or project root."
  (interactive)
  (let* ((git-root (locate-dominating-file default-directory ".git"))
         (project (project-current))
         (root (or git-root
                   (when project (project-root project))
                   default-directory)))
    (consult-fd root)))
(global-set-key (kbd "C-x C-f") #'my/find-file-fuzzy)

;; Embark - actions on completions
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

;; Corfu - in-buffer completion
(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match nil)
  (corfu-preview-current nil)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

;; Corfu terminal support (required for emacs -nw)
(use-package corfu-terminal
  :straight (:type git :host codeberg :repo "akib/emacs-corfu-terminal")
  :unless (display-graphic-p)
  :demand t
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; Cape - completion extensions (appended as fallbacks)
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t))

;; =============================================================================
;; Editing
;; =============================================================================

;; Avy - jump to char/word/line
(use-package avy
  :bind (("M-g c" . avy-goto-char)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

;; Xref - code navigation (integrates with eglot)
(use-package xref
  :straight nil
  :bind (("M-g d" . xref-find-definitions)
         ("M-g r" . xref-find-references)
         ("M-," . xref-go-back)))

;; Expand region
(use-package expand-region
  :bind (("C-x ;" . er/expand-region)
         ("C-x '" . er/mark-outside-pairs)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("M-m" . mc/mark-next-like-this)
         ("M-u" . mc/mark-all-like-this)
         ("C-S-u" . mc/mark-previous-like-this)))

;; Smart parens
(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rainbow mode - colorize color strings
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Whitespace cleanup on save
(use-package ws-butler
  :diminish ws-butler-mode
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Comment/uncomment
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; =============================================================================
;; Navigation & Project Management
;; =============================================================================

;; Ace-window - quick window switching
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Use built-in project.el
(use-package project
  :straight nil
  :bind (("C-x p f" . project-find-file)
         ("C-x p p" . project-switch-project)
         ("C-x p g" . project-find-regexp)
         ("C-x p d" . project-dired)
         ("C-x p b" . project-switch-to-buffer)
         ("C-x p k" . project-kill-buffers))
  :config
  (setq project-vc-extra-root-markers '(".project" ".projectile" "package.json" "Cargo.toml")))

;; Treemacs - project sidebar
(use-package treemacs
  :commands (treemacs treemacs-select-window treemacs-add-and-display-current-project-exclusively)
  :init
  (define-key input-decode-map "\e[32;13~" (kbd "C-s-SPC"))
  (define-key input-decode-map "\e[105;13~" (kbd "s-i"))
  (define-key input-decode-map "\e[110;13~" (kbd "s-n"))
  (define-key input-decode-map "\e[99;13~" (kbd "s-c"))
  :config
  (setq treemacs-width 35
        treemacs-no-png-images t
        treemacs-is-never-other-window nil
        treemacs-show-hidden-files t
        treemacs-silent-refresh t
        treemacs-silent-filewatch t)

  (defun my/treemacs-toggle ()
    "Toggle treemacs, always showing the git root project."
    (interactive)
    (pcase (treemacs-current-visibility)
      ('visible (delete-window (treemacs-get-local-window)))
      (_
       (let ((default-directory (or (locate-dominating-file default-directory ".git")
                                    default-directory)))
         (treemacs-add-and-display-current-project-exclusively)))))

  :bind (("C-s-SPC" . my/treemacs-toggle)))

;; Auto-open treemacs on startup with git root
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and (not (daemonp))
                       (file-directory-p default-directory))
              (let ((project-root (or (locate-dominating-file default-directory ".git")
                                      default-directory)))
                (let ((default-directory project-root))
                  (delete-other-windows)
                  (treemacs-add-and-display-current-project-exclusively)
                  ;; Switch to main window with empty buffer (so C-x C-f replaces it)
                  (other-window 1)
                  (switch-to-buffer (get-buffer-create "*empty*"))
                  ;; Set empty buffer's directory to project root
                  (setq default-directory project-root)
                  (setq buffer-read-only t))))))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;; =============================================================================
;; Git
;; =============================================================================

;; Magit
(use-package magit
  :bind (("C-M-;" . magit-status)
         ("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Diff-hl (git gutter)
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-margin-mode))

;; Git link
(use-package git-link
  :bind ("C-c g l" . git-link))

;; =============================================================================
;; LSP & Languages (Eglot)
;; =============================================================================

;; Eglot (built-in LSP client)
(use-package eglot
  :straight nil
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-sync-connect 1
        eglot-send-changes-idle-time 0.1)

  ;; Enable semantic token highlighting from LSP (colors variables, fields, etc.)
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions
                  (:semanticHighlighting (:strings t :punctuation (:enable t))))))

  ;; Use maximum tree-sitter highlighting
  (setq treesit-font-lock-level 4))

;; Tree-sitter auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; TypeScript
(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; Web mode (for templates, HTML, etc.)
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; Rust (tree-sitter mode preferred, rust-mode as fallback)
(use-package rust-ts-mode
  :straight nil
  :mode "\\.rs\\'")

(use-package rust-mode
  :unless (treesit-ready-p 'rust)
  :mode "\\.rs\\'")

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc"))

;; JSON
(use-package json-ts-mode
  :straight nil
  :mode "\\.json\\'")

;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;; =============================================================================
;; Org Mode
;; =============================================================================

(use-package org
  :straight nil
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-directory "~/Documents/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-hide-emphasis-markers t
        org-startup-indented t
        org-pretty-entities t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-confirm-babel-evaluate nil)

  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

;; Modern org appearance
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "●" "○" "●" "○" "●")))

;; =============================================================================
;; Productivity
;; =============================================================================

;; Flymake (built-in, used with eglot)
(use-package flymake
  :straight nil
  :hook (eglot-managed-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;; YASnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; Format on save
(use-package apheleia
  :diminish apheleia-mode
  :init (apheleia-global-mode +1))

;; Diminish minor modes
(use-package diminish)

;; Editable grep results
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; Flash line after jumps
(use-package pulsar
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.05
        pulsar-iterations 10
        pulsar-pulse-functions '(xref-find-definitions
                                 xref-find-references
                                 xref-go-back
                                 xref-go-forward
                                 avy-goto-char
                                 avy-goto-word-1
                                 avy-goto-line
                                 consult-line
                                 consult-goto-line
                                 scroll-up-command
                                 scroll-down-command
                                 recenter-top-bottom
                                 other-window
                                 ace-window))
  (pulsar-global-mode 1))

;; Auto-load env vars per-project (direnv)
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; =============================================================================
;; Claude Code Integration
;; =============================================================================

;; Required dependency for environment handling
(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

;; Eat terminal emulator (works in terminal Emacs)
(use-package eat
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")))
  :config
  (setq eat-term-scrollback-size 131072
        eat-enable-blinking-text nil
        eat-enable-alternative-display nil
        eat-enable-mouse t
        eat-kill-buffer-on-exit nil)

  (defun my/eat-reset-terminal ()
    "Reset eat terminal if it goes blank."
    (interactive)
    (when (derived-mode-p 'eat-mode)
      (eat-reset)))

  (defun my/eat-scroll-up ()
    "Scroll up in eat buffer."
    (interactive)
    (scroll-down-command))

  (defun my/eat-scroll-down ()
    "Scroll down in eat buffer."
    (interactive)
    (scroll-up-command))

  :bind (:map eat-mode-map
              ("C-c C-r" . my/eat-reset-terminal)
              ("C-v" . my/eat-scroll-down)
              ("M-v" . my/eat-scroll-up)))

;; Claude Code
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el"
                   :branch "main" :depth 1)
  :demand t
  :bind-keymap ("C-c c" . claude-code-command-map)
  :config
  (setq claude-code-terminal-backend 'eat
        ;; Auto-focus Claude buffer when toggling
        claude-code-toggle-auto-select t
        ;; Use Shift+Return for newlines (Return sends message)
        claude-code-newline-keybinding-style 'shift-return-to-send
        ;; Don't ask before killing instances
        claude-code-confirm-kill nil)

  ;; Force ALL claude-code buffers to display in right side window
  (add-to-list 'display-buffer-alist
               '(".*claude.*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)))

  (defvar my/claude-pending-text nil
    "Pending text to send after claude starts.")

  (defun my/claude-send-pending-text ()
    "Send pending text if any, called from claude-code-start-hook."
    (run-at-time 2.0 nil
                 (lambda ()
                   ;; Focus claude buffer
                   (when-let* ((buf (claude-code--get-or-prompt-for-buffer))
                               (win (get-buffer-window buf)))
                     (select-window win))
                   ;; Send pending text if any
                   (when my/claude-pending-text
                     (let ((text my/claude-pending-text))
                       (setq my/claude-pending-text nil)
                       (claude-code--do-send-command text))))))

  (add-hook 'claude-code-start-hook #'my/claude-send-pending-text)

  (defun my/claude-code-smart ()
    "Smart Claude Code command.
- If region selected: start/show claude and send region
- If claude not started: start it
- If claude started: toggle visibility
Always focuses the claude buffer when showing."
    (interactive)
    (let* ((default-directory (or (when-let ((proj (project-current)))
                                    (project-root proj))
                                  (locate-dominating-file default-directory ".git")
                                  default-directory))
           (has-region (use-region-p))
           (claude-buf (claude-code--get-or-prompt-for-buffer)))
      (cond
       ;; Region selected
       (has-region
        (if claude-buf
            ;; Claude running - ensure visible and send
            (progn
              (unless (get-buffer-window claude-buf)
                (display-buffer claude-buf))
              (claude-code-send-region)
              (when-let ((win (get-buffer-window claude-buf)))
                (select-window win)))
          ;; Claude not running - save region, start (hook will send)
          (setq my/claude-pending-text
                (buffer-substring-no-properties (region-beginning) (region-end)))
          (claude-code)))
       ;; No region, claude running - toggle
       (claude-buf
        (if (get-buffer-window claude-buf)
            ;; Visible - hide it
            (delete-window (get-buffer-window claude-buf))
          ;; Hidden - show and focus
          (display-buffer claude-buf)
          (when-let ((win (get-buffer-window claude-buf)))
            (select-window win))))
       ;; No region, claude not running - start
       (t
        (claude-code)))))

  (define-key claude-code-command-map (kbd "c") #'my/claude-code-smart)

  (defun my/claude-code-restart ()
    "Kill and restart Claude Code session (recovery from blank screen)."
    (interactive)
    (when-let ((buf (get-buffer "*claude-code*")))
      (kill-buffer buf))
    (my/claude-code-smart))

  (define-key claude-code-command-map (kbd "R") #'my/claude-code-restart)

  (defun my/claude-code-kill-all-and-start ()
    "Kill all Claude Code instances and start fresh."
    (interactive)
    (claude-code-kill-all)
    (run-at-time 0.5 nil #'my/claude-code-smart))

  (global-set-key (kbd "s-i") #'my/claude-code-smart)
  (global-set-key (kbd "s-n") #'my/claude-code-kill-all-and-start)

  (claude-code-mode))

;; =============================================================================
;; Custom Keybindings (from legacy config)
;; =============================================================================

;; System clipboard integration for terminal Emacs (macOS)
(setq interprogram-cut-function
      (lambda (text)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" nil "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc)))))

(global-set-key (kbd "s-c") #'kill-ring-save)

;; Quick kill buffer (no prompt if unmodified)
(defun my/kill-current-buffer ()
  "Kill current buffer without prompt if unmodified."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") #'my/kill-current-buffer)

;; Word navigation
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)

;; ESC to quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; =============================================================================
;; Reset GC after init
;; =============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds with %d GCs."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (setq gc-cons-threshold (* 32 1024 1024))))

(provide 'init)
;;; init.el ends here
