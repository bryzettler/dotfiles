;; -*- lexical-binding: t; -*-
;;; init.el --- Modern Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A clean, modern Emacs configuration optimized for terminal use (emacs -nw)
;; Uses use-package, vertico/corfu completion, eglot LSP, and catppuccin theme

;;; Code:

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
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Disable backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; UTF-8 everywhere
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

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

  ;; Better performance
  (setq read-process-output-max (* 1024 1024))

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
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon nil  ; no icons in terminal
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 20))

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
         ("M-s f" . consult-find)))

;; Fuzzy project-wide file search (C-x f)
(autoload 'consult-fd "consult" nil t)
(defun my/find-file-fuzzy ()
  "Find file with fuzzy matching across entire project."
  (interactive)
  (let* ((git-root (locate-dominating-file default-directory ".git"))
         (project (project-current))
         (root (cond
                (project (project-root project))
                (git-root git-root)
                (t default-directory))))
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
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

;; Cape - completion at point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

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
  :bind (("C-x :" . er/expand-region)
         ("C-x '" . er/mark-outside-pairs)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("M-m" . mc/mark-next-like-this)
         ("M-u" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)))

;; Smart parens
(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Comment/uncomment
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; =============================================================================
;; Navigation & Project Management
;; =============================================================================

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
  :config
  (setq treemacs-width 35
        treemacs-no-png-images t
        treemacs-is-never-other-window t
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
              (let ((default-directory (or (locate-dominating-file default-directory ".git")
                                           default-directory)))
                (treemacs-add-and-display-current-project-exclusively)))))

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
         (python-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-sync-connect nil))

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

;; Rust
(use-package rust-ts-mode
  :straight nil
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

;; =============================================================================
;; Custom Keybindings (from legacy config)
;; =============================================================================


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
            (setq gc-cons-threshold (* 16 1024 1024))))

(provide 'init)
;;; init.el ends here
