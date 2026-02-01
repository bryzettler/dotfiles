;; -*- lexical-binding: t; -*-
;;; init.el --- Modern Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A clean, modern Emacs configuration optimized for terminal use (emacs -nw)
;; Uses use-package, vertico/corfu completion, eglot LSP, and doom-dark+ theme

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

;; Diminish - hide minor modes from modeline (load early for :diminish keyword)
(use-package diminish)

;; =============================================================================
;; Terminal True Color (24-bit) Support
;; =============================================================================

;; Enable 24-bit color in terminal when COLORTERM=truecolor is set
(add-to-list 'term-file-aliases '("xterm-256color" . "xterm-direct"))
(add-to-list 'term-file-aliases '("tmux-256color" . "xterm-direct"))
(add-to-list 'term-file-aliases '("screen-256color" . "xterm-direct"))
(add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-direct"))

;; Terminal key translations
(define-key input-decode-map "\e[Z" (kbd "<backtab>"))

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

;; Doom themes (Dark+ theme)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dark+ t)
  (doom-themes-org-config)

  ;; Dark+ exact color overrides
  ;; Reference: https://github.com/microsoft/vscode/blob/main/extensions/theme-defaults/themes/dark_plus.json
  ;; Purple #c586c0 - control flow (import/export/if/return/await)
  ;; Blue #569cd6 - storage keywords (const/let/function/class/this)
  ;; Yellow #dcdcaa - functions (definitions and calls)
  ;; Green #4ec9b0 - types (classes, interfaces, type annotations)
  ;; Light Blue #9cdcfe - variables, parameters, properties
  ;; Cyan #4fc1ff - constants (UPPER_CASE), enum members
  ;; Orange #ce9178 - strings
  ;; Light Green #b5cea8 - numbers
  ;; Green #6a9955 - comments
  (custom-set-faces
   ;; Base colors
   '(default ((t :background "#1e1e1e" :foreground "#d4d4d4")))
   '(cursor ((t :background "#aeafad")))
   '(region ((t :background "#264f78")))
   '(fringe ((t :background "#1e1e1e")))
   '(line-number ((t :foreground "#858585" :background "#1e1e1e")))
   '(line-number-current-line ((t :foreground "#c6c6c6" :background "#1e1e1e")))

   ;; Syntax highlighting - Dark+ colors
   '(font-lock-keyword-face ((t :foreground "#569cd6")))
   '(font-lock-builtin-face ((t :foreground "#c586c0")))
   '(font-lock-function-name-face ((t :foreground "#dcdcaa")))
   '(font-lock-function-call-face ((t :foreground "#dcdcaa")))
   '(font-lock-variable-name-face ((t :foreground "#9cdcfe")))
   '(font-lock-variable-use-face ((t :foreground "#9cdcfe")))
   '(font-lock-type-face ((t :foreground "#4ec9b0")))
   '(font-lock-constant-face ((t :foreground "#4fc1ff")))
   '(font-lock-string-face ((t :foreground "#ce9178")))
   '(font-lock-comment-face ((t :foreground "#6a9955")))
   '(font-lock-comment-delimiter-face ((t :foreground "#6a9955")))
   '(font-lock-doc-face ((t :foreground "#6a9955")))
   '(font-lock-number-face ((t :foreground "#b5cea8")))
   '(font-lock-operator-face ((t :foreground "#d4d4d4")))
   '(font-lock-property-name-face ((t :foreground "#9cdcfe")))
   '(font-lock-property-use-face ((t :foreground "#9cdcfe")))
   '(font-lock-punctuation-face ((t :foreground "#d4d4d4")))
   '(font-lock-bracket-face ((t :foreground "#ffd700")))
   '(font-lock-preprocessor-face ((t :foreground "#c586c0")))
   '(font-lock-warning-face ((t :foreground "#f14c4c" :weight bold)))
   '(font-lock-escape-face ((t :foreground "#d7ba7d")))
   '(font-lock-regexp-grouping-backslash ((t :foreground "#d7ba7d")))
   '(font-lock-regexp-grouping-construct ((t :foreground "#d7ba7d")))

   ;; Tree-sitter builtin faces
   '(tree-sitter-hl-face:function ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:function.call ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:function.method ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:function.macro ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:function.special ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:function.builtin ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:method ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:method.call ((t :foreground "#dcdcaa")))
   '(tree-sitter-hl-face:type ((t :foreground "#4ec9b0")))
   '(tree-sitter-hl-face:type.builtin ((t :foreground "#4ec9b0")))
   '(tree-sitter-hl-face:type.parameter ((t :foreground "#4ec9b0")))
   '(tree-sitter-hl-face:type.argument ((t :foreground "#4ec9b0")))
   '(tree-sitter-hl-face:constructor ((t :foreground "#4ec9b0")))
   '(tree-sitter-hl-face:variable ((t :foreground "#9cdcfe")))
   '(tree-sitter-hl-face:variable.builtin ((t :foreground "#569cd6")))
   '(tree-sitter-hl-face:variable.parameter ((t :foreground "#9cdcfe")))
   '(tree-sitter-hl-face:variable.special ((t :foreground "#9cdcfe")))
   '(tree-sitter-hl-face:property ((t :foreground "#9cdcfe")))
   '(tree-sitter-hl-face:property.definition ((t :foreground "#9cdcfe")))
   '(tree-sitter-hl-face:keyword ((t :foreground "#569cd6")))
   '(tree-sitter-hl-face:keyword.control ((t :foreground "#c586c0")))
   '(tree-sitter-hl-face:keyword.operator ((t :foreground "#c586c0")))
   '(tree-sitter-hl-face:keyword.function ((t :foreground "#569cd6")))
   '(tree-sitter-hl-face:keyword.return ((t :foreground "#c586c0")))
   '(tree-sitter-hl-face:string ((t :foreground "#ce9178")))
   '(tree-sitter-hl-face:string.special ((t :foreground "#d7ba7d")))
   '(tree-sitter-hl-face:escape ((t :foreground "#d7ba7d")))
   '(tree-sitter-hl-face:number ((t :foreground "#b5cea8")))
   '(tree-sitter-hl-face:constant ((t :foreground "#4fc1ff")))
   '(tree-sitter-hl-face:constant.builtin ((t :foreground "#569cd6")))
   '(tree-sitter-hl-face:comment ((t :foreground "#6a9955")))
   '(tree-sitter-hl-face:doc ((t :foreground "#6a9955")))
   '(tree-sitter-hl-face:operator ((t :foreground "#d4d4d4")))
   '(tree-sitter-hl-face:punctuation ((t :foreground "#d4d4d4")))
   '(tree-sitter-hl-face:punctuation.bracket ((t :foreground "#ffd700")))
   '(tree-sitter-hl-face:punctuation.delimiter ((t :foreground "#d4d4d4")))
   '(tree-sitter-hl-face:label ((t :foreground "#c586c0")))
   '(tree-sitter-hl-face:attribute ((t :foreground "#9cdcfe")))
   '(tree-sitter-hl-face:tag ((t :foreground "#569cd6")))
   '(tree-sitter-hl-face:embedded ((t :foreground "#d4d4d4")))))

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
        vertico-count 15
        vertico-resize t)
  (setq resize-mini-windows t))

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
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s" . consult-ripgrep))
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
                   default-directory))
         (display-buffer-overriding-action '(display-buffer-same-window)))
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
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame)
  (defun my/aw-include-treemacs (orig-fn window)
    "Include treemacs windows in ace-window selection."
    (if (eq (buffer-local-value 'major-mode (window-buffer window)) 'treemacs-mode)
        nil
      (funcall orig-fn window)))
  (advice-add 'aw-ignored-p :around #'my/aw-include-treemacs))

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
  (setq project-vc-extra-root-markers '(".project" ".projectile")))

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

  (defun my/treemacs-toggle-or-visit ()
    "Toggle node if directory, visit if file."
    (interactive)
    (pcase (treemacs-current-button)
      ('nil nil)
      (btn (pcase (treemacs-button-get btn :state)
             ((or 'dir-node-open 'dir-node-closed) (treemacs-toggle-node))
             (_ (treemacs-visit-node-no-split))))))

  (defvar my/treemacs-override-map (make-sparse-keymap))
  (define-key my/treemacs-override-map (kbd "C-@") #'my/treemacs-toggle-or-visit)
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (setq-local emulation-mode-map-alists
                          (cons `((t . ,my/treemacs-override-map))
                                emulation-mode-map-alists))))
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
                  (other-window 1))))))

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
         (python-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (eglot-managed-mode . (lambda () (font-lock-flush))))
  :config
  (setq eglot-autoshutdown t
        eglot-sync-connect 1
        eglot-send-changes-idle-time 0.1
        treesit-font-lock-level 4
        eglot-ignored-server-capabilities '())

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions
                  (:semanticHighlighting (:strings t :punctuation (:enable t))))))

  ;; Semantic token faces - Dark+ colors
  (custom-set-faces
   '(eglot-semantic-class ((t :foreground "#4EC9B0" :weight semi-bold)))
   '(eglot-semantic-type ((t :foreground "#4EC9B0")))
   '(eglot-semantic-interface ((t :foreground "#4EC9B0" :slant italic)))
   '(eglot-semantic-enum ((t :foreground "#4EC9B0")))
   '(eglot-semantic-variable ((t :foreground "#9CDCFE")))
   '(eglot-semantic-parameter ((t :foreground "#9CDCFE" :slant italic)))
   '(eglot-semantic-property ((t :foreground "#9CDCFE")))
   '(eglot-semantic-function ((t :foreground "#DCDCAA")))
   '(eglot-semantic-method ((t :foreground "#DCDCAA")))
   '(eglot-semantic-namespace ((t :foreground "#4EC9B0" :weight semi-bold)))
   '(eglot-semantic-enumMember ((t :foreground "#4FC1FF")))
   '(eglot-semantic-readonly ((t :foreground "#4FC1FF")))))

;; Tree-sitter auto
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (setq treesit-auto-langs (remove 'yaml treesit-auto-langs))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; TypeScript/TSX/JS - comprehensive tree-sitter highlighting
(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :init
  ;; Custom faces for tree-sitter highlighting
  (defface ts-control-keyword '((t :foreground "#c586c0")) "Control flow - purple")
  (defface ts-storage-keyword '((t :foreground "#569cd6")) "Storage/declaration - blue")
  (defface ts-this-keyword '((t :foreground "#569cd6")) "this keyword - blue")
  (defface ts-constant '((t :foreground "#4fc1ff")) "Constants - cyan")

  :config
  (defun my/ts-treesit-rules (lang)
    "Generate Dark+ tree-sitter rules for LANG."
    (append
     ;; Control keywords - purple #c586c0
     (treesit-font-lock-rules
      :language lang :feature 'custom-control :override t
      '((["import" "from" "export" "default" "as"
          "if" "else" "return" "for" "while" "do"
          "switch" "case" "break" "continue"
          "try" "catch" "finally" "throw"
          "new" "delete" "typeof" "instanceof"
          "in" "of" "await" "async" "yield"
          "with" "debugger"] @ts-control-keyword)))
     ;; Storage keywords - blue #569cd6
     (treesit-font-lock-rules
      :language lang :feature 'custom-storage :override t
      '((["const" "let" "var" "function" "class"
          "interface" "type" "enum" "namespace" "module"
          "declare" "readonly" "public" "private" "protected"
          "static" "abstract" "extends" "implements"
          "get" "set" "keyof" "infer" "satisfies"] @ts-storage-keyword)
        (this) @ts-this-keyword))
     ;; Function/method definitions - yellow #dcdcaa
     (treesit-font-lock-rules
      :language lang :feature 'custom-function-def :override t
      '((function_declaration name: (identifier) @font-lock-function-name-face)
        (function_expression name: (identifier) @font-lock-function-name-face)
        (method_definition name: (property_identifier) @font-lock-function-name-face)
        (method_signature name: (property_identifier) @font-lock-function-name-face)
        (variable_declarator name: (identifier) @font-lock-function-name-face value: (arrow_function))
        (pair key: (property_identifier) @font-lock-function-name-face value: [(function_expression) (arrow_function)])))
     ;; Function/method calls - yellow #dcdcaa
     (treesit-font-lock-rules
      :language lang :feature 'custom-function-call :override t
      '((call_expression function: (identifier) @font-lock-function-call-face)
        (call_expression function: (member_expression property: (property_identifier) @font-lock-function-call-face))
        (new_expression constructor: (identifier) @font-lock-function-call-face)))
     ;; Type annotations - green #4ec9b0
     (treesit-font-lock-rules
      :language lang :feature 'custom-type :override t
      '((type_identifier) @font-lock-type-face
        (predefined_type) @font-lock-type-face
        (type_arguments (type_identifier) @font-lock-type-face)
        (type_parameter name: (type_identifier) @font-lock-type-face)
        (class_declaration name: (type_identifier) @font-lock-type-face)
        (interface_declaration name: (type_identifier) @font-lock-type-face)
        (type_alias_declaration name: (type_identifier) @font-lock-type-face)
        (enum_declaration name: (identifier) @font-lock-type-face)
        (extends_clause value: (identifier) @font-lock-type-face)
        (implements_clause (type_identifier) @font-lock-type-face)))
     ;; Parameters - light blue #9cdcfe
     (treesit-font-lock-rules
      :language lang :feature 'custom-parameter :override t
      '((required_parameter pattern: (identifier) @font-lock-variable-name-face)
        (optional_parameter pattern: (identifier) @font-lock-variable-name-face)
        (required_parameter pattern: (object_pattern (shorthand_property_identifier_pattern) @font-lock-variable-name-face))
        (required_parameter pattern: (array_pattern (identifier) @font-lock-variable-name-face))))
     ;; Property access - light blue #9cdcfe
     (treesit-font-lock-rules
      :language lang :feature 'custom-property :override nil
      '((member_expression property: (property_identifier) @font-lock-property-use-face)
        (pair key: (property_identifier) @font-lock-property-name-face)
        (shorthand_property_identifier) @font-lock-variable-use-face))
     ;; Constants (UPPER_CASE) - cyan #4fc1ff
     (treesit-font-lock-rules
      :language lang :feature 'ts-constant :override t
      '((identifier) @ts-constant
        (:match "^[A-Z][A-Z0-9_]+$" @ts-constant)))))

  (defun my/tsx-treesit-rules ()
    "Generate Dark+ JSX-specific rules."
    (treesit-font-lock-rules
     :language 'tsx :feature 'custom-jsx :override t
     '((jsx_opening_element name: (identifier) @font-lock-type-face)
       (jsx_closing_element name: (identifier) @font-lock-type-face)
       (jsx_self_closing_element name: (identifier) @font-lock-type-face)
       (jsx_opening_element name: (member_expression) @font-lock-type-face)
       (jsx_closing_element name: (member_expression) @font-lock-type-face)
       (jsx_self_closing_element name: (member_expression) @font-lock-type-face)
       (jsx_attribute (property_identifier) @font-lock-property-name-face))))

  ;; Advice to inject rules BEFORE mode setup completes
  (defun my/ts-inject-font-lock (orig-fun &rest args)
    "Inject Dark+ rules before typescript-ts-mode setup."
    (apply orig-fun args)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings (my/ts-treesit-rules 'typescript)))
    (treesit-font-lock-recompute-features
     '(custom-control custom-storage custom-function-def custom-function-call
                      custom-type custom-parameter custom-property custom-constant))
    (treesit-major-mode-setup))

  (defun my/tsx-inject-font-lock (orig-fun &rest args)
    "Inject Dark+ rules before tsx-ts-mode setup."
    (apply orig-fun args)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (my/ts-treesit-rules 'tsx)
                        (my/tsx-treesit-rules)))
    (treesit-font-lock-recompute-features
     '(custom-control custom-storage custom-function-def custom-function-call
                      custom-type custom-parameter custom-property custom-constant custom-jsx))
    (treesit-major-mode-setup))

  (defun my/js-inject-font-lock (orig-fun &rest args)
    "Inject Dark+ rules before js-ts-mode setup."
    (apply orig-fun args)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings (my/ts-treesit-rules 'javascript)))
    (treesit-font-lock-recompute-features
     '(custom-control custom-storage custom-function-def custom-function-call
                      custom-type custom-parameter custom-property custom-constant))
    (treesit-major-mode-setup))

  (advice-add 'typescript-ts-mode :around #'my/ts-inject-font-lock)
  (advice-add 'tsx-ts-mode :around #'my/tsx-inject-font-lock)
  (advice-add 'js-ts-mode :around #'my/js-inject-font-lock))

;; JavaScript (tree-sitter)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

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
  :mode "\\.rs\\'"
  :config
  (defun my/rust-treesit-rules ()
    "Generate Dark+ tree-sitter rules for Rust."
    (append
     (treesit-font-lock-rules
      :language 'rust :feature 'custom-control :override t
      '((["if" "else" "match" "return" "break" "continue"
          "for" "while" "loop" "in" "yield"
          "async" "await" "try" "unsafe"] @ts-control-keyword)))
     (treesit-font-lock-rules
      :language 'rust :feature 'custom-storage :override t
      '((["let" "const" "static" "fn" "struct" "enum"
          "trait" "impl" "type" "mod" "use" "pub" "crate"
          "mut" "ref" "move" "dyn" "where" "as" "extern"] @ts-storage-keyword)
        (self) @ts-this-keyword))
     (treesit-font-lock-rules
      :language 'rust :feature 'custom-function-def :override t
      '((function_item name: (identifier) @font-lock-function-name-face)
        (function_signature_item name: (identifier) @font-lock-function-name-face)))
     (treesit-font-lock-rules
      :language 'rust :feature 'custom-function-call :override t
      '((call_expression function: (identifier) @font-lock-function-call-face)
        (call_expression function: (field_expression field: (field_identifier) @font-lock-function-call-face))
        (call_expression function: (scoped_identifier name: (identifier) @font-lock-function-call-face))
        (macro_invocation macro: (identifier) @font-lock-function-call-face)
        (macro_invocation macro: (scoped_identifier name: (identifier) @font-lock-function-call-face))))
     (treesit-font-lock-rules
      :language 'rust :feature 'custom-type :override t
      '((type_identifier) @font-lock-type-face
        (primitive_type) @font-lock-type-face
        (struct_item name: (type_identifier) @font-lock-type-face)
        (enum_item name: (type_identifier) @font-lock-type-face)
        (trait_item name: (type_identifier) @font-lock-type-face)
        (impl_item trait: (type_identifier) @font-lock-type-face)
        (type_alias_item name: (type_identifier) @font-lock-type-face)
        (generic_type type: (type_identifier) @font-lock-type-face)
        (scoped_type_identifier name: (type_identifier) @font-lock-type-face)))
     (treesit-font-lock-rules
      :language 'rust :feature 'custom-property :override nil
      '((field_expression field: (field_identifier) @font-lock-property-use-face)
        (field_identifier) @font-lock-property-name-face
        (shorthand_field_initializer (identifier) @font-lock-property-name-face)))
     (treesit-font-lock-rules
      :language 'rust :feature 'custom-lifetime :override t
      '((lifetime) @ts-control-keyword
        (attribute_item) @font-lock-preprocessor-face))))

  (defun my/rust-inject-font-lock (orig-fun &rest args)
    "Inject Dark+ rules before rust-ts-mode setup."
    (apply orig-fun args)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings (my/rust-treesit-rules)))
    (treesit-font-lock-recompute-features
     '(custom-control custom-storage custom-function-def custom-function-call
                      custom-type custom-property custom-lifetime))
    (treesit-major-mode-setup))

  (advice-add 'rust-ts-mode :around #'my/rust-inject-font-lock))

(use-package rust-mode
  :unless (treesit-ready-p 'rust)
  :mode "\\.rs\\'")

;; Python (tree-sitter)
(use-package python
  :straight nil
  :mode (("\\.py\\'" . python-ts-mode))
  :config
  (defun my/python-treesit-rules ()
    "Generate Dark+ tree-sitter rules for Python."
    (append
     (treesit-font-lock-rules
      :language 'python :feature 'custom-control :override t
      '((["if" "elif" "else" "for" "while" "return" "yield"
          "try" "except" "finally" "raise" "with" "as"
          "break" "continue" "pass" "assert"
          "import" "from" "async" "await"
          "and" "or" "not" "in" "is"] @ts-control-keyword)))
     (treesit-font-lock-rules
      :language 'python :feature 'custom-storage :override t
      '((["def" "class" "lambda" "global" "nonlocal"] @ts-storage-keyword)
        (self) @ts-this-keyword))
     (treesit-font-lock-rules
      :language 'python :feature 'custom-function-def :override t
      '((function_definition name: (identifier) @font-lock-function-name-face)))
     (treesit-font-lock-rules
      :language 'python :feature 'custom-function-call :override t
      '((call function: (identifier) @font-lock-function-call-face)
        (call function: (attribute attribute: (identifier) @font-lock-function-call-face))
        (decorator (identifier) @font-lock-function-call-face)
        (decorator (attribute attribute: (identifier) @font-lock-function-call-face))))
     (treesit-font-lock-rules
      :language 'python :feature 'custom-type :override t
      '((class_definition name: (identifier) @font-lock-type-face)
        (type (identifier) @font-lock-type-face)
        (type (subscript value: (identifier) @font-lock-type-face))))
     (treesit-font-lock-rules
      :language 'python :feature 'custom-property :override nil
      '((attribute attribute: (identifier) @font-lock-property-use-face)))
     (treesit-font-lock-rules
      :language 'python :feature 'custom-parameter :override t
      '((parameters (identifier) @font-lock-variable-name-face)
        (default_parameter name: (identifier) @font-lock-variable-name-face)
        (typed_parameter (identifier) @font-lock-variable-name-face)
        (typed_default_parameter name: (identifier) @font-lock-variable-name-face)
        (list_splat_pattern (identifier) @font-lock-variable-name-face)
        (dictionary_splat_pattern (identifier) @font-lock-variable-name-face)))))

  (defun my/python-inject-font-lock (orig-fun &rest args)
    "Inject Dark+ rules before python-ts-mode setup."
    (apply orig-fun args)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings (my/python-treesit-rules)))
    (treesit-font-lock-recompute-features
     '(custom-control custom-storage custom-function-def custom-function-call
                      custom-type custom-property custom-parameter))
    (treesit-major-mode-setup))

  (advice-add 'python-ts-mode :around #'my/python-inject-font-lock))

;; YAML (yaml-mode provides proper indent cycling, unlike yaml-ts-mode)
(use-package yaml-mode
  :demand t
  :mode "\\.ya?ml\\'"
  :custom
  (yaml-indent-offset 2)
  :config
  (defun my/yaml-outdent ()
    "Outdent current line by yaml-indent-offset spaces."
    (interactive)
    (indent-rigidly (line-beginning-position) (line-end-position) (- yaml-indent-offset)))
  (define-key yaml-mode-map (kbd "RET") #'newline-and-indent)
  (define-key yaml-mode-map (kbd "<backtab>") #'my/yaml-outdent))

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

;; Vterm - robust terminal emulator (requires cmake, libtool)
(use-package vterm
  :config
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit nil))

;; Claude Code
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el"
                   :branch "main" :depth 1)
  :demand t
  :bind-keymap ("C-c c" . claude-code-command-map)
  :config
  (setq claude-code-terminal-backend 'vterm
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
    "Pending text to prefill after claude starts.")

  (defun my/claude-prefill-pending-text ()
    "Prefill pending text (don't send), called from claude-code-start-hook."
    (run-at-time 2.0 nil
                 (lambda ()
                   (when-let* ((buf (claude-code--get-or-prompt-for-buffer))
                               (win (get-buffer-window buf)))
                     (select-window win)
                     (when my/claude-pending-text
                       (let ((text my/claude-pending-text))
                         (setq my/claude-pending-text nil)
                         (with-current-buffer buf
                           (claude-code--term-send-string claude-code-terminal-backend text))))))))

  (add-hook 'claude-code-start-hook #'my/claude-prefill-pending-text)

  (defun my/claude-code-prefill-region ()
    "Insert selected region into Claude prompt without sending.
Opens Claude buffer if needed, inserts region text at prompt,
positions cursor for user to add context before sending."
    (interactive)
    (when (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
            (claude-buf (claude-code--get-or-prompt-for-buffer)))
        (if claude-buf
            (progn
              (unless (get-buffer-window claude-buf)
                (display-buffer claude-buf))
              (select-window (get-buffer-window claude-buf))
              (with-current-buffer claude-buf
                (claude-code--term-send-string claude-code-terminal-backend text)))
          (setq my/claude-pending-text text)
          (claude-code)))))

  (defun my/claude-code-smart ()
    "Smart Claude Code command.
- If region selected: start/show claude and prefill region (don't send)
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
       ;; Region selected - prefill without sending
       (has-region
        (my/claude-code-prefill-region))
       ;; No region, claude running - toggle
       (claude-buf
        (if (get-buffer-window claude-buf)
            (delete-window (get-buffer-window claude-buf))
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

;; Swap split bindings (C-x 2 = side-by-side, C-x 3 = stacked)
(global-set-key (kbd "C-x 2") #'split-window-right)
(global-set-key (kbd "C-x 3") #'split-window-below)

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

;; Interactive find/replace across project
(global-set-key (kbd "C-c r") #'project-query-replace-regexp)

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
