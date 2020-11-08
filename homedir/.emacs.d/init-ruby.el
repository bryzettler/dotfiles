(use-package enh-ruby-mode
  ;; use on most ruby based files
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|plan\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :interpreter "ruby"
  ;; might need this if it fucks with base16
  :bind ("C-c C-e" . ruby-send-region)  ;; Rebind since Rubocop uses C-c C-r
  :config
  (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
  :init
  (add-hook 'enh-ruby-mode-hook 'eldoc-mode))

(use-package ruby-electric
  :diminish
  :hook (enh-ruby-mode . ruby-electric-mode))

(use-package rspec-mode
  :config
  (setq compilation-scroll-output 'first-error)
  (rspec-install-snippets))

;; Functions to help with refactoring
(use-package ruby-refactor
  :init
  (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch))

;; Easily toggle ruby's hash syntax
(use-package ruby-hash-syntax)

;; Helpers to deal with strings and symbols
(use-package ruby-tools)

;; Use web-mode for dealing with ERB templates
(use-package web-mode
  :ensure t
  :mode "\\.erb\\'")

;; RVM integration
(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

;; Now, place point on some function, and hit F1 to see the glory. In order for this to work, we need to generate the missing docs:
;; gem rdoc --all --ri --no-rdoc
;; rvm docs generate all
(use-package yari
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (local-set-key [f1] 'yari))))

;; REPL on editor
(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;; The lint-like style checker of choice for Ruby is Rubocop. The rubocop.el mode should just work with Flycheck.
(use-package rubocop
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'rubocop-mode)
  (add-hook 'enh-ruby-mode-hook 'flycheck-mode)
  :diminish rubocop-mode)

;; Robe is a “code assistance” tool, that pretty much only works with methods
;; (and doesn’t seem to work well with direct functions). One must install the following before this will work:
;; And even then, it barely works. Once started with robe-start, we should get code completion:
(use-package robe
  :ensure t
  :bind ("C-M-." . robe-jump)

  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode)

  :config
  (defadvice inf-ruby-console-auto
    (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

;; If we have installed Company for auto-complete, use robe for this purpose:
(use-package company
  :no-require t
  :config
  (push 'company-robe company-backends))

(provide 'init-ruby)
;;; init-ruby.el ends here
