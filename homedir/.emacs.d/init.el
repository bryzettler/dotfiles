(defvar emacs-home "~/.dotfiles/homedir/.emacs.d/")
(add-to-list 'load-path emacs-home)
(setq custom-file (concat emacs-home "custom.el"))
(load custom-file)
(defvar elpa-dir (concat emacs-home "elpa"))
(setq package-user-dir elpa-dir)
(setq user-full-name "Bry Zettler"
  user-mail-address "bryanzettler@gmail.com")
(require 'init-package)
(require 'init-editor-config)
(require 'cl)
(require 'bind-key)
(require 'ui)
(require 'config)
(require 'defun)

;;;;;;;;;;;;;;;;;;;;
;;; Modes ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
(use-package evil-nerd-commenter)
(use-package expand-region)
(use-package multiple-cursors)
(use-package which-key)
(use-package counsel)
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode 1))
(use-package yasnippet-snippets)
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (setq emmet-expand-jsx-className? t))
(require 'init-neotree)
(require 'init-company) ;; tern is in here too
(require 'init-ivy)
(require 'init-swiper)
(require 'init-avy)
(require 'init-projectile)

;; Javascript
(require 'init-js)

;; pSQL
(require 'init-psql)

;; ruby
(require 'init-ruby)

;; Lastly
(require 'init-mac)
(require 'bindings)
(require 'hooks)

(provide 'init)
;;; init.el ends here
