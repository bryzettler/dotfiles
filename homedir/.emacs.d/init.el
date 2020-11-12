(defvar emacs-home "~/.dotfiles/homedir/.emacs.d/")
(add-to-list 'load-path emacs-home)
(defvar elpa-dir (concat emacs-home "elpa"))
(setq package-user-dir elpa-dir)
(setq user-full-name "Bry Zettler"
  user-mail-address "bryanzettler@gmail.com")
(provide 'init)
;;; init.el ends here
