;; mac specific settings
;; Ensure environment variables inside Emacs look the same as in the user's shell.
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-mac)
;;; init-mac.el ends here
