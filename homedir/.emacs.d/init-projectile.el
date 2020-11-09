(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy))
;; (add-hook 'projectile-after-switch-project-hook (lambda ()
;;     (projectile-invalidate-cache nil))))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  (setq projectile-enable-caching nil)
  ;; overwrite default projectile functions with counsel-projectile alternatives
  (define-key projectile-mode-map (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
  (define-key projectile-mode-map (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
  (define-key projectile-mode-map (kbd "C-c p d") 'counsel-projectile-find-dir)
  ;; currently overwriting projectile-find-file-in-directory (seems pointless)
  (define-key projectile-mode-map (kbd "C-c p l") 'counsel-projectile)
  (define-key projectile-mode-map (kbd "C-c p f") 'counsel-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c p p") 'counsel-projectile-switch-project)
  (define-key projectile-mode-map (kbd "C-c p s g") 'counsel-projectile-grep)
  (define-key projectile-mode-map (kbd "C-c p s s") 'counsel-projectile-ag))

(provide 'init-projectile)
;;; init-projectile.el ends here
