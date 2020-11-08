(use-package web-mode
  :diminish web-mode
  :mode (("\\.ejs\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :config
  (setq
   web-mode-css-indent-offset 2
   web-mode-attr-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-pairing t)
   (add-hook 'web-mode-hook 'emmet-mode))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
