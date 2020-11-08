(use-package company
  :diminish company-mode
  :config
  (global-company-mode 1)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 2
   company-tooltip-align-annotations t
   company-tooltip-limit 10
   company-tooltip-flip-when-above t
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-require-match nil
   company-begin-commands '(self-insert-command)
   company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-files))

;; tern (js)
(use-package tern
  :diminish tern-mode
  :config
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'rjsx-mode-hook 'tern-mode)
  (setq tern-command (append tern-command '("--no-port-file"))))

;; Company integration for tern (js)
(use-package company-tern
  :init (add-to-list 'company-backends '(company-tern company-web-html :with company-yasnippet))
  :config (setq company-tern-property-marker nil))

;; pos-tips
(use-package pos-tip)

(provide 'init-company)
;;; init-company ends here
