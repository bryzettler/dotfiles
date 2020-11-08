(use-package solidity-mode
	     :diminish solidity-mode
	     :mode (("\\.sol\\'" . solidity-mode))
	     :init
	     (add-hook 'solidity-mode-hook
		       (lambda ()
			 (set (make-local-variable 'company-backends)
			      (append '((company-solidity company-capf company-dabbrev-code))
				      company-backends)))))

(provide 'init-sol)
;;; init-sol.el ends here
