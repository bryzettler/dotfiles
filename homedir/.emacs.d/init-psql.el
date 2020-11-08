(add-hook 'sql-postgres
	  (lambda ()
	    (setq sql-postgres-login-params
		  '((user :default "postgres")
		    (database :default "postgres")
		    (server :default "localhost")
		    (port :default 5432)))))

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)))

(provide 'init-psql)
;;; init-psql.el ends here
