(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa". "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; update package archives
  (package-install 'use-package)) ; install most recent version of use-package

(require 'use-package)
;; always download packages if not already installed
(setq use-package-always-ensure t)

(provide 'init-package)
;;; init-package ends here
