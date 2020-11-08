;; General Customizations
(display-time)
(global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(show-paren-mode t)

(setq-default
 indent-tabs-mode nil
 truncate-lines 1
 inhibit-startup-message t
 font-lock-maximum-decoration t
 visible-bell nil
 resize-minibuffer-frame t
 transient-mark-mode t
 vc-follow-symlinks t
 edebug-trace t
 fill-adapt-mode t
 use-file-dialog nil
 indent-tabs-mode nil)

;; Tab 2
(setq tab-width 2)

;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; Disable autosave because it's slow
(setq auto-save-default nil)
(auto-save-mode -1)

(setq scroll-step 1)
(setq electric-pair-pairs '(
			    (?\' . ?\')
			    (?\` . ?\`)
			    ))

;; Cleanup whitespace on every save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; New Line Handling
(setq-default require-final-newline t
	      next-line-extends-end-of-buffer nil
	      next-line-add-newlines nil)

;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Scrolling
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Everything in UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")
(set-default-coding-systems             'utf-8)
(setq file-name-coding-system           'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-write           'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-terminal-coding-system             'utf-8)
(set-clipboard-coding-system            'utf-8)
(set-selection-coding-system            'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(add-to-list 'auto-coding-alist '("." . utf-8))

;; copy the text to the system clipboard
(setq x-select-enable-clipboard t)

;; Don't clutter up directories with files~
(defvar my/backup-directory (concat emacs-home "backups"))
(unless (file-exists-p my/backup-directory)
  (make-directory my/backup-directory t))
(setq backup-directory-alist `(("." . ,my/backup-directory))
      make-backup-files nil
      version-control nil
      backup-by-copying-when-linked nil
      delete-old-versions nil
      delete-by-moving-to-trash nil)

(provide 'config);; General Customizations
(display-time)
(global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(show-paren-mode t)

(setq-default
 indent-tabs-mode nil
 truncate-lines 1
 inhibit-startup-message t
 font-lock-maximum-decoration t
 visible-bell nil
 resize-minibuffer-frame t
 transient-mark-mode t
 vc-follow-symlinks t
 edebug-trace t
 fill-adapt-mode t
 use-file-dialog nil
 indent-tabs-mode nil)

;; Tab 2
(setq tab-width 2)

;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; Disable autosave because it's slow
(setq auto-save-default nil)
(auto-save-mode -1)

(setq scroll-step 1)
(setq electric-pair-pairs '(
                            (?\' . ?\')
                            (?\` . ?\`)
                            ))

;; Cleanup whitespace on every save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; New Line Handling
(setq-default require-final-newline t
          next-line-extends-end-of-buffer nil
          next-line-add-newlines nil)

;; Use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Scrolling
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Everything in UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")
(set-default-coding-systems             'utf-8)
(setq file-name-coding-system           'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-write           'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-terminal-coding-system             'utf-8)
(set-clipboard-coding-system            'utf-8)
(set-selection-coding-system            'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(add-to-list 'auto-coding-alist '("." . utf-8))

;; copy the text to the system clipboard
(setq x-select-enable-clipboard t)

;; Don't clutter up directories with files~
(defvar my/backup-directory (concat emacs-home "backups"))
(unless (file-exists-p my/backup-directory)
  (make-directory my/backup-directory t))
(setq backup-directory-alist `(("." . ,my/backup-directory))
      make-backup-files nil
      version-control nil
      backup-by-copying-when-linked nil
      delete-old-versions nil
      delete-by-moving-to-trash nil)

(provide 'config)
