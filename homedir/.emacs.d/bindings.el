;; navigation
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "M-<right>") 'forward-word)
(define-key global-map (kbd "M-<left>") 'backward-word)

;; multiple cursors
(define-key global-map (kbd "M-m") 'mc/mark-next-like-this)
(define-key global-map (kbd "M-u") 'mc/mark-all-like-this)

;; ivy
(define-key global-map (kbd "C-x b") 'ivy-switch-buffer)
(define-key global-map (kbd "C-x C-f") 'counsel-find-file)

;; swiper
(define-key global-map (kbd "C-s") 'swiper)
(define-key global-map (kbd "C-c C-r") 'ivy-resume)
(define-key global-map (kbd "M-x") 'counsel-M-x)

;; avy
(define-key global-map (kbd "M-s") 'avy-goto-char-timer)

;; emmet
(define-key global-map (kbd "C-j") 'emmet-expand-line)

;; expand-region
(define-key global-map (kbd "C-x ;") 'er/expand-region)

;; evil-nerd-commentor
(define-key global-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; Neotree
(global-unset-key (kbd "C-x f"))
(define-key global-map (kbd "C-x f") 'neotree-project-dir)
(define-key neotree-mode-map (kbd "RET") 'neotree-enter-hide)
(define-key neotree-mode-map (kbd "C-@") 'neotree-peek)
(define-key neotree-mode-map (kbd "TAB") 'neotree-peek)

;; Company
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "TAB") 'complete-selection-and-expand-snippet)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)()

(provide 'bindings)
;;; bindings ends here;; navigation
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "M-<right>") 'forward-word)
(define-key global-map (kbd "M-<left>") 'backward-word)

;; multiple cursors
(define-key global-map (kbd "M-m") 'mc/mark-next-like-this)
(define-key global-map (kbd "M-u") 'mc/mark-all-like-this)

;; ivy
(define-key global-map (kbd "C-x b") 'ivy-switch-buffer)
(define-key global-map (kbd "C-x C-f") 'counsel-find-file)

;; swiper
(define-key global-map (kbd "C-s") 'swiper)
(define-key global-map (kbd "C-c C-r") 'ivy-resume)
(define-key global-map (kbd "M-x") 'counsel-M-x)

;; avy
(define-key global-map (kbd "M-s") 'avy-goto-char-timer)

;; emmet
(define-key global-map (kbd "C-j") 'emmet-expand-line)

;; expand-region
(define-key global-map (kbd "C-x ;") 'er/expand-region)

;; evil-nerd-commentor
(define-key global-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

;; Neotree
(define-key global-map (kbd "C-x C-j") 'neotree-project-dir)
(define-key neotree-mode-map (kbd "RET") 'neotree-enter-hide)
(define-key neotree-mode-map (kbd "C-@") 'neotree-peek)
(define-key neotree-mode-map (kbd "TAB") 'neotree-peek)

;; Company
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "TAB") 'complete-selection-and-expand-snippet)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)()

(provide 'bindings)
;;; bindings ends here
