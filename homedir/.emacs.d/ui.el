;; cleanup the ui
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; no blink
(blink-cursor-mode 0)
(set-frame-font "menlo:pixelsize=14")

(set-face-attribute 'trailing-whitespace nil
		    :background "red1"
		    :weight 'bold)

;; Maximize the Screen
(if (equal window-system 'x)
    (progn
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			     '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			     '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))

;; Remove splash screen
(setq inhibit-splash-screen t)

(setq pos-tip-background-color "#3F3F3F")
(setq pos-tip-foreground-color "#DCDCCC")

(provide 'ui)
