;;; Misc

;; eshell
(defun file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename))))

(defun file-name-line-number-to-clipboard ()
  "Copy the current buffer file name and line number to the clipboard."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new (concat filename ":" (number-to-string (line-number-at-pos)))))))

(defun eslint-fix-file ()
  "Fix current file with exlint."
  (interactive)
  (shell-command (concat "eslint --fix " (buffer-file-name)))
  (revert-buffer t t))

(defun complete-selection-and-expand-snippet ()
  "Select the company auto-complete then try to expand."
  (interactive)
  (company-complete-selection)
  (yas-expand))

(defun pbcopy-region (start end)
      "Copy text between (START) and (END) to system clipboard."
      (interactive "r")
      (shell-command-on-region start end "pbcopy"))

(defun neo-open-file-hide (full-path &optional arg)
  "Open a file node and hides tree."
  (neo-global--select-mru-window arg)
  (find-file full-path)
  (neotree-hide))

(defun neotree-enter-hide (&optional arg)
  "Enters file and hides neotree directly"
  (interactive "P")
  (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

(defun neotree-project-dir ()
  "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
           (projectile-project-root)
           ;; (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(defun neotree-peek ()
  "Opens file leaving neotree open"
  (interactive)
  (let ((neo-window (neo-global--get-window)))
    (neotree-enter)
    (select-window neo-window)))

(provide 'defun)
;;; defun.el ends here
