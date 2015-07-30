;; Shell/Eshell/Term related settings

;; Hide passwords
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Hook for shell mode settings
(add-hook 'shell-mode-hook 'shell-hook)
(add-hook 'eshell-mode-hook 'shell-hook)

;; Add arrow key navigation to eshell
(add-hook 'eshell-mode-hook
		  (lambda ()
            (local-set-key (kbd "<up>") 'previous-line)
            (local-set-key (kbd "<down>") 'next-line)
            (local-set-key (kbd "<left>") 'left-char)
            (local-set-key (kbd "<right>") 'right-char)
            ))

(defun shell-hook ()
  (linum-mode -1)
  (setq show-trailing-whitespace nil))

;; Eshell aliases
(defalias 'ff 'find-file)

(defun create-shell ()
  "creates a shell with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun create-eshell ()
  "creates an eshell with a given name"
  (interactive);; "Prompt\n eshell name:")
  (let ((eshell-name (read-string "eshell name: " nil)))
    (eshell '1)
    (rename-buffer (concat "*" eshell-name "*"))))

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun eshell-ido-complete-command-history ()
  (interactive)
  (eshell-kill-input)
  (insert
   (ido-completing-read "Run command: " (delete-dups (ring-elements eshell-history-ring))))
  (eshell-send-input))
