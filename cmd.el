(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)


(add-hook 'shell-mode-hook 'shell-hook)
(add-hook 'eshell-mode-hook 'shell-hook)

;;Make eshell behave like shell
(add-hook 'eshell-mode-hook
		  (lambda ()
		(local-set-key (kbd "M-r") 'eshell-ido-complete-command-history)
		(local-set-key (kbd "<up>") 'previous-line)
		(local-set-key (kbd "<down>") 'next-line)
		(local-set-key (kbd "<left>") 'left-char)
		(local-set-key (kbd "<right>") 'right-char)
	  ))



(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
	  (shell (concat "*" shell-name "*"))))

(defun create-eshell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
	  (eshell (concat "*" shell-name "*"))))

(defun shell-hook ()
  (linum-mode -1)
  (setq show-trailing-whitespace nil))

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
