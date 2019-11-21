;; Shell/Eshell/Term related settings

;; Hide passwords
;; (add-hook 'comint-output-filter-functions
          ;; 'comint-watch-for-password-prompt)

;; ;; Hook for shell mode settings
;; (add-hook 'shell-mode-hook 'shell-hook)
;; (add-hook 'eshell-mode-hook 'shell-hook)

;; ;; Open shell in current buffer
;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*shell*")
;;               display-buffer-same-window))

;; (defun shell-hook ()
;;   (linum-mode -1)
;;   (setq show-trailing-whitespace nil))

;; ;; Eshell aliases
;; (defalias 'ff 'find-file)

;; (defun create-shell ()
;;   "creates a shell with a given name"
;;   (interactive);; "Prompt\n shell name:")
;;   (let ((shell-name (read-string "shell name: " nil)))
;;     (shell (concat "*" shell-name "*"))))

;; (defun create-eshell ()
;;   "creates an eshell with a given name"
;;   (interactive);; "Prompt\n eshell name:")
;;   (let ((eshell-name (read-string "eshell name: " nil)))
;;     (eshell '1)
;;     (rename-buffer (concat "*" eshell-name "*"))))

;; (defun eshell-ido-complete-command-history ()
;;   (interactive)
;;   (eshell-kill-input)
;;   (insert
;;    (ido-completing-read "Run command: " (delete-dups (ring-elements eshell-history-ring))))
;;   (eshell-send-input))
