(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))
