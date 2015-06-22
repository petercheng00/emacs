(add-to-list 'load-path "/home/pcheng/emacs/lisp")

;; Init files
(load-library "packages")
(load-library "org")
(load-library "c-dev")
(load-library "editor")
(load-library "util")
(load-library "cmd")
(load-library "system")
(load-library "custom")

;; Keybinds
(global-set-key [C-mouse-4] '(lambda () (interactive) (change-font-height +4)))
(global-set-key [C-mouse-5] '(lambda () (interactive) (change-font-height -4)))
(global-set-key [C-return] 'calculator)


(global-set-key [f5] 'compile)
(global-set-key [f9] 'deft)

(global-set-key (kbd "M-,") 'rtags-find-references-at-point)
(global-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-?") 'company-complete)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-z") 'zap-up-to-char)


(global-set-key (kbd "C-.") 'rtags-imenu)


(global-set-key (kbd "C-c =") '(lambda () (interactive) (change-font-height +4)))
(global-set-key (kbd "C-c -") '(lambda () (interactive) (change-font-height -4)))
(global-set-key (kbd "C-c d") 'delete-horizontal-space)
(global-set-key (kbd "C-c i") 'send-invisible)
(global-set-key (kbd "C-c k") 'eshell-clear-buffer)
(global-set-key (kbd "C-c m") 'remove-dos-eol)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c x") 'eshell)

(global-set-key (kbd "C-c C-r") 'query-replace)
(global-set-key (kbd "C-c C-x") 'create-eshell)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x r") 'rename-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
