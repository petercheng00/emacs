;; Init file for emacs setup.
;; Load this from .emacs

;; Path to local libraries
(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/lisp")

;; Init files
(load-library "packages")
(load-library "c-dev")
(load-library "cmd")
(load-library "editor")
(load-library "org")
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
(global-set-key (kbd "M-;") 'comment-line-or-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-?") 'company-complete)
(global-set-key (kbd "M-n") 'gcm-scroll-down)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-p") 'gcm-scroll-up)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)

(global-set-key (kbd "C-.") 'rtags-imenu)

(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "C-c =") '(lambda () (interactive) (change-font-height +4)))
(global-set-key (kbd "C-c -") '(lambda () (interactive) (change-font-height -4)))
(global-set-key (kbd "C-c d") 'delete-horizontal-space)
(global-set-key (kbd "C-c f") 'helm-projectile-find-file-in-known-projects)
(global-set-key (kbd "C-c i") 'send-invisible)
(global-set-key (kbd "C-c k") 'eshell-clear-buffer)
(global-set-key (kbd "C-c m") 'remove-dos-eol)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c x") 'eshell)
(global-set-key (kbd "C-c z") 'avy-zap-to-char)

(global-set-key (kbd "C-c C-SPC") 'avy-goto-char-2)
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)
(global-set-key (kbd "C-c C-r") 'query-replace)
(global-set-key (kbd "C-c C-s") 'sp-slurp-hybrid-sexp)
(global-set-key (kbd "C-c C-x") 'create-eshell)

(global-set-key (kbd "C-c C-S-r") 'rtags-rename-symbol)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x r") 'rename-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
