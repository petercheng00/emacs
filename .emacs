;; zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; keybinds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x x") 'shell)


;; disable startup screen
(setq inhibit-startup-message t)

;; eshell bash tab completion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

;; remove bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; highlight parens
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; line numbers
(global-linum-mode t)
(add-hook 'shell-mode-hook (lambda ()
			     (linum-mode -1)))

;; column number
(column-number-mode 1)

;; ido mode
(ido-mode t)

;; no backups
(setq make-backup-files nil)

