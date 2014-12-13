(add-to-list 'load-path "~/.emacs.d/")
;;;;;;;;;;;;;;;;;;;; package managers
;; melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


;;;;;;;;;;;;;;;;;;;; appearance
;; zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; disable startup screen
(setq inhibit-startup-message t)

;; remove bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; line numbers
(global-linum-mode t)
(add-hook 'shell-mode-hook (lambda ()
                             (linum-mode -1)))

;; column number
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;; editor
;; no tabs
(setq-default indent-tabs-mode nil)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; highlight parens
(show-paren-mode 1)
(setq show-paren-style 'expression)


;;;;;;;;;;;;;;;;;;;; utility
;; minimap
(require 'minimap)

;; no backups
(setq make-backup-files nil)

;; ido mode
(ido-mode t)

;;;;;;;;;;;;;;;;;;;; keybinds
;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;; shell
(global-set-key (kbd "C-x x") 'shell)
;; windmove
(when (fboundp 'windmove-default-keybindings)
 (windmove-default-keybindings 'meta))
;; eshell bash tab completion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

;;;;;;;;;;;;;;;;;;;; customize-group generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-minimum-width 10)
 '(minimap-mode t)
 '(minimap-window-location (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "dim gray")))))
