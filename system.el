;; Global system-level settings

;; Jump between windows
(use-package ace-window
  :ensure t)

;; Set-based completion
(use-package avy
  :ensure t)
(use-package avy-zap
  :ensure t)

;; Calculator
(use-package calculator
  :ensure t
  :defer t)

;; Move between frames with arrow keys
(use-package framemove
  :ensure t
  :config
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

;; Helm
(use-package helm
 :ensure t
 :config
 (require 'helm-config)
 (require 'helm-eshell)
 (helm-autoresize-mode 1)
 (setq helm-split-window-in-side-p t)
 (setq helm-recentf-fuzzy-match t)
 (setq helm-buffers-fuzzy-matching t)
 (setq helm-locate-fuzzy-match t)
 (setq helm-M-x-fuzzy-match t)
 (setq helm-imenu-fuzzy-match t)
 (setq helm-apropos-fuzzy-match t)
 (setq helm-lisp-fuzzy-completion t)
 (helm-mode 1)
 (add-hook 'eshell-mode-hook
           #'(lambda ()
               (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history))))

;; Git porcelain
(use-package magit
  :ensure t)

;; Dependency for rtags
(use-package popup
  :ensure t)

;; Hide minor modes
(use-package rich-minority
  :ensure t
  :config
  (setq rm-blacklist
        (mapconcat 'identity 
                   '(
                     " ,"
                     " =>"
                     " Abbrev"
                     " company"
                     " GitGutter"
                     " Helm"
                     " Irony"
                     " Projectile.*"
                     " SP"
                     " yas")
                   "\\\|")))

;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/override-theme nil)
  (setq sml/name-width 35)
  (setq sml/mode-width 'right)
  (setq sml/no-confirm-load-theme t)
  ;; (setq sml/theme 'dark)
  (if after-init-time
      (sml/setup)
    (add-hook 'after-init-hook 'sml/setup)))

;; (use-package smart-mode-line-powerline-theme
;;   :ensure t
;;   :config
;;   (setq sml/theme 'powerline))

;; Ediff split settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; Disable startup screen
(setq inhibit-startup-message t)

;; Save window configs
(winner-mode)

;; No backups
(setq make-backup-files nil)

;; Save sessions
(desktop-save-mode 1)

;; Windmove
(when (fboundp 'windmove-default-keybindings)
 (windmove-default-keybindings 'meta))
