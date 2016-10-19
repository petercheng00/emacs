;; Global system-level settings

;; Jump between windows
(use-package ace-window
  :ensure t)

;; Show search status
(use-package anzu
  :ensure t
  :config
  (setq anzu-mode-lighter "")
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode +1))

;; Set-based completion
(use-package avy
  :ensure t)
(use-package avy-zap
  :ensure t)

;; Key bindings
(use-package bind-key
  :ensure t)

;; Calculator
(use-package calculator
  :ensure t
  :defer t)

;; Hide minor modes in modeline
(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode)
  (eval-after-load "subword" '(diminish 'subword-mode))
  (diminish 'ws-butler-mode)
  (diminish 'company-mode)
  (diminish 'git-gutter-mode)
  (diminish 'helm-mode)
  (diminish 'projectile-mode)
  (diminish 'flycheck-mode)
  (diminish 'undo-tree-mode)
  (diminish 'yas-minor-mode))

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
  :ensure t
  :config
  (setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n100"))))

;; Dependency for rtags
(use-package popup
  :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; Space modeline
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (setq powerline-default-separator 'slant)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))

;; Transpose frames
(use-package transpose-frame
  :ensure t)

;; Show time in modeline
(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(display-time-mode)

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

;; Scroll compilation buffer until first error
(setq compilation-scroll-output 'first-error)

;; Confirm quit
(setq confirm-kill-emacs 'y-or-n-p)

;; No vertical splits allowed
(setq split-height-threshold nil)

(defun fullscreen-triple ()
  (interactive)
  (toggle-frame-fullscreen)
  (when (< (count-windows) 3)
      (split-window-horizontally)
      (balance-windows)))
