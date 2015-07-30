;; Global system-level settings

;; Jump between windows
(use-package ace-window
  :ensure t
  :defer t)

;; Set-based completion
(use-package avy
  :ensure t
  :defer t)
(use-package avy-zap
  :ensure t
  :defer t)

;; Calculator
(use-package calculator
  :ensure t
  :defer t)

;; Move between frames with arrow keys
(use-package framemove
  :ensure t
  :defer t
  :config
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

;; Helm
(use-package helm
  :ensure t
  :defer t
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
  :defer t)

;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/name-width 10)
  (setq sml/mode-width 'full)
  (setq sml/shorten-modes t)
  (let ((which-func '(which-func-mode ("" which-func-format " "))))
    (setq-default mode-line-format (remove which-func mode-line-format))
    (setq-default mode-line-misc-info (remove which-func mode-line-misc-info))
    (setq cell (last mode-line-format 8))
    (setcdr cell
            (cons which-func
                  (cdr cell))))
  (sml/setup))
(use-package smart-mode-line-powerline-theme
  :ensure t
  :config
  (setq sml/theme 'powerline))

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

;; Close the compilation window if there was no error at all.
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
		 (bury-buffer "*compilation*")
		 (winner-undo)
		 (message "Build successful."))
		(t
		 (message "Compilation exited abnormally: %s" string))))
