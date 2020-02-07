;; Global system-level settings

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

;; LSP support
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "ccls" "-init={\"clang\": {\"excludeArgs\": [\"-fopenmp=libomp\"]}}"))
  (add-to-list 'eglot-server-programs '((python-mode) "/home/pcheng/anaconda3/bin/pyls"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

;; Jupyter support
(use-package ein
  :ensure t
  :config
  (setq ein:jupyter-default-notebook-directory "~"))

;; Helm
(use-package helm
 :ensure t
 :config
 (require 'helm-config)
 (helm-autoresize-mode 1)
 (setq helm-split-window-in-side-p t)
 (setq helm-recentf-fuzzy-match t)
 (setq helm-buffers-fuzzy-matching t)
 (setq helm-locate-fuzzy-match t)
 (setq helm-M-x-fuzzy-match t)
 (setq helm-imenu-fuzzy-match t)
 (setq helm-apropos-fuzzy-match t)
 (setq helm-lisp-fuzzy-completion t)
 (setq helm-exit-idle-delay 0)
 (helm-mode 1))

(use-package helm-rg
  :ensure t)

;; Git porcelain
(use-package magit
  :ensure t
  :config
  (setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n100")))
  (setq magit-diff-arguments (quote ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat"))))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t)

;; Space modeline
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (setq powerline-default-separator 'slant)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (which-function-mode)
  (spaceline-toggle-which-function-on))

(use-package sudo-edit
  :ensure t)

;; Transpose frames
(use-package transpose-frame
  :ensure t)

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

;; Flash modeline instead of bell sound
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#555555")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(defun fullscreen-triple ()
  (interactive)
  (toggle-frame-fullscreen)
  (when (< (count-windows) 3)
      (split-window-horizontally)
      (balance-windows)))
