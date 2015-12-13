;; Project management and organization

;; Notes
(use-package deft
  :ensure t
  :config
  (setq deft-auto-save-interval 0))

;; Helm interface to projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; Project grouping
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (projectile-global-mode))
