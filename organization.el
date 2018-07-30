;; Project management and organization

;; Notes
(use-package deft
  :ensure t
  :config
  (setq deft-auto-save-interval 0)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-extension "org")
  (setq deft-directory "~/org")
  (setq deft-text-mode 'org-mode))

;; Helm interface to projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; Organizer
(use-package org
  :ensure t)

;; Org->hugo
(use-package ox-hugo
  :ensure t
  :after ox)

;; Project grouping
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (projectile-mode))
