;; Project management and organization

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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (projectile-global-mode))
