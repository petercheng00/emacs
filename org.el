;; perspective
;; (persp-mode)
;; (require 'persp-projectile)

;; use projectile
(projectile-global-mode)

;; projectile cache
(setq projectile-enable-caching t)

;; helm projectile
(require 'helm-projectile)
(helm-projectile-on)
