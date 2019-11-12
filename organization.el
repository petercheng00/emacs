;; Project management and organization

(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-use-rust-fd t))

;; Organizer
(use-package org
  :ensure t)

;; Org->hugo
(use-package ox-hugo
  :ensure t
  :after ox)

