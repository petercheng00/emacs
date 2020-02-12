;; Shell/Eshell/Term related settings

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-same-window))))
