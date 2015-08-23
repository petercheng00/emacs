;; C/C++ Development Settings

;; Frontends for irony
(use-package company-irony
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; Clang completion
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Clang parsing (use-package doesn't like for some reason)
;; (use-package rtags
;;   :ensure t)

;; General indentation settings
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Set specific indentation levels
(c-set-offset 'substatement-open 0)
(c-set-offset 'innamespace 0)

;; Indent macros with surrounding code
(c-set-offset (quote cpp-macro) 0 nil)
