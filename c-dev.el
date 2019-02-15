;; C/C++ Development Settings

;; Cquery
(when (file-directory-p "/home/pcheng/libraries/ccls")
  (use-package eglot
    :ensure t
    :config
    (add-hook 'c++-mode-hook 'eglot-ensure)))


  ;; (setq ccls-executable "~/libraries/ccls/Release/ccls"))
  ;; (use-package ccls
    ;; :hook ((c-mode c++-mode objc-mode) .
           ;; (lambda () (cl-pushnew #'company-lsp company-backends) (require 'ccls) (lsp)))))

;; General indentation settings
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Set specific indentation levels
(c-set-offset 'substatement-open 0)
(c-set-offset 'innamespace 0)

;; Indent macros with surrounding code
(c-set-offset (quote cpp-macro) 0 nil)

;; Open these files in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

