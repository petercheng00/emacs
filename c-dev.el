;; C/C++ Development Settings

;; Ccls
(when (file-directory-p "/home/pcheng/libraries/ccls")
    (require 'ccls)
    (setq ccls-executable "/home/pcheng/libraries/ccls/release/ccls")
    (setq ccls-extra-init-params '(:index (:threads 2))))

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

(add-hook 'c++-mode-hook 'lsp-ccls-enable)
