;; C/C++ Development Settings

(use-package ccls
  :ensure t)

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

