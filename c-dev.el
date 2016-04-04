;; C/C++ Development Settings

;; Rtags
(use-package rtags
  :ensure t
  :config
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (setq rtags-use-helm t)
  (eval-after-load 'company
    '(push 'company-rtags company-backends)))

;; General indentation settings
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Set specific indentation levels
(c-set-offset 'substatement-open 0)
(c-set-offset 'innamespace 0)

;; Indent macros with surrounding code
(c-set-offset (quote cpp-macro) 0 nil)

;; Compile command
(setq compile-command "make -C ~/mp/build/ -j6 base_app")

;; Open .h and .cl files in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c++-mode))
