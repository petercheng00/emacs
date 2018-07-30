;; Custom-set things that originally go to .emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes
   '("01b2830f44925d13b3e34eba4d1dd34af4c6c197aeb53fbe0f52aefe13e60f0d" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(fci-rule-color "#383838")
 '(global-company-mode t)
 '(line-number-mode nil)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n100"))
 '(package-selected-packages
   '(ein helm-projectile company-lsp magit-todos multiple-cursors rg helm-rg lsp-ui lsp-mode helm-ag yasnippet anzu groovy-mode ws-butler transpose-frame smooth-scrolling smartparens rainbow-delimiters pdf-tools org magit hlinum highlight-indent-guides golden-ratio git-gutter-fringe framemove flycheck fireplace deft company bash-completion avy-zap ace-window 0xc))
 '(safe-local-variable-values
   '((eval when
           (require 'rainbow-mode nil t)
           (rainbow-mode 1))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "#7F9F7F"))))
 '(mode-line-inactive ((t (:background "Black" :foreground "gray60" :box (:line-width -3 :color "black") :slant normal))))
 '(rtags-warnline ((t (:background "#366060"))))
 '(whitespace-tab ((t (:foreground "gray40")))))
