;; Init file for emacs setup.
;; Load this from .emacs

;; Machine-specific things
(setq compile-command "ninja -C ~/mp/build-clang base_app unit_tests")

;; Path to local libraries
(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/lisp")

;; Init files
(load-library "packages")
(load-library "c-dev")
(load-library "cmd")
(load-library "editor")
(load-library "organization")
(load-library "system")

;; Customization file
(setq custom-file "~/emacs/custom.el")
(load custom-file)

;; Keybinds
(bind-key [C-mouse-4] '(lambda () (interactive) (change-font-height +4)))
(bind-key [C-mouse-5] '(lambda () (interactive) (change-font-height -4)))
(bind-key* [C-return] 'calculator)

(bind-key [f5] 'recompile)
(bind-key [f11] 'fullscreen-triple)

(bind-key "M-," 'xref-pop-marker-stack)
(bind-key "M-." 'xref-find-definitions)
(bind-key "M-;" 'comment-line-or-region)
;; (bind-key "M-/" 'hippie-expand)
(bind-key "M-/" 'company-complete)
(bind-key "M-n" 'gcm-scroll-down)
(bind-key "M-o" 'next-multiframe-window)
(bind-key "M-O" 'previous-multiframe-window)
(bind-key "M-p" 'gcm-scroll-up)
(bind-key "M-x" 'helm-M-x)
(bind-key "M-y" 'helm-show-kill-ring)
(bind-key "M-z" 'avy-zap-to-char-dwim)

(bind-key "C-c SPC" 'avy-goto-line)
(bind-key "C-c ." 'eglot-code-actions)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c d" 'delete-horizontal-space)
(bind-key "C-c e" 'flycheck-next-error)
(bind-key "C-c f" 'clang-format-region)
(bind-key "C-c h" 'eglot-help-at-point)
(bind-key "C-c i" 'send-invisible)
(bind-key "C-c l" 'aweshell-autosuggest)
(bind-key "C-c m" 'mc/edit-lines)
(bind-key "C-c o" 'helm-occur)
(bind-key "C-c r" 'replace-string)
(bind-key "C-c u" 'undo-propose)
(bind-key "C-c x" 'eshell)
(bind-key "C-c z" 'avy-zap-to-char)

(bind-key "C-c C-SPC" 'avy-goto-char-2)
(bind-key* "C-c C-l" 'helm-eshell-history)
(bind-key "C-c C-r" 'query-replace)
(bind-key "C-c C-x" 'create-eshell)

(bind-key "C-x b" 'helm-mini)
(bind-key "C-x g" 'magit-status)

(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-x C-f" 'helm-find-files)

(bind-key "C-h SPC" 'helm-all-mark-rings)
