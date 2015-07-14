(require 'calculator)
(require 'cl)
(require 'flx-ido)
(require 'framemove)

;; helm
(require 'helm)
(require 'helm-config)
(helm-autoresize-mode 1)

(setq helm-split-window-in-side-p t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-locate-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)

(helm-mode 1)

;; helm shell
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; smart mode line
(setq sml/theme 'powerline)
(setq sml/no-confirm-load-theme t)
(setq sml/name-width 15)
(setq sml/mode-width 10)
(setq sml/shorten-modes t)
(let ((which-func '(which-func-mode ("" which-func-format " "))))
  (setq-default mode-line-format (remove which-func mode-line-format))
  (setq-default mode-line-misc-info (remove which-func mode-line-misc-info))
  (setq cell (last mode-line-format 8))
  (setcdr cell
          (cons which-func
                (cdr cell))))
(sml/setup)

;; ignore magit warning
(setq magit-last-seen-setup-instructions "1.4.0")

;; save window configs
(winner-mode)

;; no backups
(setq make-backup-files nil)

;; save sessions
(desktop-save-mode 1)

;; windmove
(when (fboundp 'windmove-default-keybindings)
 (windmove-default-keybindings 'meta))

;; framemove
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; Close the compilation window if there was no error at all.
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
		 (bury-buffer "*compilation*")
		 (winner-undo)
		 (message "Build successful."))
		(t
		 (message "Compilation exited abnormally: %s" string))))
