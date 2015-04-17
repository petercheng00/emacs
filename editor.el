(require 'uniquify)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;; zenburn theme
(add-to-list 'custom-theme-load-path "~/emacs/themes/")
(load-theme 'zenburn t)

;; disable startup screen
(setq inhibit-startup-message t)

;; remove bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; line numbers
(global-linum-mode t)
(add-hook 'shell-mode-hook (lambda ()
                             (linum-mode -1)))

;; column number
(column-number-mode 1)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; cycle backwards without retriggering C-u
(setq mark-command-repeat-pop t)


(defun delete-horizontal-space-forward ()
      "*Delete all spaces and tabs after point."
      (interactive "*")
      (delete-region (point) (progn (skip-chars-forward " \t") (point))))
