(require 'uniquify)
(require 'whitespace)

;; auto update buffers
(global-auto-revert-mode 1)

;; zenburn theme
(add-to-list 'custom-theme-load-path "/home/pcheng/emacs/themes/")
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

;; tabs
(setq-default indent-tabs-mode t)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; show whitespace characters
(global-whitespace-mode)
(setq whitespace-style nil)
(setq whitespace-style '(face tab-mark tabs))
(set-face-attribute 'whitespace-tab nil
                    :background nil
                    :foreground "gray40")
;; (set-face-attribute 'whitespace-indentation nil
;;                     :background nil
;;                     :foreground "gray40")
;; (set-face-attribute 'whitespace-space nil
;;                     :background nil
;;                     :foreground "#3F3F3F")

;; cycle backwards without retriggering C-u
(setq mark-command-repeat-pop t)

;; always do alignment with spaces
;; (defadvice align-regexp (around align-regexp-with-spaces activate)
;;   (let ((indent-tabs-mode nil))
;;     ad-do-it))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(defun delete-horizontal-space-forward ()
      "*Delete all spaces and tabs after point."
      (interactive "*")
      (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun change-font-height (delta)
  (set-face-attribute 'default 
                      (selected-frame)
                      :height (+ (face-attribute 'default :height) delta)))
