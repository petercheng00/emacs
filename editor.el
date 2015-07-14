(require 'git-gutter-fringe)
(require 'guess-style)
(require 'uniquify)
(require 'whitespace)
(require 'hlinum)
(require 'indent-guide)
(require 'yasnippet)

;; yasnippet
(yas-global-mode 1)

;; auto update buffers
(global-auto-revert-mode 1)

;; zenburn theme
(add-to-list 'custom-theme-load-path "/home/pcheng/emacs/themes/")
(load-theme 'zenburn t)

;; git gutter
(global-git-gutter-mode)

;; modeline font size
(let ((faces '(mode-line
               mode-line-buffer-id
               mode-line-emphasis
               mode-line-highlight
               mode-line-inactive)))
  (mapc
   (lambda (face) (set-face-attribute face nil :font "DejaVu Sans Mono-10"))
   faces))

;; disable startup screen
(setq inhibit-startup-message t)

;; remove bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; line numbers
(global-linum-mode t)
(hlinum-activate)
(set-face-attribute 'linum-highlight-face nil
                    :background "#8FB28F")

;; no col/line in modeline
(column-number-mode -1)
(line-number-mode -1)

;; fringe size
(fringe-mode '(15 . 15))

;; which function mode
(which-function-mode)

;; parentheses
(show-paren-mode 1)

;; guess indent style
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
(add-hook 'c-mode-common-hook 'guess-style-guess-all)
(global-guess-style-info-mode 1)

;; aggressive indent
(global-aggressive-indent-mode)
(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

;; cycle backwards without retriggering C-u
(setq mark-command-repeat-pop t)

;; camel case
(add-hook 'prog-mode-hook 'subword-mode)

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

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  ;; (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))
