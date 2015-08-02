;; Editor appearance, navigation, and input

;; Always maintain correct indentation
(use-package aggressive-indent
  :ensure t
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  (global-aggressive-indent-mode))

;; Completion front-end
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-async-timeout 10))

;; Live syntax checker
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; Show git line diffs in gutter
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode))

;; Guess indentation style
(require 'guess-style)
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
(add-hook 'c-mode-common-hook 'guess-style-guess-all)
(global-guess-style-info-mode 1)

;; Highlight current line number
(use-package hlinum
  :ensure t
  :config
  (hlinum-activate)
  (set-face-attribute 'linum-highlight-face nil
                      :background "#8FB28F"))

;; Parentheses navigation
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

;; Templating framework
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Auto update buffers after external changes
(global-auto-revert-mode 1)

;; Remove bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Line numbers
(global-linum-mode t)

;; No col/line in modeline
(column-number-mode -1)
(line-number-mode -1)

;; Fringe size
(fringe-mode '(15 . 15))

;; Show current function
(which-function-mode)

;; Show matching parentheses
(show-paren-mode 1)

;; Cycle backwards without retriggering C-u
(setq mark-command-repeat-pop t)

;; Break subwords by camel case
(add-hook 'prog-mode-hook 'subword-mode)

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
