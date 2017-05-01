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
  ;;(global-aggressive-indent-mode))
  )

;; Completion front-end
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-async-timeout 10)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; Close stuff
(use-package general-close
  :ensure t)

;; Show git line diffs in gutter
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode))

;; (use-package highlight-indent-guides
  ;; :ensure t
  ;; :config
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; (setq highlight-indent-guides-method 'column))

(use-package multiple-cursors
  :ensure t)

;; Colorful delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Color-themed identifiers
(use-package rainbow-identifiers
  :ensure t)

(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 5)
  smooth-scrolling-mode)

;; Undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; Trim trailing whitespace on modified lines
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

;; Templating framework
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'ess-mode-hook 'yas-minor-mode)
  (add-hook 'markdown-mode-hook 'yas-minor-mode))

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
(add-hook 'prog-mode-hook 'linum-on)

;; No col/line in modeline
(column-number-mode -1)
(line-number-mode -1)

;; Fringe size
(fringe-mode '(15 . 15))

;; Sticky function headers
;; (semantic-mode t)
;; (global-semantic-stickyfunc-mode t)
;; (global-semantic-decoration-mode t)
;; (global-semantic-highlight-func-mode t)

;; Show matching parentheses
(show-paren-mode 1)

;; Cycle backwards without retriggering C-u
(setq mark-command-repeat-pop t)

;; Break subwords by camel case
(add-hook 'prog-mode-hook 'subword-mode)

;; Add lines when at end of buffer
(setq next-line-add-newlines t)

(defun delete-horizontal-space-forward ()
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun general-close-and-indent ()
  (interactive)
  (general-close)
  (indent-for-tab-command))

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

(defun comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))
