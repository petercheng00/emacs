;; Editor appearance, navigation, and input

(use-package clang-format
  :ensure t)

(use-package cmake-mode
  :ensure t)

;; Completion front-end
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;; Show git line diffs in gutter
(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode))

;; Colorful delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Undo tree
(use-package undo-tree
:ensure t
:config
(global-undo-tree-mode)
 (setq undo-tree-auto-save-history t)
 (setq undo-tree-history-directory-alist
   (quote (("" . "~/.emacs.d/undo_hist")))))

;; Trim trailing whitespace on modified lines
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

;; Templating framework
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/emacs/snippets"))
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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Show matching parentheses
(show-paren-mode 1)

;; Cycle backwards without retriggering C-u
(setq mark-command-repeat-pop t)

;; Break subwords by camel case
(add-hook 'prog-mode-hook 'subword-mode)

(setq cmake-tab-width 4)

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
