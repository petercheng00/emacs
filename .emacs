(add-to-list 'load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;; install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'company 'magit) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)


;;;;;;;;;;;;;;;;;;;; appearance
;; zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
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

;;;;;;;;;;;;;;;;;;;; editor
;; no tabs
(setq-default indent-tabs-mode nil)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; highlight parens
(show-paren-mode 1)
(setq show-paren-style 'expression)


;;;;;;;;;;;;;;;;;;;; utility
(add-hook 'after-init-hook 'global-company-mode)
(setq ‘company-idle-delay’ 0)

;; no backups
(setq make-backup-files nil)

;; ido mode
(ido-mode t)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;; keybinds
;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;; company
(global-set-key (kbd "M-/") 'company-complete-common)
;; shell
(global-set-key (kbd "C-x x") 'shell)
;; windmove
(when (fboundp 'windmove-default-keybindings)
 (windmove-default-keybindings 'meta))
;; eshell bash tab completion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))
