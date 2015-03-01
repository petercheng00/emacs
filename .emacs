;;;;;;;;;;;;;;;;;;;; paths to stuff
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/cedet-bzr/contrib/")

;;;;;;;;;;;;;;;;;;;; Custom CEDET
(load-file "~/.emacs.d/cedet-bzr/cedet-devel-load.el")
(semantic-mode 1)
(require 'cc-mode)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(require 'semantic-tag-folding)

;;enables global support for Semanticdb;
(global-semanticdb-minor-mode)
;;enables automatic bookmarking of tags that you edited, so you can return to them later with the semantic-mrub-switch-tags command;
(global-semantic-mru-bookmark-mode)
;;activates CEDET's context menu that is bound to right mouse button;
;;(global-cedet-m3-minor-mode)
;;activates highlighting of first line for current tag (function, class, etc.);
(global-semantic-highlight-func-mode)
;;activates mode when name of current tag will be shown in top line of buffer;
(global-semantic-stickyfunc-mode)
;;activates use of separate styles for tags decoration (depending on tag's class). These styles are defined in the semantic-decoration-styles list;
(global-semantic-decoration-mode)
;;activates highlighting of local names that are the same as name of tag under cursor;
(global-semantic-idle-local-symbol-highlight-mode)
;;activates automatic parsing of source code in the idle time; Causes annoying cursor jumps on idle though
;;(global-semantic-idle-scheduler-mode)
;;activates displaying of possible name completions in the idle time. Requires that global-semantic-idle-scheduler-mode was enabled;
;;(global-semantic-idle-completions-mode)
;;activates displaying of information about current tag in the idle time. Requires that global-semantic-idle-scheduler-mode was enabled.
;;(global-semantic-idle-summary-mode)
;; tag folding
(global-semantic-tag-folding-mode)
;;;;;;;; EDE
(global-ede-mode t)

(ede-cpp-root-project "Matterport"
                :name "Matterport"
                :file "~/matterport/CMakeLists.txt"
                :system-include-path '("/usr/include/c++/4.8.2")
)


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

(ensure-package-installed 'magit 'function-args)

;; activate installed packages
(package-initialize)

;; deft
(require 'deft)

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
;; (setq-default indent-tabs-mode nil)
;; yes tabs
(setq-default c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode t)

;; c++ format
(setq c-default-style "linux"
          c-basic-offset 4)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; mark ring
(setq mark-command-repeat-pop t)

;;;;;;;;;;;;;;;;;;;; utility
;; no backups
(setq make-backup-files nil)

;; ido mode
(ido-mode t)

;; eassist
(require 'eassist)

;; windmove
(when (fboundp 'windmove-default-keybindings)
 (windmove-default-keybindings 'meta))

;; framemove
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; eshell bash tab completion
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
	(lambda (status code msg)
          ;; If M-x compile exists with a 0
          (when (and (eq status 'exit) (zerop code))
            ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*"))
          ;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))

;;;;;;;;;;;;;;;;;;;; function-args
(require 'function-args)
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)

;;;;;;;;;;;;;;;;;;;; keybinds
;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;; semantic
(global-set-key (kbd "M-.") 'semantic-ia-fast-jump)
;; shell
(global-set-key (kbd "C-x x") 'shell)
;; send invisible
(global-set-key (kbd "C-c i") 'send-invisible)
;; compile
(global-set-key [f5] 'compile)
;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; eassist
(global-set-key (kbd "C-c o") 'eassist-switch-h-cpp)
(global-set-key (kbd "C-c l") 'eassist-list-methods)
;; semantic folding
(global-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
(global-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)
;; deft
(global-set-key [f9] 'deft)
