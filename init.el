(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(require 'use-package)
(setq use-package-always-ensure t)

(org-babel-load-file "~/emacs/config.org")
