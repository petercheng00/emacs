;; Package management

;; Setup melpa
(require 'package)
(setq package-enable-at-startup nil) ; To avoid initializing twice
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Bundles package installation and configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
