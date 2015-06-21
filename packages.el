(require 'package)
(setq package-enable-at-startup nil) ; To avoid initializing twice
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
