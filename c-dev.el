(add-to-list 'load-path "/home/pcheng/libraries/cedet/")
(add-to-list 'load-path "/home/pcheng/libraries/cedet/contrib/")
(require 'cedet-devel-load)
(require 'cedet-contrib-load)
(require 'cc-mode)

(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
(semantic-mode 1)

;; c++ format
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

(setq c-default-style "linux"
      c-basic-offset 4)
