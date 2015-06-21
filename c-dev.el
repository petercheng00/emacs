;; (add-to-list 'load-path "/home/pcheng/libraries/cedet/")
;; (add-to-list 'load-path "/home/pcheng/libraries/cedet/contrib/")
;; (add-to-list 'load-path "/home/pcheng/libraries/function-args/")
;; (require 'cedet-devel-load)
;; (require 'cedet-contrib-load)
;; (require 'function-args)
;; (require 'eassist)


(require 'cc-mode)

;; semantic
;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
;; (semantic-mode 1)

;; ;; function-args
;; (fa-config-default)
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (set-default 'semantic-case-fold t)

;; c++ format
(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode nil)

(setq c-default-style "linux"
      c-basic-offset 4)

;; don't indent namespace
(c-set-offset 'innamespace 0)
