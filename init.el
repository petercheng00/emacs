; Set up straight so that we can set up org
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
; Use use-package as a frontend for straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

; Set up org so that we can load the rest of the config
(use-package org)

(org-babel-load-file "~/emacs/config.org")
