(require 'calculator)
(require 'cl)
(require 'eassist)
(require 'flx-ido)
(require 'framemove)

;; ignore magit warning
(setq magit-last-seen-setup-instructions "1.4.0")

;; save window configs
(winner-mode)

;; no backups
(setq make-backup-files nil)

;; save sessions
(desktop-save-mode 1)

;; ido mode and flx-ido
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; windmove
(when (fboundp 'windmove-default-keybindings)
 (windmove-default-keybindings 'meta))

;; framemove
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; Close the compilation window if there was no error at all.
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
		 (bury-buffer "*compilation*")
		 (winner-undo)
		 (message "Build successful."))
		(t
		 (message "Compilation exited abnormally: %s" string))))
