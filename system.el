(require 'calculator)
(require 'cl)
(require 'eassist)
(require 'framemove)

;; ignore magit warning
(setq magit-last-seen-setup-instructions "1.4.0")

;; save window configs
(winner-mode)

;; no backups
(setq make-backup-files nil)

;; save sessions
(desktop-save-mode 1)

;; ido mode
(ido-mode t)

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
