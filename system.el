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
