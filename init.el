(add-to-list 'load-path "/home/pcheng/emacs/lisp")

;; Init files
(load-library "packages")
(load-library "c-dev")
(load-library "editor")
(load-library "util")
(load-library "cmd")
(load-library "system")

;; Projects
(global-ede-mode t)

(ede-cpp-root-project "Matterport"
                :name "Matterport"
                :file "/home/pcheng/mp/mp_vision/CMakeLists.txt"
                :system-include-path '("/usr/include/c++/4.8.2")
)

;; Keybinds
(global-set-key [C-mouse-4] '(lambda () (interactive) (change-font-height +4)))
(global-set-key [C-mouse-5] '(lambda () (interactive) (change-font-height -4)))

(global-set-key [f5] 'compile)
(global-set-key [f9] 'deft)

(global-set-key (kbd "M-.") 'semantic-ia-fast-jump)
(global-set-key (kbd "M-?") 'semantic-ia-complete-symbol)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-c =") '(lambda () (interactive) (change-font-height +4)))
(global-set-key (kbd "C-c -") '(lambda () (interactive) (change-font-height -4)))
(global-set-key (kbd "C-c i") 'send-invisible)
(global-set-key (kbd "C-c l") 'eassist-list-methods)
(global-set-key (kbd "C-c o") 'eassist-switch-h-cpp)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c x") 'create-shell)

(global-set-key (kbd "C-c C-d") 'delete-horizontal-space-forward)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x r") 'rename-buffer)
(global-set-key (kbd "C-x x") 'shell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
