(add-to-list 'load-path "~/emacs/lisp")

;; Init files
(load-library "c-dev")
(load-library "editor")
(load-library "util")
(load-library "cmd")
(load-library "system")

;; Projects
(global-ede-mode t)

(ede-cpp-root-project "Matterport"
                :name "Matterport"
                :file "~/mp/mp_vision/CMakeLists.txt"
                :system-include-path '("/usr/include/c++/4.8.2")
)

;; Keybinds
(global-set-key [f5] 'compile)
(global-set-key [f9] 'deft)

(global-set-key (kbd "M-.") 'semantic-ia-fast-jump)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-c i") 'send-invisible)
(global-set-key (kbd "C-c l") 'eassist-list-methods)
(global-set-key (kbd "C-c o") 'eassist-switch-h-cpp)
(global-set-key (kbd "C-c C-d") 'delete-horizontal-space-forward)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x x") 'shell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
