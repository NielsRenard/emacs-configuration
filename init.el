
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default)))
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (monokai-theme rainbow-delimiters which-key ivy avy general use-package)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/better-defaults")
(require 'better-defaults)

(use-package general
  :ensure t
  :config (general-define-key "C-'" 'avy-goto-word-1))

(use-package avy
  :ensure t
  :commands (avy-goto-word-1))

(use-package ivy :ensure t)

(general-define-key
 :prefix "C-c"
  "b"	'ivy-switch-buffer)

(use-package which-key :ensure t)

;; clojure
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package monokai-theme
  :ensure t
  :pin melpa)
