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
 '(company-lua-interpreter (quote lua53))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default)))
 '(ido-vertical-mode t)
 '(inhibit-startup-screen t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (flycheck-kotlin flycheck-clojure flycheck-lua flycheck company-lua company lua-mode smex ido-vertical-mode projectile flx-ido aggressive-indent aggressive-indent-mode cider magit paredit clojure-mode monokai-theme rainbow-delimiters which-key ivy avy general use-package)))
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
(setq use-package-always-ensure t)
;;;; generic

;; starts garbage-collection after 20 megabytes
;; to improve performance
(setq gc-cons-threshold 20000000)

(add-to-list 'load-path "~/.emacs.d/better-defaults")
(require 'better-defaults)

(use-package general
  :ensure t
  :config (general-define-key "C-'" 'avy-goto-char-timer))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

;;;; looks

(use-package monokai-theme
  :ensure t
  :pin melpa)

(global-linum-mode)

;;;; navigation

(use-package projectile
  :ensure t
  :config (projectile-mode))

(general-define-key
 :prefix "C-c"
 "p"	'projectile-switch-project)

(general-define-key
 :prefix "C-c"
 "f"	'projectile-find-file)

(use-package flx-ido
  :requires ido
  :ensure t
  :config (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :requires ido
  :config (ido-vertical-mode))

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(use-package avy
  :ensure t
  :commands (avy-goto-char-timer))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package ivy :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))
(setq which-key-idle-delay 0.05)

(windmove-default-keybindings)

;;;; autocompletion

(use-package company
  :ensure t)

(use-package company-lua
  :ensure t
  :hook (lua-mode . company-mode)
  :config (setq company-idle-delay 0.05
                company-minimum-prefix-length 0))

;;;; syntax checking

(use-package flycheck
  :ensure t)

(use-package flycheck-clojure
  :ensure t)

(use-package flycheck-kotlin
  :ensure t)


;;;; clojure

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :ensure t)

(use-package paredit
  :ensure t
  :init (add-hook 'clojure-mode-hook #'enable-paredit-mode))


(use-package cider
  :ensure t
  :init (add-hook 'clojure-mode-hook #'cider-mode))

(use-package aggressive-indent
  :ensure t
  :init (add-hook 'clojure-mode-hook #'aggressive-indent-mode))


;;;; org

(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list "~/Documents/org/"))

;;;; fennel

(autoload 'fennel-mode "~/.emacs.d/fennel-mode/fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

