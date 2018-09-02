
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
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default)))
 '(ido-vertical-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files nil)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (org-bullets flycheck-kotlin flycheck-clojure flycheck-lua flycheck company-lua company lua-mode smex ido-vertical-mode projectile flx-ido aggressive-indent aggressive-indent-mode cider magit paredit clojure-mode monokai-theme rainbow-delimiters which-key ivy avy general use-package)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light)))))

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

(general-define-key
 :prefix "C-c"
 "s"	'projectile-grep)

(general-define-key
 :prefix "C-c"
 "."	'cider-find-dwim)

(general-define-key
 "M-;" 'avy-goto-char-timer)


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
(setq org-hide-emphasis-markers t)

;; from https://zzamboni.org/post/beautifying-org-mode-in-emacs/

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
 '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))

(add-hook 'org-mode-hook 'variable-pitch-mode)

(custom-theme-set-faces
 'user
 '(org-block                 ((t (:inherit fixed-pitch))))
 '(org-document-info         ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link                  ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value        ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))


;;;; fennel

(autoload 'fennel-mode "~/.emacs.d/fennel-mode/fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

