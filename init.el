;;; package --- Summary:
;;; Commentary:
(package-initialize)

;;; Code:
;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


;;;; generic

;; starts garbage-collection after 20 megabytes
;; to improve performance
(setq gc-cons-threshold 20000000)

(add-to-list 'load-path "~/.emacs.d/better-defaults")
(require 'better-defaults)

;; disable minimizing frame
(global-unset-key (kbd "C-z"))

;;  version control
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

;;  epub support
(use-package nov
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; https://askubuntu.com/questions/30224/how-to-disable-the-alt-hotkey-behavior-on-gnome-terminal
;; hit this to fix whitespace, nice to use use together with M-^
(define-key global-map "\M-space" 'fixup-whitespace)

;; removes trailing whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

  (use-package zone
    :ensure t
    :config (zone-when-idle 120))


;;;; looks
(use-package monokai-theme
  :ensure t
  :config (setq inhibit-startup-screen t))

(use-package doom-themes
  :ensure t)

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; icons for neo-tree
(use-package all-the-icons
;; https://github.com/domtronn/all-the-icons.el#installing-fonts
;; In order for the icons to work it is very important that you install the Resource Fonts included
;; M-x all-the-icons-install-fonts
;; Bear in mind, this will also run fc-cache -f -v on MacOS and Linux which can take some time to complete.
  :ensure t)

;; mouse-wheel scrolling
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; set linenumbers by default
(global-linum-mode)

;; less clutter in mode-line
(use-package diminish
  :ensure t)

;;;; editing
(use-package multiple-cursors
  :ensure t
  :config  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
           (global-set-key (kbd "C->") 'mc/mark-next-like-this)
           (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
           (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))


;;;; navigation
(use-package zygospore
  :ensure t
  :config (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle)
          (setq neo-smart-open t)
          (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package general
  :ensure t
  :config (general-define-key "C-'" 'avy-goto-char-timer))

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :config (projectile-mode))

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

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
 "M-p" 'switch-to-prev-buffer)

(general-define-key
 "M-n" 'switch-to-next-buffer)

;;(use-package expand-region
;;  :bind ("M-x" . smex))

(use-package flx-ido
  :requires ido
  :ensure t
  :config (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode 1))

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
  :diminish which-key-mode
  :config (which-key-mode)
          (setq which-key-idle-delay 0.05))

(windmove-default-keybindings)


;;;; autocompletion
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-lua
  :ensure t
  :hook (lua-mode . company-mode)
  :config (setq company-idle-delay 0.05
                company-minimum-prefix-length 2))

(use-package company-ghci
  :ensure t
  :hook (haskell-mode . company-mode)
  :config (setq company-idle-delay 0.05
                company-minimum-prefix-length 2))

(use-package company-ghc
  :ensure t
  :hook (haskell-mode . company-mode)
  :config (setq company-idle-delay 0.05
                company-minimum-prefix-length 2))



;;;; syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-joker
  :ensure t)

(use-package flycheck-kotlin
  :ensure t)

(use-package flycheck-haskell
  :ensure t)

(use-package perlcritic
  :ensure t
  :config (setq flycheck-perlcritic-severity 1
                flycheck-perlcriticrc "~/.perlcritic.rc"))


;;;; clojure
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-repl-mode-hook #'enable-paredit-mode))

(defun my-clojure-mode-hook ()
  "Initialize clojure refactoring and code snippets."
  (yas-minor-mode 1) ;;for adding require/use/import statements
  )


(global-set-key (kbd "C-c t") #'transpose-sexps)

;;(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(use-package cider
  :ensure t
  :init (add-hook 'clojure-mode-hook #'cider-mode)
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :init (add-hook 'clojure-mode-hook #'aggressive-indent-mode))


;;;; perl
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-mode))


;;;; haskell
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode #'subword-mode))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode #'intero-mode))

(flycheck-add-next-checker 'intero '(warning . haskell-hlint))


;;;; javascript
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))


;; for html templates
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)))


;;;; yml
(use-package yaml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;;;; docs
;; org
(define-key global-map "\C-ca" 'org-agenda)
(setq org-hide-emphasis-markers t)

;; letters as ordered list bullets
;; A. like
;; B. this
(setq org-list-allow-alphabetical t)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; emacs-reveal for presentations
;;(load "~/.emacs.d/emacs-reveal/reveal-config.el")

;; uml
(use-package plantuml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode)))

;;;; fennel
(autoload 'fennel-mode "~/.emacs.d/fennel-mode/fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

;;; godot

(use-package gdscript-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-lua-interpreter (quote lua53))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" default)))
 '(ido-vertical-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files nil)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (gdscript-mode plantuml-mode yaml-mode zygospore org-trello hindent markdown-mode intero all-the-icons doom-themes haskell-mode perlcritic flycheck-haskell company-ghc company-ghci flycheck-joker rainbow-mode volatile-highlights undo-tree ## neotree ensime clj-refactor nov org-bullets flycheck-kotlin flycheck-clojure flycheck-lua flycheck company-lua company lua-mode smex ido-vertical-mode projectile flx-ido aggressive-indent aggressive-indent-mode cider magit paredit clojure-mode monokai-theme rainbow-delimiters which-key ivy avy general use-package)))
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
