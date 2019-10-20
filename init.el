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
             '("melpa" . "https://melpa.org/packages/") t)


;;;; generic

;; starts garbage-collection after 20 megabytes
;; to improve performance
(setq gc-cons-threshold 20000000)

(add-to-list 'load-path "~/.emacs.d/better-defaults")
(require 'better-defaults)

;; disable minimizing frame
(global-unset-key (kbd "C-z"))
;; disable page-up and page-down
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))


;;  version control
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))


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
;;(use-package monokai-theme
;;  :ensure t
;;  :config (setq inhibit-startup-screen t))


(use-package doom-themes
  :ensure t
  :config (setq inhibit-startup-screen t)
  (set-default-font "-ADBO-Hasklig-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

(if (display-graphic-p)
    (load-theme 'doom-solarized-light t)
  (load-theme 'wheatgrass))

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

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode)
  (setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1))

;; swap easily between vertical/horizontal arrangement
(use-package transpose-frame
  :ensure t
  :config (global-set-key (kbd "C-|") 'transpose-frame))

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
  :config (general-define-key "C-'" 'avy-goto-char-timer)
          (general-define-key "C-M-'" 'avy-goto-line))

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :config (projectile-mode))

(use-package helm-projectile
  :ensure t
  :bind
  ("C-c f" . helm-projectile)
  ("C-c s" . helm-projectile-ag)
  ("C-c i" . helm-imenu)
  ("C-c p" . helm-projectile-switch-project))

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)
(setq projectile-globally-ignored-directories '("node_modules"))

;; (general-define-key
;;  :prefix "C-c"
;;  "f"	'projectile-find-file)

;; (general-define-key
;;  :prefix "C-c"
;;  "s"	'projectile-grep)

(general-define-key
 :prefix "C-c"
 "."	'helm-projectile-find-file-dwim)

;; (general-define-key
;;  "M-p" 'switch-to-prev-buffer)

;; (general-define-key
;;  "M-n" 'switch-to-next-buffer)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

;; hit insert to cycle to next buffer
(global-set-key (kbd "<insert>") 'other-window)
;; move around with shift+arrowkeys
(windmove-default-keybindings)


;;;; autocompletion
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("TAB" . company-indent-or-complete-common)
  :config
  (setq company-idle-delay 0.125)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  )

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
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  ("C-c n" . flycheck-next-error)
)

;; clojure flycheck
(use-package flycheck-joker
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
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

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

;;;; php
(use-package php-mode
  :ensure t)


;;;; haskell
(use-package haskell-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook 'subword-mode))

(use-package hasklig-mode
  :ensure t
  :config
  :hook (haskell-mode))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode #'intero-mode)
  (intero-global-mode 1))

(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;;;; java
(use-package lsp-mode :ensure t)
(use-package company-lsp :ensure t)
(use-package yasnippet :ensure t)
(use-package hydra :ensure t)
(use-package lsp-ui :ensure t)
(use-package lsp-ui :ensure t)
(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;;;; javascript
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'flycheck-checkers 'javascript-eslint))

;;(use-package js2-refactor
;;  :ensure t
;;  :config
;;  (add-hook 'js2-mode-hook #'js2-refactor-mode)
;;  (js2r-add-keybindings-with-prefix "C-c C-r"))

;;(use-package xref-js2
;;  :ensure t
;;  :config
;;  (add-hook 'js2-mode-hook #'js2-refactor-mode))
;;
;;
;;(use-package rjsx-mode
;;  :ensure t
;;  :config
;;  (add-to-list 'auto-mode-alist '("*.js" . rjsx-mode)))


;;;; groovy
(use-package groovy-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))

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
;; letters as ordered list bullets
;; A. like
;; B. this
(setq org-list-allow-alphabetical t)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; graphviz
(use-package graphviz-dot-mode
  :ensure t)

;; emacs-reveal for presentations
;;(load "~/.emacs.d/emacs-reveal/reveal-config.el")

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
 '(cperl-indent-level 4)
 '(custom-safe-themes
   (quote
    ("49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" default)))
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(js-indent-level 2)
 '(org-agenda-files (quote ("~/code/FH/webqube/api-mojo/notes.org")))
 '(package-selected-packages
   (quote
    (hasklig-mode groovy-mode helm-projectile gdscript-mode rjsx-mode expand-region smooth-scrolling xbm-life threes xref-js2 js2-refactor web-mode diminish intero zygospore which-key volatile-highlights use-package undo-tree smex rainbow-mode rainbow-delimiters projectile plantuml-mode perlcritic org-ref org-jira org-bullets org nov neotree monokai-theme markdown-mode magit ido-vertical-mode highlight-indentation ghub general flycheck-kotlin flycheck-joker flycheck-haskell flycheck-clojure flx-ido ensime docker-compose-mode docker company-lua company-ghci company-ghc clj-refactor avy aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
