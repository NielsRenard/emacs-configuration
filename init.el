;;; package --- Summary:
;;; Commentary:
(package-initialize)

;; Melpa
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;;; Code:
;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;; generic

;; to improve performance
;; starts garbage-collection after 20 megabytes
(setq gc-cons-threshold 20000000)
(add-to-list 'load-path "~/.emacs.d/so-long")
(require 'so-long)
(global-so-long-mode 1)

;;"A defined abbrev is a word which expands
(setq-default abbrev-mode t)

;; can just type y instead of yes
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(tool-bar-mode -1)
;;(scroll-bar-mode -1)
;; set linenumbers by default
;;(global-linum-mode)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(show-paren-mode 1)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))


;; disable minimizing frame
(global-unset-key (kbd "C-z"))
;; disable page-up and page-down
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))


;;  version control
(use-package magit
  :bind ("C-c g" . magit-status))


;; https://askubuntu.com/questions/30224/how-to-disable-the-alt-hotkey-behavior-on-gnome-terminal
;; hit this to fix whitespace, nice to use use together with M-^
(define-key global-map "\M-space" 'fixup-whitespace)

(use-package undo-tree
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package zone
  :config (zone-when-idle 1028))

;;;; looks

(use-package doom-themes
  :ensure t
  :config (setq inhibit-startup-screen t)
  ;;(set-default-font "-ADBO-Hasklig-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (set-default-font "-PfEd-Fantasque Sans Mono-normal-normal-normal-*-35-*-*-*-m-0-iso10646-1"))

;; changes themes based on time of day
(use-package theme-changer
  :config
  (setq calendar-latitude 33)
  (setq calendar-longitude 130)
  (change-theme 'doom-solarized-light 'doom-laserwave))

(setq custom-safe-themes t)

;; always use dark theme in terminal mode
(if (not(display-graphic-p))
    (load-theme 'doom-laserwave))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; icons for neo-tree
(use-package all-the-icons
  ;; https://github.com/domtronn/all-the-icons.el#installing-fonts
  ;; In order for the icons to work it is very important that you install the Resource Fonts included
  ;; M-x all-the-icons-install-fonts
  ;; Bear in mind, this will also run fc-cache -f -v on MacOS and Linux which can take some time to complete.
  )

;; mouse-wheel scrolling zoom
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; less clutter in mode-line
(use-package diminish)

;;;; editing
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))


;;;; navigation

;; modeline shows name of the function you are in
(which-function-mode t)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode)
  (setq redisplay-dont-pause t
	scroll-margin 1
	scroll-step 1
	scroll-conservatively 10000
	scroll-preserve-screen-position 1))

;; swap easily between vertical/horizontal arrangement
(use-package transpose-frame
  :config (global-set-key (kbd "C-|") 'transpose-frame))

(use-package zygospore
  :config (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

(use-package neotree
  :config (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode))

(use-package helm-projectile
  :bind
  ("C-c f" . helm-projectile)
  ("C-c s" . helm-rg)
  ("C-c i" . helm-imenu)
  ("C-c p" . helm-projectile-switch-project))

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(setq projectile-globally-ignored-directories '(".git"))
(setq projectile-globally-ignored-directories '("node_modules"))
(setq projectile-globally-ignored-directories '(".stack-work"))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flx-ido
  :requires ido
  :config (flx-ido-mode))

(use-package ido-vertical-mode
  :config (ido-vertical-mode 1))

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(use-package avy
  :ensure t
  :commands (avy-goto-char-timer)
  :bind (("<C-return>" . avy-goto-char-timer)
	 ("<C-M-return>" . avy-goto-line)))

;; better running commands by name
(use-package smex
  :bind ("M-x" . smex))

(use-package ivy)

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode)
  (setq which-key-idle-delay 0.05))

;; move around with shift+arrowkeys
(windmove-default-keybindings)


;;;; autocompletion
(use-package company
  :diminish company-mode
  :bind ("TAB" . company-indent-or-complete-common)
  :config
  (setq company-idle-delay 0.125)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations nil)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  )

(use-package company-ghci
  :hook (haskell-mode . company-mode)
  :config (setq company-idle-delay 0.05
		company-minimum-prefix-length 2))

(use-package company-ghc
  :hook (haskell-mode . company-mode)
  :config (setq company-idle-delay 0.05
		company-minimum-prefix-length 2))


;;;; syntax checking
(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind
  (("C-c C-n" . flycheck-next-error)
   ("C-c n" . flycheck-next-error)
   ("C-c C-p" . flycheck-previous-error))
  )

;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is. (make sure it exists)
(setq temporary-file-directory "~/.emacs.d/tmp")

(add-to-list 'load-path "~/.emacs.d/flycheck-inline")
(require 'flycheck-inline)

;;;; clojure
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(defun my-clojure-mode-hook ()
  "Initialize clojure refactoring and code snippets."
  (yas-minor-mode 1) ;;for adding require/use/import statements
  )

(global-set-key (kbd "C-c t") #'transpose-sexps)

(use-package cider
  :init (add-hook 'clojure-mode-hook #'cider-mode)
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :init (add-hook 'clojure-mode-hook #'aggressive-indent-mode))


;;;; perl
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-mode))

;;;; rust
(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp-deferred)
  :config (add-hook 'rust-mode-hook 'subword-mode))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;; haskell
(use-package haskell-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook 'subword-mode))

(use-package hasklig-mode
  :config
  :hook (haskell-mode))

;;;; elm

(use-package elm-mode)

;;;; lsp
(use-package lsp-mode
  :commands lsp
  :config  (setq lsp-prefer-flymake nil)
  :commands (lsp-deferred))

(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package yasnippet)
(use-package lsp-ui
  :config (lsp-ui-flycheck-enable t)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  ;; to put a different side-actions colour:
  ;;(custom-set-faces '(lsp-ui-sideline-code-action ((t (:foreground "#268bd2")))))
					;   (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
	(progn
	  (lsp-ui-doc-mode -1)
	  (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
	("C-c C-t" . lsp-describe-thing-at-point)
	("C-c C-r" . lsp-ui-peek-find-references)
	("C-c C-j" . lsp-ui-peek-find-definitions)
	("C-c C-m" . lsp-ui-imenu)
	("C-c C-s" . lsp-ui-sideline-mode)
	("M-RET"   . lsp-ui-sideline-apply-code-actions)
	("C-c C-d" . ladicle/toggle-lsp-ui-doc)))

;; java lsp
(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (add-hook 'java-mode-hook 'subword-mode)
  (require 'dap-java))

;; fix dap-java-run-test ansi color escape codes
;;(require 'ansi-color)
;;(defun colorize-compilation-buffer ()
;;  (toggle-read-only)
;;  (ansi-color-apply-on-region compilation-filter-start (point))
;;  (toggle-read-only))
;;(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package dap-mode
  :after lsp-mode
  :requires 'dap-java
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package lsp-haskell :after lsp)

(add-hook 'haskell-mode-hook 'lsp-deferred)

;;;; javascript
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;;;; purescript
(use-package purescript-mode
  :commands purescript-mode
  :mode (("\\.purs$" . purescript-mode))
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package psc-ide)

(add-hook 'purescript-mode-hook
	  (lambda ()
	    (psc-ide-mode)
	    (company-mode)
	    (turn-on-purescript-indentation)
	    (customize-set-variable 'psc-ide-add-import-on-completion t)))

;;;; groovy
(use-package groovy-mode
  :config (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))

;; for html templates
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)))


;;;; yml
(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;;;; docs
;; letters as ordered list bullets
;; A. like
;; B. this
(setq org-list-allow-alphabetical t)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; easy templates for named org src blocks
(add-to-list 'org-structure-template-alist '("n" "#+NAME: ?"))
(add-to-list 'org-structure-template-alist
	     '("s" "#+NAME: ?\n#+BEGIN_SRC \n\n#+END_SRC"))

;; graphviz
(use-package graphviz-dot-mode)

;; tex
(use-package tex
  ;; apt install texlive-latex-extra
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; emacs-reveal for presentations
;;(load "~/.emacs.d/emacs-reveal/reveal-config.el")

;;; godot

(use-package gdscript-mode)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(cperl-indent-level 4)
 '(custom-safe-themes
   (quote
    ("0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "3952ef318c8cbccf09954ecf43250ac0cbd1f4ae66b4abe569491b260f6e054b" "1ca1f43ca32d30b05980e01fa60c107b02240226ac486f41f9b790899f6f6e67" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" default)))
 '(fci-rule-color "#D6D6D6")
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(js-indent-level 2)
 '(lsp-java-import-gradle-enabled t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position (quote top))
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-fontify (quote on-demand))
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-code-actions-prefix "✡" t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol t)
 '(objed-cursor-color "#D70000")
 '(org-agenda-files (quote ("~/code/FH/webqube/api-mojo/notes.org")))
 '(package-selected-packages
   (quote
    (elm-mode lsp-ui lsp-mode theme-changer lsp-ui-flycheck zygospore yasnippet yaml-mode which-key web-mode volatile-highlights use-package undo-tree transpose-frame smooth-scrolling smex rainbow-mode rainbow-delimiters purescript-mode psc-ide php-mode paredit org-bullets neotree multiple-cursors magit lsp-java lsp-haskell js2-mode ivy ido-vertical-mode helm-rg helm-projectile helm-lsp helm-ag hasklig-mode groovy-mode graphviz-dot-mode general gdscript-mode flycheck-joker flycheck-haskell flx-ido expand-region doom-themes diminish dap-mode company-lua company-lsp company-ghci company-ghc cider auctex all-the-icons aggressive-indent)))
 '(vc-annotate-background "#FDF6E3")
 '(vc-annotate-color-map
   (list
    (cons 20 "#859900")
    (cons 40 "#959300")
    (cons 60 "#a58e00")
    (cons 80 "#b58900")
    (cons 100 "#bc7407")
    (cons 120 "#c35f0e")
    (cons 140 "#cb4b16")
    (cons 160 "#cd4439")
    (cons 180 "#d03d5d")
    (cons 200 "#d33682")
    (cons 220 "#d63466")
    (cons 240 "#d9334a")
    (cons 260 "#dc322f")
    (cons 280 "#dd5c56")
    (cons 300 "#de867e")
    (cons 320 "#dfb0a5")
    (cons 340 "#D6D6D6")
    (cons 360 "#D6D6D6")))
 '(vc-annotate-very-old-color nil)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-code-action ((t (:foreground "#268bd2")))))
