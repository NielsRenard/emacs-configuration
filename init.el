;;; package --- Summary:
;;; Commentary:
;; (package-initialize)

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

;;;; performance
;; starts garbage-collection after 20 megabytes
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ; 1mb

(add-to-list 'load-path "~/.emacs.d/so-long")
(require 'so-long)
(global-so-long-mode 1)

;; Faster than the default scp
(defvar tramp-default-method "ssh" )
(setq tramp-default-method "ssh")

;; https://github.com/dholm/benchmark-init-el/issues/15
(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;"A defined abbrev is a word which expands"
(setq-default abbrev-mode t)

;; can just type y instead of yes
(fset 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(set-default 'truncate-lines t)

(global-set-key "\C-c$" 'toggle-truncate-lines)
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

;; pick up environment variables from .envrc files
(use-package direnv
 :config
 (direnv-mode))

;; perform http calls
(use-package restclient
  :defer t
  )

(use-package company-restclient
  :defer 2
  :config (add-to-list 'company-backends 'company-restclient)
  )

(use-package jq-mode
  :defer 2)

;;  version control
(use-package magit
  :defer 2
  :bind ("C-c g" . magit-status))

(use-package git-timemachine
  :defer t
  :bind ("C-c C-h" . git-timemachine-toggle))

(use-package git-gutter
  :diminish git-gutter-mode
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:foreground "#000000" :background "#87cefa"))))
  (git-gutter:added    ((t (:foreground "#000000" :background "#50fa7b"))))
  (git-gutter:deleted  ((t (:foreground "#000000" :background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1)
  )


;; https://askubuntu.com/questions/30224/how-to-disable-the-alt-hotkey-behavior-on-gnome-terminal
;; hit this to fix whitespace, nice to use use together with M-^
(define-key global-map "\M-space" 'fixup-whitespace)

(use-package undo-tree
  :defer t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

;; (use-package zone
;;   :config (zone-when-idle 1028))

;;;; visual / looks

;; make window a little bit transparent
;;(set-frame-parameter (selected-frame) 'alpha '95)
;; (add-to-list 'default-frame-alist '(alpha '95))

(use-package doom-themes
  :ensure t
  :config (setq inhibit-startup-screen t)
  (setq custom-safe-themes t)
  (load-theme 'doom-solarized-light)
  ;;(set-default-font "Hasklig")
  ;;https://fontlibrary.org/en/font/fantasque-sans-mono
  ;; (set-frame-font "Fantasque Sans Mono")
  ;;(set-frame-font "Purisa Mono")
  )


;; always use dark theme in terminal mode
(if (not(display-graphic-p))
    (load-theme 'doom-laserwave))

;;(use-package indent-guide
;;  :config (indent-guide-global-mode))

(use-package beacon
  :defer 2
  :custom
  (beacon-color "#f1fa8c")
  :hook (after-init . beacon-mode)
  :bind  ("C-<tab>" . 'beacon-blink))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1))

(use-package rainbow-mode
  :defer 1
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; icons for neo-tree
(use-package all-the-icons
  :defer t
  ;; https://github.com/domtronn/all-the-icons.el#installing-fonts
  ;; In order for the icons to work it is very important that you install the Resource Fonts included
  ;; M-x all-the-icons-install-fonts
  ;; Bear in mind, this will also run fc-cache -f -v on MacOS and Linux which can take some time to complete.
  )

;; mouse-wheel Ctrl-scroll zoom
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
;; make scrolling usable
(setq mouse-wheel-scroll-amount '(0.02))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq ring-bell-function 'ignore)

;; less clutter in mode-line
(use-package diminish
    :defer t
  )

;;;; editing
(use-package multiple-cursors
  :defer 2
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package evil-numbers
  :defer t
  :bind
  ("C-c +" . 'evil-numbers/inc-at-pt)
  ("C-c -" . 'evil-numbers/dec-at-pt))

;; no tabs (use C-q when must use tabs)
(setq-default indent-tabs-mode nil)

(setq show-trailing-whitespace nil)

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(global-set-key (kbd "C-c <deletechar>") 'delete-trailing-whitespace)

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

;;(setq scroll-lock-mode t)
(global-hl-line-mode)
(setq blink-cursor-mode nil)
;; swap easily between vertical/horizontal arrangement
(use-package transpose-frame
  :defer 2
  :config (global-set-key (kbd "C-|") 'transpose-frame))

(use-package zygospore
  :defer 2
  :config (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

(use-package neotree
  :defer t
  :config  ;; (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode +1)
  )

(global-set-key (kbd "<f5>")
                (lambda () (interactive)
                  (find-file "~/code/notes")
                  (projectile-find-file)))

(use-package rg)
(use-package helm-rg)

(use-package helm-projectile
  :defer 1
  :bind
  ("C-x x" . helm-buffers-list)
  ("C-c f" . helm-projectile)
  ("C-c s" . helm-projectile-rg)
  ("C-c i" . helm-imenu)
  ("C-c p" . helm-projectile-switch-project))

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(setq projectile-globally-ignored-directories '(".git"))
(setq projectile-globally-ignored-directories '("node_modules"))
(setq projectile-globally-ignored-directories '(".stack-work"))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package wrap-region
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" nil org-mode)
     ("+" "+" nil org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode))))
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (add-hook 'latex-mode-hook 'wrap-region-mode))

(use-package flx-ido
  :ensure t
  :requires ido
  :config (flx-ido-mode))

(use-package avy
  :ensure t
  :commands (avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.25)
  :bind (("<C-return>" . avy-goto-char-timer)
         ("C-M-'" . avy-goto-char-timer)
	 ("<C-M-return>" . avy-goto-line)))


;; better running commands by name (needs ido-completing-read+ for fuzzy matching)
(use-package amx
  :ensure t
  :bind ("M-x" . amx))

;; accompanies amx
(use-package ido-completing-read+
  :config
  (setq ido-decorations
        (quote ("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  ;; C-n/p is more intuitive in vertical layout
  (defun ido-define-keys ()
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)
  )

(use-package ivy
  :defer t
  )

(use-package which-key
  :defer t
  :diminish which-key-mode
  :config (which-key-mode)
  (setq which-key-idle-delay 0.1))

;; move around with shift+arrowkeys
;;(windmove-default-keybindings)


;;;; autocompletion
(use-package company
  :defer 1
  :diminish company-mode
  :bind ("TAB" . company-indent-or-complete-common)
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-common-or-cycle)
        :map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  :config
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  )

(use-package company-posframe
  :defer t
  :config
  (company-posframe-mode 1)
  )

(use-package company-ghci
  :defer t
  :hook (haskell-mode . company-mode)
  :config (setq company-idle-delay 0.05
		company-minimum-prefix-length 2))

;; (use-package company-ghc
;;   :defer t
;;   :hook (haskell-mode . company-mode)
;;   :config (setq company-idle-delay 0.05
;; 		company-minimum-prefix-length 2))


;;;; syntax checking
(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (set-face-underline 'flycheck-warning nil)
  :custom
  (flycheck-display-errors-delay 0)
  :bind
  (("C-c C-e" . list-flycheck-errors)
   ("C-c C-n" . flycheck-next-error)
   ("C-c n" . flycheck-next-error)
   ("C-c C-p" . flycheck-previous-error))
  )

;; (use-package flycheck-posframe
;;   :hook (flycheck-mode . flycheck-posframe-mode)
;;   :config
;;   (flycheck-posframe-configure-pretty-defaults)
;;   (setq flycheck-posframe-border-width 30))

;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; Fix for hot reloaders freaking out over .#Files "[Error: ENOENT: no such file or directory"
(setq create-lockfiles nil)
;; This lets me say where my temp dir is. (make sure it exists)
(setq temporary-file-directory "~/.emacs.d/tmp")

;; (add-to-list 'load-path "~/.emacs.d/flycheck-inline")
;; (require 'flycheck-inline)

;;;; clojure
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :defer 4
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
  :defer t
  :init (add-hook 'clojure-mode-hook #'cider-mode)
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :defer t
  :init (add-hook 'clojure-mode-hook #'aggressive-indent-mode))


;;;; rust
(use-package toml-mode)

(use-package rustic
  :ensure t
  :custom
  ;; (lsp-rust-analyzer-server-display-inlay-hints t) ;; types everywhere
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (lsp-toggle-signature-auto-activate)
  (require 'dap-cpptools)
  :hook (
         (rustic-mode . subword-mode)
         (rustic-mode . yas-minor-mode))
  )

(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :defer t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;; haskell
(use-package haskell-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode))

(use-package flycheck-haskell
  :config (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

;; nixos

(use-package nix-mode
  :defer 2
)

;;;; elm

(use-package elm-mode
  :hook ((elm-mode . lsp-deferred)
         (elm-mode . subword-mode)))

;;;; lsp
(use-package lsp-mode
  :defer 2
  :ensure t
  :commands lsp
  :config
  (setq lsp-prefer-capf t)
  (setq lsp-prefer-flymake nil)
  '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:underline "DarkOrange1"))) t)
  (setq lsp-idle-delay 0.500)

  :commands (lsp-deferred))


(use-package lsp-haskell
  :hook (haskell-mode . lsp-deferred)
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  )

;(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package yasnippet)

(global-set-key [f8] 'treemacs)
(use-package treemacs
  :defer t
  )
(use-package treemacs-projectile)
(use-package lsp-treemacs
  :defer t
  ;; :bind
  ;; ("C-c C-e" . lsp-treemacs-errors-list)
  )

(use-package lsp-ui
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'always)
  :bind
  (:map lsp-mode-map
	("C-c C-t" . lsp-describe-thing-at-point)
	("C-c C-r" . lsp-ui-peek-find-references)
	("C-c C-j" . lsp-ui-peek-find-definitions)
	("C-c C-i" . lsp-goto-implementation)
        ("C-c C-l C-r" . lsp-rename)
        ("C-c C-l C-f" . rustic-format-buffer)
	("C-c C-m" . lsp-ui-imenu)
	("C-c C-s" . lsp-ui-sideline-mode)
	("M-RET"   . helm-lsp-code-actions)
	("C-c C-d" . lsp-ui-doc-mode)))

;;;; java
(use-package lsp-java
                                        ;  :requires dap-mode
  :ensure t
  :config
  (require 'lsp-java-boot)
;;  (require 'dap-java)
  (add-hook 'java-mode-hook 'subword-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (add-hook 'java-mode-hook #'yas-minor-mode)
  ;; http://www.skybert.net/emacs/enterprise-java-development-in-emacs/
  ;; make sure to put lombok.jar where it is expected
  (setq lsp-java-vmargs
       (list
          "-noverify"
          "-Xmx1G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          "-javaagent:/usr/sbin/lombok.jar"))
  (add-hook 'java-mode-hook #'lsp)
  )

;; fix dap-java-run-test ansi color escape codes
(require 'ansi-color
         :defer t
         )
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package dap-mode
  :after lsp-mode
;  :requires 'dap-java
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; mode for gherkin / cucumber tests
(use-package feature-mode
  :defer t)

;; use C-c C-o to set offset
;; use M-x c-show-syntactic-information (to show the variable that needs to be set)

(use-package google-c-style
  :defer 1
  :config
  (add-hook 'java-mode-hook
	    (lambda ()
            (subword-mode)
            (google-set-c-style)
            (google-make-newline-indent)
            (setq c-basic-offset 4)
	    )))

(use-package dtrt-indent
  :defer t
  )

;;;; javascript

(use-package json-mode
  :defer t
  )

;;;; groovy
(use-package groovy-mode
  :defer t
  :config (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))

;;;; yml
(use-package yaml-mode
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;;;; docs / org-mode / Org-mode

;; letters as ordered list bullets
;; A. like
;; B. this
(setq org-list-allow-alphabetical t)

(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(setq org-hide-emphasis-markers t)

(setq org-startup-with-inline-images t)

(setq org-image-actual-width nil)

(setq org-tags-column 75)

(setq org-startup-indented t)

;; (setq org-modules (quote (org-habit)))
;; (setq org-log-done t) ;; Necessary to timestamp habits
;; (setq org-habit-preceding-days 30
;;       org-habit-graph-column 40
;;       org-habit-following-days 7
;;       org-habit-graph-column 80
;;       org-habit-show-habits-only-for-today t
;;       org-habit-show-all-today t)


;; org-agenda

(setq org-agenda-files (list "~/code/notes/agenda"))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
;; (setq calendar-mark-diary-entries-flag t)
;; Highlights days with tasks on them.

(use-package htmlize
  :config
  ;;borders around exported org tables
  ;; (setq org-html-table-default-attributes
  ;;       (plist-put org-html-table-default-attributes :rules "all"))
  ;; (setq org-html-table-default-attributes
  ;;       (plist-put org-html-table-default-attributes :frame "border"))
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-htmlize-font-prefix "org-")
  (setq org-html-checkbox-type 'html))


;; attach (drag) images from web/filesystem directly to org files
(use-package org-download)

(use-package org-cliplink
  :defer t
  :bind ("C-x p l" . 'org-cliplink)
  )

(use-package visual-fill-column
  :defer t
  )

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(use-package nov
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 120)
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  ;; (setq line-spacing 0.0)
  )

;;;; org-babel

(use-package ob-rust :defer 2)
(use-package ob-restclient :defer 2)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (C . t)
   (shell . t)
   (haskell . t)
   (rust . t)
   (restclient . t)
   (dot . t)
   (java . t))
 )

;; plotting formulas
(use-package gnuplot :defer t)
(use-package gnuplot-mode :defer t)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; recipes
(use-package org-chef)

;; graphviz
(use-package graphviz-dot-mode)

;; tex
(use-package tex
  ;; apt install texlive-latex-extra
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.5))
  )

;; emacs-reveal for presentations

;;(use-package oer-reveal)
;;(use-package org-re-reveal-ref)
;;(use-package org-re-reveal)
;; (add-to-list 'load-path "~/.emacs.d/emacs-reveal")
;; (require 'emacs-reveal)
;;(load "~/.emacs.d/emacs-reveal/reveal-config.el")

;; Japanese language

(use-package google-translate
  :ensure t
  :custom
  (google-translate-backend-method 'curl)
  :config
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja")))
  (setq google-translate-show-phonetic t)
  (progn
    ;; To fix error: google-translate--search-tkk: Search failed: ",tkk:'"
    ;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-727920888
    (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
    (defalias 'gt 'google-translate-smooth-translate)
    ))



;; custom functions fns

(defun jisho-org-link (word tooltip)
  "Generate org-mode jisho link from word and insert macro for tooltip in html export."
  (interactive "MWord:\nMTooltip:")
  (let ((jisho-url "https://jisho.org/search/"))
    (progn
      (insert (concat "{{{kanji(" tooltip ")}}}"))
      (newline-and-indent)
      (org-insert-link t (concat jisho-url word) word)
      )))
(global-set-key (kbd "C-x C-j") 'jisho-org-link)


(defun jisho-org-link-region (start end)
  "Generate org-mode jisho search link from region."
  (interactive "r")
  (let ((query (buffer-substring start end))
        (jisho-url "https://jisho.org/search/"))
    (progn
      (kill-region start end)
      (org-insert-link nil (concat jisho-url query) query)
      )))

(defun er-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-backend 'ido)
 '(amx-mode t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#f7f3ee" "#955f5f" "#81895d" "#957f5f" "#7382a0" "#9c739c" "#5f8c7d" "#605a52"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(beacon-mode t)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-firefox)
 '(cperl-indent-level 4)
 '(css-indent-offset 2)
 '(cua-mode nil nil (cua-base))
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "b7e460a67bcb6cac0a6aadfdc99bdf8bbfca1393da535d4e8945df0648fa95fb" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "22a514f7051c7eac7f07112a217772f704531b136f00e2ccfaa2e2a456558d39" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "37a4701758378c93159ad6c7aceb19fd6fb523e044efe47f2116bc7398ce20c9" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "9f15d03580b08dae41a1e5c1f00d1f1aa99fea121ca32c28e2abec9563c6e32c" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "361f5a2bc2a7d7387b442b2570b0ef35198442b38c2812bf3c70e1e091771d1a" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "76f66cbdf9ada1f86f9225c0f33ae60b40d04073146c4f5c49d8189d1157728b" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "b462d00de785490a0b6861807a360f5c1e05b48a159a99786145de7e3cce3afe" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "3952ef318c8cbccf09954ecf43250ac0cbd1f4ae66b4abe569491b260f6e054b" "1ca1f43ca32d30b05980e01fa60c107b02240226ac486f41f9b790899f6f6e67" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" default))
 '(default-input-method "japanese")
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#605a52")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(git-gutter:added-sign "+" nil nil "Customized with use-package git-gutter")
 '(git-gutter:deleted-sign "-" nil nil "Customized with use-package git-gutter")
 '(git-gutter:modified-sign "~" nil nil "Customized with use-package git-gutter")
 '(graphviz-dot-view-command "dotty %s")
 '(helm-mode-fuzzy-match nil)
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#70c900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(ido-enable-flex-matching t)
 '(ido-separator nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#f1ece4" "#7382a0"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f1ece4" "#81895d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f1ece4" "#b9a992"))
 '(js-indent-level 2)
 '(linum-format 'dynamic)
 '(lsp-haskell-process-path-hie "haskell-language-server-wrapper" t)
 '(lsp-java-boot-enabled nil)
 '(lsp-java-import-gradle-enabled t)
 '(lsp-java-inhibit-message nil)
 '(lsp-java-signature-help-enabled nil)
 '(lsp-lens-enable nil)
 '(lsp-rust-analyzer-call-info-full t)
 '(lsp-rust-analyzer-display-parameter-hints t)
 '(lsp-semantic-highlighting :immediate)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 120)
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position 'top)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-fontify 'on-demand)
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-peek-show-directory t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol nil)
 '(objed-cursor-color "#955f5f")
 '(org-agenda-files
   '("~/code/japanese/nihongo/words.org" "~/code/notes/japanese/taekimgrammar.org" "~/code/notes/programming/AdventOfCodeJava.org" "/home/coob/code/notes/agenda/habits.org" "/home/coob/code/notes/agenda/todo.org"))
 '(org-modules '(org-habit))
 '(org-src-block-faces 'nil)
 '(org-src-tab-acts-natively t)
 '(package-selected-packages
   '(org windresize highlight-indent-guides visual-fill-column org-ref org-re-reveal-ref oer-reveal google-translate-default-ui define-it ob-java ob-restclient direnv org-chef jq-mode company-restclient htmlize rustic gnuplot gnuplot-mode ivy-completing-read ivy-completing-read+ nix-mode nixos-mode ido-completing-read+ amx nov nov-mode ron-mode org-brain org-cliplink benchmark-init highlight-indentation zone-nyan zone-select nyan-mode dtrt-indent command-log-mode git-timemachine git-gutter beacon company-posframe company-box json-mode restclient org-download feature-mode rjsx-mode treemacs-projectile indent-guide highlight-indent-guides-method wrap-region evil-numbers elm-mode lsp-ui lsp-mode theme-changer lsp-ui-flycheck zygospore yasnippet yaml-mode which-key web-mode volatile-highlights use-package undo-tree transpose-frame smooth-scrolling smex rainbow-mode rainbow-delimiters purescript-mode psc-ide php-mode paredit org-bullets neotree multiple-cursors magit lsp-java lsp-haskell ivy helm-rg helm-projectile helm-lsp helm-ag hasklig-mode groovy-mode graphviz-dot-mode general gdscript-mode flycheck-joker flycheck-haskell expand-region doom-themes diminish dap-mode company-lua company-lsp company-ghci company-ghc cider auctex all-the-icons aggressive-indent))
 '(pdf-view-midnight-colors (cons "#605a52" "#f7f3ee"))
 '(projectile-enable-caching t)
 '(rustic-ansi-faces
   ["#f7f3ee" "#955f5f" "#81895d" "#957f5f" "#7382a0" "#9c739c" "#5f8c7d" "#605a52"])
 '(safe-local-variable-values
   '((org-image-actual-width)
     (org-todo-keyword-faces
      ("Habit" . "red")
      ("Done" . "green"))
     (org-todo-keyword-faces
      ("HABIT" . "red")
      ("DONE" . "green"))
     (org-todo-keyword-faces
      ("HABIT" . "blue")
      ("DONE" . "green"))
     (org-todo-keyword-faces
      ("NEXT" . "blue")
      ("DONE" . "green"))))
 '(show-trailing-whitespace nil)
 '(sql-sqlite-program "sqlite3")
 '(treemacs-follow-mode t)
 '(vc-annotate-background "#f7f3ee")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#81895d")
    (cons 40 "#87855d")
    (cons 60 "#8e825e")
    (cons 80 "#957f5f")
    (cons 100 "#957f5f")
    (cons 120 "#957f5f")
    (cons 140 "#957f5f")
    (cons 160 "#977b73")
    (cons 180 "#997787")
    (cons 200 "#9c739c")
    (cons 220 "#996c87")
    (cons 240 "#976573")
    (cons 260 "#955f5f")
    (cons 280 "#9e716b")
    (cons 300 "#a78378")
    (cons 320 "#b09685")
    (cons 340 "#605a52")
    (cons 360 "#605a52")))
 '(vc-annotate-very-old-color nil)
 '(which-function-mode t)
 '(which-key-mode t)
 '(window-divider-mode t)
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#f4923b" "#70c900" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#000000" :background "#50fa7b"))))
 '(git-gutter:deleted ((t (:foreground "#000000" :background "#ff79c6"))))
 '(git-gutter:modified ((t (:foreground "#000000" :background "#87cefa"))))
 '(variable-pitch ((t (:family "Sans Serif")))))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
