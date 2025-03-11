;;; tmn.el --- tmn.el configuration -*- lexical-binding: t -*-
;;; Commentary:
;; The tmn.el configuration.

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "site-lisp/")))

;;; Code:

;; -----------------------------------------------------------------------------
;; Themes
;; -----------------------------------------------------------------------------

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline nil)

  (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))


;; -----------------------------------------------------------------------------
;; Some basic configs
;; -----------------------------------------------------------------------------

(setq-default
 auto-save-default nil
 create-lockfiles nil
 indent-tabs-mode nil
 tab-width 2
 make-backup-files nil
 tab-always-indent 'complete)

(column-number-mode t)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)

(save-place-mode 1)
(setq use-dialog-box nil)

(defun t/my-prog-mode ()
  "Default values for my prog mode."
  (display-line-numbers-mode 0)
  (set-fringe-mode 15))

(add-hook 'prog-mode-hook 't/my-prog-mode)


;; -----------------------------------------------------------------------------
;; Confirm exit emacs
;; -----------------------------------------------------------------------------

(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Dumme ape! Quitting already??? "))
          'append)


;; -----------------------------------------------------------------------------
;; Custom file
;; -----------------------------------------------------------------------------

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;; -----------------------------------------------------------------------------
;; Modify some GUI features
;; -----------------------------------------------------------------------------

;; Headerline stuff
(setq header-line-format nil)

;; Supress some defaults
(setq inhibit-startup-screen nil)

;; Disable scrollbars
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq visible-bell t)

;; (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
;; (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq vc-follow-symlinks t)


;; -----------------------------------------------------------------------------
;; Configure fonts
;; -----------------------------------------------------------------------------

(cond
 ((find-font (font-spec :family "Fira Code"))
  (set-frame-font "Fira Code:pixelsize=16"))
 ((find-font (font-spec :family "SF Mono"))
  (set-frame-font "SF Mono:pixelsize=13"))
 ((find-font (font-spec :family "Fira Code Retina"))
  (set-frame-font "Fira Code Retina:pixelsize=12"))
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-frame-font "Monaco:pixelsize=12")))


;; -----------------------------------------------------------------------------
;; Basic packages
;; -----------------------------------------------------------------------------

(use-package rg)
(use-package s)
(use-package transient)

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode 0))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package direnv
  :config
  (direnv-mode))

;; Trim spaces from end of line on save
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))



;; -----------------------------------------------------------------------------
;; Other packages
;; -----------------------------------------------------------------------------

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-hud nil
        doom-modeline-lsp t
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-env-version t))


(use-package which-key
  :ensure nil
  :config
  (progn
    (setq which-key-add-column-padding 1
          which-key-max-display-columns nil
          which-key-min-display-lines 1
          which-key-separator " "
          which-key-idle-delay 0.5)
    (which-key-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package winum
  :init
  (winum-mode)
  :bind (("C-x w `" . winum-select-window-by-number)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

(use-package emojify)

(use-package magit
  :bind (("C-c o m" . magit-status))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))


;; -----------------------------------------------------------------------------
;; Flymake
;; -----------------------------------------------------------------------------

(use-package flymake-mode
  :ensure nil
  :hook (prog-mode . flymake-mode))

(use-package flymake-ruff
  :after flymake-mode
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

;; -----------------------------------------------------------------------------
;; Eglot
;; -----------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (setq eglot-python-extra-paths '(".venv/lib/python3.12/site-packages")))


;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------

(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  :hook
  ((python-mode python-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(use-package python-pytest)

(use-package ruff-format
  :vc (:fetcher github :repo "tmn/emacs-ruff-format")
  :hook (((python-mode python-ts-mode) . ruff-format-on-save-mode)
         ((python-mode python-ts-mode) . ruff-fix-imports-on-save-mode)))


;; -----------------------------------------------------------------------------
;; C / C++
;; -----------------------------------------------------------------------------

(use-package c-mode
  :ensure nil
  :hook ((c-mode c-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

(use-package c++-mode
  :ensure nil
  :hook ((c++-mode c++-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

(setq c-default-style "linux")
(setq c-basic-offset 4)


;; -----------------------------------------------------------------------------
;; Stuff
;; -----------------------------------------------------------------------------

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)))

(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind
  (:map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate))
  :config
  (setq completion-preview-minimum-symbol-length 2))

(use-package orderless
  :custom
  (completion-category-defaults nil)

  (completion-style '(orderless flex))
  (completion-category-overrides '((file (styles  . (orderless flex)))))

  (orderless-component-separator 'orderless-escapable-split-on-space)

  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp)))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0
        vertico-cycle t)
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<escape>" . minibuffer-keyboard-quit)
              ("?" . minibuffer-completion-help)
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              ("<backspace>" . vertico-directory-delete-char)
              ("C-<backspace>" . vertico-directory-delete-word)
              ("M-G" . vertico-multiform-grid)))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x C-b" . consult-buffer)
         ("C-M-l" . consult-imenu)
         ("C-c s" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         :map minibuffer-local-map
         ("C-h" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (completion-styles '(orderless))
  :config
  (consult-preview-at-point-mode))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'auto))

(use-package consult-projectile
  :after (consult projectile)
  :bind (:map projectile-command-map)
  ("f" . consult-projectile-find-file))


;; -----------------------------------------------------------------------------
;; ...
;; -----------------------------------------------------------------------------

(use-package yasnippet
  :hook ((python-mode
          python-ts-mode) . yas-minor-mode)
  :init
  (yas-global-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))


;; -----------------------------------------------------------------------------
;; ...
;; -----------------------------------------------------------------------------


(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 500))

(use-package savehist
  :init
  (setq savehist-file "~/.emacs.d/savehist")
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        history-length 300
        savehist-autosave-interval 60
        history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history 1)
  (savehist-mode 1))


;; -----------------------------------------------------------------------------
;; Nerd icons
;; -----------------------------------------------------------------------------

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))


;; -----------------------------------------------------------------------------
;; ...
;; -----------------------------------------------------------------------------
(use-package editorconfig
  :init (editorconfig-mode 1))

;;; Other tools

(use-package css-mode
  :ensure nil
  :mode "\\.css$"
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (setq css-indent-offset 2))))

(use-package less-css-mode
  :mode "\\.less$"
  :config
  (add-hook 'less-css-mode-hook (lambda ()
                                  (setq css-indent-offset 2))))

(use-package web-mode
  :commands (web-mode)
  :mode "\\(html\\|j2\\)$"
  :config
  (progn
  (setq web-mode-markup-indent-offset 4
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t)))

(use-package json-mode
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(use-package json-reformat
  :after (json-mode)
  :commands json-reformat
  :init (setq json-reformat:indent-width 4))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)$")

(use-package mustache-mode
  :mode "\\.mustache$")

(use-package rst
  :ensure nil
  :mode (("\\.txt$" . rst-mode)
         ("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)))

(use-package qml-mode
  :mode "\\.qml$"
  :config
  (setq tab-width 4
        qml-indent-width 4))

(use-package yaml-mode)
(use-package cmake-mode)

(use-package ansi-color)
(use-package logview
  :mode "\\.\\(log\\)$")

(use-package restclient)

(use-package vterm)
(use-package multi-vterm)

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))


;;; tmn.el ends here
