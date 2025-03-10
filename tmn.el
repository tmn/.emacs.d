;;; tmn.el --- tmn.el configuration -*- lexical-binding: t -*-
;;; Commentary:
;; The tmn.el configuration.

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "site-lisp/")))

;;; Code:


(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))



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
(setq inhibit-startup-screen t)

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


;; Set reusable font name variables
(defvar t/fixed-width-font "Fira Code"
  "The font to use for monospaced (fixed width) text.")

(defvar t/variable-width-font "Iosevka Aile"
  "The font to use for variable-pitch (document) text.")

(cond
 ((find-font (font-spec :family "Fira Code"))
  (set-frame-font "Fira Code:pixelsize=13"))
 ((find-font (font-spec :family "SF Mono"))
  (set-frame-font "SF Mono:pixelsize=13"))
 ((find-font (font-spec :family "Fira Code Retina"))
  (set-frame-font "Fira Code Retina:pixelsize=12"))
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-frame-font "Monaco:pixelsize=12")))


;; -----------------------------------------------------------------------------
;; Tree-sitter
;; -----------------------------------------------------------------------------

;; (setq treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (c "https://github.com/tree-sitter/tree-sitter-c")
;;      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
;;      (css "https://github.com/tree-sitter/tree-sitter-css")
;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (html "https://github.com/tree-sitter/tree-sitter-html")
;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (make "https://github.com/alemuller/tree-sitter-make")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


;; -----------------------------------------------------------------------------
;; Basic packages
;; -----------------------------------------------------------------------------

(use-package diminish)

(use-package rg)

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
;; LSP-mode
;; -----------------------------------------------------------------------------

;; (use-package treesit
;;   :ensure nil
;;   ; :hook (prog-mode . (lambda () (when (treesit-available-p) (treesit-mode))))
;;   :preface
;;   (defun t/setup-grammars ()
;;     ""
;;     (interactive)
;;     (dolist (grammar '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;                 (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
;;                 (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;                 (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.20.8"))
;;                 (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.3"))
;;                 (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;;                 (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;;                 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;                 (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))

;;   (dolist (mapping '((python-mode . python-ts-mode)
;;                      (json-mode . json-ts-mode)
;;                      (bash-mode . bash-ts-mode)
;;                      (js2-mode . js2-ts-mode)
;;                      (c++-mode . c++-ts-mode)
;;                      (c-mode . c-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))

;;   :config
;;   (t/setup-grammars))

  ;; :hook (
  ;;        (prog-mode . tree-sitter-mode)
  ;;        (prog-mode . turn-on-tree-sitter-mode)
  ;;        (tree-sitter-after-on . tree-sitter-hl-mode)
  ;;        ))

;; (add-hook 'prog-mode-hook #'tree-sitter-mode)
;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; (use-package js
;;   :ensure nil
;;   :hook ((js-mode . lsp)
;;          (js-ts-mode . lsp)
;;          (javascript-mode . lsp)
;;          (typescript-tsx-mode . lsp))
;;   :config
;;   (setq js-indent-level 2)
;;   :custom
;;   (js-ts-mode-indent-offset 2))



;; -----------------------------------------------------------------------------
;; C / C++
;; -----------------------------------------------------------------------------


(use-package c-mode
  :ensure nil
  :hook ((c-mode c-ts-mode) . eglot-ensure))

;;   :custom
;;   (c-ts-mode-indent-offset 4))

(use-package c++-mode
  :ensure nil
  :hook ((c++-mode c++-ts-mode) . eglot-ensure))

;;   :custom
;;   (c-ts-mode-indent-offset 4))



(setq c-default-style "linux")
(setq c-basic-offset 4)

;; -----------------------------------------------------------------------------
;; JavaScript / TypeScript / Web
;; -----------------------------------------------------------------------------


;; (use-package js-mode
;;   :ensure nil)

;; -----------------------------------------------------------------------------
;; Flymake
;; -----------------------------------------------------------------------------

(use-package flymake-mode
  :ensure nil
  :hook (prog-mode . flymake-mode))

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))


;; -----------------------------------------------------------------------------
;; Eglot
;; -----------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :hook ((ptyhon-mode
          python-ts-mode) . eglot-ensure))


;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------

(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package ruff-format
  :vc (:fetcher github :repo "tmn/emacs-ruff-format")
  :hook (((python-mode python-ts-mode) . ruff-format-on-save-mode)
         ((python-mode python-ts-mode) . ruff-fix-imports-on-save-mode)))


(use-package pyvenv
  :config
  (pyvenv-mode t)

  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env ".venv/bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package transient)


;; -----------------------------------------------------------------------------
;; Stuff
;; -----------------------------------------------------------------------------

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
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
  :bind (:map projectile-command-map)
  ("f" . consult-projectile-find-file))


;; -----------------------------------------------------------------------------
;; ...
;; -----------------------------------------------------------------------------

(use-package yasnippet
  :hook ((python-mode python-ts-mode) . yas-minor-mode)
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
  :commands json-reformat
  :init (setq json-reformat:indent-width 4))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)$")

(use-package mustache-mode
  :mode "\\.mustache$")

(use-package rst
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
(use-package restclient)
(use-package vterm)
(use-package multi-vterm)
(use-package jsonnet-mode)
(use-package ansi-color)
(use-package logview
  :mode "\\.\\(log\\)$")

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))


;;; tmn.el ends here
