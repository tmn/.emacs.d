;;; tmn.el --- tmn.el configuration -*- lexical-binding: t -*-
;;; Commentary:
;; The tmn.el configuration.

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "site-lisp/")))



;;; Code:

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
(setq vc-follow-symlinks t)


;; -----------------------------------------------------------------------------
;; Configure fonts
;; -----------------------------------------------------------------------------

(cond
 ((find-font (font-spec :family "SF Mono"))
  (set-frame-font "SF Mono:pixelsize=18"))
 ((find-font (font-spec :family "Cascadia Code"))
  (set-frame-font "Cascadia Code:pixelsize=20"))
 ((find-font (font-spec :family "Fira Code"))
  (set-frame-font "Fira Code:pixelsize=18"))
 ((find-font (font-spec :family "Fira Code Retina"))
  (set-frame-font "Fira Code Retina:pixelsize=12"))
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-frame-font "Monaco:pixelsize=12")))


;; -----------------------------------------------------------------------------
;; Keymaps
;; -----------------------------------------------------------------------------


(defvar tmn-keymaps
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") 'end-of-buffer)
    (define-key map (kbd "C-<") 'beginning-of-buffer)
    map)
  "My keymaps.")

(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)

;; (define-key (current-global-map)

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
  :hook (prog-mode . flymake-mode)
)


(use-package flymake-ruff
  :hook (((python-mode python-ts-mode) . flymake-ruff-load))
  :init
  (setq flymake-log-level 'info))


;; -----------------------------------------------------------------------------
;; C / C++
;; -----------------------------------------------------------------------------

;; (use-package c-mode
;;   :ensure nil
;;   ;; :hook ((c-mode c-ts-mode) . eglot-ensure)
;;   :config
;;   (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

;; (use-package c++-mode
;;   :ensure nil
;;   ;; :hook ((c++-mode c++-ts-mode) . eglot-ensure)
;;   :config
;;   (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

(setq c-default-style "linux")
(setq c-basic-offset 4)


;; -----------------------------------------------------------------------------
;; LSP
;; -----------------------------------------------------------------------------


;; Company mode (completion)
(use-package company
    :ensure t
    ;; :config
    ;; (global-company-mode +1)
    )

(use-package spinner
  :vc (:fetcher github :repo "Malabarba/spinner.el"))

;; Used to interface with swift-lsp.
(use-package lsp-mode
  :after spinner
  :commands lsp
  :bind (("M-RET" . lsp-execute-code-action)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-print-io nil)
  (lsp-auto-configure t)
  (lsp-enable-snippet t)
  ; (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.5)
  (lsp-eldoc-render-all t)

  :init
  (setq lsp-keymap-prefix "C-c C-l")

  :hook ((lsp-mode . (lambda () (corfu-mode -1) (company-mode 1)))
         ((swift-mode
           python-mode python-ts-mode
           c-mode
           c++-mode) . lsp))

  :config
  (setq lsp-file-watch-threshold 10000)
  (dolist (directories '("[/\\\\].data\\'"
                         "[/\\\\].github\\'"
                         "[/\\\\].streamlit\\'"
                         "[/\\\\].vscode\\'"
                         "[/\\\\]gradle\\'"
                         "[/\\\\]_testlogs\\'"
                         "[/\\\\]dist\\'"
                         "[/\\\\].gradle\\'"
                         "[/\\\\].storybook\\'"
                         "[/\\\\].log\\'"
                         "[/\\\\]eclipse.jdt.ls\\'"
                         "[/\\\\]build\\'"
                         "[/\\\\]_build\\'"
                         "[/\\\\]coverage\\'"
                         "[/\\\\].DS_Store"))
    (push directories lsp-file-watch-ignored)
    (push directories lsp-file-watch-ignored-directories)))


;; lsp-mode's UI modules
(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-doc-header t
        lsp-ui-imenu-auto-refresh t
        lsp-ui-peek-always-show t)

  (progn
    (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))


;; sourcekit-lsp support
(defun find-sourcekit-lsp ()
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "~/.local/share/swiftly/bin/sourcekit-lsp"))

(use-package lsp-sourcekit
    :ensure t
    :after lsp-mode
    :custom
    (lsp-sourcekit-executable (find-sourcekit-lsp) "Find sourcekit-lsp"))


;; -----------------------------------------------------------------------------
;; Swift
;; -----------------------------------------------------------------------------



(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift")




;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------

(use-package python
  :ensure nil
  :bind (:map python-mode-map
              ("C-c C-o" . python-shell-send-file)
         :map python-ts-mode-map
              ("C-c C-o" . python-shell-send-file))
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  ;; :hook
  ;; ((python-mode python-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(use-package python-pytest)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package ruff-format
  :vc (:fetcher github :repo "tmn/emacs-ruff-format")
  :hook (((python-mode python-ts-mode) . ruff-format-on-save-mode)
         ((python-mode python-ts-mode) . ruff-fix-imports-on-save-mode)))


;; -----------------------------------------------------------------------------
;; Rust
;; -----------------------------------------------------------------------------

(use-package rust-mode
  ;; :hook (rust-mode . eglot-ensure)
  :config
  ;; (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
)



;; -----------------------------------------------------------------------------
;; Dired stuff
;; -----------------------------------------------------------------------------

(use-package dash)

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-hacks-utils
  :after dired)

(use-package dired-subtree
  :after (dired dired-hacks-utils)
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("TAB" . dired-subtree-toggle)))

(use-package dired-git-info
  :after (dired dired-hacks-utils)
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))


;; -----------------------------------------------------------------------------
;; Stuff
;; -----------------------------------------------------------------------------

(use-package corfu
  :init
  (global-corfu-mode)
  :config

  (setq corfu-min-width 40
        corfu-preview-current nil
        corfu-auto t
        corfu-auto-delay 0.2
        corfu-popupinfo-delay '(0.5 . 0.3))
  (corfu-popupinfo-mode 1))

(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind
  (:map completion-preview-active-mode-map
        ("M-i" . completion-preview-insert)
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate))
  :config
  (setq completion-preview-minimum-symbol-length 2))

(use-package orderless
  :custom
  (completion-style '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
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
(use-package cmake-mode
  :mode "CMakeLists.txt")

(use-package ansi-color)
(use-package logview
  :mode "\\.\\(log\\)$")

(use-package restclient)

(use-package eat
  :config
  (setq eat-enable-yank-to-terminal t
        eat-enable-kill-from-terminal t))

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))


;; -----------------------------------------------------------------------------
;; AI Stuff
;; -----------------------------------------------------------------------------
(use-package gptel
  :bind ("C-c o g" . gptel))

;;; tmn.el ends here
