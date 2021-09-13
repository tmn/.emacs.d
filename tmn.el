;;; tmn.el --- tmn.el configuration -*- lexical-binding: t -*-

;;; Commentary:
;; The tmn.el configuration.

;;; Code:

(defconst tmn/is-mac (eq system-type 'darwin))
(defconst tmn/is-linux (eq system-type 'gnu/linux))
(defconst tmn/is-windows (eq system-type 'windows-nt))

(when tmn/is-mac
  (setq
   ns-use-mwheel-momentum t
   ns-use-mwheel-acceleration t
   dired-use-ls-dired nil

   ns-function-modifier 'hyper
   mac-command-modifier 'meta
   mac-option-modifier 'super

   trash-directory "~/.Trash/emacs")

  ;; Map æøå keys to macOS using non-Norwegian keymap
  (define-key key-translation-map (kbd "s-a") "å")
  (define-key key-translation-map (kbd "s-'") "æ")
  (define-key key-translation-map (kbd "s-o") "ø")

  (define-key (current-global-map) (kbd "s-p") nil)
  (define-key (current-global-map) (kbd "s-o") nil)
  (define-key (current-global-map) (kbd "s-t") nil)

  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)))

(when tmn/is-windows
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(when tmn/is-linux
  (menu-bar-mode -1)
  (tool-bar-mode -1))


;; -----------------------------------------------------------------------------
;; Some basic configs
;; -----------------------------------------------------------------------------

(setq-default
 auto-save-default nil
 create-lockfiles nil
 indent-tabs-mode nil
 line-spacing 6
 make-backup-files nil
 tab-always-indent 'complete)

(column-number-mode t)
(global-hl-line-mode 1)


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

;; Fix macos titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;; -----------------------------------------------------------------------------
;; Configure fonts
;; -----------------------------------------------------------------------------
(cond
 ((find-font (font-spec :family "SF Mono"))
  (set-frame-font "SF Mono:pixelsize=12"))
 ((find-font (font-spec :family "Fira Code Retina"))
  (set-frame-font "Fira Code Retina:pixelsize=12"))
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-frame-font "Monaco:pixelsize=12")))


;; -----------------------------------------------------------------------------
;; Some custom stuffs
;; -----------------------------------------------------------------------------

(defun tmn/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun tmn/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-S-<up>") 'tmn/move-line-up)
(global-set-key (kbd "M-S-<down>") 'tmn/move-line-down)

;; Confirm exit emacs
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Dumme ape! Quitting already??? "))
          'append)


;; -----------------------------------------------------------------------------
;; Bootstrap configs
;; -----------------------------------------------------------------------------

;; String helpers
(use-package s)

(use-package rg
  :commands rg)

;; Trim spaces from end of line on save
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package doom-themes
  :init
  (load-theme 'doom-vibrant t)
  :config
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (doom-themes-visual-bell-config)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30
        doom-modeline-lsp t
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info nil
        doom-modeline-env-version t))

(use-package multiple-cursors
  :bind (("C-c C-. ."   . mc/mark-all-dwim)
         ("C-c C-. C-." . mc/mark-all-like-this-dwim)
         ("C-c C-. a"   . mc/mark-all-like-this)
         ("C-c C-. n"   . mc/mark-next-symbol-like-this)
         ("C-c C-. P"   . mc/mark-previous-symbol-like-this)
         ("C-c C-. A"   . mc/mark-all-symbols-like-this)
         ("C-c C-. f"   . mc/mark-all-like-this-in-defun)
         ("C-c C-. l"   . mc/edit-lines)
         ("C-c C-. e"   . mc/edit-ends-of-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)))

(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :hook (prog-mode . highlight-parentheses-mode))

(use-package projectile
  :commands (projectile-mode projectile-project-root projectile-find-file)
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy
        projectile-require-project-root nil
        projectile-mode-line '(:eval (format "[%s]" (projectile-project-name)))
        projectile-cache-file (locate-user-emacs-file ".cache/projectile.cache")
        projectile-known-projects-file (locate-user-emacs-file ".cache/projectile.projects")
        projectile-enable-caching t
        projectile-indexing-method 'native)

  :config
  (setq projectile-globally-ignored-directories (append '("*.gradle" "*.log" ".cache" ".git" ".idea" "node_modules"  "elpa-backups" "build" "dist" "target") projectile-globally-ignored-directories)
        projectile-globally-ignored-files (append '("*.bundle.js" "*.build.js" "*.bundle.css" ".DS_Store" "*.min.js" "*.min.css" "package-lock.json" "projectile.cache") projectile-globally-ignored-files)
        grep-find-ignored-files (append '("*.bundle.j
s" "*.build.js" "*.bundle.css" ".DS_Store" "*.min.js" "*.min.css" "package-lock.json" "node_modules/*" ".cache/*" "./gradle/*") grep-find-ignored-files)))

(use-package company
  :commands company-mode
  :hook (prog-mode . company-mode)
  :init
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.25
   company-tooltip-align-annotations t
   company-tooltip-flip-when-above t
   company-show-numbers t
   company-selection-wrap-around t
   company-require-match nil))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :bind ("M-RET" . lsp-execute-code-action)
  :config
  (progn
    (setq
     lsp-print-io nil
     lsp-auto-configure t
     lsp-enable-snippet t
     lsp-completion-provider :capf
     lsp-headerline-breadcrumb-enable nil
     lsp-idle-delay 0.500)
    (dolist (directories '("[/\\\\].data\\'"
                           "[/\\\\].github\\'"
                           "[/\\\\]gradle\\'"
                           "[/\\\\].gradle\\'"
                           "[/\\\\].storybook\\'"
                           "[/\\\\].log\\'"
                           "[/\\\\].lsp-java\\'"
                           "[/\\\\]build\\'"
                           "[/\\\\]jetty-temp\\'"
                           "[/\\\\]coverage\\'"
                           "[/\\\\].DS_Store")
                         )
      (push directories lsp-file-watch-ignored)))
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (java-mode . lsp)
         (js-mode . lsp)
         (rjsx-mode . lsp)
         (js2-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands (lsp-ui-mode
             lsp-ui-doc-show
             lsp-ui-doc-hide)
  :init
  (progn
    (defvar tmn/lsp-ui-doc-max-height 10)
    (defvar tmn/lsp-ui-doc-max-width 60)

    (setq lsp-ui-sideline-show-code-actions t
          lsp-ui-sideline-show-hover t
          lsp-ui-doc-enable nil
          lsp-ui-peek-fontify t)
    (setq lsp-ui-doc-position 'at-point
          lsp-ui-doc-max-height tmn/lsp-ui-doc-max-height
          lsp-ui-doc-max-width tmn/lsp-ui-doc-max-width))
  :config
  (progn
    (define-key lsp-command-map (kbd "d s") 'lsp-ui-doc-show)
    (define-key lsp-command-map (kbd "d g") 'lsp-ui-doc-glance)
    (define-key lsp-command-map (kbd "d h") 'lsp-ui-doc-hide)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (progn
    (dap-auto-configure-mode t)
    (dap-mode t)
    (dap-ui-mode t)
    (dap-tooltip-mode t)
    (tooltip-mode t)
    (dap-ui-controls-mode t))
  (require 'dap-lldb)
  (require 'dap-cpptools))

(use-package company-lsp
  :straight (company-lsp :type git :host github :repo "tigersoldier/company-lsp")
  :after company
  :init
  (setq company-lsp-cache-candidates t
        company-lsp-async t))

(use-package flycheck
  :demand t
  :commands flycheck-mode
  :init
  (global-flycheck-mode)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15)))
  :config
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11"
                                            flycheck-clang-language-standard "c++11"))))

(use-package winum
  :init
  (winum-mode)
  :bind (("C-x w `" . winum-select-window-by-number)
         ("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind (("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-c t t"   . treemacs)
         ("C-c t B"   . treemacs-bookmarks)
         ("C-c t C-t" . treemacs-find-file)
         ("C-c t M-t" . treemacs-find-tag))
  :config
  (progn
    (setq treemacs-project-follow-mode t)
    (treemacs-resize-icons 16)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :bind (("M-p t e" . lsp-treemacs-errors-list)
         ("M-p t r" . lsp-treemacs-references)
         ("M-p t s" . lsp-treemacs-symbols))
  :config
  (progn
    (lsp-treemacs-sync-mode 1)))

(use-package which-key
  :demand t
  :config
  (progn
    (setq which-key-idle-delay 0.15)
    (setq which-key-idle-secondary-delay 0.15)
    (which-key-mode)))

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-x C-b" . ivy-switch-buffer))
  :init
  (ivy-mode))

(use-package ivy-rich
  :after counsel
  :hook ((ivy-mode . ivy-rich-mode)
         (counsel-projectile-mode . ivy-rich-mode))
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  :commands (counsel-recentf
             counsel-find-file
             counsel-yank-pop)
  :bind (("C-x C-f" . counsel-find-file)
         ("C-r" . counsel-recentf)
         ("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)))

(use-package counsel-projectile
  :commands (counsel-projectile-find-file
             counsel-projectile-rg)
  :bind (("C-x C-o" . counsel-projectile-find-file)
         ("C-x C-p" . counsel-projectile-rg)))

(use-package counsel-etags
  :after counsel
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))


(use-package all-the-icons
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el"))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :straight t
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package editorconfig
  :init (editorconfig-mode 1))

(use-package hydra)

(use-package magit
  :commands magit-status
  :bind (("C-c o g" . magit-status))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package term
  :bind (("C-c o t" . term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))


;; Dart and flutter mode
(use-package lsp-dart
  :after lsp-mode
  :hook (dart-mode . lsp))

;; run app from desktop without emulator
(use-package hover)

;; Package `css`
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

;; Package `elm`
(use-package elm-mode
  :mode "\\.elm$"
  :hook (elm-mode . elm-format-on-save-mode))

(use-package flycheck-elm
  :commands flycheck-elm-setup
  :hook (flycheck-mode . flycheck-elm-setup))

;; Package `ember-mode`
(use-package ember-mode)

;; Package `java`
(use-package lsp-java
  :hook (java-mode . lsp)
  :init
  (defvar lsp-java-test-path (f-join temporary-file-directory "tests"))
  (setq
   lsp-java-vmargs (list
                    "-noverify"
                    "-Xmx2G"
                    "-XX:+UseG1GC"
                    "-XX:+UseStringDeduplication"
                    "-javaagent:/Users/tmn/.emacs.d/lib/lombok.jar")
   lsp-java-pop-buffer-function 'pop-to-buffer
   lsp-java-java-path "/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home/bin/java"
   lsp-java-workspace-dir (f-join lsp-java-test-path "workspace")
   lsp-java-workspace-cache-dir (f-join temporary-file-directory "workspace-cache")
   lsp-java-server-install-dir (locate-user-emacs-file ".lsp-java/server/")
   lsp-response-timeout 30)
  :config
  (progn
    (require 'lsp-java-boot)
    (add-hook 'lsp-mode-hook 'lsp-lens-mode)
    (add-hook 'java-mode-hook 'lsp-java-boot-lens-mode)))

(use-package dap-java
  :straight (dap-java :type git :host github :repo "emacs-lsp/lsp-java"))

;; Package `kotlin`
(use-package kotlin-mode
  :mode "\\.kt$")

;; Package `javascsript`
(use-package js2-mode
  :mode "\\.\\(m?js\\|es6\\)$"
  :init
  (setq-default js2-show-parse-errors nil
                js2-strict-missing-semi-warning nil
                js2-strict-inconsistent-return-warning nil
                js2-strict-var-hides-function-arg-warning nil
                js2-strict-cond-assign-warning nil
                js2-strict-var-redeclaration-warning nil
                js2-strict-trailing-comma-warning nil)

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2)
  (setq js-indent-level 2)

  :config
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1))))

(use-package js2-refactor
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode))

(use-package web-mode
  :mode "\\.html$"
  :config
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t))

(use-package rjsx-mode
  :mode "\\.js$"
  :commands (rjsx-mode))

(use-package nodejs-repl
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
              (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
              (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
              (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
              (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
              (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Package `json`
(use-package json-mode
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(use-package json-reformat
  :commands json-reformat
  :init (setq json-reformat:indent-width 2))

;; Package `json`
(use-package markdown-mode
  :mode "\\.md$")

;; Package `mustache`
(use-package mustache-mode
  :mode "\\.mustache$")

(use-package python-mode
  :hook
  (python-mode . flycheck-mode)
  (python-mode . company-mode)
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; Package org-mode
(use-package org
  :straight (org-plus-contrib :type git :repo "https://git.sr.ht/~bzg/org-contrib")
  :bind (:map org-mode-map
              ;; Prevent Org from overriding the bindings for
              ;; windmove. By default, these keys are mapped to
              ;; `org-shiftleft', etc.
              ("S-<left>" . nil)
              ("S-<right>" . nil)
              ("S-<up>" . nil)
              ("S-<down>" . nil)

              ;; Add replacements for the keybindings we just removed.
              ;; C-<left> and C-<right> are unused by Org. C-<up> and
              ;; C-<down> are bound to `org-backward-paragraph', etc.
              ;; (but see below).
              ("C-<left>" . #'org-shiftleft)
              ("C-<right>" . #'org-shiftright)
              ("C-<up>" . #'org-shiftup)
              ("C-<down>" . #'org-shiftdown)

              ;; By default, Org maps C-<up> to
              ;; `org-backward-paragraph' instead of
              ;; `backward-paragraph' (and analogously for C-<down>).
              ;; However, it doesn't do the same remapping for the
              ;; other bindings of `backward-paragraph' (e.g. M-{).
              ;; Here we establish that remapping. (This is important
              ;; since we remap C-<up> and C-<down> to other things,
              ;; above. So otherwise there would be no easy way to
              ;; invoke `org-backward-paragraph' and
              ;; `org-forward-paragraph'.)
              ([remap backward-paragraph] . #'org-backward-paragraph)
              ([remap forward-paragraph] . #'org-forward-paragraph)

              ;; See discussion of this function below.
              ("C-M-RET" . #'radian-org-insert-heading-at-point)
              ("C-M-<return>" . #'radian-org-insert-heading-at-point))
  :bind* (;; Add the global keybindings for accessing Org Agenda and
          ;; Org Capture that are recommended in the Org manual.
          ("C-c a" . #'org-agenda)
          ("C-c c" . #'org-capture))
  ;; :bind (("C-c a" . org-agenda))
  :config
  ;; If you try to insert a heading in the middle of an entry, don't
  ;; split it in half, but instead insert the new heading after the
  ;; end of the current entry.
  (setq org-insert-heading-respect-content t)

  ;; But add a new function for recovering the old behavior (see
  ;; `:bind' above).
  (defun radian-org-insert-heading-at-point ()
    "Insert heading without respecting content.
This runs `org-insert-heading' with
`org-insert-heading-respect-content' bound to nil."
    (interactive)
    (let ((org-insert-heading-respect-content nil))
      (org-insert-heading)))

  ;; Show headlines but not content by default.
  (setq org-startup-folded 'content)

  ;; Make it possible to dim or hide blocked tasks in the agenda view.
  (setq org-enforce-todo-dependencies t)

  ;; Make C-a, C-e, and C-k smarter with regard to headline tags.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  (put 'org-tags-exclude-from-inheritance 'safe-local-variable
       #'radian--list-of-strings-p)

  ;; When you create a sparse tree and `org-indent-mode' is enabled,
  ;; the highlighting destroys the invisibility added by
  ;; `org-indent-mode'. Therefore, don't highlight when creating a
  ;; sparse tree.
  (setq org-highlight-sparse-tree-matches nil))

(use-package rst
  :mode (("\\.txt$" . rst-mode)
         ("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)))

(use-package swift-mode
  :after lsp-mode
  :hook (swift-mode . lsp))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(use-package yaml-mode)

(use-package cmake-mode)

(use-package groovy-mode
  :mode "\\.\\(gradle\\|groovy\\)$")

(use-package cmake-mode)

(use-package bazel-mode
  :straight (bazel-mode :type git :host github :repo "bazelbuild/emacs-bazel-mode")
  :mode "\\.\\(star\\|bzl\\|bazel\\)$")

(use-package protobuf-mode
  :mode "\\.proto$")

(use-package qml-mode
  :mode "\\.qml$")

;;; tmn.el ends here
