;;; tmn.el --- tmn.el configuration -*- lexical-binding: t -*-
;;; Commentary:
;; The tmn.el configuration.

;;; Code:


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
(global-display-line-numbers-mode 1)
`
(save-place-mode 1)
(setq use-dialog-box nil)


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
(set-fringe-mode 10)

(setq visible-bell t)

;; (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
;; (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Follow symlinks
(setq vc-follow-symlinks t)


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

(defun t/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun t/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-S-<up>") 't/move-line-up)
(global-set-key (kbd "M-S-<down>") 't/move-line-down)


;; -----------------------------------------------------------------------------
;; Confirm exit emacs
;; -----------------------------------------------------------------------------

(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Dumme ape! Quitting already??? "))
          'append)


;; -----------------------------------------------------------------------------
;; Native Parentheses Matching
;; -----------------------------------------------------------------------------

(setq show-paren-delay 0
      show-paren-mode 1)

(defun match-paren (arg)
  "Go to the matching paren using ARG if on a paren; otherwise insert ARG."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'match-paren)


;; -----------------------------------------------------------------------------
;; Bootstrap configs
;; -----------------------------------------------------------------------------

(use-package diminish)

(use-package rg
  :commands rg)

;; Trim spaces from end of line on save
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline nil)

  ; (load-theme 'doom-vibrant t)
  ; (load-theme 'doom-opera-light t)
  (load-theme 'modus-operandi t)
  (load-theme 'doom-palenight t)
  ; (load-theme 'leuven t)
  ; (load-theme 'doom-one-light t)
  ;; (load-theme 'one-light t)
  (disable-theme 'modus-operandi)

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


;; Display Emojis in emacs
(use-package emojify
  :commands (emojify-mode))

(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("M-]" . er/contract-region)
         ("C-=" . er/mark-outside-pairs)))

(use-package multiple-cursors
  :bind (("C-c C-. ."   . mc/mark-all-dwim)
         ("C-c C-. C-." . mc/mark-all-like-this-dwim)
         ("C-c C-. n"   . mc/mark-next-symbol-like-this)
         ("C-c C-. P"   . mc/mark-previous-symbol-like-this)
         ("C-c C-. A"   . mc/mark-all-symbols-like-this)
         ("C-c C-. f"   . mc/mark-all-like-this-in-defun)
         ("C-c C-. l"   . mc/edit-lines)
         ("C-c C-. e"   . mc/edit-ends-of-lines)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)))

(use-package which-key
  :demand t
  :config
  (progn
    (setq which-key-add-column-padding 1
          which-key-max-display-columns nil
          which-key-min-display-lines 1
          which-key-separator " "
          which-key-idle-delay 0.5)
    (which-key-mode)))

(use-package hydra)



;; -----------------------------------------------------------------------------
;; Projectile
;; -----------------------------------------------------------------------------

(use-package projectile
  :commands (projectile-mode projectile-project-root projectile-find-file)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-require-project-root nil
        projectile-mode-line '(:eval (format "[%s]" (projectile-project-name)))
        projectile-cache-file (locate-user-emacs-file ".cache/projectile.cache")
        projectile-known-projects-file (locate-user-emacs-file ".cache/projectile.projects")
        projectile-enable-caching t
        projectile-indexing-method 'native)

  (setq projectile-globally-ignored-directories
        (append '("*.gradle" "*.log" ".cache" ".git" ".idea" "node_modules" ".next" "elpa-backups" "build" "dist" "target") projectile-globally-ignored-directories)

        projectile-globally-ignored-files
        (append '("*.bundle.js" "*.build.js" "*.bundle.css" ".DS_Store" "*.min.js" "*.min.css" "package-lock.json" "projectile.cache") projectile-globally-ignored-files)

        grep-find-ignored-files
        (append '("*.bundle.js" "*.build.js" "*.bundle.css" ".DS_Store" "*.min.js" "*.min.css" "package-lock.json" "node_modules/*" ".cache/*" "./gradle/*") grep-find-ignored-files)))

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode))


;; -----------------------------------------------------------------------------
;; Tree-sitter
;; -----------------------------------------------------------------------------

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


;; -----------------------------------------------------------------------------
;; Eglot stuff
;; -----------------------------------------------------------------------------

(use-package eglot
  :straight (:type built-in)
  :commands (eglot-ensure)
  :config
  ;; Patch pretty print of jsonrpc log making whole thing goes slooooooow
  (fset #'jsonrpc--log-event #'ignore))


;; -----------------------------------------------------------------------------
;; C / C++
;; -----------------------------------------------------------------------------

(use-package c++-mode
  :straight nil
  :custom
  (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode) . ("clangd")))
  :hook 
  ((c++-mode c++-ts-mode) . eglot-ensure))

(use-package c-mode
  :straight nil
  :custom
  (add-to-list 'eglot-server-programs '((c-mode c-ts-mode) . ("clangd")))
  :hook
  ((c-mode c-ts-mode) . eglot-ensure))



;; -----------------------------------------------------------------------------
;; Rust
;; -----------------------------------------------------------------------------

(use-package rust-mode
  :init
  ;; Environment Variables
  (setenv "RUSTUP_TOOLCHAIN" "stable")

  :custom
  (add-to-list 'eglot-server-programs '((rust-mode) . ("rust-analyzer" :initializationOptions
                                                   ( :procMacro (:enable t)
                                                     :cargo ( :buildScripts (:enable t)
                                                              :features "all" )))))
  :config
  (setq rust-format-on-save t)

  (use-package cargo :hook ((rust-mode . cargo-minor-mode)))
  (use-package rust-playground)
  (use-package toml-mode)

  (defun t/rust-mode-prettifying ()
    "Dired load func."
    (interactive)
    (prettify-symbols-mode))

  :hook
  ((rust-mode . t/rust-mode-prettifying)
   (rust-mode . eglot-ensure)))



;; -----------------------------------------------------------------------------
;; JavaScript / TypeScript
;; -----------------------------------------------------------------------------

(use-package t/typescript-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (add-to-list 'eglot-server-programs '((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  :hook
  ((typescript-ts-mode tsx-ts-mode) . eglot-ensure))

(use-package t/javascript-mode
  :straight nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :custom
  (add-to-list 'eglot-server-programs '((js-mode js2-mode js-ts-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
    :lint t))
  :custom
  (setq js-indent-level 2)
  :hook ((js-mode js2-mode js-ts-mode) . eglot-ensure))

(use-package nodejs-repl
  :config
  (defun t/js-ts-mode-hook ()
    (interactive)
    (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
    (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
    (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
    (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
    (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
    (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

  (add-hook 'js-ts-mode-hook 't/js-ts-mode-hook))

(use-package prettier-js
  :commands prettier-js-mode
  :hook ((json-mode
          js-ts-mode
          tsx-ts-mode
          typescript-ts-mode) . prettier-js-mode))

;; -----------------------------------------------------------------------------
;; Swift
;; -----------------------------------------------------------------------------

(use-package swift-mode
  :mode "\\.swift$"
  :config
  (require 'eglot)
  (add-to-list 'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :hook
  (swift-mode . eglot-ensure))


;; -----------------------------------------------------------------------------
;; Bash
;; -----------------------------------------------------------------------------
(use-package bash-mode
  :straight nil
  :mode ("\\.sh$" . bash-ts-mode)
  :custom
  (add-to-list 'eglot-server-programs '((bash-mode bash-ts-mode) . ("bash-language-server")))
  :hook
  ((bash-mode bash-ts-mode) . eglot-ensure))


;; -----------------------------------------------------------------------------
;; Python
;; -----------------------------------------------------------------------------
(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-pyright
  :after (python-mode)
  :config
  (defun t/python-mode ()
    (interactive)
    (require 'lsp-pyright)
    (eglot-ensure))
  :hook
  (python-mode . t/python-mode))


;; -----------------------------------------------------------------------------
;; Java / Kotlin / JVM
;; -----------------------------------------------------------------------------

;; Environment Variables

(if (file-directory-p "/opt/homebrew/opt/openjdk@11")
    (setenv "JAVA_HOME" "/opt/homebrew/opt/openjdk@11"))


(use-package lsp-java
  :after (lsp-mode)
  :config
  (setq lsp-java-vmargs
        `("-noverify"
          "-Xmx2G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/lib/lombok.jar"))
          ,(concat "-Xbootclasspath/a:" (expand-file-name "~/.emacs.d/lib/lombok.jar"))))
  (setq lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (setq lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace"))
  (setq lsp-java-workspace-cache-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace-cache"))
  :custom
  (progn
    (require 'lsp-java-boot)
    (add-hook 'lsp-mode-hook 'lsp-lens-mode)
    (add-hook 'java-mode-hook 'lsp-java-boot-lens-mode)))

(use-package dap-java
  :straight (dap-java :type git :host github :repo "emacs-lsp/lsp-java"))

;; Package `kotlin`
(use-package kotlin-mode
  :mode "\\.kt$")



;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------



;; LSP mode
;;------------------------------------------------------------------------------
(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-server-install-dir (concat user-emacs-directory "lsp/"))
  (setq lsp-keymap-prefix "C-c C-l")

  (defun t/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless

  :bind ("M-RET" . lsp-execute-code-action)
  :custom
  (lsp-print-io nil)
  (lsp-auto-configure t)
  (lsp-enable-snippet t)
  ;; (lsp-completion-provider :capf)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.5)

  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.

  :config
  (dolist (directories '("[/\\\\].data\\'"
                         "[/\\\\].github\\'"
                         "[/\\\\]gradle\\'"
                         "[/\\\\].gradle\\'"
                         "[/\\\\].storybook\\'"
                         "[/\\\\].log\\'"
                         "[/\\\\]eclipse.jdt.ls\\'"
                         "[/\\\\]build\\'"
                         "[/\\\\]jetty-temp\\'"
                         "[/\\\\]coverage\\'"
                         "[/\\\\].DS_Store"))
    (push directories lsp-file-watch-ignored))
  :hook (
         (lsp-completion-mode . t/lsp-mode-setup-completion)
         ((java-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :commands (lsp-ui-mode
             lsp-ui-doc-show
             lsp-ui-doc-hide)
  :init
  (progn
    (defvar t/lsp-ui-doc-max-height 10)
    (defvar t/lsp-ui-doc-max-width 60)

    (setq
     lsp-ui-sideline-enable t
     lsp-ui-sideline-show-code-actions t
     lsp-ui-sideline-show-hover nil
     lsp-ui-sideline-show-symbol t
     lsp-ui-sideline-show-diagnostics nil
     lsp-ui-doc-enable nil
     lsp-ui-peek-fontify t)
    (setq
     lsp-ui-doc-position 'at-point
     lsp-ui-doc-max-height t/lsp-ui-doc-max-height
     lsp-ui-doc-max-width t/lsp-ui-doc-max-width))
  :config
  (progn
    (define-key lsp-command-map (kbd "d s") 'lsp-ui-doc-show)
    (define-key lsp-command-map (kbd "d g") 'lsp-ui-doc-glance)
    (define-key lsp-command-map (kbd "d h") 'lsp-ui-doc-hide)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))


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

(use-package flycheck
  :demand t
  :commands flycheck-mode
  :init
  ;; (global-flycheck-mode)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15)))
  :config
  ;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14"
  ;;                                           flycheck-clang-language-standard "c++14")))
)

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


;;
;; Window stuff
;;

(defun t/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun t/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t
        display-line-numbers-mode nil)
  (visual-fill-column-mode 1))

(defun t/org-present-start ()
  ;; Center the presentation and wrap lines
  ;; (visual-fill-column-mode 1)


  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")

  ;; (visual-line-mode 1)
  (org-display-inline-images)
  (t/org-present-prepare-slide))

(defun t/org-present-end ()
  ;; Center the presentation and wrap lines
  ;; (visual-fill-column-mode 0)
  ;; (visual-line-mode 0)
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)

  (org-present-small)
  (org-remove-inline-images))

(defun t/org-present-prev ()
  (interactive)
  (org-present-prev)
  (t/org-present-prepare-slide))

(defun t/org-present-next ()
  (interactive)
  (org-present-next)
  (t/org-present-prepare-slide))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . t/org-mode-visual-fill))

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------
(use-package dired
  :straight nil
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (defun t/dired-load ()
    "Dired load func."
    (interactive)
    (dired-collapse))

  (defun t/dired-mode ()
    "Dired mode func."
    (interactive)
    (dired-omit-mode 1)
    (dired-hide-details-mode 1)
    (nerd-icons-dired-mode 1)
    (hl-line-mode 1))

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook 't/dired-load)
  (add-hook 'dired-mode-hook 't/dired-mode)


  (use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

  (use-package dired-single
    :straight (dired-single :type git :host github :repo "emacsmirror/dired-single")
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (use-package dired-subtree
    :defer t)

  (use-package dired-sidebar
    :defer t
    :commands (dired-sidebar-toggle-sidebar)
    :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
    :init
    (setq dired-sidebar-window-fixed nil)
    :config

    (defun t/dired-sidebar-mode ()
      (interactive)
      (visual-line-mode 0)
      (toggle-truncate-lines 1)
      (unless (file-remote-p default-directory)
                (auto-revert-mode)))

    (add-hook 'dired-sidebar-mode-hook 't/dired-sidebar-mode)

    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
    (setq dired-sidebar-theme 'nerd))

  (add-hook 'dired-mode-hook 'dired-subtree-toggle)

  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("h" . dired-single-up-directory)
              ("H" . dired-omit-mode)
              ("l" . dired-single-buffer)
              ("y" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("p" . dired-ranger-paste)))




(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<escape>" . minibuffer-keyboard-quit)
              ("?" . minibuffer-completion-help)
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              ("<backspace>" . vertico-directory-delete-char)
              ("C-w" . vertico-directory-delete-word)
              ("C-<backspace>" . vertico-directory-delete-word)
              ; ("RET" . vertico-dirrectory-enter)
              ("C-i" . vertico-quick-insert)
              ("C-o" . vertico-quick-exit)
              ("M-o" . kb/vertico-quick-embark)
              ("M-G" . vertico-multiform-grid)
              ("M-F" . vertico-multiform-flat)
              ("M-R" . vertico-multiform-reverse)
              ("M-U" . vertico-multiform-unobtrusive)
              ("C-l" . kb/vertico-multiform-flat-toggle))
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
                                          (lambda (orig cand prefix suffix index _start)
                                            (setq cand (funcall orig cand prefix suffix index _start))
                                            (concat
                                             (if (= vertico--index index)
                                                 (propertize "» " 'face 'vertico-current)
                                               "  ")
                                             cand))))


(defun t/get-project-root ()
  "Get project root."
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :bind (
         ("C-x C-b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-x C-p" . consult-ripgrep)
         :map minibuffer-local-map
         ("C-r" . consult-history))


  :custom
  (consult-project-root-function #'t/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (consult-preview-at-point-mode))

(use-package consult-projectile
  :after consult
  :bind (("C-x C-o" . consult-projectile-find-file)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))


(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))

  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))))


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




;; -----------------------------------------------------------------------------
;; ...
;; -----------------------------------------------------------------------------
(use-package magit
  :commands magit-status
  :bind (("C-c o g" . magit-status))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

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

(use-package ember-mode)

(use-package web-mode
  :commands (web-mode)
  :mode "\\.html$"
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
  :init (setq json-reformat:indent-width 2))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)$")

(use-package mustache-mode
  :mode "\\.mustache$")


(use-package origami
  :straight (origami :type git :host github :repo "gregsexton/origami.el")
  :hook ((yaml-mode . origami-mode)))

(use-package overlay
  :straight (overlay :type git :host github :repo "twada/coverlay.el"))

(use-package graphql-mode)


;; -----------------------------------------------------------------------------
;; Package org-mode
;; -----------------------------------------------------------------------------
(defun t/org-mode-setup ()
  "..."
  (interactive)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode)
  (display-line-numbers-mode 0))

(use-package org
  :straight (:type built-in)
  :hook (org-mode . t/org-mode-setup)
  :bind (:map org-mode-map
              ("C-j" . org-next-visible-heading)
              ("C-k" . org-previous-visible-heading)
              ("M-j" . org-metadown)
              ("M-k" . org-metaup))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-modules
    '(org-crypt
        org-habit
        org-bookmark
        org-eshell
        org-irc))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (use-package org-present
    :bind (:map org-present-mode-keymap
                ("C-c C-j" . t/org-present-next)
                ("C-c C-k" . t/org-present-prev))
    :hook ((org-present-mode . t/org-present-start)
           (org-present-mode-quit . t/org-present-end)))

  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))


  (set-face-attribute 'org-document-title nil :font "SF Mono" :weight 'bold :height 1.3)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "SF Mono" :weight 'medium :height (cdr face)))

  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)


  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  )

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-ai
  :hook (org-mode . org-ai-mode)
  :custom
  (org-ai-openai-api-token "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"))

(use-package rst
  :mode (("\\.txt$" . rst-mode)
         ("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)))

(use-package qml-mode
  :mode "\\.qml$"
  :config
  (setq tab-width 4
        qml-indent-width 4))

(use-package csharp-mode
  :commands (csharp-tree-sitter-mode)
  :mode "\\.cs$"
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(use-package yaml-mode)

(use-package groovy-mode
  :mode "\\.\\(gradle\\|groovy\\)$")

(use-package bazel-mode
  :straight (bazel-mode :type git :host github :repo "bazelbuild/emacs-bazel-mode")
  :mode "\\.\\(star\\|bzl\\|bazel\\)$")

(use-package protobuf-mode
  :mode "\\.proto$")

;;; Other tools

(use-package cmake-mode)

(use-package restclient)

;;; tmn.el ends here
