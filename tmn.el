;;; tmn.el --- tmn.el configuration -*- lexical-binding: t -*-
;;; Commentary:
;; The tmn.el configuration.

;;; Code:

;; -----------------------------------------------------------------------------
;; System Workaround for emacs version <29.x
;; -----------------------------------------------------------------------------

(if (version< emacs-version "29")
  (setq image-types (cons 'svg image-types)))


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

;; -----------------------------------------------------------------------------
;; Environemtn Variables
;; -----------------------------------------------------------------------------

;; Java
(if (file-directory-p "/opt/homebrew/opt/openjdk@11")
    (setenv "JAVA_HOME" "/opt/homebrew/opt/openjdk@11"))

;; Rust
(setenv "RUSTUP_TOOLCHAIN" "stable")

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
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


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

(show-paren-mode 1)
(setq show-paren-delay 0)

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

;; String helpers
(use-package s)

(use-package diminish)

(use-package rg
  :commands rg)

;; Trim spaces from end of line on save
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package doom-themes
  :init
  ; (load-theme 'doom-vibrant t)
  (load-theme 'doom-opera-light t)
  ; (load-theme 'modus-operandi t)

  ; (disable-theme 'modus-operandi)
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
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-env-version t))

(use-package emojify
  :commands (emojify-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-." . er/contract-region)))

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

  (setq projectile-globally-ignored-directories (append '("*.gradle" "*.log" ".cache" ".git" ".idea" "node_modules" ".next" "elpa-backups" "build" "dist" "target") projectile-globally-ignored-directories)
        projectile-globally-ignored-files (append '("*.bundle.js" "*.build.js" "*.bundle.css" ".DS_Store" "*.min.js" "*.min.css" "package-lock.json" "projectile.cache") projectile-globally-ignored-files)
        grep-find-ignored-files (append '("*.bundle.js" "*.build.js" "*.bundle.css" ".DS_Store" "*.min.js" "*.min.css" "package-lock.json" "node_modules/*" ".cache/*" "./gradle/*") grep-find-ignored-files)))

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package lsp-mode
  :commands lsp
  :init
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
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
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
         ((c-mode
           c++-mode
           dart-mode
           java-mode
           js-mode
           js2-mode
           rjsx-mode
           swift-mode
           typescript-mode
           tsx-mode
           python-mode
           web-mode) . lsp-deferred)
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


;;
;; Window stuff
;;

(defun t/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . t/org-mode-visual-fill))


;; (use-package treemacs-all-the-icons)

;; (use-package treemacs
;;   :commands (treemacs-follow-mode
;;              treemacs-filewatch-mode
;;              treemacs-load-theme)
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   (setq treemacs-follow-after-init t
;;         treemacs-is-never-other-window t
;;         treemacs-sorting 'alphabetic-case-insensitive-asc
;;         treemacs-persist-file "~/.emacs.d/.cache/treemacs-persist"
;;         treemacs-last-error-persist-file "~/.emacs.d/.cache/treemacs-last-error-persist")
;;   :bind (("M-0"       . treemacs-select-window)
;;          ("C-x t 1"   . treemacs-delete-other-windows)
;;          ("C-c t t"   . treemacs)
;;          ("C-c t B"   . treemacs-bookmarks)
;;          ("C-c t C-t" . treemacs-find-file)
;;          ("C-c t M-t" . treemacs-find-tag))
;;   :config
;;   (treemacs-follow-mode -1)
;;   (require 'treemacs-all-the-icons)
;;   (treemacs-load-theme "all-the-icons")
;;   (setq treemacs-project-follow-mode t)
;;   (treemacs-filewatch-mode t)

;;   :custom
;;   (treemacs-space-between-root-nodes nil)
;;   (treemacs-fringe-indicator-mode nil)
;;   (treemacs-indentation 2))

;; (use-package treemacs-projectile
;;   :after (treemacs projectile))

;; (use-package treemacs-icons-dired
;;   :after (treemacs dired)
;;   :config
;;   (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after (treemacs magit))

(use-package all-the-icons-dired)


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
    (dired-omit-mode 0)
    (dired-hide-details-mode 1)
    (all-the-icons-dired-mode 1)
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
    :config

    (defun t/dired-sidebar-mode ()
      (interactive)
      (unless (file-remote-p default-directory)
                (auto-revert-mode)))

    (add-hook 'dired-sidebar-mode-hook 't/dired-sidebar-mode)

    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

    ;; (setq dired-sidebar-subtree-line-prefix "__")
    ;; (setq dired-sidebar-theme 'vscode)
    ;; (setq dired-sidebar-use-term-integration t)
    ;; (setq dired-sidebar-use-custom-font t)
)

  (add-hook 'dired-mode-hook 'dired-subtree-toggle)

  ;; (defun t/dired-subtree-tab ()
  ;;   (interactive)

  ;;   )

  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("h" . dired-single-up-directory)
              ("H" . dired-omit-mode)
              ("l" . dired-single-buffer)
              ("y" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("p" . dired-ranger-paste)))


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

(use-package all-the-icons
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el")
  :config
  (setq all-the-icons-scale-factor 0.9))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

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

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

;; Dart and flutter mode
(use-package lsp-dart
  :after lsp-mode)


;; Package `java`
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
  :hook ((js2-mode . js2-refactor-mode)))

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

(use-package prettier-js
  :commands prettier-js-mode
  :hook ((typescript-mode
          tsx-mode
          json-mode
          web-mode) . prettier-js-mode))

(use-package typescript-mode
  :mode "\\.\\(ts\\|tsx\\)$"
  :init
  (setq-default typescript-indent-level 2))

(use-package rjsx-mode
  :mode "\\.js$"
  :commands (rjsx-mode))

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
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t))

(use-package json-mode
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(use-package json-reformat
  :commands json-reformat
  :init (setq json-reformat:indent-width 2))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)$")

(use-package mustache-mode
  :mode "\\.mustache$")

;; Python
(use-package python-mode
  :hook
  (python-mode . flycheck-mode)
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-pyright
  :after (python-mode)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; Tree Sitter
(use-package tree-sitter
  :after (tree-sitter-langs)
  :commands (tree-sitter-mode global-tree-sitter-mode)
  :init
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git :host github :repo "emacsmirror/tree-sitter-langs")
  :config
  (tree-sitter-require 'tsx))

(use-package tree-sitter-indent
  :straight (tree-sitter-indent :type git :host github :repo "emacsmirror/tree-sitter-indent"))

(use-package tsi
  :commands (tsi-typescript-mode)
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  :mode (("\\.tsx$" . tsi-typescript-mode)))

(use-package origami
  :straight (origami :type git :host github :repo "gregsexton/origami.el")
  :hook ((yaml-mode . origami-mode)))

(use-package overlay
  :straight (overlay :type git :host github :repo "twada/coverlay.el"))

(use-package graphql-mode)

(use-package tsx-mode
  :straight (tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el")
  :mode (("\\.tsx$" . tsx-mode)))


;; Package org-mode

(defun t/org-mode-setup ()
  "..."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

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


;; Qt
;; (use-package qml-mode
;;   :mode "\\.qml$"
;;   :config
;;   (setq tab-width 4
;;         qml-indent-width 4))

;; Swift
(use-package swift-mode
  :after lsp-mode)

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

;; Rust
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  (defun t/rustic-mode-hook ()
    "So that run C-c C-c C-r works without having to confirm, but don't try to
save rust buffers that are not file visiting. Once
 https://github.com/brotzeit/rustic/issues/253 has been resolved this should
 no longer be necessary."
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 't/rustic-mode-hook))

(use-package rust-playground)

(use-package toml-mode)

(use-package cargo
  :hook (rustic-mode . cargo-minor-mode))

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
