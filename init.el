;;; init.el --- Load the configuration -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs configuration

;;; Code:

(require 'package)
(unless package--initialized
  (package-initialize t))

(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (require 'cl))

(require 'bind-key)

(use-package diminish)
(use-package dash)
(use-package s)

(let ((my-gnutls-min-prime-bits 4096))
  (setq gnutls-min-prime-bits my-gnutls-min-prime-bits))

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)

(setq-default
 explicit-shell-file-name "/bin/bash"
 gc-cons-threshold 100000000
 read-process-output-max (* 1024 1024))

;; -----------------------------------------------------------------------------
;; Directories
;; -----------------------------------------------------------------------------
(defconst user-emacs-directory "~/.emacs.d/")
(defun user-emacs-file (path)
  "Prefix PATH with user-emacs-eidrectory."
  (concat user-emacs-directory path))

(defconst user-dir-snippets (user-emacs-file "snippets"))
(defconst user-dir-lisp (user-emacs-file "lisp"))
(defconst user-dir-lisp-lang (user-emacs-file "lisp/lang"))
(defconst vendor-dir-lisp (user-emacs-file "vendor"))

(add-to-list 'load-path user-dir-lisp)
(add-to-list 'load-path user-dir-lisp-lang)
(add-to-list 'load-path vendor-dir-lisp)

;; -----------------------------------------------------------------------------
;; Bootstrap configs
;; -----------------------------------------------------------------------------

(setq custom-file (user-emacs-file "custom.el"))
(load custom-file)

(require 't-core)
(require 't-editor)
(require 't-magit)
(require 't-term)
(require 't-yasnippet)

(require 'lang-css)
(require 'lang-elm)
(require 'lang-ember)
(require 'lang-html)
(require 'lang-java)
(require 'lang-js)
(require 'lang-json)
(require 'lang-kotlin)
(require 'lang-markdown)
(require 'lang-org)
(require 'lang-swift)
(require 'lang-ruby)
(require 'lang-yaml)

;; Vendor stuff
(require 'mustache-mode)


;; -----------------------------------------------------------------------------
;; ...
;; -----------------------------------------------------------------------------
(setq-default
 indent-tabs-mode nil
 line-spacing 6
 tab-always-indent 'complete
)

(setq
 inhibit-startup-screen t
 select-enable-clipboard t
 shell-file-name "/bin/zsh"
 tab-width 2
)

(when (eq system-type 'windows-nt)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(when (eq system-type 'darwin)
  (setq
   dired-use-ls-dired nil
   mac-command-modifier 'meta ; Set cmd to meta key
   mac-option-modifier nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode 1)


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; --------------------------------------

;; ;; Enable the www ligature in every possible major mode
;; (ligature-set-ligatures 't '("www"))

;; ;; Enable ligatures in programming modes
;; (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
;;                                      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
;;                                      "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
;;                                      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
;;                                      "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
;;                                      "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
;;                                      "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
;;                                      "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
;;                                      "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;;                                      "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

;; (global-ligature-mode 't)


;; --------------------------------------

(cond
 ((find-font (font-spec :family "Fira Code Retina"))
  (set-frame-font "Fira Code Retina:pixelsize=12"))
 ((find-font (font-spec :family "SF Mono"))
  (set-frame-font "SF Mono:pixelsize=12"))
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-fram-font "Monaco:pixelsize=12")))

(provide 'init)
;;; init.el ends here
