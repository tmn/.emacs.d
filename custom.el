;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(auto-save-list-file-prefix nil)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(column-number-mode t)
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(helm-follow-mode-persistent t)
 '(helm-source-names-using-follow '("Search at ~/.emacs.d/"))
 '(org-agenda-files nil)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(flycheck-elm elm-mode markdown-mode kotlin-mode json-reformat lsp-java all-the-icons helm which-key swiper-helm swiper swift-mode lsp-sourcekit diminish eslint-fix yaml-mode web-mode rjsx-mode prettier-js js2-mode flycheck ruby-end ember-mode json-mode editorconfig neotree yasnippet helm-dash helm-ls-git helm-projectile helm-ag helm-hunks wgrep-ag wgrep ag magit doom-modeline doom-themes company-lsp helm-lsp dap-mode lsp-ui lsp-mode company exec-path-from-shell highlight-parentheses multiple-cursors projectile s dash use-package))
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
