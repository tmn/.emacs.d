;;; lang-elm.el --- Elm mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elm-mode
  :ensure t
  :mode "\\.elm$"
  :init
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))

(use-package flycheck-elm
  :commands flycheck-elm-setup
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))

(provide 'lang-elm)
;;; lang-elm.el ends here
