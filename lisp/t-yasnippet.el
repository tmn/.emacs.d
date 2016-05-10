;;; t-yasnippet.el --- JSON support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :init
  (setq yas-snippet-dirs '(user-dir-snippets))
  :config
  (yas-global-mode 1))

(provide 't-yasnippet)
;;; t-yasnippet.el ends here
