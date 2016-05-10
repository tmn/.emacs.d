;;; lang-html.el --- HTML support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package html
  :ensure nil
  :mode ("\\.\\(html|htm\\)" . html-mode)
  :config
  (setq sgml-quick-keys 'close))

(provide 'lang-html)
;;; lang-html.el ends here
