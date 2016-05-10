;;; lang-org.el --- Org Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :defer t
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-log-done 'time))
;        org-log-done 'note))

(provide 'lang-org)
;;; lang-org.el ends here
