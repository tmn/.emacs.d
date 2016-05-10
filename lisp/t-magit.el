;;; t-magit.el --- Magit support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit)

(provide 't-magit)
;;; t-magit.el ends here
