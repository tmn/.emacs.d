;;; t-term.el --- Terminal Emulator -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package term
  :ensure t
  :bind (("C-c t" . term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(provide 't-term)
;;; t-term.el ends here
