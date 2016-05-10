;;; lang-ruby.el --- Ruby -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ruby-end
  :diminish ruby-end-mode
  :init
  (defun ruby-end ()
    (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
         "\\(?:^\\|\\s-+\\)\\(?:do\\)")
    (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
    (ruby-end-mode +1))

  (remove-hook 'ruby-mode-hook 'ruby-end-mode)
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)

  :config
  (remove-hook 'ruby-mode-hook 'ruby-end-mode)
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
