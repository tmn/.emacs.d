;;; lang-js.el --- JavaScript support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package js2-mode
  :mode "\\.js$"
  :init
  (setq-default js2-show-parse-errors nil
                js2-strict-missing-semi-warning nil
                js2-strict-inconsistent-return-warning nil
                js2-strict-var-hides-function-arg-warning nil
                js2-strict-cond-assign-warning nil
                js2-strict-var-redeclaration-warning nil
                js2-strict-trailing-comma-warning t)

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2)
  (setq js-indent-level 2)

  :config
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1))))

(use-package prettier-js
  :ensure t
  :commands prettier-js-mode
  :init
  (progn
    (setq prettier-js-args '("--jsx-bracket-same-line")
          prettier-js-show-errors 'buffer)

    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'css-mode-hook 'prettier-js-mode)
    (add-hook 'rjsx-mode-hook 'prettier-js-mode)))

(use-package rjsx-mode
  :mode "\\.jsx?$"
  :commands (rjsx-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'lang-js)
;;; lang-js.el ends here
