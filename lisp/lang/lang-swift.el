;;; lang-swift.el --- Swift -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit-lsp")))

(use-package swift-mode
  :ensure t
  :hook (swift-mode . lsp))
;;   :hook (swift-mode . (lambda () (lsp))))

(provide 'lang-swift)
;;; lang-swift.el ends here
