;;; lang-java.el --- Java support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-java
  :ensure t
  :init
  (setq
   lsp-java-vmargs (list
                    "-noverify"
                    "-Xmx2G"
                    "-XX:+UseG1GC"
                    "-XX:+UseStringDeduplication"
                    "-javaagent:/Users/tmn/.emacs.d/lib/lombok.jar")

   lsp-java-java-path "/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home/bin/java"
   lsp-java-workspace-dir "/Users/tmn/.emacs.d/.lsp-java/workspace"
   lsp-java-workspace-cache-dir "/Users/tmn/.emacs.d/.lsp-java/cache"
   lsp-java-server-install-dir "/Users/tmn/.emacs.d/.lsp-java/server")
  :config
  (progn
    (require 'lsp-java-boot)
    (add-hook 'lsp-mode-hook 'lsp-lens-mode)
    (add-hook 'java-mode-hook 'lsp-java-boot-lens-mode))
  (add-hook 'java-mode-hook 'lsp))

(provide 'lang-java)
;;; lang-java.el ends here
