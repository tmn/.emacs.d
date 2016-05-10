;;; lang-json.el --- JSON support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(use-package json-reformat
  :commands json-reformat
  :init (setq json-reformat:indent-width 2))

(provide 'lang-json)
;;; lang-json.el ends here
