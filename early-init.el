;;; early-init.el --- Initial config load -*- lexical-binding: t -*-

;;; Commentary:
;; Early initial file.

;;; Code:

;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
    (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
(with-current-buffer
    (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)

(setq use-package-always-ensure t
      straight-use-package-by-default t
      use-package-always-defer t)

(let ((my-gnutls-min-prime-bits 4096))
  (setq gnutls-min-prime-bits my-gnutls-min-prime-bits))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024))
      (normal-read-process-output-max (* 1024 1024)))
  (setq read-process-output-max normal-read-process-output-max)
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; early-init.el ends here
