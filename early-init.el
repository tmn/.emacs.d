;;; early-init.el --- Initial config load -*- lexical-binding: t -*-
;;; Commentary:
;; Early initial file.

;;; Code:

;; Reduce GC frequency
(let ((normal-gc-cons-threshold (* 64 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024))
      (normal-read-process-output-max (* 1024 1024)))
  (setq read-process-output-max normal-read-process-output-max)
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)
              (message "*** Emacs loaded in %s with %d garbage collections."
                       (format "%.2f sseconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done))))


;; Silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;; -----------------------------------------------------------------------------
;; package.el
;; -----------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)


(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; -----------------------------------------------------------------------------
;; use-package
;; -----------------------------------------------------------------------------

(require 'use-package)
(setq use-package-always-ensure t)


;; -----------------------------------------------------------------------------
;; vc-use-package
;; -----------------------------------------------------------------------------

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(require 'vc-use-package)


;;; early-init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url
		     "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; -----------------------------------------------------------------------------
;; Some basic configs
;; -----------------------------------------------------------------------------

(setq-default
 auto-save-default nil
 create-lockfiles nil
 indent-tabs-mode nil
 tab-width 2
 make-backup-files nil
 tab-always-indent 'complete)

(column-number-mode t)
(global-hl-line-mode 0)
(global-auto-revert-mode 1)

(save-place-mode 1)
(setq use-dialog-box nil)

(defun t/my-prog-mode ()
  "Default values for my prog mode."
  (display-line-numbers-mode 0)
  (set-fringe-mode 15))

(add-hook 'prog-mode-hook 't/my-prog-mode)

(add-to-list 'exec-path "/home/tnguye/.nvm/versions/node/v22.14.0/bin/")


;; -----------------------------------------------------------------------------
;; Custom file
;; -----------------------------------------------------------------------------

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;; -----------------------------------------------------------------------------
;; Confirm exit emacs
;; -----------------------------------------------------------------------------

(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Dumme ape! Quitting already??? "))
          'append)
