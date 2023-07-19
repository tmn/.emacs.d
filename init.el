;;; init.el --- Load the configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This init.el wraps the tmn.el configuration and supports loading
;; different user Emacs directories by exporting USER_EMACS_DIRECTORY
;; to another directory than the default .emacs.d.

;;; Code:

;; -----------------------------------------------------------------------------
;; Default coding system
;; -----------------------------------------------------------------------------
(set-default-coding-systems 'utf-8)
(server-start)

(let ((alternative-user-emacs-directory (getenv "USER_EMACS_DIRECTORY")))
  (defvar t/config-file-loaded-p nil)

  (cond
   ((and (not after-init-time) t/config-file-loaded-p))

   (alternative-user-emacs-directory
    (setq alternative-user-emacs-directory
          (file-name-as-directory alternative-user-emacs-directory))
    (setq user-emacs-directory alternative-user-emacs-directory)
    (setq user-init-file (expand-file-name "init.el" user-emacs-directory))
    (load user-init-file 'noerror 'nomessage))

   (t
    (setq t/config-file-loaded-p t)

    (defvar t/minimum-emacs-version "28.2"
      "The tmn.el configuration does not support any Emacs version below this.")

    (if (version< emacs-version t/minimum-emacs-version)
        (error (concat "Your Emacs is too old -- "
                       "this config requires v%s or higher")
               t/minimum-emacs-version)

      ;; -----------------------------------------------------------------------------
      ;; Load OS specific settings
      ;; -----------------------------------------------------------------------------
      (when (eq system-type 'darwin)
        (let ((t/lib-file (concat user-emacs-directory "osx.el")))
          (unless (file-exists-p t/lib-file)
            (error "No file found: %s" t/lib-file))
          (load (expand-file-name "osx.el" user-emacs-directory) nil 'nomessage 'nosuffix)))

      (when (eq system-type 'windows-nt)
        (let ((t/lib-file (concat user-emacs-directory "osx.el")))
          (unless (file-exists-p t/lib-file)
            (error "No file found: %s" t/lib-file))
          (load (expand-file-name "windows.el" user-emacs-directory) nil 'nomessage 'nosuffix)))

      (when (eq system-type 'gnu/linux)
        (let ((t/lib-file (concat user-emacs-directory "osx.el")))
          (unless (file-exists-p t/lib-file)
            (error "No file found: %s" t/lib-file))
          (load (expand-file-name "linux.el" user-emacs-directory) nil 'nomessage 'nosuffix)))

      ;; (when (eq system-type 'windows-nt)
      ;;   (load (expand-file-name "windows.el" user-emacs-directory) nil 'nomessage 'nosuffix))

      ;; (when (eq system-type 'gnu/linux)
      ;;   (load (expand-file-name "linux.el" user-emacs-directory) nil 'nomessage 'nosuffix))

      ;; -----------------------------------------------------------------------------
      ;; Load THE config
      ;; -----------------------------------------------------------------------------
      (let ((t/lib-file (concat user-emacs-directory "tmn.el")))
        (unless (file-exists-p t/lib-file)
          (error "No file found: %s" t/lib-file))

        (load t/lib-file nil 'nomessage 'nosuffix))))))

;;; init.el ends here
