;;; init.el --- Load the configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This init.el wraps the tmn.el configuration and supports loading
;; different user Emacs directories by exporting USER_EMACS_DIRECTORY
;; to another directory than the default .emacs.d.

;;; Code:

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

      (let ((t/lib-file (concat user-emacs-directory "tmn.el")))
        (unless (file-exists-p t/lib-file)
          (error "No file found: %s" t/lib-file))

        (load t/lib-file nil 'nomessage 'nosuffix))))))

;;; init.el ends here
