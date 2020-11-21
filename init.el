;;; init.el --- Load the configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This init.el wraps the tmn.el configuration and supports loading
;; different user Emacs directories by exporting USER_EMACS_DIRECTORY
;; to another directory than the default .emacs.d.

;;; Code:

(let ((alternative-user-emacs-directory (getenv "USER_EMACS_DIRECTORY")))
  (defvar tmn/config-file-loaded-p nil)

  (cond
   ((and (not after-init-time) tmn/config-file-loaded-p))

   (alternative-user-emacs-directory
    (setq alternative-user-emacs-directory
          (file-name-as-directory alternative-user-emacs-directory))
    (setq user-emacs-directory alternative-user-emacs-directory)
    (setq user-init-file (expand-file-name "init.el" user-emacs-directory))
    (load user-init-file 'noerror 'nomessage))

   (t
    (setq tmn/config-file-loaded-p t)

    (defvar tmn/minimum-emacs-version "27.1"
      "The tmn.el configuration does not support any Emacs version below this.")

    (if (version< emacs-version tmn/minimum-emacs-version)
        (error (concat "Your Emacs is too old -- "
                       "this config requires v%s or higher")
               tmn/minimum-emacs-version)

      (let ((tmn/lib-file (concat user-emacs-directory "tmn.el")))
        (unless (file-exists-p tmn/lib-file)
          (error "No file found: %s" tmn/lib-file))

        (load tmn/lib-file nil 'nomessage 'nosuffix))))))

;;; init.el ends here
