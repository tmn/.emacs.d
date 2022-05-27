;;; osx.el --- macOS related configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; macOS related configurations

;;; Code:

(setq
 ns-use-mwheel-momentum t
 ns-use-mwheel-acceleration t

 ns-function-modifier 'hyper
 mac-command-modifier 'meta
 mac-option-modifier 'super

 trash-directory "~/.Trash/emacs")

(setq dired-use-ls-dired t
      insert-directory-program "/opt/homebrew/bin/gls"
      dired-listing-switches "-aBhl --group-directories-first")

;; Map æøå keys to macOS using non-Norwegian keymap
(define-key key-translation-map (kbd "s-a") "å")
(define-key key-translation-map (kbd "s-'") "æ")
(define-key key-translation-map (kbd "s-o") "ø")
(define-key key-translation-map (kbd "s-A") "Å")
(define-key key-translation-map (kbd "s-\"") "Æ")
(define-key key-translation-map (kbd "s-O") "Ø")

(define-key (current-global-map) (kbd "s-p") nil)
(define-key (current-global-map) (kbd "s-o") nil)
(define-key (current-global-map) (kbd "s-t") nil)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;;; osx.el ends here
