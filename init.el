;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq esk-dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name))
      esk-autoload-file (concat esk-dotfiles-dir "loaddefs.el")
      package-user-dir (concat esk-dotfiles-dir "elpa")
      esk-custom-file (concat esk-dotfiles-dir "custom")
      esk-system-specific-config (concat esk-dotfiles-dir system-name)
      esk-user-specific-config (concat esk-dotfiles-dir user-login-name)
      esk-user-specific-dir (concat esk-dotfiles-dir user-login-name)
      esk-overrides-dir (concat esk-dotfiles-dir "overrides"))

;; Load up ELPA, the package manager

(add-to-list 'load-path esk-dotfiles-dir)

(add-to-list 'load-path (concat esk-dotfiles-dir "elpa-to-submit"))
(add-to-list 'load-path (concat esk-dotfiles-dir "elpa-to-submit/auto-complete"))
(add-to-list 'load-path (concat esk-dotfiles-dir "elpa-to-submit/emacs-dbgr"))
(add-to-list 'load-path (concat esk-dotfiles-dir "elpa-to-submit/yasnippet"))

(require 'package)
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'open-next-line)
(require 'tramp)
(require 'autopair)
(require 'yasnippet)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
;;(require 'starter-kit-perl)
;;(require 'starter-kit-ruby)
(require 'starter-kit-js)
(require 'starter-kit-python)
(require 'starter-kit-completion)
(require 'starter-kit-darcs)
(require 'starter-kit-git)
(require 'starter-kit-erc)
(require 'starter-kit-skeletons)

(regen-autoloads)

;; You can keep system- or user-specific customizations here
(add-to-list 'load-path esk-user-specific-dir)

;; Load custom.el
(load esk-custom-file 'noerror)

;; Load "system-name".el
(load esk-system-specific-config 'noerror)

;; Load "user-name".el
(load esk-user-specific-config 'noerror)

;; Overrides for possibly old bundled versions
(if (file-exists-p esk-overrides-dir)
    (add-to-list 'load-path esk-overrides-dir))

;; Automatically load all "user-name"/*.el files
(if (file-exists-p esk-user-specific-dir)
    (mapc #'load (directory-files esk-user-specific-dir nil ".*el$")))

;; The following is used by the Custom facility, and usually you don't
;; want to commit changes made here: a better way it to manually move
;; the settings to the pertinent username.el file and commit that
;; instead.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(egg-enable-tooltip t)
 '(longlines-show-hard-newlines t)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
