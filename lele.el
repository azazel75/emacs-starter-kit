;-*- coding: utf-8 -*-
;:Progetto:  dot.emacs -- Lele's personal preferences
;:Creato:    ven 09 apr 2010 01:08:16 CEST
;:Autore:    Lele Gaifax <lele@metapensiero.it>
;:Licenza:   GNU General Public License version 3 or later
;

(setq user-mail-address "lele@metapensiero.it")

;; Claws-mail

(setq auto-mode-alist
      (cons '("/tmpmsg\\." . text-mode)
            auto-mode-alist))
(setq magic-mode-alist nil)


;; ERC

(defun erc-auto-login-with-netrc (server nick)
  (when (require 'netrc nil t)
    (let ((freenode (netrc-machine (netrc-parse "~/.netrc") "freenode.net" t)))
      (when freenode
        (let ((freenode-password (netrc-get freenode "password"))
              (freenode-username (netrc-get freenode "login")))
          (when (string= freenode-username nick)
            (setq erc-prompt-for-password nil)
            (erc-message "PRIVMSG"
                         (concat "NickServ identify " freenode-password))))))))

(defun start-erc-session ()
  "Start an ERC session on freenode.net"

  (eval-when-compile (require 'erc))

  ;; Load credentials from ~/.netrc if present
  (add-hook 'erc-after-connect 'erc-auto-login-with-netrc)

  (setq erc-autojoin-channels-alist
        '(("freenode.net"
           "#rafanass"
           "#linuxtrent"
           "#darcs"
           "#revctrl"
           "#tailor"
           "#sqlalchemy"
           "#airpim"
           )))
  (setq erc-email-userid user-mail-address)
  (setq erc-nick "lelit")

  (erc-autojoin-mode 1)

  (erc-select :server "irc.freenode.net"
              :port 6667
              :nick erc-nick
              :full-name (user-full-name))
  (recentf-mode nil))

;; GNUS

(setq gnus-home-directory (concat user-specific-dir "/gnus/"))
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

(setq gnus-default-posting-charset (quote utf-8))
(setq gnus-permanently-visible-groups ".")
(setq gnus-summary-line-format "%U%R%z%d %(%[%-20,20a%]%) %I%s\n")


;; reStructuredText

(require 'rst)

;; aggiorna automaticamente il contents
(add-hook 'rst-adjust-hook 'rst-toc-update)

;; disabilita il font-lock dei titoli, dei code-blocks... che rallenta
;; troppo!
(setq rst-mode-lazy nil)
(setq rst-directive-face 'font-lock-builtin-face)

;; Attiva i bindings standard (vedi C-c p)
;;(add-hook 'rst-mode-hook 'rst-text-mode-bindings)


;; My wmii automatically starts up "emacs -f mine-emacs"

(defun mine-emacs ()
  "Connect to IRC and activate Emacs server, but ask first."
  (interactive)
  (if (y-or-n-p "IRC? ") (start-erc-session))
  (if (y-or-n-p "Emacs server? ") (server-start))
  (set-frame-font "DejaVu Sans Mono-10" t)
  (message "Have a nice day!"))
