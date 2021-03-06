;;; azazel/erc.el --- Azazel's ERC specialization

(require 'netrc)

(eval-when-compile
  (require 'erc)
  (require 'erc-join))

(defun esk/erc-auto-login-with-netrc (server nick)
  "Extract username and password from ~/.netrc to authenticate on freenode.net"

  (message "Authenticating %s on IRC server %s..." nick server)
  (let ((host (netrc-machine (netrc-parse "~/.authinfo") "freenode.net" t)))
    (if host
        (let ((password (netrc-get host "password"))
              (username (netrc-get host "login")))
          (when (string= username nick)
            (setq erc-prompt-for-password nil)
            (erc-message "PRIVMSG"
                         (concat "NickServ identify " password))))
      (message "... credentials not found in ~/.authinfo!"))))

(defun esk/start-erc-session ()
  "Start an ERC session on freenode.net"

  ;; Load credentials from ~/.netrc if present
  ;(add-hook 'erc-after-connect 'esk/erc-auto-login-with-netrc)

  (setq erc-autojoin-channels-alist
        '(("freenode.net"
           "#rafanass"
           "#linuxtrent"
           )))
  (setq erc-email-userid user-mail-address)
  (setq erc-nick '("azazel" "azazell"))

  (erc-autojoin-mode 1)

  (message "Connecting to ZNC on daneel.arstecnica.it...")
  (let ((host (netrc-machine (netrc-parse "~/.authinfo") "daneel.arstecnica.it" "7777")))
    (if host
        (let ((password (netrc-get host "password")))
          (setq erc-email-userid (netrc-get host "login"))
          (erc-open "daneel.arstecnica.it"
                    7777
                    "azazel"
                    (user-full-name)
                    t
                    password))
      (message "... credentials not found in ~/.authinfo!"))))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
