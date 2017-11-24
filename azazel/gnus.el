;;-*- coding: utf-8 -*-
;;:Progetto: dot.emacs -- Azazel's Gnus configuration
;;:Creato:   ven 24 nov 2017 16:41:07 CET
;;:Autore:   Alberto Berti <alberto@metapensiero.it>
;;:Licenza:  GNU General Public License version 3 or later
;;

(eval-when-compile
  (require 'bbdb)
  (require 'bbdb-gnus)
  (require 'gnus)
  (require 'gnus-start)
  (require 'gnus-art)
  (require 'gnus-msg)
  (require 'mm-decode))

(csetq gnus-registry-install t)

;; BBDB
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(require 'smtpmail)
(require 'nnir)

;; per il Big Brother DB
(require 'bbdb)
(require 'bbdb-gnus)
(bbdb-initialize 'gnus 'message 'sc)

(csetq gnus-read-newsrc-file nil)
(csetq gnus-save-newsrc-file nil)
(csetq sc-auto-fill-region-p nil)

(csetq gnus-permanently-visible-groups ".")
(csetq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
(csetq gnus-summary-same-subject "")

(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook  'turn-on-auto-fill)

;; prefer plain text alternative
(setq mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")
      mm-automatic-display (remove "text/html" mm-automatic-display)
      gnus-buttonized-mime-types '("multipart/alternative" "multipart/signed"))

(csetq gnus-select-method '(nnimap "arstecnica"
                                  (nnimap-address "mail.arstecnica.it")
                                  (nnimap-expunge-on-close "always")
                                  (nnir-search-engine imap)
                                  (nnimap-logout-timeout 2.0)
                             ))
(csetq imap-log t)

(csetq gnus-secondary-select-methods '((nntp "gmane"
                                            (nntp-address "news.gmane.org")
                                            (nnir-search-engine gmane))
                                      ))

; per l'uso con supercite
(add-hook 'mail-citation-hook 'sc-cite-original)
(csetq gnus-signature-separator
          "^\\(--  \\|^-- *\\|________\\========*\\)$")
(add-hook 'sc-pre-hook
          '(lambda ()
             (save-excursion
               (let ((start (point))
                     (end (mark t)))
                 (goto-char end)
                 (when (re-search-backward gnus-signature-separator start t)
                   (forward-line -1)
                   (while (looking-at "[ \t]*$")
                     (forward-line -1))
                   (forward-line 1)
                   (setq mark (set-marker (make-marker) (point)))
                   (delete-region mark (mark t)))))))


;; This prevents GNUS from inserting its default attribution header
(csetq news-reply-header-hook nil)

;; Identità (posting styles)
(csetq gnus-posting-styles
       '((".*"
         (address "alberto@metapensiero.it")
         (name "Alberto Berti"))
         ("linuxtrent"
         (address "azazel@metapensiero.it")
         (name "azazel"))
         ;;      Rispondi con lo stesso indirizzo
         ("nnml:.*"
          (From (save-excursion
                  (set-buffer gnus-article-buffer)
                  (message-fetch-field "to"))))
         ))

;; Archive - putting sent mail and news somewhere

(csetq gnus-outgoing-message-group "nnimap+arstecnica:Sent")

(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)


;; charset di default
(csetq gnus-default-posting-charset "utf-8")
;; abilita il supporto gnus per dired

(require 'gnus-dired)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(setq esk/gnus-user-groups '("gmane.comp.python.general"
                             "gmane.comp.python.devel"
                             "gmane.comp.python.pypy"))
