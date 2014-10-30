;;; esk/notmuch.el --- Notmuch customization
;;

(when (fboundp 'notmuch)
  (eval-when-compile (require 'notmuch))

  (defun esk-notmuch-show-add-deleted-tag ()
    "Tag current message as deleted."
    (interactive)
    (notmuch-show-tag-message "+deleted"))

  (defun esk-notmuch-search-add-deleted-tag ()
    "Tag current message(s) as deleted."
    (interactive)
    (let* ((region (notmuch-search-interactive-region))
           (beg (first region)) (end (second region)))
      (notmuch-search-add-tag '("+deleted") beg end)))

  (defun esk-notmuch-switch-to-hello-buffer ()
    "Switch to the main notmuch buffer."
    (interactive)
    (switch-to-buffer "*notmuch-hello*"))

  (eval-after-load 'notmuch
    '(progn
       (define-key notmuch-show-mode-map "d" 'esk-notmuch-show-add-deleted-tag)
       (define-key notmuch-search-mode-map "d" 'esk-notmuch-search-add-deleted-tag)
       (define-key notmuch-common-keymap "g" 'notmuch-refresh-this-buffer)

       (define-key esk-menu-map (kbd "m") (cons "Mail" 'esk-notmuch-switch-to-hello-buffer))

       (require 'notmuch-address)
       (setq notmuch-address-command "/usr/local/bin/notmuch-addrlookup")
       (notmuch-address-message-insinuate))))