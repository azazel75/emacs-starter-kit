;-*- coding: utf-8 -*-
;:Progetto:  dot.emacs -- Alberto's personal preferences
;:Creato:    ven 24 nov 2017 14:02:55 CET
;:Autore:    Alberto Berti <alberto@metapensiero.it>
;:Licenza:   GNU General Public License version 3 or later
;

(eval-when-compile
  (require 'thingatpt)
)

(csetq message-send-mail-function (quote message-smtpmail-send-it))
(csetq smtpmail-default-smtp-server "mail.arstecnica.it")
(csetq smtpmail-smtp-server "mail.arstecnica.it")
(csetq smtpmail-smtp-user "azazel@arstecnica.it")
(csetq user-mail-address "alberto@metapensiero.it")
(csetq inhibit-startup-screen t)
(csetq show-paren-mode t)

(esk/load "jabber")

(set-frame-font "PT Mono-12" t)

;; Enable some "dangerous" functionalities

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(eval-when-compile (require 'rst))

(eval-after-load 'rst
  '(progn
     ;; automatically update contents summary
     (add-hook 'rst-adjust-hook #'rst-toc-update)

     ;; disable new auto indent
     (add-hook 'rst-mode-hook (lambda () (electric-indent-local-mode -1)))))


;; projectile

(eval-after-load 'projectile
  '(progn
     ;; These are common associations in PatchDB context
     (add-to-list 'projectile-other-file-alist '("sql" "rst" "py"))
     (add-to-list 'projectile-other-file-alist '("rst" "sql" "py"))
     ;; And this is for javascripthon
     (add-to-list 'projectile-other-file-alist '("pj" "js" "py"))))


;; google-translate

(require 'google-translate)
(csetq google-translate-default-source-language "en")
(csetq google-translate-default-target-language "it")
(global-set-key "\C-ct." #'google-translate-at-point)
(global-set-key "\C-ctt" #'google-translate-query-translate)
(global-set-key "\C-ctr." #'google-translate-at-point-reverse)
(global-set-key "\C-ctrt" #'google-translate-query-translate-reverse)


(defun mine-emacs (&optional dont-ask)
  "Connect to IRC, GNUS, Jabber and activate Emacs server, but ask first.
If DONT-ASK is non-nil, interactively when invoked with a prefix arg,
start everything unconditionally."
  (interactive "P")


  (if (or dont-ask (y-or-n-p "Emacs server? ")) (server-start))
  (if (or dont-ask (y-or-n-p "GNUS? ")) (gnus))
  (if (or dont-ask (y-or-n-p "IRC? ")) (esk/start-erc-session))
  (if (or dont-ask (y-or-n-p "Jabber? ")) (jabber-connect-all))

  (message "Have a nice day!"))


;; My wmii automatically starts up "emacs -f mine-emacs-!"
;; My i3 automatically does
;;  exec --no-startup-id i3-msg 'workspace 4; exec emacs -f mine-emacs-!; workspace 1'

(defun mine-emacs-! ()
  "Unconditionally start my emacs setup."
  (mine-emacs t))


;; org mode


(require 'org)
(setq org-log-done 'time)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files  '("~/wip/todos"))

(defun azazel-org-mode-keys-hook ()
  (local-set-key [C-f1] 'org-insert-link)
 )

(add-hook 'org-mode-hook 'azazel-org-mode-keys-hook)

;; Customize will write the settings here

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-engine-detection t)
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
        ("jinja"    . "\\.jinja\\'"))
)
(setq web-mode-markup-indent-offset 2)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (emmet-mode)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)


(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; For elpy
(csetq elpy-rpc-python-command "python3")
;; For interactive shell
(csetq python-shell-interpreter "python3")
(package-initialize)
(elpy-enable)

(require 'org)
(defun custom-org-mode-pdf ()
  (add-to-list 'org-latex-classes
               '("djcb-org-article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{hyperref}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Gentium}
\\setromanfont [BoldFont={Gentium Basic Bold},
                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
\\setsansfont{Droid Sans}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-default-class "djcb-org-article")

  (setq org-latex-to-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))) ;; for multiple passes


;; (require 'ob-screen)    ;; requires screen, terminal
;; (setq org-babel-default-header-args:screen
;;       '((:results . "silent") (:session . "default") (:cmd . "bash") (:terminal . "sakura")))
;; (require 'ob-ditaa)
;; (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

;; (require 'pymacs)
;; (setq pymacs-python-command "/usr/bin/python3")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(muse-project-alist nil)
 '(package-selected-packages
   (quote
    (ace-jump-mode
     dired-filter
     google-translate
     muse
     nginx-mode
     ranger
     realgud
     swiper
     treemacs
     typescript-mode
     rainbow-mode
     pony-mode
     mustache-mode
     mo-git-blame
     jinja2-mode
     jabber
     handlebars-mode
     flymake-less
     flymake-cursor
     elpy
     dockerfile-mode
     bbdb
     auto-complete
     ansible
     typescript
     w3m
     zeal-at-point
     zenburn-theme
     yasnippet
     yaml-mode
     whitespace-cleanup-mode
     wgrep
     web-mode
     vc-darcs
     smartparens
     py-isort
     projectile
     magit
     json-mode
     js2-mode
     iedit
     flx-ido
     expand-region
     erc-hl-nicks))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
