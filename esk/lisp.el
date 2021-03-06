;;; esk/lisp.el --- Some helpful Lisp code
;;

(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "C-c v") #'eval-buffer)

(defface esk/paren-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'esk/faces)

;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'esk/remove-elc-on-save)

(defun esk/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "M-.") #'find-function-at-point)

;;; Enhance Lisp Modes

(dolist (x '(emacs-lisp lisp))
  (when window-system
    (font-lock-add-keywords
     (intern (concat (symbol-name x) "-mode"))
     '(("(\\|)" . 'esk/paren-face))))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) #'esk/run-coding-hook))
