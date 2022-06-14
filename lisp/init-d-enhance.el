


;; Awesomshell
;;--------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "site-lisp/aweshell" user-emacs-directory))
(require 'aweshell)

(good-scroll-mode 1)


;; ALT + number to swith to the number of a specific window
(require 'window-numbering)
(add-hook 'after-init-hook #'window-numbering-mode)

;;--------------------------------------------------------------------
(provide 'init-d-enhance)
