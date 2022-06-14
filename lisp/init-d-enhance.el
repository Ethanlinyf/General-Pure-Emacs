


;; Awesomshell
;;--------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "site-lisp/aweshell" user-emacs-directory))
(require 'aweshell)

(good-scroll-mode 1)


;; ALT + number to swith to the number of a specific window
(require 'window-numbering)
(add-hook 'after-init-hook #'window-numbering-mode)

(all-the-icons-completion-mode t)


(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

;; add which key
(require 'which-key)
(which-key-mode 1)


(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; An intuitive and efficient solution for single-buffer text search
(ctrlf-mode +1)


;;--------------------------------------------------------------------
(provide 'init-d-enhance)
