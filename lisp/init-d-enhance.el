


;; Awesomshell
;;--------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "site-lisp/aweshell" user-emacs-directory))
(require 'aweshell)

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

;; osx-lib
;; manually install the package osx-lib
;; M-x package-list-packages [enter]
;; osx-lib [install]

;; company word
;;(add-to-list 'load-path (expand-file-name "site-lisp/word-completion" user-emacs-directory))
(load "~/.emacs.d/site-lisp/word-completion/company-words.el")

(add-hook 'prog-mode-hook (lambda() (setq split-width-threshold 80)))

;; Mouse wheel scroll behavior
(setq
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      )

(setq delete-by-moving-to-trash t)  ;; disable delete directly

;;--------------------------------------------------------------------
(provide 'init-d-enhance)
