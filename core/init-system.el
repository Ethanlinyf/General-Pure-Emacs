;;; init-system.el --- GPE essential settings -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; Some essential settings for General Pure Emacs

;;; code:
(require 'gpe-const)
(require 'gpe-custom)
(require 'gpe-macro)
(require 'gpe-function)



(setq system-time-locale "C"
        display-time-24hr-format t
        display-time t ;display-time-day-and-date t
        )

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Disable the ring bell function
  (setq ring-bell-function 'ignore)


;;-------------------------------------------------------------------------------------------------
(provide 'init-system)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-system.el ends here.
