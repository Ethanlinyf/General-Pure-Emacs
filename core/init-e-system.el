;;; init-e-system.el --- GPE essential settings -*- lexical-binding: t; -*-
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

;; I/O optimisation
(with-no-warnings
  (when sys/win32p
    (setq w32-get-true-file-attributes nil   ; decrease file IO workload
          w32-pipe-read-delay 0              ; faster IPC
          w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))

  ;; Increase how much can be read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x1ee)  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject))

(when sys/macp
  (setq ns-use-native-fullscreen t))

;; UTF-8 support for the system
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless sys/win32p
  (set-selection-coding-system 'utf-8))

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

(add-hook 'after-init-hook #'server-mode)

;; kill processes when quit or exit, live-webserver
(setq confirm-kill-processes nil)

;;-------------------------------------------------------------------------------------------------
(provide 'init-e-system)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-e-system.el ends here.
