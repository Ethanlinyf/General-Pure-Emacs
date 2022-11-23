;;; init-system.el --- Foundamental settings for Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Fundamental settings for General Pure Emacs
;;--------------------------------------------------------------------
;;; Code:

(require 'pure-const)
(require 'pure-custom)
(require 'pure-function)
(require 'subr-x)

;; use for the installation for the needed software for the host system
(use-package use-package-ensure-system-package
  :ensure t)

;; set time
(use-package time
  :ensure nil
  :config
  (setq system-time-locale "C"
        display-time-24hr-format t
        ;display-time-day-and-date t
        display-time t
        ))

;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(unless sys/win32p
  (set-selection-coding-system 'utf-8))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p (daemonp)) ;; (when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :init (exec-path-from-shell-initialize)))

;;--------------------------------------------------------------------
;; A few more useful configurations...

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

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
  (setq ring-bell-function 'ignore))

;;--------------------------------------------------------------------
;; Optimisation
(with-no-warnings
  (when sys/win32p
    (setq w32-get-true-file-attributes nil   ; decrease file IO workload
          w32-pipe-read-delay 0              ; faster IPC
          w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))

  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000)  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject))

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; Compatibility
;; To allow for the usage of Emacs functions and macros that are
;; defined in newer versions of Emacs, compat.el provides definitions
;; that are installed ONLY if necessary.  These reimplementations
;;  of functions and macros are at least subsets of the actual
;; implementations.  Be sure to read the documentation string to
;;  make sure.
(use-package compat)
;;--------------------------------------------------------------------
;; Start server
(use-package server
  :ensure nil
  :if nil ;; to be integrated with thethingsengine.org
  :hook (after-init . server-mode))
;; kill processes when quit or exit, live-webserver
(setq confirm-kill-processes nil)

;;--------------------------------------------------------------------
(provide 'init-1-system)
;;; init-1-system.el ends here.
