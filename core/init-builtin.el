;;; init-builtin.el --- Basic GPE settings -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

;;; code:
(setq-default
 initial-scratch-message (concat
                          ";;--------------------------------------------------------
;; Welcome to G.P.E Basic Platform
;; Somethng Good as Indicated:\n\n\n")
 line-spacing 0.1
 truncate-lines nil
 word-wrap t)

;; Disable the ring bell function
(setq ring-bell-function 'ignore)

(setq-default make-backup-files nil)
(setq-default require-final-newline t)
(setq-default scroll-conservatively 1000)
(setq-default read-process-output-max (* 4 1024 1024))
(setq-default show-trailing-whitespace t)
(setq-default use-short-answers t)
(setq-default abbrev-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-c C-'") 'set-mark-command)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; auto save to the visited file (provided by `files.el')
(add-hook 'after-init-hook 'auto-save-visited-mode)

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; Enable to delete the selection
(add-hook 'after-init-hook #'delete-selection-mode)

;; Electric-Pair
(add-hook 'after-init-hook #'electric-indent-mode)

;; Recent Files
(add-hook 'after-init-hook (lambda ()
			                 (recentf-mode 1)
			                 (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/"))))
(setq-default recentf-max-menu-items 20
	          recentf-max-saved-items 20)
(global-set-key (kbd "C-x C-r") #'recentf-open-files)

;; Save Place
(add-hook 'after-init-hook 'save-place-mode)

;; Highlight Current Line
(use-package hl-line
  :when (display-graphic-p)
  :hook (prog-mode . hl-line-mode))

(defun pulse-save-buffers (&rest args)
  (save-some-buffers t)
  (pulse-momentary-highlight-one-line (point)))
;; auto save when frame lose focus, Alt-Tab
(add-function :after after-focus-change-function #'pulse-save-buffers)
;; auto save when buffer changed
(dolist (command '(other-window
                   switch-to-buffer
                   next-buffer
                   previous-buffer))
  (advice-add command :after #'pulse-save-buffers))

(use-package which-key :ensure t :defer t
  :hook (after-init . which-key-mode))

;; exec-path-from-shell
;; (when (or sys/macp sys/linuxp (daemonp))
;;   (require 'exec-path-from-shell)
;;   (setq exec-path-from-shell-check-startup-files nil)
;;   (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

;;-------------------------------------------------------------------------------------------------
(provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here.
