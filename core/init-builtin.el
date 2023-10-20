;;; init-builtin.el --- Basic GPE settings -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

;;; code:

(toggle-frame-fullscreen)

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

(with-no-warnings
  (setq make-backup-files nil)
  ;; (setq auto-save-default nil)
  (delete-selection-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-set-key (kbd "C-c C-'") 'set-mark-command)
  )

;; Enable abbrev
(setq-default abbrev-mode t)

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

(add-hook 'after-init #'global-auto-revert-mode)

;; Delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; Enable to delete the selection
(add-hook 'after-init-hook #'delete-selection-mode)

;; Electric-Pair
(add-hook 'after-init-hook #'electric-indent-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'electric-layout-mode)

;; Flymake
(add-hook 'prog-mode-hook #'flymake-mode)

;; Recent Files
(add-hook 'after-init-hook (lambda ()
			     (recentf-mode 1)
			     (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/"))))
(setq-default recentf-max-menu-items 20
	          recentf-max-saved-items 20)

;; Save Place
(save-place-mode 1)

;; only use spaces instead of TAB, use C-q TAB to input the TAB char
(setq-default indent-tabs-mode nil)  ; avoid to mixture the tabs and spaces in code
(setq-default tab-width 4)

(global-set-key (kbd "C-x C-r") #'recentf-open-files)

(message "init-builtin")

;;-------------------------------------------------------------------------------------------------
(provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here.
