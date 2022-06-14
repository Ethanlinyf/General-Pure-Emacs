;; init-basic.el -- Better default configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;; Commentary:
;; Add feature defined in the lisp folder
;;--------------------------------------------------------------------
;;; Code:

(require 'init-const)
(require 'init-custom)

;; Turn off the startup help screen
(setq inhibit-splash-screen 1)

;; Turn off the "tool-bar-mode" minor mode
(tool-bar-mode -1)

;; Turn off the scroll bar mode
(scroll-bar-mode -1)

;; keep the menu bar active
(menu-bar-mode 1)

;; Turn on line number and the column-number-mode
(global-linum-mode 1)
(column-number-mode 1)

;; Change the cursor type
(setq-default cursor-type 'bar)

;; Enable hightline globally
(global-hl-line-mode 1)

;; Disable the ring bell function
(setq ring-bell-function 'ignore)

;; set the system local for time
(setq system-time-local "C")

;; Set the initial scratch message
(setq-default
 initial-scratch-message (concat ";;--------------------------------------------------------------------\n;; Welcome to Pure Emacs for the ThingsEngine\n;; Somethng Good as Indicated:\n\n\n")
 line-spacing 0.1
 truncate-lines t
 word-wrap t)

;; manage by git and disable make-backup-files and auto-save-default
(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;;See matching pairs of parentheses and other characters.
(show-paren-mode t)
(setq show-paren-delay 0)
;; example for a specific mode to turn on this "show-paren-mode":
;;(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'after-init-hook 'show-paren-mode)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
            (ignore-errors (backward-up-list))
            (funcall fn)))))

;; Add file changed from outside
(global-auto-revert-mode 1)

;; Display time in the mini buffer
(display-time)
(defvar display-time-24hr-format t)

;; default character
(progn
  "Set coding system."
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8))

;; Keybindings for setting mark
(global-set-key (kbd "C-c C-'") 'set-mark-command)

;;(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'recentf-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; (add-hook 'after-init-hook 'electric-indent-mode')

(defun open-mirror-file ()
  "Quickly open index file."
  (interactive)
  (find-file "~/Documents/Org/mirror.org"))
(global-set-key (kbd "<f1>") 'open-mirror-file)


(defun open-gtd-file ()
  "Quickly open index file."
  (interactive)
  (find-file "~/Documents/Org/gtd.org"))
(global-set-key (kbd "<f2>") 'open-gtd-file)

(defun open-note-file ()
  "Quickly open index file."
  (interactive)
  (find-file "~/Documents/Org/note.org"))
(global-set-key (kbd "<f3>") 'open-note-file)

(defun open-meeting-file ()
  "Quickly open index file."
  (interactive)
  (find-file "~/Documents/Org/meeting.org"))
(global-set-key (kbd "<f4>") 'open-meeting-file)

(defun open-journal-file ()
  "Quickly open index file."
  (interactive)
  (find-file "~/Documents/Org/journal.org"))
(global-set-key (kbd "<f5>") 'open-journal-file)

(defun open-plan-file ()
  "Quickly open index file."
  (interactive)
  (find-file "~/Documents/Org/plan.org"))
(global-set-key (kbd "<f6>") 'open-plan-file)

;; kill processes when quit or exit
(setq confirm-kill-processes nil)

;; Basic Improvement -------------------------------------------------
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(setq hippie-expand-try-function-list '(try-expand-debbrev
 					try-expand-debbrev-all-buffers
 					try-expand-debbrev-from-kill
 					try-complete-file-name-partially
 					try-complete-file-name
 					try-expand-all-abbrevs
 					try-expand-list
 					try-expand-line
 					try-complete-lisp-symbol-partially
 					try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)

;; Load Dired Mode
;; (require 'dired)
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; loading later
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(require 'dired-x)
;; (setq dired-recursive-deletes 'always)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)
(put 'dired-find-alternate-file 'disabled nil)

;;--------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 30)

;; The following could be implemented by counsel
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;;--------------------------------------------------------------------
(provide 'init-b-basic)
