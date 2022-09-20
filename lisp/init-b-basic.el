;; init-b-basic.el -- Better default configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Add feature defined in the Lisp folder
;;--------------------------------------------------------------------
;;; Code:

(require 'pure-const)
(require 'pure-custom)
(require 'pure-function)

;; set the startup default directory
(setq default-directory "~/")

;; Turn off the startup help screen
(setq inhibit-splash-screen 1)

;; Turn off the "tool-bar-mode" minor mode
(tool-bar-mode -1)

;; Turn off the scroll bar mode
(scroll-bar-mode -1)

;; Keep the menu bar active
(menu-bar-mode 1)

;;(setq initial-frame-alist (quote ((fullscreen . maximized))))
(when *is-mac*
  (toggle-frame-fullscreen))


;; Turn on line number and the column-number-mode
(global-linum-mode 1)
(column-number-mode 1)

;; Change the cursor type
;; (setq-default cursor-type 'bar)

;; Enable hightline globally
(global-hl-line-mode 1)

;; Disable the ring bell function
(setq ring-bell-function 'ignore)

;; Set the system local for time
(setq system-time-local "C")

;; Set the initial scratch message
(setq-default
 initial-scratch-message (concat ";;--------------------------------------------------------------------\n;; Welcome to General Pure Emacs for ThingsEngine\n;; Somethng Good as Indicated:\n\n\n")
 line-spacing 0.1
 truncate-lines nil
 word-wrap t)

;; manage by git and disable make-backup-files and auto-save-default
(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

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

;; kill processes when quit or exit, live-webserver
(setq confirm-kill-processes nil)

;; ------------------ Indent Region or Buffer ------------------------
(defun indent-buffer()
  "To indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  "To indent the region or buffer."
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

;;--------------------- Dired Mode ----------------------------------- 

(with-eval-after-load "dired"
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file))

(when *is-mac*
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first"))


;;-----------------For org mode -------------------------------------
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)
                                    (org-indent-mode 1)))


;;------------------------ User Interface ----------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


;;--------------------------------------------------------------------
(provide 'init-b-basic)
;;; init-b-basic.el ends here
