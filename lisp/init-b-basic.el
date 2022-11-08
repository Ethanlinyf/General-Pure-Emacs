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

;; Set the initial scratch message
(setq-default
 initial-scratch-message (concat
";;--------------------------------------------------------------------
;; Welcome to General Pure Emacs for ThingsEngine
;; Somethng Goodas Indicated:\n\n\n")
 line-spacing 0.1
 truncate-lines nil
 word-wrap t)

;;--------------------------------------------------------------------
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

;;--------------------------------------------------------------------
;; Turn on line number and the column-number-mode
;; (global-display-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; Change the cursor type
;; (setq-default cursor-type 'bar)

;; Enable hightline globally
(global-hl-line-mode 1)

;;--------------------------------------------------------------------
;; Key Modifiers
(with-no-warnings
  (cond
   (sys/mac-port-p
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
                                        ;([(super w)] . delete-frame)
               ([(super z)] . undo)))
   (sys/win32p
    ;; make PC keyboard's Win key or other to type Super or Hyper
    ;; (setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super     ; Left Windows key
          w32-apps-modifier 'hyper)       ; Menu/App key
    (w32-register-hot-key [s-t]))
   (sys/mac-port-p
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo)))))

;;--------------------------------------------------------------------
;; manage by git and disable make-backup-files and auto-save-default
(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode 1)
(setq tab-width 4)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;; Keybindings for setting mark
(global-set-key (kbd "C-c C-'") 'set-mark-command)

;;----------------------- Dired Mode ---------------------------------
(with-eval-after-load "dired"
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file))

(when *is-mac*
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first"))

;;----------------- For org mode ------------------------------------
(use-package org-superstar
  :ensure t
  :hook (org-mode . (lambda ()
                      (org-superstar-mode 1)
                      (org-indent-mode 1))))

;;------------------------ User Interface ----------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;--------------------------------------------------------------------
;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :ensure t)

;;--------------------------------------------------------------------
 (use-package so-long
   :hook (after-init . global-so-long-mode))

;;--------------------------------------------------------------------
(provide 'init-b-basic)
;;; init-b-basic.el ends here.
