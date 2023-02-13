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
";;--------------------------------------------------------
;; Welcome to General Pure Emacs for ThingsEngine
;; Somethng Good as Indicated:\n\n\n")
 line-spacing 0.1
 truncate-lines nil
 word-wrap t)

;; (add-hook 'text-mode-hook #'linum-mode)
;; (add-hook 'prog-mode-hook #'linum-mode)
;; (add-hook 'org-mode-hook #'linum-mode)
(use-package display-line-numbers
  :ensure nil
  :init (setq display-line-numbers-width-start t)
  :hook ((prog-mode yaml-mode text-mode org-mode conf-mode) . display-line-numbers-mode))

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
(with-no-warnings
  (line-number-mode 1)    ;; Turn on line number and the column-number-mode
  (column-number-mode 1)  ;; Change the cursor type
  (global-hl-line-mode 1) ;; Enable hightline globally
  ;; (setq-default cursor-type 'bar) ;; Change the type of cursor
  )

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
(with-no-warnings
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (delete-selection-mode 1)
  (setq tab-width 4)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (global-set-key (kbd "C-c C-'") 'set-mark-command)  ;; Keybindings for setting mark
  )

;; Visualize TAB, (HARD) SPACE, NEWLINE
(setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
(defun enable-trailing-whitespace ()
  "Show trailing spaces and delete on saving."
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))


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
  :ensure t
  :hook (after-init . global-so-long-mode))

;;--------------------------------------------------------------------
(use-package projectile
  :ensure t
  :bind (("C-S-c p" . projectile-command-map)) ; The binding should be verified.
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

;; (use-package counsel-projectile
;;   :ensure t
;;   :after (projectile)
;;   :init (counsel-projectile-mode))

(use-package consult-projectile
  :ensure t
  :after (projectile)
  :init
  (setq projectile-switch-project-action 'projectile-dired) ;; open directory in dired-mode from dashboard
  )

;; This package will be used in minibuffer.el, dired.el, platerform.el,
;; interface.el and dashboard.el
(use-package all-the-icons
  :ensure t
  :init
  (setq all-the-icons-color-icons t)
  ;; :config
  (defun icon-displayable-p ()
    "Return non-nil if the icons are displayable."
    (and (display-graphic-p)
         (require 'all-the-icons nil t))))

(use-package hydra
  :ensure t
  :init
  (require 'hydra))


;; From Centaur Emacs
(use-package pretty-hydra
  :ensure t
  ;; :bind ("<f7>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (require 'pretty-hydra)
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icon-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(use-package use-package-hydra
  :ensure t
  :after hydra
  :init
  (require 'use-package-hydra))

;;--------------------------------------------------------------------
(provide 'init-b-basic)
;;; init-b-basic.el ends here.
