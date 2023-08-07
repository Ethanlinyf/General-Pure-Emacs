;;; init-b-basic.el -- Better default configurations. -*- lexical-binding: t; -*-
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

(use-package display-line-numbers
  :ensure nil
  :init (setq display-line-numbers-width-start t)
  :hook ((prog-mode) . display-line-numbers-mode)) ; yaml-mode text-mode org-mode conf-mode

;;--------------------------------------------------------------------
(with-no-warnings
  (line-number-mode 1)            ; Turn on line number and the column-number-mode
  (column-number-mode 1)          ; Change the cursor type
  (global-hl-line-mode 1)         ; Enable hightline globally
  (setq-default cursor-type 'bar) ; Change the type of cursor
  )

;;--------------------------------------------------------------------
;; Key Modifiers
(with-no-warnings
  (cond
   (sys/mac-port-p
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ;;([(super w)] . delete-frame)
               ([(super z)] . undo)))
   (sys/win32p
    ;; set Win key as Super and App key as Hyper
    (setq w32-lwindow-modifier 'super)  ; Left Windows key as Super
    (setq w32-apps-modifier 'hyper)     ; Menu/App key as Hyper
    (w32-register-hot-key [s-t]))))

;;--------------------------------------------------------------------
;; manage by git and disable make-backup-files and auto-save-default
(with-no-warnings
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (delete-selection-mode 1)
  (setq tab-width 4)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)                 ; avoid to mixture the tabs and spaces in code
  (global-set-key (kbd "C-c C-'") 'set-mark-command)  ; keybindings for setting mark
  )

;; ;; Visualize TAB, (HARD) SPACE, NEWLINE
;; (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
;; (defun enable-trailing-whitespace ()
;;   "Show trailing spaces and delete on saving."
;;   (setq show-trailing-whitespace t)
;;   (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
;; (add-hook 'prog-mode #'enable-trailing-whitespace)

(setq visible-bell t
      inhibit-compacting-font-caches t                                           ; Don’t compact font caches during GC
      delete-by-moving-to-trash t                                                ; Deleting files go to OS's trash folder
      make-backup-files nil                                                      ; Forbide to make backup files
      auto-save-default nil                                                      ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets                    ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)


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
  :hook
  (org-mode . (lambda () (org-superstar-mode 1) (org-indent-mode 1))))

;;------------------------ User Interface ----------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package hide-mode-line
  :ensure t)
;;--------------------------------------------------------------------
(use-package auto-save
  :ensure nil
  :load-path "site-lisp/auto-save"
  :hook (find-file-hook . auto-save-enable)
  :config
  (auto-save-idle 1)
  (auto-save-silent t)
  (auto-save-delete-trailing-whitespace t))

;;--------------------------------------------------------------------
;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  (after-init . yas-global-mode)
  :init
  (setq yas-verbosity 0) ; 1 or higher to show Yasnippet messages again
  :config
  (yas-reload-all)
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;--------------------------------------------------------------------
(use-package so-long
  :ensure t
  :hook (after-init . global-so-long-mode))

;;--------------------------------------------------------------------
(use-package projectile
  :ensure t
  :bind (("C-S-c p" . projectile-command-map)  ; The binding should be verified.
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))


;; This package will be used in minibuffer.el, dired.el, platerform.el,
;; interface.el and dashboard.el
(use-package nerd-icons
  :demand t
  :init
  (setq nerd-icons-color-icons t)
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;;--------------------------------------------------------------------
;; hydra
(use-package hydra
  :ensure t
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :init
  (require 'hydra))

;; From Centaur Emacs
(use-package pretty-hydra
  :ensure
  :after hydra
  ;; :bind ("<f8>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (require 'pretty-hydra)
  :config
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icon-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(use-package use-package-hydra
  :ensure t
  :after use-package hydra
  :init
  (require 'use-package-hydra))

;;-------------------------------------------------------------------------------------------------
(provide 'init-b-basic)
;;; init-b-basic.el ends here.
