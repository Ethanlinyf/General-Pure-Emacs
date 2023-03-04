;; init-g-interface.el -- UI configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for User Interface
;;--------------------------------------------------------------------
;;; Code:

(setq frame-title-format
      '("Puremacs"  ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; Window size and features
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (and *is-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-s-f") 'toggle-frame-fullscreen))
;;--------------------------------------------------------------------

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)

  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ns-auto-titlebar
  :if *is-mac*
  :ensure t
  :init
  (ns-auto-titlebar-mode t)) ;; not use hook for it.

;; Display minor-mode in the mode line
(use-package minions
  :ensure t
  :hook (after-init . minions-mode)
  :init
  (setq doom-modeline-minor-modes t))
;;--------------------------------------------------------------------
;; (use-package all-the-icons-ibuffer
;;   :ensure t
;;   :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :config

  ;; (setq all-the-icons-color-icons t)
  ;; (setq all-the-icons-ibuffer-color-icon t)
  ;; (setq all-the-icons-dired-monochrome nil) ;; nil means it is colourful in dired-mode

  (all-the-icons-ibuffer-mode t)

  ;; Whether display the icons.
  (setq all-the-icons-ibuffer-icon t)

  ;; Whether display the colorful icons.
  ;; It respects `all-the-icons-color-icons'.
  (setq all-the-icons-ibuffer-color-icon t)

  ;; The default icon size in ibuffer.
  (setq all-the-icons-ibuffer-icon-size 1.0)

  ;; The default vertical adjustment of the icon in ibuffer.
  (setq all-the-icons-ibuffer-icon-v-adjust 0.0)

  ;; Use human readable file size in ibuffer.
  (setq  all-the-icons-ibuffer-human-readable-size t)

  ;; A list of ways to display buffer lines with `all-the-icons'.
  ;; See `ibuffer-formats' for details.
  all-the-icons-ibuffer-formats)

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)
;;--------------------------------------------------------------------

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (foo-mode . rainbow-delimiters-mode)
  (prog-mode . rainbow-delimiters-mode))

;; osx-lib
(use-package osx-lib
  :if *is-mac*)

(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode))
;;--------------------------------------------------------------------

(auto-image-file-mode 1)
(face-remap-add-relative 'font-lock-keyword-face '(:inherit default))
(face-remap-add-relative 'font-lock-keyword-face `(:foreground ,(face-foreground 'default)))
;;--------------------------------------------------------------------

(use-package buffer-move
  :ensure t
  :bind
  ("<C-S-up>" . buf-move-up)  ;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  ("<C-S-down>" . buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

;; (use-package good-scroll
;;   :ensure t
;;   :if window-system
;;   :init (good-scroll-mode))

;;--------------------------------------------------------------------
(provide 'init-g-interface)
;;; init-g-interface.el ends here
