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

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(when *is-mac*
  (menu-bar-mode 1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when (and *is-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-s-f") 'toggle-frame-fullscreen))

(when *is-mac*
  (ns-auto-titlebar-mode))

(when *is-mac*
  (toggle-frame-fullscreen))

;;--------------------------------------------------------------------

(load-theme 'doom-one t)

(use-package all-the-icons
  :if (display-graphic-p))

;; move to basic.el
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

(setq doom-modeline-minor-modes t)

;; The following doesn't work
;; (require 'minions)
;; (add-hook 'after-doom-modeline-hook #'minions-mode)

;; Display minor-mode in the mode line
(require 'minions)
(minions-mode t)

;;--------------------------------------------------------------------

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(setq all-the-icons-color-icons t)
(setq all-the-icons-ibuffer-color-icon t)
(setq all-the-icons-dired-monochrome nil) ;; nil means it is colourful in dired-mode

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
all-the-icons-ibuffer-formats

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)

;;--------------------------------------------------------------------

(use-package all-the-icons-completion
  :ensure nil
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)))

;(all-the-icons-completion-mode)

;(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)



(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; An intuitive and efficient solution for single-buffer text search
(ctrlf-mode +1)

;; osx-lib
;; manually install the package osx-lib
;; M-x package-list-packages [enter]
;; osx-lib [install]

(use-package popwin
  :ensure nil
  :hook (after-init . popwin-mode))


;;--------------------------------------------------------------------
(require 'lin)

(setq lin-face 'lin-mac) ; check doc string for alternative styles

;; You can use this to live update the face:
;;
;; (customize-set-variable 'lin-face 'lin-green)

(setq lin-mode-hooks
      '(bongo-mode-hook
        org-mode-hook
        dired-mode-hook
        elfeed-search-mode-hook
        git-rebase-mode-hook
        grep-mode-hook
        ibuffer-mode-hook
        ilist-mode-hook
        ledger-report-mode-hook
        log-view-mode-hook
        magit-log-mode-hook
        mu4e-headers-mode-hook
        notmuch-search-mode-hook
        notmuch-tree-mode-hook
        occur-mode-hook
        org-agenda-mode-hook
        pdf-outline-buffer-mode-hook
        proced-mode-hook
        tabulated-list-mode-hook))

(lin-global-mode 1)

;;--------------------------------------------------------------------
(provide 'init-g-interface)
;;; init-g-interface.el ends here