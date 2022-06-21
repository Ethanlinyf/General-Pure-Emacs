;; init-ui.el -- UI configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
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


;; no title bar (not covienient)
;; (when *is-mac*
;;   (setq default-frame-alist '((undecorated . t)))
;;   (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;;   (add-to-list 'default-frame-alist '(internal-border-width . 5)))

;; (package-install 'popwin)
(require 'popwin)
(popwin-mode 1)

;;--------------------------------------------------------------------
(provide 'init-e-ui)
;;; ends here
