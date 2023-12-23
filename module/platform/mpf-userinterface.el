;;; mpf-userinterface.el --- Elisp Template . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; configurations for user interface.

;;; Code:

;; set a title for the active frame
(setq frame-title-format
      '("GPE-Basic"  ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; (toggle-frame-fullscreen)

(auto-image-file-mode 1)

;; Frame
;;; Emacs@28
;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa)
  (bind-key "S-s-<return>" #'toggle-frame-fullscreen)
  (and sys/mac-x-p (bind-key "C-s-f" #'toggle-frame-fullscreen))

  ;; Resize and re-position frames conveniently, on macOS: use Magnet to achieve it
  ;; (bind-keys ("C-M-<return>"    . frame-maximize)
  ;;            ("C-M-<backspace>" . frame-restore)
  ;;            ("C-M-<left>"      . frame-left-half)
  ;;            ("C-M-<right>"     . frame-right-half)
  ;;            ("C-M-<up>"        . frame-top-half)
  ;;            ("C-M-<down>"      . frame-bottom-half)))
  )

;;--------------------------------------------------------------------
;; ;; theme doom-one
;; ;; see the PR: https://github.com/doomemacs/themes/pull/779
;; (use-package doom-themes
;;   :custom
;;   (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
;;   (doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   :config
;;   ;; Enable flashing mode-line on errors
;;   ;; (doom-themes-visual-bell-config)
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;;   :init
;;   (load-theme 'doom-nord-aurora t))

;; (setq my$day-themes
;;       '(doom-solarized-light
;;         spacemacs-light
;;         doom-one-light
;;         ef-tritanopia-light
;;         ef-cyprus
;;         ef-light)
;;       my$night-themes
;;       '(spacemacs-dark
;;         doom-one
;;         doom-nord-aurora
;;         doom-opera))

;; (when (display-graphic-p)
;;     (push 'modus-vivendi my$night-themes)
;;     (push 'modus-operandi my$day-themes)
;;     (push 'ef-dark my$night-themes))

;;(require 'spacemacs-theme)

;; ;; (load-theme 'spacemacs-light t)

;; (defun switch-theme-based-on-macos ()
;;   (let ((theme (shell-command-to-string "osascript ~/.emacs.d/core/site-lisp/detect_theme.scpt")))
;;     (cond ((string-match "Dark" theme)
;;            (load-theme 'doom-nord-aurora t))
;;           ((string-match "Light" theme)
;;            (load-theme 'doom-one-light t)))))

;; ;; Call the function on Emacs startup
;; (switch-theme-based-on-macos)

;; ;; Optionally, set a timer to check periodically (e.g., every 5 minutes)
;; (run-with-timer 0 300 'switch-theme-based-on-macos)

;; auto-dark-emacs could be also considered.
(require 'doom-themes)
(require 'modus-themes)
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi-tritanopia t))
    ('dark (load-theme 'doom-nord-aurora t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package hide-mode-line
  :hook (((;; treemacs-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           embark-collect-mode
           pdf-annot-list-mode) . hide-mode-line-mode)))

(use-package buffer-move
  :ensure t
  :bind
  ("<C-S-up>" . buf-move-up)  ; (global-set-key (kbd "<C-S-up>") 'buf-move-up)
  ("<C-S-down>" . buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

;;-------------------------------------------------------------------------------------------------
(provide 'mpf-userinterface)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpf-userinterface.el ends here
