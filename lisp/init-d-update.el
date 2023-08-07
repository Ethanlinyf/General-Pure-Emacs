;;; init-d-update.el --- Foundamental settings for Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; This will be used for the Emacs update from V29.1 for GPE
;;--------------------------------------------------------------------
;;; Code:

;;; Emacs@28
;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

;;; Emacs@29
;;--------------------------------------------------------------------
;; (when emacs/>=29p
;;   (push '(sh-mode . bash-ts-mode) major-mode-remap-alist)
;;   (push '(c-mode . c-ts-mode) major-mode-remap-alist)
;;   (push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
;;   (push '(css-mode . css-ts-mode) major-mode-remap-alist)
;;   (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
;;   (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
;;   (push '(python-mode . python-ts-mode) major-mode-remap-alist))
;;--------------------------------------------------------------------
;; Change the font size globally
;; To increase the font size, type "C-x C-M-= or +"; to decrease it
;; type "C-x C-M--"; to restore the size, type "C-x C-M-0"
;;--------------------------------------------------------------------
;; "delet-process" is a command. "restart-emacs"
;; "C-x 8 e" inserts Emoji, 🐋
;; "package-upgrade" "package-upgrade-all"
;; "package-recompile" & "package-recompike-all"
;;--------------------------------------------------------------------
(when emacs/>=29p
  (setq pixel-scroll-precision-mode 1)
  (global-set-key (kbd "<f8>") 'scratch-buffer))


;; Bugfix
;;--------------------------------------------------------------------
;;; doom-one theme:
;; Warning: setting attribute ‘:background’ of face ‘font-lock-comment-face’: nil value is invalid, use ‘unspecified’ instead.

;; fixed:
;; (modeline-fg fg) --> (modeline-fg 'unspecified)

;; :background (if doom-one-brighter-comments (doom-lighten bg 0.05)) -->
;; :background (if doom-one-brighter-comments
;;                 (doom-lighten bg 0.05)
;;               'unspecified)



;;-------------------------------------------------------------------------------------------------
(provide 'init-d-update)
;;; init-d-update.el ends here
