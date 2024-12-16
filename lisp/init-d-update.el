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
       (setq ns-use-native-fullscreen nil)
       ))

(when sys/macp
  (select-frame-set-input-focus (selected-frame)))

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
  ;; (setq visible-bell t)
  (setq ring-bell-function 'ignore)
  (global-set-key (kbd "<f8>") 'scratch-buffer))

;; (set-frame-parameter nil 'alpha-background 80)
;; (push '(alpha . (90 . 90)) default-frame-alist)
;; (set-frame-parameter nil 'alpha 0.99)

;; Bugfix
;;--------------------------------------------------------------------
;;; doom-one theme:
;; Warning: setting attribute ‘:background’ of face ‘font-lock-comment-face’:
;; nil value is invalid, use ‘unspecified’ instead.

;; fixed:
;; (modeline-fg fg) --> (modeline-fg 'unspecified)

;; :background (if doom-one-brighter-comments (doom-lighten bg 0.05)) -->
;; :background (if doom-one-brighter-comments
;;                 (doom-lighten bg 0.05)
;;               'unspecified)

;;----------------------------------------------------------------------------------------------
;; include package-vc
;; (require 'cl-lib)
;; (require 'use-package-core)

;; (cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
;;   (let* ((url (format "https://www.%s.com/%s" fetcher repo))
;;          (iname (when name (intern name)))
;;          (package-name (or iname (intern (file-name-base repo)))))
;;     (unless (package-installed-p package-name)
;;       (package-vc-install url iname rev backend))))

;; (defvar package-vc-use-package-keyword :vc)

;; (defun package-vc-use-package-set-keyword ()
;;   (unless (member package-vc-use-package-keyword use-package-keywords)
;;     (setq use-package-keywords
;;           (let* ((pos (cl-position :unless use-package-keywords))
;;                  (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
;;                  (tail (nthcdr (+ 1 pos) use-package-keywords)))
;;             (append head (list package-vc-use-package-keyword) tail)))))

;; (defun use-package-normalize/:vc (name-symbol keyword args)
;;   (let ((arg (car args)))
;;     (pcase arg
;;       ((or `nil `t) (list name-symbol))
;;       ((pred symbolp) args)
;;       ((pred listp) (cond
;;                      ((listp (car arg)) arg)
;;                      ((string-match "^:" (symbol-name (car arg))) (cons name-symbol arg))
;;                      ((symbolp (car arg)) args)))
;;       (_ nil))))

;; (defun use-package-handler/:vc (name-symbol keyword args rest state)
;;   (let ((body (use-package-process-keywords name-symbol rest state)))
;;     ;; This happens at macro expansion time, not when the expanded code is
;;     ;; compiled or evaluated.
;;     (if args
;;         (use-package-concat
;;          `((unless (package-installed-p ',(pcase (car args)
;;                                             ((pred symbolp) (car args))
;;                                             ((pred listp) (car (car args)))))
;;              (apply #'slot/vc-install ',(cdr args))))
;;          body)
;;       body)))

;; (defun package-vc-use-package-override-:ensure (func name-symbol keyword ensure rest state)
;;   (let ((ensure (if (plist-member rest :vc)
;;                     nil
;;                   ensure)))
;;     (funcall func name-symbol keyword ensure rest state)))

;; (defun package-vc-use-package-activate-advice ()
;;   (advice-add
;;    'use-package-handler/:ensure
;;    :around
;;    #'package-vc-use-package-override-:ensure))

;; (defun package-vc-use-package-deactivate-advice ()
;;   (advice-remove
;;    'use-package-handler/:ensure
;;    #'package-vc-use-package-override-:ensure))

;; ;; register keyword on require
;; (package-vc-use-package-set-keyword)

;; (use-package gpe-aweshell
;;   :vc (:fetcher "github" :repo "Ethanlinyf/gpe-aweshell"))

;;-------------------------------------------------------------------------------------------------
(provide 'init-d-update)
;;; init-d-update.el ends here
