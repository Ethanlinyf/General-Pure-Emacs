;; init-basic.el -- Better default configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;; Commentary:
;; Add feature defined in the lisp folder
;;--------------------------------------------------------------------
;;; Code:

(require 'init-const)
(require 'init-custom)

;; Silence compiler warnings as they can be pretty disruptive

;; (if (and (fboundp 'native-comp-available-p)
;;        (native-comp-available-p))
;;   (message "Native compilation is available")
;; (message "Native complation is *not* available"))
;; (when emacs/>=28p
;;     (if (native-comp-available-p)
;;       (setq borg-compile-function #'native-compile)))

;; Turn off the startup help screen
(setq inhibit-splash-screen 1)

;; Turn off the "tool-bar-mode" minor mode
(tool-bar-mode -1)

;; Turn off the scroll bar mode
(scroll-bar-mode -1)

;; For mac
(menu-bar-mode 1)

;; Turn on line number and the column-number-mode
(global-linum-mode 1)
(column-number-mode 1)

;; Change the cursor type
(setq-default cursor-type 'bar)

;; Enable hightline globally
(global-hl-line-mode 1)

;; Disable the ring bell function
(setq ring-bell-function 'ignore)

;; set the system local for time
(setq system-time-local "C")

(setq-default
 initial-scratch-message (concat ";; Somethng Good as Indicated\n\n;; Welcome to Pure Emacs for the ThingsEngine\n\n")
 line-spacing 0.1
 truncate-lines t
 word-wrap t)

;; manage by git and disable make-backup-files and auto-save-default
(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

(show-paren-mode t)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; add file changed from outside
(global-auto-revert-mode 1)

(setq inhibit-compacting-font-caches t)

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
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


(setq hippie-expand-try-function-list '(try-expand-debbrev
 					try-expand-debbrev-all-buffers
 					try-expand-debbrev-from-kill
 					try-complete-file-name-partially
 					try-complete-file-name
 					try-expand-all-abbrevs
 					try-expand-list
 					try-expand-line
 					try-complete-lisp-symbol-partially
 					try-complete-lisp-symbol))

(global-set-key (kbd "M-/") 'hippie-expand)

(require 'dired-x)
;;(setq dired-recursive-deletes 'always)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)
(put 'dired-find-alternate-file 'disabled nil)

;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))





(display-time)
(defvar display-time-24hr-format t)

(progn
  "Set coding system."
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8))

;; Keybindings without extra configuration
(global-set-key (kbd "C-c C-'") 'set-mark-command)

;;(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'recentf-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; (add-hook 'after-init-hook 'electric-indent-mode')

(add-hook 'after-init-hook 'show-paren-mode)
;; @zilongshanren
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
            (ignore-errors (backward-up-list))
            (funcall fn)))))

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
;; (defun jk/org-insert-headline (level)
;;   "Insert `level' * ahead of current line."
;;   (interactive "swhich level: ")
;;   (jk/org-delete-headline)
;;   (let ((x 0) (len (string-to-number level)))
;;     (while (< x len)
;;       (if (= len (+ x 1))
;;           (insert "* ")
;;         (insert "*")
;;         )
;;     (setq x (+ x 1)))))

;; (global-set-key (kbd "C-c C-h") 'jk/org-insert-headline)

;; kill processes when quit or exit
(setq confirm-kill-processes nil)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)

    ;; (add-hook 'inferior-python-mode-hook
    ;;           #'(lambda nil
    ;;               (process-query-on-exit-flag
    ;;                (get-process "Python"))))
    ;; (eval-after-load 'python
    ;;   #'(lambda nil
    ;;       (progn
    ;;         (if
    ;;             (and
    ;;              (executable-find "python3")
    ;;              (string= python-shell-interpreter "python"))
    ;;             (progn
    ;;               (setq python-shell-interpreter "python3")))
    ;;         (eval-after-load 'exec-path-from-shell
    ;;           #'(lambda nil
    ;;               (exec-path-from-shell-copy-env "PYTHONPATH"))))))
;; (setq eaf--mac-enable-rosetta t)
;; (require 'eaf-demo)
;; (require 'eaf-file-sender)
;; (require 'eaf-music-player)
;; (require 'eaf-camera)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-image-viewer)
;; ;; (require 'eaf-vue-demo)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-browser)
;; (require 'eaf-mermaid)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-netease-cloud-music)
;;(require 'eaf-git)
;; (require 'eaf-system-monitor)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-b-basic)
