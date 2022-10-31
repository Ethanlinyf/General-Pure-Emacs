;;; init-orgs.el --- Org settings. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for org-mode
;;--------------------------------------------------------------------
;;; Code:

(setq org-directory "~/Documents/Org")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;(require 'org-bullets)
;;(add-hook 'org-mode-hook #'org-bullets-mode)
;; (require 'org-superstar)
;; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)
;;                                     (org-indent-mode 1))
                                    ;;(auto-save-and-publish-file-mode -1)
;; )

(setq org-pretty-entities t
      org-src-fontify-natively t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


(setq org-default-notes-file (concat org-directory "/note.org"))

(add-hook 'org-mode-hook #'auto-fill-mode)
;(setq-default fill-column 70)
(setq visual-line-mode t)

(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "WAIT(w@)" "NEXT(n!)" "CALENDAR(c@)" "|" "DONE(D!/!)")
;; 	(sequence "SOMEDAY(s@)" "REFER(r@)"  "|" "TRASH(T)")
;; 	(sequence "PROJECT(p@)" "|" "DONE(D!/!)" "CANCELLED(C@/!)")
;; 	(sequence "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(F@/!)")
;;         (sequence "ANCHOR(a)" "|" "DONE(D)")))

;; (setq org-todo-keyword-faces
;;       '(("TODO" . "red")
;; 	("WAIT" . "green")
;; 	("NEXT" . "green")
;; 	("DONE" . "grey")
;; 	("SOMEDAY" . "yellow")
;; 	("REFER" . (:background "gold" :foreground "white" :weight bold))
;; 	("PROJECT" . (:background "blue" :foreground "white" :weight bold))
;; 	("TRASH" . "grey")
;; 	("CANCELLED" . "lightblue")
;; 	("BUG" . "red")
;; 	("KNOWNCAUSE" . "yellow")
;; 	("FIXED" . "grey")))

(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)


(use-package pangu-spacing
  :ensure t
  :hook
  (org-mode . pangu-spacing-mode)
  (org-mode . (lambda ()
            (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))

;; Pangu-spacing support: real insert separator
 ;; (add-hook 'org-mode-hook
 ;;           (lambda ()
 ;;            (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

;; (add-hook 'org-mode-hook #'awesome-tab-mode)
;;(require 'org-tempo)
;;(add-to-list 'org-modules 'org-tempo t)

(use-package valign
  :ensure t
  :hook
  (org-mode . valign-mode))


;; (add-hook 'org-mode-hook #'valign-mode)


;;--------------------------------------------------------------------
;; pangu-spacing
;; (require 'pangu-spacing)
;; (global-pangu-spacing-mode 1)

;; (add-hook 'org-mode-hook #'pangu-spacing-mode)
;; (add-hook 'org-mode-hook
;;            '(lambda ()
;;             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

;; (require 'org-download)
(use-package org-download
  :ensure t
  :hook
  (dired-mode . org-download-enable))

;; Drag-and-drop to `dired`
; (add-hook 'dired-mode-hook 'org-download-enable)

(defun eli/org-noter-set-highlight (&rest _arg)
    "Highlight current org-noter note."
    (save-excursion
      (with-current-buffer (org-noter--session-notes-buffer org-noter--session)
        (remove-overlays (point-min) (point-max) 'org-noter-current-hl t)
        (goto-char (org-entry-beginning-position))
        (let* ((hl (org-element-context))
               (hl-begin (plist-get  (plist-get hl 'headline) :begin))
               (hl-end (1- (plist-get  (plist-get hl 'headline) :contents-begin)))
               (hl-ov (make-overlay hl-begin hl-end)))
          (overlay-put hl-ov 'face 'mindre-keyword)
          (overlay-put hl-ov 'org-noter-current-hl t))
        (org-cycle-hide-drawers 'all))))
    
  (advice-add #'org-noter--focus-notes-region
              :after #'eli/org-noter-set-highlight)
  (advice-add #'org-noter-insert-note
              :after #'eli/org-noter-set-highlight)
;;------------------------------------------------------------------------------
(provide 'init-i-org)
;;; init-i-org.el ends here
