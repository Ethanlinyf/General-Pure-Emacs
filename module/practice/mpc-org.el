;;; mpc-org.el --- Elisp Template . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;;

;;; Code:

(require 'org)

;; (setq org-directory "~/Documents/Org")

(defvar org-directory (expand-file-name "GPE-Org/" user-emacs-directory))
(unless (file-exists-p org-directory)
  (make-directory org-directory))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(use-package org-superstar
  :ensure t
  :hook
  (org-mode . (lambda () (org-superstar-mode 1) (org-indent-mode 1))))

(add-hook 'org-mode-hook #'org-indent-mode)

(use-package tablist
  :ensure t)

(use-package org-noter
  :ensure t)

(use-package org-noter-pdftools
  :ensure t)

;; (setq org-src-fontify-natively t)

(setq org-babel-load-languages
      '((js . t)
        (java . t)
        (python . t)
        (sqlite . t)
        (emacs-lisp . t)
        (shell . t)
        (ditaa . t)))
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
;; (setq-default fill-column 70)
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

;;--------------------------------------------------------------------
;; pandoc
;; (use-package ox-pandoc
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'ox
;;     (require 'ox-pandoc)))

;; (setq org-ellipsis 0xE2)

(use-package typo
  :diminish
  :hook
  ((org-mode markdown-mode gnus-message-setup) . typo-mode)
  :config
  (typo-global-mode 1))
;;-------------------------------------------------------------------------------------------------
;; (use-package org-modern
;;   :ensure t
;;   :after (org)
;;   :init
;;   (setq org-modern-table-vertical 2)
;;   (setq org-modern-block-name t)
;;   (setq org-modern-keyword t)
;;   (setq org-modern-timestamp t)
;;   :hook (org-mode . global-org-modern-mode))

;; (defun my-iconify-org-buffer ()
;;   (progn
;;     (push '(":PROPERTIES:" . ?􀈭) prettify-symbols-alist)
;;     (push '(":ID:      " . ?􀐚) prettify-symbols-alist)
;;     (push '(":ROAM_ALIASES:" . ?􀅷) prettify-symbols-alist)
;;     (push '(":END:" . ?􀅽) prettify-symbols-alist)
;;     (push '("#+TITLE:" . ?􀧵) prettify-symbols-alist)
;;     (push '("#+AUTHOR:" . ?􀉩) prettify-symbols-alist)
;;     (push '("#+RESULTS:" . ?􀎚) prettify-symbols-alist)
;;     (push '("#+ATTR_ORG:" . ?􀌞) prettify-symbols-alist)
;;     (push '("#+STARTUP: " . ?􀖆) prettify-symbols-alist))
;;   (prettify-symbols-mode 1))
;; (add-hook 'org-mode-hook #'my-iconify-org-buffer)

;; (setq org-ellipsis " 􀍠")
;; (setq org-hide-emphasis-markers t)
(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode))


(define-key org-mode-map (kbd "<C-tab>") 'org-global-cycle)

;; (global-set-key (kbd "<C-tab>") 'org-global-cycle)
;;--------------------------------------------------------------------
(use-package org-present
  :config
  (defun gpe/org-present-prepare-slide (buffer-name heading)
    (org-overview)  ; Show only top-level headlines
    (org-show-entry); Unfold the current entry
    (org-show-children))   ; show recent sub-heading

  (defun gpe/org-present-start () ; pre-present settings
    ;; (turn-off-evil-mode)
    (setq visual-fill-column-width 110
          visual-fill-column-center-text t)
    ;; 调整字体大小
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq header-line-format " ") ; add a line infron of the heading
    (display-line-numbers-mode 0)
    (org-display-inline-images) ; display image
    (read-only-mode 1)) ; read only

  (defun gpe/org-present-end () ; reset back the previous setting
    (setq-local face-remapping-alist
                '((default variable-pitch default)))
    (setq header-line-format nil)
    (org-remove-inline-images)
    (org-present-small)
    (read-only-mode 0)
    (display-line-numbers-mode 1))


  (add-hook 'org-present-mode-hook 'gpe/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'gpe/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'gpe/org-present-prepare-slide))

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"));; need to be checked
;;-------------------------------------------------------------------------------------------------
(provide 'mpc-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpc-org.el ends here.
