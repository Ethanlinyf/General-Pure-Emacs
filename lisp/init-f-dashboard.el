;;; init-TE-dashboard.el --- General Pure Emacs Dashboard configurations.	-*- lexical-binding: t no-byte-compile: t -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; Configurations for dashboard.
;;--------------------------------------------------------------------
;;; Code:

(require 'init-const)
(require 'init-custom)

(require 'dashboard)

(dashboard-setup-startup-hook)
;; Or if you use use-package
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

;; Set the title
(setq dashboard-banner-logo-title "Something Good as Indicated by ThingsEngine")

;;(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-action-dired)

;; set the number of items for each heading
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        ;;(agenda . 5)
                        ;;(registers . 5)
                        ))

;; set the headings's icon
(setq dashboard-set-heading-icons t)
(setq dashboard-heading-icons '((recents   . "file-text")
                                (bookmarks . "bookmark")
                                (agenda    . "calendar")
                                (projects  . "briefcase")
                                (registers . "database")))

(custom-set-faces '(dashboard-heading ((t (:inherit (font-lock-string-face bold))))))
;(use-package dashboard
;  :diminish dashboard-mode
;  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold))))))

;; Content is not centered by default. To center the content, set this varable as t
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)


(setq dashboard-set-file-icons t)

(page-break-lines-mode 1)


(setq dashboard-page-separator "\n\f\n")

(setq dashboard-set-navigator t)
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Homepage"
         "Browser Homepage"
         (lambda (&rest _) (browse-url "Https://thethingsengine.org")))
         (,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
          "Linkedin"
          ""
          (lambda (&rest _) (browse-url "homepage")))
         ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
         ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error)
        ("?" "" "?/h" #'show-help nil "<" ">"))
         ;; line 2
        ;; ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
          ;; "Linkedin"
          ;; ""
          ;; (lambda (&rest _) (browse-url "homepage")))
        ;; ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))
        ))

(setq thingsengine-icon t)   
(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and thingsengine-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))


(setq dashboard-set-footer t)
(setq dashboard-footer (format "Powered by ThingsEngine, %s" (format-time-string "%Y")))
;; (setq dashboard-footer-messages '("Powered by ThingsEngine"))
(setq dashboard-footer-icon (cond ((icons-displayable-p)
                             (all-the-icons-faicon "heart"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'error))
                            ((char-displayable-p ?🧡) "🧡 ")
                            (t (propertize ">" 'face 'dashboard-footer))))

;; FIXME: Insert copyright
;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
(defun my-dashboard-insert-copyright ()
  "Insert copyright in the footer."
  (when dashboard-footer
    (insert "\n  ")
    (dashboard-center-line dashboard-footer)
    (insert (propertize dashboard-footer 'face 'font-lock-comment-face))
    (insert "\n")))
(advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

(counsel-projectile-mode t)
;; (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-action-dired)

;; (when (dashboard-mode)
;;   (awesome-tab-mode nil))

(add-hook 'dashboard-mode-hook #'(lambda() (awesome-tab-mode -1)))

;; recentf excludes files: M-x recentf-cleanup
;; (add-to-list 'recentf-exclude
;;              (expand-file-name "~/.emacs.d/company-statistics-cache.el"))

;;--------------------------------------------------------------------
(provide 'init-f-dashboard)
;;; init-dashboard.el ends here
