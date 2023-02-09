;;; init-research.el --- Setup for research. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for research
;;--------------------------------------------------------------------
;;; Code:
;; (when (>= emacs-version 27.2)
;;   (use-package obsidian
;;     :ensure t
;;     :config
;;     (obsidian-specify-path "~/Documents/Research/Obsidian-Re/")
;;     (global-obsidian-mode t)
;;     :custom
;;     ;; This directory will be used for `obsidian-capture' if set.
;;     (obsidian-inbox-directory "Inbox")
;;     :bind (:map obsidian-mode-map
;;                 ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
;;                 ("C-c C-o" . obsidian-follow-link-at-point)
;;                 ;; Jump to backlinks
;;                 ("C-c C-b" . obsidian-backlink-jump)
;;                 ;; If you prefer you can use `obsidian-insert-link'
;;                 ("C-c C-l" . obsidian-insert-wikilink))))
;;--------------------------------------------------------------------
(use-package zotxt
  :ensure t
  :hook
  (org-mode . org-zotxt-mode))

;;--------------------------------------------------------------------
;; Emacs for Overleaf
(use-package atomic-chrome
  :ensure t
  :config ;; :init
  (atomic-chrome-start-server))

(use-package ess
  :ensure t)

;;--------------------------------------------------------------------
(provide 'init-r-research)
;;; init-r-research.el ends here
