;;; init-n-bridge.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

;;; Code:

(require 'package)

(when (>= emacs-major-version 27)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  )
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(defvar gpe/packages '(
                       treemacs
                       magit
                       vertico
                       which-key
                       centaur-tabs
                       markdown-mode
                       yasnippet
                       exec-path-from-shell
                       doom-themes
                       doom-modeline
                       embark-consult
                       projectile
                       format-all
                       flymake
                       flyspell
                       iedit
                       flycheck
                       nerd-icons

                       )  "Default packages." )

(setq package-selected-packages gpe/packages)

(defun gpe/packages-installed-p ()
  "Looping all the packages."
  (cl-loop for pkg in gpe/packages
	       when (not (package-installed-p pkg)) do (cl-return nil)
	       finally (cl-return t)))

(unless (gpe/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg gpe/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
;;--------------------------------------------------------------------
;; use-package with straight
;;--------------------------------------------------------------------
;; (setq package-check-signature t
;;        load-prefer-newer t)
;;
;; ;; Install straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;;--------------------------------------------------------------------
;; Setup 'use-package'
(when (version< emacs-version "29.0")
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; ;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (setq use-package-compute-statistics t))

;; Required by `use-package', as use-package optional dependency
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update
  :ensure t)

;; interactive macro expansion
(use-package macrostep
  :custom-face
  (macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

(use-package things-engine
  :ensure nil
  :init
  (message "Something Good as Indicated"))

;;--------------------------------------------------------------------
;;  (straight-use-package 'use-package
;;  (use-package straight
;;   :custom (straight-use-package-by-default t))
;;   :bind  (("C-<f2>" . hydra-straight-helper/body)))
;;-------------------------------------------------------------------------------------------------
(provide 'init-n-bridge)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-n-bridge.el ends here
