;;; init-package.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; Emacs pluggins 
;;--------------------------------------------------------------------
;;; Code:

(require 'cl)
(require 'package)

(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa.org/packages/") t)
  )

(package-initialize)

;;add whatever packages you want here stable.stable.
(defvar puremacs/packages '(
				 company
				 swiper
				 counsel
				 smartparens
				 exec-path-from-shell
				 nord-theme
                                 doom-modeline
                                 doom-themes
                                 popwin
                                 org-superstar
                                 dashboard
                                 page-break-lines
                                 projectile
                                 counsel-projectile
                                 all-the-icons
                                 magit
                                 window-numbering
                                 lsp-mode
                                 flycheck
                                 which-key
                                 yasnippet
                                 treemacs
                                 highlight-indent-guides
                                 multi-term
                                 ido
                                 ivy-rich
                                 all-the-icons-ivy-rich
                                 amx
                                 ;;--LaTeX--
                                 auctex
                                 reftex
                                 cdlatex
                                 auto-complete
                                 all-the-icons-dired
                                 diredfl
				 ;diredful
                                 maxframe
                                 ns-auto-titlebar ;; Set the MacOS transparent titlebar to match theme
                                 pdf-tools
                                 company-auctex
                                 latex-preview-pane ;;latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs. It supports PDF previews, your choice of pdflatex or xelatex, and it highlights errors in your LaTeX buffer.
                                 pangu-spacing ; Minor-mode to add space between Chinese and English characters.
                                 all-the-icons-ibuffer ; Display icons for all buffers in ibuffer.
                                 all-the-icons-ivy-rich ;
                                 good-scroll ; This package implements smooth scrolling by pixel lines. It attempts to improve upon `pixel-scroll-mode' by adding variable speed.
                                 elisp-format
                                 zotxt ; for reference
                                 org-roam
                                 org-download
                                 page-break-lines
                                 format-all
                                 htmlize ; from org-site
                                 ;;ox-twbs
                                 hungry-delete
                                 rainbow-delimiters
                                 ;;treemacs-all-the-icons
                                 corfu
                                 orderless
                                 posframe
                                 markdown-mode
                                 )  "Default packages")

(setq package-selected-packages puremacs/packages)

(defun puremacs/packages-installed-p ()
  (cl-loop for pkg in puremacs/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (puremacs/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg puremacs/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;------------------------------------------------------------------------------
;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)
;;------------------------------------------------------------------------------

(load-theme 'nord nil)

(when (display-graphic-p)
  (require 'all-the-icons))

;; (global-company-mode 1)

;;------------------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 64)

;;------------------------------------------------------------------------------
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

;;------------------------------------------------------------------------------
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;------------------------------------------------------------------------------
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-c p f") 'counsel-git)
(global-set-key (kbd "C-c a") 'org-agenda)

;;------------------------------------------------------------------------------
;;(add-hook 'after-init-hook #'doom-modeline-mode)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-unicode-fallback t)
(setq doom-modeline-mu4e nil)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)


;;(load-theme 'doom-one-light t)
(load-theme 'doom-one t)

;;------------------------------------------------------------------------------
(require 'popwin)
(popwin-mode 1)

;;------------------------------------------------------------------------------


(global-set-key "\C-x\g" 'magit-status)

;; ALT + number to swith to the number of a specific window
(require 'window-numbering)
(add-hook 'after-init-hook #'window-numbering-mode)

;; add which key
(require 'which-key)
(which-key-mode 1)

(require 'all-the-icons)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(require 'multi-term)
(setq multi-term-program "/bin/bash")

;;(add-hook 'after-init-hook 'ido-mode)
(ido-mode 1)
(ivy-rich-mode 1)
(all-the-icons-ivy-rich-mode 1)

;; The icon size
(setq all-the-icons-ivy-rich-icon-size 1.0)

;; Definitions for ivy-rich transformers.
;; See `ivy-rich-display-transformers-list' for details."
all-the-icons-ivy-rich-display-transformers-list

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)
(setq ivy-use-virtual-buffers t)

(add-hook 'after-init-hook 'amx-mode)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(with-eval-after-load "dired"
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file)
  )

;(require 'diredful)
;(diredful-mode 1)

(setq all-the-icons-color-icons t)
(setq all-the-icons-ibuffer-color-icon t)
(setq all-the-icons-dired-monochrome nil) ;; nil means it is colourful in dired-mode
;; model line
;;(require 'doom-modeline)
  ;;(doom-modeline-minor-modes t)
;;  (doom-modeline-unicode-fallback t)

;; emacs awesome shell from https://github.com/manateelazycat/aweshell
;; in the "site-lisp"
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/aweshell-master"))
(require 'aweshell)

    (use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))

;;--------------------------------------------------------------------

;; Pangu-spacing
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
;(setq pangu-spacing-real-insert-separtor t)

;;------------------------------------------------------------------------------
;; all-the-icons-ibuffer

(all-the-icons-ibuffer-mode 1)

;; Whether display the icons.
(setq all-the-icons-ibuffer-icon t)

;; Whether display the colorful icons.
;; It respects `all-the-icons-color-icons'.
(setq all-the-icons-ibuffer-color-icon t)

;; The default icon size in ibuffer.
(setq all-the-icons-ibuffer-icon-size 1.0)

;; The default vertical adjustment of the icon in ibuffer.
(setq all-the-icons-ibuffer-icon-v-adjust 0.0)

;; Use human readable file size in ibuffer.
(setq  all-the-icons-ibuffer-human-readable-size t)

;; A list of ways to display buffer lines with `all-the-icons'.
;; See `ibuffer-formats' for details.
all-the-icons-ibuffer-formats

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)

;;------------------------------------------------------------------------------
;; all-the-icons-ivy-rich

;; Whether display the icons
(setq all-the-icons-ivy-rich-icon t)

;; Whether display the colorful icons.
;; It respects `all-the-icons-color-icons'.
(setq all-the-icons-ivy-rich-color-icon t)

;; The icon size
(setq all-the-icons-ivy-rich-icon-size 1.0)

;; Whether support project root
(setq all-the-icons-ivy-rich-project t)

;; Definitions for ivy-rich transformers.
;; See `ivy-rich-display-transformers-list' for details."
all-the-icons-ivy-rich-display-transformers-list

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)

(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)

;;------------------------------------------------------------------------------
;; good-scrolling, This package implements smooth scrolling by pixel lines. It attempts to improve upon `pixel-scroll-mode' by adding variable speed.

(good-scroll-mode 1)
;;--------------------------------------------------------------------
;; Elisp-format

(add-hook 'emacs-lisp-mode 'elisp-format)
;; (require 'elisp-format)

;;--------------------------------------------------------------------
;; it is from org-site to simplify the org-site 
(require 'htmlize)

(setq htmlize-output-type 'font)

;;--------------------------------------------------------------------
;; Hunbgry-delete
(require 'hungry-delete)
;(hungry-delete-mode 1)
(global-hungry-delete-mode 1)

(setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
;;--------------------------------------------------------------------
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;--------------------------------------------------------------------
(counsel-projectile-mode 1)

(when (display-graphic-p)
  (require 'all-the-icons))

;;--------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/awesome-tab"))


(require 'awesome-tab)

(add-hook 'prog-mode-hook #'awesome-tab-mode)

(add-hook 'eshell-mode-hook #'(lambda() (awesome-tab-mode -1)))
(add-hook 'Info-mode-hook (lambda() (awesome-tab-mode -1)))


;;--------------------------------------------------------------------
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;;--------------------------------------------------------------------------------
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

;;--------------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/site-lisp/corfu-english-helper/")
(require 'corfu-english-helper)


;;------------------------------------------------------------------------------
(provide 'init-package)
;;; init-package.el ends here
