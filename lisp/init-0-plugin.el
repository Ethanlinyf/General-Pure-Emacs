;;; init-0-plugin.el --- Pligins for Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Add feature defined in the Lisp folder
;;--------------------------------------------------------------------
;;; Code:

;; (require 'cl)
(require 'package)

(when (>= emacs-major-version 26)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  )

(package-initialize)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Add whatever packages you want here
(defvar puremacs/packages '(
                            ;; wishlist:
                            ; avy
                            oauth2
			    company
                            ;; company-box
			    ;; smartparens
			    exec-path-from-shell
                            doom-modeline
                            doom-themes
                            dashboard
                            page-break-lines
                            projectile
                            counsel-projectile
                            ;; 0 all-the-icons
                            all-the-icons-completion

                            undo-tree

                            ;; ;; lsp-mode
                            ;; flycheck
                            ;; 
                            treemacs
                            ;; highlight-indent-guides
                            multi-term
                            ;; ido
                            ;; ivy-rich
                            ;; all-the-icons-ivy-rich
                            ;; amx
                            all-the-icons-dired
                            diredfl
                            ;;;diredful
                            ;; maxframe
                            ns-auto-titlebar ;; Set the MacOS transparent titlebar to match theme
                            ;; 
                            ;; 
                            ;; 
                            ;; pangu-spacing ; Minor-mode to add space between Chinese and English characters.
                            all-the-icons-ibuffer ; Display icons for all buffers in ibuffer.
                            ;; all-the-icons-ivy-rich ;
                            ;; good-scroll ; This package implements smooth scrolling by pixel lines. It attempts to improve upon `pixel-scroll-mode' by adding variable speed.
                            ;; elisp-format
                            ;; 
                            ;; 
                            ;; org-download
                            ;; format-all
                            ;; htmlize ; from org-site
                            ;; ;;ox-twbs
                            
                            ;; 
                            treemacs-all-the-icons
                            corfu
                            orderless
                            ;; 
                            ;; eglot
                            epc
                            corfu-doc
                            lin

                            ;;; D
                            which-key
                            window-numbering
                            magit
                            rainbow-delimiters
                            hungry-delete
                            ctrlf
                            popwin
                            minions
                            cape
                            ;; format-all
                            ;; crux

                            
                            ;;; G
                            yasnippet

                            ;;; H
                            posframe
                            markdown-mode

                            ;;; I
                            pyvenv

                            ;;org
                            org-superstar
                            org-noter
                            org-ql
                            org-download
                            pangu-spacing
                            ;;valign installed

                            ;;org-roam
                            org-roam
                            org-roam-ui
                            org-roam-bibtex
                            websocket

                            dash
                            f
                            s
                            emacsql
                            emacsql-sqlite
                            magit-section
                            ;; [filenotify-recursive][https://github.com/jethrokuan/filenotify-recursive]

                            ;; programming
                            
                            julia-repl


                            ;; TeX
                            auctex
                            reftex
                            cdlatex
                            company-auctex
                            pdf-tools
                            latex-preview-pane ;;latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs. It supports PDF previews, your choice of pdflatex or xelatex, and it highlights errors in your LaTeX buffer.
                            zotxt ; for reference

                            ;;; Website
                            htmlize

                            ;;; Optional
                            keycast
                            ef-themes

                            ;;; Programming
                            lua-mode

                            ;;; deno-bridge:
                            websocket ;; dependency for the package, org-roam-ui-20220225.2151
                            
                                 )  "Default packages.")

(setq package-selected-packages puremacs/packages)

(defun puremacs/packages-installed-p ()
  "Looping all the packages."
  (cl-loop for pkg in puremacs/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (puremacs/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg puremacs/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;--------------------------------------------------------------------
(provide 'init-0-plugin)
;;; init-0-plugin.el ends here
