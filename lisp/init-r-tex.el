;;; init-r-tex.el --- Setup for latex. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for the TeX system
;;--------------------------------------------------------------------
;;; Code:

(add-hook 'LaTeX-mode-hook 'lsp-bridge-mode)

(use-package auctex
  :ensure t
  :init
  (load "auctex.el" nil t t)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

;;--------------------------------------------------------------------
;; REFTEX Settings
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-toc-split-windows-horizontally t)                       ; *toc*buffer on left。
(setq reftex-toc-split-windows-fraction 0.2)                         ; *toc*buffer ratio。
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)

;;--------------------------------------------------------------------
;; CDLATEX Settings
(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex))

;;--------------------------------------------------------------------
;; pdf-tools
(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :hook
  (pdf-view-mode . (lambda() (setq display-line-numbers-mode nil))))

;;--------------------------------------------------------------------
(add-hook 'LaTeX-mode-hook (lambda ()
                             (pdf-tools-install)
                             (require 'tex-site)
                             (setq pdf-view-use-scaling t)
                             (TeX-fold-mode 1)
                             (auto-fill-mode 1)

                             (flyspell-mode 1)
                             (setq flyspell-sort-corrections nil)
                             (setq flyspell-doublon-as-error-flag nil)

                             (setq split-width-threshold 80) ;  pdf-tool to open a pdf in the right side
                             (turn-on-auto-fill)             ; LaTeX mode，turn off auto fold
                             (latex-math-mode 1)
                             (outline-minor-mode 1)
                             (imenu-add-menubar-index)

                             (setq TeX-show-compilation nil) ; NOT display compilation windows
                             (setq TeX-global-PDF-mode t)    ; PDF mode enable, not plain
                             ;;(setq TeX-engine 'default)      ; use xelatex default
                             (setq TeX-clean-confirm nil)
                             (setq TeX-save-query nil)

                             (setq font-latex-fontify-script t)
                             (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
                             ;;(setq TeX-electric-escape t)      ; press \ then, jump to mini-buffer to input commands
                             ;;(setq TeX-view-program-list '(("Evince" "evince %o"))) ;;
                             ;;(setq TeX-view-program-selection '((output-pdf "Evince")))
                             (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                                   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                                   TeX-source-correlate-start-server t)
                             ;;(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             ;;(setq TeX-command-default "XeLaTeX")
                             (add-to-list 'TeX-command-list '("LaTeX" "%`pdflatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "LaTeX")
                             ;;(setq TeX-command-default "pdflatex --synctex=1")

                             (setq TeX-fold-env-spec-list (quote (("[comment]" ("comment")) ("[figure]" ("figure")) ("[table]" ("table"))("[itemize]"("itemize"))("[enumerate]"("enumerate"))("[description]"("description"))("[overpic]"("overpic"))("[tabularx]"("tabularx"))("[code]"("code"))("[shell]"("shell")))))


                             (define-key LaTeX-mode-map (kbd "C-c C-p") 'reftex-parse-all)
                             (define-key LaTeX-mode-map (kbd "C-c C-g") #'pdf-sync-forward-search)

                             (setq LaTeX-section-hook
                                   '(LaTeX-section-heading
                                     LaTeX-section-title
                                     LaTeX-section-toc
                                     LaTeX-section-section
                                     LaTeX-section-label))

                             (setq pdf-sync-backward-display-action t
                                   pdf-sync-forward-display-action t
                                   TeX-source-correlate-mode t
                                   TeX-source-correlate-method '(
                                                                 (dvi . source-specials)
                                                                 (pdf . synctex))
                                   TeX-source-correlate-start-server t  ; [C-c C-g] to switch between source code and PDF
                                   reftex-plug-into-AUCTeX t)
                             (add-hook 'TeX-after-compilation-finished-functions
                                       #'TeX-revert-document-buffer) ;
                             (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
                             ))

;;-------------------------------------------------------------------------------------------------
(provide 'init-r-tex)
;;; init-r-tex.el ends here
