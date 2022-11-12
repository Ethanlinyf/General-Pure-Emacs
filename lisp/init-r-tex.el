;;; init-tex.el --- Setup for latex. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; Configurations for the tex system
;;--------------------------------------------------------------------
;;; Code:

(use-package tex
  :ensure auctex
  :hook
  (LaTeX-mode . lsp-bridge-mode))

;; ;; enables mixing variable-pitch and fixed-pitch fonts in the same buffer
;; (use-package mixed-pitch
;;   :diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AUCTex Initiating;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq TeX-source-correlate-method 'synctex)
(load "auctex.el" nil t t)
;; (require 'company-auctex)
;; (load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RefTex;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-toc-split-windows-horizontally t) ;;*toc*buffer on left。
(setq reftex-toc-split-windows-fraction 0.2)  ;;*toc*buffer ratio。
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;CDLaTeX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  :config
  (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
  (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil))
;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;LaTex-mode settings;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pdf-tools ;; need to manually download from nongnu
  ;; :pin manual
  :ensure t
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; (use-package pdf-tools
;;  :ensure t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (require 'tex-site)
                             ;; (require 'company-auctex)
                             ;; (company-auctex-init)
                             (setq pdf-view-use-scaling t)
                                        ;(require 'init-auto-complete)
                             ;;(require 'init-g-yasnippet)
		             (TeX-fold-mode 1)
		             (auto-fill-mode 1)
                             ;; (latex-preview-pane-enable)

		  ;;;;;;;;;;;;;;;; flyspell settings
		             (flyspell-mode 1)
		             (setq flyspell-sort-corrections nil)
		             (setq flyspell-doublon-as-error-flag nil)
                             (setq split-width-threshold 80) ;; pdf-tool to open a pdf in the right side


		             (turn-on-auto-fill)              ;;LaTeX mode，turn off auto fold
		             ;; (linum-mode 1)
		             ;;(auto-complete-mode 1)
		             (latex-math-mode 1)
		             (outline-minor-mode 1)
  		             (imenu-add-menubar-index)

		             (setq TeX-show-compilation nil)   ;;NOT display compilation windows
		             (setq TeX-global-PDF-mode t)       ;;PDF mode enable, not plain
		             ;;(setq TeX-engine 'default)  ;;use xelatex default
		             (setq TeX-clean-confirm nil)
		             (setq TeX-save-query nil)

                             (setq font-latex-fontify-script t)
		             (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
		             (setq TeX-electric-escape t)      ;; press \ then, jump to mini-buffer to input commands
                                        ;(setq TeX-view-program-list '(("Evince" "evince %o"))) ;;
                                        ;(setq TeX-view-program-selection '((output-pdf "Evince")))
                             (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                                   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                                   TeX-source-correlate-start-server t)
                                        ;(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                                        ;(setq TeX-command-default "XeLaTeX")
		             (add-to-list 'TeX-command-list '("LaTeX" "%`pdflatex -shell-escape --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
		             (setq TeX-command-default "LaTeX")
                             ;;(setq TeX-command-default "pdflatex --synctex=1")

                             (setq TeX-fold-env-spec-list (quote (("[comment]" ("comment")) ("[figure]" ("figure")) ("[table]" ("table"))("[itemize]"("itemize"))("[enumerate]"("enumerate"))("[description]"("description"))("[overpic]"("overpic"))("[tabularx]"("tabularx"))("[code]"("code"))("[shell]"("shell")))))


		             (define-key LaTeX-mode-map (kbd "C-c C-p") 'reftex-parse-all)
                             ;;(with-eval-after-load 'latex
                             (define-key LaTeX-mode-map (kbd "C-c C-g") #'pdf-sync-forward-search)

                  ;;;;;;deeper directory;;;;;;;;;;;;;
                                        ;(setq reftex-section-levels
                                        ;     '(("part" . 0) ("chapter" . 1) ("section" . 2) ("subsection" . 3)
                                        ;       ("frametitle" . 4) ("subsubsection" . 4) ("paragraph" . 5)
                                        ;       ("subparagraph" . 6) ("addchap" . -1) ("addsec" . -2)))
                             (pdf-tools-install)

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
                                   TeX-source-correlate-start-server t  ;; [C-c C-g] to switch between source code and PDF
                                   reftex-plug-into-AUCTeX t)
                             (add-hook 'TeX-after-compilation-finished-functions
		                       #'TeX-revert-document-buffer) ;
                             (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
                             ));;





;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)



;;'(LaTeX-command "latex --shell-escape --synctex=1")

;; (with-eval-after-load 'tex
;;   (add-to-list 'safe-local-variable-values
;;                '(TeX-command-extra-options . "-synctex=1")))

;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;;;; SyncTeX
;; (setq pdf-sync-backward-display-action t
;;       pdf-sync-forward-display-action t
;;       TeX-source-correlate-mode t
;;       TeX-source-correlate-method '(
;;                                     (dvi . source-specials)
;;                                     (pdf . synctex))
;;       TeX-source-correlate-start-server t  ;; [C-c C-g] to switch between source code and PDF
;;       reftex-plug-into-AUCTeX t)
;; (add-hook 'TeX-after-compilation-finished-functions
;;       	        #'TeX-revert-document-buffer) ; reload pdf buffer


;; (setq-default TeX-master nil
;;               TeX-command  "pdflatex -shell-escape -synctex=1") ;; Set default command to compile with SyncTeX


  ;;;; LaTeX-preview-pane variables
(use-package latex-preview-pane
  :ensure t
  :hook
  (LaTeX-mode . latex-preview-pane-enable))
;; (latex-preview-pane-mode .  (setq pdf-latex-command "pdflatex"
;;                                   synctex-number "1"
;;                                   shell-escape-mode "-shell-escape"
;;                                   auto-update-latex-preview-pane 'off)))
(add-hook 'latex-preview-pane-mode-hook
          (setq pdf-latex-command "pdflatex"
                synctex-number "1"
                shell-escape-mode "-shell-escape"
                auto-update-latex-preview-pane 'off))

;;;; Keybindings for LaTeX-preview-pane mode
(defun display-pdflatex-result ()
  (interactive)
  (unless (equal "*pdflatex-buffer*" (buffer-name (window-buffer)))
    (display-buffer "*pdflatex-buffer*" (if (one-window-p)
                                            'display-buffer-pop-up-window
                                          'display-buffer-reuse-window))))

;; (add-hook 'latex-preview-pane-mode-hook
;;           (lambda ()
;;             (define-key LaTeX-mode-map (kbd "<f5>")
;;               '(lambda ()
;;                  (interactive)
;;                  (save-excursion
;;                    (save-buffer)
;;                    (latex-preview-pane-update))))
;;             (local-set-key (kbd "<f6>") #'TeX-view)
;;             (local-set-key (kbd "<f12>") #'display-pdflatex-result)))

  ;;;; SyncTeX
;;   (setq pdf-sync-backward-display-action t
;;         pdf-sync-forward-display-action t
;;         TeX-source-correlate-mode t

;;         ;; TeX-source-correlate-method '(
;;         ;;                               (dvi . source-specials)
;;         ;;                               (pdf . synctex))
;;         TeX-source-correlate-start-server t
;;         reftex-plug-into-AUCTeX t)
;; (setq TeX-source-correlate-method 'synctex)
;;   (add-hook 'TeX-after-compilation-finished-functions
;; 		        #'TeX-revert-document-buffer)

;;--------------------------------------------------------------------

(defun ddd ()
  "Insert a dollar sign with a space in front."
  (interactive)
  (insert "\\"))


(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "\\") #'ddd))

;;--------------------------------------------------------------------
;; https://github.com/haji-ali/procress
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/procress")
;; (require 'procress)
;; (procress-load-default-svg-images)
;; (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)

;; (use-package procress
;;   ;; :straight (:host github :repo "haji-ali/procress")
;;   :commands procress-auctex-mode
;;   :init
;;   (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
;;   :config
;;   (procress-load-default-svg-images))

;;--------------------------------------------------------------------
(provide 'init-r-tex)
;;; init-tex ends here
