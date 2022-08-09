(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")

;; (require 'lsp-bridge)             ;; load lsp-bridge
;; (global-corfu-mode)               ;; use corfu as completion ui

;; (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
;; (require 'lsp-bridge-icon)        ;; show icon for completion items, optional

;; ;; Enable auto completion in elisp mode.
;; (dolist (hook (list
;;                'emacs-lisp-mode-hook
;;                ))
;;   (add-hook hook (lambda ()
;;                    (setq-local corfu-auto t)
;;                    )))

;; ;; Enable lsp-bridge.
;; (dolist (hook (list
;;                'c-mode-hook
;;                'c++-mode-hook
;;                'java-mode-hook
;;                'python-mode-hook
;;                'ruby-mode-hook
;;                'rust-mode-hook
;;                'elixir-mode-hook
;;                'go-mode-hook
;;                'haskell-mode-hook
;;                'haskell-literate-mode-hook
;;                'dart-mode-hook
;;                'scala-mode-hook
;;                'typescript-mode-hook
;;                'typescript-tsx-mode-hook
;;                'js2-mode-hook
;;                'js-mode-hook
;;                'rjsx-mode-hook
;;                'tuareg-mode-hook
;;                'latex-mode-hook
;;                'Tex-latex-mode-hook
;;                'texmode-hook
;;                'context-mode-hook
;;                'texinfo-mode-hook
;;                'bibtex-mode-hook
;; 	       'clojure-mode-hook
;; 	       'clojurec-mode-hook
;; 	       'clojurescript-mode-hook
;; 	       'clojurex-mode-hook
;;                ))
;;   (add-hook hook (lambda ()
;;                    (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
;;                    (lsp-bridge-mode 1)
;;                    )))

;(add-to-list 'load-path "<path-to-lsp-bridge>")

;; (require 'lsp-bridge)             ;; load lsp-bridge
;; (global-corfu-mode)               ;; use corfu as completion ui
;; (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
;; (require 'lsp-bridge-icon)        ;; show icon for completion items, optional
;; (global-lsp-bridge-mode)

;; (add-to-list 'load-path "<path-to-lsp-bridge>")
(require 'posframe)
;(require 'yasnippet)
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)       ;; provide Java third-party library jump and -data directory support, optional

;(yas-global-mode 1)
(global-lsp-bridge-mode)
(setq acm-enable-doc nil)
;;(setq lsp-bridge-complete-manually t)

;;--------------------------------------------------------------------
(provide 'init-h-lsp)
