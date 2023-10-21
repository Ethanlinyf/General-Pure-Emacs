;;; mpf-completion.el --- Basic GPE settings -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

;;; code:

(add-to-list 'load-path "/Users/ethanlin/.emacs.d/extension/lsp-bridge/")

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(setq lsp-bridge-enable-search-words t)
(add-hook 'after-init-hook #'global-lsp-bridge-mode)
(add-hook 'lsp-bridge-mode (lambda ()(corfu-mode -1)))



(message "completion")

(provide 'mpf-completion)
;;; mpf-completion.el ends here
