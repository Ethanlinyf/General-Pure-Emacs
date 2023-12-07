;;; init-enhance.el --- Elisp Template . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; Enhancements for programming

;;; Code:
;; ensure environment variables inside Emacs look the same as in the user's shell
(with-eval-after-load 'exec-path-from-shell
  (when (or sys/macp sys/linuxp (daemonp))
    (setq exec-path-from-shell-check-startup-files nil)
    (add-hook 'after-init-hook #'exec-path-from-shell-initialize)))

;; Configs for programming languages
(add-hook 'prog-mode-hook (lambda () (setq-local column-number-mode t)))

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode) ; lambda --> λ
;; (add-hook 'prog-mode-hook 'which-function-mode)

(require 'projectile)
(projectile-mode +1)
(when sys/macp
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
(when (or sys/linuxp sys/win32p)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(require 'flyspell)
(when *is-win*
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; (require 'iedit)
;; (global-set-key (kbd "C-;") #'iedit-mode)

;; undo-tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil))

;;--------------------------------------------------------------------
;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  (after-init . yas-global-mode)
  :init
  (setq yas-verbosity 0) ; 1 or higher to show Yasnippet messages again
  :config
  (yas-reload-all)
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package move-text
  :bind (("<C-M-up>" . move-text-up)
         ("<C-M-down>" . move-text-down)))

(setq split-width-threshold 80)

;;-------------------------------------------------------------------------------------------------
(provide 'init-enhance)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-enhance.el ends here.
