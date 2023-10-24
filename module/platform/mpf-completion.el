;;; mpf-completion.el --- Basic GPE settings -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

;;; code:

;; lsp-bridge

(use-package epc
  :ensure t)

(use-package corfu
  :init
  (progn
    (setq corfu-auto t)
    (setq corfu-cycle t)
    (setq corfu-quit-at-boundary t)
    (setq corfu-quit-no-match t)
    (setq corfu-preview-current nil)
    (setq corfu-min-width 80)
    (setq corfu-max-width 100)
    (setq corfu-auto-delay 0.2)
    (setq corfu-auto-prefix 1)
    (setq corfu-on-exact-match nil)
    (global-corfu-mode)
    ))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;; ;; Merge the dabbrev, dict and keyword capfs, display candidates together.
;; (setq-local completion-at-point-functions
;;             (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))

;; ;; Alternative: Define named Capf instead of using the anonymous Capf directly
;; (defun cape-dabbrev-dict-keyword ()
;;   (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
;; (setq-local completion-at-point-functions (list #'cape-dabbrev-dict-keyword))

(use-package lsp-bridge
  :load-path "extension/lsp-bridge/"
  ;; :ensure-system-package
  ;; ((epc . "pip install epc")
  ;;  (orjson . "pip install orjson")
  ;;  (six . "pip install six"))
  :commands lsp-bridge-mode
  :ensure nil
  :config
  (setq lsp-bridge-enable-search-words t)
  (setq lsp-bridge-enable-search-words t)
  (setq lsp-bridge-enable-completion-in-minibuffer t)
  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (setq acm-enable-doc nil)
  (setq lsp-bridge-enable-with-tramp t)
  (setq acm-enable-quick-access t)
  (setq acm-backend-yas-match-by-trigger-keyword t)
  (setq acm-enable-tabnine t)
  (setq acm-enable-copilot nil)
  (setq lsp-bridge-enable-org-babel t) ; error popping up, need to be checked.
  :hook
  (prog-mode . lsp-bridge-mode)
  (lsp-bridge-mode . (lambda () (corfu-mode -1))))

;;-------------------------------------------------------------------------------------------------
(provide 'mpf-completion)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpf-completion.el ends here
