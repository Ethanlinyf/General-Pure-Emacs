;; (setq package-archives
;;       '(("gnu"   . "https://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; (add-hook 'prog-mode-hook #'awesome-tab-mode)
;; (add-hook 'eshell-mode-hook #'(lambda() (awesome-tab-mode -1)))
;; (add-hook 'Info-mode-hook (lambda() (awesome-tab-mode -1)))

;;See matching pairs of parentheses and other characters.
;;(show-paren-mode t)
;;(setq show-paren-delay 0)
;; example for a specific mode to turn on this "show-paren-mode":
;;(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)


;;-------------------------------------------------------------------- 

;; https://github.com/re5et/simp
;; (use-package simple
;;   :ensure nil
;;   :hook (after-init . size-indication-mode)
;;   :init
;;   (progn
;;     (setq column-number-mode t)
;;     ))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))

;; (when (fboundp 'set-scroll-bar-mode)
;;   (set-scroll-bar-mode nil))

;; (when *is-mac*
;;   (menu-bar-mode 1))

;; (let ((no-border '(internal-border-width . 0)))
;;   (add-to-list 'default-frame-alist no-border)
;;   (add-to-list 'initial-frame-alist no-border))


;; move to basic.el
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;; The following doesn't work
;; (require 'minions)
;; (add-hook 'after-doom-modeline-hook #'minions-mode)
