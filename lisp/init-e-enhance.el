;;; init-e-enhance.el --- Enhancement. -*- lexical-binding: t; -*-
;;
; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Some enhancement for editting
;;--------------------------------------------------------------------
;;; Code:

;; abbrev settings
(setq-default abbrev-mode t)
(setq save-abbrevs nil)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...

;;(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'recentf-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; (add-hook 'after-init-hook 'electric-indent-mode')

;;See matching pairs of parentheses and other characters.
;;(show-paren-mode t)
;;(setq show-paren-delay 0)
;; example for a specific mode to turn on this "show-paren-mode":
;;(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'after-init-hook 'show-paren-mode)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
            (ignore-errors (backward-up-list))
            (funcall fn)))))

(setq hippie-expand-try-function-list '(try-expand-debbrev
 					try-expand-debbrev-all-buffers
 					try-expand-debbrev-from-kill
 					try-complete-file-name-partially
 					try-complete-file-name
 					try-expand-all-abbrevs
 					try-expand-list
 					try-expand-line
 					try-complete-lisp-symbol-partially
 					try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)

;;--------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 30)

;; The following could be implemented by counsel
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; ALT + number to swith to the number of a specific window
(use-package window-numbering
  :hook (after-init . window-numbering-mode))

(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t))

;;--------------------------------------------------------------------
;; displeay keystrok and commands on modeline
;; (require 'keycast)
;; (add-to-list 'global-mode-string '("" keycast-mode-line))
;; (keycast-mode t)

;;--------------------------------------------------------------------

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(minibuffer-mode help-modeminibuffer-inactive-mode calc-mode)))

;;--------------------------------------------------------------------
;; Mouse wheel scroll behavior
(setq
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      )

(setq delete-by-moving-to-trash t)  ;; disable delete directly

;;--------------------------------------------------------------------

(add-hook 'prog-mode-hook (lambda() (setq split-width-threshold 80)))



;;--------------------------------------------------------------------
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "de_DE")
(setq ispell-local-dictionary-alist
      '(("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

;;--------------------------------------------------------------------

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
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;;--------------------------------------------------------------------
;; Flycheck
(use-package flycheck
  :ensure t
  :init (setq global-flycheck-mode nil))


;;--------------------------------------------------------------------
    (setq ispell-program-name "hunspell")
    ;; you could set `ispell-dictionary` instead but `ispell-local-dictionary' has higher priority
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,en_US-med") nil utf-8)))
    ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
    ;; If it's nil, Emacs tries to automatically set up the dictionaries.
    (when (boundp 'ispell-hunspell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)
     (define-key flyspell-mouse-map [down-mouse-2] nil)
     (define-key flyspell-mouse-map [mouse-2] nil)))

;;--------------------------------------------------------------------
(provide 'init-e-enhance)
;;; init-e-enhance.el ends here
