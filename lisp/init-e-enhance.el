;;; init-e-enhance.el --- Enhancement. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Some enhancement for editting
;;--------------------------------------------------------------------
;;; Code:

;; ------------------ Indent Region or Buffer ------------------------
(defun indent-buffer()
  "To indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  "To indent the region or buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;;--------------------------------------------------------------------
;; (dirvish-override-dired-mode)

;; abbrev settings
(setq-default abbrev-mode t)
(setq save-abbrevs nil)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...

;; (add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; (add-hook 'after-init-hook 'electric-indent-mode')

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
;; from centaur emacs
(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push "~/.emacs.d/elgrep-data.el" recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (add-to-list 'recentf-exclude
               (expand-file-name "~/.emacs.d/company-statistics-cache.el")
               (expand-file-name "~/.emacs.d/elgrep-data.el"))
  (when *is-mac*
    (global-set-key (kbd "s-3") 'recentf-open-files)))

;; The following could be implemented by counsel
;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)

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
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
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
;; An intuitive and efficient solution for single-buffer text search
(use-package ctrlf
  :ensure t
  :init
  (ctrlf-mode +1))
;;--------------------------------------------------------------------
;; session save and restore

;; (setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
;; (setq desktop-restore-frames nil)    ; don't restore any frame

;; (defun emacs-session-restore ()
;;   "Restore emacs session."
;;   (interactive)
;;   (ignore-errors
;;     ;; Kill other windows.
;;     (delete-other-windows)
;;     ;; Kill unused buffers.
;;     (kill-unused-buffers)
;;     ;; Restore session.
;;     (desktop-read "~/.emacs.d/")
;;     ))

;; (defun emacs-session-save (&optional arg)
;;   "Save emacs session."
;;   (interactive "p")
;;   (ignore-errors
;;     (if (equal arg 4)
;;         ;; Kill all buffers if with prefix argument.
;;         (mapc 'kill-buffer (buffer-list))
;;       ;; Kill unused buffers.
;;       (kill-unused-buffers)
;;       ;; Save all buffers before exit.
;;       (auto-save-buffers))
;;     ;; Save session.
;;     (make-directory "~/.emacs.d/" t)
;;     (desktop-save "~/.emacs.d/")
;;     ;; Exit emacs.
;;     (kill-emacs)))
;;--------------------------------------------------------------------
;; ;; auto-save
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/auto-save")) ; add auto-save to your load-path
;; (require 'auto-save)
;; (auto-save-enable)

;; (setq auto-save-silent t)   ; quietly save
;; (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;; ;;; custom predicates if you don't want auto save.
;; ;;; disable auto save mode when current filetype is an gpg file.
;; (setq auto-save-disable-predicates
;;       '((lambda ()
;;           (string-suffix-p
;;            "gpg"
;;            (file-name-extension (buffer-name)) t))))

;;--------------------------------------------------------------------
;; goto-line-preview
(use-package goto-line-preview
  :ensure t)

;;--------------------------------------------------------------------
;; golden-ratio
(use-package golden-ratio
  :ensure t
  ;:hook
  ;(after-init . golden-ratio-mode)
  :config
  (setq golden-ratio-auto-scale t))

;;--------------------------------------------------------------------
;; undo-tree
(use-package undo-tree
  :ensure t
  :hook
  (after-init . undo-tree-mode))
;;--------------------------------------------------------------------
;; Elisp API Demos
;; C-h x command RET (describe-command) displays the documentation of the named command, in a window. 
(use-package elisp-demos
  :ensure t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

(use-package ace-window
  :ensure t
  :hook
  (emacs-startup . ace-window-display-mode)
  :bind
  ([remap other-window] . ace-window))
;;--------------------------------------------------------------------
(provide 'init-e-enhance)
;;; init-e-enhance.el ends here
