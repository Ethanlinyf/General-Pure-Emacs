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

;; Indent Region or Buffer
(use-package GPE-indent
  :ensure nil
  :commands (indent-region-or-buffer)
  :bind (("C-M-\\" . indent-region-or-buffer))
  :init
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
          (message "Indent buffer."))))))
;;--------------------------------------------------------------------

;; abbrev settings
(use-package GPE-abbrev
  :ensure nil
  :init
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)
  
  (defconst abbrev-file (expand-file-name "abbrev_defs" user-emacs-directory))
  (unless (file-exists-p abbrev-file)
    (shell-command (concat "touch " abbrev-file)))
  (setq abbrev-file-name abbrev-file))

;;--------------------------------------------------------------------
;; Further Enhancement
(use-package GPE-enhancement
  :ensure nil
  :init
  (setq delete-by-moving-to-trash t) ; disable delete directly
  (add-hook 'after-init-hook 'electric-pair-mode)
  (add-hook 'after-init-hook 'winner-mode)
  (add-hook 'after-init-hook 'global-auto-revert-mode)
  (add-hook 'after-init-hook 'electric-indent-mode)
  (add-hook 'prog-mode-hook (lambda() (setq split-width-threshold 80)))

  (add-hook 'after-init-hook 'show-paren-mode)
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn))))))

;;--------------------------------------------------------------------
;;happie-expand
(use-package GPE-hippie-expand
  :ensure nil
  :commands (hippie-expand)
  :bind ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-function-list '(try-expand-debbrev
 					  try-expand-debbrev-all-buffers
 					  try-expand-debbrev-from-kill
 					  try-complete-file-name-partially
 					  try-complete-file-name
 					  try-expand-all-abbrevs
 					  try-expand-list
 					  try-expand-line
 					  try-complete-lisp-symbol-partially
 					  try-complete-lisp-symbol)))

;;--------------------------------------------------------------------

;; from centaur emacs
(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 300
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir))))

  (push (expand-file-name "elgrep-data.el" user-emacs-directory) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (add-to-list 'recentf-exclude
               (expand-file-name "company-statistics-cache.el" user-emacs-directory)
               (expand-file-name "elgrep-data.el" user-emacs-directory))
  
  (when sys/macp
    (global-set-key (kbd "s-3") 'recentf-open-files)))

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
  :config
  (setq which-key-max-description-length 30
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

;; ;; Mouse wheel scroll behavior
;; (use-package GPE-mouse-wheel-scroll
;;   :ensure nil
;;   :init
;;   (setq
;;    ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;    mouse-wheel-progressive-speed nil
;;    mouse-wheel-follow-mouse t
;;    next-line-add-newlines nil
;;    read-process-output-max (* 64 1024)
;;    scroll-step 1
;;    scroll-conservatively 10000
;;    scroll-preserve-screen-position t
;;    scroll-up-aggressively 0.01
;;    scroll-down-aggressively 0.01
;;    ))

;;--------------------------------------------------------------------
;; Flycheck
(use-package flycheck
  :ensure t
  :init (setq global-flycheck-mode nil)
  :hook
  (prog-mode . flycheck-mode))

;;--------------------------------------------------------------------
(use-package GPE-flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :bind (("C-c f s" . flyspell-mode)
         ("C-c f c" . flyspell-correct-word-before-point)
         :map flyspell-mouse-map
         ([down-mouse-3] . flyspell-correct-word))
  :config
  (if (executable-find "hunspell")
      (setq ispell-program-name "hunspell")
    (setq ispell-program-name "aspell"))
  
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,en_US-med") nil utf-8)))
  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

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
  ;;:hook
  ;;(after-init . golden-ratio-mode)
  :config
  (setq golden-ratio-auto-scale t))

;;--------------------------------------------------------------------
;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t)
  ;; (defvar undo-dir (expand-file-name "undo-dir/" user-emacs-directory)
  (setq undo-tree-history-directory-alist '(("." . "undo-tree-dir/"))) ;; to be improved
  (defun my-undo-tree-save-history (undo-tree-save-history &rest args)
    (let ((message-log-max nil)
          (inhibit-message t))
      (apply undo-tree-save-history args)))
  (advice-add 'undo-tree-save-history :around 'my-undo-tree-save-history)
  :init (global-undo-tree-mode)
;; :after hydra
;; :bind ("C-x C-h u" . hydra-undo-tree/body)
;; :hydra (hydra-undo-tree (:hint nil)
;; "
;; _p_: undo  _n_: redo _s_: save _l_: load   "
;; ("p"   undo-tree-undo)
;; ("n"   undo-tree-redo)
;; ("s"   undo-tree-save-history)
;; ("l"   undo-tree-load-history)
;; ("u"   undo-tree-visualize "visualize" :color blue)
;; ("q"   nil "quit" :color blue)))
)

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
;; (use-package multiple-cursors
;;   :bind
;;   ("C-s-<mouse-1>" . mc/toggle-cursor-on-click))


;; To be enhanced as follows:
;; (use-package multiple-cursors
;;   :ensure t
;;   :after hydra
;;   :bind
;;   (("C-x C-h m" . hydra-multiple-cursors/body)
;;    ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
;;   :hydra (hydra-multiple-cursors
;; 	  (:hint nil)
;; 	  "
;; Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
;; ------------------------------------------------------------------
;;  [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
;;  [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
;;  [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
;;  [_|_] Align with input CHAR       [Click] Cursor at point"
;; 	  ("l" mc/edit-lines :exit t)
;; 	  ("a" mc/mark-all-like-this :exit t)
;; 	  ("n" mc/mark-next-like-this)
;; 	  ("N" mc/skip-to-next-like-this)
;; 	  ("M-n" mc/unmark-next-like-this)
;; 	  ("p" mc/mark-previous-like-this)
;; 	  ("P" mc/skip-to-previous-like-this)
;; 	  ("M-p" mc/unmark-previous-like-this)
;; 	  ("|" mc/vertical-align)
;; 	  ("s" mc/mark-all-in-region-regexp :exit t)
;; 	  ("0" mc/insert-numbers :exit t)
;; 	  ("A" mc/insert-letters :exit t)
;; 	  ("<mouse-1>" mc/add-cursor-on-click)
;; 	  ;; Help with click recognition in this hydra
;; 	  ("<down-mouse-1>" ignore)
;; 	  ("<drag-mouse-1>" ignore)
;; 	  ("q" nil)))

;;--------------------------------------------------------------------
;; Rectangle from Centaur Emacs
;; (use-package rect
;;   :ensure nil
;;   :bind (:map text-mode-map
;;               ("<C-return>" . rect-hydra/body)
;;               :map prog-mode-map
;;               ("<C-return>" . rect-hydra/body))
;;   :init
;;   (with-eval-after-load 'org
;;     (bind-key "<s-return>" #'rect-hydra/body org-mode-map))
;;   (with-eval-after-load 'wgrep
;;     (bind-key "<C-return>" #'rect-hydra/body wgrep-mode-map))
;;   (with-eval-after-load 'wdired
;;     (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
;;   :pretty-hydra
;;   ((:title (pretty-hydra-title "Rectangle" 'material "border_all" :height 1.2 :v-adjust -0.225)
;;            :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key ("q" "C-g"))
;;    ("Move"
;;     (("h" backward-char "←")
;;      ("j" next-line "↓")
;;      ("k" previous-line "↑")
;;      ("l" forward-char "→"))
;;     "Action"
;;     (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
;;      ("y" yank-rectangle "yank")         ; C-x r y
;;      ("t" string-rectangle "string")     ; C-x r t
;;      ("d" kill-rectangle "kill")         ; C-x r d
;;      ("c" clear-rectangle "clear")       ; C-x r c
;;      ("o" open-rectangle "open"))        ; C-x r o
;;     "Misc"
;;     (("N" rectangle-number-lines "number lines")        ; C-x r N
;;      ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
;;      ("u" undo "undo")
;;      ("r" (if (region-active-p)
;;               (deactivate-mark)
;;             (rectangle-mark-mode 1))
;;       "reset")))))


;;--------------------------------------------------------------------
;; tiny to be added.
;;--------------------------------------------------------------------

(use-package highlight-symbol
  :ensure t
  ;; :init (highlight-symbol-mode)
  :bind ("<f7>" . highlight-symbol))
;;--------------------------------------------------------------------

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-mode)
  :bind (([remap describe-command] . helpful-command)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h s" . helpful-symbol)
         ("C-h S" . describe-syntax)
         ("C-h m" . describe-mode)
         ("C-h F" . describe-face)
         ([remap describe-key] . helpful-key))
  )

;;--------------------------------------------------------------------
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;--------------------------------------------------------------------
;; Markmacro
(use-package markmacro
  :ensure nil
  :load-path "site-lisp/markmacro"
  :bind (("s-/" . markmacro-mark-words)
         ("s-?" . markmacro-mark-lines)
         ("s-<" . markmacro-apply-all)
         ("s->" . markmacro-apply-all-except-first)
         ("s-M" . markmacro-rect-set)
         ("s-D" . markmacro-rect-delete)
         ("s-F" . markmacro-rect-replace)
         ("s-I" . markmacro-rect-insert)
         ("s-C" . markmacro-rect-mark-columns)
         ("s-S" . markmacro-rect-mark-symbols)))

;;--------------------------------------------------------------------
(provide 'init-e-enhance)
;;; init-e-enhance.el ends here
