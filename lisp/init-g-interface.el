;; init-g-interface.el -- UI configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for User Interface
;;--------------------------------------------------------------------
;;; Code:

;; set a title for the active frame
(setq frame-title-format
      '("Puremacs"  ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; Window size and features
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (and *is-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-s-f") 'toggle-frame-fullscreen))

;;--------------------------------------------------------------------
;; theme doom-one
;; see the PR: https://github.com/doomemacs/themes/pull/779
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  :init
  (load-theme 'doom-one t))

;;--------------------------------------------------------------------
(use-package ns-auto-titlebar
  :if *is-mac*
  :ensure t
  :init
  (ns-auto-titlebar-mode t))

;;--------------------------------------------------------------------
;; Display minor-mode in the mode line
(use-package minions
  :ensure t
  :hook (after-init . minions-mode)
  :init
  (setq doom-modeline-minor-modes t))

;;--------------------------------------------------------------------
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  ;; Whether display the icons.
  (setq nerd-icons-ibuffer-icon t)

  ;; Whether display the colorful icons.
  ;; It respects `nerd-icons-color-icons'.
  (setq nerd-icons-ibuffer-color-icon t)

  ;; The default icon size in ibuffer.
  (setq nerd-icons-ibuffer-icon-size 1.0)

  ;; Use human readable file size in ibuffer.
  (setq  nerd-icons-ibuffer-human-readable-size t)

  ;; A list of ways to display buffer lines with `nerd-icons'.
  ;; See `ibuffer-formats' for details.
  nerd-icons-ibuffer-formats

  ;; Slow Rendering
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable
  (setq inhibit-compacting-font-caches t)
  )


;;--------------------------------------------------------------------
;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)

;;--------------------------------------------------------------------
;; nerd-icons for completion
(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (foo-mode . rainbow-delimiters-mode)
  (prog-mode . rainbow-delimiters-mode))

;; osx-lib
(use-package osx-lib
  :if *is-mac*)

(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode))

;;--------------------------------------------------------------------
(auto-image-file-mode 1)
(face-remap-add-relative 'font-lock-keyword-face '(:inherit default))
(face-remap-add-relative 'font-lock-keyword-face `(:foreground ,(face-foreground 'default)))

;;--------------------------------------------------------------------
(use-package buffer-move
  :ensure t
  :bind
  ("<C-S-up>" . buf-move-up)  ; (global-set-key (kbd "<C-S-up>") 'buf-move-up)
  ("<C-S-down>" . buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

;; Frame
(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa)
  (bind-key "S-s-<return>" #'toggle-frame-fullscreen)
  (and sys/mac-x-p (bind-key "C-s-f" #'toggle-frame-fullscreen))

  ;; Resize and re-position frames conveniently, on macOS: use Magnet to achieve it
  ;; (bind-keys ("C-M-<return>"    . frame-maximize)
  ;;            ("C-M-<backspace>" . frame-restore)
  ;;            ("C-M-<left>"      . frame-left-half)
  ;;            ("C-M-<right>"     . frame-right-half)
  ;;            ("C-M-<up>"        . frame-top-half)
  ;;            ("C-M-<down>"      . frame-bottom-half)))
  )

;; Prettify the process list
(with-no-warnings
  (add-hook 'process-menu-mode-hook
            (lambda ()
              (setq tabulated-list-format
                    (vconcat `(("" ,(if (icon-displayable-p) 2 0)))
                             tabulated-list-format))))

  (defun my-list-processes--prettify ()
    "Prettify process list."
    (when-let ((entries tabulated-list-entries))
      (setq tabulated-list-entries nil)
      (dolist (p (process-list))
        (when-let* ((val (cadr (assoc p entries)))
                    (icon (if (icons-displayable-p)
                              (concat
                               " "
                               (nerd-icons-faicon "nf-fa-bolt" :face 'nerd-icons-lblue))
                            " x"))
                    (name (aref val 0))
                    (pid (aref val 1))
                    (status (aref val 2))
                    (status (list status
                                  'face
                                  (if (memq status '(stop exit closed failed))
                                      'error
                                    'success)))
                    (buf-label (aref val 3))
                    (tty (list (aref val 4) 'face 'font-lock-doc-face))
                    (thread (list (aref val 5) 'face 'font-lock-doc-face))
                    (cmd (list (aref val 6) 'face 'completions-annotations)))
          (push (list p (vector icon name pid status buf-label tty thread cmd))
                tabulated-list-entries)))))
  (advice-add #'list-processes--refresh :after #'my-list-processes--prettify))

;;--------------------------------------------------------------------
(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           embark-collect-mode
           pdf-annot-list-mode) . hide-mode-line-mode)))

;;--------------------------------------------------------------------
;; add holo-layer; It was removed
;; (use-package holo-layer
;;   :ensure nil
;;   :load-path "site-lisp/holo-layer"
;;   :commands holo-layer-enable
;;   :init
;;   (setq holo-layer-enable-cursor-animation t)
;;   (setq holo-layer-enable-window-border t)
;;   ;; (holo-layer-enable)
;;   )

;;-------------------------------------------------------------------------------------------------
(provide 'init-g-interface)
;;; init-g-interface.el ends here
