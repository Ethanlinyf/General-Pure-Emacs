;;; init-const.el -- Better default configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Define constants.
;;--------------------------------------------------------------------
;;; Code:

(defvar *is-mac* (eq system-type 'darwin) "Current system is mac.")
(defvar *is-win* (eq system-type 'windows-nt) "Current system is windows.")
(defvar *is-nux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) "Current system is gnu/linux.")

(defconst ThingsEngine-homepage
  "https://thethingsengine.org"
  "The webpage of ThingsEngine.")

(defconst puremacs-custom-example-file
  (expand-file-name "custom-example.el" user-emacs-directory)
  "Custom example file of Pure Emacs.")

(defconst puremacs-custom-post-file
  (expand-file-name "custom-post.el" user-emacs-directory)
  "Custom file after startup.
Put private configurations to override defaults here.")

(defconst puremacs-custom-post-org-file
  (expand-file-name "custom-post.org" user-emacs-directory)
  "Custom org file after startup.
Put private configurations to override defaults here.
Loaded by `org-babel-load-file'.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25)
           (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25)
           (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

;;--------------------------------------------------------------------
(provide 'init-const)
;;; init-const.el ends here

;;;;;; Example: Switch Git Bash
;; (when *is-win*
;;   (setq explicit-shell-file-name
;;         "C:/Program Files/Git/bin/bash.exe")
;;   (setq shell-file-name explicit-shell-file-name)
;;   (add-to-list 'exec-path "C:/Program Files/Git/bin")
;;   )
