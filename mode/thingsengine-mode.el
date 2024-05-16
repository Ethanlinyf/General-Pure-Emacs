;;; thingsengine-mode.el --- A major mode for ThingsEngine. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023, by Ethanlinyf

;; Author: Dr Yufeng Lin ( e.yflin@gmail.com )
;; Version: 0.3.1
;; Created: 23 Dev 2023
;; Keywords: languages
;; Homepage: https://thethingsengine.org

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 3.

;;; Commentary:

;; A mode for ThingsEngine

;; How to use the full document

;;; Code:
(setq mymath-highlights
      '(("Sin\\|Cos\\|Sum" . 'font-lock-function-name-face)
        ("Pi\\|Infinity" . 'font-lock-constant-face)))

;;;###autoload
(define-derived-mode thingsengine-mode org-mode "ThingsEngine"
  "major mode for editing mymath language code."
  (setq font-lock-defaults '(mymath-highlights)))

(provide 'thingsengine-mode)
