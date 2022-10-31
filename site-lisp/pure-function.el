;;; pure-function.el --- Some useful functions. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Ethan YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Some useful functions to facilitate using Emacs
;;--------------------------------------------------------------------
;;; Code:

(defun TE-decimalNumber-to-binary-string (number)
  (require 'calculator)
  (let ((calculator-output-radix 'bin)
        (calculator-radix-grouping-mode nil))
    (calculator-number-to-string number)))

(defun my-subseq (foo n m)
  (let ((l (length foo)))
        (setq sub1 (nthcdr n foo))
        (setq sub2 (butlast sub1 (- l m)))
        sub2))

(defun marginalia-annotate-command (cand)
  "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-symbol', but does not show symbol class."
  (when-let* ((sym (intern-soft cand))
              (mode (if (boundp sym)
                        sym
                      (lookup-minor-mode-from-indicator cand))))
    (concat
     (if (and (boundp mode) (symbol-value mode))
         (propertize " On" 'face 'marginalia-on)
       (propertize " Off" 'face 'marginalia-off))
     (marginalia-annotate-binding cand)
     (marginalia--documentation (marginalia--function-doc sym)))))

;;--------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/OpenNextLine
;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-o") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")
;;--------------------------------------------------------------------
(provide 'pure-function)
;;; pure-function.el ends here


