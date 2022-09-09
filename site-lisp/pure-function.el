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
(provide 'pure-function)
;;; pure-function.el ends here


