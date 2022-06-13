;; (defun jk/org-insert-headline (level)
;;   "Insert `level' * ahead of current line."
;;   (interactive "swhich level: ")
;;   (jk/org-delete-headline)
;;   (let ((x 0) (len (string-to-number level)))
;;     (while (< x len)
;;       (if (= len (+ x 1))
;;           (insert "* ")
;;         (insert "*")
;;         )
;;     (setq x (+ x 1)))))

;; (global-set-key (kbd "C-c C-h") 'jk/org-insert-headline)
