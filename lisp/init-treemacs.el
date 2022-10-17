(defun icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and t
       (or (display-graphic-p) (daemonp))
       (or (featurep 'all-the-icons)
           (require 'all-the-icons nil t))))

(with-no-warnings
       (when (icon-displayable-p)
         (treemacs-create-theme "Default"
           ;;:extends "doom-colors"
           :config
           (progn
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
              :extensions (root))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (boolean-data))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
              :extensions (class))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
              :extensions (color-palette))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
              :extensions (constant))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
              :extensions (document))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
              :extensions (enumerator))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
              :extensions (enumitem))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
              :extensions (event))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (field))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
              :extensions (indexer))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
              :extensions (intellisense-keyword))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
              :extensions (interface))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (localvariable))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
              :extensions (method))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
              :extensions (namespace))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
              :extensions (numeric))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
              :extensions (operator))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
              :extensions (property))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
              :extensions (snippet))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
              :extensions (string))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
              :extensions (structure))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
              :extensions (template))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
              :extensions (collapsed) :fallback "+")
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
              :extensions (expanded) :fallback "-")
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions (classfile))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
              :extensions (default-folder-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
              :extensions (default-folder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
              :extensions (default-root-folder-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
              :extensions (default-root-folder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions ("class"))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions (file-type-jar))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (folder-open))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
              :extensions (folder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
              :extensions (folder-type-component-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
              :extensions (folder-type-component))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
              :extensions (folder-type-library-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
              :extensions (folder-type-library))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
              :extensions (folder-type-maven-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
              :extensions (folder-type-maven))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
              :extensions (folder-type-package-opened))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
              :extensions (folder-type-package))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-create))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-flat))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
              :extensions (icon-hierarchical))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-link))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-refresh))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (icon-unlink))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
              :extensions (jar))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
              :extensions (library))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
              :extensions (packagefolder-open))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
              :extensions (packagefolder))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
              :extensions (package))
             (treemacs-create-icon
              :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
              :extensions (java-project))))))





;;--------------------------------------------------------------------
 (provide 'init-treemacs)
