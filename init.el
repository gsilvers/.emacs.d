;; Read org file with axtual settings in blocks

(require 'org)
(org-babel-load-file
 (expand-file-name "readme.org"
                   user-emacs-directory))


