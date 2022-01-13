;; Read org file with axtual settings in blocks

(require 'org)
(org-babel-load-file
 (expand-file-name "readme.org"
                   user-emacs-directory))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   '(markdown-mode powershell multiple-cursors nyan-mode magit helpful counsel ivy-rich which-key rainbow-delimiters doom-themes doom-modeline ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:background "#bd93f9" :foreground "#f8f8f2"))))
 '(org-block-end-line ((t (:background "#bd93f9" :foreground "#f8f8f2")))))
