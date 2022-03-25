
;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))

;; Read org file with axtual settings in blocks
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))
(org-babel-load-file
 (expand-file-name "readme.org"
                   user-emacs-directory))

(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   '(markdown-preview-eww osm dirvish powershell company-go marginalia yasnippet-snippets which-key vterm visual-fill-column vertico use-package typescript-mode tagedit smex rainbow-delimiters paredit orderless nyan-mode multiple-cursors modus-themes magit lsp-ui lsp-treemacs lsp-ivy keycast ivy-rich ido-completing-read+ helpful general exec-path-from-shell evil-collection eterm-256color doom-themes doom-modeline counsel-projectile company-box command-log-mode clojure-mode-extra-font-locking cider better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#252525" :extend t))))
 '(org-block-begin-line ((t (:background "#212121" :extend t))))
 '(org-block-end-line ((t (:background "#212121" :extend t))))
 '(vertico-current ((t (:background "#3a3f5a")))))
