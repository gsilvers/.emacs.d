
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
 '(custom-safe-themes
   '("02fff7eedb18d38b8fd09a419c579570673840672da45b77fde401d8708dc6b5" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" default))
 '(markdown-command "C:\\Pandoc\\pandoc.exe")
 '(package-selected-packages
   '(ewal-evil-cursors ewal-doom-themes ewal emojify sly with-emacs all-the-icons-completion markdown-preview-eww osm dirvish powershell company-go marginalia yasnippet-snippets which-key vterm visual-fill-column vertico use-package typescript-mode tagedit smex rainbow-delimiters paredit orderless nyan-mode multiple-cursors modus-themes magit lsp-ui lsp-treemacs lsp-ivy keycast ivy-rich ido-completing-read+ helpful general exec-path-from-shell evil-collection eterm-256color doom-themes doom-modeline counsel-projectile company-box command-log-mode clojure-mode-extra-font-locking cider better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#252525" :extend t))))
 '(org-block-begin-line ((t (:background "#212121" :extend t))))
 '(org-block-end-line ((t (:background "#212121" :extend t))))
 '(vertico-current ((t (:background "#3a3f5a")))))
