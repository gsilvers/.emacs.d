;;; ====================================================================================  -*- lexical-binding: t; -*-
;;; init.el Base Emacs Configuration
;;; Greg Silverstein
;;; ====================================================================================
;;; This emacs configuration is deisgned to work on both android phone and
;;; on my desktop / laptop.
;;; This code is broken up into items done regardless and then conditional sections
;;; which execute only based on the appropriate enviornment.
;;; ====================================================================================


;;; ====================================================================================
;;; Universal Setup
(defun greg/universal-setup-items ()
  "Items which are executed no matter what"
  (progn

    (require 'server)
    (unless (server-running-p) (server-start))
    
 
    (require 'package)
    
    (setq package-archives
	  '(("melpa" . "https://melpa.org/packages/")
	    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("gnu"   . "https://elpa.gnu.org/packages/")))

    (package-refresh-contents)
    (package-initialize)
    (setq make-backup-files nil)
    (setq auto-save-default nil)
    (setq create-lockfiles nil)
    (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook #'visual-line-mode)

    (unless package-archive-contents
      (package-refresh-contents))
    
    (defvar my/packages
      '(modus-themes
	ef-themes
	vertico
	orderless
	marginalia
	consult
	embark
	embark-consult
	use-package
	corfu))

    (dolist (pkg my/packages)
      (unless (package-installed-p pkg)
	(package-install pkg)))

    (load-theme 'ef-bio :no-confirm)
    (setq modus-themes-italic-constructs t
	  modus-themes-bold-constructs t)
    (set-face-attribute 'default nil :family "Comic Mono" :height 160)

    (vertico-mode 1)
    (savehist-mode 1)

    (setq completion-styles '(orderless)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles partial-completion))))

    (marginalia-mode 1)
    (global-set-key (kbd "C-s") #'consult-line)
    (global-set-key (kbd "M-y") #'consult-yank-pop)

    
    (global-set-key (kbd "C-.") #'embark-act) ;; pick some keybinding
    (global-set-key (kbd "C-;") #'embark-dwim)
    (global-set-key (kbd "C-h B") #'embark-bindings)


    (column-number-mode)
    (global-display-line-numbers-mode 1)

    ;; Disable line numbers in terminal-like buffers (they're slow + visually noisy there)
    (defun greg/disable_line_numbers ()
      "Disable `display-line-numbers-mode' in the current buffer."
      (display-line-numbers-mode -1))
    (add-hook 'vterm-mode-hook #'greg/disable_line_numbers)
    (add-hook 'eshell-mode-hook #'greg/disable_line_numbers)
   
    (with-eval-after-load 'embark
      (require 'embark-consult))

    (setq corfu-auto t
	  corfu-cycle t
	  corfu-preview-current nil)
    (global-corfu-mode 1)
    (setq corfu-popupinfo-delay 0.2)
    (corfu-popupinfo-mode 1)

    
    (setq tab-always-indent 'complete)

    (defun set-desktop-size ()
      "Sets fonts etc for desktop mode / size"
      (interactive
       (progn
	 (set-face-attribute 'default nil :height 80))))


    (defun set-mobile-size ()
      "Sets fonts etc for mobile mode / size"
      (interactive
       (progn
	 (set-face-attribute 'default nil :height 130))))

    (defun log-bike-ride (duration distance elevation)
      "Prompt for a bike ride log and append it to ~/bike_log.csv.
Prompts for DURATION, DISTANCE, and ELEVATION gained, and appends
a line to the file with today's date."
      (interactive
       (list
	(read-string "Duration (e.g., 1h30m): ")
	(read-string "Distance (km or mi): ")
	(read-string "Elevation gain (m or ft): ")))
      (let* ((date (format-time-string "%Y-%m-%d"))
             (line (format "%s,%s,%s,%s\n" date duration distance elevation))
             (file (expand-file-name "~/bike_log.csv")))
	(with-temp-buffer
	  (insert line)
	  (append-to-file (point-min) (point-max) file))
	(message "Logged ride on %s." date)))

    (use-package magit
      :ensure t
      :bind (("C-x g" . magit-status)
	     ("C-x C-g" . magit-status))
      :custom
      (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

    (use-package markdown-mode
      :ensure t
      :mode ("README\\.md\\'" . gfm-mode)
      :init (setq markdown-command "multimarkdown")
      :bind (:map markdown-mode-map
		  ("C-c C-e" . markdown-do)))

    
    (use-package selected-window-accent-mode
      :ensure t 
      :config (selected-window-accent-mode 1)
      :custom
      (selected-window-accent-fringe-thickness 10)
      (selected-window-accent-custom-color "#CC5500")
      (selected-window-accent-mode-style 'subtle))


    ;; projectile basics
    (use-package projectile
      :ensure t
      :diminish projectile-mode
      :config (projectile-mode)
      :bind-keymap
      ("C-c p" . projectile-command-map)
      :init
      ;; NOTE: Set this to the folder where you keep your Git repos!
      (when (file-directory-p "~/")
	(setq projectile-project-search-path '("~/")))
      (setq projectile-switch-project-action #'projectile-dired))


    ;; perspective mode
    (use-package perspective
      :ensure t 
      :bind
      ("C-x C-b" . persp-list-buffers)
      :custom
      (persp-mode-prefix-key (kbd "C-x C-p"))
      :init
      (persp-mode))

    (use-package which-key
      :ensure t
      :init (which-key-mode)
      :diminish which-key-mode
      :config
      (setq which-key-idle-delay 1))
    (setq ivy-initial-inputs-alist nil)
     

    ))
;;; End Universal Setup
;;; ====================================================================================

;;; ====================================================================================
;;; Macos Setup
(defun greg/macos-setup-items ()
  "Items to be executed when logged in on macos only"
  (progn
    (set-desktop-size)

    (use-package vterm
      :ensure t
      :commands vterm
      :config
      (setq term-prompt-regexp ".*>\s\]")
      (setq vterm-max-scrollback 10000)
      (setq vterm-shell "/bin/bash --login"))

    (use-package consult
      :ensure t
      ;; Replace bindings. Lazily loaded due by `use-package'.
      :bind (;; C-c bindings in `mode-specific-map'
	     ("C-c M-x" . consult-mode-command)
	     ("C-c h" . consult-history)
	     ("C-c k" . consult-kmacro)
	     ("C-c m" . consult-man)
	     ("C-c i" . consult-info)
	     ([remap Info-search] . consult-info)
	     ;; C-x bindings in `ctl-x-map'
	     ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
	     ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
	     ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	     ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
	     ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
	     ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
	     ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
	     ;; Custom M-# bindings for fast register access
	     ("M-#" . consult-register-load)
	     ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
	     ("C-M-#" . consult-register)
	     ;; Other custom bindings
	     ("M-y" . consult-yank-pop) ;; orig. yank-pop
	     ;; M-g bindings in `goto-map'
	     ("M-g e" . consult-compile-error)
	     ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
	     ("M-g g" . consult-goto-line) ;; orig. goto-line
	     ("M-g M-g" . consult-goto-line) ;; orig. goto-line
	     ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
	     ("M-g m" . consult-mark)
	     ("M-g k" . consult-global-mark)
	     ("M-g i" . consult-imenu)
	     ("M-g I" . consult-imenu-multi)
	     ;; M-s bindings in `search-map'
	     ("M-s d" . consult-find) ;; Alternative: consult-fd
	     ("M-s c" . consult-locate)
	     ("M-s g" . consult-grep)
	     ("M-s G" . consult-git-grep)
	     ("M-s r" . consult-ripgrep)
	     ("M-s l" . consult-line)
	     ("M-s L" . consult-line-multi)
	     ("M-s k" . consult-keep-lines)
	     ("M-s u" . consult-focus-lines)
	     ;; Isearch integration
	     ("M-s e" . consult-isearch-history)
	     :map isearch-mode-map
	     ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
	     ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
	     ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
	     ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
	     ;; Minibuffer history
	     :map minibuffer-local-map
	     ("M-s" . consult-history) ;; orig. next-matching-history-element
	     ("M-r" . consult-history)) ;; orig. previous-matching-history-element

      ;; Enable automatic preview at point in the *Completions* buffer. This is
      ;; relevant when you use the default completion UI.
      :hook (completion-list-mode . consult-preview-at-point-mode)

      ;; The :init configuration is always executed (Not lazy)
      :init

      ;; Optionally configure the register formatting. This improves the register
      ;; preview for `consult-register', `consult-register-load',
      ;; `consult-register-store' and the Emacs built-ins.
      (setq register-preview-delay 0.5
	    register-preview-function #'consult-register-format)

      ;; Optionally tweak the register preview window.
      ;; This adds thin lines, sorting and hides the mode line of the window.
      (advice-add #'register-preview :override #'consult-register-window)

      ;; Use Consult to select xref locations with preview
      (setq xref-show-xrefs-function #'consult-xref
	    xref-show-definitions-function #'consult-xref)

      ;; Configure other variables and modes in the :config section,
      ;; after lazily loading the package.
      :config

      ;; Optionally configure preview. The default value
      ;; is 'any, such that any key triggers the preview.
      ;; (setq consult-preview-key 'any)
      ;; (setq consult-preview-key "M-.")
      ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
      ;; For some commands and buffer sources it is useful to configure the
      ;; :preview-key on a per-command basis using the `consult-customize' macro.
      (consult-customize
       consult-theme :preview-key '(:debounce 0.2 any)
       consult-ripgrep consult-git-grep consult-grep
       consult-bookmark consult-recent-file consult-xref
       consult--source-bookmark consult--source-file-register
       consult--source-recent-file consult--source-project-recent-file
       ;; :preview-key "M-."
       :preview-key '(:debounce 0.4 any))

      ;; Optionally configure the narrowing key.
      ;; Both < and C-+ work reasonably well.
      (setq consult-narrow-key "<") ;; "C-+"

      ;; Optionally make narrowing help available in the minibuffer.
      ;; You may want to use `embark-prefix-help-command' or which-key instead.
      ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

      ;; By default `consult-project-function' uses `project-root' from project.el.
      ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
      ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
      ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
      ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
      ;; (autoload 'projectile-project-root "projectile")
      ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
      ;; (setq consult-project-function nil)
      )

    (use-package typescript-mode
      :ensure t
      :mode "\\.ts\\'"
      :hook (typescript-mode . eglot-ensure))

    (use-package js
      :ensure t
      :mode "\\.js\\'"
      :hook (js-mode . eglot-ensure))

    (use-package python
      :ensure nil
      :init
      (setq major-mode-remap-alist
	    '((python-mode . python-ts-mode)))
      (setq treesit-language-source-alist
	    '((python "https://github.com/tree-sitter/tree-sitter-python")))
      :hook
      (python-ts-mode . eglot-ensure))


    (use-package eglot
      :defer t
      :bind (:map eglot-mode-map
		  ("C-c l a" . eglot-code-actions)
		  ("C-c l r" . eglot-rename)
		  ("C-c l f" . eglot-format)
		  ("C-c l d" . eldoc))
      :custom (eglot-report-progress nil)
      :config
      (setq read-process-output-max (* 1024 1024))
      (setq eglot-events-buffer-size 0)

      ;; Force Eglot to use Pyright for python-mode
      (add-to-list 'eglot-server-programs
		   `(python-mode . ("basedpyright-langserver" "--stdio")))
      (add-to-list 'eglot-server-programs
		   `(python-ts-mode . ("basedpyright-langserver" "--stdio")))

      (add-to-list 'eglot-server-programs
		   `(typescript-mode . ("typescript-language-server" "--stdio")))
      (add-to-list 'eglot-server-programs
		   `(js-mode . ("typescript-language-server" "--stdio"))))

    (setq-default eglot-workspace-configuration
		  '(:pyright (:venvPath "."
					:venv ".venv"
					:pythonVersion "3.13")))
    ))
;;; End Macos Setup
;;; ====================================================================================

;;; ====================================================================================
;;; Android Setup
(defun greg/android-setup-items ()
  "Items to be executed on an androidj phone environment only"
  (progn
    (push "/data/data/com.termux/files/usr/bin" exec-path)
    (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
			   (getenv "PATH")))

    (menu-bar-mode 1)
    (tool-bar-mode 1)
    (scroll-bar-mode 1)
    (modifier-bar-mode 1)
    (menu-bar-set-tool-bar-position `bottom)
    (set-mobile-size)
    
    (global-set-key (kbd "<volume-up>") 'scroll-down-command)
    (global-set-key (kbd "<volume-down>") 'scroll-up-command)
    
    ))
;;; End Android Setup
;;; ====================================================================================

;;; ====================================================================================
;;; Actual initialization Happens Here!
;;; ====================================================================================
(greg/universal-setup-items)
(if (eq system-type `darwin)
    (progn
      (message (format "Loading Mac Config: %s" system-type))
      (greg/macos-setup-items)
      )
  (if (eq system-type `android)
      (progn
	(message (format "Loading Android Config: %s" system-type))
	(greg/android-setup-items)
	)
    (progn
      (message (format "Unexpected Operating System Type Encountered: %s" system-type))
      (display-warning :warning (format "Unexpected Operating System Type Encountered: %s" system-type))
      )))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(corfu eat ef-themes embark-consult magit marginalia markdown-mode
	   orderless perspective projectile
	   selected-window-accent-mode typescript-mode vertico vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
