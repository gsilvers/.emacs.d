;; example

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("tromey" . "https://tromey.com/elpa/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'multtiple-cursors)
  (package-install 'multiple-cursors))

(require 'use-package)
(setq use-package-always-ensure t)

;; uncomment this as needed or run manually
;;(package-refresh-contents ) ;; also update from melpa etc

(when (memq window-system '(mac ns x))
     (setenv "PATH" (concat "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:" (getenv "PATH"))) ;; gets my homebrew installed python
     (setenv "PATH" (concat "/Users/gregsilverstein/Oracle/instantclient:" (getenv "PATH"))) ;; gets sqlplus and oracle directory in
     (setenv "PATH" (concat "/opt/homebrew/opt/openjdk:" (getenv "PATH"))) ;; stuff for java
     (setenv "PATH" (concat "/opt/homebrew/opt/openjdk/include:" (getenv "PATH"))) ;; stuff for java
     (setenv "PATH" (concat "/opt/homebrew/opt/openjdk/bin:" (getenv "PATH"))) ;; stuff for java
     (setenv "ORACLE_HOME"  "/Users/gregsilverstein/Oracle") ;; oracle stuff
     (setenv "DYLD_LIBRARY_PATH"  "/Users/gregsilverstein/Oracle/instantclient") ;; oracle stuff
     (setenv "OCI_LIB_DIR"  "/Users/gregsilverstein/Oracle/instantclient") ;; oracle stuff
     (setenv "OCI_INC_DIR"  "/Users/gregsilverstein/Oracle/instantclient/sdk/include") ;; oracle stuff
     (setenv "TNS_ADMIN"  "/Users/gregsilverstein/Oracle/network/wallet") ;; oracle stuff
     )

;; some basics
;;(scroll-bar-mode -1)          ; Disable visible scrollbar
(tool-bar-mode -1)            ; Disable the toolbar
;;(tooltip-mode -1)             ; Disable tooltips
(set-fringe-mode 10)          ; Give some breathing room
(server-start)
(tab-bar-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Set up the visible bell
;; (setq visible-bell t) ;; linux windows
;; (setq visible-bell nil)
(setq ring-bell-function (lambda ()  ;; macos
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; send backsups to one specific directory
(setq backup-directory-alist '(("." . "~/Organization/98_Emacs_Backups")))

(use-package term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp ".*>\s\]"))


(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp ".*>\s\]")  ;; Set this to match your custom shell prompt
  (setq vterm-max-scrollback 10000))

(if
(eq system-type 'windows-nt) 
  (progn
   (prefer-coding-system 'utf-8)
   (setq explicit-shell-file-name
    "C:/Users/csusggsn/AppData/Local/Programs/Git/bin/bash.exe")
   (setq shell-file-name explicit-shell-file-name)
   (add-to-list 'exec-path 
    "C:/Users/csusggsn/AppData/Local/Programs/Git/bin/bash.exe")
  ) 
)

(defun old_greg/org-mode-setup ()
  ;; (org-indent-mode) ;;Currently debating this bit
  (variable-pitch-mode 0)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun old_greg/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  )

(use-package org
  :hook (org-mode . old_greg/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (custom-set-faces
   '(org-block-begin-line ((t (:background "#bd93f9" :foreground "#f8f8f2"))))
   '(org-block-end-line   ((t (:background "#bd93f9" :foreground "#f8f8f2")))))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("planning" . ?p)
          ("note" . ?n)
          ("idea" . ?i)
          ("Personal" . ?x)
          ("Misc" . ?X)
          ("@Database Core" . ?C)
          ("@Database Web" . ?W)
          ("@Datavault" . ?D)
          ("@Misc Creditsafe" . ?M)
          ))


  (old_greg/org-font-setup))

(defun old_greg/org-mode-visual-fill ()
  (setq visual-fill-column-width 200 
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . old_greg/org-mode-visual-fill))

(if
    (eq system-type 'windows-nt) 
    (setq org-agenda-files '("C:\\Users\\csusggsn\\Organization\\todo.org"))
  )

(if
    (eq system-type 'darwin) 
    (setq org-agenda-files '("~/Organization/todo.org"))
  )

(global-set-key (kbd "C-c a") 'org-agenda)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(add-to-list 'load-path "~/.emacs.d/customizations")
(load "sqlplus.el")
(load "plsql.el")

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    ;;projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    ;;magit
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
;; (add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")
(load "dired+.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (magit tagedit rainbow-delimiters projectile smex ido-completing-read+ cider clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts

(use-package general
  :after evil
  :config
  (general-create-definer old_greg/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")

  (old_greg/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")
   "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)


  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-hl-line-mode 1)       ; HL Lines globally on
      (set-face-background hl-line-face "gray23")
      ;; hidpi
      ;;(set-face-attribute 'default nil :font "IBM Plex Mono" :height 170)
      ;; normal screens
      (set-face-attribute 'default nil :font "IBM Plex Mono" :height 135)

      (use-package doom-modeline
	:ensure t
	:init (doom-modeline-mode 1)
	:custom ((doom-modeline-height 13)))

      ;; NOTE: The first time you load your configuration on a new machine, you'll
      ;; need to run the following command interactively so that mode line icons
      ;; display correctly:
      ;;
      ;; M-x all-the-icons-install-fonts
      ;; icons 
      (use-package all-the-icons)

      (use-package doom-themes
	:init (load-theme 'doom-dracula t))

      (use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))

(setq doom-modeline-modal-icon nil)

(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                dired-mode-hook
                shell-mode-hook
                ;;org-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; python3 
(setq python-shell-interpreter "python3")

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config

  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package keycast
  :bind ("C-c t k" . +toggle-keycast)
  :config

  (defun +toggle-keycast()
    (interactive)
    (if (member '("" mode-line-keycast " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" mode-line-keycast " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast disabled"))
      (add-to-list 'global-mode-string '("" mode-line-keycast " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast enabled"))))


(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (add-to-list #'yas-snippet-dirs "my-personal-snippets")
  :diminish yas-minor-mode)


(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t)
  )

(global-set-key (kbd "C-<tab>") 'company-yasnippet)
(global-set-key (kbd "C-x <C-return>") 'window-swap-states)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit helpful counsel ivy-rich which-key rainbow-delimiters doom-themes doom-modeline ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
