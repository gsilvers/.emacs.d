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
(tool-bar-mode -1)            ; Disable the toolbar
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
   (setq python-shell-interpreter "C:\\Users\\csusggsn\\AppData\\Local\\Programs\\Python\\Python39\\python.exe")
       (setenv "PATH" (concat "C:\\Users\\csusggsn\\AppData\\Local\\Programs\\Python\\Python39" (getenv "PATH"))) ;; gets my homebrew installed python
   (setq explicit-shell-file-name        "C:\\Users\\csusggsn\\AppData\\Local\\Programs\\Git\\bin\\bash.exe")
   (setq shell-file-name explicit-shell-file-name)
   (add-to-list 'exec-path 
    "C:\\Users\\csusggsn\\AppData\\Local\\Programs\\Git\\bin\\bash.exe")
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
  (setq org-default-notes-file "~/Organization/02_Documents/inbox.org")
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

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
    (progn (setq org-agenda-files
                 (list
                  "C:\\Users\\csusggsn\\Organization\\02_Documents\\todo.org"
                  "C:\\Users\\csusggsn\\Organization\\02_Documents\\inbox.org"
                  )
                 )
           (setq org-refile-targets
                 '(
                   ("C:\\Users\\csusggsn\\Organization\\02_Documents\\todo.org" :maxlevel . 2)
                   ("C:\\Users\\csusggsn\\Organization\\02_Documents\\inbox.org" :maxlevel . 1)
                   ))
           )
  )
(if
    (eq system-type 'darwin) 
    (progn (setq org-agenda-files
                 (list
                  "~/Organization/02_Documents/todo.org"
                  "~/Organization/02_Documents/inbox.org")
                 )
           (setq org-refile-targets
                 '(("~/Organization/02_Documents/todo.org" :maxlevel . 2)
                   ("~/Organization/02_Documents/inbox.org" :maxlevel . 1)))

           )
  )
(global-set-key (kbd "C-c a") 'org-agenda)

(custom-set-faces
 '(org-block-begin-line
   ((t (:background "#212121" :extend t))))
 '(org-block
   ((t (:background "#252525" :extend t))))
 '(org-block-end-line
   ((t (:background "#212121" :extend t))))
 )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))


(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(add-to-list 'load-path "~/.emacs.d/customizations")
(load "sqlplus.el")
(load "plsql.el")


(require 'sqlplus)
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))

;;  If you want PL/SQL support also, try something like this:
;;
(require 'plsql)
(setq auto-mode-alist
      (append '(("\\.pls\\'" . plsql-mode) ("\\.pkg\\'" . plsql-mode)
                ("\\.pks\\'" . plsql-mode) ("\\.pkb\\'" . plsql-mode)
                ("\\.sql\\'" . plsql-mode) ("\\.PLS\\'" . plsql-mode) 
                ("\\.PKG\\'" . plsql-mode) ("\\.PKS\\'" . plsql-mode)
                ("\\.PKB\\'" . plsql-mode) ("\\.SQL\\'" . plsql-mode)
                ("\\.prc\\'" . plsql-mode) ("\\.fnc\\'" . plsql-mode)
                ("\\.trg\\'" . plsql-mode) ("\\.vw\\'" . plsql-mode)
                ("\\.PRC\\'" . plsql-mode) ("\\.FNC\\'" . plsql-mode)
                ("\\.TRG\\'" . plsql-mode) ("\\.VW\\'" . plsql-mode))
              auto-mode-alist ))

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
    ;;ido-completing-read+

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


(add-to-list 'load-path "~/.emacs.d/customizations")


      ;;;;
;; Customization
      ;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
;; (add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")
(load "erc_config.el")
;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
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

(global-set-key (kbd "C-M-j") 'buffer-menu)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-hl-line-mode 1)       ; HL Lines globally on
(set-face-background hl-line-face "gray22")
;; hidpi
;;(set-face-attribute 'default nil :font "IBM Plex Mono" :height 170)
;; normal screens
(set-face-attribute 'default nil :font "IBM Plex Mono" :height 135)

(use-package all-the-icons)

(modus-themes-load-vivendi)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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


(if
  (eq system-type 'windows-nt) 
  (progn 
    (setq python-shell-interpreter "python")                                    
  )
)
(if
  (eq system-type 'darwin) 
  (progn 
   (setq python-shell-interpreter "python3")
  )
)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(use-package command-log-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package keycast
  :bind ("C-c t k" . +toggle-keycast)
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-mode-line)."
    (defun +toggle-keycast()
      (interactive)
      (if (member '("" mode-line-keycast " ") global-mode-string)
          (progn (setq global-mode-string (delete '("" mode-line-keycast " ") global-mode-string))
                 (remove-hook 'pre-command-hook 'keycast--update)
                 (message "Keycast disabled"))
        (add-to-list 'global-mode-string '("" mode-line-keycast " "))
        (add-hook 'pre-command-hook 'keycast--update t)
        (message "Keycast enabled")))
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" mode-line-keycast))
)

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


(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package helpful)

(use-package yasnippet
  :ensure t
  :config
  (global-set-key (kbd "C-M-0") 'yas-expand)
  (yas-global-mode t)
  (add-to-list #'yas-snippet-dirs "my-personal-snippets")
  :diminish yas-minor-mode)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(use-package yasnippet-snippets
  :ensure t
  )
(global-set-key (kbd "M-/") 'company-yasnippet)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(defun efs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook 
      (
        (lsp-mode . efs/lsp-mode-setup)
        (python-mode . lsp)
        (lsp-mode . lsp-enable-which-key-integration)
        )
    :init
    (setq lsp-keymap-prefix "C-c 1")  ;; Or 'C-l', 's-l'
      :bind 
        (
          ("C-c 1 d" . lsp-describe-thing-at-point)
          ("C-c 1 f" . lsp-format-buffer)
          ("C-c 1 r" . lsp-rename)
        )
      )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  :config (setq lsp-ui-sideline-show-hover t
          lsp-ui-sideline-delay 0.5
          lsp-ui-doc-delay 5
          lsp-ui-sideline-ignore-duplicates t
          lsp-ui-doc-position 'bottom
          lsp-ui-doc-alignment 'frame
          lsp-ui-doc-header nil
          lsp-ui-doc-include-signature t
          lsp-ui-doc-use-childframe t)
  :bind (
        ("C-c 1 d" . lsp-ui-peek-find-definitions)
        ("C-c 1 g" . lsp-ui-peek-find-references)
        ("C-c 1 i" . lsp-ui-imenu))

  )

  (use-package lsp-treemacs
    :after lsp)

(global-set-key (kbd "C-x <C-return>") 'window-swap-states)

  (defun window-split-toggle ()
    "Toggle between horizontal and vertical split with two windows."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows!")
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))

  (use-package doom-modeline)
  (doom-modeline-mode)

  (setq doom-modeline-modal-icon nil)

  (display-time)

  (use-package goggles
    :ensure t
    :hook ((prog-mode text-mode) . goggles-mode)
    :config
    (setq-default goggles-pulse t))


  (global-set-key (kbd "C-c p") nil)

  (use-package cape
    :ensure t
    ;; Bind dedicated completion commands
    :bind (("C-c p p" . completion-at-point) ;; capf
           ("C-c p t" . complete-tag)        ;; etags
           ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
           ("C-c p f" . cape-file)
           ("C-c p k" . cape-keyword)
           ("C-c p s" . cape-symbol)
           ("C-c p a" . cape-abbrev)
           ("C-c p i" . cape-ispell)
           ("C-c p l" . cape-line)
           ("C-c p w" . cape-dict)
           ("C-c p \\" . cape-tex)
           ("C-c p _" . cape-tex)
           ("C-c p ^" . cape-tex)
           ("C-c p &" . cape-sgml)
           ("C-c p r" . cape-rfc1345))
    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-file)
                                          ;(add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-abbrev)
                                          ;(add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

  (setq-local completion-at-point-functions
              (list (cape-super-capf #'cape-dabbrev #'cape-file #'cape-keyword #'cape-symbol)))

  (use-package tempel
    :ensure t
    :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
           ("M-*" . tempel-insert))

    :init

    ;; Setup completion at point
    (defun tempel-setup-capf ()
      ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
      ;; only triggers on exact matches. Alternatively use `tempel-complete' if
      ;; you want to see all matches, but then Tempel will probably trigger too
      ;; often when you don't expect it.
      ;; NOTE: We add `tempel-expand' *before* the main programming mode Capf,
      ;; such that it will be tried first.
      (setq-local completion-at-point-functions
                  (cons #'tempel-expand
                        completion-at-point-functions)))

    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    ;; Optionally make the Tempel templates available to Abbrev,
    ;; either locally or globally. `expand-abbrev' is bound to C-x '.
    ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
    ;; (tempel-global-abbrev-mode)
    )

  (defun git-bash () (interactive)
         (prefer-coding-system 'utf-8)
         (let ((explicit-shell-file-name "C:\\Users\\csusggsn\\AppData\\Local\\Programs\\Git\\bin\\bash")) (setq explicit-bash.exe-args '("--login" "-i"))
              (call-interactively 'shell)))

(use-package osm
  :bind (("C-c m h" . osm-home)
	 ("C-c m s" . osm-search)
	 ("C-c m v" . osm-server)
	 ("C-c m t" . osm-goto)
	 ("C-c m x" . osm-gpx-show)
	 ("C-c m j" . osm-bookmark-jump))

  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information

  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

    (custom-set-variables
    '(markdown-command "C:\\Pandoc\\pandoc.exe"))  

    (global-set-key (kbd "C-c <left>") 'tab-bar-switch-to-prev-tab)
    (global-set-key (kbd "C-c <right>") 'tab-bar-switch-to-next-tab)
