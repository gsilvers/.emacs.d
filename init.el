;;; ====================================================================================
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
    (require 'package)
    
    (setq package-archives
	  '(("melpa" . "https://melpa.org/packages/")
            ("gnu"   . "https://elpa.gnu.org/packages/")))
    
    (package-initialize)
    (setq make-backup-files nil)
    (setq auto-save-default nil)
    (setq create-lockfiles nil)

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
	corfu))

    (dolist (pkg my/packages)
      (unless (package-installed-p pkg)
	(package-install pkg)))

    (load-theme 'ef-bio :no-confirm)
    (setq modus-themes-italic-constructs t
	  modus-themes-bold-constructs t)
    (set-face-attribute 'default nil :height 160)

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

    (global-set-key (kbd "<volume-up>") 'scroll-down-command)
    (global-set-key (kbd "<volume-down>") 'scroll-up-command)

    ))
;;; End Universal Setup
;;; ====================================================================================

;;; ====================================================================================
;;; Macos Setup
(defun greg/macos-setup-items ()
  "Items to be executed when logged in on macos only"
  (progn
    (set-desktop-size)
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
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
