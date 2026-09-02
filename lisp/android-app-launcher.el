;;; android-app-launcher.el --- Launch other Android apps from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Greg Silverstein

;; Author: Greg Silverstein <greg.silverstein@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, processes
;; URL: https://github.com/gsilvers/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Launch other Android applications from Emacs running on the Android port
;; (Emacs 30.1 or later, where `system-type' is `android').
;;
;; Emacs already has an intent bridge, `android-browse-url', and where it
;; suffices it is the better tool: it is a real intent sent from Emacs's own
;; Android context, needs no subprocess, and is subject to none of the
;; restrictions below.  But it builds its intent with
;;
;;     new Intent(Intent.ACTION_VIEW, Uri.parse(url))
;;
;; rather than `Intent.parseUri', so it can only reach an application that
;; has registered a URI scheme.  There is no way to name a package through
;; it.  Launching an arbitrary app therefore has to go through Android's
;; activity manager, which is what this file does.
;;
;; Everything here runs as Emacs's own application UID.  That is what makes
;; `--user 0' mandatory: without it `am' and `cmd' address themselves to
;; USER_CURRENT_OR_SELF (-2), and resolving "the current user" requires the
;; INTERACT_ACROSS_USERS permission, which no ordinary app holds.  Both are
;; then refused with:
;;
;;     Permission Denial: null asks to run as user -2
;;
;; Note that the shell has nothing to do with it.  Every process Emacs spawns
;; runs as Emacs's UID whatever shell binary it passes through, so pointing
;; `shell-file-name' at Termux's bash changes nothing -- Termux's UID is
;; likewise just another app UID.
;;
;; `monkey' is deliberately not used as a fallback.  It can launch by package
;; alone, without resolving an activity, but it injects input events and so
;; needs the `shell' UID (2000); from an app it fails with "Unable to connect
;; to window manager: is the system running?", and `--user 0' does not help.
;;
;; See android-app-launcher.org for a fuller account of `am', `pm', `cmd',
;; intents, and what an app UID may and may not do on Android.
;;
;; Usage:
;;
;;     (require 'android-app-launcher)
;;     M-x android-app-launcher-launch

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup android-app-launcher nil
  "Launch other Android applications from Emacs."
  :group 'external
  :prefix "android-app-launcher-")

(defcustom android-app-launcher-user "0"
  "Android user ID passed as `--user' to `am', `pm' and `cmd'.
Naming the user explicitly is required from an application UID; see
the Commentary.  \"0\" is the primary user and is almost always what
you want.  A secondary profile or work profile has a different ID,
which `pm list users' will report."
  :type 'string)

(defconst android-app-launcher--component-regexp
  "\\`[[:alnum:]_.]+/[[:alnum:]_.$]+\\'"
  "Regexp matching an Android PACKAGE/ACTIVITY component name.")

(defun android-app-launcher--shell (format-string &rest args)
  "Run a shell command built from FORMAT-STRING and ARGS, returning its output.
Standard error is folded into the result: these commands report refusals
there, and discarding it makes a permission problem look like an empty
answer."
  (shell-command-to-string
   (concat (apply #'format format-string args) " 2>&1")))

(defun android-app-launcher--component (output)
  "Return the PACKAGE/ACTIVITY component named in OUTPUT, or nil.
Every line is considered, because `cmd package resolve-activity --brief'
still prints a `priority=... preferredOrder=... isDefault=true' line and
the component is not reliably last."
  (seq-find (lambda (line)
              (string-match-p android-app-launcher--component-regexp line))
            (mapcar #'string-trim (split-string output "\n" t))))

(defun android-app-launcher-packages ()
  "Return a list of installed Android package names.
The list may be incomplete: Emacs does not declare QUERY_ALL_PACKAGES, so
on Android 11 and later the system may reveal only a subset.  These are
package names, not the labels shown on the home screen."
  (mapcar (lambda (line) (replace-regexp-in-string "\\`package:" "" line))
          (split-string
           (android-app-launcher--shell "pm list packages --user %s"
                                        android-app-launcher-user)
           "\n" t)))

(defun android-app-launcher-resolve (package)
  "Return the launcher component of PACKAGE, or nil if none can be resolved.
The component is a string of the form \"PACKAGE/ACTIVITY\"."
  (android-app-launcher--component
   (android-app-launcher--shell "cmd package resolve-activity --brief --user %s %s"
                                android-app-launcher-user
                                (shell-quote-argument package))))

;;;###autoload
(defun android-app-launcher-launch (package)
  "Launch the Android application PACKAGE, e.g. \"app.vanadium.browser\".
Resolve the package's launcher activity and start it with `am start'.
Interactively, complete over the installed packages.

When the activity cannot be resolved, report what the system actually
said rather than failing quietly -- that output is normally the only
clue as to why."
  (interactive (list (completing-read "App package: "
                                      (android-app-launcher-packages))))
  (let ((output (android-app-launcher--shell
                 "cmd package resolve-activity --brief --user %s %s"
                 android-app-launcher-user
                 (shell-quote-argument package))))
    (if-let* ((component (android-app-launcher--component output)))
        (message "%s" (string-trim
                       (android-app-launcher--shell
                        "am start --user %s -n %s"
                        android-app-launcher-user
                        (shell-quote-argument component))))
      (message "No launcher activity for %s: %s" package (string-trim output)))))

(provide 'android-app-launcher)
;;; android-app-launcher.el ends here
