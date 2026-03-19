;;; svg-preview-mode.el --- Minor mode for live SVG previews  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: monodev <monodev@monospace.games>
;; Created: 16 Oct 2023
;; Keywords: multimedia
;; URL: https://monospace.games/misc/svg-preview-mode

;;; Commentary

;; Enable svg-preview-mode while editing SVG files to get live previews.

;; By default previews update when the buffer changes.
;; See `svg-preview-preferred-frequency' if you wish to change this.

;;; Code:

(make-variable-buffer-local
 (defvar svg-preview--image-buffer))

(defcustom svg-preview-preferred-frequency 'after-change
  "Preferred frequency of SVG preview updates.

Available options are: after-change after-save"
  :type 'symbol
  :options '(after-change after-save))

;; Mark buffer local variables as permanent.
;; This is required to make them persist major mode changes, as SVG files can
;; can switch between image-mode and nxml-mode.
(put 'svg-preview--image-buffer 'permanent-local t)

;; Instead of marking the minor mode as permanent we use a nxml mode hook
;; and a restore function, so that local hooks introduced by the minor mode
;; can be restored.
(defun svg-preview--restore ()
  (if svg-preview--image-buffer
      (svg-preview-mode)))
(add-hook 'nxml-mode-hook 'svg-preview--restore)

(defun svg-preview--update-view (&rest x y z)
  (if (buffer-live-p svg-preview--image-buffer)
      (progn
        (with-current-buffer svg-preview--image-buffer
          (fundamental-mode))
        (copy-to-buffer svg-preview--image-buffer (point-min) (point-max))
        (with-current-buffer svg-preview--image-buffer
          (image-mode)))))

(defun svg-preview--cleanup ()
  (remove-hook 'after-save-hook 'svg-preview--update-view t)
  (remove-hook 'after-change-functions 'svg-preview--update-view t)
  (if (buffer-live-p svg-preview--image-buffer)
      (let ((buf-win (get-buffer-window svg-preview--image-buffer)))
        (if buf-win (delete-window buf-win))
        (unless (with-current-buffer svg-preview--image-buffer
                  (bound-and-true-p svg-preview--kill-origin-is-preview))
          (kill-buffer svg-preview--image-buffer))))
  (setq svg-preview--image-buffer nil))

;;;###autoload
(define-minor-mode svg-preview-mode
  "Minor mode for live SVG previews.

By default previews update when the buffer changes.
See `svg-preview-preferred-frequency' if you wish to change this."
  :lighter nil
  :global nil
  :keymap (make-sparse-keymap)
  (if svg-preview-mode
      (progn
        (unless svg-preview--image-buffer
          (setq svg-preview--image-buffer
                (get-buffer-create (concat "*svg-preview-" (buffer-name) "*")))
          (let ((buf (current-buffer)))
            (with-current-buffer svg-preview--image-buffer
              (add-hook 'kill-buffer-hook
                        #'(lambda ()
                            (setq-local svg-preview--kill-origin-is-preview t)
                            (with-current-buffer buf
                              (svg-preview-mode -1)))
                        nil t)))
          (svg-preview--update-view)
          (display-buffer svg-preview--image-buffer
                          '(display-buffer-below-selected)))
        (if (eq svg-preview-preferred-frequency 'after-save)
            (add-hook 'after-save-hook 'svg-preview--update-view nil t)
          (add-hook 'after-change-functions 'svg-preview--update-view nil t))
        (add-hook 'kill-buffer-hook 'svg-preview--cleanup nil t))
    (svg-preview--cleanup)))

(provide 'svg-preview-mode)

;;; svg-preview-mode.el ends here
