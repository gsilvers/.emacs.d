;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(setq grow-five-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "r") #'grow-by-five)
        map))

(defun grow-by-five ()
  "just doing 5 col resize"
  (interactive)
  (enlarge-window-horizontally 5)
  (set-transient-map grow-five-keymap)
) 


;; This was from a chat a user was trying to use repeat command 
;; but it was not keeping the 5
;; transient map seems cool you could make a mini menu
;; of predicates fro one command. Like split
;; and then veritcal or horizontal 
;; or maybe even better something for tab bar mode?
