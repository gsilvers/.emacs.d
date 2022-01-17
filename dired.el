(let ((map dired-mode-map))
  (define-key map (kbd "C-+") #'dired-create-empty-file)
  (define-key map (kbd "M-s f") #'nil)
  (define-key map (kbd "C-x v v") #'dired-vc-next-action))
