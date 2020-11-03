(use-package multiple-cursors
  :bind
  (("C-d" . mc/mark-next-like-this-symbol)
   ("M-<down-mouse-1>" . nil)
   ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  ;; Insert newline in multiple cursors; C-g can still be used to
  ;; cancel multi cursors
  (define-key mc/keymap (kbd "<return>") nil)) 
