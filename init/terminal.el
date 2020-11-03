(defun switch-to-eshell ()
  "Switch to eshell, or hide it if you are already inside eshell."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (delete-window)
    (eshell)))
(global-set-key (kbd "C-`") 'switch-to-eshell)
