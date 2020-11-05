(windmove-default-keybindings)
(bind-keys ("<S-escape>" . keyboard-escape-quit))
(bind-keys ("M-o" . other-window))
(use-package transpose-frame
  :config
  (defun my-rotate-frame-clockwise-with-treemacs ()
    "Rotate windows clockwise, properly showing/hiding treemacs because it interferes."
    (interactive)
    (if (eq (treemacs-current-visibility) 'visible)
	(progn
	  (treemacs)
	  (rotate-frame-clockwise)
	  (treemacs)
          ;; Hack because at this point, the treemacs window is selected. Probably not desired
        (other-window 1))
      (rotate-frame-clockwise)))
  ;;(global-set-key (kbd "M-w") 'my-rotate-frame-clockwise-with-treemacs)
)
