(use-package ace-window
  :bind (("<f9>" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(bind-keys
 ("<f10>" . delete-other-windows)
 ;; Easy to hit accidentally
 ("<f11>" . nil))
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
