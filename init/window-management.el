(use-package ace-window
  :bind (("<f9>" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(setq mw-key-topic "Windows")
(mw-global-set-key (kbd "<S-escape>") 'keyboard-escape-quit "Close other windows")
(mw-global-set-key (kbd "M-o") 'other-window "Switch window")
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
