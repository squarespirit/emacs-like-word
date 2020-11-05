(use-package browse-kill-ring
  :bind
  ("M-v" . browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings)
  (bind-keys :map browse-kill-ring-mode-map
	     ("<down>" . browse-kill-ring-forward)
	     ("<up>" . browse-kill-ring-previous)))
