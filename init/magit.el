(use-package magit
  :bind
  ("C-S-g" . magit-status)
  :config
  (setq magit-save-repository-buffers 'dontask))
