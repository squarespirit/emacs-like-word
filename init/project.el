(use-package projectile
  :demand  ;; Should always be loaded
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  ;; Unbind this so ESC exits the command map
  (define-key projectile-command-map (kbd "ESC") nil)
  :bind-keymap (("M-p" . projectile-command-map)))
(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1)
  :bind (("C-S-f" . counsel-projectile-rg)
         ("C-S-n" . counsel-projectile-find-file)))
