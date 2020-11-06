(setq mw-key-topic "Project")
(use-package projectile
  :demand  ;; Should always be loaded
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  ;; Unbind this so ESC exits the command map
  (define-key projectile-command-map (kbd "ESC") nil)
  (setq mw-key-topic "Project")
  (mw-global-set-key (kbd "M-p") 'projectile-command-map "Project commands..."))
(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1)
  (setq mw-key-topic "Project")
  (mw-global-set-key (kbd "C-S-f") 'counsel-projectile-rg "Search in project")
  (mw-global-set-key (kbd "C-n") 'counsel-projectile-find-file "Open file in project")
)

(use-package treemacs
  :config
  (treemacs-follow-mode +1)
  (define-key treemacs-mode-map (kbd "<mouse-1>") #'treemacs-single-click-expand-action)
  ;; https://github.com/hlissner/doom-emacs/issues/1177#issuecomment-464405628
;;  (defun my-treemacs-back-and-forth ()
;;    (interactive)
;;    (if (treemacs-is-treemacs-window-selected?)
;;  	(other-window 1)
;;      (treemacs-select-window)))
  (defun my-treemacs-back-and-forth ()
    "If in treemacs, close it. Otherwise, select it."
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
	(treemacs-quit)
      (treemacs-select-window)))
  ;; Cannot be bound with :bind because it's my own function defined outside the pkg
  (setq mw-key-topic "Project")
  (mw-global-set-key (kbd "C-'") 'my-treemacs-back-and-forth "File browser"))
