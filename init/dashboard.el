(use-package dashboard
  ;; Because keys are bound, it's deferred by default...
  ;; but want it to be autoloaded.
  :demand
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
;;                          (bookmarks . 5)
                          (projects . 5)))
;; Don't generate agenda on startup, so that we don't load a bunch of org files at startup
;;                           (agenda . 5)
  (setq dashboard-set-footer nil))
  ;; Keybinds don't work
  ;; (define-key dashboard-mode-map (kbd "p") 'counsel-projectile-switch-project)
  ;; (define-key dashboard-mode-map (kbd "r") 'counsel-recentf)
