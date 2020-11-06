(setq mw-key-topic "Buffers")

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  ;; (setq centaur-tabs-set-icons t) ; Unfortunately, icons are very slow on windows...
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-cycle-scope 'tabs)
  (defun centaur-tabs-buffer-groups ()
   (list
    (if (mw-is-buffer-ignored (buffer-name)) "Emacs" "User")))
  ;; I never want to switch to the Emacs buffers anyway.
  ;; This is naive but at least reduces confusion about multiple groups
  ;; (defun centaur-tabs-buffer-groups () (list "single-group"))
  ;; Don't show because of autosave
  ;; (setq centaur-tabs-set-modified-marker t)
  ;; (setq centaur-tabs-modified-marker "●")
  ;; https://github.com/ema2159/centaur-tabs/blob/master/centaur-tabs-elements.el
  (set-face-attribute 'centaur-tabs-selected nil :background "#FDFDFD" :foreground "black")
  (set-face-attribute 'centaur-tabs-selected-modified nil :background "#FDFDFD" :foreground "black")
  (set-face-attribute 'centaur-tabs-unselected nil :background "#CCCCCC" :foreground "black")
  (set-face-attribute 'centaur-tabs-unselected-modified nil :background "#CCCCCC" :foreground "black")

  (setq mw-key-topic "Buffers")
  (mw-global-set-key (kbd "C-<prior>") 'centaur-tabs-backward "Previous tab")
  (mw-global-set-key (kbd "C-<next>") 'centaur-tabs-forward "Next tab")
  (mw-global-set-key (kbd "C-S-<prior>") 'centaur-tabs-move-current-tab-to-left "Move tab left")
  (mw-global-set-key (kbd "C-S-<next>") 'centaur-tabs-move-current-tab-to-right "Move tab right"))
