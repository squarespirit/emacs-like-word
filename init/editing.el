;; Rebind some stuff that causes me pain
(bind-keys
 ;; Easier to hit. In particular, the mark can be set by one hand.
 ("M-SPC" . set-mark-command))

;; Should be on by default
(delete-selection-mode 1)

(setq-default sentence-end-double-space nil)

;; change all yes-or-no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; No overwrite mode
(global-unset-key (kbd "<insert>"))

;; Warning: works ok on real code, doesn't work that well in org mode source blocks
(bind-keys ("C-/" . comment-line))

;; No Ctrl-home/end. I rarely intend to press it, though I often press it accidentally
(global-unset-key (kbd "<C-home>"))
(global-unset-key (kbd "<C-end>"))

;; Pass through win key, so Windows keyboard shortcuts still work
(setq w32-pass-lwindow-to-system t)

;; Mouse
;; Let shift-click extend selection
(bind-keys ("<S-down-mouse-1>" . mouse-save-then-kill))
