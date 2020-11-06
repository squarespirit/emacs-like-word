(setq mw-key-topic "Editing")

;; Rebind to rectangle commands
(mw-global-set-key (kbd "M-r") (lookup-key (current-global-map) (kbd "C-x r")) "Register/rectangle cmds")

;; Backspace/delete should not copy to clipboard
;; http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun mw-delete-word-no-cut (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
     (progn
     (forward-word arg)
     (point))))
(defun mw-backward-delete-word-no-cut (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (mw-delete-word-no-cut (- arg)))
(bind-keys
 ("<C-backspace>" . mw-backward-delete-word-no-cut)
 ("<C-delete>" . mw-delete-word-no-cut)
 ;; M-backspace is already backward-kill-word.
 ;; M-backspace and M-delete both overwrite the clipboard. 
 ("<M-delete>" . kill-word))

(setq-default sentence-end-double-space nil)

;; change all yes-or-no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; No overwrite mode
(global-unset-key (kbd "<insert>"))

;; Warning: works ok on real code, doesn't work that well in org mode source blocks
(mw-global-set-key (kbd "C-/") 'comment-line "Comment line")

;; No Ctrl-home/end. I rarely intend to press it, though I often press it accidentally
(global-unset-key (kbd "<C-home>"))
(global-unset-key (kbd "<C-end>"))

;; Pass through win key, so Windows keyboard shortcuts still work
(setq w32-pass-lwindow-to-system t)

;; Mouse
;; Let shift-click extend selection
(bind-keys ("<S-down-mouse-1>" . mouse-save-then-kill))

(use-package crux)
(bind-keys
 ([remap move-beginning-of-line] . crux-move-beginning-of-line)
 ("C-k" . crux-smart-kill-line))
(mw-global-set-key (kbd "<S-return>") 'crux-smart-open-line "Insert line below")
(mw-global-set-key (kbd "C-S-j") 'crux-top-join-line "Join lines")
