;; Rebind some stuff that causes me pain
(require 'bind-key)
(bind-keys
 ;; Easier to hit.
 ("M-SPC" . cua-set-mark))
 ;;("M-/" . exchange-point-and-mark)
;; Rebind to rectangle commands
(bind-key "M-r" (lookup-key (current-global-map) (kbd "C-x r")))

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
(bind-keys ("C-/" . comment-line))

;; No Ctrl-home/end. I rarely intend to press it, though I often press it accidentally
(global-unset-key (kbd "<C-home>"))
(global-unset-key (kbd "<C-end>"))

;; Pass through win key, so Windows keyboard shortcuts still work
(setq w32-pass-lwindow-to-system t)

;; Mouse
;; Let shift-click extend selection
(bind-keys ("<S-down-mouse-1>" . mouse-save-then-kill))
