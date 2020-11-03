(use-package multiple-cursors
  :demand ;; depend on it, but 
  :bind
  (("M-<down-mouse-1>" . nil)
   ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  ;; Insert newline in multiple cursors; C-g can still be used to
  ;; cancel multi cursors
  (define-key mc/keymap (kbd "<return>") nil))

(defun mw-select-symbol ()
  "Select symbol at point"
  (interactive)
  ;; If symbol not found, this will error, exiting the function
  (beginning-of-thing 'symbol)
  (set-mark-command nil)
  (end-of-thing 'symbol))

(defun mw-select-symbol-or-next ()
  "If region is inactive, select symbol at point. Otherwise select next occurrence of region"
  (interactive)
  (if (use-region-p)
      (mc/mark-next-like-this 1)
    (mw-select-symbol)))

(defun mw-unselect-next ()
  (interactive)
  (mc/mark-next-like-this -1))

(bind-keys ("C-d" . mw-select-symbol-or-next)
	   ("C-S-d" . mw-unselect-next))

;; These commands should not be run for all cursors
(dolist
    (f '(mw-select-symbol-or-next mw-unselect-next))
  (add-to-list 'mc/cmds-to-run-once f))
