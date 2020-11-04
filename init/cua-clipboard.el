;; This file controls settings for CUA mode and the clipboard,
;; because they are intimately related.

;; Rebind CUA rectangle selection (conflicts with org C-ret)
;; Needs to happen before turning on cua mode
(setq cua-rectangle-mark-key (kbd "<C-M-return>"))
;; Do not rebind C-v so that major modes can override it.
(setq-default cua-remap-control-v nil)
;; I don't live in emacs yet...
(cua-mode t)

;; Cua-mode implies delete-selection mode though.
;; (delete-selection-mode 1)

;; Remap these keys to the C-x/C-c keymaps.
;; Using C-S-x means that they work in CUA mode even if mark is active.
(define-key key-translation-map (kbd "<f5>") (kbd "C-S-x"))
(define-key key-translation-map (kbd "<f6>") (kbd "C-S-c"))

;; Bind both to paste - but major modes can override C-v.
(bind-keys
 ("C-v" . cua-paste)
 ("C-S-v" . cua-paste))

;; https://www.emacswiki.org/emacs/CopyingWholeLines#toc7
(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))
(bind-keys
 ("C-y" . kill-whole-line)
 ("M-y" . quick-copy-line))


(use-package browse-kill-ring
  :bind
  ("M-v" . browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings)
  (bind-keys :map browse-kill-ring-mode-map
	     ("<down>" . browse-kill-ring-forward)
	     ("<up>" . browse-kill-ring-previous)))
