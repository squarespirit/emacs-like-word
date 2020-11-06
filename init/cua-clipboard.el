;; This file controls settings for CUA mode and the clipboard,
;; because they are intimately related.
(setq mw-key-topic "Editing")

;; Rebind CUA rectangle selection (conflicts with org C-ret)
;; Needs to happen before turning on cua mode
(setq cua-rectangle-mark-key (kbd "<C-M-return>"))
;; Do not rebind C-v so that major modes can override it.
(setq-default cua-remap-control-v nil)
;; I don't live in emacs yet...
(cua-mode t)
(mw-global-doc-key (kbd "C-x") "Cut")
(mw-global-doc-key (kbd "C-c") "Copy")

(mw-global-set-key (kbd "M-SPC") 'cua-set-mark "Set mark")

;; Cua-mode implies delete-selection mode though.
;; (delete-selection-mode 1)

;; Remap these keys to the C-x/C-c keymaps.
;; Using C-S-x means that they work in CUA mode even if mark is active.
;; Practically, I rarely use them
;; (define-key key-translation-map (kbd "<f5>") (kbd "C-S-x"))
;; (define-key key-translation-map (kbd "<f6>") (kbd "C-S-c"))

;; Bind both to paste - but major modes can override C-v.
(mw-global-set-key (kbd "C-v") 'cua-paste "Paste (major modes can override)")
(mw-global-set-key (kbd "C-S-v") 'cua-paste "Paste as plain text")

(use-package browse-kill-ring
  :config
  (mw-global-set-key (kbd "M-v") 'browse-kill-ring "Clipboard history")
  (browse-kill-ring-default-keybindings)
  (bind-keys :map browse-kill-ring-mode-map
	     ("<down>" . browse-kill-ring-forward)
	     ("<up>" . browse-kill-ring-previous)))

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
(mw-global-set-key (kbd "C-y") 'kill-whole-line "Cut line")
(mw-global-set-key (kbd "M-y") 'quick-copy-line "Copy line")
