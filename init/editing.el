;; Rebind CUA rectangle selection (conflicts with org C-ret)
;; Needs to happen before turning on cua mode
(setq cua-rectangle-mark-key (kbd "<C-M-return>"))
;; Do not rebind C-v so that major modes can override it.
(setq-default cua-remap-control-v nil)
;; I don't live in emacs yet...
(cua-mode t)

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

;; Rebind some stuff that causes me pain
(require 'bind-key)
(bind-keys
 ;; Easier to hit.
 ("M-SPC" . cua-set-mark))
 ;;("M-/" . exchange-point-and-mark)
;; Rebind to rectangle commands
(bind-key "M-r" (lookup-key (current-global-map) (kbd "C-x r")))

;; Remap these keys to the C-x/C-c keymaps.
;; Using C-S-x means that they work in CUA mode even if mark is active.
(define-key key-translation-map (kbd "<f5>") (kbd "C-S-x"))
(define-key key-translation-map (kbd "<f6>") (kbd "C-S-c"))

;; Load early, since it is fundamental
(use-package counsel
  ;; It appears that without this demand, projectile isn't loaded on demand
  :demand
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  ;; Prevent backspace from closing the buffer, a little surprising
  (setq ivy-on-del-error-function nil)
  ;; Seems pretty important, so that in Ctrl+f, pressing down or up can cycle
  (setq ivy-wrap t)
  ;; Only real buffers
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  :bind (("C-f" . swiper)
         ("C-e" . ivy-switch-buffer)
         :map swiper-map
         ("C-f" . swiper-C-s)
         :map ivy-minibuffer-map
         ("<S-return>" . ivy-call)
         ("<C-return>" . ivy-immediate-done)))

;; If I eventually live in emacs, this will probably be useful
;; Cua-mode implies delete-selection mode though.
;; (delete-selection-mode 1)

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

;; More context
(setq-default next-screen-context-lines 10)

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
