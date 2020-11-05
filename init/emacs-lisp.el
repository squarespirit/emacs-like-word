(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))
(defun mw-emacs-lisp-coding-defaults ()
  (smartparens-strict-mode +1))
(add-hook 'emacs-lisp-mode-hook 'mw-emacs-lisp-coding-defaults)

;; https://emacs.stackexchange.com/questions/28543/smartparens-strict-mode-c-w-kill-line-if-no-active-region
(defun my-sp-kill-region-or-line (&optional arg)
  "Kill active region or current line."
  (interactive "p")
  (if (use-region-p)
      (sp-kill-region (region-beginning) (region-end)) ;; strict-mode version of kill-region
    (sp-kill-whole-line))) ;; strict-mode version of kill-whole-line

;; Easier to reach
(bind-keys
 :map emacs-lisp-mode-map
 ("C-w" . my-sp-kill-region-or-line)
 ("<M-up>" . sp-up-sexp)
 ("<M-down>" . sp-down-sexp)
 ("<M-left>" . sp-backward-sexp)
 ("<M-right>" . sp-forward-sexp)
 ("<M-S-right>" . sp-forward-slurp-sexp)
 ("<M-S-left>" . sp-forward-barf-sexp)
 ("<M-backspace>" . sp-backward-kill-sexp)
 ("<M-delete>" . sp-kill-sexp)
 ("C-t" . sp-transpose-sexp)
 ("M-S-w" . sp-copy-sexp)
 ("M-[" . insert-parentheses)
 ("M-]" . move-past-close-and-reindent))

;; Make this available everywhere
(bind-keys ("C-M-x" . eval-defun))
