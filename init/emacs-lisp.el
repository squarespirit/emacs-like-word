(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))
(defun mw-emacs-lisp-coding-defaults ()
  (smartparens-strict-mode +1))
(add-hook 'emacs-lisp-mode-hook 'mw-emacs-lisp-coding-defaults)

;; Easier to reach
(bind-keys
 :map emacs-lisp-mode-map
 ("C-y" . sp-kill-whole-line)
 ("<M-up>" . sp-up-sexp)
 ("<M-down>" . sp-down-sexp)
 ("<M-left>" . sp-backward-sexp)
 ("<M-right>" . sp-forward-sexp)
 ("<M-S-right>" . sp-forward-slurp-sexp)
 ("<M-S-left>" . sp-forward-barf-sexp)
 ("<M-backspace>" . sp-backward-kill-sexp)
 ("<M-delete>" . sp-kill-sexp)
 ("C-SPC" . sp-mark-sexp)
 ("C-t" . sp-transpose-sexp)
 ("M-[" . insert-parentheses)
 ("M-]" . move-past-close-and-reindent))

;; Make this available everywhere
(bind-keys ("C-M-x" . eval-defun))
