(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))
(defun mw-emacs-lisp-coding-defaults ()
  (smartparens-strict-mode +1))
(add-hook 'emacs-lisp-mode-hook 'mw-emacs-lisp-coding-defaults)
;; Easier to reach
(bind-keys
 :map emacs-lisp-mode-map
 ("<M-up>" . up-list)
 ("<M-down>" . down-list)
 ("<M-left>" . backward-sexp)
 ("<M-right>" . forward-sexp)
 ("<M-backspace>" . backward-kill-sexp)
 ("<M-delete>" . kill-sexp)
 ("M-[" . insert-parentheses)
 ("M-]" . move-past-close-and-reindent))

;; Make this available everywhere
(bind-keys ("C-M-x" . eval-defun))
