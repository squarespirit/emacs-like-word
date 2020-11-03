(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))
(defun mw-emacs-lisp-coding-defaults ()
  (smartparens-strict-mode +1))
(add-hook 'emacs-lisp-mode-hook 'mw-emacs-lisp-coding-defaults)
