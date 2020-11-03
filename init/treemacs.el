(use-package treemacs
  :config
  (treemacs-follow-mode +1)
  (define-key treemacs-mode-map (kbd "<mouse-1>") #'treemacs-single-click-expand-action)
  ;; https://github.com/hlissner/doom-emacs/issues/1177#issuecomment-464405628
;;  (defun my-treemacs-back-and-forth ()
;;    (interactive)
;;    (if (treemacs-is-treemacs-window-selected?)
;;  	(other-window 1)
;;      (treemacs-select-window)))
  (defun my-treemacs-back-and-forth ()
    "If in treemacs, close it. Otherwise, select it."
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
	(treemacs-quit)
      (treemacs-select-window)))
  ;; Cannot be bound with :bind because it's my own function defined outside the pkg
  (global-set-key (kbd "C-'") 'my-treemacs-back-and-forth))
