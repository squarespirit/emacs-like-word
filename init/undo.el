(use-package undo-tree
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo)
         ("C-M-z" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1)
  ;; Unbind to not conflict with commenting
  (define-key undo-tree-map (kbd "C-/") nil))
