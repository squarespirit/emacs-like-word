(setq mw-key-topic "Editing")
(use-package undo-tree
  :config
  (mw-global-set-key (kbd "C-z") 'undo-tree-undo "Undo")
  (mw-global-set-key (kbd "C-S-z") 'undo-tree-redo "Redo")
  (mw-global-set-key (kbd "C-M-z") 'undo-tree-visualize "Visualize undo history")
  (global-undo-tree-mode 1)
  ;; Unbind to not conflict with commenting
  (define-key undo-tree-map (kbd "C-/") nil))
