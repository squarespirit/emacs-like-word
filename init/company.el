(use-package company
  :hook (after-init-hook . global-company-mode)
  ;; TODO: What to bind completion function
  :bind (("C-SPC" . company-complete)))

