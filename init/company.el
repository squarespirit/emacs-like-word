(use-package company
  ;; TODO how to lazily load?
  ;; (after-init-hook . global-company-mode) did not appear to work
  ;; Needs to be demand so that it loads even before the first time
  ;; I hit C-spc
  :demand
  :config
  (global-company-mode 1)
  ;; TODO: What to bind completion function

  ;; https://emacs.stackexchange.com/a/10838
  (setq company-dabbrev-downcase nil)
  (define-key company-mode-map (kbd "<escape>") nil))


