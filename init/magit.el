(use-package magit
  :bind
  ("C-S-g" . magit-status))
;; Need deferred loading speed,
;; so do not bind the key, just document it here
(setq mw-key-topic "Git")
(mw-global-doc-key (kbd "C-S-g") "Magit")
