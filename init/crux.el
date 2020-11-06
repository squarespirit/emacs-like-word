(use-package crux)
(bind-keys
 ([remap move-beginning-of-line] . crux-move-beginning-of-line)
 ("<S-return>" . crux-smart-open-line)
 ("C-k" . crux-smart-kill-line)
 ("C-y" . kill-whole-line)
 ;; ("C-d" . crux-duplicate-current-line-or-region)
 ("C-S-j" . crux-top-join-line)
 ("C-c f" . crux-recentf-find-file))
