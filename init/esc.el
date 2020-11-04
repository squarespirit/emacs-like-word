;; Make esc cancel like C-g
;; https://www.reddit.com/r/emacs/comments/67rlfr/esc_vs_cg/dgsozkc/
;; This seems to work the best.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;; https://stackoverflow.com/a/650386
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Issues with just binding to 'keyboard-escape-quit: ESC can kill other windows
;; Issues with just binding to 'keyboard-quit (using global-set-key): ESC can't exit minibuffer and many
;; other things
