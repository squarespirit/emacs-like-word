;; Solarized theme https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-light t))

(setq-default cursor-type 'bar)
(global-hl-line-mode +1)

;; Note: (setq-default visual-line-mode t) is somewhat buggy. It did not work
;; when org started up sometimes. This might work
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Visual-Line-Mode.html
(global-visual-line-mode +1)

;; frame title like vscode. Why is :eval needed?
(setq frame-title-format '((:eval (if (buffer-modified-p) "• ")) "%b - Emacs " emacs-version))

;; Makes icons faster on windows
;; https://github.com/domtronn/all-the-icons.el/issues/28#issuecomment-312089198
(setq inhibit-compacting-font-caches t)
