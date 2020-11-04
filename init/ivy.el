;; Load early, since it is fundamental
(use-package counsel
  ;; It appears that without this demand, projectile isn't loaded on demand
  :demand
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  ;; Prevent backspace from closing the buffer, a little surprising
  (setq ivy-on-del-error-function nil)
  ;; Seems pretty important, so that in Ctrl+f, pressing down or up can cycle
  (setq ivy-wrap t)
  ;; Only real buffers
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  :bind (("C-f" . swiper)
         ("C-e" . ivy-switch-buffer)
         :map swiper-map
         ("C-f" . swiper-C-s)
         :map ivy-minibuffer-map
         ("<S-return>" . ivy-call)
         ("<C-return>" . ivy-immediate-done)))
