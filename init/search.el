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
  ;; Fuzzy match except in swiper
  ;; https://emacs.stackexchange.com/questions/36745/enable-ivy-fuzzy-matching-everywhere-except-in-swiper
  ;; https://oremacs.com/2016/01/06/ivy-flx/
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq mw-key-topic "Editing")
  (mw-global-set-key (kbd "C-f") 'swiper "Find")
  :bind (:map swiper-map
         ("C-f" . swiper-C-s)
         :map ivy-minibuffer-map
         ("<S-return>" . ivy-call)
         ("<C-return>" . ivy-immediate-done)))

;; Better fuzzy match
(use-package flx)

;; So hydras work properly in ivy
(use-package ivy-hydra)

(setq mw-key-topic "Editing")
(mw-global-set-key (kbd "M-i") 'imenu "File index")

