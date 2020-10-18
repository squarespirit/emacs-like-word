;; Useful links
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs

;; Packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Keybinds
(cua-mode t)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)
;; C-o open; complicated stuff to open w/ GUI instead of command line
;; https://stackoverflow.com/a/26484229
(global-set-key (kbd "C-o") 'menu-find-file-existing)
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil))
    ad-do-it))
;; Use C-f to do searches
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
;; Taken from VScode
(global-set-key (kbd "C-S-k") 'kill-whole-line)
;; Page up/down scrolls half page at a time
(defun scroll-up-half () (interactive) (scroll-up (/ (window-body-height) 2)))
(defun scroll-down-half () (interactive) (scroll-down (/ (window-body-height) 2)))
(global-set-key (kbd "<prior>") 'scroll-down-half) ; note: down = backward, up = forward
(global-set-key (kbd "<next>") 'scroll-up-half)
;; No overwrite mode
(global-unset-key (kbd "<insert>"))
;; Make esc cancel like C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;; scroll one line at a time (less "jumpy" than defaults)
;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; Pass through win key, so Windows keyboard shortcuts still work
(setq w32-pass-lwindow-to-system nil)
;; Unbind CUA rectangle selection (conflicts with org C-ret)
(define-key cua-global-keymap (kbd "C-<return>") nil)

;; Looks
(setq-default cursor-type 'bar) 
;; Use variable pitch in org mode
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
(setq-default visual-line-mode 't)

;; Files
;; https://www.johndcook.com/blog/emacs_windows/
;; Use temp folder for backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;; Delete to recycle bin
(setq delete-by-moving-to-trash t)
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(setq create-lockfiles nil)

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (set-face-attribute 'default nil :font "Consolas"))
 (t (set-face-attribute 'default nil :font "DejaVu Sans Mono")))

;; Sessions
(desktop-save-mode 1)

;; Org
(setq-default org-startup-indented t)
;; Make org give up shift-arrow keys
;; https://orgmode.org/manual/Conflicts.html
(setq-default org-replace-disputed-keys t)
(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(setq-default org-special-ctrl-a/e t)
(require 'org)
(define-key org-mode-map (kbd "<home>") 'org-beginning-of-line)
(define-key org-mode-map (kbd "<end>") 'org-end-of-line)
(setq-default org-agenda-sorting-strategy '(timestamp-up))
(define-key org-mode-map (kbd "C-t") 'org-todo)
;; Suggested global keybinds in https://orgmode.org/manual/Activation.html
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Which key
(which-key-mode 1)

;; Tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
(setq centaur-tabs-height 32)
;; (setq centaur-tabs-set-icons t) ; Unfortunately, icons are very slow on windows...
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-cycle-scope 'tabs)
(defun centaur-tabs-buffer-groups ()
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
    (t "text"))))
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "●")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(org-agenda-files (quote ("~/todos.org")))
 '(package-selected-packages (quote (centaur-tabs which-key org-bullets use-package))))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground ;;"SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil ;;:underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Consolas")))))
