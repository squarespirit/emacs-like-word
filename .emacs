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
(global-set-key (kbd "C-S-q") 'save-buffers-kill-terminal)
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
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; 2 lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; Pass through win key, so Windows keyboard shortcuts still work
(setq w32-pass-lwindow-to-system nil)
;; Unbind CUA rectangle selection (conflicts with org C-ret)
(define-key cua-global-keymap (kbd "C-<return>") nil)

;; Looks
;; Solarized theme https://github.com/bbatsov/solarized-emacs
(setq solarized-use-variable-pitch nil)
;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(load-theme 'solarized-light t)
(setq-default cursor-type 'bar) 
;; Use variable pitch in org mode
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; Note: (setq-default visual-line-mode t) was somewhat buggy. It did not work
;; when org started up sometimes. This might work
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Visual-Line-Mode.html
(global-visual-line-mode +1)
;; frame title. Why is :eval needed?
(setq frame-title-format '((:eval (if (buffer-modified-p) "• ")) "%b - Emacs " emacs-version))
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq-default scroll-margin 1)
;; Lose the scroll bar, which is only a little useful, but gain the ability to resize vertical
;; splits w/ the mouse
;; https://stackoverflow.com/a/9646770
(scroll-bar-mode -1)

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
  (set-face-attribute 'default nil :font "Consolas" :height 120))
 (t (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)))

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
;; Todo states
(define-key org-mode-map (kbd "C-t") 'org-todo)
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")))

;; Suggested global keybinds in https://orgmode.org/manual/Activation.html
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; https://orgmode.org/worg/org-tutorials/org-column-view-tutorial.html
(setq-default org-columns-default-format "%50ITEM %TODO %1PRIORITY %20TAGS %20DEADLINE %20SCHEDULED")

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
(setq centaur-tabs-style "bar")
(setq centaur-tabs-height 32)
;; (setq centaur-tabs-set-icons t) ; Unfortunately, icons are very slow on windows...
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-cycle-scope 'tabs)
;; (defun centaur-tabs-buffer-groups ()
  ;; (list
  ;;  (cond
  ;;   ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
;;   (t "text"))))
;; This is naive but at least reduces confusion about multiple groups
(defun centaur-tabs-buffer-groups () (list "single-group"))
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "●")
;; https://github.com/ema2159/centaur-tabs/blob/master/centaur-tabs-elements.el
(set-face-attribute 'centaur-tabs-selected nil :background "#FDFDFD" :foreground "black")
(set-face-attribute 'centaur-tabs-selected-modified nil :background "#FDFDFD" :foreground "black")
(set-face-attribute 'centaur-tabs-unselected nil :background "#CCCCCC" :foreground "black")
(set-face-attribute 'centaur-tabs-unselected-modified nil :background "#CCCCCC" :foreground "black")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode t nil (cua-base))
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#efe4da49afb1" "#cfc4e1acd08b" "#fe52c9e6b34e" "#dbb6d3c2dcf3" "#e183dee0b053" "#f944cc6dae47" "#d35fdac4e069"))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#b3c34d" . 20)
     ("#6ccec0" . 30)
     ("#74adf5" . 50)
     ("#e1af4b" . 60)
     ("#fb7640" . 70)
     ("#ff699e" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(lsp-ui-doc-border "#586e75")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files '("~/notes/todos.org"))
 '(package-selected-packages
   '(solarized-theme centaur-tabs which-key org-bullets use-package))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
