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
;; Make esc cancel like C-g
;; https://www.reddit.com/r/emacs/comments/67rlfr/esc_vs_cg/dgsozkc/
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;; https://stackoverflow.com/a/650386
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
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
;; Backspace/delete should not copy to clipboard
;; http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))
(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))
(global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "<C-delete>") 'my-delete-word)

;; Use C-f to do searches
(global-set-key (kbd "C-f") 'isearch-forward)
;; Switch to minibuffer when starting search
;; Doesn't work
;; (defun switch-to-minibuffer ()
;;   (if (active-minibuffer-window)
;;       (select-window (active-minibuffer-window))))
;; (add-hook 'isearch-mode-hook 'switch-to-minibuffer)
;; Allow ctrl sequences to edit the search
;; Seems like only in emacs 27.1
(setq-default search-exit-option 'edit)
(setq-default isearch-allow-scroll t)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
;; Make it like search in other apps;
;; enter gives you the next result.
;; esc to exit.
;; Note: C-g still aborts the search and returns to original location
;; Important: bind as <return> (for GUIs) and not RET (for terminals)
;; http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<S-return>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-exit)
;; Auto wrap isearch https://stackoverflow.com/a/287067
;;(defadvice isearch-search (after isearch-no-fail activate)
;;  (unless isearch-success
;;    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;;    (ad-activate 'isearch-search)
;;    (isearch-repeat (if isearch-forward 'forward))
;;    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;;    (ad-activate 'isearch-search)))
;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
;;
(setq-default isearch-nonincremental t)

;; Make it easier to switch windows
(global-set-key (kbd "<f10>") 'other-window)
(global-set-key (kbd "<S-f10>") 'delete-other-windows)
(global-set-key (kbd "<M-f10>") 'delete-window)

;; Buffer switcher
(require 'bs)
(add-to-list 'bs-configurations
             '("targets" nil nil nil
	       (lambda (buf)
		 ((not (string-equal "*" (substring (buffer-name buf) 0 1)))
		  "Normal"))))
(defun bs-show-and-goto-alternate (arg)
  (interactive "P")
  (bs-show arg)
  (forward-line))
(global-set-key (kbd "C-e") 'bs-show-and-goto-alternate)
(define-key bs-mode-map (kbd "<escape>") 'bs-abort)

;; Taken from VScode
(global-set-key (kbd "C-S-k") 'kill-whole-line)
;; No overwrite mode
(global-unset-key (kbd "<insert>"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make org give up shift-arrow keys
;; https://orgmode.org/manual/Conflicts.html
;; Must be set before org loads, so set it early
(setq-default org-replace-disputed-keys t)
(setq-default org-support-shift-select t)

;; Looks
(setq-default org-startup-indented t)
;; More natural ellipsis
(setq org-ellipsis "⤵")
(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq-default org-special-ctrl-a/e t)
;; Do not override global C-e
(define-key org-mode-map (kbd "C-e") nil)

(require 'org)
(define-key org-mode-map (kbd "<home>") 'org-beginning-of-line)
(define-key org-mode-map (kbd "<end>") 'org-end-of-line)

;; Special paste in org mode
;; Redefine `yank` for org-mode. cua-paste indirectly calls it.
;; Note: if paste has bugs, run original-yank
(fset 'original-yank (symbol-function 'yank))
(defun yank (&optional arg)
  (interactive "P")
  (if (eq major-mode 'org-mode)
      ;; Copied from definition of org-paste-special
      (cond
       ((org-at-table-p) (org-table-paste-rectangle))
       ((org-kill-is-subtree-p) (org-paste-subtree arg))
       (t (original-yank arg)))
    (original-yank arg)))

;; Great command for subtree editing
;; Except, while org-mark-subtree puts the point at the beginning of the
;; selection, put it at the end, which may be more natural for
;; word processors
;; No, don't do that anymore. Subtrees can get big; keep the mark on the
;; heading since it's nice to see the heading of the subtree you selected.
;; Consecutive calls mark bigger subtrees.
;; Bug: the subtree is not shift selected, so arrow keys do not cancel
;; the selection
(defun my-mark-subtree (&optional up)
  (interactive "P")
  (if (eq last-command 'my-mark-subtree)
      (outline-up-heading 1)) ; move up 1 level
  (org-mark-subtree up))
;; Y looks like a tree :)
(define-key org-mode-map (kbd "C-y") 'my-mark-subtree)

;; Previous/next heading with smart beginning of line

;; Overcomplicated solution to go to the parent heading. Not used.
;; Not just outline-up-heading because if you are *not* on a heading
;; line, it essentially moves up two headings. Only move up one.
;; (defun my-parent-heading (arg)
;;   (interactive "p")
;;   (if (org-at-heading-p)
;;       (if (equal 1 (funcall outline-level))
;; 	  (org-previous-visible-heading arg)
;; 	(outline-up-heading arg))
;;       (org-previous-visible-heading arg))
;;   (org-beginning-of-line))
(defun my-previous-visible-heading (arg)
  (interactive "p")
  (org-previous-visible-heading arg)
  (org-beginning-of-line))
(define-key org-mode-map (kbd "C-P") 'my-previous-visible-heading)
(defun my-next-visible-heading (arg)
  (interactive "p")
  (org-next-visible-heading arg)
  (org-beginning-of-line))
(define-key org-mode-map (kbd "C-N") 'my-next-visible-heading)

(defun my-backward-heading-same-level (arg)
  (interactive "p")
  (org-backward-heading-same-level arg)
  (org-beginning-of-line))
(define-key org-mode-map (kbd "C-S-P") 'my-backward-heading-same-level)
(defun my-forward-heading-same-level (arg)
  (interactive "p")
  (org-forward-heading-same-level arg)
  (org-beginning-of-line))
(define-key org-mode-map (kbd "C-S-N") 'my-forward-heading-same-level)


;; Previous/next paragraph with smart beginning of line
;; Unbind C-S-up/down so that shift selection can take place.
;; They were org-clock-timestamps-up/down.
;; https://orgmode.org/manual/Clocking-commands.html
;; May be controversial.
(define-key org-mode-map (kbd "<C-S-up>") nil)
(define-key org-mode-map (kbd "<C-S-down>") nil)
(defun my-backward-paragraph ()
  (interactive "^")
  ;; If we're at the smart beginning of line (in front of stars),
  ;; org-backward-paragraph just goes to the actual beginning of line
  ;; (not the previous paragraph). So this is needed.
  (beginning-of-line)
  (org-backward-paragraph)
  ;; When mark is active, we want actual beginning of line, in order to
  ;; select headings.
  (unless mark-active
    (org-beginning-of-line)))
(define-key org-mode-map (kbd "<C-up>") 'my-backward-paragraph)
(defun my-forward-paragraph ()
  (interactive "^")
  (beginning-of-line)
  (org-forward-paragraph)
  (unless mark-active
    (org-beginning-of-line)))
(define-key org-mode-map (kbd "<C-down>") 'my-forward-paragraph)

;; Swap M-left/right and S-M-left/right, so that all the unshifted
;; M-<arrow keys> work on subtrees.
;; May be very controversial.
;; Implementation is hacky here; based on copying the original org functions
;; (e.g. org-metaleft) and swapping out parts.
(defun my-metaleft (&optional _arg)
  "Promote subtree, list item at point or move table column left.

This function runs the hook `org-metaleft-hook' as a first step,
and returns at first non-nil value."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaleft-hook))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-column 'left))
   ;; Promote subtree
   ((org-at-heading-p) (call-interactively 'org-promote-subtree))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-promote))
   ;; Promote item subtree
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-outdent-item-tree))
   (t (call-interactively 'backward-word))))
(define-key org-mode-map (kbd "<M-left>") 'my-metaleft)

(defun my-shiftmetaleft ()
  "Promote individual item or delete table column."
  (interactive)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaleft-hook))
   ((org-at-table-p) (call-interactively 'org-table-delete-column))
   ;; Promote individual heading
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-promote))
   ;; Promote individual item
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-outdent-item))
   (t (org-modifier-cursor-error))))
(define-key org-mode-map (kbd "<M-S-left>") 'my-shiftmetaleft)

(defun my-metaright (&optional _arg)
  "Demote subtree, list item at point or move table column right.

In front of a drawer or a block keyword, indent it correctly.

This function runs the hook `org-metaright-hook' as a first step,
and returns at first non-nil value."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-move-column))
   ((org-at-drawer-p) (call-interactively 'org-indent-drawer))
   ((org-at-block-p) (call-interactively 'org-indent-block))
   ;; Demote heading subtree
   ((org-at-heading-p) (call-interactively 'org-demote-subtree))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-demote))
   ;; Demote item tree
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-indent-item-tree))
   (t (call-interactively 'forward-word))))
(define-key org-mode-map (kbd "<M-right>") 'my-metaright)

(defun my-shiftmetaright ()
  "Demote individual heading or insert table column."
  (interactive)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-insert-column))
   ;; Demote individual heading
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-demote))
   ;; Demote individual item
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-indent-item))
   (t (org-modifier-cursor-error))))
(define-key org-mode-map (kbd "<M-S-right>") 'my-shiftmetaright)

(setq-default org-agenda-sorting-strategy '(timestamp-up))
;; Numeric priorities. TODO this does not appear to work
(setq-default org-priority-highest 1)
(setq-default org-priority-lowest 3)
(setq-default org-priority-default 2)
;; Todo states
(define-key org-mode-map (kbd "C-t") 'org-todo)
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")))

;; org goto - nice way to move around
(define-key org-mode-map (kbd "C-j") 'org-goto)
;; Make esc exit.
;; Would add this function as advice; but org-goto--set-map seems to be
;; private
;; (defun add-extra-keys-to-org-goto-map ()
;;  (define-key org-goto-map (kbd "<escape>") 'org-goto-quit))
;; (add-function 'org-goto--set-map)
;; And left/right should not exit; that is quite jarring.
;; Actually this is maybe not a good idea.
;; (defun org-goto-left ()
;;   (interactive)
;;   (backward-char))
;; (defun org-goto-right ()
;;   (interactive)
;;   (forward-char))

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
(defun centaur-tabs-buffer-groups ()
 (list
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
   (t "text"))))
;; I never want to switch to the Emacs buffers anyway.
;; This is naive but at least reduces confusion about multiple groups
;; (defun centaur-tabs-buffer-groups () (list "single-group"))
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
