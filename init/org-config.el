(setq mw-key-topic "Org")

(require 'org)

;; Global keybinds
;; Some from https://orgmode.org/manual/Activation.html
(mw-global-set-key (kbd "C-c l") 'org-store-link "Store link")
(mw-global-set-key (kbd "C-c a") 'org-agenda "Agenda")
(mw-global-set-key (kbd "C-S-a") 'org-agenda-list "Directly to agenda list")
(mw-global-set-key (kbd "C-c c") 'org-capture "Capture")

;; Make org give up shift-arrow keys
;; https://orgmode.org/manual/Conflicts.html
;; Must be set before org loads, so set it early
;; (setq-default org-replace-disputed-keys t)
;; Support shift-selection almost always...except editing timestamps
(setq-default org-support-shift-select 'always)

;; Do not override these keys
(bind-keys :map org-mode-map
	   ("C-e" . nil)  ;; buf switcher
	   ("<C-tab>" . nil)  ;; tab switcher
	   ("C-'" . nil) ;; treemacs
	   ("<S-return>" . nil))

;; Mouse support
(require 'org-mouse)

;; Basic editing
(setq-default org-special-ctrl-a/e 'reversed)
(define-key org-mode-map (kbd "<home>") 'org-beginning-of-line)
(define-key org-mode-map (kbd "<end>") 'org-end-of-line)
;; Smart paste in org mode
(define-key org-mode-map (kbd "C-v") 'org-yank)
;; Todo states
(mw-define-key 'org-mode-map (kbd "C-t") 'org-todo "Todo")
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")))

(mw-define-key 'org-mode-map (kbd "<C-up>") 'org-previous-visible-heading "Previous heading")
(mw-define-key 'org-mode-map (kbd "<C-down>") 'org-next-visible-heading "Next heading")

;; Looks
(setq-default
  org-startup-indented t
  org-startup-folded 'content
  org-startup-with-inline-images t)
;; More natural ellipsis
(setq org-ellipsis "⤵")
(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; No blank before new entry/heading
(dolist (v '(plain-list-item heading))
  (setcdr (assoc v org-blank-before-new-entry) nil))

;; Refile
;; This is absurdly powerful.N
;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
;; https://yiming.dev/blog/2018/03/02/my-org-refile-workflow/
;; Don't think I use this at all yet...
;; Possible targets are all agenda files
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(defun +org-search ()
  (interactive)
  (org-refile '(4)))
;; (define-key org-mode-map (kbd "C-n") '+org-search)

;; Somewhat easier list making
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

;; Easier access keys
(mw-define-key 'org-mode-map [C-i] 'org-toggle-item "Toggle item")
(mw-define-key 'org-mode-map (kbd "C-l") 'org-toggle-heading "Toggle heading")
(mw-define-key 'org-mode-map (kbd "C-,") 'org-insert-structure-template "Structure template")
