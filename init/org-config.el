;; Suggested global keybinds in https://orgmode.org/manual/Activation.html
(bind-keys
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c c" . org-capture))

;; Make org give up shift-arrow keys
;; https://orgmode.org/manual/Conflicts.html
;; Must be set before org loads, so set it early
;; (setq-default org-replace-disputed-keys t)
;; Support shift-selection almost always...except editing timestamps
(setq-default org-support-shift-select 'always)

(require 'org)
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
(define-key org-mode-map (kbd "C-t") 'org-todo)
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")))

(bind-keys :map org-mode-map
	   ("M-[" . org-previous-visible-heading)
	   ("M-]" . org-next-visible-heading))

(dolist
    (file '("org-paragraph-select.el"
	    "org-metaleftright.el"))
  (load-file (concat mw-init-dir file)))

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


;; Agenda
(require 'org-agenda)
;; Escape quits agenda as expected.
(define-key org-agenda-mode-map (kbd "<escape>") 'org-agenda-quit)
;; Define familiar keys in agenda
;; (define-key org-agenda-mode-map (kbd "C-s") 'org-save-all-org-buffers)
(define-key org-agenda-mode-map (kbd "C-t") 'org-agenda-todo)

;; http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span 'fortnight)
;;don't show tasks as scheduled if they are already shown as a deadline
;; (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
;; Then, the global todo list becomes a way to check what todos
;; are not scheduled
;; (setq org-agenda-todo-ignore-deadlines 'all)
;; (setq org-agenda-todo-ignore-scheduled 'all)

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

;; Source code editing
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(add-to-list 'org-structure-template-alist '("L" . "src emacs-lisp"))
