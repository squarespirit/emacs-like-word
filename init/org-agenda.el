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
