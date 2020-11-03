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
