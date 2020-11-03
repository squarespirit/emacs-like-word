;; Unbind C-S-up/down so that shift selection on paragraphs can take place.
;; They were org-clock-timestamps-up/down.
;; https://orgmode.org/manual/Clocking-commands.html
;; May be controversial.
(define-key org-mode-map (kbd "<C-S-up>") nil)
(define-key org-mode-map (kbd "<C-S-down>") nil)

;; Enable shift-selection on org paragraph
(defun mw-org-backward-paragraph ()
  (interactive "^")
  "Go backward to previous paragraph; allow for shift selection"
  (org-backward-paragraph))
(define-key org-mode-map (kbd "<C-up>") 'mw-org-backward-paragraph)
(defun mw-org-forward-paragraph ()
  (interactive "^")
  "Go forward to next paragraph; allow for shift selection"
  (org-forward-paragraph))
(define-key org-mode-map (kbd "<C-down>") 'mw-org-forward-paragraph)

;; Previous/next paragraph with smart beginning of line
;; Actually, do not do this, for simplicity/predictability.
;; (defun my-backward-paragraph ()
;;   (interactive "^")
;;   ;; If we're at the smart beginning of line (in front of stars),
;;   ;; org-backward-paragraph just goes to the actual beginning of line
;;   ;; (not the previous paragraph). So this is needed.
;;   (beginning-of-line)
;;   (org-backward-paragraph)
;;   ;; When mark is active, we want actual beginning of line, in order to
;;   ;; select headings.
;;   (unless mark-active
;;     (org-beginning-of-line)))
;; (define-key org-mode-map (kbd "<C-up>") 'my-backward-paragraph)
;; (defun my-forward-paragraph ()
;;   (interactive "^")
;;   (beginning-of-line)
;;   (org-forward-paragraph)
;;   (unless mark-active
;;     (org-beginning-of-line)))
;; (define-key org-mode-map (kbd "<C-down>") 'my-forward-paragraph)
