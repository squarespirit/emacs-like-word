(setq ivy-ignore-buffers '(mw-is-buffer-ignored))
(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-display-transformers-list
	(plist-put ivy-rich-display-transformers-list 'ivy-switch-buffer
		   '(:columns
		    ((ivy-switch-buffer-transformer (:width 30))    ; add face by the original transformer
		     (ivy-rich-switch-buffer-size (:width 7))  ; return buffer size
		     (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))  ; return buffer indicator
		     (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))            ; return major mode info
;; Unfortunately these indicators are kind of slow on Windows
;;		     (ivy-rich-switch-buffer-project (:width 15 :face success))               ; return project name `projectile'
;;		     (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))  ; return file path relative to project root or `default-directory' if project is nil
                    )
		    :predicate
		    (lambda (cand) (get-buffer cand)))))
  (ivy-rich-mode 1))
