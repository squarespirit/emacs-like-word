(bind-keys ("<C-f4>" . kill-current-buffer))
(defun mw-is-buffer-ignored (name)
  "Return t if the buffer is a non-user buffer and should be ignored. This is defined once
to provide a consistent interface across all buffer switchers."
  (and 
   (or (string-prefix-p "*" name)
       (string-prefix-p " *" name)
       (string-prefix-p "`" name)
       (string-prefix-p " `" name))
   (not (or
	 ;; Whitelist of special buffers
	 ;; (string-equal "*scratch*" name)
	 (string-prefix-p "*Custom" name)
	 ))))
