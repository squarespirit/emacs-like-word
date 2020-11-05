(defvar mw-init-dir (concat user-emacs-directory "init/")
  "Directory that contains all init files.")

(mapc
 'load-file
 ;; A few gotchas here:
 ;; t for full paths.
 ;; Exclude files starting with ., else . will be included
 (directory-files mw-init-dir t "^[^#\.].*el$"))

;; Custom file is already loaded above
;; (setq custom-file (concat mw-init-dir "custom.el"))
;; (if (file-exists-p custom-file)
   ;; (load-file custom-file))
