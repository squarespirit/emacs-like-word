(defvar mw-init-dir (concat user-emacs-directory "init/")
  "Directory that contains all init files.")

(setq custom-file (concat mw-init-dir "custom.el"))

(add-to-list 'load-path (concat user-emacs-directory "load/"))

(mapc
 'load-file
 ;; A few gotchas here:
 ;; t for full paths.
 ;; Exclude files starting with ., else . will be included
 (directory-files mw-init-dir t "^[^#\.].*el$"))

