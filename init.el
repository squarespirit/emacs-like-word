(defvar mw-init-dir (concat user-emacs-directory "init/")
  "Directory that contains all init files.")

(dolist
    (file '("gc.el"

	    "packages.el"
	    "bind-key.el"

	    "esc.el"
	    "whole-line-or-region.el"
	    "kill-ring.el"
	    "editing.el"
	    "super-save.el"
	    "ivy.el"
	    "undo.el"
	    "smartparens.el"
	    "expand-region.el"
	    "smex.el"
	    "company.el"
	    "imenu.el"
	    "which-key.el"
	    "multi-cursor.el"

	    "magit.el"
	    
	    "window-management.el"

	    "buffers.el"
	    "buffers-tabs.el"
	    "buffers-ctrl-tab.el"
	    "buffers-by-name.el"

	    "terminal.el"

	    "looks.el"
	    "scroll.el"
	    "modeline.el"
	    
	    "files.el"
	    "treemacs.el"
	    
	    "project.el"

	    "dashboard.el"
	    
	    "session-config.el"

	    "org-config.el"

	    "emacs-lisp.el"))
  (load-file (concat mw-init-dir file)))

(setq custom-file (concat mw-init-dir "custom.el"))
(if (file-exists-p custom-file)
   (load-file custom-file))


;; Load literate startup file
;; (org-babel-load-file "~/.emacs.d/config.org")
