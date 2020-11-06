(defvar mw-init-dir (concat user-emacs-directory "init/")
  "Directory that contains all init files.")

(add-to-list 'load-path (concat user-emacs-directory "load/"))

(dolist
    (file '("gc.el"

	    "packages.el"

	    "kbd.el"

	    "esc.el"
	    "cua-clipboard.el"
	    "editing.el"
	    "super-save.el"
	    "ivy.el"
	    "undo.el"
	    "crux.el"
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
	    "buffers-ctrl-e.el"

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
	    "org-paragraph-select.el"
	    "org-metaleftright.el"
	    "org-agenda.el"
	    "org-src.el"

	    "emacs-lisp.el"))
  (load-file (concat mw-init-dir file)))

(setq custom-file (concat mw-init-dir "custom.el"))
(if (file-exists-p custom-file)
   (load-file custom-file))


;; Load literate startup file
;; (org-babel-load-file "~/.emacs.d/config.org")
