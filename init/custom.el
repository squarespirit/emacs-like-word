;;(setq-default org-agenda-files
;;	      '("~/notes/Debug" "~/Debug/20-10-05" "~/Debug/20-08-17" "~/Features" "~/Dropbox/notes"))
(setq org-agenda-files (directory-files-recursively "~/notes/" "\\.org$"))

(setq-default org-capture-templates
	      '(
		;;("t" "Todo" entry (file "~/Dropbox/notes/todos.org")
		;; "* TODO %?")
		;;("n" "Note" entry (file "~/Dropbox/notes/notes.org")
		;;"* %?")
		("t" "Work todo" entry (file "~/notes/notes/rubrik_todos.org")
		 "* TODO %?")
		("n" "Work note" entry (file "~/notes/notes/notes.org")
		 "* %?")
		("u" "Company update" entry (file "~/notes/notes/company_updates.org")
		 "* %?")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(package-selected-packages
   '(rainbow-delimiters persistent-soft ergoemacs-mode treemacs-projectile ivy-rich treemacs expand-region smartparens smart-parens spaceline powerline-evil popwin counsel-projectile projectile smart-mode-line centaur-tabs which-key org-bullets solarized-theme counsel ace-window undo-tree use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
