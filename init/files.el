(setq mw-key-topic "Files")
(use-package crux)
(mw-global-set-key (kbd "C-s") 'save-buffer "Save")
(mw-global-set-key (kbd "C-S-s") 'save-buffer "Save as")
(mw-global-set-key (kbd "C-o") 'save-buffer "Open")
(mw-global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file "Rename file")
(mw-global-set-key (kbd "C-c f") 'crux-recentf-find-file "Recent files")

;; Move all the autosave files to one directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; https://www.johndcook.com/blog/emacs_windows/
;; Delete to recycle bin
(setq delete-by-moving-to-trash t)
(setq create-lockfiles nil)

;; Prefer UTF-8 and Unix line endings for new files.
(prefer-coding-system 'utf-8-unix)

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (set-face-attribute 'default nil :font "Consolas" :height 120))
 (t (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 110)))

;; Since I use Dropbox and git heavily, this is probably a good idea
;; https://magit.vc/manual/magit/Automatic-Reverting-of-File_002dVisiting-Buffers.html
(global-auto-revert-mode +1)
