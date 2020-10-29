;; https://harryrschwartz.com/2016/02/15/switching-to-a-literate-emacs-configuration

;; Startup speed boosters, based on Doom startup I think.

;; Avoid garbage collection during startup. The GC eats up quite a bit of time, easily
;; doubling the startup time. The trick is to turn up the memory threshold in order to
;; prevent it from running during startup.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Every file opened and loaded by Emacs will run through this list to check for a proper
;; handler for the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; After Emacs startup has been completed, set `gc-cons-threshold'
;; and reset `gc-cons-percentage' to its original value.
;; Also reset `file-name-handler-alist'
(add-hook 'emacs-startup-hook
          '(lambda ()
	     ;; 1mb
             (setq gc-cons-threshold (* 1024 1024)
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (makunbound 'file-name-handler-alist-original)))

;; Verbose gc
(setq garbage-collection-messages t)

;; Garbage collect on focus lost - Emacs "should" feel snappier
;; https://github.com/Bassmann/emacs-config/blob/master/emacs.org#garbage-collect-on-focus-out-emacs-should-feel-snappier
(add-hook 'focus-out-hook #'garbage-collect)

;; Load literate startup file
(org-babel-load-file "~/.emacs.d/config.org")
