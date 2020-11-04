;; scroll one line at a time (less "jumpy" than defaults)
;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; 2 lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Lose the scroll bar, which is only a little useful, but gain the ability to resize vertical
;; splits w/ the mouse
;; https://stackoverflow.com/a/9646770
(scroll-bar-mode -1)

;; More context
(setq-default next-screen-context-lines 10)

;; This does not appear to be enough - need smooth scrolling
;; Docs for scroll-step recommend against setting this and setting scroll-conservatively instead.
;; However, this appears to *slow down* scrolling thus making it less jumpy in a good way.
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; I think this needs to be relatively large, otherwise scrolling can happen faster
;; than emacs redraws and the cursor can go offscreen, triggering a jump.
(setq-default scroll-margin 3)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 10000)
