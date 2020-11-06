(defvar mw-key-topic nil
  "Topic of current keybinds. mw-global-set-key, etc. use this as the current topic when adding keys.
Thus, this should be set before calling any such functions.")

(defvar mw-key-docs nil
  "Documentation of each key. It is a list of (topic, mode, key, doc).
topic is a string.
mode is the mode in which the key is bound. Empty string means it's a global keybinding.
key is the internal representation of key (after calling kbd).
doc is a short string describing the function.")

(defun mw--add-key-doc (topic mode key doc)
  "Add a key to mw-key-docs."
  (cl-check-type topic string)
  (cl-check-type mode string)
  (cl-check-type key array)
  (cl-check-type doc string)
  (add-to-list 'mw-key-docs (list topic mode key doc) t))

(defun mw-global-doc-key (key doc)
  "Document a key, assuming it is already bound."
  (mw-doc-key "" key doc))

(defun mw-global-set-key (key cmd doc)
  "Globally set a key and add it to mw-key-docs.
key: key sequence to set - a vector
cmd: symbol of command to set
doc: short description of key
"
  (cl-check-type key array)
  ;; cmd could be symbol or keymap
  (cl-check-type doc string)
  (global-set-key key cmd)
  (mw--add-key-doc mw-key-topic "" key doc))

(defun mw-doc-key (mode key doc)
  "Document a key in some mode, assuming it is already bound."
  (mw--add-key-doc mw-key-topic mode key doc))

(defun mw-define-key (map key cmd doc)
  "Define a key in a given map and add it to mw-key-docs."
  (cl-check-type map symbol)
  (cl-check-type key array)
  ;; cmd could be symbol or keymap
  (cl-check-type doc string)
  (define-key (symbol-value map) key cmd)
  (let* ((map-name (symbol-name map))
	 (mode-name (cond
		     ((string-suffix-p "-mode-map" map-name) (string-remove-suffix "-mode-map" map-name))
		     ((string-suffix-p "-map" map-name) (string-remove-suffix "-map" map-name))
		     (t map-name))))
    (mw--add-key-doc mw-key-topic mode-name key doc))
  )

(defun mw--editing-is-empty (s)
  "Hack to make \"Editing\" sort first in list"
  (if (equal s "Editing") "" s))

(defun mw--key-less (a b)
  "Return true if a should appear before b in the key reference.
The desired sorting order is:
Sort by topic. However, the 'Editing' topic should come first.
Then sort by mode.
Then, keep the keys in the order that they were defined in.
This is important because keys that are defined nearby in code are likely to be
related."
  (let
      ((atopic (mw--editing-is-empty (car a)))
       (btopic (mw--editing-is-empty (car b)))
       (amode (cadr a))
       (bmode (cadr b)))
    (cond
     ((not (equal atopic btopic)) (string-lessp atopic btopic))
     ((not (equal amode bmode)) (string-lessp amode bmode))
     (t nil)
     )
    )
  )
	 
(defun mw-key-reference ()
  "Display key reference in a new window."
  (interactive)
  ;; Group keys by topic and mode.
  ;; But maintain the order of keys with the same topic and mode.	
  (setq mw-key-docs (cl-stable-sort mw-key-docs 'mw--key-less))
  (let (
	(temp-buf-name "Key Reference")
	(current-topic "")
	(current-mode ""))
    (with-output-to-temp-buffer temp-buf-name
      (dolist (k mw-key-docs)
	(if (not (equal (car k) current-topic))
	    (progn
	      (princ (format "\n%s ======================\n" (car k)))
	      (setq
	       current-topic (car k)
	       current-mode "")))
	(if (not (equal (cadr k) current-mode))
	    (progn
	      (princ (format "\n%s mode:\n" (capitalize (cadr k))))
	      (setq current-mode (cadr k))))
	(princ (format "%-16s%s\n" (key-description (caddr k)) (cadddr k)))) 
      )
    (switch-to-buffer-other-window temp-buf-name)
    ;; Quit with q
    (special-mode))
  )

;; (progn
;;   (setq mw-key-docs nil)
;;   (mw--add-key-doc "test" "my-mode" (kbd "C-a") "end of line")
;;   (mw--add-key-doc "test" "my-mode" (kbd "C-b") "backward char")
;;   (mw--add-key-doc "test" "" (kbd "C-c") "cancel")

;;   (setq mw-key-topic "Editing")
;;   (mw-global-set-key (kbd "C-a") 'beginning-of-line "Begin of line")
;;   (mw-define-key 'org-mode-map (kbd "C-d") 'end-of-line "end of line"))
;; (setq debug-on-error t)

(provide 'mw-keys)
