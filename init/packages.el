;; Using approach here https://medium.com/@suvratapte/configuring-emacs-from-scratch-use-package-c30382297877.
;; use-package :ensure is used to install packages.
;; However, use-package must first be installed.
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
