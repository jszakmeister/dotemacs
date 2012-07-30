;; Packages
;(setq dotfiles-dir (file-name-directory
;                    (or load-file-name (buffer-file-name))))
;(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-6.5.5")
(add-to-list 'load-path "~/.emacs.d")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;(when (null package-archive-contents)
;  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                      starter-kit-eshell zenburn-theme color-theme yasnippet
                      clojure-mode clojure-test-mode
                      markdown-mode yaml-mode
                      tuareg haskell-mode
                      marmalade oddmuse scpaste
                      swank-cdt haml-mode sass-mode
                      highlight-80+ json
                      org color-theme-blackboard
                      rainbow-delimiters))

;(dolist (p my-packages)
;  (when (not (package-installed-p p))
;    (package-install p)))
