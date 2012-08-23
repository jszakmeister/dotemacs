;; -*-no-byte-compile: t; -*-

;; Packages
;(setq dotfiles-dir (file-name-directory
;                    (or load-file-name (buffer-file-name))))
;(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/helm")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                      starter-kit-eshell zenburn-theme color-theme yasnippet
		      undo-tree powerline
                      clojure-mode clojure-test-mode
		      align-cljlet cmake-mode ctags ctags-update
		      d-mode durendal less-css-mode
		      lorem-ipsum nginx-mode nose
                      markdown-mode yaml-mode
                      tuareg haskell-mode
                      marmalade oddmuse scpaste
                      swank-cdt haml-mode sass-mode
                      highlight-80+ json
                      org color-theme-blackboard
                      rainbow-delimiters))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
