;; -*-no-byte-compile: t; -*-

;; We require Emacs 24 or better

(setq dotemacs-user-name user-login-name)

(setq dotemacs-system-config
        (concat user-emacs-directory system-name ".el")
      dotemacs-user-directory
        (concat user-emacs-directory
		(file-name-as-directory "user")
		(file-name-as-directory dotemacs-user-name))
      dotemacs-user-before-config
        (concat dotemacs-user-directory "dotemacs-before.el")
      dotemacs-user-after-config
        (concat dotemacs-user-directory "dotemacs-after.el")
      dotemacs-user-system-config
        (concat dotemacs-user-directory system-name ".el"))

(defun dotemacs-add-path-to-list (list-var path)
  (when (file-exists-p path)
    (add-to-list list-var path)))

(defun dotemacs-load-exists (path)
  (when (file-exists-p path)
    (load path)))

;; Took this from the starter-kit
(defun dotemacs-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

  If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(dotemacs-eval-after-init
 '(progn
    (when (file-exists-p dotemacs-system-config)
      (load dotemacs-system-config))
    (when (file-exists-p dotemacs-user-system-config)
      (load dotemacs-user-system-config))
    (when (file-exists-p dotemacs-user-after-config)
      (load dotemacs-user-after-config))))


(defvar dotemacs-packages
  '(
    ;starter-kit
    ;starter-kit-lisp
    ;starter-kit-bindings
    ;starter-kit-eshell
    align-cljlet
    clojure-mode
    clojure-test-mode
    cmake-mode
    ctags
    ctags-update
    d-mode
    durendal
    fill-column-indicator
    haml-mode
    haskell-mode
    highlight-80+
    json
    less-css-mode
    lorem-ipsum
    markdown-mode
    marmalade
    nginx-mode
    nose
    oddmuse
    org
    powerline
    rainbow-delimiters
    sass-mode
    scpaste
    swank-cdt
    tuareg
    undo-tree
    yaml-mode
    yasnippet
    ))

;; Bring in helm
(add-to-list 'load-path
   (concat user-emacs-directory "helm"))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Let the user get an opportunity to change things early on
;; This is intentionally run before package-intialize to allow
;; user to tweak the packages and archives.
(dotemacs-load-exists dotemacs-user-before-config)

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (p dotemacs-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; Yasnippet
(require 'yasnippet)

;; Develop in ~/emacs.d/user/<username>/snippets, but also load the default ones
(setq yas-snippet-dirs
      (list (concat dotemacs-user-directory "snippets")
            (concat user-emacs-directory "elpa/yasnippet-0.6.1/snippets")))

(yas-global-mode 1)

;; Color-theme
(dotemacs-add-path-to-list 'custom-theme-load-path
                           (expand-file-name
                             (file-name-as-directory
                               (concat user-emacs-directory "themes"))))
(dotemacs-add-path-to-list 'custom-theme-load-path
                           (expand-file-name
                             (file-name-as-directory
                               (concat dotemacs-user-directory "themes"))))

;; Use whitespace mode. This is awesomeness.
(require 'whitespace)
(global-whitespace-mode t)
;; (setq whitespace-style
;;       '(face empty indentation lines-tail newline tabs trailing))
(setq whitespace-action
      '(auto-cleanup warn-read-only))

;; Bring in user's customizations
(setq custom-file (concat dotemacs-user-directory "custom.el"))
(load custom-file)
