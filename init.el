;; -*-no-byte-compile: t; -*-

;; We require Emacs 24 or better

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
        (when (fboundp mode) (funcall mode -1)))

(defun dotemacs-turn-off-tool-bar ()
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))

;; Can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'dotemacs-turn-off-tool-bar)

;; Turn off the 3d effect for the mode-line in the gui.  Note: loading a theme
;; may turn it back on.
(set-face-attribute 'mode-line nil :box nil)

(setq dotemacs-user-name
      (or (getenv "DOTEMACS_USER") user-login-name))

(setq dotemacs-system-config
        (concat user-emacs-directory (symbol-name system-type) ".el")
      dotemacs-user-directory
        (concat user-emacs-directory
                (file-name-as-directory "user")
                (file-name-as-directory dotemacs-user-name))
      dotemacs-user-before-config
        (concat dotemacs-user-directory "dotemacs-before.el")
      dotemacs-user-after-config
        (concat dotemacs-user-directory "dotemacs-after.el")
      dotemacs-user-system-config
        (concat dotemacs-user-directory (symbol-name system-type) ".el")
      dotemacs-user-system-name-config
        (concat dotemacs-user-directory system-name ".el"))

(defun dotemacs-add-path-to-list (list-var path)
  (when (file-exists-p path)
    (add-to-list list-var path)))

(defun dotemacs-load-exists (path)
  (when (file-exists-p path)
    (load path)))

(defun dotemacs-add-to-list (list-var list-to-add)
  (dolist (item list-to-add)
    (add-to-list list-var item)))

;; Took this from the starter-kit
(defun dotemacs-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

  If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(dotemacs-eval-after-init
 `(progn
    (dolist (config '(,dotemacs-system-config
                      ,dotemacs-user-system-config
                      ,dotemacs-user-system-name-config
                      ,dotemacs-user-after-config))
      (dotemacs-load-exists config))))


(defvar dotemacs-packages
  '(
    align-cljlet
    clojure-mode
    clojure-test-mode
    cmake-mode
    color-moccur
    ctags
    ctags-update
    d-mode
    durendal
    fill-column-indicator
    haml-mode
    haskell-mode
    helm
    helm-projectile
    highlight-parentheses
    jinja2-mode
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
    virtualenv
    yaml-mode
    yasnippet
    ))


(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
;(add-to-list 'package-archives
;             '("myelpa" . "http://localhost:8081/packages/"))

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

;; Ignore a number of files for completion
(dotemacs-add-to-list 'completion-ignored-extensions
                      '(".o"
                        ".obj"
                        ".a"
                        ".lib"
                        ".so"
                        ".bak"
                        ".swp"
                        "tags"
                        "TAGS"
                        "opt"
                        ".ncb"
                        ".plg"
                        ".elf"
                        "cscope.out"
                        ".ecc"
                        ".exe"
                        ".ilk"
                        ".pyc"))

;; Helm and Projectile
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

;; projectile is a project management mode
(require 'projectile)
(projectile-global-mode t)
(global-set-key (kbd "C-c f") 'helm-projectile)

(require 'helm-match-plugin)
(require 'helm-projectile)

;; Yasnippet
(require 'yasnippet)

;; Develop in ~/emacs.d/user/<username>/snippets, but also load the default ones
(dotemacs-add-path-to-list 'yas-snippet-dirs
                           (concat user-emacs-directory "snippets"))
(dotemacs-add-path-to-list 'yas-snippet-dirs
                           (concat dotemacs-user-directory "snippets"))

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

;; Highlight lines that extend over 80 characters.  Also, show embedded tabs,
;; and spaces at the end of lines.
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-style
       '(face lines-tail tabs trailing))
(setq whitespace-action
      '(auto-cleanup warn-read-only))

(require 'nose)

;; Bring in user's customizations
(setq custom-file (concat dotemacs-user-directory "custom.el"))
(dotemacs-load-exists custom-file)

(setq inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space t
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 78
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(file-name-as-directory
                                         (concat user-emacs-directory "backups"))))
      diff-switches "-u")

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))


(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 78))

(setq-default indent-tabs-mode nil
              indicate-empty-lines t
              show-trailing-whitespace t
              imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Seed the random-number generator
(random t)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; Desktop saving

;; Load a desktop file if it exists
(desktop-save-mode t)

;; Disable desktop save mode if no file was found
;; This keeps desktops from being saved without explicitly
;; calling desktop-save somewhere first
(defun dotemacs-desktop-no-desktop-file-hook ()
  (desktop-save-mode 0))
(add-hook 'desktop-no-desktop-file-hook 'dotemacs-desktop-no-desktop-file-hook)

;; Magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

;; Automatically update TAGS on save
(require 'ctags-update)
(ctags-update-minor-mode 1)

;; Turn on auto revert globally.  This will let Emacs automatically
;; re-read the file if changes on disk.
(global-auto-revert-mode t)

;; Lisp-related
(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)))

(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'paredit-mode))

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; Color grep
(require 'color-moccur)
