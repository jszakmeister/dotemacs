;; -*-no-byte-compile: t; -*-

;; We require Emacs 24 or better

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
        (when (fboundp mode) (funcall mode -1)))

(defun dotemacs-turn-off-tool-bar ()
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))

;; Can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'dotemacs-turn-off-tool-bar)

(setq dotemacs-user-name
      (or (getenv "DOTEMACS_USER") user-login-name))

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
    helm
    helm-projectile
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
