; Turn off the visible bell
(setq visible-bell nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

; Set the font
(set-face-attribute 'default nil :font "Droid Sans Mono-16")

; Change the background color for current line
(require 'hl-line)
(set-face-background 'hl-line "#111")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Taken from Tim's emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Set focus follows mouse
(setq focus-follows-mouse t)

;;;; Windows printer setup example
;; (setq printer-name "//xxx/name")
;; (setq ps-printer-name "//xxx/name")

;;;; Enable headers (requires pr)
;; (setq lpr-header-switches t)

;;;; Proper color printing with dark backgrounds
(require 'my-print)

;;;; Tag customization and bindings
(require 'my-tags)

;;;; Default to indent with spaces, not tabs
(setq-default indent-tabs-mode nil)

;;;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;;;; Desktop saving
;; Load a desktop file if it exists
(desktop-save-mode t)
;; Disable desktop save mode if no file was found
;; This keeps desktops from being saved without explicitly
;; calling desktop-save somewhere first
(defun my-desktop-no-desktop-file-hook ()
  (desktop-save-mode 0))
(add-hook 'desktop-no-desktop-file-hook 'my-desktop-no-desktop-file-hook)

;;;; Set default mode line appearance
;; Move function display before mode display
(setq mode-line-format
      '("-"
	mode-line-mule-info
	mode-line-modified
	mode-line-frame-identification
	mode-line-buffer-identification
	"  "
	mode-line-position
	(vc-mode vc-mode)
	"  "
	(which-func-mode ("" which-func-format "--"))
	mode-line-modes
	(global-mode-string ("--" global-mode-string))
	"-%-"))

;; Show which function name in mode line
(which-function-mode t)

;;;; Enable Python for SCons files
(add-to-list 'auto-mode-alist '("Scons\\(cript\\|truct\\)" . python-mode))

;;;; End of Tim's config

;; Load some plugins
(require 'yasnippet)

;; Develop in ~/emacs.d/user/snippets, but also load the default ones
(setq yas-snippet-dirs
      (list (concat user-emacs-directory "jszakmeister/snippets")
            (concat user-emacs-directory "elpa/yasnippet-0.6.1/snippets")))

(yas-global-mode 1)

;;; Load my color theme
(require 'color-theme)
(load-file "~/.emacs.d/blackboard.el")
(autoload 'color-theme-empty-void "empty-void-theme" "" t nil)
(autoload 'color-theme-john "john-theme" "" t nil)
(color-theme-john)

;;; Don't clutter up directories with files~
;(setq auto-save-file-name-transforms
;      `((".*"
;         ,(expand-file-name (concat dotfiles-dir "saves")) t)))
(setq auto-save-file-name-transforms
      `((".*"
         ,(expand-file-name (concat user-emacs-directory "saves")) t)))

;;; Don't soft-wrap lines
(setq-default truncate-lines t)

;;; Set the default font
(set-face-attribute 'default nil
                    :family "Droid Sans Mono" :height 160)

;;; Bring in the better ido fuzzy matcher
;(require 'better-ido-matching)

;; Enable ANSI colors in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Bind dabbrev-expand to ctrl-p... it's a vim habit
;; that I'll never break
(global-set-key (kbd "C-p") 'hippie-expand)

;; Add a function to copy lines.  Why something like this isn't
;; in Emacs by default is beyond me.
;; Take from <http://www.emacswiki.org/emacs/CopyingWholeLines>
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; Bind copy-lines to a keystroke
(global-set-key "\C-c\C-k" 'copy-line)

(add-hook 'python-mode-hook 'my-python-customizations)
(defun my-python-customizations ()
  "set up my personal customizations for python mode"
  ;; put other customizations in here
  (define-key python-mode-map (kbd "C-c C-k") 'copy-line))

(add-hook 'latex-mode-hook 'my-latex-customizations)
(defun my-latex-customizations ()
  "set up my personal customizations for latex mode"
  ;; put other customizations in here
  (define-key latex-mode-map (kbd "C-c C-k") 'copy-line))

;; Make the ctrl arrow keys do word movements
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)

;; Enable column numbers in the mode line
(column-number-mode t)

(eval-after-load "ispell"
  (setq-default ispell-program-name "hunspell"))

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; Turn off the system beep
(setq visible-bell t)

(defun ido-disable-line-trucation
  () (set (make-local-variable 'truncate-lines) nil))

(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; Bring in Markdown mode
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
  (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
          " [No match]" " [Matched]" " [Not readable]"
          " [Too big]" " [Confirm]")))

(global-set-key (kbd "M-h")
                (lambda ()
                  (interactive)
                  (mark-paragraph)
                  (if (> (line-number-at-pos) 1)
                      (next-line))
                  (beginning-of-line)))

(ido-mode nil)
(ido-ubiquitous nil)
(setq ido-enable-prefix nil
      ido-enable-flex-matching nil)

(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x f") 'helm-for-files)

(require 'powerline)
(powerline-default)

;; Fill column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-width 1)
(setq fci-rule-color "grey17")
(setq-default fill-column 78)
