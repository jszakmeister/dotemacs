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
;(require 'my-print)

;;;; Tag customization and bindings
;(require 'my-tags)

;;;; Set default mode line appearance
;; Move function display before mode display
;; (setq mode-line-format
;;       '("-"
;;         mode-line-mule-info
;;         mode-line-modified
;;         mode-line-frame-identification
;;         mode-line-buffer-identification
;;         "  "
;;         mode-line-position
;;         (vc-mode vc-mode)
;;         "  "
;;         (which-func-mode ("" which-func-format "--"))
;;         mode-line-modes
;;         (global-mode-string ("--" global-mode-string))
;;         "-%-"))

;; Show which function name in mode line
(which-function-mode t)

;;;; Enable Python for SCons files
(add-to-list 'auto-mode-alist '("Scons\\(cript\\|truct\\)" . python-mode))

;;;; End of Tim's config

;;; Load my color theme
(load-theme 'john t)

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

;; Bring in Markdown mode
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Mark a paragraph, but don't include the leading blank line
(global-set-key (kbd "M-h")
                (lambda ()
                  (interactive)
                  (mark-paragraph)
                  (if (> (line-number-at-pos) 1)
                      (next-line))
                  (beginning-of-line)))

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x f") 'helm-for-files)

(require 'evil)
(evil-mode 1)

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(define-key evil-normal-state-map ",,d" 'kill-this-buffer)

(define-key evil-motion-state-map " " 'switch-to-previous-buffer)
(define-key evil-motion-state-map (kbd "C-d") 'evil-delete-char)
(define-key evil-motion-state-map [home] 'evil-beginning-of-line)
(define-key evil-motion-state-map [end] 'evil-end-of-line)

(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-insert-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-n" 'evil-complete-next)
(define-key evil-insert-state-map "\C-p" 'evil-complete-previous)
(define-key evil-insert-state-map [home] 'evil-beginning-of-line)
(define-key evil-insert-state-map [end] 'evil-end-of-line)

(require 'powerline)

(defface powerline-normal '((t (:background "green yellow"
                                :foreground "green4"
                                :inherit mode-line)))
  "Powerline face for Normal mode."
  :group 'powerline)

(defface powerline-insert '((t (:background "DodgerBlue4"
                                :foreground "white"
                                ;; Once we get the whole mode-line shaded, use these
                                ;; settings.
                                ;:background "white"
                                ;:foreground "DodgerBlue4"
                                :inherit mode-line)))
  "Powerline face for Insert mode."
  :group 'powerline)

(defface powerline-visual '((t (:background "DarkOrange1"
                                :foreground "DarkOrange4"
                                :inherit mode-line)))
  "Powerline face for Visual mode."
  :group 'powerline)

(defun powerline-evil-mode-line-tag ()
  (cond ((string= " <N> " evil-mode-line-tag) (list
                                                (powerline-raw "N " 'powerline-normal 'l)
                                                (powerline-arrow-right 'powerline-normal nil)))
        ((string= " <I> " evil-mode-line-tag) (list
                                               (powerline-raw "Insert " 'powerline-insert 'l)
                                               (powerline-arrow-right 'powerline-insert nil)))
        ((string= " <V> " evil-mode-line-tag) (list
                                               (powerline-raw "Visual " 'powerline-visual 'l)
                                               (powerline-arrow-right 'powerline-visual nil)))
        (t (list
            (powerline-raw evil-mode-line-tag nil 'l)
            (powerline-arrow-right nil nil)))))

(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq (frame-selected-window) (selected-window)))
                        (face1 (if active 'powerline-active1 'powerline-inactive1))
                        (face2 (if active 'powerline-active2 'powerline-inactive2))
                        (lhs (append
                              ;(powerline-raw evil-mode-line-tag 'powerline-normal 'l)
                              (powerline-evil-mode-line-tag)
                              (list
                               (powerline-raw "%*" nil 'l)
                               (powerline-buffer-size nil 'l)
                               (powerline-buffer-id nil 'l)

                               (powerline-raw " ")
                               (powerline-arrow-right nil face1)

                               (powerline-major-mode face1 'l)
                               (powerline-raw mode-line-process face1 'l)

                               (powerline-arrow-right face1 face2)

                               (powerline-vc face2))))
                        (rhs (list
                              (powerline-raw global-mode-string face2 'r)

                              (powerline-arrow-left face2 face1)

                              (powerline-raw "%4l" face1 'r)
                              (powerline-raw ":" face1)
                              (powerline-raw "%3c" face1 'r)

                              (powerline-arrow-left face1 nil)
                              (powerline-raw " ")

                              (powerline-raw "%6p" nil 'r)

                              (powerline-hud face2 face1))))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill face2 (powerline-width rhs))
                    (powerline-render rhs))))))

;; Fill column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-width 1)
(setq fci-rule-color "grey17")
(setq-default fill-column 78)
