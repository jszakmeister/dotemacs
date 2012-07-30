;; My color theme.
;;
;; It's a modified version of the blackboard theme, that incorporates
;; some of my settings from SlickEdit, and is made to degrade well
;; at the terminal.
;;
;; Based on JD Huntington's Blackboard theme, which has the following license:
;;
;; MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;; Credits due to the excellent TextMate Blackboard theme

(require 'color-theme)

;;;###autoload
(defun color-theme-john ()
  "Based on JD Huntington's blackboard theme, but made to match my VS scheme."
  (interactive)
  (color-theme-install
   '(color-theme-blackboard
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "grey75")
      (mouse-color . "sienna1"))
     (default ((t (:background "black" :foreground "grey75"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "black" :foreground "#F8F8F8"))))
     (font-lock-builtin-face ((t (:foreground "#94bff3"))))
     (font-lock-comment-face ((t (:italic t :foreground "#00858F"))))
     (font-lock-constant-face ((t (:foreground "#7B70FF"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#FF8000"))))
     (font-lock-keyword-face ((t (:foreground "#FFFF00"))))
     (font-lock-preprocessor-face ((t (:bold t :foreground "#FFFF00"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (font-lock-string-face ((t (:foreground "#FF0000"))))
     (font-lock-type-face ((t (:foreground "medium blue"))))
     (font-lock-variable-name-face ((t (:foreground "#800080"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#253B76"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (highlight ((t (:background "#222222"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))
