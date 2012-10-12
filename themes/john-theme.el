;; John's color theme.
;;
;; It's a modified version of the blackboard theme, that incorporates
;; some of my settings from SlickEdit, and is made to degrade well
;; at the terminal.
;;
;; Based on JD Huntington's Blackboard theme, which has the following license:
;;
;; MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;; Credits due to the excellent TextMate Blackboard theme

(deftheme john 
  "Based on Color theme by JD Huntington, which based off the TextMate Blackboard theme, created 2008-11-27")

(custom-theme-set-faces
 `john
 `(default ((t (:background "black" :foreground "grey75" ))))
 `(bold ((t (:bold t))))
 `(bold-italic ((t (:bold t))))
 `(border-glyph ((t (nil))))
 `(buffers-tab ((t (:background "black" :foreground "#F8F8F8"))))
 `(font-lock-builtin-face ((t (:foreground "#94bff3"))))
 `(font-lock-comment-face ((t (:italic t :foreground "#00858F"))))
 `(font-lock-constant-face ((t (:foreground "#7B70FF"))))
 `(font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
 `(font-lock-function-name-face ((t (:foreground "#FF8000"))))
 `(font-lock-keyword-face ((t (:foreground "#FFFF00"))))
 `(font-lock-preprocessor-face ((t (:bold t :foreground "#FFFF00"))))
 `(font-lock-reference-face ((t (:foreground "SlateBlue"))))

 `(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "red"))))

 ;; org-mode
 `(org-hide ((t (:foreground "#2e3436"))))
 `(org-level-1 ((t (:bold nil :foreground "dodger blue" :height 1.3))))
 `(org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.2))))
 `(org-level-3 ((t (:bold nil :foreground "#6ac214" :height 1.1))))
 `(org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))
 `(org-date ((t (:underline t :foreground "magenta3"))))
 `(org-footnote  ((t (:underline t :foreground "magenta3"))))
 `(org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
 `(org-special-keyword ((t (:foreground "brown"))))
 `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 `(org-block ((t (:foreground "#bbbbbc"))))
 `(org-quote ((t (:inherit org-block :slant italic))))
 `(org-verse ((t (:inherit org-block :slant italic))))
 `(org-todo ((t (:bold t :foreground "Red"))))
 `(org-done ((t (:bold t :foreground "ForestGreen"))))
 `(org-agenda-structure ((t (:weight bold :foreground "tomato"))))
 `(org-agenda-date ((t (:foreground "#6ac214"))))
 `(org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
 `(org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))

 `(font-lock-string-face ((t (:foreground "#FF0000"))))
 `(font-lock-type-face ((t (:foreground "medium blue"))))
 `(font-lock-variable-name-face ((t (:foreground "#800080"))))
 `(font-lock-warning-face ((t (:bold t :foreground "pink"))))
 `(gui-element ((t (:background "#D4D0C8" :foreground "black"))))
 `(region ((t (:background "#253B76"))))
 `(mode-line ((t (:background "grey75" :foreground "black" :box nil))))
 `(highlight ((t (:background "#222222"))))
 `(highline-face ((t (:background "SeaGreen"))))
 `(italic ((t (nil))))
 `(left-margin ((t (nil))))
 `(text-cursor ((t (:background "yellow" :foreground "black"))))
 `(toolbar ((t (nil))))
 `(underline ((nil (:underline nil))))
 `(zmacs-region ((t (:background "snow" :foreground "ble")))))

(provide-theme 'john)
