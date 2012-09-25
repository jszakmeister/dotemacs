; Powerline needs cl, but has an eval-when-compile in it,
; so it's not brought in if powerline is byte-compiled.
; So require it here, that way it's available to powerline.
(require 'cl)

(defvar user-packages
  '(evil
    evil-leader))

(dolist (p user-packages)
  (add-to-list 'dotemacs-packages p))
