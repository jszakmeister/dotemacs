(defvar user-packages
  '(evil
    evil-leader))

(dolist (p user-packages)
  (add-to-list 'dotemacs-packages p))
