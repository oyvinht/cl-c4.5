(in-package :c4.5)

(defun plist-keys (plist)
  "Get just the keys of a plist."
  (loop for key in plist by #'cddr collecting key))

(defun plist-values (plist)
  "Get just the values of a plist."
  (loop for value in (rest plist) by #'cddr collecting value))
