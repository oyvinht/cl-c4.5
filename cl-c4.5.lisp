;;; The C4.5 algorithm implemented in Common Lisp.
;;
;; Author: Øyvin Halfdan Thuv - oyvinht@pvv.ntnu.no
;;
;; Copyright: See attached (BSD) license
;;
;;;----------------------------------------------------------------------------

(in-package :c4.5)


(defun split-info (attribute-counts)
  "Measure of the amount of information from distribution of attributes: The
less likely an observation is the more info it gives."
  (- (loop with total = (apply #'+ (plist-values attribute-counts))
           for attribute-value in (plist-keys attribute-counts)
           for p-val = (/ (getf attribute-counts attribute-value 0) total)
           unless (= p-val 0) ; Because lim log(x) = 0, as x -> 0
             sum (* p-val (log p-val 2)))))


(defun entropy (dataset class-attribute comparator attribute value)
  "A number between 0 and 1 that describes the heterogeneity of the dataset with
regard to CLASS-ATTRIBUTE. When ATTRIBUTE and VALUE are non-nil, entropy is
calculated for the subset of data created by filtering on ATTRIBUTE = VALUE."
  (loop with counts ; A plist of counts per different value per attribute
        for sample in (data dataset)
        when (or (null attribute)
                 (null value)
                 (funcall comparator (attribute-value sample attribute) value))
          do (loop for class-value
                     in (plist-keys (attribute-values dataset class-attribute))
                   when (funcall *test-function*
                                 (attribute-value sample class-attribute)
                                 class-value)
                     do (incf (getf counts class-value 0)))
        finally (return (split-info counts))))


(defun gain (dataset class-attribute attribute threshold)
  "Information gain for ATTRIBUTE when classifying on CLASS-ATTRIBUTE. For
continuous attributes, threshold must be specified."
  (let ((a-vals (attribute-values dataset attribute))
        (num-examples (length (data dataset))))
    (- (entropy dataset class-attribute *test-function* nil nil)
       (if (continuous-attribute-p dataset attribute)
           (+ (* (/ (loop for (key count) on a-vals by #'cddr
                          when (< key threshold)
                            sum count)
                    num-examples) ; Ratio of examples < threshold
                 (entropy dataset class-attribute #'< attribute threshold))
              (* (/ (loop for (key count) on a-vals by #'cddr
                          when (> key threshold)
                            sum count)
                    num-examples) ; Ratio of examples > threshold
                 (entropy dataset class-attribute #'> attribute threshold)))
           (loop for value in (plist-keys a-vals)
                 sum (* (/ (getf a-vals value) num-examples)
                        (entropy dataset class-attribute *test-function*
                                 attribute value)))))))


(defun gain-ratio (dataset class-attribute attribute threshold)
  "Relative gain for ATTRIBUTE with regard to classifying on CLASS-ATTRIBUTE"
  (/ (gain dataset class-attribute attribute threshold)
     (split-info
      (if (continuous-attribute-p dataset attribute)
          (attribute-values-by-threshold dataset attribute threshold)
          (attribute-values dataset attribute)))))


(defun most-info-gaining-attribute-value (dataset class-attribute attribute)
  "Find the value of ATTRIBUTE that yields the highest gain ratio."
  (let ((akeys (sort (plist-keys (attribute-values dataset attribute)) #'<)))
    (when (> *debug* 3)
      (format t "      Gain for ~a depends on threshold: ~a~%" attribute akeys))
    (loop
      with best with max
      for i from 0 to (- (length akeys) 3)
      do (let* ((between (/ (+ (nth i akeys) (nth (1+ i) akeys)) 2))
                (r (gain-ratio dataset class-attribute attribute between)))
           (when (> *debug* 3)
             (format t "      Ratio for ~a=~,3F is ~,3F~%" attribute between r))
           (when (or (null max) (> r max)) (setf best between max r)))
      finally (return (values best max)))))


(defun gain-per-attribute (attributes dataset class-attribute)
  "Return a list of ((attribute threshold) . gain). Threshold is nil for nominal
attributes."
  (loop
    for a in attributes
    collect (let ((res (if (continuous-attribute-p dataset a)
                           (multiple-value-bind (threshold gain)
                               (most-info-gaining-attribute-value
                                dataset class-attribute a)
                             (cons (list a threshold) gain))
                           (cons (list a nil)
                                 (gain-ratio dataset class-attribute a nil)))))
              (when (> *debug* 2) (format t "    ~a gain = ~a~%" a (cdr res)))
              res)))


(defun most-info-gaining-attribute (attributes dataset class-attribute)
  "Return the attribute that best separate examples."
  (when (> *debug* 1)
    (format t "  Inspecting gain of attributes ~a~%" attributes))
  (let* ((gains (gain-per-attribute attributes dataset class-attribute))
         (best (reduce (lambda (one two)
                         (if (> (cdr one) (cdr two)) one two))
                       gains)))
    (values (caar best) (cadar best))))


(defun same-class-p (dataset class-attribute)
  "Whether all items in DATASET have the same CLASS-ATTRIBUTE."
  (if (null (data dataset))
      t
      (let ((first-class
              (attribute-value (first (data dataset)) class-attribute)))
        (every (lambda (datum)
                 (funcall *test-function*
                          first-class
                          (attribute-value datum class-attribute)))
               (rest (data dataset))))))


(defun most-common-class-value (dataset class-attribute)
  "Find most common value of CLASS-ATTRIBUTE for items of DATASET."
  (unless (and (eq (type-of dataset) 'dataset)
               (not (null (data dataset))))
    (error 'type-error :format-control "~a is not a non-empty dataset"
                       :format-arguments (list dataset)))
  (loop with max-attr = 0 and max-value = 0
        for (attr value) on (attribute-values dataset class-attribute) by #'cddr
        do (when (> value max-value) (setf max-attr attr max-value value))
        finally (return max-attr)))


(defun build-tree (dataset class-attribute)
  (let ((attrs (remove-if (lambda (a)
                            (or (eq a class-attribute)
                                (eq (attribute-type dataset a) :ignorable)))
                          (plist-keys (schema dataset)))))
    (when (> *debug* 0) (format t "~%"))
    (if (same-class-p dataset class-attribute)
        (let ((aval (attribute-value (first (data dataset)) class-attribute)))
          (when (> *debug* 0)
            (format t "All in same class -> Leaf node: ~a~%" aval))
          aval)
        (progn
          (when (> *debug* 0)
            (format t "Spliting dataset of size ~a on best attribute~%"
                    (length (data dataset))))
          (multiple-value-bind (best threshold)
              (most-info-gaining-attribute attrs dataset class-attribute)
            (when (> *debug* 0) (format t "Best attribute was ~a " best))
            (if (continuous-attribute-p dataset best)
                (let ((subsets (data-subsets dataset best threshold)))
                  (when (> *debug* 0)
                    (format t "(threshold ~a) which is continuous~%" threshold)
                    (format t "Splitting gives subsets of lengths: ~a and ~a~%"
                            (length (data (first subsets)))
                            (length (data (second subsets)))))
                  (if (or (null (data (first subsets)))
                          (null (data (second subsets))))
                      (progn
                        (format t "One subset is empty: Picking best class")
                        (most-common-class-value dataset class-attribute))
                      (progn
                         (when (> *debug* 0)
                           (format t "Building trees for each subset~%"))
                         (cons best
                               (list (list (intern (format nil "<~f" threshold))
                                           (build-tree (first subsets)
                                                       class-attribute))
                                     (list (intern (format nil ">~f" threshold))
                                           (build-tree (second subsets)
                                                       class-attribute)))))))
                (progn
                  (when (> *debug* 0)
                    (format t "which is nominal~%")
                    (format t "Creating branches for each value: ~A~%"
                            (plist-keys (attribute-values dataset best))))
                  (cons
                   best
                   (loop
                     for value in (plist-keys (attribute-values dataset best))
                     collect (let ((subset (data-subset dataset best value)))
                               (list value (build-tree
                                            subset
                                            class-attribute))))))))))))


;; Algorithm usage utilities
;;-----------------------------------------------------------------------------
(defun classify (instance tree dataset)
  "Classify instance given decision tree."
  (cond
    ((atom tree)
     (when (> *debug* 0) (format t "Reached leaf -> Answer = ~a~%" tree))
     tree)
    ((continuous-attribute-p dataset (car tree))
     (when (> *debug* 0) (format t "Continuous attribute ~a" (car tree)))
     (let*
         ((node-name (symbol-name (caar (rest tree))))
          (threshold (read-from-string (subseq node-name 1 (length node-name))))
          (value (getf instance (first tree))))
       (classify instance
                 (cond ((< value threshold)
                        (when (> *debug* 0)
                          (format t " ~a < ~a -> Choosing subtree: ~a~%"
                                  value threshold (cadr (first (rest tree)))))
                        (cadr (first (rest tree))))
                       (t
                        (when (> *debug* 0)
                          (format t " ~a > ~a -> Choosing subtree: ~a~%"
                                  value threshold (cadr (second (rest tree)))))
                        (cadr (second (rest tree)))))
                 dataset)))
    (t
     (let ((subtree (assoc (getf instance (first tree)) (rest tree))))
       (when (> *debug* 0)
         (format t "At branch ~a -> Choosing subtree below answer ~a: ~a~%"
                 (first tree) (car subtree) (cadr subtree)))
       (classify instance (cadr subtree) dataset)))))


(defun accuracy (dataset tree class-attribute)
  "Feed all examples through tree and return percentage match"
  (loop with correct = 0
        for datum in (data dataset)
        when (eq (attribute-value datum class-attribute)
                 (classify (datum->plist dataset datum) tree dataset))
          do (incf correct)
        finally (return (/ correct (length (data dataset))))))
