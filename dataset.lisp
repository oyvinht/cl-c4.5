(in-package :c4.5)


(defclass dataset ()
  ((data :accessor data :initarg :data :initform nil)
   (schema :initarg :schema :reader schema)))


(defmethod initialize-instance :after ((d dataset) &key)
  (unless (and (slot-boundp d 'schema)
               (listp (slot-value d 'schema))
               (evenp (length (slot-value d 'schema))))
    (error
     (make-condition 'simple-error
                     :format-control
                     "The schema was ~a but should be a plist on the form~%~
                      (ATTRIBUTE-1 TYPE ATTRIBUTE-2 TYPE)~%~
                      with TYPE being one of (:continuous :ignorable :nominal)."
                     :format-arguments (list (when (slot-boundp d 'schema)
                                               (slot-value d 'schema)))))))


(defclass datum ()
  ((dataset :initarg :dataset :reader dataset)
   (value-list :initarg :value-list :reader value-list)))


(defmethod attribute-index ((dataset dataset) attribute)
  (search (list attribute) (plist-keys (schema dataset))))


(defmethod attribute-type ((dataset dataset) attribute)
  (getf (schema dataset) attribute))


(defmethod attribute-value ((datum datum) attribute)
  (let ((index (search (list attribute) (plist-keys (schema (dataset datum))))))
    (nth index (value-list datum))))


(defmethod attribute-values ((dataset dataset) attribute)
  "Collect a plist with counts of values accross all samples."
  (loop with counts
        for sample in (data dataset)
        do (incf (getf counts (attribute-value sample attribute) 0))
        finally (return counts)))


(defmethod attribute-values-by-threshold ((dataset dataset) attribute threshold)
  "Split attributes into counts for :<= and :>"
  (loop with counts
        for sample in (data dataset)
        for value = (attribute-value sample attribute)
        when value
          do (incf (getf counts (if (> value threshold) :> :<=) 0))
        finally (return counts)))


(defmethod add-datum ((dataset dataset) (value-list list))
  (push (make-instance 'datum :dataset dataset :value-list value-list)
        (data dataset)))


(defmethod continuous-attribute-p ((dataset dataset) attribute)
  (eq (attribute-type dataset attribute) :continuous))


(defmethod data-subset ((dataset dataset) attribute value)
  (let ((schema (copy-list (schema dataset))))
    (remf schema attribute)
    (make-instance 'dataset
                   :schema schema
                   :data (loop for datum in (data dataset)
                               when (funcall *test-function*
                                             (attribute-value datum attribute)
                                             value)
                                 collect datum))))


(defmethod data-subsets ((dataset dataset) attribute threshold)
  "Return a list of subsets of dataset with attribute value <= threshold and
attribute value > threshold."
  (loop for datum in (data dataset)
        if (<= (attribute-value datum attribute) threshold)
          collect datum into less-or-equal
        else
          collect datum into more
        finally (return
                  (let ((schema (copy-list (schema dataset))))
                    (remf schema attribute)
                    (list (make-instance
                           'dataset :schema schema :data less-or-equal)
                          (make-instance
                           'dataset :schema schema :data more))))))


(defmethod datum->plist ((dataset dataset) (datum datum))
  (loop for key in (schema dataset) by #'cddr
        for val in (value-list datum)
        nconcing (list key val)))
