(in-package :c4.5)

(defparameter *examples-1*
  (make-instance 'dataset :schema '(:day      :ignorable
                                    :outlook  :nominal
                                    :temp     :continuous
                                    :humidity :continuous
                                    :wind     :nominal
                                    :decision :nominal))
  "Examples for testing. To play golf or not was the question.")

(dolist (datum '(( 1 sunny    85 85 weak   no )
                 ( 2 sunny    80 90 strong no )
                 ( 3 overcast 83 78 weak   yes)
                 ( 4 rain     70 96 weak   yes)
                 ( 5 rain     68 80 weak   yes)
                 ( 6 rain     65 70 strong no )
                 ( 7 overcast 64 65 strong yes)
                 ( 8 sunny    72 95 weak   no )
                 ( 9 sunny    69 70 weak   yes)
                 (10 rain     75 80 weak   yes)
                 (11 sunny    75 70 strong yes)
                 (12 overcast 72 90 strong yes)
                 (13 overcast 81 75 weak   yes)
                 (14 rain     71 80 strong no )))
  (add-datum *examples-1* datum))


(defparameter *examples-2*
  (make-instance 'dataset :schema '(:day      :ignorable
                                    :outlook  :nominal
                                    :temp     :nominal
                                    :humidity :nominal
                                    :wind     :nominal
                                    :decision :nominal))
  "Examples for testing. To play golf or not was the question.")

(dolist (datum '(( 1 sunny    hot  high   weak   no )
                 ( 2 sunny    hot  high   strong no )
                 ( 3 overcast hot  high   weak   yes)
                 ( 4 rain     mild high   weak   yes)
                 ( 5 rain     cool normal weak   yes)
                 ( 6 rain     cool normal strong no )
                 ( 7 overcast cool normal strong yes)
                 ( 8 sunny    mild high   weak   no )
                 ( 9 sunny    cool normal weak   yes)
                 (10 rain     mild normal weak   yes)
                 (11 sunny    mild normal strong yes)
                 (12 overcast mild high   strong yes)
                 (13 overcast hot  normal weak   yes)
                 (14 rain     mild high   strong no )))
  (add-datum *examples-2* datum))


(defun test-split-info ()
  (= 0.8112781 (split-info (list :yes 6 :no 2)))
  (= 1.5774063 (split-info (attribute-values *examples-1* :outlook))))

(defun test-entropy ()
  (and
   (= 0.8112781 (entropy *examples-1* :decision *test-function* :wind 'weak))
   (= 1 (entropy *examples-1* :decision *test-function* :wind 'strong))
   (= 0.97095066 (entropy *examples-1* :decision *test-function* :outlook 'sunny))
   (= 0 (entropy *examples-1* :decision *test-function* :outlook 'overcast))
   (= 0.97095066 (entropy *examples-1* :decision *test-function* :outlook 'rain))))

(defun test-gain ()
  (= 0.048126936 (gain *examples-1* :decision :wind nil)))

(defun test-gain-ratio ()
  (= 0.048848517 (gain-ratio *examples-1* :decision :wind 80))
  (= 0.15642749 (gain-ratio *examples-1* :decision :outlook 'sunny)))

(defun test-all ()
  (list
   (test-split-info)
   (test-entropy)
   (test-gain)
   (test-gain-ratio)))
