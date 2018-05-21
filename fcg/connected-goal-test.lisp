
(defun exists (pred lst)
  "Check if pred is true on at least one element of lst"
  (defparameter result nil)
  (loop for x in lst
        until result
        do (if (funcall pred x) (setf result t) nil)
        )
  result
  )

(defun count-forest-roots (node)
  "Count the number of reference units not superseeded in the node"
  (loop for unit in (left-pole-structure
                      (car-resulting-cfs (cipn-car node)))
        sum (let ((meaning-unit-superseeded 
                    (if (exists (lambda (x) (eql (car x) 'meaning-unit))
                                (cdr unit))
                           (exists (lambda (x) (eql (car x) 'tree-structure))
                                   (cdr unit))
                           t)))
              (if meaning-unit-superseeded 0 1))))

;; For debugging purposes only
;; (defun clean-cfs-structure (struct)
;;   (map 'list
;;        (lambda (x) (remove-if (lambda (y) (and (consp y)
;;                                                (or (eql (car y) 'footprints)
;;                                                    (eql (car y) 'meaning))
;;                                                ))
;;                               x))
;;        struct)
;;   )

(defmethod cip-goal-test ((node cip-node) (mode (eql :forest-is-tree)))
  (eql (count-forest-roots node) 1))

