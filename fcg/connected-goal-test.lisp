
(defparameter *connected-goal-socket* nil)

(defun open-connected-goal-socket (&optional (address "localhost") (port 4444))
  (defparameter *connected-goal-socket* (usocket:socket-connect address port)))

(defun exists (pred lst)
  "Check if pred is true on at least one element of lst"
  (let ((result nil))
    (loop for x in lst
          until result
          do (if (funcall pred x) (setf result t) nil))
    result))

(defun get-root-from-node (node)
  "Returns the root unit in the node"
  (let ((cfs (car-resulting-cfs (cipn-car node))))
    (loop for unit in (left-pole-structure cfs)
       do (if (and (consp unit) (eql (car unit) 'root)) (return-from get-root-from-node unit)))))

(defun get-node-sequence (node)
  (let ((root-unit (get-root-from-node node)))
    (loop for feature in (cdr root-unit)
       do (if (and (consp feature) (eql (car feature) 'form))
              (loop for form in (car (cdr feature))
                 do (if (and (consp form) (eql (car form) 'sequence)) (return-from get-node-sequence (cdr form))))))))

(defun unit-to-structure (unit)
  "Gets a unit and returns a (unit-name unit-superseed unit-first-word unit-last-word)"
  (let ((name (unit-name unit))
        (supers nil)
        (fst    nil)
        (lst    nil))
    (loop for feature in unit
       do (cond
            ((not (consp feature)) t)
            ((eql (car feature) 'first) (setf fst (car (cdr feature))))
            ((eql (car feature) 'last)  (setf lst (car (cdr feature))))
            ((eql (car feature) 'tree-structure) (setf supers (car (cdr (car (car (cdr feature)))))))))
    (if supers (list (list name supers fst lst)) nil)))

(defun node-to-structure (node)
  (loop for unit in (left-pole-structure (car-resulting-cfs (cipn-car node)))
     append (unit-to-structure unit)))

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

(defun get-unit-meaning (unit)
  "Return the list of meanings in the unit"
  (loop for feature in unit
     do (if (and (consp feature) (eql (car feature) 'meaning))
              (return-from get-unit-meaning (cdr feature)))))

(defun extract-meaning-from-node (node)
  "Return the union of all meaning in the node"
  (car (loop for unit in (left-pole-structure
                      (car-resulting-cfs (cipn-car node)))
      append (get-unit-meaning unit))))

(defun connected-goal-send-data (node)
  (format t "Connection test~%")
  (let ((meaning (extract-meaning-from-node node))
        (tree    (node-to-structure         node))
        (seq     (get-node-sequence         node))
        (strm    (usocket:socket-stream *connected-goal-socket*)))
    (format t "Sending: ~A~%" (list meaning tree seq))
    (force-output t)
    (format strm "~A~%" (list meaning tree seq))
    (force-output strm)))

(defun remote-validation ()
  "Returns t if the remote valids the entry, nil otherwise"
  (let* ((input (read-line (usocket:socket-stream *connected-goal-socket*)))
         (val   (read-from-string input)))
    val))

(defmethod cip-goal-test ((node cip-node) (mode (eql :forest-is-tree)))
  (if (not (eql (count-forest-roots node) 1))
        nil
        (progn (connected-goal-send-data node)
               (remote-validation))))

