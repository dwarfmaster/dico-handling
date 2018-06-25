
;; The problem ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass unknown-entity-problem    (problem) ())
(defclass unknown-attribute-problem (problem) ())

;; The diagnostic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass diagnose-unknown-entity (diagnostic)
  ((trigger :initform 'new-node)))
(defclass diagnose-unknown-attribute (diagnostic)
  ((trigger :initform 'new-node)))

;; TODO factor the following two methods
(defmethod diagnose ((diagnostic diagnose-unknown-entity) (node cip-node)
                     &key &allow-other-keys)
  "Check if there are still words in ROOT in a fully expanded node"
  (when (fully-expanded? node)
    (let ((strings-in-root (get-strings (assoc 'root
                                               (left-pole-structure
                                                 (car-resulting-cfs (cipn-car node)))))))
      (when strings-in-root
        (let ((problem (make-instance 'unknown-entity-problem)))
          (set-data problem 'strings strings-in-root)
          problem)))))

(defmethod diagnose ((diagnostic diagnose-unknown-attribute) (node cip-node)
                     &key &allow-other-keys)
  "Check if there are still words in ROOT in a fully expanded node"
  (when (fully-expanded? node)
    (let ((strings-in-root (get-strings (assoc 'root
                                               (left-pole-structure
                                                 (car-resulting-cfs (cipn-car node)))))))
      (when strings-in-root
        (let ((problem (make-instance 'unknown-attribute-problem)))
          (set-data problem 'strings strings-in-root)
          problem)))))

;; Repairs 1 : add entity ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass unknown-word-is-entity (repair)
  ((trigger :initform 'new-node)))

(defmethod repair ((repair  unknown-word-is-entity)
                   (problem unknown-entity-problem)
                   (node    cip-node)
                   &key &allow-other-keys)
  (let ((uw (first (get-data problem 'strings))))
    (multiple-value-bind (cxn-set lex-cxn)
      (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append uw "-ent-cxn")))
                          ((?word-unit)
                           (?word-ref
                             (meaning-unit ?word-unit)
                             (first        ?word-unit)
                             (last         ?word-unit))
                           <-
                           (?word-unit
                             (HASH meaning ((frame ?fent Entity            ?word-unit ?word-unit)
                                            (fe    ?fent Entity            ?fe-ent)
                                            (fe    ?fent Constituent_parts ?fe-const)
                                            (fe    ?fent Formational_cause ?fe-form)
                                            (fe    ?fent Name              ?fe-name)
                                            (fe    ?fent Type              ?fe-type)))
                             --
                             (HASH form ((string ?word-unit ,uw)))
                             )
                           )
                          :cxn-inventory ,(copy-object (original-cxn-set (construction-inventory node)))))
      (declare (ignore cxn-set))
      (make-instance 'cxn-fix
                     :repair  repair
                     :problem problem
                     :restart-data lex-cxn))))

;; Repairs 2 : add attribute ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass unknown-word-is-attribute (repair)
  ((trigger :initform 'new-node)))

(defmethod repair ((repair  unknown-word-is-attribute)
                   (problem unknown-attribute-problem)
                   (node    cip-node)
                   &key &allow-other-keys)
  (let ((uw (first (get-data problem 'strings))))
    (multiple-value-bind (cxn-set lex-cxn)
      (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append uw "-attr-cxn")))
                          ((?word-unit)
                           (?word-ref
                             (meaning-unit ?word-unit)
                             (first        ?word-unit)
                             (last         ?word-unit))
                           <-
                           (?word-unit
                             (HASH meaning ((frame ?fattr Attributes ?word-unit ?word-unit)
                                            (fe    ?fattr Entity     ?fe-ent)
                                            (fe    ?fattr Attribute  ?fe-attr)
                                            (fe    ?fattr Value      ?fe-value)))
                             --
                             (HASH form ((string ?word-unit ,uw)))
                             )
                           )
                          :cxn-inventory ,(copy-object (original-cxn-set (construction-inventory node)))))
      (declare (ignore cxn-set))
      (make-instance 'cxn-fix
                     :repair  repair
                     :problem problem
                     :restart-data lex-cxn))))

;; TODO use anti-unification for repairs

