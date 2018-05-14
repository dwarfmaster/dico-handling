

;;  ____            _       __        __            _     
;; | __ )  __ _ ___(_) ___  \ \      / /__  _ __ __| |___ 
;; |  _ \ / _` / __| |/ __|  \ \ /\ / / _ \| '__/ _` / __|
;; | |_) | (_| \__ \ | (__    \ V  V / (_) | | | (_| \__ \
;; |____/ \__,_|___/_|\___|    \_/\_/ \___/|_|  \__,_|___/
;;                                                        

(def-fcg-cxn he-cxn
             ((?he-ref-unit
                (meaning-unit ?he-unit)
                (first ?he-unit)
                (last  ?he-unit)
                )
              (?he-unit)
              <-
              (?he-unit
                (HASH meaning ((frame ?fsi Sole_instance   ?he-unit)
                               (frame ?fpe Physical_entity ?he-unit)
                               (fe ?fsi Item   ?x)
                               (fe ?fsi Type   context)
                               (fe ?fpe Entity ?x)
                               ))
               --
               (HASH form ((string ?he-unit "he"))))))

(def-fcg-cxn the-cxn
             ((?the-ref-unit
                (meaning-unit ?ent-unit)
                (first ?the-unit)
                (last  ?ent-unit-lst)
                )
              (?ent-unit-ref
                (superseeded-by ?the-ref-unit)
                )
              (?ent-unit
                (meaning ((frame ?fsi Sole_Instance ?the-unit)
                          (fe    ?fsi Item          ?ent)
                          (fe    ?fsi Type          context)
                          ))
                )
              <-
              (?ent-unit-ref
                --
                (meaning-unit ?ent-unit)
                (first ?ent-unit-fst)
                (last  ?ent-unit-lst)
                (tree-structure (not (superseeded-by ?super)))
                )
              (?ent-unit
                --
                (meaning ((frame ?fent Entity ?fr-ent-unit)
                          (fe    ?fent Entity ?ent)
                          ))
                )
              (?the-unit
                --
                (HASH form ((string ?the-unit "the")
                            (meets ?the-unit ?ent-unit-fst)
                            ))
                )
              )
             )

(def-fcg-cxn a-cxn
             ((?a-ref-unit
                (meaning-unit ?ent-unit)
                (first ?a-unit)
                (last  ?ent-unit-lst)
                )
              (?ent-unit-ref
                (tree-structure (superseeded-by ?a-ref-unit))
                )
              <-
              (?ent-unit-ref
                --
                (meaning-unit ?ent-unit)
                (first ?ent-unit-fst)
                (last  ?ent-unit-lst)
                (tree-structure (not (superseeded-by ?super)))
                )
              (?ent-unit
                --
                (meaning ((frame ?fent Entity ?fr-ent-unit)
                          (fe    ?fent Entity ?ent)
                          ))
                )
              (?a-unit
                --
                (HASH form ((string ?a-unit "a")
                            (meets  ?a-unit ?ent-unit-fst)
                            ))
                )
              )
             )


;;   ____                _                   _   _                 
;;  / ___|___  _ __  ___| |_ _ __ _   _  ___| |_(_) ___  _ __  ___ 
;; | |   / _ \| '_ \/ __| __| '__| | | |/ __| __| |/ _ \| '_ \/ __|
;; | |__| (_) | | | \__ \ |_| |  | |_| | (__| |_| | (_) | | | \__ \
;;  \____\___/|_| |_|___/\__|_|   \__,_|\___|\__|_|\___/|_| |_|___/
;;                                                                 

(def-fcg-cxn adj-noun-cxn
             ((?adj-noun-unit
                (meaning-unit ?noun-unit)
                (first ?attr-unit-fst)
                (last  ?ent-unit-lst)
                )
              (?ent-unit-ref
                (tree-structure (superseeded-by ?adj-noun-unit))
                )
              (?attr-unit-ref
                (tree-structure (superseeded-by ?adj-noun-unit))
                )
              <-
              (?ent-unit-ref
                --
                (meaning-unit ?ent-unit)
                (first ?ent-unit-fst)
                (last  ?ent-unit-lst)
                (tree-structure (not (superseeded-by ?super-ent)))
                )
              (?ent-unit
                --
                (meaning ((frame ?fent Entity ?fr-ent-unit)
                          (fe    ?fent Entity ?ent)
                          ))
                )
              (?attr-unit-ref
                --
                (meaning-unit ?attr-unit)
                (first ?attr-unit-fst)
                (last  ?attr-unit-lst)
                (tree-structure (not (superseeded-by ?super-attr)))
                )
              (?attr-unit
                --
                (meaning ((frame ?fattr Attributes ?fr-attr-unit)
                          (fe    ?fattr Entity     ?ent)
                          ))
                )
              )
             )

