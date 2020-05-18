;;; vim: autoindent expandtab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iapi_request 函数里面需要解析此格式，从而获得protocol
(deffunction pppfact (?fact)
   ;(ppfact ?fact nil)
    (implode$ (explode$ (ppfact ?fact nil)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod request ((?server SYMBOL) (?fact FACT-ADDRESS))
    (request ?server (pppfact ?fact))
    (retract ?fact)
)
(defmethod request ((?server SYMBOL) (?fact LEXEME))
    (vim [ "call", "Tapi_request", [ (str-cat ?server), (str-cat ?fact)]])
)
;;;-----------------------------------------------------------------------------
(defmethod respond ((?client SYMBOL) (?fact FACT-ADDRESS))
    (respond ?client (pppfact ?fact))
    (retract ?fact)
)
(defmethod respond ((?client SYMBOL) (?fact LEXEME))
    (vim [ "call", "Tapi_respond", [ (str-cat ?client), (str-cat ?fact)]])
)
;;;-----------------------------------------------------------------------------
(deffunction push (?named ?fact)
    (vim [ "call", "Tapi_push", [ (str-cat ?named), (pppfact ?fact)]])
    (retract ?fact)
)
;;;-----------------------------------------------------------------------------
(deffunction store_something (?table)
    (printout t (join$ ?table (format nil "%n")) crlf)
    (vim [ "call", "Tapi_store_something", (join$ ?table "<CR>") ])
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;=============================================================================
;;; query instances
;(defclass A (is-a USER)
;    (slot uuid  (type LEXEME))
;    (slot named (type LEXEME))
;)
;(defclass B (is-a USER)
;    (slot uuid  (type LEXEME))
;    (slot age   (type INTEGER))
;)
;(definstances AB
;    (of A (uuid ABC) (named 你好))
;    (of A (uuid DEF) (named 我好))
;    (of A (uuid GHI) (named 她好))
;
;    (of B (uuid ABC) (age 10))
;    (of B (uuid DEF) (age 20))
;    (of B (uuid GHI) (age 30))
;)
;(query instances "(?A A) (?B B)" "(eq (send ?A get-uuid)(send ?B get-uuid))" "user"
;    "\"(named %s)\" (send ?A get-named)"
;    "\"(age   %d)\" (send ?B get-age  )"
;)
;;;-----------------------------------------------------------------------------
;;; query facts
;(deftemplate A
;    (slot uuid  (type LEXEME))
;    (slot named (type LEXEME))
;)
;(deftemplate B
;    (slot uuid  (type LEXEME))
;    (slot age   (type INTEGER))
;)
;(deffacts AB
;    (A (uuid ABC) (named 你好))
;    (A (uuid DEF) (named 我好))
;    (A (uuid GHI) (named 她好))
;
;    (B (uuid ABC) (age 10))
;    (B (uuid DEF) (age 20))
;    (B (uuid GHI) (age 30))
;)
;(query facts "(?A A) (?B B)" "(eq ?A:uuid ?B:uuid)" "user"
;    "\"(named %s)\" ?A:named"
;    "\"(age   %d)\" ?B:age  "
;)
;;;-----------------------------------------------------------------------------
(deffunction query (?facts-or-instances ?classes ?filter ?report $?fields)
    ;;; ?facts-or-instances: facts instances
    ;;; ?classes: (?A A) (?B B)
    ;;; ?filter : (eq (send ?A get-uuid)(send ?B get-uuid))
    ;;; ?report : user
    ;;; ?fields : "\"(named %s)\" (send ?A get-named)"
    ;;;           "\"(age   %s)\" (send ?B get-age  )"
    (bind ?function (sym-cat query-local-function-(gensym*)))
    (bind ?build (create$))

    (bind ?build (append-cat$ ?build (indent 0) (L)deffunction(S)?function"()"                                                  ))
    (bind ?build (append-cat$ ?build (indent 1)     "(bind ?table (create$))"                                                   ))
    (bind ?build (append-cat$ ?build (indent 1)     (L)"do-for-all-"?facts-or-instances" ("?classes") " ?filter                 ))
    (bind ?build (append-cat$ ?build (indent 2)         "(bind ?record (create$))"                                              ))
    (progn$ (?field ?fields)
    (bind ?build (append-cat$ ?build (indent 2)         "(bind ?record (append$ ?record (format nil "?field")))"                )))
    (bind ?build (append-cat$ ?build (indent 2)         "(bind ?table (append-cat$ ?table (L)"?report"(S)(join$ ?record)(R)))"  ))
    (bind ?build (append-cat$ ?build (indent 1)     (R)                                                                         ))
    (bind ?build (append-cat$ ?build (indent 1)     "(return ?table)"                                                           ))
    (bind ?build (append-cat$ ?build (indent 0) (R)                                                                             ))

    (build (join$ ?build (N)))
    (ppdeffunction ?function)
    (bind ?result (eval (str-cat "(" ?function ")")))
    (undeffunction ?function)
    (return ?result)
)
;;;=============================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction list (?class $?slots)
    (bind ?table (create$))
    (do-for-all-instances ((?O ?class)) TRUE
        (bind ?record (create$))
        (progn$ (?slot ?slots)
            (bind ?value (send ?O (sym-cat get- ?slot)))
            (bind ?value (switch (type ?value)
                (case STRING then (Q ?value))
                (default             ?value )
            ))
            (bind ?record (append$ ?record (format nil "(%s %s)" ?slot ?value)))
        )
        (bind ?table (append$ ?table (format nil "(%s %s)" ?class (join$ ?record))))
    )
    (store_something ?table)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
