;;; vim:expandtab autoindent
;;; map <buffer> \\ :update<CR>:!clips -f fsm.clp<CR>:!dot -Tpdf -O 状态机.FSM.dot<CR>:!open 状态机.FSM.dot.pdf<CR>

(load (clips_uml "fsm/core.clp"))

(make-module (get-current-module) DOT
    (clips_uml "expression/core.clp")
    (clips_uml "utility/graphviz.clp")
    (clips_uml "fsm/expression.clp")
    (clips_uml "fsm/rule.clp")
    (clips_uml "fsm/intermediate/core.clp")
    (clips_uml "fsm/intermediate/rule.clp")
    (clips_uml "fsm/generate/dot.clp"))

(make-module (get-current-module) CPP
    (clips_uml "expression/core.clp")
    (clips_uml "fsm/expression.clp")
    (clips_uml "fsm/rule.clp")
    (clips_uml "fsm/generate/cpp.clp"))

(make-module (get-current-module) JAVA
    (clips_uml "expression/core.clp")
    (clips_uml "fsm/expression.clp")
    (clips_uml "fsm/rule.clp")
    (clips_uml "fsm/intermediate/core.clp")
    (clips_uml "fsm/intermediate/rule.clp")
    (clips_uml "fsm/generate/java.clp"))

(make-module (get-current-module) JAVASCRIPT
    (clips_uml "expression/core.clp")
    (clips_uml "fsm/expression.clp")
    (clips_uml "fsm/rule.clp")
    (clips_uml "fsm/generate/javascript.clp"))

