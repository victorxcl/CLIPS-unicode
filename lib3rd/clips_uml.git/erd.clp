;;; vim:expandtab autoindent
;;; map <buffer> \\ :update<CR>:!clips -f erd.clp<CR>:!dot -Tpdf -O 学校.ERD.dot<CR>:!open 学校.ERD.dot.pdf<CR>
(load (clips_uml "erd/core.clp"))

(make-module (get-current-module) DOT
    (clips_uml "expression/core.clp")
    (clips_uml "utility/graphviz.clp")
    (clips_uml "erd/dot.clp"))

(make-module (get-current-module) CPP
    (clips_uml "expression/core.clp")
    (clips_uml "erd/cpp.clp"))

(make-module (get-current-module) JAVA
    (clips_uml "expression/core.clp")
    (clips_uml "erd/java.clp"))

(make-module (get-current-module) JAVASCRIPT
    (clips_uml "expression/core.clp")
    (clips_uml "erd/javascript.clp"))

