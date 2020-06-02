;;; vim: expandtab autoindent
;;; map <buffer> \\ :update<CR>:1tabnext<CR>:2wincmd w<CR>:silent normal \\<CR>:2tabnext<CR>

(load (clips_uml "wireframe/core.clp"))
(load (clips_uml "wireframe/function.clp"))
(load (clips_uml "wireframe/rule.clp"))

(make-module (get-current-module) GraphViz
    (clips_uml "expression/core.clp")
    (clips_uml "utility/graphviz.clp")
    (clips_uml "wireframe/generate/graphviz.clp"))

(make-module (get-current-module) ReactJS
    (clips_uml "expression/core.clp")
    (clips_uml "wireframe/generate/reactjs.clp"))

