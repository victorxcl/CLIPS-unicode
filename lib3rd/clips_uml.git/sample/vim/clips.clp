;;; vim:expandtab autoindent

(deffunction clips_uml ($?path) (str-cat ../../ (expand$ ?path)))
(deffunction clips_cwd ($?path) (str-cat     ./ (expand$ ?path)))

(defmodule MAIN (export ?ALL))
(batch* (clips_uml utility.clp))
(batch* (clips_uml utility/vim.clp))

(defrule 测试
    ?f <- (测试 ?client ?hello)
=>
    (retract ?f)
   ;(vim [ "call", "Tapi_response", {
   ;    "client": (str-cat ?client),
   ;     "hello": (str-cat ?hello),
   ;      "time": (str-cat (time)),
   ;}])
    (Tapi_response {
        "client": (str-cat ?client),
         "hello": (str-cat ?hello),
          "time": (str-cat (time)),
    })
)

(watch all)
