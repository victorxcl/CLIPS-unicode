;;; vim:expandtab autoindent

(defrule 准备Intermediate#Machine数据
    (object (is-a Machine)
            (状态机    ?状态机)
            (初始状态  ?初始状态)
            (初始动作 $?初始动作)
            (销毁动作 $?销毁动作)
            (类        ?类))
=>
    (build "(defmethod action ($?x) (Expression#FSM#Invoke Action (expand$ ?x)))")
    (make-instance of Intermediate#Machine 
        (名称     ?状态机)
        (类       ?类)
        (初始动作 (Intermediate#Encode ?初始动作))
        (销毁动作 (Intermediate#Encode ?销毁动作))
    )
    (undefmethod action *)
)
(defrule 准备Intermediate#Machine数据-初始状态
    (object (is-a              Machine)(状态机 ?状态机 )(初始状态 ?初始状态))
    (object (is-a Intermediate#Machine)(名称   ?状态机 )                    (name ?machine))
    (object (is-a Intermediate#State  )(状态机 ?machine)(名称     ?初始状态)(name ?state))
=>
    (modify-instance ?machine (初始状态 ?state))
)
(defrule 准备Intermediate#Machine数据-切面
    (object (is-a Intermediate#Machine)(名称   ?状态机)(name ?machine))
    (object (is-a               Aspect)(状态机 ?状态机)(切面 ?切面)(脚本 $?脚本))
    (object (is-a Intermediate#Machine)(名称   ?切面  )(name ?aspect))
=>
    (make-instance of Intermediate#Aspect
        (状态机 ?machine)
        (切面   ?aspect)
        (脚本   (Intermediate#Encode ?脚本))
    )
)
(defrule 准备Intermediate#Machine数据-触发器
         (object (is-a Intermediate#Machine)(名称   ?状态机 )(name ?machine))
         (object (is-a              Trigger)(状态机 ?状态机 )(触发器 ?触发器1)(前置脚本 $?前置脚本)(后置脚本 $?后置脚本))
    (not (object (is-a Intermediate#Trigger)(状态机 ?machine)(名称   ?触发器2&:(expr-equal ?触发器1 (send ?触发器2 get-代码)))))
=>
    (build "(defmethod trigger ($?x) (Expression#FSM#Define Trigger (expand$ ?x)))")
    (build "(defmethod action  ($?x) (Expression#FSM#Invoke Action  (expand$ ?x)))")
    (make-instance of Intermediate#Trigger 
        (状态机   ?machine)
        (名称     (Intermediate#Encode ?触发器1 ))
        (前置脚本 (Intermediate#Encode ?前置脚本))
        (后置脚本 (Intermediate#Encode ?后置脚本))
    )
    (undefmethod action  *)
    (undefmethod trigger *)
)
(defrule 准备Intermediate#Machine数据-守卫
         (object (is-a Intermediate#Machine)(名称   ?状态机 )(name ?machine))
         (object (is-a              Guard )(状态机 ?状态机 )(守卫 ?守卫1)(脚本 $?脚本))
    (not (object (is-a Intermediate#Guard )(状态机 ?machine)(名称 ?守卫2&:(expr-equal ?守卫1 (send ?守卫2 get-代码)))))
=>
    (build "(defmethod guard  ($?x) (Expression#FSM#Define Guard  (expand$ ?x)))")
    (build "(defmethod action ($?x) (Expression#FSM#Invoke Action (expand$ ?x)))")
    (make-instance of Intermediate#Guard 
        (状态机 ?machine)
        (名称   (Intermediate#Encode ?守卫1))
        (脚本   (Intermediate#Encode ?脚本))
    )
    (undefmethod guard  *)
    (undefmethod action *)
)
(defrule 准备Intermediate#Machine数据-动作
         (object (is-a Intermediate#Machine)(名称   ?状态机 )(name ?machine))
         (object (is-a              Action )(状态机 ?状态机 )(动作 ?动作1)(脚本 $?脚本))
    (not (object (is-a Intermediate#Action )(状态机 ?machine)(名称 ?动作2&:(expr-equal ?动作1 (send ?动作2 get-代码)))))
=>
    (make-instance of Intermediate#Action 
        (状态机 ?machine)
        (名称   (progn (build "(defmethod action  ($?x) (Expression#FSM#Define Action  (expand$ ?x)))")
                       (Intermediate#Encode ?动作1)))
        (脚本   (progn (build "(defmethod action  ($?x) (Expression#FSM#Invoke Action  (expand$ ?x)))")
                       (build "(defmethod trigger ($?x) (Expression#FSM#Invoke Trigger (expand$ ?x)))")
                       (Intermediate#Encode ?脚本)))
    )
    (undefmethod trigger *)
    (undefmethod action  *)
)
(defrule 准备Intermediate#SubMachine数据-for-Machine
    (object (is-a Intermediate#Machine)(名称   ?状态机)(name ?machine))
    (object (is-a           SubMachine)(状态机 ?状态机)(状态 nil)(子状态机 ?子状态机名称)(原型 ?原型)(脚本 $?脚本))
    (object (is-a Intermediate#Machine)(名称   ?原型  )(name ?prototype))
=>
    (make-instance of Intermediate#SubMachine 
        (父     ?machine)
        (名称   ?子状态机名称)
        (原型   ?prototype)
        (脚本   (Intermediate#Encode ?脚本))
    )
)
(defrule 准备Intermediate#SubMachine数据-for-State
    (object (is-a Intermediate#Machine)(名称   ?状态机)(name   ?machine))
    (object (is-a   Intermediate#State)(名称   ?状态  )(状态机 ?machine)(name ?state))
    (object (is-a SubMachine)(状态机 ?状态机)(状态 ?状态)(子状态机 ?子状态机名称)(原型 ?原型)(脚本 $?脚本))
    (object (is-a Intermediate#Machine)(名称   ?原型  )(name ?prototype))
=>
    (make-instance of Intermediate#SubMachine 
        (父     ?state)
        (名称   ?子状态机名称)
        (原型   ?prototype)
        (脚本   (Intermediate#Encode ?脚本))
    )
)
(defrule 准备Intermediate#Variable数据-for-Machine
         (object (is-a Intermediate#Machine )(名称   ?状态机 )(name ?machine))
         (object (is-a              Variable)(状态机 ?状态机 )(状态 nil)(变量 ?名称)(类型 ?类型)(默认 ?默认)(枚举 $?枚举)(注解 ?注解))
    (not (object (is-a Intermediate#Variable)(父     ?machine)(名称 ?名称)))
=>
    (make-instance of Intermediate#Variable 
        (父   ?machine)
        (名称 ?名称)
        (类型 ?类型)
        (默认 ?默认)
        (枚举 ?枚举)
        (注解 ?注解)
    )
)
(defrule 准备Intermediate#Variable数据-for-State
         (object (is-a Intermediate#Machine )(名称   ?状态机)(name   ?machine))
         (object (is-a Intermediate#State   )(名称   ?状态  )(状态机 ?machine)(name ?state))
         (object (is-a              Variable)(状态机 ?状态机)(状态   ?状态)(变量 ?名称)(类型 ?类型)(默认 ?默认)(枚举 $?枚举)(注解 ?注解))
    (not (object (is-a Intermediate#Variable)(父     ?state )(名称   ?名称)))
=>
    (make-instance of Intermediate#Variable 
        (父   ?state)
        (名称 ?名称)
        (类型 ?类型)
        (默认 ?默认)
        (枚举 ?枚举)
        (注解 ?注解)
    )
)

(defrule 准备Intermediate#Callback数据-for-Machine
         (object (is-a Intermediate#Machine )(名称   ?状态机 )(name ?machine))
         (object (is-a              Callback)(状态机 ?状态机 )(状态 nil)(回调 ?回调)(参数 $?参数))
    (not (object (is-a Intermediate#Callback)(父     ?machine)(名称 ?回调)))
=>
    (make-instance of Intermediate#Callback 
        (父   ?machine)
        (名称 ?回调)
        (参数 ?参数)
    )
)
(defrule 准备Intermediate#Callback数据-for-State
         (object (is-a Intermediate#Machine )(名称   ?状态机)(name   ?machine))
         (object (is-a Intermediate#State   )(名称   ?状态  )(状态机 ?machine)(name ?state))
         (object (is-a              Callback)(状态机 ?状态机)(状态   ?状态)(回调 ?回调)(参数 $?参数))
    (not (object (is-a Intermediate#Callback)(父     ?state )(名称   ?回调)))
=>
    (make-instance of Intermediate#Callback 
        (父   ?state)
        (名称 ?回调)
        (参数 ?参数)
    )
)

(defrule 准备Intermediate#State数据
    (object (is-a Intermediate#Machine)(名称 ?状态机)(name ?machine))
    (object (is-a State)
            (状态机    ?状态机)
            (状态      ?状态)
            (进入动作 $?进入动作)
            (离开动作 $?离开动作)
            (类        ?类))
=>
    (build "(defmethod action ($?x) (Expression#FSM#Invoke Action (expand$ ?x)))")
    (make-instance of Intermediate#State 
        (状态机   ?machine)
        (名称     ?状态)
        (类       ?类)
        (进入动作 (Intermediate#Encode ?进入动作))
        (离开动作 (Intermediate#Encode ?离开动作))
    )
    (undefmethod action *)
)
(defrule 准备Intermediate#Transition数据
    (object (is-a Intermediate#Machine)(名称     ?状态机)(name ?machine))
    (object (is-a Transition)(状态机   ?状态机)
                             (状态     ?源状态)
                             (触发器   ?触发器)
                             (目标状态 ?目标状态)
                             (守卫    $?守卫)
                             (动作    $?动作))
    (object (is-a Intermediate#State  )(状态机   ?machine)(名称 ?源状态  )(name ?source))
    (object (is-a Intermediate#State  )(状态机   ?machine)(名称 ?目标状态)(name ?target))
=>
    (make-instance of Intermediate#Transition 
        (触发器   (progn (build "(defmethod trigger ($?x) (Expression#FSM#Detect Trigger (expand$ ?x)))")
                         (Intermediate#Encode ?触发器)))
        (状态机   ?machine)
        (源状态   ?source)
        (目标状态 ?target)
        (守卫     (progn (build "(defmethod guard   ($?x) (Expression#FSM#Invoke Guard   (expand$ ?x)))")
                         (Intermediate#Encode ?守卫)))
        (动作     (progn (build "(defmethod trigger ($?x) (Expression#FSM#Invoke Trigger (expand$ ?x)))")
                         (build "(defmethod action  ($?x) (Expression#FSM#Invoke Action  (expand$ ?x)))")
                         (Intermediate#Encode ?动作)))
    )
    (undefmethod trigger *)
    (undefmethod guard   *)
    (undefmethod action  *)
)
