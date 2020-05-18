;;; vim:expandtab autoindent
;;; map <buffer> \\ :update<CR>:!clips -f fsm.clp<CR>:!dot -Tpdf -O 状态机.FSM.dot<CR>:!open 状态机.FSM.dot.pdf<CR>
;;; map <buffer> \\ :update<CR>:1tabnext<CR>:2wincmd w<CR>:silent normal \\<CR>:3tabnext<CR>

;;; for check-syntax only
(defgeneric trigger)
(defgeneric guard)
(defgeneric action)
;;; for check-syntax only

;;; 错误报告，违例检查
(defrule 错误报告-重复的Action
    (object (is-a Action)(状态机 ?状态机)(动作 ?动作)(name  ?name))
    (object (is-a Action)(状态机 ?状态机)(动作 ?动作)(name ~?name))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现重复的[Action]:(S)
        (L)状态机(S)?状态机(R)
        (L)动作  (S)?动作  (R))
)
(defrule 错误报告-重复的Guard
    (object (is-a Guard)(状态机 ?状态机)(守卫 ?守卫)(name  ?name))
    (object (is-a Guard)(状态机 ?状态机)(守卫 ?守卫)(name ~?name))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现重复的[Guard]:(S)
        (L)状态机(S)?状态机(R)
        (L)守卫  (S)?守卫  (R))
)
(defrule 错误报告-重复的Trigger
    (object (is-a Trigger)(状态机 ?状态机)(触发器 ?触发器)(name  ?name))
    (object (is-a Trigger)(状态机 ?状态机)(触发器 ?触发器)(name ~?name))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现重复的[Trigger]:(S)
        (L)状态机(S)?状态机(R)
        (L)触发器(S)?触发器(R))
)
(defrule 错误报告-未定义Trigger
         (object (is-a Transition)(状态机 ?状态机)(触发器 ?触发器))
    (not (object (is-a Trigger)   (状态机 ?状态机)(触发器 ?X&:(expr-equal ?触发器 ?X))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现未定义[Trigger]:(S)
        (L)状态机(S)?状态机(R)
        (L)触发器(S)?触发器(R))
)
(defrule 错误报告-未定义Guard
         (object (is-a Transition)(状态机 ?状态机)(守卫 $? ?守卫 $?))
    (not (object (is-a Guard)     (状态机 ?状态机)(守卫    ?X&:(expr-equal ?守卫 ?X))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现未定义[Guard]:(S)
        (L)状态机(S)?状态机(R)
        (L)守卫  (S)?守卫  (R))
)
(defrule 错误报告-未定义Action
    (or  (object (is-a Machine)   (状态机 ?状态机)(初始动作 $? ?动作 $?))
         (object (is-a State)     (状态机 ?状态机)(进入动作 $? ?动作 $?))
         (object (is-a State)     (状态机 ?状态机)(离开动作 $? ?动作 $?))
         (object (is-a Transition)(状态机 ?状态机)(    动作 $? ?动作 $?)))
    (not (object (is-a Action)    (状态机 ?状态机)(    动作    ?X&:(expr-equal ?动作 ?X))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现未定义[Action]:(S)
        (L)状态机(S)?状态机(R) 
        (L)动作  (S)?动作  (R))
)
(defrule 错误报告-未定义Machine的SubMachine
         (object (is-a SubMachine)(状态机 ?状态机)(状态 nil)(子状态机 ?子状态机)(原型 ?原型))
    (not (object (is-a    Machine)(状态机 ?原型)))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现未定义Machine的[SubMachine]:(S)
        (L)状态机  (S)?状态机  (R)
        (L)原型    (S)?原型    (R)
        (L)子状态机(S)?子状态机(R))
)
(defrule 错误报告-未定义State的SubMachine
         (object (is-a SubMachine)(状态机 ?状态机)(状态 ?状态)(子状态机 ?子状态机)(原型 ?原型))
    (not (object (is-a    Machine)(状态机 ?原型)))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现未定义State的[SubMachine]:(S)
        (L)状态机  (S)?状态机  (R)
        (L)状态    (S)?状态    (R)
        (L)原型    (S)?原型    (R)
        (L)子状态机(S)?子状态机(R))
)
(defrule 错误报告-未定义SubMachine的父级Machine
         (object (is-a SubMachine)(状态机 ?状态机)(状态 nil)(子状态机 ?子状态机)(原型 ?原型))
    (not (object (is-a    Machine)(状态机 ?状态机)))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现未定义SubMachine的[父状态机]:(S)
        (L)状态机  (S)?状态机  (R)
        (L)原型    (S)?原型    (R)
        (L)子状态机(S)?子状态机(R)
    )
)
(defrule 错误报告-未定义SubMachine的父级Machine或State
         (object (is-a SubMachine)(状态机 ?状态机)(状态 ?状态&~nil)(子状态机 ?子状态机)(原型 ?原型))
    (not (object (is-a      State)(状态机 ?状态机)(状态 ?状态     )))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现未定义SubMachine的[父状态]:(S) 
        (L)状态机  (S)?状态机  (R)
        (L)状态    (S)?状态    (R)
        (L)原型    (S)?原型    (R)
        (L)子状态机(S)?子状态机(R)
    )
)
(defrule 错误报告-不规范的Trigger
    (object (is-a Trigger)(状态机 ?状态机)(触发器 ?触发器))
    (test (neq FALSE (check-syntax (str-cat ?触发器))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现不规范的[Trigger]: 
        (L)状态机(S)?状态机(R)
        (L)触发器(S)?触发器(R))
    (println (check-syntax (str-cat ?触发器)))
)
(defrule 错误报告-不规范的Guard
    (or (object (is-a Guard     )(状态机 ?状态机)(守卫    ?守卫))
        (object (is-a Transition)(状态机 ?状态机)(守卫 $? ?守卫 $?)))
    (test (neq FALSE (check-syntax (str-cat ?守卫))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现不规范的[Guard]: 
        (L)状态机(S)?状态机(R)
        (L)守卫  (S)?守卫  (R))
    (println (check-syntax (str-cat ?守卫)))
)
(defrule 错误报告-不规范的Action
    (or (object (is-a Action    )(状态机 ?状态机)(    动作    ?动作   ))
        (object (is-a Machine   )(状态机 ?状态机)(初始动作 $? ?动作 $?))
        (object (is-a State     )(状态机 ?状态机)(进入动作 $? ?动作 $?))
        (object (is-a State     )(状态机 ?状态机)(离开动作 $? ?动作 $?))
        (object (is-a Transition)(状态机 ?状态机)(    动作 $? ?动作 $?)))
    (test (neq FALSE (check-syntax (str-cat ?动作))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现不规范的[Action]: 
        (L)状态机(S)?状态机(R)
        (L)动作  (S)?动作  (R))
    (println (check-syntax (str-cat ?动作)))
)
(defrule 错误报告-不规范的Script
    (or (object (is-a Trigger)   (状态机 ?状态机)(前置脚本 $? ?脚本 $?))
        (object (is-a Trigger)   (状态机 ?状态机)(后置脚本 $? ?脚本 $?))
        (object (is-a Guard)     (状态机 ?状态机)(    脚本 $? ?脚本 $?))
        (object (is-a Action)    (状态机 ?状态机)(    脚本 $? ?脚本 $?))
        (object (is-a SubMachine)(状态机 ?状态机)(    脚本 $? ?脚本 $?)))
    (test (neq FALSE (check-syntax (str-cat ?脚本))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现不规范的[Script]:
        (L)状态机(S)?状态机(R)
        (L)脚本  (S)?脚本  (R))
    (println (check-syntax (str-cat ?脚本)))
)
(defrule 错误报告-不规范的Default
    (object (is-a Variable)(状态机 ?状态机)(默认 ?默认))
    (test (and (stringp ?默认) (neq FALSE (check-syntax (str-cat ?默认)))))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现不规范的[Default]: 
        (L)状态机(S)?状态机(R)
        (L)默认  (S)?默认  (R))
    (println (check-syntax (str-cat ?默认)))
)

(defrule 错误报告-状态机的【初始状态】必须存在
         (object (is-a Machine)(状态机 ?状态机)(初始状态 ?初始状态))
    (not (object (is-a State)  (状态机 ?状态机)(状态     ?初始状态)))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现状态机的【初始状态】不存在:(S)
        (L)状态机  (S)?状态机  (R)
        (L)初始状态(S)?初始状态(R))
)
(defrule 错误报告-不规范的【状态机】名称
    (object (is-a Machine)(状态机 ?状态机))
    (test (numberp (string-to-field ?状态机)))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现[Machine]的不规范名称:(S)
        (L)状态机(S)?状态机(R))
)
(defrule 错误报告-不规范的【状态】名称
    (object (is-a State)(状态机 ?状态机)(状态 ?状态))
    (test (numberp (string-to-field ?状态)))
=>
    (println [ERROR(L)(get-current-module)(R)] 发现[State]的不规范名称:(S)
        (L)状态机(S)?状态机(R)
        (L)状态  (S)?状态  (R))
)
