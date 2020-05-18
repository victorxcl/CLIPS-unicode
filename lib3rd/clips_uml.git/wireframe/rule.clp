;;; vim: expandtab autoindent
;;; map <buffer> \\ :update<CR>:1tabnext<CR>:2wincmd w<CR>:silent normal \\<CR>:2tabnext<CR>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component
(defrule 预处理Component的owner数据
    (object (is-a Container)(name ?container)(children $? ?component $?))
    (object (is-a Component)(name ?component))
=>
    (send ?component put-owner ?container)
)
(defrule 预处理Component的wireframe数据
    (not (object (is-a Component&~WireFrame)(owner ?x&:(eq [nil] ?x))))
    (object (is-a Component&~WireFrame)(name ?component)(owner ?owner));;; WireFrame不需要预处理
=>
    (while (and (neq [nil]            ?owner ) 
                (neq WireFrame (class ?owner))) do
        (bind ?owner (send ?owner get-owner))
    )
    (send ?component put-wireframe ?owner)
)
(defrule 预处理Component的dock数据
    (not (object (is-a Component&~WireFrame)(owner ?x&:(eq [nil] ?x))))
    (object (is-a Component)(name ?component)(wireframe ?wireframe))
    (object (is-a WireFrame)(name ?wireframe)(children $?children))
=>
    (bind ?dock ?component)
    (while (neq [nil] ?dock) do
        (if (member$ ?dock ?children) then (send ?component put-dock ?dock) (break))
        (bind ?dock (send ?dock get-owner))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WireFrame
(defrule 预处理WireFrame的local、main和hook数据
    (object (is-a WireFrame)(name ?wireframe)(children $?context ?main))
=>
    (progn$ (?v ?context)
        (if (superclassp Component (class ?v)) then (slot-append$ ?wireframe local ?v))
        (if (superclassp      Hook (class ?v)) then (slot-append$ ?wireframe hook  ?v))
    )
    (send ?wireframe put-main ?main)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Embed
(defrule 预处理Embed的default数据
    (object (is-a  WireFrame)(name      ?wireframe))
    (object (is-a      Embed)(wireframe ?wireframe)(name       ?embed)(default nil))
    (object (is-a @Reference)(name      ?reference)(annotation ?embed)(reference ?target))
=>
    (modify-instance ?embed (default ?target))
)

(defrule 预处理Embed的@ToMany数据：make-instance
         (object (is-a  WireFrame)(name      ?wireframe)(local     $?local))
         (object (is-a      Embed)(wireframe ?wireframe)(name       ?embed)(default ?target&:(neq nil ?target)))
    (not (object (is-a    @ToMany)                      (annotation ?embed)))
=>
    (make-instance of @ToMany
        (annotation ?embed)
        (one        ?embed)
        (default    (send ?wireframe local-or-wireframe-with-name ?target))
    )
)
(defrule 预处理Embed的@ToMany数据：many
    (declare (salience 100)) ;;; 确保slot-append$操作完成之后在进行错误报告
    (object (is-a  WireFrame)(name      ?wireframe)(local     $?local))
    (object (is-a      Embed)(wireframe ?wireframe)(name       ?embed))
    (object (is-a @ToMany   )(name      ?toMany   )(annotation ?embed))
    (object (is-a @Reference)(name      ?reference)(annotation ?embed)(reference ?target))
=>
    (slot-append$ ?toMany many (send ?wireframe local-or-wireframe-with-name ?target))
)
(defrule 预处理Embed的@Reference数据
    (object (is-a  WireFrame)(name      ?wireframe)(local     $?local))
    (object (is-a      Embed)(wireframe ?wireframe)(name       ?embed))
    (object (is-a @Reference)(name      ?reference)(annotation ?embed)(reference ?target))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 确保local-or-wireframe-with-name有值 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;(or           (object (is-a Component&~WireFrame)(wireframe ?wireframe)(named ?target)(name ?instance&:(member$ ?instance ?local)))
   ;    (and (not (object (is-a Component&~WireFrame)(wireframe ?wireframe)(named ?target)(name ?x       &:(member$ ?x        ?local))))
   ;         (object (is-a WireFrame)(named ?taret)(name ?instance))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 确保local-or-wireframe-with-name有值 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
=>
    (message-modify-instance ?reference 
        (source ?embed)
        (target (send ?wireframe local-or-wireframe-with-name ?target))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component
(defrule 预处理Component的@Link的@ToMany数据：make-instance
         (object (is-a  WireFrame)(name      ?wireframe)(local     $?local))
         (object (is-a  Component)(wireframe ?wireframe)(name       ?component))
         (object (is-a    @Link  )(name      ?link     )(annotation ?component)(to ?target)(by ?by))
    (not (object (is-a    @ToMany)                      (annotation ?component)))
=>
    (make-instance of @ToMany
        (annotation ?component)
        (one        ?component)
        (default    (send ?wireframe local-or-wireframe-with-name ?target))
    )
)
(defrule 预处理Component的@Link的@ToMany数据：many
    (declare (salience 100)) ;;; 确保slot-append$操作完成之后在进行错误报告
    (object (is-a  WireFrame)(name      ?wireframe)(local     $?local    ))
    (object (is-a  Component)(wireframe ?wireframe)(name       ?component))
    (object (is-a    @ToMany)(name      ?toMany   )(annotation ?component))
    (object (is-a    @Link  )(name      ?reference)(annotation ?component)(to ?target))
=>
    (slot-append$ ?toMany many (send ?wireframe local-or-wireframe-with-name ?target))
)
(defrule 预处理Component的@Link数据
    (object (is-a  WireFrame)(name      ?wireframe)(local     $?local))
    (object (is-a  Component)(wireframe ?wireframe)(name       ?component))
    (object (is-a @Link     )(name      ?link     )(annotation ?component)(to ?target)(by ?by))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 确保local-or-wireframe-with-name有值 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;(or           (object (is-a Component&~WireFrame)(wireframe ?wireframe)(named ?target)(name ?instance&:(member$ ?instance ?local)))
   ;    (and (not (object (is-a Component&~WireFrame)(wireframe ?wireframe)(named ?target)(name ?x       &:(member$ ?x        ?local))))
   ;         (object (is-a WireFrame)(named ?taret)(name ?instance))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 确保local-or-wireframe-with-name有值 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
=>
    (message-modify-instance ?link 
        (source ?component)
        (target (send ?wireframe local-or-wireframe-with-name ?target))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text
(defrule 预处理Text的text数据
    (object (is-a  Text)(name       ?text)(text nil))
    (object (is-a @Eval)(annotation ?text)(script $?script))
=>
    (send ?text put-text (join$ ?script))
)
;;; Button
(defrule 预处理Button的title数据
    (object (is-a Button)(name       ?button)(title nil))
    (object (is-a  @Eval)(annotation ?button)(script $?script))
=>
    (send ?button put-title (join$ ?script))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Form
(defrule 预处理Form的left和right数据
    (object (is-a      Form)(wireframe ?wireframe)(name ?form)(children $?children&:(evenp (length$ ?children))))
    (object (is-a WireFrame)(name      ?wireframe))
=>
    (progn$ (?v ?children)
        (if ( oddp ?v-index) then (slot-append$ ?form left  ?v))
        (if (evenp ?v-index) then (slot-append$ ?form right ?v))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 错误报告规则
(defrule  错误报告：两个WireFrame不可以有相同的名字
    (object (is-a WireFrame)(name     ?A)(named ?P))
    (object (is-a WireFrame)(name ?B&~?A)(named ?P))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现了两个相同named的WireFrame: tab (I ?P))
    (println (repeat = 80))
    (send ?A print)
    (println (repeat - 80))
    (send ?B print)
    (println (repeat ^ 80))
)
(defrule 错误报告：WireFrame内部的子组件不能有相同的名字
    (object (is-a WireFrame           )(name      ?wireframe))
    (object (is-a Component&~WireFrame)(wireframe ?wireframe)(name ?A    )(named ?named))
    (object (is-a Component&~WireFrame)(wireframe ?wireframe)(name ?B&~?A)(named ?named))
=>
    (println [ERROR(L)WireFrame(R)]: tab 在WireFrame中发现了两个相同named的local组件 tab (I ?A) (I ?B))
    (println (repeat = 80))
    (send ?A print)
    (println (repeat - 80))
    (send ?B print)
    (println (repeat ^ 80))
)

(defrule  错误报告：List如果repeat属性值不为0，那么children的数量只能有一个
    (object (is-a List)(repeat ?repeat&:(> ?repeat 0))(children $?children))
    (test (not (= 1 (length$ ?children))))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现repeat不为0的List，其children的数量不止一个: tab (I ?children))
)
(defrule 错误报告：Embed的default目标必须存在
    (object (is-a WireFrame)(name      ?wireframe))
    (object (is-a     Embed)(wireframe ?wireframe)(name ?embed)(default ?default&:(neq nil ?default)))
    (test (not (send ?wireframe local-or-wireframe-with-name ?default)))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现Embed的default目标不存在: tab (I ?default))
    (println (repeat = 80))
    (send ?embed print)
    (println (repeat ^ 80))
)
;(defrule 错误报告：Embed的@ToMany中many所有成员都必须是@Referene
;    (object (is-a   Embed)(name       ?embed))
;    (object (is-a @ToMany)(annotation ?embed)(many $? ?one $?))
;    (test (not (eq @Reference (class ?one))))
;=>
;    (println [ERROR(L)WireFrame(R)]: tab 发现Embed的@ToMany注解的many的某个成员不是@Reference类型: tab (I ?one))
;    (println (repeat = 80))
;    (send ?embed print)
;    (println (repeat - 80))
;    (send ?one print)
;    (println (repeat ^ 80))
;)
;(defrule 错误报告：只有Embed才能添加@Reference注解 =>)
(defrule 错误报告：@ToMany的default必须是many的成员
    (object (is-a @ToMany)(many $?many)(default ?default)(name ?toMany)(annotation ?annotation))
    (object (is-a Component)(name ?annotation)(wireframe ?wireframe))
    (object (is-a WireFrame)(name ?wireframe))
    (test (not (member$ ?default ?many)))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现@ToMany的default不是many的成员: tab (I ?default))
    (println (repeat = 80))
    (send ?toMany print)
    (println (repeat - 80))
    (send ?wireframe print)
    (println (repeat - 80))
    (send ?annotation print)
    (println (repeat - 80))
    (send ?default print)
    (println (repeat - 80))
    (progn$ (?v ?many) (send ?v print))
    (println (repeat ^ 80))
)
;(defrule 错误报告：同一个WireFrame内部，不能存在相同名称的Hook =>)
(defrule 错误报告：WireFrame的children的最后一个必须是Component组件
   (object (is-a WireFrame)(children $? ?main)(name ?wireframe))
   (test (not (superclassp Component (class ?main))))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现WireFrame的最后一个children不是Component:)
    (println (repeat = 80))
    (send ?wireframe print)
    (println (repeat - 80))
    (send ?main print)
    (println (repeat ^ 80))
)
;(defrule 错误报告：useContext的context字段必须有值 =>)
;(defrule 错误报告：useContext的context必须有匹配的Context存在 =>)
;(defrule 错误报告：useReducer的reducer字段必须有值 =>)
;(defrule 错误报告：useReducer的reducer必须有匹配的Reducer存在 =>)
(defrule 错误报告：Form的children数量必须为双数
    (object (is-a Form)(children $?children)(name ?form))
    (test (not (evenp (length$ ?children))))
=>
    (println [ERROR(L)WireFrame(R)]: tab Form的children数量不为双数: tab (length$ ?children))
    (println (repeat = 80))
    (send ?form print)
    (println (repeat ^ 80))
)
;(defrule 错误报告：只有Button才能添加@Link注解 =>)
;(defrule 错误报告：@Link的目标必须存在 =>)
(defrule 错误报告：Grid容器下的直接Component才允许有@Grid注解
         (object (is-a @Grid)(annotation    ?component   ))
    (not (object (is-a  Grid)(children   $? ?component $?)))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现Grid的【非】直接子组件使用了@Grid注解: tab (I ?component))
    (println (repeat = 80))
    (send ?component print)
    (println (repeat ^ 80))
)
(defrule 错误报告：Grid容器下的直接子组件必须有@Grid注解
         (object (is-a Grid     )(name ?grid )(children $?A ?child $?B))
         (test (> (length$ (create$ ?A ?child ?B)) 1))
         (object (is-a Component)(name ?child))
    (not (object (is-a @Grid)(annotation ?child)))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现Grid容器的直接子组件没有@Grid注解: tab (I ?child))
    (println (repeat = 80))
    (send ?grid print)
    (println (repeat - 80))
    (send ?child print)
    (println (repeat ^ 80))
)
;(defrule 错误报告：Grid容器下的@Grid的行列索引值不能交叠 =>)
(defrule 错误报告：检测允许使用@OnClick的组件
    (object (is-a @OnClick)(annotation ?component))
    (test (not (eq (class ?component) Button)))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现@OnClick注解用在了错误的组件上: tab (I ?component))
    (println (repeat = 80))
    (send ?component print)
    (println (repeat ^ 80))
)

(defrule 错误报告：检测@Script的language属性的合法性 
    (object (is-a @Script)(language ?language)(name ?component))
    (test (not (member$ ?language (create$ CLIPS JavaScript C++ C Lua))))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现@Script的language不合法: tab (I ?component))
    (println (repeat = 80))
    (send ?component print)
    (println (repeat ^ 80))
)
(defrule 错误报告：@Script的script字段必须有值
    (object (is-a @Script)(script $?script)(name ?component))
    (test (eq @Script (class ?component)))
    (test (not (> (length$ ?script) 0)))
=>
    (println [ERROR(L)WireFrame(R)]: tab 发现@Script的script字段没有值: tab (I ?component))
    (println (repeat = 80))
    (send ?component print)
    (println (repeat ^ 80))
)
