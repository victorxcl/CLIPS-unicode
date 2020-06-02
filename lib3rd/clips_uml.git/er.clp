;;; vim:expandtab autoindent
(defglobal ?*ER*   = "ER")
(defglobal ?*关系* = ER.关系)
(defglobal ?*继承* = ER.继承)
;(defglobal ?*关系* = t) 
(defclass 原始类型 (is-a     USER)(role abstract)(pattern-match non-reactive))
(defclass 无线连接 (is-a 原始类型)(role abstract)(pattern-match non-reactive))
(deffacts 排除原始类型
    (类型不导出 USER)
    (类型不导出 原始类型)
    (类型不导出 无线连接)
)

(deftemplate 类的继承
    (     slot 类   (type SYMBOL))
    (multislot 子类 (type SYMBOL))
    (multislot 父类 (type SYMBOL))
)
(deftemplate 类的属性
    (slot 类   (type SYMBOL))
    (slot 属性 (type SYMBOL))
)
(deftemplate 类的关系
    (slot 类   (type SYMBOL))
    (slot 关系 (type SYMBOL))
)
(defrule 创建初始事实
=>
    (assert (类的继承 (类 USER)))
)
(defrule 输出开始
    (declare (salience +1000))
=>
    (open (str-cat ?*ER* ".关系.dot") ER.关系 "w")
    (open (str-cat ?*ER* ".继承.dot") ER.继承 "w")
    (progn$ (?t (create$ ?*关系* ?*继承*))
        (printout ?t (indent 0) "// vim:expandtab autoindent                            " crlf)
        (printout ?t (indent 0) "// map \\ :update<CR>"
                                ":!dot -Tpdf -o%:t:r.pdf %<CR>"
                                ":execute printf(\"!open %s.pdf\",expand(\"%:t:r\"))<CR>" crlf)
        (printout ?t (indent 0) "strict digraph {                                       " crlf)
        (printout ?t (indent 0) "    overlap=false;                                     " crlf)
        (printout ?t (indent 0) "  //layers=\"all:客户:界面:服务:产品:资源\";           " crlf)
        (printout ?t (indent 0) "  //layerselect=客户;                                  " crlf)
        (printout ?t (indent 0) "  //ranksep=0.50;                                      " crlf)
        (printout ?t (indent 0) "  //nodesep=0.20;                                      " crlf)
        (printout ?t (indent 0) "  //ordering=out;                                      " crlf)
        (printout ?t (indent 0) "  //pencolor=black; // 用白色绘制cluster的包围盒       " crlf)
        (printout ?t (indent 0) "                                                       " crlf)
        (printout ?t (indent 0) "    node [shape=box]                                   " crlf)
       ;(printout ?t (indent 0) "    {                                                  " crlf)
       ;(printout ?t (indent 0) "        node[color=gray,fontcolor=gray,shape=circle];  " crlf)
       ;(printout ?t (indent 0) "        edge[color=gray,arrowhead=none];               " crlf)
       ;(printout ?t (indent 0) "        1->2->3->4->5->6->7->8->9->10                  " crlf)
       ;(printout ?t (indent 0) "    }                                                  " crlf)
    )
    (printout ?*关系*  (indent 0) "    layout=dot;                                        " crlf)
    (printout ?*关系*  (indent 0) "  //splines=ortho;                                     " crlf)
    (printout ?*关系*  (indent 0) "    rankdir=LR;                                        " crlf)

    (printout ?*继承*  (indent 0) "    layout=dot;                                        " crlf)
    (printout ?*继承*  (indent 0) "  //splines=ortho;                                     " crlf)
)
(defrule 输出结束
    (declare (salience -1000))
=>
    (progn$ (?t (create$ ?*关系* ?*继承*))
        (printout ?t (indent 0) "}" crlf)
    )
    (close ER.关系)
    (close ER.继承)
)
(defrule 识别所有实体继承关系
    ?f <- (类的继承 (类 ?类)(子类 $?子类)(父类 $?父类))
    (test (empty$ ?子类))
    (test (empty$ ?父类))
=>
    (modify ?f (子类 (class-subclasses   ?类))
               (父类 (class-superclasses ?类)))
    (progn$ (?子类 (class-subclasses ?类)) 
            (assert (类的继承 (类 ?子类))))
)
(defrule 识别所有实体的Dot表示
    (类的继承 (类 ?类))
    (not (类型不导出 ?类))
=>
    (progn$ (?v (class-slots ?类)) 
        (if (slot-allowed-classes ?类 ?v) 
            then (assert (类的关系 (类 ?类)(关系 ?v)))
            else (assert (类的属性 (类 ?类)(属性 ?v)))
        )
    )
)
(defrule 构建类【属性】的Dot表示
    (类的属性 (类 ?类) (属性 ?属性))
=>
    (bind ?node  (str-cat "\"" ?类 . ?属性 "\""))
    (progn (bind ?label (str-cat label="\"" ?属性 "\""))
           (bind ?n-style (str-cat ?label ",shape=ellipse,fontsize=10"))
           (bind ?e-style "arrowhead=none")
           (printout ?*关系* (indent 1) ?类 -> { ?node [ ?n-style ] } [ ?e-style ] crlf)
           (printout ?*继承* (indent 1) ?类 -> { ?node [ ?n-style ] } [ ?e-style ] crlf)
    )

    (bind ?slot-allowed-values (slot-allowed-values ?类 ?属性))
    (if (multifieldp ?slot-allowed-values) then
        (bind ?n-style "shape=box,color=gray,fontsize=8,fontcolor=gray")
        (bind ?e-style "arrowhead=none,arrowsize=0.5,style=dotted,color=gray")
        (bind ?children (join$ ?slot-allowed-values "\\n"))
        (progn$ (?t (create$ ?*关系* ?*继承*))
                (printout ?t (indent 1) ?node -> { crlf)
                (printout ?t (indent 2) "\""?children"\"" [ ?n-style ] crlf)
                (printout ?t (indent 1) } [ ?e-style ] crlf)
        )
    )
)
(defrule 构建类【关系】的Dot表示
    (类的关系 (类 ?类) (关系 ?关系))
=>
    (bind ?node     (str-cat "\"" ?类 . ?关系 "\""))
    (progn (bind ?label (str-cat label="\"" ?关系 "\""))
           (bind ?n-style (str-cat ?label ",shape=diamond,fontsize=10"))
           (bind ?e-style "arrowhead=none")
           (printout ?*关系* (indent 1) ?类 -> { ?node [ ?n-style ] } [ ?e-style ] crlf)
           (printout ?*继承* (indent 1) ?类 -> { ?node [ ?n-style ] } [ ?e-style ] crlf)
    )
    
    (bind ?slot-allowed-classes (slot-allowed-classes ?类 ?关系))
    (if (multifieldp ?slot-allowed-classes) then
        (progn$ (?v ?slot-allowed-classes)
                (bind ?label    (str-cat "\"" ?v "\""))
                (bind ?relation (str-cat "\"" ?类 . ?关系 -> ?v "\""))
                (bind ?n-style  (str-cat "color=gray,style=dashed,fontcolor=gray,label="?label""))
                (bind ?e-style  (str-cat "color=gray,arrowhead=vee,arrowsize=0.5"))
                (progn (if (and (not (superclassp 原始类型 ?v))
                                (not (member$ 无线连接 ?slot-allowed-classes)))
                           then (printout ?*关系* (indent 1) ?node -> { ?v } [ ?e-style ] crlf)
                           else (if (neq 无线连接 ?v) then 
                                    (printout ?*关系* (indent 1) ?node -> { ?relation [ ?n-style ] } [ ?e-style ] crlf))
                       )
                )
                (progn (printout ?*继承* (indent 1) ?node -> { ?relation [ ?n-style ] } [ ?e-style ] crlf))
        )
    )
)
(defrule 构建【类的子类】的Dot表示
    (类的继承 (类 ?类)(子类 $?子类))
    (not (类型不导出 ?类))
    (test (not (empty$ ?子类)))
=>
    (progn$ (?sub ?子类)
            (bind ?e-style "arrowhead=empty,arrowsize=1,color=gray,constraint=true")
            (bind ?subclass   (str-cat "\"" ?sub "\""))
            (bind ?superclass (str-cat "\"" ?类  "\""))
            (printout ?*继承* (indent 1) { ?subclass [ label="\""?sub"\"" ] } -> { ?superclass [ label="\""?类 "\"" ] } [ ?e-style ] crlf)
           ;(printout ?*关系* (indent 1) { ?subclass [ label="\""?sub"\"" ] } -> { ?superclass [ label="\""?类 "\"" ] } [ ?e-style ,style=dashed] crlf)
            (printout ?*关系* (indent 1) { ?subclass } -> { ?superclass [ label="\""?类 "\"" ] } [ ?e-style ,style=dashed] crlf)
    )
)
(defrule 构建【类的父类】的Dot表示
    (类的继承 (类 ?类)(父类 $?父类))
    (not (类型不导出 ?类))
    (test (not (empty$ ?父类)))
=>
    (progn (bind ?super (join$ ?父类 ","))
           (bind ?font  (str-cat " " FONTSIZE= "\"6\""))
           (bind ?label (str-cat <TABLE" CELLPADDING=\"2\" CELLSPACING=\"2\" BORDER=\"0\""> 
                                   <TR><TD>           ?类    </TD></TR>
                                   <TR><TD><font" "color="\"gray\""> ?super </font></TD></TR>
                                 </TABLE>))
           (printout ?*关系* (indent 1) ?类 [ label = < ?label > , margin=0] crlf)
    )
)


