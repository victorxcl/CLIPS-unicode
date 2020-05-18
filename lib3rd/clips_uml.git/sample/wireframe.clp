;;; vim: expandtab autoindent
;;; map <buffer> \\ :update<CR>:!clips -f %<CR>
;;; map <buffer> \\ :update<CR>:!clips -f %<CR>:!dot -Tpdf -O Test.wireframe.dot<CR>:!open Test.wireframe.dot.pdf<CR>
;;; map <buffer> \\ :update<CR>:1tabnext<CR>:2wincmd w<CR>:normal \\<CR>:3tabnext<CR>
 
(deffunction clips_uml ($?path) (str-cat ../ (expand$ ?path)))
(deffunction clips_cwd ($?path) (str-cat  ./ (expand$ ?path)))

(dribble-on (clips_cwd wireframe.clp.log))

(set-dynamic-constraint-checking TRUE)

(defmodule MAIN (export ?ALL))
(load (clips_uml utility.clp))

(defmodule WireFrame (import MAIN ?ALL)(export ?ALL))
(batch* (clips_uml wireframe.clp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(definstances WireFrame:GraphViz::基础表格 ([基础表格] of Multifield (multifield
;    (TABLE (TR (TD (TEXT Hello)) (TD (TEXT World)))
;           (TR (TD (TEXT Good )) (TD (TEXT Luck )))
;           (TR (TD (TEXT Happy)) (TD (TEXT Great))))
;
;    (TABLE (TR (TD (TEXT Hello) ROWSPAN: 3) (TD (TEXT World) COLSPAN: 2))
;           (TR (TD (TEXT Good )           ) (TD (TEXT Luck )           ))
;           (TR (TD (TEXT Happy)           ) (TD (TEXT Great)           )))
;)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(definstances WireFrame::扩展表格 
    (of WireFrame (children
        (Layout (Header  (Text header ))
                (Content (Text content))
                (Footer  (Text footer )) named: 上中下布局)
        (Layout (Header  (Text header ))
                (Content (Text content)))
        (Layout (Content (Text content))
                (Footer  (Text footer )))
    ))

    (of WireFrame (children
        (Layout (Sider   (Text left   ))
                (Content (Text content))
                (Sider   (Text right  )) named: 左中右布局)
        (Layout (Sider   (Text left   ))
                (Content (Text content)))
        (Layout (Content (Text content))
                (Sider   (Text right  )))
    ))

    (of WireFrame (children
        (Layout (Sider  (Text left   ))
                (Layout (Header  (Text header ))
                        (Content (Text content))
                        (Footer  (Text footer ))))

        (Layout (Sider  (Text left   ))
                (Layout (Header  (Text header ))
                        (Content (Text content))
                        (Footer  (Text footer )))
                (Sider  (Text right  )))

        (Layout (Header (Text header ))
                (Layout (Sider   (Text left   ))
                        (Content (Text content))
                        (Sider   (Text right  )))
                (Footer (Text footer )))
    ))

    (of WireFrame (named 买菜)(children
        (List (Text 今天买到菜了吗)
              (Text 今天买到菜了吗)
              (Text 今天买到菜了吗)
              (Text 今天买到菜了吗)
              (Text 今天买到菜了吗) named: 默认列表布局)
    ))

    (of WireFrame (children
        (Layout (Header (Text header ))
                (Layout (Sider   (Text left   ))
                        (Content (List (Text 搞神马灰机)
                                       (Text 搞神马灰机)
                                       (Text 搞神马灰机)
                                       (Text 搞神马灰机)
                                       (Text 搞神马灰机)))
                        (Sider   (Text right  )))
                (Footer (Text footer )))
    ))

    (of WireFrame (named 搞神马灰机)(children
        (Layout (Header (Text header ))
                (Layout (Sider   (Text left   ))
                        (Content (List (Text 搞神马灰机)
                                       (Text 搞神马灰机)
                                       (Text 搞神马灰机))
                                 (List (Text 搞神马灰机)
                                       (Text 搞神马灰机)))
                        (Sider   (Text right)
                                 (Embed (@Ref 买菜))))
                (Footer (Text footer )))
    ))


    (of WireFrame (named 分类)(children
        (Radio 神 named: 神)
        (Radio 魔 named: 魔)
        (Radio 花 named: 花)
        (List (Embed (@Ref 娱乐资讯))
              (Embed (@Ref 电视剧综艺))
              (Button 脱口秀) direction: horizontal named: 头部)
        (List (Button 娱乐  )(Button 资讯) named: 娱乐资讯  )
        (List (Button 电视剧)(Button 综艺) named: 电视剧综艺)
        (List (Embed (@Ref 花))
              (Embed (@Ref 神)) named: 花神)
        (Layout (Header (Embed (@Ref 头部)))
                (Layout (Sider   (Text left   ))
                        (Content (List (Radio 人)
                                       (Embed (@Ref 神 script: "(hello)"))
                                       (Embed (@Ref 魔 script: "(world)"))
                                       (Embed (@Ref 花神))
                                       (Radio 兽))
                                 (List (Embed (@Ref 搞神马灰机 script: "(hello world)" "(good luck)"))))
                        (Sider   (Text right  )))
                (Footer (Embed (@Ref 分类.底部))))
    ))
    (of WireFrame (named 分类.底部)(children
       ;(Layout (Header (Label 关于我们))
       ;        (Content (List (Button 公司介绍)
       ;                       (Button 联系方式))) named: 关于我们)
        (List (Button 公司介绍)
              (Button 联系方式) named: 关于我们)
        (List (Text  买到菜了吗)
              (Embed (@Ref 关于我们))
              (Text  买到菜了吗)
              (Text  买到菜了吗) direction: horizontal)
    ))
    (of WireFrame (children
        (Form (Text     昵称)(TextField 昵称)
              (Text     姓名)(TextField 姓名)
              (Text     性别)(HList (Radio 男)(Radio 女))
              (Text 个人爱好)(HList (Check 篮球)(Check 吉他)(Check 电竞)))
    ))
    (of WireFrame (children
        (Breadcrumb (Text 上海市)(Text 浦东新区)(Text 张江镇)(Text 藿香路))
    ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(definstances WireFrame::WeChat
    (of WireFrame (children
        (useState setter: setCount getter: count default: 0)
        (useState currentPage default: 微信)
        (useEffect effect: subscribe clear: unsubscribe)
        (useEffect subscribe clear: unsubscribe)
        (useEffect hello world clear: unsubscribe trace log)
        (useContext 主题)
        (useReducer 计数器 state: 0)
        (HList (Label  微信) 
               (Button + (@Link 朋友圈权限)) named: header)
        (HList (Button 微信   )
               (Button 通讯录 )
               (Button 发现   )
               (Button 我     ) named: footer)
        (Layout (Header  (Embed (@Ref header)))
                (Content (Embed (@Ref 微信   script: "1") 
                                (@Ref 通讯录 script: "2")
                                (@Ref 发现   script: "3")
                                (@Ref 我     script: "4")))
                (Footer  (Embed (@Ref footer))))
    ))
    (of WireFrame (named 微信)(children
        (List (HList (Image rows: 2 cols: 2) 
                     (VList (Text 解明亮)
                            (Text 喜欢吃西贝))) named: item)
        (List (Embed (@Ref item)) repeat: 5)
    ))
    (of WireFrame (named 通讯录)(children
        (HList (Image)
               (Text 解明亮)
               (Text 电话) named: item)
        (List (Embed (@Ref item)) repeat: 5)
    ))
    (of WireFrame (named 发现)(children
        (HList (Image)
               (Text 朋友圈) named: item)
        (List (Embed (@Ref item)) repeat: 5)
    ))
    (of WireFrame (named 我)(children
        (HList (Image)
               (Text 设置) named: item)
        (List (Embed (@Ref item)) repeat: 5)
    ))
    (of WireFrame (named 朋友圈权限)(children
        (List (Button OK)
              (Button Cancel)
              (Switch 飞行模式 status: ON)
              (Switch 飞行模式 status: OFF)
              (Check 我看对方朋友圈  checked: TRUE )
              (Check 对方看我朋友圈  checked: FALSE)
              (Radio 男 group: 性别 selected: TRUE )
              (Radio 女 group: 性别 selected: FALSE))

        (Grid (Button hello (@Grid 1 1)) (Button hello (@Grid 1 2 span: 5)) (Button hello (@Grid 1 7))
              (Button hello (@Grid 2 1)) (Button hello (@Grid 2 2 span: 5)) (Button hello (@Grid 2 7))
              (Button hello (@Grid 3 1)) (Button hello (@Grid 3 2 span: 5)) (Button hello (@Grid 3 7))
              (Button hello (@Grid 4 1)) (Button hello (@Grid 4 2 span: 5)) (Button hello (@Grid 4 7))
              rows: 5 cols: 8 gutter: 10)
        (Grid (Button hello) rows: 5 cols: 8 gutter: 10)

        (List (List (Button OK)
                    (Button Cancel) direction: horizontal)
              (List (Check 我看对方朋友圈  checked: TRUE )
                    (Check 对方看我朋友圈  checked: FALSE)
                    direction: vertical)
              (List (Radio 男 group: 性别 selected: TRUE )
                    (Radio 女 group: 性别 selected: FALSE)
                    direction: horizontal))
    ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(definstances WireFrame::Counter
    (of WireFrame (named Counter)(children
        (useState count default: 0)
        (Grid (Text Clicked (@Grid 1 1))
              (Text (@Eval "(variable count)") (@Grid 1 2))

              (Button + (@OnClick "(invoke setCount (+ (variable count) 1))")(@Grid 2 1))
              (Button - (@OnClick "(invoke setCount (- (variable count) 1))")(@Grid 2 2))

              rows: 2 cols: 2 gutter: 10)
    ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(definstances WireFrame::TodoApp
    (of WireFrame (named TodoApp)(children
        (useProperty hello default: world)
        (useScript (@Script javascript:
            (str-cat (indent 0) "import { todoApp } from 'redux'")
        ) global: TRUE)
        (useReducer todoApp)

        (HList (Button 干点啥吧 (@OnClick "(invoke dispatch (invoke toggleTodo (keypath todo id)))"))
            named: Todo properties: "(parameter todo)")

        (Grid (Label     (@Grid 1 1 span: 4) Todos)

              (TextField (@Grid 2 1 span: 3) hello)
              (Button    (@Grid 2 4 span: 1) +)

              (VList     (@Grid 3 1 span: 4)
                         (Embed (@Ref Todo) parameters: "(variable hello)") repeat: 5)

              (HList     (@Grid 4 1 span: 4)
                         (Radio 全部    )
                         (Radio 激活的  )
                         (Radio 未激活的))
              rows: 4 cols: 4 gutter: 10)
    ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule WireFrame:GraphViz::test
=>
    (bind ?lambda (lambda hello => world))
    (send ?lambda print)
    (bind ?lambda (lambda hello world => good luck))
    (send ?lambda print)
    (bind ?lambda (lambda => good luck))
    (send ?lambda print)
    (bind ?lambda (lambda => good luck))
    (send ?lambda print)
    (bind ?lambda (lambda =>))
    (send ?lambda print)
)
;(watch rules)
;(watch slots)
;(watch instances)
;(watch messages)
;(watch methods)
;(watch methods WireFrame:GraphViz::#GraphVizParse)
(watch focus)
(reset)
(focus WireFrame WireFrame:GraphViz WireFrame:ReactJS)
;(focus WireFrame WireFrame:GraphViz)
(run)

;(instances *)
;(list-deffunctions *)
(dribble-off)
(exit)
