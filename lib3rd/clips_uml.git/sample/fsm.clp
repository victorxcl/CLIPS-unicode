;;; vim:expandtab autoindent
;;; map <buffer> \\ :update<CR>:only<CR>:vertical terminal clips -f %<CR>
;;; map <buffer> \\ :update<CR>:!clips -f %<CR>:!dot -Tpdf -O 状态机.fsm.dot<CR>:!open 状态机.fsm.dot.pdf<CR>
;;; map <buffer> \\ :update<CR>:1tabnext<CR>:2wincmd w<CR>:silent normal \\<CR>:3tabnext<CR>
(dribble-on "fsm.clp.log")
(local-time)
;(set-dynamic-constraint-checking TRUE)

(deffunction clips_uml ($?path) (str-cat ../ (expand$ ?path)))
(deffunction clips_cwd ($?path) (str-cat  ./ (expand$ ?path)))
(defmodule MAIN (export ?ALL))
(batch* (clips_uml "utility.clp"))

(defmodule FSM (import MAIN ?ALL)(export ?ALL))
(batch* (clips_uml "fsm.clp"))

(defglobal FSM:DOT ?*PREFIX* = "状态机")
(defglobal FSM:JAVASCRIPT
    ?*PREFIX*                   = "状态机"
    ?*EVAL-EXPRESSION-ENABLED*  = FALSE
    ?*EVAL-VERBOSE-ENABLED*     = FALSE
    ?*EXPRESSION-HALT-ENABLED*  = FALSE
)
(defglobal FSM:JAVA 
    ?*PREFIX* = "状态机"
    ?*EXPRESSION-HALT-ENABLED* = FALSE
)

(definstances 客厅灯 ;;; 验证子状态机
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (of Machine    (状态机 灯)(初始状态 Off)(初始动作 "(action log)"
                                                      "(action trace)"))
    (of State      (状态机 灯)(状态 Off)(注解 关)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 灯)(状态 On )(注解 开)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of Transition (状态机 灯)(状态 Off)(触发器 "(trigger TurnOn)")(目标状态 On)
                   (守卫 "(guard isOff)") (动作 "(action powerOn)"
                                                "(+= (keypath self source hehe) 1)"
                                                "(+= (keypath self target wowo) 1)"
                                                "(assign (keypath self source haha) (not (keypath self haha)))"))
    (of Transition (状态机 灯)(状态 On )(触发器 "(trigger TurnOff)")(目标状态 Off)
                   (守卫 "(guard isOn)")(动作 "(action powerOff)"
                                              "(action trace)"))
    (of Trigger    (状态机 灯)(触发器 "(trigger TurnOn )")(注解 打开)(前置脚本 "(comment 打开)")(后置脚本 "(comment 打开了)"))
    (of Trigger    (状态机 灯)(触发器 "(trigger TurnOff)")(注解 关闭)(前置脚本 "(comment 关闭)")(后置脚本 "(comment 关闭了)"))
    (of Guard      (状态机 灯)(守卫 "(guard isOn )")(注解 如果灯开着)(脚本 "(is-current-state On )"))
    (of Guard      (状态机 灯)(守卫 "(guard isOff)")(注解 如果灯关着)(脚本 "(is-current-state Off)"))
    (of Action     (状态机 灯)(动作 "(action powerOn )")(注解 通电)(脚本 "(set-current-state On )"))
    (of Action     (状态机 灯)(动作 "(action powerOff)")(注解 断电)(脚本 "(set-current-state Off)"))
    (of Action     (状态机 灯)(动作 "(action log  )")(注解 输出日志))
    (of Action     (状态机 灯)(动作 "(action trace)")(注解 输出追踪))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (of Machine    (状态机 客厅灯)(初始状态 灯全关)(初始动作 "(action log)" "(action trace)"))
    (of SubMachine (状态机 客厅灯)(子状态机 灯1)(原型 灯))
    (of SubMachine (状态机 客厅灯)(子状态机 灯2)(原型 灯))
    (of SubMachine (状态机 客厅灯)(子状态机 灯3)(原型 灯))
    (of SubMachine (状态机 客厅灯)(子状态机 灯4)(原型 灯))
    (of SubMachine (状态机 客厅灯)(子状态机 灯5)(原型 灯))
    (of SubMachine (状态机 客厅灯)(子状态机 灯6)(原型 灯))
    (of SubMachine (状态机 客厅灯)(子状态机 灯7)(原型 灯))
    (of SubMachine (状态机 客厅灯)(子状态机 灯8)(原型 灯))
    (of State      (状态机 客厅灯)(状态   灯全关)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 客厅灯)(状态 灯1357开)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 客厅灯)(状态 灯1357关)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 客厅灯)(状态 灯2468开)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 客厅灯)(状态 灯2468关)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 客厅灯)(状态   灯全开)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of Transition (状态机 客厅灯)(状态   灯全关)(触发器 "(trigger TurnOn )")(目标状态 灯1357开)(动作 "(action powerOn  1357)"))
    (of Transition (状态机 客厅灯)(状态 灯1357开)(触发器 "(trigger TurnOff)")(目标状态 灯1357关)(动作 "(action powerOff 1357)"))
    (of Transition (状态机 客厅灯)(状态 灯1357关)(触发器 "(trigger TurnOn )")(目标状态 灯2468开)(动作 "(action powerOn  2468)"))
    (of Transition (状态机 客厅灯)(状态 灯2468开)(触发器 "(trigger TurnOff)")(目标状态 灯2468关)(动作 "(action powerOff 2468)"))
    (of Transition (状态机 客厅灯)(状态 灯2468关)(触发器 "(trigger TurnOn )")(目标状态   灯全开)(动作 "(action powerOn  All )"))
    (of Transition (状态机 客厅灯)(状态   灯全开)(触发器 "(trigger TurnOff)")(目标状态   灯全关)(动作 "(action powerOff All )"))

    (of Trigger    (状态机 客厅灯)(触发器 "(trigger TurnOn )")(注解 打开)(前置脚本 "(comment '打开')"))
    (of Trigger    (状态机 客厅灯)(触发器 "(trigger TurnOff)")(注解 关闭)(前置脚本 "(comment '关闭')"))

    (of Action     (状态机 客厅灯)(动作 "(action log  )")(注解 输出日志))
    (of Action     (状态机 客厅灯)(动作 "(action trace)")(注解 输出追踪))
    (of Action     (状态机 客厅灯)(动作 "(action powerOn All)"  )(脚本 "(action powerOn 1357)"
                                                                       "(action powerOn 2468)"))

    (of Action     (状态机 客厅灯)(动作 "(action powerOff All)" )(脚本 "(action powerOff 1357)"
                                                                       "(action powerOff 2468)"))

    (of Action     (状态机 客厅灯)(动作 "(action powerOn 1357)" )(脚本 "(trigger (keypath 灯1) TurnOn)"
                                                                       "(trigger (keypath 灯3) TurnOn)"
                                                                       "(trigger (keypath 灯5) TurnOn)"
                                                                       "(trigger (keypath 灯7) TurnOn)"))

    (of Action     (状态机 客厅灯)(动作 "(action powerOff 1357)")(脚本 "(trigger (keypath 灯1) TurnOff)"
                                                                       "(trigger (keypath 灯3) TurnOff)"
                                                                       "(trigger (keypath 灯5) TurnOff)"
                                                                       "(trigger (keypath 灯7) TurnOff)"))

    (of Action     (状态机 客厅灯)(动作 "(action powerOn 2468)" )(脚本 "(trigger (keypath 灯2) TurnOn)"
                                                                       "(trigger (keypath 灯4) TurnOn)"
                                                                       "(trigger (keypath 灯6) TurnOn)"
                                                                       "(trigger (keypath 灯8) TurnOn)"))

    (of Action     (状态机 客厅灯)(动作 "(action powerOff 2468)")(脚本 "(trigger (keypath 灯2) TurnOff)"
                                                                       "(trigger (keypath 灯4) TurnOff)"
                                                                       "(trigger (keypath 灯6) TurnOff)"
                                                                       "(trigger (keypath 灯8) TurnOff)"))

   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯1)(翻译 light1))
   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯2)(翻译 light2))
   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯3)(翻译 light3))
   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯4)(翻译 light4))
   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯5)(翻译 light5))
   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯6)(翻译 light6))
   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯7)(翻译 light7))
   ;(of Translation(状态机 客厅灯)(类别 SubMachine)(名称 灯8)(翻译 light8))
)
(definstances 计算器 ;; 验证多个子状态机的协作
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (of Machine    (状态机 按钮)(初始状态 弹起)(初始动作 "(action log)"
                                                         "(action trace)"))
    (of Variable   (状态机 按钮)(变量   值)(类型 STRING)(注解 按钮按下表示的意义))
    (of Callback   (状态机 按钮)(回调 输出)(参数 "(parameter A int)"))

    (of State      (状态机 按钮)(状态 弹起)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 按钮)(状态 按下)(进入动作 "(action log)")(离开动作 "(action trace)"))

    (of Transition (状态机 按钮)(状态 弹起)(触发器 "(trigger Press  )")(目标状态 按下))
    (of Transition (状态机 按钮)(状态 按下)(触发器 "(trigger Release)")(目标状态 弹起)(动作 "(callback 输出 (keypath 值))"))

    (of Trigger    (状态机 按钮)(触发器 "(trigger Press  )")(注解 按下)(前置脚本 "(comment 按下)"))
    (of Trigger    (状态机 按钮)(触发器 "(trigger Release)")(注解 按下)(前置脚本 "(comment 弹起)"))

    (of Action     (状态机 按钮)(动作 "(action log  )")(注解 输出日志))
    (of Action     (状态机 按钮)(动作 "(action trace)")(注解 输出追踪))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (of Machine    (状态机 液晶屏)(初始状态 Idle)(初始动作 "(action log)"
                                                            "(action trace)"))
    (of Variable   (状态机 液晶屏)(变量 值)(类型 STRING)(注解 数据显示))

    (of State      (状态机 液晶屏)(状态 Idle)(进入动作 "(action log)")(离开动作 "(action trace)"))

    (of Action     (状态机 液晶屏)(动作 "(action log  )")(注解 输出日志))
    (of Action     (状态机 液晶屏)(动作 "(action trace)")(注解 输出追踪))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (of Machine    (状态机 计算器)(初始状态 关机)(初始动作 "(action log)"
                                                           "(action trace)"))
    (of SubMachine (状态机 计算器)(子状态机 屏幕 )(原型 液晶屏))
    (of SubMachine (状态机 计算器)(子状态机 按钮0)(原型 按钮  )(脚本 "(assign (keypath 按钮0 值) \"0\")" "(assign (keypath 按钮0 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮1)(原型 按钮  )(脚本 "(assign (keypath 按钮1 值) \"1\")" "(assign (keypath 按钮1 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮2)(原型 按钮  )(脚本 "(assign (keypath 按钮2 值) \"2\")" "(assign (keypath 按钮2 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮3)(原型 按钮  )(脚本 "(assign (keypath 按钮3 值) \"3\")" "(assign (keypath 按钮3 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮4)(原型 按钮  )(脚本 "(assign (keypath 按钮4 值) \"4\")" "(assign (keypath 按钮4 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮5)(原型 按钮  )(脚本 "(assign (keypath 按钮5 值) \"5\")" "(assign (keypath 按钮5 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮6)(原型 按钮  )(脚本 "(assign (keypath 按钮6 值) \"6\")" "(assign (keypath 按钮6 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮7)(原型 按钮  )(脚本 "(assign (keypath 按钮7 值) \"7\")" "(assign (keypath 按钮7 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮8)(原型 按钮  )(脚本 "(assign (keypath 按钮8 值) \"8\")" "(assign (keypath 按钮8 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮9)(原型 按钮  )(脚本 "(assign (keypath 按钮9 值) \"9\")" "(assign (keypath 按钮9 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮+)(原型 按钮  )(脚本 "(assign (keypath 按钮+ 值) \"+\")" "(assign (keypath 按钮+ 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮-)(原型 按钮  )(脚本 "(assign (keypath 按钮- 值) \"-\")" "(assign (keypath 按钮- 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮*)(原型 按钮  )(脚本 "(assign (keypath 按钮* 值) \"*\")" "(assign (keypath 按钮* 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮/)(原型 按钮  )(脚本 "(assign (keypath 按钮/ 值) \"/\")" "(assign (keypath 按钮/ 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮=)(原型 按钮  )(脚本 "(assign (keypath 按钮= 值) \"=\")" "(assign (keypath 按钮= 输出) (keypath 按钮输出))"))
    (of SubMachine (状态机 计算器)(子状态机 按钮C)(原型 按钮  )(脚本 "(assign (keypath 按钮C 值) \"C\")" "(assign (keypath 按钮C 输出) (keypath 按钮输出))"))

    (of Callback   (状态机 计算器)(回调 按钮输出)(参数 "(parameter A int)"))

    (of State      (状态机 计算器)(状态 关机)(进入动作 "(action log)")(离开动作 "(action trace)"))
    (of State      (状态机 计算器)(状态 开机)(进入动作 "(action log)")(离开动作 "(action trace)"))

    (of Transition (状态机 计算器)(状态 关机)(触发器 "(trigger PowerOn )")(目标状态 开机))
    (of Transition (状态机 计算器)(状态 开机)(触发器 "(trigger PowerOff)")(目标状态 关机))

    (of Action     (状态机 计算器)(动作 "(action log  )")(注解 输出日志))
    (of Action     (状态机 计算器)(动作 "(action trace)")(注解 输出追踪))

    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮0)(翻译 button0))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮1)(翻译 button1))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮2)(翻译 button2))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮3)(翻译 button3))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮4)(翻译 button4))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮5)(翻译 button5))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮6)(翻译 button6))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮7)(翻译 button7))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮8)(翻译 button8))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮9)(翻译 button9))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮+)(翻译 buttonADD))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮-)(翻译 buttonSUB))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮*)(翻译 buttonMUL))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮/)(翻译 buttonDIV))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮=)(翻译 buttonEQL))
    (of Translation(状态机 计算器)(类别 SubMachine)(名称 按钮C)(翻译 buttonCLR))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)
(definstances 角色 ;;; 验证切面
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (of Machine    (状态机 角色)(初始状态 Idle)(初始动作 "(action log)"))
    (of Aspect     (状态机 角色)(切面 跑))
    (of Aspect     (状态机 角色)(切面 走))
    (of Aspect     (状态机 角色)(切面 爬))

    (of Machine    (状态机 跑)(初始状态 Idle)(初始动作 "(action log)"))
    (of Machine    (状态机 走)(初始状态 Idle)(初始动作 "(action log)"))
    (of Machine    (状态机 爬)(初始状态 Idle)(初始动作 "(action log)"))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)
;(watch rules)
;(watch focus)
(reset)
(focus FSM)
(focus FSM:DOT)
(focus FSM:JAVASCRIPT)
(focus FSM:JAVA)
(get-profile-percent-threshold)
(progn (profile constructs) (run) (profile off)(profile-info))
(local-time)
;(instances *)
(list-defglobals *)
;(browse-classes)
(dribble-off)
(exit)
