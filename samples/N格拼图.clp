;; 9格拼图游戏

(defglobal ?*R* = 3
		   ?*C* = 3
		   ?*最大推测深度* = 15
		   )

(defclass 推理
	(is-a USER)
	;(role abstract)
	(slot i(type INTEGER)(default 0))
	(slot p(type INSTANCE-ADDRESS SYMBOL)(default nil))
)
(defclass 拼图
	(is-a 推理)
	;(role concrete)
	;(multislot 状态 (cardinality (* ?*R* ?*C*) (* ?*R* ?*C*)))
	(multislot 状态 (cardinality 9 9))
)

(deffacts 初始事实
	(起始 4 3 _
          7 1 5
          8 2 6)
	(目标 1 2 3
          4 5 6
          7 8 _)
)

(deffunction R (?i) "将索引号i对应成为行号"
	(+ (div (- ?i 1) ?*C*) 1)
)
(deffunction C (?i) "将索引号i对应成为列号"
	(+ (mod (- ?i 1) ?*C*) 1)
)
(deffunction M (?r ?c) "将行号r和列号c对应成为索引号(member$)"
	(bind ?ir (- ?r 1))
	(bind ?ic (- ?c 1))
	(+ (* ?ir ?*C*) ?ic 1) ; 最后加上1是为了映射为CLIPS的索引号
)
(deffunction I (?i)
	(+ ?i 1)
)
(M 1 1)
;(deffunction +1 (?i) (+ ?i 1))
;(deffunction -1 (?i) (- ?i 1))
;(deffunction +2 (?i) (+ ?i 2))
;(deffunction -2 (?i) (- ?i 2))

;(printout t "(R 9) = " (R 9) crlf)
;(printout t "(C 9) = " (C 9) crlf)
;(printout t "(R 8) = " (R 8) crlf)
;(printout t "(C 8) = " (C 8) crlf)
;(printout t "(R 7) = " (R 7) crlf)
;(printout t "(C 7) = " (C 7) crlf)
;(printout t "(R 6) = " (R 6) crlf)
;(printout t "(C 6) = " (C 6) crlf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction 报告 (?o)
	(while (neq nil ?o) do
		(send ?o print)
		(progn$ (?n (send ?o get-状态))
			(printout t ?n " ")
			(if (= (mod ?n-index ?*C*) 0) then (printout t crlf))
		)
		(printout t "====================" crlf)
		(bind ?o (send ?o get-p))
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction 空白在左上 (?G)
	(bind ?i (member$ _ ?G))
	(and (= (R ?i) (I 0))
		 (= (C ?i) (I 0)))
)
(deffunction 空白在右上 (?G)
	(bind ?i (member$ _ ?G))
	(and (= (R ?i) (I 0))
		 (= (C ?i) (I (- ?*C* 1))))
)
(deffunction 空白在左下 (?G)
	(bind ?i (member$ _ ?G))
	(and (= (R ?i) (I (- ?*R* 1)))
		 (= (C ?i) (I 0)))
)
(deffunction 空白在右下 (?G)
	(bind ?i (member$ _ ?G))
	(and (= (R ?i) (I (- ?*R* 1)))
		 (= (C ?i) (I (- ?*C* 1))))
)
(deffunction 空白在左边(?G)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(and (and (>= ?r (I 1)) (<= ?r (I (- ?*R* 2))))
		 (= ?c (I 0)))
)
(deffunction 空白在右边(?G)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(and (and (>= ?r (I 1)) (<= ?r (I (- ?*R* 2))))
		 (= ?c (I (- ?*C* 1))))
)
(deffunction 空白在上边(?G)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(and (= ?r (I 0))
		 (and (>= ?c (I 1)) (<= ?c (I (- ?*C* 2)))))
)
(deffunction 空白在下边(?G)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(and (= ?r (I (- ?*R* 1)))
		 (and (>= ?c (I 1)) (<= ?c (I (- ?*C* 2)))))
)
(deffunction 空白在中间(?G)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(and (and (>= ?r (I 1)) (<= ?r (I (- ?*R* 2))))
		 (and (>= ?c (I 1)) (<= ?c (I (- ?*C* 2)))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod 交换 (?G ?r1 ?c1 ?r2 ?c2)
	(bind ?i (M ?r1 ?c1))
	(bind ?j (M ?r2 ?c2))
	(交换 ?G ?i ?j)
)
(defmethod 交换 (?G ?i ?j)
	(bind ?mi (nth$ ?i ?G))
	(bind ?mj (nth$ ?j ?G))
	(bind ?G (replace$ ?G ?i ?i ?mj))
	(bind ?G (replace$ ?G ?j ?j ?mi))
)
(交换 (create$ 
1 2 3 
4 5 6
7 8 9) 2 5)
(交换 (create$ 
1 2 3 
4 5 6
7 8 9) 1 2 2 2)
;(browse-classes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule 初始化状态
	(起始 $?G)
=>
	(make-instance of 拼图 (状态 ?G))
)
(defrule 目标状态达成
	(declare (salience 100))
	(目标 $?G)
	?o <- (object (is-a 拼图)(状态 $?G))
=>
	(printout t "目标状态已经达成" crlf)
	(报告 ?o)
)
(defrule 限制最大推测深度
	(declare (salience 100))
	?o <- (object (is-a 拼图)(i ?i&:(> ?i ?*最大推测深度*)))
=>
	(unmake-instance ?o)
)
(defrule 避免重复
	(declare (salience 100))
	      (object (is-a 拼图)(状态 $?G)(i ?i))
	?o <- (object (is-a 拼图)(状态 $?G)(i ?j&:(> ?j ?i)))
=>
	(unmake-instance ?o)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule 空白在左上
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在左上 ?G))
=>
	(printout t "空白在左上" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 (交换 ?G ?r ?c (+ ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 (交换 ?G ?r ?c ?r (+ ?c 1))))
)
(defrule 空白在右上
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在右上 ?G))
=>
	(printout t "空白在右上" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 (交换 ?G ?r ?c (+ ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (- ?c 1))))
)
(defrule 空白在左下
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在左下 ?G))
=>
	(printout t "空白在左下" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (- ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (+ ?c 1))))
)
(defrule 空白在右下
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在右下 ?G))
=>
	(printout t "空白在右下" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (- ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (- ?c 1))))
)
(defrule 空白在左
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在左边 ?G))
=>
	(printout t "空白在左边" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (- ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (+ ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (+ ?c 1))))
)
(defrule 空白在右
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在右边 ?G))
=>
	(printout t "空白在右边" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (- ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (+ ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (- ?c 1))))
)
(defrule 空白在上
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在上边 ?G))
=>
	(printout t "空白在上边" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (+ ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (- ?c 1))))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (+ ?c 1))))
)
(defrule 空白在下
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在下边 ?G))
=>
	(printout t "空白在下边" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (- ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (- ?c 1))))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (+ ?c 1))))
)
(defrule 空白在中间
	?o <- (object (is-a 拼图) (状态 $?G) (i ?index))
	(test (空白在中间 ?G))
=>
	(printout t "空白在中间" crlf)
	(bind ?i (member$ _ ?G))
	(bind ?r (R ?i))
	(bind ?c (C ?i))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (- ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c (+ ?r 1) ?c)))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (- ?c 1))))
	(message-duplicate-instance ?o (i (+ ?index 1))(p ?o)(状态 	(交换 ?G ?r ?c ?r (+ ?c 1))))
)

(set-strategy breadth)
;(watch all)
(watch rules)
(watch facts)
(watch instances)
(watch statistics)
(reset)
(run)
(facts)
(exit)
