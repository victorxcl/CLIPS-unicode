;; 如何安全过河
;; 有一条河，河边有一个猎人牵着一头狼，一个男人领着两个小孩，还有一个女人也带着两个小孩。
;; 如果猎人离开的话，狼就会把所有人都吃掉，如果男人离开的花，女人就会把男人的两个小孩掐
;; 死，而如果女人离开，男人则会把女人的两个小孩掐死。
;; 这是，河里有一条船，船上只能坐两个人（狼也算一个人），而所有人中，只有猎人、男人、女人会划船

(dribble-on log.txt)
;; 声明事实
(deffacts 初始事实
	(岸对面 A B)
	(岸对面 B A)
	(岸 A 0 猎人 狼 男人 男人小孩1 男人小孩2 女人 女人小孩1 女人小孩2)
	(岸 B 0)
	(船 A 0)
	(带着 男人 男人小孩1 男人小孩2)
	(带着 女人 女人小孩1 女人小孩2)
	(会划船 猎人)
	(会划船 男人)
	(会划船 女人)
)

(defrule 猎人和狼分开，狼会将它所在一边的所有人都吃掉
	(declare (salience 200))
	?f1 <- (岸 ?岸 ?i $?A 狼 $?B)
	(test (> (length$ (create$ ?A ?B)) 0))
	(not (岸 ?岸 ?i $? 猎人 $?))
	(岸对面 ?岸 ?对岸)
	?f2 <- (岸 ?对岸 ?i $?Y)
=>
	(printout t "猎人和狼分开，狼会将它所在一边的所有人都吃掉" crlf)
	(retract ?f1 ?f2)
)
(defrule 男人离开，女人就会把男人的两个小孩掐死
	(declare (salience 200))
	?f1 <- (岸 ?岸 ?i $? 女人 $?)
	(not (岸 ?岸 ?i $? 男人 $?))
	(or ?f2 <- (岸 ?岸 ?i $? 男人小孩1 $?)
		?f2 <- (岸 ?岸 ?i $? 男人小孩2 $?))
	(岸对面 ?岸 ?对岸)
	?f3 <- (岸 ?对岸 ?i $?)
=>
	(printout t "男人离开，女人就会把男人的两个小孩掐死" crlf)
	(retract ?f1 ?f2 ?f3)
)
(defrule 女人离开，男人就会把女人的两个小孩掐死
	(declare (salience 200))
	?f1 <- (岸 ?岸 ?i $? 男人 $?)
	(not (岸 ?岸 ?i $? 女人 $?))
	(or ?f2 <- (岸 ?岸 ?i $? 女人小孩1 $?)
		?f2 <- (岸 ?岸 ?i $? 女人小孩2 $?))
	(岸对面 ?岸 ?对岸)
	?f3 <- (岸 ?对岸 ?i $?)
=>
	(printout t "女人离开，男人就会把女人的两个小孩掐死" crlf)
	(retract ?f1 ?f2 ?f3)
)
;;船上一次最多只能坐两个人
;;只有猎人、男人、女人会划船（上船）
(defrule 只有猎人、男人、女人会划船（划船到对岸上岸1人）
	(岸 ?岸 ?i $?B ?M $?E)
	(船 ?岸 ?i)
	(会划船 ?M)
	(岸对面 ?岸 ?对岸)
	(岸 ?对岸 ?i $?Y)
=>
	(printout t ?M "划船从" ?岸 "到对岸" ?对岸 " 船上成员：" (create$ ?M) crlf)
	(assert (岸 ?岸   (+ ?i 1) ?B ?E)
			(岸 ?对岸 (+ ?i 1) ?Y ?M)
			(船 ?对岸 (+ ?i 1)))
)
(defrule 只有猎人、男人、女人会划船（划船到对岸上岸2人）
	(岸 ?岸 ?i $?B ?M $?m ?N $?E)
	(船 ?岸 ?i)
	(or (会划船 ?M) (会划船 ?N))
	(岸对面 ?岸 ?对岸)
	(岸 ?对岸 ?i $?Y)
=>
	(printout t "成员划船从" ?岸 "到对岸" ?对岸 " 船上成员：" (create$ ?M ?N) crlf)
	(assert (岸 ?岸   (+ ?i 1) ?B ?m ?E)
			(岸 ?对岸 (+ ?i 1) ?Y ?M ?N)
			(船 ?对岸 (+ ?i 1)))
)
(defrule 目标达成
	(declare (salience 200))
	(岸 A ?i)
	(岸 B ?i $?)
=>
	(printout t "目标达成" crlf)
	(halt)
)
(defrule 避免出现循环
	(declare (salience 200))
	(岸 ?岸 ?i $?X)
	?f <- (岸 ?岸 ?j&:(> ?j ?i) $?Y)
	(test (and (subsetp ?X ?Y)
		  	   (subsetp ?Y ?X)))
=>
	(printout t "避免出现循环" crlf)
	(retract ?f)
)
(defrule 调试
	(declare (salience 1000))
	(岸 ?岸 ?i $? ?X $? ?X $?)
=>
	(agenda)
)
;(dribble-on log.txt)
(set-break 调试)
(watch facts)
(watch rules)
;(watch activations)
(reset)
(run)
(show-breaks)
(dribble-off)
;(run 3000)
;(facts)
;(dribble-off)
;(exit)
