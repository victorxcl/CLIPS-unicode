;; 如何安全过河
;; 有一条河，河边有一个猎人牵着一头狼，一个男人领着两个小孩，还有一个女人也带着两个小孩。
;; 如果猎人离开的话，狼就会把所有人都吃掉，如果男人离开的花，女人就会把男人的两个小孩掐
;; 死，而如果女人离开，男人则会把女人的两个小孩掐死。
;; 这是，河里有一条船，船上只能坐两个人（狼也算一个人），而所有人中，只有猎人、男人、女人会划船

(dribble-on log.txt)
(deftemplate 状态
	(slot parent (default root))
	(slot index (default 0))
	(multislot A)
	(multislot B)
	(slot 船(allowed-symbols A B))
	(multislot history)
)
;; 声明事实
(deffacts 初始事实
	;(岸对面 A B)
	;(岸对面 B A)
	;(岸 A 0 猎人 狼 男人 男人小孩1 男人小孩2 女人 女人小孩1 女人小孩2)
	;(岸 B 0)
	;(船 A 0)
	(状态 (A 猎人 狼 男人 男人小孩1 男人小孩2 女人 女人小孩1 女人小孩2)
		  (B)
		  (船 A))
	;(带着 男人 男人小孩1 男人小孩2)
	;(带着 女人 女人小孩1 女人小孩2)
	(会划船 猎人)
	(会划船 男人)
	(会划船 女人)
)

(defrule 猎人和狼分开，狼会将它所在一边的所有人都吃掉
	(declare (salience 200))
	(or (and ?f <- (状态 (parent ?parent)(A $?A 狼 $?B))
	               (test (> (length$ (create$ ?A ?B)) 0))
	               (not (状态 (parent ?parent)(A $? 猎人 $?))))
	    (and ?f <- (状态 (parent ?parent)(B $?A 狼 $?B))
	               (test (> (length$ (create$ ?A ?B)) 0))
	               (not (状态 (parent ?parent)(B $? 猎人 $?)))))
=>
	(printout t "猎人和狼分开，狼会将它所在一边的所有人都吃掉" crlf)
	(retract ?f)
)
;(defrule 猎人和狼分开，狼会将它所在一边的所有人都吃掉
;	(declare (salience 200))
;	(and ?f <- (状态 (parent ?parent)(A $?A 狼 $?B))
;	           (test (> (length$ (create$ ?A ?B)) 0))
;	           (not (状态 (parent ?parent)(A $? 猎人 $?))))
;=>
;	(printout t "猎人和狼分开，狼会将它所在一边的所有人都吃掉" crlf)
;	(retract ?f)
;)
;(defrule 猎人和狼分开，狼会将它所在一边的所有人都吃掉
;	(declare (salience 200))
;	(and ?f <- (状态 (parent ?parent)(B $?A 狼 $?B))
;	           (test (> (length$ (create$ ?A ?B)) 0))
;	           (not (状态 (parent ?parent)(B $? 猎人 $?))))
;=>
;	(printout t "猎人和狼分开，狼会将它所在一边的所有人都吃掉" crlf)
;	(retract ?f)
;)
(defrule 男人离开，女人就会把男人的两个小孩掐死
	(declare (salience 200))
	(or (and ?f <- (状态 (parent ?parent)(A $? 女人 $?))
	               (状态 (parent ?parent)(A $? 男人小孩1|男人小孩2 $?))
	               (not (状态 (parent ?parent)(A $? 男人 $?))))
	    (and ?f <- (状态 (parent ?parent)(B $? 女人 $?))
	               (状态 (parent ?parent)(B $? 男人小孩1|男人小孩2 $?))
	               (not (状态 (parent ?parent)(B $? 男人 $?)))))
=>
	(printout t "男人离开，女人就会把男人的两个小孩掐死" crlf)
	(retract ?f)
)
(defrule 女人离开，男人就会把女人的两个小孩掐死
	(declare (salience 200))
	(or (and ?f <- (状态 (parent ?parent)(A $? 男人 $?))
	               (状态 (parent ?parent)(A $? 女人小孩1|女人小孩2 $?))
	               (not (状态 (parent ?parent)(A $? 女人 $?))))
	    (and ?f <- (状态 (parent ?parent)(B $? 男人 $?))
	               (状态 (parent ?parent)(B $? 女人小孩1|女人小孩2 $?))
	               (not (状态 (parent ?parent)(B $? 女人 $?)))))
=>
	(printout t "女人离开，男人就会把女人的两个小孩掐死" crlf)
	(retract ?f)
)
;;船上一次最多只能坐两个人
;;只有猎人、男人、女人会划船（上船）
(defrule 只有猎人、男人、女人会划船（划船从A到B上岸1人）
	?f <- (状态 (船 A)(A $?A ?M $?B)(B $?C)(index ?i)(history $?history))
	(会划船 ?M)
=>
	(printout t ?M "划船从A到B 船上成员：" (create$ ?M) crlf)
	(bind ?F (assert (状态 (船 B)(A ?A ?B)(B ?C ?M)(parent ?f)(index (+ ?i 1))(history ?history ?f))))
	(ppfact ?f)(if (not ?F) then (ppfact ?f)(halt))
)
(defrule 只有猎人、男人、女人会划船（划船从B到A上岸1人）
	?f <- (状态 (船 B)(B $?A ?M $?B)(A $?C)(index ?i)(history $?history))
	(test (> (length$ ?C) 0));; 必须保证A上还有人，否则目标就已经完成了
	(会划船 ?M)
=>
	(printout t ?M "划船从B到A 船上成员：" (create$ ?M) crlf)
	(bind ?F (assert (状态 (船 A)(B ?A ?B)(A ?C ?M)(parent ?f)(index (+ ?i 1))(history ?history ?f))))
	(ppfact ?f)(if (not ?F) then (ppfact ?f)(halt))
)

(defrule 只有猎人、男人、女人会划船（划船从A到B上岸2人）
	?f <- (状态 (船 A)(A $?A ?M $?B ?N $?C)(B $?D)(index ?i)(history $?history))
	(or (会划船 ?M)(会划船 ?N))
=>
	(printout t "成员划船从A到B, 船上成员：" (create$ ?M ?N) crlf)
	(bind ?F (assert (状态 (船 B)(A ?A ?B ?C)(B ?D ?M ?N)(parent ?f)(index (+ ?i 1))(history ?history ?f))))
	(ppfact ?f)(if (not ?F) then (ppfact ?f)(halt))
)
(defrule 只有猎人、男人、女人会划船（划船从B到A上岸2人）
	?f <- (状态 (船 B)(B $?A ?M $?B ?N $?C)(A $?D)(index ?i)(history $?history))
	(test (> (length$ ?D) 0));; 必须保证A上还有人，否则目标就已经完成了
	(or (会划船 ?M)(会划船 ?N))
=>
	(printout t "成员划船从B到A, 船上成员：" (create$ ?M ?N) crlf)
	(bind ?F (assert (状态 (船 A)(B ?A ?B ?C)(A ?D ?M ?N)(parent ?f)(index (+ ?i 1))(history ?history ?f))))
	(ppfact ?f)(if (not ?F) then (ppfact ?f)(halt))
)

(defrule 目标达成
	(declare (salience 200))
	?f <- (状态 (A)(history $?history))
=>
	(printout t "目标达成" crlf)
	(progn$ (?v (create$ ?history ?f))(ppfact ?v))
	(halt)
)
(defrule 避免出现循环
	(declare (salience 200))
	      (状态 (A $?A1)(B $?B1)(船 ?船)(index ?i))
	?f <- (状态 (A $?A2&:(and (subsetp ?A1 ?A2)(subsetp ?A2 ?A1)))
                (B $?B2&:(and (subsetp ?B1 ?B2)(subsetp ?B2 ?B1)))
		        (船 ?船)
				(index ?j&:(> ?j ?i)))
=>
	(printout t "避免出现循环" crlf)
	(retract ?f)
)
;(dribble-on log.txt)
(watch facts)
;(watch rules)
;(watch activations)
;(set-break 猎人和狼分开，狼会将它所在一边的所有人都吃掉)
;(set-break 只有猎人、男人、女人会划船（划船从A到B上岸2人）)
;(set-fact-duplication TRUE)
(reset)
(run 10000)
;(run 3000)
(facts)
;(dribble-off)
;(exit)
