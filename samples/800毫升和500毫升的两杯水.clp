(deftemplate 状态
	(slot i (type INTEGER)(default 0))
	(slot p (type FACT-ADDRESS SYMBOL)(default nil))
	(slot 杯子甲 (type INTEGER)(default 0))
	(slot 杯子乙 (type INTEGER)(default 0))
	(slot 过程 (type SYMBOL))
)
(deftemplate 容量
	(slot name (allowed-values 杯子甲 杯子乙))
	(slot max (type INTEGER)(default 0))
)

(deffacts 初始事实
	(状态 (杯子甲 0)
		  (杯子乙 0))
	(容量 (name 杯子甲)(max 500))
	(容量 (name 杯子乙)(max 800))
	(目标 100)
)

(deffunction report (?p)
	(while (neq ?p nil) do
		(ppfact ?p)
		(bind ?p (fact-slot-value ?p p)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule 目标达成，杯子甲里面装上了目标容量的水
	(declare (salience 100))
	?p <- (状态 (杯子甲 ?x))
	(目标 ?x)
=>
	(printout t "目标已达成，杯子甲里面装上了" ?x "ml的水" crlf)
	(report ?p)
)
(defrule 目标达成，杯子乙里面装上了目标容量的水
	?p <- (状态 (杯子乙 ?x))
	(目标 ?x)
=>
	(printout t "目标已达成，杯子乙里面装上了" ?x "ml的水" crlf)
	(report ?p)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule 将杯子甲装满水 "只要杯子没有盛满，就可以再次装满水"
	(容量 (name 杯子甲)(max ?max))
	?f <- (状态 (i ?i)(杯子甲 ?x&:(< ?x ?max)))
=>
	(duplicate ?f (i (+ ?i 1))(p ?f)(杯子甲 ?max)(过程 将杯子甲装满水))
)
(defrule 将杯子乙装满水 "只要杯子没有盛满，就可以再次装满水"
	(容量 (name 杯子乙)(max ?max))
	?f <- (状态 (i ?i)(杯子乙 ?x&:(< ?x ?max)))
=>
	(duplicate ?f (i (+ ?i 1))(p ?f)(杯子乙 ?max)(过程 将杯子乙装满水))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule 将杯子甲里的水倒掉
	?f <- (状态 (i ?i)(杯子甲 ?x&:(> ?x 0)))
=>
	(duplicate ?f (i (+ ?i 1))(p ?f)(杯子甲 0)(过程 将杯子甲里的水倒掉))
)
(defrule 将杯子乙里的水倒掉
	?f <- (状态 (i ?i)(杯子乙 ?x&:(> ?x 0)))
=>
	(duplicate ?f (i (+ ?i 1))(p ?f)(杯子乙 0)(过程 将杯子乙里的水倒掉))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule 将水从杯子甲倒入杯子乙
	(容量 (name 杯子甲)(max ?max1))
	(容量 (name 杯子乙)(max ?max2))
	?f <- (状态 (i ?i)
				(杯子甲 ?x1&:(> ?x1 0))
			 	(杯子乙 ?x2&:(< ?x2 ?max2)))
=>
	(bind ?F ?x1)          ; 可以倒出来的水量
	(bind ?T (- ?max2 ?x2)); 剩余的容量
	(if (> ?F ?T) 
		then (duplicate ?f (i (+ ?i 1))(p ?f)(杯子甲 (- ?F ?T))(杯子乙 ?max2)     (过程 将水从杯子甲倒入杯子乙))
		else (duplicate ?f (i (+ ?i 1))(p ?f)(杯子甲 0)        (杯子乙 (+ ?x2 ?F))(过程 将水从杯子甲倒入杯子乙)))
)
(defrule 将水从杯子乙倒入杯子甲
	(容量 (name 杯子乙)(max ?max1))
	(容量 (name 杯子甲)(max ?max2))
	?f <- (状态 (i ?i)
				(杯子乙 ?x1&:(> ?x1 0))
			 	(杯子甲 ?x2&:(< ?x2 ?max2)))
=>
	(bind ?F ?x1)          ; 可以倒出来的水量
	(bind ?T (- ?max2 ?x2)); 剩余的容量
	(if (> ?F ?T) 
		then (duplicate ?f (i (+ ?i 1))(p ?f)(杯子乙 (- ?F ?T))(杯子甲 ?max2)     (过程 将水从杯子乙倒入杯子甲))
		else (duplicate ?f (i (+ ?i 1))(p ?f)(杯子乙 0)        (杯子甲 (+ ?x2 ?F))(过程 将水从杯子乙倒入杯子甲)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule 不可以有重复的步骤
	(declare (salience 100))
	      (状态 (i ?i)           (杯子甲 ?x1)(杯子乙 ?x2))
	?f <- (状态 (i ?j&:(> ?j ?i))(杯子甲 ?x1)(杯子乙 ?x2))
=>
	(retract ?f)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(watch all)
;(watch facts)
;(watch rules)
(reset)
(facts)
(run)
(exit)
(deftemplate 杯子
