;; 就采用比较简单的15格拼图吧
;; 1234
;; 5678
;; 9ABC
;; DEF_  "_" => 表示空格
;; 随机打乱上面的阵列，然后找出还原为该目标阵列的方式
(dribble-on log.txt)

(deftemplate 拼图
    (slot parent (default root))
    (slot index (default 0))
    (multislot 阵列 (cardinality 16 16))
)

(deffacts 初始事实
    (最大步骤数目 12)
    (目标 1 2 3 4
          5 6 7 8
          9 A B C
          D E F _)
    (拼图 (阵列 1 2 3 4 
                5 A _ 8 
                D 7 6 C 
                E 9 B F))
)

(deffunction 更新 (?M ?i ?f ?s ?m)
    (bind ?M (replace$ ?M ?s ?s (nth$ ?m ?M)))
    (bind ?M (replace$ ?M ?m ?m _))
    (assert (拼图 (阵列 ?M) (parent ?f) (index (+ ?i 1))))
)


(defrule 移动
    ?f <- (拼图 (index ?i)
                (阵列 $?A _ $?B))
=>
    ;;  0  1  2  3
    ;;  4  5  6  7
    ;;  8  9 10 11
    ;; 12 13 14 15
    ;;;;;;;;;;;;;;;;;
    ;;  1  2  3  4
    ;;  5  6  7  8
    ;;  9 10 11 12
    ;; 13 14 15 16
    (bind ?M (create$ ?A _ ?B))
    (switch (length$ ?A)
            (case 0  then (更新 ?M ?i ?f  1  2)(更新 ?M ?i ?f  1  5))
            (case 1  then (更新 ?M ?i ?f  2  1)(更新 ?M ?i ?f  2  3)(更新 ?M ?i ?f  2  6))
            (case 2  then (更新 ?M ?i ?f  3  2)(更新 ?M ?i ?f  3  4)(更新 ?M ?i ?f  3  7))
            (case 3  then (更新 ?M ?i ?f  4  3)(更新 ?M ?i ?f  4  8))
            (case 4  then (更新 ?M ?i ?f  5  1)(更新 ?M ?i ?f  5  6)(更新 ?M ?i ?f  5  9))
            (case 5  then (更新 ?M ?i ?f  6  2)(更新 ?M ?i ?f  6  5)(更新 ?M ?i ?f  6  7)(更新 ?M ?i ?f  6 10))
            (case 6  then (更新 ?M ?i ?f  7  3)(更新 ?M ?i ?f  7  6)(更新 ?M ?i ?f  7  8)(更新 ?M ?i ?f  7 11))
            (case 7  then (更新 ?M ?i ?f  8  4)(更新 ?M ?i ?f  8  7)(更新 ?M ?i ?f  8 12))
            (case 8  then (更新 ?M ?i ?f  9  5)(更新 ?M ?i ?f  9 10)(更新 ?M ?i ?f  9 13))
            (case 9  then (更新 ?M ?i ?f 10  6)(更新 ?M ?i ?f 10  9)(更新 ?M ?i ?f 10 11)(更新 ?M ?i ?f 10 14))
            (case 10 then (更新 ?M ?i ?f 11  7)(更新 ?M ?i ?f 11 10)(更新 ?M ?i ?f 11 12)(更新 ?M ?i ?f 11 15))
            (case 11 then (更新 ?M ?i ?f 12  8)(更新 ?M ?i ?f 12 11)(更新 ?M ?i ?f 12 16))
            (case 12 then (更新 ?M ?i ?f 13  9)(更新 ?M ?i ?f 13 14))
            (case 13 then (更新 ?M ?i ?f 14 10)(更新 ?M ?i ?f 14 13)(更新 ?M ?i ?f 14 15))
            (case 14 then (更新 ?M ?i ?f 15 11)(更新 ?M ?i ?f 15 14)(更新 ?M ?i ?f 15 16))
            (case 15 then (更新 ?M ?i ?f 16 12)(更新 ?M ?i ?f 16 15)))
)

(defrule 目标达成
    (declare (salience 200))
    (目标 $?M)
    (拼图 (阵列 $?M))
=>
    (printout t "目标达成" crlf)
    (halt)
)

(defrule 避免循环
    (declare (salience 100))
          (拼图 (index ?i)(阵列 $?M))
    ?f <- (拼图 (index ?j&:(> ?j ?i))(阵列 $?M))
=>
    (retract ?f)
)

(defrule 不可超过最大深度
    (declare (salience 100))
    (最大步骤数目 ?max)
    ?f <- (拼图 (index ?j&:(> ?j ?max)))
=>
    (retract ?f)
)
(watch rules)
(watch facts)
(reset)
;(set-strategy breadth)
(set-strategy random)
(run )
(dribble-off)
(exit)
