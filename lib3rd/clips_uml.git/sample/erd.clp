;;; vim:expandtab autoindent
;;; map <buffer> \\ :update<CR>:only<CR>:vertical terminal clips -f %<CR>
;;; map <buffer> \\ :update<CR>:only<CR>:clips -f %<CR>
;;; map <buffer> \\ :update<CR>:!clips -f %<CR>:!dot -Tpdf -O 学校.erd.dot<CR>:!open 学校.erd.dot.pdf<CR>
;;; map <buffer> \\ :update<CR>:1tabnext<CR>:2wincmd w<CR>:normal \\<CR>:3tabnext<CR>
(dribble-on "erd.clp.log")

(deffunction clips_uml (?module) (str-cat "../" ?module))
(defmodule MAIN (export ?ALL))
(load (clips_uml "utility.clp"))

(defmodule ERD (import MAIN ?ALL)(export ?ALL))
(batch (clips_uml "erd.clp"))

(defglobal ERD:DOT ?*PREFIX*    = "学校")

(definstances 数据
    (of Entity   (实体 人))
    (of Property (实体 人)(属性 姓)(类型 STRING))
    (of Property (实体 人)(属性 名)(类型 STRING))
    (of Binding  (实体 人)(属性 姓名)(类型 STRING)(依赖 self.姓 self.名))

    (of Entity   (实体 班级))
    (of Relation (实体 班级)(关系 学生)(目标 学生)(数量 toMany)(关联 聚合))
    (of Relation (实体 班级)(关系 教师)(目标 教师)(数量 toMany)(关联 聚合))
    (of Relation (实体 班级)(关系 班长)(目标 班长))
    (of Property (实体 班级)(属性   班)(类型 STRING))
    (of Property (实体 班级)(属性 年级)(类型 STRING))

    (of Entity   (实体 学校))
    (of Relation (实体 学校)(关系 校长)(目标 校长)             (关联 聚合))
    (of Relation (实体 学校)(关系 学生)(目标 学生)(数量 toMany)(关联 聚合))
    (of Relation (实体 学校)(关系 教师)(目标 教师)(数量 toMany)(关联 聚合))
    (of Relation (实体 学校)(关系 班级)(目标 班级)(数量 toMany))
    (of Relation (实体 学校)(关系 课程)(目标 课程)(数量 toMany))
    (of Property (实体 学校)(属性 名称)(类型 STRING))

    (of Entity   (实体 课程))
    (of Relation (实体 课程)(关系 教师)(目标 教师)             (关联 聚合)(反向 课程))
    (of Relation (实体 课程)(关系 学生)(目标 学生)(数量 toMany)(关联 聚合))
    (of Property (实体 课程)(属性 名称)(类型 STRING))

    (of Entity   (实体 教师)(是一种 人))
    (of Relation (实体 教师)(关系 课程)(目标 课程)(数量 toMany)(关联 聚合)(反向 教师))

    (of Entity (实体 学生)(是一种 人))
    (of Entity (实体 校长)(是一种 人))
    (of Entity (实体 班长)(是一种 学生))
)
(watch rules)
;(watch instances)
(reset)
(focus ERD)
(focus ERD:DOT)
(get-profile-percent-threshold)
(progn (profile constructs) (run) (profile off))
(profile-info)
;(列举实例)
(dribble-off)

(exit)
