;;; vim:expandtab autoindent
(defclass Entity (is-a USER)
    (slot 实体   (type LEXEME))
    (slot 是一种 (type LEXEME))
)
(defclass Relation (is-a USER)
    (slot 实体 (type LEXEME))
    (slot 关系 (type LEXEME))
    (slot 目标 (type LEXEME))
    (slot 数量 (type SYMBOL)(allowed-symbols toOne toMany)(default toOne))
    (slot 关联 (type LEXEME)(allowed-symbols 组合 聚合)(default 组合))
    (slot 反向 (type LEXEME))
    (multislot 重载 (type LEXEME)(cardinality 2 2)); (重载 实体 关系)
)
(defclass Property (is-a USER)
    (slot 实体 (type LEXEME))
    (slot 属性 (type LEXEME))
    (slot 类型 (type LEXEME))
    (slot 默认 (type LEXEME))
    (multislot 枚举 (type LEXEME))
)
(defclass Binding (is-a Property)
    (multislot 依赖 (type LEXEME))
)


;;; 错误报告
;;; 不能有重复的Entity
;;; 不能有重复的Relation
;;; 不能有重复的Property
;;; 不能有重复的Binding
;;; 不能有循环依赖：避免内存泄漏
