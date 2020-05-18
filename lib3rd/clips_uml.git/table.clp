;;; vim:expandtab autoindent

(defclass Table   (is-a USER))
(defclass Cell    (is-a USER)
	(slot 表格 (type INSTANCE)(allowed-classes Table))
	(slot 行   (type INTEGER))
	(slot 列   (type INTEGER))
	(slot 内容 (type INSTANCE)(allowed-classes Content))
)
(defclass Content (is-a USER))
(defclass Text (is-a Content))
(defclass Formula (is-a Content))
