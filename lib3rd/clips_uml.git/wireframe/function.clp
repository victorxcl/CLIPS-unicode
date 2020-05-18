;;; vim: expandtab autoindent
;;; map <buffer> \\ :update<CR>:1tabnext<CR>:2wincmd w<CR>:silent normal \\<CR>:2tabnext<CR>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric #WireFrameParse)
(defmethod  #WireFrameParse 10 ((?x USER)    ) (halt-method-with-arguments #WireFrameParse ?x))
(defmethod  #WireFrameParse 11 ((?x USER) $?X) (halt-method-with-arguments #WireFrameParse ?x (expand$ ?X)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction #make-packed-arguments#redirect#unmake-packed-arguments (?Layout $?X)
    (bind ?x (make-packed-arguments ?X))
    (bind ?v (funcall #WireFrameParse (make-instance of ?Layout) (expand$ ?x)))
    (unmake-packed-arguments ?x)
    (return ?v)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction Layout         ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Layout       ?X))
(deffunction Header         ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Header       ?X))
(deffunction Footer         ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Footer       ?X))
(deffunction Content        ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Content      ?X))
(deffunction Sider          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Sider        ?X))
(deffunction  List          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments  List        ?X))
(deffunction HList          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments HList        ?X))
(deffunction VList          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments VList        ?X))
(deffunction Grid           ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Grid         ?X))
(deffunction Form           ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Form         ?X))
(deffunction Embed          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Embed        ?X))
(deffunction Image          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Image        ?X))
(deffunction Text           ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Text         ?X))
(deffunction Label          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Label        ?X))
(deffunction TextField      ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments TextField    ?X))
(deffunction TextArea       ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments TextArea     ?X))
(deffunction Button         ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Button       ?X))
(deffunction Check          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Check        ?X))
(deffunction Radio          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Radio        ?X))
(deffunction Switch         ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Switch       ?X))
(deffunction Breadcrumb     ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments Breadcrumb   ?X))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction useScript      ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useScript    ?X))
(deffunction useProperty    ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useProperty  ?X))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction useState       ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useState     ?X))
(deffunction useEffect      ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useEffect    ?X))
(deffunction useContext     ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useContext   ?X))
(deffunction useReducer     ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useReducer   ?X))
(deffunction useCallback    ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useCallback  ?X))
(deffunction useMemo        ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useMemo      ?X))
(deffunction useRef         ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments useRef       ?X))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction @Link          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @Link        ?X))
(deffunction @Reference     ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @Reference   ?X))
(deffunction @Ref           ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @Reference   ?X))
(deffunction @Grid          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @Grid        ?X))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction @Script        ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @Script      ?X))
(deffunction @Eval          ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @Eval        ?X))
(deffunction @OnEvent       ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @OnEvent     ?X))
(deffunction @OnClick       ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @OnClick     ?X))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction @useProperty   ($?X) (#make-packed-arguments#redirect#unmake-packed-arguments @useProperty ?X))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component
(defmethod #WireFrameParse  100 ((?x Component)) (return ?x))
(defmethod #WireFrameParse  101 ((?x Component)(?k (eq ?k      named:))(?v LEXEME)     $?X) (send            ?x put-named        ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  102 ((?x Component)(?k (eq ?k properties:))(?v LEXEME)     $?X) (slot-append$    ?x properties       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  103 ((?x Component)(?k (eq ?k properties:))(?v Multifield) $?X) (slot-append$    ?x properties (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  104 ((?x Component)(?k (eq ?k parameters:))(?v LEXEME)     $?X) (slot-append$    ?x parameters       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  105 ((?x Component)(?k (eq ?k parameters:))(?v Multifield) $?X) (slot-append$    ?x parameters (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  106 ((?x Component)(?v Annotation)                         $?X) (modify-instance ?v (annotation      ?x               )) (#WireFrameParse ?x (expand$ ?X)))

;;; Container
(defmethod #WireFrameParse  191 ((?x Container)(?v Component) $?X) (slot-append$ ?x children ?v) (#WireFrameParse ?x (expand$ ?X)))

;;; Layout
(defmethod #WireFrameParse  201 ((?x Layout)(?v Layout ) $?X) (slot-append$ ?x children ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  202 ((?x Layout)(?v Header ) $?X) (slot-append$ ?x children ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  203 ((?x Layout)(?v Footer ) $?X) (slot-append$ ?x children ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  204 ((?x Layout)(?v Content) $?X) (slot-append$ ?x children ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  205 ((?x Layout)(?v Sider  ) $?X) (slot-append$ ?x children ?v) (#WireFrameParse ?x (expand$ ?X)))
;;; Header
;;; Footer
;;; Content
;;; Sider

;;; Image
(defmethod #WireFrameParse  601 ((?x Image)(?k (eq ?k    rows:))(?v INTEGER)     $?X) (modify-instance ?x (rows    ?v)) (#WireFrameParse ?x             (expand$ ?X)))
(defmethod #WireFrameParse  602 ((?x Image)(?k (eq ?k columns:))(?v INTEGER)     $?X) (modify-instance ?x (columns ?v)) (#WireFrameParse ?x             (expand$ ?X)))
(defmethod #WireFrameParse  603 ((?x Image)(?k (eq ?k    cols:))(?v INTEGER)     $?X)                                   (#WireFrameParse ?x columns: ?v (expand$ ?X)))
(defmethod #WireFrameParse  604 ((?x Image)(?k (eq ?k    text:))(?v INTEGER)     $?X) (modify-instance ?x (text    ?v)) (#WireFrameParse ?x             (expand$ ?X)))
(defmethod #WireFrameParse  605 ((?x Image)(?v LEXEME (not (key-argument-p ?v))) $?X)                                   (#WireFrameParse ?x    text: ?v (expand$ ?X)))

;;; List
(defmethod #WireFrameParse  702 ((?x List)(?k (eq ?k direction:))(?v  LEXEME) $?X) (modify-instance ?x (direction ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse  703 ((?x List)(?k (eq ?k    repeat:))(?v INTEGER) $?X) (modify-instance ?x (repeat    ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; Grid
(defmethod #WireFrameParse  801 ((?x Grid)(?k (eq ?k    rows:))(?v INTEGER) $?X) (modify-instance ?x (rows    ?v)) (#WireFrameParse ?x             (expand$ ?X)))
(defmethod #WireFrameParse  802 ((?x Grid)(?k (eq ?k columns:))(?v INTEGER) $?X) (modify-instance ?x (columns ?v)) (#WireFrameParse ?x             (expand$ ?X)))
(defmethod #WireFrameParse  803 ((?x Grid)(?k (eq ?k    cols:))(?v INTEGER) $?X)                                   (#WireFrameParse ?x columns: ?v (expand$ ?X)))
(defmethod #WireFrameParse  804 ((?x Grid)(?k (eq ?k  gutter:))(?v INTEGER) $?X) (modify-instance ?x (gutter  ?v)) (#WireFrameParse ?x             (expand$ ?X)))

;;; Embed
(defmethod #WireFrameParse  904 ((?x Embed)(?k (eq ?k default:))(?v LEXEME) $?X) (send ?x put-default ?v) (#WireFrameParse ?x (expand$ ?X)))

;;; Text
(defmethod #WireFrameParse 1001 ((?x Text)(?v LEXEME (not (key-argument-p ?v))) $?X) (modify-instance ?x (text ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 1002 ((?x Text)(?v NUMBER)                           $?X) (modify-instance ?x (text ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 1003 ((?x Text)(?k (eq ?k text:))(?v LEXEME)         $?X) (modify-instance ?x (text ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; Label

;;; Button
(defmethod #WireFrameParse 1201 ((?x Button)(?k (eq ?k title:))(?v LEXEME)        $?X) (modify-instance ?x (title  ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 1202 ((?x Button)(?v LEXEME (not (key-argument-p ?v))) $?X) (#WireFrameParse ?x  title: ?v (expand$ ?X)))

;;; Check
(defmethod #WireFrameParse 1303 ((?x Check)(?k (eq ?k checked:))(?v LEXEME)      $?X) (modify-instance ?x (checked ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; Radio
(defmethod #WireFrameParse 1403 ((?x Radio)(?k (eq ?k selected:))(?v LEXEME)     $?X) (modify-instance ?x (selected ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 1404 ((?x Radio)(?k (eq ?k    group:))(?v LEXEME)     $?X) (modify-instance ?x (group    ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; Switch
(defmethod #WireFrameParse 1501 ((?x Switch)(?k (eq ?k status:))(?v LEXEME)     $?X) (modify-instance ?x (status ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; Context
(defmethod #WireFrameParse 1601 ((?x Context)(?k (eq ?k value:))(?v LEXEME)        $?X) (modify-instance ?x (value ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 1602 ((?x Context)(?v LEXEME (not (key-argument-p ?v))) $?X) (modify-instance ?x (value ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; Form

;;; Breadcrumb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #Hook
(defmethod #WireFrameParse 9000 ((?x Hook)) (return ?x))
(defmethod #WireFrameParse 9001 ((?x Hook)(?v Annotation) $?X) (modify-instance ?v (annotation ?x)) (#WireFrameParse ?x (expand$ ?X)))

;;; useProperty
(defmethod #WireFrameParse 9050 ((?x useProperty)(?k (eq ?k   named:))(?v LEXEME)      $?X) (modify-instance ?x (named   ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9051 ((?x useProperty)(?k (eq ?k    type:))(?v LEXEME)      $?X) (modify-instance ?x (type    ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9052 ((?x useProperty)(?k (eq ?k default:))(?v LEXEME)      $?X) (modify-instance ?x (default ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9053 ((?x useProperty)(?v LEXEME (not (key-argument-p ?v))) $?X) 
    (#WireFrameParse ?x named: ?v (expand$ ?X))
    (#WireFrameParse ?x           (expand$ ?X))
)

;;; useScript
(defmethod #WireFrameParse 9060 ((?x useScript)(?k (eq ?k global:))(?v LEXEME) $?X) (modify-instance ?x (global ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; useState
(defmethod #WireFrameParse 9101 ((?x useState)(?k (eq ?k  setter:))(?v LEXEME) $?X) (send ?x put-setter  ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9102 ((?x useState)(?k (eq ?k  getter:))(?v LEXEME) $?X) (send ?x put-getter  ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9103 ((?x useState)(?k (eq ?k default:))(?v LEXEME) $?X) (send ?x put-default ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9104 ((?x useState)(?k (eq ?k default:))(?v NUMBER) $?X) (send ?x put-default ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9105 ((?x useState)(?v LEXEME (not (key-argument-p ?v))) $?X) ;;; 便利性函数
    (#WireFrameParse ?x getter:                                    ?v  )
    (#WireFrameParse ?x setter: (str-cat set (str-make-first-upper ?v)))
    (#WireFrameParse ?x (expand$ ?X))
)

;;; useEffect
(defmethod #WireFrameParse 9201 ((?x useEffect)(?k (eq ?k effect:))(?v     LEXEME)   $?X) (slot-append$ ?x effect       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9202 ((?x useEffect)(?k (eq ?k effect:))(?v Multifield)   $?X) (slot-append$ ?x effect (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9203 ((?x useEffect)(?k (eq ?k  clear:))(?v     LEXEME)   $?X) (slot-append$ ?x clear        ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9204 ((?x useEffect)(?k (eq ?k  clear:))(?v Multifield)   $?X) (slot-append$ ?x clear  (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9205 ((?x useEffect)(?v LEXEME (not (key-argument-p ?v))) $?X) (#WireFrameParse ?x effect: ?v (expand$ ?X))) ;;; 便利性函数

;;; useContext
(defmethod #WireFrameParse 9301 ((?x useContext)(?k (eq ?k context:))(?v LEXEME)      $?X) (send ?x put-context ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9302 ((?x useContext)(?v LEXEME (not (key-argument-p ?v))) $?X) (send ?x put-context ?v) (#WireFrameParse ?x (expand$ ?X)))

;;; useReducer
(defmethod #WireFrameParse 9401 ((?x useReducer)(?k (eq ?k reducer:))(?v LEXEME)      $?X) (send ?x put-reducer ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9402 ((?x useReducer)(?v LEXEME (not (key-argument-p ?v))) $?X) (send ?x put-reducer ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9403 ((?x useReducer)(?k (eq ?k   state:))(?v LEXEME)      $?X) (send ?x put-state   ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9404 ((?x useReducer)(?k (eq ?k   state:))(?v NUMBER)      $?X) (send ?x put-state   ?v) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9405 ((?x useReducer)(?k (eq ?k prepare:))(?v LEXEME)      $?X) (send ?x put-prepare ?v) (#WireFrameParse ?x (expand$ ?X)))

;;; useCallback
(defmethod #WireFrameParse 9501 ((?x useCallback)(?k (eq ?k script:))(?v LEXEME)      $?X) (slot-append$ ?x script       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9502 ((?x useCallback)(?k (eq ?k script:))(?v Multifield)  $?X) (slot-append$ ?x script (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9503 ((?x useCallback)(?k (eq ?k depend:))(?v LEXEME)      $?X) (slot-append$ ?x depend       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9504 ((?x useCallback)(?k (eq ?k depend:))(?v Multifield)  $?X) (slot-append$ ?x depend (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9505 ((?x useCallback)(?v LEXEME (not (key-argument-p ?v)))$?X) (#WireFrameParse ?x script: ?v (expand$ ?X)))

;;; useMemo
(defmethod #WireFrameParse 9601 ((?x useMemo)(?k (eq ?k script:))(?v LEXEME)      $?X) (slot-append$    ?x  script       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9602 ((?x useMemo)(?k (eq ?k script:))(?v Multifield)  $?X) (slot-append$    ?x  script (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9603 ((?x useMemo)(?k (eq ?k depend:))(?v LEXEME)      $?X) (slot-append$    ?x  depend       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9604 ((?x useMemo)(?k (eq ?k depend:))(?v Multifield)  $?X) (slot-append$    ?x  depend (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9605 ((?x useMemo)(?k (eq ?k  named:))(?v LEXEME)      $?X) (modify-instance ?x (named        ?v               )) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9606 ((?x useMemo)(?v LEXEME (not (key-argument-p ?v)))$?X) (#WireFrameParse ?x script: ?v (expand$ ?X)))

;;; useRef
(defmethod #WireFrameParse 9701 ((?x useRef)(?k (eq ?k   named:))(?v LEXEME)     $?X) (modify-instance ?x (named   ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9702 ((?x useRef)(?k (eq ?k default:))(?v LEXEME)     $?X) (modify-instance ?x (default ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 9703 ((?x useRef)(?v LEXEME (not (key-argument-p ?v)))$?X) (#WireFrameParse ?x named: ?v (expand$ ?X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotation
(defmethod #WireFrameParse 10000 ((?x Annotation)) (return ?x))

;;; @Connection
(defmethod #WireFrameParse 10001 ((?x @Connection)(?k (eq ?k script:))(?v LEXEME)     $?X) (slot-append$ ?x script       ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10002 ((?x @Connection)(?k (eq ?k script:))(?v Multifield) $?X) (slot-append$ ?x script (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))

;;; @Grid
(defmethod #WireFrameParse 10100 ((?x @Grid)(?k (eq ?k    row:))(?v INTEGER) $?X) (modify-instance ?x (row    ?v)           ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10101 ((?x @Grid)(?k (eq ?k column:))(?v INTEGER) $?X) (modify-instance ?x (column ?v)           ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10102 ((?x @Grid)(?k (eq ?k    col:))(?v INTEGER) $?X) (modify-instance ?x (column ?v)           ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10103 ((?x @Grid)(?k (eq ?k   span:))(?v INTEGER) $?X) (modify-instance ?x (span   ?v)           ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10104 ((?x @Grid)(?r INTEGER)(?c INTEGER)         $?X) (modify-instance ?x (row    ?r)(column ?c)) (#WireFrameParse ?x (expand$ ?X)))

;;; @Link
(defmethod #WireFrameParse 10200 ((?x @Link)(?v LEXEME (not (key-argument-p ?v))) $?X) (modify-instance ?x (to ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10201 ((?x @Link)(?k (eq ?k to:))(?v LEXEME)           $?X) (modify-instance ?x (to ?v)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10202 ((?x @Link)(?k (eq ?k by:))(?v SYMBOL)           $?X) (modify-instance ?x (by ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; @Reference
(defmethod #WireFrameParse 10300 ((?x @Reference)(?v LEXEME (not (key-argument-p ?v)))  $?X) (modify-instance ?x (reference       ?v                )) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10301 ((?x @Reference)(?k (eq ?k reference:))(?v LEXEME)     $?X) (modify-instance ?x (reference       ?v                )) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10302 ((?x @Reference)(?k (eq ?k reference:))(?v Multifield) $?X) (modify-instance ?x (reference (send ?v get-multifield))) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10303 ((?x @Reference)(?k (eq ?k       ref:))(?v LEXEME)     $?X) (modify-instance ?x (reference       ?v                )) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 10304 ((?x @Reference)(?k (eq ?k       ref:))(?v Multifield) $?X) (modify-instance ?x (reference (send ?v get-multifield))) (#WireFrameParse ?x (expand$ ?X)))

;;; @Script
(defmethod #WireFrameParse 20000 ((?x @Script)(?v LEXEME (not (key-argument-p ?v)))   $?X) (slot-append$    ?x  script         ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 20001 ((?x @Script)(?k (eq ?k      named:))(?v LEXEME)     $?X) (modify-instance ?x (named          ?v               )) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 20002 ((?x @Script)(?k (eq ?k     script:))(?v LEXEME)     $?X) (slot-append$    ?x  script         ?v                ) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 20003 ((?x @Script)(?k (eq ?k     script:))(?v Multifield) $?X) (slot-append$    ?x  script   (send ?v get-multifield)) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 20004 ((?x @Script)(?k (eq ?k   language:))(?v LEXEME)     $?X) (modify-instance ?x (language       ?v               )) (#WireFrameParse ?x (expand$ ?X)))
(defmethod #WireFrameParse 20005 ((?x @Script)(?k (eq ?k javascript:))(?v LEXEME)     $?X) (#WireFrameParse ?x  language: JavaScript script: ?v (expand$ ?X)))
(defmethod #WireFrameParse 20006 ((?x @Script)(?k (eq ?k javascript:))(?v Multifield) $?X) (#WireFrameParse ?x  language: JavaScript script: ?v (expand$ ?X)))

;;; @Eval
;(defmethod #WireFrameParse 20100 ((?x @Eval)(?v LEXEME (not (key-argument-p ?v))) $?X) (#WireFrameParse ?x script: ?v (expand$ ?X)))

;;; @OnEvent
(defmethod #WireFrameParse 20200 ((?x @OnEvent)(?v LEXEME (not (key-argument-p ?v))) $?X) (modify-instance ?x (named ?v)) (#WireFrameParse ?x (expand$ ?X)))

;;; @OnClick
(defmethod #WireFrameParse 20300 ((?x @OnClick)(?v LEXEME (not (key-argument-p ?v))) $?X) (#WireFrameParse ?x script: ?v (expand$ ?X)))

