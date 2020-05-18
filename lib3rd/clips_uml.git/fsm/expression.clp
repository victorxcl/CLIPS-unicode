;;; vim:expandtab autoindent

(defgeneric state)
(defgeneric  is-current-state)
(defgeneric set-current-state)

(defgeneric define-trigger)
(defgeneric define-guard)
(defgeneric define-action)

(defgeneric invoke-trigger)
(defgeneric invoke-guard)
(defgeneric invoke-action)

(defgeneric detect-trigger)

(defclass Expression#FSM#State  (is-a Expression) 
    (slot state (type SYMBOL))
)
(defclass Expression#FSM#IsCurrentState  (is-a Expression) 
    (slot state (type SYMBOL))
)
(defclass Expression#FSM#SetCurrentState (is-a Expression) 
    (slot state (type SYMBOL))
)
(defclass Expression#FSM#Define (is-a Expression)
    (     slot kind (type SYMBOL)(allowed-values Guard Action Trigger))
    (     slot member)
    (multislot arguments)
)
(defclass Expression#FSM#Invoke (is-a Expression)
    (     slot kind (type SYMBOL)(allowed-values Guard Action Trigger))
    (multislot keypath)
    (     slot member)
    (multislot arguments)
)
(defclass Expression#FSM#Detect (is-a Expression)
    (     slot kind (type SYMBOL)(allowed-values Guard Action Trigger))
    (     slot member)
    (multislot arguments)
)

(defmethod Expression#FSM#Define ((?kind SYMBOL)(?member (lexemep ?member)) $?arguments)
    (make-instance of Expression#FSM#Define (kind ?kind)(member ?member)(arguments ?arguments))
)
(defmethod Expression#FSM#Invoke ((?kind SYMBOL)(?member (lexemep ?member)) $?arguments)
    (Expression#FSM#Invoke ?kind (make-instance of Expression#KeyPath) ?member ?arguments)
)
(defmethod Expression#FSM#Invoke ((?kind SYMBOL)(?keypath Expression#KeyPath) (?member (lexemep ?member)) $?arguments)
    (make-instance of Expression#FSM#Invoke (kind ?kind)(keypath ?keypath)(member ?member)(arguments ?arguments))
)
(defmethod Expression#FSM#Detect ((?kind SYMBOL) (?member (lexemep ?member)) $?arguments) 
    (make-instance of Expression#FSM#Detect (kind ?kind)(member ?member)(arguments ?arguments))
)

(defmethod Expression#FSM#IsCurrentState ((?state (lexemep ?state)))
    (make-instance of Expression#FSM#IsCurrentState (state ?state))
)
(defmethod Expression#FSM#SetCurrentState ((?state (lexemep ?state)))
    (make-instance of Expression#FSM#SetCurrentState (state ?state))
)

(defmethod  is-current-state ($?arguments) (Expression#FSM#IsCurrentState  (expand$ ?arguments)))
(defmethod set-current-state ($?arguments) (Expression#FSM#SetCurrentState (expand$ ?arguments)))


;;; the methods below will be deprecated
(defmethod define-guard   ($?arguments) (Expression#FSM#Define Guard   (expand$ ?arguments)))
(defmethod define-action  ($?arguments) (Expression#FSM#Define Action  (expand$ ?arguments)))
(defmethod define-trigger ($?arguments) (Expression#FSM#Define Trigger (expand$ ?arguments)))

(defmethod invoke-guard   ($?arguments) (Expression#FSM#Invoke Guard   (expand$ ?arguments)))
(defmethod invoke-action  ($?arguments) (Expression#FSM#Invoke Action  (expand$ ?arguments)))
(defmethod invoke-trigger ($?arguments) (Expression#FSM#Invoke Trigger (expand$ ?arguments)))

(defmethod detect-trigger ($?arguments) (Expression#FSM#Detect Trigger (expand$ ?arguments)))

(defmethod state (?name) (make-instance of Expression#FSM#State (state ?name)))

