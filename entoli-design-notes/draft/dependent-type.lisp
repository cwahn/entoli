
;; ===========================================================
;; UNIVERSES
;; ===========================================================
(Type0 : Type1)
(Type1 : Type2) ; ... (cumulative universes)

(Type = Type1)

;; ===========================================================
;; UNIT (formation / intro)
;; ===========================================================
(1 : Type0)

(() : 1) ; () is the only element of type 1

;; ===========================================================
;; SUM TYPE (A + B)  — formation / intro
;; ===========================================================
((|) : (Type0 -> (Type0 -> Type0)); (A | B) : Type0
    (Inj0 : (A -> (A | B)))
    (Inj1 : (B -> (A | B)))
)        

;;? Pi type
(data (Vec a n)
    (Nil :  (Vec a 0))
    (Cons : (a -> ((Vec a n) -> (Vec a (n + 1)))))
)

;; (Vec : (Type -> Nat -> Type))
;; (Vec = 
;;     Nil  (Vec a 0)
;;     Cons (a -> (Vec a n) -> (Vec a (n + 1)))
;; )

;; ;; Sigma type


