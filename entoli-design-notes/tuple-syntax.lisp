;; See if #() list pattern could be disambiguated without ambiguity
;; Targets
;; (data #2 (t0 t1) (#2 t0 t1))
;; (data #3 (t0 t1 t2) (#3 t0 t1 t2))
;; ...
;; (data #12 (t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) (#12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11))

;; ;; List expression

;; (: desugar (-> Sugared Desugared))
;; (= desugar 
;;   '() -> Nil
;;   '(x) -> (' x Nil)
;;   '(x xs..) -> (' x (desugar "'({xs..})"))
;; )

;; (: sugar (-> Desugared Sugared))
;; (= sugar
;;   Nil -> '()
;;   (' x Nil) -> '(x)
;;   (' x xs..) -> '(x (sugar xs..))
;; ) 

;; ;; List pattern

;; (: desugar-pattern (-> SugaredListPattern DesugaredListPattern))
;; (= desugar-pattern
;;   '()        -> Nil
;;   '(x)       -> (' x Nil) ;; Including the case x is _
;;   '(x "..")  -> (' x _)
;;   '(h "t..") -> (' h t)
;;   '(x y)     -> (' x (' y Nil))
;;   '(x xs..)  -> (' x (desugar-pattern "'({xs..})"))  
;; )

;; (: sugar-pattern (-> DesugaredListPattern SugaredListPattern))
;; (= sugar-pattern
;;   Nil        -> '()
;;   (' x Nil)  -> '(x)
;;   (' x xs..) -> '(x (sugar-pattern xs..))
;; )

;; Tuple expression

(: desugar-tuple (-> Sugared Desugared))
(= desugar-tuple 
  #(t0 t1) -> (#2 t0 t1)
  #(t0 t1 t2) -> (#3 t0 t1 t2)
  ...
  #(t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) -> (#12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)
)
