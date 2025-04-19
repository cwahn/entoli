;; See if '() list pattern could be disambiguated without ambiguity
;; Target
;; (data List a
;;   Nil             ;; Nil
;;   (' a (List a))  ;; Cons
;; )

;; List expression

(: desugar (Sugared -> Desugared))
(= desugar 
  '()       Nil
  '(x)      (' x Nil)
  '(x xs..) (' x (desugar "'({xs..})"))
)

(: sugar (Desugared -> Sugared))
(= sugar
  (Nil)      '()
  (' x Nil)  '(x)
  (' x xs..) '(x (sugar xs..))
) 

;; List pattern

(: desugar-pattern (SugaredListPattern -> DesugaredListPattern))
(= desugar-pattern
  '()         Nil
  '(x)        (' x Nil) ;; Including the case x is _
  '(x "..")   (' x _)
  '(h "t..")  (' h t)
  '(x y)      (' x (' y Nil)) ;; Including the case y is _ (wildcard)
  '(x xs..)   (' x (desugar-pattern "'({xs..})"))  
)

(: sugar-pattern (DesugaredListPattern -> SugaredListPattern))
(= sugar-pattern
  (Nil)        '()
  (' x Nil)  '(x)
  (' x xs..) '(x (sugar-pattern xs..))
)
