;; ===========================================================
;; SECOND PLACE OPERATOR (SPO) SYNTAX
;; ===========================================================
;;
;; This language provides a syntactic sugar called the "Second Place Operator" (SPO)
;; rule which allows for more intuitive expression of operations while maintaining
;; S-expression structure.

;; When a special character-only identifier appears in the second position of an
;; S-expression list, the lexer automatically moves it to the prefix position.
;; This applies to any built-in or user-defined identifiers except keywords like
;; `:`, `=`, or placeholders like `_` and `..`.

;; Whenever need to prevent SPO desugaring, wrap the operator in parentheses

;; ===========================================================
;; BASIC SPO USAGE
;; ===========================================================

;; Standard S-expression form (prefix notation)
(: + (-> Int (-> Int Int))) 
(+ 3 4)                       
(+ 3 4)                       

;; Using SPO (more intuitive infix-like syntax)
(: (+) (Int -> (Int -> Int))) ; Parentheses are required to prevent SPO desugaring, type expr desugars to: (-> Int (-> Int Int))
(3 + 4)                       ; Desugars to: (+ 3 4)
(x * y)                       ; Desugars to: (* x y)

;; SPO with complex expressions
((f x) + (g y))     ; Desugars to: (+ (f x) (g y))



;; ===========================================================
;; PREVENTING SPO DESUGARING
;; ===========================================================

;; To prevent SPO desugaring, wrap the operator in parentheses
;; This is particularly useful for partial application of binary functions

;; Define a partial application function
(: partial-left ((a -> (b -> c)) -> a -> (b -> c)))
(= partial-left
  f x (lambda y (f x y))
)

;; Using partial-left with + operator
(: add-five (Int -> Int))
(= add-five
  (partial-left + 5)  ;; Without parentheses, this would desugar incorrectly to (+ partial-left 5)
)

;; Alternative approach with parentheses around the operator
(: add-five2 (Int -> Int))
(= add-five2
  (partial-left (+) 5)  ;; Explicitly prevents SPO desugaring
)

;; Using the partial function
(add-five 10)        ;; Result: 15

;; ===========================================================
;; DESUGARING RULES
;; ===========================================================
;;
;; The SPO rule follows these simple transformation steps:
;;
;; 1. Identify S-expressions of the form: (a op b ...)
;;    where op consists only of special characters
;;
;; 2. Transform to: (op a b ...)
;;
;; 3. Exception: do not apply when op is a keyword (`:`, `=`) or a
;;    placeholder (`_`, `..`)
;;
;; 4. Exception: do not apply when op is wrapped in parentheses
;;
;; The SPO transformation happens during the parsing phase, before
;; any other desugaring rules are applied.

;; ? What about use of * in use statement and kind annotation? 

(use some_mod::*) ; This should not desugar to (some_mod::* use)
(kind SomeIdent *) ; Star does not comes in second place

;; Maybe just don't use * in the operator position.
(use some_mod::..)
(use some_mod::(sub_mod another_mod another_mod::.. SomeType)) ; This is rather uncommon, so not implement another keyword self