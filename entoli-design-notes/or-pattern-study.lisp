;; ;; todo Or pattern

(: is_bool_literal (Bool -> Bool))
(= is_bool_literal
  ("true" | "false") true
  _                  false
)

(: is_single_digit (Int -> Bool))
(= is_single_digit
  (| 0 1 2 3 4 5 6 7 8 9)                                  true ; Using variadic or operator
  (0 | (1 | (2 | (3 | (4 | (5 | (6 | (7 | (8 | 9)))))))))  true ; Use regular SPO syntax
  _                                                        false
)

;; If it has to be fall into s-expression, it has to be either parentheses or special parentheses
;; If one allows multiple airity arithmetic operators, it might works.


;; Let's thing about example of SExpression

(data SExpr 
    Nil
    KwLet
    KwTypeAnnot
    (List '(SExpr)) ;; List
    (Ident String) ;; Ident
    (Int Int) ;; Int
)

;; A function to check if given s-expression is a stmt.
;; A stmt is either a list starting with a keyword let or type-annot

(: is_stmt (SExpr -> Bool))
(= is_stmt
  (List '(KwLet s_exprs..)) true
  (List '(KwTypeAnnot s_exprs..)) true
  _ false
)

;; How to make this a or pattern
(: is_stmt_2 (SExpr -> Bool))
(= is_stmt_2
  (List '((KwLet | (KwTypeAnnot | kwSomeOther)) s_exprs..)) true
  _ false
)

;; Looks better but what about the case more that three?
;; Apperantly less useful.
;; if I has to allow (KwLet | KwTypeAnnot | kwSomeOther | kwSomeOther2), there has to be concept of operator associativity
;; Since (A -> B -> C) is should be desugared to (A -> (B -> C))
;; ,but (A + B + C) is desugared to ((A + B) + C)

;; (: is_single_digit (Int -> Bool))
;; (= is_single_digit
;;   (0 
;;   | (1 
;;   | (2 
;;   | (3 
;;   | (4 
;;   | (5 
;;   | (6 
;;   | (7 
;;   | (8 
;;   | 9)
;;   ))))))))  true ; Use regular SPO syntax
;;   _             false
;; )

;; Without using SPO syntax
;; (: is_single_digit (Int -> Bool))
;; (= is_single_digit
;;   (| 0
;;   (| 1
;;   (| 2
;;   (| 3
;;   (| 4
;;   (| 5
;;   (| 6
;;   (| 7
;;   (| 8
;;   (| 9)))))))))) true ; Use regular SPO syntax
;;   _              false
;; )

;; ;; Variadic or operator
;; (: is_single_digit (Int -> Bool))
;; (= is_single_digit
;;   (| 0 1 2 3 4 5 6 7 8 9)  true ; Using variadic or operator
;;   _                        false
;; )
;; This is too much, it will never get used on lambda and trait implementation as well.
