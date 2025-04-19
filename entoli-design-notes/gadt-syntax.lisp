;; GADT: Opt1
(data (Expr a)
  (: IntExpr  (Int -> (Expr Int)))
  (: BoolExpr (Bool -> (Expr Bool)))
  (: ListExpr (b -> (b -> (Expr a))) (where (a == '(b))))
  (: AddExpr  ((Expr -> Int) ((Expr -> Int) (Expr Int))))
  (: IfExpr   ((Expr -> b) ((Expr -> a) ((Expr -> a) (Expr a))))
    (where (BooleanLike b))
  )
)

;; GADT: Opt2
(data (Expr2 a)
  (IntExpr  Int                                                     (Expr2 Int))
  (BoolExpr Bool                                                    (Expr2 Bool))
  (ListExpr ((Expr2 b) (Expr2 b))           (where (a == '(b)))     (Expr2 a))
  (AddExpr  ((Expr2 Int) (Expr2 Int))                               (Expr2 Int))
  (IfExpr   ((Expr2 b) (Expr2 a) (Expr2 a)) (where (BooleanLike b)) (Expr2 a))
)

;; GADT: Opt3
(data (Expr3 a)
  (IntExpr  Int                             (where (a == Int)))
  (BoolExpr Bool                            (where (a == Bool)))
  (ListExpr ((Expr3 b) (Expr3 b))           (where (a == '(b))))
  (AddExpr  ((Expr3 Int) (Expr3 Int))       (where (a == Int)))
  (IfExpr   ((Expr3 b) (Expr3 a) (Expr3 a)) (where (BooleanLike b)))
)

;; GADT: Opt4
(data (Expr4 a)
  (IntExpr Int)                          (where (a == Int))
  (BoolExpr Bool)                        (where (a == Bool))
  (ListExpr (Expr3 b) (Expr3 b))         (where (a == '(b)))
  (AddExpr (Expr3 Int) (Expr3 Int))      (where (a == Int))
  (IfExpr (Expr3 b) (Expr3 a) (Expr3 a)) (where (BooleanLike b))
)

;; ! Some of the variable in pattern might be bound
;; Maybe related with non-linear pattern matching