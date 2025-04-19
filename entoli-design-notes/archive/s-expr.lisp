;; pub enum SExpr<'a> {
;;     Keyword(Keyword<'a>),
;;     Literal(LiteralExpr<'a>),
;;     Id(&'a str),
;;     List(Vec<SExpr<'a>>), // Non-empty list
;; }

(data SExpr
  (Keyword Keyword)
  (Literal LiteralExpr)
  (Id      String)
  (List    '(SExpr))
)

;; pub enum Keyword<'a> {
;;     Quote(&'a str),  // "'"
;;     Lambda(&'a str), // "lambda"
;;     Match(&'a str),  // "match"

;;     Arrow(&'a str),     // "->"
;;     Binding(&'a str),   // "="
;;     TypeAnnot(&'a str), // ":"
;;     Where(&'a str),     // "where"
;;     Wildcard(&'a str),  // "_"

;;     Trait(&'a str), // "trait"
;;     Data(&'a str),  // "data"
;;     Impl(&'a str),  // "impl"
;;     Type(&'a str),  // "type"

;;     KindAnnot(&'a str), // "kind"
;;     Star(&'a str),      // "*"

;;     Mod(&'a str), // "mod"
;;     Use(&'a str), // "use"

;;     Do(&'a str),        // "do"
;;     LeftArrow(&'a str), // "<-"
;; }

(data Keyword
  (Quote     String)
  (Lambda    String)  
  (Match     String)
 
  (Arrow     String)
  (Binding   String)
  (TypeAnnot String)
  (Where     String)
  (Wildcard  String)

  (Trait     String)
  (Data      String)
  (Impl      String)
  (Type      String)
 
  (KindAnnot String)
  (Star      String)

  (Mod       String)
  (Use       String)

  (Do        String)
  (LeftArrow String)
)

;; pub enum LiteralExpr<'a> {
;;     Unit(&'a str), // "()" Unit could be both type and value.
;;     Bool(&'a str), // "True" or "False"
;;     Int(&'a str),
;;     String(&'a str),
;; }

;; ! WIP
;; (data LiteralExpr
;;   (Unit   String)
;;   (Bool   String)
;;   (Int    String)
;;   (String String)
;; )

;; (: derive_eq (-> SExpr SExpr))
;; (= derive_eq s_expr
;;   (do
;;     (= ast (Lang::parse s_expr))
;;     (match ast
;;       (List '((Keyword (Data _)) (Id id) (List data_ctors))) (
;;         (= derived_eq ( List '(
;;           (Keyword (Impl "impl"))
;;           (Id id)
;;           (List '(
;;             (List '(
;;               (Keyword (Binding "="))
;;               (Id "==")
;;               (Id "lhs")
;;               (Id "rhs")
;;               (
;;                   ...
;;               )
;;             ))
;;           ))
;;         )))
;;       )
;;     )
;;   )
;; )

;; Avoid nesting depth by using local bindings
(macro derive_eq s_expr
  (do
    (= ast (Lang::parse s_expr))
    (match ast
      (List '((Keyword (Data _)) (Id id) (List data_ctors))) (
        (= eq_body (List '(
          (Keyword (Binding "="))
          (Id "==")
          (Id "lhs")
          (Id "rhs")
          (
              ...
          )
        ))

        (= impl_item (List '(
          (Keyword (Impl "impl"))
          (Id id)
          (List '(
            eq_body
          ))
        )))

        (= derived_eq (List '(
          (Keyword (Impl "impl"))
          (Id "Eq")
          (Id id)
          impl_item
        )))

        (++ s_expr derived_eq)
      ))
    )
  )
)

