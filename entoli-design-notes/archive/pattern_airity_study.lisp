;; ===========================================================
;; LITERAL VALUES
;; ===========================================================

;; Primitive literals
42          ;; Integer literal
3.14        ;; Float literal
"hello"     ;; String literal
'c'         ;; Character literal
True        ;; Boolean literal
False       ;; Boolean literal

;; List and tuple
'(1 2 3)       ;; List literal -> Coverted to '(1 '(2 '(3 '()))) : List Int
#(1 "two" 3.0) ;; Tuple literal -> Converted to (#3 1 "two" 3.0) : #3 Int String Float

;; ===========================================================
;; DATA CREATION & ACCESS
;; ===========================================================

;; Data creation
(Just 42)                           ;; Constructor for (Maybe Int)

;; Lambda function
(lambda x x) ;; Lambda function
(lambda (x y) (+ x y))


;; ===========================================================
;; FUNCTION DEFINITIONS
;; ===========================================================

;; Simple function
(: add (-> Int (-> Int Int)))
(= add (x y) (+ x y))

;; Generic function (universal quantification)
(: id_fn (-> a a))
(= id_fn (x) x)

;; Generic function with a constraint
(: show_twice (-> a String) 
  (where (Show a))
)
(= show_twice x (string-append (show x) (show x)))


;; ===========================================================
;; PATTERN MATCHING
;; ===========================================================

;; Literal pattern
(: is_42 (-> Int Bool))
(= is_42
  (42) True
  (_)  False
)

;; Tuple pattern
(: add_pair (-> (Tuple Int Int) Int))
(= add_pair
  (#(a b)) (+ a b)
)

;; List pattern
(: sum_list (-> '(Int) Int))
(= sum_list
  ('())     0
  ('(x xs)) (+ x (sum_list xs))
)

;; ? List pattern 
(: sum_list2 (-> '(Int) Int))
(= sum_list2
  ('())       0
  ('(x xs..)) (+ x (sum_list2 xs))
)

;; Complex nested list pattern
(: sum_nested (-> '('(Int)) Int))
(= sum_nested
  ('())       0
  ('(xs xss)) (+ (sum_list xs) (sum_nested xss))
)

;; ? Complex nested list pattern
(: sum_nested2 (-> '('(Int)) Int))
(= sum_nested2
  ('())         0
  ('(xs xss..)) (+ (sum_list2 xs) (sum_nested2 xss))
)

;; Nullary vs. unary constructor pattern (exhaustive)
(: is_true (-> Bool String))
(= is_true
  (True)  "It's true"
  (False) "It's false"  ;; parentheses optional for a unary constructor
)

(: is_just (-> (Maybe a) Bool))
(= is_just
  ((Just _)) True
  (_)        False
)

;; Nested pattern
(: process_nested (-> (Maybe '(Int)) Int))
(= process_nested
  ((Just '(x _))) x
  (_)             0
)

;; match expression inside function
(: process_user_data (-> (User '(Int)) Int))
(= process_user_data
  (User (= d data) ..) (match d
    ('(x _)) x
    ('())    0
  )
)


;; ===========================================================
;; TYPES AND DATA DEFINITIONS
;; ===========================================================
;;
;; Below are various data definitions showcasing:
;;   - basic enumerations (Bool, Maybe)
;;   - tuples
;;   - lists
;;   - records
;;   - sum types with multiple constructors (Shape)
;;   - parameterized types (User, Tree)
;;   - record fields, constraints on data, etc.

(data Bool
  (True)
  (False)
)


;; (data (Phantom a))
;; (data (PhantomList (List a)))

;; (data (PhantomList (List a)) (where (NumLike a))
;;   NilLike
;;   (ConsLike a) 
;; )

(data Phantom a)
(data PhantomList ('(a)))

(data PhantomList ('(a)) 
  (where (NumLike a))
  
  NilLike
  ConsLike (a)
)

(data PhantomList
  ;; (List a)
  List (a)
)


(data (Maybe a)
  Nothing
  Just (a)
)

(data Shape
  Circle    (Float)
  Rectangle (Float Float)
  Triangle  (Float Float Float)
)

(data Nat 
  Z
  S (Nat)
)

(: is_two (-> Nat Bool))
(= is_two 
  ((S (S Z))) True
  (_)         False
)

(: add (-> Nat (-> Nat Bool)))
(
  (Z x)     x
  ((S x) y) (S (add x y))
)

;; ? Data with associated function
(impl Shape 
  (: area (-> Shape Float))
  (= area
    ((Circle r))      (* 3.14159 (* r r))
    ((Rectangle w h)) (* w h)
    (_)               0.0
  )
)

(impl Shape 
  (: area (-> Shape Float))
  (= area
    (Circle r)      (* 3.14159 (* r r))
    (Rectangle w h) (* w h)
    _               0.0
  )
)

;; ? Calling associated function
(report_area (-> Shape (Io ())))
(= shape_area
  shape (do
    (print (shape.area ()))
    ;; Desugared: 
    ;; (print (Shape::area shape))
  )
)

(data (Result (a e))
  Ok (a)
  Err (e)
)

(data Tree (a)
  Leaf
  Node (a (Tree a) (Tree a))
)

;; GADT is supported
;; (data (Expr3 a)
;;   (IntExpr  Int)                             (where (== a Int))
;;   (BoolExpr Bool)                            (where (== a Bool))
;;   (ListExpr (Expr3 b) (Expr3 b))             (where (== a '(b)))
;;   (AddExpr  (Expr3 Int) (Expr3 Int))         (where (== a Int))
;;   (IfExpr   (Expr3 b) (Expr3 a) (Expr3 a))   (where (BooleanLike b))
;; )


(data Expr3 (a)
  IntExpr  (Int)                           (where (== a Int))
  BoolExpr (Bool)                          (where (== a Bool))
  ListExpr ((Expr3 b) (Expr3 b))           (where (== a '(b)))
  AddExpr  ((Expr3 Int) (Expr3 Int))       (where (== a Int))
  IfExpr   ((Expr3 b) (Expr3 a) (Expr3 a)) (where (BooleanLike b))
)


;; ===========================================================
;; TRAITS AND IMPLEMENTATIONS
;; ===========================================================
;;
;; The following traits demonstrate:
;;   - a nullary trait (no type parameters),
;;   - a unary trait (Show) with a single type parameter,
;;   - a binary trait (Convertible (a b)) with two type parameters,
;; and
;;   - minimal implementations: simple, generic, and blanket impl
;;     with constraint disambiguation.

;; Nullary trait example: no type parameters
(trait Singleton
  (: unique_instance Singleton)
)

;; Unary trait example
(trait Show (a)
  (: show (-> a String))
)

;; Binary trait example
(trait Convertible (a b)
  (: convert (-> a b))
)

;; Functor trait example
(trait Functor (f)
  (: fmap (-> (-> a b) (f a) (f b)))
)

;; Applicative trait example
(trait Applicative (f) 
  (where (Functor f))

  (: pure (-> a (f a)))
  (: <*>  (-> (f (-> a b)) (f a) (f b)))
)

;; Monad trait example
(trait Monad (m)
  (where (Applicative m))

  (: (>>=) (-> (-> a (m b)) (-> (m a) (m b))))
)

;; Simple implementation for a concrete type
(impl Show (Bool)
  (= show
    (True)  "True"
    (False) "False"
  )
)

;; Generic implementation for a parameterized type
(impl Show ((Maybe a))
  (where (Show a))

  (= show
    (Nothing)  "Nothing"
    ((Just x)) (string-append "Just " (show x))
  )
)

;; Special implementation for a specific type
(impl Show ((Maybe Int))

  (= show
    (Nothing)  "Nothing"
    ((Just x)) (string-append "Just " (show x))
  )
)

;; Blanket implementation with constraints (disambiguation)
(impl Convertible ((Maybe a) (Maybe b)) 
  (where (Convertible a b))

  (= convert
    (Nothing)  Nothing
    ((Just x)) (Just ((Convertible a b)::convert x))  ;; trait disambiguation
  )
)

;; 

;; ===========================================================
;; KIND ANNOTATIONS
;; ===========================================================
;;
;; Kinds describe the "type of a type" or higher-level abstractions:
;;   - (kind List *) means List is a type constructor: * -> *
;;   - (kind Maybe (-> * *)) means Maybe is also * -> *
;;   - (kind Monad (-> (-> * *) Constraint)) means Monad takes a
;;     type constructor as a parameter, returning a Constraint.

(kind List *)
(kind Maybe (-> * *))
(kind Monad (-> (-> * *) Constraint))


;; ===========================================================
;; MONADIC COMPUTATION
;; ===========================================================
;;
;; Demonstrates do-notation for chaining effects:
;;   - `<-` for binding within the monad
;;   - `pure` for lifting a value into the monad
;;   - `throw` or other effects as relevant to the Io or error type

(trait Monad (m)
  (where (Applicative m))

  (: (>>=) (-> (-> a (m b)) (m a) (m b)))
)

(: get_user_data (-> Int (Io (Maybe UserData))))
(= get_user_data
  (id) (do
    (<- conn open_connection)
    (<- result (query_user conn id))
    (pure result)
  )
)

(: process_result (-> (Result a Error) (Io a)))
(= process_result
  ((Ok val)) (pure val)
  ((Err e))  (do
                (<- _ (log_error e))
                (throw e)
              )
)

(: fetch_and_process (-> Int (Io (Result Data Error))))
(= fetch_and_process
  (id) (do
    (<- maybe_user (get_user id))
    (match maybe_user
      Nothing     (pure (Err (Error (= message "User not found"))))
      (Just user) (do
                    (<- result (process_user user))
                    (pure (Ok result))
                  )
    )
  )
)

;; ;; ? Monad method form 
;; (: get_user_data2 (-> Int (Io (Maybe UserData))))
;; (= get_user_data2
;;   id (open_connection.>>= (lambda conn
;;         query_user conn id.>>= (lambda result
;;           pure result
;;         )
;;       )
;;     )
;; )


;; ===========================================================
;; MODULES AND IMPORTS
;; ===========================================================
;;
;; Modules allow grouping definitions and controlling visibility.
;; `(: some_func ...)` lines indicate publicly exported items.
;; Import syntax can include selective imports, aliases, and nested modules.

(mod database
  ;; Exported functions
  (: connect (-> String (Result Connection Error)))
  (: query   (-> Connection (-> String (Result Data Error))))

  ;; Private data type
  (data Connection
    Connection (
      (: url    String)
      (: active Bool)
    )
  )

  (= connect url (make_connection url))
  (= query conn sql (run_query conn sql))
  (= make_connection url (Ok (Connection (= url url) (= active True))))
)

(use database)
(use prelude::*)
(use (collections:: Map Set))
(use math::statistics)
(use (math::
  self
  (statistics:: mean variance)
  linear_algebra::*
))

(mod utils
  (export trim split)
  
  (: trim  (-> String String))
  (= trim 
    (s) (trim_impl s)
  )

  (: split (-> String (-> String '(String))))
  (= split (s sep) (split_impl s sep))

  ;; Private helper functions
  (: trim_impl (-> String String))
  (= trim_impl 
    (s) hole!
  )
)

(: process_data (-> String (Result '(Int) Error)))
(= process_data
  raw_data (do
    (= trimmed (utils::trim raw_data))
    (= parts   (utils::split trimmed ","))
    (parse_integers parts)
  )
)

(mod my_module
  ;; Public API: only symbols listed here become visible outside.
  (export
    MyType
    my_func
  )

  ;; Data definition (private by default)
  (data MyType
    MyCons (Int)
  )
  
  ;; Public function (declared in the export list)
  (: my_func (-> Int Int))
  (= my_func (x) (+ x 1))
  
  ;; Private helper (not exported)
  (: helper (-> Int Int))
  (= helper (x) (* x 2))
)

;; Keyword macro should be reserved for future use

;; 
;; Above is a minimal-yet-comprehensive illustration of this
;; s-expression language, showing:
;;   1) literal & composite values
;;   2) data creation, record updates, and field access
;;   3) function definitions (simple, generic, constrained)
;;   4) pattern matching (literal, tuple, list, record, guards)
;;   5) type and data definitions (including parameterized)
;;   6) minimal traits & implementations, trait disambiguation
;;   7) kind annotations
;;   8) monadic do-notation
;;   9) module system & various import styles
;;
;; End of example.

;; ===========================================================
;; todo

;; todo macro syntax

;; todo Record syntax
;; (Person 
;;   (= name "Alice") 
;;   (= age 30)
;; ) ;; Record construction
;; (Person 
;;   (= age 30) 
;;   (.. old_person)
;; )  ;; Record update (copy old_person + override fields)

;; ! Directly leads to partial function
;; ;; Record field access
;; (person.age)    ;; Field access via postfix dot
;; (.name person)  ;; Field access via prefix dot


;; todo Record syntax
;; ;; Record pattern with guard
;; (: is_adult (-> Person Bool))
;; (= is_adult
;;   (Person _ (= a age) ..) (>= a 18)
;; )

;; (: classify_age (-> Person String))
;; (= classify_age
;;   (Person (= a age) ..) (where (< a 13))      "Child"
;;   (Person (= a age) ..) (where (< a 20))      "Teenager"
;;   (Person (= a age) ..) (where (< a 65) True) "Adult"
;;   _                                           "Senior"
;; )

;; ===========================================================
;; FIELD ACCESS & RECORD OPERATIONS
;; ===========================================================

;; (: get_age (-> Person Int))
;; (= get_age p p.age)

;; Block body example (illustrates local binding with (= ...))
;; (: generation (-> Person String))
;; (= generation
;;   person (
;;     (= person_age person.age) ;; local binding; block starts
;;     (if (< person_age 13)
;;       "Child"
;;     (if (< person_age 20)
;;       "Teenager"
;;     (if (< person_age 65) "Adult" "Senior")))
;;   )
;; )

;; (: make_person (-> String (-> Int Person)))
;; (= make_person
;;   (n a) (Person
;;     (= name n)
;;     (= age  a)
;;     (= email (append nm "@example.com"))
;;   )
;; )

;; ;; Record pattern with wildcard capture
;; (: person_age (-> Person Int))
;; (= person_age
;;   (Person (= a age) ..) a
;; )

;; (: increment_age (-> Person Person))
;; (= increment_age
;;   person (Person
;;     (= age (+ person.age 1))
;;     (.. person)
;;   )
;; )


;; todo Record syntax
(data Person
  Person (
    (: name  String)
    (: age   Int)
    (: email String)
  )
)

;; todo Record syntax
(data (User a (where (Show a)))
  User (
    (: id       Int)
    (: username String)
    (: active   Bool)
    (: data     a)
  )
)


;; todo
;; ===========================================================
;; Data and Type Constructor Synnonym
;; ===========================================================
;;
;; Demonstrates type constructor synonyms and data constructor synonyms
;;   - `type` keyword for type constructor synonyms
;;   - `data` keyword for data constructor synonyms

;; If Ident start with upper case, it should be Constructor binding
;; (= (Array a) '(a))
;; (= (ConvertiblePair a b) #(a b) (where (Convertible a b)))


;; Type level (value, function) binding
;; (kind Name *)
(type Name String)

;; (kind Array (-> * *))
(type Array a '(a))

;; (kind ConvertiblePair (-> * (-> * *)))
(type ConvertiblePair 
  (a b) (where (Convertible a b)) #(a b)
)

;; (kind If (-> * (-> * (-> * *))))
(type If 
  ((True a _)) a
  ((False _ b)) b
)

;; (kind IsInt (-> * Bool))
(type IsInt a (If (~ a Int) True False))
(type IsInt2 
  (Int) True
  (_)   False
)
(type IntList 
  (a) (where (IsInt a)) '(a)
  ;; Type level partial function could lead to compile failure
)

;; What about type level recursive function?
;; (kind Fib (-> * *))
(type Fib
  (Z)     Z
  ((S Z)) (S Z)
  ((S a)) (Add (Fib (S a)) (Fib (S (S a))))
)

