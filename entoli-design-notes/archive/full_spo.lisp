;; ! WIP with second-place-operator-rule
;; How ever, "_" and ".." migth be spared from this rule

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
;; Lambda function's airity is not known
(lambda x x) ;; Lambda function
(lambda (x y) (x + y))

;; Kind annotation might work for all the known 


;; ===========================================================
;; FUNCTION DEFINITIONS
;; ===========================================================

;; Simple function
(add : (Int -> (Int -> Int))) ;; Airity of add is 2
(add = (x y) (x + y))         ;; Parse pattern as binary pattern list

;; Generic function (universal quantification)
(id_fn : (a -> a)) ;; Airity of id_fn is 1
(id_fn = x x)      ;; Parse pattern as unary pattern list

;; Generic function with a constraint
(show_twice : (a -> String) 
  (where (Show a))
)
(show_twice = x (string-append (show x) (show x)))


;; ===========================================================
;; PATTERN MATCHING
;; ===========================================================

;; Literal pattern
(is_42 : (Int -> Bool))
(is_42 =
  42 True
  _  False
)

;; Tuple pattern
(add_pair : (#(Int Int) -> Int))
(add_pair =
  #(a b) (a + b)
)

;; List pattern
(sum_list : ('(Int) -> Int))
(sum_list =
  '()     0
  '(x xs) (x + (sum_list xs))
)

;; ? List pattern 
(sum_list2 : ('(Int) -> Int))
(sum_list2 =
  '()     0
  '(x xs) (x + (sum_list2 xs))
)

;; Complex nested list pattern
(sum_nested : ('('(Int)) -> Int))
(sum_nested =
  '()       0
  '(xs xss) ((sum_list xs) + (sum_nested xss))
)

;; ? Complex nested list pattern
(sum_nested2 : ('('(Int)) -> Int))
(sum_nested2 =
  '()        0
  '(xs xss) ((sum_list2 xs) + (sum_nested2 xss))
)

;; Nullary vs. unary constructor pattern (exhaustive)
(is_true : (Bool -> String))
(is_true =
  True  "It's true"
  False "It's false"  ;; parentheses optional for a unary constructor
)

(is_just : ((Maybe a) -> Bool))
(is_just =
  (Just _) True
  _        False
)

;; Nested pattern
(process_nested : ((Maybe '(Int)) -> Int))
(process_nested =
  (Just '(x _)) x
  _             0
)

;; match expression inside function
(process_user_data : ((User '(Int)) -> Int))
(process_user_data =
  (User ('(x _) = data) ..) x ;; All the other fields are expanded with the field name
  (User ('() = data) ..)    0
  ;; Is the same with
  ;; (User (d = data) ..) (match d
  ;;   '(x _) x
  ;;   '()    0
  ;; )
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

;; If don't use parenthesis, can't be parsed.
;; Can't tell next constructor and type parameter
(type Bool
  True  
  False 
)

(type (Option a)
  Nothing
  (Just a)
)
;; Explicitly disambiguated as (type (forall a (Option a)))

(type (Either a b) 
  (Left a)
  (Right b)
)
;; Explicitly disambiguated as (type (forall a b (Either a b)))

(type (Phantom a))
;; is actually disambiguated as (type (forall a (Phantom a)))
(type (PhantomList '(a)))
;; is actually disambiguated as (type (forall a (PhantomList '(a))))

;; ! Problem is that this mb-pattern is zero, or one or two s-expression
;; ! But it is hard to tell if what comes next is a data constructor or a type parameter
;; ! So need to make constructor declaration as a single pattern.
;; ! Where nullary parenthesis skip is applied.
(type (PhantomList '(a)) 
  (where (NumLike a))

  NilLike
  (ConsLike a)
)

(type PhantomLen
  (Len Int)
)


(type (Maybe a) 
  Nothing
  (Just a)
)

(type Shape
  (Circle Float)
  (Rectangle Float Float)
  (Triangle Float Float Float)
)

(type Nat 
  Z
  (S Nat)
)

(is_two : (Nat -> Bool))
(is_two = 
  (S (S Z)) True
  _         False
)

(add : (Nat -> (Nat -> Nat)))
(add =
  (Z x)     x
  ((S x) y) (S (add x y))
)

;; ? Data with associated function
(impl Shape 
  (area : (Shape -> Float))
  (area =
    (Circle r)      (3.14159 * (r * r))
    (Rectangle w h) (w * h)
    _               0.0
  )
)

(impl Shape 
  (area : (Shape -> Float))
  (area =
    (Circle r)      (3.14159 * (r * r))
    (Rectangle w h) (w * h)
    _               0.0
  )
)

;; ? Calling associated function
(shape_area : (Shape -> (Io ())))
(shape_area =
  shape (do
    (print (shape .(area)))
    ;; Desugared: 
    ;; (print (Shape::area shape))
  )
)

(type (Result a e)
  (Ok a)
  (Err e)
)

;; Compiler directive expressions
@(cfg debug)
@(derive PartialEq (Convertible '(a) (Vec a)))
(type '(a)
  Nil
  (Cons a '(a))
)

(type (Tree a)
  Leaf
  (Node a (Tree a) (Tree a))
)

;; GADT is supported
(type (Expr3 a)
  (IntExpr  Int)                           (where (a == Int))
  (BoolExpr Bool)                          (where (a == Bool))
  (ListExpr (Expr3 b) (Expr3 b))           (where (a == '(b)))
  ;; is disambiguated as (forall b (ListExpr (Expr3 b) (Expr3 b))) which is existential type
  (AddExpr  (Expr3 Int) (Expr3 Int))       (where (a == Int))
  (IfExpr   (Expr3 b) (Expr3 a) (Expr3 a)) (where (BooleanLike b))
  ;; is disambiguated as (forall b (IfExpr (Expr3 b) (Expr3 a) (Expr3 a)))
)

;; ? So can I assume that all the constructor's airity is know
;; ! It seems not, since there could be coinductive type 
;; In order to resolve coinduction, the airity of constructor should be known
;; Now the airity of Expr3 is known to be 1 or non zero
;; Therefore, trying to parsing Expr3 as unary pat will fail.
;; Therefore, try to parse whole list as a unary pattern list.


(Wrapper : ((* -> *) -> *))
(type (Wrapper f)
  (Wrapper (f Int))
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
  (unique_instance : Singleton)
)

;; Unary trait example
(trait (Show a)
  (show : (a -> String))
)

;; Binary trait example
(trait (Convertible a b)
  (convert : (a -> b))
)


;; Functor trait example
(trait (Functor f)
  (fmap : ((f a) -> ((a -> b) -> (f b)))) ;; Method friendly signature
)

;; Applicative trait example
(trait (Applicative f)
  (where (Functor f))

  (pure : (a -> (f a)))
  (ap   : ((f a) -> ((f (a -> b)) -> (f b)))) ;; Method friendly signature
)


;; Monad trait example
(trait (Monad m)
  (where (Applicative m))

  (>>= : ((m a) -> ((a -> (m b)) -> (m b)))) ;; Method friendly signature
)

;; Simple implementation for a concrete type
(impl Show Bool
  (show =
    True  "True"
    False "False"
  )
)

;; Generic implementation for a parameterized type
(impl Show (Maybe a)
  (where (Show a))

  (show =
    Nothing  "Nothing"
    (Just x) (string-append "Just " (show x))
  )
)

;; Special implementation for a specific type
(impl Show (Maybe Int)

  (show =
    Nothing  "Nothing"
    (Just x) (string-append "Just " (show x))
  )
)

;; Blanket implementation with constraints (disambiguation)
(impl Convertible ((Maybe a) (Maybe b)) 
  (where (Convertible a b))

  (convert =
    Nothing  Nothing
    (Just x) (Just ((Convertible a b)::convert x))  ;; trait disambiguation
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
(kind Maybe (* -> *))
(kind Monad ((* -> *) -> Constraint))


;; ===========================================================
;; MONADIC COMPUTATION
;; ===========================================================
;;
;; Demonstrates do-notation for chaining effects:
;;   - `<-` for binding within the monad
;;   - `pure` for lifting a value into the monad
;;   - `throw` or other effects as relevant to the Io or error type

(get_user_data : (Int -> (Io (Maybe UserData))))
(get_user_data =
  id (do
    (conn   <- open_connection)
    (result <- (query_user conn id))
    (pure result)
  )
)

(process_result : ((Result a Error) -> (Io a)))
(process_result =
  (Ok val) (pure val)
  (Err e)  (do
    (_ <- (log_error e))
    (Exception e)
  )
)

(fetch_and_process : (Int -> (Io (Result Data Error))))
(fetch_and_process =
  id (do
    (maybe_user <- (get_user id))
    (match maybe_user
      Nothing (
        (message = (format! "User {} not found" id))
        (pure (Err (Error message)))
      )
      (Just user) (do
        (result <- (process_user user))
        (pure (Ok result))
      )
    )
  )
)

;; ;; ? Monad method form 
;; (get_user_data2 : (Int -> (Io (Maybe UserData))))
;; (get_user_data2 =
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
  ;; Exported items
  (pub Connection connect query)

  ;; Private data type
  (type Connection
    (Connection 
      (name   : String)
      (active : Bool)
    )
  )

  (connect : (String -> (Result Connection Error)))
  (connect = url (make_connection url))

  (query   : (Connection -> (String -> (Result Data Error))))
  (query = (conn sql) (run_query conn sql))

  (make_connection : (String -> (Result Connection Error)))
  (make_connection = url (Ok (Connection (url = url) (active = True))))
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
  (use (prelude:: String) 
       (collections:: List))

  (pub trim split)
  
  (trim : (String -> String))
  (trim = 
    s (trim_impl s)
  ) 

  (split : (String -> (String -> '(String))))
  (split = (s sep) (split_impl s sep))

  ;; Private helper functions
  (trim_impl : (String -> String))
  (trim_impl = 
    s hole!
  )
)

(process_data : (String -> (Result '(Int) Error)))
(process_data =
  raw_data (do
    (trimmed = (utils::trim raw_data))
    (parts = (utils::split trimmed ","))
    (parse_integers parts)
  )
)

(mod my_module
  ;; Public API: only symbols listed here become visible outside.
  (pub
    MyType
    my_func
  )

  ;; Data definition (private by default)
  (type MyType
    (MyCons Int)
  )

  ;; Directly import module from path
  @(mod some_submodule (Path `("path" "to" "some_submodule")))
  (use some_submodule)
  
  ;; Public function (declared in the export list)
  (pub my_func)
  (my_func : (Int -> Int))
  (my_func = x (x + 1))

  ;; Private helper (not exported)
  (helper : (Int -> Int))
  (helper = x (x * 2))
)

;; Record syntax
(type Person
  (Person 
    (name  : String)
    (age   : Int)
    (email : String)
  )
)

;; Record syntax
(type (User a) (where (Show a))
  (User 
    (id       : Int)
    (username : String)
    (active   : Bool)
    (data     : a)
  )
)

;; Type level (value, function) binding
(name : Type0)
(name = String)

(alias ConvertiblePair 
  (a b) (where (Convertible a b)) #(a b)
)

(alias If 
  (True a _) a
  (False _ b) b
)

(alias IsInt a (If (a ~ Int) True False))
(alias IsInt2 
  Int True
  _   False
)
(alias IntList 
  a (where (IsInt a)) '(a)
  ;; Type level partial function could lead to compile failure
)

(alias Fib
  Z     Z
  (S Z) (S Z)
  (S a) ((Fib a) Add (Fib (S a)))
)


;; #[derive(Debug, PartialEq, Clone)]
;; enum TypeExpr {
;;     Var(Ident),
;;     Ctor {
;;         // Constructor would be either a constructor or a synonym
;;         ident: IdPath,
;;         args: Vec<TypeExpr>,
;;     },
;;     ForAll {
;;         vars: Vec<Ident>,
;;         body: Ptr<TypeExpr>,
;;     },
;; }

@(derive Debug PartialEq Clone)
(type TypeExpr
  (Var Ident)
  (Ctor 
    ;; Constructor would be either a constructor or a synonym
    (ident : IdPath) 
    (args  : '(TypeExpr))
  )
  (ForAll 
    (vars : '(Ident)) 
    (body : (Ptr TypeExpr))
  )
)