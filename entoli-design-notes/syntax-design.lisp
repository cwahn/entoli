;; ===========================================================
;; LITERAL VALUES
;; ===========================================================

;; Primitive literals
42          ; Integer literal
3.14        ; Float literal
"hello"     ; String literal
'c'         ; Character literal
True        ; Boolean literal
False       ; Boolean literal

;; List and tuple
'(1 2 3)       ; List literal -> Coverted to '(1 '(2 '(3 '()))) : List Int
#(1 "two" 3.0) ; Tuple literal -> Converted to (#3 1 "two" 3.0) : #3 Int String Float

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


;; Refer the separte document for detail


;; ===========================================================
;; DATA CREATION & ACCESS
;; ===========================================================

;; Data creation
(Just 42)       ; Constructor for (Maybe Int)

;; Lambda function
;; Lambda function's airity is not known
(lambda x x)            ; Lambda function
(lambda (x y) (x + y))

;; Kind annotation might work for all the known 


;; ===========================================================
;; FUNCTION DEFINITIONS
;; ===========================================================

;; Simple function
(: add (Int -> (Int -> Int))) ; Airity of add is 2
(= add (x y) (x + y))         ; Parse pattern as binary pattern list

;; Generic function (universal quantification)
(: id_fn (a -> a)) ; Airity of id_fn is 1
(= id_fn x x)      ; Parse pattern as unary pattern list

;; Generic function with a constraint
(: show_twice (a -> String) 
  (where (Show a))
)
(= show_twice x (string-append (show x) (show x)))


;; ===========================================================
;; PATTERN MATCHING
;; ===========================================================

;; Literal pattern
(: is_42 (Int -> Bool))
(= is_42
  42 True
  _  False
)

;; Tuple pattern
(: add_pair (#(Int Int) -> Int))
(= add_pair
  #(a b) (a + b)        ; Desugars to: (#2 a b), where type of a and b is Int
)

;; List pattern
(: sum_list ('(Int) -> Int))
(= sum_list
  '()     0
  '(x xs) (x + (sum_list xs))
)

(: mb_first_two ('(Int) -> (Maybe '(Int))))
(= mb_first_two
  '()        Nothing               ; Desugars to Nil
  '(x)       '(x)                  ; Desugars to (' x Nil), where type of x is Int
  '(x y)     (Just '(x y))         ; Desugars to (' x (' y Nil)), where type of x and y is Int 
  '(x xs..)  (Just '(x (fst xs)))  ; Desugars to (' x xs), where type of x is Int and xs is '(Int)
)

;; Complex nested list pattern
(: sum_nested ('('(Int)) -> Int))
(= sum_nested
  '()       0
  '(xs xss..) ((sum_list xs) + (sum_nested xss))
)

;; ? Complex nested list pattern
(: sum_nested2 ('('(Int)) -> Int))
(= sum_nested2
  '()         0
  '(xs xss..) ((sum_list2 xs) + (sum_nested2 xss))
)

;; Nullary vs. unary constructor pattern (exhaustive)
(: is_true (Bool -> String))
(= is_true
  True  "It's true"
  False "It's false"  ; parentheses optional for a unary constructor
)

(: is_just ((Maybe a) -> Bool))
(= is_just
  (Just _) True
  _        False
)

;; Nested pattern
(: process_nested ((Maybe '(Int)) -> Int))
(= process_nested
  (Just '(x _)) x
  _             0
)

;; match expression inside function
(: process_user_data ((User '(Int)) -> Int))
(= process_user_data
  (User (= d data) ..) (match d
    '(x _) x
    '()    0
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

;; If don't use parenthesis, can't be parsed.
;; Can't tell next constructor and type parameter
(data Bool
  True  
  False 
)

(data (Option a)
  Nothing
  (Just a)
)
;; Explicitly disambiguated as (data (forall a (Option a)))


(data (Result a e) 
  (Ok a)
  (Err e)
)
;; Explicitly disambiguated as (data (forall a e (Result a e)))


(data (Phantom a))
;; is actually disambiguated as (data (forall a (Phantom a)))
(data (PhantomList '(a)))
;; is actually disambiguated as (data (forall a (PhantomList '(a))))

;; ! Problem is that this mb-pattern is zero, or one or two s-expression
;; ! But it is hard to tell if what comes next is a data constructor or a type parameter
;; ! So need to make constructor declaration as a single pattern.
;; ! Where nullary parenthesis skip is applied.
(data (PhantomList '(a)) 
  (where (NumLike a))

  NilLike
  (ConsLike a)
)

(data PhantomLen
  (Len Int)
)

(data (Maybe a) 
  Nothing
  (Just a)
)

(data Shape
  (Circle    Float)
  (Rectangle Float Float)
  (Triangle  Float Float Float)
)

(data Person
  (Person
    (: name String)
    (: age  Int)
    (: email String)
  )
)

(data Nat 
  Z
  (S Nat)
)

(: is_two (Nat -> Bool))
(= is_two 
  (S (S Z)) True
  _         False
)

(: add (Nat -> (Nat -> Nat)))
(= add
  (Z x)     x
  ((S x) y) (S (add x y))
)

;; ? Data with associated function
(impl Shape 
  (: area (Shape -> Float))
  (= area
    (Circle r)      (3.14159 * (r * r))
    (Rectangle w h) (w * h)
    _               0.0
  )
)

(impl Shape 
  (: area (Shape -> Float))
  (= area
    (Circle r)      (3.14159 * (r * r))
    (Rectangle w h) (w * h)
    _               0.0
  )
)

;; ? Calling associated function
(: shape_area (Shape -> (Io ())))
(= shape_area
  shape (do
    (print (shape .(area)))
    ;; Desugared: 
    ;; (print (Shape::area shape))
  )
)

(data (Result a e)
  (Ok a)
  (Err e)
)

(: update_email (Person -> (String -> Person)))
(= update_email
  person email (
    (Person email person..)
    ;; (Person (= email email) person..)
    ;; (Person 
    ;;   (= name (person .name)) 
    ;;   (= age (person .age)) 
    ;;   (= email email)
    ;;  )
  )
)

;; Compiler directive expressions
@(cfg debug)
(data (' a)
  Nil
  (' a (' a))
)

@(derive PartialEq (Convertible '(a) (Vec a)))
@(derive PartialOrd '(a))
@(derive Ord '(a))


(data (Tree a)
  Leaf
  (Node a (Tree a) (Tree a))
)

;; GADT is supported
(data (Expr3 a)
  (IntExpr  Int)                             (where (a == Int))
  (BoolExpr Bool)                            (where (a == Bool))
  (ListExpr (Expr3 b) (Expr3 b))             (where (a == '(b)))
  ;; is disambiguated as (forall b (ListExpr (Expr3 b) (Expr3 b))) which is existential type
  (AddExpr  (Expr3 Int) (Expr3 Int))         (where (a == Int))
  (IfExpr   (Expr3 b) (Expr3 a) (Expr3 a))   (where (BooleanLike b))
  ;; is disambiguated as (forall b (IfExpr (Expr3 b) (Expr3 a) (Expr3 a)))
)

;; ? So can I assume that all the constructor's airity is know
;; ! It seems not, since there could be coinductive type 
;; In order to resolve coinduction, the airity of constructor should be known
;; Now the airity of Expr3 is known to be 1 or non zero
;; Therefore, trying to parsing Expr3 as unary pat will fail.
;; Therefore, try to parse whole list as a unary pattern list.


(: Wrapper ((* -> *) -> *))
(data (Wrapper f)
  (Wrapper (f Int))
)

;; ===========================================================
;; METHOD AND RECORD FIELD ACCESS SYNTAX
;; ===========================================================
;;
;; This language provides a unified approach to method calls and
;; record field access through dot notation, while maintaining clear
;; syntactic distinction between the two.

;; Record field access uses a dot followed by an identifier:
;;   (obj .field)
;;
;; Method calls use a dot followed by parenthesized method name and args:
;;   (obj .(method arg1 arg2))
;;
;; For chaining multiple operations, expressions must be properly parenthesized:
;;   ((obj .field1) .(method1))
;;   (((obj .(method1)) .field2) .(method2 arg))

;; Field access examples
(: get_name (Person -> String))
(= get_name
  person (person .name)  ;; Desugars to (.name person)
)

;; Multiple field access
(: format_contact (Person -> String))
(= format_contact
  person (concat '("Name: " (person .name) "Email: " (person .email)))
)

;; Nested record field access
(: get_city (User -> String))
(= get_city
  user ((user .address) .city)  ;; Desugars to (.city (.address user))
)

;; Method call followed by field access
(: get_updated_age (Person -> Int))
(= get_updated_age
  person ((person .(celebrate_birthday)) .age)
  ;; Desugars to: (.age (celebrate_birthday person))
)

;; Complex chain with multiple methods and fields
(: process_users ('(User) -> Report))
(= process_users
  users (users
    .(filter is_male)
    .(sort (lambda u (
      (= profile (u .profile))
      (= avg_visit_period ((profile .membered_for) / (profile .visit_count)))
      (avg_visit_period * 0.5)
      ;; Disugared to (* avg_visit_period 0.5)
    )))
    .(take 10)
    .(map .score) ;; This is not a field access to map, member syntax should be regular parenthesis
    .(reduce +) ;; SPO only works for regular parenthesis
    .(generate_report)
  )
  ;; Desugars to:
  ;; (generate_report
  ;;   (reduce
  ;;     (map
  ;;     (take
  ;;       (sort
  ;;       (filter users is_male)
  ;;       (lambda u (
  ;;         (= profile (.profile u))
  ;;         (= avg_visit_period (/ (.membered_for profile) (.visit_count profile)))
  ;;         (* avg_visit_period 0.5)
  ;;       )))
  ;;       10)
  ;;     .score)
  ;;     +)
  ;;   )
)


;; Refer the separte document for detail


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
(trait (Show a)
  (: show (a -> String))
)

;; Binary trait example
(trait (Convertible a b)
  (: convert (a -> b))
)


;; Functor trait example
(trait (Functor f)
  (: map ((f a) -> ((a -> b) -> (f b))))
)

;; Applicative trait example
(trait (Applicative f)
  (where (Functor f))

  (: pure (a -> (f a)))
  (: ap ((f a) -> ((f (a -> b)) -> (f b))))
)


;; Monad trait example
(trait (Monad m)
  (where (Applicative m))

  (: (>>=) ((m a) -> ((a -> (m b)) -> (m b)))) ; Expressed as := in do-notation
)

;; Simple implementation for a concrete type
(impl Show Bool
  (= show
    True  "True"
    False "False"
  )
)

;; Generic implementation for a parameterized type
(impl Show (Maybe a)
  (where (Show a))

  (= show
    Nothing  "Nothing"
    (Just x) (string-append "Just " (show x))
  )
)

;; Special implementation for a specific type
(impl Show (Maybe Int)

  (= show
    Nothing  "Nothing"
    (Just x) (string-append "Just " (show x))
  )
)

;; Blanket implementation with constraints (disambiguation)
(impl Convertible ((Maybe a) (Maybe b)) 
  (where (Convertible a b))

  (= convert
    Nothing  Nothing
    (Just x) (Just ((Convertible a b)::convert x))  ; trait disambiguation
  )
)


;; ===========================================================
;; KIND ANNOTATIONS
;; ===========================================================
;;
;; Kinds describe the "type of a type" or higher-level abstractions:
;;   - (kind List *) means List is a type constructor: * -> *
;;   - (kind Maybe (* -> *)) means Maybe is also * -> *
;;   - (kind Monad ((* -> *) Constraint)) means Monad takes a
;;     type constructor as a parameter, returning a Constraint.

(kind List *)
(kind Maybe (* -> *))
(kind Monad ((* -> *) -> Constraint))


;; ===========================================================
;; MONADIC COMPUTATION
;; ===========================================================
;;
;; Demonstrates do-notation for chaining effects:
;;   - `:=` for binding within the monad
;;   - `pure` for lifting a value into the monad
;;   - `throw` or other effects as relevant to the Io or error type

(: get_user_data (Int -> (Io (Maybe UserData))))
(= get_user_data
  id (do
    (:= conn open_connection)
    (:= result (query_user conn id))
    (pure result)
  )
)

(: process_result ((Result a Error) -> (Io a)))
(= process_result
  (Ok val) (pure val)
  (Err e)  (do
    (:= _ (log_error e))
    (Exception e)
  )
)

(: fetch_and_process (Int -> (Io (Result Data Error))))
(= fetch_and_process
  id (do
    (:= maybe_user (get_user id))
    (match maybe_user
      (Nothing)   (pure (Err (Error (= message "User not found"))))
      (Just user) (do
        (:= result (process_user user))
        (pure (Ok result))
      )
    )
  )
)

;; ;; ? Monad method form 
;; (: get_user_data2 (Int -> (Io (Maybe UserData))))
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
;; Items starting with underscore (_) are private to the module.
;; All other items are publicly exported by default.
;; Import syntax can include selective imports, aliases, and nested modules.

(mod database
  ;; Public data type (no underscore prefix)
  (data Connection
    (Connection 
      (: url    String)
      (: active Bool)
    )
  )

  ;; Public functions (no underscore prefix)
  (: connect (String -> (Result Connection Error)))
  (= connect url (_make_connection url))

  (: query   (Connection -> (String -> (Result Data Error))))
  (= query conn sql (_run_query conn sql))

  ;; Private helper function (underscore prefix)
  (: _make_connection (String -> (Result Connection Error)))
  (= _make_connection
    url (do
      (:= conn (Connection url True))
      (pure conn)
    )
  )

  ;; Private helper function (underscore prefix)  
  (: _run_query (Connection -> (String -> (Result Data Error))))
  (= _run_query conn sql hole!)
)

(use database)                ; This is a Ident
(use prelude::..)             ; This is a IdentPath as well
(use (collections:: Map Set)) ; This is Ident Tree
(use math::statistics)        ; This is just a path
(use 
  math
  (math:: (statistics:: mean variance) ..)
)


(mod utils
  (use (prelude:: String) 
       (collections:: List))

  ;; Public functions (no underscore prefix)
  (: trim (String -> String))
  (= trim s (_trim_impl s)) 

  (: split (String -> (String -> '(String))))
  (= split (s sep) (_split_impl s sep))

  ;; Private helper functions (underscore prefix)
  (: _trim_impl (String -> String))
  (= _trim_impl s hole!)

  (: _split_impl (String -> (String -> '(String))))
  (= _split_impl s sep hole!)
)

(: process_data (String -> (Result '(Int) Error)))
(= process_data
  raw_data (do
    (= trimmed (utils::trim raw_data))
    (= parts   (utils::split trimmed ","))
    (parse_integers parts)
  )
)

(mod my_module
  ;; Data definition (public by default)
  (data MyType
    (MyCons Int)
  )

  ;; Directly import module from path
  @(mod _some_submodule (Path `("path" "to" "some_submodule")))
  (use _some_submodule)
  
  ;; Public function (no underscore prefix)
  (: my_func (Int -> Int))
  (= my_func x (x + 1))

  ;; Private helper (underscore prefix)
  (: _helper (Int -> Int))
  (= _helper x (x * 2))
)

;; Keyword macro should be reserved for future use

;; ===========================================================
;; PRIVACY EXAMPLES
;; ===========================================================
;;
;; The underscore privacy model applies consistently across:
;;   - function names
;;   - data types  
;;   - constructors
;;   - fields
;;   - variables
;;   - modules

;; Private data type and constructors
(data _InternalState
  (_Private Int String)
  (_Hidden Bool)
)

;; Public data type with mixed visibility constructors
(data Result
  (Ok a)          ;; public constructor
  (_Pending a)    ;; private constructor - internal state
)

;; Private trait
(trait (_InternalShow a)
  (: _show (a -> String))
)

;; Public trait with private methods
(trait (Processor a)
  (: process (a -> a))           ;; public method
  (: _validate (a -> Bool))      ;; private method
  (: _transform (a -> a))        ;; private method
)

;; Implementation can have private helper methods
(impl Processor String
  (= process s (
    (if (_validate s)
      (_transform s)
      s)
  ))

  (= _validate s (not (empty? s)))
  (= _transform s (trim s))
)

;; Record with mixed visibility fields
(data User
  (User
    (: name String)        ;; public field
    (: email String)       ;; public field  
    (: _id Int)           ;; private field
    (: _created_at Time)  ;; private field
  )
)

;; Private module
(mod _internal_utils
  ;; Everything in a private module is effectively private
  (: helper_func (Int -> String))
  (= helper_func n (show n))
)

;; ===========================================================
;; todo

;; todo macro syntax

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
(: name Type0)
(= name String)

;; ;; (kind Array (* -> *))
;; (alias (Array a) '(a))

;; ;; (: Array (Type0 -> Type0))
;; (= Array
;;   (a b) (where (Array a b)) '(a b)
;; )

;; (kind ConvertiblePair (* -> (* -> *)))
(alias ConvertiblePair 
  (a b) (where (Convertible a b)) #(a b)
)

;; (kind If (* -> (* -> (* -> *))))
(alias If 
  (True a _) a
  (False _ b) b
)

;; (kind IsInt (* -> Bool))
(alias IsInt a (If (a ~ Int) True False))
(alias IsInt2 
  Int True
  _   False
)
(alias IntList 
  a (where (IsInt a)) '(a)
  ;; Type level partial function could lead to compile failure
)

;; What about type level recursive function?
;; (kind Fib (* -> *))
(alias Fib
  Z   Z
  (S Z) (S Z)
  (S a) (Add (Fib (S a)) (Fib (S (S a))))
)

;; todo macro

;; Low priority
