;; ===========================================================
;; ADVANCED PATTERN MATCHING
;; ===========================================================
;;
;; Pattern matching is a core feature of the language, allowing concise
;; and expressive decomposition of data structures. This section explores
;; the syntax and semantics of pattern matching, with special focus on
;; binding patterns.

;; Simple pattern matching on literal values
(: is_zero (Int -> Bool))
(= is_zero
  0 true
  _ false
)

;; Pattern matching on constructors
(: is_nothing ((Maybe a) -> Bool))
(= is_nothing
  Nothing true
  _       false
)

;; ===========================================================
;; MATCH EXPRESSIONS
;; ===========================================================

(: process_data ((Maybe Int) -> String))
(= process_data 
  maybe_value (match maybe_value
    Nothing    "No value found"
    (Just 0)   "Found zero"
    (Just n)   (string-append "Found: " (show n))
  )
)

;; ===========================================================
;; PATTERN TYPES
;; ===========================================================

;; -----------------------------------------------------------
;; LITERAL PATTERNS
;; -----------------------------------------------------------


(: describe_number (Int -> String))
(= describe_number
  0   "Zero"
  1   "One"
  42  "The answer"
  _   "Some other number"
)

;; -----------------------------------------------------------
;; CONSTRUCTOR PATTERNS
;; -----------------------------------------------------------

(: extract_value ((Maybe a) -> (Maybe a)))
(= extract_value
  Nothing    Nothing
  (Just x)   (Just x)
)

;; Nested constructor patterns
(: extract_nested ((Result (Maybe a) e) -> (Maybe a)))
(= extract_nested
  (Ok (Just x))  (Just x)
  (Ok Nothing)   Nothing
  (Err _)        Nothing
)

;; -----------------------------------------------------------
;; TUPLE PATTERNS
;; -----------------------------------------------------------

(: add_pair (#(Int Int) -> Int))
(= add_pair
  #(a b) (a + b)
)

;; Nested tuples
(: complex_tuple (#(Int #(String Bool)) -> String))
(= complex_tuple
  (#2 n #(s true))  (string-append s (show n)) ; Desugared version
  #(n #(s false)) "False case"
)

;; -----------------------------------------------------------
;; LIST PATTERNS
;; -----------------------------------------------------------

(: sum_list ('(Int) -> Int))
(= sum_list
  '()     0
  '(x xs) (x + (sum_list xs))  ;; Desugars to: (' x xs)
)

;; Multiple elements and rest pattern
(: process_list ('(Int) -> String))
(= process_list
  '()           "Empty list"
  '(x)          (string-append "Single element: " (show x))
  '(x y)        (string-append "Two elements: " (show x) ", " (show y))
  '(x xs..)     (string-append "Head: " (show x) ", Tail: " (show xs))
  '(_ y zs..)   (string-append "Second: " (show y) ", Rest: " (show zs))
)

;; ===========================================================
;; ADVANCED PATTERN BINDING
;; ===========================================================

;; -----------------------------------------------------------
;; AS-PATTERNS (`@` OPERATOR)
;; -----------------------------------------------------------


;; Using @ to bind the entire value while also destructuring it
(: process_maybe ((Maybe Int) -> String))
(= process_maybe
  ((Just n) @ m) (string-append "Got Just with value " (show n) " from " (show m))
  (Nothing @ m)  (string-append "Got Nothing from " (show m))
)

;; Useful for nested structures to avoid recomputation
(: find_nested_value ((Tree a) -> (Maybe a)))
(= find_nested_value
  Leaf                                                Nothing
  ((Node val left right) @ t) (where (predicate val)) (Just t)
)


;; -----------------------------------------------------------
;; Record PATTERNS WITH `=`
;; -----------------------------------------------------------

;; Record pattern with field binding
(: get_person_info (Person -> String))
(= get_person_info
  (Person (= "John" name) (= a age) _) 
    (string-append n " is " (show a) " years old")
)

(: format_person (Person -> String))
(= format_person
  ((Person (= n name) ..) @ p) (string-append n " (ID: " (show (p .id)) ")")
)


;; ===========================================================
;; RECORD UPDATE WITH PATTERN MATCHING
;; ===========================================================

(: increment_age (Person -> Person))
(= increment_age
  ((Person (= a age) ..) @ p) (Person (= age (a + 1)) p..)
)

;; Pattern matching with record update in a single step
(: update_email (Person -> (String -> Person)))
(= update_email
  person new_email (Person (= email new_email) person..)
)

;; ===========================================================
;; GUARDS IN PATTERN MATCHING
;; ===========================================================


(: categorize_age (Person -> String))
(= categorize_age
  (Person (= n name) (= a age) ..) (where (a < 18)) (string-append n " is a minor")
  (Person (= n name) (= a age) ..) (where (a < 65)) (string-append n " is an adult")
  (Person (= n name) (= a age) ..)                  (string-append n " is a senior")
)


;; ===========================================================
;; PATTERN MATCHING IN MONADIC CONTEXTS
;; ===========================================================

(: process_user_data (Int -> (Io (Result String Error))))
(= process_user_data
  id (do
    (:= maybe_user (fetch_user id))
    (match maybe_user
      Nothing         
        (pure (Err (Error "User not found")))
      (Just ((User(= true active) ..) @ user))
        (do
          (:= processed_data (process_data user))
          (pure (Ok ("Processed data for " ++ name))) ; All the fields of User are expanded by ellipsis
        )
      (Just (User (= false active)))
        (pure (Err (Error "User account inactive")))
    )
  )
)

;; ===========================================================
;; EXHAUSTIVENESS
;; ===========================================================

;; This is exhaustive for Bool
(: bool_to_string (Bool -> String))
(= bool_to_string
  true  "True"
  false "False"
)

;; This is not exhaustive and will generate a warning
(: maybe_to_string ((Maybe a) -> String))
(= maybe_to_string
  (Just x) (concat '("Just " (show x)))
  ;; Missing Nothing case
)

;; ===========================================================
;; MULTIPLE ARGUMENTS PATTERN MATCHING
;; ===========================================================

(: combine_maybes ((Maybe a) -> ((Maybe b) -> (Maybe #(a b)))))
(= combine_maybes
  ((Just a) (Just b)) (Just #(a b))
  (_ _)                Nothing
)

;; ===========================================================
;; RECURSIVE PATTERN MATCHING WITH GADTS
;; ===========================================================

;; Pattern matching on GADTs
(: eval (Expr a) -> a)
(= eval
  (IntExpr n)                n
  (BoolExpr b)               b
  (AddExpr e1 e2)            ((eval e1) + (eval e2))
  (IfExpr cond then else)    (if (eval cond) (eval then) (eval else))
  (ListExpr e1 e2)           (cons (eval e1) (eval e2))
)

;; ===========================================================
;; FULL RECORD EXAMPLE
;; ===========================================================

(data Organization
  (Organization
    (: name       String)
    (: founded    Int)
    (: employees  '(Employee))
  )
)

(data Employee
  (Employee
    (: name      String)
    (: position  String)
    (: age       Int)
    (: address   Address)
  )
)

(data Address
  (Address 
    (: street  String)
    (: city    String)
    (: country String)
  )
)


(: find_employee (Organization -> (String -> (Maybe Employee))))
(= find_employee
  ((Organization (= emps employees)) @ org) search_name
    (emps 
      .(find (lambda e ((e .name) == search_name)))
    )
)


(: summarize_organization (Organization -> String))
(= summarize_organization
  org (concat '(
    ;; todo f-string
    f"Organization: {(org .name)} (Founded: {(org .founded)}) has "
    "{(length (org .employees))} employees"
  ))
)


(: complex_employee_query (Organization -> '(String)))
(= complex_employee_query
  emps (emps 
    .(filter (lambda e ((e .age) > 30)))
    .(filter (lambda (Employee ..) ((address .country) == "Sweden")))
    .(map (lambda e (e .name)))
  )
)



;; ===========================================================
;; 
;; Pattern matching is a powerful feature that enables concise and 
;; expressive code, minimizing boilerplate while maintaining clarity 
;; and type safety.
;; 
;; ===========================================================