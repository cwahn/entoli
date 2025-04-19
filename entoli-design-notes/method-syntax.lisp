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


;; ===========================================================
;; RECORD DEFINITION AND FIELD ACCESS
;; ===========================================================

;; Record type definition
(type Person
  (Person 
    (: name String)
    (: age Int)
    (: email String)
  )
)

;; Field access examples
(: get_name (Person -> String))
(= get_name
  person (person .name)  ;; Desugars to (.name person)
)

;; Multiple field access
(: format_contact (Person -> String))
(= format_contact
  person (concat
           '("Name: " (person .name) "Email: " (person .email))
         )
)

;; Nested record field access
(: get_city (User -> String))
(= get_city
  user ((user .address) .city)  ;; Desugars to (.city (.address user))
)


;; ===========================================================
;; METHOD IMPLEMENTATION AND CALLING
;; ===========================================================

;; Method implementation for Person type
(impl Person
  (: greet (Person -> String))
  (= greet
    person (string-append "Hello, " (person .name))
  )

  (: celebrate_birthday (Person -> Person))
  (= celebrate_birthday
    person (Person
      (= age (+ (person .age) 1))
      ..person
    )
  )

  (: with_email (Person -> (String -> Person)))
  (= with_email
    person new_email (Person
      (= email new_email)
      ..person
    )
  )
)

;; Method call examples
(: birthday_greeting (Person -> String))
(= birthday_greeting person (person .(celebrate_birthday) .(greet))
  ;; Desugars to: (greet (celebrate_birthday person))
)

;; Method call with arguments
(: update_contact (Person -> (String -> Person)))
(= update_contact
  person new_email (person .(with_email new_email))
  ;; Desugars to: (with_email person new_email)
)

;; ===========================================================
;; MIXED METHOD AND FIELD CHAINS
;; ===========================================================

;; Field access followed by method call
(: validate_email (Person -> Bool))
(= validate_email
  person ((person .email) .(contains "@"))
  ;; Desugars to: (contains (.email person) "@")
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
    .(reduce (Num f64)::+)
    .(generate_report)
  )
)
;; Desugars to:
;;   (generate_report
;;         (reduce
;;          (map
;;           (take
;;            (sort
;;             (filter users is_male)
;;             (lambda u (
;;              (= profile (.profile u))
;;              (= avg_visit_period (/ (.membered_for profile) (.visit_count profile)))
;;              (* avg_visit_period 0.5)
;;             )))
;;            10)
;;           .score)
;;          +))

;; (: process_users ('( ->User) Report))
;; (= process_users
;;  users 
;; )

;; ===========================================================
;; DESUGARING RULES
;; ===========================================================
;;
;; Method and field chains are desugared from right to left:
;;
;; 1. For method calls: (X .(method args)) → (method X args)
;; 2. For field access: (X .field) → (.field X)
;;
;; The language automatically generates field accessor functions
;; for all record fields using the naming convention (.field_name).
;;
;; Note that chaining requires proper parenthesization except for the
;; special case of complex chains of the form:
;;   (obj .(method1) .field1 .(method2) ... )
;; which is desugared from right to left, applying each step to the
;; result of the previous operations.