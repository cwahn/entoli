# entoli
Entoli is a LISP-like strict pure functional programming language

## ES-Expression 
Entoli, being a small and LISP-like language, takes advantage of simple S-expression syntax.  
However, standard S-expressions increase the programmer's cognitive load.

There are a number of detailed issues, but we believe two major ones are:
- No infix operators (even for directional operators like `->`!)
- No field-accessor or method-call syntax

They can be resolved by simple lexer-level syntactic sugars.

### SPO Syntax
Honoring idiomatic mathematical notation, we introduce a simple syntactic sugar called SPO (Second Place Operator). As the name suggests, it is a lexer-level syntactic sugar whereby any operator (consisting only of ASCII punctuation) in the second position applies to the first argument.
```lisp
(1 + 2)               ;Desugars to (+ 1 2)
(Int -> (Int -> Int)) ;Desugars to (-> Int (-> Int Int))
```

### Objective Syntax 
Lots of modern programming languages employ objective syntax, namely field accessors and method calls (or method chaining). Considering common development environments, this means more than just stylistic sugar as they incfluence namespace and code suggestion.
```lisp
(person .name)         ;Desugars to (.name person)
(person .(set_age 20)) ;Desugars to (set_age person 20)
```

Combining these two syntactic sugar, we can write a code like this:
```lisp
(: process_users ('(User) -> Report))
(= process_users users
   (users
    .(filter is_male)
    .(sort (lambda u (
      (= profile (u .profile))
      (= avg_visit_period ((profile .membered_for) / (profile .visit_count)))
      (avg_visit_period * 0.5)
    )))
    .(take 10)
    .(map .score) 
    .(reduce +)
    .(generate_report)
  )
)
```

Which will be desugared to (also how it would look in standard S-expression):
```lisp
(: process_users (-> '(User) Report))
(= process_users users
  (generate_report
    (reduce
      (map
        (take
          (sort
            (filter users is_male)
              (lambda u (
                (= profile (.profile u))
                (= avg_visit_period (/ (.membered_for profile) (.visit_count profile)))
                (* avg_visit_period 0.5)
              )))
            10)
        .score)
      +)
  )
)
```

This is ES-expression(Entoli S-Expression). Does it look better? Is this a LISP or not? Well you tell me. 