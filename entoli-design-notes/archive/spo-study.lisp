;; CPS fibonacci
(: fib (-> Int (-> (-> Int Int) Int)))
(= fib
  (0 k) (k 0)
  (1 k) (k 1)
  (x k) (fib (- x 1) (lambda (a) 
    (fib (- x 2) (lambda (b) 
      (k (+ a b))
    ))
  ))
)


(: fib (Int .(-> (Int .(-> Int) Int))))
(= fib
  (0 k) (k 0)
  (1 k) (k 1)
  (x k) (fib (- x 1) (lambda (a) 
    (fib (- x 2) (lambda (b) 
      (k (+ a b))
    ))
  ))
)

;; With sweet expression
(: fib {Int -> {{Int -> Int} -> Int}})
(= fib
  (0 k) (k 0)
  (1 k) (k 1)
  (x k) (fib (- x 1) (lambda (a) 
    (fib (- x 2) (lambda (b) 
      (k (+ a b))
    ))
  ))
)

;; uncurried
(: fib (-> (Int (-> Int Int)) Int))
(= fib
  (0 k) (k 0)
  (1 k) (k 1)
  (x k) (fib (- x 1) (lambda (a) 
    (fib (- x 2) (lambda (b) 
      (k (+ a b))
    ))
  ))
)

;; Second operator specialization
;; S-expression list with special-character only symbol on second place only identifier automatically converted to operator
(: add (Int -> (Int -> Int)))
(= add
  (x y) (x + y)
)

(: fib_cps (Int -> ((Int -> Int) -> Int)))
(= fib_cps
  (0 k) (k 0)
  (1 k) (k 1)
  (x k) (fib_cps (x - 1) (lambda (a) 
    (fib_cps (x - 2) (lambda (b) 
      (k (a + b))
    ))
  ))
)
;; Will it not cause ambiguity?
;; What if a funciton call takes two arguments and the first argument is a binary function?

(: partial_apply ((a -> a) -> (a -> (a -> a))))
(= partial_apply
  (f x) (lambda z (f x z))
)

;; ... 

(partial_apply + 2)
;; ? How to make this not desugar to (+ partial_apply 2)
(partial_apply (+) 2) ;; This additional parenthesis should prevent desugaring

;; ? Maybe even below should be possible
(partial_apply : ((a -> a) -> (a -> (a -> a))))
(partial_apply =
  (f x) (lambda z (f x z))
)

(fib : ((Int -> Int) -> Int))
(fib = 
  0 0
  1 1
  x ((fib (x - 1)) + (fib (x - 2)))
)

;; Monad do could be better
;; (: get_user_data (-> Int (Io (Maybe UserData))))
;; (= get_user_data
;;   id (do
;;     (<- conn open_connection)
;;     (<- result (query_user conn id))
;;     (pure result)
;;   )
;; )

;; (: process_result (-> (Result a Error) (Io a)))
;; (= process_result
;;   (Ok val) (pure val)
;;   (Err e)  (do
;;             (<- _ (log_error e))
;;             (Exception e)
;;            )
;; )

(: get_user_data (Int -> (Io (Maybe UserData))))
(= get_user_data
  id (do
    (conn <- open_connection)
    (result <- (query_user conn id))
    (pure result)
  )
)

(process_result : ((Result a Error) -> (Io a)))
(process_result =
  (Ok val) (pure val)
  (Err e)  (do
            (x = 42)
            (e <- (log_error e))
            (Exception e)
           )
)

;; (: import_module_source (-> IdPath (Result () String)))
;; (= import_module_source
;;   id_path (do
;;     (<- rel_path (fold_id_path id_path))
;;     (<- source_name (match (to_str rel_path)
;;       (Just str) (pure str)
;;       _          (Exception (format "Failed to convert path {} to string" rel_path))
;;     ))
;;     (<- source (match (config .(import_src rel_path))
;;       (Ok src) (pure src)
;;       (Err e)  (Exception (format "Failed to import module from {} with error: {}" 
;;                               rel_path e))
;;     ))
;;     ;; Add the source to the context
;;     (modify-context 
;;       (lambda (ctx) (Context 
;;         (= staged_srcs (Cons source (ctx .staged_srcs)))
;;         (= staged_src_names (Cons source_name (ctx .staged_src_names)))
;;         ..ctx
;;       ))
;;     )
;;     (pure ())
;;   )
;; )

(import_module_source : ((IdPath -> (Result () String))))
(import_module_source =
  id_path (do
    (rel_path <- (fold_id_path id_path))
    (source_name <- (match (to_str rel_path)
      (Just str) (pure str)
      _          (Exception (format "Failed to convert path {} to string" rel_path))
    ))
    (source <- (match (config .(import_src rel_path))
      (Ok src) (pure src)
      (Err e)  (Exception (format "Failed to import module from {} with error: {}" 
                            rel_path e
                          ))
    ))
    ;; Add the source to the context
    (modify-context 
      (lambda (ctx) (Context 
        (staged_srcs = (Cons source (ctx .staged_srcs)))
        (staged_src_names = (Cons source_name (ctx .staged_src_names)))
        ..ctx
      ))
    )
    (pure ())
  )
)