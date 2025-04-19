;; matchExample :: Maybe Int -> String
;; matchExample v@(Just n) = "Got value: " ++ show v ++ " with number: " ++ show n
;; matchExample v@Nothing  = "Got value: " ++ show v

;; -- Example usage with lists
;; processList :: [Int] -> String
;; processList xs@(x:_) = "List " ++ show xs ++ " starts with " ++ show x
;; processList xs@[]    = "Empty list: " ++ show xs

;; Option 1: More like other languages
(match x
  (x @ (Just n)) (string-append "Got value: " (show x) " with number: " (show n))
  (x @ Nothing) (string-append "Got value: " (show x))
)

(match xs
  (xs @ (x ..)) (string-append "List " (show xs) " starts with " (show x))
  (xs @ ())     (string-append "Empty list: " (show xs))
)

;; Option 2: More natural vocalization
(match x
  ((Just n) @ x) (string-append "Got value: " (show x) " with number: " (show n))
  (Nothing @ x)  (string-append "Got value: " (show x))
)

(match xs
  ((x ..) @ xs) (string-append "List " (show xs) " starts with " (show x))
  (() @ xs)     (string-append "Empty list: " (show xs))
)

(42 @ usize)
((xs .(len)) @ u32)

(use (somd_mod::DuplicateName @ NewName))

;; ;; Option 3: Uniform binding
;; (match x
;;   (= x (Just n)) (string-append "Got value: " (show x) " with number: " (show n))
;;   (= x Nothing) (string-append "Got value: " (show x))
;; )
;; (match xs
;;   (= xs (x ..)) (string-append "List " (show xs) " starts with " (show x))
;;   (= xs ())    (string-append "Empty list: " (show xs))
;; )