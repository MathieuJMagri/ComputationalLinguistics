
;; Problem 1: define a function `absval`
(def absval (fn [x] (Math/sqrt (* x x))))
;(absval -25)

;; Problem 2: fix the functions `take-square` and `sum-of-squares`
(defn take-square [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (take-square x) (take-square y)))

;; Problem 3: define expressions `exp-13-1`, `exp-13-2`, `exp-13-3`, and `exp-13-4`
(def exp-13-1 '(+ 6 7))
(def exp-13-2 '(- 14 1))
(def exp-13-3 '(/ 26 2))
(def exp-13-4 '(* 1 13))

;; Problem 4: define a function `third`
(def third (fn [list] (nth list 2)))

;; Problem 5:
;; [let's import define `sqrt` and `abs` from Java.lang.Math for the example]
(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
;; define a function `compose` 
(defn compose ([f1 f2]
               (fn
                 ([x] (f1 (f2 x))))))



;; Problem 6: define a function `first-two`
(defn first-two [l]
  (cons
   (first l)
   (list (first (rest l)))))

;; Problem 7: define a function `remove-second`
(defn remove-second [l]
  (cons
   (first l)
   (rest (rest l))))

;; Problem 8: define a function `add-to-end`
(defn add-to-end [lst x] (concat lst [x]))

;; Problem 9: define a function `reverse`
;;           (Note that this funciton will overwrite a built in function. 
;;            This is okay.)
;; Name the function reverse-list (Slack)
(defn reverse-list [lst]
  (into () lst))


;; Problem 10: define a function `count-to-1`
(defn count-to-1 [n] (range n 0 -1))

;; Problem 11: define a function `count-to-n`
(defn count-to-n [n] (range 1 (+ n 1)))

;; Problem 12: define a function `get-max`
(defn get-max [lst]
  (apply max lst))

;; Problem 13: define a function `greater-than-five?`
(defn FiveCheck [number]
  (if (> number 5) true false))

(defn greater-than-five? [lst]
  (map FiveCheck lst))

;; Problem 14: define a function `concat-three`
;(flatten (into () (into () (conj (list 'd 'e) (list 'b 'c) (list 'a 'b)))))

(defn concat-three [lst1 lst2 lst3]
  (flatten (into () (into () (conj lst3 lst2 lst1)))))

;; Problem 15: define a function `sequence-to-power`
(defn sequence-to-power [x n]
  (apply concat (repeat n x)))

;; Problem 16: define a function `in-L-star?`
(defn in-L-star? [lst]
  (if (empty? lst) true
      (if (= (first lst) 'a) (in-L-star? (rest lst))
          false)))