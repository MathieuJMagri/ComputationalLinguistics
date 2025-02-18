(comment "Make sure that you follow the instructions carefully
          and use the right procedure names, inputs, and outputs")

;;Preliminaries
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn score-categorical [outcome outcomes params]
  (if (empty? params)
    (print "no matching outcome")
    (if (= outcome (first outcomes))
      (first params)
      (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst)
      (list-foldr f base (rest lst)))))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn score-BOW-sentence [sen probabilities]
  (list-foldr
    (fn [word rest-score]
      (+ (log2 (score-categorical word vocabulary probabilities))
        rest-score))
    0
    sen))

(defn score-corpus [corpus probabilities]
  (list-foldr
    (fn [sen rst]
      (+ (score-BOW-sentence sen probabilities) rst))
    0
    corpus))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
      (log2
        (apply +
          (map (fn [z] (Math/pow 2 z))
            (map (fn [x] (- x mx)) log-vals)))))))

(def my-corpus '((call me)
        (call ishmael)))

;;Problem 1: define `theta-corpus-joint`
;;In log form we add what we would have otherwise multiplied
(defn theta-corpus-joint [theta corpus theta-probs]
  (if (= (first thetas) theta)
    (+ (log2 (first theta-probs)) (score-corpus corpus theta))
    ;;Get second element of theta-probs then score corpus
    (+ (log2 (first (rest theta-probs))) (score-corpus corpus theta))))

;;Calling the function
(theta-corpus-joint theta1 my-corpus theta-prior)

;;Problem 2: define `compute-marginal`
;;Use previous function to define this function
(defn compute-marginal [corpus theta-probs]
  (logsumexp (list (theta-corpus-joint (first thetas) corpus theta-probs)
                   (theta-corpus-joint (first (rest thetas))
                                       corpus theta-probs))))

;;Calling the function
(compute-marginal my-corpus theta-prior)

;;Problem 3: define `compute-conditional-prob`
(defn compute-conditional-prob [theta corpus theta-probs]
  (- (theta-corpus-joint theta corpus theta-probs)
     (compute-marginal corpus theta-probs)))

;;Debugging purposes
;;(compute-conditional-prob theta1 my-corpus theta-prior)
;;(compute-conditional-prob theta2 my-corpus theta-prior)

;;Problem 4: define `compute-conditional-dist`
(defn compute-conditional-dist [corpus theta-probs]
  (list (compute-conditional-prob theta1 corpus theta-probs)
        (compute-conditional-prob theta2 corpus theta-probs)))

;;Question 5
;;(compute-conditional-dist my-corpus theta-prior)
;;Output: (-0.5849625007211561 -1.584962500721156)

;;Problem 6: define `compute-posterior-predictive`

; (defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
;   (let [conditional-dist ...
;     (compute-marginal ...

;;Helper function: 2 to the power of x helper function
(defn exp2 [x]
  (if (zero? 2) 1
      (* x (exp2 x (dec 2)))))

;;Helper function: Make a list with power of 2 values
(defn makeListPow2 [logProbs]
  (if (empty? logProbs)
    ;;If list is empty, stop recursion loop
    '()
    ;;Using Math/pow 2 gives us more precise results than exp2 function
    (cons (Math/pow 2 (first logProbs)) (makeListPow2 (rest logProbs)))))

(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
  (let [conditional-dist
        (makeListPow2 (compute-conditional-dist observed-corpus theta-probs))]
    (compute-marginal new-corpus conditional-dist)))

;;Calling the function
(compute-posterior-predictive my-corpus my-corpus theta-prior)


;Problem 7: define `sample-BOW-corpus`
(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
  (if (< (rand 1) weight)
      true
      false))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
      (first outcomes)
      (sample-categorical (rest outcomes)
                          (normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
    (if (= len 0)
      '()
      (cons (sample-categorical vocabulary probabilities)
        (sample-BOW-sentence (- len 1) probabilities))))

;;Hint: Use sample-BOW-sentence. You may also want to use the built-in function repeatedly.
;;Repeatedly: Takes a function of no args, presumably with side effects, and
;;returns an infinite (or length n if supplied) lazy sequence of calls
;;to it
(defn sample-BOW-corpus [theta sent-len corpus-len]
  (repeatedly corpus-len (fn [] (sample-BOW-sentence sent-len theta))))

;;Debugging purposes
;;(sample-BOW-corpus theta1 2 2)
;;(sample-BOW-corpus theta1 4 2)
;;(sample-BOW-corpus theta1 4 6)


;;Problem 8
; (defn sample-theta-corpus [sent-len corpus-len theta-probs]
;   (let [theta ...
;     (list theta ...

(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta (sample-categorical thetas theta-probs)]
    (list theta (sample-BOW-corpus theta sent-len corpus-len))))

;;Debugging purposes
;;(sample-theta-corpus 3 4 theta2)


;;Problem 9: define `estimate-corpus-marginal`
(defn get-theta [theta-corpus]
  (first theta-corpus))

(defn get-corpus [theta-corpus]
  (first (rest theta-corpus)))

(defn get-count [outcome lst]
  (let [filtered-lst 
        (filter (fn [x] (= outcome x)) lst)]
  (count filtered-lst)))

; ;uncomment the following after you have defined `sample-theta-corpus` above
(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
   (repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

;;Question 9
(defn estimate-corpus-marginal [corpus
                                sample-size
                                sent-len
                                corpus-len
                                theta-probs]
  (/ (apply +
            (let [sample-corpora
                  (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)]
              (map (fn [c] (if (= c corpus) 1 0))
                   (map (fn [x] (get-corpus x)) sample-corpora)))) sample-size))

;;(estimate-corpus-marginal my-corpus 10000 2 2 theta-prior)


;;Problem 11: define `rejection-sampler`

; (defn rejection-sampler
;   [theta observed-corpus sample-size sent-len corpus-len theta-probs]
;   ...)

(defn rejection-sampler [theta
                         observed-corpus
                         sample-size
                         sent-len
                         corpus-len
                         theta-probs]
)

;;Did not implement is because I did not manage to figure out completely/I did not seem to get good values or values that made sense.
