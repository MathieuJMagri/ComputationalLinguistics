;; variables and functions defined in the background section:

(def hidden-states '(Start N V))

(def vocabulary '(Call me Ishmael))

(def theta-transition-Start '(0 0.9 0.1))
(def theta-transition-N '(0 0.3 0.7))
(def theta-transition-V '(0 0.8 0.2))

(def theta-transition-dists-1
    (list theta-transition-Start 
          theta-transition-N 
          theta-transition-V))

(def theta-observation-Start '(0.0 0.0 0.0))
(def theta-observation-N '(0.1 0.5 0.4))
(def theta-observation-V '(0.8 0.1 0.1))

(def theta-observation-dists-1
    (list theta-observation-Start
          theta-observation-N
          theta-observation-V))

;; helper functions

(defn dist-lookup
    "gets the distribution over states for a given state 
     from the list of dists"
    [state states dists]
    (if (= state (first states))
        (first dists)
        (dist-lookup state (rest states) (rest dists))))

(defn log2 [n] (/ (Math/log n) (Math/log 2)))

(defn logsumexp 
  "given a list of log-vals: exponentiates, sums, and re-logs them"
  [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
       (log2
        (apply +
               (map (fn [z] (Math/pow 2 z))
                    (map (fn [x] (- x mx))
                         log-vals)))))))

(defn logscore-categorical
    "log probability of word 'outcome' given the vocabulary 'outcomes'
    and the list of categorical outcome probabilities 'params'"
    [outcome outcomes params]
    (if (= outcome (first outcomes))
        (log2 (first params))
        (logscore-categorical outcome (rest outcomes) (rest params))))

;; Problem 1  - score-next-state-word

; (defn score-next-state-word
;   "compute log probability of going from 'curr-state' to
;    'next-state' and emmitting 'next-word'"
;   [curr-state next-state next-word t-dists o-dists]
;   ... )
;;Hint: use the functions dist-lookup and logscore-categorical defined above.
(def score-next-state-word (fn [current-hidden next-hidden next-observed theta-transition-dists theta-observation-dists]
                             (+ (logscore-categorical next-hidden hidden-states (dist-lookup current-hidden hidden-states theta-transition-dists))
                              (logscore-categorical next-observed vocabulary (dist-lookup next-hidden hidden-states theta-observation-dists)))))

;; A call to (score-next-state-word current- hidden next-hidden next-observed theta-transition-dists theta-observation-dists) 
;;should return the log probability of the next hidden state and next observed word, given the current hidden state.

;; Problem 2  - compute-next-observation-marginal

; (defn compute-next-observation-marginal
;   "get log marginal probability of next word being 
;    'next-observation' given 'curr-state'"
;   [curr-state next-observation t-dists o-dists]
;   ... )

;;Hint: Use logsumexp
(def compute-next-observation-marginal (fn [current-state next-observation theta-transition-dists theta-observation-dists]
                                         (logsumexp (map (fn [x] (score-next-state-word current-state x
                                                                              next-observation
                                                                              theta-transition-dists
                                                                              theta-observation-dists))
                                                     (rest hidden-states)))))

;;x -> hidden-states
;;When (compute-next-observation-marginal current-state next- observation theta-transition-dists theta-observation-dists) is called, 
;;it should return the marginal probability of the next observed word given the current hidden state.

;; Problem 3  - score-next-states-words

; (defn score-next-states-words
;   "compute log prob of going from 'curr-state'
;     to sequence of states 'next-states'
;     and emmitting sequence of words 'next-words'"
;   [curr-state next-states next-words t-dists o-dists]
;   ... )

;;Hint: Use recursion to find a solution
(def score-next-states-words (fn [current-hidden next-hidden-states next-words theta-transition-dists theta-observation-dists]
                               (if (empty? next-hidden-states)
                                 0
                                 (+ (score-next-state-word current-hidden (first next-hidden-states) (first next-words) theta-transition-dists theta-observation-dists)
                                    (score-next-states-words (first next-hidden-states) (rest next-hidden-states) (rest next-words) theta-transition-dists theta-observation-dists)))))

;; When (score-next-states-words current-hidden next- hidden-states next-words theta-transition-dists theta-observation-dists) is called, 
;;it should return the log probability that the k hidden states and k observed words will appear immediately following the current hidden state.

;; Problem 4  - compute-next-words-marginal

; (defn compute-next-words-marginal
;   "get the log marginal prob of sequence 'next-words' given 'curr-state'"
;   [curr-state next-words t-dists o-dists]
;   ... )

;;Hint: Use recursion to write the function
(def compute-next-words-marginal (fn
                                   [current-hidden next-words theta-transition-dists theta-observation-dists]
                                   (if (empty? next-words)
                                     0
                                     (let [f (fn [hidden] (+
                                                           (compute-next-words-marginal hidden (rest next-words) theta-transition-dists theta-observation-dists)
                                                           (score-next-state-word current-hidden hidden (first next-words) theta-transition-dists theta-observation-dists)))] (logsumexp (map f hidden-states))))))

;;When (compute-next-words-marginal current-hidden next-words theta-transition-dists theta-observation-dists) is called, 
;;it should return the log of the marginal prob- ability of the list of words given the hidden state, as defined in the equation above.

;;Question 5:
(compute-next-words-marginal 'Start (list 'Call 'Ishmael) theta-transition-dists-1 theta-observation-dists-1)
(compute-next-words-marginal 'Start (list 'Ishmael 'Call) theta-transition-dists-1 theta-observation-dists-1)

;;When we call (compute-next-words-marginal 'Start (list 'Call 'Ishmael) theta-transition-dists-1 theta-observation-dists-1) and 
;;then (compute-next-words-marginal 'Start (list 'Ishmael 'Call) theta-transition-dists-1 theta-observation-dists-1), 
;;we get the following respective values: -4.496549490994431 and -2.2189341015640367. As we can see, the sequence 
;;Call-Ishmael does not produce the same value for its marginal probability and its marginal probability reverse. 

;;Given that the marginal probability of a word depends on the probability that the word in question "shows up" and 
;;the probability value of the transition from the latest transition to the newest, it is normal in our case that reversing 
;;the order in which a sequence is defined will produce two different values. Indeed, this is easy to visualize as the 
;;first sequence is as follows Start to Call to me and the second sequence is Start to me to Call. Furthermore, we know 
;;from the background information of this assignment that the values for each and every transition are mostly different, 
;;so the different in results are largely due to the different values a transition may take. 

;; Problem 6  - compute-hidden-prior

; (defn compute-hidden-prior
;   "get log prior probability 'list-of-states' given transition 
;    distributions 't-dists'"
;   [list-of-states t-dists]
;   ... )

(def compute-hidden-prior (fn [k tts] (if (= (count k) 1)
                                        (logscore-categorical (first k) hidden-states (first tts))
                                        (+  (compute-hidden-prior (rest k) tts)
                                            (logscore-categorical (first (rest k)) hidden-states (dist-lookup (first k) hidden-states tts))))))

;;it should return the log prior probability of the hidden state sequence

;; Problem 7  - compute-likelihood-of-words

; (defn compute-likelihood-of-words
;   "Likelihood (of hidden states) = conditional probability
;   of the words in the list-of-words given the list-of-states"
;   [list-of-states list-of-words o-dists]
;   ... )
(def compute-likelihood-of-words (fn [k kwords tts] (if (empty? k)
                                                      0
                                                      (+ (compute-likelihood-of-words (rest k) (rest kwords) tts)
                                                         (logscore-categorical (first kwords) vocabulary (dist-lookup (first k) hidden-states tts))))))

;;it should return the log probability of the k words being generated from the hidden states


;; Problem 8  - compute-hidden-posterior

; (defn compute-hidden-posterior
;   "computes log posterior probability of the 'list-of-states' 
;    given the observed 'list-of-words'"
;   [list-of-states list-of-words t-dists o-dists]
;   ... )
;;Hint: Use your solutions to Problems 4, 6, and 7, and don’t foget that we are in the log domain. Note also that in this function you will need to hardcode the first state as 'Start.
(def compute-hidden-posterior (fn [hidden-states words theta-transition-dists theta-observation-dists]
                                (-
                                 (+ (compute-likelihood-of-words hidden-states words theta-observation-dists)
                                    (compute-next-words-marginal 'Start words theta-transition-dists theta-observation-dists))
                                 (compute-hidden-prior hidden-states theta-transition-dists))))

;; Problem 10 - compute-next-words-marginal-mem

; (def compute-next-words-marginal-mem
;   "(like compute-next-words-marginal, but with memoization)
;   gets the log marginal prob of sequence 'next-words' given 'curr-state'"
;   ... )

;;;;;;;;;;;;
;;NOT DONE;;
;;;;;;;;;;;;