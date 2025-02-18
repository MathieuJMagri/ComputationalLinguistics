(comment "Make sure that you follow the instructions carefully
          and use the right procedure names, inputs, and outputs")

;; Problem 1: define a function `get-vocabulary`
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
     how long precisely having little or no money in my purse , and
     nothing particular to interest me on shore , I thought I would
     sail about a little and see the watery part of the world .  It is
     a way I have of driving off the spleen , and regulating the
     circulation . Whenever I find myself growing grim about the mouth
     whenever it is a damp , drizzly November in my soul whenever I
     find myself involuntarily pausing before coffin warehouses , and
     bringing up the rear of every funeral I meet and especially
     whenever my hypos get such an upper hand of me , that it requires
     a strong moral principle to prevent me from deliberately stepping
     into the street , and methodically knocking people's hats off
     then , I account it high time to get to sea as soon as I can .
     This is my substitute for pistol and ball . With a philosophical
     flourish Cato throws himself upon his sword I quietly take to the
     ship .  There is nothing surprising in this . If they but knew it
     , almost all men in their degree , some time or other , cherish
     very nearly the same feelings toward the ocean with me .))

;see if w is in l
(defn member-of-list? [w l]
  (if (empty? l)
    false
    (if (= w (first l))
      true
      (member-of-list? w (rest l)))))

;;(member-of-list? 'a moby-word-tokens) --> This returns true!

;; Define a function `add-to-end` --> THIS HELPER FUNCTION IS TAKEN FROM PROBLEM SET 2 TO HELP WITH PROBLEM SET 3
(defn add-to-end [lst x] (concat lst [x]))

(defn get-vocabulary [word-tokens vocab]
  (if (empty? word-tokens)
      vocab
      (if (member-of-list? (first word-tokens) vocab)
          (get-vocabulary (rest word-tokens) vocab)
          (get-vocabulary (rest word-tokens) 
                          (add-to-end vocab (first word-tokens))))))

(def moby-vocab (get-vocabulary moby-word-tokens '()))

;;DEBUGGING PURPOSES
;;(get-vocabulary '(the ship is the ship) '())
;;moby-vocab



;; Problem 02: define a function `get-count-of-word`
;;The variable name "c" is used in place of "count" so as to not confuse it
;;with the actual "count" function provided by clojure.
(defn get-count-of-word [w word-tokens c]
  (if (empty? word-tokens)
    c
    (if (= w (first word-tokens))
      (get-count-of-word w (rest word-tokens) (+ c 1))
      (get-count-of-word w (rest word-tokens) c))))

;;DEBUGGING PURPOSES
;;(get-count-of-word 'the (list 'the 'the 'whale) 0)
;;(get-count-of-word 'the (list 'the 'whale) 0)
  


;; Problem 03: define a variable `moby-word-frequencies`

(defn get-word-counts [vocab word-tokens]
  (let [count-word (fn [w] 
                     (get-count-of-word w word-tokens 0))]
    (map count-word vocab)))

;;Answer to the question
(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))

;DEBUGGING PURPOSES
;(get-word-counts '(whale the is) '(the is whale is))

;; Problem 04: write a function `sample-uniform-BOW-sentence`

(defn flip [p]
  (if (< (rand 1) p)
    true
    false))

(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
    (first outcomes)
    (sample-categorical (rest outcomes) (normalize (rest params)))))

(defn create-uniform-distribution [outcomes]
  (let [num-outcomes (count outcomes)]
    (map (fn [x] (/ 1 num-outcomes))
	 outcomes)))

;;List Unfold function directly taking from the Chapter 11 notes
(defn list-unfold [generator len]
  (if (= len 0)
    '()
    (cons (generator)
          (list-unfold generator (- len 1)))))

(defn sample-uniform-BOW-sentence [n vocab]
  (list-unfold (fn []
                 (sample-categorical
                  vocab
                  (create-uniform-distribution vocab)))
               n))

;;DEBUGGING PURPOSES
;(sample-uniform-BOW-sentence 3 moby-vocab)


;; Problem 5: Define a function `compute-uniform-BOW-prob`
;;Helper Function - score-categorical: taken from Chapter 10 of notes
(defn score-categorical [outcome outcomes probs]
  (if (empty? probs)
    0
    (if (= outcome (first outcomes))
      (first probs)
      (score-categorical outcome (rest outcomes) (rest probs)))))

;;The fold right function was taken from the course notes from Chapter 11
(defn list-foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst)
       (list-foldr f base (rest lst)))))

(defn compute-uniform-BOW-prob [vocab sentence]
  (list-foldr
   (fn [word rest-score]
     (* (score-categorical word vocab (create-uniform-distribution vocab))
        rest-score))
   1
   sentence))

;;(compute-uniform-BOW-prob '(the a every) '(every))
;;(compute-uniform-BOW-prob '(the a every) '(n))
;;(compute-uniform-BOW-prob '(the a every) '(the a))




;; Problem 7: Define a variable `moby-word-probabilities`
(def moby-word-probabilities (normalize moby-word-frequencies))


;;Problem 8: Run sample-BOW-sentence at least three times
(defn sample-BOW-sentence [len vocabulary probabilities]
  (if (= len 0)
    '()
    (cons (sample-categorical vocabulary probabilities)
	  (sample-BOW-sentence (- len 1) vocabulary probabilities))))

;;(sample-BOW-sentence 3 moby-vocab moby-word-probabilities)
;;After running the above function 5 times, the following sentences were obtained:
;; '(it nearly meet)
;; '(that time of)
;; '(find as I)
;; '(is part soul)
;; '(it the .)




;; Problem 9: Define a function lookup-probability
;;lookup-probability is exaclty like the function score-categorical in the Chapter 10 notes!
(defn lookup-probability [w outcomes probs]
  (if (empty? outcomes)
    0
    (if (= w (first outcomes))
      (first probs)
      (lookup-probability w (rest outcomes) (rest probs)))))

;;(defn score-categorical [outcome outcomes probs]
;;  (if (empty? probs)
;;    (throw "no matching outcome")
;;    (if (= outcome (first outcomes))
;;      (first probs)
;;      (score-categorical outcome (rest outcomes) (rest probs)))))

;;DEBUGGING PURPOSES
;;(lookup-probability 'a '(the a every) '(0.2 0.5 0.3))


;; Problem 10: Define a function compute-BOW-prob

(defn compute-BOW-prob [sentence vocabulary probabilities]
  (list-foldr
   (fn [word rest-score]
     (* (lookup-probability word vocabulary probabilities)
        rest-score))
   1
   sentence))

;;Question 11
;;(compute-BOW-prob '(it nearly meet) moby-vocab moby-word-probabilities)
;;(compute-BOW-prob '(that time of) moby-vocab moby-word-probabilities)
;;(compute-BOW-prob '(find as I) moby-vocab moby-word-probabilities)
;;(compute-BOW-prob '(is part soul) moby-vocab moby-word-probabilities)
;;(compute-BOW-prob '(it the .) moby-vocab moby-word-probabilities)
