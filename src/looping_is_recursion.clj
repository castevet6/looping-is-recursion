(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [singleton? (fn [x] (= (count a-seq) 1))]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) (first a-seq)
      :else (last-element (rest a-seq)))))


(defn seq= [seq1 seq2]
  (let [el1 (first seq1)
        el2 (first seq2)]
    (cond
      (and (empty? seq1) (empty? seq2)) true
      (or (empty? seq1) (empty? seq2)) false
      (= el1 el2) (seq= (rest seq1) (rest seq2))
      :else false)))

(defn find-first-index [pred a-seq]
  (loop [cur 0
         n (count a-seq)]
    (if (= cur n)
      nil
      (if (pred (get a-seq cur))
        cur
        (recur (inc cur) n)))))

(defn avg [a-seq]
  (loop [cur 0            ;; current element index
         sum 0            ;; sum of current elements
         n (count a-seq)] ;; total number of elements
    (if (= cur n)
      (/ sum n)
      (recur (inc cur) (+ sum (get a-seq cur)) n))))

;; toggle function from structured-data section
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq a-seq
         result-set #{}]
    (if (empty? seq)
      result-set
      (let [el (first seq)]
        (recur (rest seq) (toggle result-set el))))))

(defn fast-fibo [n]
  (loop [index 0
         max n
         f_n-1 0
         f_n-2 0]
    (if (= index max)
      (if (= index 1)
        1
        (+ f_n-1 f_n-2))
      (recur (inc index) n (+ (if (= index 1) 1 f_n-2) f_n-1) f_n-1))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         instances #{}
         result '()]
    (let [cur (first seq)]
      (cond
        (= cur nil) (reverse result)
        (contains? instances cur) (reverse result)
        :else 
        (recur (rest seq) (conj instances cur) (cons cur result))))))

