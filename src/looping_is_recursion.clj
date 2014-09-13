(ns looping-is-recursion)

(defn power [base exp]
  (letfn [(p [acc n]
            (if (zero? n)
              acc
              (recur (* acc base) (dec n))))]
    (p 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq)) 
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         s a-seq]
    (cond (empty? s) nil
          (pred (first s)) n
          :else (recur (inc n) (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         s a-seq]
    (if (empty? s)
      (/ sum count)
      (recur (+ sum (first s)) (inc count) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [values #{}
         s a-seq]
    (if (empty? s)
      values
      (recur (toggle values (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [m 1
         l 1
         c (- n 3)]
    (cond (zero? n) 0 
          (<= n 2) 1
          (zero? c) (+ m l)
          :else (recur (+ m l) m (dec c)))))

(defn cut-at-repetition [a-seq]
  (loop [values []
         s a-seq]
    (cond (empty? s) values
          (contains? (set values) (first s)) values
          :else (recur (conj values (first s)) (rest s)))))
