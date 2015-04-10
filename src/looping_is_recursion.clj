(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k] (if (zero? k)
                             acc
                             (recur (* acc base) (dec k))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         remaining a-seq]
    (cond
      (empty? remaining) nil
      (pred (first remaining)) index
      :else (recur (inc index) (rest remaining)))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         remaining a-seq]
    (if (empty? remaining)
      (/ acc n)
      (recur (+ acc (first remaining)) (inc n) (rest remaining)))))

(defn parity [a-seq]
  (let [toggle (fn [s elem] (if (contains? s elem)
                              (disj s elem)
                              (conj s elem)))]
    (loop [acc #{}
           remaining a-seq]
      (if (empty? remaining)
        acc
        (recur (toggle acc (first remaining)) (rest remaining))))))

(defn fast-fibo [n]
  (cond
    (zero? n) 0
    (<= n 2) 1
    :else (loop [curr-n   3
           curr-fib 2
           last-fib 1]
      (if (= curr-n n)
        curr-fib
        (recur (inc curr-n) (+ curr-fib last-fib) curr-fib)))))

(defn cut-at-repetition [a-seq]
  (loop [new-seq []
         seen #{}
         remaining a-seq]
    (if (or (contains? seen (first remaining)) (empty? remaining))
      new-seq
      (recur (conj new-seq (first remaining))
             (conj seen (first remaining))
             (rest remaining)))))

