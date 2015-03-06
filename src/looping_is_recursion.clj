(ns looping-is-recursion)

(defn singleton? [coll]
 (if (and ((complement empty?) coll) (empty? (rest coll))) true false ))

(defn power [base exp]
  (let [helper (fn [acc n k]
                (if (= n 0) 0
                  (if (= k 0) acc
                    (recur (* acc n) n (dec k)))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if
   (empty? a-seq) nil
   (if (singleton? a-seq) (first a-seq)
   (recur (rest a-seq)))))

(defn seq= [seq1 seq2]
  (if (and (empty? seq1) (empty? seq2)) true
    (if (or (empty? seq1) (empty? seq2)) false
      (if (not (= (first seq1) (first seq2))) false
        (recur (rest seq1) (rest seq2))))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         seq a-seq]
    (if (empty? seq) nil
      (if (pred (first seq)) i
        (recur (+ i 1) (rest seq))))))

(defn avg [a-seq]
  (loop [n 0
         s 0
         seq a-seq]
    (if (empty? seq)
      (if (= n 0) 0
        (/ s n))
      (recur (+ n 1) (+ s (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [s (set ())
         seq a-seq]
    (if (empty? seq) s
      (recur (toggle s (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (loop [i 2
         p 1
         pp 0]
    (if (= n 0) 0
      (if (= n 1) 1
        (if (= i n) (+ p pp)
        (recur (+ i 1) (+ p pp) p))))))

(defn cut-at-repetition [a-seq]
  (loop [s (set [])
         se []
         sea a-seq]
    (if (or (empty? sea) (contains? s (first sea))) (reverse se)
      (recur (conj s (first sea)) (cons (first sea) se) (rest sea)))))

