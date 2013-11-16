(ns page-signal.match-couner)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn lcso
  [^objects a1 ^objects a2]
  (let [a1-len (alength a1)
        a2-len (alength a2)
        prev (long-array (inc a2-len))
        curr (long-array (inc a2-len))]
    (loop [i 0 max-len 0 prev prev curr curr]
      (if (< i a1-len)
        (recur (inc i)
          (long
            (loop [j 0 max-len max-len]
              (if (< j a2-len)
                (let [match-len (if (.equals (aget a1 i) (aget a2 j))
                  (inc (aget prev j))
                  0)]
                  (aset curr (inc j) match-len)
                  (if (> match-len max-len)
                    (recur (inc j) match-len)
                    (recur (inc j) max-len)))
                max-len)))
          curr
          prev)
        max-len))))

(defn lcs
  [^objects a1 a1-start a1-end ^objects a2 a2-start a2-end]
  "start inclusive, end exclusive"
  {:pre [(> a1-end a1-start) (> a2-end a2-start)]}
  (let [sz (inc (- a2-end a2-start))
        prev (long-array sz)
        curr (long-array sz)
        ret (long-array 3) mx 0 ix 1 jx 2] ; 0 = max-len, 1 = i, 2 = j
    (loop [i a1-start _ nil prev prev curr curr]
      (if (< i a1-end)
        (recur (inc i)
          (loop [j a2-start]
            (when (< j a2-end)
              (let [match-len (if (.equals (aget a1 i) (aget a2 j))
                (inc (aget prev j))
                0)]
                (aset curr (inc j) match-len)
                (when (> match-len (aget ret mx))
                  (aset ret mx match-len)
                  (aset ret ix i) (aset ret jx j))
                (recur (inc j)))))
          curr
          prev)
        {:len (aget ret mx) :a1-end (aget ret ix) :a2-end (aget ret jx)}))))

(defn lcsaa
  [^objects a1 ^objects a2]
  (let [a1-len (alength a1)
        a2-len (alength a2)
        prev (long-array (inc a2-len))
        curr (long-array (inc a2-len))
        ret (long-array 3) mx 0 ix 1 jx 2] ; 0 = max-len, 1 = i, 2 = j
    (loop [i 0 prev prev curr curr]
      (if (< i a1-len)
        (do
          (loop [j 0]
            (when (< j a2-len)
              (let [match-len (if (.equals (aget a1 i) (aget a2 j))
                (inc (aget prev j))
                0)]
                (aset curr (inc j) match-len)
                (when (> match-len (aget ret mx))
                  (aset ret mx match-len)
                  (aset ret ix i) (aset ret jx j))
                (recur (inc j)))))
          (recur (inc i) curr prev))
        [(aget ret mx) (aget ret ix) (aget ret jx)]))))


(defn clcs [^objects a1 ^objects a2] ; need to take first
  (let [n (inc (alength a1))]
    (areduce a1 i
      [max-len ^longs prev ^longs curr] [0 (long-array n) (long-array n)]
      [(areduce a2 j max-len (long max-len)
         (let [match-len (if (.equals (aget a1 i) (aget a2 j))
           (inc (aget prev j))
           0)]
           (aset curr (inc j) match-len)
           (if (> match-len max-len)
             match-len
             max-len)))
       curr prev])))

(comment (defn testf
           [f]
           (let [[max-len i j] (time (f a1 a2))]
             (println "res: " res)
             (println "a1 match: ")
             (println "a2 match: "))))