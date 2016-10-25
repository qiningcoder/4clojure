 (ns enjoy)

;#104 Write Roman Numerals
(defn write-roman-numbers
  [x]
  (let [arr [{"1" "I" "2" "II" "3" "III" "4" "IV" "5" "V"
              "6" "VI" "7" "VII" "8" "VIII" "9" "IX"}
             {"1" "X" "2" "XX" "3" "XXX" "4" "XL" "5" "L"
              "6" "LX" "7" "LXX" "8" "LXXX" "9" "XC"}
             {"1" "C" "2" "CC" "3" "CCC" "4" "CD" "5" "D"
              "6" "DC" "7" "DCC" "8" "DCCC" "9" "CM"}
             {"1" "M" "2" "MM" "3" "MMM"}]
        ]
    (apply str (reverse (map-indexed #((nth arr %) (str %2)) (reverse (str x)))))))



;#92 Read Roman numerals
 (defn read-from-roman
   [rom]
   (let [num-map {"XX" 20,
                  "III" 3,
                  "MMM" 3000,
                  "XXX" 30,
                  "CD" 400,
                  "XC" 90,
                  "L" 50,
                  "M" 1000,
                  "MM" 2000,
                  "VIII" 8,
                  "CM" 900,
                  "C" 100,
                  "XL" 40,
                  "DC" 600,
                  "CC" 200,
                  "II" 2,
                  "LX" 60,
                  "LXXX" 80,
                  "V" 5,
                  "VII" 7,
                  "CCC" 300,
                  "DCC" 700,
                  "X" 10,
                  "VI" 6,
                  "IX" 9,
                  "I" 1,
                  "DCCC" 800,
                  "LXX" 70,
                  "IV" 4,
                  "D" 500}
         rom-bases ["MMM" "MM" "M" "CM" "DCCC" "DCC" "DC" "D" "CD" "CCC" "CC"
                    "C" "XC" "LXXX" "LXX" "LX" "L" "XL" "XXX" "XX" "X" "IX"
                    "VIII" "VII" "VI" "V" "IV" "III" "II" "I"]
         starts-with? (fn [s substr]
                       (= substr (.substring s 0 (min (count s) (count substr)))))
         pre (reduce #(if (and (nil? %) (starts-with? rom %2))
                        %2 %) nil rom-bases)]
     (if (nil? pre)
       0
       (+ (num-map pre) (read-from-roman (.substring rom (count pre)))))))


;#116 Prime Sandwich
 (defn balanced-prime?
   [x]
   (letfn [(prime? [x]
             (if (< x 4)
               true
               (reduce #(if %
                         (false? (zero? (rem x %2)))
                         %) true (range 2 (inc (int (Math/sqrt x)))))))
           (before-prime
             [p]
             (reduce #(if (nil? %)
                       (if (prime? %2)
                         %2 nil)
                       %) nil (range (dec p) 1 -1)))
           (after-prime
             [p]
             (loop [i (inc p)]
               (if (prime? i)
                 i
                 (recur (inc i)))))]
     (true? (and (prime? x)
                 (let [p1 (before-prime x)]
                   (if (nil? p1)
                     false
                     (= (+ x x) (+ p1 (after-prime x)))))))))

;#177 Balancing Brackets
 (defn balanced-brackets?
   [s]
   (let [m {\( \) \[ \] \{ \}}
         ks (set (keys m))
         vs (set (vals m))]
     (empty? (reduce #(if (contains? ks %2)
                       (conj % %2)
                       (if (contains? vs %2)
                         (if (empty? %)
                           (conj % %2)
                           (if (= (m (last %)) %2)
                             (vec (drop-last %))
                             (conj % %2)))
                         %)) [] s))))

;#112 Sequs Horribilis
 (defn find-nested-coll
   [n coll]
   (if (empty? coll)
     '()
     (let [e (first coll)]
       (if (number? e)
         (if (<= e n)
           (concat [e] (find-nested-coll (- n e) (rest coll)))
           '())
         (let [x (find-nested-coll n e)
               xs (apply + (flatten x))]
           (if (empty? x)
             '()
             (if (= xs (apply + (flatten e)))
               (concat [e] (find-nested-coll (- n xs) (rest coll)))
               [x])))))))

;#141 Tricky card games
 (defn find-winner
   [trump]
   #(reduce (fn [res e]
              (let [s1 (:suit res)
                    s2 (:suit e)
                    r1 (:rank res)
                    r2 (:rank e)]
                (cond
                  (= s1 s2) (if (< r1 r2) e res)
                  (= trump s1) res
                  (= trump s2) e
                  :else res))) %))

;#150 Palindromic Numbers
(defn find-all-palindromes-gt-x
  [x]
  (letfn [(split-num
            [x]
            (let [x-str (str x)
                  len (count x-str)
                  sub-len (quot len 2)
                  r (.substring ^String x-str 0 sub-len)
                  l (.substring ^String x-str (- len sub-len) len)
                  m (if (odd? len) (.substring ^String x-str sub-len (inc sub-len)))]
              {:len len :r r :l l :m m}))
          (find-1st-palindrome-gt-given
            [x]
            (let [x-split (split-num x)
                  r (:r x-split)
                  rr (apply str (reverse r))
                  l (:l x-split)
                  m (:m x-split)
                  gt #(< 0 (.compareTo ^String % ^String %2))]
              (if (= rr l)
                x
                (if (gt rr l)
                  (Integer/valueOf (str r m rr))
                  (find-1st-palindrome-gt-given
                    (Integer/valueOf
                      (apply
                        str (inc (Integer/valueOf (str r m)))
                        (repeat (count r) 0))))))))]
    (let [palindrome (find-1st-palindrome-gt-given x)]
      (lazy-cat [palindrome]
                (find-all-palindromes-gt-x (let [p-str (split-num palindrome)]
                                             (Integer/valueOf
                                               (apply
                                                 str (inc (Integer/valueOf (str (:r p-str) (:m p-str))))
                                                 (repeat (count (:r p-str)) 0)))))))))



;#195 Parentheses... Again
 (defn gen-legal-parentheses
   [cnt]
   (letfn [(glp [[a b]]
             (cond
               (and (zero? a) (zero? b)) #{""}
               (zero? a) (set (map #(str ")" %)
                                   (glp [a (dec b)])))
               (>= a b) (set (map #(str "(" %)
                                  (glp [(dec a) b])))
               :else (set (concat (map #(str "(" %)
                                       (glp [(dec a) b]))
                                  (map #(str ")" %)
                                       (glp [a (dec b)]))))))]
     (glp [cnt cnt])))

;#168 Infinite Matrix
 (defn gen-ifnt-mtrx
   ([f m n]
    (letfn [(gen-mtrx-ij
              [f i j]
              (letfn [(gen-row [i j]
                        (lazy-cat [(f i j)]
                                  (gen-row i (inc j))))
                      (gen-column [i j]
                        (lazy-cat [(gen-row i j)]
                                  (gen-column (inc i) j)))]
                (gen-column i j)))]
      (gen-mtrx-ij f m n)))
   ([f]
     (gen-ifnt-mtrx f 0 0))
   ([f m n s t]
     (map #(take t %)
          (take s (gen-ifnt-mtrx f m n)))))

;#73 Analyze a Tic-Tac-Toe Board
 (defn gen-winner
   [m]
   (letfn [(win?
             [player xs]
             (some #(apply = player %) xs))
           (winner?
             [player]
             (when (or
                     (win? player m)
                     (win? player (apply map #(vec %&) m))
                     (win? player (let [sz (count m)]
                             [(for [i (range sz)]
                                ((m i) i))
                              (for [i (range sz)]
                                ((m i) (- sz i 1)))])))
               player))]
     (or (winner? :x)
         (winner? :o))))

;#79 Triangle Minimal Path
 (defn gen-min-sum
   [g]
   (letfn [(val-ij
             [i j]
             ((nth g i) j))
           (min-sum
             [i j]
             (if (= i (count g))
               0
               (+ (val-ij i j)
                  (min (min-sum (inc i) j)
                       (min-sum (inc i) (inc j))))))]
     (min-sum 0 0)))

;#82 Word Chains
(defn word-chains?
  [xs]
  (letfn [(dis
            [w1 w2]
            (if (or (empty? w1)
                    (empty? w2))
              (+ (count w1) (count w2))
              (let [x1 (first w1)
                    y1 (first w2)]
                (if (= x1 y1)
                  (dis (rest w1) (rest w2))
                  (inc (min (dis (rest w1) (rest w2))
                            (dis (rest w1) w2)
                            (dis w1 (rest w2))))))))
          (chains?
            ([w1 w2]
             (= 1 (dis w1 w2)))
            ([w1 w2 & ws]
              (and (chains? w1 w2)
                   (apply chains? w2 ws))))
          (ps
            [xs]
            (if (empty? xs)
              '(())
              (apply concat
                     (for [e xs]
                       (map #(concat [e] %)
                            (ps (disj xs e)))))))]
    (true? (some #(apply chains? %) (ps xs)))))

;#91 Graph Connectivity
 (defn conn?
   [edges]
   (let [bs (map #(into #{} [%])
                 (distinct (flatten (vec edges))))]
     (= 1 (count
            (reduce
              (fn [bs [x y]]
                (let [[xs ys]
                      (filter
                        #(or (% x) (% y))
                        bs)]
                  (if (nil? ys)
                    bs
                    (conj
                      (filter
                        #(not (or (% x) (% y)))
                        bs)
                      (into xs ys)))))
              bs edges)))))

;#101 Levenshtein Distance
(defn dis
  [w1 w2]
  (if (or (empty? w1)
          (empty? w2))
    (+ (count w1) (count w2))
    (let [x1 (first w1)
          y1 (first w2)]
      (if (= x1 y1)
        (dis (rest w1) (rest w2))
        (inc (min (dis (rest w1) (rest w2))
                  (dis (rest w1) w2)
                  (dis w1 (rest w2))))))))

;#94 Game of Life
 (defn next-generation
   [g]
   (letfn [(elem
             [i j]
             (nth (nth g i) j))
           (nbs
             [i j]
             (let [dirs [[1 0] [1 1] [1 -1]
                         [0 1] [0 -1]
                         [-1 1] [-1 0] [-1 -1]]
                   sz (count g)]
               (reduce
                 (fn [res [a b]]
                   (let [ni (+ i a)
                         nj (+ j b)]
                     (cond
                       (or (>= ni sz) (>= nj sz)
                           (< ni 0) (< nj 0)) res
                       (= \# (elem ni nj)) (inc res)
                       :else res)))
                 0 dirs)))]
     (let [res (atom (vec (map vec g)))
           sz (count g)]
       (doseq [i (range sz) j (range sz)]
         (let [e (elem i j)
               n (nbs i j)]
           (if (= \space e)
             (when (= 3 n)
               (swap! res assoc-in [i j] \#))
             (when-not (or (= 2 n) (= 3 n))
               (swap! res assoc-in [i j] \space)))))
       (vec (map #(apply str %) @res)))))

 #_(fn
   []
   (let [q \"
         src
         [" (fn"
          "   []"
          "   (let [q \""
          ""]]))

;#119 Win at Tic-Tac-Toe
 (defn make-winner
   [player g]
   (letfn [(win?
             [player xs]
             (some #(apply = player %) xs))
           (winner?
             [player g]
             (or
               (win? player g)
               (win? player (apply map #(vec %&) g))
               (win? player (let [sz (count g)]
                              [(for [i (range sz)]
                                 ((g i) i))
                               (for [i (range sz)]
                                 ((g i) (- sz i 1)))]))))]
     (let [rs (range (count g))]
       (reduce
         (fn [res [g pos]]
           (if (winner? player g)
             (conj res pos)
             res))
         #{} (keep
               #(when (= :e (get-in g %))
                 [(assoc-in g % player) %])
               (for [x rs y rs] [x y]))))))

;#117 For Science!
 (defn path?
   [g]
   (let [r (count g)
         c (count (first g))]
     (letfn [(loc [ch g]
               (let [ps (for [x (range r)
                              y (range c)]
                          [x y])]
                 (loop [ps ps]
                   (let [[x y] (first ps)]
                     (if (= ch (nth (nth g x) y))
                       [x y]
                       (recur (rest ps)))))))
             (find-path
               [[x y] a-g]
               (condp = (nth (nth @a-g x) y)
                 \C true
                 \# false
                 (do
                   (swap! a-g assoc-in [x y] \#)
                   (some true?
                         (map #(find-path % a-g)
                              (keep
                                (fn [[dx dy]]
                                  (let [nx (+ x dx)
                                        ny (+ y dy)]
                                    (if (and
                                          (>= nx 0) (< nx r)
                                          (>= ny 0) (< ny c))
                                      [nx ny])))
                                [[-1 0] [1 0]
                                 [0 -1] [0 1]]))))))]
       (true?
         (find-path (loc \M g)
                    (atom (vec (map vec g))))))))

 (defn fps
   [x]
   (letfn [(val-of [^String s]
             (if (empty? s)
               0
               (Long/parseLong s)))
           (fnp [x]
             (let [x-str (str x)
                   len (count x-str)
                   hf (quot len 2)
                   ten-m (apply * (repeat hf 10))

                   l (quot x ten-m)
                   r (rem x ten-m)
                   rl (val-of (clojure.string/reverse
                                (.substring (str l) 0 hf)))]
               #_(prn len hf l r rl)
               (if (= rl r)
                 x
                 (if (> rl r)
                   (+ (* l ten-m) rl)
                   (+ (* (inc l) ten-m)
                      (val-of (clojure.string/reverse
                                (.substring (str (inc l)) 0 hf))))))))
           (all-ps [x]
             (let [p (fnp x)]
               (lazy-cat [p] (all-ps (inc p)))))]
     (all-ps x)))

(=
  (take 26 (fps 0))
  [0 1 2 3 4 5 6 7 8 9
   11 22 33 44 55 66 77 88 99
   101 111 121 131 141 151 161])

;Tree reparenting
(defn gen-tree
  [nr tree]
  (let [zp (clojure.zip/seq-zip tree)
        pnodes (:pnodes (second
                          (first
                            (filter
                              (fn [[nd :as loc]]
                                (= nr nd))
                              (iterate clojure.zip/next zp)))))]
    (reduce (fn [rs e]
              (concat e (list (remove #(= e %) rs))))
            pnodes)))

;Analyze Reversi

(defn analyze-reversi
  [graph color]
  (let [size (count graph)
        pos-seq (for [x (range 0 size)
                      y (range 0 size)]
                  [x y])
        dirs (remove
               (fn [[x y]]
                 (and (zero? x) (zero? y)))
               (for [x [-1 0 1]
                     y [-1 0 1]]
                 [x y]))
        legal-pos? (fn [x y]
                     (and (>= x 0) (< x size)
                          (>= y 0) (< y size)))
        reverse-pos-on-one-dir
        (fn [[x y] [dx dy]]
           (if (not= 'e (get-in graph [x y]))
             [[x y] #{}]
             (loop [nx (+ x dx) ny (+ y dy) res #{}]
               (if (legal-pos? nx ny)
                 (if (= color (get-in graph [nx ny]))
                   [[x y] res]
                   (if (= 'e (get-in graph [nx ny]))
                     [[x y] #{}]
                     (recur (+ nx dx) (+ ny dy) (conj res [nx ny]))))
                 [[x y] #{}]))))
        reverse-pos-on-all-dirs
        (fn [[x y]]
          (reduce (fn [[pos1 ps1] [pos2 ps2]]
                    [pos1 (clojure.set/union ps1 ps2)])
                  (map (fn [dir]
                         (reverse-pos-on-one-dir [x y] dir)) dirs)))]
    (into {}
          (filter (fn [[pos ps]]
                    (not (empty? ps)))
                  (map reverse-pos-on-all-dirs
                       pos-seq)))))


