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



