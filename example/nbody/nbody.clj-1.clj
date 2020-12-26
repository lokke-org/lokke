;; Author: Andy Fingerhut (andy_fingerhut@alum.wustl.edu)
;; Date: Aug 1, 2009

(ns nbody
  (:gen-class)
;;  (:use [clojure.contrib.seq-utils :only (flatten)])
  (:use [clojure.contrib.pprint :only (pprint)]))

(set! *warn-on-reflection* true)


(defn vec-add [v1 v2]
  {:x (+ (:x v1) (:x v2)),
   :y (+ (:y v1) (:y v2)),
   :z (+ (:z v1) (:z v2))})


(defn vec-sub [v1 v2]
  {:x (- (:x v1) (:x v2)),
   :y (- (:y v1) (:y v2)),
   :z (- (:z v1) (:z v2))})


(defn vec-times-scalar [v scalar]
  {:x (* scalar (:x v)),
   :y (* scalar (:y v)),
   :z (* scalar (:z v))})


(defn vec-dot-product [v1 v2]
  (+ (* (:x v1) (:x v2))
     (* (:y v1) (:y v2))
     (* (:z v1) (:z v2))))


(defn offset-momentum [bodies]
  (reduce vec-add
          (map #(vec-times-scalar (:velocity %) (:mass %))
               bodies)))


(defn n-body-system []
  (let [PI (double 3.141592653589793)
        SOLAR-MASS (double (* (double 4) PI PI))
        DAYS-PER-YEAR (double 365.24)
        bodies
        [ {:name "sun"
           :mass SOLAR-MASS
           :pos {:x 0 :y 0 :z 0}
           :velocity {:x 0 :y 0 :z 0}}
          {:name "jupiter"
           :mass (double (* 9.54791938424326609e-04 SOLAR-MASS))
           :pos {:x (double  4.84143144246472090e+00)
                 :y (double -1.16032004402742839e+00)
                 :z (double -1.03622044471123109e-01)}
           :velocity {:x (double (*  1.66007664274403694e-03 DAYS-PER-YEAR))
                      :y (double (*  7.69901118419740425e-03 DAYS-PER-YEAR))
                      :z (double (* -6.90460016972063023e-05 DAYS-PER-YEAR))}}
          {:name "saturn"
           :mass (double (* 2.85885980666130812e-04 SOLAR-MASS))
           :pos {:x (double  8.34336671824457987e+00)
                 :y (double  4.12479856412430479e+00)
                 :z (double -4.03523417114321381e-01)}
           :velocity {:x (double (* -2.76742510726862411e-03 DAYS-PER-YEAR))
                      :y (double (*  4.99852801234917238e-03 DAYS-PER-YEAR))
                      :z (double (*  2.30417297573763929e-05 DAYS-PER-YEAR))}}
          {:name "uranus"
           :mass (double (* 4.36624404335156298e-05 SOLAR-MASS))
           :pos {:x (double  1.28943695621391310e+01)
                 :y (double -1.51111514016986312e+01)
                 :z (double -2.23307578892655734e-01)}
           :velocity {:x (double (*  2.96460137564761618e-03 DAYS-PER-YEAR))
                      :y (double (*  2.37847173959480950e-03 DAYS-PER-YEAR))
                      :z (double (* -2.96589568540237556e-05 DAYS-PER-YEAR))}}
          {:name "neptune"
           :mass (double (* 5.15138902046611451e-05 SOLAR-MASS))
           :pos {:x (double  1.53796971148509165e+01)
                 :y (double -2.59193146099879641e+01)
                 :z (double  1.79258772950371181e-01)}
           :velocity {:x (double (*  2.68067772490389322e-03 DAYS-PER-YEAR))
                      :y (double (*  1.62824170038242295e-03 DAYS-PER-YEAR))
                      :z (double (* -9.51592254519715870e-05 DAYS-PER-YEAR))}}
          ]]
    (let [sun-index 0
          init-sun-velocity (vec-times-scalar (offset-momentum bodies)
                                              (double (/ -1.0 SOLAR-MASS)))]
      (assoc-in bodies [sun-index :velocity] init-sun-velocity))))


(defn kinetic-energy-1 [body]
  (* (double 0.5) (:mass body)
     (vec-dot-product (:velocity body) (:velocity body))))


(defn kinetic-energy [bodies]
  (reduce + (map kinetic-energy-1 bodies)))


(defn distance-between [pos1 pos2]
  (let [delta-pos (vec-sub pos1 pos2)]
    (Math/sqrt (vec-dot-product delta-pos delta-pos))))


(defn all-seq-ordered-pairs [s]
  (loop [s1 (seq s)
         pairs ()]
    (if s1
      (let [s1item (first s1)]
        (recur (next s1)
               (into pairs (map (fn [s2item] [s1item s2item]) (next s1)))))
      pairs)))


(comment

(defn lazy-all-seq-ordered-pairs [s]
  (lazy-seq
    (let [s (seq s)]
      (if s
        (let [earlier-item (first s)
              next-s (seq (next s))]
          (if next-s
            (concat (map (fn [later-item] [earlier-item later-item])
                         next-s)
                    (lazy-all-seq-ordered-pairs next-s))))))))
)


(defn potential-energy-body-pair [[b1 b2]]
  (let [distance (distance-between (:pos b1) (:pos b2))]
    (/ (* (:mass b1) (:mass b2))
       distance)))


(defn potential-energy [bodies]
  (- (reduce + (map potential-energy-body-pair
                    (all-seq-ordered-pairs bodies)))))


(defn energy [bodies]
  (+ (kinetic-energy bodies) (potential-energy bodies)))


(defn delta-velocities-for-body-pair [b1 b2 delta-t]
  (let [delta-pos (vec-sub (:pos b1) (:pos b2))
        dist-squared (vec-dot-product delta-pos delta-pos)
        dist (Math/sqrt dist-squared)
        mag (/ delta-t dist-squared dist)
        b1-delta-v (vec-times-scalar delta-pos (* (double -1.0) mag (:mass b2)))
        b2-delta-v (vec-times-scalar delta-pos (* mag (:mass b1)))]
    [ b1-delta-v b2-delta-v ]))


(defn bodies-new-velocities [bodies delta-t]
;;  (printf "bodies-new-velocities: (class bodies)='%s' delta-t=%.3f\n"
;;          (class bodies) delta-t)
  (loop [n 0
         velocity-changes
         (apply concat
                (map (fn [[i1 i2]]
                       (let [[delta-v1 delta-v2]
                             (delta-velocities-for-body-pair (bodies i1)
                                                             (bodies i2)
                                                             delta-t)]
                         [[i1 delta-v1] [i2 delta-v2]]))
                     (all-seq-ordered-pairs (range (count bodies)))))
         bodies bodies
         ]
;;    (when (== n 0)
;;      (println "bodies-new-velocities B: velocity-changes =")
;;      (pprint velocity-changes))
    (let [changes (seq velocity-changes)]
      (if changes
        (let [[index delta-v] (first changes)]
          (recur (inc n)
                 (next changes)
                 (assoc bodies index
                        (merge-with vec-add (bodies index) {:velocity delta-v}))))
        ;; else
        bodies))))


(defn bodies-new-positions [bodies delta-t]
  (vec (map (fn [body]
              (assoc body :pos
                     (vec-add (:pos body)
                              (vec-times-scalar (:velocity body) delta-t))))
            bodies)))


(defn advance [bodies delta-t]
  (let [bodies-with-new-velocities (bodies-new-velocities bodies delta-t)]
    (bodies-new-positions bodies-with-new-velocities delta-t)))


(defn usage [exit-code]
  (printf "usage: %s n\n" *file*)
  (printf "    n, a positive integer, is the number of simulation steps to run\n")
  (flush)
  (. System (exit exit-code)))


(defn -main [& args]
  (when (not= (count args) 1)
    (usage 1))
  (when (not (re-matches #"^\d+$" (nth args 0)))
    (usage 1))
  (def n (. Integer valueOf (nth args 0) 10))
  (when (< n 1)
    (usage 1))
  (let [bodies (n-body-system)
        delta-t 0.01]
    (printf "%.9f\n" (energy bodies))
    (loop [i 0
           bodies bodies]
;;      (printf "%d %.9f\n" i (energy bodies))
;;      (pprint bodies)
;;      (when (== 0 (rem i 10000))
;;        (printf "%d\n" i))
      (if (== i n)
        (printf "%.9f\n" (energy bodies))
        (recur (inc i) (advance bodies delta-t)))))
  (flush))
