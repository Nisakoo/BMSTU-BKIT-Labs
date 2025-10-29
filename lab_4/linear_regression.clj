(defn scalar-product [x y]
    (reduce + (mapv * x y)))

(defn vec-sum [x y]
    (mapv + x y))

(defn vec-mul [x y]
    (mapv * x y))

(defn vec-scalar-mul [num x]
    (mapv #(* % num) x))

(defn tranpose [mat]
    (apply mapv vector mat))

(defn dot-matrix-vec [mat vec]
    (mapv #(scalar-product % vec) mat))

(defn gradient [features weights target]
    (let [predicted (dot-matrix-vec features weights)]
    (vec-scalar-mul (/ 2 (count target))
                    (dot-matrix-vec (tranpose features) (mapv - predicted target)))))

(defn update-weights [weights gradient learning-rate]
    (mapv #(- %1 (* learning-rate %2)) weights gradient))

(defn transform-features [features]
    (mapv #(vector 1 %) features))

(defn fit [features target learning-rate epoches]
    (let [transformed-features (transform-features features)]
    (reduce
        (fn [weights _]
            (update-weights weights (gradient transformed-features weights target) learning-rate))
    [1 1]
    (range epoches))))

(def U [9.0 13.2 14.8 15.7 16.3 16.7 17.0 17.1])
(def I [0.17 0.09 0.06 0.04 0.03 0.02 0.01 0.01])

(println (fit I U 0.01 9100))