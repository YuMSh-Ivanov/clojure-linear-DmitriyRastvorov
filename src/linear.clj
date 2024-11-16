(ns linear)

(defn $& [op]
    (fn [& args]
        (apply mapv op args)))

(def v+ ($& +))
(def v- ($& -))
(def v* ($& *))
(def vd ($& /))

(def m+ ($& v+))
(def m- ($& v-))
(def m* ($& v*))
(def md ($& vd))

(defn dot [& vectors]
    (if (== (count vectors) 0)
        0
        (reduce + (apply v* vectors))))

(defn $*s [op]
    (fn [arg & scalars]
        (mapv #(op % (reduce * scalars)) arg)))

(def v*s ($*s *))
(def m*s ($*s v*s))

(defn transpose [matrix]
    (apply mapv vector matrix))

(defn m*v [matrix vector]
    (mapv #(dot % vector) matrix))

(defn m*m_reducing_function [matrix1 given_matrix2]
    (def matrix2 (transpose (vec (map vec given_matrix2))))
    (mapv #(m*v matrix2 %) matrix1))

(defn m*m [& matrices]
    (reduce m*m_reducing_function matrices))
