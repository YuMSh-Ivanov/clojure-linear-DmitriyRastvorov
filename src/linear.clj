(ns linear)

(defn basic_operation [op]
    (fn [& args]
        (apply mapv op args)))

(def v+ (basic_operation +))
(def v- (basic_operation -))
(def v* (basic_operation *))
(def vd (basic_operation /))

(def m+ (basic_operation v+))
(def m- (basic_operation v-))
(def m* (basic_operation v*))
(def md (basic_operation vd))

(defn dot [& vectors]
    (if (empty? vectors)
        0
        (apply + (apply v* vectors))))

(defn type*s [op]
    (fn [arg & scalars]
        (mapv #(op % (apply * scalars)) arg)))

(def v*s (type*s *))
(def m*s (type*s v*s))

(defn transpose [matrix]
    (apply mapv vector matrix))

(defn m*v [matrix vector]
    (mapv #(dot % vector) matrix))

(defn m*m_reducing_function [matrix1 given_matrix2]
    (let [matrix2 (transpose (mapv vec given_matrix2))]
    (mapv (partial m*v matrix2) matrix1)))

(defn m*m [& matrices]
    (reduce m*m_reducing_function matrices))
