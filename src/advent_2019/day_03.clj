(ns advent-2019.day-03
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(def wires (->> (io/resource "advent_2019/day_03/input")
                slurp
                str/split-lines
                (map #(str/split % #","))))

(def central-port {:x 0 :y 0})

(defn parse-delta [s]
  (let [c (first s)]
    {:axis (if (#{\L \R} c) :x :y)
     :sign (if (#{\U \R} c) + -)
     :length (Integer/parseInt (subs s 1))}))

(defn wire->points [wire]
  (reduce (fn [points s]
            (let [{:keys [axis sign length]} (parse-delta s)
                  last-point (peek points)
                  start (sign (axis last-point) 1)
                  end (sign start length)
                  step (sign 1)]
              (into points
                    (map (fn [coord] (assoc last-point axis coord)))
                    (range start end step))))
          [{:x 0 :y 0}]
          wire))

(defn manhattan-distance [p1 p2]
  (+ (Math/abs (- (:x p1) (:x p2)))
     (Math/abs (- (:y p1) (:y p2)))))

(defn part-1 []
  (let [intersections (->> wires
                           (map (comp set wire->points))
                           (apply set/intersection))]
    (->> (disj intersections central-port)
         (map #(manhattan-distance central-port %))
         (apply min))))

(defn steps [point & paths]
  (reduce + (map #(.indexOf % point) paths)))

(defn part-2 []
  (let [paths (map wire->points wires)
        intersections (apply set/intersection (map set paths))]
    (->> (disj intersections central-port)
         (map #(apply steps % paths))
         (apply min))))

(comment
  ;; This could be optimized using line segments instead of points.
  (= (part-1) 308)
  (= (part-2) 12934))
