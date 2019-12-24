(ns advent-2019.day-01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (->> (io/resource "advent_2019/day_01/input")
                slurp
                str/split-lines
                (map #(Integer/parseInt %))))

(defn fuel-required [mass]
  (- (quot mass 3) 2))

(defn part-1 []
  (reduce + (map fuel-required input)))

(defn total-fuel-required [mass]
  (->> (iterate fuel-required mass)
       rest
       (take-while pos?)
       (reduce +)))

(defn part-2 []
  (reduce + (map total-fuel-required input)))

(comment
  (= (part-1) 3406432)
  (= (part-2) 5106777))
