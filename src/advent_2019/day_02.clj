(ns advent-2019.day-02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def input (-> (io/resource "advent_2019/day_02/input")
               slurp
               str/trim
               (str/split #",")
               (as-> xs (mapv #(Integer/parseInt %) xs))))

(defn exec [memory i f]
  (let [i1 (memory (+ i 1))
        i2 (memory (+ i 2))
        i3 (memory (+ i 3))
        out (f (memory i1) (memory i2))]
    (assoc memory i3 out)))

(defn run* [memory i]
  (let [op (memory i)
        i' (+ i 4)]
    (case op
      1 (recur (exec memory i +) i')
      2 (recur (exec memory i *) i')
      99 memory)))

(defn run [memory noun verb]
  (first (run* (assoc memory 1 noun 2 verb) 0)))

(defn part-1 []
  (run input 12 2))

(defn part-2 []
  (loop [[[noun verb] & more] (for [noun (range 100)
                                    verb (range 100)]
                                [noun verb])]
    (if (= (run input noun verb) 19690720)
      (+ (* 100 noun) verb)
      (recur more))))

(comment
  (= (part-1) 5110675)
  (= (part-2) 4847))
