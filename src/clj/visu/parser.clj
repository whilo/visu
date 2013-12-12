(ns visu.parser
  (:refer-clojure :exclude [replace])
  (:require [clojure.core :as core]
            [clojure.string :refer [split replace blank? lower-case]]))

(def special-characters [";" "," "\\." "!" "\\?" ":" "\\*" "\""  "'" "_" "-" "\\)" "\\(" "\r" "\\]" "\\["])
(def stopwords (into #{} (split (slurp "data/english_stopwords.txt") #",")))


(defn- remove-special-chars [word special-chars-list]
  (if-not (seq special-chars-list)
    (lower-case word)
    (recur (replace word (re-pattern (first special-chars-list)) "") (rest special-chars-list))))


(defn get-text-frequencies [path]
  (->> (split (slurp path) #"\n")
       (map #(split % #" "))
       flatten
       (map #(remove-special-chars % special-characters))
       (remove blank?)
       (remove stopwords)
       frequencies))


(defn get-adjacency-list [path]
  "Parse adjacency list and convert to readable form"
  (let [raw-data (split (slurp path) #"\n")
        edges (map #(split % #" ") raw-data)
        keyword-sequence (range 1 (inc (count raw-data)))]
    (->> (into {} (map vector keyword-sequence edges))
         (map (fn [x] (map #(into #{} [(key x) %]) (val x))))
         flatten
         (apply vector))))


;; live coding vars
#_(get-adjacency-list "data/graph.adjl")
