(ns aoc2020_8
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def op-regex #"(\S+) (\S+)")


(defn make-bootcode-map
  "make a bootcode map indicating operation and its value
   input: \"nop +0\"
   output: {:op \"nop\", :value 0}
   "
  [input]
  (let [[_ op value] (re-find op-regex input)]
    {:op op
     :value (Integer/parseInt value)}))

(defn initial-state
  [input]
  {:line 0 :acc 0 :visited #{} :record input})

(defn parse-file-to-record
  "parse file input to data record
   input: string file
   output: boot codes vector
   ({:op \"nop\", :value 0}
   {:op \"acc\", :value 1}
   {:op \"jmp\", :value 4}..
   "
  [input]
  (let [result (->>  input
         io/resource
         slurp
         string/split-lines
         (map make-bootcode-map))]
    {:line 0 :acc 0 :visited #{} :record result}
    ))


(defn execute-operation
  "execute one line of operation using the line number 
   input:
   {:line 0,
    :acc 0,
    :visited #{},
    :record
     ({:op \"nop\", :value 0}
     {:op \"acc\", :value 1}...
   output:
    {:line 1,
     :acc 0,
     :visited #{0},
     :record
     ({:op \"nop\", :value 0}
     {:op \"acc\", :value 1}...
   "
  [program] 
  (let [{:keys [line acc visited record]} program
        code (nth record line)
        op (:op code)
        val (:value code)
        visited-update (conj visited line)]
    (case op
      "nop" {:line (inc line) :acc acc :visited visited-update :record record}
      "acc" {:line (inc line) :acc (+ acc val) :visited visited-update :record record}
      "jmp" {:line (+ line val) :acc acc :visited visited-update :record record})))


;;vector를 사용하면 안됨. contains?는 vector에서 다른 목적으로 동작함..!!!!
(def not-contains? (complement contains?))

(comment
  (->> "2020_8_sample.txt"
       ;parse
       parse-file-to-record
       ;process
       (iterate execute-operation)
       (take-while #(not-contains? (:visited %) (:line %)))
       ;aggregate
       last
       :acc))

