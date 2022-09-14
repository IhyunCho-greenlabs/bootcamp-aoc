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
  "parse file input to data record which including initialization
   input: string file
   output: boot codes vector
   {:line 0 :acc 0 :visited #{} 
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
    {:line 0 :acc 0 :visited #{} :record result}))


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


(if (= 4 5) 1 3)

;part 2



(defn test-fixing-one-instruction
  [input]
  (let [program {:line 0 :acc 0 :visited #{} :record input}
        last-line (dec (count input))
        result (->> program
                    (iterate execute-operation)
                    (take-while #(or (not-contains? (:visited %) (:line %)) (= (:line %) last-line)))
                    last)]
    (if (= (:line result) last-line)
      (:acc result)
      nil)))

(defn parse-file-to-vec-record
  "parse file input to data record
   input: string file
   output: boot codes vector
   ({:op \"nop\", :value 0}
   {:op \"acc\", :value 1}
   {:op \"jmp\", :value 4}..
   "
  [input]
  (->>  input
        io/resource
        slurp
        string/split-lines
        (map make-bootcode-map)
        vec))

(defn make-test-codes
  [input]
  (for [fix-target (range 9)
        :let [fix-target-code (nth input fix-target)]
        :when (and (not= (:op fix-target-code) "acc")
                   (not (and (= (:op fix-target-code) "nop") (= (:value fix-target-code) 0))))] ;never change nop 0 code
    (update-in input [fix-target :op]
               #(if (= "nop" %) "jmp" "nop"))))


(comment
  (->> "2020_8_sample.txt"
       ;parse
       parse-file-to-vec-record
       make-test-codes

       (drop-while #(nil? (test-fixing-one-instruction %)))
      ;(map test-fixing-one-instruction )
       )
       ;process
  )

