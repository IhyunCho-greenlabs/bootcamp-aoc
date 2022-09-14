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
        code (if (nil? (get record line))
               {:op nil :value nil}
               (get record line))
        {:keys [op value]} code
        visited-updated-program (update program :visited #(conj % line))]
    
    (if (nil? op)
      (update visited-updated-program :line inc) ; nil control
      (case op ;;update로 잘 보이게 ~~ 
        "nop" (merge visited-updated-program {:line (inc line) })
        "acc" (merge visited-updated-program {:line (inc line) :acc (+ acc value)}) 
        "jmp" (merge visited-updated-program {:line (+ line value)})))))

;;vector를 사용하면 안됨. contains?는 vector에서 다른 목적으로 동작함..!!!!
(def not-contains? (complement contains?))
;; 이것은 (not (contains? something)) 을 쓰는게 더 좋은거 같아요!

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


;part 2



(defn test-fixing-one-instruction
  "validate fixed program whether the program can reach to the end of code
   input: 
    [{:op \"nop\", :value 0}
   {:op \"acc\", :value 1}
   {:op \"jmp\", :value 4}..
   output: nil
   "
  [input]
  (let [program {:line 0 :acc 0 :visited #{} :record input}
        last-line (count input)
        result (->> program
                    (iterate execute-operation) 
                    (take-while #(and (<= (:line %) last-line)
                                      (not-contains? (:visited %) (:line %))))
                    last)]
    (if (= (:line result) last-line)
      (:acc result)
      nil) 
    ))


(defn parse-file-to-vec-record
  "parse file input to data record
   input: string file
   output: boot codes vector
   [{:op \"nop\", :value 0} {:op :nop :value 10}
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
  "make a list of test codes which converted one target instruction
   input:
  [{:op \"nop\", :value 0}
   {:op \"acc\", :value 1}
   {:op \"jmp\", :value 4}..
   output:
   [{:op \"nop\", :value 0} ;skip nop 0
   {:op \"acc\", :value 1} ;skip acc
   {:op \"nop\", :value 4}.. ;changed jmp -> nop
   "
  [input]
  (for [fix-target (range (count input))
        :let [fix-target-code (nth input fix-target)]
        :when (and (not= (:op fix-target-code) "acc")
                   (not (and (= (:op fix-target-code) "nop") 
                             (= (:value fix-target-code) 0))))] ;never change nop 0 code
    (update-in input [fix-target :op]
               #(if (= "nop" %) "jmp" "nop"))))


(comment
  (->> "2020_8_sample.txt"
       ;parse
       parse-file-to-vec-record
       make-test-codes

        ;process 
       (some test-fixing-one-instruction)))
