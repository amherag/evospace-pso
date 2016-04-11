(ns evospace-pso.core
  (:gen-class)
  (:require [pso.swarm :as swarm]
            [pso.dimension :as dimension]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            )
  (:use clojure.walk)
  (:import generic.Input
           generic.Output
           generic.Tuple
           tools.JMathPlotter
           java.util.TreeMap
           
           type1.sets.T1MF_Interface
           type1.sets.T1MF_Triangular
           type1.sets.T1MF_Gaussian
           type1.system.T1_Antecedent
           type1.system.T1_Consequent
           type1.system.T1_Rule
           type1.system.T1_Rulebase
           
           intervalType2.sets.IntervalT2MF_Interface
           intervalType2.sets.IntervalT2MF_Triangular
           intervalType2.sets.IntervalT2MF_Gaussian
           intervalType2.system.IT2_Antecedent
           intervalType2.system.IT2_Consequent
           intervalType2.system.IT2_Rule
           intervalType2.system.IT2_Rulebase
           
           generalType2zSlices.sets.GenT2zMF_Interface
           generalType2zSlices.sets.GenT2zMF_Triangular
           generalType2zSlices.system.GenT2z_Antecedent
           generalType2zSlices.system.GenT2z_Consequent
           generalType2zSlices.system.GenT2z_Rule
           generalType2zSlices.system.GenT2z_Rulebase
           generalType2zSlices.system.GenT2zEngine_Defuzzification
           generalType2zSlices.system.GenT2zEngine_Intersection
           generalType2zSlices.system.GenT2zEngine_Union))

;; Fuzzy systems used in Fuzzy PSO (Melin P. et al., 2013)

(defn fpso1 [input]
  (let [iterations (new Input "Iterations" (new Tuple 0 1))
        diversity (new Input "Diversity" (new Tuple 0 1))
        c1 (new Output "c1" (new Tuple 0 3))
        c2 (new Output "c2" (new Tuple 0 3))
        rulebase (new T1_Rulebase 9)

        ;; inputs
        low-iter-mf (new T1MF_Triangular "Low Iterations MF" -0.5 0 0.5)
        med-iter-mf (new T1MF_Triangular "Medium Iterations MF" 0 0.5 1)
        hig-iter-mf (new T1MF_Triangular "High Iterations MF" 0.5 1.0 1.5)

        low-div-mf (new T1MF_Triangular "Low Diversity MF" -0.5 0 0.5)
        med-div-mf (new T1MF_Triangular "Medium Diversity MF" 0 0.5 1)
        hig-div-mf (new T1MF_Triangular "High Diversity MF" 0.5 1.0 1.5)

        ;; outputs
        low-c1-mf (new T1MF_Triangular "Low c1 MF" 0 0.5 1)
        medlow-c1-mf (new T1MF_Triangular "MediumLow c1 MF" 0.5 1 1.5)
        med-c1-mf (new T1MF_Triangular "Medium c1 MF" 1 1.5 2)
        medhig-c1-mf (new T1MF_Triangular "MediumHigh c1 MF" 1.5 2 2.5)
        hig-c1-mf (new T1MF_Triangular "High c1 MF" 2 2.5 3)

        low-c2-mf (new T1MF_Triangular "Low c2 MF" 0 0.5 1)
        medlow-c2-mf (new T1MF_Triangular "MediumLow c2 MF" 0.5 1 1.5)
        med-c2-mf (new T1MF_Triangular "Medium c2 MF" 1 1.5 2)
        medhig-c2-mf (new T1MF_Triangular "MediumHigh c2 MF" 1.5 2 2.5)
        hig-c2-mf (new T1MF_Triangular "High c2 MF" 2 2.5 3)
        
        low-iterations (new T1_Antecedent "Low Iterations" low-iter-mf iterations)
        medium-iterations (new T1_Antecedent "Medium Iterations" med-iter-mf iterations)
        high-iterations (new T1_Antecedent "High Iterations" hig-iter-mf iterations)

        low-diversity (new T1_Antecedent "Low Diversity" low-div-mf diversity)
        medium-diversity (new T1_Antecedent "Medium Diversity" med-div-mf diversity)
        high-diversity (new T1_Antecedent "High Diversity" hig-div-mf diversity)

        low-c1 (new T1_Consequent "CT" low-c1-mf c1)
        mediumlow-c1 (new T1_Consequent "CT6" medlow-c1-mf c1)
        medium-c1 (new T1_Consequent "CT" med-c1-mf c1)
        mediumhigh-c1 (new T1_Consequent "CT6" medhig-c1-mf c1)
        high-c1 (new T1_Consequent "CT6" hig-c1-mf c1)

        low-c2 (new T1_Consequent "CT" low-c2-mf c2)
        mediumlow-c2 (new T1_Consequent "CT6" medlow-c2-mf c2)
        medium-c2 (new T1_Consequent "CT" med-c2-mf c2)
        mediumhigh-c2 (new T1_Consequent "CT6" medhig-c2-mf c2)
        high-c2 (new T1_Consequent "CT6" hig-c2-mf c2)

        rule1 (new T1_Rule (into-array [low-iterations low-diversity]) (into-array [high-c1 low-c2]))
        rule2 (new T1_Rule (into-array [low-iterations medium-diversity]) (into-array [mediumhigh-c1 medium-c2]))
        rule3 (new T1_Rule (into-array [low-iterations high-diversity]) (into-array [mediumhigh-c1 mediumlow-c2]))
        rule4 (new T1_Rule (into-array [medium-iterations low-diversity]) (into-array [mediumhigh-c1 mediumlow-c2]))
        rule5 (new T1_Rule (into-array [medium-iterations medium-diversity]) (into-array [medium-c1 medium-c2]))
        rule6 (new T1_Rule (into-array [medium-iterations high-diversity]) (into-array [mediumlow-c1 mediumhigh-c2]))
        rule7 (new T1_Rule (into-array [high-iterations low-diversity]) (into-array [medium-c1 high-c2]))
        rule8 (new T1_Rule (into-array [high-iterations medium-diversity]) (into-array [mediumlow-c1 mediumhigh-c2]))
        rule9 (new T1_Rule (into-array [high-iterations high-diversity]) (into-array [low-c1 high-c2]))
        ]
    (.addRule rulebase rule1)
    (.addRule rulebase rule2)
    (.addRule rulebase rule3)
    (.addRule rulebase rule4)
    (.addRule rulebase rule5)
    (.addRule rulebase rule6)
    (.addRule rulebase rule7)
    (.addRule rulebase rule8)
    (.addRule rulebase rule9)
    
    (.setInput iterations (nth input 0))
    (.setInput diversity (nth input 1))

    [(.get (.evaluate rulebase 1) c1)
     (.get (.evaluate rulebase 1) c2)]
    ))

;; fpso1 testing
;;(fpso1 [1 0.5])

(defn fpso2 [input]
  (let [iterations (new Input "Iterations" (new Tuple 0 1))
        error (new Input "Error" (new Tuple 0 1))
        c1 (new Output "c1" (new Tuple 0 3))
        c2 (new Output "c2" (new Tuple 0 3))
        rulebase (new T1_Rulebase 9)

        ;; inputs
        low-iter-mf (new T1MF_Triangular "Low Iterations MF" -0.5 0 0.5)
        med-iter-mf (new T1MF_Triangular "Medium Iterations MF" 0 0.5 1)
        hig-iter-mf (new T1MF_Triangular "High Iterations MF" 0.5 1.0 1.5)

        low-err-mf (new T1MF_Triangular "Low Error MF" -0.5 0 0.5)
        med-err-mf (new T1MF_Triangular "Medium Error MF" 0 0.5 1)
        hig-err-mf (new T1MF_Triangular "High Error MF" 0.5 1.0 1.5)

        ;; outputs
        low-c1-mf (new T1MF_Triangular "Low c1 MF" 0 0.5 1)
        medlow-c1-mf (new T1MF_Triangular "MediumLow c1 MF" 0.5 1 1.5)
        med-c1-mf (new T1MF_Triangular "Medium c1 MF" 1 1.5 2)
        medhig-c1-mf (new T1MF_Triangular "MediumHigh c1 MF" 1.5 2 2.5)
        hig-c1-mf (new T1MF_Triangular "High c1 MF" 2 2.5 3)

        low-c2-mf (new T1MF_Triangular "Low c2 MF" 0 0.5 1)
        medlow-c2-mf (new T1MF_Triangular "MediumLow c2 MF" 0.5 1 1.5)
        med-c2-mf (new T1MF_Triangular "Medium c2 MF" 1 1.5 2)
        medhig-c2-mf (new T1MF_Triangular "MediumHigh c2 MF" 1.5 2 2.5)
        hig-c2-mf (new T1MF_Triangular "High c2 MF" 2 2.5 3)
        
        low-iterations (new T1_Antecedent "Low Iterations" low-iter-mf iterations)
        medium-iterations (new T1_Antecedent "Medium Iterations" med-iter-mf iterations)
        high-iterations (new T1_Antecedent "High Iterations" hig-iter-mf iterations)

        low-error (new T1_Antecedent "Low Error" low-err-mf error)
        medium-error (new T1_Antecedent "Medium Error" med-err-mf error)
        high-error (new T1_Antecedent "High Error" hig-err-mf error)

        low-c1 (new T1_Consequent "CT" low-c1-mf c1)
        mediumlow-c1 (new T1_Consequent "CT6" medlow-c1-mf c1)
        medium-c1 (new T1_Consequent "CT" med-c1-mf c1)
        mediumhigh-c1 (new T1_Consequent "CT6" medhig-c1-mf c1)
        high-c1 (new T1_Consequent "CT6" hig-c1-mf c1)

        low-c2 (new T1_Consequent "CT" low-c2-mf c2)
        mediumlow-c2 (new T1_Consequent "CT6" medlow-c2-mf c2)
        medium-c2 (new T1_Consequent "CT" med-c2-mf c2)
        mediumhigh-c2 (new T1_Consequent "CT6" medhig-c2-mf c2)
        high-c2 (new T1_Consequent "CT6" hig-c2-mf c2)

        rule1 (new T1_Rule (into-array [low-iterations low-error]) (into-array [high-c1 low-c2]))
        rule2 (new T1_Rule (into-array [low-iterations medium-error]) (into-array [mediumhigh-c1 medium-c2]))
        rule3 (new T1_Rule (into-array [low-iterations high-error]) (into-array [mediumhigh-c1 mediumlow-c2]))
        rule4 (new T1_Rule (into-array [medium-iterations low-error]) (into-array [mediumhigh-c1 mediumlow-c2]))
        rule5 (new T1_Rule (into-array [medium-iterations medium-error]) (into-array [medium-c1 medium-c2]))
        rule6 (new T1_Rule (into-array [medium-iterations high-error]) (into-array [mediumlow-c1 mediumhigh-c2]))
        rule7 (new T1_Rule (into-array [high-iterations low-error]) (into-array [medium-c1 high-c2]))
        rule8 (new T1_Rule (into-array [high-iterations medium-error]) (into-array [mediumlow-c1 mediumhigh-c2]))
        rule9 (new T1_Rule (into-array [high-iterations high-error]) (into-array [low-c1 high-c2]))
        ]
    (.addRule rulebase rule1)
    (.addRule rulebase rule2)
    (.addRule rulebase rule3)
    (.addRule rulebase rule4)
    (.addRule rulebase rule5)
    (.addRule rulebase rule6)
    (.addRule rulebase rule7)
    (.addRule rulebase rule8)
    (.addRule rulebase rule9)
    
    (.setInput iterations (nth input 0))
    (.setInput error (nth input 1))

    [(.get (.evaluate rulebase 1) c1)
     (.get (.evaluate rulebase 1) c2)]
    ))

;; fpso2 testing
;;(fpso2 [1 0.5])

(defn fpso3 [input]
  (let [iterations (new Input "Iterations" (new Tuple 0 1))
        diversity (new Input "Diversity" (new Tuple 0 1))
        error (new Input "Error" (new Tuple 0 1))
        c1 (new Output "c1" (new Tuple 0 3))
        c2 (new Output "c2" (new Tuple 0 3))
        rulebase (new T1_Rulebase 9)

        ;; inputs
        low-iter-mf (new T1MF_Triangular "Low Iterations MF" -0.5 0 0.5)
        med-iter-mf (new T1MF_Triangular "Medium Iterations MF" 0 0.5 1)
        hig-iter-mf (new T1MF_Triangular "High Iterations MF" 0.5 1.0 1.5)

        low-div-mf (new T1MF_Triangular "Low Diversity MF" -0.5 0 0.5)
        med-div-mf (new T1MF_Triangular "Medium Diversity MF" 0 0.5 1)
        hig-div-mf (new T1MF_Triangular "High Diversity MF" 0.5 1.0 1.5)

        low-err-mf (new T1MF_Triangular "Low Error MF" -0.5 0 0.5)
        med-err-mf (new T1MF_Triangular "Medium Error MF" 0 0.5 1)
        hig-err-mf (new T1MF_Triangular "High Error MF" 0.5 1.0 1.5)

        ;; outputs
        low-c1-mf (new T1MF_Triangular "Low c1 MF" 0 0.5 1)
        medlow-c1-mf (new T1MF_Triangular "MediumLow c1 MF" 0.5 1 1.5)
        med-c1-mf (new T1MF_Triangular "Medium c1 MF" 1 1.5 2)
        medhig-c1-mf (new T1MF_Triangular "MediumHigh c1 MF" 1.5 2 2.5)
        hig-c1-mf (new T1MF_Triangular "High c1 MF" 2 2.5 3)

        low-c2-mf (new T1MF_Triangular "Low c2 MF" 0 0.5 1)
        medlow-c2-mf (new T1MF_Triangular "MediumLow c2 MF" 0.5 1 1.5)
        med-c2-mf (new T1MF_Triangular "Medium c2 MF" 1 1.5 2)
        medhig-c2-mf (new T1MF_Triangular "MediumHigh c2 MF" 1.5 2 2.5)
        hig-c2-mf (new T1MF_Triangular "High c2 MF" 2 2.5 3)
        
        low-iterations (new T1_Antecedent "Low Iterations" low-iter-mf iterations)
        medium-iterations (new T1_Antecedent "Medium Iterations" med-iter-mf iterations)
        high-iterations (new T1_Antecedent "High Iterations" hig-iter-mf iterations)

        low-diversity (new T1_Antecedent "Low Diversity" low-div-mf diversity)
        medium-diversity (new T1_Antecedent "Medium Diversity" med-div-mf diversity)
        high-diversity (new T1_Antecedent "High Diversity" hig-div-mf diversity)

        low-error (new T1_Antecedent "Low Error" low-err-mf error)
        medium-error (new T1_Antecedent "Medium Error" med-err-mf error)
        high-error (new T1_Antecedent "High Error" hig-err-mf error)

        low-c1 (new T1_Consequent "CT" low-c1-mf c1)
        mediumlow-c1 (new T1_Consequent "CT6" medlow-c1-mf c1)
        medium-c1 (new T1_Consequent "CT" med-c1-mf c1)
        mediumhigh-c1 (new T1_Consequent "CT6" medhig-c1-mf c1)
        high-c1 (new T1_Consequent "CT6" hig-c1-mf c1)

        low-c2 (new T1_Consequent "CT" low-c2-mf c2)
        mediumlow-c2 (new T1_Consequent "CT6" medlow-c2-mf c2)
        medium-c2 (new T1_Consequent "CT" med-c2-mf c2)
        mediumhigh-c2 (new T1_Consequent "CT6" medhig-c2-mf c2)
        high-c2 (new T1_Consequent "CT6" hig-c2-mf c2)

        rule1 (new T1_Rule (into-array [low-iterations low-diversity low-error]) (into-array [mediumlow-c1 low-c2]))
        rule2 (new T1_Rule (into-array [low-iterations low-diversity medium-error]) (into-array [mediumhigh-c1 low-c2]))
        rule3 (new T1_Rule (into-array [low-iterations low-diversity high-error]) (into-array [high-c1 low-c2]))
        rule4 (new T1_Rule (into-array [low-iterations medium-diversity low-error]) (into-array [medium-c1 medium-c2]))
        rule5 (new T1_Rule (into-array [low-iterations medium-diversity medium-error]) (into-array [mediumhigh-c1 mediumlow-c2]))
        rule6 (new T1_Rule (into-array [low-iterations medium-diversity high-error]) (into-array [mediumhigh-c1 low-c2]))
        rule7 (new T1_Rule (into-array [low-iterations high-diversity low-error]) (into-array [low-c1 mediumlow-c2]))
        rule8 (new T1_Rule (into-array [low-iterations high-diversity medium-error]) (into-array [medium-c1 medium-c2]))
        rule9 (new T1_Rule (into-array [low-iterations high-diversity high-error]) (into-array [mediumlow-c1 low-c2]))
        rule10 (new T1_Rule (into-array [medium-iterations low-diversity low-error]) (into-array [medium-c1 medium-c2]))
        rule11 (new T1_Rule (into-array [medium-iterations low-diversity medium-error]) (into-array [mediumhigh-c1 mediumlow-c2]))
        rule12 (new T1_Rule (into-array [medium-iterations low-diversity high-error]) (into-array [mediumhigh-c1 low-c2]))
        rule13 (new T1_Rule (into-array [medium-iterations medium-diversity low-error]) (into-array [mediumlow-c1 mediumhigh-c2]))
        rule14 (new T1_Rule (into-array [medium-iterations medium-diversity medium-error]) (into-array [medium-c1 medium-c2]))
        rule15 (new T1_Rule (into-array [medium-iterations medium-diversity high-error]) (into-array [mediumhigh-c1 mediumlow-c2]))
        rule16 (new T1_Rule (into-array [medium-iterations high-diversity low-error]) (into-array [low-c1 mediumhigh-c2]))
        rule17 (new T1_Rule (into-array [medium-iterations high-diversity medium-error]) (into-array [mediumlow-c1 mediumhigh-c2]))
        rule18 (new T1_Rule (into-array [medium-iterations high-diversity high-error]) (into-array [medium-c1 medium-c2]))
        rule19 (new T1_Rule (into-array [high-iterations low-diversity low-error]) (into-array [low-c1 mediumlow-c2]))
        rule20 (new T1_Rule (into-array [high-iterations low-diversity medium-error]) (into-array [medium-c1 medium-c2]))
        rule21 (new T1_Rule (into-array [high-iterations low-diversity high-error]) (into-array [mediumlow-c1 low-c2]))
        rule22 (new T1_Rule (into-array [high-iterations medium-diversity low-error]) (into-array [low-c1 mediumhigh-c2]))
        rule23 (new T1_Rule (into-array [high-iterations medium-diversity medium-error]) (into-array [mediumlow-c1 mediumhigh-c2]))
        rule24 (new T1_Rule (into-array [high-iterations medium-diversity high-error]) (into-array [medium-c1 medium-c2]))
        rule25 (new T1_Rule (into-array [high-iterations high-diversity low-error]) (into-array [low-c1 high-c2]))
        rule26 (new T1_Rule (into-array [high-iterations high-diversity medium-error]) (into-array [low-c1 mediumhigh-c2]))
        rule27 (new T1_Rule (into-array [high-iterations high-diversity high-error]) (into-array [low-c1 mediumlow-c2]))

        ]
    (.addRule rulebase rule1)
    (.addRule rulebase rule2)
    (.addRule rulebase rule3)
    (.addRule rulebase rule4)
    (.addRule rulebase rule5)
    (.addRule rulebase rule6)
    (.addRule rulebase rule7)
    (.addRule rulebase rule8)
    (.addRule rulebase rule9)
    (.addRule rulebase rule10)
    (.addRule rulebase rule11)
    (.addRule rulebase rule12)
    (.addRule rulebase rule13)
    (.addRule rulebase rule14)
    (.addRule rulebase rule15)
    (.addRule rulebase rule16)
    (.addRule rulebase rule17)
    (.addRule rulebase rule18)
    (.addRule rulebase rule19)
    (.addRule rulebase rule20)
    (.addRule rulebase rule21)
    (.addRule rulebase rule22)
    (.addRule rulebase rule23)
    (.addRule rulebase rule24)
    (.addRule rulebase rule25)
    (.addRule rulebase rule26)
    (.addRule rulebase rule27)
    
    (.setInput iterations (nth input 0))
    (.setInput diversity (nth input 1))
    (.setInput error (nth input 2))

    [(.get (.evaluate rulebase 1) c1)
     (.get (.evaluate rulebase 1) c2)]
    ))

;; fpso3 testing
;;(fpso3 [1 1 0])

;; Benchmark Functions ;;

(def evaluations-counter 0)

(defn ackley [& X]
  (def evaluations-counter (inc evaluations-counter))
  (let [x1 (first X)
        x2 (second X)]
    (+ (* 20 (- 1 (Math/exp (* -0.2 (Math/sqrt (* 0.5 (+ (Math/pow x1 2) (Math/pow x2 2))))))))
       (- (Math/exp (* 0.5 (+ (Math/cos (* 2 Math/PI x1)) (Math/cos (* 2 Math/PI x2))))))
       (Math/exp 1))))

(defn sphere [& X]
  (def evaluations-counter (inc evaluations-counter))
  (reduce + (map #(Math/pow % 2) X)))

(defn rastrigin [& X]
  (def evaluations-counter (inc evaluations-counter))
  (+ (reduce + (map (fn [x]
                      (- (Math/pow x 2) (* 10 (Math/cos (* 2 Math/PI x))))) X))
     (* 10 (count X))))

(defn griewank [& X]
  (def evaluations-counter (inc evaluations-counter))
  (+ 1.0 (- (reduce + (map (fn [x] (/ (Math/pow x 2) 4000)) X))
            (reduce * (map-indexed (fn [idx x] (Math/cos (/ x (Math/sqrt (inc idx))))) X)))))

(defn schaffer2 [& X]
  (def evaluations-counter (inc evaluations-counter))
  (let [x1 (first X)
        x2 (second X)]
    (+ 0.5 (/ (- (* (Math/pow (Math/sin (- (Math/pow x1 2) (Math/pow x2 2))) 2)) 0.5)
              (Math/pow (+ 1.0 (* 0.001 (+ (Math/pow x1 2) (Math/pow x2 2)))) 2)))))

;; Main ;;

(def ackley-d [(pso.dimension/build :numerator -5.0 5.0)
               (pso.dimension/build :denominator -5.0 5.0)])

(def sphere-d [(pso.dimension/build :numerator -5.0 5.0)
               (pso.dimension/build :denominator -5.0 5.0)])

;; Redefining PSO implementation

(defn calculate-velocity [particle best-particle
                          vel-mult pos-mult swarm-pos-mult]
  ;; Original implementation was using random pos-mult and swarm-pos-mult
  ;; Example: if pos-mult = 3, then (rand pos-mult) or (rand 3) was used as final pos-mult
  (let [v1 (pso.util/nv* vel-mult (:velocity particle))
        v2 (pso.util/nv* pos-mult
                         (pso.util/v- (get-in particle [:best-result :position])
                                      (get-in particle [:result :position])))
        v3 (pso.util/nv* swarm-pos-mult
                         (pso.util/v- (get-in best-particle [:result :position])
                                      (get-in particle [:result :position])))]
    (pso.util/v+ v1 v2 v3)))

(defn transform [{:keys [velocity result best-result vel-mult pos-mult swarm-pos-mult] :as particle}
                 {:keys [dimensions best-particle fitness-fn comparator-fn] :as swarm}]
  (let [new-position (->> (pso.util/v+ (:position result) velocity)
                          (mapv min (map :maximum dimensions) ,,,)
                          (mapv max (map :minimum dimensions) ,,,))
        new-result {:position new-position :value (apply fitness-fn new-position)}]
    (assoc particle
           :velocity (calculate-velocity particle best-particle vel-mult pos-mult swarm-pos-mult)
           :result new-result
           :last-result result
           :best-result (first (sort-by :value comparator-fn [new-result best-result])))))

(letfn [(value-fn [v] (get-in v [:result :value]))
        (best-fn [particles comparator-fn]
          (first (sort-by value-fn comparator-fn particles)))]

  (defn update [{:keys [best-particle particles comparator-fn] :as swarm}]
    (let [new-particles (pmap #(transform % swarm) particles)]
      (assoc swarm
             :particles new-particles
             :best-particle (best-fn particles comparator-fn)
             )))

  (defn build [dimensions fitness-fn comparator-fn size c1 c2]
    (let [swarm {:fitness-fn fitness-fn
                 :comparator-fn comparator-fn
                 :dimensions dimensions}
          particles (repeatedly size #(pso.particle/build swarm :vel-mult 1 :pos-mult c1 :swarm-pos-mult c2 :initial-vel-mult 1))]
      (assoc swarm
             :particles particles
             :best-particle (best-fn particles comparator-fn)))))


;; Main Program

(defn update-pos-mults
  "Used to update :pos-mult and :swarm-pos-mult in the particles according to the fuzzy systems."
  [{:keys [particles comparator-fn] :as swarm}
   c1 c2]
  (let [new-particles (map (fn [particle]
                             (assoc particle
                                    :pos-mult c1
                                    :swarm-pos-mult c2)) particles)]
    (assoc swarm
           :particles new-particles)))

(defn get-best-particle [{:keys [particles comparator-fn] :as swarm}]
  (letfn [(value-fn [v] (get-in v [:best-result :value]))
          (best-fn [ps comparator-fn]
            (first (sort-by value-fn comparator-fn ps)))]
    (best-fn particles comparator-fn)))

(defn calc-error
  "Error between each particle in the swarm and the best particle."
  [{:keys [best-particle particles] :as swarm}]
  (let [bp-result (:value (:result best-particle))
        p-results (map (fn [particle]
                         (:value (:result particle))) particles)
        errors (map #(- % bp-result) p-results)
        error (/ (reduce + errors)
                 (count p-results))
        min-error (apply min errors)
        max-error (apply max errors)]
    (if (= min-error max-error)
      1
      (/ (- error
            min-error)
         (- max-error min-error)))))

(defn calc-diversity
  "As is explained in Melin P. et al. (2013)."
  [{:keys [best-particle particles] :as swarm}]
  (let [bp-result (:position (:result best-particle))
        p-results (map (fn [particle]
                         (:position (:result particle))) particles)
        diversities (map (fn [p-result]
                           (Math/sqrt (reduce + (map #(Math/pow (- %1 %2) 2)
                                                     bp-result p-result))))
                         p-results)
        diversity (/ (reduce + diversities)
                     (count p-results))
        min-diversity (apply min diversities)
        max-diversity (apply max diversities)]
    (if (= min-diversity max-diversity)
      0
      (/ (- diversity
            min-diversity)
         (- max-diversity min-diversity)))
    ))

;; EvoSpace Interface ;;
(defn swarm-to-init [{:keys [particles] :as swarm}]
  (json/write-str {:sample_id nil
                   :sample (map (fn [particle]
                                  {:score (get-in particle [:result :value])
                                   :fitness (get-in particle [:result :value])
                                   :chromosome
                                   particle}) particles)}))

;;(swarm-to-init gswarm)
(defn evospace-get-lowest []
  (let [particle (keywordize-keys
                  (json/read-str
                   (first (get (json/read-str
                                (:body (client/get "http://localhost:3000/evospace/pop/zrange/0/0")))
                               "sample"))))]
    {:id (get particle :id)
     :fitness (get particle :fitness)
     :score (get particle :score)
     :velocity (get-in particle [:chromosome :velocity])
     :result (get-in particle [:chromosome :result])
     :last-result (get-in particle [:chromosome :last-result])
     :best-result (get-in particle [:chromosome :best-result])
     :vel-mult (get-in particle [:chromosome :vel-mult])
     :pos-mult (get-in particle [:chromosome :pos-mult])
     :swarm-pos-mult (get-in particle [:chromosome :swarm-pos-mult])
     }
    ))

(defn sample-to-swarm [sample swarm]
  (let [comparator-fn (get swarm :comparator-fn)
        particles (map (fn [new-particle particle]
                         (assoc new-particle
                                :score (get-in new-particle [:result :value])
                                :fitness (get-in new-particle [:result :value])
                                :id (get particle :id)))
                       (map :chromosome (get sample :sample))
                       (get sample :sample))
        best-particle (evospace-get-lowest)]
    (assoc swarm :particles particles
           :best-particle best-particle
           :sample_id (get sample :sample_id))))

(defn swarm-to-sample [swarm]
  (let [particles (get swarm :particles)
        formatted-particles (map (fn [particle]
                             {:id (get particle :id)
                              :score (get-in particle [:result :value])
                              :fitness (get-in particle [:result :value])
                              :chromosome {:velocity (get particle :velocity)
                                           :result (get particle :result)
                                           :score (get-in particle [:result :value])
                                           :fitness (get-in particle [:result :value])
                                           :best-result (get particle :best-result)
                                           :last-result (get particle :last-result)
                                           :vel-mult (get particle :vel-mult)
                                           :pos-mult (get particle :pos-mult)
                                           :swarm-pos-mult (get particle :swarm-pos-mult)}}) particles)]
    {:sample_id (get swarm :sample_id)
     :sample formatted-particles}))

;;(swarm-to-sample (sample-to-swarm gsample gswarm))
;;(sample-to-swarm gsample gswarm)
;;(def gsample (evospace-get-sample 5))
;;(evospace-initialize gswarm)

(defn evospace-initialize [swarm]
  (if (= (:status (client/post "http://localhost:3000/evospace/pop/initialize")) 200)
    (client/post "http://localhost:3000/evospace/pop/sample"
                 {:body (str "{\"sample\": " (swarm-to-init swarm) "}")
                  :content-type :json
                  :accept :json})))

(defn evospace-get-sample [size]
  (let [sample (keywordize-keys (get (json/read-str
                                      (:body (client/get
                                              (str "http://localhost:3000/evospace/pop/sample/" size))))
                                     "result"))]
    (assoc sample :sample
           (map (fn [particle]
                  (keywordize-keys (json/read-str particle)))
                (get sample :sample)))))

(defn evospace-put-sample [sample]
  (client/post "http://localhost:3000/evospace/pop/sample"
               {:body (str "{\"sample\": " (json/write-str sample) "}")
                :content-type :json
                :accept :json}))

(defn evospace-get-population []
  (map (fn [particle]
         (:chromosome (keywordize-keys (json/read-str particle))))
       (get (json/read-str
             (:body (client/get "http://localhost:3000/evospace/pop/zrange/0/-1")))
            "sample")))

(defn evospace-get-swarm
  "Gets all the population with the format of a swarm."
  []
  (sample-to-swarm
   {:sample_id nil
    :sample (map (fn [particle]
                   (keywordize-keys (json/read-str particle)))
                 (get (json/read-str
                       (:body (client/get "http://localhost:3000/evospace/pop/zrange/0/-1")))
                      "sample"))
    }
   (build sphere-d sphere (comparator <) 1 0 0)))

;;(evospace-get-sample 10)

(defn evospace-get-best-particle []
  (letfn [(value-fn [v] (get-in v [:best-result :value]))
        (best-fn [particles comparator-fn]
          (first (sort-by value-fn comparator-fn particles)))]
  (get-in (best-fn (evospace-get-population) (comparator <))
          [:best-result :value])))

;;(evospace-put-sample gsample)
;;(evospace-get-sample 5)

;;(def gswarm (build sphere-d sphere (comparator <) 10 1 3))
;;(simple-pso 1000 1000 1 3 sphere sphere-d (comparator <))

(defn simple-pso [generations swarm-size c1 c2 test-function dimensions comparator]
  (loop [swarm (build dimensions test-function comparator swarm-size c1 c2)
         g generations]
    (if (or (zero? g) (< (get-in (get-best-particle swarm) [:best-result :value])
                         sphere-threshold))
    ;;(if (zero? g)
      (do
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        (get-in (get-best-particle swarm) [:best-result :value]))
      (do
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        ;; diversity
        ;;(println (calc-diversity swarm))
        ;;(println (:swarm-pos-mult (nth (:particles new-swarm) 0))) ;;no
        (recur (update swarm) (dec g))))))

(defn fuzzy-pso1 [generations swarm-size c1 c2 test-function dimensions comparator]
  (loop [swarm (build dimensions test-function comparator swarm-size c1 c2)
         g generations]
    (if (or (zero? g) (< (get-in (get-best-particle swarm) [:best-result :value])
                         sphere-threshold))
      ;;(if (zero? g)
      (do
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        (get-in (get-best-particle swarm) [:best-result :value]))
      (let [new-swarm (apply (partial update-pos-mults swarm)
                             (fpso1 [(/ g generations) (calc-diversity swarm)]))]
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        ;;(println (:swarm-pos-mult (nth (:particles new-swarm) 0))) ;;no
        (recur (update new-swarm) (dec g))))))

(defn fuzzy-pso2 [generations swarm-size c1 c2 test-function dimensions comparator]
  (loop [swarm (build dimensions test-function comparator swarm-size c1 c2)
         g generations]
    (if (or (zero? g) (< (get-in (get-best-particle swarm) [:best-result :value])
                         sphere-threshold))
      ;;(if (zero? g)
      (do
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        (get-in (get-best-particle swarm) [:best-result :value]))
      (let [new-swarm (apply (partial update-pos-mults swarm)
                             (fpso2 [(/ g generations) (calc-error swarm)]))]
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        ;;(println (:swarm-pos-mult (nth (:particles new-swarm) 0))) ;;no
        (recur (update new-swarm) (dec g))))))

(defn fuzzy-pso3 [generations swarm-size c1 c2 test-function dimensions comparator]
  (loop [swarm (build dimensions test-function comparator swarm-size c1 c2)
         g generations]
    (if (or (zero? g) (< (get-in (get-best-particle swarm) [:best-result :value])
                         sphere-threshold))
      ;;(if (zero? g)
      (do
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        (get-in (get-best-particle swarm) [:best-result :value]))
      (let [new-swarm (apply (partial update-pos-mults swarm)
                             (fpso3 [(/ g generations) (calc-diversity swarm) (calc-error swarm)]))]
        ;;(println (get-in (get-best-particle swarm) [:best-result :value]))
        ;;(println (:swarm-pos-mult (nth (:particles new-swarm) 0))) ;; no
        (recur (update new-swarm) (dec g))))))

(defn evospace-client [generations swarm c1 c2 test-function dimensions comparator]
  (loop [local-swarm (assoc swarm :particles (map (fn [particle]
                                                    (assoc particle
                                                           :pos-mult c1
                                                           :swarm-pos-mult c2))
                                                  (get swarm :particles)))
         g generations]
    (if (or (zero? g) (< (evospace-get-best-particle)
                         sphere-threshold))
      ;;(if (zero? g)
      (do
        local-swarm)
      (do
        (recur (update local-swarm) (dec g))))))

(defn evospace-pso [howmany-clients generations swarm-size lim-c1 lim-c2 test-function dimensions comparator]
  (let [clients-params (map (fn [_] [(rand lim-c1) (rand lim-c2)]) (range howmany-clients))
        prototype-swarm (build dimensions test-function comparator swarm-size lim-c1 lim-c2)]
    (evospace-initialize (build dimensions test-function comparator swarm-size lim-c1 lim-c2))
    (loop [g generations]
      (if (or (zero? g) (< (evospace-get-best-particle)
                           sphere-threshold))
        ;;(if (zero? g)
        (do
          (evospace-get-best-particle))
        (do
          (doall (map (fn [params]
                        (let [result (evospace-client 20
                                                      (sample-to-swarm (evospace-get-sample 5)
                                                                       prototype-swarm)
                                                      (nth params 0)
                                                      (nth params 1)
                                                      sphere sphere-d comparator
                                                      )
                              put-sample-result (evospace-put-sample (swarm-to-sample result))
                              ]
                          result))
                      clients-params))
          ;;(println (evospace-get-best-particle))
          ;;(println (calc-diversity (evospace-get-swarm)))
          (recur (dec g)))
        ))
    (evospace-get-best-particle)))

;;(evospace-get-best-particle)

;;(evospace-initialize (build sphere-d sphere (comparator <) 100 3 3))
;;(evospace-pso 4 30 10 3 3 sphere sphere-d (comparator <))
;;(evospace-get-best-particle)

(def evaluations-counter 0)

;;diversity
(simple-pso 1000 1000 1 3 sphere sphere-d (comparator <))
(evospace-pso 10 100 100 3 3 sphere sphere-d (comparator <))

(map (fn [params]
       [(/ (reduce + (map (fn [_]
                           (simple-pso 100 200
                                       (nth params 0)
                                       (nth params 1)
                                       sphere sphere-d (comparator <))
                            ) (range 5))) 5)
        params]
       )
     (map (fn [_] [(rand 3) (rand 3)]) (range 100)))

;;
(map (fn [_]
       (def evaluations-counter 0)
       (let [res (simple-pso 10000 200 1 3 sphere sphere-d (comparator <))]
         [evaluations-counter res])
       )
     (range 100))

;;
(map (fn [_]
       (def evaluations-counter 0)
       (let [res (simple-pso 10000 200 0.46298840763849525 0.019330306832754274 sphere sphere-d (comparator <))]
         [evaluations-counter res])
       )
     (range 100))

;;
(map (fn [_]
       (def evaluations-counter 0)
       (let [res (fuzzy-pso1 10000 200 1 3 sphere sphere-d (comparator <))]
         [evaluations-counter res])
       )
     (range 100))

(map (fn [_]
       (def evaluations-counter 0)
       (let [res (fuzzy-pso2 10000 200 1 3 sphere sphere-d (comparator <))]
         [evaluations-counter res])
       )
     (range 100))

(map (fn [_]
       (def evaluations-counter 0)
       (let [res (fuzzy-pso3 10000 200 1 3 sphere sphere-d (comparator <))]
         [evaluations-counter res])
       )
     (range 100))

(map (fn [_]
       (def evaluations-counter 0)
       (let [res (evospace-pso 30 10000 200 1 3 sphere sphere-d (comparator <))]
         [evaluations-counter res])
       )
     (range 10))


;;(time (simple-pso 1000 10000 1 3 sphere sphere-d (comparator <)))
;;macbook "Elapsed time: 168972.929656 msecs"
;;server "Elapsed time: 165859.10716 msecs"
;;(fuzzy-pso1 1000 1000 1 3 sphere sphere-d (comparator <))
;;(fuzzy-pso2 1000 1000 1 3 sphere sphere-d (comparator <))
;;(fuzzy-pso3 1000 1000 1 3 sphere sphere-d (comparator <))

;;results
;;(float (/ (reduce + (map first results-simple-pso-no)) (count results-simple-pso-no)))
;;(float (/ (reduce + (map first results-simple-pso-no-2)) (count results-simple-pso-no-2)))
;;(float (/ (reduce + (map first results-simple-pso)) (count results-simple-pso)))
;;(float (/ (reduce + (map first results-fuzzy-pso1)) (count results-fuzzy-pso1)))
;;(float (/ (reduce + (map first results-fuzzy-pso2)) (count results-fuzzy-pso2)))
;;(float (/ (reduce + (map first results-fuzzy-pso3)) (count results-fuzzy-pso3)))
;;(float (/ (reduce + (map first results-evospace-pso)) (count results-evospace-pso)))

(float (/ (reduce + (map first coco)) (count coco)))

9000
8032.3 con 100 iteraciones
;; nota para mí: aumentar las iteraciones internas
(def coco '([4571 0.015045893936652096]
            [1460 0.004375613926080566]
            [797 0.01923785802840169]
            [11383 0.0073521753008506845]
            [400 0.0032345752698253313]
            [672 0.003950705108262222]
            [400 0.005898593299795982]
            [2175 0.0024073460048924535]
            [5385 0.02084609475338006]
            [4625 0.01901530467241605]))

(def coco '([400 0.02240089899996909]
            [400 0.011894250919507328]
            [17104 0.009510035516810768]
            [400 0.004241978379381504]
            [11293 0.01860128323039283]
            [2224 7.604722106580409E-4]
            [3034 0.012526554169860324]
            [8101 0.016540459398027958]
            [6014 0.014356760821408354]
            [10398 0.0035634920526445117]))

(def coco '([3398 8.782659830610688E-4]
            [1350 0.006945548912115548]
            [400 0.01706846602600484]
            [19990 0.009713785854967344]
            [400 0.018560582735896468]
            [23927 0.0052273927003536015]
            [6849 0.020933049892377428]
            [603 0.016021064535167558]
            [5636 0.006666562987894706]
            [585 0.002773043985378292]))

;;(schaffer2 0 0)
;;(griewank 0)
;;(rastrigin 0 0)
;;(sphere 0 0 0)
;;(ackley 0 0)

;; obtener threshold
;;(def sphere-threshold (nth (sort (comparator <) runs-sphere) 89))
;; obtener parámetros
;;(sort-by first (comparator <) sphere-rand-params)

(def sphere-rand-params '([0.023415768531351855 [2.5340061940951384 1.9646077555951849]]
                          [0.0417143162592723 [2.626013175453656 0.9170057336425763]]
                          [0.02022616207841136 [0.5350534138333943 0.24956640218830373]]
                          [0.01203689115458048 [1.315979734829482 0.39087861347325636]]
                          [0.05542158131869479 [1.8559145866064588 2.4062740440675934]]
                          [0.008078961594329312 [1.7446994342782836 0.6686227229779016]]
                          [0.0340303590087999 [1.9467243596108537 1.4898876439589364]]
                          [0.04150190128129922 [2.428249428668893 1.9183755403831677]]
                          [0.02374762569742029 [0.3940951702017842 2.7864818130180202]]
                          [0.030549099988936103 [2.6244445273359376 1.3375469791415013]]
                          [0.013963943975464247 [1.17314122912592 1.5195750774447883]]
                          [0.03741628196894904 [2.7868391994324893 2.1312662529042754]]
                          [0.0473990153921076 [0.7700958081597505 2.6871895770194216]]
                          [0.013819778007033564 [1.8922983701998475 0.5509737336467835]]
                          [0.04360391769169345 [0.5923607306802627 2.906978626655543]]
                          [0.02663393505182236 [2.2674724769118377 1.5127809786482533]]
                          [0.006737259372824357 [2.4607582708665103 0.329570800274285]]
                          [0.03639596530550588 [2.2143170607606577 2.66971421710513]]
                          [0.05238641256332131 [2.700433852143533 1.956649432938431]]
                          [0.05681114175409976 [2.2533521419080613 2.760197893377359]]
                          [0.009648290990608769 [2.412288004924202 0.07040038879100297]]
                          [0.0217538210510385 [1.1656607311494023 0.5971280434442207]]
                          [0.09349377609023123 [1.915997103610248 2.944831617631152]]
                          [0.022972246157891698 [2.220681531820415 1.1976269366136765]]
                          [0.019341527275895238 [2.1086088652455115 0.5670814297416688]]
                          [0.018611604151608625 [2.0266544246248874 1.2830627810614288]]
                          [0.018055330306966995 [2.579382795333826 0.7482035325238503]]
                          [0.012549107513659518 [1.3341333104344637 0.7021601623788077]]
                          [0.01040290097606088 [0.7619709463345442 0.7480304944124401]]
                          [0.007039987175367449 [0.8602831874324137 0.2470196894937725]]
                          [0.030867667107880786 [1.1892170068159917 2.3775872001088056]]
                          [0.011682618498011332 [1.7827370622286072 2.842607095143364]]
                          [0.01628743673018371 [2.7913938243171743 0.5433514983487392]]
                          [0.02774888107154294 [0.6529190831814994 1.998570359308749]]
                          [0.006137298240559975 [0.5852268972520105 0.6699028147260208]]
                          [0.008403015786199711 [0.19322275428578362 1.3174535046969513]]
                          [0.005633036616969353 [0.20103968703549546 1.1438228369142052]]
                          [0.005595744009397549 [0.9293579879155586 0.7739167099608554]]
                          [0.04475038859656718 [2.4916695241491316 2.504621846440244]]
                          [0.013960378015326153 [2.7106437328441237 0.4065998524608819]]
                          [0.024701180173791595 [1.9645034586528576 1.1836792531079146]]
                          [0.022942123996923552 [1.562917986523018 1.9762499161029958]]
                          [0.04694761830075336 [2.919045684364609 2.1850581513128264]]
                          [0.0222142945926846 [1.6225663922746565 0.2141108206514719]]
                          [0.045884911720012 [0.9682622961158128 2.531131715766681]]
                          [0.012756290042977278 [1.2932400402316042 1.538173083337846]]
                          [0.022565736083185725 [2.407416863006878 2.2724293848281127]]
                          [0.014768110262609912 [1.047474235085426 0.42878238603132846]]
                          [0.024214393620863195 [2.7720420324416355 1.54372668422602]]
                          [0.02767046147861903 [0.4702223648778302 1.509595174122449]]
                          [0.03572464053737926 [0.1483700952460102 2.9247731605764637]]
                          [0.017271236623803553 [2.1598760994390553 1.828115301600228]]
                          [0.009414046449775412 [2.3649772327044363 0.23416601868494136]]
                          [0.07974246730043297 [0.9646453981076305 2.8720345577903497]]
                          [0.051911706233257894 [2.103573985250642 1.8487472178242572]]
                          [0.09341681236582658 [0.738229317039049 2.493344762988307]]
                          [0.0537147798561771 [1.2198054823848117 2.698517625299531]]
                          [0.012314379455936782 [0.34851015615418424 0.14167958685565185]]
                          [0.007191222082325893 [2.161603745035233 0.23842929621939613]]
                          [0.0172061670731744 [1.0665612786424215 1.631586896361392]]
                          [0.030770455343910673 [2.3130651618703326 0.9435342583467987]]
                          [0.01985964107323956 [0.7005604996476148 0.08542549110487652]]
                          [0.004288991750441357 [0.3760040879098042 1.2128351960935921]]
                          [0.020117149884904262 [0.9202590158198543 0.704320448510855]]
                          [0.011029531325042433 [0.1372500266398352 2.7469527426970215]]
                          [0.004401819597170552 [1.0505183865366425 0.4043583448037351]]
                          [0.02558197209869869 [2.6991760400813836 2.1737448080480166]]
                          [0.0212153035354242 [0.6289918473565327 1.657958125797982]]
                          [0.01071129953895727 [1.8176012123556162 0.1467372802607666]]
                          [0.012817452568205972 [1.7203652045560087 0.7558807746049098]]
                          [0.05013648553182617 [2.1519585096665743 2.0633519503589812]]
                          [0.09536256453583736 [2.0393875010416056 1.889396282055231]]
                          [0.013072788897754576 [1.9998779870392052 1.6343288474201072]]
                          [0.021753498765795604 [0.6394034037679304 0.9056828619092117]]
                          [0.014860475956156005 [0.03123632307043467 1.1872673993817182]]
                          [0.014849624860915697 [2.412345436305804 0.8495974166873346]]
                          [0.025314612228248386 [2.6691658500561077 1.1192566896301965]]
                          [0.02428865715985954 [1.9105365097146847 0.13102122255913728]]
                          [0.03278228661199457 [2.6286143654659613 2.219877168740314]]
                          [0.006386394132728476 [0.7093899597216424 0.18115334542604122]]
                          [0.008556358485756055 [1.500972908789112 0.7684747451802082]]
                          [0.036584502793415706 [1.2048950377018428 1.815616996554383]]
                          [0.0034360286714193167 [1.994972997062669 0.5859741686008527]]
                          [0.01690853592704023 [1.2583353783710127 0.4704859681850988]]
                          [0.08667117929428868 [2.5094793833117293 2.5916156107711865]]
                          [0.002851925413066304 [0.46298840763849525 0.019330306832754274]]
                          [0.00884109674550709 [1.122988948906403 0.8788675492935755]]
                          [0.017663648503067196 [1.013611231798119 1.0151231997446186]]
                          [0.039675563221540755 [1.8375720433140894 1.8823838965272315]]
                          [0.007802702776038993 [1.209321373026675 0.9303422095286298]]
                          [0.028091525543837686 [1.011675178519175 1.859241032623868]]
                          [0.03453570922150472 [1.514036698532875 1.2918414718874625]]
                          [0.006465568580815033 [0.3566318028884654 1.6528094773218538]]
                          [0.032611959785270574 [2.8359429801595866 2.5240624437382086]]
                          [0.05254952107738799 [1.6559086541816228 2.5464793798855796]]
                          [0.008334159340032524 [0.26851195296314867 2.8248937241018504]]
                          [0.04750713999648149 [1.3182936692954557 2.830918333848873]]
                          [0.022549156327267512 [2.994627987202673 0.9363912527438364]]
                          [0.021737419151364658 [2.513889692635254 1.5867706407569115]]
                          [0.011025628823739824 [2.305768260676592 0.4747096032238708]]))

(def runs-sphere '(0.0013130632027289584
                   0.009097850524843262
                   0.008822198377855247
                   0.0012357289050507658
                   0.010889976410871664
                   0.006981435822018783
                   0.029923134442855666
                   0.017805637861967115
                   0.006852867985182884
                   0.0065094158461893835
                   0.003538289679425793
                   0.0033533444972507634
                   0.006792880736445228
                   0.006531847382725133
                   0.004172051319939973
                   0.011682395451997323
                   0.008893418164176114
                   0.020201471878401017
                   0.01764494991129574
                   0.012361698355144541
                   0.00969733927508911
                   0.002659882705602994
                   0.0060003119793323
                   5.707803587871894E-5
                   0.025033703675116738
                   0.007715237305962236
                   0.04702806833789961
                   0.014790171691843488
                   0.021951968691631747
                   0.0010427613019713044
                   0.0018448621565407032
                   3.585086129145704E-4
                   0.0019334944750707965
                   0.008502662924534114
                   0.027225228068415513
                   0.015263666653679281
                   0.004441813641148696
                   0.0015234209958793026
                   0.02187972904120136
                   0.012099385703164678
                   0.002351974014920687
                   0.015098042758788193
                   0.009058534384546763
                   0.004765519625466581
                   0.010668760192252233
                   0.020487484881837183
                   0.01864873276854468
                   0.0171704869606743
                   0.011332532363295327
                   0.021871487450782382
                   0.0015310014493037372
                   4.628124026985462E-4
                   0.008363211144458867
                   0.0066159979731785536
                   0.0037695202904823367
                   0.005220165031562249
                   0.0050834880381031315
                   0.03192129310200122
                   0.022591940014438625
                   0.011917164309600797
                   0.03420735568123044
                   0.006247024763809854
                   0.02918614196358942
                   0.0031941666789854556
                   0.004747187719970238
                   0.008508372771531174
                   0.0030394003968774947
                   0.0069994203398260945
                   0.0104733533588045
                   0.0023972274557410153
                   0.022127491957070713
                   0.001727469129827742
                   7.367458747646116E-4
                   0.0010085777523181668
                   0.01050282776694379
                   0.008196169166156403
                   0.03652735877915278
                   0.001021310880275475
                   0.003591843982896603
                   0.01987675955280636
                   0.026470607553428574
                   0.0037139473804399473
                   0.013168714998237789
                   0.012765410337680826
                   0.010866878192244116
                   0.005890679303287917
                   0.0022012372023305286
                   0.0071827499418175055
                   0.016253444695892086
                   9.58557076115538E-4
                   0.0063572507394678195
                   0.01603340567343442
                   0.006086532871941075
                   0.010675660392641418
                   0.0013095803879857547
                   0.03054309635451179
                   0.011226334840856392
                   0.005991044251487759
                   0.014617421072857413
                   0.00915513643097011))

(def results-simple-pso-no '([373 0.00934209437006021]
                             [65123 0.0037670751807504666]
                             [185010 0.020984285904051703]
                             [964 0.01911860703568945]
                             [394 0.021052090166590955]
                             [18927 0.01597185708584442]
                             [386 0.008058597606427086]
                             [195931 0.015085944981533518]
                             [109678 0.018780788500342045]
                             [106192 0.018164819152909194]
                             [380 0.006353023198922659]
                             [10332 0.01662039591967143]
                             [200 0.013120129896101998]
                             [381 0.011867908680592492]
                             [44618 0.021219423274778347]
                             [48298 0.014262170909387866]
                             [5359 0.02256248007222698]
                             [200280 0.006057610803489659]
                             [200 0.0016084641781074806]
                             [25820 0.021849618236298655]
                             [11509 0.021640130870907708]
                             [567 0.014788611020946958]
                             [19055 0.017065383030923673]
                             [81990 0.017137952493582687]
                             [1715 0.011966644076218516]
                             [386 0.010846434765648146]
                             [100258 0.02233869946240081]
                             [385 0.00392182007910619]
                             [50429 0.0014267173266512985]
                             [200 0.014360687764950949]
                             [23394 0.008919066669039865]
                             [200 0.016084046346675755]
                             [49896 0.009282995979040096]
                             [200 0.021095710449400597]
                             [116027 0.014834302722085434]
                             [74291 0.0014052283761135432]
                             [559 0.01107735629377278]
                             [16967 0.006349620157497845]
                             [1097 0.007939591728696392]
                             [96616 0.00869277666692451]
                             [562 0.010365634181624178]
                             [383 0.0202622891446882]
                             [182027 0.0018032381245739256]
                             [42427 0.019995672569258976]
                             [91154 0.016895298959514284]
                             [112790 0.011483098969388159]
                             [200 0.011733735975580828]
                             [560 0.006068059082839608]
                             [153293 0.0013503189376043037]
                             [383 0.005091629093417881]
                             [23435 5.164685333315703E-4]
                             [104820 0.015898351074519625]
                             [566 0.021436495150935375]
                             [100590 0.008140391657994291]
                             [51948 0.018407989692295523]
                             [375 0.00841029179160152]
                             [48759 0.018880259091931743]
                             [118272 0.004286369469555527]
                             [29072 0.012434841942405388]
                             [552 0.005974817722999345]
                             [75594 0.0036292459718647394]
                             [132995 0.02218299134895119]
                             [390 0.018627105558824044]
                             [200 0.011522016031220599]
                             [5273 0.006132555085456947]
                             [115903 0.011546201330257258]
                             [52383 0.01275890863120242]
                             [49669 0.016381938743828123]
                             [50564 0.00749977909824866]
                             [101307 0.008882150737040491]
                             [200 0.0062054397325323335]
                             [12678 0.00795511933531628]
                             [54766 0.008262157460585052]
                             [200 0.011967696510818228]
                             [146794 0.007265240921557231]
                             [23587 0.010521640884798168]
                             [54087 0.012478774658108788]
                             [38633 0.008920868893803714]
                             [391 0.005761451877833278]
                             [561 0.015564080471759459]
                             [200 0.017464565124023276]
                             [200 0.021255146432639246]
                             [154804 0.017859626826347338]
                             [384 0.017856531385335942]
                             [9123 0.020053263611172337]
                             [200 0.01951355810293296]
                             [128040 0.019746975685919904]
                             [6737 0.017738266377522405]
                             [200 0.011763872370547344]
                             [128406 0.005615555458975874]
                             [381 0.015195383961547492]
                             [116871 0.0019561808246488376]
                             [7068 0.013032387825062943]
                             [372 0.015797869678021662]
                             [23907 0.01387591831065534]
                             [200 0.019936379106376998]
                             [23287 0.011950286880300144]
                             [563 0.015807627146467364]
                             [51443 0.02178774004663162]
                             [5352 0.008129853150288684]))

(def results-simple-pso-no-2 '([200 0.01918287021396998]
                               [200 0.005238039606589362]
                               [56742 0.001636355077909317]
                               [70378 0.006549907455733128]
                               [200 0.0094241285439058]
                               [16734 0.01640949280022899]
                               [16178 0.001189051317377883]
                               [105990 0.019908567729846463]
                               [200 0.0022891581448128222]
                               [398 0.00239208001522526]
                               [39392 0.007034029002734979]
                               [76309 0.012480175621983657]
                               [126255 0.0010794722521080445]
                               [92930 0.0203076720733556]
                               [78186 0.002767153567028158]
                               [29632 0.011659681231211727]
                               [101260 0.020621226464307738]
                               [385 0.022106235537819832]
                               [152292 0.010264798986415693]
                               [16899 0.009171276032584292]
                               [200 0.013279885865137123]
                               [200 0.014638137790785181]
                               [58783 0.014435058979013258]
                               [32091 0.008416572861130905]
                               [937 0.010375856796006694]
                               [17017 0.02133147689056205]
                               [79479 0.00947371744597365]
                               [43862 6.400346780816143E-4]
                               [126002 0.009635160223190687]
                               [377 0.0013851677015408199]
                               [2608 0.001418787189064161]
                               [81605 0.012469039076920143]
                               [200 0.014483390778453554]
                               [11991 0.006288070653223579]
                               [184525 0.013311711756896442]
                               [101084 0.008212392652038504]
                               [96267 0.01840800707489019]
                               [58006 0.015932301896607694]
                               [7195 0.021288007127428857]
                               [200 0.010108041039117752]
                               [52631 0.02093940424231925]
                               [200 0.006079315269012283]
                               [97648 0.012363960512497895]
                               [4407 0.021790749352695933]
                               [145309 0.005638781832426671]
                               [24074 0.02033254404931953]
                               [9150 0.011027143659112417]
                               [5666 0.004827207350755736]
                               [200 6.145403119951364E-4]
                               [386 0.011228090547360516]
                               [120634 0.011401356655223323]
                               [66770 0.016430038245962255]
                               [200 0.003537735176316977]
                               [3518 0.021343900907951668]
                               [390 0.02034578977420356]
                               [7022 0.008481257332582603]
                               [1453 0.01685028833503233]
                               [374 0.01585327620037335]
                               [103779 0.017420832856197967]
                               [13676 0.01933112919327518]
                               [51599 0.0017250908590156478]
                               [379 0.00808943396451349]
                               [107965 0.013546590585123698]
                               [114112 0.008377756207022996]
                               [4975 0.018693386460421832]
                               [382 0.014001830378071456]
                               [389 3.881944871397621E-4]
                               [2376 0.002545827853054813]
                               [200 0.0023515215861906586]
                               [7151 0.008887750801289868]
                               [31356 0.01598565185873804]
                               [214739 0.01894519173523864]
                               [382 0.012243954861273835]
                               [114950 5.294622778893308E-4]
                               [124910 5.436578250403348E-4]
                               [572 0.00891268338723505]
                               [189317 0.019269993610319495]
                               [6620 0.01940865679577711]
                               [25824 0.01834026013246653]
                               [106405 0.01646613518641867]
                               [17426 0.004400431394537264]
                               [93284 0.002517659800634396]
                               [939 0.0021082195743179427]
                               [43437 0.00448118194092509]
                               [103807 0.008779209054500596]
                               [142288 0.009309047499517834]
                               [124225 0.012494622970044195]
                               [46517 0.018079436932421977]
                               [33021 0.010747858238168498]
                               [95156 0.021788783600215123]
                               [178581 0.001988008242406893]
                               [27160 0.022484443540038864]
                               [200 0.010490981060950146]
                               [17070 0.02217033650748511]
                               [2411 0.004585098337366677]
                               [91097 0.01957117117806064]
                               [385 0.010860144482463861]
                               [31623 0.0022451474594344994]
                               [564 0.021686761373180857]
                               [101348 0.018101879960242626]))

(def results-simple-pso '([12570 0.015913425608252922]
                          [8408 0.016301900303291282]
                          [2324 0.008209870528347814]
                          [200 0.01844854064911869]
                          [200 0.013338393650464936]
                          [1332 0.01638832945577332]
                          [6045 0.022348791673961865]
                          [1256 0.008507658963651066]
                          [200 7.397281046675778E-4]
                          [3485 0.007332808180686687]
                          [200 0.007528543087331548]
                          [2364 0.004208810582656138]
                          [3282 0.010379072036764914]
                          [1282 0.010494251684478598]
                          [1091 0.008071934484431664]
                          [1291 0.004710626154567922]
                          [1117 0.0034334858494072364]
                          [1466 0.013515613133712372]
                          [28906 0.010245520058862544]
                          [2186 0.006356063843865091]
                          [1107 0.007613587818298578]
                          [200 2.1190653392794514E-4]
                          [200 0.01867607345660824]
                          [3275 0.0217292230051698]
                          [379 0.021509139448623482]
                          [376 0.017140969186445178]
                          [2368 0.021080644119668937]
                          [14277 0.006018842714206949]
                          [3095 0.003922253273752022]
                          [1318 0.0022267660920525234]
                          [565 0.005530667269329083]
                          [2195 0.019958225148450207]
                          [391 0.003184233624304342]
                          [384 0.0010377943790267595]
                          [4157 0.018375026445204536]
                          [6632 0.009658162159388315]
                          [12172 0.0013351018846847107]
                          [5436 0.011032373759523003]
                          [1294 0.0031089756738152696]
                          [376 0.019521702856674303]
                          [4339 0.007720451666187551]
                          [14280 0.0185038369490827]
                          [200 0.008963196773173138]
                          [762 0.010607648981702628]
                          [7758 0.01201886326265265]
                          [3456 0.010709876899141173]
                          [200 0.02147955990607263]
                          [2386 0.0041171141925794895]
                          [200 0.02092574331782681]
                          [1289 0.011766115487381636]
                          [2365 0.020754817806844458]
                          [1528 0.014975576326395703]
                          [391 0.006452960598641913]
                          [393 0.022551905022307868]
                          [1959 0.009131380613512626]
                          [4354 0.014232340261602045]
                          [200 0.016613884302175152]
                          [200 0.022042953755956298]
                          [12192 0.017681052445391896]
                          [21045 0.014600558632143155]
                          [23360 0.0114029275140156]
                          [6761 0.014908343745589275]
                          [20456 0.013295306739015562]
                          [3214 0.01847981282681153]
                          [13084 0.00717001766461154]
                          [200 0.01584857909704857]
                          [374 0.002845758272293709]
                          [375 0.010250446070931181]
                          [200 0.00841427523485137]
                          [1636 0.002256355338231746]
                          [25653 0.014193818474864199]
                          [1242 0.005905860907223973]
                          [1070 0.005472895823037634]
                          [200 0.014738108863058997]
                          [200 0.008426524320477335]
                          [384 0.007471931750613399]
                          [560 0.019109994806208813]
                          [9235 0.009767164293103292]
                          [200 0.020264499028220143]
                          [732 0.013062121611547557]
                          [1263 0.003803077669604825]
                          [200 0.006105617352217276]
                          [3842 8.961157606067565E-4]
                          [200 0.014266579050014648]
                          [922 0.002874525778418832]
                          [1127 5.501157325592496E-4]
                          [1271 0.018464586137810744]
                          [8568 0.021176044472276487]
                          [10799 0.0031563771376128605]
                          [5643 0.014302938139299316]
                          [19896 0.004093662082321452]
                          [200 0.0017252208840752969]
                          [547 0.008294161287388305]
                          [1624 0.014354725296025187]
                          [117017 0.0013613572554579132]
                          [385 0.018999315067103898]
                          [1122 0.005123292271435437]
                          [47396 0.005439111529443303]
                          [20804 0.008285231090781574]
                          [200 0.016746643007260278]))

(def results-fuzzy-pso1 '([200 0.012015185751330561]
                          [27688 0.0017245469250327546]
                          [13005 0.013553650772204126]
                          [14770 0.00956190689767367]
                          [37183 0.006682640265758429]
                          [383 0.001757796507169267]
                          [55696 0.009609915746231368]
                          [939 0.004408133039087922]
                          [70192 0.005572611430865125]
                          [560 0.0056315874683474975]
                          [379 0.012398343421334616]
                          [200 0.004289418247489717]
                          [28547 0.005470220716395306]
                          [200 0.01889573637244371]
                          [1863 0.02135414828063889]
                          [369 8.11812517033186E-4]
                          [384 0.01719952464678879]
                          [3613 0.01129471823801722]
                          [33928 0.011781102377864732]
                          [17640 0.015676881432697975]
                          [3096 0.0014705043050925307]
                          [74403 0.015366646535182]
                          [96470 0.0057865221634271075]
                          [74543 0.005553528098942667]
                          [25384 0.022402907357469123]
                          [380 0.015751457111157014]
                          [67960 0.020369657240392032]
                          [59457 0.018726157601668045]
                          [386 0.009418600313047556]
                          [9648 0.014417824814479672]
                          [20543 0.014568240234895146]
                          [572 0.013649805838939417]
                          [377 0.007585343828011895]
                          [36375 0.008884052289361472]
                          [86467 0.017409563714958914]
                          [90058 0.013600042628335054]
                          [200 0.016042491752335606]
                          [912 0.003414182924909993]
                          [381 0.0010748647217680711]
                          [3624 0.013624654422472756]
                          [8848 0.0014118812934950412]
                          [3814 0.02099187665808813]
                          [9618 0.021220617025024403]
                          [395 0.007689198950265928]
                          [589 0.00689537029420739]
                          [390 0.012129120030398913]
                          [12688 0.001447192646914737]
                          [32622 0.016931723932623342]
                          [21132 0.021971335641533685]
                          [33870 0.02168783676223967]
                          [200 0.0035432954243300252]
                          [106603 0.016646865533551172]
                          [130463 5.670745612048763E-4]
                          [12277 0.01730980947600085]
                          [391 0.0028306702060812788]
                          [37008 0.012218344593118182]
                          [71193 0.00543011171254891]
                          [2578 0.010561768597931197]
                          [26648 0.011074573905242163]
                          [69531 1.288150025171182E-4]
                          [43955 0.0057858691514078514]
                          [73581 0.007008109247948935]
                          [91312 0.0194177506633221]
                          [3635 0.012428196632053598]
                          [394 0.015825403177856086]
                          [88327 0.004358910633939596]
                          [389 9.817756352797711E-4]
                          [382 0.0132760554214233]
                          [383 0.0036020633041527706]
                          [28672 0.007011869333608133]
                          [200 0.004161853484317517]
                          [200 0.008394780910455679]
                          [70258 0.02088579606940067]
                          [12448 0.004682881199503644]
                          [12162 0.01664706696649875]
                          [81397 0.0026537983952599773]
                          [50075 0.013336633716249158]
                          [80827 0.009266888574475288]
                          [200 0.018170207939709356]
                          [83892 0.010728329001878931]
                          [63558 0.01753188478397643]
                          [381 0.011188565171152133]
                          [572 0.01106369858753491]
                          [927 0.020804839640252714]
                          [563 0.002522832518424748]
                          [200 0.010378476986397262]
                          [739 8.815378212294777E-4]
                          [566 0.013562568621590582]
                          [200 0.007282232496665304]
                          [13749 0.001354590074470735]
                          [80816 0.012419709240066548]
                          [16094 0.013456383447994876]
                          [8287 0.012785183147039917]
                          [45128 0.015413871655432825]
                          [390 0.005143430797309354]
                          [40223 0.00199570114075536]
                          [33979 0.002472145920872025]
                          [29277 0.022322597029502095]
                          [200 0.0090982831032888]
                          [24610 0.020019938843633026]))

(def results-fuzzy-pso2 '([200 0.008214934105863042]
 [200 0.009379000924937462]
 [65676 0.006222091233949779]
 [28978 7.959817550578507E-5]
 [79696 0.012515700639778738]
 [11475 0.0068041213976513595]
 [28116 0.010290695956764073]
 [9747 0.008691567520824128]
 [758 0.019602007848755605]
 [50517 0.007499349967055967]
 [379 0.010695613983991069]
 [200 0.011107578392080552]
 [29325 0.01872917153903074]
 [573 0.016941823956543447]
 [383 0.015774013535288782]
 [572 0.008084740834922974]
 [16624 9.197907480007683E-4]
 [42019 0.008188745962689297]
 [55267 7.703052364176723E-4]
 [48209 0.006238950636101025]
 [66339 0.008376425215412384]
 [32003 0.006929756213439416]
 [177125 0.0026968461776796346]
 [39950 0.0017065295543530288]
 [21754 0.018384780033618167]
 [15491 0.006611192253738004]
 [39121 0.0013806486156851452]
 [11524 0.007806911690559815]
 [382 0.0076149034263645705]
 [200 0.014801107493356312]
 [11921 0.0014580813850062018]
 [385 5.509565181026928E-4]
 [29288 0.00343938666716399]
 [36129 0.020267679144808932]
 [104438 0.019503959572927432]
 [54213 0.012171037686671073]
 [380 0.006457153348197587]
 [386 0.011277407353225233]
 [3857 0.0016260804458099886]
 [200 0.006777245089165575]
 [43149 0.016257173430297416]
 [13262 0.02254430494583732]
 [34765 4.4043434260610507E-4]
 [1324 0.0010436827566628964]
 [43237 0.0038038729268073702]
 [200 0.01657798195207431]
 [86928 0.016103585023650686]
 [93344 0.004423697720517483]
 [947 0.0090333622025456]
 [34393 0.01760146251702125]
 [145835 0.0052600657933226025]
 [41629 0.0027415303329303346]
 [3402 0.005158031513623685]
 [58922 0.004730518131520519]
 [378 0.020619609142348817]
 [15592 0.0029889014012460125]
 [35977 0.007462845940923061]
 [391 0.01925609844753335]
 [8003 8.618935256701073E-4]
 [43173 0.01562921464003098]
 [200 0.02234147816371538]
 [46312 0.009914393448802282]
 [81842 0.020375981401961722]
 [200 0.004715137584884415]
 [200 0.007315704056497821]
 [561 0.0014419819316130065]
 [30736 0.020426719390062274]
 [200 0.022092731190589653]
 [105942 0.02241254607946353]
 [575 0.007980837375575853]
 [4619 0.016945630439846173]
 [59990 0.005678357935621844]
 [71539 0.014502643368596408]
 [389 0.010517098076351143]
 [933 0.003966439292343959]
 [3437 0.01380420606327765]
 [2047 0.018465664311992827]
 [200 0.004976064288482054]
 [35387 0.013915690672006069]
 [44183 0.017937028735678835]
 [24237 0.019848131550894856]
 [40713 0.012119970938787236]
 [33342 0.018647603000773526]
 [200 0.016366715756230003]
 [79645 0.01665588514998954]
 [370 9.148280841962182E-4]
 [27384 0.0012771629386561921]
 [386 1.0925763355066042E-4]
 [377 0.007983169302078656]
 [384 0.010422843022594063]
 [16631 0.006052068413289295]
 [38139 0.013515353270623738]
 [911 0.0013665304634978984]
 [384 0.006488218716984805]
 [29652 0.017965560721396302]
 [35482 0.014708346700397227]
 [16644 0.019952814494335017]
 [87933 0.004600659122199018]
 [116401 0.005176804968356043]
 [54471 0.0197417212629833]))

(def results-fuzzy-pso3 '([19079 0.0024525510166025716]
 [3657 0.005703827811117543]
 [29632 0.02003342426558146]
 [389 0.00681418526698403]
 [34187 0.02163533747354712]
 [200 0.0019495169499021572]
 [19885 0.021384413305429985]
 [63207 0.012925644079260737]
 [6246 0.007901826980836585]
 [200 0.006097277218447858]
 [200 0.019566153104074]
 [200 0.020602215535366574]
 [41434 0.02247722783930017]
 [200 7.726902504275928E-4]
 [22084 0.011700507518295796]
 [14315 0.010421622264772458]
 [32104 0.0026099545160301876]
 [384 0.005700881077997494]
 [15008 0.006210040208992449]
 [2698 0.01892012158202863]
 [780 0.01825453444614821]
 [4886 0.009564659854169795]
 [66586 0.013240489752002503]
 [200 0.012162220098117805]
 [6612 6.077102078644622E-4]
 [382 0.002023363499189687]
 [41408 0.005935324268980344]
 [395 0.011064784820662475]
 [11405 0.0029861979760740097]
 [9904 0.009254244984867845]
 [35990 0.0018705965820739374]
 [90915 2.1636316502863726E-4]
 [200 0.021977254707520533]
 [381 0.02141994235314307]
 [200 0.004571639220729963]
 [200 0.007545393813784926]
 [5723 0.01831071838378286]
 [18201 0.01832229199041511]
 [200 0.01881073529251429]
 [11300 0.006558125614074876]
 [9037 0.0163437900331255]
 [554 0.015829968874720966]
 [12310 0.01868781274919929]
 [200 0.015285421687000565]
 [17043 0.004803878568842037]
 [22438 0.01961324439899833]
 [55090 0.0014785355711224852]
 [200 0.022516826399346282]
 [12124 0.011885262431239234]
 [17103 0.007775552738341533]
 [35445 0.011695527225337754]
 [200 0.004138076035625023]
 [80751 0.004233394340802729]
 [24836 0.015454138480158646]
 [32699 0.008449478698453142]
 [22186 0.004821932408150377]
 [97670 0.0013990455327579105]
 [8202 0.006548857096292581]
 [20701 0.0017335408975308387]
 [4691 0.004818393701191306]
 [2732 0.008733281419111066]
 [200 0.00863607679377429]
 [28524 0.017832654085804204]
 [200 0.009536285147535229]
 [558 0.004421241512162997]
 [385 0.0024353172700902377]
 [11348 0.001236972461935627]
 [119523 0.015955219372417475]
 [83506 0.013281816365292031]
 [46185 0.01141501014570972]
 [166403 0.012837428557215702]
 [53188 0.004395822320023483]
 [377 0.005573231856274386]
 [200 0.01063240749849792]
 [34538 0.018331714553922074]
 [21448 0.015195764511610126]
 [39642 0.019477483288815784]
 [18715 0.01433552516340613]
 [566 0.0012334104450784777]
 [81885 0.011177851980834876]
 [26880 0.014502459148285306]
 [22732 0.005019917528097059]
 [27749 0.021660218840954617]
 [8092 0.01849873469968898]
 [32235 0.003188586885232078]
 [48909 0.009102032648263654]
 [6127 0.018818753361615308]
 [42962 0.0026390854220258793]
 [82893 0.02092053177678597]
 [9780 0.01588325749076088]
 [7785 1.278268340992154E-4]
 [200 0.006524480707337844]
 [30335 0.00483183150985782]
 [3949 0.01802495908823767]
 [367 0.002876247997824945]
 [13403 0.010737775113899391]
 [54873 0.013841016928378766]
 [200 0.007961097777721166]
 [393 0.014107612226994332]
 [43335 0.012377720015529984]))

(def results-evospace-pso '([10000 0.011486414077336794]
                            [1417 0.01959249590322062]
                            [7214 0.020279254565928724]
                            [3600 0.0019028061307614658]
                            [5806 0.017483884679309447]
                            [2478 0.021094676719342308]
                            [703 0.004779797256929822]
                            [132375 0.007992949047575793]
                            [9713 0.0022370303592573024]
                            [5045 0.012850873590674116]
                            [24678 0.0020743642098161098]
                            [38378 0.021980278724417313]
                            [31265 0.019116198567556905]
                            [829 0.0211506869781943]
                            [400 0.007545163031254052]
                            [8560 0.01902761650968277]
                            [14226 0.010906117010628479]
                            [964 0.00890463583854431]
                            [4303 0.012602995426952663]
                            [5067 0.009928128554172804]
                            [36012 0.0023806216175847]
                            [17046 0.019102676048555083]
                            [400 0.0016498152326818658]
                            [6524 0.0037849362137602387]
                            [2670 0.018506677494627925]
                            [16430 0.01006686398330097]
                            [400 0.009123605461614823]
                            [400 0.013624394830562289]
                            [19261 0.009335503041495222]
                            [19924 0.013792273300402185]
                            [2376 0.00776079066363722]
                            [959 0.005023506838123129]
                            [1349 0.014525630207958348]
                            [27427 0.009273353033668942]
                            [4919 0.022491712867366905]
                            [17219 0.015947945954259954]
                            [400 0.004861350726284564]
                            [4940 0.003133391627004356]
                            [544 0.009753550472527824]
                            [698 0.012801984812157998]
                            [3614 0.010919211802703542]
                            [10363 0.014564625792244667]
                            [47727 0.002073672410649079]
                            [9172 6.354811024745896E-4]
                            [9406 0.005440159217882264]
                            [400 0.008119620855862158]
                            [6468 0.013623174164849618]
                            [11938 0.0021014082287674827]
                            [1252 0.007841883370617258]
                            [1370 0.01959810622327475]
                            [28042 0.020503550832876666]
                            [1953 0.01787098282008838]
                            [19152 0.0221609835066982]
                            [538 0.011960109519493254]
                            [46060 0.015643651741534616]
                            [5594 0.020894838351619975]
                            [5381 0.018935783253491205]
                            [27626 0.013049548508690851]
                            [10994 0.01491570689757335]
                            [4846 0.006896009094665065]
                            [543 0.0117352592229247]
                            [4312 0.009668785687686587]
                            [400 0.013834029098485981]
                            [521 0.01632682677493493]
                            [54908 0.014054375796862341]
                            [1421 0.012951879234878137]
                            [11966 0.011112996806806146]
                            [1354 0.0068171143931365094]
                            [2966 0.019618487381981565]
                            [3339 0.013951304702726059]
                            [8762 0.011985045354070419]
                            [6836 0.009406556988599527]
                            [54063 0.014373340047202107]
                            [13652 0.012093798362579143]
                            [9816 0.014250124601010668]
                            [1557 0.016583831647999227]
                            [2615 0.0023444570839402656]
                            [1035 0.016969634836850354]
                            [8952 0.021121953315916746]
                            [1981 0.008031356514630572]
                            [20260 0.019308664099740726]
                            [43521 0.007449917973695898]
                            [2528 0.013221611980553268]
                            [1139 0.0022769710764568287]
                            [9770 0.01711030371895603]
                            [1811 0.011999681918859872]
                            [59801 0.018759720219481392]
                            [550 0.01160665538862036]
                            [1672 0.0046526954384129]
                            [5601 0.013489684798696517]
                            [4763 0.0036023312146846592]
                            [8690 0.018437508820626274]
                            [710 0.0107138756227085]
                            [400 0.011512976832545397]
                            [4244 0.012370727063167476]
                            [400 0.013267116458475314]
                            [7308 0.0021371802993567788]
                            [10342 7.619131705423392E-4]
                            [1793 0.014121882455876528]
                            [4151 0.004772344110321847]))
