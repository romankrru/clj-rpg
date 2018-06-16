(ns clojure-rpg.core)

(def max-lvl 10)

;; lvl(integer) -> att(integer)
;; given lvl calculate attack by formula
;; att = lvl * 6
(defn calc-attack
  "given lvl calculate attack"
  [lvl]
  (* lvl 6))

;; lvl(integer) -> def(integer)
;; given lvl calculate deffence by formula
;; def = lvl * 1.3
(defn calc-def
  "given lvl calculate deffence"
  [lvl]
  (int (* lvl 1.3)))

;; lvl(integer) -> hp(integer)
;; given lvl calculate health points by formula
;; hp = lvl * 20
(defn calc-hp
  "given lvl calculate health"
  [lvl]
  (* lvl 20))

(defn calc-sides
  [lvl]
  (if (> lvl 5) 4 6))

;; given sides count roll the dice
;; sides(int) ->  roll dice(int)
(defn roll-dice
  "given sides count roll the dice"
  [sides]
  (inc (rand-int sides)))

(defn kill-negative
  [n]
  (if (neg? n) 0 n))

(defn calc-base-damage
  [att def]
  (kill-negative (- att def)))

(defn create-character
  [name lvl]
  {:name name
   :lvl lvl
   :att (calc-attack lvl)
   :def (calc-def lvl)
   :hp (calc-hp lvl)})

(defn real-damage
  [base sides]
  (let [rd (roll-dice sides)
        s (/ sides 2)]
    (cond
      (< rd s) (int (/ base 2))
      (= rd sides) (* base 2)
      (>= rd s) base)))

;; from(character) + to(character) ->
;; [damage(integer), character]
(defn take-damage
  [from to]
  (let [bd (calc-base-damage (:att from) (:def to))
        s (calc-sides (:lvl from))
        rd (real-damage bd s)]
    [rd (update-in to [:hp] #(- % rd))]))

(def config
  {:player player
   :enemy big-troll})

(def log-template "Charater %s received %d damage. New hp: %d")

(defn print-battle-log
  [damage character]
  (let [name (:name character)
        newHp (:hp character)
        s (format log-template name damage newHp)]
    (println s)))

(defn print-winner
  [p-hp e-hp]
  (if (<= p-hp e-hp)
    (println "Enemy won!")
    (println "Palyer won!")))

(defn game-logic
  [config]
  (loop [player (:player config)
         enemy (:enemy config)
         round 1]
    (if (or (<= (:hp player) 0)
            (<= (:hp enemy) 0))
      (print-winner (:hp player) (:hp enemy))
      (let [pl->en (take-damage player enemy)
            en->pl (take-damage enemy player)]
        (do
          (println (str "Round " round "\n"))
          (print-battle-log (pl->en 0) (pl->en 1))
          (print-battle-log (en->pl 0) (en->pl 1))
          (recur (en->pl 1) (pl->en 1) (inc round)))))))

(defn test-f
  []
  (loop [i 1]
    (if (< i 10)
      (println i)
      (recur (inc i)))))

(defn test-a
  []
  (loop [x 10]
    (when (> x 1)
      (println x)
      (recur (- x 2)))))

(def player (create-character "Robert" 6))
(def troll (create-character "Troll" 2))
(def big-troll (create-character "Big Troll" 4))
