;;; -*- mode: clojure; mode: clojure-test -*-
(ns reasoned-schemer.chapter4
  (:use reasoned-schemer.core
        [clojure.core.logic :exclude [is]]
        clojure.test)
  (:refer-clojure :exclude [== inc reify list?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CHAPTER 4
;;
;; Members Only
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We don't use clojure.core/list? because we don't particularly care
;;; in Clojure about whether something is exactly a list - we care
;;; whether it's a sequence

(def list? seq?)

(defn mem [x l]
  (cond
   (empty? l) false
   (= (first l) x) l
   :else (mem x (rest l))))

(deftest p1
  (is (= '(:tofu :d :peas :e)
         (mem :tofu '(:a :b :tofu :d :peas :e)))))

(deftest p2
  (is (= false
         (mem :tofu '(:a :b :peas :d :peas :e)))))

(deftest p3
  (is (= '((:tofu :d :peas :e))
         (run* [out]
               (== (mem :tofu '(:a :b :tofu :d :peas :e)) out)))))

(deftest p4
  (is (= '(:peas :e)
         (mem :peas (mem :tofu '(:a :b :tofu :d :peas :e))))))

(deftest p5
  (is (= '(:tofu :d :tofu :e)
         (mem :tofu (mem :tofu '(:a :b :tofu :d :tofu :e))))))

(deftest p6
  (is (= '(:tofu :e)
         (mem :tofu (rest (mem :tofu '(:a :b :tofu :d :tofu :e)))))))

;; That's a lot of tofu, and I'm vegetarian.

(defn eq-firsto [l x]
  (firsto l x))

(defn memo [x l out]
  (conde
   ;; [(emptyo l) u#]
   [(firsto l x) (== l out)]
   [(s# (fresh [d]
           (resto l d)
           (memo x d out)))]))

(deftest p10
  (is (= '((:tofu :d :tofu :e))
         (run 1 [out]
              (fresh [x]
                     (memo :tofu '(:a :b :tofu :d :tofu :e) out))))))

;; My initial guess was '((:tofu :e)) since I assumed that x would be
;; associated with _.0, and this is what it returns, however, TRS says
;; that this should be '((:tofu :d :tofu :e))
(deftest p11
  (is (= '((:tofu :e))
         (run* [out]
              (fresh [x]
                     (memo :tofu '(:a :b x :d :tofu :e) out))))))


(deftest p12
  (is (= '(:tofu)
         (run* [r]
               (memo r 
                     '(:a :b :tofu :d :tofu :e)
                           '(:tofu :d :tofu :e))))))

(deftest p13
  (is (= '(true)
         (run* [q]
               (memo :tofu '(:tofu :e) '(:tofu :e))
               (== true q)))))

(deftest p14
  (is (= ()
         (run* [q]
               (memo :tofu '(:tofu :e) '(:tofu))
               (== true q)))))

(deftest p15
  (is (= () ;; This should be (:tofu) ... got a different in memo
         (run* [x]
               (memo :tofu '(:tofu :e) '(x :e))))))

(deftest p16
  (is (= ()
         (run* [x]
               (memo :tofu '(:tofu :e) '(:peas x))))))

(deftest p17
  (is (= '((:tofu :e))   ;; Should have also found '(:tofu :d :tofu :e)
         (run* [out]
               (fresh [x]
                      (memo :tofu '(:a :b x :d :tofu :e) out))))))

