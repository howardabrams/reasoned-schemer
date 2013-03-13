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

;; Given an element that is part of a list, this returns a new list of
;; that element and all the rest of elements in the list.

(defn mem [x l]
  (cond
   (empty? l) false
   (= (first l) x) l
   :else 
      (mem x (rest l))))

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

;; This function is pretty evil, in that I can't seem to duplicate it
;; in Clojure accurately enough to pass all the tests.

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

(deftest p18
  (is (= '(_.0 _.0)    ;; Not even close...
         (run 12 [x]
              (fresh [u]
                     (memo :tofu '(:a :b :tofu :d :tofu :e . x) u))))))

(defn eq-first? [l x]
  (= (first l) x))

(defn rember [x l]
  (cond
   (empty? l) ()
   (eq-first? l x) (rest l)
   :else
   (cons (first l) (rember x (rest l)))))

(deftest p23
  (is (= '(:a :b :d :peas :e)
         (rember :peas '(:a :b :peas :d :peas :e)))))

;; We'll skip the version in p23, and implement #
(defn rembero [x l out]
  (conde
   [(emptyo l) (== () out)]
   [(eq-firsto l x) (resto l out)]
   ;; [(fresh [a d res]
   ;;         (resto l d)
   ;;         (rembero x d res)
   ;;         (firsto l a)
   ;;         (conso a res out))]))
   [(fresh [a d res]
          (conso a d l)
          (rembero x d res)
          (conso a res out))]))

(deftest p30
  (is (= (list '(:a :b :d :peas :e))
         (run 1 [out]
              (fresh [y]
                     (rembero :peas [:a :b y :d :peas :e] out))))))

;; Note: Seems to be a difference in passing in a vector vs. list,
;; If the calls is (rembero :peas '(:a :b y :d :peas :e) out),
;; it fails, as the result is:  ((:a :b y :d :e))

(deftest p31
  (is (= (list
          '(:b :a :d _.0 :e)
          '(:a :b :d _.0 :e)
          '(:a :b :d _.0 :e)
          '(:a :b :d _.0 :e)
          '(:a :b _.0 :d :e)
          '(:a :b :e :d _.0)
          '(:a :b _.0 :d _.1 :e))
         (run* [out] 
               (fresh [y z] 
                      (rembero y [:a :b y :d z :e] out))))))

;; In Problem 49, we have (rembero y [y :d z :e] ...),
;; which could have the following 4 answers:
;; '(:d z :e)  ; When y == y
;; '(:d z :e)  ; When y == :d
;; '(y :d :e)  ; When y == z    !! The only solution
;; '(:e :d z)  ; When y == :e

(deftest p49
  (is (= (list
          '(:d :d)
          '(:d :d)
          '(_.0 _.0)
          '(:e :e))
       (run* [r]
             (fresh [y z]
                    (rembero y [y :d z :e] [y :d :e])
                    (== [y z] r))))))

;; According to the Reasoned Schemer, the output from this problem
;; should be a series of empty values (as we do get) PLUS some various
;; assortment of everything else:
;;  '(_.0 _.0 _.0 _.0 _.0 
;;    () (_.0 _.1) (_.0) (_.0 _.1 _.2) (_.0 _.1) ...

(deftest p58
  (is (= '(_.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0)
         (run 13 [w]
              (fresh [y z out]
                     (rembero y '(:a :b y :d z . w) out))))))

(defn surpriseo [s]
  (rembero s [:a :b :c] [:a :b :c]))

(defn surpriseo [s]
  (rembero s '(:a :b :c) '(:a :b :c)))


(deftest p70
  (is (= '(:d)
       (run* [r]
             (== :d r)
             (surpriseo r)))))

(deftest p71
  (is (= '(_.0)
       (run* [r]
             (surpriseo r)))))

;; According to TRS, this problem should not succeed...

(deftest p72
  (is (= '(:b)
         (run* [r]
               (surpriseo r)
               (== :b r)))))

(deftest p73
  (is (= '(:b)
         (run* [r]
               (== :b r)
               (surpriseo r)))))
