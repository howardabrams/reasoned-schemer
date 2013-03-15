;;; -*- mode: clojure; mode: clojure-test -*-
(ns reasoned-schemer.chapter5
  (:use reasoned-schemer.core
        [clojure.core.logic :exclude [is]]
        clojure.test)
  (:refer-clojure :exclude [== inc reify list?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CHAPTER 5
;;
;; Double Your Fun
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn append [l s]
  (cond
   (empty? l) s
   :else (cons (first l)
               (append (rest l) s))))

(deftest p2
  (is (= '(:a :b :c :d :e)
       (append '(:a :b :c) '(:d :e)))))

(deftest p3
  (is (= '(:a :b :c)
       (append '(:a :b :c) ()))))

(deftest p4
  (is (= '(:a :b :c)
       (append () '(:a :b :c)))))

(deftest p5
  (is (thrown? IllegalArgumentException 
               (append :a '(:d :e)))))

(deftest p6
  (is (thrown? IllegalArgumentException 
               (append '(:d :e) :a))))

;; Of course, appendo is already defined in core.logic...

(defn our-appendo
  "Similar to conso, but appends a list of elements to another list."
  [l s out]
  (conde
   [(emptyo l) (== s out)]
   [(fresh [a d res]
           (firsto l a)
           (resto l d)
           (appendo d s res)
           (conso a res out))]))

(deftest p10
  (is (= '((:cake :tastes :yummy))
       (run* [x]
             (appendo '(:cake) '(:tastes :yummy) x)))))

;; x is related to the combined list, and y could be anything
(deftest p11
  (is (= (list '(:cake :with :ice _.0 :tastes :yummy))
       (run* [x]
             (fresh [y]
                    (appendo
                     (list :cake :with :ice y)
                     (list :tastes :yummy)
                     x))))))

;; What is the Clojure equivalent to using "dot" at the end of a list to make a tuple?
;; And here is where my lack of subtle Clojure knowledge would come in
;; handy, as I can't get this test to pass even though the output of
;; each part seems identical.
;;
;; (deftest p12
;;      ;;  ((:cake :with :ice :cream . _.0))
;;   (is (= (list (list :cake :with :ice :cream '_.0))
;;        (run* [x]
;;              (fresh [y]
;;                     (appendo
;;                      (list :cake :with :ice :cream)
;;                      y
;;                      x))))))

;; In order to actually test the results that aren't a real list,
;; we'll create some helpful functions that allow us to extract a
;; particular element from a "logical list" built with LCons

(defn l-nth 
  "Returns the nth element of a logical list: LCons"
  [l n]
  (if (= 0 n)
    (lfirst l)
    (recur (lnext l) (- n 1))))

(defn l-rest
  "Returns the logial list starting with the nth element"
  [l n]
  (if (= 0 n)
    l
    (recur (lnext l) (- n 1))))

;; This is a more thorough test than we really need to, but it should
;; verify our l-nth and l-rest helper functions as well.

(deftest p12b
  (let* [run-results
         (run* [x]
               (fresh [y]
                      (appendo
                       (list :cake :with :ice :cream)
                       y
                       x)))
         results (first run-results)]

        (is (lcons? results))
        (is (= :cake  (l-nth results 0)))
        (is (= :with  (l-nth results 1)))
        (is (= :ice   (l-nth results 2)))
        (is (= :cream (l-nth results 3)))
        ;; You can't take the "lfirst" of this...
        (is (= '_.0 (l-rest results 4)))
        ))

;; While I can not test the LCons even when it isn't a fully formed list,
;; but I'm still not sure how to create such odd "tupling" in Clojure...

(deftest p13
  (is (= '((:cake :with :ice . _.0 :d :t))
       (run 1 [x]
             (fresh [y]
                    (appendo
                     (list :cake :with :ice '. y)
                     (list :d :t)
                     x))))))

(deftest p14
  (is (= '(_.0)
       (run 1 [y]
             (fresh [x]
                    (appendo
                     '(:cake :with :ice . y)
                     [:d :t]
                     x))))))

(defn appendo-p15
  "Similar to conso, but appends a list of elements to another list."
  [l s out]
  (conde
   [(emptyo l) (== s out)]
   [(fresh [a d res]
           (conso a d l)
           (appendo d s res)
           (conso a res out))]))

(deftest p16
  (is (= '((:cake :with :ice :d :t))
       (run 5 [x]
             (fresh [y]
                    (appendo
                     '(:cake :with :ice . y)
                     [:d :t]
                     x))))))

(deftest p17
  (is (= '(_.0)
       (run 5 [y]
             (fresh [x]
                    (appendo
                     '(:cake :with :ice . y)
                     [:d :t]
                     x))))))

(deftest p20
  (is (= '(_.0)
       (run 5 [x]
             (fresh [y]
                    (appendo
                     '(:cake :with :ice . y)
                     '(:d :t . y)
                     x))))))

