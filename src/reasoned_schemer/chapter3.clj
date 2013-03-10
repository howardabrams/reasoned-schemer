;;; -*- mode: clojure; mode: clojure-test -*-
(ns reasoned-schemer.chapter3
  (:use reasoned-schemer.core
        [clojure.core.logic :exclude [is]]
        clojure.test)
  (:refer-clojure :exclude [== inc reify list?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CHAPTER 3
;;
;; Seeing Old Friends in New Ways
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We don't use clojure.core/list? because we don't particularly care
;;; in Clojure about whether somethign is exactly a list - we care
;;; whether it's a sequence

(def list? seq?)

(deftest p1
  (is (= true
         (list? '([:a] [:a :b] :c)))))

(deftest p2
  (is (= true
         (list? '()))))

(deftest p3
  (is (= false
         (list? :s))))

(deftest p4
  (is (= false
         (list? (llist :d :a :t :e :s)))))

(defn listo [l]
  (conde
   ((emptyo l) s#)
   ((pairo l)
    (fresh [d]
           (resto l d)
           (listo d)))))

(deftest p7
  (is (= '[_.0]
         (run* [x]
               (listo (list :a :b x :d))))))

;; Note that, unlike in Scheme, there's a difference between nil and
;; an empty list.
(deftest p10
  (is (= [[]]
         (run 1 [x]
               (listo (llist :a :b :c x))))))

(deftest p13
  (is (try
        (doall
         (run* [x]
               (listo (llist :a :b :c x))))
        false
        (catch StackOverflowError _ true))))

(deftest p14
  (is (= [[]
          ['_.0]
          ['_.0 '_.1]
          ['_.0 '_.1 '_.2]
          ['_.0 '_.1 '_.2 '_.3]]
         (run 5 [x]
              (listo (llist :a :b :c x))))))

(defn lol? [l]
  (cond
   (empty? l) true
   (list? (first l)) (lol? (rest l))
   :else false))

(deftest p16
  (is (lol? '((:a :b) (1 2))))
  (is (not (lol? '(1 2))))
  (is (lol? '())))

(defn lolo [l]
  (conde
   ((emptyo l) s#)
   ((fresh [a]
           (firsto l a)
           (listo a))
    (fresh [d]
           (resto l d)
           (lolo d)))))

(deftest p20
  (is (= ['()]
         (run 1 [l]
              (lolo l)))))

(deftest p21
  (is (= [true]
         (run* [q]
               (fresh [x y]
                      (lolo '((:a :b) (x :c) (:d y)))
                      (== true q))))))

(deftest p22
  (is (= [true]
         (run 1 [q]
              (fresh [x]
                     (lolo (llist '(:a :b) x))
                     (== true q))))))

(deftest p23
  (is (= ['()]
         (run 1 [x]
              (lolo (llist '(:a :b) '(:c :d) x))))))

;; TRS has this as
;; (()
;;  (())
;;  (() ())
;;  (() () ())
;;  (() () () ())
;;
;; The reason for the difference is that conde in the book is a
;; depth-first search where conde in core.logic is an interleaving
;; one.
(deftest p24
  (is (= ['()
          '(())
          '((_.0))
          '(() ())
          '((_.0 _.1))]
         (run 5 [x]
              (lolo (llist '(:a :b) '(:c :d) x))))))

(defn twinso [s]
  (fresh [x y]
         (conso x y s)
         (conso x '() y)))

(deftest p32
  (is (= [true]
         (run* [q]
               (twinso '(:tofu :tofu))
               (== true q)))))

(deftest p33
  (is (= [:tofu]
         (run* [z]
               ;; Note: do not write this as '(z tofu)! Quoting z
               ;; means that we're looking for the symbol z, not the
               ;; logic variable z. Found this out the hard way. :p
               (twinso (list z :tofu))))))

(defn twinso-36 [s]
  (fresh [x]
         (== (list x x) s)))

(deftest p36
  (is (= [:tofu]
         (run* [q]
               (twinso-36 (list :tofu q))))))

(defn loto [l]
  (conde
   ((emptyo l) s#)
   ((fresh [a]
           (firsto l a)
           (twinso a))
    (fresh [d]
           (resto l d)
           (loto d)))))

(deftest p38
  (is (= ['()]
         (run 1 [z]
              (loto (llist '(:g :g) z))))))

(deftest p42
  (is (= ['()
          '((_.0 _.0))
          '((_.0 _.0) (_.1 _.1))
          '((_.0 _.0) (_.1 _.1) (_.2 _.2))
          '((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3))]
         (run 5 [z]
              (loto (llist '(:g :g) z))))))

(deftest p45
  (is (= ['(:e (_.0 _.0) ())
          '(:e (_.0 _.0) ((_.1 _.1)))
          '(:e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
          '(:e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
          '(:e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4)))]
         (run 5 [r]
              (fresh [w x y z]
                     (loto (llist (list :g :g) (list :e w) (list x y) z))
                     (== r (list w (list x y) z)))))))
(deftest p47
  (is (= ['((:g :g) (:e :e) (_.0 _.0))
          '((:g :g) (:e :e) (_.0 _.0) (_.1 _.1))
          '((:g :g) (:e :e) (_.0 _.0) (_.1 _.1) (_.2 _.2))]
       (run 3 [out]
            (fresh [w x y z]
                   (== (llist (list :g :g) (list :e w) (list x y) z) out)
                   (loto out))))))

(defn listofo [predo l]
  (conde
   ((emptyo l) s#)
   ((fresh [a]
           (firsto l a)
           (predo a))
    (fresh [d]
           (resto l d)
           (listofo predo d)))))

(deftest p49
  (is (= ['((:g :g) (:e :e) (_.0 _.0))
          '((:g :g) (:e :e) (_.0 _.0) (_.1 _.1))
          '((:g :g) (:e :e) (_.0 _.0) (_.1 _.1) (_.2 _.2))]
         (run 3 [out]
              (fresh [w x y z]
                     (== out (llist (list :g :g) (list :e w) (list x y) z))
                     (listofo twinso out))))))

(defn loto [l]
  (listofo twinso l))

(defn eq-first? [l x]
  (= (first l) x))

(deftest p51-1
  (is (= true (eq-first? '(:a :b :c) :a))))

(deftest p51-2
  (is (= false (eq-first? '(:a :b :c) :d))))

(defn member? [x l]
  (cond
   (empty? l) false
   (eq-first? l x) true
   :else (member? x (rest l))))

(deftest p52
  (is (= true (member? :olive '(:virgin :olive :oil)))))

(defn eq-firsto [l x]
  (firsto l x))

;; membero is already part of core logic, but I suppose it could be
;; defined as:
;;
(defn memberoo [x l]
  (conde
   [(eq-firsto l x) s#]
   [(fresh [d]
           (resto l d)
           (memberoo x d))]))

(deftest p57
  (is (= '(true)
         (run* [q]
               (membero :olive '(:virgin :olive :oil))
               (== true q)))))

(deftest p58
  (is (= '(:hummus)
         (run 1 [y]
              (membero y '(:hummus :with :pita))))))

(deftest p59
  (is (= '(:with)
         (run 1 [y]
              (membero y '(:with :pita))))))

(deftest p60
  (is (= '(:pita)
         (run 1 [y]
              (membero y '(:pita))))))

(deftest p61
  (is (= ()
         (run* [y]
              (membero y ())))))

(deftest p62
  (is (= '(:hummus :with :pita)
         (run* [y]
              (membero y '(:hummus :with :pita))))))

(defn identity [l]
  (run* [y] (membero y l)))

;; Note: '(:pasta x :fragioli) is NOT the same as (list :pasta x :fragioli)
(deftest p66
  (is (= '(:e)
         (run* [x]
               (membero :e (list :pasta x :fragioli))))))

(deftest p69
  (is (= '(_.0)
         (run 1 [x]
              (membero :e (list :pasta :e x :fragiolio))))))

(deftest p70
  (is (= '(:e)
         (run 1 [x]
              (membero :e (list :pasta x :e :fragiolio))))))

(deftest p71
  (is (= '([:e _.0] [_.0 :e])
         (run* [r]
               (fresh [x y]
                      (membero :e (list :pasta x :fagioli y))
                      (== [x y] r))))))

(deftest p73
  (is (= (list '(:tofu . _.0))  ;; Not sure the exact syntax to get this to pass
         (run 1 [l]
              (membero :tofu l)))))

(deftest p76
  (is (= (list '(:tofu . _.0)
               '(_.0 :tofu . _.1)
               '(_.0 _.1 :tofu . _.2)
               '(_.0 _.1 _.2 :tofu . _.3)
               '(_.0 _.1 _.2 _.3 :tofu . _.4))
         (run 5 [l]
              (membero :tofu l)))))

(defn pmembero [x l]
  (conde
   [(emptyo l) u#]
   [(eq-firsto l x) (resto l ())]
   [(fresh [d]
           (resto l d)
           (pmembero x d))]))

(deftest p80
  (is (= (list '(:tofu)
               '(_.0 :tofu)
               '(_.0 _.1 :tofu)
               '(_.0 _.1 _.2 :tofu)
               '(_.0 _.1 _.2 _.3 :tofu))
         (run 5 [l]
              (pmembero :tofu l)))))

(deftest p81
  (is (= '(true)
         (run* [q]
               (pmembero :tofu (list :a :b :tofu :d :tofu))
               (== true q)))))

(defn pmembero [x l]
  (conde
   [(emptyo l) u#]
   [(eq-firsto l x) (resto l ())]
   [(eq-firsto l x) s#]
   [(fresh [d]
           (resto l d)
           (pmembero x d))]))

(deftest p84
  (is (= '(true true true)
         (run* [q]
               (pmembero :tofu (list :a :b :tofu :d :tofu))
               (== true q)))))

(defn pmembero [x l]
  (conde
;   [(emptyo l) u#]
   [(eq-firsto l x) (resto l ())]
   [(eq-firsto l x) (fresh [a d]   ;; '(a . d) => [a d]
                           (resto l (list a d)))]
   [(fresh [d]
           (resto l d)
           (pmembero x d))]))

(deftest p88
  (is (= '(true true)
         (run* [q]
               (pmembero :tofu (list :a :b :tofu :d :tofu))
               (== true q)))))

(deftest p89
  (is (= (list
          '(:tofu)
          '(:tofu _.0 _.1)
          '(_.0 :tofu)
          '(_.0 :tofu _.1 _.2)
          '(_.0 _.1 :tofu)
          '(_.0 _.1 :tofu _.2 _.3)
          '(_.0 _.1 _.2 :tofu)
          '(_.0 _.1 _.2 :tofu _.3 _.4)
          '(_.0 _.1 _.2 _.3 :tofu)
          '(_.0 _.1 _.2 _.3 :tofu _.4 _.5)
          '(_.0 _.1 _.2 _.3 _.4 :tofu)
          '(_.0 _.1 _.2 _.3 _.4 :tofu _.5 _.6))
         (run 12 [l]
              (pmembero :tofu l)))))


(defn pmembero [x l]
  (conde
   [(eq-firsto l x) (fresh [a d]
                           (resto l (list a d)))]
   [(eq-firsto l x) (resto l ())]
   [(fresh [d]
           (resto l d)
           (pmembero x d))]))

(deftest p94
  (is (= (list
          '(:tofu _.0 _.1)
          '(:tofu)
          '(_.0 :tofu _.1 _.2)
          '(_.0 :tofu)
          '(_.0 _.1 :tofu _.2 _.3)
          '(_.0 _.1 :tofu)
          '(_.0 _.1 _.2 :tofu _.3 _.4)
          '(_.0 _.1 _.2 :tofu)
          '(_.0 _.1 _.2 _.3 :tofu _.4 _.5)
          '(_.0 _.1 _.2 _.3 :tofu)
          '(_.0 _.1 _.2 _.3 _.4 :tofu _.5 _.6)
          '(_.0 _.1 _.2 _.3 _.4 :tofu))
         (run 12 [l]
              (pmembero :tofu l)))))

(defn first-value [l]
  (run 1 [y]
       (membero y l)))

(deftest p96
  (is (= '(:pasta)
         (first-value '(:pasta :e :fagioli)))))


(defn memberrevo [x l]
  (conde
   ; [(emptyo l) u#]
   [(fresh [d]
           (resto l d)
           (memberrevo x d))]
   [(eq-firsto l x)]))

(deftest p100
  (is (= '(:fagioli :e :pasta)
         (run* [x]
               (memberrevo x '(:pasta :e :fagioli))))))

(defn reverse-list [l]
  (run* [y]
        (memberrevo y l)))