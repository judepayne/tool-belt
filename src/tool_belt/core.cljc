(ns tool-belt.core
  (:require [clojure.walk       :as w]))

;; A personal utility library for Clojure/script with lots of useful and reuseable functions.

;; 1. COLLECTIONS

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))


(def not-in?
  "true if coll does not contains elm"
  (complement in?))


(defn apply-to-if
  [m test f & args]
  "Applies f to m and m if test is true. For conditional steps in threaded -> chains."
  (if test
    (apply f m args)
    m))


(defn join
  "Concat a collection of colls."
  [colls] (reduce concat colls))


(defn deep-merge
  "Merges maps recursively down."
  [& maps]
  (apply
   (fn m [& maps]
     (cond
       (every? map? maps)        (apply merge-with m maps)
       (every? sequential? maps) (reduce concat [] maps)
      :else  maps))
   maps))


(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))


(defn update-in-all
  "Like update-in but when special key :all is supplied in the ks vector,
  all map-entries at that level will be accepted."
  [m ks f & args]
  (let [up (fn up [m ks f args]
             (let [[k & ks] ks]
               (if ks
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (up v ks f args))) {} m)
                   (assoc m k (up (get m k) ks f args)))
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (apply f v args))) {} m)
                   (if-let [v (get m k)]
                     (assoc m k (apply f (get m k) args))
                     m)))))]
       (up m ks f args)))


(defn update-in-all-if
  "Like update-in-all but only if test is true."
  [m test ks f & args]
  (if test
    (apply update-in-all m ks f args)
    m))


(defn dissoc-in
  "Dissocs at the ks vector in a nested map.
  Supplying special key :all in the last position in ks will dissoc all entries.
  Supplying :all in another possible will cause all map-entries at that level to be recursed into."
  [m ks]
  (let [up (fn up [m ks]
             (let [[k & ks] ks]
               (if ks
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (up v ks))) {} m)
                   (assoc m k (up (get m k) ks)))
                 (if (= k :all)
                   {}
                   (reduce (fn [acc [kz v]]  (if (= k kz) acc (assoc acc kz v))) {} m)))))]
       (up m ks)))


(defn dissoc-in-when
  "Like dissoc-in, but items are dissoc'd only when (pred value-of-entry) is true."
  [m ks pred]
  (let [up (fn up [m ks]
             (let [[k & ks] ks]
               (if ks
                 (if (= k :all)
                   (reduce (fn [acc [kz v]] (assoc acc kz (up v ks))) {} m)
                   (assoc m k (up (get m k) ks)))
                 (if (= k :all)
                   (reduce (fn [acc [kz v]]
                             (if (pred v) acc (assoc acc kz v))) {} m)
                   (reduce (fn [acc [kz v]]
                             (if (and (= k kz) (pred v)) acc (assoc acc kz v))) {} m)))))]
       (up m ks)))


;; Performance could be improved by building into dissoc-in
(defn dissoc-in-clean
  "Likes dissoc-in but when the result of the dissoc leaves in empty collection in
  the nested map, removes that map-entry that it is in, so cleaning the map from
  empty nested collections like ..:b {:a #{} :b 1}...  [:a #{}] would be removed."
  [m ks]
  (let [k (butlast ks)
        tgt (get-in m k)
        res (dissoc tgt (last ks))]
    (if (empty? res)
      (dissoc-in m k)
      (dissoc-in m ks))))


(defn prewalk
  "Like clojure.walk/prewalk but stateful.
   parent is the parent of the form. state is recursed down through the form,
   at each level updated by the state-fn (which must accept previous state, the
   parent form and the current form. replace-fn accepts the current state and form."
  ([parent state state-fn replace-fn form]
   (letfn [(pw [parent state form]
             (let [nxt-s (state-fn state parent form)
                   nxt-f (replace-fn nxt-s form)]
               (w/walk (partial pw form nxt-s) identity nxt-f)))]
     (pw parent state form))))


(defn vec? [x]
  "Is x a vector?"
  (and (sequential? x) (not (map-entry? x))))


(defn with-path
  "Decorates every level of a nested form with it's path added in a :$path key."
  [form]
  (prewalk nil
           []
           ;; state function
           (fn [state parent form]
             (cond
               (map-entry? form)                    (conj state (key form))
               
               (and (or (map? form) (vec? form))
                    (vec? parent))
               (conj state (.indexOf parent form))
               
               :else state))
           ;; replace function
           (fn [state form]
             (if (map? form) (merge {:$path state} form) form))

           form))


(defn update-keys
  "Walks a nested map m recursively updating all keys with the supplied
   key-fn where the supplied predicate update? fn for the key is true.
   key-fn should have one parameter; the key.
   update? should accept two parameters; the key and the parent key."
  [m update? key-fn]
  (let [down (fn f [x p]
               (cond
                 (map-entry? x)   (let [[k v] x]
                                    (if (nil? v) nil   ;;prunes where v is nil
                                        (first {(f k p)
                                                (if (coll? v)
                                                  (f v k)
                                                  v)})))
                 
                 (seq? x)         (map #(f % p) x)

                 (coll? x)        (into (empty x) (map #(f % p) x))

                 :else            (if (update? x p) (key-fn x) x)))]
     (down m nil)))


;; BASICS

(defn err
  "Creates an exception object with error-string."
  [error-string]
  #?(:clj (Exception. ^String error-string)
     :cljs (js/Error. error-string)))


(defn parse-int [s]
  (try
    (let [n #?(:clj (Integer/parseInt s)
               :cljs (cljs.reader/read-string s))]
      (if (integer? n) n (throw (err (str s " should be an integer.")))))
    #? (:clj (catch Exception e (throw (err (str s " should be an integer."))))
        :cljs (catch js/Error e (throw (err (str s " should be an integer.")))))))


(defn parse-float [s]
  (try
    (let [n #?(:clj (Float/parseFloat s)
               :cljs (cljs.reader/read-string s))]
      (if (float? n) n (throw (err (str s " should be a floating point number.")))))
    #? (:clj (catch Exception e (throw (err (str s " should be a floating point number."))))
        :cljs (catch js/Error e (throw (err (str s " should be a floating point number.")))))))
