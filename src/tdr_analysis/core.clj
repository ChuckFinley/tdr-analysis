(ns tdr-analysis.core
	(:require [tdr-analysis.io :as io])
	(:require [clojure.contrib.math :as math]))

(defn vert-vel [{x0 :time y0 :pressure} {x1 :time y1 :pressure}]
	(cond
		(= x1 x0) 0.0
		:else (/ (- y1 y0) (- x1 x0))))

(defn calculate-vert-vel
	([data] (calculate-vert-vel nil data))
	([earlier [p0 & ps]]
		(if-let [p1 (first ps)]
			(recur (conj earlier (assoc p0 :vert-vel (vert-vel p0 p1))) ps)
			(conj earlier (assoc p0 :vert-vel 0)))))

(defn surface? [{p :pressure}]
	(< p 0.1))

(defn idxs [i d]
	(if (surface? d)
		[-1 i]
		[i (inc i)]))

(defn calculate-dive-idx [data]
	(let [data (sort-by :time data)]
		(loop [i 0
					 earlier nil
					 [d & later] (partition-by surface? data)]
			(let [[idx0 idx1] (idxs i (first d))
						dive (map #(assoc % :dive-idx idx0) d)]
				(if later
					(recur idx1 (concat earlier dive) later)
					(concat earlier dive))))))

(defn analyze-dives [dives]
	(for [idx (range (inc (reduce max (map :dive-idx dives))))
				:let [dive (filter #(= idx (:dive-idx %)) dives)]]
		{:dive-idx idx
		:data-points dive
		:begin-dive (reduce min (map :time dive))
		:end-dive (reduce max (map :time dive))
		:max-depth (reduce max (map :pressure dive))
		:num-datapoints (count dive)}))

(defn analyze-data [data]
	(-> data
			calculate-vert-vel
			calculate-dive-idx))