(ns tdr-analysis.core
	(:require [tdr-analysis.io :as io])
	(:require [clojure.contrib.math :as math])
	(:use [tdr-analysis.util]))

(defn calculate-forward [f k final]
	"Some variables are functions of a point and the point after (e.g.
		vertical velocity and vertical acceleration). This function takes a
		calculation function, a key, and the final value. It applies the 
		calculation function to each point and the following point and associates
		the result with the key (e.g. associate :vert-vel with the vertical
		velocity between each point and the next). The final point doesn't have a
		next point, so it just gets the final value."
	(fn [data]
		(let [pn (last data)]
			(conj
				(for [[p0 p1] (partition 2 1 data)]
					(assoc p0 k (f p0 p1)))
				(assoc pn k final)))))

(defn slope [x-key y-key]
	(fn [{x0 x-key y0 y-key} {x1 x-key y1 y-key}]
		{:pre [(not= x0 x1)]}
		(/ (- y1 y0) (- x1 x0))))

(def vert-vel (slope :time :pressure))
(def vert-acc (slope :time :vert-vel))

(def calculate-vert-vel (calculate-forward vert-vel :vert-vel 0.0))
(def calculate-vert-acc (calculate-forward vert-acc :vert-acc 0.0))

(defn surface? [{p :pressure}]
	(< p 0.1))
(def submerged? (complement surface?))

(defn calculate-dive-idx [data]
	(apply concat
		(let [dive-parts (partition-by surface? (sort-by :time data))
					dive-idxs (interleave (iterate inc 1) (iterate dec -1))] ; dive and inter-dive periods are numbered 1, -1, 2, -2, 3, -3...
			(for [[idx part] (select-across dive-idxs dive-parts)]
				(map #(assoc % :dive-idx idx) part)))))

(defn analyze-dives [dives]
	(for [dive (partition-by :dive-idx dives)
				:let [idx (:dive-idx (first dive))]]
		{:dive-idx idx
		:begin-dive (reduce min (map :time dive))
		:end-dive (reduce max (map :time dive))
		:max-depth (reduce max (map :pressure dive))
		:num-datapoints (count dive)}))

(defn analyze-data [data]
	(-> data
			calculate-vert-vel
			calculate-vert-acc
			calculate-dive-idx))