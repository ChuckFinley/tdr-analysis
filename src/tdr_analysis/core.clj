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

(defn calculate-thru-0
	"Calculate which points have velocity pass through 0 (local extrema
	in pressure with respect to time)"
	([dive]
		(loop [result []
					 data dive
					 trend 1]
			(if-let [point (first data)]
				(let [v (:vert-vel point)]
					(recur (conj result (assoc point :thru-0 (neg? (* v trend))))
								 (rest data)
								 (if (zero? v) trend v)))
				result))))

(defn find-wiggles
	"Splits a dive into elements - wiggles and steps. Wiggles are spans
	of data points where velocity passes through 0 three times. Steps are
	not yet implemented."
	([dive]
		(let [dive (calculate-thru-0 dive)]
			(for [wiggle-boundaries
						(->>	dive
									(drop-while (complement :thru-0))
									(filter :thru-0)
									(partition 3 2))
						:let [[start middle end] (map :time wiggle-boundaries)]]
				(filter #(between (:time %) start end) dive)))))

(defn step-vel
	"Calculate if a point has vertical velocity in the step element range
	i.e. 0 < vert-vel < Tvert_vel where Tvert_vel = .35"
	([{v :vert-vel}]
		(let [Tvert_vel 0.35]
			(range-case v
				[< 0]						-1
				[0 Tvert_vel]		0
				:else						1))))

(defn step? [r1 r2 r3]
	(let [[s1 s2 s3] (map #(step-vel (first %)) [r1 r2 r3])]
		(and (pos? s1) (zero? s2) (pos? s3))))

(defn find-steps
	"Steps are elements where the vertical velocity dips below Tvert_vel
	but not below 0"
	([dive]
		(for [step-boundaries
					(->>	dive
								(partition-by step-vel)
								(partition 3 1)
								(filter #(apply step? %))
								(map second))]
			(let [start	(:time (first step-boundaries))
						end		(:time (last step-boundaries))]
				(filter #(between (:time %) start end) dive)))))

(defn analyze-elements [dive]
	(sort-by :begin (concat
		(for [wiggle (find-wiggles dive)]
			{:type :wiggle
			:begin (reduce min (map :time wiggle))
			:end (reduce max (map :time wiggle))
			:amplitude (-
				(reduce max (map :pressure wiggle))
				(reduce min (map :pressure wiggle)))
			:datapoints wiggle})
		(for [step (find-steps dive)]
			{:type :step
			:begin (reduce min (map :time step))
			:end (reduce max (map :time step))
			:amplitude (-
				(reduce max (map :pressure step))
				(reduce min (map :pressure step)))
			:datapoints step}))))

(defn analyze-dives [dives]
	(for [dive (partition-by :dive-idx dives)
				:let [idx (:dive-idx (first dive))
							elements (analyze-elements dive)]]
		{:dive-idx idx
		:begin (reduce min (map :time dive))
		:end (reduce max (map :time dive))
		:max-depth (reduce max (map :pressure dive))
		:num-elements (count elements)
		:num-datapoints (count dive)
		:elements elements
		:datapoints dive}))

(defn analyze-data [data]
	(-> data
			calculate-vert-vel
			calculate-dive-idx))

(defn nth-dive [data n] (first (filter #(= n (:dive-idx %)) (analyze-dives (analyze-data data)))))


