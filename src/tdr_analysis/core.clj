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
				(vec (map (fn [p0 p1] (assoc p0 k (f p0 p1))) data (next data)))
				(assoc pn k final)))))

(defn slope [x-key y-key]
	(fn [{x0 x-key y0 y-key} {x1 x-key y1 y-key}]
		{:pre [(not= x0 x1)]}
		(if (some nil? [x0 y0 x1 y1]) nil
			(/ (- y1 y0) (- x1 x0)))))

(def calculate-vert-vel (calculate-forward (slope :elapsed :pressure) :vert-vel 0.0))
(def calculate-vert-acc (calculate-forward (slope :elapsed :vert-vel) :vert-acc 0.0))

(defn calculate-dive-idx
	"Split data into submerged and surface partitions. Number the dives 1, 2, 3... and
	the inter-dive periods -1, -2, -3..."
	[data]
	(apply concat
		(map
			(fn [part idx] (map #(assoc % :dive-idx idx) part))	; assoc points in parts with dive idx
			(partition-by #(< 0.1 (:pressure %)) data)					; partition by submerged or surface
			(interleave (iterate inc 1) (iterate dec -1)))))		; idxs are 1 -1 2 -2 3 -3 ...

(defn find-wiggles
	"Splits a dive into wiggles, defined as three or more points where the vertical
	velocities of the first and last points are negative and the vertical velocities
	in between are all positive or zero (at least one positive)."
	([{dive :datapoints}]
		(for [[neg1 pos neg2] (->>	dive
																; split up into pos/zero & neg partitions
																(partition-by (comp neg? :vert-vel))
																; drop leading pos/zero partition
																(drop-while (comp (complement neg?) :vert-vel first))
																; group partitions into (neg pos/zero neg) super partitions
																(partition 3 2))
					; filter out super partitions of pattern (neg zero neg) - these are not wiggles!
					:when (some (comp pos? :vert-vel) pos)]
			; one neg at start and end, all pos/zero in the middle
			(concat [(last neg1)] pos [(first neg2)]))))

(defn find-steps
	"Steps are elements where the vertical velocity dips below Tvert_vel
	but not below 0"
	([{dive :datapoints}]
		(let [Tvert-vel	0.35
					step-vel	#(if (neg? %) -1 (if (> % Tvert-vel) 1 0))
					step?			(fn [[[{v1 :vert-vel} & v1s] [{v2 :vert-vel} & v2s] [{v3 :vert-vel} & v3s]]]
											(and (pos? (step-vel v1)) (zero? (step-vel v2)) (pos? (step-vel v3))))]
			(->>	dive
						; partition by vertical velocity compared to (0, Tvert-vel)
						(partition-by (comp step-vel :vert-vel))
						; group partitions into triplets
						(partition 3 1)
						; a step is a sequence of points in (0, Tvert-vel) with > Tvert-vel on each side
						(filter step?)
						; the middle partition in the triplet is the step
						(map second)))))

(defn analyze-elements [dive]
	(sort-by :begin
		(map
			#(let [	data			(:datapoints %)
							begin			(reduce min (map :elapsed data))
							end				(reduce max (map :elapsed data))
							amplitude	(-
													(reduce max (map :pressure data))
													(reduce min (map :pressure data)))]
			(assoc %	:begin			begin
								:end			 	end
								:amplitude	amplitude
								:duration		(- end begin)))
			(concat
				(for [wiggle (find-wiggles dive)]
					{	:type :wiggle
						:datapoints wiggle})
				(for [step (find-steps dive)]
					{	:type :step
						:datapoints step})))))

(defn bottom-phase-elements [elements ledge]
	(->>	elements
				; bottom phase starts with first element deeper than ledge
				(drop-while #(< (-> % :datapoints first :pressure) ledge))
				reverse
				; bottom phase ends with last element deeper than ledge
				(drop-while #(< (-> % :datapoints last :pressure) ledge))
				reverse))

(defn analyze-bottom-phase [dive]
	{:pre (contains? dive :elements)}
	(let [ledge					(* 0.75 (:max-depth dive))
				elements			(bottom-phase-elements (:elements dive) ledge)]
		(if (empty? elements)
			nil ; there are no elements deeper than the ledge, then there is no bottom phase
			(let [begin				(-> elements first :begin)
						end					(-> elements last :end)
						duration		(- end begin)
						datapoints	(filter #(between (:elapsed %) begin end) (:datapoints dive))
						min-depth		(reduce min (map :pressure datapoints))
						max-depth		(:max-depth dive)
						depth-range	(- max-depth min-depth)]
				{	:ledge						ledge
					:elements					elements
					:begin						begin
					:end							end
					:duration					duration
					:broadness-idx		(/ duration (:duration dive))
					:depth-range-idx	(/ depth-range (:max-depth dive))
					:symmetry-idx			(/ (-> (filter #(= max-depth (:pressure %)) datapoints) first :elapsed) duration)
					:raggedness-idx		(apply + (->> elements (filter #(= (:type %) :wiggle)) (map :amplitude)))}))))

(defn analyze-dives [data]
	(for [dive (partition-by :dive-idx data)
				:let [begin	(reduce min (map :elapsed dive))
							end		(reduce max (map :elapsed dive))]]
		((comp
			#(assoc % :bottom-phase (analyze-bottom-phase %))
			#(assoc % :elements (analyze-elements %)))
		{	:dive-idx 			(:dive-idx (first dive))
			:begin 					begin
			:end 						end
			:duration 			(- end begin)
			:max-depth 			(reduce max (map :pressure dive))
			:num-datapoints	(count dive)
			:datapoints 		dive})))

(defn analyze-data [data]
	(-> data
			calculate-vert-vel
			calculate-dive-idx))

(defn dives-with [k v ds]
	(cond
		(= k :element-type)
		(filter #(some v (map :type (:elements %))) ds)
		(= k :dive-idx)
		(filter #(v (:dive-idx %)) ds)))

;(def analyzed-data (analyze-data io/data))
;(def analyzed-dives (analyze-dives analyzed-data))
