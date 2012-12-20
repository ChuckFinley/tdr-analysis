(ns tdr-analysis.util
	(:require [clj-time.core :as time])
	(:require [clj-time.format :as time.format]))

(defn between [x lower upper]
	(and (>= x lower) (<= x upper)))

(defn datetime [{:keys [date time]}]
	(time.format/parse
		(time.format/formatter "M/d/yyyy HH:mm:ss")
		(str date " " time)))

(defn mean [coll]
	(if (empty? coll)
		nil
		(/ (apply + coll) (count coll))))

(defn update
	"Analogy - update:update-in::assoc:assoc-in"
	([m k f & args]
		(assoc m k (apply f (get m k) args))))