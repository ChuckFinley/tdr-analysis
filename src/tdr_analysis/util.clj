(ns tdr-analysis.util)

(defn between [x lower upper]
	(and (>= x lower) (<= x upper)))