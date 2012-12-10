(ns tdr-analysis.util)

(defn manip-map [f ks m]
	"Takes a function, a set of keys, and a map and applies the function
	to the map on the given keys. A modified version of the original map
	is returned with the results of the function applied to each keyed
	entry."
	(conj m
		(let [only (select-keys m ks)]
			(zipmap (keys only) (map f (vals only))))))

(defn select-across [& colls]
	"Returns a sequence of tuples where each tuple is the nth value
	from all the collections. Stops when the first collection runs
	out.
	e.g. [1 2 3] [4 5 6] => ((1 4) (2 5) (3 6))"
	(partition (count colls) (apply interleave colls)))

(defn every-nth
	([n]
		(every-nth n 0))
	([n offset]
		(fn [coll]
			(map coll (range offset (count coll) n)))))

; how would one do for-across? I think a macro would be needed