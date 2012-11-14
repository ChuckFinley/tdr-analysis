(ns tdr-analysis.core
	(:require [clojure.data.csv :as csv])
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as string])
	(:require [clj-time.core :as time])
	(:require [clj-time.format :as time]))

(def sample-file "assets/10Aug11.csv")

(defn get-col-heads
	"Take the first row of a csv file (the headers), convert it to lower
	case, map it to keywords, and create a hashmap of keyword=>index. 
	Now can be used to index into csv fields by name."
	([]
	(get-col-heads sample-file))
	([filename]
	(with-open [csv-file (io/reader sample-file)]
		(let [headers (.readLine csv-file)]
			(zipmap
				(map keyword (string/split (string/lower-case headers) #","))
				(iterate inc 0))))))

(def col-heads (get-col-heads))

(defn csv-tuples
	"fields => vector of keyword identifiers for csv fields.
	A sequence of vectors containing a tuple for the fields given. Use
	next to strip the tuple from the header line."
	([fields]
	(csv-tuples sample-file fields))
	([filename fields]
	(next
		(with-open [csv-file (io/reader sample-file)]
			(doall
				(for [row (csv/read-csv csv-file)]
					(apply vector
						(for [field fields
							:let [idx (col-heads field)]]
							(nth row idx nil)))))))))