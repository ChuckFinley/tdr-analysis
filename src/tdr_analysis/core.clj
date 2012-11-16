(ns tdr-analysis.core
	(:require [clojure-csv.core :as csv])
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as string])
	(:require [clj-time.core :as time])
	(:require [clj-time.coerce :as time.coerce]))

(defn parse-csv
	"Microsoft puts carriage returns at the end of lines in CSV files
	which screws up a lot of stuff"
	[f] (csv/parse-csv f :end-of-line "\r"))

(def sample-file "assets/10Aug11.csv")

(defn get-col-heads
	"Take the first row of a csv file (the headers), convert it to lower
	case, map it to keywords, and create a hashmap of keyword=>index. 
	Now can be used to index into csv fields by name."
	([]
	(get-col-heads sample-file))
	([filename]
	(with-open [csv-file (io/reader filename)]
		(let [headers (first (parse-csv csv-file))]
			(zipmap
				(map #(keyword (string/lower-case %)) headers)
				(iterate inc 0))))))

(def col-heads (get-col-heads))

(defn idxs [& fields]
	(into [] (for [f fields] (col-heads f))))

(defn cleanse-csv [f v]
	(cond
		(= f :datetime)
			(let [[M D Y h m s] (map #(Integer. %) (re-seq #"\d+" v))]
				(time.coerce/to-long (time/date-time (+ 2000 Y) M D h m s)))
		:else (read-string v)))

(defn csv-tuples
	"fields => vector of keyword identifiers for csv fields.
	Output: a sequence of tuples for the fields given."
	([fields]
		(csv-tuples sample-file fields))
	([filename fields]
		(with-open [csv-file (io/reader filename)]
			(doall
				(next
					(for [row (parse-csv csv-file)]
						(into []
							(for [f fields]
								(try
									(cleanse-csv f (row (col-heads f)))
									(catch Exception e))))))))))

(defn slope [[x1 y1] [x0 y0]]
	(/ (- x1 x0) (- y1 y0)))

(defn seq-slope
	"t => sequence of tuples e.g. ([x0 y0] [x1 y1]). Must be sorted by x.
	Output tuples with slope (0 first slope) e.g ([x0 y0 0] [x1 y1 s1])"
	([t]
		(seq-slope t [0 (-> t first second)]))
	([t xy0]
		(if-let [xy1 (first t)]
			(cons [(first xy1) (second xy1) (slope xy1 xy0)] (seq-slope (next t) xy1))
			() )))