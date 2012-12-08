(ns tdr-analysis.io
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

(defn cleanse-csv [[k v]]
	(try
		(cond
			(= k :time)
				(let [[M D Y h m s] (map #(Integer. %) (re-seq #"\d+" v))]
					[k (time.coerce/to-long (time/date-time (+ 2000 Y) M D h m s))])
			:else [k (read-string v)])
		(catch Exception e nil)))

(defn get-file
	"fields => a set of keyword identifiers for csv fields.
	Output: a sequence of records for the fields given."
	([]
		(get-file sample-file))
	([filename]
		(with-open [csv-file (io/reader filename)]
			(doall
				(next
					(for [row (parse-csv csv-file)
								:let [record
											(map cleanse-csv {:time (row 3) :pressure (row 4)})]
								:when (every? identity record)]
							(into {} record)))))))

(defn update
	"applies f to value for k in h"
	[h f k]
	(if (contains? h k)
		(assoc h k (f (k h)))
		h))

(defn normalize-datetime
	"Reduce datetime to seconds since beginning of log and drop milliseconds"
	[t]
	(let [t0 (reduce min (map :time t))]
		(map (fn [r] (update r #(/ (- % t0) 1000) :time)) t)))

(defn strr
	"Applies str to scalars in a collection recursively (for writing to csv)"
	[l]
	(if (coll? l)
		(map strr l)
		(str l)))

(defn write-csv
	"Take table of records and spit it to a csv file"
	([table file]
		(spit file
			(csv/write-csv
				(cons (map name (keys (first table))) (strr (map vals table)))))))

(def data (normalize-datetime (get-file)))

