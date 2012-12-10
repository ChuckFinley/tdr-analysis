(ns tdr-analysis.io
	(:require [clojure-csv.core :as csv])
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as string])
	(:require [clj-time.core :as time])
	(:require [clj-time.coerce :as time.coerce])
	(:use [tdr-analysis.util]))

(defn parse-csv
	"Microsoft puts carriage returns at the end of lines in CSV files
	which screws up a lot of stuff"
	[f] (csv/parse-csv f :end-of-line "\r"))

(def sample-file "assets/10Aug11.csv")

(defn clean-value [[k v]]
	"k => field keyword
	v => field value
	Returns the parsed value of v (based on the type of k)"
	(try
		(cond
			(= k :time)
				(let [[M D Y h m s] (map #(Integer. %) (re-seq #"\d+" v))]
					(time.coerce/to-long (time/date-time (+ 2000 Y) M D h m s)))
			:else (read-string v))
		(catch Exception e nil)))

(defn clean-row [r]
	(into {}
		(for [[k v] (select-across [:time :pressure] (map r [3 4]))] ; [3 4] are the time, pressure indices
			[k (clean-value [k v])])))

(defn get-file
	([]
		(get-file sample-file))
	([filename]
		(with-open [file (io/reader filename)]
			(doall
				(next		; Drop the header row
					(for [row (parse-csv file)
								:let [r (clean-row row)]
								:when (every? identity r)]
						r))))))

(defn normalize-datetime
	"Reduce datetime to seconds since beginning of log and drop milliseconds"
	[t]
	(let [t0 (reduce min (map :time t))
				normalize #(/ (- % t0) 1000)]
		(map #(manip-map normalize #{:time} %) t)))

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

