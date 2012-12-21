(ns tdr-analysis.io
	(:require [clojure-csv.core :as csv])
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as string])
	(:require [clj-time.core :as time])
	(:require [clj-time.coerce :as time.coerce])
	(:require [clj-time.format :as time.format])
	(:use [tdr-analysis.util]))

(def log1 "assets/2012/869/LAT150_0869_120819_142105_00.csv")
(def log2 "assets/2012/869/LAT150_0869_120819_142105_01.csv")
(def log3 "assets/2012/869/LAT150_0869_120819_142105_02.csv")		; no pressure, not used

(defn parse-csv
	"Microsoft puts carriage returns at the end of lines in CSV files
	which screws up a lot of stuff"
	[f] (csv/parse-csv f :end-of-line "\r"))

(defn clean-row [fields log-id row]
	(if (every? string/blank? row) nil
		(zipmap fields
			(map (fn [k v] (let [v (string/trim v)] (cond
					(= k :rec#) (str log-id "-" v)
					(contains? #{:date :time} k) v
					:else (read-string v))))
				fields row))))

(defn normalize-time
	"Reduce time to seconds since start of log"
	([log]
	;{:pre [	(not-any? nil? (map :date log))
	;				(not-any? nil? (map :time log)) ]}
		(let [datetimes	(map datetime log)
					t0 				(reduce
											#(if (time/before? %1 %2) %1 %2)
											datetimes)		; find the earliest datetime in the log
					elapsed		(map #(time/in-secs (time/interval t0 %)) datetimes)]
			(map #(assoc %1 :elapsed %2) log elapsed))))

(defn normalize-fields
	"Aggregate records by time elapsed."
	([fields log]
		(map
			; Keep a list of rec#. Don't change elapsed, date, or time. Take mean of other fields.
			#(into {:rec# (map :rec# %) :date (:date (first %)) :time (:time (first %)) :elapsed (:elapsed (first %))}
				(map (fn [f] (let [xs (filter identity (map f %))] {f (mean xs)})) fields))
			(partition-by :elapsed log))))		; aggregated by elapsed

(defn get-logs
	([]
		(get-logs ["00"	"01"] [log1 log2] [[:pressure :temp]	[:pressure :temp]]))
	([log-ids filenames fields]
		(->>
			(map
				(fn [log-id filename fields]
					(with-open [log (io/reader filename)]
						(doall
							(->> 	log
										(csv/parse-csv)
										(drop 3)		; the first three rows are headers
										(map (partial clean-row (apply conj [:rec# :date :time] fields) log-id))
										(remove nil?)))))
				log-ids filenames fields)
			(apply concat)
			(normalize-time)		; normalize by seconds since beginning of logs
			(sort-by :elapsed)
			(normalize-fields (set (flatten fields))))))

(defn strr
	"Applies str to scalars in a collection recursively (for writing to csv)"
	[l]
	(if (coll? l)
		(map strr l)
		(str l)))

(defn write-csv
	"Spit records to a csv file"
	([records file]
		(spit file
			(csv/write-csv
				(cons (map name (keys (first records))) (strr (map vals records)))))))

(def data (get-logs))

