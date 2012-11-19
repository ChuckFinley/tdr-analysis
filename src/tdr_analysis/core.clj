(ns tdr-analysis.core
	(:require [clojure-csv.core :as csv])
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as string])
	(:require [clj-time.core :as time])
	(:require [clj-time.coerce :as time.coerce])
	(:require [clojure.contrib.math :as math]))

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
				(map #(keyword (string/lower-case %)) headers)))))

(def col-heads (get-col-heads))

(defn cleanse-csv [[k v]]
	(try
		(cond
			(= k :datetime)
				(let [[M D Y h m s] (map #(Integer. %) (re-seq #"\d+" v))]
					[k (time.coerce/to-long (time/date-time (+ 2000 Y) M D h m s))])
			:else [k (read-string v)])
		(catch Exception e nil)))

(defn csv-records
	"fields => a set of keyword identifiers for csv fields.
	Output: a sequence of records for the fields given."
	([fields]
		(csv-records sample-file fields))
	([filename fields]
		(with-open [csv-file (io/reader filename)]
			(doall
				(next
					(for [row (parse-csv csv-file)
								:let [record
											(map cleanse-csv (filter #(fields (first %)) (zipmap col-heads row)))]
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
	(let [t0 (reduce min (map :datetime t))]
		(map (fn [r] (update r #(/ (- % t0) 1000) :datetime)) t)))

(def time-pres
	(normalize-datetime (csv-records #{:datetime :pressure})))

(defn velocity [{x0 :datetime y0 :pressure} {x1 :datetime y1 :pressure}]
	(cond
		(= x1 x0) 0.0
		:else (/ (- y1 y0) (- x1 x0))))

(defn motion
	"pressure < 0 => surf
	 velocity > .25 => desc
	 velocity < .25 => asc
	 (abs velocity) < .25 AND pressure > 1 => bott"
	([{p :pressure v :velocity}]
		(motion p v))
	([p v]
		(cond
			(< p 0.0) :surf
			(> v 0.25) :desc
			(< v -0.25) :asc
			(and (< (math/abs v) 0.25) (> p 1)) :bott)))

(defn strr
	"Applies str to scalars in a collection recursively (for writing to csv)"
	[l]
	(if (coll? l)
		(map strr l)
		(str l)))

(defn analyze-dives
	"Convert {time, pressure} to {time, presure, velocity, motion, dive #}"
	([[r0 & rn]]
		(let [r0 (assoc r0 :velocity 0 :motion nil :dive -1)
					r1 (first rn)
					v1 (velocity r0 r1)
					m1 (motion (:pressure r1) v1)]
			(cons r0 (analyze-dives rn v1 m1 -1))))
	([[r0 & rn] v m d]
		(if-let [r1 (first rn)]
			(cons
				(assoc r0 :velocity v :motion m :dive d)
				(let [p1 (:pressure r1)
							v1 (velocity r0 r1)
							m1 (motion p1 v1)]
					(analyze-dives rn v1 m1 d)))
			[(assoc r0 :velocity v :motion m :dive d)])))

(defn write-csv
	"Take table of records and spit it to a csv file"
	([table file]
		(spit file
			(csv/write-csv
				(cons (map name (keys (first table))) (strr (map vals table)))))))