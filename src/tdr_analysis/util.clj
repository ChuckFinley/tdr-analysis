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

(defn between [x lower upper]
	(and (>= x lower) (<= x upper)))

; from http://stackoverflow.com/questions/6344454/clojure-range-case-macro
(defmacro range-case [target & cases]
  "Compare the target against a set of ranges or constant values and return
   the first one that matches. If none match, and there exists a case with the
   value :else, return that target. Each range consists of a vector containing
   one of the following patterns:
     [upper-bound]                 if this is the first pattern, match any
                                   target <= upper-bound
                                   otherwise, match any target <= previous
                                   upper-bound and <= upper-bound
     [< upper-bound]               if this is the first pattern, match any
                                   target < upper-bound
                                   otherwise, match any target <= previous
                                   upper-bound and < upper-bound
     [lower-bound upper-bound]     match any target where lower-bound <= target
                                   and target <= upper-bound
     [< lower-bound upper-bound]   match any target where lower-bound < target
                                   and target <= upper-bound
     [lower-bound < upper-bound]   match any target where lower-bound <= target
                                   and target < upper-bound
     [< lower-bound < upper-bound] match any target where lower-bound < target
                                   and target < upper-bound
   Example:
     (range-case target
                 [0 < 1] :strongly-disagree
                 [< 2]     :disagree
                 [< 3]     :neutral
                 [< 4]     :agree
                 [5]       :strongly-agree
                 42          :the-answer
                 :else       :do-not-care)
   expands to
     (cond
       (and (<= 0 target) (< target 1)) :strongly-disagree
       (and (<= 1 target) (< target 2)) :disagree
       (and (<= 2 target) (< target 3)) :neutral
       (and (<= 3 target) (< target 4)) :agree
       (<= 4 target 5) :strongly-agree
       (= target 42) :the-answer
       :else :do-not-care)
    Test cases:
      (use '[clojure.test :only (deftest is run-tests)])
      (deftest unit-tests
        (letfn [(test-range-case [target]
                                 (range-case target
                                             [0 < 1] :strongly-disagree
                                             [< 2]   :disagree
                                             [< 3]   :neutral
                                             [< 4]   :agree
                                             [5]     :strongly-agree
                                             42      :the-answer
                                             :else   :do-not-care))]
      (is (= (test-range-case 0) :strongly-disagree))
      (is (= (test-range-case 0.5) :strongly-disagree))
      (is (= (test-range-case 1) :disagree))
      (is (= (test-range-case 1.5) :disagree))
      (is (= (test-range-case 2) :neutral))
      (is (= (test-range-case 2.5) :neutral))
      (is (= (test-range-case 3) :agree))
      (is (= (test-range-case 3.5) :agree))
      (is (= (test-range-case 4) :strongly-agree))
      (is (= (test-range-case 4.5) :strongly-agree))
      (is (= (test-range-case 5) :strongly-agree))
      (is (= (test-range-case 42) :the-answer))
      (is (= (test-range-case -1) :do-not-care))))
    (run-tests)"
  (if (odd? (count cases))
    (throw (IllegalArgumentException. (str "no matching clause: "
                                           (first cases))))
    `(cond
      ~@(loop [cases cases ret [] previous-upper-bound nil]
          (cond
           (empty? cases)
           ret

           (= :else (first cases))
           (recur (drop 2 cases) (conj ret :else (second cases)) nil)

           (vector? (first cases))
           (let [condition (first cases)
                 clause (second cases)

                 [case-expr prev-upper-bound]
                 (let [length (count condition)]
                   (cond
                    (= length 1)
                    (let [upper-bound (first condition)]
                      [(if previous-upper-bound
                         `(and (<= ~previous-upper-bound ~target)
                               (<= ~target ~upper-bound))
                         `(<= ~target ~upper-bound))
                       upper-bound])

                    (= length 2)
                    (if (= '< (first condition))
                      (let [[_ upper-bound] condition]
                        [(if previous-upper-bound
                           `(and (<= ~previous-upper-bound ~target)
                                 (< ~target ~upper-bound))
                           `(< ~target ~upper-bound))
                         upper-bound])
                      (let [[lower-bound upper-bound] condition]
                        [`(and (<= ~lower-bound ~target)
                               (<= ~target ~upper-bound))
                         upper-bound]))

                    (= length 3)
                    (cond
                     (= '< (first condition))
                     (let [[_ lower-bound upper-bound] condition]
                       [`(and (< ~lower-bound ~target)
                              (<= ~target ~upper-bound))
                        upper-bound])

                     (= '< (second condition))
                     (let [[lower-bound _ upper-bound] condition]
                       [`(and (<= ~lower-bound ~target)
                              (< ~target ~upper-bound))
                        upper-bound])

                     :else
                     (throw (IllegalArgumentException. (str "unknown pattern: "
                                                            condition))))

                    (and (= length 4)
                         (= '< (first condition))
                         (= '< (nth condition 3)))
                    (let [[_ lower-bound _ upper-bound] condition]
                      [`(and (< ~lower-bound ~target) (< ~target ~upper-bound))
                       upper-bound])

                    :else
                    (throw (IllegalArgumentException. (str "unknown pattern: "
                                                           condition)))))]
             (recur (drop 2 cases)
                    (conj ret case-expr clause)
                    prev-upper-bound))

           :else
           (let [[condition clause]
                 `[(= ~target ~(first cases)) ~(second cases)]]
             (recur (drop 2 cases) (conj ret condition clause) nil)))))))

; how would one do for-across? I think a macro would be needed