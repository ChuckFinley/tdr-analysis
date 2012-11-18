# tdr-analysis

The final goal is to create a summary of a collection of logs. What goes in the summary? That's a good question. Some ideas are:
* Number of dives
* Number of dive bouts
* Dives per bout
* And on a per dive basis:
  * Time elapsed
  * Max depth
  * Time at bottom
  * Time descending/ascending


## Usage

Try it out: `(seq-slope time-pres)`. `time-pres` produces a sequence of tuples [datetime, pressure] from the sample file 10Aug11.csv. Running `seq-slope` on such a sequence adds slope to the tuples: [datetime, pressure, slope]. Hopefully I can use slope to calculate when dives begin/end and when the bottom is reached.


