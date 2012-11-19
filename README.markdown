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

Try it out: `(write-csv (analyze-dives time-pres) "sample-output.csv")`. `time-pres` produces a records table (sequence of hash-maps) from the sample file 10Aug11.csv. Running `analyze-dives` on it adds velocity and motion (ascending, descending, surface, bottom) to the records. `write-csv` demunges the results and spits them to the file of your choosing.


