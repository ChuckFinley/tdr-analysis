# tdr-analysis

This project is an implementation of the Halsey, Bost, Handrich algorithm (see here: http://www.cebc.cnrs.fr/publipdf/2007/HPB30.pdf). It analyzes the data from a temperature and depth recorder. Currently, it calculates the following variables:

*	Vertical velocity
*	Dive index
*	Per dive:
	*	Dive start and end
	*	Maximum depth
	*	Number of datapoints
	*	Elements (wiggles and steps)
	*	Bottom phase start and end
	*	Broadness
	*	Depth range
	*	Symmetry
	*	Raggedness

When completed, it will also calculate
*	Per dive:
	*	Dive bout index
	*	Depth consistency

*	Per dive bout
	*	Bout start and end
	*	Number of dives
	*	Inter-dive period
	*	Inter-bout period

## Usage

Try it out: `analyzed-dives` is a collection of all the analyzed dives in the sample input file. It's a big collection so use `(pprint (nth analyzed-dives 4))` (or any other index) to see specific dives in a readable format.
