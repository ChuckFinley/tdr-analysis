# tdr-analysis

This project is an implementation of the Halsey, Bost, Handrich algorithm (see here: http://www.cebc.cnrs.fr/publipdf/2007/HPB30.pdf). It analyzes the data from a temperature and depth recorder. Currently, it calculates the following variables:

*	Vertical velocity
*	Dive index
*	Per dive:
	*	Dive start and end
	*	Maximum depth
	*	Number of datapoints

When completed, it will also calculate
*	Per dive:
	*	Dive bout index
	*	Elements (wiggles and steps)
	*	Bottom phase start and end
	*	Broadness
	*	Depth range
	*	Depth consistency
	*	Symmetry
	*	Raggedness
*	Per dive bout
	*	Bout start and end
	*	Number of dives
	*	Inter-dive period
	*	Inter-bout period

## Usage

Try it out: `(analyze-data io/data)` will calculate vertical velocity and dive index. This: `(analyze-dives (analyze-data io/data))` will summarize the dives.


