# pricesensitivitymeter, v1.3.0 (release date: 2024-02-25)

- new parameter "acceptable_range": allows to switch between van Westendorp's original definition of acceptable price ranges and a narrower definition which is used by some market research companies
- new parameters "pi_calibrated_toocheap" and "pi_calibrated_tooexpensive" for functions psm_analysis() and psm_analysis_weighted(): allows to specify a calibrated purchase probability at the "too cheap" and "too expensive" price, respectively, which is different from zero
- consistency: renames "trial" to "reach"

# pricesensitivitymeter, v1.2.2 (release date: 2021-10-19)

- psm_plot(): flexible color for Indifference Price Point and Optimal Price Point
- psm_plot(): enforcing digits for Indifference Price Point and Optimal Price Point labels
- change in DESCRIPTION to address a CRAN note (no need for lazy loading of data)

# pricesensitivitymeter, v1.2.1 (release date: 2021-03-21)

- fixes URL in package description
- fixes date formatting in DESCRIPTION

# pricesensitivitymeter, v1.2 (release date: 2021-03-21)

- new function psm_plot() for automatic plotting of the result
- package now also supports input in tibble format
- fix in plotting vignette: fix position of indifference price point
- fixes in vignette formatting to emphasize function names
- change of Github URLs due to change of Github username
- fix of external URLs that have changed
- housekeeping: re-factoring code

# pricesensitivitymeter, v1.1.1 (release date: 2019-08-23)

- fixes a markdown issue in the README file

# pricesensitivitymeter, v1.1.0 (release date: 2019-08-22)

- adds the parameter interpolation_steps that lets users define the granularity of the interpolation between the observed prices
- fixes a bug that caused respondents with intransitive price preferences to be used in the analysis when using psm_analysis_weighted() (and adds some unit testing to check this in the future)
- corrects the "Interpolation in Small Samples" vignette: now shows correctly "not cheap" instead of "cheap" and "not expensive" instead of "expensive"
- extends unit test coverage

# pricesensitivitymeter, v1.0.1 (release date: 2019-04-29)

- makes some tests slightly more lenient to account for rounding differences

# pricesensitivitymeter, v1.0.0 (release date: 2019-04-28)

- adds the psm_analysis_weighted() function to deal with weighted input data. This requires the survey package.
- adds the argument intersection_method to define how to calculate the price points in case there are multiple intersections (should be a rare case)
- amends the psm class definition and the summary function for the psm class to account for weighted/unweighted data
- bug fix: if a respondent has the same answer for both the "cheap" and the "expensive" price, the interpolation of purchase probabilities in the Newton Miller Smith extension now returns non-empty answers
- bug fix: if "too cheap" information is missing, skip some calculations

# pricesensitivitymeter, v0.4.1 (release date: 2018-10-21)

- fixes issue with calculation of "point of marginal cheapness" and "point of marginal expensiveness" (acceptable price range). also affects documentation and visualization vignette. thanks to athib2 for flagging it
- fixes bug in calculation of % of removed cases (did show only 0/1 before, now proper percentages)
- fixes typos in summary() function and in the documentation
- adds unit testing for Newton Miller Smith input as dataframe

# pricesensitivitymeter, v0.3.3 (release date: 2018-10-14)

- fixes an issue in the unit testing with an if condition with length greater 1 (thanks to Tomas Kalibera for automated testing in R-devel)

# pricesensitivitymeter, v0.3.2 (release date: 2018-05-12)

- changing links from http://... to https://...

# pricesensitivitymeter, v0.3.0 (release date: 2018-05-10)

- adding optional argument to interpolate between price points
- adding a vignette for the interpolate argument
- edits in the README: fixing typos, amending the installation instructions

# pricesensitivitymeter, v0.2.1 (release date: 2018-04-12)

- addressing feedback from CRAN submit: adding proper citation of the main publication in the DESCRIPTION file

# pricesensitivitymeter, v0.2 (release date: 2018-04-11)

- adding support for running the analysis without a "too cheap" price
- adding a charting vignette
- adding automated testing of function inputs and function outputs
- renaming the main function from "PSManalysis" to "psm_analysis" (for readability reasons)
- improving smoothing of trial/revenue optimization when using Newton Miller Smith extension: setting up the price matrix with continuous price steps between the minimum and the maximum to avoid jumps that come from the lack of price points in a specific area
- refining the data input checks (resulting from the c() implementation of hierarchies between data classes)

# pricesensitivitymeter, v0.1 (release date: 2017-05-20)

- first complete Github version incl. function documentation
